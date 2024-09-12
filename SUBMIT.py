#!/usr/local/bin/python3

# Copyright © 2018 by John Murray
# No rights reserved.

from ftplib import FTP

import argparse
import multiprocessing
import os
import re
import subprocess
import sys
import time

envEditor = ''

def opts():
    parser = argparse.ArgumentParser(description='Submit a job to Z/OS, wait for it to end and get the output back.',
                                    formatter_class=argparse.RawDescriptionHelpFormatter,
                                    epilog='Notes.\nUserid and password will be obtained from environment variables ZOSUSER and ZOSPSWD if defined.\n'
                                            'Hostname will be obtained from environment variables ZOSHOST if defined.\n'
                                            'A directory called "listing" will be created if one does not exist. The script uses this directory for temporary files and listings.\n'
                                            'If the user decides to edit the listing, the name of the editor will be obtained from environment variable ZOSEDITOR. If the variable is not defined, the editor will default to "x"')
    parser.add_argument('-d',
                        action='store_true',
                        dest='debug',
                        default=False,
                        help='Run the command in debug mode')
    parser.add_argument('-e',
                        action='store_true',
                        dest='edit',
                        default=False,
                        help='Edit the listing file')
    parser.add_argument('-w',
                        default=5,
                        action='store',
                        dest='wait',
                        metavar='seconds',
                        help='Seconds to sleep before checking for job completion',
                        type=int)
    parser.add_argument('-u',
                        action='store',
                        dest='user',
                        metavar='userid',
                        help='Userid to sign-in to zOS with')
    parser.add_argument('-p',
                        action='store',
                        dest='pswd',
                        metavar='password',
                        help='Password')
    parser.add_argument('-s',
                        action='store',
                        dest='host',
                        metavar='hostname',
                        help='Hostname of host to submit job to')
    parser.add_argument('JCLFile',
                        action='store',
                        help='JCL file to submit')

    args = parser.parse_args()

    return args

def settleEnvs(args):
    global envEditor

    rc = True

    if args.user == None:
        user = os.getenv('ZOSUSER')
        if user != None and len(user) != 0:
            args.user = user
        else:
            print('User not sepcified and ZOSUSER not defined')
            rc = False

    if args.pswd == None:
        pswd = os.getenv('ZOSPSWD')
        if pswd != None and len(pswd) != 0:
            args.pswd = pswd
        else:
            print('Password not sepcified and ZOSPSWD not defined')
            rc = False

    if args.host == None:
        host = os.getenv('ZOSHOST')
        if host != None and len(host) != 0:
            args.host = host
        else:
            print('Host not sepcified and ZOSHOST not defined')
            rc = False

    if args.edit:
        envEditor = os.getenv('ZOSEDITOR')
        if envEditor == None or len(envEditor) == 0:
            envEditor = 'x'

    return rc

def debugOpts(args):
    if args.debug:
        print('Options:')
        print('\tedit    ' + str(args.edit))
        print('\twait    ' + str(args.wait))
        print('\tuser    ' + str(args.user))
        print('\tpswd    ' + str(args.pswd))
        print('\thost    ' + str(args.host))
        print('\tJCLFile ' + args.JCLFile)
        print()

class Error(Exception):
    pass

class SubmitError(Error):
    def __init__(self,
                 expression,
                 message):
        self.expression = expression
        self.message    = message

class Submit:
    def __init__(self,
                 debug=False,
                 edit=False,
                 wait=5,
                 user=None,
                 pswd=None,
                 host=None,
                 editor=None,
                 JCLFile=None):
        self.debug   = debug
        self.edit    = edit
        self.wait    = wait
        self.user    = user
        self.pswd    = pswd
        self.host    = host
        self.editor  = editor
        self.JCLFile = JCLFile
        self.jesLevelRe   = re.compile(r'JESINTERFACELEVEL is ([0-9])')
        self.jobNameRe    = re.compile(r'\/\/([a-zA-Z0-9$#@]+)\s+JOB')
        self.jobIDRe      = re.compile(r'It is known to JES as ([a-zA-Z0-9$#@]+)')
        self.userRe       = re.compile(r'\?user\?')
        self.pswdRe       = re.compile(r'\?pswd\?')
        self.includeRe    = re.compile(r'\?([^\?]+)\?')
        self.endRe        = re.compile(r'^\/\/ *$')
        self.jesStatus1Re = re.compile(r'^\s*(\S+)\s+(\S+)\s+(\S+)\s*')
        self.jesStatus2Re = re.compile(r'^\s*(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+')

    def processJob(self):
        if not os.path.exists('listings'):
            os.mkdir('listings')

        self.openFTP()

        self.jesLevel()

        tmpFileName = self.buildJob()

        jobID = self.sendFTP(tmpFileName)

        listing = self.getJobListing(jobID)

        listingFileName = self.saveListing(jobID,
                                           listing)

        if self.edit:
            self.editListing(listingFileName)

        self.closeFTP(jobID)

        self.cleanup(tmpFileName)

    def openFTP(self):
            self.ftp = FTP(host=self.host,
                           user=self.user,
                           passwd=self.pswd)

            self.ftp.sendcmd('site filetype=jes')

    def jesLevel(self):
        stat = self.ftp.sendcmd('stat')

        jesLevelMatch = self.jesLevelRe.search(stat)

        if jesLevelMatch == None:
            raise SubmitError('jesLevelInterface',
                              'JESINTERFACELEVEL not found via stat command')

        self.jesLevelInterface = jesLevelMatch.group(1)

        if self.debug:
            print('jesLevel: ' + self.jesLevelInterface)

    def buildJob(self):
        try:
            jobNameFound = False

            jclCards = self.getFile(self.JCLFile)

            jclCards = self.fixJobName(jclCards)

            jclCards = self.repUserPswd(jclCards)

            jclCards = self.includes(jclCards)

            tmpFileName = self.createJCLFile(jclCards)

            if self.debug:
                print('buildJob: ' + tmpFileName)

        except OSError as osError:
            raise SubmitError('buildJob',
                              'Problem opening JCLFile ' + self.JCLFile + '(' + str(osError) + ')')

        return tmpFileName

    def getFile(self,
                fileName):
        file = open(fileName, 'r')

        lines = file.readlines()

        file.close()

        lines = ''.join(lines)
        
        return lines

    def fixJobName(self,
                   jclCards):
        jobNameMatch = self.jobNameRe.search(jclCards)

        if jobNameMatch != None:
            if self.jesLevelInterface == '1':
                self.jobName = self.user + '0'
            else:
                self.jobName = jobNameMatch.group(1)

            jclCards = jclCards.replace(jobNameMatch.group(1),
                                        self.jobName,
                                        1)
            if self.debug:
                print('fixJobName:' + self.jobName)
        else:
            raise SubmitError('buildJob',
                              'Job card not found in ' + self.JCLFile)

        return jclCards

    def repUserPswd(self,
                    jclCards):
        jclCards = jclCards.replace('?user?',
                                    self.user,
                                    1)

        jclCards = jclCards.replace('?pswd?',
                                    self.pswd,
                                    1)
        return jclCards

    def includes(self,
                 jclCards):
        while True:
            includeMatch = self.includeRe.search(jclCards)

            if includeMatch != None:
                includeCards = self.getFile(includeMatch.group(1))

                jclCards = jclCards.replace(includeMatch.group(0) + "\n",
                                            includeCards,
                                            1)
            else:
                break

        return jclCards

    def createJCLFile(self,
                      jclCards):
        tmpFileName = 'listings/' + os.path.basename(self.JCLFile) + '.' + str(multiprocessing.current_process().pid)

        tmpFile = open(tmpFileName, 'w')

        for jclCard in jclCards.split(sep='\n'):
            tmpFile.write(jclCard + '\n')

            if re.search(r'^\/\/ *$', jclCard) != None:
                break

        tmpFile.close()

        return tmpFileName

    def sendFTP(self,
                tmpFileName):
        zosMessage = self.ftp.storlines('stor ' + os.path.basename(tmpFileName), open(tmpFileName, 'rb'))

        return self.getJobID(zosMessage)

    def getJobID(self,
                 zosMessage):
        jobIDMatch = self.jobIDRe.search(zosMessage)

        if jobIDMatch == None:
            raise SubmitError('getJobID',
                              'Job ID not found in: ' + zosMessage)

        jobID = jobIDMatch.group(1)

        print('Job ' + self.JCLFile + ' submitted for execution on ' + self.host + ' as ' + jobID)

        return jobID

    def getJobListing(self,
                      jobID):
        notFound = True
        heldOutList = []
        listingList = []
        
        while notFound:
            time.sleep(int(self.wait))

            # These are closures: it has access to heldOutList which is in the current scope
            #
            def ftpDir(line):
                heldOutList.append(line)

            def ftpRetr(line):
                listingList.append(line)

            self.ftp.dir(ftpDir)

            for heldOut in heldOutList:
                if self.jesLevelInterface == '1':
                    jesStatus1Match = self.jesStatus1Re.match(heldOut)

                    job, jobid, state = jesStatus1Match.group(1, 2, 3)
                else:
                    jesStatus2Match = self.jesStatus2Re.match(heldOut)

                    job, jobid, owner, state = jesStatus2Match.group(1, 2, 3, 4)

                if jobid == jobID:
                    if state == 'OUTPUT':
                        self.ftp.retrlines('RETR ' + jobid, ftpRetr)

                        listing = '\n'.join(listingList)

                        notFound = False

                        break
        return listing

    def saveListing(self,
                    jobID,
                    listing):
        listingFileName = 'listings/' + os.path.basename(self.JCLFile) + '.' + jobID

        listingFile = open(listingFileName, 'w')

        listingFile.write(listing.encode(encoding="ascii", errors="replace").decode())

        listingFile.close()

        print('Job output for ' + self.JCLFile + ' is in ' + listingFileName)

        return listingFileName

    def editListing(self,
                    listingFileName):
        completed = subprocess.run([self.editor, listingFileName])

    def closeFTP(self,
                 jobID):
        self.ftp.delete(jobID)

        self.ftp.close()

    def cleanup(self,
                tmpFileName):
        completed = subprocess.run(['rm', tmpFileName])

def main():
    try:
        args = opts()

        if settleEnvs(args):
            debugOpts(args)

            submit = Submit(debug=args.debug,
                            edit=args.edit,
                            wait=args.wait,
                            user=args.user,
                            pswd=args.pswd,
                            host=args.host,
                            editor=envEditor,
                            JCLFile=args.JCLFile)
    
            submit.processJob()
    except SubmitError as submitError:
        print('SubmitError: ' + submitError.expression + ' - ' + submitError.message)

if __name__ == '__main__':
    main()
