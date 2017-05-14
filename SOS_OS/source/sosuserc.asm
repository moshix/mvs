         TITLE 'Sample Operating System Version 2.00: Demo Program'     00010000
*********************************************************************** 00020000
*                                                                     * 00030000
*  *****************************************************************  * 00040000
*  *                                                               *  * 00050000
*  *                       Sample Operating System                 *  * 00060000
*  *                             Version 2.00                      *  * 00070000
*  *                        Developed at MIT 1973                  *  * 00080000
*  *                                                               *  * 00090000
*  *****************************************************************  * 00100000
*                                                                     * 00110000
*  Title: Demo user program for use with the Sample Operating System  * 00120000
*                                                                     * 00130000
*  Function:                                                          * 00140000
*                                                                     * 00150000
*   - print logo                                                      * 00160000
*   - read card containing message to print                           * 00170000
*   - format entry point to six hex digits and place                  * 00180000
*     it in columnes 54-59 of the message read                        * 00190000
*   - print message                                                   * 00200000
*   - format start message and parameter prompt for console at 009    * 00201002
*   - setup console CCWs                                              * 00202002
*   - display start message and parameter prompt on console           * 00203002
*   - wait for parameter input                                        * 00204002
*   - print parameters entered                                        * 00205002
*   - format end message and display it on console                    * 00206002
*   - exit                                                            * 00210000
*                                                                     * 00220000
*********************************************************************** 00230000
         PUNCH '$JOB,2K,READER=IN,PRINTER=OUT,CONSOLE=EXCP' job card    00240002
SOSUSERC CSECT ,              begin of program                          00248002
         BALR  R15,0          establish addressability ..               00256002
         USING *,R15            .. and tell assembler                   00264002
         LA    R2,PRNTLOGO    send logo ..                              00272002
         SVC   C'S'             .. to printer                           00290000
         MVC   WAITMSG+8(4),WAITLNG initialize reply length             00300002
         LA    R2,WAITMSG     wait for reply indicating ..              00303002
         SVC   C'R'             .. printing is completed                00310000
         LA    R2,READCARD    read card ..                              00320002
         SVC   C'S'             .. containing next message              00330000
         MVC   WAITMSG+8(4),WAITLNG initialize reply length             00340002
         LA    R2,WAITMSG     wait for reply indicating ..              00343002
         SVC   C'R'             .. card has been read                   00350000
         LR    R2,R15         base address ..                           00360002
         S     R2,ENTRY         .. minus two ..                         00367002
         ST    R2,ENTRY           .. is entry address                   00374002
         UNPK  ENTRYU(7),ENTRY+1(4) unpack address                      00390000
         TR    ENTRYU(6),HEXTAB translate to hex                        00400000
         MVC   LINE+53(6),ENTRYU move address into message              00410000
         LA    R2,PRINTMSG    send message ..                           00420002
         SVC   C'S'             .. to printer                           00430000
         MVC   WAITMSG+8(4),WAITLNG initialize reply length             00440002
         LA    R2,WAITMSG     wait for reply indicating ..              00443002
         SVC   C'R'             .. printing is completed                00450000
         MVC   STREAMNO(1),LINE+25 move job info ..                     00450202
         MVC   CNSENTRY(6),LINE+53   .. into console prompt             00450402
         MVI   LINE,C' '      blank ..                                  00450602
         MVC   LINE+1(131),LINE .. print line                           00450802
         LA    R2,CNSPRMPT    console prompt address                    00451002
         ICM   R2,B'1000',WRITE insert write command                    00451202
         ST    R2,CCW1        store CCW                                 00451402
         LA    R2,LCNPRMPT    length of console prompt                  00451602
         ST    R2,CCW1+4      store length in CCW, zero all flags       00451802
         OI    CCW1+4,X'40'   indicate command chaining                 00452002
         LA    R2,CNSINPUT    address of console input area             00452202
         ICM   R2,B'1000',READ insert read command                      00452402
         ST    R2,CCW2        store CCW                                 00452602
         LA    R2,LCNSINPT    length of console input area              00452802
         ST    R2,CCW2+4      store length in CCW, zero all flags       00453002
         OI    CCW2+4,X'20'   suppress length indication                00453202
         LA    2,CONSOLIO     send prompt ..                            00453402
         SVC   C'S'             .. to console                           00453602
         MVC   WAITMSG+8(4),WAITLNG initialize reply length             00453802
         LA    2,WAITMSG      wait for reply indicating ..              00454002
         SVC   C'R'             .. console I/O is completed             00454202
         LA    2,CONSOLOK     release ..                                00454402
         SVC   C'S'             .. console                              00454602
         LA    R2,LPE+LCNSINPT-1 total length of print message ..       00454802
         SH    R2,WAITMSG+18    .. minus residual count minus one       00455002
         EX    R2,GETINPUT    move to print line                        00455202
         LA    R2,PRINTMSG    send message ..                           00455402
         SVC   C'S'             .. to printer                           00455602
         MVC   WAITMSG+8(4),WAITLNG initialize reply length             00455802
         LA    R2,WAITMSG     wait for reply indicating ..              00456002
         SVC   C'R'             .. printing is completed                00456202
         MVC   CNSPRMPT+4(7),ENDED make it a termination message        00456402
         LA    R2,CNSPRMPT    console termination message address       00456602
         ICM   R2,B'1000',WRITEACR insert write command                 00456802
         ST    R2,CCW1        store CCW                                 00457002
         LA    R2,23          length of console prompt                  00457202
         ST    R2,CCW1+4      store length in CCW, zero all flags       00457402
         LA    2,CONSOLIO     send prompt ..                            00457602
         SVC   C'S'             .. to console                           00457802
         MVC   WAITMSG+8(4),WAITLNG initialize reply length             00458002
         LA    2,WAITMSG      wait for reply indicating ..              00458202
         SVC   C'R'             .. console I/O is completed             00458402
         LA    2,CONSOLOK     release ..                                00458602
         SVC   C'S'             .. console                              00458802
         SVC   C'H'           halt job                                  00460000
GETINPUT MVC   LINE(1),PE     move output message to print line (EX'ed) 00460502
READ     DC    X'0A'          console read                              00461002
WRITE    DC    X'01'          console write                             00461502
WRITEACR DC    X'09'          console write with auto CR                00462002
CCW1     DS    D              console write CCW                         00462502
CCW2     DS    D              console read CCW                          00463002
PE       DC    C'Parameters entered on console: ' echo parameters       00463502
LPE      EQU   *-PE           length of echo message                    00464002
CNSINPUT DC    40C' '         input from console                        00464502
LCNSINPT EQU   *-CNSINPUT     length of console input area              00465002
CNSPRMPT DC    C'Job started on stream-' prompt to ..                   00465502
STREAMNO DC    C' '                               .. be     ..          00466002
         DC    C', user program entry point = '  .. issued ..           00466502
CNSENTRY DC    C'      '                        .. on     ..            00467002
         DC    C', enter parameters: '         .. console               00467502
LCNPRMPT EQU   *-CNSPRMPT     length of console prompt                  00468002
ENDED    DC    CL7'ended'     convert started to ended message          00468502
         DS    0F             align parameter lists                     00470000
PRNTLOGO DC    CL8'PRINTER'   print a line: process name ..             00480002
         DC    F'8'             .. length of command ..                 00486002
         DC    C'PRIN',A(LOGO)    .. command                            00492002
LOGO     DC    CL132'--- Sample Operating System Version 2.00 ---' logo 00498002
PRINTMSG DC    CL8'PRINTER'   print a line: process name ..             00504002
         DC    F'8'             .. length of command ..                 00510002
         DC    C'PRIN',A(LINE)    .. command                            00516002
LINE     DC    132C' '        line to be printed or card read           00522002
WAITLNG  DC    F'8'           maximum length of reply to be received    00528002
WAITMSG  DS    CL8            wait for msg: originator returned here    00534002
         DS    F                .. length of return area ..             00540002
         DS    CL8                .. message received                   00546002
READCARD DC    CL8'READER'    read a card: process name ..              00552002
         DC    F'8'             .. length of command ..                 00558002
         DC    C'READ',A(LINE)    .. command                            00564002
CONSOLIO DC    CL8'CONSOLE'   perform console I/O: process name ..      00570002
         DC    F'12'            .. length of command ..                 00576002
         DC    C'EXCP',X'00000009',A(CCW1) .. command                   00582002
CONSOLOK DC    CL8'CONSOLE'   release console: process name ..          00588002
         DC    F'2'           .. length of command ..                   00594002
         DC    C'OK'            .. command                              00600002
ENTRY    DC    F'2'           entry address will end up here            00620000
         DC    X'00'          food for UNPK                             00630000
ENTRYU   DC    7X'00'         unpacked entry point plus excess byte     00640000
         ORG   *-240          first 240 bytes of HEXTAB are not needed  00650000
HEXTAB   DS    0X             translation table for unpacked to hex     00660000
         ORG   ,              restore current location                  00670000
         DC    C'0123456789ABCDEF' hex characters                       00680000
R2       EQU   2              register 2                                00690002
R15      EQU   15             register 15                               00690502
         END   ,              end of program                            00691002
