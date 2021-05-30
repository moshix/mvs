//HERC01X  JOB  (HERC01),                                               00000100
//             'MODIFY QEDIT EX ',                                      00000200
//             CLASS=A,                                                 00000300
//             MSGCLASS=A,COND=(4,LT),                                  00000401
//             REGION=4M,                                               00000500
//             MSGLEVEL=(1,1)                                           00000600
//********************************************************************* 00000700
//*                                                                     00000800
//PIC1   EXEC ASMFCG,PARM.ASM=(OBJ,NODECK),PARM.GO='/MODPGM2',          00000900
//            MAC1='SYS1.AMODGEN',                                      00001000
//            MAC2='SYS1.MACLIB'                                        00001100
//ASM.SYSIN DD *                                                        00001200
MODPGM2  START 0                                                        00001300
*                                                                       00001400
*  WHERE I GOT MY CODE EXAMPLE:                                         00001500
*                                                                       00001600
*  HTTPS://WWW.IBM.COM/DOCS/EN/ZOS/2.2.0?TOPIC=CONTROL-COMMUNICATING-   00001700
*  PROGRAM-EXTRACT-QEDIT                                                00001800
*                                                                       00001900
R0       EQU   0                                                        00002000
R1       EQU   1                                                        00002100
R2       EQU   2                                                        00002200
R3       EQU   3                                                        00002300
R4       EQU   4                                                        00002400
R5       EQU   5                                                        00002500
R6       EQU   6                                                        00002600
R7       EQU   7                                                        00002700
R8       EQU   8                                                        00002800
R9       EQU   9                                                        00002900
R10      EQU   10                                                       00003000
R11      EQU   11                                                       00003100
R12      EQU   12                                                       00003200
R13      EQU   13                                                       00003300
R14      EQU   14                                                       00003400
R15      EQU   15                                                       00003500
         SPACE 3                                                        00003600
         DS    0H                                                       00003700
         STM   R14,R12,12(R13)       SAVE CALLERS REGISTERS             00003800
         BALR  R12,0                 COPY BASE REGISTER                 00003900
         USING *,R12                 TELL ASMBLR BASE REGISTER          00004000
         ST    R13,SAVEAREA+4        SAVE CALLERS SAP                   00004100
         LA    R13,SAVEAREA          POINT TO OUR SAVE AREA             00004200
         LR    R4,R1                 SAVE PARM POINTER                  00004300
         SPACE 3                                                        00004400
         LA    R9,COMADDR         GET COMMUNICATIONS AREA               00004500
*                                 ADDRESS AT COMADDR                    00004600
*------------------------------------------------------------------*    00004700
*        OBTAIN ADDRESS OF THE CIB                                 *    00004800
*------------------------------------------------------------------*    00004900
         EXTRACT (R9),FIELDS=COMM,MF=(E,EXTRACT)                        00005000
*                                 EXTRACT THE COMMUNICATIONS AREA       00005100
         L     R9,COMADDR         GET ADDRESS OF THE AREA               00005200
         USING COM,R9             USE R9 AS BASE ADDRESS OF COMM AREA   00005300
         ICM   R7,15,COMCIBPT     GET CIB ADDRESS FROM COM AREA         00005400
         BZ    NOCIB              NO CIB, TASK WAS NOT STARTED          00005500
         BAL   R14,DOCIB          PROCESS THE CIB                       00005600
NOCIB    DS    0H                                                       00005700
         QEDIT ORIGIN=COMCIBPT,CIBCTR=5   SET MODIFY LIMIT TO 5         00005800
         L     R1,COMECBPT        GET ADDRESS OF THE COMMUNICATION ECB  00005900
         O     R1,HIBITON         SET HIGH BIT - LAST ECB IN LIST       00006000
         ST    R1,MODECB          PUT ADDR OF MODIFY ECB IN LIST        00006100
*                                                                       00006200
*                                                                       00006300
*                                                                       00006400
WAIT     DS    0H                                                       00006500
         WAIT  1,ECBLIST=ECBS       WAIT FOR A MODIFY/STOP              00006600
*                                                                       00006700
*  WHEN POSTED HERE, A MODIFY OR STOP HAS BEEN ISSUED                   00006800
*                                                                       00006900
         ICM   R7,15,COMCIBPT     GET CIB ADDRESS FROM COM AREA         00007000
         USING CIB,R7             BASE CIB MAPPING                      00007100
         CLI   CIBVERB,CIBMODFY   WAS IT A MODIFY?                      00007200
         BNE   NOTMDFY            NO, GO FREE CIB                       00007300
         BAL   R14,DOCIB          IT WAS A MODIFY, GO PROCESS COMMAND   00007400
*                                                                       00007500
*                                                                       00007600
*                                                                       00007700
*------------------------------------------------------------------ *   00007800
*        FREE THE CIB                                               *   00007900
*------------------------------------------------------------------ *   00008000
NOTMDFY  DS    0H                                                       00008100
         BAL   R14,DELCIB         FREE CIB                              00008200
         CLI   CIBVERB,CIBSTOP    WAS IT A STOP?                        00008300
         BE    EXITRTN            BRANCH TO ROUTINE HANDLING ERRORS     00008400
         B     WAIT               WAIT FOR ANOTHER MODIFY               00008500
*        .                                                              00008600
EXITRTN  DS    0H                                                       00008700
         WTO 'BYE BYE...'                                               00008800
         L     R13,SAVEAREA+4       RELOAD CALLERS SAP                  00008900
         LM    R14,R12,12(R13)      RELOAD CALLERS REGISTERS            00009000
         SR    R15,R15              ZERO RETURN CODE                    00009100
         BR    R14                  RETURN TO CALLER                    00009200
         LTORG                                                          00009300
SAVEAREA DS    18F                                                      00009400
DELCIB   DS    0H                                                       00009500
*        USE QEDIT TO FREE THE CIB                                      00009600
*        QEDIT WILL ALSO CLEAR THE ECB                                  00009700
*                                                                       00009800
         QEDIT ORIGIN=COMCIBPT,BLOCK=(R7)   FREE THE CIB                00009900
         BR    R14                                                      00010000
DOCIB    DS    0H                                                       00010100
*        WTO 'OUTPUT FROM MODIFY',ROUTCDE=(2,11)                        00010200
*                                                                       00010300
         LH    R1,CIBDATLN    LENGTH                                    00010400
         BCTR  R1,R0          DECREMENT FOR EX MOVE                     00010500
         EX    R1,TARGET      EXECUTE MOVE                              00010600
         LA    R2,MSGTAB                                                00010700
         L     R1,0(,2)                                                 00010800
         WTO   MF=(E,(1))                                               00010900
         BR 14                                                          00011000
TARGET   MVC   MSG1A(0),CIBDATA                                         00011100
*-------------------------------------------------------------------*   00011200
* YOUR ROUTINE TO HANDLE CIB PROCESSING WOULD GO HERE.              *   00011300
*-------------------------------------------------------------------*   00011400
*                                                                       00011500
*                                                                       00011600
*                                                                       00011700
*-------------------------------------------------------------------*   00011800
*  CONSTANTS                                                        *   00011900
*-------------------------------------------------------------------*   00012000
         DS    0F                 FULLWORD ALIGNMENT                    00012100
HIBITON  DC    X'80000000'        USED TO TURN HIGH ORDER BIT ON        00012200
MSGTAB   DC    A(MSG1)                                                  00012300
MSG1     WTO   ('                                                '),   X00012400
               ROUTCDE=(2),DESC=(7),MF=L                                00012500
MSG1A    EQU   MSG1+4   OFFSET OVER LEN AND DES CODES                   00012600
*-------------------------------------------------------------------*   00012700
*  FIELDS REQUIRED IN DYNAMIC STORAGE                               *   00012800
*-------------------------------------------------------------------*   00012900
         DS    0F                 FULLWORD ALIGNMENT OR S201 ABEND      00013000
ECBS     DS    0CL4               ECB LIST FOR WAIT                     00013100
MODECB   DS    A                  ADDR(MODIFY/STOP ECB)                 00013200
STIMECB  DS    A                  ADDR(STIMER ECB)                      00013300
COMADDR  DS    F                  ADDR(COMAREA) FROM EXTRACT            00013400
EXTRACT  EXTRACT MF=L             EXTRACT PARAMETER LIST                00013500
*-------------------------------------------------------------------*   00013600
*  REQUIRED DSECTS                                                  *   00013700
*-------------------------------------------------------------------*   00013800
COM      DSECT                                                          00013900
         IEZCOM   ,                COM AREA                             00014000
CIB      DSECT                                                          00014100
         IEZCIB   ,                CIB                                  00014200
         END                                                            00014300
//GO.SYSPRINT DD SYSOUT=*                                               00014400
//GO.SYSUDUMP DD SYSOUT=*                                               00014500
