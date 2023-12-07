//COBASM5  JOB (COBOL),'CALL ASM FROM COBOL',MSGLEVEL=(1,1)
//      CLASS=A, MSGCLASS=A,REGION=8M,TIME=1440,    
//ASM   EXEC ASMFC,PARM.ASM=(OBJ,NODECK,NOESD,LIST,XREF)
//ASM.SYSIN DD *
         PRINT GEN
WTOCONS  TITLE 'CALL ASM FROM COBOL'
         ENTRY WTOCONS          ENTRY POINTS
WTOCONS  CSECT ,
         SAVE  (14,12),,*       SAVE REGISTERS                          00001400
         LR    R12,R15          ESTABLISH MODULE ADDRESSABILITY         00001500
         USING WTOCONS,R12      TELL ASSEMBLER OF BASE                  00001600
         LA    R2,SAVEA         CHAIN ..                                00001700
         ST    R13,4(,R2)         .. THE ..                             00001800
         ST    R2,8(,R13)           .. SAVE ..                          00001900
         LR    R13,R2                 .. AREAS                          00002000
* START OF PROGRAM LOGIC
         WTO   'HELLO WORLD FROM ASM'
*                                                                       00008200
* RETURN TO CALLER                                                      00008300
*                                                                       00008400
RETURN   L     R13,4(,R13)      CALLER'S SAVE AREA POINTER              00008500
         RETURN (14,12),RC=0    RESTORE REGISTERS AND RETURN            00008600
* DATA AREA                                                             00008800
*                                                                       00008900
SAVEA    DS    18F              SAVE AREA                               00009000
R0       EQU   0                REGISTER  0                             00009500
R1       EQU   1                REGISTER  1                             00009600
R2       EQU   2                REGISTER  2                             00009700
R3       EQU   3                REGISTER  3                             00009800
R4       EQU   4                REGISTER  4                             00009900
R5       EQU   5                REGISTER  5                             00010000
R6       EQU   6                REGISTER  6                             00010100
R7       EQU   7                REGISTER  7                             00010200
R8       EQU   8                REGISTER  8                             00010300
R9       EQU   9                REGISTER  9                             00010400
R10      EQU   10               REGISTER 10                             00010500
R11      EQU   11               REGISTER 11                             00010600
R12      EQU   12               REGISTER 12                             00010700
R13      EQU   13               REGISTER 13                             00010800
R14      EQU   14               REGISTER 14                             00010900
R15      EQU   15               REGISTER 15                             00011000
         END   WTOCONS          END OF PROGRAM                          00011100
/*
//ASM.SYSGO DD UNIT=VIO,SPACE=(800,(1,1)),DISP=(,PASS),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800),DSN=&&WTOCONS
//COB2ASM  EXEC COBUCG,
//         PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'
//COB.SYSPUNCH DD DUMMY
//COB.SYSIN    DD *
  100 * //   COBOL TO ASM
  110 * //
  120 * //
  130 * //
  150 ***
  160 ***
  170 ***
  180  IDENTIFICATION DIVISION.
  190  PROGRAM-ID.  'COB2AS'.
  200 ***
  210 ***
  220 ***
  230  ENVIRONMENT DIVISION.
  240 **
  250 **
  260  CONFIGURATION SECTION.
  270  SOURCE-COMPUTER.  IBM-370.
  280  OBJECT-COMPUTER.  IBM-370.
  300 **
  380  DATA DIVISION.
  390 **
  400 **
  410  PROCEDURE DIVISION.
  420      DISPLAY 'HELLO FROM COBOL'
  430      CALL 'WTOCONS'
  440      STOP RUN.
/*
//COB.SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR
//GO.SYSLIN   DD
//            DD DSN=&&WTOCONS,DISP=(OLD,DELETE)
//GO.SYSOUT   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)
//
