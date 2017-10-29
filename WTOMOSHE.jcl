//PM55ASM   JOB  CLASS=A,NOTIFY=&SYSUID,MSGCLASS=X,MSGLEVEL=(1,0)
//         SET  STU=&SYSUID               /* DATA SET HLQ = USERID   */
//***********************************************************
//**  THIS ASSEMBLER PROGRAM OUTPUTS A VARIBLE TEXT OT CONSOLE
//**  WHICH IS USEFUL WHEN YOU WANT TO OUTPUT A DECIMAL NUMBER
//**  TO THE CONSOLE. THIS COULD OF COURSE BE TURNED INTO A MACRO
//***********************************************************
//S0010    EXEC PROC=ASMACLG
//C.SYSLIB DD
//         DD   DISP=SHR,DSN=&STU..COPY.BOOKS
//G.OUTDD    DD SYSOUT=*,RECFM=FBA,LRECL=133
//C.SYSIN DD *
         PRINT GEN
TESTWTO  CSECT
         SAVE   (14,12)
         BASR   R12,0
         USING  *,R12
         ST     R13,SAVEA+4
         LA     R13,SAVEA
         LA     R5,23               ARBITRARY VALUE FOR R5 OF 10
         CVD    R5,NUMDEC
         UNPK   UNDEC,NUMDEC+1
         MVC    WTOLIST+20(8),UNDEC  WTO MACRO EXPANDS TO 24 BYTES
WTOLIST  WTO    'REG R5 VLUE XXXXXXXX'
         L      R13,SAVEA+4
         RETURN (14,12),RC=0           RETURN TO CALLER (Z/OS)
         LTORG
SAVEA    DS 18F
NUMDEC   DS D
UNDEC    DS PL4
R0        EQU 0
R1        EQU 1
R2        EQU 2
R3        EQU 3
R4        EQU 4
R5        EQU 5
R6        EQU 6
R7        EQU 7
R8        EQU 8
R9        EQU 9
R10       EQU 10
R11       EQU 11
R12       EQU 12
R13       EQU 13
R14       EQU 14
R15       EQU 15
          END
/*
//SYSUDUMP DD SYSOUT=*,RECFM=FBA,LRECL=133
//G.SYSUDUMP DD SYSOUT=*,RECFM=FBA,LRECL=133
