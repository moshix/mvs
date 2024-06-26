//HERC01U  JOB (BAL),
//             'Ackerman function',
//             CLASS=A,
//             MSGCLASS=H,
//             TIME=1440,
//             MSGLEVEL=(1,1)
//SYSLIB      DD DSN=SYS2.MACLIB,DSN=SHR
//            DD DSN=SYS1.MACLIB,DSN=SHR
//********************************************************************
//*   THIS IS AN IMPLEMENTATION OF THE ACKERMAN FUNCTION
//*   IT'S A NEAT WAY TO STRESS TEST YOUR SYSTEM SINCE THIS CAN 
//*   VERY EASILY TAKE HOURS OR DAYS TO COMPUTE ON REAL HARDWARE
//********************************************************************
//ULAM  EXEC ASMFCG,PARM.ASM=(OBJ,NODECK),MAC1='SYS2.MACLIB',
//             REGION.GO=328K,PARM.GO='/1000'
//ASM.SYSIN DD *
         PRINT GEN
         MACRO 
&LAB     XDECO  &REG,&TARGET
&LAB     B      I&SYSNDX           branch around work area
P&SYSNDX DS     0D,PL8             packed
W&SYSNDX DS     CL13               char
I&SYSNDX CVD    &REG,P&SYSNDX          convert to decimal
         MVC    W&SYSNDX,=X'40202020202020202020212060'  nice mask
         EDMK   W&SYSNDX,P&SYSNDX+2    edit and mark
         BCTR   R1,0                   locate the right place
         MVC    0(1,R1),W&SYSNDX+12    move the sign
         MVC    &TARGET.(12),W&SYSNDX  move to target
         MEND
*
*
ACKERMAN CSECT
         USING  ACKERMAN,R12       r12 : base register
         LR     R12,R15            establish base register
         ST     R14,SAVER14A       save r14
         LA     R4,0               m=0
PROPEN   OPEN (SYSPRINT,OUTPUT)
LOOPM    CH     R4,=H'3'           do m=0 to 3
         BH     ELOOPM
         LA     R5,0               n=0
LOOPN    CH     R5,=H'8'           do n=0 to 8         
         BH     ELOOPN
         LR     R1,R4              m
         LR     R2,R5              n
         BAL    R14,ACKER          r1=acker(m,n)
         XDECO  R1,PG+19
         XDECO  R4,XD
         MVC    PG+10(2),XD+10
         XDECO  R5,XD
         MVC    PG+13(2),XD+10
         PUT    SYSPRINT,PG        XPRNT  PG,44
         LA     R5,1(R5)           n=n+1
         B      LOOPN
ELOOPN   LA     R4,1(R4)           m=m+1
         B      LOOPM
ELOOPM   L      R14,SAVER14A       restore r14
         BR     R14                return to caller
SAVER14A DS     F                  static save r14
PG       DC     CL44'Ackermann(xx,xx) = xxxxxxxxxxxx'
XD       DS     CL12
ACKER    CNOP   0,4                function r1=acker(r1,r2)
         LR     R3,R1              save argument r1 in r3
         LR     R9,R10             save stackptr (r10) in r9 temp
         LA     R1,STACKLEN        amount of storage required
         GETMAIN RU,LV=(R1)        allocate storage for stack
         USING  STACK,R10          make storage addressable
         LR     R10,R1             establish stack addressability
         ST     R14,SAVER14B       save previous r14
         ST     R9,SAVER10B        save previous r10
         LR     R1,R3              restore saved argument r1
START    ST     R1,M               stack m
         ST     R2,N               stack n
IF1      C      R1,=F'0'           if m<>0
         BNE    IF2                then goto if2
         LR     R11,R2             n
         LA     R11,1(R11)         return n+1
         B      EXIT
IF2      C      R2,=F'0'           else if m<>0
         BNE    IF3                then goto if3
         BCTR   R1,0               m=m-1
         LA     R2,1               n=1
         BAL    R14,ACKER          r1=acker(m)
         LR     R11,R1             return acker(m-1,1)
         B      EXIT
IF3      BCTR   R2,0               n=n-1
         BAL    R14,ACKER          r1=acker(m,n-1)
         LR     R2,R1              acker(m,n-1)
         L      R1,M               m
         BCTR   R1,0               m=m-1         
         BAL    R14,ACKER          r1=acker(m-1,acker(m,n-1))
         LR     R11,R1             return acker(m-1,1)
EXIT     L      R14,SAVER14B       restore r14
         L      R9,SAVER10B        restore r10 temp
         LA     R0,STACKLEN        amount of storage to free
*         FREEMAIN A=(R10),LV=(R0)  free allocated storage
         LR     R1,R11             value returned
         LR     R10,R9             restore r10
         BR     R14                return to caller
         LTORG
         DROP   R12                base no longer needed
SYSPRINT DCB MACRF=PM,DDNAME=SYSPRINT,DSORG=PS,LRECL=121,RECFM=FBA
DEC      DS     XL8                cvd work area
STACK    DSECT                     dynamic area
SAVER14B DS     F                  saved r14
SAVER10B DS     F                  saved r10
M        DS     F                  m
N        DS     F                  n
STACKLEN EQU    *-STACK
         YREGS  
         END    ACKERMAN
/*
//SYSPRINT    DD SYSOUT=H
//GO.OUTDD    DD SYSOUT=H
//
