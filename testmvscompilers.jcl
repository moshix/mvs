//NLTLIB JOB 'S322-0C4','WFJM',
//      USER=HERC01,PASSWORD=CUL8TR,
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CREDS  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=A
//SYSUT2   DD  DSNAME=HERC01.LTLIB,
//      UNIT=3350,VOL=SER=PUB000,
//      SPACE=(80,(2000,200,15)),DISP=(NEW,CATLG),
//      DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//SYSIN    DD  DATA,DLM='@@'
./        ADD   NAME=$$$INDEX,LEVEL=00,SOURCE=0,LIST=ALL
//********************************************************************
//*
//*  Name: HERC01.LTLIB($$$INDEX)
//*
//*  Desc: Short index of PDS with MVS 3.8j language test jobs
//*  
//*  For full information consult GitHub project wfjm/mvs38j-langtest
//*    https://github.com/wfjm/mvs38j-langtest/blob/master/README.md
//*
//*  The index simply gives the relation between the 7 character
//*  PDS member names and the jcl file names of the project.
//*  Member names as well as file name are composed of
//*    Case ID      - test case identifier (hewo,sine,....)
//*    Compiler ID  - compiler identifier  (a60,asm,....)
//*    Job type     - t --> test jobs
//*                   f --> benchmark jobs
//*                   p --> print benchmark jobs (for soep/soeq)
//*
//********************************************************************
Membername Short description                                          x
---------- ------------------------------------------------------------
HEWO --- section --- The classical 'Hello Word'
HEWOA60    hewo_a60.jcl
HEWOASM    hewo_asm.jcl
HEWOCOB    hewo_cob.jcl
HEWOFOG    hewo_forg.jcl
HEWOFOH    hewo_forh.jcl
HEWOFOW    hewo_forw.jcl
HEWOGCC    hewo_gcc.jcl
HEWOJCC    hewo_jcc.jcl
HEWOPAS    hewo_pas.jcl
HEWOPLI    hewo_pli.jcl
HEWOSIM    hewo_sim.jcl
SINE --- section --- Line printer plot of sine and cosine
SINEA60    sine_a60.jcl
SINEFOG    sine_forg.jcl
SINEFOH    sine_forh.jcl
SINEFOW    sine_forw.jcl
SINEGCC    sine_gcc.jcl
SINEJCC    sine_jcc.jcl
SINEPAS    sine_pas.jcl
SINEPLI    sine_pli.jcl
SINESIM    sine_sim.jcl
SOEP --- section --- Sieve of Eratosthenes prime search (byte)
SOEPA60    soep_a60_f.jcl
SOEPA60    soep_a60_p.jcl
SOEPA60    soep_a60_t.jcl
SOEPASM    soep_asm_f.jcl
SOEPASM    soep_asm_p.jcl
SOEPASM    soep_asm_t.jcl
SOEPGCC    soep_gcc_f.jcl
SOEPGCC    soep_gcc_p.jcl
SOEPGCC    soep_gcc_t.jcl
SOEPJCC    soep_jcc_f.jcl
SOEPJCC    soep_jcc_p.jcl
SOEPJCC    soep_jcc_t.jcl
SOEPFOG    soep_forg_f.jcl
SOEPFOG    soep_forg_p.jcl
SOEPFOG    soep_forg_t.jcl
SOEPFOH    soep_forh_f.jcl
SOEPFOH    soep_forh_p.jcl
SOEPFOH    soep_forh_t.jcl
SOEPFOW    soep_forw_f.jcl
SOEPFOW    soep_forw_p.jcl
SOEPFOW    soep_forw_t.jcl
SOEPPAS    soep_pas_f.jcl
SOEPPAS    soep_pas_p.jcl
SOEPPAS    soep_pas_t.jcl
SOEPPLI    soep_pli_f.jcl
SOEPPLI    soep_pli_p.jcl
SOEPPLI    soep_pli_t.jcl
SOEPSIM    soep_sim_f.jcl
SOEPSIM    soep_sim_p.jcl
SOEPSIM    soep_sim_t.jcl
SOEQ --- section --- Sieve of Eratosthenes prime search (bit)
SOEQASM    soeq_asm_f.jcl
SOEQASM    soeq_asm_p.jcl
SOEQASM    soeq_asm_t.jcl
SOEQGCC    soeq_gcc_f.jcl
SOEQGCC    soeq_gcc_p.jcl
SOEQGCC    soeq_gcc_t.jcl
SOEQJCC    soeq_jcc_f.jcl
SOEQJCC    soeq_jcc_p.jcl
SOEQJCC    soeq_jcc_t.jcl
SOEQPAS    soeq_pas_f.jcl
SOEQPAS    soeq_pas_p.jcl
SOEQPAS    soeq_pas_t.jcl
SOEQPLI    soeq_pli_f.jcl
SOEQPLI    soeq_pli_p.jcl
SOEQPLI    soeq_pli_t.jcl
TOWH --- section --- Tower of Hanoi solver
TOWHA60    towh_a60_f.jcl
TOWHA60    towh_a60_t.jcl
TOWHASM    towh_asm_f.jcl
TOWHASM    towh_asm_t.jcl
TOWHGCC    towh_gcc_f.jcl
TOWHGCC    towh_gcc_t.jcl
TOWHJCC    towh_jcc_f.jcl
TOWHJCC    towh_jcc_t.jcl
TOWHFOG    towh_forg_f.jcl
TOWHFOG    towh_forg_t.jcl
TOWHFOH    towh_forh_f.jcl
TOWHFOH    towh_forh_t.jcl
TOWHFOW    towh_forw_f.jcl
TOWHFOW    towh_forw_t.jcl
TOWHPAS    towh_pas_f.jcl
TOWHPAS    towh_pas_t.jcl
TOWHPLI    towh_pli_f.jcl
TOWHPLI    towh_pli_t.jcl
TOWHSIM    towh_sim_f.jcl
TOWHSIM    towh_sim_t.jcl
MCPI --- section --- Monte Carlo estimate of pi
MCPIA60    mcpi_a60_f.jcl
MCPIA60    mcpi_a60_t.jcl
MCPIASM    mcpi_asm_f.jcl
MCPIASM    mcpi_asm_t.jcl
MCPIGCC    mcpi_gcc_f.jcl
MCPIGCC    mcpi_gcc_t.jcl
MCPIJCC    mcpi_jcc_f.jcl
MCPIJCC    mcpi_jcc_t.jcl
MCPIFOG    mcpi_forg_f.jcl
MCPIFOG    mcpi_forg_t.jcl
MCPIFOH    mcpi_forh_f.jcl
MCPIFOH    mcpi_forh_t.jcl
MCPIFOW    mcpi_forw_f.jcl
MCPIFOW    mcpi_forw_t.jcl
MCPIPAS    mcpi_pas_f.jcl
MCPIPAS    mcpi_pas_t.jcl
MCPIPLI    mcpi_pli_f.jcl
MCPIPLI    mcpi_pli_t.jcl
MCPISIM    mcpi_sim_f.jcl
MCPISIM    mcpi_sim_t.jcl
./        ADD   NAME=HEWOA60,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
 OUTSTRING (1,'('Hello World !')');
'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOASM,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
        PRINT NOGEN              don't show macro expansions
HEWO     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING HEWO,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BNE   ABND8              abort if open failed
        PUT   SYSPRINT,MSG       write the message
        CLOSE SYSPRINT           close SYSPRINT
*
        L     R13,SAVE+4         get old save area back
        RETURN (14,12),RC=0      return to OS
*
ABND8    ABEND 8                  bail out with abend U008
*
* File and work area definitions
*
SAVE     DS    18F                local save area
MSG      DC    CL133' Hello World !'
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=1330
        YREGS ,
        END   HEWO               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOCOB,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#COB JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
// REGION=2048K,TIME=(1,0),PRTY=8
//CLG EXEC COBUCLG,
//      PARM.LKED='MAP,LIST,LET'
//COB.SYSIN DD *
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.     'HEWO'.
000300 ENVIRONMENT DIVISION.
001000 DATA DIVISION.
100000 PROCEDURE DIVISION.
100100 00-MAIN.
100500     DISPLAY 'Hello World !'.
100600     STOP RUN.
/*
//GO.SYSIN  DD * 
//GO.SYSOUT DD SYSOUT=*
/*
//
./        ADD   NAME=HEWOFOG,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C
C --- main program ---------------------------------------------------
C     PROGRAM HEWO
     WRITE(6,9000)
     STOP
C
9000 FORMAT(1X,'Hello World !')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOFOH,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C
C --- main program ---------------------------------------------------
C     PROGRAM HEWO
     WRITE(6,9000)
     STOP
C
9000 FORMAT(1X,'Hello World !')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOFOW,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           HEWO#FOW,T=(1,0),P=100,CHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C
C --- main program ---------------------------------------------------
C     PROGRAM HEWO
     WRITE(6,9000)
     STOP
C
9000 FORMAT(1X,'Hello World !')
C
     END
$ENTRY
$STOP
/*
//
./        ADD   NAME=HEWOGCC,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
#include <stdio.h>

int main () 
{
 printf ("Hello World !\n");
 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOJCC,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
#include <stdio.h>

int main () 
{
 printf ("Hello World !\n");
 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOPAS,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=1024K,
//      OPT='M+',
//      GOPARM='/STACK=512k'
//COMPILE.SYSIN DD *
program hewo(input,output);
begin
 writeln(' ','Hello World !');
end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOPLI,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
 HELLO: PROC OPTIONS(MAIN) REORDER;
   PUT SKIP LIST('Hello World !');
 END HELLO;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
/*
//
./        ADD   NAME=HEWOSIM,LEVEL=00,SOURCE=0,LIST=ALL
//HEWO#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
BEGIN
 OutText( "Hello World !" );
 OutImage;
 OutImage;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEA60,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: sine_a60.a60 978 2017-12-28 21:32:18Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-12-27   978   1.1    use outsymbol
*  2017-09-05   946   1.0    Initial version
*;

 'INTEGER' I,J,ISIN,ICOS;
 'REAL'    X,XRAD,FSIN,FCOS;
 'INTEGER' 'ARRAY' PLOT[1:81];

 'COMMENT' set record lenth = 132 and page length = 64;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 OUTSTRING (1,'('          x   sin(x)        cos(x)         ')');
 OUTSTRING (1,'('-1                -0.5                  0')');
 OUTSTRING (1,'('                 +0.5                 +1')');
 SYSACT(1,14,1);
 OUTSTRING (1,'('                                           ')');
 OUTSTRING (1,'('+-------------------.-------------------:')');
 OUTSTRING (1,'('-------------------.-------------------+')');
 SYSACT(1,14,1);

 'FOR' I := 0 'STEP' 1 'UNTIL' 60 'DO' 'BEGIN'
   X := 6.0 * I;
   XRAD := X/57.2957;
   FSIN := SIN(XRAD);
   FCOS := COS(XRAD);
   OUTINTEGER(1,6*I);
   OUTREAL(1,FSIN);
   OUTREAL(1,FCOS);

   'COMMENT' printer plot symbols: 1=+ 2=. 3=: 4=* 5=# 6=blank;
   'FOR' J := 1 'STEP' 1 'UNTIL' 81 'DO' PLOT[J] := 6;
   PLOT[ 1] := 1;
   PLOT[21] := 2;
   PLOT[41] := 3;
   PLOT[61] := 2;
   PLOT[81] := 1;
   ISIN := ENTIER(41.5 + 40.0 * FSIN);
   ICOS := ENTIER(41.5 + 40.0 * FCOS);
   PLOT[ISIN] := 4;
   PLOT[ICOS] := 5;
   'FOR' J := 1 'STEP' 1 'UNTIL' 81 'DO'
     OUTSYMBOL (1,'('+.:*# ')',PLOT[J]);
   SYSACT(1,14,1);
 'END';

 OUTSTRING (1,'('                                           ')');
 OUTSTRING (1,'('+-------------------.-------------------:')');
 OUTSTRING (1,'('-------------------.-------------------+')');
 SYSACT(1,14,1);

'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEFOG,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: sine_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SINE
     INTEGER PLOT(81)
     INTEGER I,J,ISIN,ICOS
     REAL*4 X,XRAD,FSIN,FCOS
     INTEGER CBL,CPL,CDO,CCO,CST,CHA
     DATA CBL/1H /,CPL/1H+/,CDO/1H./,CCO/1H:/,CST/1H*/,CHA/1H#/
C     
     WRITE(6,9000)
     WRITE(6,9010)
C
C Fortran IV(1966): DO limits must all to be > 0 -- FORTRAN-G enforces this
     DO 100 I=1,61
       X    = 6. * (I-1)
       XRAD = X/57.2957795131
       FSIN = SIN(XRAD)
       FCOS = COS(XRAD)
       DO 200 J=1,81
         PLOT(J) = CBL
200    CONTINUE
       PLOT( 1) = CPL
       PLOT(21) = CDO
       PLOT(41) = CCO
       PLOT(61) = CDO
       PLOT(81) = CPL
       ISIN = 41.5 + 40. * FSIN
       ICOS = 41.5 + 40. * FCOS
       PLOT(ISIN) = CST
       PLOT(ICOS) = CHA
       WRITE(6,9020) X,FSIN,FCOS,PLOT
100  CONTINUE
     WRITE(6,9010)
     STOP
C
9000 FORMAT(1X,'     x   sin(x)   cos(x)   ',
    *         '-1                -0.5                  0',
    *         '                 +0.5                 +1')
9010 FORMAT(1X,'                           ',
    *         '+-------------------.-------------------:',
    *         '-------------------.-------------------:')
9020 FORMAT(1X,F6.0,1X,F8.5,1X,F8.5,3X,81A1)
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEFOH,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: sine_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SINE
     INTEGER PLOT(81)
     INTEGER I,J,ISIN,ICOS
     REAL*4 X,XRAD,FSIN,FCOS
     INTEGER CBL,CPL,CDO,CCO,CST,CHA
     DATA CBL/1H /,CPL/1H+/,CDO/1H./,CCO/1H:/,CST/1H*/,CHA/1H#/
C     
     WRITE(6,9000)
     WRITE(6,9010)
C
C Fortran IV(1966): DO limits must all to be > 0 -- FORTRAN-G enforces this
     DO 100 I=1,61
       X    = 6. * (I-1)
       XRAD = X/57.2957795131
       FSIN = SIN(XRAD)
       FCOS = COS(XRAD)
       DO 200 J=1,81
         PLOT(J) = CBL
200    CONTINUE
       PLOT( 1) = CPL
       PLOT(21) = CDO
       PLOT(41) = CCO
       PLOT(61) = CDO
       PLOT(81) = CPL
       ISIN = 41.5 + 40. * FSIN
       ICOS = 41.5 + 40. * FCOS
       PLOT(ISIN) = CST
       PLOT(ICOS) = CHA
       WRITE(6,9020) X,FSIN,FCOS,PLOT
100  CONTINUE
     WRITE(6,9010)
     STOP
C
9000 FORMAT(1X,'     x   sin(x)   cos(x)   ',
    *         '-1                -0.5                  0',
    *         '                 +0.5                 +1')
9010 FORMAT(1X,'                           ',
    *         '+-------------------.-------------------:',
    *         '-------------------.-------------------:')
9020 FORMAT(1X,F6.0,1X,F8.5,1X,F8.5,3X,81A1)
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEFOW,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           SINE#FOW,T=(1,0),P=100,CHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: sine_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SINE
     INTEGER PLOT(81)
     INTEGER I,J,ISIN,ICOS
     REAL*4 X,XRAD,FSIN,FCOS
     INTEGER CBL,CPL,CDO,CCO,CST,CHA
     DATA CBL/1H /,CPL/1H+/,CDO/1H./,CCO/1H:/,CST/1H*/,CHA/1H#/
C     
     WRITE(6,9000)
     WRITE(6,9010)
C
C Fortran IV(1966): DO limits must all to be > 0 -- FORTRAN-G enforces this
     DO 100 I=1,61
       X    = 6. * (I-1)
       XRAD = X/57.2957795131
       FSIN = SIN(XRAD)
       FCOS = COS(XRAD)
       DO 200 J=1,81
         PLOT(J) = CBL
200    CONTINUE
       PLOT( 1) = CPL
       PLOT(21) = CDO
       PLOT(41) = CCO
       PLOT(61) = CDO
       PLOT(81) = CPL
       ISIN = 41.5 + 40. * FSIN
       ICOS = 41.5 + 40. * FCOS
       PLOT(ISIN) = CST
       PLOT(ICOS) = CHA
       WRITE(6,9020) X,FSIN,FCOS,PLOT
100  CONTINUE
     WRITE(6,9010)
     STOP
C
9000 FORMAT(1X,'     x   sin(x)   cos(x)   ',
    *         '-1                -0.5                  0',
    *         '                 +0.5                 +1')
9010 FORMAT(1X,'                           ',
    *         '+-------------------.-------------------:',
    *         '-------------------.-------------------:')
9020 FORMAT(1X,F6.0,1X,F8.5,1X,F8.5,3X,81A1)
C
     END
$ENTRY
$STOP
/*
//
./        ADD   NAME=SINEGCC,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: sine_cc.c 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-07-30   931   0.1    Initial version                         */

#include <stdio.h>
#include <math.h>

int main () 
{
 char  plot[82];
 int i,j,isin,icos;
 double x,xrad,fsin,fcos;
 char* f1 = "     x   sin(x)   cos(x)   "
            "-1                -0.5                  0"
            "                 +0.5                 +1";
 char* f2 = "                           "
            "+-------------------.-------------------:"
            "-------------------.-------------------:";

 plot[81] = 0;
 printf ("%s\n",f1);
 printf ("%s\n",f2);

 for (i=0; i<=60; i++) {
   x    = 6. * i;
   xrad = x/57.2957795131;
   fsin = sin(xrad);
   fcos = cos(xrad);
   for (j=0; j<81; j++) plot[j] = ' ';
   plot[ 0] = '+';
   plot[20] = '.';
   plot[40] = ':';
   plot[60] = '.';
   plot[80] = '+';
   isin = 40.5 + 40. * fsin;
   icos = 40.5 + 40. * fcos;
   plot[isin] = '*';
   plot[icos] = '#';
   printf("%6.0f %8.5f %8.5f   %s\n", x,fsin,fcos,plot);
 }
 printf ("%s\n",f2);
 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEJCC,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: sine_cc.c 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-07-30   931   0.1    Initial version                         */

#include <stdio.h>
#include <math.h>

int main () 
{
 char  plot[82];
 int i,j,isin,icos;
 double x,xrad,fsin,fcos;
 char* f1 = "     x   sin(x)   cos(x)   "
            "-1                -0.5                  0"
            "                 +0.5                 +1";
 char* f2 = "                           "
            "+-------------------.-------------------:"
            "-------------------.-------------------:";

 plot[81] = 0;
 printf ("%s\n",f1);
 printf ("%s\n",f2);

 for (i=0; i<=60; i++) {
   x    = 6. * i;
   xrad = x/57.2957795131;
   fsin = sin(xrad);
   fcos = cos(xrad);
   for (j=0; j<81; j++) plot[j] = ' ';
   plot[ 0] = '+';
   plot[20] = '.';
   plot[40] = ':';
   plot[60] = '.';
   plot[80] = '+';
   isin = 40.5 + 40. * fsin;
   icos = 40.5 + 40. * fcos;
   plot[isin] = '*';
   plot[icos] = '#';
   printf("%6.0f %8.5f %8.5f   %s\n", x,fsin,fcos,plot);
 }
 printf ("%s\n",f2);
 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEPAS,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=1024K,
//      OPT='M+',
//      GOPARM='/STACK=512k'
//COMPILE.SYSIN DD *
(* $Id: sine_pas.pas 964 2017-11-19 08:47:46Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-09-08   949   1.0    Initial version                         *)

program sine(input,output);
var
  i,j,isin,icos    : integer;
  x,xrad,fsin,fcos : real;
  plot             : ARRAY[1 .. 81] of char;

begin

  writeln(' ','     x   sin(x)   cos(x)   ',
              '-1                -0.5                  0',
              '                 +0.5                 +1');
  writeln(' ', '                           ',
               '+-------------------.-------------------:',
                '-------------------.-------------------+');

  for i := 0 to 60 do begin
     x := 6.0 * i;
     xrad := x/57.2957795131;
     fsin := sin(xrad);
     fcos := cos(xrad);
     for j := 1 to 81 do plot[j] := ' ';
     plot[ 1] := '+';
     plot[21] := '.';
     plot[41] := ':';
     plot[61] := '.';
     plot[81] := '+';
     isin := trunc(41.5 + 40.0 * fsin);
     icos := trunc(41.5 + 40.0 * fcos);
     plot[isin] := '*';
     plot[icos] := '#';
     write(' ',x:6:1,fsin:9:5,fcos:9:5,'   ');
     for j := 1 to 81 do write(plot[j]:1);
     writeln(' ');
  end;
  writeln(' ', '                           ',
               '+-------------------.-------------------:',
                '-------------------.-------------------+');
end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINEPLI,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: sine_pli.pli 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-09-07   947   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

 SINE: PROC OPTIONS(MAIN) REORDER;
   DCL (I,ISIN,ICOS) BIN FIXED(31);
   DCL (X,XRAD,FSIN,FCOS) DEC FLOAT(6);
   DCL PLOT CHAR(81);

   PUT SKIP EDIT('     x   sin(x)   cos(x)   ',
                 '-1                -0.5                  0',
                 '                 +0.5                 +1')
                 (A,A,A);
   PUT SKIP EDIT('                           ',
                 '+-------------------.-------------------:',
                 '-------------------.-------------------+')
                 (A,A,A);

   DO I=0 TO 60;
     X = 6. * I;
     XRAD = X/57.2957795131;
     FSIN = SIN(XRAD);
     FCOS = COS(XRAD);
     PLOT = ' ';
     SUBSTR(PLOT, 1,1) = '+';
     SUBSTR(PLOT,21,1) = '.';
     SUBSTR(PLOT,41,1) = ':';
     SUBSTR(PLOT,61,1) = '.';
     SUBSTR(PLOT,81,1) = '+';
     ISIN = 41.5 + 40. * FSIN;
     ICOS = 41.5 + 40. * FCOS;
     SUBSTR(PLOT,ISIN,1) = '*';
     SUBSTR(PLOT,ICOS,1) = '#';
     PUT SKIP EDIT (X,FSIN,FCOS,PLOT)
       (F(6,0),X(1),F(8,5),X(1),F(8,5),X(3),A);
   END;

   PUT SKIP EDIT('                           ',
                 '+-------------------.-------------------:',
                 '-------------------.-------------------+')
                 (A,A,A);

 END SINE;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SINESIM,LEVEL=00,SOURCE=0,LIST=ALL
//SINE#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-09-08   949   1.0    Initial version
*;

BEGIN
  INTEGER i,j,isin,icos;
  REAL x,xrad,fsin,fcos;
  CHARACTER ARRAY plot(1:81);

  OutText("     x   sin(x)   cos(x)   ");
  OutText("-1                -0.5                  0");
  OutText("                 +0.5                 +1");
  OutImage;
  OutText("                           ");
  OutText("+-------------------.-------------------:");
  OutText("-------------------.-------------------+");
  OutImage;

  FOR i := 0 STEP 1 UNTIL 60 DO BEGIN
     x := 6.0 * i;
     xrad := x/57.2957795131;
     fsin := sin(xrad);
     fcos := cos(xrad);
     FOR j := 1 STEP 1 UNTIL 81 DO plot(j) := ' ';
     plot( 1) := '+';
     plot(21) := '.';
     plot(41) := ':';
     plot(61) := '.';
     plot(81) := '+';
     isin := Entier(41.5 + 40.0 * fsin);
     icos := Entier(41.5 + 40.0 * fcos);
     plot(isin) := '*';
     plot(icos) := '#';
     OutFix(x,0,6);
     OutFix(fsin,5,9);
     OutFix(fcos,5,9);
     OutText("   ");
     FOR j := 1 STEP 1 UNTIL 81 DO OutChar(plot(j));
     OutImage;
  END;            
  OutText("                           ");
  OutText("+-------------------.-------------------:");
  OutText("-------------------.-------------------+");
  OutImage;
  OutImage;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
/*
//
./        ADD   NAME=SOEPA60F,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: soep_a60.a60 975 2017-12-25 19:22:43Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-12-25   975   1.2    use sqrt(nmax) as outer loop end
*  2017-12-23   972   1.1    change (n-1)/2 --> n'/'2
*                            use integer '/' divide
*  2017-09-17   951   1.0    Initial version
*  2017-09-05   946   0.1    First draft
*;

 'INTEGER' NMAX,PRNT,IMAX,NMSQRT;
 'INTEGER' I,N,IMIN;
 'INTEGER' NP,IL,NL;

 'COMMENT' set record lenth = 132 and page length = 64;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,NMAX);
 ININTEGER(0,PRNT);

 'IF' NMAX <= 10   'THEN' 'BEGIN'
   OUTSTRING (1,'('nmax must be >= 10, abort')');
   'GOTO' DONE;
 'END';

 NMSQRT := ENTIER(SQRT(NMAX));
 IMAX := (NMAX-1)'/'2;

 'BEGIN'
   'BOOLEAN' 'ARRAY' PRIME[0:IMAX];
   'FOR' I := 0 'STEP' 1 'UNTIL' IMAX 'DO' PRIME[I] := 'TRUE';

   'FOR' N := 3 'STEP' 2 'UNTIL' NMSQRT 'DO' 'BEGIN'
     'IF' PRIME[N'/'2]  'THEN' 'BEGIN'
       IMIN := (N*N) '/' 2;
       'FOR' I := IMIN 'STEP' N 'UNTIL' IMAX 'DO' PRIME[I] := 'FALSE';
     'END';
   'END';

   'IF' PRNT > 0  'THEN' 'BEGIN'
     OUTSTRING (1,'('List of Primes up to ')');
     OUTINTEGER(1, NMAX);
     SYSACT(1,14,1);
     OUTINTEGER(1, 2);
     NP := 1;
     'FOR' I := 1 'STEP' 1 'UNTIL' IMAX 'DO' 'BEGIN'
       'IF' PRIME[I] 'THEN' 'BEGIN'
         OUTINTEGER(1, 1+2*I);
         NP := NP + 1;
         'IF' NP = 10 'THEN' 'BEGIN'
           SYSACT(1,14,1);
           NP := 0;
         'END';
       'END';
     'END';
     'IF' NP > 0 'THEN' SYSACT(1,14,1);
   'END';

   IL :=  4;
   NL := 10;
   NP :=  1;
   'FOR' I := 1 'STEP' 1 'UNTIL' IMAX 'DO' 'BEGIN'
     'IF' PRIME[I] 'THEN' NP := NP+ 1;
     'IF' I = IL 'THEN' 'BEGIN'
       NL := 2*IL + 2;
       OUTSTRING (1,'('pi(')');
       OUTINTEGER(1, NL);
       OUTSTRING (1,'('):')');
       OUTINTEGER(1, NP);
       SYSACT(1,14,1);
       IL := 10*(IL+1)-1;
     'END';
   'END';

   'IF' NL < NMAX 'THEN' 'BEGIN'
     OUTSTRING (1,'('pi(')');
     OUTINTEGER(1, NMAX);
     OUTSTRING (1,'('):')');
     OUTINTEGER(1, NP);
     SYSACT(1,14,1);
   'END';

 'END';

 DONE:
'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPA60P,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: soep_a60.a60 975 2017-12-25 19:22:43Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-12-25   975   1.2    use sqrt(nmax) as outer loop end
*  2017-12-23   972   1.1    change (n-1)/2 --> n'/'2
*                            use integer '/' divide
*  2017-09-17   951   1.0    Initial version
*  2017-09-05   946   0.1    First draft
*;

 'INTEGER' NMAX,PRNT,IMAX,NMSQRT;
 'INTEGER' I,N,IMIN;
 'INTEGER' NP,IL,NL;

 'COMMENT' set record lenth = 132 and page length = 64;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,NMAX);
 ININTEGER(0,PRNT);

 'IF' NMAX <= 10   'THEN' 'BEGIN'
   OUTSTRING (1,'('nmax must be >= 10, abort')');
   'GOTO' DONE;
 'END';

 NMSQRT := ENTIER(SQRT(NMAX));
 IMAX := (NMAX-1)'/'2;

 'BEGIN'
   'BOOLEAN' 'ARRAY' PRIME[0:IMAX];
   'FOR' I := 0 'STEP' 1 'UNTIL' IMAX 'DO' PRIME[I] := 'TRUE';

   'FOR' N := 3 'STEP' 2 'UNTIL' NMSQRT 'DO' 'BEGIN'
     'IF' PRIME[N'/'2]  'THEN' 'BEGIN'
       IMIN := (N*N) '/' 2;
       'FOR' I := IMIN 'STEP' N 'UNTIL' IMAX 'DO' PRIME[I] := 'FALSE';
     'END';
   'END';

   'IF' PRNT > 0  'THEN' 'BEGIN'
     OUTSTRING (1,'('List of Primes up to ')');
     OUTINTEGER(1, NMAX);
     SYSACT(1,14,1);
     OUTINTEGER(1, 2);
     NP := 1;
     'FOR' I := 1 'STEP' 1 'UNTIL' IMAX 'DO' 'BEGIN'
       'IF' PRIME[I] 'THEN' 'BEGIN'
         OUTINTEGER(1, 1+2*I);
         NP := NP + 1;
         'IF' NP = 10 'THEN' 'BEGIN'
           SYSACT(1,14,1);
           NP := 0;
         'END';
       'END';
     'END';
     'IF' NP > 0 'THEN' SYSACT(1,14,1);
   'END';

   IL :=  4;
   NL := 10;
   NP :=  1;
   'FOR' I := 1 'STEP' 1 'UNTIL' IMAX 'DO' 'BEGIN'
     'IF' PRIME[I] 'THEN' NP := NP+ 1;
     'IF' I = IL 'THEN' 'BEGIN'
       NL := 2*IL + 2;
       OUTSTRING (1,'('pi(')');
       OUTINTEGER(1, NL);
       OUTSTRING (1,'('):')');
       OUTINTEGER(1, NP);
       SYSACT(1,14,1);
       IL := 10*(IL+1)-1;
     'END';
   'END';

   'IF' NL < NMAX 'THEN' 'BEGIN'
     OUTSTRING (1,'('pi(')');
     OUTINTEGER(1, NMAX);
     OUTSTRING (1,'('):')');
     OUTINTEGER(1, NP);
     SYSACT(1,14,1);
   'END';

 'END';

 DONE:
'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPA60T,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: soep_a60.a60 975 2017-12-25 19:22:43Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-12-25   975   1.2    use sqrt(nmax) as outer loop end
*  2017-12-23   972   1.1    change (n-1)/2 --> n'/'2
*                            use integer '/' divide
*  2017-09-17   951   1.0    Initial version
*  2017-09-05   946   0.1    First draft
*;

 'INTEGER' NMAX,PRNT,IMAX,NMSQRT;
 'INTEGER' I,N,IMIN;
 'INTEGER' NP,IL,NL;

 'COMMENT' set record lenth = 132 and page length = 64;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,NMAX);
 ININTEGER(0,PRNT);

 'IF' NMAX <= 10   'THEN' 'BEGIN'
   OUTSTRING (1,'('nmax must be >= 10, abort')');
   'GOTO' DONE;
 'END';

 NMSQRT := ENTIER(SQRT(NMAX));
 IMAX := (NMAX-1)'/'2;

 'BEGIN'
   'BOOLEAN' 'ARRAY' PRIME[0:IMAX];
   'FOR' I := 0 'STEP' 1 'UNTIL' IMAX 'DO' PRIME[I] := 'TRUE';

   'FOR' N := 3 'STEP' 2 'UNTIL' NMSQRT 'DO' 'BEGIN'
     'IF' PRIME[N'/'2]  'THEN' 'BEGIN'
       IMIN := (N*N) '/' 2;
       'FOR' I := IMIN 'STEP' N 'UNTIL' IMAX 'DO' PRIME[I] := 'FALSE';
     'END';
   'END';

   'IF' PRNT > 0  'THEN' 'BEGIN'
     OUTSTRING (1,'('List of Primes up to ')');
     OUTINTEGER(1, NMAX);
     SYSACT(1,14,1);
     OUTINTEGER(1, 2);
     NP := 1;
     'FOR' I := 1 'STEP' 1 'UNTIL' IMAX 'DO' 'BEGIN'
       'IF' PRIME[I] 'THEN' 'BEGIN'
         OUTINTEGER(1, 1+2*I);
         NP := NP + 1;
         'IF' NP = 10 'THEN' 'BEGIN'
           SYSACT(1,14,1);
           NP := 0;
         'END';
       'END';
     'END';
     'IF' NP > 0 'THEN' SYSACT(1,14,1);
   'END';

   IL :=  4;
   NL := 10;
   NP :=  1;
   'FOR' I := 1 'STEP' 1 'UNTIL' IMAX 'DO' 'BEGIN'
     'IF' PRIME[I] 'THEN' NP := NP+ 1;
     'IF' I = IL 'THEN' 'BEGIN'
       NL := 2*IL + 2;
       OUTSTRING (1,'('pi(')');
       OUTINTEGER(1, NL);
       OUTSTRING (1,'('):')');
       OUTINTEGER(1, NP);
       SYSACT(1,14,1);
       IL := 10*(IL+1)-1;
     'END';
   'END';

   'IF' NL < NMAX 'THEN' 'BEGIN'
     OUTSTRING (1,'('pi(')');
     OUTINTEGER(1, NMAX);
     OUTSTRING (1,'('):')');
     OUTINTEGER(1, NP);
     SYSACT(1,14,1);
   'END';

 'END';

 DONE:
'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPASMF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NOLIST,NOXREF,NORLD,NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: soep_asm.asm 972 2017-12-23 20:55:41Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-11-12   961   1.0    Initial version
* 2017-10-03   954   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  4  NMAX out of range
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get NMAX
        ST    R1,NMAX
        BAL   R14,IINT10         get PRNT
        STC   R1,PRNT
*
        L     R1,NMAX
        C     R1,=F'10'          is NMAX >= 10
        BL    NMAXBAD            if < not
        C     R1,=F'10000000'    is NMAX <= 10000000
        BNH   NMAXOK             if <= yes
NMAXBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
NMAXOK   EQU   *         
*
* setup phase ---------------------------------------------------------
*
*   calculate sqrt(nmax) -----------------------------------
*     use simple bi-section method
*       R4   low  bound
*       R5   high bound
*       R7   middle (low+high)/2
*
        LA    R4,1               set  low bound
        L     R5,NMAX            set high bound
        LA    R6,32              set iteration limit
NMSQRTLP LR    R7,R4              R7:= low
        AR    R7,R5              R7:= (low+high)
        SRA   R7,1               R7:= (low+high)/2
        LR    R3,R7
        MR    R2,R7              (R2,R3) := R7*R7
        LTR   R2,R2              more than 32 bit ?
        BNE   NMSQRTHI           if != yes, mid too high
        CL    R3,NMAX            mid*mid > NMAX
        BH    NMSQRTHI           if > yes, mid too high
        LR    R4,R7              here mid to  low:  low := mid
        B     NMSQRTGO
NMSQRTHI LR    R5,R7              here mid to high: high := mid
NMSQRTGO LR    R8,R5              R8 := high
        SR    R8,R4              R8 := high-low
        LR    R1,R6
        C     R8,=F'1'           spread <= 1 ?
        BNH   NMSQRTOK           if <= yes, quit
        BCT   R6,NMSQRTLP
        ABEND 99                 abort if doesn't converge
NMSQRTOK EQU   *
        ST    R4,NMSQRT

*   allocate PRIME array -----------------------------------
        L     R2,NMAX
        BCTR  R2,0               NMAX-1
        SRA   R2,1               (NMAX-1)/2
        ST    R2,IMAX
        LA    R5,1(R2)           IMAX+1  (24 bit enough)
        GETMAIN RU,LV=(5)        allocate storage for PRIME
        ST    R1,PRIME           store sieve base
        LR    R9,R1              R9 := PRIME base
*
*   set each PRIME array byte to X'01' ---------------------
        LR    R4,R1              R4 := PRIME
*                                 R5 := IMAX+1 (still)
        XR    R6,R6              R6 := 0
        L     R7,=X'01000000'    R7 := padding=1 and length=0
        MVCL  R4,R6              set all PRIME bytes to 1
*
* sieve phase ---------------------------------------------------------
*   outer loop:  ind  R6  n
*                inc  R4  2
*                lim  R5  sqrt(NMAX)
*   inner loop:  ind  R3  p
*                inc  R6  n
*                lim  R7  pmax
*                     R0,R1,R2    temporaries
*   register usage:
*     R0    temporary
*     R1    temporary
*     R2    temporary
*     R3    inner loop ind p
*     R4    outer loop inc 2
*     R5    outer loop lim sqrt(NMAX)
*     R6    inner loop inc n   (and outer loop ind !!)
*     R7    inner loop lim pmax
*     R8    -- unused --
*     R9    &prime
*     R10   -- unused --
*     R11   -- unused --
*
*
*   equivalent C code:
*     pmax  = &prime[imax];
*     for (n=3; n<=nmsqrt; n+=2) {
*       if (prime[(n-1)/2] == 0) continue;
*       for (p=&prime[(n*n-1)/2]; p<=pmax; p+=n) *p = 0;
*     }
*
        LA    R6,3               outer ind: R6:=3
        LA    R4,2               outer inc: R4:=2
        L     R5,NMSQRT          outer lim: R5:=NMSQRT
        LR    R7,R9                         R7:=&prime
        A     R7,IMAX            inner lim: R7:=&prime[imax]
SIEVO    LR    R2,R6              R2:=n
        SRA   R2,1               R2:=n/2
        AR    R2,R9              R2:=&prime[n/2]
        CLI   0(R2),X'00'        test prime candidate
        BE    SIEVOC             if = not, continue outer loop
*
        LR    R1,R6              R1:=n
        MR    R0,R6              R1:=n*n (lower half, enough)
        LR    R3,R1              R3:=n*n too
*
        SRA   R3,1               R3:=(n*n)/2
        AR    R3,R9              R3:=&prime[(n*n-1)/2]
*
SIEVI    MVI   0(R3),X'00'        *p=0
        BXLE  R3,R6,SIEVI
*
SIEVOC   BXLE  R6,R4,SIEVO
*
* print primes table --------------------------------------------------
*   loop:  ind  R3  i
*          inc  R4  1
*          lim  R5  imax
*               R2  np
*
        CLI   PRNT,X'00'         primes to be printed ?
        BE    NOPRNT             if = skip
        L     R1,MSGLIST         
        BAL   R14,OTEXT          print heading
        L     R1,NMAX            
        BAL   R14,OINT10         print nmax
        BAL   R14,OPUTLINE       write line
*
        LA    R1,2
        BAL   R14,OINT10         print "2"  (1st prime...)
        LA    R2,1               np=1
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,IMAX            lim: R5:=IMAX
PRTLOOP  LR    R6,R3              R6:=i
        AR    R6,R9              R6:=&primes[i]
        CLI   0(R6),X'00'        test whether prime
        BE    PRTLOOPC           if = not, continue
        LR    R1,R3              R1:=i
        SLA   R1,1               R1:=2*i
        LA    R1,1(R1)           R1:=1+2*i
        BAL   R14,OINT10         and print F(10)
        LA    R2,1(R2)           np+=1
        C     R2,=F'10'          check wheter = 10
        BNZ   PRTLOOPC           if != not, continue
        BAL   R14,OPUTLINE       write line
        XR    R2,R2              np=0
PRTLOOPC EQU   *
        BXLE  R3,R4,PRTLOOP
*
        LTR   R2,R2              check prime count np
        BZ    NOPRNT
        BAL   R14,OPUTLINE       write line
NOPRNT   EQU   *
*
* print primes count --------------------------------------------------
*   loop:  ind  R3  i
*          inc  R4  1
*          lim  R5  imax
*               R2  np
*               R7  il
*               R8  nl
*
        LA    R2,1               np=1
        LA    R7,4               il=4
        LA    R8,10              nl=10
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,IMAX            lim: R5:=IMAX
TBLLOOP  LR    R6,R3              R6:=i
        AR    R6,R9              R6:=&primes[i]
        CLI   0(R6),X'00'        test whether prime
        BE    NOPRIME            if = not
        LA    R2,1(R2)           np+= 1
NOPRIME  CR    R3,R7              test i != il
        BNE   TBLLOOPC
        LR    R8,R7              nl=il
        SLA   R8,1               nl=2*il
        LA    R8,2(R8)           nl=2+2*il
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        LR    R1,R8
        BAL   R14,OINT10         print nl
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
        LR    R1,R7              R1:=il
        LA    R1,1(R1)           R1:=il+1
        M     R0,=F'10'          R1:=10*(il+1)
        S     R1,=F'1'           R1:=10*(il+1)-1
        LR    R7,R1              update il
*
TBLLOOPC EQU   *
        BXLE  R3,R4,TBLLOOP
*
        C     R8,NMAX            is nl != nmax ?
        BE    TBLNOTR            if = not, skip extra summary
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
TBLNOTR  EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
NMAX     DC    F'10000000'        highest prime to search for
NMSQRT   DS    F                  sqrt(NMAX)
IMAX     DS    F                  highest prime array index
PRIME    DS    F                  prime array pointer
PRNT     DC    X'00'              print enable flag
*
* message strings
*
MSGPERR  OTXTDSC C'NMAX must be >= 10 and <= 10000000, abort'
MSGLIST  OTXTDSC C'List of Primes up to '
MSGPI    OTXTDSC C'pi('
MSGPISEP OTXTDSC C'): '
*
* spill literal pool
*
        LTORG
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPASMP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NOLIST,NOXREF,NORLD,NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: soep_asm.asm 972 2017-12-23 20:55:41Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-11-12   961   1.0    Initial version
* 2017-10-03   954   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  4  NMAX out of range
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get NMAX
        ST    R1,NMAX
        BAL   R14,IINT10         get PRNT
        STC   R1,PRNT
*
        L     R1,NMAX
        C     R1,=F'10'          is NMAX >= 10
        BL    NMAXBAD            if < not
        C     R1,=F'10000000'    is NMAX <= 10000000
        BNH   NMAXOK             if <= yes
NMAXBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
NMAXOK   EQU   *         
*
* setup phase ---------------------------------------------------------
*
*   calculate sqrt(nmax) -----------------------------------
*     use simple bi-section method
*       R4   low  bound
*       R5   high bound
*       R7   middle (low+high)/2
*
        LA    R4,1               set  low bound
        L     R5,NMAX            set high bound
        LA    R6,32              set iteration limit
NMSQRTLP LR    R7,R4              R7:= low
        AR    R7,R5              R7:= (low+high)
        SRA   R7,1               R7:= (low+high)/2
        LR    R3,R7
        MR    R2,R7              (R2,R3) := R7*R7
        LTR   R2,R2              more than 32 bit ?
        BNE   NMSQRTHI           if != yes, mid too high
        CL    R3,NMAX            mid*mid > NMAX
        BH    NMSQRTHI           if > yes, mid too high
        LR    R4,R7              here mid to  low:  low := mid
        B     NMSQRTGO
NMSQRTHI LR    R5,R7              here mid to high: high := mid
NMSQRTGO LR    R8,R5              R8 := high
        SR    R8,R4              R8 := high-low
        LR    R1,R6
        C     R8,=F'1'           spread <= 1 ?
        BNH   NMSQRTOK           if <= yes, quit
        BCT   R6,NMSQRTLP
        ABEND 99                 abort if doesn't converge
NMSQRTOK EQU   *
        ST    R4,NMSQRT

*   allocate PRIME array -----------------------------------
        L     R2,NMAX
        BCTR  R2,0               NMAX-1
        SRA   R2,1               (NMAX-1)/2
        ST    R2,IMAX
        LA    R5,1(R2)           IMAX+1  (24 bit enough)
        GETMAIN RU,LV=(5)        allocate storage for PRIME
        ST    R1,PRIME           store sieve base
        LR    R9,R1              R9 := PRIME base
*
*   set each PRIME array byte to X'01' ---------------------
        LR    R4,R1              R4 := PRIME
*                                 R5 := IMAX+1 (still)
        XR    R6,R6              R6 := 0
        L     R7,=X'01000000'    R7 := padding=1 and length=0
        MVCL  R4,R6              set all PRIME bytes to 1
*
* sieve phase ---------------------------------------------------------
*   outer loop:  ind  R6  n
*                inc  R4  2
*                lim  R5  sqrt(NMAX)
*   inner loop:  ind  R3  p
*                inc  R6  n
*                lim  R7  pmax
*                     R0,R1,R2    temporaries
*   register usage:
*     R0    temporary
*     R1    temporary
*     R2    temporary
*     R3    inner loop ind p
*     R4    outer loop inc 2
*     R5    outer loop lim sqrt(NMAX)
*     R6    inner loop inc n   (and outer loop ind !!)
*     R7    inner loop lim pmax
*     R8    -- unused --
*     R9    &prime
*     R10   -- unused --
*     R11   -- unused --
*
*
*   equivalent C code:
*     pmax  = &prime[imax];
*     for (n=3; n<=nmsqrt; n+=2) {
*       if (prime[(n-1)/2] == 0) continue;
*       for (p=&prime[(n*n-1)/2]; p<=pmax; p+=n) *p = 0;
*     }
*
        LA    R6,3               outer ind: R6:=3
        LA    R4,2               outer inc: R4:=2
        L     R5,NMSQRT          outer lim: R5:=NMSQRT
        LR    R7,R9                         R7:=&prime
        A     R7,IMAX            inner lim: R7:=&prime[imax]
SIEVO    LR    R2,R6              R2:=n
        SRA   R2,1               R2:=n/2
        AR    R2,R9              R2:=&prime[n/2]
        CLI   0(R2),X'00'        test prime candidate
        BE    SIEVOC             if = not, continue outer loop
*
        LR    R1,R6              R1:=n
        MR    R0,R6              R1:=n*n (lower half, enough)
        LR    R3,R1              R3:=n*n too
*
        SRA   R3,1               R3:=(n*n)/2
        AR    R3,R9              R3:=&prime[(n*n-1)/2]
*
SIEVI    MVI   0(R3),X'00'        *p=0
        BXLE  R3,R6,SIEVI
*
SIEVOC   BXLE  R6,R4,SIEVO
*
* print primes table --------------------------------------------------
*   loop:  ind  R3  i
*          inc  R4  1
*          lim  R5  imax
*               R2  np
*
        CLI   PRNT,X'00'         primes to be printed ?
        BE    NOPRNT             if = skip
        L     R1,MSGLIST         
        BAL   R14,OTEXT          print heading
        L     R1,NMAX            
        BAL   R14,OINT10         print nmax
        BAL   R14,OPUTLINE       write line
*
        LA    R1,2
        BAL   R14,OINT10         print "2"  (1st prime...)
        LA    R2,1               np=1
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,IMAX            lim: R5:=IMAX
PRTLOOP  LR    R6,R3              R6:=i
        AR    R6,R9              R6:=&primes[i]
        CLI   0(R6),X'00'        test whether prime
        BE    PRTLOOPC           if = not, continue
        LR    R1,R3              R1:=i
        SLA   R1,1               R1:=2*i
        LA    R1,1(R1)           R1:=1+2*i
        BAL   R14,OINT10         and print F(10)
        LA    R2,1(R2)           np+=1
        C     R2,=F'10'          check wheter = 10
        BNZ   PRTLOOPC           if != not, continue
        BAL   R14,OPUTLINE       write line
        XR    R2,R2              np=0
PRTLOOPC EQU   *
        BXLE  R3,R4,PRTLOOP
*
        LTR   R2,R2              check prime count np
        BZ    NOPRNT
        BAL   R14,OPUTLINE       write line
NOPRNT   EQU   *
*
* print primes count --------------------------------------------------
*   loop:  ind  R3  i
*          inc  R4  1
*          lim  R5  imax
*               R2  np
*               R7  il
*               R8  nl
*
        LA    R2,1               np=1
        LA    R7,4               il=4
        LA    R8,10              nl=10
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,IMAX            lim: R5:=IMAX
TBLLOOP  LR    R6,R3              R6:=i
        AR    R6,R9              R6:=&primes[i]
        CLI   0(R6),X'00'        test whether prime
        BE    NOPRIME            if = not
        LA    R2,1(R2)           np+= 1
NOPRIME  CR    R3,R7              test i != il
        BNE   TBLLOOPC
        LR    R8,R7              nl=il
        SLA   R8,1               nl=2*il
        LA    R8,2(R8)           nl=2+2*il
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        LR    R1,R8
        BAL   R14,OINT10         print nl
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
        LR    R1,R7              R1:=il
        LA    R1,1(R1)           R1:=il+1
        M     R0,=F'10'          R1:=10*(il+1)
        S     R1,=F'1'           R1:=10*(il+1)-1
        LR    R7,R1              update il
*
TBLLOOPC EQU   *
        BXLE  R3,R4,TBLLOOP
*
        C     R8,NMAX            is nl != nmax ?
        BE    TBLNOTR            if = not, skip extra summary
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
TBLNOTR  EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
NMAX     DC    F'10000000'        highest prime to search for
NMSQRT   DS    F                  sqrt(NMAX)
IMAX     DS    F                  highest prime array index
PRIME    DS    F                  prime array pointer
PRNT     DC    X'00'              print enable flag
*
* message strings
*
MSGPERR  OTXTDSC C'NMAX must be >= 10 and <= 10000000, abort'
MSGLIST  OTXTDSC C'List of Primes up to '
MSGPI    OTXTDSC C'pi('
MSGPISEP OTXTDSC C'): '
*
* spill literal pool
*
        LTORG
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPASMT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: soep_asm.asm 972 2017-12-23 20:55:41Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-11-12   961   1.0    Initial version
* 2017-10-03   954   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  4  NMAX out of range
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get NMAX
        ST    R1,NMAX
        BAL   R14,IINT10         get PRNT
        STC   R1,PRNT
*
        L     R1,NMAX
        C     R1,=F'10'          is NMAX >= 10
        BL    NMAXBAD            if < not
        C     R1,=F'10000000'    is NMAX <= 10000000
        BNH   NMAXOK             if <= yes
NMAXBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
NMAXOK   EQU   *         
*
* setup phase ---------------------------------------------------------
*
*   calculate sqrt(nmax) -----------------------------------
*     use simple bi-section method
*       R4   low  bound
*       R5   high bound
*       R7   middle (low+high)/2
*
        LA    R4,1               set  low bound
        L     R5,NMAX            set high bound
        LA    R6,32              set iteration limit
NMSQRTLP LR    R7,R4              R7:= low
        AR    R7,R5              R7:= (low+high)
        SRA   R7,1               R7:= (low+high)/2
        LR    R3,R7
        MR    R2,R7              (R2,R3) := R7*R7
        LTR   R2,R2              more than 32 bit ?
        BNE   NMSQRTHI           if != yes, mid too high
        CL    R3,NMAX            mid*mid > NMAX
        BH    NMSQRTHI           if > yes, mid too high
        LR    R4,R7              here mid to  low:  low := mid
        B     NMSQRTGO
NMSQRTHI LR    R5,R7              here mid to high: high := mid
NMSQRTGO LR    R8,R5              R8 := high
        SR    R8,R4              R8 := high-low
        LR    R1,R6
        C     R8,=F'1'           spread <= 1 ?
        BNH   NMSQRTOK           if <= yes, quit
        BCT   R6,NMSQRTLP
        ABEND 99                 abort if doesn't converge
NMSQRTOK EQU   *
        ST    R4,NMSQRT

*   allocate PRIME array -----------------------------------
        L     R2,NMAX
        BCTR  R2,0               NMAX-1
        SRA   R2,1               (NMAX-1)/2
        ST    R2,IMAX
        LA    R5,1(R2)           IMAX+1  (24 bit enough)
        GETMAIN RU,LV=(5)        allocate storage for PRIME
        ST    R1,PRIME           store sieve base
        LR    R9,R1              R9 := PRIME base
*
*   set each PRIME array byte to X'01' ---------------------
        LR    R4,R1              R4 := PRIME
*                                 R5 := IMAX+1 (still)
        XR    R6,R6              R6 := 0
        L     R7,=X'01000000'    R7 := padding=1 and length=0
        MVCL  R4,R6              set all PRIME bytes to 1
*
* sieve phase ---------------------------------------------------------
*   outer loop:  ind  R6  n
*                inc  R4  2
*                lim  R5  sqrt(NMAX)
*   inner loop:  ind  R3  p
*                inc  R6  n
*                lim  R7  pmax
*                     R0,R1,R2    temporaries
*   register usage:
*     R0    temporary
*     R1    temporary
*     R2    temporary
*     R3    inner loop ind p
*     R4    outer loop inc 2
*     R5    outer loop lim sqrt(NMAX)
*     R6    inner loop inc n   (and outer loop ind !!)
*     R7    inner loop lim pmax
*     R8    -- unused --
*     R9    &prime
*     R10   -- unused --
*     R11   -- unused --
*
*
*   equivalent C code:
*     pmax  = &prime[imax];
*     for (n=3; n<=nmsqrt; n+=2) {
*       if (prime[(n-1)/2] == 0) continue;
*       for (p=&prime[(n*n-1)/2]; p<=pmax; p+=n) *p = 0;
*     }
*
        LA    R6,3               outer ind: R6:=3
        LA    R4,2               outer inc: R4:=2
        L     R5,NMSQRT          outer lim: R5:=NMSQRT
        LR    R7,R9                         R7:=&prime
        A     R7,IMAX            inner lim: R7:=&prime[imax]
SIEVO    LR    R2,R6              R2:=n
        SRA   R2,1               R2:=n/2
        AR    R2,R9              R2:=&prime[n/2]
        CLI   0(R2),X'00'        test prime candidate
        BE    SIEVOC             if = not, continue outer loop
*
        LR    R1,R6              R1:=n
        MR    R0,R6              R1:=n*n (lower half, enough)
        LR    R3,R1              R3:=n*n too
*
        SRA   R3,1               R3:=(n*n)/2
        AR    R3,R9              R3:=&prime[(n*n-1)/2]
*
SIEVI    MVI   0(R3),X'00'        *p=0
        BXLE  R3,R6,SIEVI
*
SIEVOC   BXLE  R6,R4,SIEVO
*
* print primes table --------------------------------------------------
*   loop:  ind  R3  i
*          inc  R4  1
*          lim  R5  imax
*               R2  np
*
        CLI   PRNT,X'00'         primes to be printed ?
        BE    NOPRNT             if = skip
        L     R1,MSGLIST         
        BAL   R14,OTEXT          print heading
        L     R1,NMAX            
        BAL   R14,OINT10         print nmax
        BAL   R14,OPUTLINE       write line
*
        LA    R1,2
        BAL   R14,OINT10         print "2"  (1st prime...)
        LA    R2,1               np=1
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,IMAX            lim: R5:=IMAX
PRTLOOP  LR    R6,R3              R6:=i
        AR    R6,R9              R6:=&primes[i]
        CLI   0(R6),X'00'        test whether prime
        BE    PRTLOOPC           if = not, continue
        LR    R1,R3              R1:=i
        SLA   R1,1               R1:=2*i
        LA    R1,1(R1)           R1:=1+2*i
        BAL   R14,OINT10         and print F(10)
        LA    R2,1(R2)           np+=1
        C     R2,=F'10'          check wheter = 10
        BNZ   PRTLOOPC           if != not, continue
        BAL   R14,OPUTLINE       write line
        XR    R2,R2              np=0
PRTLOOPC EQU   *
        BXLE  R3,R4,PRTLOOP
*
        LTR   R2,R2              check prime count np
        BZ    NOPRNT
        BAL   R14,OPUTLINE       write line
NOPRNT   EQU   *
*
* print primes count --------------------------------------------------
*   loop:  ind  R3  i
*          inc  R4  1
*          lim  R5  imax
*               R2  np
*               R7  il
*               R8  nl
*
        LA    R2,1               np=1
        LA    R7,4               il=4
        LA    R8,10              nl=10
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,IMAX            lim: R5:=IMAX
TBLLOOP  LR    R6,R3              R6:=i
        AR    R6,R9              R6:=&primes[i]
        CLI   0(R6),X'00'        test whether prime
        BE    NOPRIME            if = not
        LA    R2,1(R2)           np+= 1
NOPRIME  CR    R3,R7              test i != il
        BNE   TBLLOOPC
        LR    R8,R7              nl=il
        SLA   R8,1               nl=2*il
        LA    R8,2(R8)           nl=2+2*il
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        LR    R1,R8
        BAL   R14,OINT10         print nl
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
        LR    R1,R7              R1:=il
        LA    R1,1(R1)           R1:=il+1
        M     R0,=F'10'          R1:=10*(il+1)
        S     R1,=F'1'           R1:=10*(il+1)-1
        LR    R7,R1              update il
*
TBLLOOPC EQU   *
        BXLE  R3,R4,TBLLOOP
*
        C     R8,NMAX            is nl != nmax ?
        BE    TBLNOTR            if = not, skip extra summary
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
TBLNOTR  EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
NMAX     DC    F'10000000'        highest prime to search for
NMSQRT   DS    F                  sqrt(NMAX)
IMAX     DS    F                  highest prime array index
PRIME    DS    F                  prime array pointer
PRNT     DC    X'00'              print enable flag
*
* message strings
*
MSGPERR  OTXTDSC C'NMAX must be >= 10 and <= 10000000, abort'
MSGLIST  OTXTDSC C'List of Primes up to '
MSGPI    OTXTDSC C'pi('
MSGPISEP OTXTDSC C'): '
*
* spill literal pool
*
        LTORG
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPGCCF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: soep_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-10-15   956   1.0    Initial version                         */
/* 2017-08-17   941   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int imax;
 int i,n;
 int np,il,nl;
 char *prime;
 char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt  = sqrt((double)nmax);
 imax    = (nmax-1)/2;
 prime   = malloc(imax+1);              /* need [1,...,imax] */
 pmax    = &prime[imax];

 for (p=prime; p<=pmax;) *p++ = 1;

 for (n=3; n<=nmsqrt; n+=2) {    
   if (prime[n/2] == 0) continue;
   for (p=&prime[(n*n)/2]; p<=pmax; p+=n) *p = 0;
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=imax;i++) {
     if (! prime[i]) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=imax;i++) {
   if (prime[i]) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPGCCP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: soep_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-10-15   956   1.0    Initial version                         */
/* 2017-08-17   941   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int imax;
 int i,n;
 int np,il,nl;
 char *prime;
 char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt  = sqrt((double)nmax);
 imax    = (nmax-1)/2;
 prime   = malloc(imax+1);              /* need [1,...,imax] */
 pmax    = &prime[imax];

 for (p=prime; p<=pmax;) *p++ = 1;

 for (n=3; n<=nmsqrt; n+=2) {    
   if (prime[n/2] == 0) continue;
   for (p=&prime[(n*n)/2]; p<=pmax; p+=n) *p = 0;
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=imax;i++) {
     if (! prime[i]) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=imax;i++) {
   if (prime[i]) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPGCCT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: soep_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-10-15   956   1.0    Initial version                         */
/* 2017-08-17   941   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int imax;
 int i,n;
 int np,il,nl;
 char *prime;
 char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt  = sqrt((double)nmax);
 imax    = (nmax-1)/2;
 prime   = malloc(imax+1);              /* need [1,...,imax] */
 pmax    = &prime[imax];

 for (p=prime; p<=pmax;) *p++ = 1;

 for (n=3; n<=nmsqrt; n+=2) {    
   if (prime[n/2] == 0) continue;
   for (p=&prime[(n*n)/2]; p<=pmax; p+=n) *p = 0;
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=imax;i++) {
     if (! prime[i]) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=imax;i++) {
   if (prime[i]) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPJCCF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: soep_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-10-15   956   1.0    Initial version                         */
/* 2017-08-17   941   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int imax;
 int i,n;
 int np,il,nl;
 char *prime;
 char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt  = sqrt((double)nmax);
 imax    = (nmax-1)/2;
 prime   = malloc(imax+1);              /* need [1,...,imax] */
 pmax    = &prime[imax];

 for (p=prime; p<=pmax;) *p++ = 1;

 for (n=3; n<=nmsqrt; n+=2) {    
   if (prime[n/2] == 0) continue;
   for (p=&prime[(n*n)/2]; p<=pmax; p+=n) *p = 0;
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=imax;i++) {
     if (! prime[i]) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=imax;i++) {
   if (prime[i]) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPJCCP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: soep_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-10-15   956   1.0    Initial version                         */
/* 2017-08-17   941   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int imax;
 int i,n;
 int np,il,nl;
 char *prime;
 char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt  = sqrt((double)nmax);
 imax    = (nmax-1)/2;
 prime   = malloc(imax+1);              /* need [1,...,imax] */
 pmax    = &prime[imax];

 for (p=prime; p<=pmax;) *p++ = 1;

 for (n=3; n<=nmsqrt; n+=2) {    
   if (prime[n/2] == 0) continue;
   for (p=&prime[(n*n)/2]; p<=pmax; p+=n) *p = 0;
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=imax;i++) {
     if (! prime[i]) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=imax;i++) {
   if (prime[i]) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=100000
//GO.STDERR DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPJCCT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: soep_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-10-15   956   1.0    Initial version                         */
/* 2017-08-17   941   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int imax;
 int i,n;
 int np,il,nl;
 char *prime;
 char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt  = sqrt((double)nmax);
 imax    = (nmax-1)/2;
 prime   = malloc(imax+1);              /* need [1,...,imax] */
 pmax    = &prime[imax];

 for (p=prime; p<=pmax;) *p++ = 1;

 for (n=3; n<=nmsqrt; n+=2) {    
   if (prime[n/2] == 0) continue;
   for (p=&prime[(n*n)/2]; p<=pmax; p+=n) *p = 0;
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=imax;i++) {
     if (! prime[i]) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=imax;i++) {
   if (prime[i]) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPFOGF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPFOGP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPFOGT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPFOHF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPFOHP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPFOHT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPFOWF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           SOEP#FOW,T=(1,0),P=100,CHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
$ENTRY
 10000000         0
$STOP
/*
//
./        ADD   NAME=SOEPFOWP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           SOEP#FOW,T=(1,0),P=2000,NOCHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
$ENTRY
 10000000         1
$STOP
/*
//
./        ADD   NAME=SOEPFOWT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           SOEP#FOW,T=(1,0),P=100,NOCHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: soep_for.f 975 2017-12-25 19:22:43Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-25   975   1.1    use sqrt(nmax) as outer loop end
C 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
C 2017-09-17   951   1.0    Initial version
C 2017-08-26   942   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM SOEP
     INTEGER NMAX,PRNT,IMAX,NMSQRT
     INTEGER I,N,IMIN
     INTEGER NP,IL,NL
     INTEGER PLIST(10)
     LOGICAL*1 PRIME(5000000)
C
     READ(5,9000,ERR=910,END=900) NMAX,PRNT
     IF (NMAX .LT. 10 .OR. NMAX .GT. 10000000) GOTO 920
C
     NMSQRT = IFIX(SQRT(FLOAT(NMAX)))
     IMAX = (NMAX-1)/2
     DO 100 I=1,IMAX
       PRIME(I) = .TRUE.
100  CONTINUE
C
     DO 300 N=3,NMSQRT,2
       IF (.NOT. PRIME(N/2)) GOTO 300
       IMIN = (N*N)/2
       DO 200 I=IMIN,IMAX,N
         PRIME(I) = .FALSE.
200    CONTINUE
300  CONTINUE
C
     IF (PRNT .EQ. 0) GOTO 500
     WRITE(6,9010) NMAX
     PLIST(1) = 2
     NP = 1
     DO 400 I=1,IMAX
       IF (.NOT. PRIME(I)) GOTO 400
       NP = NP + 1
       PLIST(NP) = 1+2*I
       IF (NP .LT. 10) GOTO 400
       WRITE(6,9020) PLIST
       NP = 0
400  CONTINUE
     IF (NP .NE. 0) WRITE(6,9020) (PLIST(I),I=1,NP)
500  CONTINUE
C
     IL = 4
     NL = 10
     NP = 1
     DO 600 I=1,IMAX
       IF (PRIME(I)) NP = NP + 1
       IF (I .NE. IL) GOTO 650
       NL = 2*IL+2
       WRITE(6,9030) NL,NP
       IL = 10*(IL+1)-1
650    CONTINUE
600  CONTINUE
     IF (NL .NE. NMAX) WRITE(6,9030) NMAX,NP
C
900  CONTINUE
     STOP
910  WRITE(6,9040)
     STOP
920  WRITE(6,9050)
     STOP
C
9000 FORMAT(2I10)
9010 FORMAT(1X,'List of Primes up to',I8)
9020 FORMAT(10(1X,I7))
9030 FORMAT(1X,'pi(',I8,'): ',I8)
9040 FORMAT(1X,'conversion error, abort')
9050 FORMAT(1X,'nmax out of range (10...10000000), abort')
C
     END
$ENTRY
   100000         1
$STOP
/*
//
./        ADD   NAME=SOEPPASF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6500K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=6500K,
//      OPT='M+,D-',
//      GOPARM='/STACK=5500K'
//COMPILE.SYSIN DD *
(* $Id: soep_pas.pas 975 2017-12-25 19:22:43Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        *)
(* 2017-12-25   974   1.1    5M sieve array                          *)
(* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  *)
(* 2017-09-07   948   1.0    Initial version                         *)

program soep(input,output);
var
  nmax,prnt,imax : integer;
  nmsqrt         : integer;
  i,n,imin       : integer;
  np,il,nl       : integer;
  rnmax          : real;
  sieve          : ARRAY[1 .. 5000000] of boolean;

begin

  read(nmax);
  read(prnt);

  if (nmax < 10) or (nmax > 10000000) then begin
     writeln(' ', 'nmax out of range (10...10000000), abort');
     exit(8);
  end;

  rnmax  := nmax;
  nmsqrt := trunc(sqrt(nmax));
  imax := (nmax-1) div 2;
  for i := 1 to imax do sieve[i] := TRUE;

  n    := 3;
  while n <= nmsqrt do begin
     if sieve[n div 2] then begin
        i := (n*n) div 2;
        while i <= imax do begin
           sieve[i] := FALSE;
           i := i + n;
        end;
     end;
     n := n + 2;
  end;

  if prnt > 0 then begin
     writeln(' ', 'List of Primes up to ', nmax:8);
     write(2:8);
     np := 1;
     for i := 1 to imax do begin
        if sieve[i] then begin
           write(1+2*i:8);
           np := np + 1;
           if np = 10 then begin
              writeln(' ');
              np := 0;
           end;
        end;
     end;
     if np > 0 then writeln();
  end;

  il :=  4;
  nl := 10;
  np :=  1;
  for i := 1 to imax do begin
     if sieve[i] then np := np + 1;
     if i = il then begin
        nl := 2*il + 2;
        writeln(' ', 'pi(', nl:8, '): ', np:8);
        il := 10*(il+1)-1;
     end;
  end;

  if nl < nmax then writeln(' ', 'pi(', nmax:8, '): ', np:8);

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPPASP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6500K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=6500K,
//      OPT='M+,D-',
//      GOPARM='/STACK=5500K'
//COMPILE.SYSIN DD *
(* $Id: soep_pas.pas 975 2017-12-25 19:22:43Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        *)
(* 2017-12-25   974   1.1    5M sieve array                          *)
(* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  *)
(* 2017-09-07   948   1.0    Initial version                         *)

program soep(input,output);
var
  nmax,prnt,imax : integer;
  nmsqrt         : integer;
  i,n,imin       : integer;
  np,il,nl       : integer;
  rnmax          : real;
  sieve          : ARRAY[1 .. 5000000] of boolean;

begin

  read(nmax);
  read(prnt);

  if (nmax < 10) or (nmax > 10000000) then begin
     writeln(' ', 'nmax out of range (10...10000000), abort');
     exit(8);
  end;

  rnmax  := nmax;
  nmsqrt := trunc(sqrt(nmax));
  imax := (nmax-1) div 2;
  for i := 1 to imax do sieve[i] := TRUE;

  n    := 3;
  while n <= nmsqrt do begin
     if sieve[n div 2] then begin
        i := (n*n) div 2;
        while i <= imax do begin
           sieve[i] := FALSE;
           i := i + n;
        end;
     end;
     n := n + 2;
  end;

  if prnt > 0 then begin
     writeln(' ', 'List of Primes up to ', nmax:8);
     write(2:8);
     np := 1;
     for i := 1 to imax do begin
        if sieve[i] then begin
           write(1+2*i:8);
           np := np + 1;
           if np = 10 then begin
              writeln(' ');
              np := 0;
           end;
        end;
     end;
     if np > 0 then writeln();
  end;

  il :=  4;
  nl := 10;
  np :=  1;
  for i := 1 to imax do begin
     if sieve[i] then np := np + 1;
     if i = il then begin
        nl := 2*il + 2;
        writeln(' ', 'pi(', nl:8, '): ', np:8);
        il := 10*(il+1)-1;
     end;
  end;

  if nl < nmax then writeln(' ', 'pi(', nmax:8, '): ', np:8);

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPPAST,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6500K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=6500K,
//      OPT='M+',
//      GOPARM='/STACK=5500K'
//COMPILE.SYSIN DD *
(* $Id: soep_pas.pas 975 2017-12-25 19:22:43Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        *)
(* 2017-12-25   974   1.1    5M sieve array                          *)
(* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  *)
(* 2017-09-07   948   1.0    Initial version                         *)

program soep(input,output);
var
  nmax,prnt,imax : integer;
  nmsqrt         : integer;
  i,n,imin       : integer;
  np,il,nl       : integer;
  rnmax          : real;
  sieve          : ARRAY[1 .. 5000000] of boolean;

begin

  read(nmax);
  read(prnt);

  if (nmax < 10) or (nmax > 10000000) then begin
     writeln(' ', 'nmax out of range (10...10000000), abort');
     exit(8);
  end;

  rnmax  := nmax;
  nmsqrt := trunc(sqrt(nmax));
  imax := (nmax-1) div 2;
  for i := 1 to imax do sieve[i] := TRUE;

  n    := 3;
  while n <= nmsqrt do begin
     if sieve[n div 2] then begin
        i := (n*n) div 2;
        while i <= imax do begin
           sieve[i] := FALSE;
           i := i + n;
        end;
     end;
     n := n + 2;
  end;

  if prnt > 0 then begin
     writeln(' ', 'List of Primes up to ', nmax:8);
     write(2:8);
     np := 1;
     for i := 1 to imax do begin
        if sieve[i] then begin
           write(1+2*i:8);
           np := np + 1;
           if np = 10 then begin
              writeln(' ');
              np := 0;
           end;
        end;
     end;
     if np > 0 then writeln();
  end;

  il :=  4;
  nl := 10;
  np :=  1;
  for i := 1 to imax do begin
     if sieve[i] then np := np + 1;
     if i = il then begin
        nl := 2*il + 2;
        writeln(' ', 'pi(', nl:8, '): ', np:8);
        il := 10*(il+1)-1;
     end;
  end;

  if nl < nmax then writeln(' ', 'pi(', nmax:8, '): ', np:8);

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPPLIF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=2200K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: soep_pli.pli 976 2017-12-26 15:35:59Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-26   976   1.3    use CHAR(1) array; go for max PRIME size*/
/* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        */
/* 2017-12-25   974   1.1    use 2-dim PRIME array                   */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-09-17   951   1.0    Initial version                         */
/* 2017-09-01   945   0.1    First draft                             */

 SOEP: PROC OPTIONS(MAIN) REORDER;
   DCL (NMAX,PRNT,IMAX)  BIN FIXED(31) INIT(0);
   DCL (NMSQRT)          BIN FIXED(31) INIT(0);
   DCL (I,N,IMIN)        BIN FIXED(31) INIT(0);
   DCL (NP,IL,NL)        BIN FIXED(31) INIT(0);
   /* In PL/I(F) V5.5 array bounds are BIN(15) ! limited to 32k !!   */
   /* And the maximal aggregate size is 2 MByte !!                   */
   /* So go for a 2-dimensional array; 1954*1024 = 2000896;          */
   /* Use 0 as lower bound to make index calculations easy:          */
   /*   PRIME(I) turns into PRIME(I/1024,MOD(I,1024))                */
   DCL PRIME(0:1953,0:1023)  CHAR(1);

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(NMAX,PRNT) (F(10),F(10));

   /*IF NMAX < 10 | NMAX  > 4000000 THEN DO;*/
   IF NMAX  > 4000000 THEN DO;
     PUT SKIP EDIT('nmax out of range (10...4000000), abort') (A);
     GOTO DONE;
   END;

   NMSQRT = FLOOR(SQRT(NMAX));
   IMAX = (NMAX-1)/2;

   DO I=1 TO IMAX;
     PRIME(I/1024,MOD(I,1024)) = '1';
   END;

   DO N=3 TO NMSQRT BY 2;
     I  = N/2;
     IF PRIME(I/1024,MOD(I,1024)) = '1' THEN DO;
       IMIN = N*N/2;
       DO I=IMIN TO IMAX BY N;
         PRIME(I/1024,MOD(I,1024)) = '0';
       END;
     END;
   END;

   IF PRNT > 0 THEN DO;
     PUT SKIP EDIT('List of Primes up to ',NMAX) (A,F(8));
     PUT SKIP EDIT(' ',2) (A,F(7));
     NP = 1;
     DO I=1 TO IMAX;
       IF PRIME(I/1024,MOD(I,1024)) = '1' THEN DO;
         PUT EDIT(' ',1+2*I) (A,F(7));
         NP = NP + 1;
         IF NP = 10 THEN DO;
           PUT SKIP;
           NP = 0;
         END;
       END;
     END;
     IF NP > 0 THEN PUT SKIP;
   END;

   IL =  4;
   NL = 10;
   NP =  1;
   DO I=1 TO IMAX;
     IF PRIME(I/1024,MOD(I,1024)) = '1' THEN NP = NP + 1;
     IF I = IL THEN DO;
       NL = 2*IL + 2;
       PUT SKIP EDIT('pi(',NL,'): ',NP) (A,F(8),A,F(8));
       IL = 10*(IL+1)-1;
     END;
   END;

   IF NL < NMAX THEN PUT SKIP EDIT('pi(',NMAX,'): ',NP)
                                  (A,F(8),A,F(8));

   DONE:;

 END SOEP;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
  4000000         0
/*
//
./        ADD   NAME=SOEPPLIP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=2200K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: soep_pli.pli 976 2017-12-26 15:35:59Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-26   976   1.3    use CHAR(1) array; go for max PRIME size*/
/* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        */
/* 2017-12-25   974   1.1    use 2-dim PRIME array                   */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-09-17   951   1.0    Initial version                         */
/* 2017-09-01   945   0.1    First draft                             */

 SOEP: PROC OPTIONS(MAIN) REORDER;
   DCL (NMAX,PRNT,IMAX)  BIN FIXED(31) INIT(0);
   DCL (NMSQRT)          BIN FIXED(31) INIT(0);
   DCL (I,N,IMIN)        BIN FIXED(31) INIT(0);
   DCL (NP,IL,NL)        BIN FIXED(31) INIT(0);
   /* In PL/I(F) V5.5 array bounds are BIN(15) ! limited to 32k !!   */
   /* And the maximal aggregate size is 2 MByte !!                   */
   /* So go for a 2-dimensional array; 1954*1024 = 2000896;          */
   /* Use 0 as lower bound to make index calculations easy:          */
   /*   PRIME(I) turns into PRIME(I/1024,MOD(I,1024))                */
   DCL PRIME(0:1953,0:1023)  CHAR(1);

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(NMAX,PRNT) (F(10),F(10));

   /*IF NMAX < 10 | NMAX  > 4000000 THEN DO;*/
   IF NMAX  > 4000000 THEN DO;
     PUT SKIP EDIT('nmax out of range (10...4000000), abort') (A);
     GOTO DONE;
   END;

   NMSQRT = FLOOR(SQRT(NMAX));
   IMAX = (NMAX-1)/2;

   DO I=1 TO IMAX;
     PRIME(I/1024,MOD(I,1024)) = '1';
   END;

   DO N=3 TO NMSQRT BY 2;
     I  = N/2;
     IF PRIME(I/1024,MOD(I,1024)) = '1' THEN DO;
       IMIN = N*N/2;
       DO I=IMIN TO IMAX BY N;
         PRIME(I/1024,MOD(I,1024)) = '0';
       END;
     END;
   END;

   IF PRNT > 0 THEN DO;
     PUT SKIP EDIT('List of Primes up to ',NMAX) (A,F(8));
     PUT SKIP EDIT(' ',2) (A,F(7));
     NP = 1;
     DO I=1 TO IMAX;
       IF PRIME(I/1024,MOD(I,1024)) = '1' THEN DO;
         PUT EDIT(' ',1+2*I) (A,F(7));
         NP = NP + 1;
         IF NP = 10 THEN DO;
           PUT SKIP;
           NP = 0;
         END;
       END;
     END;
     IF NP > 0 THEN PUT SKIP;
   END;

   IL =  4;
   NL = 10;
   NP =  1;
   DO I=1 TO IMAX;
     IF PRIME(I/1024,MOD(I,1024)) = '1' THEN NP = NP + 1;
     IF I = IL THEN DO;
       NL = 2*IL + 2;
       PUT SKIP EDIT('pi(',NL,'): ',NP) (A,F(8),A,F(8));
       IL = 10*(IL+1)-1;
     END;
   END;

   IF NL < NMAX THEN PUT SKIP EDIT('pi(',NMAX,'): ',NP)
                                  (A,F(8),A,F(8));

   DONE:;

 END SOEP;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
  4000000         1
/*
//
./        ADD   NAME=SOEPPLIT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=2200K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: soep_pli.pli 976 2017-12-26 15:35:59Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-26   976   1.3    use CHAR(1) array; go for max PRIME size*/
/* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        */
/* 2017-12-25   974   1.1    use 2-dim PRIME array                   */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-09-17   951   1.0    Initial version                         */
/* 2017-09-01   945   0.1    First draft                             */

 SOEP: PROC OPTIONS(MAIN) REORDER;
   DCL (NMAX,PRNT,IMAX)  BIN FIXED(31) INIT(0);
   DCL (NMSQRT)          BIN FIXED(31) INIT(0);
   DCL (I,N,IMIN)        BIN FIXED(31) INIT(0);
   DCL (NP,IL,NL)        BIN FIXED(31) INIT(0);
   /* In PL/I(F) V5.5 array bounds are BIN(15) ! limited to 32k !!   */
   /* And the maximal aggregate size is 2 MByte !!                   */
   /* So go for a 2-dimensional array; 1954*1024 = 2000896;          */
   /* Use 0 as lower bound to make index calculations easy:          */
   /*   PRIME(I) turns into PRIME(I/1024,MOD(I,1024))                */
   DCL PRIME(0:1953,0:1023)  CHAR(1);

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(NMAX,PRNT) (F(10),F(10));

   /*IF NMAX < 10 | NMAX  > 4000000 THEN DO;*/
   IF NMAX  > 4000000 THEN DO;
     PUT SKIP EDIT('nmax out of range (10...4000000), abort') (A);
     GOTO DONE;
   END;

   NMSQRT = FLOOR(SQRT(NMAX));
   IMAX = (NMAX-1)/2;

   DO I=1 TO IMAX;
     PRIME(I/1024,MOD(I,1024)) = '1';
   END;

   DO N=3 TO NMSQRT BY 2;
     I  = N/2;
     IF PRIME(I/1024,MOD(I,1024)) = '1' THEN DO;
       IMIN = N*N/2;
       DO I=IMIN TO IMAX BY N;
         PRIME(I/1024,MOD(I,1024)) = '0';
       END;
     END;
   END;

   IF PRNT > 0 THEN DO;
     PUT SKIP EDIT('List of Primes up to ',NMAX) (A,F(8));
     PUT SKIP EDIT(' ',2) (A,F(7));
     NP = 1;
     DO I=1 TO IMAX;
       IF PRIME(I/1024,MOD(I,1024)) = '1' THEN DO;
         PUT EDIT(' ',1+2*I) (A,F(7));
         NP = NP + 1;
         IF NP = 10 THEN DO;
           PUT SKIP;
           NP = 0;
         END;
       END;
     END;
     IF NP > 0 THEN PUT SKIP;
   END;

   IL =  4;
   NL = 10;
   NP =  1;
   DO I=1 TO IMAX;
     IF PRIME(I/1024,MOD(I,1024)) = '1' THEN NP = NP + 1;
     IF I = IL THEN DO;
       NL = 2*IL + 2;
       PUT SKIP EDIT('pi(',NL,'): ',NP) (A,F(8),A,F(8));
       IL = 10*(IL+1)-1;
     END;
   END;

   IF NL < NMAX THEN PUT SKIP EDIT('pi(',NMAX,'): ',NP)
                                  (A,F(8),A,F(8));

   DONE:;

 END SOEP;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEPSIMF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM=NOSUBCHK,
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end
* 2017-12-24   973   1.1    use WHILE not FOR to avoid compiler bug 
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-09-17   951   1.0    Initial version
* 2017-09-08   949   0.1    First draft
*
* Note on SIMULA 67 (VERS 12.00) complier bug workaround
* - an inner loop codes as
*     FOR i:= n2 // 2 STEP n UNTIL imax DO prime(i) := FALSE
*   fails with 'FIXED POINT OVFL at line' of the FOR loop.
* - The equivalent WHILE loop used below works.
*;

BEGIN
  INTEGER nmax,prnt,imax,nmsqrt;
  INTEGER i,n,imin;
  INTEGER np,il,nl;
  BOOLEAN ARRAY prime(1:5000000);

  nmax := InInt;
  prnt := InInt;

  IF nmax < 10 OR nmax > 10000000 THEN BEGIN
     OutText("nmax out of range (10...10000000), abort");
     GOTO done;
  END;

  nmsqrt := Entier(Sqrt(nmax));
  imax := (nmax-1) // 2;
  FOR i := 1 STEP 1 UNTIL imax DO prime(i) := TRUE;

  FOR n := 3 STEP 2 UNTIL nmsqrt DO BEGIN
     IF prime(n//2) THEN BEGIN
        i := (n*n) // 2;
        WHILE i <= imax DO BEGIN
           prime(i) := FALSE;
           i:= i +  n;
        END;
     END;
  END;

  IF prnt > 0 THEN BEGIN
     OutText("List of Primes up to ");
     OutInt(nmax,8);
     OutImage;
     OutInt(2,8);
     np := 1;
     FOR i := 1 STEP 1 UNTIL imax DO BEGIN
        IF prime(i) THEN BEGIN
           OutInt(1+2*i,8);
           np := np + 1;
           IF np = 10 THEN BEGIN
              OutImage;
              np := 0;
           END;
        END;
     END;
     IF np > 0 THEN OutImage;
  END;

  il :=  4;
  nl := 10;
  np :=  1;

  FOR i := 1 STEP 1 UNTIL imax DO BEGIN
     IF prime(i) THEN np := np + 1;
     IF i = il THEN BEGIN
        nl := 2*il + 2;
        OutText("pi(");
        OutInt(nl,8);
        OutText(")=");
        OutInt(np,8);
        OutImage;
       il := 10*(il+1)-1;
     END;
  END;

  IF nl < nmax THEN BEGIN
     OutText("pi(");
     OutInt(nmax,8);
     OutText(")=");
     OutInt(np,8);
     OutImage;
  END;

  done:
  OutImage;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
 10000000         0
/*
//
./        ADD   NAME=SOEPSIMP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM=NOSUBCHK,
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end
* 2017-12-24   973   1.1    use WHILE not FOR to avoid compiler bug 
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-09-17   951   1.0    Initial version
* 2017-09-08   949   0.1    First draft
*
* Note on SIMULA 67 (VERS 12.00) complier bug workaround
* - an inner loop codes as
*     FOR i:= n2 // 2 STEP n UNTIL imax DO prime(i) := FALSE
*   fails with 'FIXED POINT OVFL at line' of the FOR loop.
* - The equivalent WHILE loop used below works.
*;

BEGIN
  INTEGER nmax,prnt,imax,nmsqrt;
  INTEGER i,n,imin;
  INTEGER np,il,nl;
  BOOLEAN ARRAY prime(1:5000000);

  nmax := InInt;
  prnt := InInt;

  IF nmax < 10 OR nmax > 10000000 THEN BEGIN
     OutText("nmax out of range (10...10000000), abort");
     GOTO done;
  END;

  nmsqrt := Entier(Sqrt(nmax));
  imax := (nmax-1) // 2;
  FOR i := 1 STEP 1 UNTIL imax DO prime(i) := TRUE;

  FOR n := 3 STEP 2 UNTIL nmsqrt DO BEGIN
     IF prime(n//2) THEN BEGIN
        i := (n*n) // 2;
        WHILE i <= imax DO BEGIN
           prime(i) := FALSE;
           i:= i +  n;
        END;
     END;
  END;

  IF prnt > 0 THEN BEGIN
     OutText("List of Primes up to ");
     OutInt(nmax,8);
     OutImage;
     OutInt(2,8);
     np := 1;
     FOR i := 1 STEP 1 UNTIL imax DO BEGIN
        IF prime(i) THEN BEGIN
           OutInt(1+2*i,8);
           np := np + 1;
           IF np = 10 THEN BEGIN
              OutImage;
              np := 0;
           END;
        END;
     END;
     IF np > 0 THEN OutImage;
  END;

  il :=  4;
  nl := 10;
  np :=  1;

  FOR i := 1 STEP 1 UNTIL imax DO BEGIN
     IF prime(i) THEN np := np + 1;
     IF i = il THEN BEGIN
        nl := 2*il + 2;
        OutText("pi(");
        OutInt(nl,8);
        OutText(")=");
        OutInt(np,8);
        OutImage;
       il := 10*(il+1)-1;
     END;
  END;

  IF nl < nmax THEN BEGIN
     OutText("pi(");
     OutInt(nmax,8);
     OutText(")=");
     OutInt(np,8);
     OutImage;
  END;

  done:
  OutImage;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEPSIMT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEP#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end
* 2017-12-24   973   1.1    use WHILE not FOR to avoid compiler bug 
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-09-17   951   1.0    Initial version
* 2017-09-08   949   0.1    First draft
*
* Note on SIMULA 67 (VERS 12.00) complier bug workaround
* - an inner loop codes as
*     FOR i:= n2 // 2 STEP n UNTIL imax DO prime(i) := FALSE
*   fails with 'FIXED POINT OVFL at line' of the FOR loop.
* - The equivalent WHILE loop used below works.
*;

BEGIN
  INTEGER nmax,prnt,imax,nmsqrt;
  INTEGER i,n,imin;
  INTEGER np,il,nl;
  BOOLEAN ARRAY prime(1:5000000);

  nmax := InInt;
  prnt := InInt;

  IF nmax < 10 OR nmax > 10000000 THEN BEGIN
     OutText("nmax out of range (10...10000000), abort");
     GOTO done;
  END;

  nmsqrt := Entier(Sqrt(nmax));
  imax := (nmax-1) // 2;
  FOR i := 1 STEP 1 UNTIL imax DO prime(i) := TRUE;

  FOR n := 3 STEP 2 UNTIL nmsqrt DO BEGIN
     IF prime(n//2) THEN BEGIN
        i := (n*n) // 2;
        WHILE i <= imax DO BEGIN
           prime(i) := FALSE;
           i:= i +  n;
        END;
     END;
  END;

  IF prnt > 0 THEN BEGIN
     OutText("List of Primes up to ");
     OutInt(nmax,8);
     OutImage;
     OutInt(2,8);
     np := 1;
     FOR i := 1 STEP 1 UNTIL imax DO BEGIN
        IF prime(i) THEN BEGIN
           OutInt(1+2*i,8);
           np := np + 1;
           IF np = 10 THEN BEGIN
              OutImage;
              np := 0;
           END;
        END;
     END;
     IF np > 0 THEN OutImage;
  END;

  il :=  4;
  nl := 10;
  np :=  1;

  FOR i := 1 STEP 1 UNTIL imax DO BEGIN
     IF prime(i) THEN np := np + 1;
     IF i = il THEN BEGIN
        nl := 2*il + 2;
        OutText("pi(");
        OutInt(nl,8);
        OutText(")=");
        OutInt(np,8);
        OutImage;
       il := 10*(il+1)-1;
     END;
  END;

  IF nl < nmax THEN BEGIN
     OutText("pi(");
     OutInt(nmax,8);
     OutText(")=");
     OutInt(np,8);
     OutImage;
  END;

  done:
  OutImage;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEQASMF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6500K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NOLIST,NOXREF,NORLD,NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: soeq_asm.asm 972 2017-12-23 20:55:41Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-11-19   965   1.1    no XR in inner loop, bit reversed prime[]
* 2017-11-18   963   1.0    Initial version
*
       PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  4  open SYSPRINT failed
*   RC =  8  open SYSIN failed
*   RC = 12  unexpected SYSIN EOF
*   RC = 16  NMAX out of range
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'08'
        B     EXIT               quit with RC=8
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get NMAX
        ST    R1,NMAX
        BAL   R14,IINT10         get PRNT
        STC   R1,PRNT
*
        L     R1,NMAX
        C     R1,=F'10'          is NMAX >= 10
        BL    NMAXBAD            if < not
        C     R1,=F'100000000'   is NMAX <= 100000000
        BNH   NMAXOK             if <= yes
NMAXBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
NMAXOK   EQU   *         
*
* setup phase ---------------------------------------------------------
*
*   calculate sqrt(nmax) -----------------------------------
*     use simple bi-section method
*       R4   low  bound
*       R5   high bound
*       R7   middle (low+high)/2
*
        LA    R4,1               set  low bound
        L     R5,NMAX            set high bound
        LA    R6,32              set iteration limit
NMSQRTLP LR    R7,R4              R7:= low
        AR    R7,R5              R7:= (low+high)
        SRA   R7,1               R7:= (low+high)/2
        LR    R3,R7
        MR    R2,R7              (R2,R3) := R7*R7
        LTR   R2,R2              more than 32 bit ?
        BNE   NMSQRTHI           if != yes, mid too high
        CL    R3,NMAX            mid*mid > NMAX
        BH    NMSQRTHI           if > yes, mid too high
        LR    R4,R7              here mid to  low:  low := mid
        B     NMSQRTGO
NMSQRTHI LR    R5,R7              here mid to high: high := mid
NMSQRTGO LR    R8,R5              R8 := high
        SR    R8,R4              R8 := high-low
        LR    R1,R6
        C     R8,=F'1'           spread <= 1 ?
        BNH   NMSQRTOK           if <= yes, quit
        BCT   R6,NMSQRTLP
        ABEND 99                 abort if doesn't converge
NMSQRTOK EQU   *
        ST    R4,NMSQRT

*   allocate PRIME array -----------------------------------
        L     R2,NMAX
        BCTR  R2,0               NMAX-1
        SRA   R2,1               (NMAX-1)/2
        ST    R2,BIMAX
        A     R2,=F'7'           BIMAX+7
        SRA   R2,3               (BIMAX+7)/8
        ST    R2,WIMAX
        LR    R5,R2
        A     R5,=F'1'           WIMAX+1
        GETMAIN RU,LV=(5)        allocate storage for PRIME
        ST    R1,PRIME           store sieve base
        LR    R9,R1              R9 := PRIME base
*
*   set each PRIME array byte to X'01' ---------------------
        LR    R4,R1              R4 := PRIME
*                                 R5 := sizeof(PRIME) (still)
        XR    R6,R6              R6 := 0
        L     R7,=X'FF000000'    R7 := padding=0xFF and length=0
        MVCL  R4,R6              set all PRIME words to 0xFFFF
*
* sieve phase ---------------------------------------------------------
*   outer loop:  ind  R6   n
*                inc  R4   2
*                lim  R5   sqrt(NMAX)
*   inner loop:  ind  R3   i
*                inc  R6   n
*                lim  R7   bimax
*                     R9   &prime
*                     R8   0x80     
*                     R10  0x07
*                     R11  0xFF7F
*                     R0,R1,R2,R15     temporaries
*
*
*   equivalent C code:
*     for (n=3; n<=nmsqrt; n+=2) {
*       i = n/2;
*       if ((prime[i>>3] & (0x80>>(i&0x7))) == 0) continue;
*       for (i=(n*n)/2; i<=bimax ; i+=n) {
*         prime[i>>3] &= (0xff7f>>(i&0x7);     '!!pseudo code !!'
*       }
*     }
*
        LA    R6,3               outer ind: R6:=3
        LA    R4,2               outer inc: R4:=2
        L     R5,NMSQRT          outer lim: R5:=NMSQRT
        L     R7,BIMAX           inner lim: R7:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        L     R11,=X'FFFFFF7F'   R11:=0xffffff7f
*
SIEVO    LR    R2,R6              R2:=n
        SRA   R2,1               R2:=n/2
        LR    R15,R2             i
        NR    R15,R10            i&0x07
        LR    R1,R8              0x80
        SRL   R1,0(R15)          0x80>>(i&0x7)
        SRL   R2,3               i>>3
        IC    R2,0(R2,R9)        prime[i>>3]
        NR    R2,R1              prime[i>>3] & (0x80>>(i&0x7))
        BZ    SIEVOC             if =0 not, continue outer loop
*
        LR    R1,R6              R1:=n
        MR    R0,R6              R1:=n*n (lower half, enough)
        LR    R3,R1              R3:=n*n too
        SRA   R3,1               R3:=(n*n)/2
*
SIEVI    LR    R2,R3              i
        NR    R2,R10             i&0x7
        LR    R1,R11             0xff7f
        SRL   R1,0(R2)           0xff7f>>(i&0x7)
        LR    R2,R3              i
        SRL   R2,3               i>>3
        IC    R0,0(R2,R9)        prime[i>>3]
        NR    R0,R1              & 0xff7f>>(i&0x7)
        STC   R0,0(R2,R9)        prime[i>>3] &= 0xff7f>>(i&0x7)
        BXLE  R3,R6,SIEVI
*
SIEVOC   BXLE  R6,R4,SIEVO
*
* print primes table --------------------------------------------------
*   loop:  ind  R3   i
*          inc  R4   1
*          lim  R5   imax
*               R2   np
*               R9   &prime
*               R8   0x80   
*               R10  0x07
*               R11  1
PRT      EQU   *
        CLI   PRNT,X'00'         primes to be printed ?
        BE    NOPRNT             if = skip
        L     R1,MSGLIST
        BAL   R14,OTEXT          print heading
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        BAL   R14,OPUTLINE       write line
*
        LA    R1,2
        BAL   R14,OINT10         print "2"  (1st prime...)
        LA    R2,1               np=1
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,BIMAX           lim: R5:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        LA    R11,1              R11:=1
PRTLOOP  LR    R6,R3              i
        NR    R6,R10             i&0x7
        LR    R1,R8              0x80
        SRL   R1,0(R6)           0x80>>(i&0x7)
        LR    R6,R3              i
        SRL   R6,3               i>>3
        IC    R0,0(R6,R9)        prime[i>>3]
        NR    R0,R1              prime[i>>3] & (0x80>>(i&0x7))
        BE    PRTLOOPC           if = not, continue
        LR    R1,R3              R1:=i
        SLA   R1,1               R1:=2*i
        AR    R1,R11             R1:=1+2*i
        BAL   R14,OINT10         and print F(10)
        AR    R2,R11             np+=1
        C     R2,=F'10'          check whether = 10
        BNZ   PRTLOOPC           if != not, continue
        BAL   R14,OPUTLINE       write line
        XR    R2,R2              np=0
PRTLOOPC EQU   *
        BXLE  R3,R4,PRTLOOP
*
        LTR   R2,R2              check prime count np
        BZ    NOPRNT
        BAL   R14,OPUTLINE       write line
NOPRNT   EQU   *
*
* print primes count --------------------------------------------------
*   loop:  ind  R3   i
*          inc  R4   1
*          lim  R5   imax
*               R2   np
*               R7   il
*               R6   nl
*               R9   &prime
*               R8   0x80   
*               R10  0x07
*               R11  1
*
TBL      EQU   *
        LA    R2,1               np=1
        LA    R7,4               il=4
        LA    R6,10              nl=10
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,BIMAX           lim: R5:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        LA    R11,1              R11:=1
TBLLOOP  LR    R15,R3             i
        NR    R15,R10            i&0x7
        LR    R1,R8              0x80
        SRL   R1,0(R15)          0x80>>(i&0x7)
        LR    R15,R3             i
        SRL   R15,3              i>>3
        IC    R0,0(R15,R9)       prime[i>>3]
        NR    R0,R1              prime[i>>3] & (1<<(i&0x7))
        BE    NOPRIME            if = not
        AR    R2,R11             np+= 1
NOPRIME  CR    R3,R7              test i != il
        BNE   TBLLOOPC
        LR    R6,R7              nl=il
        SLA   R6,1               nl=2*il
        A     R6,=F'2'           nl=2+2*il
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        LR    R1,R6
        BAL   R14,OINT10         print nl
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
        LR    R1,R7              R1:=il
        AR    R1,R11             R1:=il+1
        M     R0,=F'10'          R1:=10*(il+1)
        SR    R1,R11             R1:=10*(il+1)-1
        LR    R7,R1              update il
*
TBLLOOPC EQU   *
        BXLE  R3,R4,TBLLOOP
*
        C     R6,NMAX            is nl != nmax ?
        BE    TBLNOTR            if = not, skip extra summary
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
TBLNOTR  EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
NMAX     DC    F'10000000'        highest prime to search for
NMSQRT   DS    F                  sqrt(NMAX)
BIMAX    DS    F                  highest prime array bit index
WIMAX    DS    F                  highest prime array word index
PRIME    DS    F                  prime array pointer
PRNT     DC    X'00'              print enable flag
*
* message strings
*
MSGPERR  OTXTDSC C'NMAX must be >= 10 and <= 100000000, abort'
MSGLIST  OTXTDSC C'List of Primes up to '
MSGPI    OTXTDSC C'pi('
MSGPISEP OTXTDSC C'): '
*
* spill literal pool
*
        LTORG
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
100000000         0
/*
//
./        ADD   NAME=SOEQASMP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=650K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NOLIST,NOXREF,NORLD,NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: soeq_asm.asm 972 2017-12-23 20:55:41Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-11-19   965   1.1    no XR in inner loop, bit reversed prime[]
* 2017-11-18   963   1.0    Initial version
*
       PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  4  open SYSPRINT failed
*   RC =  8  open SYSIN failed
*   RC = 12  unexpected SYSIN EOF
*   RC = 16  NMAX out of range
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'08'
        B     EXIT               quit with RC=8
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get NMAX
        ST    R1,NMAX
        BAL   R14,IINT10         get PRNT
        STC   R1,PRNT
*
        L     R1,NMAX
        C     R1,=F'10'          is NMAX >= 10
        BL    NMAXBAD            if < not
        C     R1,=F'100000000'   is NMAX <= 100000000
        BNH   NMAXOK             if <= yes
NMAXBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
NMAXOK   EQU   *         
*
* setup phase ---------------------------------------------------------
*
*   calculate sqrt(nmax) -----------------------------------
*     use simple bi-section method
*       R4   low  bound
*       R5   high bound
*       R7   middle (low+high)/2
*
        LA    R4,1               set  low bound
        L     R5,NMAX            set high bound
        LA    R6,32              set iteration limit
NMSQRTLP LR    R7,R4              R7:= low
        AR    R7,R5              R7:= (low+high)
        SRA   R7,1               R7:= (low+high)/2
        LR    R3,R7
        MR    R2,R7              (R2,R3) := R7*R7
        LTR   R2,R2              more than 32 bit ?
        BNE   NMSQRTHI           if != yes, mid too high
        CL    R3,NMAX            mid*mid > NMAX
        BH    NMSQRTHI           if > yes, mid too high
        LR    R4,R7              here mid to  low:  low := mid
        B     NMSQRTGO
NMSQRTHI LR    R5,R7              here mid to high: high := mid
NMSQRTGO LR    R8,R5              R8 := high
        SR    R8,R4              R8 := high-low
        LR    R1,R6
        C     R8,=F'1'           spread <= 1 ?
        BNH   NMSQRTOK           if <= yes, quit
        BCT   R6,NMSQRTLP
        ABEND 99                 abort if doesn't converge
NMSQRTOK EQU   *
        ST    R4,NMSQRT

*   allocate PRIME array -----------------------------------
        L     R2,NMAX
        BCTR  R2,0               NMAX-1
        SRA   R2,1               (NMAX-1)/2
        ST    R2,BIMAX
        A     R2,=F'7'           BIMAX+7
        SRA   R2,3               (BIMAX+7)/8
        ST    R2,WIMAX
        LR    R5,R2
        A     R5,=F'1'           WIMAX+1
        GETMAIN RU,LV=(5)        allocate storage for PRIME
        ST    R1,PRIME           store sieve base
        LR    R9,R1              R9 := PRIME base
*
*   set each PRIME array byte to X'01' ---------------------
        LR    R4,R1              R4 := PRIME
*                                 R5 := sizeof(PRIME) (still)
        XR    R6,R6              R6 := 0
        L     R7,=X'FF000000'    R7 := padding=0xFF and length=0
        MVCL  R4,R6              set all PRIME words to 0xFFFF
*
* sieve phase ---------------------------------------------------------
*   outer loop:  ind  R6   n
*                inc  R4   2
*                lim  R5   sqrt(NMAX)
*   inner loop:  ind  R3   i
*                inc  R6   n
*                lim  R7   bimax
*                     R9   &prime
*                     R8   0x80     
*                     R10  0x07
*                     R11  0xFF7F
*                     R0,R1,R2,R15     temporaries
*
*
*   equivalent C code:
*     for (n=3; n<=nmsqrt; n+=2) {
*       i = n/2;
*       if ((prime[i>>3] & (0x80>>(i&0x7))) == 0) continue;
*       for (i=(n*n)/2; i<=bimax ; i+=n) {
*         prime[i>>3] &= (0xff7f>>(i&0x7);     '!!pseudo code !!'
*       }
*     }
*
        LA    R6,3               outer ind: R6:=3
        LA    R4,2               outer inc: R4:=2
        L     R5,NMSQRT          outer lim: R5:=NMSQRT
        L     R7,BIMAX           inner lim: R7:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        L     R11,=X'FFFFFF7F'   R11:=0xffffff7f
*
SIEVO    LR    R2,R6              R2:=n
        SRA   R2,1               R2:=n/2
        LR    R15,R2             i
        NR    R15,R10            i&0x07
        LR    R1,R8              0x80
        SRL   R1,0(R15)          0x80>>(i&0x7)
        SRL   R2,3               i>>3
        IC    R2,0(R2,R9)        prime[i>>3]
        NR    R2,R1              prime[i>>3] & (0x80>>(i&0x7))
        BZ    SIEVOC             if =0 not, continue outer loop
*
        LR    R1,R6              R1:=n
        MR    R0,R6              R1:=n*n (lower half, enough)
        LR    R3,R1              R3:=n*n too
        SRA   R3,1               R3:=(n*n)/2
*
SIEVI    LR    R2,R3              i
        NR    R2,R10             i&0x7
        LR    R1,R11             0xff7f
        SRL   R1,0(R2)           0xff7f>>(i&0x7)
        LR    R2,R3              i
        SRL   R2,3               i>>3
        IC    R0,0(R2,R9)        prime[i>>3]
        NR    R0,R1              & 0xff7f>>(i&0x7)
        STC   R0,0(R2,R9)        prime[i>>3] &= 0xff7f>>(i&0x7)
        BXLE  R3,R6,SIEVI
*
SIEVOC   BXLE  R6,R4,SIEVO
*
* print primes table --------------------------------------------------
*   loop:  ind  R3   i
*          inc  R4   1
*          lim  R5   imax
*               R2   np
*               R9   &prime
*               R8   0x80   
*               R10  0x07
*               R11  1
PRT      EQU   *
        CLI   PRNT,X'00'         primes to be printed ?
        BE    NOPRNT             if = skip
        L     R1,MSGLIST
        BAL   R14,OTEXT          print heading
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        BAL   R14,OPUTLINE       write line
*
        LA    R1,2
        BAL   R14,OINT10         print "2"  (1st prime...)
        LA    R2,1               np=1
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,BIMAX           lim: R5:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        LA    R11,1              R11:=1
PRTLOOP  LR    R6,R3              i
        NR    R6,R10             i&0x7
        LR    R1,R8              0x80
        SRL   R1,0(R6)           0x80>>(i&0x7)
        LR    R6,R3              i
        SRL   R6,3               i>>3
        IC    R0,0(R6,R9)        prime[i>>3]
        NR    R0,R1              prime[i>>3] & (0x80>>(i&0x7))
        BE    PRTLOOPC           if = not, continue
        LR    R1,R3              R1:=i
        SLA   R1,1               R1:=2*i
        AR    R1,R11             R1:=1+2*i
        BAL   R14,OINT10         and print F(10)
        AR    R2,R11             np+=1
        C     R2,=F'10'          check whether = 10
        BNZ   PRTLOOPC           if != not, continue
        BAL   R14,OPUTLINE       write line
        XR    R2,R2              np=0
PRTLOOPC EQU   *
        BXLE  R3,R4,PRTLOOP
*
        LTR   R2,R2              check prime count np
        BZ    NOPRNT
        BAL   R14,OPUTLINE       write line
NOPRNT   EQU   *
*
* print primes count --------------------------------------------------
*   loop:  ind  R3   i
*          inc  R4   1
*          lim  R5   imax
*               R2   np
*               R7   il
*               R6   nl
*               R9   &prime
*               R8   0x80   
*               R10  0x07
*               R11  1
*
TBL      EQU   *
        LA    R2,1               np=1
        LA    R7,4               il=4
        LA    R6,10              nl=10
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,BIMAX           lim: R5:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        LA    R11,1              R11:=1
TBLLOOP  LR    R15,R3             i
        NR    R15,R10            i&0x7
        LR    R1,R8              0x80
        SRL   R1,0(R15)          0x80>>(i&0x7)
        LR    R15,R3             i
        SRL   R15,3              i>>3
        IC    R0,0(R15,R9)       prime[i>>3]
        NR    R0,R1              prime[i>>3] & (1<<(i&0x7))
        BE    NOPRIME            if = not
        AR    R2,R11             np+= 1
NOPRIME  CR    R3,R7              test i != il
        BNE   TBLLOOPC
        LR    R6,R7              nl=il
        SLA   R6,1               nl=2*il
        A     R6,=F'2'           nl=2+2*il
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        LR    R1,R6
        BAL   R14,OINT10         print nl
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
        LR    R1,R7              R1:=il
        AR    R1,R11             R1:=il+1
        M     R0,=F'10'          R1:=10*(il+1)
        SR    R1,R11             R1:=10*(il+1)-1
        LR    R7,R1              update il
*
TBLLOOPC EQU   *
        BXLE  R3,R4,TBLLOOP
*
        C     R6,NMAX            is nl != nmax ?
        BE    TBLNOTR            if = not, skip extra summary
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
TBLNOTR  EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
NMAX     DC    F'10000000'        highest prime to search for
NMSQRT   DS    F                  sqrt(NMAX)
BIMAX    DS    F                  highest prime array bit index
WIMAX    DS    F                  highest prime array word index
PRIME    DS    F                  prime array pointer
PRNT     DC    X'00'              print enable flag
*
* message strings
*
MSGPERR  OTXTDSC C'NMAX must be >= 10 and <= 100000000, abort'
MSGLIST  OTXTDSC C'List of Primes up to '
MSGPI    OTXTDSC C'pi('
MSGPISEP OTXTDSC C'): '
*
* spill literal pool
*
        LTORG
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEQASMT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: soeq_asm.asm 972 2017-12-23 20:55:41Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2
* 2017-11-19   965   1.1    no XR in inner loop, bit reversed prime[]
* 2017-11-18   963   1.0    Initial version
*
       PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  4  open SYSPRINT failed
*   RC =  8  open SYSIN failed
*   RC = 12  unexpected SYSIN EOF
*   RC = 16  NMAX out of range
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'08'
        B     EXIT               quit with RC=8
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get NMAX
        ST    R1,NMAX
        BAL   R14,IINT10         get PRNT
        STC   R1,PRNT
*
        L     R1,NMAX
        C     R1,=F'10'          is NMAX >= 10
        BL    NMAXBAD            if < not
        C     R1,=F'100000000'   is NMAX <= 100000000
        BNH   NMAXOK             if <= yes
NMAXBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
NMAXOK   EQU   *         
*
* setup phase ---------------------------------------------------------
*
*   calculate sqrt(nmax) -----------------------------------
*     use simple bi-section method
*       R4   low  bound
*       R5   high bound
*       R7   middle (low+high)/2
*
        LA    R4,1               set  low bound
        L     R5,NMAX            set high bound
        LA    R6,32              set iteration limit
NMSQRTLP LR    R7,R4              R7:= low
        AR    R7,R5              R7:= (low+high)
        SRA   R7,1               R7:= (low+high)/2
        LR    R3,R7
        MR    R2,R7              (R2,R3) := R7*R7
        LTR   R2,R2              more than 32 bit ?
        BNE   NMSQRTHI           if != yes, mid too high
        CL    R3,NMAX            mid*mid > NMAX
        BH    NMSQRTHI           if > yes, mid too high
        LR    R4,R7              here mid to  low:  low := mid
        B     NMSQRTGO
NMSQRTHI LR    R5,R7              here mid to high: high := mid
NMSQRTGO LR    R8,R5              R8 := high
        SR    R8,R4              R8 := high-low
        LR    R1,R6
        C     R8,=F'1'           spread <= 1 ?
        BNH   NMSQRTOK           if <= yes, quit
        BCT   R6,NMSQRTLP
        ABEND 99                 abort if doesn't converge
NMSQRTOK EQU   *
        ST    R4,NMSQRT

*   allocate PRIME array -----------------------------------
        L     R2,NMAX
        BCTR  R2,0               NMAX-1
        SRA   R2,1               (NMAX-1)/2
        ST    R2,BIMAX
        A     R2,=F'7'           BIMAX+7
        SRA   R2,3               (BIMAX+7)/8
        ST    R2,WIMAX
        LR    R5,R2
        A     R5,=F'1'           WIMAX+1
        GETMAIN RU,LV=(5)        allocate storage for PRIME
        ST    R1,PRIME           store sieve base
        LR    R9,R1              R9 := PRIME base
*
*   set each PRIME array byte to X'01' ---------------------
        LR    R4,R1              R4 := PRIME
*                                 R5 := sizeof(PRIME) (still)
        XR    R6,R6              R6 := 0
        L     R7,=X'FF000000'    R7 := padding=0xFF and length=0
        MVCL  R4,R6              set all PRIME words to 0xFFFF
*
* sieve phase ---------------------------------------------------------
*   outer loop:  ind  R6   n
*                inc  R4   2
*                lim  R5   sqrt(NMAX)
*   inner loop:  ind  R3   i
*                inc  R6   n
*                lim  R7   bimax
*                     R9   &prime
*                     R8   0x80     
*                     R10  0x07
*                     R11  0xFF7F
*                     R0,R1,R2,R15     temporaries
*
*
*   equivalent C code:
*     for (n=3; n<=nmsqrt; n+=2) {
*       i = n/2;
*       if ((prime[i>>3] & (0x80>>(i&0x7))) == 0) continue;
*       for (i=(n*n)/2; i<=bimax ; i+=n) {
*         prime[i>>3] &= (0xff7f>>(i&0x7);     '!!pseudo code !!'
*       }
*     }
*
        LA    R6,3               outer ind: R6:=3
        LA    R4,2               outer inc: R4:=2
        L     R5,NMSQRT          outer lim: R5:=NMSQRT
        L     R7,BIMAX           inner lim: R7:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        L     R11,=X'FFFFFF7F'   R11:=0xffffff7f
*
SIEVO    LR    R2,R6              R2:=n
        SRA   R2,1               R2:=n/2
        LR    R15,R2             i
        NR    R15,R10            i&0x07
        LR    R1,R8              0x80
        SRL   R1,0(R15)          0x80>>(i&0x7)
        SRL   R2,3               i>>3
        IC    R2,0(R2,R9)        prime[i>>3]
        NR    R2,R1              prime[i>>3] & (0x80>>(i&0x7))
        BZ    SIEVOC             if =0 not, continue outer loop
*
        LR    R1,R6              R1:=n
        MR    R0,R6              R1:=n*n (lower half, enough)
        LR    R3,R1              R3:=n*n too
        SRA   R3,1               R3:=(n*n)/2
*
SIEVI    LR    R2,R3              i
        NR    R2,R10             i&0x7
        LR    R1,R11             0xff7f
        SRL   R1,0(R2)           0xff7f>>(i&0x7)
        LR    R2,R3              i
        SRL   R2,3               i>>3
        IC    R0,0(R2,R9)        prime[i>>3]
        NR    R0,R1              & 0xff7f>>(i&0x7)
        STC   R0,0(R2,R9)        prime[i>>3] &= 0xff7f>>(i&0x7)
        BXLE  R3,R6,SIEVI
*
SIEVOC   BXLE  R6,R4,SIEVO
*
* print primes table --------------------------------------------------
*   loop:  ind  R3   i
*          inc  R4   1
*          lim  R5   imax
*               R2   np
*               R9   &prime
*               R8   0x80   
*               R10  0x07
*               R11  1
PRT      EQU   *
        CLI   PRNT,X'00'         primes to be printed ?
        BE    NOPRNT             if = skip
        L     R1,MSGLIST
        BAL   R14,OTEXT          print heading
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        BAL   R14,OPUTLINE       write line
*
        LA    R1,2
        BAL   R14,OINT10         print "2"  (1st prime...)
        LA    R2,1               np=1
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,BIMAX           lim: R5:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        LA    R11,1              R11:=1
PRTLOOP  LR    R6,R3              i
        NR    R6,R10             i&0x7
        LR    R1,R8              0x80
        SRL   R1,0(R6)           0x80>>(i&0x7)
        LR    R6,R3              i
        SRL   R6,3               i>>3
        IC    R0,0(R6,R9)        prime[i>>3]
        NR    R0,R1              prime[i>>3] & (0x80>>(i&0x7))
        BE    PRTLOOPC           if = not, continue
        LR    R1,R3              R1:=i
        SLA   R1,1               R1:=2*i
        AR    R1,R11             R1:=1+2*i
        BAL   R14,OINT10         and print F(10)
        AR    R2,R11             np+=1
        C     R2,=F'10'          check whether = 10
        BNZ   PRTLOOPC           if != not, continue
        BAL   R14,OPUTLINE       write line
        XR    R2,R2              np=0
PRTLOOPC EQU   *
        BXLE  R3,R4,PRTLOOP
*
        LTR   R2,R2              check prime count np
        BZ    NOPRNT
        BAL   R14,OPUTLINE       write line
NOPRNT   EQU   *
*
* print primes count --------------------------------------------------
*   loop:  ind  R3   i
*          inc  R4   1
*          lim  R5   imax
*               R2   np
*               R7   il
*               R6   nl
*               R9   &prime
*               R8   0x80   
*               R10  0x07
*               R11  1
*
TBL      EQU   *
        LA    R2,1               np=1
        LA    R7,4               il=4
        LA    R6,10              nl=10
        LA    R3,1               ind: R3:=1
        LA    R4,1               inc: R4:=1
        L     R5,BIMAX           lim: R5:=BIMAX
        LA    R8,X'80'           R8:=0x80
        LA    R10,X'07'          R10:=0x07
        LA    R11,1              R11:=1
TBLLOOP  LR    R15,R3             i
        NR    R15,R10            i&0x7
        LR    R1,R8              0x80
        SRL   R1,0(R15)          0x80>>(i&0x7)
        LR    R15,R3             i
        SRL   R15,3              i>>3
        IC    R0,0(R15,R9)       prime[i>>3]
        NR    R0,R1              prime[i>>3] & (1<<(i&0x7))
        BE    NOPRIME            if = not
        AR    R2,R11             np+= 1
NOPRIME  CR    R3,R7              test i != il
        BNE   TBLLOOPC
        LR    R6,R7              nl=il
        SLA   R6,1               nl=2*il
        A     R6,=F'2'           nl=2+2*il
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        LR    R1,R6
        BAL   R14,OINT10         print nl
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
        LR    R1,R7              R1:=il
        AR    R1,R11             R1:=il+1
        M     R0,=F'10'          R1:=10*(il+1)
        SR    R1,R11             R1:=10*(il+1)-1
        LR    R7,R1              update il
*
TBLLOOPC EQU   *
        BXLE  R3,R4,TBLLOOP
*
        C     R6,NMAX            is nl != nmax ?
        BE    TBLNOTR            if = not, skip extra summary
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "pi(...."
        L     R1,NMAX
        BAL   R14,OINT10         print nmax
        L     R1,MSGPISEP
        BAL   R14,OTEXT          print "):..."
        LR    R1,R2
        BAL   R14,OINT10         print np
        BAL   R14,OPUTLINE       write line
*
TBLNOTR  EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
NMAX     DC    F'10000000'        highest prime to search for
NMSQRT   DS    F                  sqrt(NMAX)
BIMAX    DS    F                  highest prime array bit index
WIMAX    DS    F                  highest prime array word index
PRIME    DS    F                  prime array pointer
PRNT     DC    X'00'              print enable flag
*
* message strings
*
MSGPERR  OTXTDSC C'NMAX must be >= 10 and <= 100000000, abort'
MSGLIST  OTXTDSC C'List of Primes up to '
MSGPI    OTXTDSC C'pi('
MSGPISEP OTXTDSC C'): '
*
* spill literal pool
*
        LTORG
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEQGCCF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=7000K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: soeq_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.1.1  change (n-1)/2 --> n/2                  */
/* 2017-11-20   966   1.1    add LOOKUP,STATISTICS ifdefs            */
/* 2017-11-17   962   1.0    Initial version                         */
/* 2017-10-15   956   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* #define LOOKUP     */
/* #define STATISTICS */

#ifdef LOOKUP
#define TSTMASK(ind) tstmask[ind]
#define CLRMASK(ind) clrmask[ind]
const unsigned char tstmask[] = {0x01,0x02,0x04,0x08,
                                0x10,0x20,0x40,0x80};
const unsigned char clrmask[] = {0xfe,0xfd,0xfb,0xf7,
                                0xef,0xdf,0xbf,0x7f};

#else
#define TSTMASK(ind)  (1<<(ind))
#define CLRMASK(ind) ~(1<<(ind))
#endif

#ifdef STATISTICS
#define SCOUNT(var)  var += 1;
double StatOloop = 0.;
double StatIloop = 0.;
#else
#define SCOUNT(var)
#endif


int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int bimax;
 int wimax;
 int i,n;
 int np,il,nl;
 unsigned char *prime;
 unsigned char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt = sqrt((double)nmax);
 bimax  = (nmax-1)/2;
 wimax  = (bimax+7)/8;
 prime  = malloc(sizeof(char)*(wimax+1));       /* need [1,...,wimax] */
 pmax   = &prime[wimax];

 for (p=prime; p<=pmax;) *p++ = 0xff;

 for (n=3; n<=nmsqrt; n+=2) {
   i = n/2;
   if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
   SCOUNT(StatOloop);
   for (i=(n*n)/2; i<=bimax ; i+=n) {
     prime[i>>3] &= CLRMASK(i&0x7);
     SCOUNT(StatIloop);
   }
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=bimax;i++) {
     if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=bimax;i++) {
   if ((prime[i>>3] & TSTMASK(i&0x7))) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

#ifdef STATISTICS
 printf("StatOloop: %20.0f\n",StatOloop);
 printf("StatIloop: %20.0f\n",StatIloop);
#endif

return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
100000000         0
/*
//
./        ADD   NAME=SOEQGCCP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: soeq_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.1.1  change (n-1)/2 --> n/2                  */
/* 2017-11-20   966   1.1    add LOOKUP,STATISTICS ifdefs            */
/* 2017-11-17   962   1.0    Initial version                         */
/* 2017-10-15   956   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* #define LOOKUP     */
/* #define STATISTICS */

#ifdef LOOKUP
#define TSTMASK(ind) tstmask[ind]
#define CLRMASK(ind) clrmask[ind]
const unsigned char tstmask[] = {0x01,0x02,0x04,0x08,
                                0x10,0x20,0x40,0x80};
const unsigned char clrmask[] = {0xfe,0xfd,0xfb,0xf7,
                                0xef,0xdf,0xbf,0x7f};

#else
#define TSTMASK(ind)  (1<<(ind))
#define CLRMASK(ind) ~(1<<(ind))
#endif

#ifdef STATISTICS
#define SCOUNT(var)  var += 1;
double StatOloop = 0.;
double StatIloop = 0.;
#else
#define SCOUNT(var)
#endif


int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int bimax;
 int wimax;
 int i,n;
 int np,il,nl;
 unsigned char *prime;
 unsigned char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt = sqrt((double)nmax);
 bimax  = (nmax-1)/2;
 wimax  = (bimax+7)/8;
 prime  = malloc(sizeof(char)*(wimax+1));       /* need [1,...,wimax] */
 pmax   = &prime[wimax];

 for (p=prime; p<=pmax;) *p++ = 0xff;

 for (n=3; n<=nmsqrt; n+=2) {
   i = n/2;
   if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
   SCOUNT(StatOloop);
   for (i=(n*n)/2; i<=bimax ; i+=n) {
     prime[i>>3] &= CLRMASK(i&0x7);
     SCOUNT(StatIloop);
   }
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=bimax;i++) {
     if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=bimax;i++) {
   if ((prime[i>>3] & TSTMASK(i&0x7))) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

#ifdef STATISTICS
 printf("StatOloop: %20.0f\n",StatOloop);
 printf("StatIloop: %20.0f\n",StatIloop);
#endif

return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEQGCCT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=6000K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: soeq_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.1.1  change (n-1)/2 --> n/2                  */
/* 2017-11-20   966   1.1    add LOOKUP,STATISTICS ifdefs            */
/* 2017-11-17   962   1.0    Initial version                         */
/* 2017-10-15   956   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* #define LOOKUP     */
/* #define STATISTICS */

#ifdef LOOKUP
#define TSTMASK(ind) tstmask[ind]
#define CLRMASK(ind) clrmask[ind]
const unsigned char tstmask[] = {0x01,0x02,0x04,0x08,
                                0x10,0x20,0x40,0x80};
const unsigned char clrmask[] = {0xfe,0xfd,0xfb,0xf7,
                                0xef,0xdf,0xbf,0x7f};

#else
#define TSTMASK(ind)  (1<<(ind))
#define CLRMASK(ind) ~(1<<(ind))
#endif

#ifdef STATISTICS
#define SCOUNT(var)  var += 1;
double StatOloop = 0.;
double StatIloop = 0.;
#else
#define SCOUNT(var)
#endif


int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int bimax;
 int wimax;
 int i,n;
 int np,il,nl;
 unsigned char *prime;
 unsigned char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt = sqrt((double)nmax);
 bimax  = (nmax-1)/2;
 wimax  = (bimax+7)/8;
 prime  = malloc(sizeof(char)*(wimax+1));       /* need [1,...,wimax] */
 pmax   = &prime[wimax];

 for (p=prime; p<=pmax;) *p++ = 0xff;

 for (n=3; n<=nmsqrt; n+=2) {
   i = n/2;
   if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
   SCOUNT(StatOloop);
   for (i=(n*n)/2; i<=bimax ; i+=n) {
     prime[i>>3] &= CLRMASK(i&0x7);
     SCOUNT(StatIloop);
   }
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=bimax;i++) {
     if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=bimax;i++) {
   if ((prime[i>>3] & TSTMASK(i&0x7))) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

#ifdef STATISTICS
 printf("StatOloop: %20.0f\n",StatOloop);
 printf("StatIloop: %20.0f\n",StatIloop);
#endif

return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEQJCCF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=7000K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: soeq_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.1.1  change (n-1)/2 --> n/2                  */
/* 2017-11-20   966   1.1    add LOOKUP,STATISTICS ifdefs            */
/* 2017-11-17   962   1.0    Initial version                         */
/* 2017-10-15   956   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* #define LOOKUP     */
/* #define STATISTICS */

#ifdef LOOKUP
#define TSTMASK(ind) tstmask[ind]
#define CLRMASK(ind) clrmask[ind]
const unsigned char tstmask[] = {0x01,0x02,0x04,0x08,
                                0x10,0x20,0x40,0x80};
const unsigned char clrmask[] = {0xfe,0xfd,0xfb,0xf7,
                                0xef,0xdf,0xbf,0x7f};

#else
#define TSTMASK(ind)  (1<<(ind))
#define CLRMASK(ind) ~(1<<(ind))
#endif

#ifdef STATISTICS
#define SCOUNT(var)  var += 1;
double StatOloop = 0.;
double StatIloop = 0.;
#else
#define SCOUNT(var)
#endif


int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int bimax;
 int wimax;
 int i,n;
 int np,il,nl;
 unsigned char *prime;
 unsigned char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt = sqrt((double)nmax);
 bimax  = (nmax-1)/2;
 wimax  = (bimax+7)/8;
 prime  = malloc(sizeof(char)*(wimax+1));       /* need [1,...,wimax] */
 pmax   = &prime[wimax];

 for (p=prime; p<=pmax;) *p++ = 0xff;

 for (n=3; n<=nmsqrt; n+=2) {
   i = n/2;
   if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
   SCOUNT(StatOloop);
   for (i=(n*n)/2; i<=bimax ; i+=n) {
     prime[i>>3] &= CLRMASK(i&0x7);
     SCOUNT(StatIloop);
   }
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=bimax;i++) {
     if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=bimax;i++) {
   if ((prime[i>>3] & TSTMASK(i&0x7))) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

#ifdef STATISTICS
 printf("StatOloop: %20.0f\n",StatOloop);
 printf("StatIloop: %20.0f\n",StatIloop);
#endif

return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
100000000         0
/*
//
./        ADD   NAME=SOEQJCCP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: soeq_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.1.1  change (n-1)/2 --> n/2                  */
/* 2017-11-20   966   1.1    add LOOKUP,STATISTICS ifdefs            */
/* 2017-11-17   962   1.0    Initial version                         */
/* 2017-10-15   956   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* #define LOOKUP     */
/* #define STATISTICS */

#ifdef LOOKUP
#define TSTMASK(ind) tstmask[ind]
#define CLRMASK(ind) clrmask[ind]
const unsigned char tstmask[] = {0x01,0x02,0x04,0x08,
                                0x10,0x20,0x40,0x80};
const unsigned char clrmask[] = {0xfe,0xfd,0xfb,0xf7,
                                0xef,0xdf,0xbf,0x7f};

#else
#define TSTMASK(ind)  (1<<(ind))
#define CLRMASK(ind) ~(1<<(ind))
#endif

#ifdef STATISTICS
#define SCOUNT(var)  var += 1;
double StatOloop = 0.;
double StatIloop = 0.;
#else
#define SCOUNT(var)
#endif


int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int bimax;
 int wimax;
 int i,n;
 int np,il,nl;
 unsigned char *prime;
 unsigned char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt = sqrt((double)nmax);
 bimax  = (nmax-1)/2;
 wimax  = (bimax+7)/8;
 prime  = malloc(sizeof(char)*(wimax+1));       /* need [1,...,wimax] */
 pmax   = &prime[wimax];

 for (p=prime; p<=pmax;) *p++ = 0xff;

 for (n=3; n<=nmsqrt; n+=2) {
   i = n/2;
   if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
   SCOUNT(StatOloop);
   for (i=(n*n)/2; i<=bimax ; i+=n) {
     prime[i>>3] &= CLRMASK(i&0x7);
     SCOUNT(StatIloop);
   }
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=bimax;i++) {
     if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=bimax;i++) {
   if ((prime[i>>3] & TSTMASK(i&0x7))) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

#ifdef STATISTICS
 printf("StatOloop: %20.0f\n",StatOloop);
 printf("StatIloop: %20.0f\n",StatIloop);
#endif

return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=100000
//GO.STDERR DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEQJCCT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: soeq_cc.c 972 2017-12-23 20:55:41Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-23   972   1.1.1  change (n-1)/2 --> n/2                  */
/* 2017-11-20   966   1.1    add LOOKUP,STATISTICS ifdefs            */
/* 2017-11-17   962   1.0    Initial version                         */
/* 2017-10-15   956   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* #define LOOKUP     */
/* #define STATISTICS */

#ifdef LOOKUP
#define TSTMASK(ind) tstmask[ind]
#define CLRMASK(ind) clrmask[ind]
const unsigned char tstmask[] = {0x01,0x02,0x04,0x08,
                                0x10,0x20,0x40,0x80};
const unsigned char clrmask[] = {0xfe,0xfd,0xfb,0xf7,
                                0xef,0xdf,0xbf,0x7f};

#else
#define TSTMASK(ind)  (1<<(ind))
#define CLRMASK(ind) ~(1<<(ind))
#endif

#ifdef STATISTICS
#define SCOUNT(var)  var += 1;
double StatOloop = 0.;
double StatIloop = 0.;
#else
#define SCOUNT(var)
#endif


int main() 
{
 int nmax;
 int nmsqrt;
 int prnt;
 int bimax;
 int wimax;
 int i,n;
 int np,il,nl;
 unsigned char *prime;
 unsigned char *p,*pmax;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &nmax, &prnt) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }
 if (nmax < 10) {
   printf("nmax must be >= 10, abort\n");
   return 1;
 }

 /* prime:  i=(n-1)/2 --> 3->[1], 5->[2]; ... 99-> [49]; ... */
 nmsqrt = sqrt((double)nmax);
 bimax  = (nmax-1)/2;
 wimax  = (bimax+7)/8;
 prime  = malloc(sizeof(char)*(wimax+1));       /* need [1,...,wimax] */
 pmax   = &prime[wimax];

 for (p=prime; p<=pmax;) *p++ = 0xff;

 for (n=3; n<=nmsqrt; n+=2) {
   i = n/2;
   if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
   SCOUNT(StatOloop);
   for (i=(n*n)/2; i<=bimax ; i+=n) {
     prime[i>>3] &= CLRMASK(i&0x7);
     SCOUNT(StatIloop);
   }
 }

 if (prnt) {
   printf("List of Primes up to %d\n",nmax);
   printf(" %7d",2);
   np = 1;
   for (i=1;i<=bimax;i++) {
     if ((prime[i>>3] & TSTMASK(i&0x7)) == 0) continue;
     printf(" %7d",1+2*i);
     np += 1;
     if (np != 10) continue;
     printf("\n");
     np= 0;
   }
   if (np != 0) printf("\n");
 }

 il =  4;
 nl = 10;
 np =  1;
 for (i=1;i<=bimax;i++) {
   if ((prime[i>>3] & TSTMASK(i&0x7))) np += 1;
   if (i != il) continue;
   nl =  2*il+2;
   printf("pi(%10d): %10d\n",nl,np);
   il = 10*(il+1)-1;
 }
 if (nl != nmax) printf("pi(%10d): %10d\n",nmax,np);

#ifdef STATISTICS
 printf("StatOloop: %20.0f\n",StatOloop);
 printf("StatIloop: %20.0f\n",StatIloop);
#endif

return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEQPASF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=7500K,TIME=(5,0),PRTY=2
//CLG EXEC PASCLG,GOTIME=3600,GOREG=7500K,
//      OPT='M+,D-',
//      GOPARM='/STACK=6500K'
//COMPILE.SYSIN DD *
(* $Id: soeq_pas.pas 977 2017-12-27 12:46:21Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(* Remarks:                                                          *)
(* - The MVS Compiler uses 8 bytes to represent a set, sets are      *)
(*   limited to 64 members. seoq uses therefore sets with 64 members.*)
(* - the '<=' operator is slightly faster than the 'in' operator.    *)
(* - the set '*' operator is slightly faster than the '-' operator.  *)
(* - all this leads to a slightly different implementation than the  *)
(*   one scetched in 'Pascal User Manual and Report. 2nd Edition',   *)
(*   published 1975 by Springer.                                     *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-27   977   1.1    use '<=' and '*' instead of 'in' and '-'*)
(* 2017-12-26   976   1.0    Initial version (derived from soeq_pas) *)

program soep(input,output);
type
  bits =  (b00,b01,b02,b03,b04,b05,b06,b07,
           b08,b09,b10,b11,b12,b13,b14,b15,
           b16,b17,b18,b19,b20,b21,b22,b23,
           b24,b25,b26,b27,b28,b29,b30,b31,
           b32,b33,b34,b35,b36,b37,b38,b39,
           b40,b41,b42,b43,b44,b45,b46,b47,
           b48,b49,b50,b51,b52,b53,b54,b55,
           b56,b57,b58,b59,b60,b61,b62,b63);
  bset = set of bits;
var
  nmax,prnt,imax : integer;
  nmsqrt         : integer;
  wimax,iw       : integer;
  i,n,imin       : integer;
  np,il,nl       : integer;
  rnmax          : real;
  sieve          : ARRAY[0 .. 781250] of bset;
  btst           : ARRAY[0 .. 63 ] of bset;
  bclr           : ARRAY[0 .. 63 ] of bset;
  b              : bits;
  ball           : bset;
begin

  ball := [b00 .. b63];
  b    := b00;
  btst[0] := [b];
  bclr[0] := ball - [b];
  for i := 1 to 63 do begin
     b := succ(b);
     btst[i] := [b];
     bclr[i] := ball - [b];
  end;

  read(nmax);
  read(prnt);

  if (nmax < 10) or (nmax > 100000000) then begin
     writeln(' ', 'nmax out of range (10...100000000), abort');
     exit(8);
  end;

  rnmax  := nmax;
  nmsqrt := trunc(sqrt(nmax));
  imax   := (nmax-1) div 2;
  wimax  := (imax+63) div 64;
  for i := 0 to wimax do sieve[i] := ball;

  n := 3;
  while n <= nmsqrt do begin
     i := n div 2;
     if btst[i mod 64] <= sieve[i div 64] then begin
        i := (n*n) div 2;
        while i <= imax do begin
           iw := i div 64;
           sieve[iw] := sieve[iw] * bclr[i mod 64];
           i := i + n;
        end;
     end;
     n := n + 2;
  end;

  if prnt > 0 then begin
     writeln(' ', 'List of Primes up to ', nmax:9);
     write(2:8);
     np := 1;
     for i := 1 to imax do begin
        if btst[i mod 64] <= sieve[i div 64] then begin
           write(1+2*i:8);
           np := np + 1;
           if np = 10 then begin
              writeln(' ');
              np := 0;
           end;
        end;
     end;
     if np > 0 then writeln();
  end;

  il :=  4;
  nl := 10;
  np :=  1;
  for i := 1 to imax do begin
     if btst[i mod 64] <= sieve[i div 64] then np := np + 1;
     if i = il then begin
        nl := 2*il + 2;
        writeln(' ', 'pi(', nl:9, '): ', np:9);
        il := 10*(il+1)-1;
     end;
  end;

  if nl < nmax then writeln(' ', 'pi(', nmax:9, '): ', np:9);

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
100000000         0
/*
//
./        ADD   NAME=SOEQPASP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=7500K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=7500K,
//      OPT='M+,D-',
//      GOPARM='/STACK=6500K'
//COMPILE.SYSIN DD *
(* $Id: soeq_pas.pas 977 2017-12-27 12:46:21Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(* Remarks:                                                          *)
(* - The MVS Compiler uses 8 bytes to represent a set, sets are      *)
(*   limited to 64 members. seoq uses therefore sets with 64 members.*)
(* - the '<=' operator is slightly faster than the 'in' operator.    *)
(* - the set '*' operator is slightly faster than the '-' operator.  *)
(* - all this leads to a slightly different implementation than the  *)
(*   one scetched in 'Pascal User Manual and Report. 2nd Edition',   *)
(*   published 1975 by Springer.                                     *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-27   977   1.1    use '<=' and '*' instead of 'in' and '-'*)
(* 2017-12-26   976   1.0    Initial version (derived from soeq_pas) *)

program soep(input,output);
type
  bits =  (b00,b01,b02,b03,b04,b05,b06,b07,
           b08,b09,b10,b11,b12,b13,b14,b15,
           b16,b17,b18,b19,b20,b21,b22,b23,
           b24,b25,b26,b27,b28,b29,b30,b31,
           b32,b33,b34,b35,b36,b37,b38,b39,
           b40,b41,b42,b43,b44,b45,b46,b47,
           b48,b49,b50,b51,b52,b53,b54,b55,
           b56,b57,b58,b59,b60,b61,b62,b63);
  bset = set of bits;
var
  nmax,prnt,imax : integer;
  nmsqrt         : integer;
  wimax,iw       : integer;
  i,n,imin       : integer;
  np,il,nl       : integer;
  rnmax          : real;
  sieve          : ARRAY[0 .. 781250] of bset;
  btst           : ARRAY[0 .. 63 ] of bset;
  bclr           : ARRAY[0 .. 63 ] of bset;
  b              : bits;
  ball           : bset;
begin

  ball := [b00 .. b63];
  b    := b00;
  btst[0] := [b];
  bclr[0] := ball - [b];
  for i := 1 to 63 do begin
     b := succ(b);
     btst[i] := [b];
     bclr[i] := ball - [b];
  end;

  read(nmax);
  read(prnt);

  if (nmax < 10) or (nmax > 100000000) then begin
     writeln(' ', 'nmax out of range (10...100000000), abort');
     exit(8);
  end;

  rnmax  := nmax;
  nmsqrt := trunc(sqrt(nmax));
  imax   := (nmax-1) div 2;
  wimax  := (imax+63) div 64;
  for i := 0 to wimax do sieve[i] := ball;

  n := 3;
  while n <= nmsqrt do begin
     i := n div 2;
     if btst[i mod 64] <= sieve[i div 64] then begin
        i := (n*n) div 2;
        while i <= imax do begin
           iw := i div 64;
           sieve[iw] := sieve[iw] * bclr[i mod 64];
           i := i + n;
        end;
     end;
     n := n + 2;
  end;

  if prnt > 0 then begin
     writeln(' ', 'List of Primes up to ', nmax:9);
     write(2:8);
     np := 1;
     for i := 1 to imax do begin
        if btst[i mod 64] <= sieve[i div 64] then begin
           write(1+2*i:8);
           np := np + 1;
           if np = 10 then begin
              writeln(' ');
              np := 0;
           end;
        end;
     end;
     if np > 0 then writeln();
  end;

  il :=  4;
  nl := 10;
  np :=  1;
  for i := 1 to imax do begin
     if btst[i mod 64] <= sieve[i div 64] then np := np + 1;
     if i = il then begin
        nl := 2*il + 2;
        writeln(' ', 'pi(', nl:9, '): ', np:9);
        il := 10*(il+1)-1;
     end;
  end;

  if nl < nmax then writeln(' ', 'pi(', nmax:9, '): ', np:9);

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=100000
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEQPAST,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=7500K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=7500K,
//      OPT='M+',
//      GOPARM='/STACK=6500K'
//COMPILE.SYSIN DD *
(* $Id: soeq_pas.pas 977 2017-12-27 12:46:21Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(* Remarks:                                                          *)
(* - The MVS Compiler uses 8 bytes to represent a set, sets are      *)
(*   limited to 64 members. seoq uses therefore sets with 64 members.*)
(* - the '<=' operator is slightly faster than the 'in' operator.    *)
(* - the set '*' operator is slightly faster than the '-' operator.  *)
(* - all this leads to a slightly different implementation than the  *)
(*   one scetched in 'Pascal User Manual and Report. 2nd Edition',   *)
(*   published 1975 by Springer.                                     *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-27   977   1.1    use '<=' and '*' instead of 'in' and '-'*)
(* 2017-12-26   976   1.0    Initial version (derived from soeq_pas) *)

program soep(input,output);
type
  bits =  (b00,b01,b02,b03,b04,b05,b06,b07,
           b08,b09,b10,b11,b12,b13,b14,b15,
           b16,b17,b18,b19,b20,b21,b22,b23,
           b24,b25,b26,b27,b28,b29,b30,b31,
           b32,b33,b34,b35,b36,b37,b38,b39,
           b40,b41,b42,b43,b44,b45,b46,b47,
           b48,b49,b50,b51,b52,b53,b54,b55,
           b56,b57,b58,b59,b60,b61,b62,b63);
  bset = set of bits;
var
  nmax,prnt,imax : integer;
  nmsqrt         : integer;
  wimax,iw       : integer;
  i,n,imin       : integer;
  np,il,nl       : integer;
  rnmax          : real;
  sieve          : ARRAY[0 .. 781250] of bset;
  btst           : ARRAY[0 .. 63 ] of bset;
  bclr           : ARRAY[0 .. 63 ] of bset;
  b              : bits;
  ball           : bset;
begin

  ball := [b00 .. b63];
  b    := b00;
  btst[0] := [b];
  bclr[0] := ball - [b];
  for i := 1 to 63 do begin
     b := succ(b);
     btst[i] := [b];
     bclr[i] := ball - [b];
  end;

  read(nmax);
  read(prnt);

  if (nmax < 10) or (nmax > 100000000) then begin
     writeln(' ', 'nmax out of range (10...100000000), abort');
     exit(8);
  end;

  rnmax  := nmax;
  nmsqrt := trunc(sqrt(nmax));
  imax   := (nmax-1) div 2;
  wimax  := (imax+63) div 64;
  for i := 0 to wimax do sieve[i] := ball;

  n := 3;
  while n <= nmsqrt do begin
     i := n div 2;
     if btst[i mod 64] <= sieve[i div 64] then begin
        i := (n*n) div 2;
        while i <= imax do begin
           iw := i div 64;
           sieve[iw] := sieve[iw] * bclr[i mod 64];
           i := i + n;
        end;
     end;
     n := n + 2;
  end;

  if prnt > 0 then begin
     writeln(' ', 'List of Primes up to ', nmax:9);
     write(2:8);
     np := 1;
     for i := 1 to imax do begin
        if btst[i mod 64] <= sieve[i div 64] then begin
           write(1+2*i:8);
           np := np + 1;
           if np = 10 then begin
              writeln(' ');
              np := 0;
           end;
        end;
     end;
     if np > 0 then writeln();
  end;

  il :=  4;
  nl := 10;
  np :=  1;
  for i := 1 to imax do begin
     if btst[i mod 64] <= sieve[i div 64] then np := np + 1;
     if i = il then begin
        nl := 2*il + 2;
        writeln(' ', 'pi(', nl:9, '): ', np:9);
        il := 10*(il+1)-1;
     end;
  end;

  if nl < nmax then writeln(' ', 'pi(', nmax:9, '): ', np:9);

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=SOEQPLIF,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=2200K,TIME=(5,0),PRTY=2
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: soeq_pli.pli 976 2017-12-26 15:35:59Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-26   976   1.3    rename to SOEQ; go for max PRIME size   */
/* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        */
/* 2017-12-25   974   1.1    use 2-dim PRIME array                   */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-09-17   951   1.0    Initial version                         */
/* 2017-09-01   945   0.1    First draft                             */

 SOEQ: PROC OPTIONS(MAIN) REORDER;
   DCL (NMAX,PRNT,IMAX)  BIN FIXED(31) INIT(0);
   DCL (NMSQRT)          BIN FIXED(31) INIT(0);
   DCL (I,N,IMIN)        BIN FIXED(31) INIT(0);
   DCL (NP,IL,NL)        BIN FIXED(31) INIT(0);
   /* in PL/I(F) V5.5 array bounds are BIN(15) ! limited to 32k !!   */
   /* And the maximal aggregate size is 2 MByte !! BIT arrays are    */
   /* bit packed, so maximal total bit array size is 16 M entries.   */
   /* So go for a 2-dimensional array; 15626*1024 = 16001024;        */
   /* use 0 as lower bound to make index calculations easy:          */
   /*   PRIME(I) turns into PRIME(I/1024,MOD(I,1024))                */
   DCL PRIME(0:15625,0:1023)  BIT(1);

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(NMAX,PRNT) (F(10),F(10));

   /*IF NMAX < 10 | NMAX  > 32000000 THEN DO;*/
   IF NMAX  > 32000000 THEN DO;
     PUT SKIP EDIT('nmax out of range (10...32000000), abort') (A);
     GOTO DONE;
   END;

   NMSQRT = FLOOR(SQRT(NMAX));
   IMAX = (NMAX-1)/2;

   DO I=1 TO IMAX;
     PRIME(I/1024,MOD(I,1024)) = '1'B;
   END;

   DO N=3 TO NMSQRT BY 2;
     I  = N/2;
     IF PRIME(I/1024,MOD(I,1024)) THEN DO;
       IMIN = N*N/2;
       DO I=IMIN TO IMAX BY N;
         PRIME(I/1024,MOD(I,1024)) = '0'B;
       END;
     END;
   END;

   IF PRNT > 0 THEN DO;
     PUT SKIP EDIT('List of Primes up to ',NMAX) (A,F(8));
     PUT SKIP EDIT(' ',2) (A,F(7));
     NP = 1;
     DO I=1 TO IMAX;
       IF PRIME(I/1024,MOD(I,1024)) THEN DO;
         PUT EDIT(' ',1+2*I) (A,F(7));
         NP = NP + 1;
         IF NP = 10 THEN DO;
           PUT SKIP;
           NP = 0;
         END;
       END;
     END;
     IF NP > 0 THEN PUT SKIP;
   END;

   IL =  4;
   NL = 10;
   NP =  1;
   DO I=1 TO IMAX;
     IF PRIME(I/1024,MOD(I,1024)) THEN NP = NP + 1;
     IF I = IL THEN DO;
       NL = 2*IL + 2;
       PUT SKIP EDIT('pi(',NL,'): ',NP) (A,F(8),A,F(8));
       IL = 10*(IL+1)-1;
     END;
   END;

   IF NL < NMAX THEN PUT SKIP EDIT('pi(',NMAX,'): ',NP)
                                  (A,F(8),A,F(8));

   DONE:;

 END SOEQ;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
 32000000         0
/*
//
./        ADD   NAME=SOEQPLIP,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=2200K,TIME=(2,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: soeq_pli.pli 976 2017-12-26 15:35:59Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-26   976   1.3    rename to SOEQ; go for max PRIME size   */
/* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        */
/* 2017-12-25   974   1.1    use 2-dim PRIME array                   */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-09-17   951   1.0    Initial version                         */
/* 2017-09-01   945   0.1    First draft                             */

 SOEQ: PROC OPTIONS(MAIN) REORDER;
   DCL (NMAX,PRNT,IMAX)  BIN FIXED(31) INIT(0);
   DCL (NMSQRT)          BIN FIXED(31) INIT(0);
   DCL (I,N,IMIN)        BIN FIXED(31) INIT(0);
   DCL (NP,IL,NL)        BIN FIXED(31) INIT(0);
   /* in PL/I(F) V5.5 array bounds are BIN(15) ! limited to 32k !!   */
   /* And the maximal aggregate size is 2 MByte !! BIT arrays are    */
   /* bit packed, so maximal total bit array size is 16 M entries.   */
   /* So go for a 2-dimensional array; 15626*1024 = 16001024;        */
   /* use 0 as lower bound to make index calculations easy:          */
   /*   PRIME(I) turns into PRIME(I/1024,MOD(I,1024))                */
   DCL PRIME(0:15625,0:1023)  BIT(1);

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(NMAX,PRNT) (F(10),F(10));

   /*IF NMAX < 10 | NMAX  > 32000000 THEN DO;*/
   IF NMAX  > 32000000 THEN DO;
     PUT SKIP EDIT('nmax out of range (10...32000000), abort') (A);
     GOTO DONE;
   END;

   NMSQRT = FLOOR(SQRT(NMAX));
   IMAX = (NMAX-1)/2;

   DO I=1 TO IMAX;
     PRIME(I/1024,MOD(I,1024)) = '1'B;
   END;

   DO N=3 TO NMSQRT BY 2;
     I  = N/2;
     IF PRIME(I/1024,MOD(I,1024)) THEN DO;
       IMIN = N*N/2;
       DO I=IMIN TO IMAX BY N;
         PRIME(I/1024,MOD(I,1024)) = '0'B;
       END;
     END;
   END;

   IF PRNT > 0 THEN DO;
     PUT SKIP EDIT('List of Primes up to ',NMAX) (A,F(8));
     PUT SKIP EDIT(' ',2) (A,F(7));
     NP = 1;
     DO I=1 TO IMAX;
       IF PRIME(I/1024,MOD(I,1024)) THEN DO;
         PUT EDIT(' ',1+2*I) (A,F(7));
         NP = NP + 1;
         IF NP = 10 THEN DO;
           PUT SKIP;
           NP = 0;
         END;
       END;
     END;
     IF NP > 0 THEN PUT SKIP;
   END;

   IL =  4;
   NL = 10;
   NP =  1;
   DO I=1 TO IMAX;
     IF PRIME(I/1024,MOD(I,1024)) THEN NP = NP + 1;
     IF I = IL THEN DO;
       NL = 2*IL + 2;
       PUT SKIP EDIT('pi(',NL,'): ',NP) (A,F(8),A,F(8));
       IL = 10*(IL+1)-1;
     END;
   END;

   IF NL < NMAX THEN PUT SKIP EDIT('pi(',NMAX,'): ',NP)
                                  (A,F(8),A,F(8));

   DONE:;

 END SOEQ;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
 10000000         1
/*
//
./        ADD   NAME=SOEQPLIT,LEVEL=00,SOURCE=0,LIST=ALL
//SOEQ#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=2200K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: soeq_pli.pli 976 2017-12-26 15:35:59Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-26   976   1.3    rename to SOEQ; go for max PRIME size   */
/* 2017-12-25   975   1.2    use sqrt(nmax) as outer loop end        */
/* 2017-12-25   974   1.1    use 2-dim PRIME array                   */
/* 2017-12-23   972   1.0.1  change (n-1)/2 --> n/2                  */
/* 2017-09-17   951   1.0    Initial version                         */
/* 2017-09-01   945   0.1    First draft                             */

 SOEQ: PROC OPTIONS(MAIN) REORDER;
   DCL (NMAX,PRNT,IMAX)  BIN FIXED(31) INIT(0);
   DCL (NMSQRT)          BIN FIXED(31) INIT(0);
   DCL (I,N,IMIN)        BIN FIXED(31) INIT(0);
   DCL (NP,IL,NL)        BIN FIXED(31) INIT(0);
   /* in PL/I(F) V5.5 array bounds are BIN(15) ! limited to 32k !!   */
   /* And the maximal aggregate size is 2 MByte !! BIT arrays are    */
   /* bit packed, so maximal total bit array size is 16 M entries.   */
   /* So go for a 2-dimensional array; 15626*1024 = 16001024;        */
   /* use 0 as lower bound to make index calculations easy:          */
   /*   PRIME(I) turns into PRIME(I/1024,MOD(I,1024))                */
   DCL PRIME(0:15625,0:1023)  BIT(1);

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(NMAX,PRNT) (F(10),F(10));

   /*IF NMAX < 10 | NMAX  > 32000000 THEN DO;*/
   IF NMAX  > 32000000 THEN DO;
     PUT SKIP EDIT('nmax out of range (10...32000000), abort') (A);
     GOTO DONE;
   END;

   NMSQRT = FLOOR(SQRT(NMAX));
   IMAX = (NMAX-1)/2;

   DO I=1 TO IMAX;
     PRIME(I/1024,MOD(I,1024)) = '1'B;
   END;

   DO N=3 TO NMSQRT BY 2;
     I  = N/2;
     IF PRIME(I/1024,MOD(I,1024)) THEN DO;
       IMIN = N*N/2;
       DO I=IMIN TO IMAX BY N;
         PRIME(I/1024,MOD(I,1024)) = '0'B;
       END;
     END;
   END;

   IF PRNT > 0 THEN DO;
     PUT SKIP EDIT('List of Primes up to ',NMAX) (A,F(8));
     PUT SKIP EDIT(' ',2) (A,F(7));
     NP = 1;
     DO I=1 TO IMAX;
       IF PRIME(I/1024,MOD(I,1024)) THEN DO;
         PUT EDIT(' ',1+2*I) (A,F(7));
         NP = NP + 1;
         IF NP = 10 THEN DO;
           PUT SKIP;
           NP = 0;
         END;
       END;
     END;
     IF NP > 0 THEN PUT SKIP;
   END;

   IL =  4;
   NL = 10;
   NP =  1;
   DO I=1 TO IMAX;
     IF PRIME(I/1024,MOD(I,1024)) THEN NP = NP + 1;
     IF I = IL THEN DO;
       NL = 2*IL + 2;
       PUT SKIP EDIT('pi(',NL,'): ',NP) (A,F(8),A,F(8));
       IL = 10*(IL+1)-1;
     END;
   END;

   IF NL < NMAX THEN PUT SKIP EDIT('pi(',NMAX,'): ',NP)
                                  (A,F(8),A,F(8));

   DONE:;

 END SOEQ;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
   100000         1
/*
//
./        ADD   NAME=TOWHA60F,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(10,0),PRTY=2
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: towh_a60.a60 964 2017-11-19 08:47:46Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-09-05   946   1.0    Initial version
*;

 'INTEGER' NCALL,NMOVE;
 'INTEGER' CURSTK,MAXSTK;
 'INTEGER' MAXDSK,TRACE;
 'INTEGER' 'ARRAY' TOW[1:3];
 'INTEGER' NDSK;

 'PROCEDURE' TRC(TXT,N,F,T);
   'STRING'  TXT;
   'VALUE'   N,F,T;
   'INTEGER' N,F,T;
 'BEGIN'
   OUTSTRING (1, TXT);
   OUTINTEGER(1, NDSK);
   OUTSTRING (1,'(' :')');
   OUTINTEGER(1, N);
   OUTINTEGER(1, F);
   OUTINTEGER(1, T);
   OUTSTRING (1,'(' :')');
   OUTTARRAY (1, TOW);
   SYSACT(1,14,1);
 'END';

 'PROCEDURE' MOV(N,F,T);
   'VALUE'   N,F,T;
   'INTEGER' N,F,T;
 'BEGIN'
   'INTEGER' O;
   O := 6-(F+T);
   CURSTK := CURSTK + 1;
   NCALL  := NCALL  + 1;
   'IF' CURSTK > MAXSTK 'THEN' MAXSTK := CURSTK;

   'IF' N = 1 'THEN' 'BEGIN'
     NMOVE  := NMOVE  + 1;
     TOW[F] := TOW[F] - 1;
     TOW[T] := TOW[T] + 1;
     'IF' TRACE > 0 'THEN' TRC('('mov-do: ')',N,F,T);
   'END'
   'ELSE' 'BEGIN'
     'IF' TRACE > 0 'THEN' TRC('('mov-go: ')',N,F,T);
     MOV(N-1,F,O);
     MOV(1,F,T);
     MOV(N-1,O,T);
   'END';

   CURSTK := CURSTK - 1;
 'END';

 'COMMENT' set record lenth = 132 and page length = 62;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,MAXDSK);
 ININTEGER(0,TRACE);

 'FOR' NDSK := 2 'STEP' 1 'UNTIL' MAXDSK 'DO' 'BEGIN'
    NCALL  := 0;
    NMOVE  := 0;
    MAXSTK := 0;
    CURSTK := 0;
    TOW[1] := NDSK;
    TOW[2] := 0;
    TOW[3] := 0;
    'IF' TRACE > 0  'THEN' 'BEGIN'
      OUTSTRING (1,'('STRT ndsk=')');
      OUTINTEGER(1, NDSK);
      SYSACT(1,14,1);
    'END';
    MOV(NDSK,1,3);
    OUTSTRING (1,'('DONE ndsk=')');
    OUTINTEGER(1, NDSK);
    OUTSTRING (1,'(':  maxstk=')');
    OUTINTEGER(1, MAXSTK);
    OUTSTRING (1,'('   ncall=')');
    OUTINTEGER(1, NCALL);
    OUTSTRING (1,'('   nmove=')');
    OUTINTEGER(1, NMOVE);
    SYSACT(1,14,1);
 'END';

'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHA60T,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: towh_a60.a60 964 2017-11-19 08:47:46Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-09-05   946   1.0    Initial version
*;

 'INTEGER' NCALL,NMOVE;
 'INTEGER' CURSTK,MAXSTK;
 'INTEGER' MAXDSK,TRACE;
 'INTEGER' 'ARRAY' TOW[1:3];
 'INTEGER' NDSK;

 'PROCEDURE' TRC(TXT,N,F,T);
   'STRING'  TXT;
   'VALUE'   N,F,T;
   'INTEGER' N,F,T;
 'BEGIN'
   OUTSTRING (1, TXT);
   OUTINTEGER(1, NDSK);
   OUTSTRING (1,'(' :')');
   OUTINTEGER(1, N);
   OUTINTEGER(1, F);
   OUTINTEGER(1, T);
   OUTSTRING (1,'(' :')');
   OUTTARRAY (1, TOW);
   SYSACT(1,14,1);
 'END';

 'PROCEDURE' MOV(N,F,T);
   'VALUE'   N,F,T;
   'INTEGER' N,F,T;
 'BEGIN'
   'INTEGER' O;
   O := 6-(F+T);
   CURSTK := CURSTK + 1;
   NCALL  := NCALL  + 1;
   'IF' CURSTK > MAXSTK 'THEN' MAXSTK := CURSTK;

   'IF' N = 1 'THEN' 'BEGIN'
     NMOVE  := NMOVE  + 1;
     TOW[F] := TOW[F] - 1;
     TOW[T] := TOW[T] + 1;
     'IF' TRACE > 0 'THEN' TRC('('mov-do: ')',N,F,T);
   'END'
   'ELSE' 'BEGIN'
     'IF' TRACE > 0 'THEN' TRC('('mov-go: ')',N,F,T);
     MOV(N-1,F,O);
     MOV(1,F,T);
     MOV(N-1,O,T);
   'END';

   CURSTK := CURSTK - 1;
 'END';

 'COMMENT' set record lenth = 132 and page length = 62;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,MAXDSK);
 ININTEGER(0,TRACE);

 'FOR' NDSK := 2 'STEP' 1 'UNTIL' MAXDSK 'DO' 'BEGIN'
    NCALL  := 0;
    NMOVE  := 0;
    MAXSTK := 0;
    CURSTK := 0;
    TOW[1] := NDSK;
    TOW[2] := 0;
    TOW[3] := 0;
    'IF' TRACE > 0  'THEN' 'BEGIN'
      OUTSTRING (1,'('STRT ndsk=')');
      OUTINTEGER(1, NDSK);
      SYSACT(1,14,1);
    'END';
    MOV(NDSK,1,3);
    OUTSTRING (1,'('DONE ndsk=')');
    OUTINTEGER(1, NDSK);
    OUTSTRING (1,'(':  maxstk=')');
    OUTINTEGER(1, MAXSTK);
    OUTSTRING (1,'('   ncall=')');
    OUTINTEGER(1, NCALL);
    OUTSTRING (1,'('   nmove=')');
    OUTINTEGER(1, NMOVE);
    SYSACT(1,14,1);
 'END';

'END'
/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHASMF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(10,0),PRTY=2
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NOLIST,NOXREF,NORLD,NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: towh_asm.asm 968 2017-12-03 16:58:43Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-11-12   961   1.0    Initial version
* 2017-10-10   955   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Tower of Hanoi
*   RC =  0  ok
*   RC =  4  MAXDSK out of range
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        L     R15,=A(STACK)      R15 := current save area
        ST    R13,4(R15)         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LR    R13,R15            setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT05         get NMAX
        ST    R1,MAXDSK
        BAL   R14,IINT05         get PRNT
        STC   R1,TRACE
*
        L     R1,MAXDSK
        C     R1,=F'2'           is MAXDSK >= 2
        BL    MAXDBAD            if < not
        C     R1,=F'30'          is MAXDSK <= 30
        BNH   MAXDOK             if <= yes
MAXDBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
MAXDOK   EQU   *         
*
* outer loop over ndsk cases ------------------------------------------
*
DLOOP    XR    R2,R2              R2 := 0
        ST    R2,NCALL           ncall = 0
        ST    R2,NMOVE           nmove = 0
        ST    R2,MAXSTK          maxstk = 0
        ST    R2,CURSTK          curstk = 0
        L     R3,NDSK
        ST    R3,TOW+4           tow[1] = ndsk
        ST    R2,TOW+8           tow[2] = 0
        ST    R2,TOW+12          tow[3] = 0
*
        CLI   TRACE,X'00'        trace enabled ?
        BE    NOTRCLP
        L     R1,MSGSTRT
        BAL   R14,OTEXT          print "STRT..."
        LR    R1,R3
        BAL   R14,OINT04         print ndsk
        BAL   R14,OPUTLINE       write line
NOTRCLP  EQU   *
*
        LR    R0,R3
        LA    R1,1
        LA    R2,3
        LA    R15,MOV
        BALR  R14,R15            mov(ndsk,1,3)
*
        L     R1,MSGDONE
        BAL   R14,OTEXT          print "DONE..."
        L     R1,NDSK
        BAL   R14,OINT04         print ndsk
        L     R1,MSGDOSTK
        BAL   R14,OTEXT          print "  maxstk..."
        L     R1,MAXSTK
        BAL   R14,OINT04         print maxstk
        L     R1,MSGDONCA
        BAL   R14,OTEXT          print "  ncall..."
        L     R1,NCALL
        BAL   R14,OINT10         print ncall
        L     R1,MSGDONMO
        BAL   R14,OTEXT          print "  nmove..."
        L     R1,NMOVE
        BAL   R14,OINT10         print nmove
        BAL   R14,OPUTLINE       write line
*
        L     R1,NDSK            R1 := ndsk
        LA    R1,1(R1)           R1 := ndsk + 1
        C     R1,MAXDSK          is ndsk+1 <= maxdsk
        ST    R1,NDSK            ndsk++
        BNH   DLOOP              if <= yes, go for next size
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R15,=A(STACK)
        L     R13,4(R15)         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* mov function (called recursively) -----------------------------------
*
*   mov(n,f,t)
*   Register usage
*     R0    n    (input)
*     R1    f    (input)
*     R2    t    (input)
*     R3    copy of n
*     R4    copy of f
*     R5    copy of t
*     R6    work register
*     R7    work register
*     R8    constant 1  (used often)
*     R9    used as linkage for MOVTRC
*     R10   not used
*     R11   not used
*     R12   base (kept from caller !)
*
*
*
MOV      SAVE  (14,10)            Save input registers (not R11,R12) 
        LA    R15,(4*18)(R13)    R15 := current save area
        ST    R13,4(R15)         set back pointer in current save area
        LR    R3,R13             remember callers save area
        LR    R13,R15            setup current save area
        ST    R13,8(R3)          set forw pointer in callers save area
*
        LR    R3,R0              keep n
        LR    R4,R1              keep f
        LR    R5,R2              keep t
        LA    R8,1               constant 1 (often used below)
*
        L     R6,CURSTK
        AR    R6,R8
        ST    R6,CURSTK          curstk++
        C     R6,MAXSTK          is curstk > maxstk ?
        BNH   MAXSTKOK           if <= not, skip maxstk
        ST    R6,MAXSTK          maxstk = curstk
MAXSTKOK EQU   *
*
        L     R6,NCALL
        AR    R6,R8
        ST    R6,NCALL           ncall++
*
        CR    R3,R8              is n == 1 ?
        BNE   MOVGO              if != not, mov-go case
*
* mov-do case
*        
        L     R6,NMOVE
        AR    R6,R8
        ST    R6,NMOVE           nmove++
        LR    R7,R4              R7 := f
        SLA   R7,2
        L     R6,TOW(R7)         R6 := tow[f]
        SR    R6,R8
        ST    R6,TOW(R7)         tow[f]--
        LR    R7,R5              R7 := t
        SLA   R7,2
        L     R6,TOW(R7)         R6 := tow[t]
        AR    R6,R8
        ST    R6,TOW(R7)         tow[t]++
*
        CLI   TRACE,X'00'        trace enabled ?
        BE    NOTRCDO
        L     R1,MSGTRCDO
        BAL   R9,MOVTRC
NOTRCDO  EQU   *
*
        B     MOVEND
*
* mov-go case
*
MOVGO    EQU   *
        CLI   TRACE,X'00'        trace enabled ?
        BE    NOTRCGO
        L     R1,MSGTRCGO
        BAL   R9,MOVTRC
NOTRCGO  EQU   *
*
        LR    R6,R3
        SR    R6,R8              R6 := n-1
        LA    R7,6
        SR    R7,R4
        SR    R7,R5              R7 := 6-(f+t)
*
        LA    R15,MOV
        LR    R0,R6              R0 := n-1
        LR    R1,R4              R1 := f
        LR    R2,R7              R2 := o
        BALR  R14,R15            mov(n-1,f,o)
*
        LA    R0,1               R0 := 1
        LR    R1,R4              R1 := f
        LR    R2,R5              R2 := t
        BALR  R14,R15            mov(1,f,t)
*
        LR    R0,R6              R0 := n-1
        LR    R1,R7              R0 := o
        LR    R2,R5              R0 := t
        BALR  R14,R15            mov(n-1,o,t)
*
MOVEND   EQU   *
        L     R5,CURSTK
        SR    R5,R8
        ST    R5,CURSTK          curstk--
        L     R13,4(R13)         get old save area back
        RETURN (14,10),T         return to caller
*
* local print handler
* used with BAL 9, no local frame !!
*
MOVTRC   BAL   R14,OTEXT          print prefix
        L     R1,CURSTK
        BAL   R14,OINT04         print curstk
        L     R1,MSGCSEP
        BAL   R14,OTEXT          print " : "
        LR    R1,R3
        BAL   R14,OINT04         print n
        LR    R1,R4
        BAL   R14,OINT04         print f
        LR    R1,R5
        BAL   R14,OINT04         print t
        L     R1,MSGCSEP
        BAL   R14,OTEXT          print " : "
        L     R1,TOW+4
        BAL   R14,OINT04         print tow[1]
        L     R1,TOW+8
        BAL   R14,OINT04         print tow[2]
        L     R1,TOW+12
        BAL   R14,OINT04         print tow[3]
        BAL   R14,OPUTLINE       write line
        BR    R9
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
*
* OINT04 ---------------------------------------------------
*   print integer, like PL/I F(4) or C %4d format 
*   very fast, for non-negative numbers only !
*
OINT04   LA    R15,999
        CLR   R1,R15             too large ?
        BH    OINT04F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI04L,R15),OEI04   setup pattern
        ED    0(OEI04L,R15),OCVD+6  and edit
        LA    R15,OEI04L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT04F  LA    R1,4
        B     OSFILL
*
OEI04    DC    C' ',X'20',X'21',X'20'     ED pattern: bd(d
OEI04L   EQU   *-OEI04
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT05 ---------------------------------------------------
*   read integer, like PL/I F(5) or C %5d format 
*
IINT05   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(5,R15)   pack next 5 char
        CVB   R1,ICVB            and convert
        LA    R15,5(R15)         push pointer by 5 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
RC       DC    F'0'               return code
MAXDSK   DC    F'10'              maximal number of disks
TRACE    DC    X'00'              trace enable flag
*
NDSK     DC    F'2'
NCALL    DC    F'0'
NMOVE    DC    F'0'
MAXSTK   DC    F'0'
CURSTK   DC    F'0'
TOW      DC    4F'0'
*
* message strings
*
MSGPERR  OTXTDSC C'maxdsk out of range (2...30), abort'
MSGSTRT  OTXTDSC C'STRT ndsk='
MSGDONE  OTXTDSC C'DONE ndsk='
MSGDOSTK OTXTDSC C':  maxstk='
MSGDONCA OTXTDSC C'  ncall='
MSGDONMO OTXTDSC C'  nmove='
MSGTRCDO OTXTDSC C'mov-do: '
MSGTRCGO OTXTDSC C'mov-go: '
MSGCSEP  OTXTDSC C' : '
*
* spill literal pool
*
        LTORG
*
* Place the STACK in separate CSECT. Is quite large (~2 kByte)
*
DATA     CSECT
STACK    DS    (32*18)F           save area STACK
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHASMT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: towh_asm.asm 968 2017-12-03 16:58:43Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-11-12   961   1.0    Initial version
* 2017-10-10   955   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Tower of Hanoi
*   RC =  0  ok
*   RC =  4  MAXDSK out of range
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        L     R15,=A(STACK)      R15 := current save area
        ST    R13,4(R15)         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LR    R13,R15            setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
* read input parameters, and check range ------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT05         get NMAX
        ST    R1,MAXDSK
        BAL   R14,IINT05         get PRNT
        STC   R1,TRACE
*
        L     R1,MAXDSK
        C     R1,=F'2'           is MAXDSK >= 2
        BL    MAXDBAD            if < not
        C     R1,=F'30'          is MAXDSK <= 30
        BNH   MAXDOK             if <= yes
MAXDBAD  L     R1,MSGPERR
        BAL   R14,OTEXT          print error
        BAL   R14,OPUTLINE       write line
        MVI   RC+3,X'04'
        B     EXIT               quit with RC=4
MAXDOK   EQU   *         
*
* outer loop over ndsk cases ------------------------------------------
*
DLOOP    XR    R2,R2              R2 := 0
        ST    R2,NCALL           ncall = 0
        ST    R2,NMOVE           nmove = 0
        ST    R2,MAXSTK          maxstk = 0
        ST    R2,CURSTK          curstk = 0
        L     R3,NDSK
        ST    R3,TOW+4           tow[1] = ndsk
        ST    R2,TOW+8           tow[2] = 0
        ST    R2,TOW+12          tow[3] = 0
*
        CLI   TRACE,X'00'        trace enabled ?
        BE    NOTRCLP
        L     R1,MSGSTRT
        BAL   R14,OTEXT          print "STRT..."
        LR    R1,R3
        BAL   R14,OINT04         print ndsk
        BAL   R14,OPUTLINE       write line
NOTRCLP  EQU   *
*
        LR    R0,R3
        LA    R1,1
        LA    R2,3
        LA    R15,MOV
        BALR  R14,R15            mov(ndsk,1,3)
*
        L     R1,MSGDONE
        BAL   R14,OTEXT          print "DONE..."
        L     R1,NDSK
        BAL   R14,OINT04         print ndsk
        L     R1,MSGDOSTK
        BAL   R14,OTEXT          print "  maxstk..."
        L     R1,MAXSTK
        BAL   R14,OINT04         print maxstk
        L     R1,MSGDONCA
        BAL   R14,OTEXT          print "  ncall..."
        L     R1,NCALL
        BAL   R14,OINT10         print ncall
        L     R1,MSGDONMO
        BAL   R14,OTEXT          print "  nmove..."
        L     R1,NMOVE
        BAL   R14,OINT10         print nmove
        BAL   R14,OPUTLINE       write line
*
        L     R1,NDSK            R1 := ndsk
        LA    R1,1(R1)           R1 := ndsk + 1
        C     R1,MAXDSK          is ndsk+1 <= maxdsk
        ST    R1,NDSK            ndsk++
        BNH   DLOOP              if <= yes, go for next size
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R15,=A(STACK)
        L     R13,4(R15)         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* mov function (called recursively) -----------------------------------
*
*   mov(n,f,t)
*   Register usage
*     R0    n    (input)
*     R1    f    (input)
*     R2    t    (input)
*     R3    copy of n
*     R4    copy of f
*     R5    copy of t
*     R6    work register
*     R7    work register
*     R8    constant 1  (used often)
*     R9    used as linkage for MOVTRC
*     R10   not used
*     R11   not used
*     R12   base (kept from caller !)
*
*
*
MOV      SAVE  (14,10)            Save input registers (not R11,R12) 
        LA    R15,(4*18)(R13)    R15 := current save area
        ST    R13,4(R15)         set back pointer in current save area
        LR    R3,R13             remember callers save area
        LR    R13,R15            setup current save area
        ST    R13,8(R3)          set forw pointer in callers save area
*
        LR    R3,R0              keep n
        LR    R4,R1              keep f
        LR    R5,R2              keep t
        LA    R8,1               constant 1 (often used below)
*
        L     R6,CURSTK
        AR    R6,R8
        ST    R6,CURSTK          curstk++
        C     R6,MAXSTK          is curstk > maxstk ?
        BNH   MAXSTKOK           if <= not, skip maxstk
        ST    R6,MAXSTK          maxstk = curstk
MAXSTKOK EQU   *
*
        L     R6,NCALL
        AR    R6,R8
        ST    R6,NCALL           ncall++
*
        CR    R3,R8              is n == 1 ?
        BNE   MOVGO              if != not, mov-go case
*
* mov-do case
*        
        L     R6,NMOVE
        AR    R6,R8
        ST    R6,NMOVE           nmove++
        LR    R7,R4              R7 := f
        SLA   R7,2
        L     R6,TOW(R7)         R6 := tow[f]
        SR    R6,R8
        ST    R6,TOW(R7)         tow[f]--
        LR    R7,R5              R7 := t
        SLA   R7,2
        L     R6,TOW(R7)         R6 := tow[t]
        AR    R6,R8
        ST    R6,TOW(R7)         tow[t]++
*
        CLI   TRACE,X'00'        trace enabled ?
        BE    NOTRCDO
        L     R1,MSGTRCDO
        BAL   R9,MOVTRC
NOTRCDO  EQU   *
*
        B     MOVEND
*
* mov-go case
*
MOVGO    EQU   *
        CLI   TRACE,X'00'        trace enabled ?
        BE    NOTRCGO
        L     R1,MSGTRCGO
        BAL   R9,MOVTRC
NOTRCGO  EQU   *
*
        LR    R6,R3
        SR    R6,R8              R6 := n-1
        LA    R7,6
        SR    R7,R4
        SR    R7,R5              R7 := 6-(f+t)
*
        LA    R15,MOV
        LR    R0,R6              R0 := n-1
        LR    R1,R4              R1 := f
        LR    R2,R7              R2 := o
        BALR  R14,R15            mov(n-1,f,o)
*
        LA    R0,1               R0 := 1
        LR    R1,R4              R1 := f
        LR    R2,R5              R2 := t
        BALR  R14,R15            mov(1,f,t)
*
        LR    R0,R6              R0 := n-1
        LR    R1,R7              R0 := o
        LR    R2,R5              R0 := t
        BALR  R14,R15            mov(n-1,o,t)
*
MOVEND   EQU   *
        L     R5,CURSTK
        SR    R5,R8
        ST    R5,CURSTK          curstk--
        L     R13,4(R13)         get old save area back
        RETURN (14,10),T         return to caller
*
* local print handler
* used with BAL 9, no local frame !!
*
MOVTRC   BAL   R14,OTEXT          print prefix
        L     R1,CURSTK
        BAL   R14,OINT04         print curstk
        L     R1,MSGCSEP
        BAL   R14,OTEXT          print " : "
        LR    R1,R3
        BAL   R14,OINT04         print n
        LR    R1,R4
        BAL   R14,OINT04         print f
        LR    R1,R5
        BAL   R14,OINT04         print t
        L     R1,MSGCSEP
        BAL   R14,OTEXT          print " : "
        L     R1,TOW+4
        BAL   R14,OINT04         print tow[1]
        L     R1,TOW+8
        BAL   R14,OINT04         print tow[2]
        L     R1,TOW+12
        BAL   R14,OINT04         print tow[3]
        BAL   R14,OPUTLINE       write line
        BR    R9
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
*
* OINT04 ---------------------------------------------------
*   print integer, like PL/I F(4) or C %4d format 
*   very fast, for non-negative numbers only !
*
OINT04   LA    R15,999
        CLR   R1,R15             too large ?
        BH    OINT04F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI04L,R15),OEI04   setup pattern
        ED    0(OEI04L,R15),OCVD+6  and edit
        LA    R15,OEI04L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT04F  LA    R1,4
        B     OSFILL
*
OEI04    DC    C' ',X'20',X'21',X'20'     ED pattern: bd(d
OEI04L   EQU   *-OEI04
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT05 ---------------------------------------------------
*   read integer, like PL/I F(5) or C %5d format 
*
IINT05   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(5,R15)   pack next 5 char
        CVB   R1,ICVB            and convert
        LA    R15,5(R15)         push pointer by 5 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
RC       DC    F'0'               return code
MAXDSK   DC    F'10'              maximal number of disks
TRACE    DC    X'00'              trace enable flag
*
NDSK     DC    F'2'
NCALL    DC    F'0'
NMOVE    DC    F'0'
MAXSTK   DC    F'0'
CURSTK   DC    F'0'
TOW      DC    4F'0'
*
* message strings
*
MSGPERR  OTXTDSC C'maxdsk out of range (2...30), abort'
MSGSTRT  OTXTDSC C'STRT ndsk='
MSGDONE  OTXTDSC C'DONE ndsk='
MSGDOSTK OTXTDSC C':  maxstk='
MSGDONCA OTXTDSC C'  ncall='
MSGDONMO OTXTDSC C'  nmove='
MSGTRCDO OTXTDSC C'mov-do: '
MSGTRCGO OTXTDSC C'mov-go: '
MSGCSEP  OTXTDSC C' : '
*
* spill literal pool
*
        LTORG
*
* Place the STACK in separate CSECT. Is quite large (~2 kByte)
*
DATA     CSECT
STACK    DS    (32*18)F           save area STACK
*
* other defs and end
*
        YREGS ,
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHGCCF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(10,0),PRTY=2
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: towh_cc.c 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-08-09   934   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>

void mov(int n, int f, int t);

int ncall  =  0;
int nmove  =  0;
int curstk =  0;
int maxstk =  0;
int maxdsk =  0;
int trace  =  0;
int tow[4];

int main(argc, argv)
int     argc;
char    *argv[];
{
 int ndsk;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &maxdsk, &trace) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }

 /* avoid | here, lots of fun with ASCII -> EBCDIC mapping */
 /* if (maxdsk < 2 || maxdsk > 32) { */
 if ((maxdsk < 2) + (maxdsk > 32)) {
   printf("maxdsk out of range (2...32), abort\n");
   return 1;
 }

 for (ndsk=2; ndsk<=maxdsk; ndsk++) {
   ncall  = 0;
   nmove  = 0;
   maxstk = 0;
   curstk = 0;
   tow[1] = ndsk;
   tow[2] = 0;
   tow[3] = 0;

   if (trace) printf("STRT ndsk=%2d\n", ndsk);
   mov(ndsk,1,3);
   printf("DONE ndsk=%2d:  maxstk=%2d  ncall=%10d  nmove=%10d\n",
          ndsk,maxstk,ncall,nmove);
 }
 return 0;
}

void mov(int n, int f, int t)
{
 int o = 6-(f+t);
 curstk++;
 ncall++;
 if (curstk > maxstk) maxstk = curstk;
 if(n == 1) {
   nmove++;
   tow[f]--;
   tow[t]++;
   if (trace) printf("mov-do: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
 } else {
   if (trace) printf("mov-go: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
   mov(n-1,f,o);
   mov(1,f,t);
   mov(n-1,o,t);
 }
 curstk--;
 return;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHGCCT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: towh_cc.c 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-08-09   934   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>

void mov(int n, int f, int t);

int ncall  =  0;
int nmove  =  0;
int curstk =  0;
int maxstk =  0;
int maxdsk =  0;
int trace  =  0;
int tow[4];

int main(argc, argv)
int     argc;
char    *argv[];
{
 int ndsk;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &maxdsk, &trace) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }

 /* avoid | here, lots of fun with ASCII -> EBCDIC mapping */
 /* if (maxdsk < 2 || maxdsk > 32) { */
 if ((maxdsk < 2) + (maxdsk > 32)) {
   printf("maxdsk out of range (2...32), abort\n");
   return 1;
 }

 for (ndsk=2; ndsk<=maxdsk; ndsk++) {
   ncall  = 0;
   nmove  = 0;
   maxstk = 0;
   curstk = 0;
   tow[1] = ndsk;
   tow[2] = 0;
   tow[3] = 0;

   if (trace) printf("STRT ndsk=%2d\n", ndsk);
   mov(ndsk,1,3);
   printf("DONE ndsk=%2d:  maxstk=%2d  ncall=%10d  nmove=%10d\n",
          ndsk,maxstk,ncall,nmove);
 }
 return 0;
}

void mov(int n, int f, int t)
{
 int o = 6-(f+t);
 curstk++;
 ncall++;
 if (curstk > maxstk) maxstk = curstk;
 if(n == 1) {
   nmove++;
   tow[f]--;
   tow[t]++;
   if (trace) printf("mov-do: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
 } else {
   if (trace) printf("mov-go: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
   mov(n-1,f,o);
   mov(1,f,t);
   mov(n-1,o,t);
 }
 curstk--;
 return;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHJCCF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(10,0),PRTY=2
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: towh_cc.c 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-08-09   934   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>

void mov(int n, int f, int t);

int ncall  =  0;
int nmove  =  0;
int curstk =  0;
int maxstk =  0;
int maxdsk =  0;
int trace  =  0;
int tow[4];

int main(argc, argv)
int     argc;
char    *argv[];
{
 int ndsk;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &maxdsk, &trace) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }

 /* avoid | here, lots of fun with ASCII -> EBCDIC mapping */
 /* if (maxdsk < 2 || maxdsk > 32) { */
 if ((maxdsk < 2) + (maxdsk > 32)) {
   printf("maxdsk out of range (2...32), abort\n");
   return 1;
 }

 for (ndsk=2; ndsk<=maxdsk; ndsk++) {
   ncall  = 0;
   nmove  = 0;
   maxstk = 0;
   curstk = 0;
   tow[1] = ndsk;
   tow[2] = 0;
   tow[3] = 0;

   if (trace) printf("STRT ndsk=%2d\n", ndsk);
   mov(ndsk,1,3);
   printf("DONE ndsk=%2d:  maxstk=%2d  ncall=%10d  nmove=%10d\n",
          ndsk,maxstk,ncall,nmove);
 }
 return 0;
}

void mov(int n, int f, int t)
{
 int o = 6-(f+t);
 curstk++;
 ncall++;
 if (curstk > maxstk) maxstk = curstk;
 if(n == 1) {
   nmove++;
   tow[f]--;
   tow[t]++;
   if (trace) printf("mov-do: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
 } else {
   if (trace) printf("mov-go: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
   mov(n-1,f,o);
   mov(1,f,t);
   mov(n-1,o,t);
 }
 curstk--;
 return;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHJCCT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: towh_cc.c 964 2017-11-19 08:47:46Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-08-09   934   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>

void mov(int n, int f, int t);

int ncall  =  0;
int nmove  =  0;
int curstk =  0;
int maxstk =  0;
int maxdsk =  0;
int trace  =  0;
int tow[4];

int main(argc, argv)
int     argc;
char    *argv[];
{
 int ndsk;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d", &maxdsk, &trace) != 2) {
   printf("conversion error, abort\n");
   return 1;
 }

 /* avoid | here, lots of fun with ASCII -> EBCDIC mapping */
 /* if (maxdsk < 2 || maxdsk > 32) { */
 if ((maxdsk < 2) + (maxdsk > 32)) {
   printf("maxdsk out of range (2...32), abort\n");
   return 1;
 }

 for (ndsk=2; ndsk<=maxdsk; ndsk++) {
   ncall  = 0;
   nmove  = 0;
   maxstk = 0;
   curstk = 0;
   tow[1] = ndsk;
   tow[2] = 0;
   tow[3] = 0;

   if (trace) printf("STRT ndsk=%2d\n", ndsk);
   mov(ndsk,1,3);
   printf("DONE ndsk=%2d:  maxstk=%2d  ncall=%10d  nmove=%10d\n",
          ndsk,maxstk,ncall,nmove);
 }
 return 0;
}

void mov(int n, int f, int t)
{
 int o = 6-(f+t);
 curstk++;
 ncall++;
 if (curstk > maxstk) maxstk = curstk;
 if(n == 1) {
   nmove++;
   tow[f]--;
   tow[t]++;
   if (trace) printf("mov-do: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
 } else {
   if (trace) printf("mov-go: %2d : %2d %2d %2d : %2d %2d %2d\n",
                     curstk,n,f,t,tow[1],tow[2],tow[3]);
   mov(n-1,f,o);
   mov(1,f,t);
   mov(n-1,o,t);
 }
 curstk--;
 return;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHFOGF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(10,0),PRTY=2
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: towh_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM TOWH
C     
     IMPLICIT LOGICAL (A-Z)
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     INTEGER NDSK
C
     READ(5,9000,ERR=910,END=900) MAXDSK,TRACE
C
     DO 100 NDSK=2,MAXDSK
       NCALL  = 0
       NMOVE  = 0
       MAXSTK = 0
       TOW(1) = NDSK
       TOW(2) = 0
       TOW(3) = 0
       IF (TRACE .NE. 0) WRITE(6,9010) NDSK
       CALL MOV(NDSK,1,3)
       WRITE(6,9020) NDSK,MAXSTK,NCALL,NMOVE
 100  CONTINUE
C
900  CONTINUE
     STOP
910  WRITE(6,9030)
     STOP
C
9000 FORMAT(2I5)
9010 FORMAT(1X,'STRT ndsk=',I2)
9020 FORMAT(1X,'DONE ndsk=',I2,':  maxstk=',I2,'  ncall=',I10,
    *       '  nmove=',I10)
9030 FORMAT(1X,'conversion error, abort')
     END
C
C --- subroutine mov -------------------------------------------------
C
     SUBROUTINE MOV(N,F,T)
     IMPLICIT LOGICAL (A-Z)
     INTEGER N,F,T
     INTEGER O,L1,S
C
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     L     = 1
     LN(1) = N
     LF(1) = F
     LT(1) = T
C
1000 CONTINUE
     NCALL  = NCALL  + 1
     IF (L .GT. MAXSTK) MAXSTK = L
     LS(L) = 1
C
     IF (LN(L) .NE. 1) GOTO 1900
     NMOVE  = NMOVE  + 1
     TOW(LF(L)) = TOW(LF(L)) - 1
     TOW(LT(L)) = TOW(LT(L)) + 1
     IF (TRACE .NE. 0) WRITE(6,9000) L,LN(L),LF(L),LT(L),TOW
     L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
1900 IF (TRACE .NE. 0) WRITE(6,9010) L,LN(L),LF(L),LT(L),TOW
C
2000 CONTINUE
     IF (TRACE .GT. 1) WRITE(6,9020) L,LN(L),LF(L),LT(L),TOW,L,LS(L)
     O = 6-(LF(L)+LT(L))
     L1 = L + 1
C Fortran IV(1966): computed GOTO selectors must be un-subscripted integers
     S = LS(L)
     GOTO (2100,2200,2300,2400), S
C
2100 LN(L1) = LN(L)-1
     LF(L1) = LF(L)
     LT(L1) = O
     LS(L)  = 2
     L = L1
     GOTO 1000
C
2200 LN(L1) = 1
     LF(L1) = LF(L)
     LT(L1) = LT(L)
     LS(L)  = 3
     L = L1
     GOTO 1000
C
2300 LN(L1) = LN(L)-1
     LF(L1) = O
     LT(L1) = LT(L)
     LS(L)  = 4
     L = L1
     GOTO 1000
C
2400 L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
9000 FORMAT(1X,'mov-do: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9010 FORMAT(1X,'mov-go: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9020 FORMAT(1X,'step:   ',I2,' :',3(1X,I2),' :',3(1X,I2),
    *       ' :',I2,'-',I2)
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHFOGT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: towh_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM TOWH
C     
     IMPLICIT LOGICAL (A-Z)
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     INTEGER NDSK
C
     READ(5,9000,ERR=910,END=900) MAXDSK,TRACE
C
     DO 100 NDSK=2,MAXDSK
       NCALL  = 0
       NMOVE  = 0
       MAXSTK = 0
       TOW(1) = NDSK
       TOW(2) = 0
       TOW(3) = 0
       IF (TRACE .NE. 0) WRITE(6,9010) NDSK
       CALL MOV(NDSK,1,3)
       WRITE(6,9020) NDSK,MAXSTK,NCALL,NMOVE
 100  CONTINUE
C
900  CONTINUE
     STOP
910  WRITE(6,9030)
     STOP
C
9000 FORMAT(2I5)
9010 FORMAT(1X,'STRT ndsk=',I2)
9020 FORMAT(1X,'DONE ndsk=',I2,':  maxstk=',I2,'  ncall=',I10,
    *       '  nmove=',I10)
9030 FORMAT(1X,'conversion error, abort')
     END
C
C --- subroutine mov -------------------------------------------------
C
     SUBROUTINE MOV(N,F,T)
     IMPLICIT LOGICAL (A-Z)
     INTEGER N,F,T
     INTEGER O,L1,S
C
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     L     = 1
     LN(1) = N
     LF(1) = F
     LT(1) = T
C
1000 CONTINUE
     NCALL  = NCALL  + 1
     IF (L .GT. MAXSTK) MAXSTK = L
     LS(L) = 1
C
     IF (LN(L) .NE. 1) GOTO 1900
     NMOVE  = NMOVE  + 1
     TOW(LF(L)) = TOW(LF(L)) - 1
     TOW(LT(L)) = TOW(LT(L)) + 1
     IF (TRACE .NE. 0) WRITE(6,9000) L,LN(L),LF(L),LT(L),TOW
     L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
1900 IF (TRACE .NE. 0) WRITE(6,9010) L,LN(L),LF(L),LT(L),TOW
C
2000 CONTINUE
     IF (TRACE .GT. 1) WRITE(6,9020) L,LN(L),LF(L),LT(L),TOW,L,LS(L)
     O = 6-(LF(L)+LT(L))
     L1 = L + 1
C Fortran IV(1966): computed GOTO selectors must be un-subscripted integers
     S = LS(L)
     GOTO (2100,2200,2300,2400), S
C
2100 LN(L1) = LN(L)-1
     LF(L1) = LF(L)
     LT(L1) = O
     LS(L)  = 2
     L = L1
     GOTO 1000
C
2200 LN(L1) = 1
     LF(L1) = LF(L)
     LT(L1) = LT(L)
     LS(L)  = 3
     L = L1
     GOTO 1000
C
2300 LN(L1) = LN(L)-1
     LF(L1) = O
     LT(L1) = LT(L)
     LS(L)  = 4
     L = L1
     GOTO 1000
C
2400 L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
9000 FORMAT(1X,'mov-do: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9010 FORMAT(1X,'mov-go: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9020 FORMAT(1X,'step:   ',I2,' :',3(1X,I2),' :',3(1X,I2),
    *       ' :',I2,'-',I2)
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHFOHF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(10,0),PRTY=2
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: towh_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM TOWH
C     
     IMPLICIT LOGICAL (A-Z)
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     INTEGER NDSK
C
     READ(5,9000,ERR=910,END=900) MAXDSK,TRACE
C
     DO 100 NDSK=2,MAXDSK
       NCALL  = 0
       NMOVE  = 0
       MAXSTK = 0
       TOW(1) = NDSK
       TOW(2) = 0
       TOW(3) = 0
       IF (TRACE .NE. 0) WRITE(6,9010) NDSK
       CALL MOV(NDSK,1,3)
       WRITE(6,9020) NDSK,MAXSTK,NCALL,NMOVE
 100  CONTINUE
C
900  CONTINUE
     STOP
910  WRITE(6,9030)
     STOP
C
9000 FORMAT(2I5)
9010 FORMAT(1X,'STRT ndsk=',I2)
9020 FORMAT(1X,'DONE ndsk=',I2,':  maxstk=',I2,'  ncall=',I10,
    *       '  nmove=',I10)
9030 FORMAT(1X,'conversion error, abort')
     END
C
C --- subroutine mov -------------------------------------------------
C
     SUBROUTINE MOV(N,F,T)
     IMPLICIT LOGICAL (A-Z)
     INTEGER N,F,T
     INTEGER O,L1,S
C
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     L     = 1
     LN(1) = N
     LF(1) = F
     LT(1) = T
C
1000 CONTINUE
     NCALL  = NCALL  + 1
     IF (L .GT. MAXSTK) MAXSTK = L
     LS(L) = 1
C
     IF (LN(L) .NE. 1) GOTO 1900
     NMOVE  = NMOVE  + 1
     TOW(LF(L)) = TOW(LF(L)) - 1
     TOW(LT(L)) = TOW(LT(L)) + 1
     IF (TRACE .NE. 0) WRITE(6,9000) L,LN(L),LF(L),LT(L),TOW
     L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
1900 IF (TRACE .NE. 0) WRITE(6,9010) L,LN(L),LF(L),LT(L),TOW
C
2000 CONTINUE
     IF (TRACE .GT. 1) WRITE(6,9020) L,LN(L),LF(L),LT(L),TOW,L,LS(L)
     O = 6-(LF(L)+LT(L))
     L1 = L + 1
C Fortran IV(1966): computed GOTO selectors must be un-subscripted integers
     S = LS(L)
     GOTO (2100,2200,2300,2400), S
C
2100 LN(L1) = LN(L)-1
     LF(L1) = LF(L)
     LT(L1) = O
     LS(L)  = 2
     L = L1
     GOTO 1000
C
2200 LN(L1) = 1
     LF(L1) = LF(L)
     LT(L1) = LT(L)
     LS(L)  = 3
     L = L1
     GOTO 1000
C
2300 LN(L1) = LN(L)-1
     LF(L1) = O
     LT(L1) = LT(L)
     LS(L)  = 4
     L = L1
     GOTO 1000
C
2400 L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
9000 FORMAT(1X,'mov-do: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9010 FORMAT(1X,'mov-go: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9020 FORMAT(1X,'step:   ',I2,' :',3(1X,I2),' :',3(1X,I2),
    *       ' :',I2,'-',I2)
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHFOHT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: towh_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM TOWH
C     
     IMPLICIT LOGICAL (A-Z)
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     INTEGER NDSK
C
     READ(5,9000,ERR=910,END=900) MAXDSK,TRACE
C
     DO 100 NDSK=2,MAXDSK
       NCALL  = 0
       NMOVE  = 0
       MAXSTK = 0
       TOW(1) = NDSK
       TOW(2) = 0
       TOW(3) = 0
       IF (TRACE .NE. 0) WRITE(6,9010) NDSK
       CALL MOV(NDSK,1,3)
       WRITE(6,9020) NDSK,MAXSTK,NCALL,NMOVE
 100  CONTINUE
C
900  CONTINUE
     STOP
910  WRITE(6,9030)
     STOP
C
9000 FORMAT(2I5)
9010 FORMAT(1X,'STRT ndsk=',I2)
9020 FORMAT(1X,'DONE ndsk=',I2,':  maxstk=',I2,'  ncall=',I10,
    *       '  nmove=',I10)
9030 FORMAT(1X,'conversion error, abort')
     END
C
C --- subroutine mov -------------------------------------------------
C
     SUBROUTINE MOV(N,F,T)
     IMPLICIT LOGICAL (A-Z)
     INTEGER N,F,T
     INTEGER O,L1,S
C
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     L     = 1
     LN(1) = N
     LF(1) = F
     LT(1) = T
C
1000 CONTINUE
     NCALL  = NCALL  + 1
     IF (L .GT. MAXSTK) MAXSTK = L
     LS(L) = 1
C
     IF (LN(L) .NE. 1) GOTO 1900
     NMOVE  = NMOVE  + 1
     TOW(LF(L)) = TOW(LF(L)) - 1
     TOW(LT(L)) = TOW(LT(L)) + 1
     IF (TRACE .NE. 0) WRITE(6,9000) L,LN(L),LF(L),LT(L),TOW
     L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
1900 IF (TRACE .NE. 0) WRITE(6,9010) L,LN(L),LF(L),LT(L),TOW
C
2000 CONTINUE
     IF (TRACE .GT. 1) WRITE(6,9020) L,LN(L),LF(L),LT(L),TOW,L,LS(L)
     O = 6-(LF(L)+LT(L))
     L1 = L + 1
C Fortran IV(1966): computed GOTO selectors must be un-subscripted integers
     S = LS(L)
     GOTO (2100,2200,2300,2400), S
C
2100 LN(L1) = LN(L)-1
     LF(L1) = LF(L)
     LT(L1) = O
     LS(L)  = 2
     L = L1
     GOTO 1000
C
2200 LN(L1) = 1
     LF(L1) = LF(L)
     LT(L1) = LT(L)
     LS(L)  = 3
     L = L1
     GOTO 1000
C
2300 LN(L1) = LN(L)-1
     LF(L1) = O
     LT(L1) = LT(L)
     LS(L)  = 4
     L = L1
     GOTO 1000
C
2400 L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
9000 FORMAT(1X,'mov-do: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9010 FORMAT(1X,'mov-go: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9020 FORMAT(1X,'step:   ',I2,' :',3(1X,I2),' :',3(1X,I2),
    *       ' :',I2,'-',I2)
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHFOWF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(20,0),PRTY=2
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           TOWH#FOW,T=(20,0),P=100,NOCHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: towh_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM TOWH
C     
     IMPLICIT LOGICAL (A-Z)
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     INTEGER NDSK
C
     READ(5,9000,ERR=910,END=900) MAXDSK,TRACE
C
     DO 100 NDSK=2,MAXDSK
       NCALL  = 0
       NMOVE  = 0
       MAXSTK = 0
       TOW(1) = NDSK
       TOW(2) = 0
       TOW(3) = 0
       IF (TRACE .NE. 0) WRITE(6,9010) NDSK
       CALL MOV(NDSK,1,3)
       WRITE(6,9020) NDSK,MAXSTK,NCALL,NMOVE
 100  CONTINUE
C
900  CONTINUE
     STOP
910  WRITE(6,9030)
     STOP
C
9000 FORMAT(2I5)
9010 FORMAT(1X,'STRT ndsk=',I2)
9020 FORMAT(1X,'DONE ndsk=',I2,':  maxstk=',I2,'  ncall=',I10,
    *       '  nmove=',I10)
9030 FORMAT(1X,'conversion error, abort')
     END
C
C --- subroutine mov -------------------------------------------------
C
     SUBROUTINE MOV(N,F,T)
     IMPLICIT LOGICAL (A-Z)
     INTEGER N,F,T
     INTEGER O,L1,S
C
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     L     = 1
     LN(1) = N
     LF(1) = F
     LT(1) = T
C
1000 CONTINUE
     NCALL  = NCALL  + 1
     IF (L .GT. MAXSTK) MAXSTK = L
     LS(L) = 1
C
     IF (LN(L) .NE. 1) GOTO 1900
     NMOVE  = NMOVE  + 1
     TOW(LF(L)) = TOW(LF(L)) - 1
     TOW(LT(L)) = TOW(LT(L)) + 1
     IF (TRACE .NE. 0) WRITE(6,9000) L,LN(L),LF(L),LT(L),TOW
     L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
1900 IF (TRACE .NE. 0) WRITE(6,9010) L,LN(L),LF(L),LT(L),TOW
C
2000 CONTINUE
     IF (TRACE .GT. 1) WRITE(6,9020) L,LN(L),LF(L),LT(L),TOW,L,LS(L)
     O = 6-(LF(L)+LT(L))
     L1 = L + 1
C Fortran IV(1966): computed GOTO selectors must be un-subscripted integers
     S = LS(L)
     GOTO (2100,2200,2300,2400), S
C
2100 LN(L1) = LN(L)-1
     LF(L1) = LF(L)
     LT(L1) = O
     LS(L)  = 2
     L = L1
     GOTO 1000
C
2200 LN(L1) = 1
     LF(L1) = LF(L)
     LT(L1) = LT(L)
     LS(L)  = 3
     L = L1
     GOTO 1000
C
2300 LN(L1) = LN(L)-1
     LF(L1) = O
     LT(L1) = LT(L)
     LS(L)  = 4
     L = L1
     GOTO 1000
C
2400 L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
9000 FORMAT(1X,'mov-do: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9010 FORMAT(1X,'mov-go: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9020 FORMAT(1X,'step:   ',I2,' :',3(1X,I2),' :',3(1X,I2),
    *       ' :',I2,'-',I2)
C
     END
$ENTRY
  22    0
$STOP
/*
//
./        ADD   NAME=TOWHFOWT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           TOWH#FOW,T=(1,0),P=100,CHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: towh_for.f 964 2017-11-19 08:47:46Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-08-09   934   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- main program ---------------------------------------------------
C     PROGRAM TOWH
C     
     IMPLICIT LOGICAL (A-Z)
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     INTEGER NDSK
C
     READ(5,9000,ERR=910,END=900) MAXDSK,TRACE
C
     DO 100 NDSK=2,MAXDSK
       NCALL  = 0
       NMOVE  = 0
       MAXSTK = 0
       TOW(1) = NDSK
       TOW(2) = 0
       TOW(3) = 0
       IF (TRACE .NE. 0) WRITE(6,9010) NDSK
       CALL MOV(NDSK,1,3)
       WRITE(6,9020) NDSK,MAXSTK,NCALL,NMOVE
 100  CONTINUE
C
900  CONTINUE
     STOP
910  WRITE(6,9030)
     STOP
C
9000 FORMAT(2I5)
9010 FORMAT(1X,'STRT ndsk=',I2)
9020 FORMAT(1X,'DONE ndsk=',I2,':  maxstk=',I2,'  ncall=',I10,
    *       '  nmove=',I10)
9030 FORMAT(1X,'conversion error, abort')
     END
C
C --- subroutine mov -------------------------------------------------
C
     SUBROUTINE MOV(N,F,T)
     IMPLICIT LOGICAL (A-Z)
     INTEGER N,F,T
     INTEGER O,L1,S
C
     COMMON /DAT1/NCALL,NMOVE,MAXSTK,MAXDSK,TRACE,TOW(3)
     INTEGER      NCALL,NMOVE,MAXDSK,MAXSTK,TRACE,TOW
     COMMON /DAT2/L,LN(32),LF(32),LT(32),LS(32)
     INTEGER      L,LN,LF,LT,LS
C
     L     = 1
     LN(1) = N
     LF(1) = F
     LT(1) = T
C
1000 CONTINUE
     NCALL  = NCALL  + 1
     IF (L .GT. MAXSTK) MAXSTK = L
     LS(L) = 1
C
     IF (LN(L) .NE. 1) GOTO 1900
     NMOVE  = NMOVE  + 1
     TOW(LF(L)) = TOW(LF(L)) - 1
     TOW(LT(L)) = TOW(LT(L)) + 1
     IF (TRACE .NE. 0) WRITE(6,9000) L,LN(L),LF(L),LT(L),TOW
     L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
1900 IF (TRACE .NE. 0) WRITE(6,9010) L,LN(L),LF(L),LT(L),TOW
C
2000 CONTINUE
     IF (TRACE .GT. 1) WRITE(6,9020) L,LN(L),LF(L),LT(L),TOW,L,LS(L)
     O = 6-(LF(L)+LT(L))
     L1 = L + 1
C Fortran IV(1966): computed GOTO selectors must be un-subscripted integers
     S = LS(L)
     GOTO (2100,2200,2300,2400), S
C
2100 LN(L1) = LN(L)-1
     LF(L1) = LF(L)
     LT(L1) = O
     LS(L)  = 2
     L = L1
     GOTO 1000
C
2200 LN(L1) = 1
     LF(L1) = LF(L)
     LT(L1) = LT(L)
     LS(L)  = 3
     L = L1
     GOTO 1000
C
2300 LN(L1) = LN(L)-1
     LF(L1) = O
     LT(L1) = LT(L)
     LS(L)  = 4
     L = L1
     GOTO 1000
C
2400 L = L - 1
     IF (L .EQ. 0) RETURN
     GOTO 2000
C
9000 FORMAT(1X,'mov-do: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9010 FORMAT(1X,'mov-go: ',I2,' :',3(1X,I2),' :',3(1X,I2))
9020 FORMAT(1X,'step:   ',I2,' :',3(1X,I2),' :',3(1X,I2),
    *       ' :',I2,'-',I2)
C
     END
$ENTRY
   4    1
$STOP
/*
//
./        ADD   NAME=TOWHPASF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(10,0),PRTY=2
//CLG EXEC PASCLG,GOTIME=3600,GOREG=1024K,
//      OPT='M+,D-',
//      GOPARM='/STACK=512k'
//COMPILE.SYSIN DD *
(* $Id: towh_pas.pas 964 2017-11-19 08:47:46Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-09-07   948   0.1    Initial version                         *)

program towh(input,output);
var
  ncall,nmove   : integer;
  curstk,maxstk : integer;
  maxdsk,trace  : integer;
  ndsk          : integer;
  tow           : ARRAY[1 .. 3] of integer;

procedure mov(n,f,t: integer); 
var
  o : integer;
begin
  o := 6-(f+t);
  curstk := curstk + 1;
  ncall  := ncall  + 1;
  if maxstk < curstk then maxstk := curstk;
  if n = 1 then begin
     nmove  := nmove + 1;
     tow[f] := tow[f] - 1;
     tow[t] := tow[t] + 1;
     if trace > 0 then writeln(' ','mov-do: ',curstk:2,
                               ' :',n:3,f:3,t:3,
                               ' :',tow[1]:3,tow[2]:3,tow[3]:3);
  end else begin
     if trace > 0 then writeln(' ','mov-go: ',curstk:2,
                               ' :',n:3,f:3,t:3,
                               ' :',tow[1]:3,tow[2]:3,tow[3]:3);
     mov(n-1,f,o);
     mov(1,f,t);
     mov(n-1,o,t);
  end;
  curstk := curstk - 1;
end;

begin

  read(maxdsk);
  read(trace);

  for ndsk := 2 to maxdsk do begin
     ncall  := 0;
     nmove  := 0;
     maxstk := 0;
     curstk := 0;
     tow[1] := ndsk;
     tow[2] := 0;
     tow[3] := 0;
     if trace > 0 then  writeln(' ','STRT ndsk=',ndsk:2);
     mov(ndsk,1,3);
     writeln(' ','DONE ndsk=',ndsk:2,': maxstk=',maxstk:2,
             '  ncall=',ncall:10,'  nmove=',nmove:10);
  end;

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHPAST,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=1024K,
//      OPT='M+',
//      GOPARM='/STACK=512k'
//COMPILE.SYSIN DD *
(* $Id: towh_pas.pas 964 2017-11-19 08:47:46Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-09-07   948   0.1    Initial version                         *)

program towh(input,output);
var
  ncall,nmove   : integer;
  curstk,maxstk : integer;
  maxdsk,trace  : integer;
  ndsk          : integer;
  tow           : ARRAY[1 .. 3] of integer;

procedure mov(n,f,t: integer); 
var
  o : integer;
begin
  o := 6-(f+t);
  curstk := curstk + 1;
  ncall  := ncall  + 1;
  if maxstk < curstk then maxstk := curstk;
  if n = 1 then begin
     nmove  := nmove + 1;
     tow[f] := tow[f] - 1;
     tow[t] := tow[t] + 1;
     if trace > 0 then writeln(' ','mov-do: ',curstk:2,
                               ' :',n:3,f:3,t:3,
                               ' :',tow[1]:3,tow[2]:3,tow[3]:3);
  end else begin
     if trace > 0 then writeln(' ','mov-go: ',curstk:2,
                               ' :',n:3,f:3,t:3,
                               ' :',tow[1]:3,tow[2]:3,tow[3]:3);
     mov(n-1,f,o);
     mov(1,f,t);
     mov(n-1,o,t);
  end;
  curstk := curstk - 1;
end;

begin

  read(maxdsk);
  read(trace);

  for ndsk := 2 to maxdsk do begin
     ncall  := 0;
     nmove  := 0;
     maxstk := 0;
     curstk := 0;
     tow[1] := ndsk;
     tow[2] := 0;
     tow[3] := 0;
     if trace > 0 then  writeln(' ','STRT ndsk=',ndsk:2);
     mov(ndsk,1,3);
     writeln(' ','DONE ndsk=',ndsk:2,': maxstk=',maxstk:2,
             '  ncall=',ncall:10,'  nmove=',nmove:10);
  end;

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHPLIF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(10,0),PRTY=2
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: towh_pli.pli 981 2018-01-02 13:30:49Z mueller $ */
/*
/* Copyright 2017-2018 by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2018-01-02   981   1.0    add ON units for ENDFILE and CONVERSION */
/* 2017-09-07   947   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

 TOWH: PROC OPTIONS(MAIN) REORDER;
   DCL (NCALL,NMOVE)    BIN FIXED(31) INIT(0);
   DCL (CURSTK,MAXSTK)  BIN FIXED(31) INIT(0);
   DCL (MAXDSK,TRACE)   BIN FIXED(31) INIT(0);
   DCL TOW(3)           BIN FIXED(31) INIT((3)0);
   DCL NDSK             BIN FIXED(31);

   DCL MOV ENTRY(BIN FIXED(31),BIN FIXED(31),BIN FIXED(31));

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(MAXDSK,TRACE) (F(5),F(5));

   DO NDSK=2 TO MAXDSK;
     NCALL  = 0;
     NMOVE  = 0;
     MAXSTK = 0;
     CURSTK = 0;
     TOW(1) = NDSK;
     TOW(2) = 0;
     TOW(3) = 0;
     IF TRACE > 0 THEN PUT SKIP EDIT('STRT ndsk=',NDSK) (A,F(2));
     CALL MOV(NDSK,1,3);
     PUT SKIP EDIT('DONE ndsk=',NDSK, ':  maxstk=',MAXSTK,
                   '  ncall=',NCALL, '  nmove=',NMOVE)
                   (2(A,F(2)),2(A,F(10)));
   END;
   DONE:;

   /* procedure MOV -----------------------------------------------*/
   MOV: PROC(N,F,T) RECURSIVE;
     DCL (N,F,T)  BIN FIXED(31);
     DCL O BIN FIXED(31);
     O = 6-(F+T);
     CURSTK = CURSTK + 1;
     NCALL  = NCALL  + 1;
     MAXSTK = MAX(MAXSTK,CURSTK);
     IF N = 1 THEN DO;
       NMOVE  = NMOVE  + 1;
       TOW(F) = TOW(F) - 1;
       TOW(T) = TOW(T) + 1;
       IF TRACE > 0 THEN PUT SKIP EDIT('mov-do: ',CURSTK,
                                ' :',N,F,T, ' :',TOW)
                                (A,F(2),2(A,3(X(1),F(2))));
     END;
     ELSE DO;
       IF TRACE > 0 THEN PUT SKIP EDIT('mov-go: ',CURSTK,
                                ' :',N,F,T, ' :',TOW)
                                (A,F(2),2(A,3(X(1),F(2))));
       CALL MOV(N-1,F,O);
       CALL MOV(1,F,T);
       CALL MOV(N-1,O,T);
     END;
     CURSTK = CURSTK - 1;
   END MOV;

 END TOWH;

/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHPLIT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: towh_pli.pli 981 2018-01-02 13:30:49Z mueller $ */
/*
/* Copyright 2017-2018 by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2018-01-02   981   1.0    add ON units for ENDFILE and CONVERSION */
/* 2017-09-07   947   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

 TOWH: PROC OPTIONS(MAIN) REORDER;
   DCL (NCALL,NMOVE)    BIN FIXED(31) INIT(0);
   DCL (CURSTK,MAXSTK)  BIN FIXED(31) INIT(0);
   DCL (MAXDSK,TRACE)   BIN FIXED(31) INIT(0);
   DCL TOW(3)           BIN FIXED(31) INIT((3)0);
   DCL NDSK             BIN FIXED(31);

   DCL MOV ENTRY(BIN FIXED(31),BIN FIXED(31),BIN FIXED(31));

   ON ENDFILE(SYSIN) BEGIN;
      PUT SKIP EDIT('Unexpected EOF, abort')(A);
      GOTO DONE;
   END;
   ON CONVERSION     BEGIN;
      PUT SKIP EDIT('Conversion error, abort')(A);
      GOTO DONE;
   END;

   GET EDIT(MAXDSK,TRACE) (F(5),F(5));

   DO NDSK=2 TO MAXDSK;
     NCALL  = 0;
     NMOVE  = 0;
     MAXSTK = 0;
     CURSTK = 0;
     TOW(1) = NDSK;
     TOW(2) = 0;
     TOW(3) = 0;
     IF TRACE > 0 THEN PUT SKIP EDIT('STRT ndsk=',NDSK) (A,F(2));
     CALL MOV(NDSK,1,3);
     PUT SKIP EDIT('DONE ndsk=',NDSK, ':  maxstk=',MAXSTK,
                   '  ncall=',NCALL, '  nmove=',NMOVE)
                   (2(A,F(2)),2(A,F(10)));
   END;
   DONE:;

   /* procedure MOV -----------------------------------------------*/
   MOV: PROC(N,F,T) RECURSIVE;
     DCL (N,F,T)  BIN FIXED(31);
     DCL O BIN FIXED(31);
     O = 6-(F+T);
     CURSTK = CURSTK + 1;
     NCALL  = NCALL  + 1;
     MAXSTK = MAX(MAXSTK,CURSTK);
     IF N = 1 THEN DO;
       NMOVE  = NMOVE  + 1;
       TOW(F) = TOW(F) - 1;
       TOW(T) = TOW(T) + 1;
       IF TRACE > 0 THEN PUT SKIP EDIT('mov-do: ',CURSTK,
                                ' :',N,F,T, ' :',TOW)
                                (A,F(2),2(A,3(X(1),F(2))));
     END;
     ELSE DO;
       IF TRACE > 0 THEN PUT SKIP EDIT('mov-go: ',CURSTK,
                                ' :',N,F,T, ' :',TOW)
                                (A,F(2),2(A,3(X(1),F(2))));
       CALL MOV(N-1,F,O);
       CALL MOV(1,F,T);
       CALL MOV(N-1,O,T);
     END;
     CURSTK = CURSTK - 1;
   END MOV;

 END TOWH;

/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=TOWHSIMF,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1000K,TIME=(10,0),PRTY=2
//CLG EXEC SIMCLG,
//      PARM.SIM=NOSUBCHK,
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-09-08   950   1.0    Initial version
*;

BEGIN   
  INTEGER ncall,nmove;
  INTEGER curstk,maxstk;
  INTEGER maxdsk,trace;
  INTEGER ndsk;
  INTEGER ARRAY tow(1:4);

  PROCEDURE trc(txt,n,f,t);
     VALUE txt;
     TEXT txt;
     INTEGER n,f,t;
  BEGIN
     OutText(txt);
     OutInt(curstk,2);
     OutText(" :");
     OutInt(n,3);
     OutInt(f,3);
     OutInt(t,3);
     OutText(" :");
     OutInt(tow(1),3);
     OutInt(tow(2),3);
     OutInt(tow(3),3);
     OutImage;
  END;

  PROCEDURE mov(n,f,t);
     INTEGER n,f,t;
  BEGIN
     INTEGER o;
     o := 6-(f+t);
     curstk := curstk + 1;
     ncall  := ncall  + 1;
     IF maxstk < curstk THEN maxstk := curstk;
     IF n = 1 THEN BEGIN
        nmove  := nmove + 1;
        tow(f) := tow(f) - 1;
        tow(t) := tow(t) + 1;
        IF trace > 0 THEN trc("mov-do: ",n,f,t);
     END ELSE BEGIN;
        IF trace > 0 THEN trc("mov-go: ",n,f,t);
        mov(n-1,f,o);
        mov(1,f,t);
        mov(n-1,o,t);
     END;
     curstk := curstk - 1;
  END;

  maxdsk := InInt;
  trace  := InInt;

  FOR ndsk := 2 STEP 1 UNTIL maxdsk DO BEGIN
     ncall  := 0;
     nmove  := 0;
     maxstk := 0;
     curstk := 0;
     tow(1) := ndsk;
     tow(2) := 0;
     tow(3) := 0;
     IF trace > 0 THEN BEGIN
        OutText("STRT ndsk=");
        OutInt(ndsk,2);
        OutImage;
     END;
     mov(ndsk,1,3);
     OutText("DONE ndsk=");
     OutInt(ndsk,2);
     OutText(": maxstk=");
     OutInt(ndsk,2);
     OutText("  ncall=");
     OutInt(ncall,10);
     OutText("  nmove=");
     OutInt(nmove,10);
     OutImage;
  END;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
  22    0
/*
//
./        ADD   NAME=TOWHSIMT,LEVEL=00,SOURCE=0,LIST=ALL
//TOWH#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-09-08   950   1.0    Initial version
*;

BEGIN   
  INTEGER ncall,nmove;
  INTEGER curstk,maxstk;
  INTEGER maxdsk,trace;
  INTEGER ndsk;
  INTEGER ARRAY tow(1:4);

  PROCEDURE trc(txt,n,f,t);
     VALUE txt;
     TEXT txt;
     INTEGER n,f,t;
  BEGIN
     OutText(txt);
     OutInt(curstk,2);
     OutText(" :");
     OutInt(n,3);
     OutInt(f,3);
     OutInt(t,3);
     OutText(" :");
     OutInt(tow(1),3);
     OutInt(tow(2),3);
     OutInt(tow(3),3);
     OutImage;
  END;

  PROCEDURE mov(n,f,t);
     INTEGER n,f,t;
  BEGIN
     INTEGER o;
     o := 6-(f+t);
     curstk := curstk + 1;
     ncall  := ncall  + 1;
     IF maxstk < curstk THEN maxstk := curstk;
     IF n = 1 THEN BEGIN
        nmove  := nmove + 1;
        tow(f) := tow(f) - 1;
        tow(t) := tow(t) + 1;
        IF trace > 0 THEN trc("mov-do: ",n,f,t);
     END ELSE BEGIN;
        IF trace > 0 THEN trc("mov-go: ",n,f,t);
        mov(n-1,f,o);
        mov(1,f,t);
        mov(n-1,o,t);
     END;
     curstk := curstk - 1;
  END;

  maxdsk := InInt;
  trace  := InInt;

  FOR ndsk := 2 STEP 1 UNTIL maxdsk DO BEGIN
     ncall  := 0;
     nmove  := 0;
     maxstk := 0;
     curstk := 0;
     tow(1) := ndsk;
     tow(2) := 0;
     tow(3) := 0;
     IF trace > 0 THEN BEGIN
        OutText("STRT ndsk=");
        OutInt(ndsk,2);
        OutImage;
     END;
     mov(ndsk,1,3);
     OutText("DONE ndsk=");
     OutInt(ndsk,2);
     OutText(": maxstk=");
     OutInt(ndsk,2);
     OutText("  ncall=");
     OutInt(ncall,10);
     OutText("  nmove=");
     OutInt(nmove,10);
     OutImage;
  END;
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
   4    1
/*
//
./        ADD   NAME=MCPIA60F,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(10,0),PRTY=2
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='LONG',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: mcpi_a60.a60 978 2017-12-28 21:32:18Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-12-28   978   1.1    use inverse to avoid divide by constant
*  2017-09-17   951   1.0    Initial version
*;

 'REAL'    RR32,RDIV,PI,RR32I,RDIVI;
 'REAL'    RSEED,RLAST;
 'BOOLEAN' RANINI;
 'INTEGER' IDBGRR,IDBGRN,IDBGMC;
 'INTEGER' I,NTRY,NHIT,NGO;
 'REAL'    PIEST,PIERR;
 'REAL'    RHIT,RTRY;
 'REAL'    X,Y,R;
 'REAL' 'ARRAY' RSHUF[0:127];

 'REAL' 'PROCEDURE' RANRAW;
 'BEGIN'
   'REAL'    RFAC,RNEW;
   RNEW := RSEED * 69069.0;
   RFAC := RNEW * RR32I;
   RFAC := ENTIER(RFAC);
   RNEW := RNEW - RFAC * RR32;
   'IF' IDBGRR > 0 'THEN' 'BEGIN'
     OUTSTRING (1,'('RR:')');
     OUTREAL(1, RSEED);
     OUTREAL(1, RNEW);
     SYSACT(1,14,1);
   'END';
   RSEED  := RNEW;
   RANRAW := RNEW;
 'END';

 'REAL' 'PROCEDURE' RANNUM;
 'BEGIN'
   'REAL'    RNEW;
   'INTEGER' I;
   'IF' 'NOT' RANINI 'THEN' 'BEGIN'
     'FOR' I := 0 'STEP' 1 'UNTIL' 127 'DO' RSHUF[I] := RANRAW;
     RANINI := 'TRUE';
   'END';

   I := ENTIER(RLAST*RDIVI);
   RLAST := RSHUF[I];
   RSHUF[I] := RANRAW;
   RNEW := RLAST * RR32I;
   'IF' IDBGRN > 0 'THEN' 'BEGIN'
     OUTSTRING (1,'('RN:')');
     OUTINTEGER(1, I);
     OUTREAL(1, RLAST);
     OUTREAL(1, RNEW);
     SYSACT(1,14,1);
   'END';
   RANNUM := RNEW;
 'END';

 'COMMENT' setup constants;
 RR32   := 4294967296.0;
 RDIV   := 33554432.0;
 PI     := 3.141592653589793;
 RR32I  := 1.0/RR32;
 RDIVI  := 1.0/RDIV;
 RSEED  := 12345.0;
 RANINI := 'FALSE';

 'COMMENT' set record lenth = 132 and page length = 64;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,IDBGRR);
 ININTEGER(0,IDBGRN);
 ININTEGER(0,IDBGMC);

 'IF' IDBGRR = 0 'AND' IDBGRN = 0 'AND' IDBGMC = 0 'THEN' 'BEGIN'
   OUTSTRING (1,'('          ntry         nhit')');
   OUTSTRING (1,'('              pi-est                  pi-err')');
   OUTSTRING (1,'('                    seed')');
   SYSACT(1,14,1);
 'END';

 LOOP:
   ININTEGER(0,NGO);
   'IF' NGO = 0 'THEN' 'GOTO' DONE;
   'FOR' I := 1 'STEP' 1 'UNTIL' NGO 'DO' 'BEGIN'
     X := 2.0 * RANNUM - 1.0;
     Y := 2.0 * RANNUM - 1.0;
     R := X*X + Y*Y;
     NTRY := NTRY + 1;
     'IF' R <= 1.0 'THEN' NHIT := NHIT + 1;
     'IF' IDBGMC > 0 'THEN' 'BEGIN'
       OUTSTRING (1,'('MC:')');
       OUTREAL(1, X);
       OUTREAL(1, Y);
       OUTREAL(1, R);
       OUTINTEGER(1, NHIT);
       SYSACT(1,14,1);
     'END';
   'END';

   RTRY := NTRY;
   RHIT := NHIT;
   PIEST := 4.0 * (RHIT / RTRY);
   PIERR := PIEST - PI;
   OUTSTRING (1,'('PI:')');
   OUTINTEGER(1, NTRY);
   OUTINTEGER(1, NHIT);
   OUTREAL(1, PIEST);
   OUTREAL(1, PIERR);
   OUTREAL(1, RLAST);
   SYSACT(1,14,1);      
 'GOTO' LOOP;

 DONE:
'END';

/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIA60T,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#A60 JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC ALGOFCLG,
//      PARM.ALGOL='LONG',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO=''
//ALGOL.SYSIN DD *
'BEGIN'
'COMMENT'
* $Id: mcpi_a60.a60 978 2017-12-28 21:32:18Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*  
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*  
*   Revision History:
*  Date         Rev Version  Comment
*  2017-12-28   978   1.1    use inverse to avoid divide by constant
*  2017-09-17   951   1.0    Initial version
*;

 'REAL'    RR32,RDIV,PI,RR32I,RDIVI;
 'REAL'    RSEED,RLAST;
 'BOOLEAN' RANINI;
 'INTEGER' IDBGRR,IDBGRN,IDBGMC;
 'INTEGER' I,NTRY,NHIT,NGO;
 'REAL'    PIEST,PIERR;
 'REAL'    RHIT,RTRY;
 'REAL'    X,Y,R;
 'REAL' 'ARRAY' RSHUF[0:127];

 'REAL' 'PROCEDURE' RANRAW;
 'BEGIN'
   'REAL'    RFAC,RNEW;
   RNEW := RSEED * 69069.0;
   RFAC := RNEW * RR32I;
   RFAC := ENTIER(RFAC);
   RNEW := RNEW - RFAC * RR32;
   'IF' IDBGRR > 0 'THEN' 'BEGIN'
     OUTSTRING (1,'('RR:')');
     OUTREAL(1, RSEED);
     OUTREAL(1, RNEW);
     SYSACT(1,14,1);
   'END';
   RSEED  := RNEW;
   RANRAW := RNEW;
 'END';

 'REAL' 'PROCEDURE' RANNUM;
 'BEGIN'
   'REAL'    RNEW;
   'INTEGER' I;
   'IF' 'NOT' RANINI 'THEN' 'BEGIN'
     'FOR' I := 0 'STEP' 1 'UNTIL' 127 'DO' RSHUF[I] := RANRAW;
     RANINI := 'TRUE';
   'END';

   I := ENTIER(RLAST*RDIVI);
   RLAST := RSHUF[I];
   RSHUF[I] := RANRAW;
   RNEW := RLAST * RR32I;
   'IF' IDBGRN > 0 'THEN' 'BEGIN'
     OUTSTRING (1,'('RN:')');
     OUTINTEGER(1, I);
     OUTREAL(1, RLAST);
     OUTREAL(1, RNEW);
     SYSACT(1,14,1);
   'END';
   RANNUM := RNEW;
 'END';

 'COMMENT' setup constants;
 RR32   := 4294967296.0;
 RDIV   := 33554432.0;
 PI     := 3.141592653589793;
 RR32I  := 1.0/RR32;
 RDIVI  := 1.0/RDIV;
 RSEED  := 12345.0;
 RANINI := 'FALSE';

 'COMMENT' set record lenth = 132 and page length = 64;
 SYSACT(1,6,132);
 SYSACT(1,8,64);

 ININTEGER(0,IDBGRR);
 ININTEGER(0,IDBGRN);
 ININTEGER(0,IDBGMC);

 'IF' IDBGRR = 0 'AND' IDBGRN = 0 'AND' IDBGMC = 0 'THEN' 'BEGIN'
   OUTSTRING (1,'('          ntry         nhit')');
   OUTSTRING (1,'('              pi-est                  pi-err')');
   OUTSTRING (1,'('                    seed')');
   SYSACT(1,14,1);
 'END';

 LOOP:
   ININTEGER(0,NGO);
   'IF' NGO = 0 'THEN' 'GOTO' DONE;
   'FOR' I := 1 'STEP' 1 'UNTIL' NGO 'DO' 'BEGIN'
     X := 2.0 * RANNUM - 1.0;
     Y := 2.0 * RANNUM - 1.0;
     R := X*X + Y*Y;
     NTRY := NTRY + 1;
     'IF' R <= 1.0 'THEN' NHIT := NHIT + 1;
     'IF' IDBGMC > 0 'THEN' 'BEGIN'
       OUTSTRING (1,'('MC:')');
       OUTREAL(1, X);
       OUTREAL(1, Y);
       OUTREAL(1, R);
       OUTINTEGER(1, NHIT);
       SYSACT(1,14,1);
     'END';
   'END';

   RTRY := NTRY;
   RHIT := NHIT;
   PIEST := 4.0 * (RHIT / RTRY);
   PIERR := PIEST - PI;
   OUTSTRING (1,'('PI:')');
   OUTINTEGER(1, NTRY);
   OUTINTEGER(1, NHIT);
   OUTREAL(1, PIEST);
   OUTREAL(1, PIERR);
   OUTREAL(1, RLAST);
   SYSACT(1,14,1);      
 'GOTO' LOOP;

 DONE:
'END';

/*
//GO.ALGLDD01 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIASMF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(10,0),PRTY=2
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NOLIST,NOXREF,NORLD,NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: mcpi_asm.asm 979 2017-12-29 18:40:40Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-29   979   1.2    some more code optimizations
* 2017-12-28   978   1.1    use inverse to avoid divide by constant
* 2017-11-12   961   1.0    Initial version
* 2017-10-10   955   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
        LD    FR0,=D'1.'
        LDR   FR2,FR0
        DD    FR0,RR32
        STD   FR0,RR32I          RR32I = 1./RR32
*
* read debug flags ----------------------------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get PRNT
        STC   R1,IDBGRR
        BAL   R14,IINT10         get PRNT
        STC   R1,IDBGRN
        BAL   R14,IINT10         get PRNT
        STC   R1,IDBGMC
        MVI   IEOFOK,X'01'       expect EOF from now on
*
        CLI   IDBGRR,X'00'       if any trace skip header print
        BNE   NOHDPRT
        CLI   IDBGRN,X'00'
        BNE   NOHDPRT
        CLI   IDBGMC,X'00'
        BNE   NOHDPRT
        L     R1,MSGHD1
        BAL   R14,OTEXT
        L     R1,MSGHD2
        BAL   R14,OTEXT
        BAL   R14,OPUTLINE       write header
NOHDPRT  EQU   *
*
* main body -----------------------------------------------------------
*
* outer loop
*
        XR    R3,R3              ntry = 0
        XR    R4,R4              nhit = 0
*
OLOOP    BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get PRNT
        LTR   R1,R1              is ngo == 0
        BE    OLOOPE             if = yes, quit outer loop
*
* inner loop
*
        LR    R2,R1              loop counter
*
ILOOP    EQU   *
        BAL   R8,RANNUM
        MD    FR0,=D'2.'
        SD    FR0,=D'1.'
        STD   FR0,X
        BAL   R8,RANNUM
        MD    FR0,=D'2.'
        SD    FR0,=D'1.'
        STD   FR0,Y
        MDR   FR0,FR0
        LD    FR2,X
        MDR   FR2,FR2
        ADR   FR0,FR2
        STD   FR0,R
        A     R3,=F'1'
        CD    FR0,=D'1.'
        BH    CMISS
        A     R4,=F'1'
CMISS    EQU   *
        CLI   IDBGMC,X'00'
        BE    NODBGMC
        L     R1,MSGMC
        BAL   R14,OTEXT          print "MC: "
        LD    FR0,X
        BAL   R14,OFIX1308       print x
        LD    FR0,Y
        BAL   R14,OFIX1308       print y
        LD    FR0,R
        BAL   R14,OFIX1308       print r
        LR    R1,R4
        BAL   R14,OINT10         print nhit
        BAL   R14,OPUTLINE       write line
*
NODBGMC  EQU   *
        BCT   R2,ILOOP
*
        L     R0,ODNZERO
        ST    R0,ODTEMP
        ST    R4,ODTEMP+4        nhit as denorm float
        SDR   FR0,FR0            FR0 := 0.
        AD    FR0,ODTEMP         add to re-normalize, FR0:=nhit
        ST    R3,ODTEMP+4        ntry as denorm float
        SDR   FR2,FR2            FR2 := 0.
        AD    FR2,ODTEMP         add to re-normalize, FR2:=ntry
        DDR   FR0,FR2            nhit/ntry
        MD    FR0,=D'4.'         piest = 4.*nhit/ntry
        STD   FR0,PIEST
        SD    FR0,PI             piest - pi
        LPDR  FR0,FR0            pierr = abs(piest - pi)
        STD   FR0,PIERR
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "PI: "
        LR    R1,R3
        BAL   R14,OINT10         print ntry
        LR    R1,R4
        BAL   R14,OINT10         print nhit
        LD    FR0,PIEST
        BAL   R14,OFIX1308       print piest
        LD    R0,PIERR
        BAL   R14,OFIX1308       print pierr
        LD    FR0,RLAST
        BAL   R14,OFIX1200       print rlast
        BAL   R14,OPUTLINE       write line
*
        B     OLOOP
OLOOPE   EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* RANNUM --------------------------------------------------------------
*   uses   all float regs
*   uses   R0,R1,R6,R7,R8,R9,R14,R15
*   keeps  R2-R5,R10-R11
*
RANNUM   CLI   RANINI,X'00'       init done ?
        BNE   RANNUMGO           if != yes
*
        L     R6,=A(RSHUF)       pointer to rshuf
        LA    R7,128             loop count
RANNUML  BAL   R9,RANRAW          get raw 
        STD   FR0,0(R6)          store
        LA    R6,8(R6)           push pointer
        BCT   R7,RANNUML         and loop
        MVI   RANINI,X'01'       ranini = true
*
RANNUMGO L     R6,=A(RSHUF)       pointer to rshuf
        LD    FR0,RLAST
        AW    FR0,ODNZERO        denormalize
        STD   FR0,RFAC1            
        L     R7,RFAC1+4         int(rlast)
        SRL   R7,25              int(rlast/rdiv)
        SLA   R7,3               convert index to offset
        LD    FR0,0(R7,R6)       rshuf[i]
        STD   FR0,RLAST          rlast = rshuf[i]
        BAL   R9,RANRAW          get new random number
        STD   FR0,0(R7,R6)       rshuf[i] = ranraw()
        LD    FR0,RLAST
        MD    FR0,RR32I          rlast*rr32i
        CLI   IDBGRN,X'00'       RN trace ?
        BE    RANNUMNT
*
        STD   FR0,RNEW           save rnew
        L     R1,MSGRN
        BAL   R14,OTEXT          print "RN: "
        LR    R1,R7
        SRA   R1,3               convert back to index
        BAL   R14,OINT10         print i
        LD    FR0,RLAST
        BAL   R14,OFIX1200       print rlast
        LD    FR0,RNEW
        BAL   R14,OFIX1308       print rnew
        BAL   R14,OPUTLINE       write line
        LD    FR0,RNEW           restore rnew
*
RANNUMNT EQU   *
*
        BR    R8
*
* RANRAW --------------------------------------------------------------
*   uses   all float regs
*   uses   R0,R1,R14,R15
*   keeps  R2-R11

RANRAW   LD    FR0,RSEED           
        MD    FR0,RFACTOR        rnew1 = rseed * 69069.
        LDR   FR6,FR0            save rnew1
        LDR   FR2,FR0            rmsb = rnew1
        AW    FR2,ODNZERO        denormalize
        STD   FR2,RFAC1          save
        XR    R1,R1              R1:=0
        ST    R1,RFAC1+4         clear lower 32 bits of rmsb
        SD    FR0,RFAC1          rnew = rnew1 modulo 2^32 !!
        STD   FR0,RNEW
        CLI   IDBGRR,X'00'       RR trace ?
        BE    RANRAWNT
*
        STD   FR4,RFAC           really save rfac
        STD   FR6,RNEW1          really save rnew1
        L     R1,MSGRR
        BAL   R14,OTEXT          print "RR: "
        LD    FR0,RSEED
        BAL   R14,OFIX1200       print rseed
        LD    FR0,RNEW
        BAL   R14,OFIX1200       print rnew
        L     R1,MSGCSEP
        BAL   R14,OTEXT          print " : "
        L     R1,RFAC+4
        BAL   R14,OINT10         print ifac
        BAL   R14,OPUTLINE       write line
*
RANRAWNT LD    FR0,RNEW
        STD   FR0,RSEED
        BR    R9
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
*
* OHEX10 ---------------------------------------------------
*   print integer, like C "  %8.8x" format 
*
OHEX10   ST    R14,OHEX10L        save R14
        L     R15,OLPTR          R15 points to edit position
        LA    R15,2(R15)         add two blanks
        LA    R14,8(R15)         end of buffer
*
OHEX10NL XR    R0,R0              R0 := 0
        SLDA  R0,4               get next 4 bits into R0
        AH    R0,=X'00F0'        add '0'
        CH    R0,=X'00F9'        above 9 ?
        BNH   OHEX10OK           if <= no, skip A-F correction
        SH    R0,=X'0039'        sub (0xF0('0')+10)-0xC1('A')=0x39
OHEX10OK STC   R0,0(R15)          store hex digit
        LA    R15,1(R15)         push pointer
        CR    R15,R14            beyond end ?
        BL    OHEX10NL           if < not, do next nibble
*
        ST    R15,OLPTR          store pointer
        L     R14,OHEX10L        restore R14 linkage
        BR    R14
*
OHEX10L  DS    F                  save area for R14 (return linkage)
*
* OHEX210 --------------------------------------------------
*   print 64 field as two 32 bit hex numbers
*     R1  points to memory location of 64 bit value
*     rendered as "  %8.8x  %8.8x"
*
OHEX210  ST    R14,OHEX210L       save R14
        ST    R1,OHEX210V        save R1
        L     R1,0(R1)           get high part
        BAL   R14,OHEX10         convert
        L     R1,OHEX210V
        L     R1,4(R1)           get low part
        BAL   R14,OHEX10         convert
        L     R14,OHEX210L       restore R14 linkage
        BR    R14                and return
*
OHEX210L DS    F                  save area for R14 (return linkage)
OHEX210V DS    F                  save area for R1 (value ptr)
*
* OFIX1308, OFIX1306 - -------------------------------------
*   print double, like
*     OFIX1308: PL/I F(13,8) or C %13.8f format 
*     OFIX1306: PL/I F(13,6) or C %13.6f format 
*     input value in floating reg FR0 
*     handles signed numbers
*
OFIX1308 MD    FR0,=D'1.E8'       'shift' 8 digits left
        LA    R1,OEF1308         pointer to edit pattern
        LA    R0,3               offset to one behind X'21' position
        B     OFIX13XX
*
OFIX1306 MD    FR0,=D'1.E6'       'shift' 6 digits left
        LA    R1,OEF1306         pointer to edit pattern
        LA    R0,5               offset to one behind X'21' position
*
OFIX13XX LPDR  FR2,FR0            get abbs() value
        CD    FR2,=D'2.E9'       too large ?
        BNL   OFX13XXF           if >= yes, do OSFILL
*
        LDR   FR4,FR2
        AW    FR4,ODNZERO        FR4 := de-normalized FR2
        SDR   FR6,FR6            FR6 := 0.
        ADR   FR6,FR4            get integer part 
        SDR   FR2,FR4            get fractional part
        CD    FR2,=D'0.5'        check if >= 0.5
        BL    OFX13XXR           if < no need to round up
        AW    FR4,ODNONE         otherwise add LSB DENORM
OFX13XXR STD   FR4,ODTEMP         roll-out to memory
        L     R15,ODTEMP+4       get integer part
        CVD   R15,OCVD           convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEF13XXL,R15),0(R1)   setup pattern
        LR    R1,R15             setup R1 in case of miss
        AR    R1,R0              to one behind X'21' position
        EDMK  0(OEF13XXL,R15),OCVD+2    and edit (and set R1)
        LTDR  FR0,FR0            negative number ?
        BNM   OFX13XXP           if >= not
        BCTR  R1,0               decrement pointer
        MVI   0(R1),C'-'         write '-' sign
OFX13XXP LA    R15,OEF13XXL(R15)  push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OFX13XXF LA    R1,OEF13XXL
        B     OSFILL
*
OEF1306  DC    C' ',3X'20',X'21',X'20',C'.',6X'20' pat: bddd(d.dddddd
OEF1308  DC    C' ',1X'20',X'21',X'20',C'.',8X'20' pat: bd(d.dddddddd
OEF13XXL EQU   *-OEF1308

*
* OFIX1200 -------------------------------------------------
*   print double, like PL/I F(12,0) or C %12.0f format 
*     input value in floating reg FR0 
*     only for non-negatve numbers
*
OFIX1200 LTDR  FR0,FR0            check whether negative 
        BL    OFX1200F           if < yes, do OSFILL
        CD    FR0,=D'99999999999.'  too large ?
        BH    OFX1200F           if > yes, do OSFILL
        AW    FR0,ODNZERO        de-normalize
        STD   FR0,ODTEMP         roll-out to memory
        L     R1,ODTEMP+4
        L     R0,ODTEMP
        N     R0,=X'00FFFFFF'
        D     R0,=F'100000000'   now R0 lower 9, R1 upper digits
        CVD   R0,OCVD            BCD convert lower part
        L     R15,OLPTR          R15 points to edit position
        LA    R15,2(R15)         add two blanks
        LTR   R1,R1              upper != 0
        BNZ   OFX1200B           if != yes, handle large number
*
        MVC   0(OEI10L,R15),OEI10   setup pattern (from OINT10)
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OFX1200B EQU   *
        MVC   0(OEF10LL,R15),OEF10L   setup pattern
        ED    0(OEF10LL,R15),OCVD+3  and edit
        CVD   R1,OCVD            BCD convert upper part
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEF10UL,R15),OEF10U  setup pattern
        ED    0(OEF10UL,R15),OCVD+6  and edit
        LA    R15,12(R15)        push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OFX1200F LA    R1,12
        B     OSFILL
*
OEF10L   DC    C' ',X'21',8X'20'                   pat: b(dddddddd
OEF10LL  EQU   *-OEF10L
OEF10U   DC    C' ',X'20',X'21',X'20'              pat: bd(d
OEF10UL  EQU   *-OEF10U
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
IDBGRR   DC    X'00'              trace RR enable
IDBGRN   DC    X'00'              trace RN enable
IDBGMC   DC    X'00'              trace MC enable
RANINI   DC    X'00'              init RSHUF done flag
        DS    0D
RFACTOR  DC    D'69069.'
RSEED    DC    D'12345.'
RLAST    DC    D'0.'
RR32     DC    D'4294967296.'     is 4*1024*1024*1024
RR32I    DS    D
RNEW     DS    D
RNEW1    DS    D
RFAC     DS    D
RFAC1    DS    D
*
PI       DC    D'3.141592653589793'
PIEST    DS    D
PIERR    DS    D
*
X        DS    D
Y        DS    D
R        DS    D
*
* message strings
*
MSGHD1   OTXTDSC C'          ntry      nhit       pi-est'
MSGHD2   OTXTDSC C'       pi-err        seed'
MSGMC    OTXTDSC C'MC: '
MSGPI    OTXTDSC C'PI: '
MSGRR    OTXTDSC C'RR: '
MSGRN    OTXTDSC C'RN: '
MSGCSEP  OTXTDSC C' : '
*
* spill literal pool
*
        LTORG
*
* data section
*
DATA     CSECT
RSHUF    DS    128D
*
* other defs and end
*
        YREGS ,
FR0      EQU   0
FR2      EQU   2
FR4      EQU   4
FR6      EQU   6
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIASMT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#ASM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=128K,TIME=(1,0),PRTY=8
//CLG EXEC ASMFCLG,
//      MAC1='SYS2.MACLIB',
//      PARM.ASM='NODECK,LOAD',
//      PARM.LKED='MAP,LIST,LET,NCAL',
//      COND.LKED=(8,LE,ASM),
//      PARM.GO='',
//      COND.GO=((8,LE,ASM),(4,LT,LKED))
//ASM.SYSUT1 DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(600,100))
//ASM.SYSUT2 DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSUT3 DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(300,50))
//ASM.SYSGO  DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(2000,500))
//ASM.SYSIN  DD *
*        1         2         3         4         5         6         71
*23456789*12345*789012345678901234*678901234567890123456789012345678901
* $Id: mcpi_asm.asm 979 2017-12-29 18:40:40Z mueller $
*
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
*
* This program is free software; you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
*
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-29   979   1.2    some more code optimizations
* 2017-12-28   978   1.1    use inverse to avoid divide by constant
* 2017-11-12   961   1.0    Initial version
* 2017-10-10   955   0.1    First draft
*
        PRINT NOGEN              don't show macro expansions
*
* Prime number search
*   RC =  0  ok
*   RC =  8  unexpected SYSIN EOF
*   RC = 12  open SYSIN failed
*   RC = 16  open SYSPRINT failed
*
* local macros --------------------------------------------------------
*
*
* OTXTDSC - setup text descriptor for simple output system -
*
        MACRO
&LABEL   OTXTDSC  &TEXT
TEXT     CSECT
SPTR&SYSNDX DC    &TEXT
&SYSECT  CSECT
        DS    0F
&LABEL   DC    AL1(L'SPTR&SYSNDX),AL3(SPTR&SYSNDX)
        MEND
*
* main preamble -------------------------------------------------------
*
MAIN     START 0                  start main code csect at base 0
        SAVE  (14,12)            Save input registers
        LR    R12,R15            base register := entry address
        USING MAIN,R12           declare base register
        ST    R13,SAVE+4         set back pointer in current save area
        LR    R2,R13             remember callers save area
        LA    R13,SAVE           setup current save area
        ST    R13,8(R2)          set forw pointer in callers save area
*
* open datasets -------------------------------------------------------
*
        OPEN  (SYSPRINT,OUTPUT)  open SYSPRINT
        LTR   R15,R15            test return code
        BE    OOPENOK
        MVI   RC+3,X'10'
        B     EXIT               quit with RC=16
OOPENOK  OPEN  (SYSIN,INPUT)      open SYSIN
        LTR   R15,R15            test return code
        BE    IOPENOK
        MVI   RC+3,X'0C'
        B     EXIT               quit with RC=12
IOPENOK  EQU   *
*
        LD    FR0,=D'1.'
        LDR   FR2,FR0
        DD    FR0,RR32
        STD   FR0,RR32I          RR32I = 1./RR32
*
* read debug flags ----------------------------------------------------
*
        BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get PRNT
        STC   R1,IDBGRR
        BAL   R14,IINT10         get PRNT
        STC   R1,IDBGRN
        BAL   R14,IINT10         get PRNT
        STC   R1,IDBGMC
        MVI   IEOFOK,X'01'       expect EOF from now on
*
        CLI   IDBGRR,X'00'       if any trace skip header print
        BNE   NOHDPRT
        CLI   IDBGRN,X'00'
        BNE   NOHDPRT
        CLI   IDBGMC,X'00'
        BNE   NOHDPRT
        L     R1,MSGHD1
        BAL   R14,OTEXT
        L     R1,MSGHD2
        BAL   R14,OTEXT
        BAL   R14,OPUTLINE       write header
NOHDPRT  EQU   *
*
* main body -----------------------------------------------------------
*
* outer loop
*
        XR    R3,R3              ntry = 0
        XR    R4,R4              nhit = 0
*
OLOOP    BAL   R14,IGETLINE       read input line
        BAL   R14,IINT10         get PRNT
        LTR   R1,R1              is ngo == 0
        BE    OLOOPE             if = yes, quit outer loop
*
* inner loop
*
        LR    R2,R1              loop counter
*
ILOOP    EQU   *
        BAL   R8,RANNUM
        MD    FR0,=D'2.'
        SD    FR0,=D'1.'
        STD   FR0,X
        BAL   R8,RANNUM
        MD    FR0,=D'2.'
        SD    FR0,=D'1.'
        STD   FR0,Y
        MDR   FR0,FR0
        LD    FR2,X
        MDR   FR2,FR2
        ADR   FR0,FR2
        STD   FR0,R
        A     R3,=F'1'
        CD    FR0,=D'1.'
        BH    CMISS
        A     R4,=F'1'
CMISS    EQU   *
        CLI   IDBGMC,X'00'
        BE    NODBGMC
        L     R1,MSGMC
        BAL   R14,OTEXT          print "MC: "
        LD    FR0,X
        BAL   R14,OFIX1308       print x
        LD    FR0,Y
        BAL   R14,OFIX1308       print y
        LD    FR0,R
        BAL   R14,OFIX1308       print r
        LR    R1,R4
        BAL   R14,OINT10         print nhit
        BAL   R14,OPUTLINE       write line
*
NODBGMC  EQU   *
        BCT   R2,ILOOP
*
        L     R0,ODNZERO
        ST    R0,ODTEMP
        ST    R4,ODTEMP+4        nhit as denorm float
        SDR   FR0,FR0            FR0 := 0.
        AD    FR0,ODTEMP         add to re-normalize, FR0:=nhit
        ST    R3,ODTEMP+4        ntry as denorm float
        SDR   FR2,FR2            FR2 := 0.
        AD    FR2,ODTEMP         add to re-normalize, FR2:=ntry
        DDR   FR0,FR2            nhit/ntry
        MD    FR0,=D'4.'         piest = 4.*nhit/ntry
        STD   FR0,PIEST
        SD    FR0,PI             piest - pi
        LPDR  FR0,FR0            pierr = abs(piest - pi)
        STD   FR0,PIERR
*
        L     R1,MSGPI
        BAL   R14,OTEXT          print "PI: "
        LR    R1,R3
        BAL   R14,OINT10         print ntry
        LR    R1,R4
        BAL   R14,OINT10         print nhit
        LD    FR0,PIEST
        BAL   R14,OFIX1308       print piest
        LD    R0,PIERR
        BAL   R14,OFIX1308       print pierr
        LD    FR0,RLAST
        BAL   R14,OFIX1200       print rlast
        BAL   R14,OPUTLINE       write line
*
        B     OLOOP
OLOOPE   EQU   *
*
* close datasets and return to OS -------------------------------------
*
EXIT     CLOSE SYSPRINT           close SYSPRINT
        CLOSE SYSIN              close SYSIN
        L     R13,SAVE+4         get old save area back
        L     R0,RC              get return code
        ST    R0,16(R13)         store in old save R15
        RETURN (14,12)           return to OS (will setup RC)
*
* RANNUM --------------------------------------------------------------
*   uses   all float regs
*   uses   R0,R1,R6,R7,R8,R9,R14,R15
*   keeps  R2-R5,R10-R11
*
RANNUM   CLI   RANINI,X'00'       init done ?
        BNE   RANNUMGO           if != yes
*
        L     R6,=A(RSHUF)       pointer to rshuf
        LA    R7,128             loop count
RANNUML  BAL   R9,RANRAW          get raw 
        STD   FR0,0(R6)          store
        LA    R6,8(R6)           push pointer
        BCT   R7,RANNUML         and loop
        MVI   RANINI,X'01'       ranini = true
*
RANNUMGO L     R6,=A(RSHUF)       pointer to rshuf
        LD    FR0,RLAST
        AW    FR0,ODNZERO        denormalize
        STD   FR0,RFAC1            
        L     R7,RFAC1+4         int(rlast)
        SRL   R7,25              int(rlast/rdiv)
        SLA   R7,3               convert index to offset
        LD    FR0,0(R7,R6)       rshuf[i]
        STD   FR0,RLAST          rlast = rshuf[i]
        BAL   R9,RANRAW          get new random number
        STD   FR0,0(R7,R6)       rshuf[i] = ranraw()
        LD    FR0,RLAST
        MD    FR0,RR32I          rlast*rr32i
        CLI   IDBGRN,X'00'       RN trace ?
        BE    RANNUMNT
*
        STD   FR0,RNEW           save rnew
        L     R1,MSGRN
        BAL   R14,OTEXT          print "RN: "
        LR    R1,R7
        SRA   R1,3               convert back to index
        BAL   R14,OINT10         print i
        LD    FR0,RLAST
        BAL   R14,OFIX1200       print rlast
        LD    FR0,RNEW
        BAL   R14,OFIX1308       print rnew
        BAL   R14,OPUTLINE       write line
        LD    FR0,RNEW           restore rnew
*
RANNUMNT EQU   *
*
        BR    R8
*
* RANRAW --------------------------------------------------------------
*   uses   all float regs
*   uses   R0,R1,R14,R15
*   keeps  R2-R11

RANRAW   LD    FR0,RSEED           
        MD    FR0,RFACTOR        rnew1 = rseed * 69069.
        LDR   FR6,FR0            save rnew1
        LDR   FR2,FR0            rmsb = rnew1
        AW    FR2,ODNZERO        denormalize
        STD   FR2,RFAC1          save
        XR    R1,R1              R1:=0
        ST    R1,RFAC1+4         clear lower 32 bits of rmsb
        SD    FR0,RFAC1          rnew = rnew1 modulo 2^32 !!
        STD   FR0,RNEW
        CLI   IDBGRR,X'00'       RR trace ?
        BE    RANRAWNT
*
        STD   FR4,RFAC           really save rfac
        STD   FR6,RNEW1          really save rnew1
        L     R1,MSGRR
        BAL   R14,OTEXT          print "RR: "
        LD    FR0,RSEED
        BAL   R14,OFIX1200       print rseed
        LD    FR0,RNEW
        BAL   R14,OFIX1200       print rnew
        L     R1,MSGCSEP
        BAL   R14,OTEXT          print " : "
        L     R1,RFAC+4
        BAL   R14,OINT10         print ifac
        BAL   R14,OPUTLINE       write line
*
RANRAWNT LD    FR0,RNEW
        STD   FR0,RSEED
        BR    R9
*
* include simple output system ----------------------------------------
*
* simple output system procedures -------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* OSKIP02 --------------------------------------------------
*   add 2 blanks
*
OSKIP02  LA    R1,2
*
* OSKIP ----------------------------------------------------
*   add blanks, count in R1
*
OSKIP    A     R1,OLPTR           new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OTAB  ----------------------------------------------------
*   set output column, position in R1
*
OTAB     A     R1,=A(OLBUF+1)     new edit position
        ST    R1,OLPTR           store pointer
        BR    R14
*
* OSFILL ---------------------------------------------------
*   add " ***" pattern, total length in R1
*
OSFILL   L     R15,OLPTR          R15 points to edit position
        MVI   0(R15),C' '        initial blank
        B     OSFILLN
OSFILLL  MVI   0(R15),C'*'        further '*'
OSFILLN  LA    R15,1(R15)
        BCT   R1,OSFILLL
        ST    R15,OLPTR          store pointer
        BR    R14
*
* OTEXT ----------------------------------------------------
*   print text, R1 hold descriptor address
*   descriptor format
*        DC  AL1(<length of string>)
*        DC  AL2(<address of string>)
*
OTEXT    ST    R14,OTEXTL         save R14
        LR    R14,R1
        SRL   R14,24             R14 now string length
        L     R15,OLPTR          R15 points to edit position
        LR    R0,R15             R0 too
        AR    R0,R14             push pointer, add length
        ST    R0,OLPTR           store pointer
        BCTR  R14,0              decrement length for EX
        EX    R14,OTEXTMVC       copy string via EX:MVC
        L     R14,OTEXTL         restore R14 linkage
        BR    R14
*
OTEXTMVC MVC   0(1,R15),0(R1)     length via EX, dst R15, src R1
OTEXTL   DS    F                  save area for R14 (return linkage)
*
* OPUTLINE -------------------------------------------------
*   write line to SYSPRINT
*
OPUTLINE ST    R14,OPUTLNEL       save R14
        L     R15,=A(OLBUF)
        CLI   133(R15),X'00'     check fence byte
        BNE   OPUTLNEA           crash if fence blown
        L     R1,=A(SYSPRINT)    R1 point to DCB
        LR    R0,R15             R1 point to buffer
        PUT   (1),(0)            write line
        L     R15,=A(OLBUF)      point to CC of OLBUF
        MVI   0(R15),C' '        blank OLBUF(0)
        MVC   1(L'OLBUF-1,R15),0(R15)    propagate blank
        LA    R15,1(R15)         point to 1st print char in OLBUF
        ST    R15,OLPTR          reset current position pointer
        LA    R15,1              
        AH    R15,OLCNT          increment line counter
        STH   R15,OLCNT
        SH    R15,OLMAX          R15 := OLCNT-OLMAX
        BL    OPUTLNES           if < no new page
        XR    R15,R15            R15 := 0
        SH    R15,OLCNT          clear line counter
        L     R15,=A(OLBUF)      point to CC of OLBUF
*        MVI   0(R15),C'1'        set new page CC in OLBUF
OPUTLNES L     R14,OPUTLNEL       restore R14 linkage
        BR    R14
*
OPUTLNEA ABEND 255                abend in case of errors
*
OPUTLNEL DS    F                  save area for R14 (return linkage)
*
* Work area for simple output system ------------------------
*
OLPTR    DC    A(OLBUF+1)         current output line position
OLCNT    DC    H'0'               line counter
OLMAX    DC    H'60'              lines per page
OCVD     DS    D                  buffer for CVD (8 byte, DW aligned)
*
ODTEMP   DS    D                  double buffer for conversions
ODNZERO  DC    X'4E000000',X'00000000'     denormalized double zero
ODNONE   DC    X'4E000000',X'00000001'     denormalized double one
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
              RECFM=FBA,LRECL=133,BLKSIZE=0
OLBUF    DC    CL133' ',X'00'     output line buffer and fence byte
*
MAIN     CSECT
*
* OINT10 ---------------------------------------------------
*   print integer, like PL/I F(10) or C %10d format
*   very fast, for non-negative numbers only !
*
OINT10   CL    R1,=F'999999999'   too large ?
        BH    OINT10F            if > yes, do OSFILL
        CVD   R1,OCVD            convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEI10L,R15),OEI10   setup pattern
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OINT10F  LA    R1,10
        B     OSFILL
*
OEI10    DC    C' ',7X'20',X'21',X'20'             pat: bddddddd(d
OEI10L   EQU   *-OEI10
*
* OHEX10 ---------------------------------------------------
*   print integer, like C "  %8.8x" format 
*
OHEX10   ST    R14,OHEX10L        save R14
        L     R15,OLPTR          R15 points to edit position
        LA    R15,2(R15)         add two blanks
        LA    R14,8(R15)         end of buffer
*
OHEX10NL XR    R0,R0              R0 := 0
        SLDA  R0,4               get next 4 bits into R0
        AH    R0,=X'00F0'        add '0'
        CH    R0,=X'00F9'        above 9 ?
        BNH   OHEX10OK           if <= no, skip A-F correction
        SH    R0,=X'0039'        sub (0xF0('0')+10)-0xC1('A')=0x39
OHEX10OK STC   R0,0(R15)          store hex digit
        LA    R15,1(R15)         push pointer
        CR    R15,R14            beyond end ?
        BL    OHEX10NL           if < not, do next nibble
*
        ST    R15,OLPTR          store pointer
        L     R14,OHEX10L        restore R14 linkage
        BR    R14
*
OHEX10L  DS    F                  save area for R14 (return linkage)
*
* OHEX210 --------------------------------------------------
*   print 64 field as two 32 bit hex numbers
*     R1  points to memory location of 64 bit value
*     rendered as "  %8.8x  %8.8x"
*
OHEX210  ST    R14,OHEX210L       save R14
        ST    R1,OHEX210V        save R1
        L     R1,0(R1)           get high part
        BAL   R14,OHEX10         convert
        L     R1,OHEX210V
        L     R1,4(R1)           get low part
        BAL   R14,OHEX10         convert
        L     R14,OHEX210L       restore R14 linkage
        BR    R14                and return
*
OHEX210L DS    F                  save area for R14 (return linkage)
OHEX210V DS    F                  save area for R1 (value ptr)
*
* OFIX1308, OFIX1306 - -------------------------------------
*   print double, like
*     OFIX1308: PL/I F(13,8) or C %13.8f format 
*     OFIX1306: PL/I F(13,6) or C %13.6f format 
*     input value in floating reg FR0 
*     handles signed numbers
*
OFIX1308 MD    FR0,=D'1.E8'       'shift' 8 digits left
        LA    R1,OEF1308         pointer to edit pattern
        LA    R0,3               offset to one behind X'21' position
        B     OFIX13XX
*
OFIX1306 MD    FR0,=D'1.E6'       'shift' 6 digits left
        LA    R1,OEF1306         pointer to edit pattern
        LA    R0,5               offset to one behind X'21' position
*
OFIX13XX LPDR  FR2,FR0            get abbs() value
        CD    FR2,=D'2.E9'       too large ?
        BNL   OFX13XXF           if >= yes, do OSFILL
*
        LDR   FR4,FR2
        AW    FR4,ODNZERO        FR4 := de-normalized FR2
        SDR   FR6,FR6            FR6 := 0.
        ADR   FR6,FR4            get integer part 
        SDR   FR2,FR4            get fractional part
        CD    FR2,=D'0.5'        check if >= 0.5
        BL    OFX13XXR           if < no need to round up
        AW    FR4,ODNONE         otherwise add LSB DENORM
OFX13XXR STD   FR4,ODTEMP         roll-out to memory
        L     R15,ODTEMP+4       get integer part
        CVD   R15,OCVD           convert
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEF13XXL,R15),0(R1)   setup pattern
        LR    R1,R15             setup R1 in case of miss
        AR    R1,R0              to one behind X'21' position
        EDMK  0(OEF13XXL,R15),OCVD+2    and edit (and set R1)
        LTDR  FR0,FR0            negative number ?
        BNM   OFX13XXP           if >= not
        BCTR  R1,0               decrement pointer
        MVI   0(R1),C'-'         write '-' sign
OFX13XXP LA    R15,OEF13XXL(R15)  push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OFX13XXF LA    R1,OEF13XXL
        B     OSFILL
*
OEF1306  DC    C' ',3X'20',X'21',X'20',C'.',6X'20' pat: bddd(d.dddddd
OEF1308  DC    C' ',1X'20',X'21',X'20',C'.',8X'20' pat: bd(d.dddddddd
OEF13XXL EQU   *-OEF1308

*
* OFIX1200 -------------------------------------------------
*   print double, like PL/I F(12,0) or C %12.0f format 
*     input value in floating reg FR0 
*     only for non-negatve numbers
*
OFIX1200 LTDR  FR0,FR0            check whether negative 
        BL    OFX1200F           if < yes, do OSFILL
        CD    FR0,=D'99999999999.'  too large ?
        BH    OFX1200F           if > yes, do OSFILL
        AW    FR0,ODNZERO        de-normalize
        STD   FR0,ODTEMP         roll-out to memory
        L     R1,ODTEMP+4
        L     R0,ODTEMP
        N     R0,=X'00FFFFFF'
        D     R0,=F'100000000'   now R0 lower 9, R1 upper digits
        CVD   R0,OCVD            BCD convert lower part
        L     R15,OLPTR          R15 points to edit position
        LA    R15,2(R15)         add two blanks
        LTR   R1,R1              upper != 0
        BNZ   OFX1200B           if != yes, handle large number
*
        MVC   0(OEI10L,R15),OEI10   setup pattern (from OINT10)
        ED    0(OEI10L,R15),OCVD+3  and edit
        LA    R15,OEI10L(R15)       push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OFX1200B EQU   *
        MVC   0(OEF10LL,R15),OEF10L   setup pattern
        ED    0(OEF10LL,R15),OCVD+3  and edit
        CVD   R1,OCVD            BCD convert upper part
        L     R15,OLPTR          R15 points to edit position
        MVC   0(OEF10UL,R15),OEF10U  setup pattern
        ED    0(OEF10UL,R15),OCVD+6  and edit
        LA    R15,12(R15)        push pointer
        ST    R15,OLPTR          store pointer
        BR    R14
*
OFX1200F LA    R1,12
        B     OSFILL
*
OEF10L   DC    C' ',X'21',8X'20'                   pat: b(dddddddd
OEF10LL  EQU   *-OEF10L
OEF10U   DC    C' ',X'20',X'21',X'20'              pat: bd(d
OEF10UL  EQU   *-OEF10U
* include simple input system -----------------------------------------
*
* simple input system procedures --------------------------------------
* calling and register convention:
*    R1       holds value (or descriptor pointer)
*    R0,R1    may be modified
*    R14,R15  may be modified
*    R2-R11   are not changed
*
* in short
*    R1 holds input or output value (or pointer)
*    call with BAL  R14,<routine>
*
* IGETLINE -------------------------------------------------
*   read line from SYSIN
*   EOF handling:
*   - IEOFOK holds the 'EOF OK' flag
*   - if EOF seen and IEOFOK  = X'00', program ends with RC=8
*   - if EOF seen and IEOFOK != X'00', program ends with RC=0
*
IGETLINE ST    R14,IGETLNEL       save R14
        L     R1,=A(SYSIN)
        L     R0,=A(ILBUF)
        GET   (1),(0)            read line
        L     R0,=A(ILBUF)
        ST    R0,ILPTR           set input ptr to begin of line
        L     R14,IGETLNEL       restore R14 linkage
        BR    R14
*
IGETLNEL DS    F                  save area for R14 (return linkage)
*
* IEOFHDL --------------------------------------------------
*
IEOFHDL  BALR  R12,R0             where are we ?
        LA    R15,*-MAIN         offset from MAIN to here
        SR    R12,R15            base reg now points to MAIN
        LA    R14,EXIT
        CLI   IEOFOK,X'00'       is EOF ok ?
        BNER  R14                if != yes, jump to EXIT
        MVI   RC+3,X'08'         otherwise set RC=8
        BR    R14                and jump to EXIT
*
* Work area for simple output system ------------------------
*
ILPTR    DC    A(ILBUF)           current input line position
IEOFOK   DS    X'00'              EOF ok flag
ICVB     DS    D                  buffer for CVB (8 byte, DW aligned)
*
* DCB and OLBUF in separate CSECT
*
SIOSDATA CSECT
        DS    0F
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,EODAD=IEOFHDL            X
              RECFM=FB,LRECL=80,BLKSIZE=0
ILBUF    DC    CL80' '            input line buffer
MAIN     CSECT
*
* IINT10 ---------------------------------------------------
*   read integer, like PL/I F(10) or C %10d format 
*
IINT10   L     R15,ILPTR          get input pointer
        PACK  ICVB(8),0(10,R15)  pack next 10 char
        CVB   R1,ICVB            and convert
        LA    R15,10(R15)        push pointer by 10 char
        ST    R15,ILPTR          and update
        BR    R14
*
* Work area definitions -----------------------------------------------
*
SAVE     DS    18F                local save area
RC       DC    F'0'               return code
IDBGRR   DC    X'00'              trace RR enable
IDBGRN   DC    X'00'              trace RN enable
IDBGMC   DC    X'00'              trace MC enable
RANINI   DC    X'00'              init RSHUF done flag
        DS    0D
RFACTOR  DC    D'69069.'
RSEED    DC    D'12345.'
RLAST    DC    D'0.'
RR32     DC    D'4294967296.'     is 4*1024*1024*1024
RR32I    DS    D
RNEW     DS    D
RNEW1    DS    D
RFAC     DS    D
RFAC1    DS    D
*
PI       DC    D'3.141592653589793'
PIEST    DS    D
PIERR    DS    D
*
X        DS    D
Y        DS    D
R        DS    D
*
* message strings
*
MSGHD1   OTXTDSC C'          ntry      nhit       pi-est'
MSGHD2   OTXTDSC C'       pi-err        seed'
MSGMC    OTXTDSC C'MC: '
MSGPI    OTXTDSC C'PI: '
MSGRR    OTXTDSC C'RR: '
MSGRN    OTXTDSC C'RN: '
MSGCSEP  OTXTDSC C' : '
*
* spill literal pool
*
        LTORG
*
* data section
*
DATA     CSECT
RSHUF    DS    128D
*
* other defs and end
*
        YREGS ,
FR0      EQU   0
FR2      EQU   2
FR4      EQU   4
FR6      EQU   6
        END   MAIN               define main entry point
/*
//GO.SYSUDUMP DD SYSOUT=*,OUTLIM=2000
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIGCCF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(10,0),PRTY=2
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: mcpi_cc.c 978 2017-12-28 21:32:18Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-28   978   1.1    use inverse to avoid divide by constant */
/* 2017-08-12   938   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>

double rseed = 12345.;
double rlast = 0.;
double rshuf[128];
double rr32 = 4294967296.;                 /* 4*1024*1024*1024 */
double rdiv =   33554432.;                 /* rr32 / 128 */
double rr32i;                              /* setup in main() */
double rdivi;                              /* setup in main() */
int ranini = 0;
int idbgrr = 0;
int idbgrn = 0;
int idbgmc = 0;

double ranraw()
{
 double rnew,rnew1;
 double rfac;
 int    ifac;

 rnew1 = rseed * 69069.;
 rfac  = rnew1 * rr32i;
 ifac  = rfac;
 rfac  = ifac;
 rnew  = rnew1 - rfac * rr32;
 if (idbgrr) printf("RR: %12.0f %12.0f : %16.0f %9d\n",
                    rseed,rnew, rnew1,ifac);
 rseed = rnew;                    
 return rnew;
}

double rannum()
{
 int i;
 double rnew;

 if (ranini == 0) {
   for (i=0; i<128; i++) rshuf[i] = ranraw();
   ranini = 1;
 }

 i = rlast * rdivi;
 rlast = rshuf[i];
 rshuf[i] = ranraw();
 rnew = rlast * rr32i;
 if (idbgrn) printf("RN: %12d %12.0f %12.8f\n", i,rlast,rnew);
 return rnew;
}

int main() 
{
 int i;
 int ntry = 0;
 int nhit = 0;
 int ngo;
 double pi = 3.141592653589793;
 double piest;
 double pierr;

 /* setup global constants */
 rr32i = 1./rr32;
 rdivi = 1./rdiv;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d %d", &idbgrr, &idbgrn, &idbgmc) != 3) {
   printf("conversion error, abort\n");
   return 1;
 }

 if (idbgrr == 0 && idbgrn == 0 && idbgmc == 0)
   printf("            ntry         nhit       pi-est"
          "       pi-err         seed\n");

 while (scanf(" %d", &ngo) == 1 && ngo > 0) {
   for (i=0; i<ngo; i++) {
     double x,y,r;
     x = 2.*rannum() - 1.;
     y = 2.*rannum() - 1.;
     r = x*x + y*y;
     ntry += 1;
     if (r <= 1.) nhit += 1;
     if (idbgmc) printf("MC: %12.8f %12.8f %12.8f %12d\n", x,y,r,nhit);
   }

   piest = 4. * ((double)nhit / (double)ntry);
   pierr = piest - pi;
   if (pierr < 0.) pierr = -pierr;
   printf("PI: %12d %12d %12.8f %12.8f %12.0f\n",
          ntry, nhit, piest, pierr, rlast);

 }
 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIGCCT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#GCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC GCCCLG,COPTS='-O3',
//      PARM.LKED='MAP,LIST'
//COMP.SYSIN DD DATA,DLM='/@'
/* $Id: mcpi_cc.c 978 2017-12-28 21:32:18Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-28   978   1.1    use inverse to avoid divide by constant */
/* 2017-08-12   938   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>

double rseed = 12345.;
double rlast = 0.;
double rshuf[128];
double rr32 = 4294967296.;                 /* 4*1024*1024*1024 */
double rdiv =   33554432.;                 /* rr32 / 128 */
double rr32i;                              /* setup in main() */
double rdivi;                              /* setup in main() */
int ranini = 0;
int idbgrr = 0;
int idbgrn = 0;
int idbgmc = 0;

double ranraw()
{
 double rnew,rnew1;
 double rfac;
 int    ifac;

 rnew1 = rseed * 69069.;
 rfac  = rnew1 * rr32i;
 ifac  = rfac;
 rfac  = ifac;
 rnew  = rnew1 - rfac * rr32;
 if (idbgrr) printf("RR: %12.0f %12.0f : %16.0f %9d\n",
                    rseed,rnew, rnew1,ifac);
 rseed = rnew;                    
 return rnew;
}

double rannum()
{
 int i;
 double rnew;

 if (ranini == 0) {
   for (i=0; i<128; i++) rshuf[i] = ranraw();
   ranini = 1;
 }

 i = rlast * rdivi;
 rlast = rshuf[i];
 rshuf[i] = ranraw();
 rnew = rlast * rr32i;
 if (idbgrn) printf("RN: %12d %12.0f %12.8f\n", i,rlast,rnew);
 return rnew;
}

int main() 
{
 int i;
 int ntry = 0;
 int nhit = 0;
 int ngo;
 double pi = 3.141592653589793;
 double piest;
 double pierr;

 /* setup global constants */
 rr32i = 1./rr32;
 rdivi = 1./rdiv;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d %d", &idbgrr, &idbgrn, &idbgmc) != 3) {
   printf("conversion error, abort\n");
   return 1;
 }

 if (idbgrr == 0 && idbgrn == 0 && idbgmc == 0)
   printf("            ntry         nhit       pi-est"
          "       pi-err         seed\n");

 while (scanf(" %d", &ngo) == 1 && ngo > 0) {
   for (i=0; i<ngo; i++) {
     double x,y,r;
     x = 2.*rannum() - 1.;
     y = 2.*rannum() - 1.;
     r = x*x + y*y;
     ntry += 1;
     if (r <= 1.) nhit += 1;
     if (idbgmc) printf("MC: %12.8f %12.8f %12.8f %12d\n", x,y,r,nhit);
   }

   piest = 4. * ((double)nhit / (double)ntry);
   pierr = piest - pi;
   if (pierr < 0.) pierr = -pierr;
   printf("PI: %12d %12d %12.8f %12.8f %12.0f\n",
          ntry, nhit, piest, pierr, rlast);

 }
 return 0;
}
/@
//GO.SYSPRINT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIJCCF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(10,0),PRTY=2
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: mcpi_cc.c 978 2017-12-28 21:32:18Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-28   978   1.1    use inverse to avoid divide by constant */
/* 2017-08-12   938   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>

double rseed = 12345.;
double rlast = 0.;
double rshuf[128];
double rr32 = 4294967296.;                 /* 4*1024*1024*1024 */
double rdiv =   33554432.;                 /* rr32 / 128 */
double rr32i;                              /* setup in main() */
double rdivi;                              /* setup in main() */
int ranini = 0;
int idbgrr = 0;
int idbgrn = 0;
int idbgmc = 0;

double ranraw()
{
 double rnew,rnew1;
 double rfac;
 int    ifac;

 rnew1 = rseed * 69069.;
 rfac  = rnew1 * rr32i;
 ifac  = rfac;
 rfac  = ifac;
 rnew  = rnew1 - rfac * rr32;
 if (idbgrr) printf("RR: %12.0f %12.0f : %16.0f %9d\n",
                    rseed,rnew, rnew1,ifac);
 rseed = rnew;                    
 return rnew;
}

double rannum()
{
 int i;
 double rnew;

 if (ranini == 0) {
   for (i=0; i<128; i++) rshuf[i] = ranraw();
   ranini = 1;
 }

 i = rlast * rdivi;
 rlast = rshuf[i];
 rshuf[i] = ranraw();
 rnew = rlast * rr32i;
 if (idbgrn) printf("RN: %12d %12.0f %12.8f\n", i,rlast,rnew);
 return rnew;
}

int main() 
{
 int i;
 int ntry = 0;
 int nhit = 0;
 int ngo;
 double pi = 3.141592653589793;
 double piest;
 double pierr;

 /* setup global constants */
 rr32i = 1./rr32;
 rdivi = 1./rdiv;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d %d", &idbgrr, &idbgrn, &idbgmc) != 3) {
   printf("conversion error, abort\n");
   return 1;
 }

 if (idbgrr == 0 && idbgrn == 0 && idbgmc == 0)
   printf("            ntry         nhit       pi-est"
          "       pi-err         seed\n");

 while (scanf(" %d", &ngo) == 1 && ngo > 0) {
   for (i=0; i<ngo; i++) {
     double x,y,r;
     x = 2.*rannum() - 1.;
     y = 2.*rannum() - 1.;
     r = x*x + y*y;
     ntry += 1;
     if (r <= 1.) nhit += 1;
     if (idbgmc) printf("MC: %12.8f %12.8f %12.8f %12d\n", x,y,r,nhit);
   }

   piest = 4. * ((double)nhit / (double)ntry);
   pierr = piest - pi;
   if (pierr < 0.) pierr = -pierr;
   printf("PI: %12d %12d %12.8f %12.8f %12.0f\n",
          ntry, nhit, piest, pierr, rlast);

 }
 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIJCCT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#JCC JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=8192K,TIME=(1,0),PRTY=8
//CLG EXEC JCCCLG,
//      JOPTS='-o',
//      PARM.LKED='NCAL,MAP,LIST,NORENT'
//COMPILE.SYSIN DD DATA,DLM='/@'
/* $Id: mcpi_cc.c 978 2017-12-28 21:32:18Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-28   978   1.1    use inverse to avoid divide by constant */
/* 2017-08-12   938   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

#include <stdio.h>
#include <stdlib.h>

double rseed = 12345.;
double rlast = 0.;
double rshuf[128];
double rr32 = 4294967296.;                 /* 4*1024*1024*1024 */
double rdiv =   33554432.;                 /* rr32 / 128 */
double rr32i;                              /* setup in main() */
double rdivi;                              /* setup in main() */
int ranini = 0;
int idbgrr = 0;
int idbgrn = 0;
int idbgmc = 0;

double ranraw()
{
 double rnew,rnew1;
 double rfac;
 int    ifac;

 rnew1 = rseed * 69069.;
 rfac  = rnew1 * rr32i;
 ifac  = rfac;
 rfac  = ifac;
 rnew  = rnew1 - rfac * rr32;
 if (idbgrr) printf("RR: %12.0f %12.0f : %16.0f %9d\n",
                    rseed,rnew, rnew1,ifac);
 rseed = rnew;                    
 return rnew;
}

double rannum()
{
 int i;
 double rnew;

 if (ranini == 0) {
   for (i=0; i<128; i++) rshuf[i] = ranraw();
   ranini = 1;
 }

 i = rlast * rdivi;
 rlast = rshuf[i];
 rshuf[i] = ranraw();
 rnew = rlast * rr32i;
 if (idbgrn) printf("RN: %12d %12.0f %12.8f\n", i,rlast,rnew);
 return rnew;
}

int main() 
{
 int i;
 int ntry = 0;
 int nhit = 0;
 int ngo;
 double pi = 3.141592653589793;
 double piest;
 double pierr;

 /* setup global constants */
 rr32i = 1./rr32;
 rdivi = 1./rdiv;

 /* JCC on MVS doesn't skip initial white space, add leading ' ' to force */
 if (scanf(" %d %d %d", &idbgrr, &idbgrn, &idbgmc) != 3) {
   printf("conversion error, abort\n");
   return 1;
 }

 if (idbgrr == 0 && idbgrn == 0 && idbgmc == 0)
   printf("            ntry         nhit       pi-est"
          "       pi-err         seed\n");

 while (scanf(" %d", &ngo) == 1 && ngo > 0) {
   for (i=0; i<ngo; i++) {
     double x,y,r;
     x = 2.*rannum() - 1.;
     y = 2.*rannum() - 1.;
     r = x*x + y*y;
     ntry += 1;
     if (r <= 1.) nhit += 1;
     if (idbgmc) printf("MC: %12.8f %12.8f %12.8f %12d\n", x,y,r,nhit);
   }

   piest = 4. * ((double)nhit / (double)ntry);
   pierr = piest - pi;
   if (pierr < 0.) pierr = -pierr;
   printf("PI: %12d %12d %12.8f %12.8f %12.0f\n",
          ntry, nhit, piest, pierr, rlast);

 }
 return 0;
}
/@
//GO.STDOUT DD SYSOUT=*,OUTLIM=5000
//GO.STDERR DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIFOGF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(10,0),PRTY=2
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: mcpi_for.f 978 2017-12-28 21:32:18Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-28   978   1.1    use inverse to avoid divide by constant
C 2017-08-12   938   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- function ranraw ------------------------------------------------
C
C Fortran IV(1966): function syntax is: 'type FUNCTION name*precision (args)
C however gfortran -std=legacy wants:   'type*precision FUNCTION name (args)
     REAL FUNCTION RANRAW*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RFAC,RNEW
     INTEGER IFAC
C
     RNEW = RSEED * 69069.D0
     RFAC = RNEW * RR32I
     IFAC = RFAC
     RFAC = IFAC
     RNEW = RNEW - RFAC * RR32
     IF (IDBGRR .NE. 0) WRITE(6,9000) RSEED,RNEW
     RSEED = RNEW
     RANRAW = RNEW
     RETURN
C
9000 FORMAT(1X,'RR: ',F12.0,1X,F12.0)
     END
C
C --- function rannum ------------------------------------------------
C
     REAL FUNCTION RANNUM*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RANRAW
     INTEGER I
C
     IF (RANINI) GOTO 1000
     DO 100 I=1,128
       RSHUF(I) = RANRAW(DUMMY)
100  CONTINUE
     RANINI = .TRUE.
1000 CONTINUE
C     
     I = RLAST * RDIVI
     RLAST = RSHUF(I+1)
     RSHUF(I+1) = RANRAW(DUMMY)
     RANNUM = RLAST * RR32I
     IF (IDBGRN .NE. 0) WRITE(6,9000) I,RLAST,RANNUM
     RETURN
C
9000 FORMAT(1X,'RN: ',I12,1X,F12.0,1X,F12.8)
     END
C
C --- main program ---------------------------------------------------
C     PROGRAM MCPI
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     INTEGER I
     INTEGER NTRY,NHIT,NGO
     REAL*8 PI,PIEST,PIERR
     REAL*8 X,Y,R
     REAL*8 DUMMY
     REAL*8 RANNUM
     REAL*8 RTRY,RHIT
     DATA PI /3.141592653589793D0/
     DATA NTRY /0/
     DATA NHIT /0/
C
     RR32   = 4294967296.D0
     RR32I  = 1./RR32
     RDIV   = 33554432.D0
     RDIVI  = 1./RDIV
C
     RSEED  = 12345.D0
     RLAST  = 0.D0
     RANINI = .FALSE.
C
     READ(5,9000,ERR=910,END=900) IDBGRR,IDBGRN,IDBGMC
C
     IF (IDBGRR.EQ.0 .AND. IDBGRN.EQ.0 .AND. IDBGMC.EQ.0)
    X  WRITE(6,9005)
C
100  READ(5,9010,ERR=910,END=900) NGO
     IF (NGO .LE. 0) GOTO 900
C
     DO 200 I=1,NGO
       X = 2.*RANNUM(DUMMY) - 1.
       Y = 2.*RANNUM(DUMMY) - 1.
       R = X*X + Y*Y
       NTRY = NTRY + 1
       IF (R .LE. 1.) NHIT = NHIT + 1
       IF (IDBGMC .NE. 0) WRITE(6,9030) X,Y,R,NHIT
200  CONTINUE
C
     RTRY = NTRY
     RHIT = NHIT
     PIEST = 4.* RHIT / RTRY
     PIERR = PIEST - PI
     IF (PIERR .LT. 0.) PIERR = -PIERR
     WRITE(6,9020) NTRY, NHIT,PIEST,PIERR,RLAST
     GOTO 100
C
900  CONTINUE
C
     STOP
C
910  WRITE(6,9040)
     STOP
C
9000 FORMAT(3I10)
9005 FORMAT(1X,'            ntry         nhit       pi-est',
    X          '       pi-err        seed')
9010 FORMAT(I10)
9020 FORMAT(1X,'PI: ',I12,1X,I12,1X,F12.8,1X,F12.8,1X,F12.0)
9030 FORMAT(1X,'MC: ',F12.8,1X,F12.8,1X,F12.8,1X,I12)
9040 FORMAT(1X,'conversion error, abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIFOGT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#FOG JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTGCLG,
//      PARM.FORT='',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: mcpi_for.f 978 2017-12-28 21:32:18Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-28   978   1.1    use inverse to avoid divide by constant
C 2017-08-12   938   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- function ranraw ------------------------------------------------
C
C Fortran IV(1966): function syntax is: 'type FUNCTION name*precision (args)
C however gfortran -std=legacy wants:   'type*precision FUNCTION name (args)
     REAL FUNCTION RANRAW*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RFAC,RNEW
     INTEGER IFAC
C
     RNEW = RSEED * 69069.D0
     RFAC = RNEW * RR32I
     IFAC = RFAC
     RFAC = IFAC
     RNEW = RNEW - RFAC * RR32
     IF (IDBGRR .NE. 0) WRITE(6,9000) RSEED,RNEW
     RSEED = RNEW
     RANRAW = RNEW
     RETURN
C
9000 FORMAT(1X,'RR: ',F12.0,1X,F12.0)
     END
C
C --- function rannum ------------------------------------------------
C
     REAL FUNCTION RANNUM*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RANRAW
     INTEGER I
C
     IF (RANINI) GOTO 1000
     DO 100 I=1,128
       RSHUF(I) = RANRAW(DUMMY)
100  CONTINUE
     RANINI = .TRUE.
1000 CONTINUE
C     
     I = RLAST * RDIVI
     RLAST = RSHUF(I+1)
     RSHUF(I+1) = RANRAW(DUMMY)
     RANNUM = RLAST * RR32I
     IF (IDBGRN .NE. 0) WRITE(6,9000) I,RLAST,RANNUM
     RETURN
C
9000 FORMAT(1X,'RN: ',I12,1X,F12.0,1X,F12.8)
     END
C
C --- main program ---------------------------------------------------
C     PROGRAM MCPI
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     INTEGER I
     INTEGER NTRY,NHIT,NGO
     REAL*8 PI,PIEST,PIERR
     REAL*8 X,Y,R
     REAL*8 DUMMY
     REAL*8 RANNUM
     REAL*8 RTRY,RHIT
     DATA PI /3.141592653589793D0/
     DATA NTRY /0/
     DATA NHIT /0/
C
     RR32   = 4294967296.D0
     RR32I  = 1./RR32
     RDIV   = 33554432.D0
     RDIVI  = 1./RDIV
C
     RSEED  = 12345.D0
     RLAST  = 0.D0
     RANINI = .FALSE.
C
     READ(5,9000,ERR=910,END=900) IDBGRR,IDBGRN,IDBGMC
C
     IF (IDBGRR.EQ.0 .AND. IDBGRN.EQ.0 .AND. IDBGMC.EQ.0)
    X  WRITE(6,9005)
C
100  READ(5,9010,ERR=910,END=900) NGO
     IF (NGO .LE. 0) GOTO 900
C
     DO 200 I=1,NGO
       X = 2.*RANNUM(DUMMY) - 1.
       Y = 2.*RANNUM(DUMMY) - 1.
       R = X*X + Y*Y
       NTRY = NTRY + 1
       IF (R .LE. 1.) NHIT = NHIT + 1
       IF (IDBGMC .NE. 0) WRITE(6,9030) X,Y,R,NHIT
200  CONTINUE
C
     RTRY = NTRY
     RHIT = NHIT
     PIEST = 4.* RHIT / RTRY
     PIERR = PIEST - PI
     IF (PIERR .LT. 0.) PIERR = -PIERR
     WRITE(6,9020) NTRY, NHIT,PIEST,PIERR,RLAST
     GOTO 100
C
900  CONTINUE
C
     STOP
C
910  WRITE(6,9040)
     STOP
C
9000 FORMAT(3I10)
9005 FORMAT(1X,'            ntry         nhit       pi-est',
    X          '       pi-err        seed')
9010 FORMAT(I10)
9020 FORMAT(1X,'PI: ',I12,1X,I12,1X,F12.8,1X,F12.8,1X,F12.0)
9030 FORMAT(1X,'MC: ',F12.8,1X,F12.8,1X,F12.8,1X,I12)
9040 FORMAT(1X,'conversion error, abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIFOHF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1440),PRTY=2
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: mcpi_for.f 978 2017-12-28 21:32:18Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-28   978   1.1    use inverse to avoid divide by constant
C 2017-08-12   938   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- function ranraw ------------------------------------------------
C
C Fortran IV(1966): function syntax is: 'type FUNCTION name*precision (args)
C however gfortran -std=legacy wants:   'type*precision FUNCTION name (args)
     REAL FUNCTION RANRAW*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RFAC,RNEW
     INTEGER IFAC
C
     RNEW = RSEED * 69069.D0
     RFAC = RNEW * RR32I
     IFAC = RFAC
     RFAC = IFAC
     RNEW = RNEW - RFAC * RR32
     IF (IDBGRR .NE. 0) WRITE(6,9000) RSEED,RNEW
     RSEED = RNEW
     RANRAW = RNEW
     RETURN
C
9000 FORMAT(1X,'RR: ',F12.0,1X,F12.0)
     END
C
C --- function rannum ------------------------------------------------
C
     REAL FUNCTION RANNUM*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RANRAW
     INTEGER I
C
     IF (RANINI) GOTO 1000
     DO 100 I=1,128
       RSHUF(I) = RANRAW(DUMMY)
100  CONTINUE
     RANINI = .TRUE.
1000 CONTINUE
C     
     I = RLAST * RDIVI
     RLAST = RSHUF(I+1)
     RSHUF(I+1) = RANRAW(DUMMY)
     RANNUM = RLAST * RR32I
     IF (IDBGRN .NE. 0) WRITE(6,9000) I,RLAST,RANNUM
     RETURN
C
9000 FORMAT(1X,'RN: ',I12,1X,F12.0,1X,F12.8)
     END
C
C --- main program ---------------------------------------------------
C     PROGRAM MCPI
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     INTEGER I
     INTEGER NTRY,NHIT,NGO
     REAL*8 PI,PIEST,PIERR
     REAL*8 X,Y,R
     REAL*8 DUMMY
     REAL*8 RANNUM
     REAL*8 RTRY,RHIT
     DATA PI /3.141592653589793D0/
     DATA NTRY /0/
     DATA NHIT /0/
C
     RR32   = 4294967296.D0
     RR32I  = 1./RR32
     RDIV   = 33554432.D0
     RDIVI  = 1./RDIV
C
     RSEED  = 12345.D0
     RLAST  = 0.D0
     RANINI = .FALSE.
C
     READ(5,9000,ERR=910,END=900) IDBGRR,IDBGRN,IDBGMC
C
     IF (IDBGRR.EQ.0 .AND. IDBGRN.EQ.0 .AND. IDBGMC.EQ.0)
    X  WRITE(6,9005)
C
100  READ(5,9010,ERR=910,END=900) NGO
     IF (NGO .LE. 0) GOTO 900
C
     DO 200 I=1,NGO
       X = 2.*RANNUM(DUMMY) - 1.
       Y = 2.*RANNUM(DUMMY) - 1.
       R = X*X + Y*Y
       NTRY = NTRY + 1
       IF (R .LE. 1.) NHIT = NHIT + 1
       IF (IDBGMC .NE. 0) WRITE(6,9030) X,Y,R,NHIT
200  CONTINUE
C
     RTRY = NTRY
     RHIT = NHIT
     PIEST = 4.* RHIT / RTRY
     PIERR = PIEST - PI
     IF (PIERR .LT. 0.) PIERR = -PIERR
     WRITE(6,9020) NTRY, NHIT,PIEST,PIERR,RLAST
     GOTO 100
C
900  CONTINUE
C
     STOP
C
910  WRITE(6,9040)
     STOP
C
9000 FORMAT(3I10)
9005 FORMAT(1X,'            ntry         nhit       pi-est',
    X          '       pi-err        seed')
9010 FORMAT(I10)
9020 FORMAT(1X,'PI: ',I12,1X,I12,1X,F12.8,1X,F12.8,1X,F12.0)
9030 FORMAT(1X,'MC: ',F12.8,1X,F12.8,1X,F12.8,1X,I12)
9040 FORMAT(1X,'conversion error, abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIFOHT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#FOH JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC FORTHCLG,
//      PARM.FORT='OPT=2',
//      PARM.LKED='MAP,LIST,LET'
//FORT.SYSIN DD *
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: mcpi_for.f 978 2017-12-28 21:32:18Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-28   978   1.1    use inverse to avoid divide by constant
C 2017-08-12   938   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- function ranraw ------------------------------------------------
C
C Fortran IV(1966): function syntax is: 'type FUNCTION name*precision (args)
C however gfortran -std=legacy wants:   'type*precision FUNCTION name (args)
     REAL FUNCTION RANRAW*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RFAC,RNEW
     INTEGER IFAC
C
     RNEW = RSEED * 69069.D0
     RFAC = RNEW * RR32I
     IFAC = RFAC
     RFAC = IFAC
     RNEW = RNEW - RFAC * RR32
     IF (IDBGRR .NE. 0) WRITE(6,9000) RSEED,RNEW
     RSEED = RNEW
     RANRAW = RNEW
     RETURN
C
9000 FORMAT(1X,'RR: ',F12.0,1X,F12.0)
     END
C
C --- function rannum ------------------------------------------------
C
     REAL FUNCTION RANNUM*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RANRAW
     INTEGER I
C
     IF (RANINI) GOTO 1000
     DO 100 I=1,128
       RSHUF(I) = RANRAW(DUMMY)
100  CONTINUE
     RANINI = .TRUE.
1000 CONTINUE
C     
     I = RLAST * RDIVI
     RLAST = RSHUF(I+1)
     RSHUF(I+1) = RANRAW(DUMMY)
     RANNUM = RLAST * RR32I
     IF (IDBGRN .NE. 0) WRITE(6,9000) I,RLAST,RANNUM
     RETURN
C
9000 FORMAT(1X,'RN: ',I12,1X,F12.0,1X,F12.8)
     END
C
C --- main program ---------------------------------------------------
C     PROGRAM MCPI
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     INTEGER I
     INTEGER NTRY,NHIT,NGO
     REAL*8 PI,PIEST,PIERR
     REAL*8 X,Y,R
     REAL*8 DUMMY
     REAL*8 RANNUM
     REAL*8 RTRY,RHIT
     DATA PI /3.141592653589793D0/
     DATA NTRY /0/
     DATA NHIT /0/
C
     RR32   = 4294967296.D0
     RR32I  = 1./RR32
     RDIV   = 33554432.D0
     RDIVI  = 1./RDIV
C
     RSEED  = 12345.D0
     RLAST  = 0.D0
     RANINI = .FALSE.
C
     READ(5,9000,ERR=910,END=900) IDBGRR,IDBGRN,IDBGMC
C
     IF (IDBGRR.EQ.0 .AND. IDBGRN.EQ.0 .AND. IDBGMC.EQ.0)
    X  WRITE(6,9005)
C
100  READ(5,9010,ERR=910,END=900) NGO
     IF (NGO .LE. 0) GOTO 900
C
     DO 200 I=1,NGO
       X = 2.*RANNUM(DUMMY) - 1.
       Y = 2.*RANNUM(DUMMY) - 1.
       R = X*X + Y*Y
       NTRY = NTRY + 1
       IF (R .LE. 1.) NHIT = NHIT + 1
       IF (IDBGMC .NE. 0) WRITE(6,9030) X,Y,R,NHIT
200  CONTINUE
C
     RTRY = NTRY
     RHIT = NHIT
     PIEST = 4.* RHIT / RTRY
     PIERR = PIEST - PI
     IF (PIERR .LT. 0.) PIERR = -PIERR
     WRITE(6,9020) NTRY, NHIT,PIEST,PIERR,RLAST
     GOTO 100
C
900  CONTINUE
C
     STOP
C
910  WRITE(6,9040)
     STOP
C
9000 FORMAT(3I10)
9005 FORMAT(1X,'            ntry         nhit       pi-est',
    X          '       pi-err        seed')
9010 FORMAT(I10)
9020 FORMAT(1X,'PI: ',I12,1X,I12,1X,F12.8,1X,F12.8,1X,F12.0)
9030 FORMAT(1X,'MC: ',F12.8,1X,F12.8,1X,F12.8,1X,I12)
9040 FORMAT(1X,'conversion error, abort')
C
     END
/*
//GO.FT06F001 DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIFOWF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(20,0),PRTY=2
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           MCPI#FOW,T=(20,0),P=100,NOCHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: mcpi_for.f 978 2017-12-28 21:32:18Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-28   978   1.1    use inverse to avoid divide by constant
C 2017-08-12   938   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- function ranraw ------------------------------------------------
C
C Fortran IV(1966): function syntax is: 'type FUNCTION name*precision (args)
C however gfortran -std=legacy wants:   'type*precision FUNCTION name (args)
     REAL FUNCTION RANRAW*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RFAC,RNEW
     INTEGER IFAC
C
     RNEW = RSEED * 69069.D0
     RFAC = RNEW * RR32I
     IFAC = RFAC
     RFAC = IFAC
     RNEW = RNEW - RFAC * RR32
     IF (IDBGRR .NE. 0) WRITE(6,9000) RSEED,RNEW
     RSEED = RNEW
     RANRAW = RNEW
     RETURN
C
9000 FORMAT(1X,'RR: ',F12.0,1X,F12.0)
     END
C
C --- function rannum ------------------------------------------------
C
     REAL FUNCTION RANNUM*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RANRAW
     INTEGER I
C
     IF (RANINI) GOTO 1000
     DO 100 I=1,128
       RSHUF(I) = RANRAW(DUMMY)
100  CONTINUE
     RANINI = .TRUE.
1000 CONTINUE
C     
     I = RLAST * RDIVI
     RLAST = RSHUF(I+1)
     RSHUF(I+1) = RANRAW(DUMMY)
     RANNUM = RLAST * RR32I
     IF (IDBGRN .NE. 0) WRITE(6,9000) I,RLAST,RANNUM
     RETURN
C
9000 FORMAT(1X,'RN: ',I12,1X,F12.0,1X,F12.8)
     END
C
C --- main program ---------------------------------------------------
C     PROGRAM MCPI
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     INTEGER I
     INTEGER NTRY,NHIT,NGO
     REAL*8 PI,PIEST,PIERR
     REAL*8 X,Y,R
     REAL*8 DUMMY
     REAL*8 RANNUM
     REAL*8 RTRY,RHIT
     DATA PI /3.141592653589793D0/
     DATA NTRY /0/
     DATA NHIT /0/
C
     RR32   = 4294967296.D0
     RR32I  = 1./RR32
     RDIV   = 33554432.D0
     RDIVI  = 1./RDIV
C
     RSEED  = 12345.D0
     RLAST  = 0.D0
     RANINI = .FALSE.
C
     READ(5,9000,ERR=910,END=900) IDBGRR,IDBGRN,IDBGMC
C
     IF (IDBGRR.EQ.0 .AND. IDBGRN.EQ.0 .AND. IDBGMC.EQ.0)
    X  WRITE(6,9005)
C
100  READ(5,9010,ERR=910,END=900) NGO
     IF (NGO .LE. 0) GOTO 900
C
     DO 200 I=1,NGO
       X = 2.*RANNUM(DUMMY) - 1.
       Y = 2.*RANNUM(DUMMY) - 1.
       R = X*X + Y*Y
       NTRY = NTRY + 1
       IF (R .LE. 1.) NHIT = NHIT + 1
       IF (IDBGMC .NE. 0) WRITE(6,9030) X,Y,R,NHIT
200  CONTINUE
C
     RTRY = NTRY
     RHIT = NHIT
     PIEST = 4.* RHIT / RTRY
     PIERR = PIEST - PI
     IF (PIERR .LT. 0.) PIERR = -PIERR
     WRITE(6,9020) NTRY, NHIT,PIEST,PIERR,RLAST
     GOTO 100
C
900  CONTINUE
C
     STOP
C
910  WRITE(6,9040)
     STOP
C
9000 FORMAT(3I10)
9005 FORMAT(1X,'            ntry         nhit       pi-est',
    X          '       pi-err        seed')
9010 FORMAT(I10)
9020 FORMAT(1X,'PI: ',I12,1X,I12,1X,F12.8,1X,F12.8,1X,F12.0)
9030 FORMAT(1X,'MC: ',F12.8,1X,F12.8,1X,F12.8,1X,I12)
9040 FORMAT(1X,'conversion error, abort')
C
     END
$ENTRY
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
$STOP
/*
//
./        ADD   NAME=MCPIFOWT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#FOW JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG  EXEC WATFIV
//SYSIN DD *
$JOB           MCPI#FOW,T=(1,0),P=100,CHECK
C        1         2         3         4         5         6         712--------
C2345*78901234567890123456789012345678901234567890123456789012345678901234567890
C $Id: mcpi_for.f 978 2017-12-28 21:32:18Z mueller $
C
C Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
C
C This program is free software; you may redistribute and/or modify
C it under the terms of the GNU General Public License version 3.
C See Licence.txt in distribition directory for further details.
C
C  Revision History:
C Date         Rev Version  Comment
C 2017-12-28   978   1.1    use inverse to avoid divide by constant
C 2017-08-12   938   1.0    Initial version
C 2017-07-30   931   0.1    First draft
C
C --- function ranraw ------------------------------------------------
C
C Fortran IV(1966): function syntax is: 'type FUNCTION name*precision (args)
C however gfortran -std=legacy wants:   'type*precision FUNCTION name (args)
     REAL FUNCTION RANRAW*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RFAC,RNEW
     INTEGER IFAC
C
     RNEW = RSEED * 69069.D0
     RFAC = RNEW * RR32I
     IFAC = RFAC
     RFAC = IFAC
     RNEW = RNEW - RFAC * RR32
     IF (IDBGRR .NE. 0) WRITE(6,9000) RSEED,RNEW
     RSEED = RNEW
     RANRAW = RNEW
     RETURN
C
9000 FORMAT(1X,'RR: ',F12.0,1X,F12.0)
     END
C
C --- function rannum ------------------------------------------------
C
     REAL FUNCTION RANNUM*8 (DUMMY)
C
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     REAL*8 DUMMY
     REAL*8 RANRAW
     INTEGER I
C
     IF (RANINI) GOTO 1000
     DO 100 I=1,128
       RSHUF(I) = RANRAW(DUMMY)
100  CONTINUE
     RANINI = .TRUE.
1000 CONTINUE
C     
     I = RLAST * RDIVI
     RLAST = RSHUF(I+1)
     RSHUF(I+1) = RANRAW(DUMMY)
     RANNUM = RLAST * RR32I
     IF (IDBGRN .NE. 0) WRITE(6,9000) I,RLAST,RANNUM
     RETURN
C
9000 FORMAT(1X,'RN: ',I12,1X,F12.0,1X,F12.8)
     END
C
C --- main program ---------------------------------------------------
C     PROGRAM MCPI
     COMMON /DBG/IDBGRR,IDBGRN,IDBGMC
     COMMON /RANFAC/RR32,RR32I,RDIV,RDIVI
     COMMON /RAN/RLAST,RSEED,RSHUF(128),RANINI
     REAL*8 RR32,RR32I,RDIV,RDIVI
     REAL*8 RLAST,RSEED,RSHUF
     LOGICAL RANINI
C
     INTEGER I
     INTEGER NTRY,NHIT,NGO
     REAL*8 PI,PIEST,PIERR
     REAL*8 X,Y,R
     REAL*8 DUMMY
     REAL*8 RANNUM
     REAL*8 RTRY,RHIT
     DATA PI /3.141592653589793D0/
     DATA NTRY /0/
     DATA NHIT /0/
C
     RR32   = 4294967296.D0
     RR32I  = 1./RR32
     RDIV   = 33554432.D0
     RDIVI  = 1./RDIV
C
     RSEED  = 12345.D0
     RLAST  = 0.D0
     RANINI = .FALSE.
C
     READ(5,9000,ERR=910,END=900) IDBGRR,IDBGRN,IDBGMC
C
     IF (IDBGRR.EQ.0 .AND. IDBGRN.EQ.0 .AND. IDBGMC.EQ.0)
    X  WRITE(6,9005)
C
100  READ(5,9010,ERR=910,END=900) NGO
     IF (NGO .LE. 0) GOTO 900
C
     DO 200 I=1,NGO
       X = 2.*RANNUM(DUMMY) - 1.
       Y = 2.*RANNUM(DUMMY) - 1.
       R = X*X + Y*Y
       NTRY = NTRY + 1
       IF (R .LE. 1.) NHIT = NHIT + 1
       IF (IDBGMC .NE. 0) WRITE(6,9030) X,Y,R,NHIT
200  CONTINUE
C
     RTRY = NTRY
     RHIT = NHIT
     PIEST = 4.* RHIT / RTRY
     PIERR = PIEST - PI
     IF (PIERR .LT. 0.) PIERR = -PIERR
     WRITE(6,9020) NTRY, NHIT,PIEST,PIERR,RLAST
     GOTO 100
C
900  CONTINUE
C
     STOP
C
910  WRITE(6,9040)
     STOP
C
9000 FORMAT(3I10)
9005 FORMAT(1X,'            ntry         nhit       pi-est',
    X          '       pi-err        seed')
9010 FORMAT(I10)
9020 FORMAT(1X,'PI: ',I12,1X,I12,1X,F12.8,1X,F12.8,1X,F12.0)
9030 FORMAT(1X,'MC: ',F12.8,1X,F12.8,1X,F12.8,1X,I12)
9040 FORMAT(1X,'conversion error, abort')
C
     END
$ENTRY
        1         1         1
       10
        0
$STOP
/*
//
./        ADD   NAME=MCPIPASF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(10,0),PRTY=2
//CLG EXEC PASCLG,GOTIME=3600,GOREG=1024K,
//      OPT='M+,D-',
//      GOPARM='/STACK=512k'
//COMPILE.SYSIN DD *
(* $Id: mcpi_pas.pas 978 2017-12-28 21:32:18Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-28   978   1.1    use inverse to avoid divide by constant *)
(* 2017-09-17   951   1.0    Initial version                         *)
(* 2017-09-07   948   0.1    First draft                             *)

program mcpi(input,output);
const
  rr32 = 4294967296.0;
  rdiv = 33554432.0;
  pi   = 3.141592653589793;
var
  rseed,rlast          : real;
  ranini               : boolean;
  idbgrr,idbgrn,idbgmc : integer;
  i,ntry,nhit,ngo      : integer;
  piest,pierr          : real;
  rhit,rtry            : real;
  x,y,r                : real;
  rr32i,rdivi          : real;
  rshuf                : ARRAY[0 .. 127] of real;

function ranraw(dummy :real) : real; 
var
  rfac,rnew : real;
begin
  rnew := rseed * 69069.0;
  rfac := rnew * rr32i;
  rfac := trunc(rfac);
  rnew := rnew - rfac * rr32;
  if idbgrr > 0 then writeln(' ','RR: ',rseed:14:1,rnew:14:1);
  rseed := rnew;
  ranraw := rnew;
end;

function rannum(dummy :real) : real; 
var
  rnew : real;
  i    : integer;
begin
  if not ranini then begin
     for i := 0 to 127 do rshuf[i] := ranraw(0.0);
     ranini := TRUE;
  end;

  i := trunc(rlast*rdivi);
  rlast := rshuf[i];
  rshuf[i] := ranraw(0.0);
  rnew := rlast * rr32i;
  if idbgrn > 0 then writeln(' ','RN: ',i:12,rlast:14:1,rnew:14:8);
  rannum := rnew;
end;

begin
  rseed  := 12345.0;
  ranini := FALSE;

  rr32i  := 1.0/rr32;
  rdivi  := 1.0/rdiv;

  read(idbgrr);
  read(idbgrn);
  read(idbgmc);

  if (idbgrr=0) and (idbgrn=0) and (idbgmc=0) then
     writeln(' ','            ntry        nhit      pi-est',
             '      pi-err        seed');

  while TRUE do begin
     read(ngo);
     if ngo = 0 then exit(0);
     for i := 1 to ngo do begin
        x := 2.0 * rannum(0.0) - 1.0;
        y := 2.0 * rannum(0.0) - 1.0;
        r := x*x + y*y;
        ntry := ntry + 1;
        if r <= 1.0 then nhit := nhit + 1;
        if idbgmc > 0 then writeln(' ','MC: ',
                                   x:12:8,y:12:8,r:12:8,nhit:12);
     end;

     rtry := ntry;
     rhit := nhit;

     piest := 4.0 * (rhit / rtry);
     pierr := piest - pi;
     writeln(' ','PI: ',ntry:12,nhit:12,piest:12:8,pierr:12:8,
                        rlast:14:1);
  end;

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIPAST,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#PAS JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1024K,TIME=(1,0),PRTY=8
//CLG EXEC PASCLG,GOTIME=3600,GOREG=1024K,
//      OPT='M+',
//      GOPARM='/STACK=512k'
//COMPILE.SYSIN DD *
(* $Id: mcpi_pas.pas 978 2017-12-28 21:32:18Z mueller $ *)
(*
(* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> *)
(*
(* This program is free software; you may redistribute and/or modify *)
(* it under the terms of the GNU General Public License version 3.   *)
(* See Licence.txt in distribition directory for further details.    *)
(*                                                                   *)
(*  Revision History:                                                *)
(* Date         Rev Version  Comment                                 *)
(* 2017-12-28   978   1.1    use inverse to avoid divide by constant *)
(* 2017-09-17   951   1.0    Initial version                         *)
(* 2017-09-07   948   0.1    First draft                             *)

program mcpi(input,output);
const
  rr32 = 4294967296.0;
  rdiv = 33554432.0;
  pi   = 3.141592653589793;
var
  rseed,rlast          : real;
  ranini               : boolean;
  idbgrr,idbgrn,idbgmc : integer;
  i,ntry,nhit,ngo      : integer;
  piest,pierr          : real;
  rhit,rtry            : real;
  x,y,r                : real;
  rr32i,rdivi          : real;
  rshuf                : ARRAY[0 .. 127] of real;

function ranraw(dummy :real) : real; 
var
  rfac,rnew : real;
begin
  rnew := rseed * 69069.0;
  rfac := rnew * rr32i;
  rfac := trunc(rfac);
  rnew := rnew - rfac * rr32;
  if idbgrr > 0 then writeln(' ','RR: ',rseed:14:1,rnew:14:1);
  rseed := rnew;
  ranraw := rnew;
end;

function rannum(dummy :real) : real; 
var
  rnew : real;
  i    : integer;
begin
  if not ranini then begin
     for i := 0 to 127 do rshuf[i] := ranraw(0.0);
     ranini := TRUE;
  end;

  i := trunc(rlast*rdivi);
  rlast := rshuf[i];
  rshuf[i] := ranraw(0.0);
  rnew := rlast * rr32i;
  if idbgrn > 0 then writeln(' ','RN: ',i:12,rlast:14:1,rnew:14:8);
  rannum := rnew;
end;

begin
  rseed  := 12345.0;
  ranini := FALSE;

  rr32i  := 1.0/rr32;
  rdivi  := 1.0/rdiv;

  read(idbgrr);
  read(idbgrn);
  read(idbgmc);

  if (idbgrr=0) and (idbgrn=0) and (idbgmc=0) then
     writeln(' ','            ntry        nhit      pi-est',
             '      pi-err        seed');

  while TRUE do begin
     read(ngo);
     if ngo = 0 then exit(0);
     for i := 1 to ngo do begin
        x := 2.0 * rannum(0.0) - 1.0;
        y := 2.0 * rannum(0.0) - 1.0;
        r := x*x + y*y;
        ntry := ntry + 1;
        if r <= 1.0 then nhit := nhit + 1;
        if idbgmc > 0 then writeln(' ','MC: ',
                                   x:12:8,y:12:8,r:12:8,nhit:12);
     end;

     rtry := ntry;
     rhit := nhit;

     piest := 4.0 * (rhit / rtry);
     pierr := piest - pi;
     writeln(' ','PI: ',ntry:12,nhit:12,piest:12:8,pierr:12:8,
                        rlast:14:1);
  end;

end.
/*
//GO.OUTPUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPIPLIF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(10,0),PRTY=2
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: mcpi_pli.pli 978 2017-12-28 21:32:18Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-28   978   1.1    use inverse to avoid divide by constant */
/* 2017-09-07   947   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

 MCPI: PROC OPTIONS(MAIN) REORDER;
   DCL RSEED        DEC FLOAT(16) INIT(12345.);
   DCL RLAST        DEC FLOAT(16) INIT(0.);
   DCL RSHUF(0:127) DEC FLOAT(16) INIT((128)0.);
   DCL RR32         DEC FLOAT(16) INIT(4294967296.);
   DCL RDIV         DEC FLOAT(16) INIT(33554432.);
   DCL RANINI       BIN FIXED(31) INIT(0);
   DCL (IDBGRR,IDBGRN,IDBGMC) BIN FIXED(31) INIT(0);

   DCL RANRAW ENTRY RETURNS(DEC FLOAT(16));
   DCL RANNUM ENTRY RETURNS(DEC FLOAT(16));

   DCL (I,NTRY,NHIT,NGO)      BIN FIXED(31) INIT(0);
   DCL (PIEST,PIERR)          DEC FLOAT(16);
   DCL (RHIT,RTRY)            DEC FLOAT(16);
   DCL (RR32I,RDIVI)          DEC FLOAT(16);
   DCL (X,Y,R)                DEC FLOAT(16);
   DCL PI    DEC FLOAT(16) INIT(3.141592653589793E0);

   ON ENDFILE(SYSIN) GOTO DONE;
   ON CONVERSION     GOTO ABORT;

   RR32I = 1./RR32;
   RDIVI = 1./RDIV;

   GET EDIT(IDBGRR,IDBGRN,IDBGMC) (3(F(10)));

   IF IDBGRR=0 & IDBGRN=0 & IDBGMC=0 THEN
      PUT SKIP EDIT('            ntry         nhit       pi-est',
               '       pi-err         seed')(A,A);

   DO WHILE('1'B);
     GET SKIP EDIT(NGO) (F(10));
     IF NGO = 0 THEN GOTO DONE;

     DO I=1 TO NGO;
        X = 2.*RANNUM - 1.;
        Y = 2.*RANNUM - 1.;
        R = X*X + Y*Y;
        NTRY = NTRY + 1;
        IF R <= 1. THEN NHIT = NHIT + 1;
        IF IDBGMC > 0 THEN PUT SKIP EDIT('MC: ',X,Y,R,NHIT)
                                    (A,3(F(12,8),X(1)),F(12));
     END;

     RTRY = NTRY;
     RHIT = NHIT;

     PIEST = 4.E0 * (RHIT / RTRY);
     PIERR = PIEST - PI;
     IF PIERR < 0. THEN PIERR = -PIERR;
     PUT SKIP EDIT('PI: ',NTRY,NHIT,PIEST,PIERR,RLAST)
              (A,2(F(12),X(1)),2(F(12,8),X(1)),F(12));
   END;
   GOTO DONE;

   ABORT: PUT SKIP EDIT('Conversion error, abort')(A);
   DONE:;

   /* procedure RANRAW --------------------------------------------*/
   RANRAW: PROC RETURNS(DEC FLOAT(16));

     DCL (RFAC,RNEW) DEC FLOAT(16);
     DCL IFAC  BIN FIXED(31);

     RNEW = RSEED * 69069.;
     RFAC = RNEW * RR32I;
     IFAC = RFAC;
     RFAC = IFAC;
     RNEW = RNEW - RFAC * RR32;
     IF IDBGRR > 0 THEN PUT SKIP EDIT('RR: ',RSEED,RNEW)
                                 (A,F(12,0),X(1),F(12,0));
     RSEED = RNEW;
     RETURN(RNEW);
   END RANRAW;

   /* procedure RANNUM --------------------------------------------*/
   RANNUM: PROC RETURNS(DEC FLOAT(16));
     DCL I     BIN FIXED(31);
     DCL RNEW  DEC FLOAT(16);

     IF RANINI = 0 THEN DO;
       DO I=0 TO 127;
         RSHUF(I) = RANRAW;
       END;
       RANINI = 1;
     END;

     I = RLAST * RDIVI;
     RLAST = RSHUF(I);
     RSHUF(I) = RANRAW;
     RNEW = RLAST * RR32I;
     IF IDBGRN > 0 THEN PUT SKIP EDIT('RN: ',I,RLAST,RNEW)
                            (A,F(12),X(1),F(12,0),X(1),F(12,8));
     RETURN(RNEW);
   END RANNUM;
 END MCPI;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPIPLIT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#PLI JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=256K,TIME=(1,0),PRTY=8
//CLG EXEC PL1LFCLG,
//      PARM.PL1L='LOAD,NODECK,OPT=2',
//      PARM.LKED='MAP,LIST'
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN DD *
/*      1         2         3         4         5         6         7*/--------
/*4567890123456789012345678901234567890123456789012345678901234567890*/--------
/* $Id: mcpi_pli.pli 978 2017-12-28 21:32:18Z mueller $ */
/*
/* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de> */
/*
/* This program is free software; you may redistribute and/or modify */
/* it under the terms of the GNU General Public License version 3.   */
/* See Licence.txt in distribition directory for further details.    */
/*                                                                   */
/*  Revision History:                                                */
/* Date         Rev Version  Comment                                 */
/* 2017-12-28   978   1.1    use inverse to avoid divide by constant */
/* 2017-09-07   947   1.0    Initial version                         */
/* 2017-07-30   931   0.1    First draft                             */

 MCPI: PROC OPTIONS(MAIN) REORDER;
   DCL RSEED        DEC FLOAT(16) INIT(12345.);
   DCL RLAST        DEC FLOAT(16) INIT(0.);
   DCL RSHUF(0:127) DEC FLOAT(16) INIT((128)0.);
   DCL RR32         DEC FLOAT(16) INIT(4294967296.);
   DCL RDIV         DEC FLOAT(16) INIT(33554432.);
   DCL RANINI       BIN FIXED(31) INIT(0);
   DCL (IDBGRR,IDBGRN,IDBGMC) BIN FIXED(31) INIT(0);

   DCL RANRAW ENTRY RETURNS(DEC FLOAT(16));
   DCL RANNUM ENTRY RETURNS(DEC FLOAT(16));

   DCL (I,NTRY,NHIT,NGO)      BIN FIXED(31) INIT(0);
   DCL (PIEST,PIERR)          DEC FLOAT(16);
   DCL (RHIT,RTRY)            DEC FLOAT(16);
   DCL (RR32I,RDIVI)          DEC FLOAT(16);
   DCL (X,Y,R)                DEC FLOAT(16);
   DCL PI    DEC FLOAT(16) INIT(3.141592653589793E0);

   ON ENDFILE(SYSIN) GOTO DONE;
   ON CONVERSION     GOTO ABORT;

   RR32I = 1./RR32;
   RDIVI = 1./RDIV;

   GET EDIT(IDBGRR,IDBGRN,IDBGMC) (3(F(10)));

   IF IDBGRR=0 & IDBGRN=0 & IDBGMC=0 THEN
      PUT SKIP EDIT('            ntry         nhit       pi-est',
               '       pi-err         seed')(A,A);

   DO WHILE('1'B);
     GET SKIP EDIT(NGO) (F(10));
     IF NGO = 0 THEN GOTO DONE;

     DO I=1 TO NGO;
        X = 2.*RANNUM - 1.;
        Y = 2.*RANNUM - 1.;
        R = X*X + Y*Y;
        NTRY = NTRY + 1;
        IF R <= 1. THEN NHIT = NHIT + 1;
        IF IDBGMC > 0 THEN PUT SKIP EDIT('MC: ',X,Y,R,NHIT)
                                    (A,3(F(12,8),X(1)),F(12));
     END;

     RTRY = NTRY;
     RHIT = NHIT;

     PIEST = 4.E0 * (RHIT / RTRY);
     PIERR = PIEST - PI;
     IF PIERR < 0. THEN PIERR = -PIERR;
     PUT SKIP EDIT('PI: ',NTRY,NHIT,PIEST,PIERR,RLAST)
              (A,2(F(12),X(1)),2(F(12,8),X(1)),F(12));
   END;
   GOTO DONE;

   ABORT: PUT SKIP EDIT('Conversion error, abort')(A);
   DONE:;

   /* procedure RANRAW --------------------------------------------*/
   RANRAW: PROC RETURNS(DEC FLOAT(16));

     DCL (RFAC,RNEW) DEC FLOAT(16);
     DCL IFAC  BIN FIXED(31);

     RNEW = RSEED * 69069.;
     RFAC = RNEW * RR32I;
     IFAC = RFAC;
     RFAC = IFAC;
     RNEW = RNEW - RFAC * RR32;
     IF IDBGRR > 0 THEN PUT SKIP EDIT('RR: ',RSEED,RNEW)
                                 (A,F(12,0),X(1),F(12,0));
     RSEED = RNEW;
     RETURN(RNEW);
   END RANRAW;

   /* procedure RANNUM --------------------------------------------*/
   RANNUM: PROC RETURNS(DEC FLOAT(16));
     DCL I     BIN FIXED(31);
     DCL RNEW  DEC FLOAT(16);

     IF RANINI = 0 THEN DO;
       DO I=0 TO 127;
         RSHUF(I) = RANRAW;
       END;
       RANINI = 1;
     END;

     I = RLAST * RDIVI;
     RLAST = RSHUF(I);
     RSHUF(I) = RANRAW;
     RNEW = RLAST * RR32I;
     IF IDBGRN > 0 THEN PUT SKIP EDIT('RN: ',I,RLAST,RNEW)
                            (A,F(12),X(1),F(12,0),X(1),F(12,8));
     RETURN(RNEW);
   END RANNUM;
 END MCPI;
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ADD   NAME=MCPISIMF,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1000K,TIME=(10,0),PRTY=2
//CLG EXEC SIMCLG,
//      PARM.SIM=NOSUBCHK,
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-28   978   1.1    use inverse to avoid divide by constant
* 2017-09-17   951   1.0    Initial version
* 2017-09-08   949   0.1    First draft
*;

BEGIN
  LONG REAL rr32,rr32i;
  LONG REAL rdiv,rdivi;
  LONG REAL pi;
  LONG REAL rseed,rlast;
  BOOLEAN ranini;
  INTEGER idbgrr,idbgrn,idbgmc;
  INTEGER i,ntry,nhit,ngo;
  LONG REAL piest,pierr;
  LONG REAL rhit,rtry;
  LONG REAL x,y,r;
  LONG REAL ARRAY rshuf(0:127);

  LONG REAL PROCEDURE ranraw;
  BEGIN
     LONG REAL rfac,rnew;
     rnew := rseed * 69069.0;
     rfac := rnew * rr32i;
     rfac := Entier(rfac);
     rnew := rnew - rfac * rr32;
     IF idbgrr > 0 THEN BEGIN
        OutText("RR: ");
        OutFix(rseed,1,14);
        OutFix(rnew,1,14);
        OutImage;
     END;
     rseed  := rnew;
     ranraw := rnew;
  END ** ranraw **;

  LONG REAL PROCEDURE rannum;
  BEGIN
     LONG REAL rnew;
     INTEGER i;
     IF NOT ranini THEN BEGIN
        FOR i := 0 STEP 1 UNTIL 127 DO rshuf(i) := ranraw;
        ranini := TRUE;
     END;
     i := Entier(rlast*rdivi);
     rlast := rshuf(i);
     rshuf(i) := ranraw;
     rnew := rlast * rr32i;
     IF idbgrn > 0 THEN BEGIN
        OutText("RN: ");
        OutInt(I,12);
        OutFix(rlast,1,14);
        OutFix(rnew,8,14);
        OutImage;
     END;
     rannum := rnew;
  END ** rannum **;

  rr32   := 4294967296.0;
  rr32i  := 1.0/rr32;
  rdiv   := 33554432.0;
  rdivi  := 1.0/rdiv;
  pi     := 3.141592653589793;
  rseed  := 12345.0;
  ranini := FALSE;

  idbgrr := InInt;
  idbgrn := InInt;
  idbgmc := InInt;

  IF idbgrr=0 AND idbgrn=0 AND idbgmc=0 THEN BEGIN
     OutText("            ntry        nhit      pi-est");
     OutText("      pi-err        seed");
     OutImage;
  END;

  WHILE TRUE DO BEGIN
     ngo :=  InInt;
     IF ngo = 0 THEN GOTO done;
     FOR i := 1 STEP 1 UNTIL ngo DO BEGIN
        x := 2.0 * rannum - 1.0;
        y := 2.0 * rannum - 1.0;
        r := x*x + y*y;
        ntry := ntry + 1;
        IF r <= 1.0 THEN nhit := nhit + 1;
        IF idbgrr > 0 THEN BEGIN
           OutText("MC: ");
           OutFix(x,8,12);
           OutFix(y,8,12);
           OutFix(r,8,12);
           OutInt(nhit,12);
           OutImage;
        END;
     END;
     rtry := ntry;
     rhit := nhit;

     piest := 4.0 * (rhit / rtry);
     pierr := piest - pi;

     OutText("PI: ");
     OutInt(ntry,12);
     OutInt(nhit,12);
     OutFix(piest,8,12);
     OutFix(pierr,8,12);
     OutFix(rlast,1,14);
     OutImage;
  END;

  done:
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        0         0         0
      100
      200
      700
     2000
     7000
    20000
    70000
   200000
   700000
  2000000
        0
/*
//
./        ADD   NAME=MCPISIMT,LEVEL=00,SOURCE=0,LIST=ALL
//MCPI#SIM JOB 'S322-0C4','WFJM',
//      CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//      REGION=1000K,TIME=(1,0),PRTY=8
//CLG EXEC SIMCLG,
//      PARM.SIM='',
//      PARM.LKED='MAP,LIST,LET',
//      PARM.GO='LINECNT=64'
//SIM.SYSIN DD *
COMMENT
* 
* Copyright 2017- by Walter F.J. Mueller <W.F.J.Mueller@gsi.de>
* 
* This program is free software, you may redistribute and/or modify
* it under the terms of the GNU General Public License version 3.
* See Licence.txt in distribition directory for further details.
* 
*  Revision History:
* Date         Rev Version  Comment
* 2017-12-28   978   1.1    use inverse to avoid divide by constant
* 2017-09-17   951   1.0    Initial version
* 2017-09-08   949   0.1    First draft
*;

BEGIN
  LONG REAL rr32,rr32i;
  LONG REAL rdiv,rdivi;
  LONG REAL pi;
  LONG REAL rseed,rlast;
  BOOLEAN ranini;
  INTEGER idbgrr,idbgrn,idbgmc;
  INTEGER i,ntry,nhit,ngo;
  LONG REAL piest,pierr;
  LONG REAL rhit,rtry;
  LONG REAL x,y,r;
  LONG REAL ARRAY rshuf(0:127);

  LONG REAL PROCEDURE ranraw;
  BEGIN
     LONG REAL rfac,rnew;
     rnew := rseed * 69069.0;
     rfac := rnew * rr32i;
     rfac := Entier(rfac);
     rnew := rnew - rfac * rr32;
     IF idbgrr > 0 THEN BEGIN
        OutText("RR: ");
        OutFix(rseed,1,14);
        OutFix(rnew,1,14);
        OutImage;
     END;
     rseed  := rnew;
     ranraw := rnew;
  END ** ranraw **;

  LONG REAL PROCEDURE rannum;
  BEGIN
     LONG REAL rnew;
     INTEGER i;
     IF NOT ranini THEN BEGIN
        FOR i := 0 STEP 1 UNTIL 127 DO rshuf(i) := ranraw;
        ranini := TRUE;
     END;
     i := Entier(rlast*rdivi);
     rlast := rshuf(i);
     rshuf(i) := ranraw;
     rnew := rlast * rr32i;
     IF idbgrn > 0 THEN BEGIN
        OutText("RN: ");
        OutInt(I,12);
        OutFix(rlast,1,14);
        OutFix(rnew,8,14);
        OutImage;
     END;
     rannum := rnew;
  END ** rannum **;

  rr32   := 4294967296.0;
  rr32i  := 1.0/rr32;
  rdiv   := 33554432.0;
  rdivi  := 1.0/rdiv;
  pi     := 3.141592653589793;
  rseed  := 12345.0;
  ranini := FALSE;

  idbgrr := InInt;
  idbgrn := InInt;
  idbgmc := InInt;

  IF idbgrr=0 AND idbgrn=0 AND idbgmc=0 THEN BEGIN
     OutText("            ntry        nhit      pi-est");
     OutText("      pi-err        seed");
     OutImage;
  END;

  WHILE TRUE DO BEGIN
     ngo :=  InInt;
     IF ngo = 0 THEN GOTO done;
     FOR i := 1 STEP 1 UNTIL ngo DO BEGIN
        x := 2.0 * rannum - 1.0;
        y := 2.0 * rannum - 1.0;
        r := x*x + y*y;
        ntry := ntry + 1;
        IF r <= 1.0 THEN nhit := nhit + 1;
        IF idbgrr > 0 THEN BEGIN
           OutText("MC: ");
           OutFix(x,8,12);
           OutFix(y,8,12);
           OutFix(r,8,12);
           OutInt(nhit,12);
           OutImage;
        END;
     END;
     rtry := ntry;
     rhit := nhit;

     piest := 4.0 * (rhit / rtry);
     pierr := piest - pi;

     OutText("PI: ");
     OutInt(ntry,12);
     OutInt(nhit,12);
     OutFix(piest,8,12);
     OutFix(pierr,8,12);
     OutFix(rlast,1,14);
     OutImage;
  END;

  done:
END;
/*
//GO.SYSOUT DD SYSOUT=*,OUTLIM=5000
//GO.SYSIN DD *
        1         1         1
       10
        0
/*
//
./        ENDUP
@@
/*
//
