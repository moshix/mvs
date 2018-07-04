//HERC01C JOB (COBOL),
//             'Eratosthenes Sieve',
//             CLASS=A,
//             MSGCLASS=A,
//             REGION=8M,TIME=1440,
//             MSGLEVEL=(1,1),
//  USER=HERC01,PASSWORD=CUL8TR
//PRIMES   EXEC COBUCG,
//         PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'
//COB.SYSPUNCH DD DUMMY
//COB.SYSIN    DD *
   10 * //////////////////////////////////////////////////////////         PRIME
   20 * // Name: Peter M. Maurer                                           PRIME
   30 * // Program: Sieve of Eratosthenes                                  PRIME
   40 * // Due: Never                                                      PRIME
   50 * // Language: COBOL                                                 PRIME
   60 * //                                                                 PRIME
   70 * // Changes:                                                        PRIME
   80 * // - Juergen Winkelmann, 2014/10/25, o adaption to IBM OS COBOL    PRIME
   90 * //                                   o read limit from SYSIN       PRIME
  100 * //                                   o n**2 (sqrt) shortcut        PRIME
  110 * //                                   o skip even numbers           PRIME
  120 * //                                   o compact output format       PRIME
  130 * //                                   o 32767 prime flags           PRIME
  140 * //////////////////////////////////////////////////////////         PRIME
  150 ***                                                                  PRIME
  160 ***                                                                  PRIME
  170 ***                                                                  PRIME
  180  IDENTIFICATION DIVISION.                                            PRIME
  190  PROGRAM-ID.  'PRIMES'.                                              PRIME
  200 ***                                                                  PRIME
  210 ***                                                                  PRIME
  220 ***                                                                  PRIME
  230  ENVIRONMENT DIVISION.                                               PRIME
  240 **                                                                   PRIME
  250 **                                                                   PRIME
  260  CONFIGURATION SECTION.                                              PRIME
  270  SOURCE-COMPUTER.  IBM-360.                                          PRIME
  280  OBJECT-COMPUTER.  IBM-360.                                          PRIME
  290 **                                                                   PRIME
  300 **                                                                   PRIME
  310  INPUT-OUTPUT SECTION.                                               PRIME
  320  FILE-CONTROL.                                                       PRIME
  330      SELECT PRIMES-SYSIN                                             PRIME
  340         ASSIGN TO UT-S-SYSIN.                                        PRIME
  350 ***                                                                  PRIME
  360 ***                                                                  PRIME
  370 ***                                                                  PRIME
  380  DATA DIVISION.                                                      PRIME
  390 **                                                                   PRIME
  400 **                                                                   PRIME
  410  FILE SECTION.                                                       PRIME
  420  FD  PRIMES-SYSIN                                                    PRIME
  430      RECORDING MODE IS F                                             PRIME
  440      RECORD CONTAINS 80 CHARACTERS                                   PRIME
  450      BLOCK  CONTAINS  1 RECORDS                                      PRIME
  460      LABEL RECORDS ARE OMITTED                                       PRIME
  470      DATA RECORD IS PRIMES-SYSIN-RECORD.                             PRIME
  480  01  PRIMES-SYSIN-RECORD.                                            PRIME
  490   02 PRIMES-SYSIN-NUMBER PIC 99999999 OCCURS 10.                     PRIME
  500 **                                                                   PRIME
  510 **                                                                   PRIME
  520  WORKING-STORAGE SECTION.                                            PRIME
  530      77 I PIC 99999999 COMP VALUE 1.                                 PRIME
  540      77 J PIC 99999999 COMP.                                         PRIME
  550      77 K PIC 99999999 COMP VALUE 1.                                 PRIME
  560      77 N PIC 99999999 COMP.                                         PRIME
  570      77 N-2 PIC 99999999 COMP.                                       PRIME
  580      77 SQRTN PIC 99999999 COMP.                                     PRIME
  590      77 PRODUCT PIC 99999999 COMP.                                   PRIME
  600      01 BLANK-LINE PIC X(160).                                       PRIME
  610      01 OUT-INTEGER.                                                 PRIME
  620       02 SHOWIT PIC ZZZZZZZZ OCCURS 20.                              PRIME
  630      01 OUT REDEFINES OUT-INTEGER.                                   PRIME
  640       02 OUT-LINE PIC X(160).                                        PRIME
  650      01 PRIME-FLAGS.                                                 PRIME
  660       02 ISPRIME PIC 9 OCCURS 32767.                                 PRIME
  670 ***                                                                  PRIME
  680 ***                                                                  PRIME
  690 ***                                                                  PRIME
  700  PROCEDURE DIVISION.                                                 PRIME
  710 **                                                                   PRIME
  720 **                                                                   PRIME
  730  MAIN-PART.                                                          PRIME
  740      OPEN INPUT PRIMES-SYSIN.                                        PRIME
  750      READ PRIMES-SYSIN AT END DISPLAY '** EOF on SYSIN **'.          PRIME
  760      MOVE PRIMES-SYSIN-NUMBER (1) TO N.                              PRIME
  770      CLOSE PRIMES-SYSIN.                                             PRIME
  780      SUBTRACT 2 FROM N GIVING N-2.                                   PRIME
  790 *                                                                    PRIME
  800      PERFORM NEXT-SQUARE UNTIL SQRTN GREATER N.                      PRIME
  810      MOVE I TO SQRTN.                                                PRIME
  820 *                                                                    PRIME
  830      MOVE 3 TO I.                                                    PRIME
  840      PERFORM INIT-1 UNTIL I GREATER N.                               PRIME
  850 *                                                                    PRIME
  860      MOVE 3 TO I.                                                    PRIME
  870      PERFORM CHECK-NUMBER UNTIL I GREATER SQRTN OR EQUAL SQRTN.      PRIME
  880 *                                                                    PRIME
  890      MOVE 3 TO I.                                                    PRIME
  900      MOVE 2 TO J.                                                    PRIME
  910      MOVE J TO SHOWIT (K).                                           PRIME
  920      PERFORM PRINT UNTIL I GREATER N.                                PRIME
  930 *                                                                    PRIME
  940      MOVE K TO SHOWIT (1).                                           PRIME
  950      MOVE N TO SHOWIT (2).                                           PRIME
  960      DISPLAY ' '.                                                    PRIME
  970      DISPLAY SHOWIT (1), ' primes up to ', SHOWIT (2), ' found.'.    PRIME
  980      STOP RUN.                                                       PRIME
  990 **                                                                   PRIME
 1000 **                                                                   PRIME
 1010  INIT-1.                                                             PRIME
 1020      MOVE 1 TO ISPRIME (I).                                          PRIME
 1030      ADD 2 TO I.                                                     PRIME
 1040 **                                                                   PRIME
 1050 **                                                                   PRIME
 1060  CHECK-NUMBER.                                                       PRIME
 1070      PERFORM ADVANCE UNTIL I GREATER THAN SQRTN OR EQUAL TO SQRT     PRIME
 1080 -     N OR ISPRIME (I) EQUAL TO 1.                                   PRIME
 1090      IF ISPRIME (I) EQUAL TO 1                                       PRIME
 1100          ADD I I GIVING J                                            PRIME
 1110          MULTIPLY I BY I GIVING PRODUCT                              PRIME
 1120          PERFORM CROSS-OUT UNTIL PRODUCT GREATER THAN N.             PRIME
 1130      ADD 2 TO I.                                                     PRIME
 1140 **                                                                   PRIME
 1150 **                                                                   PRIME
 1160  ADVANCE.                                                            PRIME
 1170      ADD 2 TO I.                                                     PRIME
 1180 **                                                                   PRIME
 1190 **                                                                   PRIME
 1200  CROSS-OUT.                                                          PRIME
 1210      MOVE 0 TO ISPRIME (PRODUCT).                                    PRIME
 1220      ADD J TO PRODUCT.                                               PRIME
 1230 **                                                                   PRIME
 1240 **                                                                   PRIME
 1250  NEXT-SQUARE.                                                        PRIME
 1260      ADD 1 TO I.                                                     PRIME
 1270      MULTIPLY I BY I GIVING SQRTN.                                   PRIME
 1280 **                                                                   PRIME
 1290 **                                                                   PRIME
 1300  PRINT.                                                              PRIME
 1310      IF ISPRIME (I) EQUAL TO 1                                       PRIME
 1320          MOVE I TO SHOWIT (J)                                        PRIME
 1330          ADD 1 TO K                                                  PRIME
 1340          ADD 1 TO J                                                  PRIME
 1350          IF J GREATER 20                                             PRIME
 1360              DISPLAY OUT-LINE                                        PRIME
 1370              MOVE BLANK-LINE TO OUT-LINE                             PRIME
 1380              MOVE 1 TO J.                                            PRIME
 1390      IF I GREATER N-2 AND J NOT EQUAL 1 DISPLAY OUT-LINE.            PRIME
 1400      ADD 2 TO I.                                                     PRIME
/*
//COB.SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR
//GO.SYSOUT   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)
//GO.SYSIN    DD *
    2000
/*
//
