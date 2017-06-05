       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALL370.
      ****************************************************************
      *   CALLCIO  is a test program for the subprogram "TESTCIO".   *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILE-NAME1 PIC X(64) VALUE 'CALLCIO.CBL '.
       01  FILE-NAME2 PIC X(64) VALUE 'TESTCIO.ALC '.
       PROCEDURE DIVISION.
       SET-UP.
               DISPLAY "CALLCIO.CBL STARTING TEST OF TESTCIO.BIN".
       CALL-CIO.
               CALL "TESTCIO" USING FILE-NAME1.
               DISPLAY "BACK FROM TESTCIO.BIN RC=" RETURN-CODE.
               CALL "TESTCIO" USING FILE-NAME2.
               DISPLAY "BACK FROM TESTCIO.BIN RC=" RETURN-CODE.
       END-JOB.
               DISPLAY "THAT'S ALL FOR NOW".
               EXIT PROGRAM.
               STOP RUN.
