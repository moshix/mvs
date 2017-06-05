              IDENTIFICATION DIVISION.
       PROGRAM-ID. CALL370.
      ****************************************************************
      *   CALL370  is a test program for the subprogram "TEST370".   *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D-DATE PIC 9(6)  VALUE 082987.
       01  X-DATE PIC X(6)  VALUE '082987'.
       01  P-DATE PIC 9(15) USAGE COMP-3 VALUE 082987.
       01  C-DATE PIC 9(6)  USAGE COMP VALUE 082987.
       PROCEDURE DIVISION.
       SET-UP.
               DISPLAY "CALL370.CBL STARTING TEST OF TEST370.BIN".
       CALL-370.
               CALL "TEST370" USING D-DATE, X-DATE, P-DATE, C-DATE.
               DISPLAY "BACK FROM FIRST CALL RC=" RETURN-CODE.
               CALL "TEST370" USING D-DATE, X-DATE, P-DATE, C-DATE.
               DISPLAY "BACK FROM SECOND CALL RC=" RETURN-CODE.
       END-JOB.
               DISPLAY "THAT'S ALL FOR NOW".
               EXIT PROGRAM.
               STOP RUN.
