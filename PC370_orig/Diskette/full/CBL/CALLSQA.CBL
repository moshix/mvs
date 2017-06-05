              IDENTIFICATION DIVISION.
       PROGRAM-ID. CALL370.
      ****************************************************************
      *   CALLSQA  is a test program for the subprogram "TESTSQA".   *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ADDR-MEMORY PIC 9(9)  USAGE COMP VALUE 0.
       01  LENGTH-MEMORY PIC 9(9)  USAGE COMP VALUE 0.
       01  REQUEST-MEMORY PIC 9(9) USAGE COMP VALUE 1024.
       PROCEDURE DIVISION.
       SET-UP.
               DISPLAY "CALL TESTSQA.BIN TO GET NEXT FREE SQA BLOCK".
       CALL-370.
               MOVE REQUEST-MEMORY TO LENGTH-MEMORY.
               CALL "TESTSQA" USING ADDR-MEMORY LENGTH-MEMORY.
               DISPLAY "RETURN CODE    =" RETURN-CODE
               DISPLAY "MEMORY ADDRESS =" ADDR-MEMORY.
               DISPLAY "MEMORY LENGTH  =" LENGTH-MEMORY.
       END-JOB.
               DISPLAY "EXITING TESTSQA NOW".
               EXIT PROGRAM.
               STOP RUN.
