REM This is a demo of PC/370 subroutines called from Micro Focus COBOL.
REM The COBOL programs are included in CBL source and INT object form.
REM The Micro Focus runtime program RUN.EXE is required to execute demo.
REM The Micro Focus extended memory shell XM.EXE is required to demo
REM execution of same programs in extended memory protect mode.
REM Note the following COBOL directive file was used to set IBM 370
PAUSE data format compatability.
COPY CBL\COBOL.DIR CON:
REM The following COBOL program and ALC subroutine tests 4 different
PAUSE data formats being passed using standard linkage conventions.
COPY CBL\CALL370.CBL CON:
PAUSE Next assemble the ALC program.
A370 CBL\TEST370/LX
COPY CBL\TEST370.PRN CON:
PAUSE Next link the ALC program.
L370 CBL\TEST370/LXB
COPY CBL\TEST370.LST CON:
PAUSE Now make PC/370 resident
E370R42
PAUSE Now execute the COBOL program in real mode and then extended mode
CD CBL
C:\C2WB\RUN CALL370
C:\C2WB\XM C:\C2WB\RUN CALL370
CD ..
PAUSE To remove the current resident emulator, execute it again as follows.
E370R42
erase cbl\test370.obj
erase cbl\test370.lst
erase cbl\test370.prn
REM The following COBOL program and ALC subroutine tests file I/O within
REM ALC subroutine using system queue area (SQA) memory allocated in the
PAUSE emulator region for dynamic file buffer.
COPY CBL\CALLCIO.CBL CON:
PAUSE Next assemble the ALC program.
A370 CBL\TESTCIO/LX
COPY CBL\TESTCIO.PRN CON:
PAUSE Next link the ALC program.
L370 CBL\TESTCIO/LXB
COPY CBL\TESTCIO.LST CON:
PAUSE Now make PC/370 resident and request 8k byte SQA (x'200' paragraphs)
E370R42.EXE 200
PAUSE Next execute the COBOL program in real and then extended mode
CD CBL
C:\C2WB\RUN CALLCIO
C:\C2WB\XM C:\C2WB\RUN CALLCIO
CD ..
PAUSE To remove the current resident emulator, execute it again as follows.
E370R42
erase cbl\testcio.obj
erase cbl\testcio.lst
erase cbl\testcio.prn
REM The following COBOL program and ALC subroutine tests SQA memory allocation.
COPY CBL\CALLSQA.CBL CON:
PAUSE Next assemble the ALC program.
A370 CBL\TESTSQA/LX
COPY CBL\TESTSQA.PRN CON:
PAUSE Next link the ALC program.
L370 CBL\TESTSQA/LXB
COPY CBL\TESTSQA.LST CON:
PAUSE Now make PC/370 resident and request 2k byte SQA (x'80' paragraphs)
E370R42 80
CD CBL
PAUSE Now execute the COBOL program once to allocate first half of SQA.
C:\C2WB\RUN CALLSQA
REM   Now run simple echo message COM program in separate address space
PAUSE to verify SQA memory is isolated and preserved across COM executions.
CD ..
A370 CBL\TESTCOM
L370 CBL\TESTCOM/G
ERASE CBL\TESTCOM.OBJ
CD CBL
PAUSE Now execute the COBOL program again to allocate last half of SQA.
C:\C2WB\XM C:\C2WB\RUN CALLSQA
PAUSE Now execute the COBOL program again to show SQA allocated and RC = 1.
C:\C2WB\RUN CALLSQA
CD ..
PAUSE To remove the current resident emulator, execute it again as follows.
E370R42
erase cbl\testsqa.obj
erase cbl\testsqa.lst
erase cbl\testsqa.prn
REM   That's the end of the demo.  Remember to make PC/370 resident before
REM   starting the Micro Focus Workbench or calls to PC/370 BIN files will
REM   result in error message and exit from call.
