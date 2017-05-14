         TITLE 'Sample Operating System     Version 2.00'               00010002
*********************************************************************** 00020000
*                                                                     * 00030000
*  *****************************************************************  * 00040000
*  *                                                               *  * 00050000
*  *                       Sample Operating System                 *  * 00060002
*  *                             Version 2.00                      *  * 00067002
*  *                        Developed at MIT 1973                  *  * 00074002
*  *                                                               *  * 00090000
*  *****************************************************************  * 00100000
*                                                                     * 00100602
*  Update 2015/10/31 Juergen Winkelmann, e-mail winkelmann@id.ethz.ch * 00101202
*                                                                     * 00101802
*   - change storage protection alignments to 4K  \                   * 00102402
*   - replace SSK/ISK instructions with SSKE/ISKE  > 4K support       * 00103002
*   - minor changes in storage protection logic   /                   * 00103602
*   - change number of parallel processing streams to 4               * 00104202
*   - change core size to 16M                                         * 00104802
*   - replace table of valid $JOB card core requests with general     * 00105402
*     logic rounding up any none full page request entered to next    * 00106002
*     full page                                                       * 00106602
*   - add IPL card and two card loader for one stop creation of an    * 00107202
*     IPLable card deck                                               * 00107802
*   - ignore external interrupts during initialization to avoid       * 00108402
*     IPLRTN getting interrupted by the interval timer                * 00109002
*                                                                     * 00109103
*  Update 2015/11/05 Juergen Winkelmann, e-mail winkelmann@id.ethz.ch * 00109203
*                                                                     * 00109303
*   - allow reloading card readers without needing to re-IPL the      * 00109403
*     system. This functionality relies on Hercules' card reader      * 00109503
*     behavior with the EOF initialization in place. It will not      * 00109603
*     work in INTR mode.                                              * 00109703
*                                                                     * 00109744
*  Update 2015/11/13 Juergen Winkelmann, e-mail winkelmann@id.ethz.ch * 00109784
*                                                                     * 00109824
*   - add UCB to support a console at 009 using the EXCP device       * 00109864
*     handler.                                                        * 00109904
*                                                                     * 00110000
*********************************************************************** 00120000
         SPACE 3                                                        00130000
         PRINT ON,NODATA,GEN                                            00140000
PROGRAM  CSECT ,                sample operating system starts at zero  00150002
CARDLDR  CSECT ,                two card loader follows at the end      00150102
***                                                                     00150202
*** IPL card                                                            00150302
***                                                                     00150402
IPLCARD  CSECT ,                IPLable deck must begin with this card  00150502
PSWD     DC    F'0',X'00'       initial program status word, disabled   00150602
         DC    AL3(LOADER)      start execution at load address         00150702
CCW1     DC    X'02',AL3(LOADER) read 1st card to load address          00150802
         DC    XL4'40000050'    chain, read length = 80                 00150902
CCW2     DC    X'02',AL3(LOADER+80) read 2nd card to load addr + 80     00151002
         DC    XL4'00000050'    read length = 80                        00151102
         DC    C'Sample Operating System     Version 2.00' eye catcher  00151202
         DC    16X'00'          pad to card length                      00151302
***                                                                     00151402
*** loader                                                              00151502
***                                                                     00151602
*                                                                       00151702
* Initialize                                                            00151802
*                                                                       00151902
CARDLDR  CSECT ,                two card loader must follow IPL card    00152002
         BALR  R12,0            establish ..                            00152102
         LA    R2,2                         .. base ..                  00152202
         SR    R12,R2                                 .. register       00152302
         USING CARDLDR,R12      tell assembler                          00152402
         LA    R11,0            addressability of ..                    00152502
         USING PROGRAM,R11        .. sample operating system            00152602
         LA    R2,0             I/O ..                                  00152702
         LA    R3,IOINTRPT            .. new PSWD                       00152802
         STM   R2,R3,IONEW      store I/O new PSWD                      00152902
         SSM   ENBLECH0         enable interrupts from channel 0        00153002
         LA    R5,CCWCHAIN      address of card reader CCW chain        00153102
         ST    R5,CAW           store address in CAW                    00153202
         L     R3,NUMCARDS      number of cards to read                 00153302
         L     R4,LOADADDR      target address of loaded code           00153402
*                                                                       00153502
* create CCW chain                                                      00153602
*                                                                       00153702
NEXTCARD LR    R2,R4            load next card here                     00153802
         ICM   R2,B'1000',READ  insert write command                    00153902
         ST    R2,0(,R5)        store CCW                               00154002
         LA    R2,80            length of card                          00154102
         ST    R2,4(,R5)        store length in CCW, zero all flags     00154202
         OI    4(R5),X'40'      indicate command chaining               00154302
         LA    R4,80(,R4)       increment target address                00154402
         LA    R5,8(,R5)        point to next CCW                       00154502
         BCT   R3,NEXTCARD      read next card                          00154602
         S     R5,EIGHT         point to previous CCW                   00154702
         NI    4(R5),X'BF'      clear command chaining flag             00154802
*                                                                       00154902
* read cards and wait for completion                                    00155002
*                                                                       00155102
         SIO   12(0)            read cards                              00155202
         LA    R2,*+12          continue here after I/O completion      00155302
         ST    R2,CONTINUE      store continue address in PSWD skeleton 00155402
         LPSW  WAITPSWD         wait for I/O completion                 00155502
*                                                                       00155602
* "IPL" the Sample Operating System                                     00155702
*                                                                       00155802
         LPSW  0                transfer control                        00155902
*                                                                       00156002
* I/O interrupt handler                                                 00156102
*                                                                       00156202
IOINTRPT EQU   *                                                        00156302
         TM    CSW+4,X'04'      device end received?                    00156402
         BNO   IOINTRTN         -> no,  keep waiting                    00156502
         NI    IOOLD+1,X'FD'    -> yes, terminate wait state and ..     00156602
         NI    IOOLD,X'7F'         .. and disable channel 0 interrupts  00156702
IOINTRTN LPSW  IOOLD            return to mainline                      00156802
         DROP  R11,R12          no longer needed                        00156902
*                                                                       00157002
* Data area                                                             00157102
*                                                                       00157202
ENBLECH0 DC    C'80'            mask to enable channel 0 interrupts     00157302
READ     DC    X'02'            read a card                             00157402
         DS    0D               align                                   00157502
WAITPSWD DC    X'80020000'      wait with channel 0 interrupts enabled  00157602
CONTINUE DS    F                continue here after wait                00157702
LOADADDR DC    F'0'             code is to be loaded here               00157802
NUMCARDS DC    F'75'            number of cards to read                 00157904
EIGHT    DC    F'8'             CCW length                              00158002
CCWCHAIN DS    0D               start of card reader CCW chain          00158102
***                                                                     00158202
*** Sample Operating System code begins here                            00158302
***                                                                     00158402
PROGRAM  CSECT ,                   sample OS must follow loader cards   00158502
         SPACE 1                                                        00160000
CORESIZE EQU   16777216            bytes of core in object machine      00170002
         SPACE 1                                                        00180000
         USING *,0 COMMUNICATIONS AREA                                  00190000
         SPACE 1                                                        00200000
IPLPSW   DC    B'00000000',B'00000000',X'0000',X'00',AL3(IPLRTN)        00210000
IPLCCW1  DS    D .                 IPL CCW #1                           00220000
IPLCCW2  DS    D .                 IPL CCW #2                           00230000
EXTOLD   DS    D .                 EXTERNAL OLD PSW                     00240000
SVCOLD   DS    D .                 SVC OLD PSW                          00250000
PGMOLD   DS    D .                 PROGRAM INTERRUPT OLD PSW            00260000
MCHKOLD  DS    D .                 MACHINE CHECK OLD PSW                00270000
IOOLD    DS    D .                 I/O INTERRUPT OLD PSW                00280000
CSW      DS    D .                 CHANNEL STATUS WORD                  00290000
CAW      DS    F .                 CHANNEL ADDRESS WORD                 00300000
UNUSED0  DS    F .                                                      00310000
TIMER    DC    F'-1' .             TIMER                                00320000
UNUSED1  DC    F'0' .                                                   00330000
EXTNEW   DC    B'00000000',B'00000000',X'0000',X'00',AL3(EXTHANDL)      00340000
SVCNEW   DC    B'00000000',B'00000000',X'0000',X'00',AL3(SVCHANDL)      00350000
PGMNEW   DC    B'00000000',B'00000000',X'0000',X'00',AL3(PGMHANDL)      00360000
MCHKNEW  DC    B'00000000',B'00000010',X'0000',X'00',AL3(0)             00370000
IONEW    DC    B'00000000',B'00000000',X'0000',X'00',AL3(IOINTRPT) <-+  00380002
***                                                                  |  00382002
***  IOINTRPT will be replaced with IOHANDL after IPL by IPLRTN -----+  00384002
***                                                                     00386002
         ORG   *+X'100' SPACE OVER STAND ALONE DUMP AREA                00390000
FSBPTR   DC    A(VERYEND) .        FSB POINTER                          00400000
FSBSEM   DC    F'1,0' .            FSB SEMAPHORE                        00410000
MEMORY   DC    F'0,0' .            MEMORY SEMAPHORE                     00420000
CAWSEM   DC    F'1,0' .            CAW SEMAPHORE                        00430000
         SPACE 1                                                        00440000
TRAPSAVE DS    16F .               STORAGE FOR EXTERNAL INTERRUPTS      00450000
IOHSAVE  DS    16F .               STORAGE FOR I/O INTERRUPTS           00460000
         SPACE 1                                                        00470000
SYSSEMSA DS    CL84 .              SYSTEM SEMAPHORE SAVE AREA           00480000
         SPACE 1                                                        00490000
RUNNING  DS    A .                 RUNNING                              00500000
NEXTTRY  DS    A .                 NEXTTRY                              00510000
NEXTTRYM DS    C,0H .              NEXTTRY MODIFIED                     00520000
         EJECT                                                          00530000
*********************************************************************** 00540000
*                                                                     * 00550000
*              EXTERNAL, PROGRAM, AND SVC INTERRUPT HANDLERS          * 00560000
*                                                                     * 00570000
*********************************************************************** 00580000
         SPACE 1                                                        00590000
EXTHANDL EQU   * .                 EXTERNAL INTERRUPT HANDLER           00600000
         STM   0,15,TRAPSAVE .     SAVE REGISTERS                       00610000
         BALR  1,0 .               ESTABLISH ADDRESSING                 00620000
         USING *,1                                                      00630000
         CLI   EXTOLD+3,X'80' .    SEE IF TIMER TRAP                    00640000
         BNE   EXTHRET .           IF NOT, IGNORE                       00650000
         L     15,RUNNING .        SET UP REGISTERS FOR TRAFFIC         00660000
         USING PCB,15 .             CONTROLLER (XPER)                   00670000
         CLI   PCBBLOKT,X'FF' .    IF BLOCKED, NO PROCESS IS            00680000
         BE    EXTHRET .            RUNNABLE, SO RETURN                 00690000
         LA    14,PCBISA .         GET SAVE AREA                        00700000
         USING SA,14                                                    00710000
         MVC   SAPSW,EXTOLD .      AND STORE OLD STUFF INTO IT          00720000
         MVC   SAREGS,TRAPSAVE                                          00730000
         B     XPER .              THEN GO TO TRAFFIC SCHEDULER         00740000
         DROP  14,15                                                    00750000
EXTHRET  LM    0,15,TRAPSAVE .     TO IGNORE AN INTERRUPT, RELOAD       00760000
         LPSW  EXTOLD .            AND TRANSFER BACK                    00770000
         SPACE 1                                                        00780000
PGMHANDL EQU   * .                 PROGRAM INTERRUPT HANDLER            00790000
         SVC   C'?' .              IN ANY CASE, AN ERROR                00800000
         EJECT                                                          00810000
*********************************************************************** 00820000
*                                                                     * 00830000
*                          SVC INTERRUPT HANDLER                      * 00840000
*                                                                     * 00850000
*        FOR ALL ROUTINES ENTERED BY SVC INTERRUPT, THE               * 00860000
*        FOLLOWING REGISTERS CONTAIN THIS INFORMATION:                * 00870000
*                                                                     * 00880000
*        REGISTER  1 - BASE REGISTER FOR ROUTINE                      * 00890000
*        REGISTER  2 - POINTER TO ARGUMENT LIST (IF ANY)              * 00900000
*        REGISTER 14 - POINTER TO SAVEAREA USED FOR THIS SVC          * 00910000
*        REGISTER 15 - POINTER TO PCB PRESENTLY RUNNING               * 00920000
*                                                                     * 00930000
*********************************************************************** 00940000
         SPACE 1                                                        00950000
SVCHANDL EQU   * .                 SVC HANDLER                          00960000
         STM   0,15,TRAPSAVE .     SAVE REGISTERS                       00970000
         BALR  9,0 .               ESTABLISH ADDRESSING                 00980000
         USING *,9                                                      00990000
         LM    10,14,SVCCONST .    INITIALIZE REGISTERS                 01000000
         IC    10,SVCOLD+3 .       GET SVC CODE                         01010000
         IC    10,SVCHTABL(10) .   TRANSLATE INTO TABLE OFFSET          01020000
         LA    10,SVCRTN(10) .     REG 10 -> THE CORRECT PSW            01030000
         CLI   2(10),X'00' .       IS THIS CALL PROTECTED?              01040000
         BE    SVCHPROT .          THEN SEE IF WE CAN CALL IT           01050000
SVCOK    L     15,RUNNING .        GET PCB POINTER                      01060000
         USING PCB,15                                                   01070000
         CLI   3(10),X'00' .       IS IT A SYSTEM SAVEAREA?             01080000
         BE    SYSSEM .            DON'T USE REG 14 AS PCB POINTER      01090000
         LR    14,15 .             ELSE, SET UP PCB POINTER             01100000
SYSSEM   IC    11,3(10) .          GET POINTER TO SAVE AREA OFFSET      01110000
         A     14,SVCSAVE(11) .    REG 14 -> SAVE AREA                  01120000
         CLI   SVCOLD+3,C'.' .     ARE WE CALLING XPER?                 01130000
         BE    SVCXPER .           IF SO, DON'T SAVE RETURN STATUS      01140000
         USING SA,14                                                    01150000
         MVC   SAPSW,SVCOLD .      SAVE PSW                             01160000
         MVC   SAREGS,TRAPSAVE .   SAVE REGISTERS                       01170000
SVCXPER  L     1,4(10) .           MAKE ADDRESSING EASY WITHIN          01180000
         LPSW  0(10) .              ROUTINE, AND GO THERE               01190000
SVCHPROT L     12,SVCOLD .         GET PROTECTION KEY                   01200000
         NR    12,13 .             IS IT A USER?                        01210000
         BZ    SVCOK .             IF NO, THAT'S FINE                   01220000
         LA    10,SVCRTN+136 .     ELSE SET UP CALL TO XQUE             01230000
         B     SVCOK .                                                  01240000
         DROP  9                                                        01250000
SVCCONST DC    3F'0',X'00F00000',F'0'                                   01260000
         SPACE 1                                                        01270000
SVCHTABL DC    256X'84' .          TABLE OF PSW OFFSETS                 01280000
         ORG   SVCHTABL+C'P'                                            01290000
         DC    AL1(0)                                                   01300000
         ORG   SVCHTABL+C'V'                                            01310000
         DC    AL1(8)                                                   01320000
         ORG   SVCHTABL+C'!'                                            01330000
         DC    AL1(16)                                                  01340000
         ORG   SVCHTABL+C','                                            01350000
         DC    AL1(24)                                                  01360000
         ORG   SVCHTABL+C'B'                                            01370000
         DC    AL1(32)                                                  01380000
         ORG   SVCHTABL+C'A'                                            01390000
         DC    AL1(40)                                                  01400000
         ORG   SVCHTABL+C'F'                                            01410000
         DC    AL1(48)                                                  01420000
         ORG   SVCHTABL+C'I'                                            01430000
         DC    AL1(56)                                                  01440000
         ORG   SVCHTABL+C'J'                                            01450000
         DC    AL1(64)                                                  01460000
         ORG   SVCHTABL+C'.'                                            01470000
         DC    AL1(72)                                                  01480000
         ORG   SVCHTABL+C'R'                                            01490000
         DC    AL1(80)                                                  01500000
         ORG   SVCHTABL+C'S'                                            01510000
         DC    AL1(88)                                                  01520000
         ORG   SVCHTABL+C'C'                                            01530000
         DC    AL1(96)                                                  01540000
         ORG   SVCHTABL+C'N'                                            01550000
         DC    AL1(104)                                                 01560000
         ORG   SVCHTABL+C'Y'                                            01570000
         DC    AL1(112)                                                 01580000
         ORG   SVCHTABL+C'Z'                                            01590000
         DC    AL1(120)                                                 01600000
         ORG   SVCHTABL+C'D'                                            01610000
         DC    AL1(128)                                                 01620000
         ORG   SVCHTABL+C'?'                                            01630000
         DC    AL1(136)                                                 01640000
         ORG   SVCHTABL+C'H'                                            01650000
         DC    AL1(144)                                                 01660000
         ORG   SVCHTABL+C'E'                                            01670000
         DC    AL1(152)                                                 01680000
         ORG   SVCHTABL+256                                             01690000
         SPACE 1                                                        01700000
SVCRTN   DS    0D .                THE PSWS                             01710000
*                  IN THE FOLLOWING PSWS, THE THIRD BYTE INDICATES    * 01720000
*                  WHETHER THE SVC IS RESTRICTED:                     * 01730000
*                             X'00' -> OPERATING SYSTEM ONLY          * 01740000
*                             X'FF' -> AVAILABLE TO USER ALSO         * 01750000
*                                                                     * 01760000
*                   THE FOURTH BYTE INDICATES WHICH SAVE AREA TO USE; * 01770000
*                   SVCSAVE BELOW SHOWS THE CODE VALUES.              * 01780000
         DC    B'00000000',B'00000000',X'0000',X'00',AL3(XP)            01790000
         DC    B'00000000',B'00000000',X'0000',X'00',AL3(XV)            01800000
         DC    B'00000000',B'00000000',X'0004',X'00',AL3(XEXC)          01810000
         DC    B'00000000',B'00000000',X'0004',X'00',AL3(XCOM)          01820000
         DC    B'00000000',B'00000000',X'0004',X'00',AL3(XB)            01830000
         DC    B'11111111',B'00000000',X'000C',X'00',AL3(XA)            01840000
         DC    B'11111111',B'00000000',X'000C',X'00',AL3(XF)            01850000
         DC    B'00000000',B'00000000',X'0004',X'00',AL3(XI)            01860000
         DC    B'00000000',B'00000000',X'0004',X'00',AL3(XJ)            01870000
         DC    B'00000000',B'00000000',X'0004',X'00',AL3(XPER)          01880000
         DC    B'11111111',B'00000000',X'FF08',X'00',AL3(XR)            01890000
         DC    B'11111111',B'00000000',X'FF08',X'00',AL3(XS)            01900000
         DC    B'11111111',B'00000000',X'FF08',X'00',AL3(XC)            01910000
         DC    B'00000000',B'00000000',X'FF04',X'00',AL3(XN)            01920000
         DC    B'00000000',B'00000000',X'FF08',X'00',AL3(XY)            01930000
         DC    B'11111111',B'00000000',X'FF08',X'00',AL3(XZ)            01940000
         DC    B'11111111',B'00000000',X'FF08',X'00',AL3(XD)            01950000
         DC    B'00000000',B'00000000',X'FF04',X'00',AL3(XQUE)          01960000
         DC    B'11111111',B'00000000',X'FF08',X'00',AL3(XH)            01970000
         DC    B'11111111',B'00000000',X'000C',X'00',AL3(XAUTO)         01980000
         SPACE 1                                                        01990000
SVCSAVE  DS    0F .                THE SAVE AREA OFFSETS                02000000
         DC    A(SYSSEMSA) .       CODE 00 -> SYSSEMSA                  02010000
         DC    A(PCBISA-PCB) .     CODE 04 -> INTERRUPT SAVE AREA       02020000
         DC    A(PCBFSA-PCB) .     CODE 08 -> FAULT SAVE AREA           02030000
         DC    A(PCBMSA-PCB) .     CODE 0C -> MEMORY SAVE AREA          02040000
         SPACE 3                                                        02050000
*********************************************************************** 02060000
*                                                                     * 02070000
* RETURN SEQUENCE FOR REQUEST DRIVEN ROUTINES AND TRAFFIC CONTROLLER  * 02080000
*                                                                     * 02090000
*********************************************************************** 02100000
         SPACE 1                                                        02110000
         DS    0D                                                       02120000
RETURN   DC    B'00000000',B'00000000',X'0000',X'00',AL3(RETURNR)       02130000
         SPACE 1                                                        02140000
RETURNR  EQU   * .                 RETURN ROUTINE FOR SVC'S AND XPER    02150000
         MVC   SVCOLD,SAPSW .      SAVE PSW IN A SAFE PLACE             02160000
         LM    0,15,SAREGS .       RELOAD REGISTERS                     02170000
         LPSW  SVCOLD .            AND RETURN                           02180000
         EJECT                                                          02190000
*********************************************************************** 02200000
*                                                                     * 02210000
*                          REQUEST DRIVEN ROUTINES                    * 02220000
*                                                                     * 02230000
*********************************************************************** 02240000
         SPACE 3                                                        02250000
*********************************************************************** 02260000
*                                                                     * 02270000
*                                  XP ROUTINE                         * 02280000
*                                                                     * 02290000
*        FUNCTION: TO IMPLEMENT "P" PRIMITIVE FOR SEMAPHORES          * 02300000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS SM         * 02310000
*                    SM        DS 0D   SEMAPHORE DEFINITION           * 02320000
*                    SMVAL     DS F    VALUE                          * 02330000
*                    SMPTR     DS A    POINTER TO FIRST WAITER        * 02340000
*   ROUTINES USED: XPER                                               * 02350000
*       PROCEDURE: SUBTRACT ONE FROM SMVAL; IF NON-NEGATIVE, RETURN.  * 02360000
*                  IF NEGATIVE, PLACE RUNNING PROCESS AT END OF LIST  * 02370000
*                  OF PRECESSES WAITING ON SM. BLOCK CALLING PROCESS; * 02380000
*                  ENTER TRAFFIC CONTROLLER.                          * 02390000
*    ERROR CHECKS: NONE                                               * 02400000
*      INTERRUPTS: OFF                                                * 02410000
*     USER ACCESS: NO                                                 * 02420000
*                                                                     * 02430000
*********************************************************************** 02440000
         SPACE 1                                                        02450000
XP       EQU   * .                 THE XP ROUTINE                       02460000
         USING *,1                                                      02470000
         USING SM,2 .              ARGUMENT IS A SEMAPHORE              02480000
         L     3,SMVAL  .          GET THE VALUE                        02490000
         BCTR  3,0 .               SUBTRACT ONE                         02500000
         ST    3,SMVAL .           AND STORE IT BACK                    02510000
         LTR   3,3 .               SET CONDITION CODE                   02520000
         BM    XPWAIT .            IF IT'S NEGATIVE, MUST WAIT          02530000
         LPSW  RETURN .            ELSE RETURN NOW                      02540000
XPWAIT   LA    4,SMPTR .           START GOING DOWN                     02550000
         L     5,SMPTR .            CHAIN OF POINTERS                   02560000
         DROP  15                                                       02570000
         USING PCB,5                                                    02580000
XPLOOP   LTR   5,5 .               IF REACHED END                       02590000
         BZ    XPTHEN .            ADD OUR PCB ON. ELSE,                02600000
         LA    4,PCBNSW .          INCREMENT POINTERS                   02610000
         L     5,PCBNSW                                                 02620000
         B     XPLOOP .            AND TRY AGAIN                        02630000
         DROP  5                                                        02640000
         USING PCB,15                                                   02650000
XPTHEN   MVC   0(4,4),RUNNING .    WE'RE AT THE END                     02660000
         ST    5,PCBNSW .          STORE NULL POINTER                   02670000
         MVI   PCBBLOKT,X'FF' .    AND WE'RE BLOCKED                    02680000
         MVC   PCBISA,SYSSEMSA .   SWITCH SAVE AREAS                    02690000
         B     XPER .              SO RUN SOMEONE ELSE                  02700000
         DROP  2                                                        02710000
         EJECT                                                          02720000
*********************************************************************** 02730000
*                                                                     * 02740000
*                                  XV ROUTINE                         * 02750000
*                                                                     * 02760000
*        FUNCTION: TO IMPLEMENT "V" PRIMITIVE FOR SEMAPHORES          * 02770000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS SM         * 02780000
*                    SM        DS 0D   SEMAPHORE DEFINITION           * 02790000
*                    SMVAL     DS F    VALUE                          * 02800000
*                    SMPTR     DS A    POINTER TO FIRST WAITER        * 02810000
*   ROUTINES USED: NONE                                               * 02820000
*       PROCEDURE: ADD ONE TO SMVAL; IF > ZERO, RETURN. IF ZERO OR    * 02830000
*                  LESS, REMOVE FIRST PROCESS FROM WAITER CHAIN;      * 02840000
*                  UNBLOCK IT; IF NEXTTRYM NOT SET, SET IT AND SET    * 02850000
*                  NEXTTRY TO THAT PROCESS; RETURN; IF NEXTTRYM SET,  * 02860000
*                  RETURN.                                            * 02870000
*    ERROR CHECKS: NONE                                               * 02880000
*      INTERRUPTS: OFF                                                * 02890000
*     USER ACCESS: NO                                                 * 02900000
*                                                                     * 02910000
*********************************************************************** 02920000
         SPACE 1                                                        02930000
XV       EQU   * .                 THE XV ROUTINE                       02940000
         USING *,1                                                      02950000
         USING SM,2 .              ARGUMENT IS A SEMAPHORE              02960000
         L     3,SMVAL .           GET THE VALUE                        02970000
         A     3,=F'1' .           ADD ONE                              02980000
         ST    3,SMVAL .           AND STORE IT BACK                    02990000
         BNP   XVWAKEUP .          IF <=0, SOMEONE'S WAITING            03000000
         LPSW  RETURN .            ELSE RETURN                          03010000
XVWAKEUP L     4,SMPTR .           GET THE FIRST OF THE GUYS            03020000
         DROP  15                                                       03030000
         USING PCB,4                                                    03040000
         MVC   SMPTR,PCBNSW .      REMEMBER THE REST                    03050000
         MVI   PCBBLOKT,X'00' .    WE'RE NO LONGER BLOCKING HIM         03060000
         CLI   NEXTTRYM,X'FF' .    IS NEXT TRY MODIFIED?                03070000
         BE    XVRET .             IF SO, WELL OK                       03080000
         ST    4,NEXTTRY           ELSE MODIFY NEXTTRY                  03090000
         MVI   NEXTTRYM,X'FF' .    AND SAY SO                           03100000
XVRET    LPSW  RETURN .            GET BACK                             03110000
         DROP  2,4                                                      03120000
         EJECT                                                          03130000
*********************************************************************** 03140000
*                                                                     * 03150000
*                          XPER ROUTINE (TRAFFIC CONTROLLER)          * 03160000
*                                                                     * 03170000
*        FUNCTION: TO IMPLEMENT MULTIPROGRAMMING                      * 03180000
*       DATABASES: NONE                                               * 03190000
*   ROUTINES USED: NONE                                               * 03200000
*       PROCEDURE: STARTING WITH NEXTTRY, SEARCH FOR PROCESS ON ALL   * 03210000
*                  PCB CHAIN NOT BLOCKED OR STOPPED; IF FOUND, USE AS * 03220000
*                  NEW RUNNING, FOR 50 MS OF TIME AND RETURN. ELSE,   * 03230000
*                  ENTER WAIT STATE WITH INTERRUPTS ON, AND TRY TO    * 03240000
*                  SCHEDULE AGAIN AFTER INTERRUPT; RETURN.            * 03250000
*    ERROR CHECKS: NONE                                               * 03260000
*      INTERRUPTS: OFF                                                * 03270000
*     USER ACCESS: NO                                                 * 03280000
*                                                                     * 03290000
*********************************************************************** 03300000
         SPACE 1                                                        03310000
XPER     EQU   * .                 ROUTINE XPER: TRAFFIC SCHEDULER      03320000
         SSM   IONEW .             MASK OFF INTERRUPTS                  03330000
         BALR  1,0                                                      03340000
         USING *,1                                                      03350000
         L     10,NEXTTRY .        START LOOKING AT NEXTTRY             03360000
         LR    11,10 .             REMEMBER WHICH THAT WAS              03370000
         USING PCB,10                                                   03380000
GWLOOP   CLI   PCBBLOKT,X'FF' .    IF IT'S BLOCKED                      03390000
         BE    GWINC .             IGNORE                               03400000
         CLI   PCBSTOPT,X'FF' .    ELSE, IF IT'S NOT STOPPED            03410000
         BNE   GWRUN .             WE CAN RUN IT                        03420000
GWINC    L     10,PCBNPALL .       ELSE, GO TO THE NEXT                 03430000
         CR    10,11 .             IF WE'VE SEEN ALL, QUIT              03440000
         BNE   GWLOOP .            ELSE TRY AGAIN                       03450000
         LPSW  IDLE .              SIT AND WAIT                         03460000
         DS    0D                                                       03470000
IDLE     DC    B'11111110',B'00000010',X'0000',X'00',AL3(XPER)          03480000
         SPACE 1                                                        03490000
GWRUN    MVC   NEXTTRY,PCBNPALL .  GET A NEW NEXTTRY                    03500000
         MVI   NEXTTRYM,X'00' .    NOT MODIFIED                         03510000
         ST    10,RUNNING .        GET A NEW RUNNING                    03520000
         LA    14,PCBISA                                                03530000
         MVC   TIMER,QUANTUM .     INTERRUPT AFTER 50 MS                03540000
         LPSW  RETURN .            AND GO TO RETURNR                    03550000
QUANTUM  DC    X'00000F00' .       QUANTUM OF TIME                      03560000
         DROP  10                                                       03570000
         USING PCB,15                                                   03580000
         EJECT                                                          03590000
*********************************************************************** 03600000
*                                                                     * 03610000
*                          XEXC ROUTINE                               * 03620000
*                                                                     * 03630000
*        FUNCTION: TO ENTER SMC SECTION                               * 03640000
*       DATABASES: NONE                                               * 03650000
*   ROUTINES USED: NONE                                               * 03660000
*       PROCEDURE: INCREMENT SMC BYTE IN PCB BY ONE; RETURN.          * 03670000
*    ERROR CHECKS: NONE                                               * 03680000
*      INTERRUPTS: OFF                                                * 03690000
*     USER ACCESS: NO                                                 * 03700000
*                                                                     * 03710000
*********************************************************************** 03720000
         SPACE 1                                                        03730000
XEXC     EQU   * .                 ROUTINE XEXC: ENTER SMC SECTION      03740000
         USING *,1                                                      03750000
         SR    8,8                                                      03760000
         IC    8,PCBINSMC                                               03770000
         LA    8,1(8) .            ADD ONE TO SMC BYTE                  03780000
         STC   8,PCBINSMC                                               03790000
         LPSW  RETURN .            AND LEAVE                            03800000
         SPACE 1                                                        03810000
*********************************************************************** 03820000
*                                                                     * 03830000
*                          XCOM ROUTINE                               * 03840000
*                                                                     * 03850000
*        FUNCTION: TO LEAVE SMC SECTION                               * 03860000
*       DATABASES: NONE                                               * 03870000
*   ROUTINES USED: XP, XV                                             * 03880000
*       PROCEDURE: DECREMENT SMC BYTE IN PCB BY ONE; IF NOT ZERO,     * 03890000
*                  RETURN. ELSE, CHECK FOR STOP WAITING; IF STOP      * 03900000
*                  WAITING, ALLOW STOP AND BLOCK SELF; RETURN. IF NO  * 03910000
*                  STOP WAITING, RETURN.                              * 03920000
*    ERROR CHECKS: NONE                                               * 03930000
*      INTERRUPTS: OFF                                                * 03940000
*     USER ACCESS: NO                                                 * 03950000
*                                                                     * 03960000
*********************************************************************** 03970000
         SPACE 1                                                        03980000
XCOM     EQU   * .                 ROUTINE XCOM: LEAVE SMC              03990000
         USING *,1                                                      04000000
         SR    8,8                                                      04010000
         IC    8,PCBINSMC                                               04020000
         BCTR  8,0 .               SUBTRACT ONE FROM IN SMC BYTE        04030000
         STC   8,PCBINSMC                                               04040000
         LTR   8,8 .               IS IT ZERO?                          04050000
         BNZ   XCOMRET .           NO, THEN GET BACK, OTHERWISE         04060000
         CLI   PCBSW,X'00' .       IS STOP WAITING?                     04070000
         BE    XCOMRET .           IF NOT, RETURN                       04080000
         MVI   PCBSW,X'00' .       STOPS NOT WAITING AFTER THIS         04090000
         LA    2,PCBSRS .          WE'LL "V" THE STOPPER,               04100000
         SVC   C'V'                                                     04110000
         LA    2,PCBSES .          AND "P" THE STOPPEE.                 04120000
         SVC   C'P'                                                     04130000
XCOMRET  LPSW  RETURN .            AND HERE (IF EVER) WE RETURN         04140000
         EJECT                                                          04150000
*********************************************************************** 04160000
*                                                                     * 04170000
*                            XA ROUTINE                               * 04180000
*                         XAUTO ROUTINE                               * 04190000
*                                                                     * 04200000
*        FUNCTION: TO ALLOCATE MEMORY                                 * 04210000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XAX:       * 04220000
*                    XAX       DS 0D                                  * 04230000
*                    XAXSIZE   DS F     SIZE OF BLOCK TO BE ALLOCATED * 04240000
*                    XAXADDR   DS A     ADDRESS OF FIRST BYTE OF BLOCK* 04250000
*                    XAXALGN   DS F     ALIGNMENT OF BLOCK            * 04260000
*   ROUTINES USED: XEXC, XCOM, XP, XV, XB                             * 04270000
*       PROCEDURE: LOCK FSB SEMAPHORE; SEARCH FREE STORAGE FOR LARGE  * 04280000
*                  ENOUGH MEMORY BLOCK; ALIGN BOUNDARY; USE XB TO     * 04290000
*                  CHAIN ANY LEFTOVER BLOCKS TO FREE STORAGE LIST;    * 04300000
*                  PLACE ADDRESS OF ALLOCATED BLOCK IN XAXADDR; UNLOCK* 04310000
*                  FSB SEMAPHORE; RETURN. IF CAN'T SATISFY REQUEST,   * 04320000
*                  UNLOCK FSB SEMAPHORE, APPLY XP ROUTINE TO MEMORY   * 04330000
*                  SEMAPHORE, BLOCKING PROCESS RUNNING UNTIL MEMORY   * 04340000
*                  FREED; THEN UNBLOCK; TRY TO SATISFY REQUEST AGAIN. * 04350000
*    ERROR CHECKS: NONE                                               * 04360000
*      INTERRUPTS: ON                                                 * 04370000
*     USER ACCESS: NO                                                 * 04380000
*                                                                     * 04390000
*********************************************************************** 04400000
         SPACE 1                                                        04410000
XA       EQU   * .                 THE XA ROUTINE, TO ALLOCATE          04420000
         USING *,1                                                      04430000
         LA    0,1 .               SET REGISTER ZERO TO ONE TO          04440000
         B     XACOM .              INDICATE C'A' CALL                  04450000
XAUTO    EQU   * .                 AUTO STORAGE ENTRY POINT             04460000
         USING *,1                                                      04470000
         SR    0,0 .               REG0=0 INDICATES C'E' CALL           04480000
         L     1,=A(XA) .          RESET BASE REGISTER PROPERLY         04490000
         USING XA,1                                                     04500000
XACOM    SVC   C'!' .              ENTER SMC                            04510000
         LR    7,2                                                      04520000
         USING XAX,7 .             ARGUMENT LIST                        04530000
         L     6,XAXSIZE .         GET THE SIZE REQUESTED               04540000
XATOP    LA    2,FSBSEM .          LOCK THE FSB SEMAPHORE               04550000
         SVC   C'P' .                                                   04560000
         LA    5,FSBPTR .          START LOOKING DOWN                   04570000
         L     4,FSBPTR .           THE FREE STORAGE LIST               04580000
         L     8,XAXALGN .         WE WOULD HAVE TO START AT WITH       04590000
         BCTR  8,0 .                THIS CONSTANT TO FIND ALIGNMENT     04600000
         USING FSB,4                                                    04610000
XALOOP   LTR   4,4 .               IF AT THE END                        04620000
         BZ    XAWAIT .             WAIT UNTIL A "FREE" OP              04630000
         LR    13,4 .              FIND THE LOCATION                    04640000
         BCTR  13,0 .               IN THIS BLOCK WITH THIS             04650000
         OR    13,8 .              ALIGNMENT                            04660000
         LA    13,1(13) .          THAT'S IT                            04670000
         LR    9,13 .              AND NOW GET IN REG 9                 04680000
         SR    9,4 .                WHAT IS WASTED AT THE FRONT         04690000
         L     3,FSBSIZE .         GET SIZE MINUS WASTE AT              04700000
         SR    3,9 .                FRONT, LEAVING EFFECTIVE SIZE       04710000
         CR    6,3 .               IS IT ENOUGH?                        04720000
         BNP   XAFOUND .           EUREKA!                              04730000
         LA    5,FSBNEXT .         OH WELL, GET THE NEXT FREE           04740000
         L     4,FSBNEXT .          STORAGE BLOCK ON THE CHAIN          04750000
         B     XALOOP .            BETTER LUCK NEXT TIME                04760000
XAWAIT   SVC   C'V' .              NEED TO WAIT                         04770000
         LA    2,MEMORY .          SO WE LET OTHER PEOPLE GET IN        04780000
         SVC   C'P' .              SO THEY'LL WAKE US UP                04790000
         B     XATOP .             AND THEN WE'LL TRY AGAIN             04800000
XAFOUND  ST    13,XAXADDR .        WE'VE NOW GOT THE ADDRESS            04810000
         MVC   0(4,5),FSBNEXT .    UNLINK THE BLOCK OUT                 04820000
         L     12,FSBSIZE .        GET THE WHOLE BLOCK SIZE             04830000
         LA    2,SATEMP .          START MAKING UP ARG LISTS            04840000
         USING XBX,2 .             FOR THE XB ROUTINE                   04850000
         LR    10,13 .             THE STARTING LOCATION                04860000
         SR    10,4 .              MINUS THE START OF THE BLOCK         04870000
         BZ    XANF .              IF NONE WASTED AT THE FRONT, SKIP    04880000
         ST    4,XBXADDR .         ELSE FREE, STARTING THERE            04890000
         ST    10,XBXSIZE .         UP TO THE BEGINNING OF THE          04900000
         SVC   C'B' .               ALLOCATION; INSERT IT IN THE CHAIN  04910000
XANF     LR    11,13 .             THE STARTING ADDR PLUS THE SIZE      04920000
         AR    11,6 .               GIVES THE FIRST UNUSED ADDR         04930000
         SR    12,10 .             MINUS THE WASTE AT FRONT,            04940000
         SR    12,6 .              MINUS THE PART ALLOCATED. IF         04950000
         BZ    XARETURN .          NONE LEFT OVER, GOOD                 04960000
         ST    11,XBXADDR .        ELSE STORE ADDRESS AND               04970000
         ST    12,XBXSIZE .        SIZE, AND LINK ONTO                  04980000
         SVC   C'B' .              FREE STORAGE LIST                    04990000
         DROP  2                                                        05000000
XARETURN LA    2,FSBSEM .          WE ARE DONE, SO NOW SOMEONE          05010000
         SVC   C'V' .               ELSE CAN COME IN                    05020000
         LTR   0,0 .               IS THIS FOR AUTOMATIC STORAGE?       05030000
         BNZ   XABACK .            IF NOT, RETURN NOW                   05040000
         ST    6,PCBASIZE .        OTHERWISE STORE SIZE AND             05050000
         ST    13,PCBAADDR .        ADDRESS OF AUTOMATIC STORAGE        05060000
XABACK   SVC   C',' .              LEAVE SMC SECTION                    05070000
         LPSW  RETURN .            GET BACK JOJO                        05080000
         DROP  4,7                                                      05090000
         EJECT                                                          05100000
*********************************************************************** 05110000
*                                                                     * 05120000
*                            XF ROUTINE                               * 05130000
*                                                                     * 05140000
*        FUNCTION: TO FREE MEMORY                                     * 05150000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XFX:       * 05160000
*                    XFX       DS 0D                                  * 05170000
*                    XFXSIZE   DS F     SIZE OF BLOCK TO BE FREED     * 05180000
*                    XFXADDR   DS A     ADDRESS OF FIRST BYTE OF BLOCK* 05190000
*   ROUTINES USED: XEXC, XP, XV, XB, XCOM                             * 05200000
*       PROCEDURE: LOCK FSB SEMAPHORE; SEARCH FREE STORAGE LIST TO    * 05210000
*                  FIND IF ANY FREE BLOCK CONTIGUOUSLY FOLLOWS OR     * 05220000
*                  PRECEDES BLOCK TO BE FREED; IF THERE IS ANY,       * 05230000
*                  COMPACT THEM INTO A SINGLE BLOCK OF COMBINED SIZE; * 05240000
*                  USE XB TO CHAIN COMPACTED BLOCK ONTO FREE STORAGE  * 05250000
*                  LIST; WAKEUP ALL PROCESSES WAITING ON MEMORY       * 05260000
*                  SEMAPHORE; UNLOCK FSB SEMAPHORE; RETURN            * 05270000
*    ERROR CHECKS: NONE                                               * 05280000
*      INTERRUPTS: ON                                                 * 05290000
*     USER ACCESS: NO                                                 * 05300000
*                                                                     * 05310000
*********************************************************************** 05320000
         SPACE 1                                                        05330000
XF       EQU   * .                 THE XF ROUTINE, TO FREE STORAGE      05340000
         USING *,1                                                      05350000
         SVC   C'!' .              ENTER SMC SECTION                    05360000
         LR    7,2                                                      05370000
         USING XFX,7 .             THE ARGUMENT LIST                    05380000
         L     3,XFXSIZE .         GET THE SIZE                         05390000
         L     4,XFXADDR .         AND THE ADDRESS                      05400000
         LR    5,3 .               GET THE ADDRESS OF THE END OF THE    05410000
         AR    5,4 .                BLOCK TO BE FREED                   05420000
         LA    2,FSBSEM .          LOCK FSBSEM                          05430000
         SVC   C'P'                                                     05440000
         LA    8,FSBPTR .          START LOOKING DOWN THE FREE          05450000
         L     6,FSBPTR .           STORAGE LIST, FOR COMPACTION        05460000
         USING FSB,6                                                    05470000
XFLOOP   LTR   6,6 .               ARE WE THROUGH?                      05480000
         BZ    XFLINK .            IF SO, JUST ADD IT ON                05490000
         L     9,FSBNEXT .         IF NOT. GET THE NEXT PTR             05500000
         CR    6,5 .               IS THIS BLOCK RIGHT AFTER OURS?      05510000
         BNE   XFTHEN .            IF NOT, OK. BUT IF IT IS,            05520000
         ST    9,0(8) .            WE CAN COMPACT, SO UNCHAIN IT        05530000
         A     3,FSBSIZE .         AND REMEMBER THE NEW SIZE            05540000
         B     XFBACKUP .          AND ON TO THE NEXT                   05550000
XFTHEN   LR    10,6 .              MAYBE IT'S RIGHT BEFORE OURS         05560000
         A     10,FSBSIZE .        GET ENDING ADDRESS OF FREE BLOCK     05570000
         CR    10,4 .              IS IT RIGHT BEFORE OURS?             05580000
         BNE   XFINC .             OH FUDGE! NO!                        05590000
         ST    9,0(8) .            IF SO, UNLINK IT                     05600000
         LR    4,6 .               GET THE NEW BEGINNING LOCATION       05610000
         A     3,FSBSIZE .         AND NEW SIZE OF FREE BLOCK           05620000
XFBACKUP LR    6,8 .               BACK UP ONE FSB                      05630000
XFINC    LA    8,FSBNEXT .         ON TO THE NEXT FSB                   05640000
         L     6,FSBNEXT                                                05650000
         B     XFLOOP .            TRY, TRY AGAIN                       05660000
XFLINK   LA    2,SATEMP .          START TO CALL XB                     05670000
         USING XBX,2                                                    05680000
         ST    3,XBXSIZE .         STORE SIZE                           05690000
         ST    4,XBXADDR .         AND ADDRESS                          05700000
         SVC   C'B' .              LINK IT ONTO THE FSB CHAIN           05710000
         USING SM,2                                                     05720000
         LA    2,MEMORY .          GET VALUE OF MEMORY SEMAPHORE        05730000
         LA    11,1(0,0) .         SUBTRACT FROM ONE, IT'S A HANDLE     05740000
         S     11,SMVAL .          ON THE # OF PEOPLE WAITING           05750000
         DROP  2                                                        05760000
XFVLOOP  BCT   11,XFVDO .          LOOP IF ANYONE ELSE IS WAITING       05770000
         LA    2,FSBSEM .          WE'RE THROUGH, SO                    05780000
         SVC   C'V' .              UNBLOCK FSBSEM                       05790000
         SVC   C',' .              LEAVE SMC                            05800000
         LPSW  RETURN .            RETURN                               05810000
XFVDO    SVC   C'V' .              WAKE SOMEONE UP                      05820000
         B     XFVLOOP .           TRY AGAIN FOR ANOTHER                05830000
         DROP  6,7                                                      05840000
         EJECT                                                          05850000
*********************************************************************** 05860000
*                                                                     * 05870000
*                            XB ROUTINE                               * 05880000
*                                                                     * 05890000
*        FUNCTION: TO CHAIN A STORAGE BLOCK ONTO FREE STORAGE LIST    * 05900000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XBX:       * 05910000
*                    XBX       DS 0D                                  * 05920000
*                    XBXSIZE   DS F     SIZE OF BLOCK                 * 05930000
*                    XBXADDR   DS A     ADDRESS OF FIRST BYTE OF BLOCK* 05940000
*   ROUTINES USED: NONE                                               * 05950000
*       PROCEDURE: SEARCH FREE STORAGE LIST TO FIND WHERE TO INSERT   * 05960000
*                  FREE BLOCK IN ORDER OF INCREASING SIZE; FORMAT     * 05970000
*                  BLOCK LIKE AN FSB; INSERT; RETURN.                 * 05980000
*    ERROR CHECKS: NONE                                               * 05990000
*      INTERRUPTS: OFF                                                * 06000000
*     USER ACCESS: NO                                                 * 06010000
*        COMMENTS: SINCE XB ROUTINE ONLY CALLED BY XA AND XF, FSB     * 06020000
*                  SEMAPHORE IS ALREADY LOCKED.                       * 06030000
*                                                                     * 06040000
*********************************************************************** 06050000
         SPACE 1                                                        06060000
XB       EQU   *                                                        06070000
         USING *,1                                                      06080000
         USING XBX,2 .             ARGUMENT LIST                        06090000
         L     3,XBXSIZE .         GET THE SIZE                         06100000
         L     4,XBXADDR .         AND THE ADDRESS                      06110000
         LA    8,FSBPTR .          START LOOKING DOWN THE CHAIN         06120000
         L     6,FSBPTR                                                 06130000
         LTR   6,6 .               IF ZERO POINTER, WE ARE AT           06140000
         BZ    XBINSERT .          END OF CHAIN ALREADY                 06150000
         USING FSB,6                                                    06160000
XBLOOP   C     3,FSBSIZE .         IF THE SIZE OF OURS IS LESS,         06170000
         BNP   XBINSERT .           TIME TO INSERT                      06180000
         LA    8,FSBNEXT .         ELSE GO ON TO THE NEXT               06190000
         L     6,FSBNEXT                                                06200000
         LTR   6,6 .               IF NOT ALREADY THROUGH               06210000
         BNZ   XBLOOP .            BRANCH BACK                          06220000
XBINSERT ST    4,0(8) .            NOW, LINK OURS ON                    06230000
         DROP  6                                                        06240000
         USING FSB,4                                                    06250000
         ST    6,FSBNEXT .         MAKE OURS POINT TO THE NEXT          06260000
         ST    3,FSBSIZE .         WITH THE RIGHT SIZE                  06270000
         LPSW  RETURN .            AND RETURN                           06280000
         DROP  2,4                                                      06290000
         EJECT                                                          06300000
*********************************************************************** 06310000
*                                                                     * 06320000
*                            XC ROUTINE                               * 06330000
*                                                                     * 06340000
*        FUNCTION: TO CREATE A PROCESS                                * 06350000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XCX:       * 06360000
*                    XCX       DS 0D                                  * 06370000
*                    XCXNAME   DS CL8   NAME OF PROCESS TO BE CREATED * 06380000
*   ROUTINES USED: XEXC, XCOM, XN, XA, XI, XQUE                       * 06390000
*       PROCEDURE: USE XA TO ALLOCATE NEW PCB; PLACE XCXNAME IN PCB;  * 06400000
*                  INITIALIZE SEMAPHORES; STOP; BLOCK; OUT OF SMC;    * 06410000
*                  CALL XI TO LINK PCB ONTO PCB CHAINS; RETURN.       * 06420000
*    ERROR CHECKS: IF NAME ALREADY USED IN THIS GROUP, XQUE ENTERED.  * 06430000
*      INTERRUPTS: ON                                                 * 06440000
*     USER ACCESS: YES                                                * 06450000
*                                                                     * 06460000
*********************************************************************** 06470000
         SPACE 1                                                        06480000
XC       EQU   * .                 THE XC ROUTINE: CREATE A PROCESS     06490000
         USING *,1                                                      06500000
         LR    7,2                                                      06510000
         USING XCX,7 .             ARGUMENT LIST                        06520000
         LA    2,SATEMP .          READY TO MAKE CALLS OUT              06530000
         USING XNX,2 .             A XN-LIKE ARGUMENT LIST              06540000
         MVC   XNXNAME,XCXNAME .   GET THE NAME                         06550000
         SVC   C'N' .              AND CALL TO FIND THE PCB             06560000
         CLC   XNXADDR,=A(0) .     SEE IF THERE                         06570000
         BNE   XCERR .             IF ALREADY EXISTS, BAD               06580000
         SVC   C'!' .              ENTER SMC SECTION                    06590000
         DROP  2                                                        06600000
         USING XAX,2 .             READY TO CALL XA                     06610000
         MVC   XAXSIZE,=A(LENPCB) . WE KNOW THE SIZE                    06620000
         MVC   XAXALGN,=F'8' .     AND THE ALIGNMENT                    06630000
         SVC   C'A' .              SO CALL                              06640000
         L     2,XAXADDR .         FIND THE ADDRESS                     06650000
         DROP  2,15                                                     06660000
         USING PCB,2 .             FILL IN THE PCB                      06670000
         MVC   PCBNAME,XCXNAME .   GIVE IT A NAME                       06680000
         MVI   PCBSTOPT,X'FF' .    IT'S STOPPED                         06690000
         MVC   PCBBLOKT(PCBISA-PCBBLOKT),TEMPLATE+1 INITIALIZE PCB      06700000
         SVC   C'I' .              THREAD IT ON                         06710000
         SVC   C',' .              LEAVE SMC SECTION                    06720000
         LPSW  RETURN .            AND RETURN                           06730000
XCERR    SVC   C'?' .              IF ALREADY EXISTS,KERROR             06740000
         DROP  2,7                                                      06750000
         EJECT                                                          06760000
*********************************************************************** 06770000
*                                                                     * 06780000
*                            XD ROUTINE                               * 06790000
*                                                                     * 06800000
*        FUNCTION: TO DESTROY A PROCESS                               * 06810000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XDX:       * 06820000
*                    XDX       DS 0D                                  * 06830000
*                    XDXNAME   DS CL8  NAME OF PROCESS TO BE DESTROYED* 06840000
*   ROUTINES USED: XEXC, XJ, XS, XN, XF, XCOM, XQUE                   * 06850000
*       PROCEDURE: USE XN TO FIND PCB FOR PROCESS TO BE DESTROYED;    * 06860000
*                  USE XJ TO UNLOCK PCB FROM PROCESS CHAINS; IF ANY   * 06870000
*                  MESSAGES FOR THIS PROCESS, FREE STORAGE FOR THEM;  * 06880000
*                  IF THERE IS ANY AUTOMATIC STORAGE, FREE IT;        * 06890000
*                  FREE STORAGE FOR PCB; RETURN.                      * 06900000
*    ERROR CHECKS: IF NAME DOESN'T EXIST OR PROCESS NOT STOPPED,      * 06910000
*                  XQUE ENTERED.                                      * 06920000
*      INTERRUPTS: ON                                                 * 06930000
*     USER ACCESS: YES                                                * 06940000
*                                                                     * 06950000
*********************************************************************** 06960000
         SPACE 1                                                        06970000
XD       EQU   * .                 XD ROUTINE: DESTROY A PROCESS        06980000
         USING *,1                                                      06990000
         LR    7,2                                                      07000000
         USING XDX,7 .             ARG LIST                             07010000
         LA    2,SATEMP .          READY TO CALL OUT                    07020000
         USING XNX,2 .             WILL CALL XN                         07030000
         MVC   XNXNAME,XDXNAME .   GET NAME                             07040000
         SVC   C'N' .              AND CALL                             07050000
         L     2,XNXADDR .         GET ADDRESS                          07060000
         DROP  2                                                        07070000
         LTR   2,2 .               IF ADDRESS IS NULL,                  07080000
         BZ    XDERR .             IT'S AN ERROR                        07090000
         USING PCB,2                                                    07100000
         CLI   PCBSTOPT,X'FF' .    IF NOT STOPPED                       07110000
         BNE   XDERR .             IT'S AN ERROR                        07120000
         SVC   C'!' .              ENTER SMC SECTION                    07130000
         DROP  2                                                        07140000
         USING PCB,15                                                   07150000
         SVC   C'J' .              ELSE UNTHREAD THE ENTRY              07160000
         LR    8,2 .               REMEMBER THE PCB POINTER             07170000
         LA    2,SATEMP .          READY TO CALL OUT AGAIN              07180000
         USING PCB,8                                                    07190000
         DROP  15                                                       07200000
         L     9,PCBFM .           GET FIRST MESSAGE                    07210000
XDLOOP   LTR   9,9 .               ANY MORE MESSAGES?                   07220000
         BZ    XDCHECK .           IF NOT, FINISH UP                    07230000
         USING MSG,9                                                    07240000
         L     10,MSGNEXT .        ELSE REMEMBER NEXT                   07250000
         L     11,MSGSIZE .        GET THE SIZE                         07260000
         LA    11,15(11) .          AND MAKE IT SOME NUMBER             07270000
         N     11,=F'-8' .          OF DOUBLEWORDS                      07280000
         USING XFX,2                                                    07290000
         ST    9,XFXADDR .         FREE THE LOCATION                    07300000
         ST    11,XFXSIZE .         THE NUMBER OF WORDS                 07310000
         SVC   C'F' .              DO IT                                07320000
         LR    9,10 .              ON TO THE NEXT                       07330000
         B     XDLOOP .            GET THE NEXT MESSAGE                 07340000
XDCHECK  CLC   PCBAADDR(4),=A(0) . HAS AUTOMATIC STORAGE BEEN           07350000
         BE    XDTHEN .             ALLOCATED? IF NOT, GO FINISH UP     07360000
         LA    2,PCBASIZE .        SET UP THE ARGUMENT LIST             07370000
         SVC   C'F' .              FREE IT                              07380000
         LA    2,SATEMP .          RESET REGISTER 2                     07390000
XDTHEN   ST    8,XFXADDR .         READY TO FREE THE PCB                07400000
         MVC   XFXSIZE,=A(LENPCB) . THE SIZE                            07410000
         SVC   C'F' .              FREE IT                              07420000
         SVC   C',' .              LEAVE SMC                            07430000
         LPSW  RETURN .            AND RETURN                           07440000
XDERR    SVC   C'?' .              IF PROCESS DOES NOT EXIST            07450000
         DROP  2,7,8,9                                                  07460000
         USING PCB,15                                                   07470000
         SPACE 3                                                        07480000
*********************************************************************** 07490000
*                                                                     * 07500000
*                            XH ROUTINE                               * 07510000
*                                                                     * 07520000
*        FUNCTION: TO HALT A JOB                                      * 07530000
*       DATABASES: NONE                                               * 07540000
*   ROUTINES USED: XS, XR                                             * 07550000
*       PROCEDURE: SEND MESSAGE TO SUPERVISOR PROCESS FOR THIS JOB    * 07560000
*                  INDICATING NORMAL TERMINATION; TRIES TO READ       * 07570000
*                  MESSAGES FOREVER LOOPING; BLOCKS ITSELF, THEREBY   * 07580000
*                  NEVER RETURNING.                                   * 07590000
*    ERROR CHECKS: NONE                                               * 07600000
*      INTERRUPTS: ON                                                 * 07610000
*     USER ACCESS: YES                                                * 07620000
*        COMMENTS: USER NORMALLY USES THIS ROUTINE TO END A JOB.      * 07630000
*                                                                     * 07640000
*********************************************************************** 07650000
         SPACE 1                                                        07660000
XH       EQU   * .                 THE XH ROUTINE: HALT A JOB           07670000
         USING *,1                                                      07680000
         LA    2,XHMSG1 .          SEND A MESSAGE TO *IBSUP             07690000
         SVC   C'S' .              SEND IT                              07700000
XHLOOP   LA    2,XHMSG2 .          READY TO READ A REPLY                07710000
         SVC   C'R' .               WHICH NEVER COMES                   07720000
         B     XHLOOP .            BUT IF IT DOES WERE READY            07730000
         DS    0F                                                       07740000
XHMSG1   DC    CL8'*IBSUP' .       SAY TO *IBSUP                        07750000
         DC    F'12' .             TWELVE CHARACTERS                    07760000
         DC    C'PROGRAM HALT' .   SAYING WERE OK                       07770000
XHMSG2   DS    CL8 .               WHO SENDS US A MESSAGE               07780000
         DC    F'1' .              ONE CHARACTER                        07790000
         DS    CL1,0H .            WHICH GOES HERE                      07800000
         EJECT                                                          07810000
*********************************************************************** 07820000
*                                                                     * 07830000
*                            XI ROUTINE                               * 07840000
*                                                                     * 07850000
*        FUNCTION: TO CHAIN A PCB ONTO PROCESS CHAINS                 * 07860000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS OF A PCB   * 07870000
*   ROUTINES USED: NONE                                               * 07880000
*       PROCEDURE: POINTER USED TO CHAIN PCB INTO ALL PCB CHAIN AND   * 07890000
*                  THIS GROUP CHAIN RIGHT AFTER RUNNING PCB; RETURN.  * 07900000
*    ERROR CHECKS: NONE                                               * 07910000
*      INTERRUPTS: OFF                                                * 07920000
*     USER ACCESS: NO                                                 * 07930000
*                                                                     * 07940000
*********************************************************************** 07950000
         SPACE 1                                                        07960000
XI       EQU   * .                 THE XI ROUTINE: THREAD IN A PCB      07970000
         USING *,1                                                      07980000
         L     10,PCBNPALL .       GET THE NEXT 'ALL' PCB               07990000
         ST    2,PCBNPALL .        STORE THIS PCB RIGNT AFTER MINE      08000000
         DROP  15                                                       08010000
         USING PCB,10                                                   08020000
         ST    2,PCBLPALL .        THE NEXT ONE DOWN POINTS BACK        08030000
         DROP  10                                                       08040000
         USING PCB,2                                                    08050000
         ST    15,PCBLPALL .       THIS PCB POINTS BACK                 08060000
         ST    10,PCBNPALL .        AND FORWARD                         08070000
         DROP  2                                                        08080000
         USING PCB,15                                                   08090000
         L     10,PCBNPTG .        GET NEXT "THIS GROUP" PCB            08100000
         ST    2,PCBNPTG .         RUNNING PCB POINTS TO NEW MEMBER     08110000
         DROP  15 .                 OF PROCESS GROUP                    08120000
         USING PCB,10                                                   08130000
         ST    2,PCBLPTG .         NEXT PCB DOWN POINTS BACK            08140000
         DROP  10                                                       08150000
         USING PCB,2                                                    08160000
         ST    15,PCBLPTG .        AND WE POINT BACKWARD                08170000
         ST    10,PCBNPTG .        AND FORWARD                          08180000
         DROP  2                                                        08190000
         LPSW  RETURN .            RETURN                               08200000
         USING PCB,15                                                   08210000
         EJECT                                                          08220000
*********************************************************************** 08230000
*                                                                     * 08240000
*                            XJ ROUTINE                               * 08250000
*                                                                     * 08260000
*        FUNCTION: TO UNCHAIN A PCB FROM PROCESS CHAINS               * 08270000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS OF A PCB   * 08280000
*   ROUTINES USED: NONE                                               * 08290000
*       PROCEDURE: POINTERS TO PCB IN ALL PCB CHAIN AND THIS GROUP    * 08300000
*                  CHAIN MODIFIED WITHOUT FREEING STORAGE; RETURN.    * 08310000
*    ERROR CHECKS: NONE                                               * 08320000
*      INTERRUPTS: OFF                                                * 08330000
*     USER ACCESS: NO                                                 * 08340000
*                                                                     * 08350000
*********************************************************************** 08360000
         SPACE 1                                                        08370000
XJ       EQU   * .                 THE XJ ROUTINE: UNTHREAD A PCB       08380000
         USING *,1                                                      08390000
         DROP  15                                                       08400000
         USING PCB,2                                                    08410000
         L     11,PCBLPALL .       GET PRECEDING PCB                    08420000
         L     10,PCBNPALL .        AND FOLLOWING ONE IN "ALL"          08430000
         DROP  2 .                  CHAIN                               08440000
         USING PCB,11                                                   08450000
         ST    10,PCBNPALL .       LAST POINTS TO NEXT                  08460000
         DROP  11                                                       08470000
         USING PCB,10                                                   08480000
         ST    11,PCBLPALL .       NEXT POINTS TO LAST                  08490000
         DROP  10                                                       08500000
         USING PCB,2                                                    08510000
         L     11,PCBLPTG .        REDO FOR THIS GROUP PCB CHAIN        08520000
         L     10,PCBNPTG                                               08530000
         DROP  2                                                        08540000
         USING PCB,11                                                   08550000
         ST    10,PCBNPTG .        LAST POINTS TO NEXT                  08560000
         DROP  11                                                       08570000
         USING PCB,10                                                   08580000
         ST    11,PCBLPTG .        NEXT POINTS TO LAST                  08590000
         DROP  10                                                       08600000
         LPSW  RETURN .            AND RETURN                           08610000
         USING PCB,15                                                   08620000
         EJECT                                                          08630000
*********************************************************************** 08640000
*                                                                     * 08650000
*                            XN ROUTINE                               * 08660000
*                                                                     * 08670000
*        FUNCTION: TO FIND THE PCB FOR A PROCESS GIVEN ITS NAME ONLY  * 08680000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XNX        * 08690000
*                    XNX       DS 0D                                  * 08700000
*                    XNXNAME   DS CL8   NAME OF PROCESS               * 08710000
*                    XNXADDR   DS A     ADDRESS OF PCB                * 08720000
*   ROUTINES USED: NONE                                               * 08730000
*       PROCEDURE: SEARCH THIS GROUP PCB CHAIN FOR NAME; IF FOUND,    * 08740000
*                  STORE POINTER IN XNXADDR. IF NOT FOUND, STORE      * 08750000
*                  ZERO IN XNXADDR; RETURN.                           * 08760000
*    ERROR CHECKS: NONE                                               * 08770000
*      INTERRUPTS: OFF                                                * 08780000
*     USER ACCESS: YES                                                * 08790000
*                                                                     * 08800000
*********************************************************************** 08810000
         SPACE 1                                                        08820000
XN       EQU   * .                 THE XN ROUTINE: FIND A NAMED PCB     08830000
         USING *,1                                                      08840000
         USING XNX,2 .             THE ARG LIST                         08850000
         LR    10,15 .             FIRST PCB TO LOOK AT IS OURS         08860000
         DROP  15                                                       08870000
         USING PCB,10                                                   08880000
XNXLOOP  L     10,PCBNPTG .        LOOK AT NEXT PCB                     08890000
         CLC   PCBNAME,XNXNAME .   HAS IT THE RIGHT NAME?               08900000
         BE    XNXFOUND .          IF YES, OH JOY.                      08910000
         CR    10,15 .             IF NOT, ARE WE THROUGH?              08920000
         BNE   XNXLOOP .           IF NOT, TRY THE NEXT PCB             08930000
         LA    10,0 .              ELSE, IT'S NOT HERE                  08940000
XNXFOUND ST    10,XNXADDR .        FOUND IT. SAY WHERE.                 08950000
         LPSW  RETURN .            AND RETURN                           08960000
         DROP  2,10                                                     08970000
         USING PCB,15                                                   08980000
         EJECT                                                          08990000
*********************************************************************** 09000000
*                                                                     * 09010000
*                            XR ROUTINE                               * 09020000
*                                                                     * 09030000
*        FUNCTION: TO READ A MESSAGE                                  * 09040000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XRX        * 09050000
*                    XRX       DS 0D                                  * 09060000
*                    XRXNAME   DS CL8   NAME OF SENDER PROCESS        * 09070000
*                    XRXSIZE   DS F     SIZE OF MESSAGE TEXT          * 09080000
*                    XRXTEXT   DS C     TEXT OF MESSAGE               * 09090000
*   ROUTINES USED: XP, XEXC, XN, XCOM, XF                             * 09100000
*       PROCEDURE: USE XP ON MESSAGE SEMAPHORE RECEIVER TO SEE IF ANY * 09110000
*                  MESSAGES WAITING; IF NONE, PROCESS BLOCKED UNTIL   * 09120000
*                  THERE IS ONE; LOCK MESSAGE CHAIN; REMOVE A MESSAGE * 09130000
*                  FROM CHAIN AND UNLOCK IT; MOVE TEXT OF MESSAGE,    * 09140000
*                  PADDING WITH BLANKS OR TRUNCATING AS NECESSARY;    * 09150000
*                  INDICATE CORRECT MESSAGE LENGTH AND NAME OF        * 09160000
*                  MESSAGE SENDER; FREE STORAGE USED TO HOLD MESSAGE, * 09170000
*                  AND RETURN.                                        * 09180000
*    ERROR CHECKS: NONE                                               * 09190000
*      INTERRUPTS: ON                                                 * 09200000
*     USER ACCESS: YES                                                * 09210000
*                                                                     * 09220000
*********************************************************************** 09230000
         SPACE 1                                                        09240000
XR       EQU   * .                 THE XR ROUTINE: READ A MESSAGE       09250000
         USING *,1                                                      09260000
         LR    7,2                                                      09270000
         USING XRX,7 .             ARG LIST                             09280000
         LA    2,PCBMSR .          SEE IF MESSAGES WAITING              09290000
         SVC   C'P'                                                     09300000
         SVC   C'!' .              ENTER SMC SECTION                    09310000
         LA    2,PCBMSC .          THEN LOCK THE MESSAGE CHAIN          09320000
         SVC   C'P'                                                     09330000
         L     5,PCBFM .           GET THE FIRST MESSAGE                09340000
         USING MSG,5                                                    09350000
         MVC   PCBFM,MSGNEXT .     REMEMBER THE NEXT                    09360000
         SVC   C'V' .              UNLOCK THE MESSAGE CHAIN             09370000
         L     6,XRXSIZE .         GET THE BUFFER CAPACITY              09380000
         S     6,=F'2' .           MINUS 1, MINUS 1                     09390000
         MVI   XRXTEXT,C' ' .      MOVE IN A BLANK                      09400000
         BM    XRNOB                                                    09410000
         EX    6,XRFILL .          THEN FILL THE REST WITH BLANKS       09420000
XRNOB    LA    6,1(6) .            THEN GET PROPER BUFFER COUNT         09430000
         C     6,MSGSIZE .         COMPARE WITH MESSAGE LENGTH          09440000
         BL    XRTHEN .            IF LESS, HANDLE ACCORDINGLY          09450000
         L     6,MSGSIZE .         ELSE COUNT FOR MVC IS MESSAGE        09460000
         BCTR  6,0 .                SIZE MINUS ONE                      09470000
XRTHEN   LTR   6,6 .               ANY CHARACTERS TO MOVE?              09480000
         BM    XRAFT .             IF NOT, DON'T                        09490000
         EX    6,XRMOVE .          ELSE MOVE THEM                       09500000
XRAFT    LA    6,1(6) .            THEN GET LENGTH                      09510000
         ST    6,XRXSIZE .         STORE IT                             09520000
         L     10,MSGSENDR .       GET SENDER'S PCB                     09530000
         DROP  15                                                       09540000
         USING PCB,10                                                   09550000
         MVC   XRXNAME,PCBNAME .   AND STORE SENDER'S NAME              09560000
         L     6,MSGSIZE .         GET SIZE OF MESSAGE TEXT             09570000
         LA    6,LENMSG(6) .        ADD SIZE OF MESSAGE BLOCK           09580000
         LA    6,7(6) .            AND TRUNCATE                         09590000
         N     6,=F'-8' .          UP                                   09600000
         LR    2,5 .               SET UP POINTER TO XFX                09610000
         USING XFX,2                                                    09620000
         ST    5,XFXADDR .         STORE ADDRESS                        09630000
         ST    6,XFXSIZE .         STORE SIZE                           09640000
         SVC   C'F' .              AND FREE THE MESSAGE BLOCK           09650000
         SVC   C',' .              LEAVE SMC                            09660000
         LPSW  RETURN .            AND RETURN                           09670000
XRFILL   MVC   XRXTEXT+1,XRXTEXT . FILL WITH BLANKS                     09680000
XRMOVE   MVC   XRXTEXT,MSGTEXT .   MOVE TEXT                            09690000
         DROP  2,5,7,10                                                 09700000
         USING PCB,15                                                   09710000
         SPACE 3                                                        09720000
*********************************************************************** 09730000
*                                                                     * 09740000
*                            XS ROUTINE                               * 09750000
*                                                                     * 09760000
*        FUNCTION: TO SEND A MESSAGE                                  * 09770000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XSX        * 09780000
*                    XSX       DS 0D                                  * 09790000
*                    XSXNAME   DS CL8   NAME OF TARGET PROCESS        * 09800000
*                    XSXSIZE   DS F     SIZE OF TEXT                  * 09810000
*                    XSXTEXT   DS C     TEXT OF MESSAGE               * 09820000
*   ROUTINES USED: XP, XV, XEXC, XCOM, XA, XQUE                       * 09830000
*       PROCEDURE: USE XN TO GET POINTER TO PCB OF TARGET PROCESS;    * 09840000
*                  USE LENGTH OF MESSAGE AND XA TO ALLOCATE BLOCK FOR * 09850000
*                  MESSAGE; LOCK MESSAGE CHAIN OF TARGET PROCESS;     * 09860000
*                  PUT MESSAGE BLOCK AT END OF CHAIN; STORE SENDER    * 09870000
*                  NAME, SIZE, AND TEXT OF MESSAGE; UNLOCK CHAIN;     * 09880000
*                  INDICATE MESSAGE CHAIN IS ONE LONGER; RETURN.      * 09890000
*    ERROR CHECKS: IF NO PROCESS BY GIVEN NAME, ENTER XQUE.           * 09900000
*      INTERRUPTS: ON                                                 * 09910000
*     USER ACCESS: YES                                                * 09920000
*                                                                     * 09930000
*********************************************************************** 09940000
         SPACE 1                                                        09950000
XS       EQU   * .                 THE XS ROUTINE: SEND MESSAGES        09960000
         USING *,1                                                      09970000
         LR    7,2                                                      09980000
         USING XSX,7 .             ARG LIST                             09990000
         LA    2,SATEMP .          READY TO CALL OUT                    10000000
         USING XNX,2 .             ABOUT TO CALL XN                     10010000
         MVC   XNXNAME,XSXNAME .   GIVE NAME OF TARGET PROCESS          10020000
         SVC   C'N' .              SEE WHERE IT IS                      10030000
         L     4,XNXADDR .         GET THE POINTER                      10040000
         LTR   4,4 .               IS THERE INDEED ONE?                 10050000
         BZ    XSERR .             IF NOT, ERROR                        10060000
         USING PCB,4                                                    10070000
         DROP  2,15                                                     10080000
         USING XAX,2 .             READY TO CALL XA                     10090000
         SVC   C'!' .              ENTERING SMC SECTION                 10100000
         L     3,XSXSIZE .         GET THE STATED SIZE                  10110000
         LA    3,LENMSG(3) .       PLUS THE AMOUNT OF OVERHEAD          10120000
         LA    3,7(3) .            AND TRUNCATE                         10130000
         N     3,=F'-8' .          UP                                   10140000
         ST    3,XAXSIZE .         THAT'S THE SIZE OF THE REGION TO     10150000
         MVC   XAXALGN,=F'8' .     ALLOCATE, ON A DOUBLEWORD BOUND      10160000
         SVC   C'A' .              SO ALLOCATE ALREADY                  10170000
         L     5,XAXADDR .         GET THE ADDRESS                      10180000
         DROP  2                                                        10190000
         LA    2,PCBMSC .          GET THE MESSAGE CHAIN SEMAPHORE      10200000
         SVC   C'P' .               AND LOCK IT                         10210000
         LA    8,PCBFM .           THEN START DOWN THE MESSAGE          10220000
         L     9,PCBFM .            CHAIN                               10230000
         USING MSG,9                                                    10240000
XSLOOP   LTR   9,9 .               ARE WE THROUGH?                      10250000
         BZ    XSADD .             IF SO ADD IT ON                      10260000
         LA    8,MSGNEXT .         IF NOT, ON TO THE NEXT               10270000
         L     9,MSGNEXT                                                10280000
         B     XSLOOP .            AND TRY AGAIN                        10290000
XSADD    ST    5,0(8) .            CHAIN OURS ON THE END                10300000
         DROP  9                                                        10310000
         USING MSG,5                                                    10320000
         MVC   MSGNEXT,=A(0) .     SET NEXT POINTER NULL                10330000
         ST    15,MSGSENDR .       STORE THE SENDER                     10340000
         L     6,XSXSIZE .         GET THE TEXT LENGTH                  10350000
         ST    6,MSGSIZE .         AND STORE IT                         10360000
         BCTR  6,0 .               ONE LESS                             10370000
         LTR   6,6 .               TEST LENGTH                          10380000
         BM    XSAFT .             IF ZERO, NOTHING TO MOVE             10390000
         EX    6,XSMOVE .          ELSE, MOVE IT                        10400000
XSAFT    SVC   C'V' .              UNLOCK THE MESSAGE CHAIN             10410000
         LA    2,PCBMSR .          THEN SAY THERE'S                     10420000
         SVC   C'V' .               ONE MORE MESSAGE                    10430000
         SVC   C',' .              LEAVE SMC SECTION                    10440000
         LPSW  RETURN .            AND RETURN                           10450000
XSERR    SVC   C'?'                                                     10460000
XSMOVE   MVC   MSGTEXT,XSXTEXT .   THE MOVE FOR THE TEXT                10470000
         DROP  4,5,7                                                    10480000
         USING PCB,15                                                   10490000
         EJECT                                                          10500000
*********************************************************************** 10510000
*                                                                     * 10520000
*                            XY ROUTINE                               * 10530000
*                                                                     * 10540000
*        FUNCTION: TO START A PROCESS                                 * 10550000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XYX        * 10560000
*                    XYX       DS 0D                                  * 10570000
*                    XYXNAME   DS CL8   NAME OF PROCESS TO BE STARTED * 10580000
*                    XYXADDR   DS A     STARTING ADDRESS OF PROCESS   * 10590000
*   ROUTINES USED: XN, XEXC, XCOM, XQUE                               * 10600000
*       PROCEDURE: USE XN TO GET POINTER TO THE PCB OF PROCESS TO BE  * 10610000
*                  STARTED; STORE IN PCB INTERRUPT SAVE AREA REGISTERS* 10620000
*                  AND PSW WITH STARTING ADDRESS AS SENT FROM STARTING* 10630000
*                  PROCESS; STOPPED BIT TURNED OFF; RETURN.           * 10640000
*    ERROR CHECKS: IF NO PROCESS BY GIVEN NAME, XQUE ENTERED.         * 10650000
*      INTERRUPTS: OFF                                                * 10660000
*     USER ACCESS: YES                                                * 10670000
*                                                                     * 10680000
*********************************************************************** 10690000
         SPACE 1                                                        10700000
XY       EQU   * .                 THE XY ROUTINE: START A PROCESS      10710000
         USING *,1                                                      10720000
         LR    7,2                                                      10730000
         USING XYX,7 .             THE ARG LIST                         10740000
         LA    2,SATEMP .          READY TO CALL OUT                    10750000
         USING XNX,2                                                    10760000
         MVC   XNXNAME,XYXNAME .   GIVE XN A NAME                       10770000
         SVC   C'N' .              CALL XN                              10780000
         L     10,XNXADDR .        WHERE IS THE PCB?                    10790000
         LTR   10,10 .              OR IS THERE ONE?                    10800000
         BZ    XYERR .             IF NOT, OH HISS BOO                  10810000
         DROP  2,14,15                                                  10820000
         USING PCB,10                                                   10830000
         LA    13,PCBISA .         GET INTO THAT PCB'S ISA              10840000
         USING SA,13                                                    10850000
         MVC   SAPSW,(SAPSW-SA)(14) . GIVE IT THE CALLER'S PSW          10860000
         MVC   SAPSW+5(3),XYXADDR+1 . BUT AT THE REQUESTED ADDRESS      10870000
         MVC   SAREGS,(SAREGS-SA)(14) .GIVE IT HIS REGISTERS            10880000
         MVI   PCBSTOPT,X'00' .    IT'S NO LONGER STOPPED               10890000
         LPSW  RETURN .            AND RETURN                           10900000
XYERR    SVC   C'?' .              WE DONE BAD                          10910000
         DROP  7,10,13                                                  10920000
         USING SA,14                                                    10930000
         USING PCB,15                                                   10940000
         EJECT                                                          10950000
*********************************************************************** 10960000
*                                                                     * 10970000
*                            XZ ROUTINE                               * 10980000
*                                                                     * 10990000
*        FUNCTION: TO STOP A PROCESS                                  * 11000000
*       DATABASES: UPON ENTRY, REGISTER 2 CONTAINS ADDRESS XZX        * 11010000
*                    XZX       DS 0D                                  * 11020000
*                    XZXNAME   DS CL8   NAME OF PROCESS TO BE STOPPED * 11030000
*   ROUTINES USED: XN, XEXC, XCOM, XQUE, XP                           * 11040000
*       PROCEDURE: CHECK THAT USER PROCESS CAN'T STOP SYSTEM          * 11050000
*                  PROCESS; USE XN TO GET PCB POINTER; IF IN SMC, SET * 11060000
*                  STOP WAITING BIT AND BLOCK SELF UNTIL STOP         * 11070000
*                  PERFORMED; ELSE SET STOPPED BIT, AND RETURN.       * 11080000
*    ERROR CHECKS: IF NO PROCESS BY GIVEN NAME OR USER TRIES TO       * 11090000
*                  STOP A SYSTEM PROCESS, XQUE ENTERED.               * 11100000
*      INTERRUPTS: ON                                                 * 11110000
*     USER ACCESS: YES                                                * 11120000
*                                                                     * 11130000
*********************************************************************** 11140000
         SPACE 1                                                        11150000
XZ       EQU   * .                 THE XZ ROUTINE: STOP A PROCESS       11160000
         USING *,1                                                      11170000
         LR    7,2                                                      11180000
         USING XZX,7 .             ARG LIST                             11190000
         CLI   PCBNAME,C'*' .      IS STOPPER A * PROCESS               11200000
         BE    XZFINE .            THAT'S OK                            11210000
         CLI   XZXNAME,C'*' .       IF NOT, IS STOPPEE A * ?            11220000
         BE    XZERR .             CAN'T DO THAT                        11230000
XZFINE   LA    2,SATEMP .          READY TO CALL OUT                    11240000
         USING XNX,2 .             WILL CALL XN                         11250000
         MVC   XNXNAME,XZXNAME .   GIVE IT THE NAME                     11260000
         SVC   C'N' .              AND DO THE CALL                      11270000
         L     10,XNXADDR .        GET THE PCB'S ADDRESS                11280000
         LTR   10,10 .             SEE IF NULL                          11290000
         BZ    XZERR .             IF SO, ERROR                         11300000
         SVC   C'!' .              ENTER SMC                            11310000
         DROP  2,15                                                     11320000
         USING PCB,10                                                   11330000
XZSTOP   CLI   PCBINSMC,X'00' .    SEE IF IN SMC                        11340000
         BNE   XZINSMC .           IF SO, BAD                           11350000
         MVI   PCBSTOPT,X'FF' .    ELSE JUST STOP IT                    11360000
         SVC   C',' .              LEAVE SMC                            11370000
         LPSW  RETURN .            AND RETURN                           11380000
XZINSMC  MVI   PCBSW,X'FF' .       IF IN SMC, SAY STOP WAITING          11390000
         LA    2,PCBSRS .          AND STOP OURSELVES AGAINST           11400000
         SVC   C'P' .               A SEMAPHORE                         11410000
         B     XZSTOP .            THEN WE CAN REALLY STOP IT           11420000
XZERR    SVC   C'?' .              AN ERROR                             11430000
         DROP  10,7                                                     11440000
         USING PCB,15                                                   11450000
         EJECT                                                          11460000
*********************************************************************** 11470000
*                                                                     * 11480000
*                            XQUE ROUTINE                             * 11490000
*                                                                     * 11500000
*        FUNCTION: TO SIGNAL ERROR CONDITION                          * 11510000
*       DATABASES: NONE                                               * 11520000
*   ROUTINES USED: XR, XS                                             * 11530000
*       PROCEDURE: SEND MESSAGE TO SUPERVISOR PROCESS FOR THIS JOB    * 11540000
*                  INDICATING ABNORMAL TERMINATION; TRY TO READ       * 11550000
*                  MESSAGES, FOREVER LOOPING; BLOCK ITSELF, THEREBY   * 11560000
*                  NEVER RETURNING.                                   * 11570000
*    ERROR CHECKS: NONE                                               * 11580000
*      INTERRUPTS: OFF                                                * 11590000
*     USER ACCESS: YES                                                * 11600000
*                                                                     * 11610000
*********************************************************************** 11620000
         SPACE 1                                                        11630000
XQUE     EQU   * .                 THE XQUE ROUTINE: ERROR!             11640000
         USING *,1                                                      11650000
         LA    2,XQUEM1 .          SEND AN ERROR MESSAGE TO *IBSUP      11660000
         SVC   C'S'                                                     11670000
XQUELOOP LA    2,XQUEM2 .          WAIT FOR REPLY                       11680000
         SVC   C'R'                                                     11690000
         B     XQUELOOP .          BUT IGNORE IT                        11700000
         DS    0F                                                       11710000
XQUEM1   DC    CL8'*IBSUP'                                              11720000
         DC    F'12'                                                    11730000
         DC    CL12'PROGRAM FLOP'                                       11740000
XQUEM2   DS    CL8                                                      11750000
         DC    F'1'                                                     11760000
         DS    CL1,0H                                                   11770000
         DROP  14,15                                                    11780000
         EJECT                                                          11790000
*********************************************************************** 11800000
*                                                                     * 11810000
*                           INPUT/OUTPUT ROUTINES                     * 11820000
*                                                                     * 11830000
*********************************************************************** 11840000
         SPACE 1                                                        11850000
*********************************************************************** 11860000
*                                                                     * 11870000
*             SYSTEM SUPPLIED DEVICE HANDLER FOR READERS              * 11880000
*                                                                     * 11890000
*********************************************************************** 11900000
         SPACE 1                                                        11910000
RDRHANDL EQU   * .                 THE READER HANDLER                   11920000
         USING UCB,3 .             STARTED WITH REG3 -> UCB             11930000
         BALR  1,0                                                      11940000
         USING *,1 .               ESTABLISH ADDRESSING                 11950000
         LA    2,RDRHSEM .         LOCK OURSELVES UNTIL WE SET UP       11960000
         SVC   C'P' .               AN AUTOMATIC STORAGE AREA           11970000
         LA    2,RDRHAAS .         READY TO ALLOCATE                    11980000
         USING XAX,2                                                    11990000
         SVC   C'E' .              ALLOCATE                             12000000
         L     12,XAXADDR .        GET A PTR                            12010000
         DROP  2                                                        12020000
         LA    2,RDRHSEM .         AND UNBLOCK OURSELVES                12030000
         SVC   C'V'                                                     12040000
         SRL   4,16 .              SHIFT KEY                            12050000
         SR    10,10 .             CLEAR REG 10                         12060000
         USING RDRHAS,12 .         AUTOMATIC AREA                       12070000
         MVI   JOBBIT,X'00' .      INITIALIZE                           12080000
         LA    6,RDRHCCB .         GET PTR TO CCB                       12090000
RDRHLOOP LA    2,RDRHMSG .         TRY TO READ A MESSAGE                12100000
         USING XRX,2                                                    12110000
         MVC   XRXSIZE,=F'8' .     WE CAN TAKE 8 CHARS                  12120000
         SVC   C'R' .              READ IT                              12130000
         CLC   =C'READ',XRXTEXT .  IF FIRST WORD IS READ, OK            12140000
         BNE   RDRHLOOP .          ELSE IGNORE                          12150000
         L     5,XRXTEXT+4 .       GET 2ND WORD OF TEXT                 12160000
         DROP  2                                                        12170000
         LA    2,UCBUS .           LOCK THE UCB AND IT'S UNIT           12180000
         SVC   C'P'                                                     12190000
         LA    2,RDRHMSG .         RESET ADDRESSING POINTER             12200000
         USING XRX,2                                                    12210000
         CLI   JOBBIT,X'FF' .      HAVE WE JUST READ $JOB CARD?         12220000
         BNE   RDRHMORE .          IF NO, GO CHECK PROTECTION, ELSE     12230000
         CLI   XRXNAME,C'*' .      IS JSP CALLING US?                   12240000
         BNE   RDRHNO .            IF NOT, TELL HIM NO.                 12250000
         MVC   0(80,5),RDRHTEMP .  IF IT IS, GIVE JSP THE $JOB CARD     12260000
         MVI   JOBBIT,X'00' .      SAY WE DON'T HAVE $JOB WAITING       12270000
         B     RDRHSOK .            AND SEND MESSAGE BACK               12280000
         DROP  2                                                        12290000
RDRHMORE CLI   RDRHMSG,C'*' .      IS SYSTEM CALLING?                   12300000
         BE    RDRHPOK .           THEN PROTECTION OK, ELSE             12310000
         LR    11,5 .              GET ADDRESS THAT'S TO HOLD CARD,     12320000
         N     11,PROTCON1 .       get the page boundary                12330002
*        ISKE  10,11 .             find storage key                     12334002
         DC    X'B22900AB'         Assembler (XF) doesn't support ISKE  12338002
         N     10,PROTCON2 .       ignore low order bits                12342002
         CR    10,4 .              DOES IT MATCH OURS?                  12350000
         BNE   RDRHNO .            IF NOT, TELL HIM NO                  12360000
         LA    11,79(5) .          CHECK LAST BYTE ADDR OF CARD         12370000
         N     11,PROTCON1 .       get the page boundary                12380002
*        ISKE  10,11 .             find storage key                     12384002
         DC    X'B22900AB'         Assembler (XF) doesn't support ISKE  12388002
         N     10,PROTCON2 .       ignore low order bits                12392002
         CR    10,4 .              DOES IT MATCH OURS?                  12400000
         BNE   RDRHNO .            IF NOT, TELL HIM NO                  12410000
RDRHPOK  N     5,CCBCON1 .         MAKE ADDRESS INTO                    12420000
         ST    5,RDRHCCB .         A CCW (OR CCB)                       12430000
         OI    RDRHCCB,X'02'                                            12440000
         MVC   RDRHCCB+4,=F'80' .  WE'LL READ EIGHTY CHARACTERS         12450000
         MVC   UCBCSW(4),=A(0) .   CLEAR THE LAST CSW THERE             12460000
         MVC   UCBCSW+4(4),=A(0)                                        12470000
         LA    2,CAWSEM .          LOCK THE CAW                         12480000
         SVC   C'P'                                                     12490000
         ST    6,CAW .             THAT'S THE CAW                       12500000
         L     7,UCBADDR .         GET THE UNIT ADDRESS                 12510000
         SIO   0(7) .              START THE I/O                        12520000
         BNZ   RDSTATUS .          BRANCH IF SIO UNSUCCESSFUL           12530000
         SVC   C'V' .              THEN UNLOCK THE CAW                  12540000
RDRHWAIT LA    2,UCBWS .           NOW WAIT FOR AN INTERRUPT            12550000
         SVC   C'P'                                                     12560000
         TM    UCBCSW+4,X'85' .    CHECK THE STATUS                     12570003
         BZ    RDRHWAIT .          IF NOT FINISHED, WAIT                12580000
         TM    UCBCSW+4,X'01' .    CHECK FOR EXCEPTION                  12590000
         BO    RDRHEXC .           if yes, ignore this interrupt        12600003
         TM    UCBCSW+4,X'80' .    if no, check for attention           12602003
         BO    RDRHPOK .           if yes, try to restart the I/O       12604003
         B     RDRHOK .            else, all is groovy                  12606003
RDRHEXC  NI    UCBCSW+4,X'FE' .    clear exception ..                   12608003
         B     RDRHWAIT .           .. and continue waiting             12610003
RDRHNO   MVC   RDRHM+12(2),=C'NO' . message back is no                  12612003
         B     RDRHSEND .          GET READY TO SEND                    12620000
RDRHOK   CLI   RDRHMSG,C'*' .      IS THE SYSTEM CALLING?               12630000
         BE    RDRHSOK .           THAT'S FINE. OTHERWISE,              12640000
         CLC   =C'$JOB,',0(5) .    WAS IT A $JOB CARD?                  12650000
         BE    ENDADATA .          OOPS! WE HIT END OF DATA STREAM      12660000
RDRHSOK  MVC   RDRHM+12(2),=C'OK' .GROOVINESS MESSAGE                   12670000
RDRHSEND MVC   RDRHM+8(4),=F'2' .  SAY THERE ARE 2 CHARACTERS           12680000
         MVC   RDRHM+0(8),RDRHMSG+0 . SEND BACK TO SAME GUY             12690000
         LA    2,UCBUS .           NOW UNLOCK UCB AND UNIT              12700000
         SVC   C'V'                                                     12710000
         LA    2,RDRHM .           SET UP MESSAGE                       12720000
         SVC   C'S' .               AND SEND IT                         12730000
         B     RDRHLOOP                                                 12740000
ENDADATA MVC   RDRHM+12(2),=C'NO' . TELL USER NO MORE CARDS             12750000
         MVC   RDRHTEMP(80),0(5) . SAVE THE $JOB CARD                   12760000
         MVI   0(5),C' ' .         BLANK OUT THE USER'S COPY            12770000
         MVC   1(79,5),0(5)                                             12780000
         MVI   JOBBIT,X'FF' .      INDICATE WE HAVE A NEW $JOB CARD     12790000
         B     RDRHSEND .          AND SEND THE MESSAGE BACK            12800000
RDSTATUS SVC   C'V' .              UNLOCK THE CAW                       12810000
         LA    2,UCBWS .           AND WAIT FOR AN INTERRUPT            12820000
         SVC   C'P'                                                     12830000
         B     RDRHPOK .           AND TRY TO RESTART THE I/O           12840000
         DROP  3,12                                                     12850000
         SPACE 1                                                        12860000
RDRHSEM  DC    F'1,0'                                                   12870000
CCBCON1  DC    X'00FFFFFF' MASK                                         12880000
PROTCON1 DC    X'00FFF000'         page alignment                       12890002
PROTCON2 DC    X'FFFFFFF0'         ignore low order bits                12893002
RDRHAAS  DC    A(LENRDRHA) ALLOCATE ARGLIST FOR STORAGE                 12900000
         DC    F'0'                                                     12910000
         DC    F'8'                                                     12920000
         SPACE 3                                                        12930000
*********************************************************************** 12940000
*                                                                     * 12950000
*             SYSTEM SUPPLIED DEVICE HANDLER FOR PRINTERS             * 12960000
*                                                                     * 12970000
*********************************************************************** 12980000
         SPACE 1                                                        12990000
PRTHANDL EQU   * .                 THE PRINTER HANDLER                  13000000
         USING UCB,3 .             ENTERED WITH REG3 -> THE UCB         13010000
         BALR  1,0                                                      13020000
         USING *,1 .               ESTABLISH ADDRESSING                 13030000
         LA    2,PRTHSEM .         LOCK UNTIL ALLOCATE STORAGE          13040000
         SVC   C'P' .                                                   13050000
         LA    2,PRTHAAS .         READY TO ALLOCATE                    13060000
         USING XAX,2                                                    13070000
         SVC   C'E' .              ALLOCATE                             13080000
         L     12,XAXADDR .        GET THE ADDRESS                      13090000
         DROP  2                                                        13100000
         LA    2,PRTHSEM .                                              13110000
         SVC   C'V'                UNLOCK TO ROUTINE                    13120000
         SRL   4,16 .              SHIFT KEY                            13130000
         SR    10,10 .             CLEAR REG 10                         13140000
         USING PRTHAS,12 .         ADDRESSING IN THE AUTO AREA          13150000
         LA    6,PRTHCCB .         MAKE A CAW                           13160000
PRTHLOOP LA    2,PRTHMSG .         READY TO READ A MESSAGE              13170000
         USING XRX,2                                                    13180000
         MVC   XRXSIZE,=F'8' .     WE CAN TAKE 8 CHARACTERS             13190000
         SVC   C'R' .              READ IT                              13200000
         L     5,XRXTEXT+4 .       LOAD THE ADDRESS                     13210000
         CLC   =C'PRIN',XRXTEXT .  IS IT A PRIN REQUEST?                13220000
         BE    PRTHPRIN                                                 13230000
         CLC   =C'STC1',XRXTEXT .  OR A SKIP REQUEST?                   13240000
         BE    PRTHSTC1                                                 13250000
         B     PRTHLOOP .          IF NEITHER, IGNORE                   13260000
         DROP  2                                                        13270000
PRTHPRIN LA    2,UCBUS                                                  13280000
         SVC   C'P' .              LOCK THE UCB AND UNIT                13290000
         CLI   PRTHMSG,C'*' .      IS SYSTEM CALLING?                   13300000
         BE    PRTHPOK .           THEN PROTECTION OK. ELSE             13310000
         LR    11,5 .              GET ADDRESS THAT'S TO HOLD MSG,      13320000
         N     11,PROTCON1 .       get the page boundary                13330002
*        ISKE  10,11 .             find storage key                     13334002
         DC    X'B22900AB'         Assembler (XF) doesn't support ISKE  13338002
         N     10,PROTCON2 .       ignore low order bits                13342002
         CR    10,4 .              DOES IT MATCH OURS?                  13350000
         BNE   PRTHNO .            IF NOT, TELL HIM NO                  13360000
         LA    11,131(5) .         CHECK LAST BYTE ADDRESS OF LINE      13370000
         N     11,PROTCON1 .       get the page boundary                13380002
*        ISKE  10,11 .             find storage key                     13384002
         DC    X'B22900AB'         Assembler (XF) doesn't support ISKE  13388002
         N     10,PROTCON2 .       ignore low order bits                13392002
         CR    10,4 .              DOES IT MATCH OURS?                  13400000
         BNE   PRTHNO .            IF NOT, TELL HIM NO                  13410000
PRTHPOK  N     5,CCBCON1 .         MAKE A WRITE REQUEST                 13420000
         ST    5,PRTHCCB .         FOR THE CCB                          13430000
         OI    PRTHCCB,X'09' .     PRINT COMMAND CODE                   13440000
         MVC   PRTHCCB+4,=F'132' . WE'LL PRINT 132 CHARACTERS           13450000
         B     PRTHCOMM .          BRANCH TO COMMON SECTION             13460000
PRTHSTC1 MVC   PRTHCCB(8),=X'8900000020000001' SKIP TO TOP OF PAGE      13470000
         LA    2,UCBUS                                                  13480000
         SVC   C'P' .              LOCK THE UCB AND UNIT                13490000
PRTHCOMM LA    2,CAWSEM .          LOCK THE CAW                         13500000
         SVC   C'P'                                                     13510000
         ST    6,CAW .             STORE OUR CAW                        13520000
         MVC   UCBCSW(4),=A(0) .   CLEAR THE LAST CSW THERE             13530000
         MVC   UCBCSW+4(4),=A(0)                                        13540000
         L     7,UCBADDR .         GET THE ADDRESS                      13550000
         SIO   0(7) .              START THE I/O                        13560000
         BNZ   PTSTATUS .          BRANCH IF SIO UNSUCCESSFUL           13570000
         SVC   C'V' .              AND UNLOCK THE CAW                   13580000
PRTHWAIT LA    2,UCBWS .           START TO WAIT                        13590000
         SVC   C'P'                                                     13600000
         TM    UCBCSW+4,X'05' .    IS THE UNIT READY?                   13610000
         BZ    PRTHWAIT .          IF NOT, ITS STILL ON. WAIT           13620000
         TM    UCBCSW+4,X'01' .    WAS THERE AN EXCEPTION?              13630000
         BZ    PRTHOK .            IF NOT, GOOD                         13640000
PRTHNO   MVC   PRTHM+12(2),=C'NO' .THERE WAS, SO SAY SO                 13650000
         B     PRTHSEND                                                 13660000
PRTHOK   MVC   PRTHM+12(2),=C'OK' .NO ERRORS                            13670000
PRTHSEND MVC   PRTHM+8(4),=F'2' .  SENDING 2 CHARACTERS                 13680000
         MVC   PRTHM+0(8),PRTHMSG+0 . SEND TO OUR SENDER                13690000
         LA    2,UCBUS                                                  13700000
         SVC   C'V' .              UNLOCK THE UCB                       13710000
         LA    2,PRTHM                                                  13720000
         SVC   C'S' .              SEND IT                              13730000
         B     PRTHLOOP .          AND READ ANOTHER MESSAGE             13740000
PTSTATUS SVC   C'V' .              UNLOCK THE CAW                       13750000
         LA    2,UCBWS .           AND WAIT FOR THE INTERRUPT           13760000
         SVC   C'P'                                                     13770000
         B     PRTHCOMM .          AND TRY TO RESTART THE I/O           13780000
         DROP  3,12                                                     13790000
         SPACE 2                                                        13800000
PRTHSEM  DC    F'1,0' LOCK                                              13810000
PRTHAAS  DC    A(LENPRTHA) XA ARG LIST FOR AUTO STORAGE                 13820000
         DC    F'0'                                                     13830000
         DC    F'8'                                                     13840000
         EJECT                                                          13850000
*********************************************************************** 13860000
*                                                                     * 13870000
*             SYSTEM ROUTINE FOR USER SUPPLIED DEVICE HANDLER         * 13880000
*                                                                     * 13890000
*********************************************************************** 13900000
         SPACE 1                                                        13910000
EXCPHNDL EQU   * .                 EXCP DEVICE HANDLER                  13920000
         USING UCB,3 .             WILL HAVE REG3 -> UCB                13930000
         BALR  1,0                                                      13940000
         USING *,1 .               ESTABLISH ADDRESSING                 13950000
         LA    2,EXCPHSEM .        LOCK OURSELVES UNTIL WE HAVE         13960000
         SVC   C'P' .              SET UP AUTOMATIC STORAGE             13970000
         LA    2,EXCPHAAS .        READY TO ALLOCATE                    13980000
         USING XAX,2                                                    13990000
         SVC   C'E' .              ALLOCATE                             14000000
         L     12,XAXADDR .        GET POINTER TO AUTO STORAGE          14010000
         DROP  2                                                        14020000
         LA    2,EXCPHSEM .        AND UNLOCK OURSELVES                 14030000
         SVC   C'V'                UNLOCK TO ROUTINE                    14040000
         LR    4,11                                                     14050000
         SLL   4,8 .               SHIFT KEY FOR CAW                    14060000
         USING EXCPHAS,12 .        FOR ADDRESSING AUTO AREA             14070000
EXCPLOOP LA    2,EXCPHMSG .        TRY TO READ A MESSAGE                14080000
         USING XRX,2                                                    14090000
         MVC   XRXSIZE,=F'12' .    WE'LL TAKE 12 CHARACTERS             14100000
         SVC   C'R'                                                     14110000
         CLC   =C'EXCP',XRXTEXT .  IS IT AN EXCP MESSAGE?               14120000
         BNE   EXCPLOOP .          IF NOT, IGNORE IT                    14130000
         L     5,XRXTEXT+4 .       REG 5 CONTAINS CHAN AND DEV          14140000
         L     6,XRXTEXT+8 .       REG 6 CONTAINS ADDR OF CCWS          14150000
         DROP  2                                                        14160000
         LA    7,UCBTABLE .        GET PTR TO UCB TABLE                 14170000
EXCPCOMP C     5,0(7) .            COMPARE UNIT ADDRESS                 14180000
         BE    EXCPFIND .          THAT'S THE UCB WE WANT               14190000
         LA    7,UCBLENG(7) .      GET PTR TO NEXT UCB                  14200000
         C     7,=A(UCBTBEND) .    ARE WE THROUGH WITH TABLE?           14210000
         BNE   EXCPCOMP .          IF NOT, LOOK SOME MORE               14220000
         SVC   C'?' .              ELSE ERROR                           14230000
EXCPFIND LR    3,7 .               SET REG 3 TO UCB PTR                 14240000
         LA    2,UCBUS                                                  14250000
         SVC   C'P' .              LOCK THE UCB                         14260000
         OR    6,4 .               OR IN THE USER'S KEY                 14270000
         MVC   UCBCSW(4),=A(0) .   CLEAR THE LAST CSW THERE             14280000
         MVC   UCBCSW+4(4),=A(0)                                        14290000
         LA    2,CAWSEM                                                 14300000
         SVC   C'P' .              LOCK CAW                             14310000
         ST    6,CAW .             STORE OUR CAW                        14320000
         SIO   0(5) .              START THE I/O                        14330000
         SVC   C'V' .              UNLOCK THE CAW                       14340000
EXCPWAIT LA    2,UCBWS .           NOW WAIT FOR AN INTERRUPT            14350000
         SVC   C'P'                                                     14360000
         MVC   EXCPHM+12(8),UCBCSW . GIVE USER HIS CSW                  14370000
         MVC   EXCPHM+8(4),=F'12'                                       14380000
         MVC   EXCPHM(8),EXCPHMSG                                       14390000
         LA    2,EXCPHM                                                 14400000
         SVC   C'S' .              AND SENT THE MESSAGE                 14410000
         LA    2,EXCPHMSG .        AND WAIT FOR A REPLY                 14420000
         USING XRX,2                                                    14430000
         MVC   XRXSIZE(4),=F'8' .  FROM THE USER                        14440000
         SVC   C'R'                                                     14450000
         CLC   =C'OK',XRXTEXT .    AM I DONE?                           14460000
         BE    EXCPDONE                                                 14470000
         CLC   =C'AGAIN',XRXTEXT . DOES HE WANT ANOTHER CSW?            14480000
         BE    EXCPWAIT                                                 14490000
         SVC   C'?' .              WRONG MESSAGE                        14500000
         DROP  2                                                        14510000
EXCPDONE LA    2,UCBUS .           UNLOCK UNIT                          14520000
         SVC   C'V'                                                     14530000
         B     EXCPLOOP .          AND GET ANOTHER MESSAGE              14540000
         DROP  3,12                                                     14550000
EXCPHSEM DC    F'1,0'                                                   14560000
EXCPHAAS DC    A(LENEXCPA) .       ALLOCATION OF AUTO STORAGE           14570000
         DC    F'0'                                                     14580000
         DC    F'8'                                                     14590000
         SPACE 3                                                        14600000
         LTORG                                                          14610000
         EJECT                                                          14620000
*********************************************************************** 14630000
*                                                                     * 14640000
*             UNIT CONTROL BLOCKS                                     * 14650000
*                                                                     * 14660000
*********************************************************************** 14670000
         SPACE 1                                                        14680000
UCBTABLE DS    0F .                TABLE OF UNIT CONTROL BLOCKS         14690000
*                          UCB FOR READER 1                             14700000
UCBRDR1  DC    X'00000012' .       DEVICE ADDRESS,                      14710000
         DC    F'1,0' .            USER SEMAPHORE,                      14720000
         DC    F'0,0' .            WAIT SEMAPHORE,                      14730000
         DC    F'0,0' .            CHANNEL STATUS WORD                  14740000
         DC    X'00'                                                    14750000
         DS    0F                                                       14760000
*                          UCB FOR PRINTER 1                            14770000
UCBPRT1  DC    X'00000010' .       DEVICE ADDRESS,                      14780000
         DC    F'1,0' .            USER SEMAPHORE,                      14790000
         DC    F'0,0' .            WAIT SEMAPHORE,                      14800000
         DC    F'0,0' .            CHANNEL STATUS WORD                  14810000
         DC    X'00'                                                    14820000
         DS    0F                                                       14830000
*                          UCB FOR READER 2                             14840000
UCBRDR2  DC    X'0000000C' .       DEVICE ADDRESS,                      14850000
         DC    F'1,0' .            USER SEMAPHORE,                      14860000
         DC    F'0,0' .            WAIT SEMAPHORE,                      14870000
         DC    F'0,0' .            CHANNEL STATUS WORD                  14880000
         DC    X'00'                                                    14890000
         DS    0F                                                       14900000
*                          UCB FOR PRINTER 2                            14910000
UCBPRT2  DC    X'0000000E' .       DEVICE ADDRESS,                      14920000
         DC    F'1,0' .            USER SEMAPHORE,                      14930000
         DC    F'0,0' .            WAIT SEMAPHORE,                      14940000
         DC    F'0,0' .            CHANNEL STATUS WORD                  14950000
         DC    X'00'                                                    14960000
         DS    0F                                                       14970000
*                          UCB for READER 3                             14970302
UCBRDR3  DC    X'00000112' .       device address,                      14970602
         DC    F'1,0' .            user semaphore,                      14970902
         DC    F'0,0' .            wait semaphore,                      14971202
         DC    F'0,0' .            channel status word                  14971502
         DC    X'00'                                                    14971802
         DS    0F                                                       14972102
*                          UCB for PRINTER 3                            14972402
UCBPRT3  DC    X'00000110' .       device address,                      14972702
         DC    F'1,0' .            user semaphore,                      14973002
         DC    F'0,0' .            wait semaphore,                      14973302
         DC    F'0,0' .            channel status word                  14973602
         DC    X'00'                                                    14973902
         DS    0F                                                       14974202
*                          UCB for READER 4                             14974502
UCBRDR4  DC    X'0000010C' .       device address,                      14974802
         DC    F'1,0' .            user semaphore,                      14975102
         DC    F'0,0' .            wait semaphore,                      14975402
         DC    F'0,0' .            channel status word                  14975702
         DC    X'00'                                                    14976002
         DS    0F                                                       14976302
*                          UCB for PRINTER 4                            14976602
UCBPRT4  DC    X'0000010E' .       device address,                      14976902
         DC    F'1,0' .            user semaphore,                      14977202
         DC    F'0,0' .            wait semaphore,                      14977502
         DC    F'0,0' .            channel status word                  14977802
         DC    X'00'                                                    14978102
         DS    0F                                                       14978402
*                          UCB for CONSOLE 1                            14978504
UCBCONS1 DC    X'00000009' .       device address,                      14978604
         DC    F'1,0' .            user semaphore,                      14978704
         DC    F'0,0' .            wait semaphore,                      14978804
         DC    F'0,0' .            channel status word                  14978904
         DC    X'00'                                                    14979004
         DS    0F                                                       14979104
UCBTBEND EQU   *                                                        14980000
         EJECT                                                          14990000
*********************************************************************** 15000000
*                                                                     * 15010000
*             I/O INTERRUPT HANDLER                                   * 15020000
*                                                                     * 15030000
*********************************************************************** 15040000
         SPACE 1                                                        15050000
IOHANDL  EQU   * .                 THE I/O INTERRUPT HANDLER            15060000
         STM   0,15,IOHSAVE .      SAVE REGISTERS                       15070000
         BALR  1,0                                                      15080000
         USING *,1 .               ESTABLISH ADRESSING                  15090000
         NI    IOOLD+1,X'FD' .     TURN OFF WAIT BIT                    15100000
         L     6,=A(UCBTABLE) .    GET POINTER TO UCB TABLE             15110000
IOCOMP   CLC   2(2,6),IOOLD+2 .    COMPARE DEVICE AND CHANNEL           15120000
         BE    IODEVFND .          IF EQUAL, REG 6 INDICATES PTR        15130000
         LA    6,UCBLENG(6) .      INCREMENT TO NEXT ENTRY              15140000
         C     6,=A(UCBTBEND) .    ARE WE AT END OF TABLE?              15150000
         BNE   IOCOMP .            IF NOT DONE, TRY NEXT UCB            15160000
         B     IOBACK .            ELSE, IGNORE IT                      15170000
         USING UCB,6 .             IT'S A UCB PTR                       15180000
IODEVFND MVC   UCBCSW(4),CSW .     MOVE IN THE NEW CSW                  15190000
         L     7,CSW+4 .           GET STATUS BYTE                      15200000
         O     7,UCBCSW+4 .        OR IN NEW STATUS INFORMATION         15210000
         ST    7,UCBCSW+4 .         AND STORE IT BACK                   15220000
         MVC   UCBCSW+6(2),CSW+6 . MOVE IN BYTE COUNT                   15230000
         LA    2,UCBWS                                                  15240000
         CLI   UCBFPR,X'00' .      IS FAST PROCESSING                   15250000
         BE    IONOFPR .            REQUIRED? IF NOT, RETURN            15260000
         L     15,RUNNING .        IF SO, STOP GUY NOW RUNNING          15270000
         USING PCB,15                                                   15280000
         CLI   PCBBLOKT,X'FF' .    IS ANYONE REALLY RUNNING?            15290000
         BE    IOWAIT .            IF NOT, START UP SLEEPER             15300000
         LA    13,PCBISA .         IF SO, STOP RUNNING PROCESS          15310000
         USING SA,13                                                    15320000
         MVC   SAPSW,IOOLD .       SAVE PROCESS WHICH WAS               15330000
         MVC   SAREGS,IOHSAVE .     INTERRUPTED                         15340000
         DROP  13,15                                                    15350000
IOWAIT   MVI   NEXTTRYM,X'00' .    MAKE NEXTTRY NOT MODIFIED            15360000
         SVC   C'V' .              SO CAN FAST PROCESS SLEEPER          15370000
         SVC   C'.' .              GO PROCESS IT RIGHT AWAY             15380000
IONOFPR  SVC   C'V' .              AND WAKE UP THE SLEEPER              15390000
IOBACK   LM    0,15,IOHSAVE .      RELOAD OUR REGISTERS                 15400000
         LPSW  IOOLD .             AND STEALTHILY RETURN                15410000
         DROP  1,6                                                      15420000
         EJECT                                                          15430000
*********************************************************************** 15440000
*                                                                     * 15450000
*             IPL ENTERED ROUTINE                                     * 15460000
*                                                                     * 15470000
*        FUNCTION: TO INITIALIZE SYSTEM PARAMETERS, SET STORAGE KEYS, * 15480000
*                  AND CREATE MULTIPLE JOB STREAMS.                   * 15490000
*                                                                     * 15500000
*********************************************************************** 15510000
         SPACE 1                                                        15520000
IPLRTN   EQU   * .                 THE IPL-ENTERED ROUTINE              15530000
         BALR  1,0                                                      15540000
         USING *,1 .               ESTABLISH ADDRESSING                 15550000
         MVC   IONEW+5(3),SOSIONEW activate IO handler                  15553002
         MVC   EXTNEW+5(3),IPLEXNEW ignore external interrupts for now  15556002
         LA    15,IPLPCB .         I'M RUNNING                          15560000
         ST    15,RUNNING .        INITIALIZE 'RUNNING'                 15570000
         ST    15,NEXTTRY .        INITIALIZE 'NEXTTRY'                 15580000
         MVC   VERYEND,=A(0,CORESIZE-(VERYEND-PROGRAM)) FREE CORE       15590000
         LA    3,8 .               SET ZERO KEY AND FETCH PROTECT       15600000
         L     2,CORESIZ .         START PAST THE LAST BLOCK            15610000
IPLCL    S     2,PAGESIZE .        get the previous block, page aligned 15620002
         BM    IPLTH .             IF NEGATIVE, WE'RE THROUGH HERE      15630000
*        SSKE  3,2 .               else set the storage key to          15640002
         DC    X'B22B0032'         Assembler (XF) doesn't support SSKE  15643002
         B     IPLCL .              ZERO, AND WORK BACKWARDS            15650000
IPLTH    SR    4,4 .               INDEX IN TABLES FOR INPUT STREAM     15660000
         L     5,STREAMS .         HOW MANY STREAMS?                    15670000
IPLLOOP  LA    2,IPLAPCBS .        READY TO ALLOCATE A PCB              15680000
         USING XAX,2                                                    15690000
         SVC   C'A' .              ALLOCATE                             15700000
         L     2,XAXADDR .         GET THE ADDRESS                      15710000
         MVC   0(TYPLEN,2),TYPPCB .MAKE IT LOOK LIKE A PCB              15720000
         SVC   C'I' .              CHAIN IT ON                          15730000
         USING PCB,2                                                    15740000
         ST    2,PCBNPTG .         BUT PUT IT IN A GROUP BY ITSELF      15750000
         ST    2,PCBLPTG                                                15760000
         DROP  2                                                        15770000
         USING PCB,15                                                   15780000
         ST    15,PCBLPTG .        LIKEWISE FOR THE IPL PCB             15790000
         ST    15,PCBNPTG                                               15800000
         DROP  15                                                       15810000
         USING PCB,2                                                    15820000
         LA    8,PCBISA .          GET THE NEW PCB'S ISA                15830000
         USING SA,8                                                     15840000
         LA    9,SAREGS .          ABOUT TO FIX INIT REGS               15850000
         USING REGS,9                                                   15860000
         LA    10,UCBTAB                                                15870000
         AR    10,4                                                     15880000
         MVC   REG3,0(10) .        REG3 -> (RDRUCB,PRTUCB)              15890000
         MVC   REG4,KEYTAB-UCBTAB(10) . REG4 = KEY                      15900000
         DROP  9                                                        15910000
         LA    4,4(4) .            GO TO NEXT JOB STREAM                15920000
         BCT   5,IPLLOOP .         DO FOR EACH STREAM                   15930000
         MVC   EXTNEW+5(3),SOSEXNEW reactivate ext interrupt handler    15935002
         SVC   C'.' .              THEN ENTER TRAFFIC CONTROLLER        15940000
         SPACE 1                                                        15950000
STREAMS  DC    F'4' .              NUMBER OF STREAMS                    15960002
         SPACE 1                                                        15970000
UCBTAB   EQU   * .                 TABLE OF PTRS TO UCB BLOCKS          15980000
         DC    A(UCBLP1)                                                15990000
         DC    A(UCBLP2)                                                16000000
         DC    A(UCBLP3)                                                16003002
         DC    A(UCBLP4)                                                16006002
         SPACE 1                                                        16010000
KEYTAB   EQU   * .                 TABLE OF PROTECTION KEYS             16020000
         DC    X'00100000'         storage key for stream 1 region      16030002
         DC    X'00200000'         storage key for stream 2 region      16034002
         DC    X'00300000'         storage key for stream 3 region      16038002
         DC    X'00400000'         storage key for stream 4 region      16042002
         SPACE 1                                                        16050000
UCBLP1   DC    A(UCBRDR1,UCBPRT1)                                       16060000
UCBLP2   DC    A(UCBRDR2,UCBPRT2)                                       16070000
UCBLP3   DC    A(UCBRDR3,UCBPRT3)                                       16073002
UCBLP4   DC    A(UCBRDR4,UCBPRT4)                                       16076002
         SPACE 1                                                        16080000
         DS    0D                                                       16090000
IPLPCB   DC    CL8' ' .            IPL ROUTINE PCB                      16100000
         DC    4A(IPLPCB)                                               16110000
         DC    X'FF000000' .       INITIALIZED FLAGS                    16120000
         DC    F'1,0'                                                   16130000
         DC    5F'0,0'                                                  16140000
         DC    X'0002000000000000'                                      16150000
         DS    CL76                                                     16160000
         DS    CL84                                                     16170000
         DS    CL84                                                     16180000
         SPACE 1                                                        16190000
IPLAPCBS DC    A(LENPCB) .         ALLOC LIST FOR PCB'S                 16200000
         DC    A(0)                                                     16210000
         DC    F'8'                                                     16220000
CORESIZ  DC    A(CORESIZE) .       BYTES OF CORE IN OBJECT MACHINE      16230000
         SPACE 1                                                        16240000
         DS    0D                                                       16250000
TYPPCB   DC    CL8'*IBSUP' .       A TEMPLATE *IBSUP PCB                16260000
         DC    4A(0)                                                    16270000
TEMPLATE DC    X'00000000' .       INITIALIZED FLAGS                    16280000
         DC    F'1,0'                                                   16290000
         DC    5F'0,0'                                                  16300000
         DC    X'FF00000000',AL3(JSP)                                   16310000
TYPLEN   EQU   *-TYPPCB                                                 16320000
EXINTRPT LPSW  EXTOLD           ignore external interrupts              16321002
         DS    0F               align                                   16322002
         DC    X'00'            filler                                  16323002
SOSIONEW DC    AL3(IOHANDL)     sample OS IO new PSW instruction addr   16324002
         DC    X'00'            filler                                  16325002
SOSEXNEW DC    AL3(EXTHANDL)    sample OS ext new PSW instruction addr  16326002
         DC    X'00'            filler                                  16327002
IPLEXNEW DC    AL3(EXINTRPT)    IPLRTN ext new PSW instruction addr     16328002
         EJECT                                                          16330000
*********************************************************************** 16340000
*                                                                     * 16350000
*             JOB STREAM PROCESSOR                                    * 16360000
*                                                                     * 16370000
*********************************************************************** 16380000
         SPACE 1                                                        16390000
JSP      EQU   * .                 THE JOB STREAM PROCESSOR             16400000
         BALR  1,0 .                (PROCESS *IBSUP)                    16410000
         USING *,1 .               ESTABLISH ADDRESSING                 16420000
         LA    2,JSPSUSEM .        LOCK OURSELVES UNTIL                 16430000
         SVC   C'P' .               WE CAN ALLOCATE STORAGE             16440000
         LA    2,JSPAAS .          READY TO ALLOCATE                    16450000
         USING XAX,2                                                    16460000
         SVC   C'E' .              ALLOCATE                             16470000
         L     12,XAXADDR .        PTR TO AUTO AREA                     16480000
         DROP  2                                                        16490000
         USING JSPAS,12 .          USE FOR ADDRESSING                   16500000
         LA    2,JSPSUSEM .        UNLOCK OURSELVES                     16510000
         SVC   C'V'                                                     16520000
         MVC   TREAD+0(8),=CL8'*IN' . INITIALIZE VALUES IN AUTOMATIC    16530000
         MVC   TREAD+8(4),=F'8' .   STORAGE                             16540000
         MVC   TREAD+12(4),=C'READ'                                     16550000
         LA    2,CARD                                                   16560000
         ST    2,ACARD                                                  16570000
         MVC   USERL+0(8),=CL8'USERPROG'                                16580000
         MVC   WRITE(12),SKIP                                           16590000
         MVC   WRITE+12(4),=C'PRIN'                                     16600000
         LA    5,LINE                                                   16610000
         ST    5,WRITE+16                                               16620000
         MVC   CORE+8(4),PAGESIZE  align to page boundary               16630002
         MVC   TALK+0(8),=CL8'USERPROG'                                 16640000
         MVC   TALK+8(4),=F'12'                                         16650000
         MVC   ANYBACK+8(4),=F'1'                                       16660000
         MVC   RLDTEMP,=A(0)                                            16670000
         ST    4,KEY .             STORE KEY                            16680000
         LR    5,3 .               GET PTR TO UCB PTR BLOCK             16690000
         L     3,0(5) .            GET READER POINTER                   16700000
         LA    2,INSEQ .           READY TO CREATE & START *IN          16710000
         SVC   C'C' .              CREATE                               16720000
         SVC   C'Y' .              START                                16730000
         L     3,4(5) .            GET PTR TO PRINTER UCB               16740000
         LA    2,OUTSEQ .          READY TO CREATE & START *OUT         16750000
         SVC   C'C' .              CREATE                               16760000
         SVC   C'Y' .              START                                16770000
         SPACE 1                                                        16780000
LOOP     LA    2,TREAD .           READT TO READ A CARD                 16790000
         SVC   C'S' .              START TO READ                        16800000
         MVC   RREPLY1,=F'132' .   132 CHARS FOR REPLY                  16810000
         LA    2,RREPLY                                                 16820000
         SVC   C'R' .              LISTEN FOR REPLY                     16830000
         CLC   REPLY(2),=C'OK' .   IS REPLY 'OK'?                       16840000
         BNE   STOP .              IF NOT, STOP                         16850000
         CLC   =C'$JOB,',CARD .    HAVE WE A JOB CARD?                  16860000
         BE    JOB .               GOOD!                                16870000
         B     LOOP .              ELSE LOOP                            16880000
STOP     LA    2,JSPNEVER .        WAIT FOR A "V" OPERATION             16890000
         SVC   C'P' .               THAT NEVER COMES                    16900000
         SPACE 1                                                        16910000
JOB      MVI   LOADED,X'00' .      REMEMBER NOT LOADED                  16920000
         MVC   LINE,=CL8' ' .      CLEAR A LINE, PUT IN                 16930000
         MVC   LINE+8(124),LINE+7 .ALL BLANKS                           16940000
         MVC   LINE(80),CARD .     GET READY TO SEND $JOB CARD          16950000
         LA    2,WRITE .           TO PRINTER                           16960000
         SVC   C'S' .              SEND IT                              16970000
         LA    2,RREPLY                                                 16980000
         SVC   C'R' .              AND WAIT FOR REPLY                   16990000
         LA    2,USERL .           CREATE USERPROG                      17000000
         SVC   C'C'                                                     17010000
         LA    4,CARD+4 .          START TO SCAN CARD                   17020000
         BAL   3,SCAN .            GET NEXT TOKEN                       17030000
         BCTR  5,0 .               less one to remove K                 17040002
         O     5,COREPKLN .        length of packed size for execute    17049002
         EX    5,COREPACK .        pack core digits                     17058002
         CVB   8,COREPCKD .        convert core requested to binary     17067002
         SR    9,9 .               is core ..                           17076002
         SRDL  8,2 .                .. modulo four ..                   17085002
         LTR   9,9 .                .. equal zero?                      17094002
         BZ    COREOK .             -> yes, use it                      17103002
         LA    8,1(,8) .            -> no, up one page                  17112002
COREOK   SLL   8,12 .              core bytes, rounded up to full pages 17121002
         ST    8,CORE .            remember core requirement            17130002
ASGNUNIT BAL   3,SCAN .            GET NEXT TOKEN                       17150000
         CLI   0(4),C'=' .         IS IT AN '='?                        17160000
         BNE   LOAD .              IF NOT, LOAD IN THE OBJECT DECK      17170000
         CLI   0(9),C'*' .         HAS USER NAMED IT STARTING           17180000
         BE    EXPUNGE .            WITH '*'? IF SO, THROW HIM OUT      17190000
         LA    2,SEQ .             ELSE CREATE A PROCESS                17200000
         MVC   SEQ,=CL8' ' .       BLANK OUT THE NAME                   17210000
         EX    5,UNAMMOV .         THEN MOVE THE RELEVANT               17220000
         SVC   C'C' .               CHARACTERS AND CREATE               17230000
         LA    2,SEQ .             WE'LL START IT IN A MOMENT           17240000
         BAL   3,SCAN .            SCAN AGAIN                           17250000
         EX    5,CMPIN .           IS IT 'IN'?                          17260000
         BE    ASIN .              IF SO, ASSIGN IT AS IN               17270000
         EX    5,CMPOUT .          IF IT'S 'OUT'                        17280000
         BE    ASOUT .              ASSIGN IT AS OUT                    17290000
         EX    5,CMPEXCP .         IS IT 'EXCP'?                        17300000
         BE    ASEXCP .             IF SO, ASSIGN IT AS EXCP            17310000
         B     EXPUNGE .            ERROR: GO ON TO NEXT JOB            17320000
UNAMMOV  MVC   SEQ(0),0(9) .       MOVE THE UNIT'S PROCESS NAME         17330000
CMPIN    CLC   0(0,9),=C'IN ' .    DOES IT SAY 'IN'?                    17340000
CMPOUT   CLC   0(0,9),=C'OUT ' .   DOES IT SAY 'OUT'?                   17350000
CMPEXCP  CLC   0(0,9),=C'EXCP ' .  DOES IT SAY 'EXCP'?                  17360000
         SPACE 1                                                        17370000
ASIN     LA    11,=CL8'*IN' .      POINT TO NAME OF READER HANDLER      17380000
SETDIM   MVC   UNITRTN,=A(DIM) .   USE DIM AS THE INTERFACE             17390000
         SVC   C'Y'                                                     17400000
         B     ASGNUNIT                                                 17410000
ASOUT    LA    11,=CL8'*OUT' .     POINT TO NAME OF PRINTER HANDLER     17420000
         B     SETDIM                                                   17430000
ASEXCP   MVC   UNITRTN,=A(EXCPHNDL) . USE FOR USER SUPPLIED             17440000
         L     11,KEY                                                   17450000
         SVC   C'Y' .              I/O ROUTINE                          17460000
         B     ASGNUNIT                                                 17470000
         SPACE 1                                                        17480000
LOAD     LA    2,CORE .            READY TO ALLOCATE THE REGION         17490000
         SVC   C'A' .              AND ALLOCATE IT                      17500000
         MVI   LOADED,X'FF' .      REMEMBER THAT WE'RE LOADED           17510000
         L     9,CORE+4 .          GET THE FIRST ADDRESS                17520000
         L     4,KEY .             GET THE KEY                          17530000
         SRL   4,16                                                     17540000
         O     4,FETCHPRT          fetch protected                      17545002
         LR    3,9 .               GET THE BLOCK FOLLOWING OURS         17550000
         AR    3,8                                                      17560000
LOADSK   S     3,PAGESIZE .        get the previous block, page aligned 17570002
         CR    3,9 .               HAVE WE PASSED THE START?            17580000
         BL    LOADLOOP .          IF SO, START LOADING                 17590000
*        SSKE  4,3 .               else set this block to the key       17600002
         DC    X'B22B0043'         Assembler (XF) doesn't support SSKE  17603002
         B     LOADSK .            AND BRANCH BACK                      17610000
LOADLOOP LA    2,TREAD .           READ IN OBJECT DECK                  17620000
         SVC   C'S' .              GET A CARD A'READING                 17630000
         MVC   RREPLY1,=F'132'                                          17640000
         LA    2,RREPLY                                                 17650000
         SVC   C'R' .              WAIT FOR ANSWER                      17660000
         CLC   CARD+1(3),=C'TXT' . IS IT A TXT CARD?                    17670000
         BE    TXTCARD                                                  17680000
         CLC   CARD+1(3),=C'RLD' . IS IT A RLD CARD?                    17690000
         BE    RLDCARD                                                  17700000
         CLC   CARD+1(3),=C'END' . IS IT AN END CARD?                   17710000
         BE    ENDCARD                                                  17720000
         B     LOADLOOP .          IF NONE, IGNORE.                     17730000
         SPACE 1                                                        17740000
TXTCARD  L     10,CARD+4 .         GET THE RELATIVE ADDRESS             17750000
         AR    10,9 .              PLUS THE ABSOLUTE ADDRESS            17760000
         LH    11,CARD+10 .        GET THE COUNT,                       17770000
         BCTR  11,0 .               DECREMENTED                         17780000
         EX    11,TXTMOV .         AND MOVE THE TEXT                    17790000
         B     LOADLOOP .          AND READ ANOTHER CARD! OH WOW!       17800000
TXTMOV   MVC   0(0,10),CARD+16                                          17810000
         SPACE 1                                                        17820000
RLDCARD  LH    11,CARD+10 .        GET THE BYTE COUNT                   17830000
         LA    13,CARD+20 .        AND AN INDEX INTO THE CARD           17840000
RLDLOOP  L     10,0(13) .          GET THE LOCATION TO BE RLD'D         17850000
         AR    10,9 .              GET THE ABSOLUTE ADDRESS             17860000
         TM    3(13),X'03' .       IS IT A FULLWORD?                    17870000
         BNZ   NOTALGND .          IF NO, HANDLE AS THREE BYTES         17880000
         L     7,0(10) .           GET THAT WORD (HAD BETTER BE         17890000
         AR    7,9 .                ONE); ADD THE RELOCATION            17900000
         ST    7,0(10) .            ADDRESS, AND STORE IT BACK          17910000
RLDCONT  TM    0(13),X'01' .       CHECK IF LONG OR SHORT FIELD         17920000
         BNZ   SHORT .             AND BRANCH ACCORDINGLY               17930000
         LA    4,8 .               SKIP EIGHT BYTES                     17940000
         B     RLDFINI                                                  17950000
SHORT    LA    4,4 .               SKIP FOUR BYTES                      17960000
RLDFINI  AR    13,4 .              INCREMENT THE CARD INDEX             17970000
         SR    11,4 .              DECREMENT THE BYTE COUNT             17980000
         BP    RLDLOOP .           AND TRY AGAIN                        17990000
         B     LOADLOOP .          OR READ ANOTHER CARD                 18000000
NOTALGND MVC   RLDTEMP+1(3),0(10) . PUT ADDRESS HERE                    18010000
         L     7,RLDTEMP .         RELOCATE IT                          18020000
         AR    7,9                                                      18030000
         ST    7,RLDTEMP .         AND PUT IT BACK TO                   18040000
         MVC   0(3,10),RLDTEMP+1 .  WHERE IT BELONGS                    18050000
         NI    RLDTEMP,X'00' .     CLEAR OUT TEMPORARY                  18060000
         B     RLDCONT .           AND LOOP BACK                        18070000
         SPACE 1                                                        18080000
ENDCARD  LA    2,USERL .           FIND THE PCB FOR USERPROG            18090000
         SVC   C'N'                                                     18100000
         L     4,USERL+8 .         GET THE ADDRESS                      18110000
         USING PCB,4                                                    18120000
         MVI   PCBBLOKT,X'FF' .    TEMPORARILY BLOCK IT                 18130000
         ST    9,USERL+8 .         STORE THE BEGINNING ADDRESS          18140000
         SVC   C'Y' .              THEN START IT                        18150000
         L     5,KEY .             GET THE KEY                          18160000
         O     5,PCBISA+0 .        THEN OR THIS INTO THE                18170000
         ST    5,PCBISA+0 .         FIRST WORD OF THE PCB               18180000
         OI    PCBISA+1,X'01' .    OR IN A 'PROGRAM STATE' BIT          18190000
         MVI   PCBBLOKT,X'00' .    AND THEN UNBLOCK IT                  18200000
         DROP  4                                                        18210000
         LA    2,TALK .            LISTEN TO WHAT IT SAYS               18220000
         SVC   C'R'                                                     18230000
         SPACE 1                                                        18240000
         MVC   LINE(8),=CL8' ' .   IF JOB FINISHED, CLEAR A LINE        18250000
         MVC   LINE+8(124),LINE+7                                       18260000
         MVC   LINE(12),TALK+12 .  MOVE THE MESSAGE ONTO THE LINE       18270000
         LA    2,WRITE .           AND SAY TO WRITE IT                  18280000
         SVC   C'S'                                                     18290000
         LA    2,ANYBACK                                                18300000
         SVC   C'R'                                                     18310000
         LA    2,SKIP .            SKIP TO THE TOP OF THE NEXT PAGE     18320000
         SVC   C'S'                                                     18330000
         LA    2,ANYBACK                                                18340000
         SVC   C'R'                                                     18350000
         SPACE 1                                                        18360000
EXPUNGE  L     5,RUNNING .         EXPUNGE A JOB: LOOK AT ALL PCBS      18370000
         LA    2,SEQ                                                    18380000
         USING PCB,5                                                    18390000
EXPLOOP  MVC   SEQ(8),PCBNAME .    GET THE PROCESS NAME                 18400000
         L     4,PCBNPTG .         GET THE NEXT PTR                     18410000
         CLI   SEQ+0,C'*' .        IS IT A '*' PROCESS?                 18420000
         BE    EXPNXT .            IF SO, SKIP OVER                     18430000
         SVC   C'Z' .              ELSE STOP IT                         18440000
         SVC   C'D' .              AND DESTROY IT                       18450000
EXPNXT   LR    5,4 .               GO TO THE NEXT PCB                   18460000
         C     5,RUNNING .         ARE WE THROUGH?                      18470000
         BNE   EXPLOOP .           IF NOT, LOOP AGAIN                   18480000
         CLI   LOADED,X'00' .      WAS CORE ALLOCATED?                  18490000
         BE    LOOP .              IF NOT, GO READ THE NEXT $JOB CARD   18500000
         LA    4,8 .               set zero key and fetch protect       18510002
         LR    3,9 .               AND A POINTER TO THE NEXT            18520000
         AR    3,8 .                BLOCK AFTER OURS                    18530000
LOADCL   S     3,PAGESIZE .        get the previous block, page aligned 18540002
         CR    3,9 .               ARE WE THROUGH?                      18550000
         BL    LOADD .             IF SO, GO FREE CORE                  18560000
*        SSKE  4,3 .               else clear storage key               18570002
         DC    X'B22B0043'         Assembler (XF) doesn't support SSKE  18573002
         B     LOADCL .            AND LOOP BACK                        18580000
LOADD    LA    2,CORE                                                   18590000
         SVC   C'F' .              FREE THE STORAGE                     18600000
         B     LOOP .              READ ANOTHER $JOB CARD               18610000
         SPACE 1                                                        18620000
SCAN     SR    5,5 .               START THE TOKEN COUNT AT ZERO        18630000
SCANLOOP LA    4,1(4) .            GO TO NEXT CHARACTER                 18640000
         CLI   0(4),C',' .         DO WE HAVE A DELIMITER? IF SO,       18650000
         BE    TOKSTART                                                 18660000
         CLI   0(4),C'=' .         DITTO                                18670000
         BE    TOKSTART                                                 18680000
         CLI   0(4),C' ' .         DITTO                                18690000
         BE    TOKSTART                                                 18700000
         LA    5,1(5) .            AND UP COUNT                         18710000
         B     SCANLOOP .          AND LOOP                             18720000
TOKSTART LR    9,4 .               SET REG9 TO START                    18730000
         SR    9,5 .               OF THIS TOKEN                        18740000
         BCTR  5,0 .               LESS ONE FOR EXECUTE INSTRUCTION     18750000
         BR    3                                                        18760000
         SPACE 2                                                        18770000
JSPNEVER DC    F'0,0' .            A GOOD WAY TO DIE: P(JSPNEVER)       18780000
SKIP     DC    CL8'*OUT' .         MESSAGE BLOCK FOR A NEW PAGE         18790000
         DC    F'8'                                                     18800000
         DC    CL4'STC1'                                                18810000
INSEQ    DC    CL8'*IN' .          SEQ TO CREATE & START *IN            18820000
         DC    A(RDRHANDL)                                              18830000
OUTSEQ   DC    CL8'*OUT' .         SEQ TO CREATE & START *OUT           18840000
         DC    A(PRTHANDL)                                              18850000
COREPACK PACK  COREPCKD(1),0(1,9) . executed to pack core size req'd    18860002
COREPCKD DS    D .                 packed core requirement goes here    18880002
COREPKLN DC    X'00000070' .       length of packed size for execute    18900002
PAGESIZE DC    F'4096' .           page size for core computation       18920002
JSPSUSEM DC    F'1,0' .            SEMAPHORE TO LOCK ROUTINE            18990000
JSPAAS   DC    A(LENJSPAS) .       ALLOCATE LIST FOR AUTO STORAGE       19000000
         DS    A                                                        19010000
FETCHPRT DC    F'8'                reused to or in fetch protection     19020002
         EJECT                                                          19030000
*********************************************************************** 19040000
*                                                                     * 19050000
*                            DEVICE INTERFACE MODULE                  * 19060000
*                                                                     * 19070000
*        FUNCTION: TO INTERFACE BETWEEN USERPROG AND DEVICE HANDLER   * 19080000
*       DATABASES: NONE                                               * 19090000
*   ROUTINES USED: XA, XP, XV, XR, XS                                 * 19100000
*       PROCEDURE: ALLOCATE AUTOMATIC STORAGE; START TO READ MESSAGE  * 19110000
*                  FROM USER; SEND MESSAGE TO DEVICE HANDLER;         * 19120000
*                  CONTINUE LOOPING, SENDING MESSAGES FROM USER TO    * 19130000
*                  DEVICE HANDLER AND BACK.                           * 19140000
*    ERROR CHECKS: NONE                                               * 19150000
*      INTERRUPTS: ON                                                 * 19160000
*     USER ACCESS: YES                                                * 19170000
*                                                                     * 19180000
*********************************************************************** 19190000
         SPACE 1                                                        19200000
DIM      EQU   * .                 THE DEVICE INTERFACE MODULE          19210000
         BALR  1,0                                                      19220000
         USING *,1 .               ESTABLISH ADDRESSING                 19230000
         LA    2,DIMSEM .          LOCK UNTIL GET STORAGE               19240000
         SVC   C'P'                                                     19250000
         LA    2,DIMAAS .          READY TO ALLOCATE STORAGE            19260000
         USING XAX,2                                                    19270000
         SVC   C'E' .              DO IT                                19280000
         L     12,XAXADDR .        GET THE ADDRESS                      19290000
         DROP  2                                                        19300000
         LA    2,DIMSEM .          UNLOCK OURSELVES                     19310000
         SVC   C'V'                                                     19320000
         USING DIMAS,12 .          USE 12 FOR AUTO STORAGE              19330000
         MVC   DIMLMS,0(11) .      MOVE NAME OF RECIEVER                19340000
         LA    8,132 .             REG 8 = SIZE OF MESSAGE              19350000
DIMLOOP  ST    8,DIMMSG+8 .        GET READY TO READ A MESSAGE          19360000
         LA    2,DIMMSG                                                 19370000
         SVC   C'R' .              READ                                 19380000
         MVC   DIMTEMP,DIMMSG .    SAVE SENDER NAME                     19390000
         MVC   DIMMSG,DIMLMS .     SEND IT BACK TO THE LAST GUY         19400000
         SVC   C'S' .              SEND IT                              19410000
         MVC   DIMLMS,DIMTEMP .    AND REMEMBER WHO TO SEND TO NEXT     19420000
         B     DIMLOOP .           RELOOP                               19430000
DIMSEM   DC    F'1,0' .            SEMAPHORE FOR ENTRY                  19440000
DIMAAS   DC    A(DIMLEN) .         ALLOCATE SEQ FOR AUTO STORAGE        19450000
         DC    A(0)                                                     19460000
         DC    F'8'                                                     19470000
         DROP  12                                                       19480000
         EJECT                                                          19490000
         LTORG                                                          19500000
VERYEND  DS    6D .                beginning of free storage            19510004
LOADER   DS    0D                  IPL loader goes here                 19521002
         EJECT                                                          19521203
R0       EQU   0                                                        19521502
R1       EQU   1                                                        19522002
R2       EQU   2                                                        19522502
R3       EQU   3                                                        19523002
R4       EQU   4                                                        19523502
R5       EQU   5                                                        19524002
R6       EQU   6                                                        19524502
R7       EQU   7                                                        19525002
R8       EQU   8                                                        19525502
R9       EQU   9                                                        19526002
R10      EQU   10                                                       19526502
R11      EQU   11                                                       19527002
R12      EQU   12                                                       19527502
R13      EQU   13                                                       19528002
R14      EQU   14                                                       19528502
R15      EQU   15                                                       19529002
*********************************************************************** 19530000
*                                                                     * 19540000
*                          DATABASE DEFINITIONS                       * 19550000
*                                                                     * 19560000
*********************************************************************** 19570000
         SPACE 1                                                        19580000
PCB      DSECT .                   PROCESS CONTROL BLOCK DEFINITION     19590000
PCBNAME  DS    CL8 .               NAME                                 19600000
PCBNPTG  DS    F .                 NEXT POINTER THIS GROUP              19610000
PCBLPTG  DS    F .                 LAST POINTER THIS GROUP              19620000
PCBNPALL DS    F .                 NEXT POINTER ALL                     19630000
PCBLPALL DS    F .                 LAST POINTER ALL                     19640000
PCBSTOPT DS    C .                 STOPPED                              19650000
PCBBLOKT DS    C .                 BLOCKED                              19660000
PCBINSMC DS    C .                  IN SMC                              19670000
PCBSW    DS    C .                 STOP WAITING                         19680000
PCBMSC   DS    CL8 .               MESSAGE SEMAPHORE COMMON             19690000
PCBMSR   DS    CL8 .               MESSAGE SEMAPHORE RECEIVER           19700000
PCBFM    DS    F .                 FIRST MESSAGE                        19710000
PCBNSW   DS    F .                 NEXT SEMAPHORE WAITER                19720000
PCBSRS   DS    CL8 .               STOPPER SEMAPHORE                    19730000
PCBSES   DS    CL8 .               STOPPEE SEMAPHORE                    19740000
PCBASIZE DS    F .                 AUTOMATIC STORAGE SIZE               19750000
PCBAADDR DS    A .                 AUTOMATIC STORAGE ADDRESS            19760000
PCBISA   DS    CL84 .              INTERRUPT SAVE AREA                  19770000
PCBFSA   DS    CL84 .              FAULT SAVE AREA                      19780000
PCBMSA   DS    CL84 .              MEMORY SAVE AREA                     19790000
         DS    0D .                (ALIGN)                              19800000
LENPCB   EQU   *-PCB .             (LENGTH)                             19810000
         SPACE 1                                                        19820000
SA       DSECT .                   SAVE AREA DEFINITION                 19830000
SAPSW    DS    D .                 PROGRAM STATUS WORD                  19840000
SAREGS   DS    CL64 .              REGISTERS                            19850000
SATEMP   DS    CL12 .              TEMPORARIES                          19860000
         SPACE 1                                                        19870000
REGS     DSECT .                   REGISTER DEFINITION                  19880000
REG0     DS    F .                 REGISTER 0                           19890000
REG1     DS    F .                 REGISTER 1                           19900000
REG2     DS    F .                 REGISTER 2                           19910000
REG3     DS    F .                 REGISTER 3                           19920000
REG4     DS    F .                 REGISTER 4                           19930000
REG5     DS    F .                 REGISTER 5                           19940000
REG6     DS    F .                 REGISTER 6                           19950000
REG7     DS    F .                 REGISTER 7                           19960000
REG8     DS    F .                 REGISTER 8                           19970000
REG9     DS    F .                 REGISTER 9                           19980000
REG10    DS    F .                 REGISTER 10                          19990000
REG11    DS    F .                 REGISTER 11                          20000000
REG12    DS    F .                 REGISTER 12                          20010000
REG13    DS    F .                 REGISTER 13                          20020000
REG14    DS    F .                 REGISTER 14                          20030000
REG15    DS    F .                 REGISTER 15                          20040000
         SPACE 1                                                        20050000
FSB      DSECT .                   FREE STORAGE BLOCK DEFINITIONS       20060000
FSBNEXT  DS    A .                 NEXT                                 20070000
FSBSIZE  DS    F .                 SIZE                                 20080000
         SPACE 1                                                        20090000
SM       DSECT .                   SEMAPHORE DEFINITION                 20100000
SMVAL    DS    F .                 VALUE                                20110000
SMPTR    DS    F .                 PTR                                  20120000
         SPACE 1                                                        20130000
MSG      DSECT .                   MESSAGE DEFINITION                   20140000
MSGSENDR DS    A .                 POINTER TO SENDER'S PCB              20150000
MSGNEXT  DS    A .                 NEXT                                 20160000
MSGSIZE  DS    F .                 SIZE                                 20170000
MSGTEXT  DS    0C .                TEXT                                 20180000
LENMSG   EQU   *-MSG .             (LENGTH)                             20190000
         SPACE 1                                                        20200000
XAX      DSECT .                   XA ARGUMENT LIST                     20210000
XAXSIZE  DS    F .                 SIZE                                 20220000
XAXADDR  DS    F .                 ADDRESS                              20230000
XAXALGN  DS    F .                 ALIGNMENT                            20240000
         SPACE 1                                                        20250000
XFX      DSECT .                   XF ARGUMENT LIST                     20260000
XFXSIZE  DS    F .                 SIZE                                 20270000
XFXADDR  DS    F .                 ADDRESS                              20280000
         SPACE 1                                                        20290000
XBX      DSECT .                   XB ARGUMENT LIST                     20300000
XBXSIZE  DS    F .                 SIZE                                 20310000
XBXADDR  DS    F .                 ADDRESS                              20320000
         SPACE 1                                                        20330000
XCX      DSECT .                   XC ARGUMENT LIST                     20340000
XCXNAME  DS    CL8 .               NAME                                 20350000
         SPACE 1                                                        20360000
XDX      DSECT .                   AD ARGUMENT LIST                     20370000
XDXNAME  DS    CL8 .               NAME                                 20380000
         SPACE 1                                                        20390000
XNX      DSECT .                   XN ARGUMENT LIST                     20400000
XNXNAME  DS    CL8 .               NAME                                 20410000
XNXADDR  DS    A .                 ADDRESS                              20420000
         SPACE 1                                                        20430000
XRX      DSECT .                   XR ARGUMENT LIST                     20440000
XRXNAME  DS    CL8 .               NAME                                 20450000
XRXSIZE  DS    F .                 SIZE                                 20460000
XRXTEXT  DS    0C .                TEXT                                 20470000
         SPACE 1                                                        20480000
XSX      DSECT .                   XS ARGUMENT LIST                     20490000
XSXNAME  DS    CL8 .               NAME                                 20500000
XSXSIZE  DS    F .                 SIZE                                 20510000
XSXTEXT  DS    0C .                TEXT                                 20520000
         SPACE 1                                                        20530000
XYX      DSECT .                   XY ARGUMENT LIST                     20540000
XYXNAME  DS    CL8 .               NAME                                 20550000
XYXADDR  DS    A .                 ADDR                                 20560000
         SPACE 1                                                        20570000
XZX      DSECT .                   XZ ARGUMENT LIST                     20580000
XZXNAME  DS    CL8 .               NAME                                 20590000
         SPACE 1                                                        20600000
RDRHAS   DSECT .                   READER HANDLER AUTOMATIC STORAGE     20610000
RDRHCCB  DS    2F .                CCB                                  20620000
RDRHMSG  DS    CL8 .               MESSAGE BLOCK FOR REQUESTS           20630000
         DS    F'8'                                                     20640000
         DS    CL8                                                      20650000
RDRHTEMP DS    CL80 .              AREA FOR $JOB IN DATA STREAM         20660000
RDRHM    DS    CL8 .               MESSAGE BLOCK FOR REPLY              20670000
         DS    F'2'                                                     20680000
         DS    CL2                                                      20690000
JOBBIT   DS    1C                                                       20700000
         DS    0D                                                       20710000
LENRDRHA EQU   *-RDRHAS .          (LENGTH)                             20720000
         SPACE 1                                                        20730000
PRTHAS   DSECT .                   PRINTER HANDLER AUTOMATIC STORAGE    20740000
PRTHCCB  DS    2F .                CCB                                  20750000
PRTHMSG  DS    CL8 .               MESSAGE BLOCK FOR REQUESTS           20760000
         DS    F'2'                                                     20770000
         DS    CL8                                                      20780000
PRTHM    DS    CL8 .               MESSAGE BLOCK FOR REPLY              20790000
         DS    F'2'                                                     20800000
         DS    CL2                                                      20810000
         DS    0D                                                       20820000
LENPRTHA EQU   *-PRTHAS .          (LENGTH)                             20830000
         SPACE 1                                                        20840000
EXCPHAS  DSECT .                   EXCP HANDLER AUTOMATIC STORAGE       20850000
EXCPHMSG DS    CL8 .               MESSAGE BLOCK FOR REQUESTS           20860000
         DS    F'12'                                                    20870000
         DS    CL12                                                     20880000
EXCPHM   DS    CL8 .               MESSAGE BLOCK FOR REPLY              20890000
         DS    F'12'                                                    20900000
         DS    CL12                                                     20910000
         DS    0D                                                       20920000
LENEXCPA EQU   *-EXCPHAS .          (LENGTH)                            20930000
         SPACE 1                                                        20940000
UCB      DSECT .                   UNIT CONTROL BLOCK DEFINITION        20950000
UCBADDR  DS    F .                 ADDRESS                              20960000
UCBUS    DS    FL8 .               USER SEMAPHORE                       20970000
UCBWS    DS    FL8 .               WAITER SEMAPHORE                     20980000
UCBCSW   DS    FL8 .               CHANNEL STATUS WORD                  20990000
UCBFPR   DS    CL1 .               FAST PROCESSING REQUIRED             21000000
         DS    0F                                                       21010000
UCBLENG  EQU   *-UCB                                                    21020000
         SPACE 1                                                        21030000
JSPAS    DSECT .                   JSP AUTOMATIC STORAGE                21040000
LINE     DS    CL132 .             PRINTED LINE                         21050000
         DS    0F                                                       21060000
CARD     DS    CL80 .              CARD READ                            21070000
         DS    0F                                                       21080000
RREPLY   DS    CL8 .               MESSAGE BLOCK FOR REPLIES            21090000
RREPLY1  DS    F                                                        21100000
REPLY    DS    CL132                                                    21110000
TREAD    DS    0F .                MESSAGE BLOCK FOR READING            21120000
         DS    CL8'*IN'                                                 21130000
         DS    F'8'                                                     21140000
         DS    CL4'READ'                                                21150000
ACARD    DS    A(0)                                                     21160000
WRITE    DS    CL8'*OUT' .         MESSAGE BLOCK TO PRINT A LINE        21170000
         DS    F'8'                                                     21180000
         DS    CL4'PRIN'                                                21190000
         DS    A(LINE)                                                  21200000
KEY      DS    F                                                        21210000
USERL    DS    CL8'USERPROG' .     LIST FOR MANIPULATING USERPROG       21220000
         DS    F                                                        21230000
SEQ      DS    CL8' ' .            COMMON ARG LIST FOR I/O PROCESS      21240000
UNITRTN  DS    A                                                        21250000
CORE     DS    F .                 MEMORY ALLOCATED AND FREE            21260000
         DS    F .                 SEQUENCE                             21270000
         DS    F'4096'             align to page boundary               21280002
RLDTEMP  DS    F                                                        21290000
TALK     DS    CL8'USERPROG' .     MESSAGE BLOCK FOR MESSAGE FROM       21300000
         DS    F'12' .              USERPROG                            21310000
         DS    CL12                                                     21320000
ANYBACK  DS    CL8 .               MESSAGE BLOCK FOR IGNORING MESS      21330000
         DS    F'1'                                                     21340000
         DS    CL1                                                      21350000
LOADED   DS    C .                 IS CORE ALLOCATED                    21360000
         DS    0D                                                       21370000
LENJSPAS EQU   *-JSPAS .           (LENGTH)                             21380000
         SPACE 1                                                        21390000
DIMAS    DSECT .                   DEVICE INTERFACE MODULE STORAGE      21400000
DIMMSG   DS    CL8 .               MESSAGE BLOCK                        21410000
         DS    F'132'                                                   21420000
         DS    CL132                                                    21430000
DIMLMS   DS    CL8 .               LAST MESSAGE SENDER                  21440000
DIMTEMP  DS    CL8 .               TEMPORARY                            21450000
         DS    0D                                                       21460000
DIMLEN   EQU   *-DIMAS .           (LENGTH)                             21470000
         END                                                            21480000
