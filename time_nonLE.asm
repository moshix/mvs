
*
* Name: TIMEASMN
*
* Author: David Alcock
*
* Written: 2000-02-16
*
* Purpose: Shows local and GMT time in Assembler
*
* Attributes: Non-reentrant, Non-Authorized
*
* Requirements: HLASM or higher
*
         PRINT NOGEN
         YREGS ,
         CVT   DSECT=YES,LIST=NO
         PRINT GEN
*
** Convert assembly date from format "YYYYMMDD" to "YYYY-MM-DD"
*
         LCLC  &ASMDATE
&ASMDATE SETC  '&SYSDATC'(1,4).'-'.'&SYSDATC'(5,2).'-'.'&SYSDATC'(7,2)
         EJECT ,
*
** Standard ESA entry housekeeping
*
TIMEASM  CSECT ,
         BAKR  R14,0                    Save regs
         LAE   R12,0(R15,0)             Get base register
         USING TIMEASM,R12              Get addressibility
*
** Obtain the time via the TIME SVC And format it
*
         TIME  DEC
         STCM  R0,B'1111',DOUBLE        Save time from TIME macro
*
         UNPK  MSG3H(9),DOUBLE(5)            >
         MVZ   MSG3H(8),=8X'00'              >> Hex convert
         TR    MSG3H(8),=C'0123456789ABCDEF' >
         MVI   MSG3H+8,C' '                  Fix
*- 01234567      01234567
*-"HHMMSSTH" to "HH:MM:SS"
         MVC   MSG3C(2),MSG3H
         MVI   MSG3C+2,C':'
         MVC   MSG3C+3(2),MSG3H+2
         MVI   MSG3C+5,C':'
         MVC   MSG3C+6(2),MSG3H+4
*
** Obtain the time via the STCK instruction and format it
*
         STCK  STCKCONI                   Get it
*-This system doesn't have the STCKE support in HLASM yet
*=====>  STCKE  STCKCONIE                 Get it
*
** Format the time from the STCK instruction
*
         STCKCONV STCKVAL=STCKCONI,       Convert this TOD Stamp       @
               CONVVAL=STCKCONO,          ..Into these date/time areas @
               TIMETYPE=DEC,              ..Output time format         @
               DATETYPE=YYYYMMDD,         ..Output Date format         @
               MF=(E,PARMLIST)
         LTR   R15,R15                    STCKCONV worked?
         BNZ   E$STCKC                    Failed, ABEND me
*
         UNPK  MSG5H(9),STCKCONO(5)
         MVZ   MSG5H(8),=8X'00'
         TR    MSG5H(8),=C'0123456789ABCDEF'
         UNPK  MSG5H+8(9),STCKCONO+4(5)
         MVZ   MSG5H+8(8),=8X'00'
         TR    MSG5H+8(8),=C'0123456789ABCDEF'
         MVI   MSG5H+16,C' '
*
         MVC   MSG5C(2),MSG5H
         MVI   MSG5C+2,C':'
         MVC   MSG5C+3(2),MSG5H+2
         MVI   MSG5C+5,C':'
         MVC   MSG5C+6(2),MSG5H+4
*
         LA    R0,2
         LA    R1,STCKCONI
         LA    R15,MSG5BV
F$M5     DS    0H
         UNPK  0(9,R15),0(5,R1)
         MVZ   0(8,R15),=8X'00'
         TR    0(8,R15),=C'0123456789ABCDEF'
         MVI   8(R15),C' '
         LA    R1,4(R1)
         LA    R15,9(R15)
         BCT   R0,F$M5
*
** Format the time from the STCKE instruction
*
*-This system doesn't have the STCKE support in HLASM yet
 AGO .STCKEX
         STCKCONV STCKEVAL=STCKCONIE,     Convert this TOD Stamp       @
               CONVVAL=STCKCONO,          ..Into these date/time areas @
               TIMETYPE=DEC,              ..Output time format         @
               DATETYPE=YYYYMMDD,         ..Output Date format         @
               MF=(E,PARMLIST)
         LTR   R15,R15                    STCKCONV worked?
         BNZ   E$STCKC                    Failed, ABEND me
*
         UNPK  MSG6H(9),STCKCONO(5)
         MVZ   MSG6H(8),=8X'00'
         TR    MSG6H(8),=C'0123456789ABCDEF'
         UNPK  MSG6H+8(9),STCKCONO+4(5)
         MVZ   MSG6H+8(8),=8X'00'
         TR    MSG6H+8(8),=C'0123456789ABCDEF'
         MVI   MSG6H+16,C' '
*
         MVC   MSG6C(2),MSG6H
         MVI   MSG6C+2,C':'
         MVC   MSG6C+3(2),MSG6H+2
         MVI   MSG6C+5,C':'
         MVC   MSG6C+6(2),MSG6H+4
*
         LA    R0,4
         LA    R1,STCKCONIE
         LA    R15,MSG6BV
F$M6     DS    0H
         UNPK  0(9,R15),0(5,R1)
         MVZ   0(8,R15),=8X'00'
         TR    0(8,R15),=C'0123456789ABCDEF'
         MVI   8(R15),C' '
         LA    R1,4(R1)
         LA    R15,9(R15)
         BCT   R0,F$M6
.STCKEX  ANOP  , @#$@#$@#$@#$@#$@#$@#$@#$@#&@#(*$&#@*($&#@$_#$*__
 B DAVE
 BC 12,DAVE
 BC 12,DAVE
*
 BC 3,DAVE
 BC 3,DAVE
DAVE DS 0H
*
** Let's adjust the STCK output for GMT
*
         LM    R2,R3,STCKCONI             Get STCK output (local time)
*-Adapted from SYS1.V2R5M0.SHASSRC(HASCSRIC):
         L     R5,CVTPTR                  -> CVT
         L     R5,CVTEXT2-CVTMAP(,R5)     -> CVT Exxtension
         LM    R14,R15,CVTLDTO-CVTXTNT2(R5) Time zone diff
         LM    R6,R7,CVTLSO-CVTXTNT2(R5)  Get leap seconds
         ALR   R3,R15                     Add low order time offset
         BC    12,CG$NOVER                Branch if no overflow
         AL    R2,=F'1'                   Carry the 1
CG$NOVER DS    0H
         ALR   R2,R14                     Add high order words
         SLR   R3,R7                      Minus low-order leap seconds
         BC    3,CG$BORRW                 Branch if no borrow
         BCTR  R2,0                       Subtract one for borrow
CG$BORRW DS    0H
         SLR   R2,R6                      Minus high-order leap seconds
         STM   R2,R3,STCKCONI             Save TOD adjusted for GMT
*
         STCKCONV STCKVAL=STCKCONI,       Convert this TOD Stamp       @
               CONVVAL=STCKCONO,          ..Into these date/time areas @
               TIMETYPE=DEC,              ..Output time format         @
               DATETYPE=YYYYMMDD,         ..Output Date format         @
               MF=(E,PARMLIST)
         LTR   R15,R15                    STCKCONV worked?
         BNZ   E$STCKC                    Failed, ABEND me
*
         UNPK  MSG10H(9),STCKCONO(5)
         MVZ   MSG10H(8),=8X'00'
         TR    MSG10H(8),=C'0123456789ABCDEF'
         UNPK  MSG10H+8(9),STCKCONO+4(5)
         MVZ   MSG10H+8(8),=8X'00'
         TR    MSG10H+8(8),=C'0123456789ABCDEF'
         MVI   MSG10H+16,C' '
*
         MVC   MSG10C(2),MSG10H
         MVI   MSG10C+2,C':'
         MVC   MSG10C+3(2),MSG10H+2
         MVI   MSG10C+5,C':'
         MVC   MSG10C+6(2),MSG10H+4
*
         LA    R0,2
         LA    R1,STCKCONI
         LA    R15,MSG10BV
F$M10    DS    0H
         UNPK  0(9,R15),0(5,R1)
         MVZ   0(8,R15),=8X'00'
         TR    0(8,R15),=C'0123456789ABCDEF'
         MVI   8(R15),C' '
         LA    R1,4(R1)
         LA    R15,9(R15)
         BCT   R0,F$M10
*
** Print messages on console
*
         MVC   WTOMSG1+4+28(L'MSG3C),MSG3C
         WTO   ,MF=(E,WTOMSG1)
         MVC   WTOMSG2+4+28(L'MSG5C),MSG5C
         WTO   ,MF=(E,WTOMSG2)
         MVC   WTOMSG3+4+28(L'MSG10C),MSG10C
         WTO   ,MF=(E,WTOMSG3)
*
** Print messages on SYSPRINT file
*
         OPEN  (SYSPRINT,(OUTPUT))
         PUT   SYSPRINT,MSG1
         PUT   SYSPRINT,MSG2              Blank line
         PUT   SYSPRINT,MSG3
*
         PUT   SYSPRINT,MSG2              Blank line
         PUT   SYSPRINT,MSG5B
         PUT   SYSPRINT,MSG5              STCK
*
         PUT   SYSPRINT,MSG2              Blank line
         PUT   SYSPRINT,MSG10B            GMT
         PUT   SYSPRINT,MSG10             GMT
*
         PUT   SYSPRINT,MSG2              Blank line
         PUT   SYSPRINT,MSG6B
         PUT   SYSPRINT,MSG6              STCKE
*
         PUT   SYSPRINT,MSG2              Blank line
         PUT   SYSPRINT,MSG7
         PUT   SYSPRINT,MSG8
         PUT   SYSPRINT,MSG9
         PUT   SYSPRINT,MSG2              Blank line
         CLOSE (SYSPRINT)
*
** Standard ESA exit housekeeping code
*
         SPACE 2
EXIT     DS    0H
         SLR   R15,R15                    Set return code
         PR    ,                          Return to caller
         SPACE 1
*
** Error: a macro failed. ABEND so we can look in dump for clues
*
E$CONVT  ABEND 1,DUMP
E$STCKC  ABEND 2,DUMP
*
** Variables
*
DOUBLE   DS    D
DOUBLE2  DS    D
PARMLIST DS    20F
         DS    0D
STCKCONI DS    D
STCKCONIE DS   2D
STCKCONO DS    CL16
*
CONVTODW DS    0F
CT_TIME  DS    XL4
         DS    XL4
CT_DATE  DS    XL4
         DS    XL4
*
MSG1     DC    CL80' '
         ORG   MSG1                     Redefine CL80 area
         DC    C'Assembler (Non-LE) --------------------------------'
         ORG   ,
MSG2     DC    CL80' '
MSG3     DC    CL80' '
         ORG   MSG3                     Redefine CL80 area
         DC    C'TIME macro returns: '
MSG3H    DC    C'xxxxxxxx'
         DC    C' - Formatted: '
MSG3C    DC    C'hh:mm:ss'
         DC    C' (local)'
         ORG   ,
MSG5     DC    CL80' '
         ORG   MSG5                     Redefine CL80 area
         DC    C'STCK - STCKCON macro returns: '
MSG5H    DC    C'xxxxxxxxyyyyyyyy'
         DC    C' - Formatted: '
MSG5C    DC    C'hh:mm:ss'
         DC    C' (local)'
         ORG   ,
MSG5B    DC    CL80' '
         ORG   MSG5B                    Redefine CL80 area
         DC    C'STCK returned: '
MSG5BV   DC    C'xxxxxxxx xxxxxxxx'
         DC    C' for local time'
         ORG   ,
MSG6     DC    CL80' '
         ORG   MSG6                     Redefine CL80 area
         DC    C'STCKE - STCKCON macro returns: '
MSG6H    DC    C'xxxxxxxxyyyyyyyy'
         DC    C' - Formatted: '
MSG6C    DC    C'hh:mm:ss'
         DC    C' (local)'
         ORG   ,
MSG6B    DC    CL80' '
         ORG   MSG6B                    Redefine CL80 area
         DC    C'STCKE returned: '
MSG6BV   DC    C'xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx'
         DC    C' for local time'
         ORG   ,
MSG10    DC    CL80' '
         ORG   MSG10                    Redefine CL80 area
         DC    C'STCK - STCKCON macro returns: '
MSG10H   DC    C'xxxxxxxxyyyyyyyy'
         DC    C' - Formatted: '
MSG10C   DC    C'hh:mm:ss'
         DC    C' (GMT)'
         ORG   ,
MSG10B   DC    CL80' '
         ORG   MSG10B                   Redefine CL80 area
         DC    C'STCK converted to: '
MSG10BV  DC    C'xxxxxxxx xxxxxxxx'
         DC    C' for GMT time'
         ORG   ,
*
MSG7     DC    CL80'Environment information:'
MSG8     DC    CL80'- Date Assembled...........&ASMDATE.'
MSG9     DC    CL80'- Assembler version........&SYSVER.'
*
*               0....+....1....+....2....+....
WTOMSG1  WTO   'Assembler (non-LE) returns: hh:mm:ss (via TIME SVC opti@
               on - DEC option)',MF=L
WTOMSG2  WTO   'Assembler (non-LE) returns: hh:mm:ss (via STCK)',      @
               MF=L
WTOMSG3  WTO   'Assembler (non-LE) returns: hh:mm:ss (after STCK is con@
               verted via CVT TimeZone)',MF=L
*
SYSPRINT DCB   DDNAME=SYSPRINT,                                        @
               DSORG=PS,MACRF=PM,RECFM=F,LRECL=80
*
         LTORG ,
         DS    0D
TIMEASML EQU   *-TIMEASM                Trivia about module length
         END   ,
//L.SYSPRINT DD SYSOUT=Z <- non-output
//G.SYSPRINT DD SYSOUT=*
