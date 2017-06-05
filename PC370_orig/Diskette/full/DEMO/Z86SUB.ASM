         TITLE   'Z86SUB - 8086 USER EXIT'
;
; PGMID. Z86SUB.ASM LINKED TO Z86SUB.EXE
; AUTHOR. DON HIGGINS
; DATE.   04/12/87 CONVERTED TO MASM V4 STD. EXE CONVENTIONS WITH FAR EXIT
;
CODE     SEGMENT PUBLIC
         ASSUME  CS:CODE
PRTMSG   PROC    FAR
         PUSH  DS    ;SAVE DS
         PUSH  CS
         POP   DS    ;MOVE CS TO DS
         MOV   DX,OFFSET MSG
         MOV   AH,9
         INT   21H
         POP   DS    ;RESTORE DS
         RET
PRTMSG   ENDP
MSG      DB    'HELLO FROM Z86 SUB',0DH,0AH,'$'
CODE     ENDS
         END
