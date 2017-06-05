*
* IHAPCB - I HAD A PC COMMUNICATIONS VECTOR TABLE BLOCK
*
* FOR MORE INFORMATION SEE SVC.DOC
*
IHAPCB   DSECT
PCVT     DC    C'PCVT'  IDENTIFIER REQUIRED BY PC/370 SVC 34
PCIN     DS    H        80X86 INTERRUPT NUMBER (0-255)
PCPF     DS    H        PF FLAGS REGISTER
PCAX     DS    H        AX
PCBX     DS    H        BX
PCCX     DS    H        CX
PCDX     DS    H        DX
PCDS     DS    H        DS
PCSI     DS    H        SI
PCES     DS    H        ES
PCDI     DS    H        DI
