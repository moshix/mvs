//MOSHIXA  JOB  NOTIFY=&SYSUID,MSGCLASS=H,REGION=100M,CLASS=A   
//S0010    EXEC PROC=ASMACLG,                                   
//             PARM.L='MAP,LET,LIST,RMODE(24)'                  
//C.SYSLIB DD                                                   
//         DD   DISP=SHR,DSN=CEE.SCEEMAC                        
//C.SYSIN  DD   DISP=SHR,DSN=MOSHIX.ADVASM.LABS(ASMSUB)         
//L.SYSLIB DD   DISP=SHR,DSN=CEE.SCEELKED                       
//G.SYSUDUMP DD SYSOUT=*                                        
//G.OUTDD  DD   SYSOUT=(,INTRDR)                                
//SYSPRINT DD   SYSOUT=*                                        
