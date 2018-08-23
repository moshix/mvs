//MOSHIXFR JOB 'DUMP SMF RECORDS',REGION=8M,CLASS=A,                 
//      MSGCLASS=H,NOTIFY=&SYSUID                                    
//SMFREP   EXEC PGM=IFASMFDP                                         
//SYSPRINT  DD  SYSOUT=*                                             
//IN        DD  DISP=SHR,DSN=SYS1.MAN1                               
//*IN        DD  DISP=SHR,DSN=SYS1.MAN2                              
//*IN        DD  DISP=SHR,DSN=SYS1.MAN3                              
//OUT  DD  DSN=MOSHIX.SMFDUMP,DISP=(,CATLG,DELETE),UNIT=SYSALLDA,    
// SPACE=(CYL,(190,1),RLSE),BUFNO=20,VOL=SER=ZDKAN1,                 
//  BLKSIZE=27998,LRECL=32760,RECFM=VBS                              
//SYSIN DD *                                                         
    DATE(2001001,2018240)                                            
    START(0001)                                                      
    END(2359)                                                        
    INDD(IN,OPTIONS(DUMP))                                           
    OUTDD(OUT,TYPE(00:254))                                          
/*                                                                   
//                                                                   
