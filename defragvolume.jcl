//XYZ2A  JOB  (ACCOUNT),'DEFRAG VOLUME',CLASS=A,MSGCLASS=H,
//    REGION=300M,NOTIFY=&SYSUID,MSGLEVEL=(1,1)
//**********************************************************
//** THIS WILL ONLY WORK ON MVS/ESA AND HIGHER VERSIONS
//** THERE IS NO ADRDSSU IN MVS 3.8 UNFORTUNATELY
//** DEFRAGGING IS SOMETIMES NECESSARY FOR HIGH TRAFFIC VOLUMES
//***********************************************************

//STEP1  EXEC  PGM=ADRDSSU,REGION=2048K                                         
//SYSPRINT DD  SYSOUT=*                                                        
//INVOL1   DD  VOL=SER=Z9DIS6,UNIT=3390,DISP=OLD                              
//SYSIN    DD    *     
  DEFRAG DDNAME(INVOL1) -                                              
     MAXMOVE(9999,2) PASSDELAY(10) -                              
     FASTREP(PREF) -                                              
     WAIT(2,3)  
/*
//
