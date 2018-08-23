//MOSHIXIC JOB 'FORMAT SMF RECORDS',REGION=5M,CLASS=A,              
//      MSGCLASS=H,NOTIFY=&SYSUID                                   
//SMF89    EXEC  PGM=ICETOOL                                        
//TOOLMSG  DD  SYSOUT=*                                             
//DFSMSG   DD  SYSOUT=*                                             
//VLSHCNTL DD  *                                                    
 OPTION COPY,VLSHRT                                                 
 INCLUDE COND=(6,1,BI,EQ,X'1E')                                     
/*                                                                  
//RAWSMF   DD  DISP=SHR,DSN=MOSHIX.SMFDUMP                          
//SORTSMF  DD  DISP=(NEW,DELETE,DELETE),SPACE=(CYL,(50,50,0))       
//VREPT    DD  SYSOUT=*                                             
//TOOLIN   DD  *                                                    
  SORT FROM(RAWSMF) TO(SORTSMF) USING(VLSH)                         
  DISPLAY FROM(SORTSMF) LIST(VREPT) -                               
     HEADER('SMFTYP')      ON(6,1,BI) -                             
     HEADER('DATE')        ON(11,4,DT1,E'9999/99/99') -             
     HEADER('TIME')        ON(7,4,TM1,E'99:99:99') -                
     HEADER('SYSTEM')      ON(15,4,CH) -                            
     HEADER('SMF#') ON(6,1,BI) -                                    
     HEADER('JOBNAME') ON(55,8,CH) -                                
     HEADER('USERID') ON(23,8,CH) -                                 
  BLANK                                                             
/*                                                                  
//                                                                  
