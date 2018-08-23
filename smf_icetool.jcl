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
     TITLE('WLM POSTPROCESSOR REPORT') DATE TIME -                 
      HEADER('TIME') ON(13,8,CH) -                                 
      HEADER('S.CLASS') ON(46,8,CH) -                              
      HEADER('PERIOD.') ON(55,1,CH) -                              
      HEADER('LOCAL PI') ON(57,3,CH) -                             
      HEADER('GOAL TYPE') ON(67,15,CH) -                           
      HEADER('GOAL VALUE') ON(83,3,CH) -                           
      HEADER('IMP') ON(87,1,CH) -                                  
      HEADER('PERC.') ON(90,2,CH) -                                
      HEADER('CPU DP') ON(93,3,CH) -                               
      HEADER('IO DP') ON(97,3,CH) -                                
      BREAK(1,11,CH) -                                             
      BTITLE('DAILY REPORT') -                                     
  BLANK                                                            
/*                                                                 
//                                                                 
