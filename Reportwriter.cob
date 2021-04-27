//HERC01S  JOB (COBOL),                                                 
//             'PARYOLL REPORT',                                        
//             CLASS=A,                                                 
//             MSGCLASS=A,                                              
//             REGION=8M,TIME=1440,                                     
//             MSGLEVEL=(1,1)                                           
//SAL      EXEC COBUCG,                                                 
//         PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'            
//COB.SYSPUNCH DD DUMMY                                                 
//COB.SYSIN    DD *                                                     
  150 ***   PAYROLL REPORT PROGRAM                                      
  160 ***   MVT COBOL COMPILER 1972                                     
  170 ***   APRIL 26, 2021                                              
  175 ***                                                               
  180  IDENTIFICATION DIVISION.                                         
  200  PROGRAM-ID. SALARIES.                                            
  210  DATE-WRITTEN. APRIL 26, 2021.                                    
  220  DATE-COMPILED. APRIL 27, 2021.                                   
  300  ENVIRONMENT DIVISION.                                            
 1200  CONFIGURATION SECTION.                                           
 1210  SOURCE-COMPUTER. IBM-370.                                        
 1220  OBJECT-COMPUTER. IBM-370.                                        
 1290                                                                   
 1300  INPUT-OUTPUT SECTION.                                            
 1400  FILE-CONTROL.                                                    
 1420        SELECT SALARIES ASSIGN TO DA-S-INDD.                       
 1500        SELECT REPORT-FILE ASSIGN TO UR-S-SYSPRINT.                
 1600                                                                   
 2400  DATA DIVISION.                                                   
 2500  FILE SECTION.                                                    
 2600  FD   SALARIES                                                    
 2800       BLOCK CONTAINS 0 RECORDS                                    
 3000       DATA RECORD IS SALARIES-REC.                                
 3010                                                                   
 3100  01    SALARIES-REC.   
3100  01    SALARIES-REC.                                           
3300        05 EMPLOYEE-ID PIC 9(10).                               
3400        05 NAME PIC A(23).                                      
3500        05 SALARY PIC  9(4).                                    
3501        05 RESERVED PIC A(43).                                  
3510                                                                
3520  FD    REPORT-FILE                                             
3530        LABEL RECORDS ARE OMITTED                               
3540        REPORT IS SALARIES-REPORT.                              
3590                                                                
3600     WORKING-STORAGE SECTION.                                   
3601     77  TOTAL                       PIC 9(9)    VALUE ZERO.    
3605     77  TAX                         PIC 9(9)    VALUE ZERO.    
3606     77  ONLY-TAX                    PIC 9(9)    VALUE ZERO.    
3610     77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.     
3620     88  END-OF-FILE                         VALUE 'Y'.         
3690                                                                
3990                                                                
4000  REPORT SECTION.                                               
4100  RD   SALARIES-REPORT                                          
4110       PAGE LIMIT IS 66 LINES                                   
4120       HEADING 1                                                
4130       FIRST DETAIL 5                                           
4140       LAST DETAIL 58.                                          
4150                                                                
4160  01   PAGE-HEAD-GROUP TYPE PAGE HEADING.                       
4170  02   LINE 1.                                                  
4180       03  COLUMN 39   PIC X(47) VALUE                          
4190           'P A Y R O L L   R E P O R T  -  B I M  C O R P.'.   
4200  02   LINE PLUS 2.                                             
4210       03  COLUMN 01   PIC X(08) VALUE 'EMPL.ID  '.             
4220       03  COLUMN 10   PIC X(25) VALUE 'EMPLOYEE NAME'.         
4330       03  COLUMN 41   PIC X(15) VALUE 'MONTHLY WAGE  '.        
4335       03  COLUMN 58   PIC X(09) VALUE 'WAGE TAX'.              
4336       03  COLUMN 76   PIC X(16) VALUE 'CUMULATIVE WAGES'.      
4337       05  COLUMN 100   PIC X(4) VALUE 'PAGE'. 
4338       10  COLUMN 106 PIC ZZ9 SOURCE PAGE-COUNTER.           
4350                                                             
4360  01   SALARY-DETAIL TYPE DETAIL.                            
4370       03  LINE PLUS 1.                                      
4380       03  COLUMN 01   PIC X(05) SOURCE EMPLOYEE-ID.         
4382       03  COLUMN 10   PIC X(25) SOURCE NAME.                
4383       03  COLUMN 41   PIC $9,999    SOURCE SALARY.          
4385       03  COLUMN 57   PIC $$$$$99   SOURCE ONLY-TAX.        
4386       03  COLUMN 75   PIC $$$$,$$9  SOURCE TOTAL.           
4399                                                             
5000  PROCEDURE DIVISION.                                        
5010  000-INITIATE.                                              
5020                                                             
5100        OPEN INPUT SALARIES.                                 
5200        OPEN OUTPUT REPORT-FILE.                             
5210                                                             
5220        INITIATE SALARIES-REPORT.                            
5230                                                             
5300        READ SALARIES                                        
5400         AT END MOVE 'Y' TO END-OF-FILE-SWITCH.              
5405                                                             
5500     END-READS.                                              
5510                                                             
5600        PERFORM 100-PROCESS-TRANSACTION-DATA THRU 199-EXIT   
5610          UNTIL END-OF-FILE.                                 
5620                                                             
5630  000-TERMINATE.                                             
5640      TERMINATE SALARIES-REPORT.                             
5650                                                             
5660      CLOSE SALARIES, REPORT-FILE.                           
5670                                                             
6000       STOP RUN.                                             
6010                                      
6100  100-PROCESS-TRANSACTION-DATA.                              
6102         MULTIPLY SALARY BY 1.43 GIVING TAX.    
 6102         MULTIPLY SALARY BY 1.43 GIVING TAX.               
 6103         SUBTRACT SALARY FROM TAX GIVING ONLY-TAX.         
 6104         ADD SALARY  TO TOTAL.                             
 6105         ADD ONLY-TAX TO TOTAL.                            
 6200         GENERATE SALARY-DETAIL.                           
 6300         READ SALARIES                                     
 6400            AT END                                         
 6500                MOVE 'Y' TO END-OF-FILE-SWITCH.            
 6506                                                           
 6600     END-READ.                                             
 6610                                                           
 6620  199-EXIT.                                                
 6650         EXIT.                                             
 6660                                                           
/*                                                              
//COB.SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR                   
//SYSABEND    DD SYSOUT=*                                       
//GO.SYSUDUMP DD SYSOUT=*                                       
//GO.SYSPRINT DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=161) 
//GO.INDD     DD DSNAME=HERC01.SALARIES.INPUT,DISP=SHR          
//                                                                                            
 
