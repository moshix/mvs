//HERC01A  JOB (BAL),                                               
//             'PARAMETER CALLING  ',                               
//             CLASS=A,                                             
//             MSGCLASS=X,                                          
//             TIME=1440,                                           
//             MSGLEVEL=(1,1)                                       
//ASMCLG  EXEC ASMFCG,PARM.ASM=(OBJ,NODECK),MAC1='SYS2.MACLIB',     
//             REGION.GO=428K,PARM.GO='/12345'                      
//ASM.SYSLIB   DD                                                   
//             DD DISP=SHR,DSN=SYS1.MACLIB                          
//ASM.SYSIN DD *                                                    
            PRINT GEN                                                
RDPARM      CSECT                                                    
            STM    R14,R12,12(R13)     SAVE CALLERS REGISTERS        
            BALR   R12,0               COPY BASE REG                 
            USING  *,R12               TELL ASSEMBLER BASE REG       
            ST     R13,SAVEAREA+4      SAVE CALLER SAP               
            LA     R13,SAVEAREA        POINT TO ORU SAVE AREA        
*                                                                    
            L      R2,0(,R1)           GET PARAM FIELD ADDR          
            LH     R3,0(,R2)           GET LENGTH OF PARM FIELD      
            LA     2,2(,R2)            ADDRS OF FIRST BYTE OF FIELD  
*                                                                    
* PRINT ON CONSOLE LEGNTH OF PARAMETER FIELD                         
            CVD    R3,NUMDEC           FROM BINARY TO DECIMAL        
            UNPK   UNDEC,NUMDEC+1      MAKE HUMAN READABLE           
            MVC    WTOC+20(2),UNDEC    COPY WORKLOAD INTO NEXT INST  
WTOC        WTO    'PARM LENGTH       '                              
*                                                                    
            LR     R6,R3               COPY INTO R6                  
            LA     R7,1                VALUE OF 1 INTO R7            
            SR     R6,R7                                             
            LTR    R3,R3               WAS A PARAM SPECIFIED         
            BZ     NOPARM                                            
*                                      SO WE DO HAVE A PARAM         
            LA     R4,WTOL+15          ADDR OF WTO WITH PARMS        
*           MVC    0(R6,R4),0(R2)      COPY WORKLOAD INTO NEXT INST  
            EX     R6,MVCINS                                         
WTOL        WTO    'PARMS:      
            B CLOSE                                             
NOPARM      DS 0H                                               
            WTO 'NO PARAMETERS GIVEN BY CALLER'                 
CLOSE       L      R13,SAVEAREA+4      RELOAD CALLERS SA        
            LM     R14,R12,12(R13)     CALLERS REGS             
            SR     R15,R15             RESET TO RC TO ZERO      
            BR     R14                 RETURN TO MAMA           
*                                                               
            LTORG                                               
SAVEAREA    DS 18F                                              
NUMDEC      DS D                                                
UNDEC       DS PL4                                              
MVCINS      MVC 0(0,R4),0(R2)                                   
* EQUATES                                                       
R1    EQU 1                                                     
R2    EQU 2                                                     
R3    EQU 3                                                     
R4    EQU 4                                                     
R5    EQU 5                                                     
R6    EQU 6                                                     
R7    EQU 7                                                     
R8    EQU 8                                                     
R9    EQU 9                                                     
R10   EQU 10                                                    
R11   EQU 11                                                    
R12   EQU 12                                                    
R13   EQU 13                                                    
R14   EQU 14                                                    
R15   EQU 15                                                    
      END                                                       
/*                                                              
//                                                              '                                     
