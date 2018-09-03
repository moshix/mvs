*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
* *
* ICETOOL SYMBOL MAP FOR SMF RECORD TYPE 65 *
* *
* SOURCE DOCUMENTATION: SA22-7630-7 *
* LOCAL SYMBOLS ADDED WHERE APPROPRIATE *
*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
POSITION,19
SMF65SBS,*,4,BI                         RESERVED
SMF65SUB,*,2,CH                         THE ACTION TAKEN ON THE CATALOG
*
*
*
*
 SMF65SUB_IN,C'IN'                      INSDERT
 SMF65SUB_DE,C'IN'                      DELETE
 SMF65SUB_UP,C'IN'                      UPDATE
SMF65POF,*,4,BI                          OFFSET OF PRODUCT SECTION F
*                                       START OF THE RECORD, INCLUDI
*                                       LENGTH OF THE PRODUCT SECTIO
SMF65PLN,*,2,BI
SMF65PNO,*,2,BI                         NUMBER OF PRODUCT SECTIONS
SMF65DOF,*,4,BI                          OFFSET OF DATA SECTION FROM
*                                      START OF THE RECORD, INCLUDING
*                                      LENGTH OF THE DATA SECTION
SMF65DLN,*,2,BI
SMF65DNO,*,2,BI                        NUMBER OF DATA SECTIONS
SMF65VER,*,2,CH                        VERSION OF THE TYPE 65 RECORD
SMF65PNM,*,8,CH
*                                      CATALOG MANAGEMENT PRODUCT ID
SMF65JNM_1,*,1,CH                      *** LOCAL SYMBOL ***
SMF65JNM_2,=,2,CH                      *** LOCAL SYMBOL ***
SMF65JNM_3,=,3,CH                      *** LOCAL SYMBOL ***
SMF65JNM_4,=,4,CH                      *** LOCAL SYMBOL ***
SMF65JNM_5,=,5,CH                      *** LOCAL SYMBOL ***
SMF65JNM_6,=,6,CH                      *** LOCAL SYMBOL ***
SMF65JNM_7,=,7,CH                      *** LOCAL SYMBOL ***
SMF65JNM,=,8,CH                        JOB NAME. THE JOB LOG IDENTIF
*                                      CATION CONSISTS OF THE THE JO
*                                      NAME, TIME AND DATE THAT THE
*                                      READER RECOGNIZED THE JOB CAR
*                                      (FOR THIS JOB). IF A SYSTEM T
*                                      CAUSED THE RECORD TO BE WRITT
*                                      THE JOB NAME AND USER IDENTIF
*                                      CATION FIELDS CONTAIN BLANKS
*                                      THE TIME AND DATE FIELDS CONT
*                                      ZEROS.
SMF65RST,*,4,BI                        TIME IN HUNDREDTHS OF A SECON
*                                      THAT THE READER RECOGNIZED TH
*                                      JOB CARD (FOR THIS JOB).
SMF65RDT,*,4,DT1                       DATE WHEN THE READER RECOGNIZ
*                                       THE JOB CARD (FOR THIS JOB),
*                                       THE FORM 0CYYDDDF.
SMF65UID,*,8,CH                        USER-DEFINED IDENTIFICATION
*                                     FIELD (TAKEN FROM COMMON EXIT
*                                     PARAMETER AREA, NOT FROM USER=
*                                     PARAMETER ON JOB STATEMENT).
SMF65FNC,*,1,CH                      CONTAINS 'S' IF A DATASET WAS
*                                   SCRATCHED; 'U' IF ONLY CATALOG
*                                   ENTRIES WERE MODIFIED.
 SMF65FNC_S,C'S'                         SCRATCHED
 SMF65FNC_U,C'U'                     AME OF THE CATALOG IN WHICH
SMF65CNM,*,44,CH
*                                     RECORD WAS UPDATED OR DELETED.
SMF65TYP,*,1,CH                          ENTRY TYPE IDENTIFIER.
 SMF65TYP_A,C'A'                         NON-VSAM DATASET
 SMF65TYP_B,C'B'                         GENERATION DATA GROUP BASE
 SMF65TYP_C,C'C'                         CLUSTER
 SMF65TYP_D,C'D'                         DATASET
 SMF65TYP_E,C'E'                         VSAM EXTENSION RECORD
 SMF65TYP_F,C'F'                         FREE SPACE
 SMF65TYP_G,C'G'                         ALTERNATE INDEX
 SMF65TYP_H,C'H'                         ACTIVE GENERATION DATASET (GDS)
*                                       ENTRY IN GDG BASE
 SMF65TYP_I,C'I'                         INDEX
 SMF65TYP_J,C'J'                         GDG EXTENSION RECORD
 SMF65TYP_K,C'K'                         VSAM VOLUME RECORD (VVR)
 SMF65TYP_L,C'L'                         LIBRARY CONTROL SYSTEM LIBRARY
*                                      RECORD
 SMF65TYP_M,C'M'                         MASTER CATALOG
 SMF65TYP_N,C'N'                         ON-VSAM HEADER RECORD
 SMF65TYP_O,C'O'                         OBJECT ACCESS METHOD (OAM) NON-
*                                       VSAM RECORD
 SMF65TYP_P,C'P'                         PAGE SPACE
 SMF65TYP_Q,C'Q'                         VVR HEADER
 SMF65TYP_R,C'R'                         PATH
 SMF65TYP_T,C'T'                          TRUE NAME RECORD
 SMF65TYP_U,C'U'                          USER CATALOG
 SMF65TYP_V,C'V'                          VOLUME
 SMF65TYP_W,C'W'                          LIBRARY CONTROL SYSTEM VOLUM
 SMF65TYP_X,C'X'                          ALIAS
 SMF65TYP_Y,C'Y'                          UPGRADE
 SMF65TYP_Z,C'Z'                          VVR HEADER PRIMARY
 SMF65TYP_0,X'00'                        NORMAL NON-VSAM RECORD
 SMF65TYP_1,C'01'                        JES3 RECORD
SMF65ENM_01,*,01,CH                      LOCAL SYMBOL
SMF65ENM_02,=,02,CH                      LOCAL SYMBOL
SMF65ENM_03,=,03,CH                      LOCAL SYMBOL
SMF65ENM_04,=,04,CH                      LOCAL SYMBOL
SMF65ENM_05,=,05,CH                      LOCAL SYMBOL
SMF65ENM_06,=,06,CH                      LOCAL SYMBOL
SMF65ENM_07,=,07,CH                      LOCAL SYMBOL
SMF65ENM_08,=,08,CH                      LOCAL SYMBOL
SMF65ENM_09,=,09,CH                      LOCAL SYMBOL
SMF65ENM_10,=,10,CH                      LOCAL SYMBOL
SMF65ENM_11,=,11,CH                      LOCAL SYMBOL
SMF65ENM_12,=,12,CH
SMF65ENM_13,=,13,CH                      LOCAL SYMBOL
SMF65ENM_14,=,14,CH                      LOCAL SYMBOL
SMF65ENM_15,=,15,CH                      LOCAL SYMBOL
SMF65ENM_16,=,16,CH                      LOCAL SYMBOL
SMF65ENM_17,=,17,CH                      LOCAL SYMBOL
SMF65ENM_18,=,18,CH                      LOCAL SYMBOL
SMF65ENM_19,=,19,CH                      LOCAL SYMBOL
SMF65ENM_20,=,20,CH                      LOCAL SYMBOL
SMF65ENM,=,44,CH                         ENTRY NAME
SMF65NNM,*,44,CH                         RESERVED.
SMF65CRC,*,2,BI                          ATALOG RECORD FOR UPDATED O
*                                        DELETED ENTRY (THE LENGTH O
*                                        THIS RECORD IS CONTAINED IN
*                                        FIRST TWO BYTES OF THIS FIE

