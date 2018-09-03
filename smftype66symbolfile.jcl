*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
* *
* ICETOOL SYMBOL MAP FOR SMF RECORD TYPE 66 *
* *
* SOURCE DOCUMENTATION: SA22-7630-7 *
* LOCAL SYMBOLS ADDED WHERE APPROPRIATE *
*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
POSITION,19
SMF66SBS,*,4,BI                    BI RESERVED
SMF66SUB,*,2,CH                    CH THE ACTION TAKEN ON THE CATALOG
*                                   ENTRY, VALID VALUES ARE:
*                                   IN ( INSERT )
*                                   DE ( DELETE )
*                                          DATE )
 SMF66SUB_IN,C'IN'                   INSERT
 SMF66SUB_DE,C'IN'                   DELETE
 SMF66SUB_UP,C'IN'                   UPDATE
SMF66POF,*,4,BI                     OFFSET OF PRODUCT SECTION FROM
*                                   START OF THE RECORD, INCLUDIN
*                                   THE RDW.
SMF66PLN,*,2,BI                    LENGTH OF THE PRODUCT SECTION
SMF66PNO,*,2,BI                    NUMBER OF PRODUCT SECTIONS
SMF66DOF,*,4,BI                    OFFSET OF DATA SECTION FROM
*                                   START OF THE RECORD, INCLUDING
*                                   THE RDW.
SMF66DLN,*,2,BI                   LENGTH OF THE DATA SECTION
SMF66DNO,*,2,BI                   NUMBER OF DATA SECTIONS
SMF66VER,*,2,CH                   VERSION OF THE TYPE 65 RECORD
SMF66PNM,*,8,CH                   CATALOG MANAGEMENT PRODUCT
*                                 IDENTIFIER.
SMF66JNM_1,*,1,CH                 *** LOCAL SYMBOL ***
SMF66JNM_2,=,2,CH                 *** LOCAL SYMBOL ***
SMF66JNM_3,=,3,CH                 *** LOCAL SYMBOL ***
SMF66JNM_4,=,4,CH                 *** LOCAL SYMBOL ***
SMF66JNM_5,=,5,CH                 *** LOCAL SYMBOL ***
SMF66JNM_6,=,6,CH                 *** LOCAL SYMBOL ***
SMF66JNM_7,=,7,CH                 *** LOCAL SYMBOL ***
SMF66JNM,=,8,CH                   JOB NAME. THE JOB LOG IDENTIFI-
*                                 CATION CONSISTS OF THE THE JOB
*                                 NAME, TIME, AND DATE THAT THE
*                                 READER RECOGNIZED THE JOB CARD
*                                 (FOR THIS JOB). IF A SYSTEM TASK
*                                 CAUSED THE RECORD TO BE WRITTEN,
*                                 THE JOB NAME AND USER IDENTIFI-
*                                 CATION FIELDS CONTAIN BLANKS AND
*                                 THE TIME AND DATE FIELDS CONTAIN
*                                 ZEROS.
SMF66RST,*,4,BI                   TIME IN HUNDREDTHS OF A SECOND,
*                                 THAT THE READER RECOGNIZED THE
*                                 JOB CAD (FOR THIS JOB).
SMF66RDT,*,4,DT1                  DATE WHEN THE READER RECOGNIZED
*                                 THE JOB CARD (FOR THIS JOB), IN
*                                 THE FORM 0CYYDDDF.
SMF66UID,*,8,CH                   USER-DEFINED IDENTIFICATION
*                                 FIELD (TAKEN FROM COMMON EXIT
*                                 PARAMETER AREA, NOT FROM USER
*                                 PARAMETER ON JOB STATEMENT).
SMF66FNC,*,1,CH                   CONTAINS 'R' IF CATALOG ENTRY IS
*                                 RENAMED.
 SMF66FNC_R,C'R'                   RENAMED
 SMF66FNC_U,C'U'                   CATALOG ENTRIES MODIFIED.
SMF66CNM,*,44,CH                   NAME OF THE CATALOG IN WHICH
*                                 RECORD WAS UPDATED OR DELETED.
SMF66TYP,*,1,CH                   ENTRY TYPE IDENTIFIER.
 SMF66TYP_A,C'A'                   NON-VSAM DATASET
 SMF66TYP_B,C'B'                   GENERATION DATA GROUP BASE
 SMF66TYP_C,C'C'                   CLUSTER
 SMF66TYP_D,C'D'                   DATASET
 SMF66TYP_E,C'E'                   VSAM EXTENSION RECORD
 SMF66TYP_F,C'F'                   FREE SPACE
 SMF66TYP_G,C'G'                   ALTERNATE INDEX
 SMF66TYP_H,C'H'                   ACTIVE GENERATION DATASET (GDS)
*                                ENTRY IN GDG BASE
 SMF66TYP_I,C'I'                  INDEX
 SMF66TYP_J,C'J'                  GDG EXTENSION RECORD
 SMF66TYP_K,C'K'                  VSAM VOLUME RECORD (VVR)
 SMF66TYP_L,C'L'                  LIBRARY CONTROL SYSTEM LIBRAR
*                                RECORD
 SMF66TYP_M,C'M'                  MASTER CATALOG
 SMF66TYP_N,C'N'                  NON-VSAM HEADER RECORD
 SMF66TYP_O,C'O'                  OBJECT ACCESS METHOD (OAM) NO
*                                VSAM RECORD
 SMF66TYP_P,C'P'                  PAGE SPACE
 SMF66TYP_Q,C'Q'                  VVR HEADER
 SMF66TYP_R,C'R'                  PATH
 SMF66TYP_T,C'T'                  TRUE NAME RECORD
 SMF66TYP_U,C'U'                  USER CATALOG
 SMF66TYP_V,C'V'                  VOLUME
 SMF66TYP_W,C'W'                  LIBRARY CONTROL SYSTEM VOLUME
 SMF66TYP_X,C'X'                  ALIAS
 SMF66TYP_Y,C'Y'                  UPGRADE
 SMF66TYP_Z,C'Z'                  VVR HEADER PRIMARY
 SMF66TYP_0,X'00'                  NORMAL NON-VSAM RECORD
 SMF66TYP_1,C'01'                 JES3 RECORD
SMF66ENM_01,*,01,CH                LOCAL SYMBOL
SMF66ENM_02,=,02,CH                LOCAL SYMBOL
SMF66ENM_03,=,03,CH                LOCAL SYMBOL
SMF66ENM_04,=,04,CH                LOCAL SYMBOL
SMF66ENM_05,=,05,CH                LOCAL SYMBOL
SMF66ENM_06,=,06,CH                LOCAL SYMBOL
SMF66ENM_07,=,07,CH                LOCAL SYMBOL
SMF66ENM_08,=,08,CH                LOCAL SYMBOL
SMF66ENM_09,=,09,CH                LOCAL SYMBOL
SMF66ENM_10,=,10,CH                LOCAL SYMBOL
SMF66ENM_11,=,11,CH                LOCAL SYMBOL
SMF66ENM_12,=,12,CH                LOCAL SYMBOL
SMF66ENM_13,=,13,CH                LOCAL SYMBOL
SMF66ENM_14,=,14,CH                LOCAL SYMBOL
SMF66ENM_15,=,15,CH                LOCAL SYMBOL
SMF66ENM_16,=,16,CH                LOCAL SYMBOL
SMF66ENM_17,=,17,CH                LOCAL SYMBOL
SMF66ENM_18,=,18,CH                LOCAL SYMBOL
SMF66ENM_19,=,19,CH                LOCAL SYMBOL
SMF66ENM_20,=,20,CH                LOCAL SYMBOL
SMF66ENM,=,44,CH                    ENTRY NAME.
SMF66NNM,*,44,CH                    RESERVED.
SMF66CRC,*,2,BI                    CATALOG RECORD FOR UPDATED OR
*                                  DELETED ENTRY (THE LENGTH OF
*                                  THIS RECORD IS CONTAINED IN THE

