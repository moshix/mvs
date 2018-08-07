SMFHEADR ICETOOL SYMBOL FILE
*---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+----*
* *
* ICETOOL SYMBOL MAP FOR SMF RECORD HEADER *
* *
* This layout will accommodate both types of SMF records, those with *
* subtypes and those without. If the individual record layout is for*
 34 © 2005. Xephon USA telephone (214) 340 5690, fax (214) 341 7081.
* a record without subtypes, the symbol definition for the actual SMF*
* record should begin with a POSITION,19 directive. *
* *
* Source documentation: SA22-763Ø-7 *
* Local symbols added where appropriate *
*---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+----*
SMF_RDW,1,4,BI Record descriptor word
SMFXLEN,=,2,BI Record length
SMFXSEG,*,2,BI Segment length
SMFXFLG,*,1,BI Flag byte
SMFXRTY,*,1,BI SMF record type
 SMFØ17,17 *** Local symbol ***
 SMFØ18,18 *** Local symbol ***
 SMFØ21,21 *** Local symbol ***
 SMFØ65,65 *** Local symbol ***
 SMFØ66,66 *** Local symbol ***
SMFXTME,*,4,BI Time since midnight in
* hundredths of a second, since
* the record was moved into the
* SMF buffer
SMFXDTE,*,4,PD Date when the record was moved
SMFXDTE_DT1,=,4,DT1 Format the date for reporting
* into the SMF buffer, in the form
* ØcyydddF, where c is Ø for 19xx
* and c is 1 for 2Øxx.
SMFXSID,*,4,CH System identifier
SMFXSSI,*,4,CH Subsystem identifier
SMFXSTY,*,2,BI Subtype indicator
SMF65 ICETOOL SYMBOL FILE
*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
* *
* ICETOOL SYMBOL MAP FOR SMF RECORD TYPE 65 *
* *
* Source documentation: SA22-763Ø-7 *
* Local symbols added where appropriate *
*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
POSITION,19
SMF65SBS,*,4,BI Reserved
SMF65SUB,*,2,CH The action taken on the catalog
* entry, valid values are:
* IN ( Insert )
* DE ( Delete )
* UP ( Update )
 SMF65SUB_IN,C'IN' Insert
 SMF65SUB_DE,C'IN' Delete
 SMF65SUB_UP,C'IN' Update
SMF65POF,*,4,BI Offset of product section from
© 2005. Reproduction prohibited. Please inform Xephon of any infringement. 35
* start of the record, including
* the RDW.
SMF65PLN,*,2,BI Length of the product section
SMF65PNO,*,2,BI Number of product sections
SMF65DOF,*,4,BI Offset of data section from
* start of the record, including
* the RDW.
SMF65DLN,*,2,BI Length of the data section
SMF65DNO,*,2,BI Number of data sections
SMF65VER,*,2,CH Version of the type 65 record
SMF65PNM,*,8,CH Catalog management product
* identifier.
SMF65JNM_1,*,1,CH *** Local symbol ***
SMF65JNM_2,=,2,CH *** Local symbol ***
SMF65JNM_3,=,3,CH *** Local symbol ***
SMF65JNM_4,=,4,CH *** Local symbol ***
SMF65JNM_5,=,5,CH *** Local symbol ***
SMF65JNM_6,=,6,CH *** Local symbol ***
SMF65JNM_7,=,7,CH *** Local symbol ***
SMF65JNM,=,8,CH Job name. The job log identifi-
* cation consists of the the job
* name, time and date that the
* reader recognized the JOB card
* (for this job). If a system task
* caused the record to be written,
* the job name and user identifi-
* cation fields contain blanks and
* the time and date fields contain
* zeros.
SMF65RST,*,4,BI Time in hundredths of a second,
* that the reader recognized the
* JOB card (for this job).
SMF65RDT,*,4,DT1 Date when the reader recognized
* the JOB card (for this job), in
* the form ØcyydddF.
SMF65UID,*,8,CH User-defined identification
* field (taken from common exit
* parameter area, not from USER=
* parameter on job statement).
SMF65FNC,*,1,CH Contains 'S' if a dataset was
* scratched; 'U' if only catalog
* entries were modified.
 SMF65FNC_S,C'S' Scratched
 SMF65FNC_U,C'U' Catalog entries modified.
SMF65CNM,*,44,CH Name of the catalog in which
* record was updated or deleted.
SMF65TYP,*,1,CH Entry type identifier.
 SMF65TYP_A,C'A' Non-VSAM dataset
 SMF65TYP_B,C'B' Generation data group base
 SMF65TYP_C,C'C' Cluster
 36 © 2005. Xephon USA telephone (214) 340 5690, fax (214) 341 7081.
 SMF65TYP_D,C'D' Dataset
 SMF65TYP_E,C'E' VSAM extension record
 SMF65TYP_F,C'F' Free space
 SMF65TYP_G,C'G' Alternate index
 SMF65TYP_H,C'H' Active generation dataset (GDS)
* entry in GDG base
 SMF65TYP_I,C'I' Index
 SMF65TYP_J,C'J' GDG extension record
 SMF65TYP_K,C'K' VSAM volume record (VVR)
 SMF65TYP_L,C'L' Library control system library
* record
 SMF65TYP_M,C'M' Master catalog
 SMF65TYP_N,C'N' Non-VSAM header record
 SMF65TYP_O,C'O' Object Access Method (OAM) non-
* VSAM record
 SMF65TYP_P,C'P' Page space
 SMF65TYP_Q,C'Q' VVR header
 SMF65TYP_R,C'R' Path
 SMF65TYP_T,C'T' True name record
 SMF65TYP_U,C'U' User catalog
 SMF65TYP_V,C'V' Volume
 SMF65TYP_W,C'W' Library control system volume
 SMF65TYP_X,C'X' Alias
 SMF65TYP_Y,C'Y' Upgrade
 SMF65TYP_Z,C'Z' VVR header primary
 SMF65TYP_Ø,X'ØØ' Normal non-VSAM record
 SMF65TYP_1,C'Ø1' JES3 record
SMF65ENM_Ø1,*,Ø1,CH Local symbol
SMF65ENM_Ø2,=,Ø2,CH Local symbol
SMF65ENM_Ø3,=,Ø3,CH Local symbol
SMF65ENM_Ø4,=,Ø4,CH Local symbol
SMF65ENM_Ø5,=,Ø5,CH Local symbol
SMF65ENM_Ø6,=,Ø6,CH Local symbol
SMF65ENM_Ø7,=,Ø7,CH Local symbol
SMF65ENM_Ø8,=,Ø8,CH Local symbol
SMF65ENM_Ø9,=,Ø9,CH Local symbol
SMF65ENM_1Ø,=,1Ø,CH Local symbol
SMF65ENM_11,=,11,CH Local symbol
SMF65ENM_12,=,12,CH Local symbol
SMF65ENM_13,=,13,CH Local symbol
SMF65ENM_14,=,14,CH Local symbol
SMF65ENM_15,=,15,CH Local symbol
SMF65ENM_16,=,16,CH Local symbol
SMF65ENM_17,=,17,CH Local symbol
SMF65ENM_18,=,18,CH Local symbol
SMF65ENM_19,=,19,CH Local symbol
SMF65ENM_2Ø,=,2Ø,CH Local symbol
SMF65ENM,=,44,CH Entry name.
SMF65NNM,*,44,CH Reserved.
SMF65CRC,*,2,BI Catalog record for updated or
© 2005. Reproduction prohibited. Please inform Xephon of any infringement. 37
* deleted entry (the length of
* this record is contained in the
* first two bytes of this field).
SMF66 ICETOOL SYMBOL FILE
*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
* *
* ICETOOL SYMBOL MAP FOR SMF RECORD TYPE 66 *
* *
* Source documentation: SA22-763Ø-7 *
* Local symbols added where appropriate *
*---+---+---+---+---+---+---+---+---+---+---+---+---+---*
POSITION,19
SMF66SBS,*,4,BI Reserved
SMF66SUB,*,2,CH The action taken on the catalog
* entry, valid values are:
* IN ( Insert )
* DE ( Delete )
* UP ( Update )
 SMF66SUB_IN,C'IN' Insert
 SMF66SUB_DE,C'IN' Delete
 SMF66SUB_UP,C'IN' Update
SMF66POF,*,4,BI Offset of product section from
* start of the record, including
* the RDW.
SMF66PLN,*,2,BI Length of the product section
SMF66PNO,*,2,BI Number of product sections
SMF66DOF,*,4,BI Offset of data section from
* start of the record, including
* the RDW.
SMF66DLN,*,2,BI Length of the data section
SMF66DNO,*,2,BI Number of data sections
SMF66VER,*,2,CH Version of the type 65 record
SMF66PNM,*,8,CH Catalog management product
* identifier.
SMF66JNM_1,*,1,CH *** Local symbol ***
SMF66JNM_2,=,2,CH *** Local symbol ***
SMF66JNM_3,=,3,CH *** Local symbol ***
SMF66JNM_4,=,4,CH *** Local symbol ***
SMF66JNM_5,=,5,CH *** Local symbol ***
SMF66JNM_6,=,6,CH *** Local symbol ***
SMF66JNM_7,=,7,CH *** Local symbol ***
SMF66JNM,=,8,CH Job name. The job log identifi-
* cation consists of the the job
* name, time, and date that the
* reader recognized the JOB card
* (for this job). If a system task
* caused the record to be written,
 38 © 2005. Xephon USA telephone (214) 340 5690, fax (214) 341 7081.
* the job name and user identifi-
* cation fields contain blanks and
* the time and date fields contain
* zeros.
SMF66RST,*,4,BI Time in hundredths of a second,
* that the reader recognized the
* JOB cad (for this job).
SMF66RDT,*,4,DT1 Date when the reader recognized
* the JOB card (for this job), in
* the form ØcyydddF.
SMF66UID,*,8,CH User-defined identification
* field (taken from common exit
* parameter area, not from USER=
* parameter on job statement).
SMF66FNC,*,1,CH Contains 'R' if catalog entry is
* renamed.
 SMF66FNC_R,C'R' Renamed
 SMF66FNC_U,C'U' Catalog entries modified.
SMF66CNM,*,44,CH Name of the catalog in which
* record was updated or deleted.
SMF66TYP,*,1,CH Entry type identifier.
 SMF66TYP_A,C'A' Non-VSAM dataset
 SMF66TYP_B,C'B' Generation data group base
 SMF66TYP_C,C'C' Cluster
 SMF66TYP_D,C'D' Dataset
 SMF66TYP_E,C'E' VSAM extension record
 SMF66TYP_F,C'F' Free space
 SMF66TYP_G,C'G' Alternate index
 SMF66TYP_H,C'H' Active generation dataset (GDS)
* entry in GDG base
 SMF66TYP_I,C'I' Index
 SMF66TYP_J,C'J' GDG extension record
 SMF66TYP_K,C'K' VSAM volume record (VVR)
 SMF66TYP_L,C'L' Library control system library
* record
 SMF66TYP_M,C'M' Master catalog
 SMF66TYP_N,C'N' Non-VSAM header record
 SMF66TYP_O,C'O' Object Access Method (OAM) non-
* VSAM record
 SMF66TYP_P,C'P' Page space
 SMF66TYP_Q,C'Q' VVR header
 SMF66TYP_R,C'R' Path
 SMF66TYP_T,C'T' True name record
 SMF66TYP_U,C'U' User catalog
 SMF66TYP_V,C'V' Volume
 SMF66TYP_W,C'W' Library control system volume
 SMF66TYP_X,C'X' Alias
 SMF66TYP_Y,C'Y' Upgrade
 SMF66TYP_Z,C'Z' VVR header primary
 SMF66TYP_Ø,X'ØØ' Normal non-VSAM record