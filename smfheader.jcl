*---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+----*
* *
* ICETOOL SYMBOL MAP FOR SMF RECORD HEADER *
* *
* THIS LAYOUT WILL ACCOMMODATE BOTH TYPES OF SMF RECORDS, THOSE WITH *
* SUBTYPES AND THOSE WITHOUT. IF THE INDIVIDUAL RECORD LAYOUT IS FOR*
* A RECORD WITHOUT SUBTYPES, THE SYMBOL DEFINITION FOR THE ACTUAL SMF*
* RECORD SHOULD BEGIN WITH A POSITION,19 DIRECTIVE. *
* *
* SOURCE DOCUMENTATION: SA22-7630-7 *
* LOCAL SYMBOLS ADDED WHERE APPROPRIATE *
*---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+----*
SMF_RDW,1,4,BI                          RECORD DESCRIPTOR WORD
SMFXLEN,=,2,BI                           RECORD LENGTH
SMFXSEG,*,2,BI                          SEGMENT LENGTH
SMFXFLG,*,1,BI                          FLAG BYTE
SMFXRTY,*,1,BI                          SMF RECORD TYPE
 SMF017,17                              *** LOCAL SYMBOL ***
 SMF018,18                              *** LOCAL SYMBOL ***
 SMF021,21                              *** LOCAL SYMBOL ***
 SMF065,65                              *** LOCAL SYMBOL ***
 SMF066,66                              *** LOCAL SYMBOL ***
SMFXTME,*,4,BI                          TIME SINCE MIDNIGHT IN
*                                       HUNDREDTHS OF A SECOND, SINCE
*                                       THE RECORD WAS MOVED INTO THE
*                                       SMF BUFFER                   D
SMFXDTE,*,4,PD                          DATE WHEN THE RECORD WAS MOVED
SMFXDTE_DT1,=,4,DT1                     FORMAT THE DATE FOR REPORTING
*                                       INTO THE SMF BUFFER, IN THE FORM
*                                       0CYYDDDF, WHERE C IS 0 FOR 19XX
*                                       AND C IS 1 FOR 20XX.
SMFXSID,*,4,CH
SMFXSSI,*,4,CH                          SUBSYSTEM IDENTIFIER
SMFXSTY,*,2,BI                          SUBTYPE INDICATOR

