****************************************************************
*        This is COGS.CPY - Cogsworth's Inventory Data         *
*        Usage: COPY  COGS      (with COPY in column 10)       *
****************************************************************
IREC     DS    0CL41        1-41  Inventory record
IDESC    DS    CL10         1-10  Product description
ICALIF   DS    CL3         11-13  Units sold in Calif
IILL     DS    CL3         14-16  Units sold in Illinois
IUTAH    DS    CL3         17-19  Units sold in Utah
IWISC    DS    CL3         20-22  Units sold in Wisconsin
IBEGIN   DS    CL3         23-25  Beginning inventory
IPURCH   DS    CL3         26-28  Purchases throughout year
IQOH     DS    CL3         29-31  Actual quantity on hand
ICOST    DS    CL4         32-35  Cost (each) 99V99
ISELL    DS    CL4         36-39  Sell for (each) 99V99
ICRLF    DS    CL2         40-41  PC/370 only - CR/LF
