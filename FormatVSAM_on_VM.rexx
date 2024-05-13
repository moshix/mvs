/* REXX EXEC to format a disk for a VSAM catalog*/
100.MAINLINE:
/* Direct your PUNCH to an unused RDR class*/ 
SPOOL PUNCH '*' CLASS P CONT
/* Put IPL FMT and its batch commands into your RDR */ 
'PUNCH IPL FMT * (NOH'
'PUNCH FORMAT DATA A (NOH'
SPOOL PUNCH '*' CLASS P CLOSE
/* Make sure IPL FMT heads the RDR queue */
'ORDER RDR CLASS P'
/* Invoke IPL FMT by IPLing your RDA */
'IPLOOC'
EXIT
