// Things to do:
//
// 1) Add support for DSORG=PS, PDS/E, RECFM=V[B][S]
// 2) IB - blank out when ISPF stats present
// 4) Improve error handling/return, especially void functions & their calleds
// 5) Externalize translate table (or at least it's update) to accept input file
//

/*
        This program reads the output of TSO TRANSMIT, which has been   
        (binary) file transferred from OS/390 to a PC, and produces
		one file for each PDS member in the TRANSMIT dataset.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define DIAG_GEN_FLOW
#define DIAG_GEN_MSG
#define DIAG_GEN_SNAP

// #undef  DIAG_GEN_FLOW
// #undef  DIAG_GEN_MSG
// #undef  DIAG_GEN_SNAP

// handy macro definitions:

#define snapid      " " __FILE__ " line " makestr(__LINE__) " "
#define makestr(x)  whack(x)
#define whack(x)    #x

#define snapset(x)

// Print string & decimal number in various formats:

#undef  prtsd
#define prtsd(str, i) {char buf[256]; sprintf(buf, "%s: %d (signed), or %u (unsigned), or 0x%4.4X (hex) \n", str, i, i, i); diagmsg(buf);}

#undef diagflow
#ifndef DIAG_GEN_FLOW
    #define diagflow(x)
#else
    #define diagflow(x) printf(x "\n")
#endif

#undef diagmsg
#ifndef DIAG_GEN_MSG
    #define diagmsg(x)
#else
    #define diagmsg(x)  printf(x "\n")
#endif

#undef  snap
#ifndef DIAG_GEN_SNAP
    #define snap(a,b,c)
#else
    #define snap(a,b,c)  snaplong(a,b,c)
#endif

int  procdata();
int  writemem();
int  makerec(char *, int, int);
void openout();
int  closeout(int);
char *assocmem(unsigned int);
void nextmem();
int  procdir();
int  cmpttr(const void *, const void *);
unsigned char*diralloc();
int  parsedirblk(int);
int  parsemem(int);
int	 printdirmem();
char *ispfdate(char *);
void juliangreg(int, int, int *, int *);
void unpackdate(unsigned char *, int *, int *);
int	 unpack(int);
void getblock(int, int);
void getseg(int);
int  cmdline(int, char *[]);
int  parseopt(char *, char *, char *);
void tranmap();
int  aschex(char *);
void printhelp();
void cleanup();
int  getvbin(char *, int);
void strxset(char *, int, char *, int);
void ebcdic2ascii(void *, int);
void snaplong(void *, int, char *);
int  jmmsnap(void *, int, int, char *);
int  halt(char *);

// -------  GLOBALS --------------------------------------------------

#define FIXED_ENTRY_LENGTH 16		// PDS member (8), ttr (3), slack - stored in "dir"

// Debugging

int				ctlsnap = 0;		// 1 = snap Control segments when read
int				datasnap = 0;		// 1 = snap Data segments when read
int				snapdatablk = 0;	// 1 = snap all data blocks
int				getsegstats = 0;	// 1 = show getseg stats **** 
int				prtoutrec = 0;		// 1 = write records to stdout
int				dbugshowclose = 0;	// 1 = show FNout fclose
int				dbugshowopen = 0;	// 1 = show FNout fopen
int				showsnapaddr = 1;	// 1 = show snap addresses
int				snapcr = 0;			// 1 = begin snap with blank line
int				snapshorthdr = 0;	// 1 = snap header = title only

// Options

char			opttran			= '+';		// translate from EBCDIC to ASCII (makrec)
char			optseq			= '-';		// don't preserve sequence numbers
char			opttrimblank	= '+';		// trim trailing blanks
char			optrdw			= '-';		// no RDW
char			optxmisum		= '+';		// display TRANSMIT dataset summary
char			optdsattr		= '+';		// display dataset attributes
char			optdir			= '+';		// display PDS directory
char			optwrite		= '+';		// write output files
char			opthalt			= '+';		// halt for <press enter> msg in halt()
char			optabout		= '-';		// don't display copyright info
char			opthelp			= '-';		// don't display general help
char			opthelptran		= '-';		// don't display xlate help
char			opthelprdw		= '-';		// don't dispaly RDW help
char			opthelpseq		= '-';		// don't display SEQ help
char			opthelpbug		= '-';		// don't display debugging help
char			optsyntax		= '-';		// don't display syntax help
char			optdirhex		= '-';		// don't dump directory in hex
char			optmap			= '-';		// map translate table

// Debugging Options

char			optlist				= '-';		// list records on stdout
char			optdumpdir			= '-';		// dump IEBCOPY dir blocks
char			optgetseg			= '-';		// getseg segment info
char			optsnapseg			= '-';		// getseg snap
char			optgetblock			= '-';		// getblock block info
char			optsnapblock		= '-';		// getblock snap data block
char			optsnaphalt			= '-';		// snap <press enter> prompt
char			optblock1			= '-';		// dump block 1
char			optblock2			= '-';		// dump block 2

char			*program	= "recv390";	// program name
char			*notprogram = "       ";	// blanks same length as program
int				fatal = 0;					// 1 = abandon execution
int				unsupported = 0;			// 1 = dataset not supported
int				snapassumeascii = 0;		// 1 = snap() won't xlate display to EBCDIC

FILE			*fin  = NULL;
FILE			*fout = NULL;
char			FNout[FILENAME_MAX] = "Dummy.txt";
char			FNin[sizeof(FNout)] = "OS390.XMI";
char			outext[40] = "TXT";
char			dsn[80] = "";
char			dsnmem[sizeof(dsn)];
char			*pmem = NULL;			// ptr to ASCII member name
char			membername[8] = "";		// ASCII member name from assocmem

unsigned char	line[255];				// EBCDIC data from TRANSMIT segment
unsigned char	tranline[sizeof(line)];	// ASCII conversion of "line"
unsigned char	*rec;					// record buffer for makerec
unsigned char	datestr[80];			// ispf date as char string from ispfdate()

int				recpos = 0;				// position in rec for makerec
unsigned char	*block = 0;				// instorage current IEBCOPY block
unsigned char	*dir = 0;				// instorage PDS directory storage
int				dirpos = 0;				// position in "dir" directory
int				outdirpos = 0;			// openout() dirpos for closeout()
int				assocpos = 0;			// assocmem() dirpos 
int				dirlen = 0;				// length of "dir" used 
int				dirblkpos = 0;			// position in 256 byte PDS dir block
int				direntries = 0;
int				ispfstats = 0;			// ISPF stats available (procdir/parsemem)

int				fileswritten = 0;
int				databytesread = 0;		// total bytes of data read
int				databyteswritten = 0;
int				datarecswritten  = 0;
int				outmembytes = 0;
int				outrecbytes = 0;		// # bytes output for record (writemem/makerec)
int				membyteswritten = 0;	// # bytes written for member
int				memrecswritten = 0;		// # records written for member
int				warncounts = 0;			// # members whose rec count didn't verify

int				blocklen = 0;			// IEBCOPY block length
int				blocktrailer = 0;		// set by getblock()
int				seglen = 0;				// length of TRANSMIT segment (0 - 253)
int				segflag = 0;			// segment flag
int				segbytesread = 0;
int				pos = 0;				// position within block or line
int				maxpos = 0;				// position within block set by getblock()
int				datablock = 0;

int				dsorg = 0;				// dataset dsorg
int				recfm = 0;				// dataset recfm
int				blksize = 0;			// dataset blksize
int				lrecl = 0;				// dataset lrecl
int				vbfile = 0;				// 1 = file is V[B] recfm

int				msgfile = 0;			// 1 = process msgfile
char			lastmem[8];				// 8x'ff' End-of-Directory mem name

/* code page 37 below */
unsigned char	trantab[256] = { 

//-----------------------------------------------------------------------------------
//      +0   +1   +2   +3   +4   +5   +6   +7   +8   +9   +a   +b   +c   +d   +e   +f
//-----------------------------------------------------------------------------------

      0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
// 00
//
      0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
// 10
//
      0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
// 20
//
      0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
// 30
//
	   0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x5b,0x2e,0x3c,0x28,0x2b,0x7c,
// 40    --                                                --   --   --   --   --   --
//       sp                                                [    .    <    (    +    |
//
	   0x26,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x21,0x24,0x2a,0x29,0x3b,0x5e,
// 50    --                                                --   --   --   --   --   --
//       &                                                 !    $    *    )    ;    ^
//
	   0x2d,0x2f,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x7c,0x2c,0x25,0x5f,0x3e,0x3f,
// 60    --   --                                           --   --   --   --   --   --
//       -    /                                            |    ,    %    _    >    ?
//
	   0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x3a,0x23,0x40,0x27,0x3d,0x22,
// 70                                                      --   --   --   --   --   --
//                                                         :    #    @    '    =    "
//

//------------------------------------------------------------------------------------
//       +0   +1   +2   +3   +4   +5   +6   +7   +8   +9   +a   +b   +c   +d   +e   +f 
//------------------------------------------------------------------------------------

	   0x20,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x20,0x20,0x20,0x20,0x20,0x20,
// 80         --   --   --   --   --   --   --   --   --
//            a    b    c    d    e    f    g    h    i
//
	   0x20,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70,0x71,0x72,0x20,0x20,0x20,0x20,0x20,0x20,
// 90         --   --   --   --   --   --   --   --   --
//            j    k    l    m    n    o    p    q    r
//
	   0x20,0x7e,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7a,0x20,0x20,0x20,0x5b,0x20,0x20,
// a0         --   --   --   --   --   --   --   --   --                  --
//            ~    s    t    u    v    w    x    y    z                   [
//
	   0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x5d,0x20,0x20,
// b0                                                                     --
//                                                                        ]
//
	   0x7b,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x20,0x20,0x20,0x20,0x20,0x20,
// c0    --   --   --   --   --   --   --   --   --   --
//       {    A    B    C    D    E    F    G    H    I
//
	   0x7d,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f,0x50,0x51,0x52,0x20,0x20,0x20,0x20,0x20,0x20,
// d0    --   --   --   --   --   --   --   --   --   --
//       }    J    K    L    M    N    O    P    Q    R  
//
	   0x5c,0x01,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5a,0x20,0x20,0x20,0x20,0x20,0x20,
// e0    --        --   --   --   --   --   --   --   --
//       \         S    T    U    V    W    X    Y    Z  
//
	   0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x20,0x20,0x20,0x20,0x20,0x20};
// f0    --   --   --   --   --   --   --   --   --   --
//       0    1    2    3    4    5    6    7    8    9
//

//------------------------------------------------------------------------------------
//       +0   +1   +2   +3   +4   +5   +6   +7   +8   +9   +a   +b   +c   +d   +e   +f
//------------------------------------------------------------------------------------
         

// -------  MAIN -----------------------------------------------------

int main(int argc, char *argv[]) {

	int			i, x, rc;
	int			segtype;
	int			blksize;
	int			templen;
	int			tempnum;
	char		orignode[9] = "?";
	char		origuser[9] = "?";
	char		targnode[9] = "?";
	char		targuser[9] = "?";
	char		origtime[20] = "?";
	int			numfiles = 0;
	int			rectype;
	int			filenum;
	char		utility[9] = "?";

	rc = cmdline(argc, argv);
	if ((opthelp == '+') || (rc) || (fatal)) {
		cleanup();
		return rc;
	}

	getseg(ctlsnap);
	x = memcmp(tranline, "INMR01", 6);
	if (x != 0) {
		printf("Input file not a TSO TRANSMIT dataset\n");
		cleanup();
		return 4;
	}
		
	while (!feof(fin)) {

// Process the Data segments

		while (! (segflag & 0x20) ) {
			rc = procdata();								// process Data segments
			if (rc) {
				cleanup();
				return fatal;
			}
//			snap(line, seglen, "Segment emitted from procdata");
		}

// Process the Control segments

		rectype = 0;
		x = memcmp(tranline, "INMR0", 5);
		if (x == 0) {
			rectype = getvbin(&tranline[pos+5], 1);			// what kind of INMR0?
//			printf("INMR0 record %d\n", rectype);
			pos = pos + 6;
			datablock = 0;									// reset data blk ctr
		}

		if (rectype == '2') {
			if (optxmisum=='+') {				// show info gathered from INMR01
				optxmisum = '-';				// only show it once
				printf("\n");
				printf("TSO TRANSMIT dataset sent to user %s at node %s\n", 
					targuser, targnode);
				printf("from user %s at node %s ", origuser, orignode);
				printf("on %.2s/%.2s/%.4s at %.2s:%.2s:%.2s\n", 
					origtime+4, origtime+6, origtime+0, 
					origtime+8, origtime+10, origtime+12);
				if (numfiles > 1) {	
					printf("contains %d files", numfiles);
					printf(", the first of which is this message:\n");
				} else {
					printf("contains one file.\n");
				}
			}
			filenum = getvbin(&line[pos], 4);
			pos = pos + 4;
		}

// Terminate when trailer found

		if (rectype == '6') {
			while (!(feof(fin))) {
				fgetc(fin);			// Read rest of input so stats look OK
				segbytesread++;
			}
			segbytesread--;			// went one too far
			cleanup();
			return 0;
		}

// Deal with stuff in this Control segment

		while (pos < seglen) {
			segtype = getvbin(&line[pos], 2);
			pos = pos + 2;
//			if (rectype != '2') {
//				printf("Current key %.2x\n", segtype);
//				printf("Current position %d seglen %d\n", pos, seglen);
//				snap(&line[pos], seglen, "current position");
//			}

			switch (segtype) {
			case 0x1001:		// target node name
				templen = getvbin(&line[pos+2], 2);
				strxset(targnode, sizeof(targnode), &tranline[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x1002:		// target userid
				templen = getvbin(&line[pos+2], 2);
				strxset(targuser, sizeof(targuser), &tranline[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x1011:		// origin node name
				templen = getvbin(&line[pos+2], 2);
				strxset(orignode, sizeof(orignode), &tranline[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x1012:		// origin userid
				templen = getvbin(&line[pos+2], 2);
				strxset(origuser, sizeof(origuser), &tranline[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x1024:		// origin time stamp
				templen = getvbin(&line[pos+2], 2);
				strxset(origtime, sizeof(origtime), &tranline[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x102f:		// number of files
				templen = getvbin(&line[pos+2], 2);
				numfiles = getvbin(&line[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x0042:		// logical record length
				templen = getvbin(&line[pos+2], 2);
				lrecl = getvbin(&line[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x0030:		// blocksize
				templen = getvbin(&line[pos+2], 2);
				blksize = getvbin(&line[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x1028:		// utility name
				templen = getvbin(&line[pos+2], 2);
				strxset(utility, sizeof(utility), &tranline[pos+4], templen);
				pos = pos + 4 + templen;
				break;
			case 0x0002:		// dsname
				tempnum = getvbin(&line[pos+0], 2);
				pos = pos + 2;
				memset( dsn, 0, sizeof(dsn));
				for (i = 0; i < tempnum; i++) {
					templen = getvbin(&line[pos], 2);
					pos = pos + 2;
					strncat(dsn, &tranline[pos], templen);
					pos = pos + templen;
					if (i + 1 != tempnum)
						strncat(dsn, ".", 1);
				}
				break;
			case 0x0028:		// terminal allocation - flag only, count = 0
				tempnum = getvbin(&line[pos+0], 2);
				pos = pos +2;
				msgfile = 1;								// inline MESSAGE
				break;
			case 0x1023:		// origin version number
			case 0x1025:		// destination time stamp
			case 0x1026:		// acknowledgement request
			case 0x1027:		// receive error code
			case 0x1029:		// user parameter string
			case 0x102a:		// transmitted record count
			case 0x0001:		// ddname
			case 0x0003:		// member name
			case 0x000b:		// secondary space quantity
			case 0x000c:		// dir blks
			case 0x0022:		// expiration date terminal
			case 0x003c:		// dsorg
			case 0x0049:		// recfm
			case 0x1020:		// last ref date
			case 0x1021:		// last chg date
			case 0x1022:		// create   date
			case 0x102c:		// pri space qty
			case 0x8012:		// dataset type
				tempnum = getvbin(&line[pos+0], 2);
				pos = pos + 2;
				if (tempnum != 1) {
					for (i = 0; i < tempnum; i++) {
						templen = getvbin(&line[pos], 2);
						pos = pos + 2 + templen;
					}
				} else {
					templen = getvbin(&line[pos], 2);
					pos = pos + 2 + templen;
				}
				break;
			default:
				printf("unknown segment key %.2x\n", segtype);
				printf("pos %d, seglen %d\n", pos, seglen);
				snap(line, seglen, "line");
				tempnum = getvbin(&line[pos+0], 2);
				pos = pos + 2;
				for (i = 0; i < tempnum; i++) {
					templen = getvbin(&line[pos], 2);
					pos = pos + 2 + templen;		// just ignore it
				}
				break;
			}										// switch
		}											// pos < seglen
		getseg(ctlsnap);
	}												// ! eof
	cleanup();		
	return 0;
} /* main */

//--------------------------------------------------------------------

//* Process Data segments

// Upon entry, line & tranline contain the first segment of the 
// first block from IEBCOPY, INMCOPY, AMSCIPHER or inline MESSAGE
// This code knows what to do for IEBCOPY & inline MESSAGE... 
// Others: tough luck.

// Deciphering IEBCOPY data - Notes:

// TSO TRANSMIT doesn't give us the BDW & RDW from the IEBCOPY blocks.
// It does, however, set segflag x'80' & x'40' so that we can determine
// data block boundaries.  MAIN resets blocknum for us whenever it sees an
// INMR0n record, so data block numbers are always relative to the most
// recently seen INMR0n segment.  This is mildly useful when there's an
// imbedded MESSAGE from TSO TRANSMIT (at least during debugging).

// The TSO Customization doc for segflag says "record", but it means "block".

// Example 1: segflag x'c0' means segment contains entire block
// Example 2: segflag x'80' means segment is 1st in the block
// Example 3: segflag x'00' means segment is neither first nor last in block,
//            ala VTAM middle-of-chain
// Example 4: segflag x'40' means segment is last in block

// Segflag x'20' means Control record (otherwise it's a Data record).
// Segflag x'10' says 'This is record number of next record', but I've never
// seen one.

// This code handles all the data for all members or one MESSAGE per call.

int procdata( ) {
		char	txtdsorg[5] = "";
		char	txtrecfm[4] = "";
const	int		eyepds  = 0x00ca6d0f;		// IEBCOPY PDS eyecatcher
/*
const	int		eyepdse = 0x01ca6d0f;		// IEBCOPY PDSE eyecatcher (someday)
*/
		int		rc = 0;

	if (msgfile) {
		printf("\n");
		while (! (segflag & 0x20) ) {
			printf("%.*s\n", seglen, tranline);
			getseg(0);								// eat inline MESSAGE w/o snap
		}
		msgfile = 0;								// only one MESSAGE per TRANSMIT
		return 0;
	}

	getblock(1, 0);									//	get rest of 1st block, append line

// Contents of 1st IEBCOPY block (BDW & RDW are not present in TRANSMIT block):
//
// +00 (4) 0x00ca6d0f	PDS eyecatcher
// +04 (2) dsorg
// +06 (2) blksize
// +08 (2) lrecl
// +10 (1) recfm		and probably some other stuff
//

	if (optblock1 == '+')
		snap( block, blocklen, "Data Block One");

	if (!(memcmp(block, &eyepds, 4))) {
		printf("Fatal error; procdata could not verify IEBCOPY PDS eyecatcher\n");
		snap(block, blocklen, "IEBCOPY header block");
		fatal = 1;
		return fatal;
	}

	dsorg	= getvbin(&block[4], 2);
	blksize = getvbin(&block[6], 2);
	lrecl	= getvbin(&block[8], 2);					// lrecl from 1st IEBCOPY block
	recfm   = getvbin(&block[10], 1);

	unsupported = 1;									// assume unsupported dataset
	if (dsorg & 0x8000)	strcpy(txtdsorg, "ISAM");
	if (dsorg & 0x4000)	strcpy(txtdsorg, "PS");
	if (dsorg & 0x2000)	strcpy(txtdsorg, "DA");
	if (dsorg & 0x0200)	{
		strcpy(txtdsorg, "PO"); 
		unsupported = 0;
	}
	if (txtdsorg[0] == '\0')	
		strcpy(txtdsorg, "?");
	if (dsorg & 0x01)	strcat(txtdsorg, "U");		// unmovable

	if (recfm & 0x80)	strcpy(txtrecfm, "F");
	if (recfm & 0x40) {
		strcpy(txtrecfm, "V");
		vbfile = 1;
	}
	if ((recfm & 0xc0) == 0xc0)	{
		strcpy(txtrecfm, "U");
		optseq = '+';			// no RECFM=U sequence fields
	}
	if (recfm & 0x10)	strcat(txtrecfm, "B");
	if (recfm & 0x08)	{
		strcat(txtrecfm, "S");
		if (!(memcmp(txtrecfm, "VBS", 3))) unsupported = 1;
	}
	if (recfm & 0x04)	strcat(txtrecfm, "A");
	if (recfm & 0x02)	strcat(txtrecfm, "M");

	if (optdsattr=='+') {
		printf("\n");
		printf("Dataset %s\n", dsn);
		printf("Dsorg %s ", txtdsorg);
		printf("recfm %s ", txtrecfm);
		printf("blksize %d ", blksize);
		printf("lrecl %d\n", lrecl);
	}

	if (unsupported) {
		printf("\n");
		printf("This dataset is not currently supported.\n\n");
		rc = 1;
		if (optdir=='+')
			rc = halt("Press enter to display directory, enter 'x' to exit");
		if (rc) {
			fatal = 1;
			return fatal;
		}
	}

	getblock(0, 0);									// get Data Block Two
	if (optblock2=='+')
		snap(block, blocklen, "Block 2");

// Contents of 2nd IEBCOPY block  -  secret (well ... _I_ don't know)
//
// Appears to have "dataset/member id" value that appears in blocks 4 - n
// So far, I haven't had to care.  This block is mostly x'00' anyway.

	getblock(0, 0);									// get Data Block Three

// Contents of 3rd IEBCOPY block (assuming no BDW & RDW):
//
// +00 (8) 8x'00' directory eyecatcher
//
//			FOLLOWING OCCURS FOR EACH PDS DIRECTORY BLOCK
//
// +08 (2) presumably PDS directory keylen = 8
// +10 (2) presumably pds directory blksize = 0x0100
// +12 (8) presumably high key value from CKD DASD key
//
//         BEGINNING OF FIRST PDS DIRECTORY BLOCK
//
// +20 (2) length of bytes in PDS directory block used 
//         including space consumed by this halfword
// +22 (8) first member name begins here
//
//         MORE PDS DIRECTORY BLOCKS MAY FOLLOW IN SAME IEBCOPY BLOCK
//
//		   Additional IEBCOPY PDS directory blocks may also follow
//		   when IEBCOPY decides they won't fit in current block
//

	rc = procdir();										// parse & store PDS directory
	if (rc) 
		return rc;

	if (unsupported) {									// at least let user see dir
		fatal = 1;
		return fatal;
	}
	rc = writemem();
	if (rc) {
		fatal = 1;
		return fatal;
	}
	return rc;
} /* procdata */

//--------------------------------------------------------------------

//* Write all the data for IEBCOPY unloaded PDS members

int writemem( ) {
	int			outlen;
	int			bytesleftinblock;
	int			bytesinblock = 0;
	int			bytes;
	int			rc = 0;

	if (optwrite=='-') {
		fatal = 1;
		return fatal;
	}
	closeout(1);										// print header line
	dirpos = 0 - FIXED_ENTRY_LENGTH;					// prepare for nextmem()
	nextmem();
	openout();
	while (! (segflag & 0x20)) {						// have Data segment
		bytesinblock = getvbin(&block[9], 3);			// # data bytes in IEBCOPY block
		if (vbfile) {
			pos += 4;									// account for VB BDW
			bytesinblock -= 4;
		}
		databytesread += bytesinblock;
		if (bytesinblock < 1) {							// member EOF block
			rc = closeout(0);
			if (rc) {
				fatal = 1;
				return fatal;
			}
			getblock(0, 0);								// ignore empty block
			continue;									// check for INMR06
		}
		outmembytes = 0;								// # bytes output for block
		while (outmembytes < bytesinblock) {
			makerec(NULL, 0, 0);						// clear output record
			outrecbytes = 0;							// # bytes output for rec
			if (vbfile) {
				lrecl = getvbin(&block[pos], 2);		// record length for V[B]
				lrecl -= 4;
				pos += 4;
				if (pos >= maxpos)
					break;
			}
			while ((outmembytes < bytesinblock) && (outrecbytes < lrecl)) {
				if (prtoutrec)							// add extra debug info
					printf("%.4x %.4x %.4x ", blocklen, maxpos, pos);
				if (pos >= maxpos) {
					getblock(0,0);	
				} else {
					outlen = lrecl - outrecbytes;		// desired # rec output bytes
					bytesleftinblock = maxpos - pos;	// # bytes avail in block
					if (outlen > bytesleftinblock) 
						outlen = bytesleftinblock;		// all we can output for now
					makerec(&block[pos], outlen, 0);	// queue [partial] record
				}
			}
			memrecswritten++;
			bytes = makerec(rec,0,prtoutrec);			// [list &] output rec
			if (fatal)
				return fatal;
			membyteswritten += bytes;					// accum bytes written for mem
		}
		if (blocktrailer) {								// was there a blk trailer?
			rc = closeout(0);
			if (rc) {
				fatal = 1;
				return fatal;
			}
		}
		getblock(0, 0);									// get new data block
	}													// while Data
	closeout(0);
	return rc;
} /* writemem */

//--------------------------------------------------------------------

//* Make record, output "lrecl" bytes when complete

//  Also updates outmembytes, outrecbytes, and pos based on "len" passed
//  Trim trailing blanks

int makerec(char *ptxt, int len, int showrec ) {
//	char		showbuf[sizeof(line)];
	int			bytesout, i, validseq, x;

	if (rec == NULL) {
		rec = malloc(lrecl);
		if (rec == NULL) {
			printf("Fatal error; storage allocation failed for %d bytes\n", lrecl);
			fatal = 1;
			return fatal;
		}
	}
	if (ptxt == NULL) {
		memset(rec, 0, sizeof(rec));				// clear record buffer
		recpos = 0;
		return 0;
	}
	// ---------------------------------------------
	if (len == 0) {									// output record
		if (opttran=='+')
			ebcdic2ascii( rec, lrecl);				// ASCII-ize buffer

		bytesout = lrecl;
		if ((optseq == '-') && (bytesout > 8)) {	// remove sequence field
			validseq = 1;							// assume seq field valid
			if (vbfile) {
				for (i=0; i < 8; i++) {
					x = rec[i];
					if (!isdigit(x))
						validseq = 0;
				}
			} else {
				for (i=lrecl - 1; i >= lrecl - 8; i--) {
					x = rec[i];
					if (!isdigit(x))
						validseq = 0;
				}
			}
			if (validseq) {
				if (vbfile) {
					for (i = 0; i < lrecl - 8; i++) {
						rec[i] = rec[i+8];				// shove VB line over 8 bytes
					}
				} else {
					memset(&rec[lrecl-8], ' ', 8);		// blank out sequence field
				}
				bytesout = bytesout - 8;
			}
		}
		if (opttrimblank == '+') {					// trim trailing blanks
			for (i = bytesout - 1; i > 0; i--)
				if (rec[i] == ' ') {
					rec[i] = '\0';
					bytesout--;
				} else
					break;
		}

		if (bytesout < 0) {
			bytesout = 1;							// negative byte count bad
		}
		if (optrdw=='+') {
			fprintf(fout, "%c%c%c%c",
				((bytesout+4)/256), ((bytesout+4) - ((bytesout+4)/256)), 0, 0);
		}
				
		fprintf(fout, "%.*s\n", bytesout, rec);		// output record
		if (optlist=='+')
			printf("%.*s\n", bytesout, rec);		// list record
		bytesout = bytesout + 2;						// account for "\r"
		databyteswritten += bytesout;				// total data bytes written
		datarecswritten++;							// total data records written
		recpos = 0;
		return bytesout;							// # bytes written
	} 
	//----------------------------------------------
	memcpy(&rec[recpos], ptxt, len);				// add text to buffer
	if (showrec) {
		snap( rec, lrecl, "makerec fragmented rec");
	}
	recpos = recpos + len;
	outmembytes = outmembytes + len;
	outrecbytes = outrecbytes + len;
	pos = pos + len;
	return 0;	
} /* makerec */

//--------------------------------------------------------------------

// Close previous output file
// Build output file name, open it

void openout( ) {
	unsigned int	i;
	int				c;
	char			blank = ' ';

	memset(dsnmem, 0, sizeof(dsnmem));
	strcpy(dsnmem, dsn);
	strcat(dsnmem, "(");
	
	memset(FNout, 0, sizeof(FNout));

	memcpy(FNout, membername, 8);
	for (i = 7; i >= 0; i--) {
		if (FNout[i] == blank)
			FNout[i] = '\0';
		else
			break;
	}

	strcat(dsnmem, FNout);
	strcat(dsnmem, ")");
	strcat(FNout, ".");
	strcat(FNout, outext);
	for (i = 0; i < strlen(FNout); i++) {
		c = tolower(FNout[i]);
		FNout[i] = (char) c;
	}

	if (dbugshowopen)
		printf("openout:  opening %s\n", FNout);
	fout = fopen(FNout, "w");
	if (fout) {
		outdirpos = assocpos;						// save for closeout()
		fileswritten++;								// accumulate files written
	} else {
		outdirpos = 0xff;							// bad open
		printf("Error opening %s for output\n", FNout);
	}
	return;
} /* openout */

//--------------------------------------------------------------------

//* Close output file, list

int closeout(int prthdr) {
	int			len;
	int			dircurlines;
	int			rc = 0;
	char		work[FILENAME_MAX];

	if (prthdr) {
		len = 9 + strlen(outext);
		printf("\n");
		memset(work, ' ', sizeof(work));
		memcpy(work, "output", 6);
		work[len] = 0x00;
		printf("%s      bytes    records\n", work);
		memset(work, ' ', sizeof(work));
		memcpy(work, "filename", 8);
		work[len] = 0x00;
		printf("%s    written    written\n", work);
		memset(work, '-', len);
		work[len] = 0x00;
		printf("%s ---------- ----------\n", work);
//		printf(    "output            bytes....records\n");
//		printf(    "filename        written    written\n");
//		printf(    "------------ ---------- ----------\n");
	}
	if (!fout)
		return 0;

	if (dbugshowclose) 
		printf("closeout: closing %s\n", FNout);

	fclose(fout);
	fout = NULL;
	
	memset(work, ' ', sizeof(work));
	memcpy(work, FNout, strlen(FNout));
	work[9+strlen(outext)] = 0x00;
	printf("%s %10d %10d", work, membyteswritten, memrecswritten);

	if (outdirpos == 0xff)
		printf(" open err\n");
	else {
		dircurlines = getvbin(&dir[outdirpos+14], 2);	// # lines ISPF says in member
		if (dircurlines == memrecswritten)
			printf(" OK\n");
		else
			if (dircurlines == 0)
				printf(" ? no ISPF statistics\n");
			else {
				printf(" error; ISPF statistics records: %d\n", dircurlines);
				rc = halt(NULL);
				warncounts++;
				if (rc)
					fatal = 1;
			}
	}
	membyteswritten = 0;
	memrecswritten = 0;
	nextmem();
	if (assocpos < dirlen)
		openout();
	return rc;
} /* closeout */

//--------------------------------------------------------------------

//* Associate member name with TTR

// IEBCOPY puts the record TTR in each data block header,
// so just need to find it in our list.  If we didn't find it, it
// (likely) means we have another block for the same member.

char *assocmem(unsigned int memttr) {
	char			*p;
	int				scanpos;
unsigned	int		curttr;

	assocpos = 0;
	for (scanpos = 0; scanpos < dirlen; scanpos += FIXED_ENTRY_LENGTH) {
		curttr = getvbin( &dir[scanpos+8], 3);
		if (curttr == memttr) {
			p = &dir[scanpos];
			memcpy(membername, p, 8);			// copy member name to return
			ebcdic2ascii( membername, 8);
			p[12] = (char)0xff;					// show member used (debug)
			assocpos = scanpos;					// tell closeout()
			return membername;
		}
	}
	return 0;
} /* assocmem */

//--------------------------------------------------------------------

//* Next member

void nextmem() {

	dirpos += FIXED_ENTRY_LENGTH;
	memcpy(membername, &dir[dirpos], 8);				// new member name
	ebcdic2ascii(membername, 8);				// convert to ASCII
	assocpos = dirpos;
	return;
} /* nextmem */

//--------------------------------------------------------------------

//* Parse PDS directory block'(s) entries in current IEBCOPY block
//* See procdata() for what we know about this IEBCOPY block
//*
//* This is first pass through directory. printdir actually creates "dir" list
//* using the entry count accumulated in direntries.  We basically scan the
//* IEBCOPY block to get this count.
//
// IEBCOPY lumps all the directory blocks for a PDS 
// into one block, with no apparent editing aside from slapping 
// it's IEBCOPY block junk at the front of each 256 byte PDS directory block.

// Further testing reveals some TRANSMIT datasets have more than one IEBCOPY
// directory block containing PDS directory blocks.  We cope with this added
// complication, and when we return the 1st PDS member's IEBCOPY block is in
// "block".

int procdir( ) {
	int	moredir = 1;
	char	work[8];

	if (optdir=='+') {
		printf("\n");
		printf("                                                     ----- LINES -----\n");
		printf("MEMBER      TTR IB VV.MM    CREATED         MODIFIED   CUR  INIT   MOD USERID\n"); 
	}
	memset(work, 0, sizeof(work));					// IEBCOPY "dir blk" header
	memset(lastmem, 0xff, sizeof(lastmem));			// end-of-dir mem name/highkey
	while (moredir) {
		pos = 0;									// scan blocks, count
		if (optdumpdir=='+')
			snap(block, blocklen, "IEBCOPY directory block");
		while (pos < blocklen) {
			pos = pos + 20;							// at 256 byte pds dir blk
			if ((parsedirblk(0)) == 0)				// count # members
				break;
		}
		dir = diralloc();							// alloc new "dir"
		pos = 0;
		while (pos < blocklen) {
			pos = pos + 20;
			if ((parsedirblk(1)) == 0)				// add member to "dir", list
				break;
		}
		getblock(0, 0);								// see if another IEBCOPY dir blk
		if (memcmp(work, &block[0], 8))				// more dir blocks?
			moredir = 0;							
	}

	qsort(dir, dirlen / FIXED_ENTRY_LENGTH,			// sort "dir" by TTR
			FIXED_ENTRY_LENGTH,
			cmpttr);

	if (optdir=='+') {
		printf("\n");
		printf("There are %d member(s) in the PDS directory.\n", 
			dirlen / FIXED_ENTRY_LENGTH);
	}
	return 0;
} /* procdir */

//--------------------------------------------------------------------

//* Compare TTRs for qsort

int cmpttr(const void *entry1, const void *entry2) {
	int	result;

	result = memcmp((char *)entry1+8, (char *)entry2+8, 3); 
		return result;
} /* cmpttr */

//--------------------------------------------------------------------

unsigned char *diralloc( ) {
	unsigned char *newdir;
	int		newdirlen;

	newdirlen = direntries * FIXED_ENTRY_LENGTH;
	newdir = malloc(newdirlen);
	if (!newdir) {
		printf("Fatal error allocating PDS directory storage\n");
		fatal = 1;
		return NULL;
	}
	memset(newdir, 0, newdirlen);
	if (dir) 
		memcpy(newdir, dir, dirlen);
	dir = newdir;
	dirlen = newdirlen;
	return newdir;
} /* allocdir */

//--------------------------------------------------------------------

//* Parse dir block ... block being the 256 byte PDS dir block

//  Increments direntries during phase 0

int parsedirblk(int phase ) {
	int		usedbytes;							// # bytes used in 256 byte block
	int		deadbytes;			// # unused bytes at end of 256 byte blk
	int		entlen;

	usedbytes = getvbin( &block[pos], 2);		// how many bytes in block used
	if (usedbytes > 254) {
		printf("Fatal error; parsedirblk found invalid PDS directory block\n");
		snap(&block[pos], 256, "invalid directory block");
		fatal = 1;
		return fatal;
	}
	deadbytes = 256 - usedbytes;				// dead bytes @ end of dir blk
	pos = pos + 2;
	usedbytes = usedbytes - 2;
	while (usedbytes > 0) {
		if (phase == 1) 
			entlen = printdirmem();				// add to "dir", print
		else
			entlen = parsemem(phase);			// just count member
		if (entlen == 0)
			return 0;
		pos += entlen;
		usedbytes -= entlen;
		if (phase == 0)
			direntries++;
	}
	pos = pos + deadbytes;						// account for dead bytes
	return 1;									// probably more entries
} /* parsedirblk */

//--------------------------------------------------------------------

//* Parse member's PDS directory entry, return entry length

int  parsemem(int phase ) {
	int					entlen;
	const unsigned int	ebcdicblanks =0x40404040;
	int					x;
	
	x = memcmp( &block[pos], lastmem, 8);			// x'ff..ff' member name?
	if (x == 0)
		return 0;
	x = getvbin( &block[pos + 8 + 3], 1);
	x = x & 0x1f;									// isolate x'1f' bits
	x = x * 2;										// # bytes userdata in entry
	entlen = x + 8 + 3 + 1;			// user data length, member, ttr, indicator byte

	if (entlen== 42) {								// probably ISPF stats
		if (!(memcmp(&block[pos+40],&ebcdicblanks,2))) {
			ispfstats = 1;							// close enough
		}
	}
	return entlen;
} /* parsemem */

//--------------------------------------------------------------------

//* print directory member entry, add it to "dir"

int printdirmem() {
	int		entlen;
	char	msg[101];
	char	work[sizeof(msg)];
	int		temp, i, scr;
	int		entispfstats;

	entlen = parsemem(0);					// determine how long entry is
	if (entlen == 0) {
		return 0;
	}

	memset( &dir[dirpos], 0, FIXED_ENTRY_LENGTH);
	memcpy( &dir[dirpos], &block[pos], 11);

	entispfstats = 1;								// assume dir ent has ISPF stats
	if (optdirhex == '+')
		entispfstats = 0;
	if (entlen != 42)	
		entispfstats = 0;
	memset(work, 0x40, 3);							// last 2 chars always EBCDIC blanks
	if (memcmp(&block[pos+40],work, 2))	
		entispfstats = 0;
	if (entispfstats)
		memcpy(&dir[dirpos+14], &block[pos+26], 2);		// save # lines in "dir" entry

	if (optdir=='+') {
		memset(work, 0, sizeof(work));
		memcpy(work, &block[pos], 8);				// MEMBER
		ebcdic2ascii(work, 8);					// convert to ascii
		strcpy(msg, work);
		temp = getvbin(&block[pos+8], 3);			// TTR
		sprintf(work, " %.6X", temp);				// make display hex, use upper case letters
		strcat(msg, work);
		temp = getvbin(&block[pos+11], 1);			// Indicator byte
		sprintf(work, " %.2X", temp);
		strcat(msg, work);

		if (!entispfstats) {						// not ISPF stats
			temp = getvbin(&block[pos+11], 1);
			strcat(msg, " ");
			temp = temp & 0x1f;						// mask out all but length bits
			temp = temp * 2;						// user data is counted in halfwords
			for (i = 0; i < temp; i++) {
				scr = getvbin(&block[pos+12+i], 1);
				sprintf(work, "%.2X", scr);			// list userdata
				strcat(msg, work);
			}
		} else {
			temp = getvbin(&block[pos+12], 1);			// VERS
			sprintf(work, " %.2d.", temp);	
			strcat(msg, work);
			temp = getvbin(&block[pos+13], 1);			// MOD
			sprintf(work, "%.2d", temp);
			strcat(msg, work);

			strcat(msg, ispfdate(&block[pos]));			// Creation & modification dates
	
			temp = getvbin(&block[pos+24], 1);			// modified HH time
			sprintf(work, " %.2x", temp);
			strcat(msg, work);
			temp = getvbin(&block[pos+25], 1);			// modified MM time
			sprintf(work, ":%.2x", temp);
			strcat(msg, work);
			temp = getvbin(&block[pos+26], 2);			// Current # lines
			sprintf(work, " %5d", temp);	
			strcat(msg, work);
			temp = getvbin(&block[pos+28], 2);			// Initial # lines
			sprintf(work, " %5d", temp);
			strcat(msg, work);
			temp = getvbin(&block[pos+30], 2);			// Modified # lines
			sprintf(work, " %5d ", temp);
			strcat(msg, work);
			memset(work, 0, sizeof(work));
			memcpy(work, &block[pos+32], 8);
			ebcdic2ascii(work, 8);
			work[8] = 0x00;
			strcat(msg, work);
		}
		printf("%s\n", msg);
	}
	dirpos += FIXED_ENTRY_LENGTH;
	return entlen;
} /* printdirmem */

//--------------------------------------------------------------------

//* Convert ISPF date stored in user-data portion of PDS directory entry
//* into character string

// There are several date-related fields in an ISPF statistics directory entry
// +14 (2)  currently unknown (checksum?)
// +16 (4)  packed CCYYDDDF creation date
// +20 (4)  packed CCYYDDDF modification date
// +24 (1)  hex HH modified hours
// +25 (1)  hex MM modified minutes

// We return a string with creation date and modification date only
// Our caller handles HH:MM, since it's so straight-forward 

char *ispfdate(char *pdirent) {
	int		year, yearday;			// year, Julian day of year (1 - 365)
	int		month, monthday;		// month, day of month (1 - 31)
	char	work[12];

	memset(datestr, 0, sizeof(datestr));
	unpackdate(&block[pos+16], &year, &yearday);
	juliangreg(year, yearday, &month, &monthday);
	sprintf(datestr, " %.4d/%.2d/%.2d", year, month, monthday);

	unpackdate(&block[pos+20], &year, &yearday);
	juliangreg(year, yearday, &month, &monthday);
	memset(work, 0, sizeof(work));
	sprintf(work, " %.4d/%.2d/%.2d", year, month, monthday);
	strcat(datestr, work);
	return datestr;
} /* ispfdate */

//--------------------------------------------------------------------

//* Unpack ISPF date

// packed: x'ccyydddf'	cc (century) relative 1900

void unpackdate(unsigned char *packed, int *xyear, int *xjulian) {
	int		century, year, day, temp, x, y, junk;

	junk = getvbin(packed, 4);	// debug aid
	temp = packed[0]; century = (unpack(temp) * 100) + 1900;

	temp = packed[1]; 
	year = unpack(temp); 
	year += century;

	temp = packed[2]; x = unpack(temp);
	temp = packed[3]; y = unpack(temp);  
	day = (x * 10) + y;

	*xyear = year;
	*xjulian = day;
	return;
} /* unpackdate */

//--------------------------------------------------------------------
int unpack(int packed) {
	int		result;
	int		hi, lo;

	hi = packed / 16;
	lo = packed - (hi * 16);
	if (lo > 9) 				// x'.f' or x'.c'
		result = hi;
	else 
		result = (hi * 10) + lo;
	return result;
}
//--------------------------------------------------------------------

//* Date conversion routine ... month_day K&R 2nd Edition Chapter 5.7 pag 111

static char daytab[2][13] = {
	{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
	{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} };
//       J   F   M   A   M   J   J   A   S   O   N   D

void juliangreg(int year, int yearday, int *pmonth, int *pday) {
	int		i, leap;

	leap = ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0) || (year%1000 == 0) ;
	for (i = 1; yearday > daytab[leap][i]; i++)
		yearday -= daytab[leap][i];
	*pmonth = i;
	*pday = yearday;
	return;
} /* monthday */

//--------------------------------------------------------------------

//* Read whole IEBCOPY Data block into storage

//  We set: block, blocklen, blocktrailer, pos, maxpos
//  Note readseg resets pos

void getblock(int append, int snapblk) {
		long	zeros = 0x00;
		int		blockpos;
const	int		qsamblksize = 32 * 1024;
unsigned char	*blockx;

	blocktrailer = 0;						// assume no block trailer present
	datablock++;
	if (block == 0) {
		block = malloc(qsamblksize);		// QSAM max blocksize
		if (block == NULL) {
			printf("Fatal error allocating buffer\n");
			fatal = 1;
			return;
		} 
	}
	memset( block, 0, qsamblksize);
	if (append) {
		if (segflag & 0x20) {
			printf("Fatal error: getblock called with Control segflag %.2x\n", segflag);
			fatal = 1;
			return;
		}
		memcpy( block, line, seglen);	// EBCDIC buffer w/o BDW & RDW space
	} else {
		seglen = 0;
		segflag = 0x00;
	}
	blocklen = blockpos = seglen;
	while (! (segflag & 0x40)) {		// not last segment in block
		getseg(0);						// get data quietly
		if (segflag & 0x20)				// INMR06?
			return;						// yep
		blockpos = blocklen + seglen;
		if (blockpos > (qsamblksize)) {
			printf("Fatal error; block exceeds buffer size!\n");
			fatal = 1;
			return;
		}
		memcpy( &block[blocklen], line, seglen);
		blocklen = blockpos;
	}

	pos = 12;							// 1st position in block for data
										// (for VB, this is BDW)

//	if ((memcmp(&block[blocklen-12], &zeros, 4) == 0) &&
	if ((memcmp(&block[blocklen-12], &block[blocklen-12], 6) == 0) &&
		(memcmp(&block[blocklen-3],  &zeros, 3) == 0) ) { 	// block trailer?
		blocktrailer = 1;									// block trailer
		maxpos = blocklen - 12;
	} else {
		maxpos = blocklen;									// no block trailer
	}

	if (snapblk) {
		printf("Block length %d\n", blocklen);
		snap( block, blocklen, "getblock");
	}
	if ((optgetblock=='+') || (optsnapblock=='+')) {
		printf("\n");
		printf("Getblock: Data Block %.3d length %.6d (0x%.4x) ", 
			datablock, blocklen, blocklen);
		if (blocktrailer)
			printf("trailer\n");
		else
			printf("\n");

		printf("Getblock: "
			"%.2x%.2x%.2x%.2x "
			"%.2x%.2x%.2x%.2x "
			"%.2x%.2x%.2x%.2x ... ",
			block[0], block[1], block[2], block[3],
			block[4], block[5], block[6], block[7],
			block[8], block[9], block[10], block[11]);

		blockx = &block[blocklen-12];
		printf(
			"%.2x%.2x%.2x%.2x "
			"%.2x%.2x%.2x%.2x "
			"%.2x%.2x%.2x%.2x "
			"\n", 
			blockx[0], blockx[1], blockx[2], blockx[3],
			blockx[4], blockx[5], blockx[6], blockx[7],
			blockx[8], blockx[9], blockx[10], blockx[11]);
	}
	if (optsnapblock=='+') {
		snap(block, blocklen, "getblock: Block");
	}
	return;
} /* getblock */

//--------------------------------------------------------------------

//* Read in a new segment

// Seglen includes the length of it's own data

void getseg( int snapget) {
	int		temp;
	int		showstats = 0;
	int		i;

	pos = 0;
	memset( line, 0, sizeof(line));
	seglen = fgetc(fin);
	segbytesread += seglen;							// accumulate segment bytes read
	seglen = seglen - 2;					

	segflag = fgetc(fin);

	if (snapget)		showstats = 1;
	if (getsegstats)	showstats = 1;
	if (optgetseg=='+') showstats = 1;
	if (showstats) {
		if (segflag & 0x20) {
			printf("Control segment; seglen %.4d, segflag %.2x\n", seglen, segflag);
		} else {
			printf("Data segment; seglen %.4d, segflag %.2x\n", seglen, segflag);
		}
	}
	for (i = 0; i < seglen; i++) {
		temp = fgetc(fin);
		line[i] = (char) temp;
	}
	if ((snapget == 1) || (optsnapseg=='+')) {
		snap(line, seglen, "segment");
	}
	memcpy(tranline, line, sizeof(line));
	ebcdic2ascii(tranline, seglen);
	return;	
} /* getseg */

//--------------------------------------------------------------------

//* Process command line: setup options, open input

int cmdline(int argc, char *argv[]) {
	int		argnum;
	int		lenarg;
	int		arghit;
	int		opthit;
	char	optchar;
	char	*arg;

	opthit = 1;										// assume we find + or -
	for (argnum = 1; ((opthit) && (argnum < argc)); argnum++) {
		arg = argv[argnum];
		lenarg = strlen(argv[argnum]);
		arghit = 0;									// not 0 when we find option match
		optchar = 0x00;
		switch (argv[argnum][0]) {
		case '+':
			optchar = '+';
			break;
		case '-':
			optchar = '-';
			break;
		default:
			opthit = 0;								// no + or -
			argnum--;								// we went one too far, back up
			break;
		}
		if (optchar) {
			arghit += parseopt(arg, "tran",				&opttran);
			arghit += parseopt(arg, "seq",				&optseq);
			arghit += parseopt(arg, "rdw",				&optrdw);
			arghit += parseopt(arg, "about",			&optabout);
			arghit += parseopt(arg, "help",				&opthelp);
			arghit += parseopt(arg, "syntax",			&optsyntax);
			arghit += parseopt(arg, "dirhex",			&optdirhex);
			arghit += parseopt(arg, "helptran",			&opthelptran);
			arghit += parseopt(arg, "helprdw",			&opthelprdw);
			arghit += parseopt(arg, "helpseq",			&opthelpseq);
			arghit += parseopt(arg, "helpbug",			&opthelpbug);
			arghit += parseopt(arg, "list",				&optlist);
			arghit += parseopt(arg,	"dumpdir",			&optdumpdir);
			arghit += parseopt(arg, "getseg",			&optgetseg);
			arghit += parseopt(arg, "snapseg",			&optsnapseg);
			arghit += parseopt(arg, "getblock",			&optgetblock);
			arghit += parseopt(arg, "snapblock",		&optsnapblock);
			arghit += parseopt(arg, "snaphalt",			&optsnaphalt);
			arghit += parseopt(arg, "block1",			&optblock1);
			arghit += parseopt(arg, "block2",			&optblock2);
			arghit += parseopt(arg, "trim",				&opttrimblank);
			arghit += parseopt(arg, "dir",				&optdir);
			arghit += parseopt(arg, "xmisum",			&optxmisum);
			arghit += parseopt(arg, "dsattr",			&optdsattr);
			arghit += parseopt(arg, "halt",				&opthalt);
			arghit += parseopt(arg, "write",			&optwrite);
			arghit += parseopt(arg, "map",				&optmap);
			if (arghit == 0) {
				printf("Option error: %s\n", argv[argnum]);
				printhelp();
				fatal = 1;
				return fatal;
			}
		}
	}

	if ((opthelp=='+') 
		|| (optsyntax=='+') 
		|| (optabout=='+') 
		|| (opthelptran=='+')
		|| (opthelprdw=='+')
		|| (opthelpseq=='+')
		|| (opthelpbug=='+')) {
		printhelp();
		fatal = 1;
		return fatal;
	}

	if (argc > argnum) {							// argv[1] = fn[.ext]
		memset(FNin, 0, sizeof(FNin));
		strcpy(FNin, argv[argnum]);
		argnum++;
		fin = fopen(FNin, "rb");
		if (!fin) {
			strcat(FNin, ".XMI");
			fin = fopen(FNin, "rb");
		} else { }
	} else {
		fin = fopen(FNin, "rb");
	}

	if (argc > argnum) {							// argv[2] = output ft
		memset(outext, 0, sizeof(outext));
		strcpy(outext, argv[argnum]);
		argnum++;
	}

	if (fin == NULL) {
		printf("Error opening input file %s\n", FNin);
		printf("Execution will terminate following display of help.\n");
		halt("Press enter to continue");
		optsyntax = '+';							// turn on syntax display
		printhelp();
		fatal = 1;
		return fatal; 
	}

	tranmap();									// see if user wants trans updates
	if (optmap=='+') {
		snapassumeascii = 1;
		snapshorthdr = 1;
		snap(trantab, 256, "EBCDIC to ASCII translation");
	}
	if (fatal) {
		printf("Execution terminated\n");
		return fatal;
	}
	return 0;
} /* cmdline */

//--------------------------------------------------------------------

//* Parse option

int parseopt(char *parg, char *pvalue, char *flag) {

	if ((strlen(parg) - 1) == strlen(pvalue)) {
		if (!(memcmp(&parg[1], pvalue, strlen(pvalue)))) {
			*flag = parg[0];
			return 1;
		} else 
			return 0;
	}
	return 0;
} /* parseopt */

//--------------------------------------------------------------------

//* Update trantab from user specifications

void tranmap() {
	FILE	*fmap;
	int		ndx, value, x, y;
	char	buf[FILENAME_MAX + 8];
	char	condext[FILENAME_MAX];
	char	*p;

	if ((fmap = fopen("RECV390.MAP", "r"))) {
		while (!(feof(fmap))) {
			memset(buf, 0, sizeof(buf));
			p = fgets(buf, 3, fmap);			// 1st 2 chars = hex index
			if (p == NULL)
				break;
			ndx = aschex(buf);
			if (ndx == 0x100) 
				break;
			fgetc(fmap);						// ignore space char
			memset(buf, 0, sizeof(buf));
			p = fgets(buf, 3, fmap);			// next 2 chars = hex value
			if (p == NULL)
				break;
			value = aschex(buf);
			if (value == 0x100) 
				break;
			memset(condext, 0, sizeof(condext));
			x = fgetc(fmap);					// space means extension follows
			if (x == ' ') {
				memset(buf, 0, sizeof(buf));
				p = fgets(buf, sizeof(buf) - 1, fmap);
				x = strlen(buf) - 1;			// ignore \r after extension
				y = memcmp(buf, outext, x);
				if (y)							// matches output extension?
					continue;					// no match; next record
				strcpy(condext, outext);
			}
			trantab[ndx] = (char) value;
			printf("EBCDIC %.2x = ASCII %.2x %s\n", ndx, value, condext);
		}
		fclose(fmap);
	}
	return;
} /* tranmap */

//--------------------------------------------------------------------

//* Convert ASCII "display hex" char string to hex

// Example:  c'2b' -> x'2b'

int aschex(char *p) {
	int		len, value;
	char	x, y;

	x = '0';
	value = 0x100;								// bad hex string value
	len = strlen(p);
	if ((len == 0) || (len > 2)) {
		printf("Invalid hex value %s\n", p);
		printf("Remainder of translation changes ignored\n");
		fatal = 1;
		return value;
	}
	y = p[0];
	if (isxdigit(y)) {
		if (len == 2) {
			x = p[1];
			if (isxdigit(x)) {
				sscanf(p, "%2x", &value);
			} else 
				printf("Invalid hex digit %c\n", x);
		} else {
			sscanf(p, "%1x", &value);
		}
	} else 
		printf("Invalid hex digit %c\n", y);
	if (value == 0x100) {
		fatal = 1;
		printf("Remainder of translation changes ignored\n");
	}
	return value;
} /* aschex */

//--------------------------------------------------------------------

//* Print syntax help

void printhelp( ) {
	if (opthelp=='+')	printf("\n");
	if ((optabout=='+') || (opthelp=='+')) {
		printf("\n");
		printf("Copyright 2000, 2001, Enhanced Software Services, Inc.\n");
		printf("V1R1M4 - doc & license at http://ensose.com/recv390.html\n");
	}
	if (opthelp == '+') {
		printf("\n"
			"This program runs on Windows or OS/2 in console mode.  "
			"It reads the output of \n"
			"the OS/390 TSO/E TRANSMIT command, "
			"and produces one file per PDS member.\n"
			"\n"
			"Only PDSes are supported.  "
			"RECFM F, V, and U are supported; spanned\n"
			"records (V[B]S) are not currently supported.  When ISPF adds VBS PDS member \n"
			"support, I'll seriously consider adding spanned record support.\n"
			"\n"
			"If an embedded MESSAGE is present, it will be displayed.  This is sometimes\n"
			"useful to keep track of the status of changes against the TRANSMITted data.\n"
			"\n"
			"If ISPF statistics are present in the PDS directory, they will be displayed\n"
			"prior to creating the output files, and record written counts verified.\n"
			"\n"
			"Options are specified with either a + or - sign in front of the option name.\n"
			"A + sign enables the option, a - sign disables the option; examples below.\n"
			"\n"
			"If any help option (below) is specified, no other action is taken after the\n"
			"help information is displayed.\n"
			"\n"
			);
		halt("Press enter to continue");
	}

	if ((optsyntax=='+') || (opthelp=='+')) {
		printf("\n");
		printf("Syntax: [options...] [[path]fn[.exti] [exto]]\n");
		printf("\n");
		printf(" path      path name of input file (default=current directory)\n");
		printf(" fn[.exti] input file name (default fn=OS390 ext=XMI)\n");
		printf(" [exto]    output file extension (default=TXT)\n");
		printf("           output fn = PDS member name\n");
		printf("                                            Help (default: not displayed\n");
		printf("Default options    on/yes(+)   off/no(-)          unless error occurs):\n");
		printf("------------------------------------------- ------------------------------\n");
		printf("+tran    translate from EBCDIC to ASCII     -about    copyright & version\n");
		printf("-seq     remove sequence field              -help     general help\n");
		printf("+trim    remove trailing blanks             -syntax   syntax help\n");
		printf("-rdw     no OS/390 RDW                      -helpbug  debug help\n");
		printf("+dir     display PDS directory              -helptran translation help\n");
		printf("+write   write output file(s)               -helprdw  RDW help\n");
		printf("+halt    issue <press enter> prompt         -helpseq  sequence field help\n");
		printf("+xmisum  display TRANSMIT dataset summary\n");
		printf("+dsattr  display dataset attributes\n");
		printf("-dirhex  don't display directory entries'\n");
		printf("         userdata in hex\n");
		printf("-map     don't show translation table\n");
		printf("\n");
		halt("Press enter to continue");
	}
	if (opthelp == '+') {
		printf("\n");
		printf("Syntax examples and their results\n");
		printf("---------------------------------\n");
		printf("%s +help      General help, no other actions taken\n", program);
		printf("%s +syntax    Syntax help, no other actions taken\n", program);
		printf("%s +about     Copyright & version, no other actions taken\n", program);
		printf("\n");
		printf("%s ABC        Input file ABC.XMI\n", program);
		printf("%s            Output files mem1.TXT, mem2.TXT ...\n", notprogram);
		printf("%s            Sequence field removed, trailing blanks removed\n", notprogram);
		printf("%s ABC.XMI    Exactly same as above example\n", program);
		printf("%s ABC ASM    Same input as above, output mem1.ASM, mem2.ASM ...\n", program);
		printf("%s            Sequence field removed, trailing blanks removed\n", notprogram);
		printf("%s +seq ABC   Preserve sequence field in each record\n", program);
		printf("%s -trim ABC  Sequence field removed, trailing blanks preserved\n", program);
		printf("%s +seq -trim Sequence field preserved, trailing blanks preserved\n", program);
		printf("%s            Input file OS390.XMI\n", program);
		printf("%s            Sequence field removed, trailing blanks removed\n", notprogram);
		printf("\n\n\n\n");
		halt("Press enter to continue");
		fatal = 1;
	}
	if (opthelptran=='+') {
		printf("\n");
		printf("Translation help:\n"
			"\n"
			"It is assumed the TRANSMITted dataset is encoded in the EBCDIC character set.\n"
			"There is a default translation table used to map EBCDIC to ASCII, which may\n"
			"be displayed with the +map option.\n"
			"If the default translation is not suitable, the user may supply an update\n"
			"specification in the RECV390.MAP file in the current directory.\n"
			"When the RECV390.MAP file is present, the change specifications in it will\n"
			"be applied before the +map option is processed, thus the user can determine\n"
			"what values will be applied when the output file(s) are written.\n"
			"The RECV390.MAP file must contain lines formatted exactly as below:\n"
			"\n"
			"First two characters:  EBCDIC character (example: C1 = EBCDIC A)\n"
			"Next character:        space\n"
			"Next two characters:   ASCII value (example: 41 = ASCII A)\n"
			"Next character:        end-of-line or space\n"
			"remaining characters:  conditional change extension (example: asm)\n"
			"\n"
			"When extension is present on the change line, the specified extension\n"
			"will be (case-sensitively) compared to the output extension (either\n"
			"specified on the command line or defaulted); when equal, the change\n"
			"will be applied to the table, otherwise the change line will be ignored.\n"
			"\n"
			);
		halt("Press enter to continue");
	}
	if (opthelprdw=='+') {
		printf("\n");
		printf("RDW help:\n");
		printf("\n"
			"RDW stands for Record Descriptor Word.  It is a four byte field which\n"
			"prefixes each record in an OS/390 RECFM V[B] (variable length record) file.\n"
			"RECV390 supports writing RDWs in front of any type of output record,\n"
			"be it RECFM F (fixed), V (variable), or U (undefined).\n"
			"\n"
			"RECFM U PDSes are frequently used to store OS/390 load modules.\n"
			"You may find it useful to use -tran +rdw for load modules, if you\n"
			"wish to somehow manipulate them on your PC.\n"
			"\n"
			"The format of the RDW is:\n"
			"\n"
			"  +0 length=2:  'length' of the following record, plus 4\n"
			"  +2 length=2:  two bytes of hex zeroes\n"
			"  +4 length=n:  record, of 'length' minus 4\n"
			"\n"
			"Example: x'0007',x'0000',c'ABC'\n"
			"\n"
			"Since the I/O model of a PC doesn't understand OS/390 'blocking', we don't\n"
			"do anything about writing BDWs (block descriptor words).\n"
			"\n"
			);
		halt("Press enter to continue");
		fatal = 1;
	}
	if (opthelpseq=='+') {
		printf("\n"
			"Sequence field help:\n"
			"\n"
			"Sequence fields are assumed to begin in columns 73 - 80 (relative one)\n"
			"for RECFM=F, columns 1 - 8 (relative one) for RECFM=V.  RECFM=U files\n"
			"are not expected to have sequence fields.\n"
			"\n"
			"For each record output, when the -seq option is in effect, the sequence\n"
			"field is verified to be all numeric.  Should any character in the sequence\n"
			"area fail this test, the record is assumed to contain no sequence field,\n"
			"and the area is preserved, hence ignoring the seq option.\n"
			"\n\n\n\n\n\n\n\n\n\n\n\n"
			);
		halt("Press enter to continue");
	}
	if (opthelpbug=='+') {
		printf("\n");
		printf("Debugging options:\n");
		printf("\n"
			"These debugging options are mostly for my use; however, you are\n"
			"more than welcome to play with them.\n"
			"They are a good way to begin understanding both the TRANSMIT\n"
			"wrapper data, as well as the IEBCOPY unload data formats.\n"
			"If you don't understand the output, you'll know exactly how I felt\n"
			"when I began developing RECV390.\n"
			"\n"
			"+list          list output records on stdout\n"
			"+block1        snap Data block one (IEBCOPY dataset attributes)\n"
			"+block2        snap Data block two (IEBCOPY mystery block)\n"
			"+dumpdir       snap IEBCOPY directory block(s)\n"
			"+getseg        print getseg() summary information for each segment\n"
			"+snapseg       snap getseg() data (length and flag: use +getseg)\n"
			"+getblock      print getblock() summary information for each Data block\n"
			"+snapblock     snap IEBCOPY Data block contents\n"
			"+snaphalt      pause for <press enter> during snap of data\n"
			"               (snap data is presented 256 bytes at a time)\n"
			"\n"
			"P.S.  If anyone figures out the contents of 'block1' and 'block2',\n"
			"please let me know.  Parts of them are still a mystery to me.\n"
			"\n"
			);
		halt("Press enter to continue");
		fatal = 1;
	}
	return;
} /* printhelp */

//--------------------------------------------------------------------

//* Clean up prior to exiting

void cleanup( ) {

	if (fin)	fclose(fin);
	if (fout)	fclose(fout);
	if (block)	free(block);
	if (dir)	free(dir);

	if (fatal == 0) {
		printf("\n");
		printf("Read %d bytes of %s\n", segbytesread, FNin);
		printf("Read %d bytes of %s data\n", databytesread, dsn);
		if ((fileswritten == 0) || (fileswritten > 1)) 
			printf("Wrote %d files, %d bytes, %d records\n", 
					fileswritten, databyteswritten, datarecswritten);
		else
			printf("Wrote %d file, %d bytes, %d records\n",  
					fileswritten, databyteswritten, datarecswritten);
		if ( (dirlen / FIXED_ENTRY_LENGTH) != fileswritten) 
			printf("Warning - # PDS members vs. # files written mismatch\n");
		if (warncounts)
			printf("Warning - %d PDS member(s) appear to have wrong number of records\n",
				warncounts);
		printf("Done\n");
	}
	return;
} /* cleanup */

//--------------------------------------------------------------------
//   Various utility routines
//--------------------------------------------------------------------

// Get OS/390 binary field (Big Endian) of length "len"

int getvbin( char * ptr, int len) {
	int				binval, i;
	unsigned char	byte1;

	binval = 0;
	for (i = 0; i < len; i++) {
		byte1 = * (unsigned char *) ptr;
		binval = (binval * 256) + (int) byte1;
		ptr++;
	}
	return binval;
} /* getvbin */

//--------------------------------------------------------------------

// set string to nulls, copy lenin chars to out

void strxset( char * out, int lenout, char * in, int lenin) {
	
	memset(out, 0, lenout);
	memcpy(out, in, lenin);
	return;
} /* strxset */

//--------------------------------------------------------------------

// Convert EBCDIC to ASCII

void ebcdic2ascii( void *in, int len) {
	char *			ptr;
	int				i;
	unsigned char   oldchar;
	unsigned char   newchar;

	for (i = 0; i < len; i++) {
		ptr = &((char *)in)[i];
		oldchar = *ptr;
		newchar = trantab[oldchar];
		((unsigned char *)in)[i] = newchar;
	}
	return;
} /* ebcdic2ascii */

//-----------------------------------------------------------------------------

//* Segmented snap so data doesn't roll off screen

void snaplong( void * p, int len, char * title) {
	int		snapseg = 256;
	int		snaplen;
	int		i, rc;

	if (optsnaphalt=='+') {
		for (i = 0; i < len; i = i + snapseg) {
			if (len > snapseg)
				printf("Segmented snap; total length %d, offset %d (0x%.2x)\n", len, i, i);
			snaplen = len - i;
			if (snaplen > snapseg)
				snaplen = snapseg;
			rc = jmmsnap(&((char *)p)[i], snaplen, i, title);	
			if (rc)
				return;
		}
	} else 
		jmmsnap(p, len, 0, title);

	return;
} /* snaplong */

//-----------------------------------------------------------------------------
//
//                        Snap  -  Version 1.1
//                 Copyright, 1993, 2000  James M. Morrison
//
// 22 March 2000 Intel-specific version  -  supports EBCDIC characters 
//
//      This code provides the caller with a 'display hexadecimal' dump
//      of the supplied storage.  The output looks like:
//
//      <snap> 000290DC.29 (0x001D) title:
//      000290dc 0000  48656c6c 6f207468 65726520 616e6420 *Hello there and *
//      000290ec 0010  686f7720 61726520 796f753f 00       *how are you?.*
//
//      Here's what the first line is about:
//
//      <snap>      Snap tag
//
//                  Looks pretty silly, until you have a (large) wrong length,
//                  then it makes scanning the snap output in your text editor
//                  quite a bit easier.
//
//      000290DC    Address
//
//                  The address of the storage you have requested be snapped.
//                  You might not care what the address is, but if you blow
//                  up, it might prove handy to have.
//
//      29          Length of storage request in decimal
//
//      (0x001D)    Length of storage request in hexadecimal
//
//      title:      User supplied title
//
//      Each line following consists of:
//
//      000290dc    Beginning address of storage displayed on this line
//
//      0000        Offset from beginning of storage request, in hexadecimal
//
//      48656c6c    Hexadecimal representation of storage
//
//                  Sixteen bytes per line, spaced every four bytes.
//
//      *Hello there and *      Character representation of storage
//
//                  Sixteen bytes per line, no extraneous spaces.
//                  The asterisks (*) delimit the characters, since
//                  the last line might possibly be shorter than the rest.
//                  Unprintable characters (ANSI C definition) are shown
//                  as periods (.) in case you want to send the output to
//                  a device (like a printer) which might take offense at
//                  raw hex data showing up in it's data stream.
//
//-----------------------------------------------------------------------------
// 

#define prt(x)   printf(x)         // use ANSI C printf

//-----------------------------------------------------------------------------
//
//      Snap - display data area in hex and character format
//
//      Arguments:
//
//               pointer to storage area
//
//               length of storage area
//
//				 offset to begin display at
//
//               title (null terminated string)
//
//-----------------------------------------------------------------------------

int jmmsnap( void *ptr, int len, int offset, char *title)
{
   int i, j, k, m, x, cd;
   char workchar;
   unsigned int  workint;
   char prtbuf[128];
   char hdrbuf[128];

   //--------------------------------------------------------------------------
   //   Display user-supplied title
   //--------------------------------------------------------------------------
	if (snapcr)
		printf("\n");
	if (snapshorthdr) {
		prt(title);
		prt("\n");
	} else {
		sprintf(hdrbuf, "<snap> %p.%d (0x%.4X) %s\n", ptr, len, len, title);
		prt( hdrbuf );
	}
	snapshorthdr = 0;
	prtbuf[0] = '\0';

   for (i=0; i < len; i = i + 16) {

      //-----------------------------------------------------------------------
      //    Build hexadecimal display
      //-----------------------------------------------------------------------

		if (showsnapaddr)
			x = sprintf(prtbuf, "%.8X %.4x  ", 
				(int)(ptr + i), i + offset);
		else
			x = sprintf(prtbuf, "%.4x  ", i + offset);
		for (j = 0; j < 16; j = j + 4) {
			for (k = 0; k < 4; k++) {
				if (i+j+k < len) {
					workchar = * ((char *)ptr+i+j+k);
					workint = workchar;
					// following line of code is Intel-specific
					memset(((char *)&workint)+1, 0, sizeof(workint) - 1);
					sprintf(&prtbuf[x], "%.2x", workint);
				} else
					memset(&prtbuf[x], ' ', 2);
				x = x + 2;
			}
			prtbuf[x] = ' ';           // space between 4 bytes of data
			x = x + 1;
		}

      //-----------------------------------------------------------------------
      //    Build character display
      //-----------------------------------------------------------------------

      prtbuf[x] = '*';
      x = x + 1;
      for (m = 0,cd = 0; (m < 16) & (i+m < len); m++) {
		  workchar = * ((char *)ptr+i+m);
		  if (snapassumeascii == 0) 
			  ebcdic2ascii(&workchar, 1);
		  if (isprint(workchar)) {
			  prtbuf[x+m+cd] = workchar;
		  } else {
			  workchar = * ((char *)ptr+i+m);
			  if (isprint(workchar))
				  prtbuf[x+m+cd] = workchar;
			  else
				  prtbuf[x+m+cd] = '.';
		  }
//       prtbuf[x+m+cd] = isprint(*((char *)ptr+i+m)) ? *((char *)ptr+i+m) : '.';
		 if (prtbuf[x+m+cd] == '%') {
			 prtbuf[x+m+cd+1] = '%';		// double up % chars
			 cd++;
		 }
	  }	
      prtbuf[x+m+cd  ] = '*';
      prtbuf[x+m+cd+1] = '\0';
      prt(prtbuf);                  // end of line
      prt("\n");

   }
   snapassumeascii = 0;					// reset for next call
   if (optsnaphalt=='+') {
		x = halt(NULL);
		return x;
   }
   return 0;
} /* snap */

//--------------------------------------------------------------------

int halt(char *msg) {
	char	buf[4];

	if (opthalt=='-')
		return 0;
	if (msg == NULL)
		fprintf(stderr, "Press enter to continue, or type 'x' to exit\n");
	else
		fprintf(stderr, "%s\n", msg);
	fgets(buf,sizeof(buf), stdin);
	if (buf[0] == 'x')
		return 1;
	return 0;								// continue
} /* halt */

