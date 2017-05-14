# mvs
this repository includes some very useful utilities for people working with MVS, z/OS on real mainframes or emulated ones. 

1. recv390 is a receive command for Linux or Windows. It extracts XMIT files. recv390 is by James Morrison. I made some fixes and 
   somall enhancement to the program. compile simply with gcc -o recv90 recv390.c.


2. SYCPLK.ccdk is a 3390-3 volume (SYSCPK) which incluedes about 10 or 12 compilers for the mainframe, including:
 PLI F compiler
 Cobol compiler from the 60s
 RPG compiler
 PL/360 compiler
 Pascal compiler
 the amazing aSSIST assembler
 Simula compiler
 and many other goodies. 

 Simply mount the volume on your system, add it to the VTLST00 member as private, and then extend your JES2 to use the included
 SYS2.PROCLIB on SYSCPK. This proclib has procedures for all the included compilers. The run-time libraries, like SYSC.PLILIB are of course included in the volume. 

 I  tested in on everything from MVS 3.8 to OS390 all the way to z/OS 1.13 and it works beautifully. There is also a version of this volume on 3350 disk image, but OS390 and up don't have support for such old disks anymore. Copying stuff over to a 3390 disk isn't trivial because of the blocking required by certain compiler libraries. In short, there is a need for this volume. 

3. sort.jcl is a sort job which stresses a Tk4 MVS 3.8 by creating millions of random records and then using IBM sort to sort them. 

4. the amazing standalone operating system for S/360 from Prof Madnick's seminal book on operating system. I also include the assembly output. 

5. my .vimrc file for VIM. It color highlights correctly JCl and assembler for S/370. 

6. my PL/I implementation of the N x N queens problem

7. Sorichetti's vpwpc output seprator for Hercules printers
