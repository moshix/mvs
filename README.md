# MVS and z/VM Utilities

this repository includes some very useful utilities for people working with MVS, z/OS on real mainframes or emulated ones. Most of the topics covered by these programs are covered in my youtube mainframe channel here: https://www.youtube.com/channel/UCR1ajTWGiUtiAv8X-hpBY7w

Here are some of the programs and tools included in this repo:

1. recv390 is a receive command for Linux or Windows. It extracts XMIT files. recv390 is by James Morrison. I made some fixes and 
   somall enhancement to the program. compile simply with gcc -o recv90 recv390.c.


2. SYCPLK.ccdk is a 3390-3 volume (SYSCPK) which incluedes about 10 or 12 compilers for the mainframe, including:

- PLI F compiler
- Cobol compiler from the 60s
- RPG compiler
- PL/360 compiler
- Pascal compiler
- the amazing ASSIST assembler
- Simula compiler
- Basic
- and many other goodies. 

 Simply mount the volume on your system, add it to the VTLST00 member as private, and then extend your JES2 to use the included
 SYS2.PROCLIB on SYSCPK. This proclib has procedures for all the included compilers. The run-time libraries, like SYSC.PLILIB are of course included in the volume. 

 I  tested in on everything from MVS 3.8 to OS390 all the way to z/OS 1.13 and it works beautifully. There is also a version of this volume on 3350 disk image, but OS390 and up don't have support for such old disks anymore. Copying stuff over to a 3390 disk isn't trivial because of the blocking required by certain compiler libraries. In short, there is a need for this volume. 

3. sort.jcl is a sort job which stresses a Tk4 MVS 3.8 by creating millions of random records and then using IBM sort to sort them. 

4. the amazing standalone operating system for S/360 from Prof Madnick's seminal book on operating system. I also include the assembly output. 

5. my .vimrc file for VIM. It color highlights correctly JCl and assembler for S/370. 

6. my PL/I implementation of the N x N queens problem

7. Sorichetti's vpwpc output seprator for Hercules printers

8. The usefuljcl/ directory contains very useful JCL jobs which you will use on adaily basis as a MVS or z/OS SYSPROG

9. a calculator for ideal blocksize given a DASD type and a logical record length, written by me in Go language. It's called blocksize.go and I also inlude a Linux 64bit binary

10. the PC370 source code for the simply amazing S370 assembler which runs in MS-DOS. 

11. Lots of JCL and source code files which I use in my moshix mainframe channel vidoes

12. Some JCL for Cobol and VSAM on z/OS

13. My REXX programs for both z/OS and MVS 3.8 to get information from the operating system, such as online users, IPL time etc. 

14. SMF reporting tools

15. Rexx panel programs for TSO

16. Panel/Rexx programs for TSO




Enjoy!

moshix
