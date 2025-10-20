
# MVS and VM Software

For discussion on the topics covered in this repository, join the Forum3270 BBS (a BBS for 3270 terminals) at [Forum3270](https://www.moshix.tech/:3270)

This repository includes some very useful utilities for people working with MVS, z/OS, VM/SP and z/VM on real mainframes or emulated ones. Most of the topics covered by these programs are covered in my youtube mainframe channel here: https://www.youtube.com/channel/UCR1ajTWGiUtiAv8X-hpBY7w

Here are some of the programs and tools included in this repo:

1. recv390 is a receive command for Linux or Windows. It extracts XMIT files. recv390 is by James Morrison. I made some fixes and small enhancements to the program. Compile simply with gcc -o recv90 recv390.c.


2. SYCPLK.ccdk is a 3390-3 volume (SYSCPK) which includes about 10 or 12 compilers for the mainframe, including:

- PLI F compiler
- Cobol compiler from the 60s
- RPG compiler
- PL/360 compiler
- Pascal compiler
- the amazing ASSIST assembler
- Simula compiler
- Basic
- and tons more!

 Simply mount the volume on your system, add it to the VTLST00 member as private, and then extend your JES2 to use the included
 SYS2.PROCLIB on SYSCPK. This proclib has procedures for all the included compilers. The run-time libraries, like SYSC.PLILIB are of course included in the volume. There is also a version of this volume on 3350 disk image, but OS390 and up don't have support for such old disks anymore.

3. sort.jcl is a sort job which stresses a TK4 MVS 3.8 by creating millions of random records and then using IBM Sort to sort them. Requires TK4-

4. The amazing standalone operating system for S/360 from Prof Madnick's seminal book on operating system. I also include the assembly output. 

5. My .vimrc file for VIM. It color highlights correctly JCl and assembly for S/370. 

6. VM goodies

7. The usefuljcl/ directory contains very useful JCL jobs which you will use on adaily basis as a MVS or z/OS SYSPROG

8. a calculator for ideal blocksize given a DASD type and a logical record length, written by me in Go language. It's called blocksize.go and I also inlude a Linux 64bit binary

9. the PC370 source code for the simply amazing S370 assembler which runs in MS-DOS. 

10. Lots of JCL and source code files which I use in my moshix mainframe channel vidoes

11. Some JCL for Cobol and VSAM on z/OS

12. A REXX programs for both z/OS and MVS 3.8 to get information from the operating system, such as online users, IPL time etc. 

13. SMF reporting tools

14. Rexx panel programs for TSO

15. Panel/Rexx programs for TSO

16. LOGREC cleaning JCL

18. Creating users on TSO HOWTO

19. Sort jobs example with IBM Sort (Iceman)

21. My .X3270pro file (UPDATE: NOW WITH TURBOPASCAL theme)

22. ISPF Panel Rexx examples

23. JSON code for interaction between Golang and DB2

24. A z/VM machine load information utility

25. A working copy of the original CHRIMAS EXEC (OR CHRISTMA EXEC) worm from 1988

26. A payroll report source code in MVT Cobol with test data and the JES2 output listing

27. A VM/370 editor
    
28. The TELPAR operating system for EDA (Electronic Design Automation) and 
 ATG (Automatic Test Generation) of Stanford University

29. A disassembler

30.  Sample parameterhandling jobs, 

31.  submit bash script for z/OS or MVS workflow in vim

32.  3270 extended attribute codes
    
33. A treatise on the handling of virtual storage in SVS, MFT, MVT and MVS

34. some very cool VM/370 add-ons. 

35. a tape with VM/370 games. 

36. Matlab for VM/CMS!!    

37. A JCL and PLI aware small editor

38. A generalized IP address parser for all your NICs (important when you do IPv4 stuff with Hercules)  

39. An HP41 calculator in Fortran and REXX

40. an IBM 1403 font
    
41. an assembler in bash
  
42. and more...much, much more  

General invocations
===================

**Printing on VM**

Start 00e cl a nosep, on the operator console.  

spool print system  
print file listing a   


**Printing on MVS without JES2 charset translation**

$T PRINTER(1),TRANS=NO


**CCKD support for Hercules (althought new bugs were discovered in CCKD as of 4.7)**

***@ubuntu:~$ sudo apt install zlib1g-dev

***@ubuntu:~$ sudo apt install libbz2-dev

Quick initialize a new 3390 without using MVS, with dasdload and this following config file:
*
* Pack layout file for MOSHIZX volume
*
moshix 3390
sysvtoc        vtoc    trk 29


Where to go next?
=================

Check out my ISPF website at [www.moshix.tech](https://www.moshix.tech/) where you find NJE resources and a whole bunch of MVS 3.8 mainframe games!

moshix   
<br>
Salzburg, October 2025
