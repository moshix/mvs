
[![Discord](https://img.shields.io/discord/423767742546575361.svg?label=&logo=discord&logoColor=ffffff&color=7389D8&labelColor=6A7EC2)](https://discord.gg/vpEv3HJ)
<a href=" https://github.com/moshix/mvs/blob/master/codenotary.com"><img src="https://raw.githubusercontent.com/moshix/mvs/master/secured-by-immudb.svg" width="109px;"/></a>
<a href="https://hits.seeyoufarm.com"><img src="https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fmoshix%2Fmvs&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=hits&edge_flat=false"/></a>
[![View SBOM](https://img.shields.io/badge/sbom.sh-viewSBOM-blue?link=https%3A%2F%2Fsbom.sh%2Fad375d15-0553-4170-bb1e-a6e40ac5dd45)](https://sbom.sh/ad375d15-0553-4170-bb1e-a6e40ac5dd45)

# MVS and VM Goodies


Tapes are the standard way to exchange files, utilities and information between mainframers. Some mainframe shops are now connected thru the HNET Bitnet II international dial-up network. I also noticed that recently the industry is moving towards a new fad called the "World Wide Web", which is why I now make this collection of tools and utilities available on Github in this crazy new world of the Internet!

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
 SYS2.PROCLIB on SYSCPK. This proclib has procedures for all the included compilers. The run-time libraries, like SYSC.PLILIB are of course included in the volume. 

 I  tested in on everything from MVS 3.8 to OS390 all the way to z/OS 1.13 and it works beautifully. There is also a version of this volume on 3350 disk image, but OS390 and up don't have support for such old disks anymore. Copying stuff over to a 3390 disk isn't trivial because of the blocking required by certain compiler libraries. In short, there is a need for this volume. 

3. sort.jcl is a sort job which stresses a Tk4 MVS 3.8 by creating millions of random records and then using IBM Sort to sort them. Requires TK4-

4. The amazing standalone operating system for S/360 from Prof Madnick's seminal book on operating system. I also include the assembly output. 

5. My .vimrc file for VIM. It color highlights correctly JCl and assembler for S/370. 

6. VM goodies

7. The usefuljcl/ directory contains very useful JCL jobs which you will use on adaily basis as a MVS or z/OS SYSPROG

8. a calculator for ideal blocksize given a DASD type and a logical record length, written by me in Go language. It's called blocksize.go and I also inlude a Linux 64bit binary

9. the PC370 source code for the simply amazing S370 assembler which runs in MS-DOS. 

10. Lots of JCL and source code files which I use in my moshix mainframe channel vidoes

11. Some JCL for Cobol and VSAM on z/OS

12. My REXX programs for both z/OS and MVS 3.8 to get information from the operating system, such as online users, IPL time etc. 

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

27. VM/370 editor
    
28. The TELPAR operating system for EDA (Electronic Design Automation) and 
 ATG (Automatic Test Generation) of Stanford University

29. A disassembler

30.  Sample parameterhandling jobs, 

31.  submit bash script for z/OS or MVS workflow in vim

32.  3270 extended attribute codes
    
33. A treatise on the handling of virtual storage in SVS, MFT, MVT and MVS

34.  some very cool VM/370 add-ons. 

35.  a tape with VM/370 games. 

36. Matlab for VM/CMS  

37. A JCL and PLI aware small editor

38. A generalized IP address parser for all your NICS (important when you do IPv4 stuff with Hercules)  

39.  An HP41 calculator in Fortran and REXX

40.  an IBM 1403 font
  
42.  and more...much, much more

Useful Links and Invocations
============================

z/OS 1.13 Indexed full set of manuals: https://www-40.ibm.com/servers/resourcelink/svc00100.nsf/pages/zosv1r13-pdf-download?OpenDocument#ASM
<br>




Printing on VM
===============

Start 00e cl a nosep, on the operator console.  

spool print system  
<br>print file listing a   

moshix   
<br>
Tel Aviv, October 7, 2023 
