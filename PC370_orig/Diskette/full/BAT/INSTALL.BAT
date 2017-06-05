rem   PC/370 installation command file to create %2:\R42 from %1.
rem   This command requires two parameters as follows:
rem     1.  Source floppy disk drive (usually A or B).
rem     2.  Target hard disk drive (usually C or D).
rem   Source drive = %1 and destination drive = %2
rem   If the above source and destinations are wrong, enter CTL-BREAK
pause If ok, press enter to create directories
%2:
mkdir R42
cd R42
mkdir BAT
mkdir LIB
mkdir CPY
mkdir MAC
mkdir CBL
mkdir DOC
mkdir DEMO
mkdir UTIL
pause Insert PC/370 distribution disk with \R42 directory in drive %1 and enter
copy %1:\read.me
copy %1:\R42\*.*
copy %1:\BAT\*.* BAT
copy %1:\CPY\*.* CPY
copy %1:\LIB\*.* LIB
pause Insert PC/370 distribution disk with \DEMO directory in drive %1 and enter
copy %1:\DEMO\*.* DEMO
copy %1:\DOC\*.*  DOC
copy %1:\MAC\*.*  MAC
pause Insert PC/370 distribution disk with \CBL directory in drive %1 and enter
copy %1:\UTIL\*.* UTIL
copy %1:\CBL\*.* CBL
rem   The PC/370 system is now installed.
rem   The batch commands can be run using the current directory.
rem   To run the macro preprocessor demo enter, bat\runmac.
rem   To run the Micro Focus COBOL/2 subroutine demo enter, bat\runcbl.
rem   To run the utility demos enter, bat\runutil.
rem   To rebuild the subroutine library from source, enter bat\bldlib.
rem   To rebuild the utilities from source, enter bat\bldutil.
pause Now to run the PC/370 demo programs, press enter.
bat\rundemo
