Sample Operating System Version 2.00 - Console I/O Via EXCP Device Handler Demo
===============================================================================

The book "Operating Systems" by Stuart E. Madnick and John J. Donovan
(McGraw-Hill 1974) describes a sample operating system suitable to run on
IBM S/360 and S/370 computers.

While providing a programming interface for card readers and printers, the
sample operating system doesn't come with equivalent support for console
devices. It provides, however, an "EXCP Device Handler" allowing user programs
to execute channel programs addressing arbitrary devices, as long as UCBs for
these devices are defined in the UCB table. Thus, after adding a "console UCB"
to the UCB table and after adding a matching device to the hardware (Hercules),
user programs can perform "console I/O" using the EXCP device handler.

As opposed to a system wide console task handling console I/O asynchronously
from requesting programs, allowing user programs to access the console device
directly requires serialization. The sample operating system serializes the I/O
using a wait semaphore defined in the UCB. That means all requests are queued
and user programs have to wait until it's their turn to access the device.

Serialized console I/O shouldn't be a problem when messages are to be sent to
the console, as long as no message flooding occurs. However, when it comes to
reading from the console (replies, parameters, etc.) serialization can lead to a
complete halt of running jobs if a read request isn't answered in a timely
manner. This has to be kept in mind when using such a "poor man's" console.

The Console I/O package contains an updated version of the demo user program
shown in figure 7-11 of the "Operating Systems" book. In addition to printing a
message confirming its successful entry into the system, it reads "parameters"
from the console and echoes them back to the printer. To support this program a
UCB to handle a "console" at 009 has been added to the sample operating system.

This is meant as a proof of concept style example on using the EXCP device
handler only. In particular, I/O is not checked for completeness, success or
failure, which certainly would need to be done for real world usability.


Installation:
-------------

Before installing the Console I/O package, please ensure you have the sample
operating system with the card reader handling enhancement installed, as found
and described in

https://groups.yahoo.com/neo/groups/hercules-390/files/Madnick_4_KB_RDR_for_Hercules.zip

It is recommended to install the Console I/O package on a separate copy of the
sample operating system, as it changes the Hercules configuration (hardware)
and the system configuration (device support). To install the package unzip the
archive found at

https://groups.yahoo.com/neo/groups/hercules-390/files/Madnick_with_Console.zip

into the folder containing the copy of the sample operating system to be used,
allowing the unzip program to merge folders and replace files already existing.


Contents:
---------

README_with_Console.txt - this file
conf/madnick.cnf        - Hercules configuration file
source/sos4krdc.asm     - sample OS source, updated to include console UCB
source/sosuserc.asm     - demo user program source, updated for console I/O
rdr/sample_operating_system_version_2.00.ipldeck - card deck to IPL sample OS
rdr/demo_user_program.deck - card deck to submit the demo user program
sysgen/madnick_with_console.xmi - XMITted PDS containing source and
                                  build information

Note that rdr/sample_operating_system_version_2.00.ipldeck is identical to
member IPL4KRDC of the PDS contained in sysgen/madnick_with_console.xmi.
Follow the instructions in member $README of this PDS to rebuild the IPL
deck from source.


Usage:
------

The following steps provide minimal information to IPL the sample operating
system and to run the demo user program from four card readers simultaneously:

o make sure to have Hercules in your path
o run start_herc (*i*x systems) or start_herc.bat (Windows systems)
o connect a telnet (_not_ tn3270) session to your local port 3215
o enter "ipl c" at the Hercules console prompt
o The system will enter a wait state (PSW=FE0200008000056A) when it is ready
  to process jobs
o enter "script scripts/load_card_decks" at the Hercules console prompt and
  reply politely to the requests being issued at the telnet session
o the system will enter a wait state (PSW=FE0200008000056A) after completion
  of the four jobs
o review the output of the four jobs in folder prt
o to rerun, enter "script scripts/load_card_decks" at the Hercules console prompt



Have fun!

----------
13.11.2015, Juergen Winkelmann, ETH Zuerich
e-mail: winkelmann@id.ethz.ch
