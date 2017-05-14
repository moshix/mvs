Sample Operating System Version 2.00
====================================

The book "Operating Systems" by Stuart E. Madnick and John J. Donovan
(McGraw-Hill 1974) describes a sample operating system suitable to run on
IBM S/360 and S/370 computers. It evolved from the thesis work of John
DeTreville and was further refined by Richard Swift.

Peter Flass typed in the source from the assembly listing in the book and
made it available to the Hercules community in 2002. At one time Peter had
it running under VM/370, which he achieved by disabling the storage protection
key handling, i.e. running it without any storage protection. This still was
state of affairs when I stumbled across Peter's source in October 2015.

As Hercules in S/370 mode defaults to emulating a system without having the
storage-key 4K-byte-block facility installed, storage protection works the
same way it did on the S/360 which was the original target of the sample
operating system. Thus I was able to reinstate storage protection and bring
the source back to the exact state as printed in the book. I didn't verify this
line by line, but I cross checked the program length and quite a few offsets
for being identical, which they are, so I think this assumption is robust.

As expected the system assembled using this "original" source, works under
Hercules in S/370 mode exactly as documented in the book (there are some rough
edges in loading (IPLing) it, and in handling the card readers, though). From
a software conservation point of view it deemed important to me to publish this
unmodified version of the sample operating system in the hercules-390 Yahoo
group.

At first, I didn't intend to do much more. However, there was quite a bit of
resonance in this group and the more I looked into it, the more its simple yet
versatile design fascinated me. This lead me to creating a few use cases
demonstrating some capabilities of the system. Additionally I introduced a few
enhancements to the system, from redesigning storage protection to 4K key
blocks, over smoothening IPL and card reader handling, up to introducing
new funtionality to the EXCP device handler. These enhancements were done
"minimally invasive", i.e. there was nothing changed that didn't _need_ to be
changed to achieve the desired outcome.


Packaging and Installation:
---------------------------

All of the above was published ad'hoc as it came to live during the November to
December 2015 timeframe. This lead to a somewhat chaotic chain of updates to the
sample operating system, each of which depending on its predecessor, while on
the other hand the use cases don't depend on each other and got in parts
overlaid by the later update packages.

To bring packaging back into a defined state I'm now publishing the "3270
Graphics Demo" (a 3270 PSS graphics based SOS logo) as a "final" use case, which
at the same time consolidates all the previous ones into a single ZIP archive,
making them accessible through Hercules scripts.

The Madnick_3270_Graphics_Demo.zip package is the _only_ one needed to get
_all_ use cases that were discussed over the past two months. It is installed by
simply unzipping it into an arbitrary _empty_ folder. File README.txt in the
root folder of the unzipped archive contains information on the system's current
configuration and on how to run the use cases.

For those interested in the complete update chain the previous "single use case"
packages remain available for download. Namely the first package
("Madnick_for_Hercules.zip") may be of particular interrest as it features the
original 1974 system, running out of the box under Hercules.

It should be avoided to intermix both installation types, i.e. don't install
any of the previous packages into the same folder as the 3270 Graphics Demo
package and don't install the 3270 Graphics Demo package into the same folder
as any of the previous ones.


Files in Folder "Sample Operating System":
------------------------------------------

README.txt                      --  This file.

                                  / Consolidated package containing all single
                                 /  use case packages plus a 3270 graphics SOS
                                /   logo and, as a bonus, three variants of the
Madnick_3270_Graphics_Demo.zip <    "99 Bottles of Beer" song lyrics. Install
                                \   only this package, unless you particularly
                                 \  want to look at one of the previous states
                                  \ of the resurrection and refurbishment work.

Madnick_3270_Terminal_Demo.zip  \    Single use case packages retained to
Madnick_with_Console.zip         \   document the sequence of steps taken during
Madnick_Sieve_Primes.zip          \  the resurrection and refurbishment of the
Madnick_4_KB_RDR_for_Hercules.zip /  Sample Operating System. None of these
Madnick_4_KB_for_Hercules.zip    /   packages is needed any more to install the
Madnick_for_Hercules.zip        /    system in its current state.


Credits:
--------

Stuart E. Madnick   \ authors of the book "Operating Systems"
John J. Donovan     / (McGraw-Hill 1974)
John DeTreville     \ authors of the Sample Operating System
Richard Swift       / as listed in "Operating Systems"
Peter Flass        -- made the source he typed in from the listing printed
                      in the book available electronically in 2002
Harold Grovesteen  \  various helpful comments in the hercules-390 Yahoo
Laddie Hanus        > group, particularly for sorting out the state of affairs
Martin Zettel      /  of the storage protection logic in Peter's upload
James Francis Cray -- digged Peter's source out in October 2015


----------
2015/12/18, Juergen Winkelmann, ETH Zuerich
e-mail: winkelmann@id.ethz.ch
