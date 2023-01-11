# C64 Assembly Coding Guide

Hi, I'm Spiro, aka @notawizard on Youtube and the fediverse and I would like to help you get started programming in assembly language for the Commodore 64.

This page doesn't cover everything (nothing ever could be), just what I know and have experience with, and tried to narrow down to a few good options without overloading you with *everything*. :)

I also haven't mentioned IDEs and editors as they are are personal choice (and subject to religious wars which I don't care to partake in). The cross assemblers I have included tend to be command line based and can often be tied into whatever editor/IDE you use. If command line scares you, then assembly will make you shit bricks, so you may as well save your sanity now and find a different hobby.

A quick note on the CPU architecture. The c64 uses a [MOS 6510](https://en.wikipedia.org/wiki/MOS_Technology_6510), which is 100% compatible with the [6502](https://en.wikipedia.org/wiki/MOS_Technology_6502). All it added was a few I/O address pins which allowed the c64 to bank switch different portions of memory out (you'll find out more about this with the 64k of RAM having ROM and BASIC doubled up on top).

You will need to learn 6502 machine language and can use any source, whether it's for the c64 specifically, or Apple ][, NES, or Atari8.

#### Machine Language vs Assembly Language vs Machine Code

The terms are used interchangeably, but do actually have different meanings and I feel it's important for someone new to assembly to understand the distinction.

First off, "**machine code**" refers to the native hex or binary representations of the CPU's instructions. We don't use machine code. We use human readable opcodes like LDA, STA, JMP, etc, which we call mnemonics, and mnemonics and memory addresses (usually written in hex) are the building blocks of "**machine language**."

A short program in machine language might look like:

	$C000	INC $D020
	$C003	JMP $C000

The first column is the memory addresses (which you don't type in, but need to refer to where your branches will happen). The second and third columns are the machine language instructions.

Normally, you would use an "ML monitor" for entering machine language, such as SuperMon or from a freezer cartridge like Action Replay or using the one built into your emulator.

"**Assembly language**" refers to the extra luxuries that an assembler gives you on top of "machine language." For example, text labels, psuedo opcodes, the ability to do simple maths when referring to numbers, macros, and so on. 

The same short program in assembly language might look like:

	loop:
		INC $D020
		JMP loop

If you find a resource teaching you "assembly language" and they use the term properly, then you will have to use the same assembler that they use or you will need to know how to change it to the syntax your assembler uses.

Some things remain the same across assemblers, but many don't. Their own features made the selling points for why you should buy or use their assembler over anyone elses. :)

But more importantly, you should start with learning machine language on a monitor because it will give you the fundamental skills you will be using to debug your code when you move to an assembler.

It also means that you can look at code written for other assemblers and have a good understanding of what you need to change to get that code to work in your preferred assembler.

There are no shortcuts in learning assembly language. You can either waste your life looking for them, or you can pull your socks up and be productive. If you want the easy path to coding success, go learn a modern language like Python. But if you want to be a complete sadomasochist and learn to program to the bare metal on a 40 year old computer, then go all in baby!

;)


## Start Here

This section will have a few links to the absolute fundamentals of the 6502/6510 architecture and are necessary pre-requisites for you to code in machine/assembly language on the c64.

* [Assembly in one step](https://dwheeler.com/6502/oneelkruns/asm1step.html) - the absolute bare minimum you need to know to program in assembly on the 6502. 
* [Commodore 64 Architecture](https://www.atarimagazines.com/compute/issue32/112_1_COMMODORE_64_ARCHITECTURE.php) - from Compute! issue 32, Jan 1983 (by Jim Butterfield)


## Reference

* [6502 opcodes](http://www.6502.org/tutorials/6502opcodes.html)
* [c64 standard kernal functions](https://sta.c64.org/cbm64krnfunc.html)
* [c64 memory map](https://sta.c64.org/cbm64mem.html)
* [c64 colour codes](https://sta.c64.org/cbm64col.html)
* [C=Hacking](http://www.ffd2.com/fridge/chacking/) - C=Hacking was a plaintext technical publication for the Commodore 8-bit community that ran from 1992 to 2002
* [The MOS 6567/6569 video controller (VIC-II) and its application in the Commodore 64](https://csdb.dk/release/?id=154952) - a very thorough paper on the documented and undocumented features of the VIC-II (by Christian Bauer)
* [VIC Timing](https://www.linusakesson.net/programming/vic-timing/victiming.pdf) - PDF file of the raster timing on the VIC (by Linus Åkesson aka LFT)
* [The Dreams: AAY-Helpfiles](https://www.the-dreams.de/aay.html) - Series of documents: All About Your... (6510/1541/1581/etc) (by Ninja/The Dreams)


## Cross Platform Tools

These run on Linux, MacOS, and Windows.

* [VICE](https://vice-emu.sourceforge.io/) - The Versatile Commodore Emulator
* [IceBroLite](https://github.com/Sakrac/IceBroLite/releases) - c64 debugger (by Sakrac)
* [C64 65XE Debugger](https://sourceforge.net/projects/c64-debugger/) - c64, Atari XL/XE, NES debugger (by slajerek)
* [Codebase64 Cross Development Page](https://codebase64.org/doku.php?id=base:crossdev) - more links and info about other cross development tools


#### Windows-only Tools

* [DirMaster](https://style64.org/dirmaster) - "DirMaster is a Windows-based GUI application designed to help Commodore enthusiasts explore and manage their disk image collections." (by Style)



## Cross Development 6502 Assemblers

Most of these run on Linux, MacOS, and Windows.

* [KickAssembler](http://theweb.dk/KickAssembler) - KickAssembler, cross platform, runs everywhere, written in Java, and has the best functionality I've ever seen in an assembler (by Mads Nielsen)
* [ACME](https://sourceforge.net/projects/acme-crossass/) - a cross assembler that has been developed since 1998 (by Marco Baye)
* [ACME docs](https://sourceforge.net/p/acme-crossass/code-0/323/tree/trunk/docs/) - sourceforge makes it hard to find the actual files, so here's a direct link to the docs of the r323 trunk (olddevspeak for the latest code branch)
* [cc65](https://cc65.github.io/) - includes a macro assembler, c compiler, linker, and other tools
* [xa65](http://www.floodgap.com/retrotech/xa/) - runs on Unix platforms (including MacOS) (by André Fachat, currently maintained by Cameron Kaiser)


## Hardware

Some hardware useful for developing natively on your physical C=64.

* [SD2IEC](https://www.thefuturewas8bit.com/sd2iec-info) - SD card interface for your 64
* [EasyFlash 3](http://skoe.de/easyflash/ef3intro/) - Install several cartridge images onto one cartridge. A lot of games are being converted to EasyFlash cart images these days.


## Wikis & Infosites

* [6502.org](http://www.6502.org/) - a good resource for 6502 information
* [Codebase64](https://codebase64.org/doku.php?id=start) - democoding resources, a bit hit & miss
* [Replay Resources](https://rr.pokefinder.org/wiki/Main_Page) - Info about replay/freezer carts like Action Replay
* [c64 wiki](https://www.c64-wiki.com/wiki/Main_Page) - general c64 wiki with lots of good info and details on functions, BASIC commands, and more

## Native C=64 Tools

* [Super Snapshot](https://rr.pokefinder.org/wiki/Super_Snapshot) - A Canadian alternative to ActionReplay with, IMO a better machine language monitor and with very good documentation.
* [Turbo Assembler v5.2/FLT](http://www.fairlight.to/docs/text/xass33.html) - TurboAss v5.2 as modified by Bacchus of FairLight for non REU machines. You will need [Lynx](https://csdb.dk/release/?id=190028) to "dissolve" the file from a .lnx file to a .prg.
* [TurboMacroPro](http://turbo.style64.org/) - a more advanced evolution of Turbo Assembler series with macro support. Also has a larger memory footprint, which is why I still prefer tass v5.2 above.

## Web Tools

* [RetroPixels](https://www.micheldebree.nl/retropixels/) - Convert modern format images into C64 format (by Michel de Bree)
* [PETSCII Editor](https://petscii.krissz.hu/) - web based tool to design PETSCII screens, charsets, and sprites
* [Spritemate](https://www.spritemate.com/) - web based sprite editor



## Tutorials


* [A$$EMBLE IT!](http://tnd64.unikat.sk/assemble_it.html) - Assembly demo & game coding (by TND)
* [Programming the C64 VIC chip](https://www.youtube.com/watch?v=NDymM14uQWM) - FairLight TV episode #64 (by Bacchus/FLT)
* [An Introduction to Programming C-64 Demos](http://www.antimon.org/code/Linus/demo_prog.html) - An early democoding tutorial which is hard to follow if you don't already know the concepts behind the c64 and VIC chip (by Linus Åkerlund aka Puterman/FLT 2001)
* [VIC-II for Beginners](https://dustlayer.com/vic-ii/2013/4/22/when-visibility-matters) (by actraiser/dustlayer)



## Books

* [Bombjack's c64/128 archive](https://commodore.bombjack.org/commodore/index.htm) - A curated list of books, manuals, and magazines
* [Archive.org](https://archive.org) - All the books. Bombjack has them all easier to find and view, but if something is missing, or you're looking for a different version, chances are it'll be here.

### My Recommendations

* [C=64 Programmer's Reference Guide (recreation)](https://pickledlight.blogspot.com/p/commodore-64-guides.html) - This is a remade, edited version (by pickledlight). It's free to download, but he has laid it out in a way that is compatible with Lulu for you to print your own copy. He also has the C64 User Guide.
* [C=64 PRG Original](https://archive.org/details/c64-programmer-ref) - or you can get the original version from archive.org
* [Machine Language for the Commodore 64, by Jim Butterfield](https://archive.org/details/machinelanguagef0000butt)
* [Commodore 64 Whole Memory Guide, by Tim Arnot](https://commodore.bombjack.org/books/commodore/books/Commodore_64_Whole_Memory_Guide.zip) - direct download link - ZIP file


## Software Repositories

* [C64 Scene Database](https://csdb.dk/)
* [c64rulez](http://c64.rulez.org/) - The hosting center of C64 maniacs (Lion/Kempelen, Soci/Singular)
* [Extra Large c64 Archives](https://c64.rulez.org/xlfiles/)
* [Gangsta's Paradise](http://c64.rulez.org/pub/c64/) - Chromance FTP server
* [Pokefinder](http://ftp.pokefinder.org/) - FTP search tool

