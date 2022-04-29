# c64
my c=64 files and code

This repo contains code and files I'm working on for the Commodore 64.

# Not A Wizard
My Youtube channel (Not a Wizard) is at:
https://www.youtube.com/channel/UC8M0VmsO-i-8aDJNJH9NWXg

Videos will complement the files here.

# How to set up a modern computer to code for the c64

### 1. Download Visual Studio Code. 

It's totally free and is my favourite thing ever by Microsoft. Slightly ahead of the Microsoft Mouse.

https://code.visualstudio.com

It's available for Windows, Mac, and Linux. And there's even a [web version](https://vscode.dev/) if you don't want to download it or just want to have a play before you download.

### 2. Get the extension XC-BASIC by Viza.

https://marketplace.visualstudio.com/items?itemName=viza.xcbasiclanguagevscodeext

I used to use "Commodore 64 BASIC v2" by gverduci, but he made a bunch of changes that changed and broke the default behaviour. I think he fixed them based on my bug reports but while he was doing that I found XC-BASIC. Now XC is designed to work with the XC cross compiler. But you can ignore the cross compiler stuff and just use it for syntax highlighting.

Upside: XC BASIC works on .bas files, BASICv2 only works on .prg files, which doesn't make sense to me because those are the tokenised files you run in your emulator, not plaintext code files.

But if you're happy with that quirk, both are good.

### 3. Get KickAssembler for assembly coding

If you want to do assembly coding for your c64, then KickAssembler kicks arse. I don't endorse kicking asses because donkeys are very nice animals.

http://theweb.dk/KickAssembler

https://marketplace.visualstudio.com/items?itemName=CaptainJiNX.kickass-c64

Bacchus/FLT has a video about [setting up a c64 assembler environment](https://youtu.be/mjPucWkmqCg?t=2262). He uses Paul Hocker's VSCode extension, where I prefer CaptainJiNX's version, but they'll get you the same result.

### 4. Download VICE.

Guess I should include this. Might be important. 

https://vice-emu.sourceforge.io/

You could use any emulator, but VICE is recommended because it comes bundled with some very very useful command line tools.

They include ``petcat`` to convert plaintext code to tokenised code and vice versa, and ``c1541`` which lets you work on d64 files (disk images).

### 5. Convert your code back and forth (tokenised/non-tokenised).

Non-tokenised code is useful so you can edit it in VSCode.

Tokenised code is effectively a PRG file you can run in your emulator.

I have videos both conversions:

 - [How to convert c64 source code to plain text on a modern computer](https://www.youtube.com/watch?v=javD5O5hlEc)

 - [How to convert plaintext c64 BASIC source to a running program](https://www.youtube.com/watch?v=SBUEi_OYz84)

### 6. Convert and run your programs from right inside VSCode.

It's possible to set up the VSCode shortcut keys to do your compiling and running, but I'm command line guy.

I use Ctrl-` to bring up a terminal below the code window, and that's where I run ``petcat`` to do my conversion and run x64sc to run the PRG file immediately.

Works for me. Others will have tutes online on how to configure shortcut keys and how to fire up your PRG file in a debugger instead, but I haven't got time for that.

I need coffee.

Hope that's been helpful and I welcome you to the club of allegedly masochistic retro fantatics who play with 40 year old technology for fun! :D


