// Spiro's template file for Kickassembler format c=64 asm 
	* = $0801 "Basic Upstart"
BasicUpstart(init)
#import "inc/macro.inc"


// music is Badgers Badgers Everywhere, by Mitch:
// https://csdb.dk/sid/?id=18581
.var music = LoadSid("music/Badgers.sid")


// the const is a template which ignores the first 2 bytes in the 
// loaded binary file (which hold the start address of the file)
// charset file downloaded from: http://home-2002.code-cop.org/c64/font_12.html
.const CHAR_MAP = "C64FILE"
.var charmap = LoadBinary("charset/lady_tut.64c", CHAR_MAP)


// use these vars so I can shift the text around
.var screenbase = $0400
.var colbase = $d800
.var screen = $0400 + (40*23)
.var colmem = $d800 + (40*23)

.var delaylen = 1

.const	raster_music	= $00
.const	raster_bar_a	= $e0
.const	raster_scroll	= $ea
.const	raster_bar_b	= $fa



//	C O D E
//	####################################################################

	* = $080d "Code"
init:
	// black border + bg
	lda	#$00
	sta	$d020
	sta	$d021
	tax
	tay
	lda	#music.startSong-1
	jsr	music.init



	// clear screen
	lda	#$93
	jsr	$ffd2
	lda	#$0e
	jsr	$ffd2

	lda	#$8		// VIC control register
	sta	$d016		// no multicolour, no horiz. scroll, 40 column mode

	jsr	display_logo
	jsr	text_colours
	
	lda	$d018
	ora	#$0e 
	sta	$d018

	print_text(40*14+23, credits_code)
	screen_colours(40*14+23, GREEN, 5)
	print_text(40*16+15, credits_gfx)
	screen_colours(40*16+15, CYAN, 4)
	print_text(40*18+3, credits_music)
	screen_colours(40*18+3, YELLOW, 6)


	lda	#<scrolltext
	sta	scrolltext_base
	lda	#>scrolltext
	sta	scrolltext_base+1

	sei			// disable IRQs
	lda	#$7f		// turn off CIA IRQs
	sta	$dc0d		// CIA1
	sta	$dd0d		// CIA2
	and	$d011		// clear most significant bit of VIC's raster register
	sta	$d011
	lda	#$08
	sta	$d016

	set_irq(raster_music, playsid)


	lda	#$01		// enable raster interrupt signals
	sta	$d01a

	cli
	jmp *



//	I N T E R R U P T   V E C T O R S
//	####################################################################


playsid:
	jsr	music.play

	asl	$d019
	set_irq(raster_bar_a, bottom_rasterbar_a)
	pop_stack()
	rti


bottom_rasterbar_a:
	lda	$d012
	cmp	raster_bar_a+1
	bne	*-1

	lda	#WHITE
	sta	$d020
	sta	$d021

	ldx	#$0a
	dex
	bne	*-1

	lda	#00
	sta	$d020
	sta	$d021

	asl	$d019
	set_irq(raster_scroll, scroller)
	pop_stack()
	rti

scroller:
	// scroll one pixel
	lda	scroll_offset
	sta	$d016


	dec	delay
	bne	!++

	dec	scroll_offset
	// bpl	reset_delay
	bne	reset_delay

	lda	#7
	sta	scroll_offset

	// now shift all characters one char position back
	ldy	#$00			// y holds the screen location offset
!:	lda	screen+1,y		// shift chars on screen from $00..$27
	sta	screen,y
	iny
	cpy	#$27
	bne	!-
	// print a character
gettext:
	lda	scrolltext
	cmp	#$ff
	bne	printtext

	lda	#<scrolltext		// if we read a $ff byte (end of scrolltext)
	sta	gettext+1		// reset address back to start
	lda	#>scrolltext
	sta	gettext+2
	jmp	gettext

printtext:
	sta	screen+$27
	inc	gettext+1
	bne	reset_delay
	inc	gettext+2



reset_delay:
	lda	#delaylen
	sta	delay
!:	
	asl	$d019
	set_irq(raster_bar_b, bottom_rasterbar_b)
	pop_stack()
	rti


bottom_rasterbar_b:
	lda	$d012
	cmp	raster_bar_b+1
	bne	*-1


	// just re-enable 40 col mode, turn off everything else
	lda	#$08
	sta	$d016

	lda	#WHITE
	sta	$d020

	ldx	#$0a
	dex
	bne	*-1

	lda	#00
	sta	$d020

	asl	$d019
	set_irq(raster_music, playsid)
	pop_stack()
	rti




//	R O U T I N E S
//	####################################################################

text_colours:
	lda	#WHITE
	ldx	#$0
!:	sta	colmem,x
	inx
	cpx	#$27
	bne !-

	lda	#LIGHT_GREY
	sta	colmem+$02
	sta	colmem+$03
	sta	colmem+$22
	sta	colmem+$23


	lda	#LIGHT_BLUE
	sta	colmem+$01
	sta	colmem+$24

	lda	#BLUE
	sta	colmem
	sta	colmem+$25
	sta	colmem+$26
	sta	colmem+$27
	rts


display_logo:
	paint_logo($a0, logo_line1, screenbase, 2)
	paint_logo($a0, logo_line2, screenbase, 11)
	paint_logo($a0, logo_line3, screenbase, 10)
	paint_logo($a0, logo_line4, screenbase, 13)
	paint_logo($a0, logo_line5, screenbase, 17)
	paint_logo($a0, logo_line6, screenbase, 5)
	paint_logo($de, logo_line7, screenbase+200, 9)
	paint_logo($de, logo_line8, screenbase+200, 16)
	paint_logo($de, logo_line9, screenbase+200, 2)



	paint_logo_colour(WHITE, colbase)
	paint_logo_colour(YELLOW, colbase+80)
	paint_logo_colour(ORANGE, colbase+160)
	paint_logo_colour(BROWN, colbase+240)
	paint_logo_colour(DARK_GREY, colbase+320)

	rts



//	M A C R O S
//	####################################################################


.macro print_text(offset, textlabel) {
	push_stack()
	ldx	#0
!:	lda	textlabel,x
	cmp	#$ff
	beq	!+
	sta	screenbase+offset,x
	inx
	jmp	!-
!:	pop_stack()
}

.macro screen_colours(offset, colour, count) {
	push_stack()
	ldx	#0
	lda	#colour
!:	sta	colbase+offset,x
	inx
	cpx	#count
	bne	!-

	pop_stack()
}

.macro paint_logo(char, data, basemem, count) {
	lda	#char
	ldx	#0
!:	ldy	data,x
	sta	basemem,y
	inx
	cpx	#count
	bne	!-
}

.macro paint_logo_colour(colour, mem) {
	lda	#colour
	ldy	#0
!:	sta	mem,y
	iny
	cpy	#80
	bne	!-

}



//	D A T A 
//	####################################################################

	*=$1000 "Data"

scroll_offset:
	.byte 7
delay:
	.byte delaylen


credits_music:
	.text "music: badgers badgers                  "
	.text "       everywhere <mitch>"
	.byte $ff
credits_gfx:
	.text "gfx: spiro"
	.byte $ff
credits_code:
	.text "code: spiro"
	.byte $ff

scrolltext_base:
	.word 0
scrolltext:
	//.encoding "screencode_upper"
	.text "Gentles and ladymen ... I'm Spiro, and I'm Not a "
	.text "Wizard ... :)   "
	.text "Welcome to my first attempt at a scroller "
	.text "intro ... thanks to Bacchus/FLT for the motivation "
	.text "and for making some amazing videos on FairLight TV, "
	.text "especially those going over how the VIC works. It's "
	.text "been an interesting journey so far and it's only "
	.text "just getting started.   ...   "
	.text "                 ...........                    "
	.text "I hope it's at least an inspiration for someone out "
	.text "there, because I know while I have been struggling "
	.text "and constantly thinking 'maybe I'm too dumb for this' "
	.text "that taking things piece by piece and breaking them "
	.text "down, you eventually get it.    ...    At the moment "
	.text "I've managed to achieve a smooth scroller with music "
	.text "playing in the background, a PETSCII logo (I haven't "
	.text "figured out loading graphics successfully yet), and "
	.text "changing the character set (which is why the logo looks "
	.text "different to my original one)   ...  I'll sort that out "
	.text "too soon   ...  and stable rasterbars! "
	.text "                 ...........                    "
	.text "I really want to thank everyone watching and commenting "
	.text "on my youtube channel (@notawizard) and a special "
	.text "shoutout to Ian Witham for giving me the aha! moment "
	.text "for how to use $d016 properly. Yes it's been documented "
	.text "in a bunch of places, some of which I've even read, but "
	.text "it was that mental click in my head of 'after you've "
	.text "scrolled 8 pixels, shift the row over and scroll 8 "
	.text "more!'  :)  "
	.text "                 ...........                    "
	.text "The other tools that have been very useful, especially "
	.text "when learning and wondering why what I'm doing "
	.text "doesn't match what I'm reading and what others are "
	.text "saying was the c64 debugger and the monitor built "
	.text "into VICE. At one point I thought I had done every"
	.text "thing right, but the scrolltext and the logo would "
	.text "flash in multicolour mode.   ....   Pull out the "
	.text "ML monitor and look at the memory address where I was "
	.text "setting $d016 and ... I realised I was accidentally "
	.text "using immediate addressing (using the # in front of "
	.text "the label) when ORing into $d016 and I needed to use "
	.text "absolute addressing ... I took out the # and voila! "
	.text "the data was what I was expecting! haha   "
	.text "                 ...........                    "
	.text "After that, I had to figure out the exact right "
	.text "rasterline to trigger the scroll routine on because "
	.text "for some reason, the character shifter was glitching "
	.text "and messing up the first few characters on the line. "
	.text "The last thing was to turn off the scrolling in $d016 "
	.text "so that the logo at the top wasn't cycling back and "
	.text "forward (not in a good way.. It'd scroll left 8 pixels, "
	.text "then jump back 7 pixels, and scroll left, then jump "
	.text "back.. "
	.text "                 ...........                    "
	.text "The last things to figure out were the text wrapping "
	.text "at 256 bytes and the glitchy rasterbars. After playing "
	.text "around with different rasterlines to see if I was "
	.text "hitting badlines, I eventually settled on adding "
	.text "delays, plus checking for the following rasterline "
	.text "after the triggered one. ... in other words, trigger "
	.text "an interrupt at $e0, then the first thing the vector "
	.text "code does is check $d012 for $e1. Once it hits that, "
	.text "it does the rest of the routine. Then getting the right "
	.text "delay length in each one was just a matter of trial and "
	.text "error, and voila, nice stable rasterbars. ... "
	.text "The scrolltext was a fun one. I played with indirect "
	.text "addresses, but found LDA indirects only use one byte, "
	.text "so are probably looking for zero page addresses. I "
	.text "needed 2 bytes to store my pointer. ... in the end I "
	.text "found a great tutorial by TND, and advice echoed by youtH "
	.text "on mastodon/fediverse, I self modified the code. So no "
	.text "registers were needed for offsets either. Basically, "
	.text "the code does GETTEXT: LDA SCROLLTEXT (SCROLLTEXT is the label "
	.text "where my scrolltext is and GETTEXT is the label for that block "
	.text "of code), and then I increment the actual byte in the code "
	.text "with INC GETTEXT+1 and INC GETTEXT+2 ... then when I read a "
	.text "$ff from the scrolltext (it used to be a 0, but then I realised "
	.text "that wouldn't let me use the @ character, so I changed it to "
	.text "$ff), I reset the scrolltext pointer and the text "
	.text "loops.... just like this... "
	.text "         "
	.byte $ff

logo_line1:
	.byte	3,15
logo_line2:
	.byte	40+3,40+7,40+8,40+12,40+13,40+15,40+19,40+28,40+30,40+32,40+33
logo_line3:
	.byte	80+3,80+9,80+11,80+15,80+18,80+20,80+26,80+28,80+32,80+34
logo_line4:
	.byte	120+3,120+4,120+9,120+11,120+15,120+18,120+20,120+23,120+26,120+28,120+30,120+32,120+34
logo_line5:
	.byte	160+3,160+5,160+7,160+8,160+9,160+11,160+15,160+18,160+19,160+20,160+22,160+24,160+26,160+28,160+30,160+32,160+34
logo_line6:
	.byte	200+5,200+9,200+24,200+28,200+34


logo_line7:		// char $de ;  base + 200
	.byte	03,07,11,15,18,22,26,30,32	
logo_line8:
	.byte	40+03,40+05,40+07,40+08,40+09,40+11,40+16,40+19,40+22,40+23,40+24,40+27,40+28,40+30,40+32,40+34
logo_line9:
	.byte	80+24,120+24

//	C H A R S E T
//	####################################################################
	*=$3800 "Charset"
	.fill charmap.getSize(), charmap.get(i)

//	M U S I C
//	####################################################################

	*=music.location "Music"
	.fill	music.size, music.getData(i)


// Print the music info while assembling
.print ""
.print "SID Data"
.print "--------"
.print "location=$"+toHexString(music.location)
.print "init=$"+toHexString(music.init)
.print "play=$"+toHexString(music.play)
.print "songs="+music.songs
.print "startSong="+music.startSong
.print "size=$"+toHexString(music.size)
.print "name="+music.name
.print "author="+music.author
.print "copyright="+music.copyright
.print ""
.print "Additional tech data"
.print "--------------------"
.print "header="+music.header
.print "header version="+music.version
.print "flags="+toBinaryString(music.flags)
.print "speed="+toBinaryString(music.speed)
.print "startpage="+music.startpage
.print "pagelength="+music.pagelength


/*
  SID Data
  --------
  location=$2800
  init=$2800
  play=$2803
  songs=1
  startSong=1
  size=$af0
  name=Badgers Badgers Everywhere
  author=M. Nilsson-Vonderburgh (Mitch)
  copyright=1995 Crest
  
  Additional tech data
  --------------------
  header=PSID
  header version=2
  flags=10100
  speed=0
  startpage=0
  pagelength=0


Memory Map
----------
Default-segment:
  $0801-$080c Basic Upstart
  $080d-$0ad3 Code
  $1000-$2073 Data
  $2800-$32ef Music
  $3800-$3fff Charset

*/
