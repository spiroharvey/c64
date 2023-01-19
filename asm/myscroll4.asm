/*
file:		myscroll4.asm
format: 	KickAssembler
yawot?:		Learning democoding skillz by making an intro
whodunnit?:	Spiro Harvey <spiro.harvey@gmail.com> aka iDLe
		https://youtube.com/@notawizard

This version:	Graphic logo,
		header text with colour washer,
		sanity retained (mostly)

Previous version:
		Displays a petscii logo,
		a scrolltext at the bottom, 2 rasterbars
		around the scrolltext, plays some music,
		and uses a different characterset
*/
	* = $0801 "Basic Upstart"
BasicUpstart(init)
#import "inc/macro.inc"

// music is Badgers Badgers Everywhere by Mitch:
// https://csdb.dk/sid/?id=18581
.var music = LoadSid("music/Badgers.sid")


// the const is a template which ignores the first 2 bytes in the 
// loaded binary file (which hold the start address of the file)
// charset file downloaded from: http://home-2002.code-cop.org/c64/font_12.html
.const BIN = "C64FILE"
.var charmap = LoadBinary("charset/lady_tut.64c", BIN)

// make your own logo with:
// c64 Characterset logo generator: 
// https://codepo8.github.io/logo-o-matic/#goto-madhires

// RetroPixels online (convert images to c64 Koala format): 
// https://www.micheldebree.nl/retropixels/

.var image = LoadBinary("gfx/idle_cosowi_yuv.kla", BIN)

// use these vars so I can shift the text around
.var screenbase = $0400
.var colbase = $d800
.var screen = $0400 + (40*23)
.var colmem = $d800 + (40*23)

// offsets for KOALA gfx data
.var bitmap_scrmem	= $1f40
.var bitmap_colmem	= $2328
.var bitmap_bg		= $2710


.var delaylen = 1

.const	raster_music	= $00
.const	raster_bitmapon = $5a
.const	raster_bitmapoff= $da
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



	// **********   LOAD BITMAP   ***************************************
	ldx	#$0
!:	// copy screen to screen ram
	lda	bitmap+bitmap_scrmem,x
	sta	$4400,x
	lda	bitmap+bitmap_scrmem+$100,x
	sta	$4500,x
	lda	bitmap+bitmap_scrmem+$200,x
	sta	$4600,x
	lda	bitmap+bitmap_scrmem+$300,x
	sta	$4700,x

	// copy colours to colour RAM
	lda	bitmap+bitmap_colmem,x  
	sta	$d800,x
	lda	bitmap+bitmap_colmem+$100,x
	sta	$d900,x
	lda	bitmap+bitmap_colmem+$200,x
	sta	$da00,x
	lda	bitmap+bitmap_colmem+$300,x
	sta	$db00,x
	dex
	bne !-

	// **********   DISPLAY HEADER   ************************************
	lda	#$3
	sta	$dd00
	lda	#$18
	sta	$d018
	ldx	#$00
!:	lda	header_text,x
	cmp	#$ff
	beq	!+
//	sta	screenbase,x
	sta	screenbase+20-((header_text_len-header_text)/2),x
	inx
	jmp	!-
!:
	ldx	#$28
	lda	#WHITE
!:	sta	colbase,x
	dex
	bne	!-
	

	// **********   SET THE TEXT COLOURS FOR THE SCROLLER   *************
	jsr	text_colours

	// **********   LOAD SCROLLTEXT INTO BASE MEMORY ADDRESS   **********
	lda	#<scrolltext
	sta	scrolltext_base
	lda	#>scrolltext
	sta	scrolltext_base+1

	// **********   INTERRUPT INITIALISATION   **************************
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
	// set back default bank for
	// upper screen redraw
	lda	#$3
	sta	$dd00
	lda	#$1e
	sta	$d018
	jsr	wash_header

	jsr	music.play



	// jsr	wash_logo

	asl	$d019
	set_irq(raster_bitmapon, bitmap_on)
	pop_stack()
	rti


bitmap_on:
	// inc $d020
	// set bit 5 = bitmap mode on
	lda	$d011
	ora	#32
	sta	$d011

	// set bit 4 = multicolour mode on
	lda	$d016
	ora	#16
	sta	$d016

	lda	#$2		// bank 2 ($4000)
	sta	$dd00
	lda	#$18 	    	// screen at $0400, bitmap at $2000
	sta	$d018
	// dec $d020

	asl	$d019
	set_irq(raster_bitmapoff, bitmap_off)
	pop_stack()
	rti


bitmap_off:
	// inc $d020
	// set bit 5 = bitmap mode off
	lda	$d011
	and	#255-32
	sta	$d011

	// set bit 4 = multicolour mode off
	lda	$d016
	and	#255-16
	sta	$d016

	lda	#$3		// bank 3 ($0000 - $3fff)
	sta	$dd00

	// change charset base addr to $3800
	lda	#$1e
	sta	$d018

	// dec $d020
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


wash_header:
	push_stack()
	lda	colourtable		// load first colour
	sta	colourtable+22		// store it at back of table
	ldx	#$00
!:	lda	colourtable+1,x
	sta	colourtable,x
	lda	colourtable,x
	sta	colbase+20-((header_text_len-header_text)/2),x
	inx
	cpx	#$28
	bne	!-
	pop_stack()
	rts


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


scrolltext_base:
	.word 0
scrolltext:
	//.encoding "screencode_upper"
	.text "Gentles and ladymen ... I'm Spiro, and I'm Not a "
	.text "Wizard ... :)   "
	.text "Well this is interesting progress. I have a graphic "
	.text "onscreen, a header with colour washer, and everything "
	.text "is basically the same. :)"
	.text "                 ...........                    "
	.text "It's fun learning how the C=64 works and how to "
	.text "all these cool things. Of course they're very "
	.text "simple compared to the demos we see made by veterans, "
	.text "but even they started somewhere.    ...    I hope I "
	.text "can inspire others to jump in on learning democoding. "
	.text "Heck, even if you don't use a C=64, but want to learn "
	.text "on an Amiga, an Atari, a Spectrum, a NES, or whatever, "
	.text "hopefully by seeing that I'm building up the skills "
	.text "as I go, that you can too. "
	.text "Or perhaps you want to make a game and want to use "
	.text "some of these techniques."
	.text "                 ...........                    "
	.text "Well, I did most of my rambling in the video, so if "
	.text "you haven't seen that, check out my channel at "
	.text "youtube.com/@notawizard  :)"
	.text "                 ...........                    "
	.text "Scrolltext loops..........                      "
	.byte $ff


header_text:
	.text	"* Not A Wizard *"
header_text_len:
	.byte	$ff

colourtable:
	.byte	$00,$00
	.byte	$01,$01,$07,$07,$08,$08,$09,$09,$0b
	.byte	$0b,$09,$09,$08,$08,$07,$07,$01,$01
	.byte	$00,$00

//	C H A R S E T
//	####################################################################
	*=$3800 "Charset"
	.fill charmap.getSize(), charmap.get(i)

//	M U S I C
//	####################################################################

	*=music.location "Music"
	.fill	music.size, music.getData(i)

//	B I T M A P
//	####################################################################

	*=$6000 "Logo Image"
bitmap:
	.fill	image.getSize(), image.get(i)


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
  $080d-$0a62 Code
  $1000-$1433 Data
  $2800-$32ef Music
  $3800-$3fff Charset
  $6000-$8710 Logo Image

*/
