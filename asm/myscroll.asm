// Spiro's template file for Kickassembler format c=64 asm 
	* = $0801 "Basic Upstart"
BasicUpstart(init)

// use these vars so I can shift the text around
.var screenbase = $0400
.var colbase = $d800
.var screen = $0400 + (40*23)
.var colmem = $d800 + (40*23)

// music is Purple by Richard Bayliss/TND:
// https://csdb.dk/sid/?id=2438
.var music = LoadSid("music/Purple.sid")

	* = $1ce0 "Program Init"
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

	jsr	display_logo

	jsr	text_colours
	
	sei			// disable IRQs
	lda	#$7f		// turn off CIA IRQs
	sta	$dc0d		// CIA1
	sta	$dd0d		// CIA2



//	lda	$dc0d		// acknowledge pending interrupts
//	lda	$dd0d		// from CIA1 & 2

	//set_irq($20, top_rasterbar)
	set_irq($30, scrolltext)

	lda	#$01		// enable raster interrupt signals
	sta	$d01a

	cli
	jmp *



//	I N T E R R U P T
//	####################################################################

scrolltext:
	// do the work -------------------------
	inc	$d019		// interrupt status register
				// acknowledge raster interrupt by
				// writing a 1 to bit 0
	//inc	$d020
	jsr	music.play
	dec	delay
	bne	!+++

	lda	#5
	sta	delay
	
	lda	#$0		// reset column pointer
	sta	col_ptr

	ldx	text_ptr
	lda	text,x		// get next character
	bne	!+
	jsr	reset_text	
	jmp	!++
!:	sta	screen+$28	// print char
!:	ldy	col_ptr
	lda	screen+1,y
	sta	screen,y
	inc	col_ptr
	cpy	#$28
	bcc	!-
	inc	text_ptr


!:	//dec	$d020
	set_irq($e0, bottom_rasterbar_a)
	jmp	$ea81


top_rasterbar:
	inc	$d019
// !:	lda	$d012
// 	cmp	#$20
// 	bne	!-

	lda	#WHITE
	sta	$d020
!:	lda	$d012
	cmp	#$22
	bne	!-
	lda	#00
	sta	$d020


	set_irq($30, scrolltext)

	jmp	$ea81


bottom_rasterbar_a:
	inc	$d019
	lda	#WHITE
	sta	$d020
	sta	$d021
!:	lda	$d012
	cmp	#$e2
	bne	!-
	lda	#00
	sta	$d020
	sta	$d021

	set_irq($fa, bottom_rasterbar_b)
	jmp	$ea81

bottom_rasterbar_b:
	inc	$d019
	lda	#WHITE
	sta	$d020
!:	lda	$d012
	cmp	#$fc
	bne	!-
	lda	#00
	sta	$d020

	//set_irq($20, top_rasterbar)
	set_irq($30, scrolltext)
	jmp	$ea81



//	R O U T I N E S
//	####################################################################

reset_text:
	lda #$0
	sta text_ptr
	rts

ras_line:
	lda	#WHITE
	sta	$d020
	lda	$d012
	cmp	#$21

	lda	#00
	sta	$d020
	rts

text_colours:
	// set colours in colour mem 	
	lda	#WHITE
	ldx	#$0
!:	sta	colmem,x
	inx
	cpx	#$27
	bne !-

	lda	#LIGHT_GREY
	sta	colmem+$02
	sta	colmem+$03
	sta	colmem+$24
	sta	colmem+$25

	lda	#LIGHT_BLUE
	sta	colmem+$01
	sta	colmem+$26
	
	lda	#BLUE
	sta	colmem
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

.macro set_irq(rasterline, vector) {
	lda	#$7f		// clear high bit of rasterline
	and	$d011		// clear most significant bit of VIC's raster register
	sta	$d011

	lda	#rasterline	// set raster interrupt to trigger on line
	sta	$d012

	lda	#<vector	// set pointer for raster interrupt vector
	sta	$0314
	lda	#>vector
	sta	$0315
}


//	D A T A 
//	####################################################################


row:	.byte 0
col:	.byte 0
text_ptr:
	.byte 0
col_ptr:
	.byte 0

delay:
	.byte 5

text:
	.text "this is a scrolltext with some rubbish text...."
	.text "how much text can I add before I hit a 256 byte "
	.text "limit?  I could say some interesting stuff here "
	.text "but can't really think of anything at the moment."
	.text "                 ...........                    "
	.text "this is a hard scroller which is kinda rough and "
	.text "shit looking, but I haven't figured out $d016 yet. "
	.text "                 ...........                    "
	.text "I haven't paid attention to capital letters.. that "
	.text "might mess things up too... "
	.text "         "
	.byte 0

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
