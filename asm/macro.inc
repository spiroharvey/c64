#importonce

.macro push_stack() {
	pha
	txa
	pha
	tya
	pha
}

.macro pop_stack() {
	pla
	tay
	pla
	tax
	pla
}

.macro cls() {
	push_stack()
	ldx	#$00

!:	lda	#$20		// wipe screen with spaces
	sta	$0400,x
	sta	$0500,x
	sta	$0600,x
	sta	$06e8,x

	lda	#$00		// set fg colour to black
	sta	$d800,x
	sta	$d900,x
	sta	$da00,x
	sta	$dae8,x
	dex
	bne	!-
	pop_stack()
}

.macro set_irq(rasterline, vector) {
	// lda	#$7f		// clear high bit of rasterline
	// and	$d011		// clear most significant bit of VIC's raster register
	// sta	$d011

	lda	#rasterline	// set raster interrupt to trigger on line
	sta	$d012

	lda	#<vector	// set pointer for raster interrupt vector
	sta	$0314
	lda	#>vector
	sta	$0315
}
