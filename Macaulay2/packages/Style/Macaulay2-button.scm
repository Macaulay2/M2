(define (cadddr x) (car (cdddr x)))

(define (Macaulay2-button text filename)
  (let* ((padding 4)
	 (image-width 110)
	 (image-height 30)
	 (image (car (gimp-image-new image-width image-height RGB)))
	 (drawable (car (gimp-layer-new image image-width image-height RGB "layer 1" 100 NORMAL)))
	 (border 0)
	 (antialias TRUE)
	 (size 20.0)
	 (size-type POINTS)
	 (foundry "*")
	 ; (family "charter")
	 (family "*")
	 ; (weight "black")
	 (weight "*")
	 ; (slant "r")
	 (slant "*")
	 (set-width "*")
	 (spacing "*")
	 (number-of-colors 240) 
	 (registry "*")
	 (encoding "*")
	 (extents (gimp-text-get-extents text size size-type foundry family weight slant set-width spacing registry encoding ))
	 (text-width (car extents))
	 (text-height (cadr extents))
	 (text-ascent (caddr extents))
	 (text-descent (cadddr extents))
	 (x-text (/ (- image-width text-width) 2))
	 (y-text (- image-height text-height))
	 (dither-type 0)
	 (MAKE_PALETTE 0)
	 (palette-type MAKE_PALETTE)
	 (alpha-dither 0)
	 (remove-unused 0)
	 (palette "")
	 )
    (gimp-image-add-layer image drawable 0)
    (gimp-palette-set-background  '(20 40 40))
    (gimp-palette-set-foreground '(128 255 255))
    (gimp-blend drawable
		FG-BG-RGB		;type of blend
		NORMAL			;paint application mode
		SHAPEBURST-SPHERICAL	;gradient type
		100.0			;opacity
		0			;offset
		REPEAT-NONE		;repeat mode
		FALSE			;supersample
		0			;maximum recursion levels for supersampling
		0.0			;supersampling threshold
		0.0 0.0			;x1 y1
		0.0 0.0			;x2 y2
		)
    (gimp-palette-set-foreground '(0 0 255))
    (gimp-floating-sel-anchor
     (car (gimp-text image drawable x-text y-text text border antialias size size-type foundry family weight slant set-width spacing registry encoding)))
    (gimp-display-new image)
    (gimp-convert-indexed image dither-type palette-type number-of-colors alpha-dither remove-unused palette )
    (file-gif-save 1 image drawable filename filename 0 0 0 0)
    (gimp-image-clean-all image)))

(script-fu-register
 "Macaulay2-button"
 "<Toolbox>/Xtns/Macaulay2/Button"
 "Button for Macaulay 2"
 "Dan Grayson <dan@math.uiuc.edu>" "public domain" "1997"
 "" 
 SF-VALUE "text" "\"Next\""
 SF-VALUE "filename" "\"/tmp/next-button.gif\""
 )
