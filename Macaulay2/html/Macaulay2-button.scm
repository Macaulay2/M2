(define (cadddr x) (car (cdddr x)))

(define (Macaulay2-button text filename)
  (let* ((padding 4) (image-width 110) (image-height 30) 
	 (image (car (gimp-image-new image-width image-height RGB)))
	 (drawable (car (gimp-layer-new image image-width image-height RGB "layer 1" 100 NORMAL)))
	 (border 0) (antialias TRUE) (size 20.0) (size-type POINTS) (foundry "*") (family "charter")
	 (weight "black") (slant "r") (set-width "*") (spacing "*") (number-of-colors 240) 
	 (extents (gimp-text-get-extents text size size-type foundry family weight slant set-width spacing))
	 (text-width (car extents)) (text-height (cadr extents))
	 (text-ascent (caddr extents)) (text-descent (cadddr extents))
	 (x-text (/ (- image-width text-width) 2)) (y-text (- image-height text-height))
	 )
    (gimp-image-add-layer image drawable 0)
    (gimp-palette-set-background  '(20 40 40))
    (gimp-palette-set-foreground '(128 255 255))
    (gimp-blend image drawable FG-BG-RGB NORMAL SHAPEBURST-SPHERICAL
		100.0			;opacity
		0			;offset
		REPEAT-NONE
		FALSE 0 0		;supersample
		0 0			;x1 y1
		0 0			;x2 y2
		)
    ; (gimp-edit-fill image drawable)
    (gimp-palette-set-foreground '(0 0 255))
    (gimp-floating-sel-anchor
     (car (gimp-text image drawable x-text y-text text border antialias size size-type
		     foundry family weight slant set-width spacing)))
    (gimp-display-new image)
    (gimp-convert-indexed image 0 number-of-colors)
    (file-gif-save 1 image drawable filename filename 0 0 0 0)
    (gimp-image-clean-all image)))

(script-fu-register
 "Macaulay2-button"
 "<Toolbox>/Xtns/Macaulay2/Button"
 "Button for Macaulay 2"
 "Dan Grayson <dan@math.uiuc.edu>" "public domain" "1997"
 "" 
 SF-VALUE "text" "\"Next\""
 SF-VALUE "filename" "\"up.gif\""
 )
