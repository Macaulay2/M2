(define (cadddr x) (car (cdddr x)))

(define (Macaulay2-empty-button filename)
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
	 (family "charter")
	 (weight "black")
	 (slant "r")
	 (set-width "*")
	 (spacing "*")
	 (number-of-colors 240))
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
    (gimp-palette-set-foreground '(0 0 255))
    (gimp-display-new image)
    (gimp-convert-indexed image 0 number-of-colors)
    (file-gif-save 1 image drawable filename filename 0 0 0 0)
    (gimp-image-clean-all image)))

(script-fu-register
 "Macaulay2-empty-button"
 "<Toolbox>/Xtns/Macaulay2/EmptyButton"
 "Empty Button for Macaulay 2"
 "Dan Grayson <dan@math.uiuc.edu>" "public domain" "1997"
 "" 
 SF-VALUE "filename" "\"/tmp/null-button.gif\""
 )
