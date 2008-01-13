--- status: DRAFT
--- author(s): kummini
--- notes: 

undocumented {
	     (char,FractionField), 
	     (char,QuotientRing),
	     (char,PolynomialRing)}

document { 
     Key => {char, (char,Ring), (char,AffineVariety), (char, InexactField), (char,ProjectiveVariety)},
     Headline => "computes the characteristic of the ring or field",
     Usage => "char F",
     Inputs => {
	   	"F" => Ring => {"or ", ofClass AffineVariety, " or ", ofClass ProjectiveVariety}
	  },
     Outputs => {
		{"the characteristic of the ring.  If ", TT "F", " is an affine or projective variety, then
		the characteristic of the corresponding ring is returned"}
	  },
     EXAMPLE lines ///
		R = ZZ/10007[x,y];
		char R
		R = ZZ[x]/823671827384723894723894723892
     	        char R
	  ///,
     }
