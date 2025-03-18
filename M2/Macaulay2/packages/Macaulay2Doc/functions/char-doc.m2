--- status: DRAFT
--- author(s): kummini
--- notes: 

undocumented {
	     (char,FractionField), 
	     (char,InexactField),
	     (char,QuotientRing),
	     (char,PolynomialRing)}

document { 
     Key => {char, (char,Ring)},
     Headline => "get the characteristic of the ring or field",
     Usage => "char F",
     Inputs => {"F" => Ring},
     Outputs => {{"the characteristic of the ring."}},
     EXAMPLE lines ///
		R = ZZ/10007[x,y];
		char R
		R = ZZ[x]/823671827384723894723894723892
     	        char R
	  ///,
     }
