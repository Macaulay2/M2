--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => {asin,(asin,ZZ),(asin,RR),(asin, QQ)},
     Headline => "compute the arcsine",
     Usage => "asin x",
     Inputs => {
	  "x" => RR
	  },
     Outputs => {
	  RR => {"the arcsine (in radians) of ", TT "x"}
	  },
     EXAMPLE {
	  "asin 1"
	  }
     }

