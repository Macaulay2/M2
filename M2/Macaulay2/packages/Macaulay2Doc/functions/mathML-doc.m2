--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented methods mathML

document { 
     Key => mathML,
     Headline => "convert to MathML format",
     Usage => "mathML x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to ML format"}
	  },
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML (x+2*y-1)^2"
	  },
     EXAMPLE {
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SeeAlso => {tex, texMath}
     }

