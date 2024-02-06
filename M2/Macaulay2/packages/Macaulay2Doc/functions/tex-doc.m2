--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented methods tex

document { 
     Key => tex,
     Headline => "convert to $\\TeX$ format",
     Usage => "tex x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to $\\TeX$ format"}
	  },
     EXAMPLE {
	  "R = ZZ[a..f];",
      	  "tex matrix {{a^2+2,b,c},{d,e,f^3-a}}",
	  },
     Caveat => {
	  "No attempt is made to wrap large matrices or equations."
	  },
     SeeAlso => {texMath, mathML, showTex}
     }
