--- status: DRAFT
--- author(s): from before, MES
--- notes: 

undocumented {(showTex, Thing)}

document { 
     Key => showTex,
     Headline => "convert to TeX and display on screen",
     Usage => "showTex x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Consequences => {
     	  {TT "showTex x", " converts ", TT "x", 
	  " to TeX format, and displays it on the screen."}
	  },
     "For example, the following line would display a matrix in another
     window.",
     PRE "showTex matrix{{1,2,3},{4,5,6}}",
     Caveat => UL {
	  "No attempt is made to wrap large matrices or equations.",
	  "The code for this function is Unix dependent at the moment, requiring that
	  the commands latex and xdvi are present."
	  },
     SeeAlso => {tex, texMath, mathML}
     }
