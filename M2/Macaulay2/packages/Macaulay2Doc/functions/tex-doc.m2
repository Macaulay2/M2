--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented {
     (tex, COMMENT),
     (tex, IMG),
     (tex, TO2),
     (tex, TO),
     (tex, HEADER1),
     (tex, TOH),
     (tex, HEADER2),
     (tex, HEADER3),
     (tex, HEADER4),
     (tex, HEADER5),
     (tex, Thing),
     (tex, HEADER6),
     (tex, LITERAL),
     (tex, STRONG),
     (tex, Hypertext),
     (tex, String),
     (tex, ITALIC),
     (tex, TEX),
     (tex, BR),
     (tex, TT),
     (tex, HR),
     (tex, PARA),
     (tex, CODE),
     (tex, HREF),
     (tex, TABLE),
     (tex, PRE),
     (tex, Nothing),
     (tex, UL)
     }

document { 
     Key => tex,
     Headline => "convert to TeX format",
     Usage => "tex x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to TeX format"}
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
