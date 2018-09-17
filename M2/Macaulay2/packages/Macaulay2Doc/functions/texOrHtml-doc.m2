--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented {
    (texOrHtml, Thing),
    (texOrHtml, Net),
    (texOrHtml, Hypertext),
    (texOrHtml, String),
    (texOrHtml, Descent),
    (texOrHtml, Holder),
    (texOrHtml, Describe),
    (texOrHtml, RowExpression)
	     }

document { 
     Key => texOrHtml,
     Headline => "convert to html format with some TeX math",
     Usage => "texOrHtml x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to html/TeX math format"}
	  },
     "Converts the input to HTML, with mathematical objects converted to
     TeX surrounded by \\( \\)",
     EXAMPLE {
	  "R = ZZ[x];",
      	  "texOrHtml (x-1)^6",
	  "texOrHtml \"Hello world\""
	  },
     SeeAlso => {texMath, html}
     }
