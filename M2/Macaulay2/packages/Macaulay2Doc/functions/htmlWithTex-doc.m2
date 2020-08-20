--- status: DRAFT
--- author(s): P. Zinn-Justin
--- notes: 

undocumented {
    (htmlWithTex, Thing),
    (htmlWithTex, Net),
    (htmlWithTex, Hypertext),
    (htmlWithTex, String),
    (htmlWithTex, Descent),
--    (htmlWithTex, Holder),
--    (htmlWithTex, RowExpression)
	     }

document { 
     Key => htmlWithTex,
     Headline => "convert to HTML format with some TeX",
     Usage => "htmlWithTex x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to html/TeX format"}
	  },
     "Converts the input to HTML, with mathematical objects converted to
     TeX. Used by the ", TO "WebApp"," printing mode.",
     EXAMPLE {
	  "R = ZZ[x];",
      	  "htmlWithTex (x-1)^6",
	  "htmlWithTex \"Hello world\""
	  },
     SeeAlso => {texMath, html}
     }
