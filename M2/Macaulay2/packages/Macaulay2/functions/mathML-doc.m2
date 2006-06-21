--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented {
     (mathML, String),
     (mathML, MatrixExpression),
     (mathML, Boolean),
     (mathML, InfiniteNumber),
     (mathML, Divide),
     (mathML, Product),
     (mathML, ZZ),
     (mathML, QQ),
     (mathML, Holder),
     (mathML, RR),
     (mathML, Thing),
     (mathML, Power),
     (mathML, Type),
     (mathML, ZeroExpression),
     (mathML, OneExpression),
     (mathML, Minus),
     (mathML, Sum),
     (mathML, Symbol),
     (mathML, Hypertext),
     (mathML, Nothing)
     }

document { 
     Key => mathML,
     Headline => "convert to MathML format",
     Usage => "mathML x",
     Inputs => {
	  "x" => "any Macaulay 2 object"
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

