--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented methods toString

document { 
     Key => toString,
     Headline => "convert to a string",
     Usage => "toString x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {"a string representation of ", TT "x"}
	  },
     "Sometimes the string representation is just the name.
     See also ", TO "toExternalString", " which will try to convert ", TT "x", "
     to a string that can be read back into the program later.",
     EXAMPLE {
	  "toString {1,4,a,f,212312,2.123243242}"
	  },
     "If a ring has a global name, then toString returns this name.",
     EXAMPLE {
	  "R = QQ[x_1..x_5];",
	  "toString R",
	  "toExternalString R",
	  "toString(QQ[a])"
	  },
     "Matrices and ring elements are linearized",
     EXAMPLE {
	  "toString (x_1^3-3/4*x_5*x_3)",
	  "toString vars R",
	  "toExternalString vars R"
	  },
     SeeAlso => {toExternalString, value}
     }

