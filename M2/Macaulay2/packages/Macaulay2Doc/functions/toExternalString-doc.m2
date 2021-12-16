--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented methods toExternalString

document { 
     Key => toExternalString,
     Headline => "convert to a readable string",
     Usage => "toExternalString x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {"a string representation of ", TT "x", ", which can be used, in conjunction with ", TO "value", ", to read the object back into the program later"}
	  },
     "See also ", TO "toString", " which simply converts ", TT "x", " to a string that can be displayed meaningfully.",     
     EXAMPLE {
	  "toString {1,4,a,f,212312,2.123243242}"
	  },
     EXAMPLE {
	  "R = QQ[x_1..x_5];",
	  "toExternalString R",
	  },
     "Matrices and ring elements are linearized",
     EXAMPLE {
	  "x_1^3-3/4*x_5*x_3",
	  "toExternalString oo",
	  "value oo"
	  },
     Caveat => "Not everything can be converted to a string in such a way that it can be read back into the program later.",
     SeeAlso => {toString, value}
     }


