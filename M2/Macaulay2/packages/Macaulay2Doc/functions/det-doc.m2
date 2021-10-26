--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {det,(det,Matrix),(determinant,MutableMatrix)},
     Headline => "determinant of a matrix",     
     Usage => "det M",
     Inputs => {
	  "M" => {"a square ", TO2("Matrix","matrix")}
	  },
     Outputs => {
	  RingElement => {"which is the determinant of ", TT "M"} 
	  },
     EXAMPLE {
	  "R = QQ[vars(0..8)]",
	  "M = genericMatrix(R,2,2)",
	  "det M",
	  "N = genericMatrix(R,3,3)",
	  "det N"
	  },
     SeeAlso => {exteriorPower, minors, permanents, pfaffians}     
     }
scan({det,minors,exteriorPower}, fn -> document { 
     Key => [fn, Strategy],
     Headline => "choose between Bareiss and Cofactor algorithms",     
     Usage => toString fn | "(M, Strategy => s)",     
     Inputs => {
	  "M" => Matrix,
	  "s" => Symbol => {"either ", TT "Bareiss", " or ", TT "Cofactor"}	  
	  },
     Consequences => {
	  { "If ", TT "s", " is ", TO "Bareiss", ", then the Bareiss algorithm is used; if ",
	       TT "s", " is ", TO "Cofactor", ", then cofactor expansion is used."}},     
     "The ", TO2("Ring","ring"), " of ", TT "M", " determines the default strategy.  If the ring is a ", 
     TO2("PolynomialRing","polynomial ring"), " or a field (as identified by ", TO "isField",  
     ") then the ", TO "Bareiss", " algorithm is used.  If the ring is a ", 
     TO2("QuotientRing", "quotient ring"), " (which has not been declared a field by ", TO "toField", 
     "), then the ", TO "Cofactor", " algorithm is used.",
     PARA{},     
     Caveat => {
	  {"The ", TO "Bareiss", " algorithm returns a ring element that may differ from the actual 
	       determinant by a zero divisor in the ring.  Thus, an ", EM "incorrect", 
	       " answer may be computed if the ring contains zero divisors."}
	  },
     SeeAlso => select({det, exteriorPower, minors}, g -> g =!= fn)
     })

