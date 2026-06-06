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
     SeeAlso => {exteriorPower, minors, permanents, pfaffians},
     Subnodes => { TO [determinant, Strategy], TO Bareiss, TO Cofactor, TO Dynamic },
     }
scan({det,minors,exteriorPower}, fn -> document { 
     Key => [fn, Strategy],
     Headline => "choose between Bareiss, Cofactor and Dynamic algorithms",
     Usage => toString fn | "(M, Strategy => s)",     
     Inputs => {
	  "M" => Matrix,
	  "s" => Symbol => {"either ", TT "Bareiss", ", ", TT "Cofactor", ", or ", TT "Dynamic"}	  
	  },
     Consequences => {
	  { "If ", TT "s", " is ", TO "Bareiss", ", then the Bareiss algorithm is used; if ",
	       TT "s", " is ", TO "Cofactor", ", then cofactor expansion is used; if ",
            TT "s", " is ", TO "Dynamic", ", then a dynamic programming algorithm is used."}},     
     "The ", TO2("Ring","ring"), " of ", TT "M", " determines the default strategy.  If the ring is a ", 
     TO2("PolynomialRing","polynomial ring"), " or a field (as identified by ", TO "isField",  
     ") then the ", TO "Bareiss", " algorithm is used.  If the ring is a ", 
     TO2("QuotientRing", "quotient ring"), " (which has not been declared a field by ", TO "toField", 
     "), then the ", TO "Cofactor", " algorithm is used.", "The ", TO "Dynamic", " algorithm implements a 
     variant of cofactor expansion that caches intermediate results. This strategy introduces some memory overhead,
     but can be faster than ", TO "Cofactor", ", especially with sparse matrices of low degree.",
     PARA{},     
     Caveat => {
	  {"The ", TO "Bareiss", " algorithm returns a ring element that may differ from the actual 
	       determinant by a zero divisor in the ring.  Thus, an ", EM "incorrect", 
	       " answer may be computed if the ring contains zero divisors."}
	  },
     SeeAlso => select({det, exteriorPower, minors}, g -> g =!= fn)
     })

document { Key => Bareiss,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }
document { Key => Cofactor,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }
document { Key => Dynamic,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }
