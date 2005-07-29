document {
     Key => Ideal,
     Headline => "the class of all ideals",
     SeeAlso => "ideals",
     "For basic information about ideals in ", EM "Macaulay 2", ", see ",
     TO "ideals", ".",
     PARA,
     "Common ways to make an ideal:",
     UL {
	  TO "ideal",
	  TO "annihilator",
	  TO "content",
	  TO "fittingIdeal",
	  TO (kernel, RingMap),
	  TO minors,
	  TO pfaffians
  	  },
     "Common ways to get information about an ideal:",
     UL {
	  TO (generators, Ideal),
	  TO (isSubset, Ideal, Ideal)
	  },
     "Common operations on ideals:",
     UL {
	  TO (symbol +,Ideal,Ideal),
	  TO (symbol *,Ideal,Ideal),
	  TO (symbol ==,Ideal,Ideal),
	  TO (symbol ==,Ideal,ZZ),
	  TO (symbol ^,Ideal,ZZ),
	  TO (trim, Ideal)
	  },
     "Groebner bases, normal forms, free resolutions",
     UL {
	  TO gb,
	  TO leadTerm,
	  TO codim,
	  TO dim,
	  TO (symbol%,Ideal,Ideal),
	  TO resolution,
	  TO betti
	  },
     "Numeric information about homogeneous ideals",
     UL {
	  TO degree,
	  TO poincare,
	  TO hilbertFunction,
	  TO hilbertPolynomial,
	  TO hilbertSeries,
	  TO genera,
	  TO euler
	  },
     "Primary decomposition and components of an ideal",
     UL {
	  TO decompose,
	  TO radical,
	  TO ass,
	  TO primaryDecomposition,
	  TO topComponents,
	  TO saturate,
	  TO quotient,
	  TO (symbol :,Ideal,Ideal),
	  TO intersect
	  },
     "Ideals from geometry",
     UL {
	  TO Fano,
	  TO Grassmannian,
	  TO monomialCurveIdeal,
	  TO singularLocus
	  },
     "Common ways to use an ideal:",
     UL {
	  TO (symbol /, Ring, Ideal),
	  },
     PARA,
     "An ideal ", TT "I", " is an immutable object, so if you want to 
     cache information about it, put it in the hash table ", TT "I.cache", ".",
     PARA
     }
document {
     Key => {(symbol *,Ideal,Ideal),
	  (symbol *,Ideal,MonomialIdeal),
	  (symbol *,MonomialIdeal,Ideal)
	  },
     Headline => "product of ideals",
     Usage => "I * J",
     Inputs => {
	  "I" => null,
	  "J" => {"in the same ring as ", TT "I"},
	  },
     Outputs => {
	  {"the product of the two ideals ", TT "I", " and ", TT "J", ""}
	  },
     EXAMPLE {
	  "R = QQ[t][a..d];",
	  "I = ideal(a,(t+1)*c) * ideal(a^2,b^2)"
	  },
     "The generators produced are not generally minimal.  Use ",
     TO (trim,Ideal), " or ", TO (mingens,Ideal), " to find a smaller
     generating set.",
     SeeAlso => {"ideals"}
     }
document {
     Key => {(symbol +,Ideal,Ideal),
	  (symbol +,Ideal,MonomialIdeal),
	  (symbol +,MonomialIdeal,Ideal)
	  },
     Headline => "sum of ideals",
     Usage => "I + J",
     Inputs => {
	  "I" => null,
	  "J" => {"in the same ring as ", TT "I"},
	  },
     Outputs => {
	  {"the sum of the two ideals ", TT "I", " and ", TT "J", ""}
	  },
     EXAMPLE {
	  "R = QQ[t][a..d];",
	  "I = ideal(a,(t+1)*c) + ideal(a^2,b^2)"
	  },
     "The generators produced are not generally minimal.  Use ",
     TO (trim,Ideal), " or ", TO (mingens,Ideal), " to find a smaller
     generating set.",
     EXAMPLE {
	  "trim I"
	  },
     SeeAlso => {"ideals"}
     }
document {
     Key => {(symbol ^,Ideal,ZZ),
	  (symbol ^,MonomialIdeal,ZZ)
	  },
     Headline => "power of an ideal",
     Usage => "I^n",
     Inputs => {
	  "I" => null,
	  "n" => {"at least zero"},
	  },
     Outputs => {
	  {"the ideal ", TT "I^n"}
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^2, b^2-c*d);",
	  "I^3"
	  },
     "The generators produced are often not minimal.  Use ",
     TO (trim,Ideal), " or ", TO (mingens,Ideal), " to find a smaller
     generating set.",
     EXAMPLE {
	  "trim I^3"
	  },
     SeeAlso => {"ideals"}
     }
document {
     Key => {(symbol ==,Ideal,Ideal),
	  (symbol ==,Ideal,MonomialIdeal),
	  (symbol ==,MonomialIdeal,Ideal)
	  },
     Headline => "equality of ideals",
     Usage => "I == J",
     Inputs => {
	  "I" => null,
	  "J" => {"in the same ring as ", TT "I"},
	  },
     Outputs => {
	  Boolean => {"whether the two ideals are the same"}
	  },
     "If the ideals ", TT "I", " and ", TT "J", " are in a polynomial ring,
     (or a quotient of a polynomial ring), then equality is checked by
     comparing ", TO "Groebner bases", " of the two ideals.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^2,b);",
	  "J = ideal(a,c);",
	  "I + J == ideal(a,b,c)"
	  },
     "To test whether an ideal is the zero ideal or the entire ring, use ",
	  TO (symbol ==, Ideal, ZZ), ".",
     SeeAlso => {"ideals"}
     }
document {
     Key => (symbol ==, Ideal, ZZ),
     Usage => {"I == n"},
     Inputs => {"I" => null,"n" => "either 0, or 1."},
     Outputs => {
	  Boolean => {"If ", TT "n==1", ", then whether ", 
	       TT "I", " is the unit ideal.
     	       If ", TT "n==0", ", then whether ", TT "I", " is the zero ideal.  Any other value
     	       for ", TT "n", " is an error."}},
     EXAMPLE {
	  "R = QQ[x];",
      	  "ideal(x^2,x+1) == 1",
      	  "ideal(0_R) == 0"
	  },
     SeeAlso => {(symbol==, Ideal, Ideal)}
     }

document {
     Key => {(isSubset,Ideal,Ideal)
	  },
     Headline => "membership predicate",
     Usage => "isSubset(I,J)",
     Inputs => {
	  "I" => null,
	  "J" => {"in the same ring as ", TT "I"},
	  },
     Outputs => {
	  Boolean => {"true if ", TT "I", " is contained in ", TT "J"}
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^2-b*c-1,a*c-1,b^3-1);",
	  "isSubset(I^2,I)",
	  "isSubset(I,I^2)"
	  },
     "In polynomial rings, this is accomplished by computing
     a ", TO "Groebner basis", " of ", TT "J", " and testing 
     whether every element of ", TT "I", " reduces to 0 modulo ", TT "I", "."
     }
