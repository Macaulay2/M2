document { (ass, Ideal),
     Headline => "find the associated primes of an ideal",
     Synopsis => {
	  "L = ass(I)",
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", 
	          TT "R", "."},
	  "L" => {"a list of prime ideals in ", TT "R", "."}
	  },
     PARA,
     "Computes the set of associated primes for the ideal ", TT "I", 
     ".  The resulting list
     is stashed in ", TT "I", " under the key ", TT "Assasinator", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
	  "I = intersect(ideal(a^2,b),ideal(a,b,c^5),ideal(b^4,c^4))",
	  "ass(I)"
	  },
     EXAMPLE {
	  "R = ZZ/7[x,y,z]/(x^2,x*y);",
	  "I=ideal(0_R);",
	  "ass I"
	  },
     PARA,
     "The associated primes are found using the Ext modules: The 
     associated primes of codimension ", TT "i", " of ", TT "I", " and ",
     TT "Ext^i(R^1/I,R)", " are identical, as shown in 
     Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992).",
     CAVEAT ("This function uses ", TT "decompose", 
	  ", which currently only works
          over finite ground fields, not the rationals or integers."),
     PARA,
     BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
     Last modified June 2000.",
     SEEALSO {(primaryDecomposition,Ideal), 
     	       radical, decompose, top, 
	       removeLowestDimension}
     }

document { ass => Strategy,
     "The strategy option value should be one of the following.",
     SHIELD MENU{
	  ("1", " -- The assasinator is found using Ext modules."),
	  ("2", " -- The assasinator is found using ", TO(top), " on a 
	       series of ideals.")
	  },
     PARA,
     "The default strategy is 2.",
     H3 "Strategy => 1",
     "The associated primes are found using the Ext modules: The 
     associated primes of codimension ", TT "i", " of ", TT "I", " and ",
     TT "Ext^i(R^1/I,R)", " are identical, as shown in 
     Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992).",
     H3 "Strategy => 2", 
     "The associated primes are found by iterating the two steps:  take the 
     minimal primes of", TO(top), " of the ideal, ", TT "I", ", and replace ",
     TT "I", " by ", TT "I:(top I)", "."
     }

document { ass => PrintLevel,
     "The PrintLevel should be one of the following.",
     SHIELD MENU {
	  ("0", " -- (default) Returns the list of associated primes."),
	  ("1", " -- Returns strategy number, associated primes as they are 
	       found, and full list at the end."),
	  ("2", " -- Returns the results of ", TT "PrintLevel=>1", " and signals 
	       presence in loop.  With ", TT "Strategy => 1", ", the codimension 
	       of the associated primes is the loop number.")
	       },
     EXAMPLE {
	  "R = ZZ/31991[x,y,z];",
	  "I = ideal(x*y*z, x*y^2 , x^3*y)",
	  "ass (I,PrintLevel=>2)"
	  }
     }     

document { (primaryComponent, Ideal, Ideal),
    Headline => "find a primary component corresponding to an associated prime",
    Synopsis => {
	 "Q = primaryComponent(I,P)",
   	 "I" => {"an ideal in a (quotient of a) polynomial ring ", 
	          TT "R", "."},
	 "P" => {"an associated prime of ", TT "I", "."},
	 "Q" => {"a ", TT "P", "-primary ideal of ", TT "I", "."}
	 },
    PARA,
    "Q is top(I + P^m), for sufficiently large m.  The criterion that Q
    is primary is given in 
    Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992).",
    "However, we use", TO (localize,Ideal,Ideal), ".",
    EXAMPLE {
	 },
    CAVEAT "",
    PARA,
    BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
    Last modified June 2000.",
    SEEALSO {(ass,Ideal),
	 (primaryDecomposition,Ideal), 
	 radical, decompose, top, 
	 removeLowestDimension}
    }

document { (primaryDecomposition, Ideal),
     Headline => "find a primary decomposition of an ideal",
     Synopsis => {
	  "L = primaryDecomposition I",
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", 
	          TT "R", "."},
	  "L" => {"a list of primary ideals whose 
	           intersection is ", TT "I", "."}
	  },
     PARA,
     "This routine returns an irredundant primary decomposition
     for the ideal ", TT "I", ".  The specific algorithm used varies
     depending on the characteristics of the ideal, and can also be specified
     using the optional argument ", TT "Strategy", ".",
     PARA,
     "Give examples here.",
     PARA,
     "Give references to algorithms used here.",
     CAVEAT "put possible problems here.  e.g. over QQ.",
     BOLD "Authors:", " W. Decker, G. Smith, M. Stillman, C. Yackel.", BR,
     BOLD "Maintainer:", " C. Yackel cyackel@math.indiana.edu.  ",
     "Last modified June 2000.",
     SEEALSO {(ass,Ideal), 
	  radical, decompose, top, 
	  removeLowestDimension}
     }

document { primaryDecomposition => Strategy,
     "The strategy option value should be one of the following.",
     SHIELD MENU {
          ("Monomial", " -- uses Alexander duality of a monomial ideal"),
	  ("Binomial", " -- finds a cellular resolution of a 
	                     binomial ideal"),
	  ("EHV", " -- uses the algorithm of Eisenbud-Huneke-Vasconcelos"),
	  ("SY", " -- uses the algorithm of Shimoyama-Yokoyama"),
	  ("Hybrid"," -- uses parts of the above two algorithms"),
	  ("GTZ", " -- uses the algorithm of Gianni-Trager-Zacharias.  
	           NOT IMPLEMENTED YET.")
          },
     PARA,
     "The default strategy depends on the ideal.  If the ideal is generated
     by monomials, then ", TT "Strategy => Monomial", " is implied.  
     In all other cases, the default is ", TT "Strategy => SY", ".",
     PARA,
     H3 "Strategy => Monomial",
     "Description, reference if possible, and then an example.  Also warn
     that ideal must be monomial.",
     H3 "Strategy => Binomial",
     "Description: get cellular resolution.  Give reference, example.",
     H3 "Strategy => EHV",
     "Description, example, reference",
     H3 "Strategy => SY", 
     "Description, example, reference",
     H3 "Strategy => Hybrid",
     "Description, example, reference", TO (localize,Ideal,Ideal)
     }

document { (localize,Ideal,Ideal),
     Headline => "localize an ideal at a prime ideal",
     Synopsis => {
	  "J = localize(I,P)",
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", 
	          TT "R", "."},
	  "P" => {"a prime ideal in the same ring."},
	  "J" => {"the extension contraction ideal I R_P intersect R."}
	  },
     PARA,
     "What else to say here?",
      EXAMPLE {
	  "R = ZZ/(101)[x,y];",
	  "I = ideal (x^2,x*y);",
	  "P1 = ideal (x);",
	  "localize(I,P1)",
	  "P2 = ideal (x,y);",
	  "localize(I,P2)",
	  },
     EXAMPLE {
	  "R = ZZ/31991[x,y,z];",
	  "I = ideal(x^2,x*z,y*z);",
	  "P1 = ideal(x,y);",
	  "localize(I,P1)",
	  "P2 = ideal(x,z);",
	  "localize(I,P2)",
	  },
     CAVEAT "The ideal P is not checked to be prime.",
     BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
     Last modified June 2000.",
     SEEALSO {(primaryDecomposition,Ideal), radical, decompose, top, 
	  removeLowestDimension}
     }

document { localize => Strategy,
	  "The strategy option value should be one of the following.",
	  SHIELD MENU{
	       ("1" , " -- Uses a separator to find the localization"),
	       ("2" , " -- Uses the algorithm of Eisenbud-Huneke-Vasconcelos")
	       },
	  PARA,
	  "The default strategy is 1.",
	  H3 "Strategy => 1",
	  "This strategy uses a separator polynomial - a polynomial in all of 
	  the associated primes of ", TT "I", " but ", TT "P", " and those 
	  contained in ", TT "P", ".  In this strategy, the assasinator of the 
	  ideal will be recalled, or recomputed, if unknown.  The method is 
	  described in  
	  Shimoyama-Yokoyama, J. Symbolic computation, 22(3) 247-277 (1996).",
	  H3 "Strategy => 2",
	  "This strategy does not require the calculation of the assasinator, 
	  but can require the computation of high powers of ideals. The 
	  method appears in 
	  Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992)."
          }

document { localize => PrintLevel,
	  "The PrintLevel option value should be one of the following.",
	  SHIELD MENU{
	       ("1" , " -- Informs the user of the current operation"),
	       ("2" , " -- Prints the current operation and its result")},
	  EXAMPLE{}
	  }

