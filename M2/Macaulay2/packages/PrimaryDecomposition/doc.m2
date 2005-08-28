document { 
     Key => (primaryDecomposition,Ideal),
     Headline => "irredundant primary decomposition of an ideal",
     Usage => "primaryDecomposition I",
     Inputs => {
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", TT "R", "."}
	  },
     Outputs => {
	  List => {"of ", TO2(Ideal,"ideals"), ", a minimal list of primary ideals whose intersection is ", TT "I"}
	  },
     "This routine returns an irredundant primary decomposition
     for the ideal ", TT "I", ".  The specific algorithm used varies
     depending on the characteristics of the ideal, and can also be specified
     using the optional argument ", TT "Strategy", ".",
     PARA,
     "Primary decompositions algorithms are very sensitive to their input.  Some
     algorithms work very well on certain classes of ideals, but poorly on other classes.
     If this function seems to be taking too long, try another algorithm (using ",
     TO [primaryDecomposition,Strategy], ").",
     EXAMPLE {
	  "R = QQ[a..i];",
	  "I = permanents(2,genericMatrix(R,a,3,3))",
          "C = primaryDecomposition I;",
	  "I == intersect C",
	  "#C",
	  },
     PARA,
     "Recall that ", TO (symbol/,List,Function), " applies a function to each element of a
     list, returning the results as a list.  This is often useful with lists of ideals,
     such as the list ", TT "C", " of primary components.",
     EXAMPLE {
	  "C/toString/print;",
	  "C/codim",
	  "C/degree"
	  },
     PARA,
     "The corresponding list of associated prime ideals is cached at I.cache.ass,
     and can be obtained by using ", TO (ass,Ideal), ".",
     EXAMPLE {
	  "(ass I)/print"
	  },
     Caveat => {"The ground ring must be a prime field."},
     SeeAlso => {PrimaryDecomposition,(ass,Ideal), 
	  radical, decompose, topComponents, 
	  removeLowestDimension}
     }
document { 
     Key => [primaryDecomposition, PrintLevel],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document {
     Key => (ass, Ideal),
     Headline => "find the associated primes of an ideal",
     Usage => "ass(I)",
     Inputs => {
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", TT "R"}
	  },
     Outputs => {
	  {"a list of prime ideals in ", TT "R", " that are associated to ", TT "I"}
	  },
     "Computes the set of associated primes for the ideal ", TT "I", ".  The resulting list
     is stashed in ", TT "I", " under the key ", TT "Assassinator", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
	  "I = intersect(ideal(a^2,b),ideal(a,b,c^5),ideal(b^4,c^4))",
	  "ass I"
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
     Caveat => ("This function uses ", TT "decompose", 
	  ", which currently only works
          over finite ground fields, not the rationals or integers."),
     PARA,
     BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
     Last modified June 2000.",
     SeeAlso => {(primaryDecomposition,Ideal), 
     	       radical, decompose, topComponents, 
	       removeLowestDimension}
     }

document {
     Key => [ass,Strategy],
     "The strategy option value should be one of the following.",
     UL{
	  SEQ ("1", " -- The assasinator is found using Ext modules."),
	  SEQ ("2", " -- The assasinator is found using ", TO(topComponents), " on a 
	       series of ideals.")
	  },
     PARA,
     "The default strategy is 2.",
     HEADER3 "Strategy => 1",
     "The associated primes are found using the Ext modules: The 
     associated primes of codimension ", TT "i", " of ", TT "I", " and ",
     TT "Ext^i(R^1/I,R)", " are identical, as shown in 
     Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992).",
     HEADER3 "Strategy => 2", 
     "The associated primes are found by iterating the two steps:  take the 
     minimal primes of", TO(topComponents), " of the ideal, ", TT "I", ", and replace ",
     TT "I", " by ", TT "I:(topComponents I)", "."
     }

document {
     Key => [ass,PrintLevel],
     "The PrintLevel should be one of the following.",
     UL {
	  SEQ ("0", " -- (default) Returns the list of associated primes."),
	  SEQ ("1", " -- Returns strategy number, associated primes as they are 
	       found, and full list at the end."),
	  SEQ ("2", " -- Returns the results of ", TT "PrintLevel=>1", " and signals 
	       presence in loop.  With ", TT "Strategy => 1", ", the codimension 
	       of the associated primes is the loop number.")
	       },
     EXAMPLE {
	  "R = ZZ/31991[x,y,z];",
	  "I = ideal(x*y*z, x*y^2 , x^3*y)",
	  "ass (I,PrintLevel=>2)"
	  }
     }     

document {
     Key => (localize,Ideal,Ideal),
     Headline => "localize an ideal at a prime ideal",
     Usage => "localize(I,P)",
     Inputs => {
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", 
	          TT "R", "."},
	  "P" => {"a prime ideal in the same ring."}
	  },
     Outputs => { {"the extension contraction ideal I R_P intersect R."} },
     "The result is the ideal obtained by first extending to
     the localized ring and then contracting back to the original
     ring.",
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
     Caveat => "The ideal P is not checked to be prime.",
     BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
     Last modified June 2000.",
     SeeAlso => {(primaryDecomposition,Ideal), radical, decompose, topComponents, 
	  removeLowestDimension}
     }
///
document {
     Key => [localize,Strategy],
	  "The strategy option value should be one of the following.",
	  UL{
	       SEQ ("0" , " -- Uses the algorithm of Eisenbud-Huneke-Vasconcelos"),
	       SEQ ("1" , " -- Uses a separator to find the localization")
	       },
	  PARA,
	  "The default strategy is 1.",
	  HEADER3 "Strategy => 0",
	  "This strategy does not require the calculation of the assasinator, 
	  but can require the computation of high powers of ideals. The 
	  method appears in 
	  Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992).",
          HEADER3 "Strategy => 1",
	  "This strategy uses a separator polynomial - a polynomial in all of 
	  the associated primes of ", TT "I", " but ", TT "P", " and those 
	  contained in ", TT "P", ".  In this strategy, the assasinator of the 
	  ideal will be recalled, or recomputed using ", TO [ass,Strategy] ,
	  " = 1, if unknown.  The separator 
	  polynomial method is described in  
	  Shimoyama-Yokoyama, J. Symbolic computation, 22(3) 247-277 (1996).",
	  HEADER3 "Strategy => 2",
	  "This is the same as ", TT "Strategy => 1", " except that, if 
	  unknown, the assasinator is computer using ", TO ass => Strategy ,
	  " = 2."
	   }
///
document {
     Key => [localize,PrintLevel],
	  "The PrintLevel option value should be one of the following.",
	  UL{
	       SEQ ("0", " -- default"),
	       SEQ ("1" , " -- Informs the user of the current operation"),
	       SEQ ("2" , " -- Prints the current operation and its result")},
	  EXAMPLE{
	       "R = ZZ/(101)[x,y];",
	       "I = ideal (x^2,x*y);",
	       "P1 = ideal (x);",
	       "localize(I,P1,PrintLevel => 1)",
	       "P2 = ideal (x,y);",
	       "localize(I,P2,PrintLevel => 2)",
	       }
	  }


document {
     Key => (primaryComponent, Ideal, Ideal),
     Headline => "find a primary component corresponding to an associated prime",
     Usage => "Q = primaryComponent(I,P)",
     Inputs => {
   	  "I" => {"an ideal in a (quotient of a) polynomial ring ", 
	       TT "R"},
	  "P" => {"an associated prime of ", TT "I"}
	  },
     Outputs => {
	  "Q" => {"a ", TT "P", "-primary ideal of ", TT "I", "."}
	  },
     PARA,
     "Q is topComponents(I + P^m), for sufficiently large m.  The criterion that Q
     is primary is given in 
     Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992).",
     "However, we use", TO (localize,Ideal,Ideal), ".",
     PARA,
     BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
     Last modified June, 2000.",
     SeeAlso => {(ass,Ideal), (primaryDecomposition,Ideal), radical, decompose, topComponents, removeLowestDimension}
     }

document {
     Key => [primaryComponent,Strategy],
     "The Strategy option value sets the localize strategy 
     option, and should be one of the following.",
     UL{
	  SEQ ("0", " -- Uses ", TT "localize", " Strategy 0"),
	  SEQ ("1", " -- Uses ", TT "localize", " Strategy 1"),
	  SEQ ("2", " -- Uses ", TT "localize", " Strategy 2")}
     }

document {
     Key => [primaryComponent,Increment],
      "The Increment option value should be an integer.  As explained in ",
      TO (primaryComponent,Ideal,Ideal), " the algorithm, given in 
      Eisenbud-Huneke-Vasconcelos, Invent math, 110, 207-235 (1992),
      relies on ", TT  "topComponents(I + P^m)", " for ", TT "m", " sufficiently large.
      The algoritm begins with ", TT "m = 1", ", and increases m by the 
      value of the ", TT "Increment", "option until ", TT "m", " is 
      sufficiently large.  The default value is 1." 
     }

document {
     Key => [primaryComponent,PrintLevel],     
      "The Strategy option value should be one of the following.",
     UL{
	  SEQ ("0", " -- default"),
	  SEQ ("1", " -- informs user of the current power being checked."),
	  SEQ ("2", " -- gives output of time-consuming processes.")
     	  },
     PARA,
     "See ", TO (primaryComponent,Ideal,Ideal) , " for more information."
     }

document {
     Key => [primaryDecomposition,Strategy],
     "The strategy option value should be one of the following.",
     UL {
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
     HEADER3 "Strategy => Monomial",
     "Description, reference if possible, and then an example.  Also warn
     that ideal must be monomial.",
     HEADER3 "Strategy => Binomial",
     "Description: get cellular resolution.  Give reference, example.",
     HEADER3 "Strategy => EHV",
     "Description, example, reference",
     HEADER3 "Strategy => SY", 
     "Description, example, reference",
     HEADER3 "Strategy => Hybrid",
     "Description, example, reference", TO (localize,Ideal,Ideal)
     }

TEST ///
-- monomial ideal:
R = QQ[a..g]
I = ideal(a*b*c^3, a^3*d*f^2, a*b*c*d*e, b*c^4*d^3, e*f^5)
C = primaryDecomposition I
A = ass I
scan(#C, i -> radical(monomialIdeal C_i) == monomialIdeal A_i)
-- radical of a monomial ideal should immediately call the monomial ideal cdoe too
radical C_1
I = ideal(a^2,a*b,b^2)
C = primaryDecomposition I
irreducibleDecomposition monomialIdeal I

I = intersect(ideal(a^2,a*b,b^2), ideal(b,c,d^10))
C = primaryDecomposition I
ass I
///
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
