document {
     Key => {(associatedPrimes,Ideal),(associatedPrimes,MonomialIdeal),(associatedPrimes,Module),(associatedPrimes,Ring)},
     Headline => "find associated primes",
     Usage => "associatedPrimes I\nass I",
     Inputs => {
	  "I" => Nothing => {"an ideal or module over a (quotient of a) polynomial ring ", TT "R"}
	  },
     Outputs => {
	  {"a list of the prime ideals in ", TT "R", " that are associated to ", TT "I"}
	  },
     TT "ass", " is an abbreviation for ", TT "associatedPrimes", ".",
     PARA{},
     "Computes the list of associated primes for a module ", TT "M", " using Ext modules: the 
     codimension ", TT "i", " associated primes of ", TT "M", " and ",
     TT "Ext^i(M,R)", " are identical, as shown in 
     Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235.",
     PARA{},
     TO primaryDecomposition, " also computes the associated primes.  After doing 
     a primaryDecomposition, calling ", TO associatedPrimes, " requires no new computation,
     and the list of associated primes is in the same order as 
     the list of primary components returned by ", TO primaryDecomposition, ". Conversely,
     calling ", TO associatedPrimes, " beforehand will speed up the process of ", 
     TO primaryDecomposition, ".",
     EXAMPLE {
	  "R = QQ[a..d]",
	  "M = coker(transpose matrix{{1_R,1,1,1}} | diagonalMatrix vars R)",
	  "associatedPrimes M"
	  },
     PARA{},
     "For an ideal ", TT "I", ", ", TT "associatedPrimes I", " is mathematically equivalent to ",
     TT "associatedPrimes comodule I", ".",
     EXAMPLE {
	  "I = intersect(ideal(a^2,b),ideal(a,b,c^5),ideal(b^4,c^4))",
	  "associatedPrimes I",
	  "associatedPrimes comodule I"
	  },
     PARA{},
     "For a ring ", TT "R", ", ", TT "associatedPrimes R", " is equivalent to ",
     TT "associatedPrimes ideal R", ", the associated primes of the defining ideal of ", TT "R", ".",
     EXAMPLE {
	  "R = QQ[x,y,z]/(x^2,x*y)",
	  "associatedPrimes R"
	  },
     PARA{},
     "If the ideal is ", ofClass MonomialIdeal, ", then a more efficient 
     method is used.  This monomial ideal
     code was written by Greg Smith and Serkan Hosten.  The above comments 
     about primary decomposition hold in this case too.",
     EXAMPLE lines ///
     	  R = QQ[a..f];
	  I = monomialIdeal ideal"abc,bcd,af3,a2cd,bd3d,adf,f5"
	  ass I
	  primaryDecomposition I
     ///,
     PARA{},
     "If the option ", TO CodimensionLimit, " is provided (with module input), then only 
     associated primes of codimension at most this value are found (unless all associated 
     primes have already been found). Calling ",
     TT "associatedPrimes M", " with a different value of ", TO CodimensionLimit, 
     " will remember the primes already found. The default value is -1, meaning all associated 
     primes are found.",
     PARA{},
     "The list of associated primes corresponds to the list of primary components of ", TT "I", ": the
     ", TT "i", "-th associated prime is the radical of the ", TT "i", "-th primary component.",
     PARA {
	 "Original author (for ideals): ", "C. Yackel, http://faculty.mercer.edu/yackel_ca/", 
	 ". Updated for modules by J. Chen.",
	 },
     SeeAlso => {(primaryDecomposition,Ideal),(primaryDecomposition, Module),
     	       "radical", "minimalPrimes", "topComponents", 
	       "removeLowestDimension"}
     }


document {
     Key => {(localize,Ideal,Ideal),localize},
     Headline => "localize an ideal at a prime ideal",
     Usage => "localize(I,P)",
     Inputs => {
	  "I" => {"an ideal in a (quotient of a) polynomial ring ", TT "R"},
	  "P" => {"a prime ideal in the same ring"}
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
     SeeAlso => {(primaryDecomposition,Ideal), radical, minimalPrimes, topComponents, 
	  removeLowestDimension}
     }

document {
     Key => [localize,Strategy],
	  "The strategy option value should be one of the following.",
	  UL{
	       LI ("0" , " -- Uses the algorithm of Eisenbud-Huneke-Vasconcelos"),
	       LI ("1" , " -- Uses a separator to find the localization")
	       },
	  "The default strategy is 1.",
	  HEADER3 "Strategy => 0",
	  "This strategy does not require the calculation of the assassinator, 
	  but can require the computation of high powers of ideals. The 
	  method appears in 
	  Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235.",
          HEADER3 "Strategy => 1",
	  "This strategy uses a separator polynomial - a polynomial in all of 
	  the associated primes of ", TT "I", " but ", TT "P", " and those 
	  contained in ", TT "P", ".  In this strategy, the assassinator of the 
	  ideal will be recalled, or recomputed using ", TO [associatedPrimes,Strategy] ,
	  " = 1, if unknown.  The separator 
	  polynomial method is described in  
	  Shimoyama-Yokoyama, J. Symbolic computation, 22(3) 247-277 (1996).",
	  HEADER3 "Strategy => 2",
	  "This is the same as ", TT "Strategy => 1", " except that, if 
	  unknown, the assassinator is computer using ", TO [associatedPrimes, Strategy],
	  " = 2."
	   }


document {
     Key => {(primaryComponent, Ideal, Ideal),primaryComponent},
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
     "Q is topComponents(I + P^m), for sufficiently large m.  The criterion that Q
     is primary is given in 
     Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235.  However, we use ", TO (localize,Ideal,Ideal), ".",
     PARA {
	 BOLD "Author and maintainer: ", "C. Yackel, cyackel@math.indiana.edu.  
	 Last modified June, 2000."},
     SeeAlso => {(associatedPrimes,Ideal), (primaryDecomposition,Ideal), radical, minimalPrimes, topComponents, removeLowestDimension}
     }

document {
     Key => {isPrimary, (isPrimary, Module, Module), (isPrimary, Ideal), (isPrimary, Ideal, Ideal)},
     Headline => "determine whether a submodule is primary",
     Usage => concatenate("isPrimary Q\n", "isPrimary(Q, P)\n", "isPrimary(M, Q)"),
     Inputs => {
	  "Q" => "a submodule or ideal to be checked for being primary",
	  "P" => Ideal => {"the ", TO "radical", " of ", TT "Q"},
	  "M" => "the ambient module"
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "Q", " is primary, ",
	       TO "false", " otherwise"}
	  },
     "Checks to see if a given submodule ", TT "Q", " of a module ", TT "M", " is primary, 
     i.e. whether or not ", TT "M/Q", " has exactly one associated prime (which is equivalent 
     for finitely generated modules over Noetherian rings). If the input is a single ideal, then 
     the ambient module is taken to be the ring (i.e. the free module of rank 1), and does 
     not need to be specified.",
     PARA{},
     EXAMPLE lines ///
     	  Q = ZZ/101[x,y,z]
	  isPrimary ideal(y^6)
	  isPrimary(ideal(y^6), ideal(y))
	  isPrimary ideal(x^4, y^7)
	  isPrimary ideal(x*y, y^2)
     ///,
     SeeAlso => {(primaryDecomposition, Ideal), (primaryDecomposition, Module), associatedPrimes}
     }

document {
     Key => [primaryComponent,Strategy],
     "The Strategy option value sets the localize strategy 
     option, and should be one of the following.",
     UL{
	  LI ("0", " -- Uses ", TT "localize", " Strategy 0"),
	  LI ("1", " -- Uses ", TT "localize", " Strategy 1"),
	  LI ("2", " -- Uses ", TT "localize", " Strategy 2")}
     }

document {
     Key => {[primaryComponent,Increment],Increment},
      "The Increment option value should be an integer.  As explained in ",
      TO (primaryComponent,Ideal,Ideal), " the algorithm, given in 
      Eisenbud-Huneke-Vasconcelos, Invent. Math. 110 (1992) 207-235,
      relies on ", TT  "topComponents(I + P^m)", " for ", TT "m", " sufficiently large.
      The algoritm begins with ", TT "m = 1", ", and increases m by the 
      value of the ", TT "Increment", "option until ", TT "m", " is 
      sufficiently large.  The default value is 1." 
     }

-- FIXME: This code causes an error message:
--  PrimaryDecomposition/doc.m2:210:1:(1):[14]: item to be documented comes from another package: Macaulay2Doc :: primaryDecomposition
--
-- document { 
--      Key => {primaryDecomposition,(primaryDecomposition,Ideal)},
--      Headline => "irredundant primary decomposition of an ideal",
--      Usage => "primaryDecomposition I",
--      Inputs => {
-- 	  "I" => { ofClass Ideal, " or ", ofClass MonomialIdeal, " in a (quotient of a) polynomial ring ", TT "R", "."}
-- 	  },
--      Outputs => {
-- 	  List => {"of ", TO2(Ideal,"ideals"), ", a minimal list of primary ideals whose intersection is ", TT "I"}
-- 	  },
--      "This routine returns an irredundant primary decomposition
--      for the ideal ", TT "I", ".  The specific algorithm used varies
--      depending on the characteristics of the ideal, and can also be specified
--      using the optional argument ", TT "Strategy", ". ",
--      "In all cases, the radical of each entry of the output is equal to the corresponding ",
--      "entry of the output of ", TO "associatedPrimes", ".",
--      PARA{},
--      "Primary decompositions algorithms are very sensitive to their input.  Some
--      algorithms work very well on certain classes of ideals, but poorly on other classes.
--      If this function seems to be taking too long, try another algorithm (using ",
--      TO [primaryDecomposition,Strategy], ").",
--      EXAMPLE {
-- 	  "R = QQ[a..i];",
-- 	  "I = permanents(2,genericMatrix(R,a,3,3))",
--           "C = primaryDecomposition I;",
-- 	  "I == intersect C",
-- 	  "#C",
-- 	  },
--      PARA{},
--      "Recall that ", TO (symbol/,List,Function), " applies a function to each element of a
--      list, returning the results as a list.  This is often useful with lists of ideals,
--      such as the list ", TT "C", " of primary components.",
--      EXAMPLE {
-- 	  "C/toString/print;",
-- 	  "C/codim",
-- 	  "C/degree"
-- 	  },
--      PARA{},
--      "The corresponding list of associated prime ideals is cached in ", TT ///I.cache#"AssociatedPrimes"///, ",
--      and can be obtained by using ", TO (associatedPrimes,Ideal), ".",
--      EXAMPLE {
-- 	  "associatedPrimes I / print;"
-- 	  },
--      Caveat => {"The ground ring must be a prime field."},
--      SeeAlso => {PrimaryDecomposition,(associatedPrimes,Ideal), radical, minimalPrimes, topComponents, removeLowestDimension}
--      }


document {
     Key => {[primaryDecomposition,Strategy],EisenbudHunekeVasconcelos,ShimoyamaYokoyama,Hybrid,GTZ},
     "The strategy option value should be one of the following.",
     UL {
          ("Monomial", " -- uses Alexander duality of a monomial ideal"),
	  ("Binomial", " -- finds a cellular resolution of a 
	                     binomial ideal.  NOT IMPLEMENTED YET."),
	  ("EisenbudHunekeVasconcelos", " -- uses the algorithm of Eisenbud-Huneke-Vasconcelos"),
	  ("ShimoyamaYokoyama", " -- uses the algorithm of Shimoyama-Yokoyama"),
	  ("Hybrid"," -- uses parts of the above two algorithms"),
	  ("GTZ", " -- uses the algorithm of Gianni-Trager-Zacharias.  
	           NOT IMPLEMENTED YET.")
          },
     "The default strategy depends on the ideal.  If the ideal is generated
     by monomials, then ", TT "Strategy => Monomial", " is implied.  
     In all other cases, the default is ", TT "Strategy => ShimoyamaYokoyama", ".",
     HEADER3 "Strategy => Monomial",
     "This strategy only works for monomial ideals, and is the default strategy for such ideals.  See the chapter
     \"Monomial Ideals\" in the Macaulay2 book.",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = ideal(x^2,x*y)
	  primaryDecomposition(I, Strategy => Monomial)
     ///,
     HEADER3 "Strategy => EisenbudHunekeVasconcelos",
     "See \"Direct methods for primary decomposition\" by Eisenbud, Huneke, and Vasconcelos, Invent. Math. 110, 207-235 (1992).",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = ideal(x^2,x*y)
	  primaryDecomposition(I, Strategy => EisenbudHunekeVasconcelos)
     ///,
     HEADER3 "Strategy => ShimoyamaYokoyama", 
     "This strategy is the default for non-monomial ideals.  See \"Localization and Primary Decomposition of Polynomial ideals\" by Shimoyama and Yokoyama, J. Symb. Comp. 22, 247-277 (1996).",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = ideal(x^2,x*y)
	  primaryDecomposition(I, Strategy => ShimoyamaYokoyama)
     ///,
     HEADER3 "Strategy => Hybrid",
     "Use a hybrid of the Eisenbud-Huneke-Vasconcelos and Shimoyama-Yokoyama strategies.  The field ",
     TT "Strategy", " is a list of two integers, indicating the strategy to use for finding associated primes and localizing, respectively. ",
     "WARNING: Setting the second paramter to 1 works only if the ideal is homogeneous and equidimensional.",
     EXAMPLE lines ///
     	  Q = QQ[x,y]
	  I = intersect(ideal(x^2), ideal(y^2))
	  primaryDecomposition(I, Strategy => new Hybrid from (1,1))
	  primaryDecomposition(I, Strategy => new Hybrid from (1,2))
	  primaryDecomposition(I, Strategy => new Hybrid from (2,1))
	  primaryDecomposition(I, Strategy => new Hybrid from (2,2))
     ///,
     }

TEST ///
-- monomial ideal:
R = QQ[a..g]
I = ideal(a*b*c^3, a^3*d*f^2, a*b*c*d*e, b*c^4*d^3, e*f^5)
C = primaryDecomposition I
A = associatedPrimes I
scan(#C, i -> radical(monomialIdeal C_i) == monomialIdeal A_i)
-- radical of a monomial ideal should immediately call the monomial ideal cdoe too
radical C_1
I = ideal(a^2,a*b,b^2)
C = primaryDecomposition I
irreducibleDecomposition monomialIdeal I

I = intersect(ideal(a^2,a*b,b^2), ideal(b,c,d^10))
C = primaryDecomposition I
associatedPrimes I
///

document { Key => {(irreducibleDecomposition,MonomialIdeal),irreducibleDecomposition},
     Headline => "express a monomial ideal as an intersection of irreducible monomial ideals",
     Usage => "irreducibleDecomposition I",
     Inputs => { "I" },
     EXAMPLE lines ///
        QQ[x..z];
        I = monomialIdeal (x*y^3, x*y^2*z)
	w = irreducibleDecomposition I
	assert( I == intersect w )
     ///,
     Outputs => {{ "a list of irreducible monomial ideals whose intersection is ", TT "I" }}}

document {
     Key => {primaryDecomposition,(primaryDecomposition, Ideal),(primaryDecomposition,MonomialIdeal)},
     Headline => "irredundant primary decomposition of an ideal",
     Usage => "primaryDecomposition I",
     Inputs => {
	  "I" => { ofClass Ideal, " or ", ofClass MonomialIdeal, " in a (quotient of a) polynomial ring ", TT "R", "."}
	  },
     Outputs => {
	  List => {"of ", TO2(Ideal,"ideals"), ", a minimal list of primary ideals whose intersection is ", TT "I"}
	  },
     "This routine returns an irredundant primary decomposition
     for the ideal ", TT "I", ".  The specific algorithm used varies
     depending on the characteristics of the ideal, and can also be specified
     using the optional argument ", TT "Strategy", ". ",
     "In all cases, the radical of each entry of the output is equal to the corresponding ",
     "entry of the output of ", TO "associatedPrimes", ".",
     PARA{},
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
     PARA{},
     "Recall that ", TO (symbol/,List,Function), " applies a function to each element of a
     list, returning the results as a list.  This is often useful with lists of ideals,
     such as the list ", TT "C", " of primary components.",
     EXAMPLE {
	  "C/toString/print;",
	  "C/codim",
	  "C/degree"
	  },
     PARA{},
     "The corresponding list of associated prime ideals is cached in ", TT ///I.cache#"AssociatedPrimes"///, ",
     and can be obtained by using ", TO (associatedPrimes,Ideal), ".",
     EXAMPLE {
	  "associatedPrimes I / print;"
	  },
     Caveat => {"The ground ring must be a prime field."},
     SeeAlso => {PrimaryDecomposition,(primaryDecomposition, Module),(associatedPrimes,Ideal), radical, minimalPrimes, topComponents, removeLowestDimension}
     }
     
document {
     Key => {(primaryDecomposition, Module),(primaryDecomposition, Ring)},
     Headline => "irredundant primary decomposition of a module",
     Usage => "primaryDecomposition M",
     Inputs => {
	  "M" => Module
	  },
     Outputs => {
	  List => {"of ", TO2(Module,"submodules"), ", a minimal list of primary submodules whose intersection is ", TT "0"}
	  },
     "This routine returns an irredundant primary decomposition for the zero submodule of ", 
     TT "M", ", i.e. a list of submodules ", TT "Q_i", " of ", TT "M", 
     " such that the intersection of all the ", TT "Q_i", " is ", TT "0", " and ", TT "Ass(M/Q_i) = {p_i}",
     " for some unique associated prime ", TT "p_i", " of ", TT "M", ", which is minimal (both
     inclusion-wise and cardinality-wise). The ",
     TT "i", "-th element of this output is primary to the ", TT "i", "-th element of the output of ",
     TT "associatedPrimes M", ". The algorithm used is inspired by the
     Eisenbud-Huneke-Vasconcelos algorithm, modified to work for modules.",
     PARA{},
     EXAMPLE {
	  "R = QQ[x_0..x_3]",
	  "(I1,I2,I3) = ({1,2,3},{1,3,4},{1,4,5})/monomialCurveIdeal_R",
          "M = comodule I1 ++ comodule I2 ++ comodule I3",
	  "associatedPrimes M",
	  "C = primaryDecomposition M;",
	  "netList C",
	  "intersect C == 0 and all(C, isPrimary_M)",
	  "C/degree"
	  },
     PARA{},
     "Recall that in Macaulay2, a module is commonly represented as a ",
     TO subquotient, ", which is an ordered pair consisting of (generators, relations) 
     represented as column matrices. As submodules of ", TT "M", ", each module in the 
     output list has the same relations as ", TT "M", ", and has generators which are ", 
     TT "R", "-linear combinations of generators of ", TT "M", ".",
     PARA{},
     "To obtain a primary decomposition of a submodule ", TT "N", ", run this function on
     the quotient ", TT "M/N", ". Note that in general ", TT "/", " does not check
     whether the second input is actually a submodule of the first, and a non-sensible 
     result may be returned if this is not the case.",
     -- EXAMPLE {
	  -- "N = coker map(M, R^1, transpose matrix{{1_R,1,1}}) -- coker of diagonal map",
	  -- "primaryDecomposition N",
	  -- "netList(oo/gens)"
	  -- },
     PARA{},
     "This method generalizes primary decomposition of ideals (more precisely, cyclic modules)
     as can be seen by calling ", TT "primaryDecomposition comodule I", " for an ideal ",
     TT "I", ". For convenience, one can also call ", TT "primaryDecomposition R", " for a
     ring ", TT "R", " (which is most useful when ", TT "R", " is a ", TO "QuotientRing",
     "). The corresponding list of associated prime ideals is cached in ", 
     TT ///M.cache#"AssociatedPrimes"///, ", and can be obtained with ", TT "associatedPrimes M", 
     ". When computing primary decompositions of ideals with this method, remember to add 
     back the original ideal, to obtain the corresponding primary ideals, as in the following example.",
     EXAMPLE {
	  "I = intersect((ideal(x_0..x_3))^5, (ideal(x_0..x_2))^4, (ideal(x_0..x_1))^3)",
	  "S = R/I",
	  "associatedPrimes S",
	  "comps = primaryDecomposition S",
	  "apply(comps, Q -> ideal mingens(I + ideal gens Q))",
	  "I == intersect oo"
	  },
     PARA{},
     "The results of the computation are stored in ", TT ///M.cache#"primaryComponents"///, 
     ", which is a ", TO "HashTable", " whose keys are associated primes and values are      
     the corresponding primary components. The computation may be interrupted at any point,
     and can be resumed later without recomputing already-known primary components. To
     display detailed information throughout the computation, set the global variable ",
     TO "debugLevel", " to a value greater than 0, e.g. ", TT "debugLevel = 1", ".",
     Caveat => {"Note that although isolated components (i.e. those corresponding to minimal
     primes) are uniquely determined by the module, embedded components are never unique,
     and thus specifying generators of an embedded component requires non-canonical choices.
     For speed purposes, this algorithm searches for embedded components obtained by adding 
     a bracket power of the embedded prime, with exponent determined by the degrees of
     generators of the embedded prime and ", TT "ann M", ". In particular, the generators 
     of an embedded component may not be of minimal possible degree."},
     SeeAlso => {(primaryDecomposition,Ideal),(associatedPrimes,Module),isPrimary,topComponents}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
