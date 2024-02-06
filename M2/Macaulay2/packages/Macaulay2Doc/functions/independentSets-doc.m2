--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {independentSets,
	  (independentSets,Ideal),
	  (independentSets,MonomialIdeal),
	  [independentSets,Limit]},
     Headline => "some size-maximal independent subsets of variables modulo an ideal",
     Usage => "independentSets J",
     Inputs => {
	  "J" => Nothing => {ofClass Ideal, ", or ", ofClass MonomialIdeal},
	  Limit => ZZ => "the maximum number of independent sets to be found"
	  },
     Outputs => {
	  List => {" of products of variables.  The support of any one of these products
	    is a maximal independent subset of variables modulo ", TT "J"}
	  },
     "An independent set of variables of an ideal ", TT "J", " in a polynomial ring ", TT "R", " is a 
     set of variables that are algebraically independent modulo ", TT "J", " (i.e. there is no
	  polynomial in ", TT "J", " involving only these sets of variables.",
     PARA{},
     "If the Krull dimension of ", TT "R/J", " is ", TT "d", ", then a maximal independent set is an independent set
     having size ", TT "d", ".",
     PARA{},
     EXAMPLE lines ///
     	  R = QQ[a..h];
	  I = minors(2,genericMatrix(R,a,2,4))
	  inI = ideal leadTerm I
	  independentSets I
	  independentSets inI
	  ///,
     PARA{},
     "The independent sets returned correspond one for one with the minimal
     primes of smallest codimension of the ideal of lead terms of J.",
     EXAMPLE lines ///
          I = ideal"abc,bcd,cde,adf,cgh,b3f,a3g"
	  minimalPrimes I
	  independentSets I
	  ///,
     "The optional Limit argument is useful if you need only one, or several such independent sets.",
     EXAMPLE lines ///
	  L = independentSets(I, Limit=>1)
          ///,
     PARA{},
     "Often, you want the list of the variables in a maximal independent set, or the list of those not in the set.",
     EXAMPLE lines ///
          support L_0
	  rsort toList(set gens R - set support L_0)
          ///,
     PARA{},
     "This function is useful as a subroutine to primary decomposition algorithms.",
     SeeAlso => {"PrimaryDecomposition::PrimaryDecomposition", "MinimalPrimes::minimalPrimes", support, rsort}
     }
