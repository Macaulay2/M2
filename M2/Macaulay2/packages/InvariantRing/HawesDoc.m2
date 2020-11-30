-*
   Copyright 2014, Thomas Hawes.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

------------------------------------------------
-- hironakaDecomposition (renamed from invariantRing)
-- Optional arguments: DegreeVector, PrintDegreePolynomial
------------------------------------------------
 
document {
     Key =>{hironakaDecomposition,(hironakaDecomposition,FiniteGroupAction)},
     Headline => "calculates a Hironaka decomposition for the invariant ring of
     a finite group",
     Usage => "hironakaDecomposition G",
     Inputs => {"G"=> FiniteGroupAction},
     Outputs => {
          Sequence =>{
	       TT "(P,S)", ", where ", TT "P", " is a ", TO List, " of primary 
	       invariants (i.e. a homogeneous system of parameters for the 
	       invariant ring) and ", TT "S", " is a ", TO List, " of
               corresponding secondary invariants"
	       }
          },
     PARA{
	  "This function is provided by the package ", TO InvariantRing, "."
	  },
     PARA{
	  TO hironakaDecomposition, "  makes use of the functions ",
     	  TO primaryInvariants, " and ", TO secondaryInvariants,
     	  " in order to compute a Hironaka decomposition of
	  the invariant ring of a finite group ", TT "G", " acting
	  on a polynomial ring ", TT "R", ". It outputs a sequence ",
	  TT "({f", SUB TT "1", TT ",...," , TT "f", SUB TT "n",
	  TT "}, {g", SUB TT "1", TT ",...," , TT "g", SUB TT "r", TT"})",
	  " of primary and secondary invariants such that ",
	  TT "R", SUP TT "G", TT "=A", TT "g", SUB TT "1", TEX "\\oplus", 
	  TT "...", TEX "\\oplus", TT "A", TT "g", SUB TT "r", ", where ", TT "A=K[", 
	  TT "f", SUB TT "1", TT ",...," , TT "f", SUB TT "n", TT "]",
	  " and ", TT "K", " is the field of coefficients of ",
	  TT "R", "."
	  },
     PARA{
	  "All of the optional arguments of ", 
	  TO hironakaDecomposition, " play the same role as for the functions ",
	  TO primaryInvariants, " and ", TO secondaryInvariants,
	  ". By default, the function ",
          TO hironakaDecomposition, " calls upon ", TO primaryInvariants, ", with the
     	  optional argument ", TO Dade, " set to ", TO false, ", to compute a set
    	  of primary invariants, resulting in Kemper's 'optimal' algorithm being
     	  used (see ", TO "hsop algorithms", " for more information)."
          }, 
     PARA{
	  "The example below computes a set of primary and secondary invariants 
	  for an action of the cyclic group of order 4 on ", TT "QQ[x,y]", "."
	  },
     EXAMPLE {
          "C4=finiteAction({matrix{{0,-1},{1,0}}},QQ[x,y])",
	  "hironakaDecomposition C4"
          },  
     PARA{
	  "From the output one sees that ", TT "QQ[x,y]", SUP(TT "C4"), 
	  TT "=QQ[f", SUB(TT "1"), TT "f", SUB(TT "2"), TT "]", TEX "\\oplus", TT "QQ[f", 
	  SUB(TT "1"), TT "f", SUB(TT "2"), TT "](x", SUP(TT "4"), TT "+y", 
	  SUP(TT "4"), TT ")", ", where ", TT "f", SUB(TT "1"), TT "=x", SUP(TT "2"), 
	  TT "+y", SUP(TT "2"), " and ",TT "f", SUB(TT "2"), TT "=xy", 
	  SUP(TT "3"), TT "-x", SUP(TT "3"), TT "y", "."
	  }, 
     Caveat=>{
	  "Currently hironakaDecomposition works only with polynomial rings and 
	  matrices over fields of characteristic 0."
	  },
     SeeAlso=>{"hsop algorithms"}
     }

------------------------------------------------
-- molienSeries
------------------------------------------------

document {
    Key => {molienSeries,(molienSeries,FiniteGroupAction)},
    Headline => "computes the Molien (Hilbert) series of the invariant ring 
    of a finite group",
    Usage => "molienSeries G",
    Inputs =>{"G"=> FiniteGroupAction},
    Outputs =>{
    	Divide=>{
	    "the Molien series of the invariant ring of G as a rational 
	    function"
	    }
	},  
    PARA{
	"This function is provided by the package ", TO InvariantRing, "."
	},
    PARA{
	"The example below computes the Molien series for the dihedral 
	group with 6 elements. ", TT "K", " is the field obtained by 
	adjoining a primitive third root of unity to ", TO QQ, "."
	}, 
    EXAMPLE {
	"K=toField(QQ[a]/(a^2+a+1));",
	"A=matrix{{a,0},{0,a^2}};",
	"B=sub(matrix{{0,1},{1,0}},K);",
	"D6=finiteAction({A,B},K[x,y])",
	"molienSeries D6"
	}
    }

------------------------------------------------
-- primaryInvariants
-- Optional arguments: Dade, DegreeVector
------------------------------------------------

document {
     Key=> {primaryInvariants,(primaryInvariants,FiniteGroupAction)},
     Headline=> "computes a list of primary invariants for the 
     invariant ring of a finite group",
     Usage=> "primaryInvariants G",
     Inputs=>{
	  "G" => FiniteGroupAction
	  },
     Outputs=>{
	  List => {
	       " consisting of a homogeneous system of parameters (hsop) for the 
	       invariant ring of the group action"
	       }
	  },
     PARA{
	  "There are two algorithms implemented in ", TO primaryInvariants, 
	  ". The default algorithm (corresponding to the optional argument ", 
	  TO Dade, " taking the value ", TO false, ") currently only works with 
	  polynomial rings over fields of characteristic zero. The second is the 
	  Dade algorithm, corresponding to the optional argument ", TO Dade, 
	  " taking the value ", TO true, ". This algorithm can calculate an 
	  hsop over finite fields, so long as the field is sufficiently large. 
	  See ", TO "hsop algorithms", " for a discussion comparing the two 
	  algorithms."
	  }, 
     EXAMPLE {
	  "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=finiteAction({A,B},QQ[x,y,z])",
	  "primaryInvariants S3"
	  },
     PARA{
	  "Below, the invariant ring ", TT "QQ[x,y,z]", SUP TT "S3", " is 
	  calculated with ", TT "K", " being the field with 101 elements."
	  },
     EXAMPLE {
	  "K=GF(101)",
	  "S3=finiteAction({A,B},K[x,y,z])",
	  "primaryInvariants(S3,Dade=>true)"
	  },
     Caveat=> {
	  "Currently users can only use ", TO primaryInvariants, " to calculate 
	  a hsop for the invariant ring over a finite field by using the Dade 
	  algorithm. Users should enter the finite field as a ", TO GaloisField,
     	  " or a quotient field of the form ", TO ZZ, "/p and are advised to
      	  ensure that the ground field has 
	  cardinality greater than ", TT "|G|", SUP TT "n-1", ", where ", 
	  TT "n", " is the number of variables in the polynomial ring ",
	  ". Using a ground field smaller than this runs the risk of the
     	  algorithm getting stuck in an infinite loop; ", TO primaryInvariants,
     	  " displays a warning message asking the user whether they wish to
     	  continue with the computation in this case. See ", 
	  TO "hsop algorithms", " for a discussion on the Dade algorithm."
	  },
     PARA{
	  "This function is provided by the package ", TO InvariantRing, "."
	  }
     }

document {
     Key => {[primaryInvariants, Dade],Dade},
     Headline=> "an optional argument for primaryInvariants determining whether 
     to use the Dade algorithm",
     Usage=> "primaryInvariants G",
     Inputs=>{
	  "G" => FiniteGroupAction
	  },
     Outputs=>{
	  List => {
	       " consisting of a homogeneous system of parameters (hsop) for the 
	       invariant ring of the group action"
	       }
	  },
     PARA{
	  TO Dade, " takes ", TO Boolean, " values and is set to ", TO false, 
	  " by default. If ", TO Dade, " is set to ", TO true, ", then ", 
	  TO primaryInvariants, " will use the Dade algorithm to calculate a 
	  homogeneous system of parameters (hsop) for the invariant ring of a
     	  finite group."
     	  },
     PARA{
	  "The example below computes the invariant ring of ", TT "S3", " acting 
	  on ", TT "QQ[x,y,z]", " by permutations on the variables. ", TO Dade, 
	  " is set to ", TO true, "."
	  },   
     EXAMPLE {
          "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=finiteAction({A,B},QQ[x,y,z])",
          "primaryInvariants(S3,Dade=>true)"
          },
     PARA{
	  "Compare this result to the hsop output when Dade is left to its 
	  default value ", TO false, "."
	  }, 
     EXAMPLE {
          "primaryInvariants(S3)"
          },
     PARA{
	  "Below, the invariant ring ", TT "QQ[x,y,z]", SUP TT "S3", " is 
	  calculated with ", TT "K", " being the field with 101 elements."
	  },
     EXAMPLE {
	  "K=GF(101)",
	  "S3=finiteAction({A,B},K[x,y,z])",
	  "primaryInvariants(S3,Dade=>true)"
	  },
     PARA{
	  "For more information about the algorithms used to calculate a hsop 
	  in primaryInvariants, see ", TO "hsop algorithms", "."
	  },
     Caveat=> {
	  "Currently users can only use ", TO primaryInvariants, " to calculate 
	  a hsop for the invariant ring over a finite field by using the Dade 
	  algorithm. Users should enter the finite field as a ", TO GaloisField,
     	  " or a quotient field of the form ", TO ZZ, "/p and are advised to
      	  ensure that the ground field has 
	  cardinality greater than ", TT "|G|", SUP TT "n-1", ", where ", 
	  TT "n", " is the number of variables in the polynomial ring ", TT "R", 
	  ". Using a ground field smaller than this runs the risk of the
     	  algorithm getting stuck in an infinite loop; ", TO primaryInvariants,
     	  " displays a warning message asking the user whether they wish to
     	  continue with the computation in this case. See ", 
	  TO "hsop algorithms", " for a discussion on the Dade algorithm."
	  },
     SeeAlso=>{"hsop algorithms","primaryInvariants"}
     }

document {
     Key => {[primaryInvariants, DegreeVector],[hironakaDecomposition,DegreeVector],DegreeVector},
     Headline=> "an optional argument for primaryInvariants that finds invariants 
     of certain degrees",
     Usage=> "primaryInvariants G",
     Inputs=>{
	  "G" => FiniteGroupAction
	  },
     Outputs=>{
	  List => {
	       " consisting of a homogeneous system of parameters (hsop) for
	       the invariant ring of the group action"
	       }
	  },
     PARA{
	  "By default, ", TO primaryInvariants," uses an optimising algorithm 
	  which tests for the existence of a homogeneous system of parameters 
	  (hsop) ", TT "(f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", TT ")", 
	  " with positive degrees corresponding to ", TT "(d", SUB TT "1", 
	  TT ",...,", TT "d", SUB TT "n", TT ")", " in ", TO ZZ, SUP TT "n", ". 
	  If it is known that a hsop exists for a certain collection of 
	  degrees, this can be assigned, as a ", TO List, ", to the optional 
	  argument ", TO DegreeVector, ". ", TO primaryInvariants, " will then 
	  output a hsop corresponding to this list of degrees. If however no 
	  such hsop exists, ", TO primaryInvariants, " outputs an error 
	  message."
	  },
     PARA{
	  "Note that the ", TO List, " assigned to ", TO DegreeVector, " is
          ignored if ", TO Dade, " is set to ", TO true, "."
	  },
     EXAMPLE {
          "A=matrix{{0,1,0},{0,0,1},{1,0,0}};", 
	  "B=matrix{{0,1,0},{1,0,0},{0,0,1}};", 
	  "S3=finiteAction({A,B},QQ[x,y,z])",
          "primaryInvariants(S3,DegreeVector=>{3,3,4})"
          },
     Caveat=> {
	  "Currently users can only use ", TO primaryInvariants, " to calculate 
	  a hsop for the invariant ring over a finite field by using the Dade 
	  algorithm. Users should enter the finite field as a ", TO GaloisField,
     	  " or a quotient field of the form ", TO ZZ, "/p and are advised to
      	  ensure that the ground field has 
	  cardinality greater than ", TT "|G|", SUP TT "n-1", ", where ", 
	  TT "n", " is the number of variables in the polynomial ring ", TT "R", 
	  ". Using a ground field smaller than this runs the risk of the
     	  algorithm getting stuck in an infinite loop; ", TO primaryInvariants,
     	  " displays a warning message asking the user whether they wish to
     	  continue with the computation in this case. See ", 
	  TO "hsop algorithms", " for a discussion on the Dade algorithm."
	  },
     SeeAlso=>{primaryInvariants}
     }

------------------------------------------------
-- hsop algorithms
-- Extra documentation that compares the default and Dade algorithms used in 
-- primaryInvariants
------------------------------------------------

document {
     Key => "hsop algorithms",
     Headline => "an overview of the algorithms used in primaryInvariants",
     PARA{
	  "This page contains a discussion on the two algorithms that are used 
	  in the function ", TO primaryInvariants, ", which computes a 
	  homogenous system of parameters (hsop) for the invariant ring ", 
	  TT "R:=K[x", SUB TT "1", TT ",...,x", SUB TT "n", TT "]", SUP TT "G", 
	  " of a finite group ", TT "G", ". Which algorithm is used depends on 
	  the ", TO Boolean, " value the optional argument ", TO Dade, " takes. 
	  In the case where it is set to ", TO false, " it uses what shall be 
	  referred to as the 'default' algorithm. If it is set to ", TO true, 
	  " then it uses what shall be called the 'Dade' algorithm."
	  },
     PARA{
	  "The default algorithm is an implementation of the 'optimal' algorithm 
	  given in [K]. It is optimal in the sense that it finds a hsop ", 
	  TT "f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", " such that the 
	  number of secondary invariants required to make ", TT "R", " into a 
	  free ", TT "K[f", SUB TT "1", TT ",...,f", SUB TT "n", TT "]", 
	  "-module is minimal. The first step in the default algorithm is to 
	  cycle through the ", TO List, " s", TT "={d", SUB TT "1", TT ",...,", 
	  TT "d", SUB TT "n", TT "}", " of possible degrees for the hsop. It 
	  tests the degrees against two restrictions that are known to hold for 
	  any hsop of ", TT "R", ": firstly, the order of ", TT "G", " must 
	  divide the product ", TT "d", SUB TT "1", TT "*...*", TT "d", 
	  SUB TT "n", " and secondly, the polynomial ", TT EM "H", 
	  TT "(R,T)*(1-T", SUP TT "d1", TT ")*...*(1-T", SUP TT "dn", TT ")", 
	  " must lie in ", TO ZZ, "[", TT "T", "], where ", TT EM "H", 
	  TT "(R,T)", " is the Molien (Hilbert) series of ", TT "R", 
	  " [DK, p83]. Once a ", TO List, " of suitable degrees is found, the 
	  algorithm uses a Krull-dimension based test that holds for algebras 
	  over infinite fields to determine the existence of a hsop with the 
	  candidate degrees; see [K, Theorem 2]. It then finds such a hsop if 
	  one exists, or tries a new ", TO List, " of degrees if such a hsop 
	  does not exist. Note: if one knows a priori that a hsop exists for 
	  some ", TO List, " of degrees, this can be assigned to the optional 
	  argument ", TO DegreeVector, " and the default algorithm will compute 
	  a hsop with degrees corresponding to this ", TO List, ". Finally, 
	  users should be aware that the default algorithm currently only works 
	  in the case where ", TT "R", " is defined over a field of 
	  characteristic zero."
	  },
     PARA{
	  "The Dade algorithm is simpler than the default algorithm. It first 
	  constructs a Dade basis ", TT "v", SUB TT "1", TT ",...,v", 
	  SUB TT "n", " for the dual space ", TT "V", SUP TT "*", " spanned 
	  by ", TT "x", SUB TT "1", TT ",...,x", SUB TT "n", ". Then for each ", 
	  TT "i", ", it computes the polynomial ", TT "f", SUB TT "i", " defined 
	  as the product over the ", TT "G", "-orbit of ", TT "v", SUB TT "i", 
	  ". The resulting collection ", TT "f", SUB TT "1", TT ",...,", TT "f", 
	  SUB TT "n", " is a hsop for ", TT "R", "; see [DK, pp80,81]. In the 
	  implemented Dade algorithm, a Dade basis is constructed iteratively by 
	  choosing ", TO random, " linear forms such that ", TT "v", SUB TT "i", 
	  " is not contained in any of the vector subspaces ", TT "span", 
	  SUB TT "K", TT "{", TT "w", SUB TT "1", TT ",...,w", SUB TT "i-1", 
	  TT "}", ", where ", TT "w", SUB TT "j", " is in the ", TT "G", "-orbit 
	  of ", TT "v", SUB TT "j", ". The Dade algorithm can work with the case 
	  of finite fields, provided that the field is large enough to ensure ", 
	  TT "K", SUP TT "n", " cannot be filled by the union of the subspaces 
	  mentioned in the construction of the Dade basis. A sufficient, though 
	  not necessarily optimal, requirement is that ", TT "|K|>|G|", 
	  SUP TT "n-1", ". Because of the random generation involved in the 
	  construction of a Dade basis, the Dade algorithm will generally 
	  output ", TT "n", " primary invariants of degrees equalling the order 
	  of ", TT "G", " that have ugly coefficients."
	  }, 
     PARA{
	  "The example below provides a good comparison of the two different 
	  algorithms and their relative merits."
	  }, 
     EXAMPLE {
	  "A=matrix{{0,-1,0},{1,0,0},{0,0,-1}}",
	  "B=matrix{{0,-1,0},{1,0,0},{0,0,1}}",
          "C4xC2=finiteAction({A,B},QQ[x,y,z])",
          },
     PARA{
	  "The two algorithms used in ", TO primaryInvariants, " are 
	  timed. One sees that the Dade algorithm is faster, 
	  however the primary invariants output are all of degree 8 and have 
	  ugly coefficients."
	  },
     EXAMPLE {
	  "time P1=primaryInvariants C4xC2",
	  "time P2=primaryInvariants(C4xC2,Dade=>true)"
	  },
     PARA{
	  "The extra work done by the default algorithm to ensure an optimal 
	  hsop is rewarded by needing to calculate a smaller collection of 
	  corresponding secondary invariants.  In fact, it has proved quicker 
	  overall to calculate the invariant ring based on the optimal 
	  algorithm rather than the Dade algorithm."
	  },
     EXAMPLE {
	  "time secondaryInvariants(P1,C4xC2)",
	  "time secondaryInvariants(P2,C4xC2)",
	  "#oo"
	  },
     PARA{
	  "Of course, currently one advantage of the Dade algorithm is that it 
	  can calculate a hsop for the invariant ring when considering a 
	  finite field. Since ", TT "|C4xC2|", SUP TT "2", "=64, it is safe to 
	  consider the finite field with 101 elements."
	  },
     EXAMPLE{
	  "K=GF(101);",
	  "C4xC2=finiteAction({A,B},K[x,y,z])",
	  "primaryInvariants(C4xC2,Dade=>true)"
	  },     
     PARA{EM "References"},
     PARA{
	  "[DK] Derksen, H., Kemper, G. ", EM "Computational Invariant Theory", 
	  ". Berlin Heidelberg New York: Springer-Verlag, 2002"
	  },
     PARA{
	  "[K] Kemper, G. ", EM "An Algorithm to Calculate Optimal Homogeneous 
	  Systems of Parameters", ". J. Symbolic Computation ", EM "27", 
	  " (1999), 171-184"
	  },
     SeeAlso=>{primaryInvariants,Dade,DegreeVector}    
     }

------------------------------------------------
-- secondaryInvariants
-- Optional arguments: PrintDegreePolynomial 
------------------------------------------------

document {
	Key => {secondaryInvariants,(secondaryInvariants,List,FiniteGroupAction)},
	Headline => "computes secondary invariants for the invariant ring of a 
	finite group",
	Usage => "secondaryInvariants(P,G)",
	Inputs => {
		"P" => List => {
		     "a list of primary invariants in ", TT "n", " variables ", 
		     TT "f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", " for 
		     the invariant ring of ", TT "G", " defined over a field ",
	             TT "K", " of characteristic zero"
		     },
		"G" => FiniteGroupAction
		},
	Outputs => {
		List => {
		     "a list ", TT "S", " of secondary invariants for the 
		     invariant ring ", TT "R=K[x", SUB TT "1", TT ",...,x", 
		     SUB TT "n", TT "]", SUP TT "G", " of ", TT "G", " that 
		     makes ", TT "R", " into a free ", TT "K[f", SUB TT "1", 
		     TT ",...,f", SUB TT "n", TT "]", "-module with basis ", 
		     TT "S"
		     }
		},
	PARA{
	     "The example below computes the secondary invariants for the 
	     dihedral group with 6 elements, given a set of primary 
	     invariants", TT "P", "."
	     },
	EXAMPLE{
	     "K=toField(QQ[a]/(a^2+a+1));",
	     "R=K[x,y];",
	     "A=matrix{{a,0},{0,a^2}};",
	     "B=sub(matrix{{0,1},{1,0}},K);",
	     "D6=finiteAction({A,B},R)",
	     "P={x^3+y^3,-(x^3-y^3)^2};",
	     "secondaryInvariants(P,D6)"
	     },
	Caveat=> {
	     PARA{
		  "Currently, a user needs to ensure that the all primary 
		  invariants are defined with coefficients in a ring that ", 
		  EM "Macaulay2", " recognises as a characteristic zero field 
		  (see ", TO toField, " for a way to do this)."
		  },
	     PARA{
	  	  "Note also that the function ", TO secondaryInvariants, 
		  " only works when ", TT "R", " is defined over a field of 
		  characteristic zero."
	  	  }
	     },
	PARA{
	     "This function is provided by the package ", TO InvariantRing, "."
	     }
	}

document {
     Key => {[secondaryInvariants, PrintDegreePolynomial],[hironakaDecomposition, PrintDegreePolynomial],PrintDegreePolynomial},
     Headline => "an optional argument for secondaryInvariants that determines 
     the printing of an informative polynomial",
     Usage => "secondaryInvariants(P,G)",
	Inputs => {
		"P" => List => {
		     "a list of primary invariants in ", TT "n", " variables ", 
		     TT "f", SUB TT "1", TT ",...,", TT "f", SUB TT "n", " for 
		     the invariant ring of ", TT "G", " defined over a field ",
                     TT "K", " of characteristic zero"
		     },
		"G" => FiniteGroupAction
		},
	Outputs => {
		List => {
		     "a list ", TT "S", " of secondary invariants for the 
		     invariant ring ", TT "R=K[x", SUB TT "1", TT ",...,x", 
		     SUB TT "n", TT "]", SUP TT "G", " of ", TT "G", " that 
		     makes ", TT "R", " into a free ", TT "K[f", SUB TT "1", 
		     TT ",...,f", SUB TT "n", TT "]", "-module with basis ", 
		     TT "S"
		     }
		},
     PARA{
	  TO PrintDegreePolynomial, " takes a ", TO Boolean, " value and is 
	  set to ", 
	  TO false, " by default. If it is set to ", TO true, ", then ", 
	  TO secondaryInvariants, " will print a polynomial in the variable ",
          TT "T", ". This polynomial encodes the degrees of the secondary
          invariants (given by the exponents of ", TT "T", " appearing in it) 
	  and the number of secondary invariants of a given degree (the
          coefficient of the corresponding term in the
          polynomial). Specifically, if ", TO secondaryInvariants, " takes as 
          input a set of primary invariants of degrees ", TT "d", SUB TT "1", 
	  ",", TT "...", ",", TT "d", SUB TT "n", " for an invariant ring ", 
	  TT "S", 
	  SUP TT "G", " of a finite group ", TT "G", ", and ", TT "H(S", 
	  SUP TT "G", TT ",T)", "denotes the Molien (Hilbert) series of ",
	  TT "S", SUP TT "G", ", then ", TO secondaryInvariants, " will compute
	  the polynomial ",  TT "H(S", SUP TT "G", TT ",T)*", TT "(1-T", 
	  SUP(TT "d", SUB TT "1"), TT ")*...*(1-T", SUP(TT "d", SUB TT "n"), 
	  TT ")", "."   
     	  },    
     PARA{
	  "The example below computes the secondary invariants for the dihedral 
	  group with 6 elements, given a set of primary invariants ", TT "P",
          ". The optional argument ", TO PrintDegreePolynomial, " is set to ", 
	  TO true, " in order to see which degrees the secondary invariants 
	  should have."
	  },
	EXAMPLE{
	     "K=toField(QQ[a]/(a^2+a+1));",
	     "R=K[x,y];",
	     "A=matrix{{a,0},{0,a^2}};",
	     "B=sub(matrix{{0,1},{1,0}},K);",
	     "D6=finiteAction({A,B},R)",
	     "P={x^3+y^3,-(x^3-y^3)^2};",
	     "secondaryInvariants(P,D6,PrintDegreePolynomial=>true)"
	     },
	Caveat=> {
	     "Currently, a user needs to ensure that the all primary invariants 
	     are defined with coefficients in a ring that ", EM "Macaulay2", 
	     " recognises as a characteristic zero field (see ", TO toField, 
	     " for a way to do this)."
	},
     }
