-- -*- coding: utf-8 -*-
--		Copyright 1993-2007 by Daniel R. Grayson 

document {
     Key => {powermod,(powermod,ZZ,ZZ,ZZ)},
     Headline => "powers of integers mod N",
     Usage => "powermod(x,i,N)",
     Inputs => {
	  "x" => ZZ,
	  "i" => ZZ,
	  "N" => ZZ
	  },
     Outputs => {
	  ZZ => {"the ", TT "i", "-th power of ", TT "x", " modulo ", TT "N"}
	  },
     EXAMPLE lines ///
     	  powermod(2,3,10)
     	  powermod(2,4,10)
     	  powermod(2,30000,100000)
     	  powermod(2,30000,100000000000000000000)
	  powermod(2,3331333,3331333)
     ///
     }     

document {
     Key => {(markedGB, Matrix, Matrix), markedGB,
	  [markedGB,SyzygyMatrix],[markedGB,MinimalMatrix],[markedGB,ChangeMatrix]},
     Usage => "markedGB(lt,m)",
     Headline => "make a marked Gröbner basis",
     Inputs => {
	  "lt" => {"the matrix of monomials in (the columns of) ", TT "m", " to mark as lead terms, with respect to an
	       unspecified monomial ordering"},
	  "m" => {"the matrix whose columns are to form the generators of a Gröbner basis"},
	  SyzygyMatrix => Matrix => {"the matrix of syzygies"},
	  MinimalMatrix => Matrix => {"the matrix of minimal generators" },
	  ChangeMatrix => Matrix => {"the change-of-basis matrix" }
	  },
     Outputs => {
	  GroebnerBasis => {"the resulting Gröbner basis"}
	  }
     }

document {
     Key => utf8,
     Headline => "encode and decode unicode utf-8-encoded strings",
     SYNOPSIS (
     	  Usage => "utf8 x",
	  Inputs => {
	       "x" => List => {"a list of small natural numbers to serve as character codes"}
	       },
	  Outputs => {
	       String => {"a string obtained by encoding the character codes in ", TT "x", " according to the utf-8 encoding standard"}
	       },
	  EXAMPLE lines ///
	       s = utf8 {119, 111, 51, 32, 25105}
	  ///
	  ),
     SYNOPSIS (
     	  Usage => "utf8 s",
	  Inputs => {
	       "s" => String
	       },
	  Outputs => {
	       List => {"a list of the integer codes obtained by decoding the string ", TT "s", " according to the utf-8 encoding standard"}
	       },
	  EXAMPLE lines ///
	       utf8 s
	  ///
	  ),
     PARA {
	  "The two operations described above are inverse to each other."
	  },
     SeeAlso => {ascii},
     }

document {
     Key => "topLevelMode",
     Headline => "the current top level mode",
     Usage => "topLevelMode = x",
     Inputs => {
	  "x" => Symbol => {TO "TeXmacs", ", or ", TO "Standard"}
	  },
     Consequences => {
	  {"the interpreter will produce input and output prompts appropriate for the mode, and will
	       format output values appropriately"}
	  },
     PARA "This variable is intended for internal use only."
     }

document {
     Key => {(httpHeaders,String),httpHeaders},
     Headline => "prepend http headers to a string",
     Usage => "httpHeaders s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  String => {"the string obtained from ", TT "s", " by prepending appropriate headers to it"}
	  },
     PARA {
	  "This function is experimental, and is intended to support the development of web servers."
	  },
     EXAMPLE ///httpHeaders "hi there"///
     }

document {
     Key => globalAssign,
     Headline => "global assignment, in function form",
     Usage => "globalAssign(s,v)",
     Inputs => {
	  "s" => Symbol => "the symbol whose value is to be set",
	  "v" => Thing => {"the value to be assigned to ", TT "s"},
	  },
     Outputs => {
	  Thing => "v"
	  },
     PARA {
	  "This function mimics what happens in the interpreter when an assignment to a
	  global variable occurs, and can be useful if the name of the symbol is not
	  known when the code is written.  If the value changes,
	  then ", TO "GlobalReleaseHook", " and ", TO "GlobalAssignHook", " are called appropriately."
	  },
     EXAMPLE lines ///
     	  x = y
	  globalAssign(x,4)
	  x
	  y
     ///
     }

document {
     Key => getSymbol,
     Headline => "make a global user symbol from a string",
     Usage => "getSymbol s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  Symbol => {"a global symbol whose name is provided by the string ", TT "s", "
	       in the private dictionary for the package ", TO "User", "."
	       }
	  },
     EXAMPLE lines ///
     	  x = "aaaa"
     	  s = getSymbol x
	  dictionary s
	  s === getSymbol x
	  keys User#"private dictionary"
     ///,
     Caveat => {
	  "The old behavior, up to version 1.3.1, was to provide a previously existing global symbol, if
	  one exists and is visible in one of the dictionaries in ", TO "dictionaryPath", ", or, if not, to create a new
	  global symbol in the first mutable dictionary listed in ", TO "dictionaryPath", "."
	  },
     SeeAlso => { getGlobalSymbol }
     }

document {
     Key => {(Ext,Module,Module),(Ext,Ideal,Ideal),(Ext,Ideal,Module),(Ext,Ideal,Ring),(Ext,Module,Ideal),(Ext,Module,Ring)},
     Headline => "total Ext module",
     Usage => "Ext(M,N)",
     Inputs => { "M" => {ofClass{Ideal,Ring}}, "N" => {ofClass{Ideal,Ring}}},
     Outputs => {
	  TEX { "the $Ext$ module of $M$ and $N$,
	       as a multigraded module, with the modules $Ext^i(M,N)$ for all values of $i$ appearing simultaneously." }},
     PARA { "The modules ", TT "M", " and ", TT "N", " should be graded (homogeneous) modules over the same ring." },
     PARA { "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way." },
     PARA TEX {
	  "The computation of the total Ext module is possible for modules over the
	  ring $R$ of a complete intersection, according the algorithm
	  of Shamash-Eisenbud-Avramov-Buchweitz.  The result is provided as a finitely
	  presented module over a new ring with one additional variable of degree
	  ", TT "{-2,-d}", " for each equation of degree ", TT "d", " defining $R$.  The 
	  variables in this new ring have degree length 1 more than the degree length of 
	  the original ring, i.e., is multigraded, with the
	  degree ", TT "d", " part of $Ext^n(M,N)$ appearing as the degree
	  ", TT "prepend(-n,d)", " part of ", TT "Ext(M,N)", ".  We illustrate this in 
	  the following example."
	  },
     EXAMPLE lines ///
     R = QQ[x,y]/(x^3,y^2);
     N = cokernel matrix {{x^2, x*y}}
     H = Ext(N,N);
     ring H
     S = ring H;
     H
     isHomogeneous H
     rank source basis( { -2,-3 }, H)
     rank source basis( { -3 }, Ext^2(N,N) )
     rank source basis( { -4,-5 }, H)
     rank source basis( { -5 }, Ext^4(N,N) )
     hilbertSeries H
     hilbertSeries(H,Order=>11)
     ///,
     PARA{ "The result of the computation is cached for future reference." }
     }

document {
     Key => {(Ext,ZZ,Module,Module),
	  (Ext, ZZ, Ideal, Ideal),
	  (Ext, ZZ, Ideal, Module),
	  (Ext, ZZ, Module, Ideal),
	  (Ext, ZZ, Ideal, Ring),
	  (Ext, ZZ, Module, Ring)
	  },
     Usage => "Ext^i(M,N)",
     Headline => "Ext module",
     Inputs => { "i", "M", "N" },
     Outputs => {
	  { "the ", TT "i", "-th ", TT "Ext", " module of ", TT "M", " and ", TT "N" }
	  },
     "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     EXAMPLE lines ///
     	  R = ZZ/32003[a..d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  M = R^1/I
	  Ext^1(M,R)
	  Ext^2(M,R)
	  Ext^3(M,R)
	  Ext^1(I,R)
          ///,
     "As an efficiency consideration, it is generally much more efficient to compute
     Ext^i(R^1/I,N) rather than Ext^(i-1)(I,N).  The latter first computes a presentation 
     of the ideal I, and then a free resolution of that.  For many examples, the
     difference in time and space required can be very large.",
     SeeAlso => {resolution,Tor,Hom,monomialCurveIdeal,(Ext,ZZ,Matrix,Module),(Ext,ZZ,Module,Matrix)}
     }

document {
     Key => {(Ext, ZZ, Matrix, Module),
	  (Ext, ZZ, Matrix, Ideal),
	  (Ext, ZZ, Matrix, Ring)
	  },
     Usage => "Ext^i(f,N)",
     Headline => "map between Ext modules",
     Inputs => { "i", "f" => "M1 --> M2", "N" },
     Outputs => {
	  Matrix => {TEX ///the map $Ext^i(M2,N) \rightarrow{} Ext^i(M1,N)$///}
	  },
     "If ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     EXAMPLE lines ///
     	  R = ZZ/32003[a..d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  M1 = R^1/I
	  M2 = R^1/ideal(I_0,I_1)
	  f = inducedMap(M1,M2)
	  Ext^1(f,R)
	  g = Ext^2(f,R)
	  source g == Ext^2(M1,R)
	  target g == Ext^2(M2,R)
	  Ext^3(f,R)
          ///,
     SeeAlso => {resolution,Tor,Hom,(Ext,ZZ,Module,Module)}
     }

document {
     Key => {(Ext, ZZ, Module, Matrix),
	  (Ext, ZZ, Ideal, Matrix)
	  },
     Usage => "Ext^i(M,f)",
     Headline => "map between Ext modules",
     Inputs => { "i", "M", "f" => "N1 --> N2"},
     Outputs => {
	  Matrix => {TEX ///the induced map $Ext^i(M,N1) \rightarrow{} Ext^i(M,N2)$///}
	  },
     "If ", TT "M", " is an ideal, it is regarded as a module in the evident way.",
     PARA{},
     -- the code for Hom(Module,Matrix) is wrong, so we disable this example temporarily
     -- EXAMPLE lines ///
     -- 	  R = ZZ/32003[a..d];
     -- 	  I = monomialCurveIdeal(R,{1,3,4})
     -- 	  M = R^1/I
     -- 	  f = map(R^1,module I,gens I)
     -- 	  Ext^1(M,f)
     -- 	  g = Ext^2(M,f)
     -- 	  source g == Ext^2(M,source f)
     -- 	  target g == Ext^2(M,target f)
     -- 	  Ext^3(f,R)
     --      ///,
     SeeAlso => {resolution,Tor,Hom,(Ext,ZZ,Module,Module),(Ext,ZZ,Matrix,Module)}
     }

document {
     Key => {
	  (Ext, ZZ, CoherentSheaf, SumOfTwists),
	  (Ext, ZZ, SheafOfRings, SumOfTwists)
	  },
     Usage => "Ext^i(M,N(>=d))",
     Headline => "global Ext",
     Inputs => { "i", "M", "N(>=d)"},
     Outputs => {
	  Module => {"The R-module ", TEX ///$\oplus_{j \geq d} Ext^i_X(M,N(j))$///}
	  },
     "If ", TT "M", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X = Proj R", ".",
     PARA{},
     "As an example, we consider the rational quartic curve in 
     ", TEX "$P^3$", ".",
     EXAMPLE lines ///
     	  S = QQ[a..d];
	  I = monomialCurveIdeal(S,{1,3,4})
	  R = S/I
	  X = Proj R
     	  IX = sheaf (module I ** R)
     	  Ext^1(IX,OO_X(>=-3))
	  Ext^0(IX,OO_X(>=-10))
	  ///,
     PARA{},
     "The method used may be found in: 
     Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746",
     PARA{},
     TEX ///If the vector space $Ext^i(M,N)$ is desired, see ///, TO (Ext,ZZ,CoherentSheaf,CoherentSheaf), ".",
     SeeAlso => {resolution,Tor,Hom,HH,sheafExt}
     }

document {
     Key => {
	  (Ext, ZZ, CoherentSheaf, CoherentSheaf),
	  (Ext, ZZ, SheafOfRings, CoherentSheaf),
	  (Ext, ZZ, CoherentSheaf, SheafOfRings),
	  (Ext, ZZ, SheafOfRings, SheafOfRings)
	  },
     Usage => "Ext^i(M,N)",
     Headline => "global Ext",
     Inputs => { "i", "M", "N"},
     Outputs => {
	  Module => {"The global Ext module ", TEX "$Ext^i_X(M,N)$"}
	  },
     "If ", TT "M", " or ", TT "N", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
     PARA{},
     "As an example, we compute Hom_X(I_X,OO_X), and Ext^1_X(I_X,OO_X), for the rational quartic curve in 
     ", TEX "$P^3$", ".",
     EXAMPLE lines ///
     	  S = QQ[a..d];
	  I = monomialCurveIdeal(S,{1,3,4})
	  R = S/I
	  X = Proj R
     	  IX = sheaf (module I ** R)
     	  Ext^1(IX,OO_X)
	  Hom(IX,OO_X)
	  ///,
     "The Ext^1 being zero says that the point corresponding to I on the Hilbert scheme is
     smooth (unobstructed), and vector space dimension of Hom tells us that the
     dimension of the component at the point I is 16.",
     PARA{},
     "The method used may be found in: 
     Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746",
     PARA{},
     TEX ///If the module $\oplus_{d\geq 0} Ext^i(M,N(d))$ is desired, see ///, TO (Ext,ZZ,CoherentSheaf,SumOfTwists), ".",
     SeeAlso => {resolution,Tor,Hom,HH,sheafExt,(Ext,ZZ,CoherentSheaf,SumOfTwists)}
     }

document {
     Key => Ext,
     Headline => "compute an Ext module"
     }

document {
     Key => dd,
     Headline => "differential in a chain complex",
     TT "dd", " -- a symbol used as a key in a chain complex, under which
     are stored the differentials.",
     PARA{},
     "The differentials are stored as the members in a chain complex
     map of degree ", TT "-1", ".  The map ", TT "C.dd_i", " is the
     one whose source is the module ", TT "C_i", ", and it is arranged to
     be zero if no map has been stored in that spot.",
     EXAMPLE {
	  "R = ZZ/101[a,b];",
      	  "C = resolution cokernel vars R",
      	  "C.dd",
      	  "C.dd_2",
	  },
     SeeAlso => "ChainComplex"
     }

TEST "
assert( toString Tor == \"Tor\" )
"

document {
     Key => Tor,
     Headline => "Tor module"
     }
document {
     Key => {(Tor,ZZ,Module,Module),(Tor,ZZ,Module,Ring),(Tor,ZZ,Ideal,Ideal),(Tor,ZZ,Ideal,Module),(Tor,ZZ,Module,Ideal),(Tor,ZZ,Ideal,Ring)},
     Headline => "compute a Tor module",
     Usage => "Tor_i(M,N)",
     Inputs => { "i", "M", "N" },
     Outputs => {
	  { "the ", TT "i", "-th ", TT "Tor", " module of ", TT "M", " and ", TT "N" }
	  },
     PARA {
	  "If ", TT "N", " is a ring (instead of a module), then the free module of rank 1 over it is used instead.  If
	  ", TT "M", " or ", TT "N", " is an ideal of ring, then the underlying module is used instead."
	  }
     }

document {
     Key => MonomialIdeal, 
     Headline => "the class of all monomial ideals handled by the engine",
     "Monomial ideals are kinds of ideals, but many algorithms are much faster.
     Generally, any routines available for ideals are also available for monomial
     ideals.",
     EXAMPLE lines ///
         R = QQ[a..d];
	 I = monomialIdeal(a*b*c,b*c*d,a^2*d,b^3*c)
	 I^2
	 I + monomialIdeal(b*c)
	 I : monomialIdeal(b*c)
	 radical I
	 associatedPrimes I
	 primaryDecomposition I
     ///,
     HEADER3 "Specialized functions only available for monomial ideals",
     UL {
	  TO (borel,MonomialIdeal),
	  TO (isBorel,MonomialIdeal),
	  TO (symbol-, MonomialIdeal, MonomialIdeal),
	  TO (dual, MonomialIdeal),
	  TO independentSets,
	  TO "PrimaryDecomposition::irreducibleDecomposition",
	  TO standardPairs,
	  },
     EXAMPLE lines ///
         borel I
	 isBorel I
	 I - monomialIdeal(b^3*c,b^4)
	 standardPairs I
	 independentSets I
	 dual I
     ///,
     "The ring of a monomial ideal must be a commutative polynomial ring.  This ring must
     not be a skew commuting ring, and/or a quotient ring.",
     }

document {
     Key => (symbol -, MonomialIdeal, MonomialIdeal),
     Headline => "monomial ideal difference",
     Usage => "I - J",
     Inputs => {
	  "I", "J"
	  },
     Outputs => { 
	  MonomialIdeal => {"generated by those minimal generators of I which are not in J"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  I = monomialIdeal(a^3,b^2,a*b*c)
	  J = monomialIdeal(a^2,b^3,a*b*c)
	  I - J
	  J - I
	  I - (I-J)
          ///,
     SeeAlso => {MonomialIdeal}
     }

--document { (complement, MonomialIdeal),
--					  -- compute { m^c : m minimal generator of I }, where
--					  -- m^c is the product of the variables 
--					  -- not in the support of m.",
--     TT "complement I", " -- computes the complement of a monomial ideal.",
--     PARA{},
--     "The complement of ", TT "I", " is defined to be the ideal generated by those monomials
--     which have no factor in common with some monomial of ", TT "I", ".",
--     SeeAlso => MonomialIdeal
--     }
--
--document { (complement, MonomialIdeal, Monomial),
--					  -- (I,p) -- compute { p/m : m in I }, where p is a given monomial",
--     TT "complement(I,p)", " -- ",
--     SeeAlso => MonomialIdeal
--     }     
--

document {
     Key => {[(dual,MonomialIdeal),Strategy], [(dual,MonomialIdeal,List),Strategy], [(dual,MonomialIdeal,RingElement),Strategy]},
     PARA {
	  "Specify ", TT "Strategy => 1", " to test an older strategy for performing the computation."
	  }
     }

document {
     Key => (dual,MonomialIdeal,List),
     Headline => "the Alexander dual",
     Usage => "dual(I,e)",
     Inputs => {
	  "I" => {"a monomial ideal"},
	  "e" => {"a list of exponents"}
	  },
     Outputs => { {"the Alexander dual of ", TT "I", " formed with respect to the monomial whose exponents are listed in ", TT "e" } },
     PARA { "The monomial corresponding to ", TT "e", " should be divisible by all of the minimal generators of ", TT "I", "." },
     PARA {
	  "The computation is done by calling the ", TO "frobby", " library, written by B. H. Roune;
	  setting ", TO "gbTrace", " to a positive value will cause a message to be printed when it is called."
	  },
     SeeAlso => {(dual,MonomialIdeal)}
     }

document {
     Key => (dual,MonomialIdeal,RingElement),
     Headline => "the Alexander dual",
     Usage => "dual(I,m)",
     Inputs => {
	  "I" => {"a monomial ideal"},
	  "m" => {"a monomial"}
	  },
     Outputs => { {"the Alexander dual of ", TT "I", " formed with respect to ", TT "m", "." } },
     "The monomial ", TT "m", " should be divisible by all
     of the minimal generators of ", TT "I", ".",
     SeeAlso => {(dual,MonomialIdeal)}
     }

document {
     Key => (dual,MonomialIdeal),
     Headline => "the Alexander dual of a monomial ideal",
     Usage => "dual I",
     Inputs => {
	  "I" => {"a monomial ideal"}
	  },
     Outputs => {
	  {"the Alexander dual of ", TT "I"}
	  },
     "If ", TT "I", "is a square free monomial ideal then ",
     TT "I", " is the Stanley-Reisner ideal of a simplicial 
     complex.  In this case, ", TT "dual I", " is the 
     Stanley-Reisner ideal associated to the dual complex.  
     In particular, ", TT "dual I", " is obtained by 
     switching the roles of minimal generators and prime 
     components.",
     EXAMPLE {
	  "QQ[a,b,c,d];",
	  "I = monomialIdeal(a*b, b*c, c*d)",
	  "dual I",
	  "intersect(monomialIdeal(a,b), 
	       monomialIdeal(b,c),
	       monomialIdeal(c,d))",
	  "dual dual I" 
	  },
     PARA{},
     "For a general monomial ideal, the Alexander dual 
     defined as follows:  Given two list of nonnegative 
     integers ", TT "a", " and ", TT "b", "for which ", 
     TT "a_i >= b_i", " for all ", TT "i", " let ", 
     TT "a\\b", " denote the list whose ", TT "i", "-th 
     entry is ", TT "a_i+1-b_i", "if ", TT "b_i >= 1", 
     "and ", TT "0", "otherwise.  The Alexander dual with 
     respect to ", TT "a", " is the ideal generated by 
     a monomial ", TT "x^a\\b", " for each irreducible 
     component ", TT "(x_i^b_i)", " of ", TT "I", ".  
     If ", TT "a", " is not provided, it is assumed to be 
     the least common multiple of the minimal generators
     of ", TT "I", ".",
     EXAMPLE {
     	  "QQ[x,y,z];",
	  "I = monomialIdeal(x^3, x*y, y*z^2)", 
	  "dual(I, {4,4,4})",
	  "intersect( monomialIdeal(x^2),
	       monomialIdeal(x^4, y^4),
	       monomialIdeal(y^4, z^3))",
	  },  
     PARA{},
     "One always has ", TT "dual( dual(I, a), a) == I", 
     " however ", TT "dual dual I", "may not equal ", 
     TT "I", ".",
     EXAMPLE lines ///
	  QQ[x,y,z];
	  J = monomialIdeal( x^3*y^2, x*y^4, x*z, y^2*z)
	  dual dual J
	  dual( dual(J, {3,4,1}), {3,4,1})
	  ///,
     PARA{},
     "See Ezra Miller's Ph.D. thesis 'Resolutions and 
     Duality for Monomial Ideals'.",
     PARA { "Implemented by Greg Smith." },
     PARA {
	  "The computation is done by calling the ", TO "frobby", " library, written by B. H. Roune;
	  setting ", TO "gbTrace", " to a positive value will cause a message to be printed when it is called."
	  }
     }

document {
     Key => {
	  monomialSubideal,
	  (monomialSubideal, Ideal)},
     Headline => "find the largest monomial ideal in an ideal",
     Usage => "monomialSubideal I",
     Inputs => {
	  "I" => Ideal
	  },
     Outputs => {
	  MonomialIdeal => {"the largest monomial ideal contained in ", TT "I"}
	  },
     EXAMPLE lines ///
	  QQ[a,b,c,d];
	  I = ideal(b*c, c^2 - b*d, -a*c+b^2)
	  monomialSubideal I
	  ///,
     PARA{},
     "Implemented by Greg Smith."
     }


TEST "
R = ZZ/101[x,y]
assert( 
     intersect(image matrix {{1},{x}}, image matrix {{x}, {x^2}}) 
     == image matrix {{x}, {x^2}}
     )
assert(
     intersect(image matrix {{1},{x}}, image matrix {{x}, {x^3}}) ==  0
     )
assert( intersect( ideal(x^2,y), ideal (x,y^2)) == ideal (y^2, x^2, x*y) )
"

TEST "
R = ZZ/101[a..d]
assert(
     intersect(
	  subquotient(matrix {{a}},matrix {{d}}),
	  subquotient(matrix {{b}},matrix{{d}})
	  )
     ==
     subquotient(matrix {{a*b}},matrix {{d}})
     )
"

document {
     Key => {isBorel,(isBorel, MonomialIdeal)},
     Headline => "whether an ideal is fixed by upper triangular changes of coordinates"
     }

document {
     Key => GradedModule,
     Headline => "the class of all graded modules",
     "A new graded module can be made with 'M = new GradedModule'.
     The i-th module can be installed with a statement like ", TT "M#i=N", ",
     and can be retrieved with an expression like ", TT "M_i", ".  The ground
     ring should be installed with a statement like ", TT "M.ring = R", ".",
     SeeAlso => "GradedModuleMap"
     }

document {
     Key => GradedModuleMap,
     Headline => "the class of all maps between graded modules",
     SeeAlso => "GradedModule"
     }

document {
     Key => {gradedModule,(gradedModule, Module), (gradedModule, List), (gradedModule, Sequence), (gradedModule, ChainComplex)},
     Headline => "make a graded module",
     Usage => "gradedModule v",
     Inputs => { "v" => List => "a module, or a list or sequence of modules" },
     Outputs => {{"the graded module with the ", TT "i", "-th element of ", TT "v", " installed as its ", TT "i", "-th component"}},
     EXAMPLE lines ///
     	  gradedModule ZZ^2
     	  gradedModule(ZZ^2,ZZ^3,ZZ^400)
     ///,
     "If ", TT "v", " is ", ofClass ChainComplex, " then the return value is the graded module underlying it.",
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  C = res coker vars R
	  gradedModule C
     ///,
     }

document {
     Key => {gradedModuleMap,(gradedModuleMap, Sequence), (gradedModuleMap, ModuleMap), (gradedModuleMap, List)},
     Headline => "make a map of graded modules",
     Usage => "gradedModuleMap v",
     Inputs => { "v" => List => "a matrix, or a list or sequence of matrices" },
     Outputs => {{"the graded module map with the ", TT "i", "-th element of ", TT "v", " installed as its ", TT "i", "-th component"}},
     EXAMPLE lines ///
     	  gradedModuleMap(random(ZZ^3,ZZ^4),random(ZZ^2,ZZ^2))
     ///
     }

document {
     Key => ChainComplex,
     Headline => "the class of all chain complexes",
     "For an overview of creating and using chain complexes in Macaulay2, see ",
     TO "chain complexes", ".",
     PARA{},
     "Common ways to create a chain complex",
     UL {
	  TO chainComplex,
	  TO (resolution,Module),
	  },
     "Information about a chain complex",
     UL {
	  TO (length,ChainComplex),
	  TO (min,GradedModule),
	  TO (max,GradedModule),
	  TO (betti,GradedModule),
	  TO (ring,ChainComplex)
	  },
     "Operations on chain complexes",
     UL {
	  TO (dual,ChainComplex),
	  TO (symbol++,ChainComplex,ChainComplex),
	  TO (symbol**,ChainComplex,Module),
	  TO (symbol**,ChainComplex,ChainComplex),
	  TO (Hom,ChainComplex,Module),
	  TO (symbol SPACE, ChainComplex, Array)
	  },
     }

document {
     Key => (complete, ChainComplex),
     Headline => "complete the internal parts",
     TT "complete C", " -- fills in the modules of a chain complex
     obtained as a resolution with information from the engine.",
     PARA{},
     "For the sake of efficiency, when a chain complex arises as
     a resolution of a module, the free modules are not filled in until
     they are needed.  This routine can be used to fill them all in, and
     is called internally when a chain complex is printed.
     Normally users will not need this function, unless they use ", TO "#", " to
     obtain the modules of the chain complex, or use ", TO "keys", " to
     see which spots are occupied by modules.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "C = resolution cokernel vars R;",
      	  "keys C",
      	  "complete C;",
      	  "keys C"
	  }
     }


document {
     Key => (length, ChainComplex),
     Headline => "length of a chain complex or graded module",
     TT "length C", " -- the length of a chain complex.",
     PARA{},
     "The length of a chain complex is defined to be the difference
     between the smallest and largest indices of spots occupied by
     modules, even if those modules happen to be zero."
     }

document {
     Key => ChainComplexMap,
     Headline => "the class of all maps between chain complexes",
     "The usual algebraic operations are available: addition, subtraction,
     scalar multiplication, and composition.  The identity map from a
     chain complex to itself can be produced with ", TO "id", ".  An
     attempt to add (subtract, or compare) a ring element to a chain complex
     will result in the ring element being multiplied by the appropriate
     identity map."
     }


document { Key => {extend},
     Headline => "extend a module map to a chain map, if possible" }
document {
     Key => {(extend,ChainComplex,ChainComplex,Matrix),[extend,Verify]},
     Usage => "extend(D,C,f0)",
     Inputs => {
	  "D",
	  "C",
	  "f0" => { "a map from ", TT "C_0", " to ", TT "D_0" },
	  Verify => Boolean => {"whether to check that the map extends"}
	  },
     Outputs => { { "a chain complex map ", TT "f: D <--- C", " of degree 0 extending ", TT "f0", " in the sense that ", TT "f_0==f0" } },
     EXAMPLE {
	  "R = ZZ/101[a..c]",
	  "I = image vars R",
	  "J = image symmetricPower (2,vars R)",
	  "g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))",
	  "g_1",
	  "g_2"
	  },
     SeeAlso => {(symbol _,ChainComplex,ZZ),(symbol _,ChainComplexMap,ZZ),(cone,ChainComplexMap) }
     }

TEST "
R = ZZ/101[a..c]
I = image vars R
J = image symmetricPower (2,vars R)
g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))
E = cone g
"

document { Key => cone,
     Headline => "mapping cone of a chain map" }
document {
     Key => (cone,ChainComplexMap),
     Usage => "cone f",
     Inputs => { "f" },
     Outputs => { {"the mapping cone of a ", TT "f", ""} },
     {
	  EXAMPLE {
	       "R = ZZ/101[x,y,z]",
	       "m = image vars R",
	       "m2 = image symmetricPower(2,vars R)",
	       "M = R^1/m2",
	       "N = R^1/m",
	       "C = cone extend(resolution N,resolution M,id_(R^1))",
	       },
	  "Let's check that the homology is correct; for example, ", TT "HH_0", " should be zero.",
	  EXAMPLE "prune HH_0 C",
	  "Let's check that ", TT "HH_1", " is isomorphic to ", TT "m/m2", ".",
	  EXAMPLE {
	       "prune HH_1 C",
	       "prune (m/m2)"
	       }
	  }}

document {
     Key => {nullhomotopy,(nullhomotopy, ChainComplexMap)},
     Headline => "make a null homotopy",
     TT "nullhomotopy f", " -- produce a nullhomotopy for a map f of 
     chain complexes.",
     PARA{}, 
     "Whether f is null homotopic is not checked.",
     PARA{},
     "Here is part of an example provided by Luchezar Avramov.  We
     construct a random module over a complete intersection, resolve 
     it over the polynomial ring, and produce a null homotopy for the
     map that is multiplication by one of the defining equations
     for the complete intersection.",
     EXAMPLE {
	  "A = ZZ/101[x,y];",
      	  "M = cokernel random(A^3, A^{-2,-2})",
      	  "R = cokernel matrix {{x^3,y^4}}",
      	  "N = prune (M**R)",
      	  "C = resolution N",
      	  "d = C.dd",
      	  "s = nullhomotopy (x^3 * id_C)",
      	  "s*d + d*s",
      	  "s^2"
	  },
     }

TEST ///
R=ZZ[a]
assert( toString a === "a" )
assert( toString a^2 === "a^2" )
///

TEST "
    -- test of lift/promote of an ideal
    A = ZZ/101[a..d]
    A = QQ[a..d]
    A = GF(5,2)[a..d]
    B = A/(a^2-d^2)
    use A
    I = ideal(a,b)
    assert(ring I === A)
    I1 = I*B
    I2 = lift(I1,A)
    assert(trim I2 == ideal(a,b,d^2))
    use B
    C = B/(b^3-c^3)
    I3 = I2*C
    I3a = I*C
    assert(I3 == I3a)
    I4 = lift(I3,B)
    I5 = trim lift(I3,A)
    use A
    assert(I5 == ideal(a,b,c^3,d^2))
"

TEST ///
     z = 2 - 3*ii
     w = 4 + 5*ii
     x = 2 + ii - ii

     eps = 10.^-10
     small = (x) -> abs x < eps
     near = (w,z) -> small realPart(w-z) and small imaginaryPart(w-z) 

     assert( z*w == 23  - 2*ii )
     assert( near(z/w , -7/41 + -22/41 * ii ) )
     assert( 1 + z == 3 - 3*ii )
     assert( 2*w == 8 + 10*ii )
     assert( z + w == 6 + 2*ii )
     assert( toString w == "4+5*ii" )
     assert( conjugate z == 2 + 3*ii )
     assert( x == 2 )
     assert( x == 2. )
     assert( x == 2/1 )
     assert( net ( 2 - 3 * ii ) === "2-3*ii" )
     ///


TEST "
assert (
     (0,0)..(2,3) == 
     ((0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3))
     )
"

TEST "
R=ZZ/101[a,b]
f=matrix(R,{{1,a},{0,1}})
g=matrix(R,{{1,0},{b,1}})
h=f*g*f*g
assert( h^3 * h^-1 == h^2 * h^0 )
assert( h * h^-1 == 1 )
"

TEST "
R=ZZ/101[a,b]
f = matrix {{a}}
assert( source f != target f)
assert( target f == target f^2 )
assert( source f == source f^2 )
assert( target f == target f^0 )
assert( source f != source f^0 )
"

TEST "
R = ZZ/101[a..d]
F = R^3
H = subquotient(F_{1,2}, F_{2})
f = map(H,cover H,id_(cover H))
assert( cokernel f == 0 )
assert( kernel f == image R^2_{1} )
assert( isWellDefined f )
assert( isSurjective f )
assert( not isInjective f )
"


TEST "
R = ZZ/101[x,y,z]
assert isHomogeneous map(R^2,2,(i,j)->R_j)
assert isHomogeneous map(R^2,5,{{x,y,z,x^2,y^2},{x,0,z,z^2,0}})
"

TEST "
R = ZZ/101[a..d]
f=1+a+b+c+d
assert(size f == 5)
S = R/a
assert(size promote(f,S) == 4)
"

TEST "
R = ZZ/101
exteriorPower(3,R^5)
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
M = Ext^2(coker generators I, R)
prune exteriorPower(3,M)
exteriorPower(0,R^3)
exteriorPower(0,M)
prune exteriorPower(1,M)
exteriorPower(2,M)
exteriorPower(-1,M)
exteriorPower(-2,M)

M = subquotient(matrix{{a,b,c}}, matrix{{a^2,b^2,c^2,d^2}})
N = subquotient(matrix{{a^2,b^2,c^2}}, matrix{{a^3,b^3,c^3,d^3}})
m = map(N,M,matrix(R,{{1,0,0},{0,1,0},{0,0,1}}))
source m
target m
trim ker m
M1 = coker presentation M
N1 = coker presentation N
m1 = map(N1,M1,matrix m)
M2 = trim exteriorPower(2,M)
N2 = trim exteriorPower(2,N)
"

TEST "
R = ZZ/101[a .. i]
m = genericMatrix(R,a,3,3)
assert( exteriorPower(1,m) == m )
assert( minors(1,m) == image vars R )
assert( exteriorPower(2,m*m) == exteriorPower(2,m)*exteriorPower(2,m) )
assert( 
     exteriorPower(2,m)
     == 
     matrix {
	  {-b*d+a*e, -b*g+a*h, -e*g+d*h},
	  {-c*d+a*f, -c*g+a*i, -f*g+d*i},
	  {-c*e+b*f, -c*h+b*i, -f*h+e*i}} )
assert( exteriorPower(3,m) == matrix {{-c*e*g+b*f*g+c*d*h-a*f*h-b*d*i+a*e*i}} )
"

TEST "
k = ZZ/101
f = random(k^3,k^9)
R = k[a,b,c]
g = random(R^4,R^{-2,-2})
"

TEST "
R = ZZ/101[a..d]
assert( hilbertFunction(3,R) === 20 )
assert( hilbertFunction(10,R) === 286 )
"

TEST "
R = ZZ/101[a..f]
assert( dim image matrix {{a,b}} == 6 )
assert( dim coker matrix {{a,b}} == 4 )
assert( dim coker matrix {{a-1,b-c^2}} == 4 )
assert( dim ideal (a,b) == 4 )
assert( codim ideal (a,b) == 2 )
assert( dim R == 6 )
assert( dim (R/a) == 5 )
"

TEST "
	R = ZZ/101[a..d]
	f = matrix{{a,b},{c,d}}
	g = matrix(R,{{1},{0}})
	M = subquotient(g,f)
	assert( numgens source basis(3,M) == 16 )
"

TEST "
L = ZZ/5[t]
M = L/(t^2+t+1)
G = GF(M,Variable => v,PrimitiveElement => t-1)
assert( lift(v,M) + 1 == lift (v+1,M) )
assert( lift(v^6,M) == (lift(v,M))^6 )
assert( lift(v^7,M) == (lift(v,M))^7 )
"

TEST "
R=ZZ/101[a,b,c]
f = map(R,R,matrix(ZZ/101,{{1,2,3},{4,5,6},{7,8,9}}))
assert( f(a) == a + 4*b + 7*c )
assert( kernel f == ideal ( a-2*b+c ) )
"

TEST "
f = map(frac (QQ[r,s,t]), QQ[x,y,z], {(r-s)/t,(s-t)/r,(t-r)/s})
assert( kernel( f, SubringLimit => 1 ) == ideal(x*y*z+x+y+z) )
"

TEST "
S = ZZ/101[x,y]
R = ZZ/101[t,u]
f = map(S,R,{x,0})
assert( kernel f == ideal u )
"

TEST "
S = ZZ/101[x,y]
R = ZZ/101[t,u]
f = map(S,R,{x,1})
assert( kernel f == ideal (u-1) )
"

TEST "
R = ZZ/101[a..f]
m = matrix {{a*b*c*d, b^2*c*d, b*c^2*d, b*c*d^2, b*c*d*e, 
	     c*d*e*f, a*d*e*f, a*b*e*f, a*b*c*f, b*c*d*f}}
f = map(R,ZZ/101[x_0..x_9],m)
J = kernel f
"

TEST "
S = ZZ/101[a..j]
m = matrix {{d*g*i-a*g*j, c*h^2-e*h*i, a*b^2*g-a*b*d*h, b*d*f-d*e*j}}
E = Ext^3(cokernel m, S)
annihilator E
"

TEST "
    R = ZZ/101[s,t]
    J = image matrix {{s^4, s^3*t, s*t^3, t^4}}
    S = symmetricAlgebra J  -- MES: make an assertion here...
"

TEST "
R = ZZ/101[a,b,c,d]
f = matrix {{c^3-b*d^2, b*c-a*d, b^3-a^2*c, a*c^2-b^2*d}}
M = cokernel f
assert( codim M === 2 )
assert( dim M === 2 )
E = Ext^2(M, R^1)
T = (degreesRing R)_0
p = poincare E
assert ( p == 3*T^(-3)-5*T^(-2)+1*T^(-1)+1 )
assert( dim E === 2 )
assert( dim Ext^1(M,R^1) === -1 )
-- assert ( poincare prune Ext^2(M,M) == (4T^-3 + 2T^-2 - 5T^-1 + 3) (1 - T)^2 )

F = Ext^3(M, R^1)
assert( dim F === 0 )
assert( degree F === 1 )

assert( Ext^4(M,R^1) == 0 )

k = cokernel vars R
N = cokernel matrix {{1_R}}
assert( dim Ext^2(N,k) === -1 )

g = vars R
P = (image g) / (image matrix {{a^2, b^2, c^2, d^2}})

assert( degree Hom(P,k) === 4 )
assert( degree P === 15 )
assert( dim P === 0 )
assert( pdim P === 4 )

assert( degree Ext^4(P,P) === 15 )

image g / image(g**g**g)
"

TEST "
eg1 = () -> (
  R = ZZ/101[a..d];
  m = matrix {{a*d - b*c, a^2*c - b^3, c^3 - b*d^2, a*c^2 - b^2*d}};
  C = resolution cokernel m;
  E2 = Ext^2(cokernel m, R)
  )
eg1()

eg2 = () -> (
  -- gbTrace = 3;
  R = ZZ/101[a..f];
  m = matrix {{a*b*c - d*e*f, a*b*d - c*e*f, a*e*f - b*c*d}};
  C = resolution cokernel m;
  -- topComponents m
  )
eg2()

eg3 = () -> (
  -- test newCoordinateSystem
  R = ZZ/101[a..f];
  m = matrix {{a-b+c, a-d-f}};
  newCoordinateSystem(R, m))
--eg3()
"

TEST ///
     Q = ZZ/101[x,y]
     I = ideal(x^3,y^5)
     R = Q/I
     N = cokernel random (R^3, R^{2:-2})
     M = cokernel random (R^3, R^{2:-2})
     E = Ext(N,M)
     scan(4, d -> (
	  bd := basis Ext^d(N,M);
	  assert(
	       tally splice apply(-10..10,i -> rank source basis({-d,i},E) : {i}) ===
	       tally apply(rank source bd, i -> degree bd_i))))
///

TEST "
-- copyright 1995 Michael E. Stillman
-- several tests of tensor products and Tor
-- many of these examples were created by David Eisenbud

-- Test 1.  Checking that Tor_i(M,k) and Tor_i(k,M) both give
-- the graded betti numbers of M.

R = ZZ/101[a..d]
k = cokernel vars R
M = cokernel matrix {{a*d - b*c, a^2*c - b^3, c^3 - b*d^2, a*c^2 - b^2*d}}

T0 = Tor_0(M,k)
S0 = Tor_0(k,M)
T1 = Tor_1(M,k)
S1 = Tor_1(k,M)
T2 = Tor_2(M,k)
S2 = Tor_2(k,M)
T3 = Tor_3(M,k)
S3 = Tor_3(k,M)
T4 = Tor_4(M,k)
S4 = Tor_4(k,M)

T = (degreesRing R)_0

assert(poincare T0 ==             (1-T)^4)
assert(poincare T1 == (T^2+3*T^3)*(1-T)^4)
assert(poincare T2 == 4*T^4*      (1-T)^4)
assert(poincare T3 ==  T^5 *      (1-T)^4)
assert(poincare T4 ==  0)

assert(poincare T0 == poincare S0)
assert(poincare T1 == poincare S1)
assert(poincare T2 == poincare S2)
assert(poincare T3 == poincare S3)
assert(poincare T4 == poincare S4)

-- notice that degree Tor_i(M,k) gives the i th betti number of M,
-- as does Tor_i(k,M), and the graded betti numbers can be seen by using 
-- 'see target prune Tor_i(k,M)'
-- or by using, for example

hf = poincare T2;
if hf != 0 then while substitute(hf,{T=>1}) == 0 do hf = hf // (1-T);
hf

-- Test 2.  Intersection multiplicity according to Serre
-- The intersection of two planes in 4-space meeting another such.
-- The multiplicity should be 4.  Serre's technique says that this
-- number should be the alternating sum of the 'degree Tor_i(R/I,R/J)':

R = ZZ/101[a..d]
I = generators intersect(image matrix {{a,b}}, image matrix {{c,d}});
J = generators intersect(image matrix {{a-c-b, b-d}}, image matrix {{a-d, b-c}});

U0 = Tor_0(cokernel I, cokernel J);
U1 = Tor_1(cokernel I, cokernel J);
U2 = Tor_2(cokernel I, cokernel J);
U3 = Tor_3(cokernel I, cokernel J);
U4 = Tor_4(cokernel I, cokernel J)

U0 = prune U0
assert( numgens target presentation U0 == 1 )
assert( numgens source presentation U0 == 8 )

U1 = prune U1
assert( numgens target presentation U1 == 4 )
assert( numgens source presentation U1 == 16 )

U2 = prune U2
assert( numgens target presentation U2 == 1 )
assert( numgens source presentation U2 == 4 )

U3 = prune U3
assert( numgens target presentation U3 == 0 )
assert( numgens source presentation U3 == 0 )

U4 = prune U4
assert( numgens target presentation U4 == 0 )
assert( numgens source presentation U4 == 0 )

assert( degree U0 == 7 )
assert( degree U1 == 4 )
assert( degree U2 == 1 )
assert( degree U3 == 0 )
assert( degree U4 == 0 )
"

TEST "
R=ZZ/101[x]
assert(monomialIdeal vars R != 0)
assert(monomialIdeal map(R^1,R^1,0) == 0)
"

TEST ///
     -- here we test the commutativity of the pentagon of associativities!
     C = QQ^1[0] ++ QQ^1[-1]
     assert(
	  (tensorAssociativity(C,C,C) ** C) * tensorAssociativity(C,C**C,C) * (C ** tensorAssociativity(C,C,C))
	  ==
	  tensorAssociativity(C**C,C,C) * tensorAssociativity(C,C,C**C)
	  )
     ///

document {
     Key => {(symbol ++,ChainComplex,ChainComplex),(symbol ++,GradedModule,GradedModule),
	  (symbol ++,ChainComplexMap,ChainComplexMap)},
     Headline => "direct sum",
     TT "C++D", " -- direct sum of chain complexes and maps",
     EXAMPLE lines ///
	  C = resolution cokernel matrix {{4,5}}
      	  C ++ C[-2]
     ///
     }

document {
     Key => (components, ChainComplex),
     TT "components C", " -- returns from which C was formed, if C
     was formed as a direct sum.",
     PARA{},
     SeeAlso => "isDirectSum"
     }

document {
     Key => (symbol SPACE, ChainComplex, Array),
     Headline => "degree shift",
     Usage => "D = C[i]",
     Inputs => {
	  "C" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "D" => {"a new chain complex ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", ".  The signs of the
	       differentials are reversed if ", TT "i", " is odd."}
	  },
     EXAMPLE lines ///
     R = QQ[x..z];
     C = res coker vars R
     C[3]
     ///
     }

document {
     Key => (symbol SPACE, ChainComplexMap, Array),
     Headline => "degree shift",
     Usage => "g = f[i]",
     Inputs => {
	  "f" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "g" => {"a new chain complex map ", TT "g", " in which ", TT "g_j", " is ", TT "f_(i+j)", "."}
	  },
     EXAMPLE lines ///
     R = QQ[x..z];
     C = res coker vars R;
     f = id_C
     f[3]
     ///
     }

document {
     Key => (symbol SPACE, GradedModule, Array),
     Headline => "degree shift",
     TT "C[i]", " -- shifts the graded module ", TT "C", ", producing a new graded module
     ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", "."
     }

document {
     Key => (dual, ChainComplex),
     Headline => "dual",
     TT "dual C", " -- the dual of a chain complex."
     }





TEST "
R = ZZ/101[a .. d,Degrees=>{1,2,3,5}]
f = vars R
C = resolution cokernel f
assert(regularity C === 7)
M = kernel f
assert( numgens source M.generators === 6 )
assert( kernel presentation kernel f === kernel presentation kernel f )

g = map(cokernel f, target f, id_(target f))
N = kernel g
assert( numgens source N.generators === 4 )
assert( kernel g == image f )
W = kernel f ++ cokernel f
P = poincare W
assert( P == poincare kernel f + poincare cokernel f )
assert( P == poincare prune W )
"

document {
     Key => syzygyScheme,
     Headline => "construct a syzygy scheme",
     TT "syzygyScheme(C,i,v)", " -- produce the syzygy scheme from a map
     ", TT "v : R^j ---> C_i", " which selects some syzygies from a resolution ", TT "C", "."
     }

document {
     Key => (sum, ChainComplex),
     Headline => "direct sum of the components of a chain complex",
     TT "sum C", " -- yields the sum of the modules in a chain complex.",
     PARA{},
     "The degrees of the components are preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "C = res coker vars R",
      	  "sum C",
      	  "degrees oo",
	  },
     SeeAlso => {"sum",(sum, ChainComplexMap)}
     }

document {
     Key => (sum, ChainComplexMap),
     Headline => "direct sum of the components of a chain map",
     TT "sum C", " -- yields the sum of the modules in a chain complex map.",
     PARA{},
     "The degrees of the components are preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "C = res coker vars R",
      	  "sum C.dd",
      	  "betti oo",
	  },
     SeeAlso => {"sum", (sum, ChainComplex)}
     }

document {
     Key => (NewMethod, ChainComplex),
     Headline => "make a new chain complex from scratch",
     TT "C = new ChainComplex", " -- make a new chain complex.",
     PARA{},
     "The new chain complex is initialized with a differential of
     degree ", TT "-1", " accessible as ", TT "C.dd", " and of type
     ", TO "ChainComplexMap", ".  You can take the new chain complex and
     fill in the ring, the modules, and the differentials.",
     EXAMPLE {
	  "C = new ChainComplex;",
      	  "C.ring = ZZ;",
      	  "C#2 = ZZ^1;",
      	  "C#3 = ZZ^2;",
      	  "C.dd#3 = matrix {{3,-11}};",
      	  "C",
      	  "C.dd"
	  },
     }

document {
     Key => {(symbol **, ChainComplex, ChainComplex), (symbol**, ChainComplex, Module),(symbol**, Module, ChainComplex)},
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of two chain complexes.",
     PARA{},
     "The result, ", TT "E", ", is a chain complex.  Each module ", TT "E_k", " 
     in it is a direct sum of modules of the form ", TT "C_i**D_j", " with
     ", TT "i+j=k", ", and the preferred keys for the components of this direct
     sum are the pairs ", TT "(i,j)", ", sorted increasing values of ", TT "i", ".  For
     information about how to use preferred keys, see ", TO "directSum", "."
     }

document {
     Key => (symbol **, ChainComplex, GradedModule),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a chain complex with a graded module.",
     PARA{},
     "The result is a chain complex."
     }

document {
     Key => (symbol **, GradedModule, ChainComplex),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a graded module with a chain complex.",
     PARA{},
     "The result is a chain complex."
     }

document {
     Key => {(symbol **, ChainComplexMap, ChainComplex),(symbol**, ChainComplexMap, Module)},
     Headline => "tensor product",
     TT "f ** C", " -- tensor product of a map of chain complexes with a chain complex.",
     PARA{},
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => {(symbol **, ChainComplex, ChainComplexMap),(symbol**, Module, ChainComplexMap)},
     Headline => "tensor product",
     TT "C ** f", " -- tensor product of a chain complex with a map of chain complexes.",
     PARA{},
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (symbol **,ChainComplex,Ring),
     Usage => "C ** R",
     Inputs => {"C","R"},
     Outputs => {{"the tensor product of ", TT "C", " with ", TT "R" }},
     EXAMPLE lines ///
     R = QQ[a..d];
     C = res coker vars R
     S = R[x]
     C**S
     ///
     }

document {
     Key => (symbol **, ChainComplexMap, ChainComplexMap),
     Headline => "tensor product",
     TT "f ** g", " -- tensor product of two maps of chain complexes.",
     SeeAlso => "ChainComplexMap"
     }

TEST ///
     -- here we test the commutativity of the pentagon of associativities!
     C = QQ^1[0] ++ QQ^1[-1]
     assert(
	  (tensorAssociativity(C,C,C) ** C) * tensorAssociativity(C,C**C,C) * (C ** tensorAssociativity(C,C,C))
	  ==
	  tensorAssociativity(C**C,C,C) * tensorAssociativity(C,C,C**C)
	  )
     ///

document {
     Key => {tensorAssociativity,
	  (tensorAssociativity, Module, Module, Module),
	  (tensorAssociativity, ChainComplex, ChainComplex, ChainComplex),
	  (tensorAssociativity, GradedModule, GradedModule, GradedModule)},
     Headline => "associativity isomorphisms for tensor products",
     TT "tensorAssociativity(A,B,C)", " -- produces the isomorphism from
     A**(B**C) to (A**B)**C.",
     PARA{},
     "Currently implemented for modules, graded modules, and chain complexes.",
     SeeAlso => {"ChainComplex", "Module"}
     }

document {
     Key => (symbol SPACE, Module, Array),
     Headline => "make a chain complex from a module",
     TT "M[n]", " -- create a chain complex with the module M concentrated
     in degree -n.",
     PARA{},
     SeeAlso => "ChainComplex"
     }


document {
     Key => (dual,ChainComplexMap),
     Headline => "dual of a chain complex",
     Usage => "dual C",
     Inputs => { "C" },
     Outputs => { { "the dual of the chain complex ", TT "C", "" } },
     EXAMPLE {
	  "R = QQ[a..f]",
	  "M = coker genericMatrix(R,a,2,3)",
	  "res M",
	  "dual oo"
	  }
     }



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
