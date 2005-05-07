--		Copyright 1993-1999 by Daniel R. Grayson


document {
     Key => pushForward,
     TT "pushForward(f,M)", " -- yields an R-presentation of the S-module ", TT "M", ", where
     ", TT "f:R --> S", " is a ring map, and ", TT "M", " is considered as 
     an ", TT "R", "-module via ", TT "f", ".",
     PARA,
     "If ", TT "M", " is not finitely generated over ", TT "R", ", then an error is raised.",
     PARA,
     "Currently, ", TT "R", " and ", TT "S", " must both be polynomial rings over the same base field."
     }

document {
     Key => [pushForward,StopBeforeComputation],
     TT "StopBeforeComputation => false", " -- an optional argument used with
     ", TO "pushForward", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", "."
     }

document {
     Key => [pushForward,StopWithMinimalGenerators],
     TT "StopWithMinimalGenerators => true", " -- an option for ", TO "pushForward", "
     that specifies that the computation should stop as soon as a
     complete list of minimal generators for the submodule or ideal has been
     determined.",
     PARA,
     "The value provided is simply passed on to ", TO "gb", ": see 
     ", TO [gb,StopWithMinimalGenerators], " for details."
     }

document {
     Key => [pushForward,Strategy],
     TT "pushForward(f,M,Strategy => v)", " -- an option for ", TO pushForward, " 
     which can be used to specify the strategy to be used in the computation.",
     PARA,
     "The strategy option value ", TT "v", " should be one of the following.",
     UL {
	  TO "NonLinear",
     	  TO "Linear"
	  }
     }

TEST "
    R = ZZ/101[a,b]
    S = ZZ/101[a,b,c]
    M = cokernel matrix{{c^3}}
    f = map(S,R)
    assert( R^{0,-1,-2} == pushForward(f,M) )
"

document {
     Key => UseHilbertFunction,
     TT "UseHilbertFunction", " -- an option for ", TO "pushForward1", "."
     }

document {
     Key => (Ext,Module,Module),
     Usage => "Ext(M,N)",
     Inputs => {
	  "M" => null,
	  "N" => null
	  },
     Outputs => {
	  { "the Ext module of ", TT "M", " and ", TT "N", ",
	       as a bigraded module, with the modules ", TT {"Ext", SUP "i", "(M,N)"}, "
	       for all values of ", TT "i", " appearing simultaneously." }
	  },
     "The modules ", TT "M", " and ", TT "N", " should be graded (homogeneous) modules 
     over the same ring.",
     PARA,
     "The computation of the total Ext module is possible for modules over the
     ring ", TT "R", " of a complete intersection, according the algorithm
     of Shamash-Eisenbud-Avramov-Buchweitz.  The result is provided as a finitely
     presented module over a new ring with one additional variable of degree
     ", TT "{-2,-d}", " for each equation of degree ", TT "d", " defining ", TT "R", ".  The 
     variables in this new ring have degree length 2, i.e., is bigraded, with the
     degree ", TT "i", " part of ", TT "Ext^n(M,N)", " appearing as the degree
     ", TT "{-n,i}", " part of ", TT "Ext(M,N)", ".  We illustrate this in 
     the following example.",
     EXAMPLE {
	  "R = QQ[x,y]/(x^3,y^2);",
      	  "N = cokernel matrix {{x^2, x*y}}",
      	  "H = Ext(N,N);",
	  "ring H",
	  "S = ring H;",
	  "H",
	  "isHomogeneous H",
      	  "rank source basis( {-2,-3}, H)",
      	  "rank source basis( {-3}, Ext^2(N,N) )",
	  },
     PARA,
     "The result of the computation is cached for future reference.",
     SeeAlso =>{"ScriptedFunctor", "Adjust", "Repair"}
     }

document {
     Key => (Ext,ZZ,Module,Module),
     Usage => "Ext^i(M,N)",
     Inputs => {
	  "i" => null,
	  "M" => null,
	  "N" => null
	  },
     Outputs => {
	  { "the ", TT "Ext", " module of ", TT "M", " and ", TT "N" }
	  },
     "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way."
     }

document {
     Key => Ext,
     Headline => "compute an Ext module"
     }

document {
     Key => Adjust,
     Headline => "adjust the multi-degree",
     TT "Adjust", " -- an option used when creating a polynomial ring
     to specify a linear function for transforming multi-degrees of monomials 
     into multi-degrees whose first component is positive, for internal use.",
     PARA,
     "This facility is used in particular by ", TT "(Ext,Module,Module)", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y, Degrees => {-1,-2}, 
	  Repair => d -> -d,
	  Adjust => d -> -d];",
     	  ///degree \ gens R///,
	  "transpose vars R"
     	  },
     SeeAlso => { "Repair" }
     }

document {
     Key => Repair,
     Headline => "repair the multi-degree",
     TT "Adjust", " -- an option used when creating a polynomial ring
     to specify a linear function for transforming internally used multi-degrees
     (whose first component is positive) of monomials into the original externally 
     used multi-degrees.",
     PARA,
     "This facility is used in particular by ", TT "(Ext,Module,Module)", ".",
     SeeAlso => { "Adjust" }
     }

document {
     Key => dd,
     Headline => "differential in a chain complex",
     TT "dd", " -- a symbol used as a key in a chain complex, under which
     are stored the differentials.",
     PARA,
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
     Headline => "compute a Tor module",
     TT "Tor_i(M,N)", " -- compute the Tor module of two modules M, N.",
     SeeAlso =>{"ScriptedFunctor"}
     }

document {
     Key => MonomialIdeal, 
     Headline => "the class of all monomial ideals handled by the engine",
     SeeAlso => "engine" 
     }

--document { (complement, MonomialIdeal),
--					  -- compute { m^c : m minimal generator of I }, where
--					  -- m^c is the product of the variables 
--					  -- not in the support of m.",
--     TT "complement I", " -- computes the complement of a monomial ideal.",
--     PARA,
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
--document { (symbol -,MonomialIdeal,MonomialIdeal),
--					  -- compute { m minimal generator of I : m not a minimal gen of J }
--     SeeAlso => MonomialIdeal
--     }


document {
     Key => (dual,MonomialIdeal,List),
     Headline => "the Alexander dual",
     Usage => "dual(I,e)",
     Inputs => {
	  "I" => {"a monomial ideal"},
	  "e" => {"a list of exponents"}
	  },
     Outputs => { {"the Alexander dual of ", TT "I", " formed with respect to the monomial whose exponents are listed in ", TT "e" } },
     "The monomial corresponding to ", TT "e", " should be divisible by all of the minimal generators of ", TT "I", ".",
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
     PARA,
     "For a general monomial ideal, the Alexander dual 
     defined as follows:  Given two list of nonnegative 
     integers ", TT "a", " and ", TT "b", "for which ", 
     TT "a_i >= b_i", " for all ", TT "i", " let ", 
     TT "a\b", " denote the list whose ", TT "i", "-th 
     entry is ", TT "a_i+1-b_i", "if ", TT "b_i >= 1", 
     "and ", TT "0", "otherwise.  The Alexander dual with 
     respect to ", TT "a", " is the ideal generated by 
     a monomial ", TT "x^a\b", " for each irreducible 
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
     PARA,
     "One always has ", TT "dual( dual(I, a), a) == I", 
     " however ", TT "dual dual I", "may not equal ", 
     TT "I", ".",
     EXAMPLE {
	  "QQ[x,y,z];",
	  "J = monomialIdeal( x^3*y^2, x*y^4, x*z, y^2*z)",
	  "dual dual J", 
	  "dual( dual(J, {3,4,1}), {3,4,1})"
	  },
     PARA,
     "See Ezra Miller's Ph.D. thesis 'Resolutions and 
     Duality for Monomial Ideals'.",
     PARA,
     "Implemented by Greg Smith."
     }
document {
     Key => (monomialSubideal, Ideal),
     Headline => "find the largest monomial ideal in an ideal",
     Usage => "monomialSubideal I",
     Inputs => {
	  "I" => {"an ", TT "Ideal", "."}
	  },
     Outputs => {
	  {"the largest monomial ideal contained in ", TT "I"}
	  },
     EXAMPLE {
	  "QQ[a,b,c,d];",
	  "I = ideal(b*c, c^2 - b*d, -a*c+b^2)",
	  "monomialSubideal I"
	  },
     PARA "Implemented by Greg Smith"
     }

document {
     Key => monomialIdeal,
     Headline => "make a monomial ideal"
     }

document {
     Key => (monomialIdeal, Ideal),
     Usage => "monomialIdeal J",
     Inputs => {
	  "J" => "an ideal"
	  },
     Outputs => { { "the monomial ideal generated by the lead terms of a Groebner basis of ", TT "J" } }
     }

document {
     Key => (monomialIdeal, List),
     Usage => "monomialIdeal {f,g,h,...}",
     Inputs => {
	  "{f,g,h,...}" => "a list of ring elements"
	  },
     Outputs => { { "the monomial ideal generated by the ring elements listed." } }
     }

document {
     Key => (monomialIdeal, Sequence),
     Usage => "monomialIdeal(f,g,h,...)",
     Inputs => { "(f,g,h,...)" => "a sequence of ring elements" },
     Outputs => { { "the monomial ideal generated by the ring elements listed." } }
     }

document {
     Key => (monomialIdeal, Matrix),
     Usage => "monomialIdeal f",
     Inputs => { "f" => "a 1 by n matrix" },
     Outputs => { { "the monomial ideal generated by the lead terms of ", TT "f" } },
     EXAMPLE {
	  "R=ZZ/101[a,b,c]",
      	  "m = monomialIdeal vars R",
      	  "m^2",
      	  "m^3"
	  }
     }

document {
     Key => (monomialIdeal, MonomialIdeal),
     TT "monomialIdeal I", " -- create a copy of a monomial ideal I."
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
     Key => intersect,
     Headline => "compute an intersection",
     TT "intersect(M,N,...)", " -- calculate the intersection of 
     submodules of the same free module, or of monomial ideals in the same ring."
     }

document {
     Key => isBorel,
     Headline => "whether an ideal is fixed by upper triangular changes of coordinates"
     }

document {
     Key => minprimes,
     Headline => "compute the minimal primes",
     TT "minprimes I", " -- compute the minimal primes of a monomial ideal I.",
     PARA,
     "Not working yet."
     }

TEST "
    R = ZZ/101[a..d]
    f = matrix{{a^3 + b^3 + c^3 + d^3 + (a+b+c)^3}}
    fdual = fromDual f
    assert(f - toDual(4, fdual) == 0)
"

document {
     Key => fromDual,
     TT "fromDual f", " -- given a 1 by n matrix ", TT "f", " over a polynomial
     ring ", TT "R", ", computes ", TT "g", " so that ", TT "Hom(R/image g, E)", "
     corresponds to ", TT "f", ", where ", TT "E", " is the injective envelope 
     of the coefficient field of ", TT "R", ".",
     PARA,
     "This function mimics the script ", TT "<l_from_dual", " of Macaulay, and
     is probably going to be changed substantially.",
     SeeAlso => "toDual"
     }

document {
     Key => toDual,
     TT "toDual(d,f)", " -- given a 1 by n matrix ", TT "f", " over a polynomial
     ring R and an integer d such that the d-th power of each variable is in
     the image of ", TT "f", ", computes ", TT "Hom(R/image f, E)", ", where 
     ",  TT "E", " is the injective envelope of the coefficient field of R.",
     PARA,
     "This function mimics the script ", TT "<l_to_dual", " of Macaulay, and
     is probably going to be changed substantially.",
     SeeAlso => "fromDual"
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
     Key => gradedModule,
     Headline => "make a graded module",
     TT "gradedModule", " -- a method for creating graded modules."
     }

document {
     Key => gradedModuleMap,
     Headline => "make a map of graded modules",
     TT "gradedModuleMap", " -- a method for creating maps of graded modules."
     }

document {
     Key => coimage,
     Headline => "coimage of a map",
     TT "coimage f", " -- coimage of a map.",
     PARA,
     "The coimage of a map differs slightly from the image, in that the
     coimage is a quotient module of the source of the map, but the image
     is a submodule of the target of the map.  The image and the coimage
     are isomorphic modules.",
     SeeAlso => "GradedModule"
     }

document {
     Key => ChainComplex,
     Headline => "the class of all chain complexes",
     SeeAlso => "free resolutions, chain complexes, and homological algebra"
     }

document {
     Key => (complete, ChainComplex),
     Headline => "complete the internal parts",
     TT "complete C", " -- fills in the modules of a chain complex
     obtained as a resolution with information from the engine.",
     PARA,
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
     Key => (symbol _, ChainComplex, ZZ),
     Headline => "get component of chain complex",
     Usage => "C_i",
     Inputs => { "C" =>"" , "i" => "" },
     Outputs => {{"the ", TT "i", "-th module of the chain complex ", TT "C"}},
     EXAMPLE {
	  "R = ZZ[x..z]",
	  "C = res coker vars R",
	  "C_2",
	  "rank C_2",
	  "degrees C_2"
	  },
     SUBSECTION "Programming hint",
	  "The chain complex ", TT "C", " is implemented as a hash table, but since the computation of a projective resolution can be stopped prematurely, Macaulay 2 doesn't bother
	  populating the hash table with the relevant free modules until explicitly requested by the user, for example, in response to the
	  command ", TT "C_i", " described above.  The hash table ", TT "C", " can be examined directly with code like ", TT "C#i", ", but in order to populate 
	  the hash table completely, use ", TO (complete, ChainComplex), ".",
     SeeAlso => {res, coker, vars, rank, degrees}
     }

document {
     Key => (length, ChainComplex),
     Headline => "length of a chain complex or graded module",
     TT "length C", " -- the length of a chain complex.",
     PARA,
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

document {
     Key => (symbol _, ChainComplexMap, ZZ),
     Headline => "get component of chain map",
     Usage => "p_i",
     Inputs => {
	  "p" => ("a map ", TT "D", " <- ", TT "C", " of chain complexes, of degree ", TT "d", ", say"),
	  "i" => ()
	  },
     Outputs => { ("the component ", TT "D_(i+d) <- C_i" ) },
     EXAMPLE {
	  "R = ZZ/101[a..c]",
	  "I = image vars R",
	  "J = image symmetricPower (2,vars R)",
	  "g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))",
	  "g_1",
	  "g_2"
	  },
     SeeAlso => { (symbol _, ChainComplex, ZZ), extend, resolution, image, vars, symmetricPower }
     }

document { Key => extend, Headline => "extend a module map to a chain map, if possible" }
document {
     Key => (extend,ChainComplex,ChainComplex,Matrix),
     Usage => "extend(D,C,f0)",
     Inputs => {
	  "D" => "",
	  "C" => "",
	  "f0" => { "a map from ", TT "C_0", " to ", TT "D_0" },
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
     SeeAlso => {(symbol "_",ChainComplex,ZZ),(symbol "_",ChainComplexMap,ZZ),(cone,ChainComplexMap) }
     }

TEST "
R = ZZ/101[a..c]
I = image vars R
J = image symmetricPower (2,vars R)
g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))
E = cone g
"

document { Key => cone, Headline => "mapping cone of a chain map" }
document {
     Key => (cone,ChainComplexMap),
     Usage => "cone f",
     Inputs => { "f"=>"" },
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
     Key => nullhomotopy,
     Headline => "make a null homotopy",
     TT "nullhomotopy f", " -- produce a nullhomotopy for a map f of 
     chain complexes.",
     PARA, 
     "Whether f is null homotopic is not checked.",
     PARA,
     "Here is part of an example provided by Luchezar Avramov.  We
     construct a random module over a complete intersection, resolve 
     it over the polynomial ring, and produce a null homotopy for the
     map which is multiplication by one of the defining equations
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

document {
     Key => (homology,ZZ,ChainComplex),
     Headline => "homology of a chain complex",
     TT "HH_i C", " -- homology at the i-th spot of the chain complex ", TT "C", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})",
      	  "M = HH_1 C",
      	  "prune M",
	  },
     }

TEST "
S = ZZ/101[x,y,z]
M = cokernel vars S
assert ( 0 == HH_-1 res M )
assert ( M == HH_0 res M )
assert ( 0 == HH_1 res M )
assert ( 0 == HH_2 res M )
assert ( 0 == HH_3 res M )
assert ( 0 == HH_4 res M )
"

document {
     Key => quotient,
     Headline => "ideal or submodule quotient",
     Usage => "quotient(I,J) or I:J",
     Inputs => {
	  "I" => "an ideal or submodule",
	  "J" => "an ideal, ring element or submodule"
	  },
     Outputs => {
	  {"the ideal or submodule I:J.  See the appropriate 
	       subsection for the definition of I:J."}
	  },
     "Groebner bases will be computed as needed.",
     PARA,
     "The colon operator ", TO (symbol :), " may be used as an abbreviation 
     of ", TT "quotient", " if no options need to be supplied.",
     PARA,
     "If the second input ", TT "J", " is a ring element ", TT "f", 
     ", then the principal ideal generated by ", TT "f", " is used.",
     PARA,
     "The computation is not stored anywhere yet, BUT, it will soon be stored
     under I.cache.QuotientComputation{J}, or I.QuotientComputation{J},
     so that the computation can be restarted after an interrupt.",
     SeeAlso => {saturate},
     Subnodes => {
	  TO (quotient,Ideal,Ideal),
	  TO (quotient,Module,Ideal),
	  TO (quotient,Module,Module)
	  }
     }

document {
     Key => (symbol :, Ideal, Ideal),
     Headline => "ideal quotient",
     TT "I:J", " -- computes the ideal quotient.",
     PARA,
     "The notation ", TT "I:J", " is equivalent to ", TT "quotient(I,J)", ",
     although with the latter form, optional arguments can be provided.
     See ", TO quotient, " for further details."
     }

///
-- old, replaced documentation
document {
     Key => quotient,
     Headline => "ideal or submodule quotient",
     TT "quotient(I,J)", " -- computes the ideal or submodule quotient ", TT "(I:J)", ".", 
     PARA,
     "The arguments should be ideals in the same ring, or submodules of the same
     module.  If ", TT "J", " is a ring element, then the principal ideal 
     generated by ", TT "J", " is used.",
     PARA,
     "The operator ", TO ":", " can be used as an abbreviation, but without optional
     arguments; see ", TO (symbol :, Module, Module), ".",
     PARA,
     "For ideals, the quotient is the set of ring elements r such that rJ is
     contained in I.  If I is a submodule of a module M, and J is an ideal,
     the quotient is the set of elements m of M such that Jm is contained in
     I.  Finally, if I and J are submodules of the same module M, then the
     result is the set of ring elements r such that rJ is contained in I.",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
      	  "J = monomialCurveIdeal(R,{1,4,7})",
      	  "I = ideal(J_1-a^2*J_0,J_2-d*c*J_0)",
      	  "I : J",
	  },
     PARA,
     "The computation is currently not stored anywhere: this means
     that the computation cannot be continued after an interrupt.
     This will be changed in a later version."
     }
///

document {
     Key => [quotient,Strategy],
     TT "quotient(I,J,Strategy => v)", " -- an option which can
     be used to specify the strategy to be used in the computation.",
     PARA,
     "The strategy option value ", TT "v", " should be one of the following.",
     UL {
	  TO "Iterate",
          TO "Linear"
          }
     }

TEST "
  -- quotient(Ideal,Ideal)
  -- quotient(Ideal,RingElement)
  -- options to test: DegreeLimit, BasisElementLimit, PairLimit, 
  --    MinimalGenerators,
  --    Strategy=>Iterate, Strategy=>Linear
  R = ZZ/101[a..d]
  I1 = monomialCurveIdeal(R, {1,3,7})
  I2 = ideal((gens I1)_{0,1})
  I3 = quotient(I2,I1)
  I4 = quotient(I2,I3)
  I5 = quotient(I2, c)

  assert(I2 == 
       intersect(I3,I4)
       )
  
  assert(ideal(c,d) ==
       quotient(I2, I5)
       )

  assert(I3 ==
       I2 : I1
       )

--  assert(ideal(d) + I2 ==
--       quotient(I2,I1,DegreeLimit=>1)
--       )

  assert(I3 ==
       quotient(I2,I1,Strategy=>Iterate)
       )
  
  quotient(I2,I1,MinimalGenerators=>false)
--  stderr << \"  -- this fails currently\" << endl
--  assert(I5 ==
--       quotient(I2, c,Strategy=>Linear)
--       )
  
"

TEST "
  -- quotient(Ideal,Ideal)
  -- quotient(Ideal,RingElement)
  -- options to test: DegreeLimit, BasisElementLimit, PairLimit, 
  --    MinimalGenerators,
  --    Strategy=>Iterate, Strategy=>Linear
  R = ZZ/101[vars(0..3)]/(a*d)
  I1 = ideal(a^3, b*d)
  I2 = ideal(I1_0)

  I3 = quotient(I2,I1)
  assert(I3 == ideal(a))
  I4 = quotient(I2,I3)
  assert(I4 == ideal(a^2,d))
  I5 = quotient(I2, d)
  assert(I5 == ideal(a))
"

TEST "
  --    quotient(Module,RingElement)
  --    quotient(Module,Ideal)
  
  -- This tests 'quotmod0' (default)
  R = ZZ/101[vars(0..4)]/e
  m = matrix{{a,c},{b,d}}
  M = subquotient(m_{0}, a^2**m_{0} | a*b**m_{1})
  J = ideal(a)
  Q1 = quotient(M,J)
  
  -- Now try the iterative version
  Q2 = quotient(M,J,Strategy=>Iterate)
  assert(Q1 == Q2)

  m = gens M  
  F = target m
  mm = generators M | relations M
  j = transpose gens J
  g = (j ** F) | (target j ** mm)
  h = syz gb(g, 
	  Strategy=>LongPolynomial,
	  SyzygyRows=>numgens F,
	  Syzygies=>true)
  trim subquotient(h % M.relations, 
             M.relations)
  
"

TEST "
  --    quotient(Module,Module)
  R = ZZ/101[a..d]
  M = image matrix{{a,b},{c,d}}
  N = super M
  I = quotient(M,N)
  assert(I ==
            quotient(M,N,Strategy=>Iterative)
	)
   
  assert(I == 
            M : N
	)
  assert(I ==
            ann(N/M)
	)
"

TEST "
  --    quotient(Module,Module)
  R = ZZ/101[vars(0..14)]
  N = coker genericMatrix(R,a,3,5)
  M = image N_{}
  I = quotient(M,N)
  assert(I ==
            quotient(M,N,Strategy=>Iterative)
	)
   
  assert(I == 
            M : N
	)
  assert(I ==
            ann(N/M)
	)
"

TEST "
  R = ZZ/101[a..d]
  M = coker matrix{{a,b},{c,d}}
  m1 = basis(2,M)
  image m1
  M1 = subquotient(matrix m1, relations M)
  Q1 = M1 : a  
  Q2 = quotient(M1,ideal(a,b,c,d),Strategy=>Iterate)
  assert(Q1 == Q2)
"

TEST "
  R = ZZ/101[a..d]
  mrels = matrix{{a,b},{c,d}}
  mgens = matrix(R,{{1,0},{0,0}})
  M = trim subquotient(mgens, mrels)
  Q1 = quotient(image M_{},a*d-b*c)
  assert(Q1 == super M)  -- fails: bug in == ...
"

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
     assert( z*w == 23  - 2*ii )
     assert( z/w == -7/41 + -22/41 * ii )
     assert( 1 + z == 3 - 3*ii )
     assert( 2*w == 8 + 10*ii )
     assert( z + w == 6 + 2*ii )
     assert( toString w == "4.+5.*ii" )
     assert( conjugate z == 2 + 3*ii )
     assert( x == 2 )
     assert( x == 2. )
     assert( x == 2/1 )
     assert( net ( 2 - 3 * ii ) === "2. - 3.ii"^0 )
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

document {
     Key => MinimalGenerators,
     TT "MinimalGenerators => true", " -- an option for certain functions
     which specifies whether to compute minimal generators for the result.",
     PARA,
     UL {
	  TO [quotient,MinimalGenerators],
	  TO [saturate,MinimalGenerators],
	  },
     PARA,
     "The default value is ", TT "true", "."
     }

document {
     Key => [quotient,MinimalGenerators],
     TT "MinimalGenerators => true", " -- an option for ", TO "quotient", "
     which specifies whether to compute minimal generators for the result.",
     PARA,
     "The default value is ", TT "true", "."
     }

document {
     Key => [saturate,MinimalGenerators],
     TT "MinimalGenerators => true", " -- an option for ", TO "saturate", "
     which specifies whether to compute minimal generators for the result.",
     PARA,
     "The default value is ", TT "true", "."
     }

document {
     Key => Bayer,
     Headline => "a Strategy option value for saturate",
     SeeAlso => {[saturate,Strategy]}
     }

document {
     Key => Iterate,
     Headline => "a Strategy option value for saturate",
     SeeAlso => {[saturate,Strategy]}
     }

document {
     Key => Linear,
     Headline => "a Strategy option value for saturate",
     SeeAlso => {[saturate,Strategy]}
     }
     

TEST "
-- The ideal case
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
F = I_0
J = ideal(F*I_1, I_2, F^2*I_3)
saturate(J,F)

J = truncate(4,I)
time saturate(J,a,Strategy=>Bayer)
time saturate(J,a,Strategy=>Linear)
time saturate(J,a,Strategy=>Iterate)
time saturate(J,a,Strategy=>Default)
time saturate(J,a,Strategy=>Eliminate)

time saturate(J,Strategy=>Default)
time saturate(J,Strategy=>Iterate)
assert(try saturate(J,Strategy=>Bayer) else true)
assert(try saturate(J,Strategy=>Linear) else true)
assert(try saturate(J,Strategy=>Eliminate) else true)
"

TEST "
-- The module case
R = ZZ/101[a..d]
M = subquotient(matrix{{a^2,b^2},{a*d,c^2}}, matrix{{c},{d}})

time saturate(M,a)
time saturate(M,a,Strategy=>Bayer)
time saturate(M,a,Strategy=>Linear)
time saturate(M,a,Strategy=>Eliminate)
time saturate(M,a,Strategy=>Iterate)
"

TEST "
R = ZZ/101[x,y,z,a,b,c,d]
S = ZZ/101[x,y,z]
row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
row1 = matrix{{a,b,c,d}}
J = minors(2,row1 || row2)
-- gbTrace = 3
  -- best time so far for the following: 30.41 seconds
  -- but this doesn't yet include finding a minimal set
  -- of generators for the image
time saturate(J, ideal row2)
time saturate(J, ideal row2, Strategy=>Iterate)
time saturate(J, ideal row2, MinimalGenerators=>false)
  
  -- the time for the following is 40.58 seconds...
  -- but I think too many GB's are being done...
time (
  J1 = quotient(J, ideal row2);
  J2 = quotient(J1, ideal row2);
  J3 = quotient(J2, ideal row2);
  J4 = quotient(J3, ideal row2);
  )
"

TEST "
R = ZZ/101[x,y,z,a,b,c,d]
--R = ZZ/101[a,b,c,d,x,y,z]  This order is VERY BAD!!
--R = ZZ/101[x,y,z,a,b,c,d,MonomialOrder=>ProductOrder{3,4}]
S = ZZ/101[x,y,z]
row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
row1 = matrix{{a,b,c,d}}
J = minors(2,row1 || row2)
-- gbTrace = 3
F = row2_(0,0)
  -- For this example, just saturate w.r.t. F.
  -- best time: 21.76 seconds
time saturate(J, F)
time saturate(J, F, Strategy=>Bayer)  -- 21.76
time saturate(J, F, Strategy=>Eliminate) -- 26.08
"

TEST "
R = ZZ/101[a..f]
m = monomialCurveIdeal(R,{1,3,4})
I = ideal(d-c) + m 
saturate(I,a+b)
I
"

TEST ///
	R = ZZ/101[a..f]
	I = ideal (d^2, d*f, f^2)
	J = ideal (d,f)
	assert( saturate(I,J) == R )
///

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
  -- top m
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
     Key => (homology,ZZ,ChainComplexMap),
     Headline => "homology of a chain complex map",
     TT "HH_i f", " -- provides the map on the ", TT "i", "-th homology module
     by a map ", TT "f", " of chain complexes.",
     PARA,
     SeeAlso => {"homology", "HH"}
     }

document {
     Key => (homology,ChainComplex),
     Headline => "homology of a chain complex",
     TT "HH C", " -- produces the direct sum of the homology modules of the
     chain complex ", TT "C", " as a graded module.",
     PARA,
     SeeAlso => {"GradedModule", "HH"}
     }

document {
     Key => chainComplex,
     Headline => "make a chain complex",
     TT "chainComplex", " -- a method for creating chain complexes.",
     }

document {
     Key => (chainComplex, Matrix),
     Headline => "make a small chain complex",
     TT "chainComplex f", " -- create a chain complex ", TT "C", " with
     the map ", TT "f", " serving as the differential ", TT "C.dd_1", "."
     }

document {
     Key => (chainComplex, Sequence),
     Headline => "make a chain complex",
     TT "chainComplex(f,g,h,...)", " -- create a chain complex ", TT "C", " whose
     differentials are the maps ", TT "f", ", ", TT "g", ", ", TT "h", ".",
     PARA,
     "The map ", TT "f", " appears as ", TT "C.dd_1", ", the map ", TT "g", " appears
     as ", TT "C.dd_2", ", and so on."
     }

document {
     Key => (symbol ++,ChainComplex,ChainComplex),
     Headline => "direct sum",
     TT "C++D", " -- direct sum of chain complexes.",
     PARA,
     EXAMPLE {
	  "C = resolution cokernel matrix {{4,5}}",
      	  "C ++ C[-2]"
	  },
     }

document {
     Key => (components, ChainComplex),
     TT "components C", " -- returns from which C was formed, if C
     was formed as a direct sum.",
     PARA,
     SeeAlso => "isDirectSum"
     }

document {
     Key => (symbol " ", ChainComplex, Array),
     Headline => "degree shift",
     Usage => "D = C[i]",
     Inputs => {
	  "C" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "D" => {"a new chain complex ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", ".  The signs of the
	       differentials are reversed if ", TT "i", " is odd."}
	  }
     }

document {
     Key => (symbol " ", GradedModule, Array),
     Headline => "degree shift",
     TT "C[i]", " -- shifts the graded module ", TT "C", ", producing a new graded module
     ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", "."
     }

document {
     Key => (Hom,ChainComplex,Module),
     Headline => "Hom",
     TT "Hom(C,M)", " -- produces the Hom complex from a chain complex C and
     a module M."
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
     PARA,
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
     PARA,
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
     PARA,
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
     Key => (chainComplex,GradedModule),
     Headline => "make a chain complex from a graded module",
     TT "chainComplex M", " -- convert a graded module to a chain complex by
     installing the zero map as differential.",
     PARA,
     SeeAlso => {"GradedModule", "ChainComplex"}
     }

document {
     Key => (symbol **, ChainComplex, ChainComplex),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of two chain complexes.",
     PARA,
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
     PARA,
     "The result is a chain complex."
     }

document {
     Key => (symbol **, GradedModule, ChainComplex),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a graded module with a chain complex.",
     PARA,
     "The result is a chain complex."
     }

document {
     Key => (symbol **, ChainComplexMap, ChainComplex),
     Headline => "tensor product",
     TT "f ** C", " -- tensor product of a map of chain complexes with a chain complex.",
     PARA,
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (symbol **, ChainComplex, ChainComplexMap),
     Headline => "tensor product",
     TT "C ** f", " -- tensor product of a chain complex with a map of chain complexes.",
     PARA,
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (symbol **, ChainComplexMap, ChainComplexMap),
     Headline => "tensor product",
     TT "f ** g", " -- tensor product of two maps of chain complexes.",
     PARA,
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (max,ChainComplex),
     Headline => "maximum index in a chain complex",
     TT "max C", " -- the maximum index occuring in a chain complex."
     }

document {
     Key => (min,ChainComplex),
     Headline => "minimum index in a chain complex",
     TT "min C", " -- the minimum index occuring in a chain complex."
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
     Key => tensorAssociativity,
     Headline => "associativity isomorphisms for tensor products",
     TT "tensorAssociativity(A,B,C)", " -- produces the isomorphism from
     A**(B**C) to (A**B)**C.",
     PARA,
     "Currently implemented for modules, graded modules, and chain complexes.",
     SeeAlso => {"ChainComplex", "Module"}
     }

document {
     Key => (symbol " ", Module, Array),
     Headline => "make a chain complex from a module",
     TT "M[n]", " -- create a chain complex with the module M concentrated
     in degree -n.",
     PARA,
     SeeAlso => "ChainComplex"
     }

document {
     Key => (map,ChainComplex,ChainComplex,Function),
     Headline => "make a map of chain complexes",
     TT "map(C,D,f)", " -- construct a map from the chain complex ", TT "D", " to the chain
     complex ", TT "C", " which in degree ", TT "k", " is the map provided
     as the value of ", TT "f(k)", ".",
     PARA,
     "As a convenience, the value returned by f(k) is used as the third argument
     in ", TT "map(C_k,D_k,f(k))", ".",
     PARA,
     "The function ", TT "f", " is called only for those indices which represent spots
     occupied in both the source and target chain complexes.",
     PARA,
     SeeAlso => "ChainComplex"
     }

document {
     Key => (dual,ChainComplexMap),
     Headline => "dual of a chain complex",
     Usage => "dual C",
     Inputs => { "C" => null },
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
