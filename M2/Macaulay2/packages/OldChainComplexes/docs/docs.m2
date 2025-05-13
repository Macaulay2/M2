-- TODO:
-- (End, ChainComplex)
-- (ambient, GradedModule)
-- (degree, ChainComplexMap)
-- (cokernel, ChainComplexMap),
-- (cokernel, GradedModuleMap),
-- (components, GradedModule),
-- (components, ChainComplexMap),
-- (components, GradedModuleMap),
-- (cover, GradedModule)
-- (isDirectSum, ChainComplex),
-- (isDirectSum, GradedModule),
-- (directSum, ChainComplex),
-- (directSum, GradedModule),
-- (formation, ChainComplex),
-- (formation, ChainComplexMap),
-- (formation, GradedModule),
-- (formation, GradedModuleMap)
-- (id, ChainComplex)
-- (image, ChainComplexMap),
-- (image, GradedModuleMap),
-- (tensorAssociativity, ChainComplex, ChainComplex, ChainComplex),
-- (tensorAssociativity, GradedModule, GradedModule, GradedModule)
-- (minimalPresentation, ChainComplex),
-- (minimalPresentation, ChainComplexMap),
-- (minimalPresentation, GradedModule),
-- (minimalPresentation, GradedModuleMap),
-- (prune, ChainComplex),
-- (prune, ChainComplexMap)
-- (prune, GradedModule),
-- (prune, GradedModuleMap),
-- (rank, GradedModule), -- and ChainComplex
-- (regularity, ChainComplex)
-- (super, GradedModule),
-- (symbol ++,GradedModule,Module)
-- (symbol ++,GradedModuleMap,GradedModuleMap),
-- (symbol ++,Module,GradedModule),
-- (symbol ^, Ring, BettiTally)

-- [resolution, ParallelizeByDegree] -- was same as ParallelizeByDegree

document {
    Key => (homology,ZZ,ChainComplexMap),
    Headline => "homology of a chain complex map",
    Usage => "HH_i f",
    Inputs => { "i", "f" },
    Outputs => { {"the map on the ", TT "i", "-th homology module induced by the map ", TT "f", " of chain complexes" } },
}

document {
    Key => (homology,ChainComplexMap),
    Headline => "homology of a chain complex map",
    Usage => "HH f",
    Inputs => { "i" },
    Outputs => { {"the map on the homology induced by the map ", TT "f", " of chain complexes" } },
}

document {
    Key => (homology,ChainComplex),
    Headline => "homology of a chain complex",
    Usage => "HH C",
    Inputs => { "C" },
    Outputs => { {"the homology of ", TT "C"} },
    EXAMPLE {
	"R = QQ[x]/x^5;",
	"C = res coker vars R",
	"M = HH C",
	"prune M",
    },
}

document {
    Key => (homology,ZZ,ChainComplex),
    Headline => "homology of a chain complex",
    Usage => "HH_i C",
    Inputs => { "i", "C" },
    Outputs => { {"the homology at the i-th spot of the chain complex ", TT "C", "."} },
    EXAMPLE {
	"R = ZZ/101[x,y]",
	"C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})",
	"M = HH_1 C",
	"prune M",
    },
}

document {
    Key => (cohomology,ZZ,ChainComplex),
    Headline => "cohomology of a chain complex",
    Usage => "HH^i C",
    Inputs => {"i"=> ZZ, "C" => ChainComplex},
    Outputs => {Module => {"HH^i C", " -- homology at the i-th spot of the chain complex ", TT "C", "."}},
    "By definition, this is the same as computing HH_(-i) C.",
    PARA{},
    EXAMPLE {
	"R = ZZ/101[x,y]",
	"C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})",
	"M = HH^1 C",
	"prune M"
    },
    PARA{},
    "Here is another example computing simplicial cohomology
    (for a hollow tetrahedron):",
    EXAMPLE {
	"needsPackage \"SimplicialComplexes\"",
	"R = QQ[a..d]",
	"D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}",
	"C = complex D",
	"HH_2 C",
	"prune oo"
    },
    SeeAlso => {"GradedModule", "HH"},
}

document {
    Key => (cohomology,ZZ,ChainComplexMap),
    Headline => "cohomology of a chain complex map",
    Usage => "HH^i f",
    Inputs => {"i","f"},
    Outputs => {Matrix=>{"the ", TT "i","-th cohomology map induced by the chain complex map ", TT "f"}},
    "The command provides the map on the ", TT "i", "-th cohomology module
    induced by a map ", TT "f", " of chain complexes.",
    SeeAlso => {"cohomology", "HH", "ChainComplex"}
}

doc ///
Node
  Key
   (Hom, Module, ChainComplex)
   (Hom, ChainComplex, Module)
   (Hom, Module, ChainComplexMap)
   (Hom, ChainComplexMap, Module)
  Headline
    the Hom functor
  Usage
    Hom(M, C)
    Hom(C, M)
  Inputs
    M:Module
    C:ChainComplex
    DegreeLimit => {ZZ,List}
      see @TO [Hom, DegreeLimit]@
    MinimalGenerators => Boolean
      see @TO [Hom, MinimalGenerators]@
    Strategy => Thing
      see @TO [Hom, Strategy]@
  Outputs
    :ChainComplex
      the chain complex whose $i$-th spot is $\mathrm{Hom}(M, C_i)$,
      in the first case, or $\mathrm{Hom}(C_(-i), M)$ in the second case.
  Description
    Example
      R = QQ[a..d];
      C = res coker vars R
      M = R^1/(a,b)
      C' = Hom(C,M)
      C'.dd_-1
      C'.dd^2 == 0
  Synopsis
    Heading
      induced map on Hom
    Usage
      Hom(f,M)
      Hom(M,f)
    Inputs
      f:ChainComplexMap
      M:Module
      DegreeLimit => {ZZ,List}
        see @TO [Hom, DegreeLimit]@
      MinimalGenerators => Boolean
        see @TO [Hom, MinimalGenerators]@
      Strategy => Thing
        see @TO [Hom, Strategy]@
    Outputs
       :ChainComplexMap
         the induced map on Hom
  Caveat
    See @TO "Complexes :: Hom(Complex,Complex)"@ for Hom of two chain complexes.
  SeeAlso
    resolution
///

document {
    Key => (source,ChainComplexMap),
    Headline => "find the source of a map of chain complexes",
    Usage => "source f",
    Inputs => {"f" => ChainComplexMap},
    Outputs => {{"the source chain complex of ", TT "f"}},
    "In the example below, we have a map between two modules and extend it to
    a map between projective resolutions of the two modules. Then ",
    TT "source",  " gives the source of the map of chain complexes.",
    EXAMPLE {
	"R = ZZ[x,y,z];",
	"M = R^1/(x,y,z);",
	"N = R^1/(x^2,y^2,x*y*z,z^2);",
	"g = map(N,M,x*y);",
	"f = res g;",
	"source f"
    },
    "(That was an expensive way of resolving ", TT "M", ".)",
}

document {
    Key => (source,GradedModuleMap),
    Headline => "find the source of a map of graded modules",
    Usage => "source f",
    Inputs => {"f"},
}

document {
    Key => (target, ChainComplexMap),
    Headline => "find the target of a map of chain complexes",
    Usage => "target f",
    Inputs => { "f" => ChainComplexMap },
    Outputs => { {"the target chain complex of ", TT "f"} },
    "In the example below, we have a map between two modules and extend it to
    a map between projective resolutions of the two modules. Then ",
    TT "target",  " gives the target of the map of chain complexes.",
    EXAMPLE {
	"R = ZZ[x,y,z];",
	"M = R^1/(x,y,z);",
	"N = R^1/(x^2,y^2,x*y*z,z^2);",
	"g = map(N,M,x*y);",
	"f = res g;",
	"target f"
    },
    "(That was an expensive way of resolving ", TT "N", ".)",
}

document {
    Key => (target, GradedModuleMap),
    Headline => "find the target of a map of graded modules",
    Usage => "target f",
    Inputs => {"f"},
}

document {
    Key => (transpose, ChainComplexMap),
    Headline => "transpose a map of chain complexes",
    Usage => "transpose f",
    Inputs => {"f" => ChainComplexMap},
    Outputs => { ChainComplexMap => { "the transpose of ", TT "f" } },
    "The output of ", TT "transpose", " is a map from the duals of the
    original source and target free modules. See the degree of the target
    module in the following example",
    EXAMPLE {
	"S = ZZ/10007[x,y,z];",
	"F = res ideal vars S;",
	"F.dd",
	"transpose F.dd"
    },
    "Note that ", TT "M2", " treats the differentials of a chain complex map as map of degree -1.",
}

document {
    Key => (max,GradedModule),
    Usage => "max C",
    Inputs => { "C" },
    Outputs => { ZZ => {"the maximum index of a component, possibly zero, of the graded module ", TT "C" } },
    EXAMPLE lines ///
        R = QQ[a..e]
	C = res coker vars R
	max C
	dual C
	max dual C
    ///,
    SeeAlso => {(min,GradedModule)},
}

document {
    Key => (length, GradedModule),
    Headline => "length of a graded module",
    "The length of a graded module is the difference between the largest and
    smallest indices of occupied spots.  Chain complexes are graded modules,
    so this function applies to them, too.",
    EXAMPLE {
	"R = QQ[x..z];",
	"C = res coker vars R",
	"length C"
    },
}

document {
    Key => (min,GradedModule),
    Usage => "max C",
    Inputs => { "C" },
    Outputs => { ZZ => {"the minimum index of a component, possibly zero, of the graded module ", TT "C" } },
    EXAMPLE lines ///
        R = QQ[a..e]
	C = res coker vars R
	min C
	dual C
	min dual C
    ///,
    SeeAlso => {(max,GradedModule)},
}

document {
    Key => (symbol **, GradedModule, GradedModule),
    Usage => "C ** D",
    Inputs => {"C","D"},
    Outputs => {{"the tensor product of ", TT "C", " with ", TT "D"}},
    EXAMPLE lines ///
        C = gradedModule(ZZ^1,ZZ^6,ZZ^2)
	C ** C
	betti oo
    ///,
}

document {
    Key => {
	(symbol **, GradedModule, Module),
	(symbol **, Module, GradedModule),
    },
    Usage => "C ** M",
    Inputs => {"C","M"},
    Outputs => {{"the tensor product of ", TT "C", " with ", TT "M"}},
    EXAMPLE lines ///
        C = gradedModule(ZZ^1,ZZ^6,ZZ^2)
	C ** ZZ^3
	betti oo
    ///,
    PARA {"It also works the other way around."},
    EXAMPLE lines ///
        ZZ^3 ** C
    ///,
}

document {
    Key => (symbol |, GradedModuleMap, GradedModuleMap),
    Usage => "f|g",
    Inputs => {"f","g"},
    Outputs => {{"the map of graded modules whose component in degree ", TT "i", " is ", TT "f_i|g_i", " see ", TO (symbol |, Matrix, Matrix)}},
    EXAMPLE lines ///
       f = gradedModuleMap( matrix "1;2", matrix "2,3" )
       f | f
    ///,
}

document {
    Key => (symbol ||, GradedModuleMap, GradedModuleMap),
    Usage => "f||g",
    Inputs => {"f","g"},
    Outputs => {{"the map of graded modules whose component in degree ", TT "i", " is ", TT "f_i||g_i", " see ", TO (symbol ||, Matrix, Matrix)}},
    EXAMPLE lines ///
       f = gradedModuleMap( matrix "1;2", matrix "2,3" )
       f || f
    ///,
}

-- This will be consumed into the Complexes package
doc ///
Node
  Key
    (poincare, ChainComplex)
  Headline
    assemble degrees of a chain complex into a polynomial
  Usage
    poincare C
  Inputs
    C:ChainComplex
  Outputs
    :RingElement
      in the Laurent polynomial ring @TO2 (degreesRing, "degrees ring")@, whose variables correspond to the degrees of the ambient ring
  Description
    Text
      We compute @TO poincare@ for a chain complex.
    Example
      R = ZZ/32003[a..h];
      C = res ideal(a*b, c*d, e*f)
      poincare C
    Text
      Note that since the Hilbert series is additive in exact sequences, for a free resolution this only depends
      on the Betti numbers of the resolution. For more details, see @TO "Hilbert functions and free resolutions"@.
    Example
      b = betti C
      poincare b
  SeeAlso
    poincareN
    degreesRing
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    reduceHilbert
    (poincare, BettiTally)

Node
  Key
    (poincareN, ChainComplex)
  Headline
    assemble degrees into polynomial
  Usage
    poincareN C
  Inputs
    C:ChainComplex
  Outputs
    :RingElement
      in the Laurent polynomial ring @TO2 (degreesRing, "degrees ring")@, whose variables correspond to the degrees of the ambient ring
  Description
    Text
      This function encodes information about the degrees of basis elements of a free chain complex in a
      polynomial. The polynomial has terms $S^i T_0^{d_0} \cdots T_{n-1}^{d_{n-1}}$ in it for each basis
      element of @TT "C_i"@ with multi-degree @TT "{d_0,...,d_(n-1)}"@.
    Example
      R = ZZ/101[a,b,c, Degrees=>{1,1,2}];
      C = res cokernel vars R
      betti C
      p = poincareN C
    Text
      Setting the @TT "S"@ variable to -1 gives the polynomial calculated by @TO (poincare, ChainComplex)@.
    Example
      sub(p, {S => -1})
      poincare C
    Text
      Conversely, setting it to 1 gives the same polynomial for the direct sum of components of the complex.
    Example
      sub(p, {S => 1})
      poincare sum C
  SeeAlso
    poincare
    degreesRing
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    reduceHilbert
    (poincare, BettiTally)
    (sum, ChainComplex)
///
