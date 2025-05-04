-- also includes: vector

undocumented (module, Module)

document {
    Key => Module,
    Headline => "the class of all modules",
    PARA{},
    "For basic information about modules in ", EM "Macaulay2", ", see ", TO "modules", ".",
    PARA{},
    "Common ways to make a module:",
    UL {
	TO (symbol ^, Ring, ZZ),
	TO (symbol ^, Ring, List),
	TO (cokernel, Matrix),
	TO (image, Matrix),
	TO (kernel, Matrix),
    },
    "Common ways to get information about modules:",
    UL {
	TO (ring, Module),
	TO (numgens, Module),
	TO (degrees, Module),
	TO (generators, Module),
	TO (relations, Module),
	TO "isFreeModule",
	TO (isHomogeneous, Module),
    },
    "Numerical information about a module:",
    UL {
	TO (codim, Module),
	TO (dim, Module),
	TO (rank, Module)
    },
    "Submodules, quotients, and subquotient modules:",
    UL {
	TO (ambient, Module),
	TO (cover, Module),
	TO (super, Module),
	TO (symbol /, Module, Module),
	TO (subquotient, Matrix, Matrix),
	TO (isSubset, Module, Module),
    },
    "Common operations on modules:",
    UL {
	TO (symbol +, Module, Module),
	TO (symbol ==, Module, Module),
	TO (symbol ++, Module, Module),
	TO (symbol ^, Module, List),
	TO (symbol **, Module, Module),
	TO (symbol ^**, Module, ZZ),
	TO (symbol _, Module, List),
    },
    "Minimalization:",
    UL {
	TO (mingens,Module),
	TO (trim,Module),
	TO (minimalPresentation,Module)
    },
    "Graded modules:",
    UL {
	TO basis,
	TO "Truncations::truncate(ZZ,Module)",
	TO (degree, Module),
	TO "Varieties::genera(Module)",
	TO (hilbertSeries, Module),
	TO (hilbertFunction, ZZ, Module),
	TO (poincare, Module),
	TO (regularity, Module),
    },
    "Annihilators, quotients and Gröbner bases:",
    UL {
	TO (gb, Module),
	TO "Saturation::Ideal : Ideal",
	TO "Saturation::annihilator(Module)",
	TO "Saturation::saturate(Module,Ideal)",
    },
    "Common homological computations:",
    UL {
	TO "OldChainComplexes :: resolution(Module)",
	TO (pdim, Module),
	TO "Hom",
	TO (homomorphism,Matrix),
	TO (Ext,ZZ,Module,Module),
	TO (Tor,ZZ,Module,Module),
	TO (cohomology,ZZ,Module),
	TO (homology, Matrix, Matrix),
	TO (fittingIdeal, ZZ, Module),
    },
    "Multilinear algebra:",
    UL {
	TO (exteriorPower,ZZ,Module),
    },
    -- TODO: merge these in the list above
    Subnodes => {
	TO (symmetricAlgebra, Module),
	TO (symmetricPower, ZZ, Module),
	TO (exteriorPower, ZZ, Module),
	TO (formation, Module),
	TO (symbol +, Module, Module),
	TO (dual, Module),
	TO (symbol SPACE, Module, Array),
	TO ambient,
	TO (ambient, Module),
	TO (super, Module),
	TO cover,
	TO (cover, Module),
	TO (coverMap, Module),
	TO (length, Module),
	TO (dim, Module),
	TO (symbol ++, Module, Module),
	TO (factor, Module),
	TO (hilbertPolynomial, Module),
	TO inducedMap,
	TO (isSubset, Module, Module),
	TO (map, Module),
	TO (mingens, Module),
	TO (minimalPresentation, Module),
	TO (numgens, Module),
	TO (pdim, Module),
	TO (presentation, Module),
	TO (pushForward, RingMap, Module),
	TO (isSubquotient, Module, Module),
	TO (tensor, Module, Module),
	TO (symbol **, Module, Ring),
	TO (tensor, Matrix, Module),
	TO (symbol ^**, Module, ZZ),
	TO (wedgeProduct, ZZ, ZZ, Module),
	TO (symbol ^, Module, ZZ),
	TO (symbol ^, Module, Array),
	TO (symbol ^, Module, List),
	TO (symbol /, Module, Module),
	TO (symbol _, Module, List),
	TO (symbol _, ZZ, Module),
	TO (symbol _, Module, Array),
	TO truncate,
    },
}

document {
    Key => gradedModule,
    Headline => "make a graded module",
    SeeAlso => {
	"Complexes :: gradedModule(Complex)",
	"OldChainComplexes :: gradedModule(List)",
    },
}

document {
     Key => module,
     Headline => "make or get a module",
    Subnodes => {
	TO (module, Ring),
	-- TO (module, Ideal), -- TODO
	TO (module, Vector),
    },
     }

document { Key => (module, Ring),
     Usage => "module R",
     Inputs => {"R"},
     Outputs => {{"the free module of rank 1 over the ring R"}},
     EXAMPLE lines ///
     	  ZZ
	  module ZZ
     ///}

document { Key => (module, Vector),
     Headline => "the module of a vector",
     Usage => "module v",
     Inputs => {"v"},
     Outputs => {{"the module that contains the vector ", TT "v"}},
     "The class of ", TT "v", " is also equal to the module of ", TT "v", ".",
     EXAMPLE lines ///
     	  F = ZZ^4
	  v = F_2
	  module v
	  class v
     ///}


document {
    Key => (symbol SPACE, Module, Array),
    Headline => "make a chain complex from a module",
    TT "M[n]", " -- create a chain complex with the module M concentrated in degree -n.",
    PARA{},
    SeeAlso => {
	"Complexes :: Complex",
	"OldChainComplexes :: ChainComplex",
    },
}

document {
     Key => {relations,(relations, Module)},
     Headline => "the defining relations",
     TT "relations M", " -- produce the relations defining a module M.",
     PARA{},
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA{},
     SeeAlso => {"generators","subquotient"}}

document {
    Key => Vector,
    Headline => "the class of all elements of modules that are handled by the engine",
    "If ", TT "R", " is a ring handled by the engine, and ", TT "M", " is a
    module over ", TT "R", ", then M is a subclass of Vector.",
    PARA{},
    SeeAlso => {RingElement, Module, Matrix},
    Subnodes => {
	TO vector,
	TO (entries, Vector),
	TO (symbol **, Vector, Vector),
	TO (symbol ||, Vector, Vector),
    },
}

doc ///
  Key
    vector
    (vector, Module, List)
    (vector, Module, Matrix)
    (vector, Module, Number)
    (vector, Module, RingElement)
    (vector, List)
    (vector, Matrix)
    (vector, Number)
    (vector, RingElement)
    (vector, Ring, List)
    (vector, Ring, Matrix)
    (vector, Ring, Number)
    (vector, Ring, RingElement)
    (vector, RingFamily, List)
    (vector, RingFamily, Matrix)
    (vector, RingFamily, Number)
    (vector, RingFamily, RingElement)
  Headline
    make a vector
  Usage
    vector(M, x)
    vector x
  Inputs
    M:Module
    x:{List, Matrix, Number, RingElement}
  Outputs
    :Vector
  Description
    Text
      For any $R$-module $M$, there exists an isomorphism between
      $\operatorname{Hom}(R,M)$ and $M$ given by $f\mapsto f(1)$.
      Therefore, internally all @TO Vector@ objects representing
      elements of $M$ correspond to matrices with source $R^1$ and
      target $M$.  A vector may be constructed from such a matrix using
      @SAMP "vector"@.
    Example
      R = QQ[x,y,z]
      f = matrix {{x}, {y}, {z}}
      vector f
    Text
      Alternatively, $M$ may be specified if it differs from the target
      of the matrix.
    Example
      g = matrix {{1}, {2}, {3}}
      vector(R^3, g)
    Text
      If the matrix would have only one element, then that element may be
      given instead.  If the module is not provided, then the result will
      be an element of the free module of rank one of the ring of the given
      element.
    Example
      vector 2
      vector x
      vector(R^1, 2)
    Text
      Alternatively, a list of elements may be provided.  If the module is
      not specified, then the vector will be an element of a free module over
      a ring containing all the elements of the list.
    Example
      vector {1, 2, 3}
      vector {1, x, y}
      vector(R^3, {1, 2, 3})
    Text
      Alternatively, the ring $R$ may be provided instead of $M$, and the
      resulting vector will be an element of the appropriate free module over
      $R$.
    Example
      vector(QQ, {1, 2, 3})
      vector(R, {1, 2, 3})
      vector(R, 2)
///
