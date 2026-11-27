-- also includes: vector

undocumented (module, Module)

doc ///
  Key
    Module
  Headline
    the class of all modules
  Description
    Text
      For basic information about modules in @EM "Macaulay2"@, see
      @TO "modules"@.

      Common ways to make a module:

      @UL {
          TO (symbol ^, Ring, ZZ),
          TO (symbol ^, Ring, List),
          TO (cokernel, Matrix),
          TO (image, Matrix),
          TO (kernel, Matrix)
          }@

      Common ways to get information about modules:

      @UL {
          TO (ring, Module),
          TO (numgens, Module),
          TO (degrees, Module),
          TO (generators, Module),
          TO (relations, Module),
          TO "isFreeModule",
          TO (isHomogeneous, Module)
          }@

      Numerical information about a module:

      @UL {TO (codim, Module), TO (dim, Module), TO (rank, Module)}@

      Submodules, quotients, and subquotient modules:

      @UL {
          TO (ambient, Module),
          TO (cover, Module),
          TO (super, Module),
          TO (symbol /, Module, Module),
          TO (subquotient, Matrix, Matrix),
          TO (isSubset, Module, Module)
          }@

      Common operations on modules:

      @UL {
          TO (symbol +, Module, Module),
          TO (symbol ==, Module, Module),
          TO (symbol ++, Module, Module),
          TO (symbol ^, Module, List),
          TO (symbol **, Module, Module),
          TO (symbol ^**, Module, ZZ),
          TO (symbol _, Module, List)
          }@

      Minimalization:

      @UL {
          TO (mingens, Module),
          TO (trim, Module),
          TO (minimalPresentation, Module)
          }@

      Graded modules:

      @UL {
          TO basis,
          TO "Truncations::truncate(ZZ,Module)",
          TO (degree, Module),
          TO "Varieties::genera(Module)",
          TO (hilbertSeries, Module),
          TO (hilbertFunction, ZZ, Module),
          TO (poincare, Module),
          TO (regularity, Module)
          }@

      Annihilators, quotients and Gröbner bases:

      @UL {
          TO (gb, Module),
          TO "Saturation::Ideal : Ideal",
          TO "Saturation::annihilator(Module)",
          TO "Saturation::saturate(Module,Ideal)"
          }@

      Common homological computations:

      @UL {
          TO "OldChainComplexes :: resolution(Module)",
          TO (pdim, Module),
          TO "Hom",
          TO (homomorphism, Matrix),
          TO (Ext, ZZ, Module, Module),
          TO (Tor, ZZ, Module, Module),
          TO (cohomology, ZZ, Module),
          TO (homology, Matrix, Matrix),
          TO (fittingIdeal, ZZ, Module)
          }@

      Multilinear algebra:

      @UL {TO (exteriorPower, ZZ, Module)}@
  Subnodes
    -- TODO: merge these in the list above
    (symmetricAlgebra, Module)
    (symmetricPower, ZZ, Module)
    (exteriorPower, ZZ, Module)
    (formation, Module)
    (symbol +, Module, Module)
    (dual, Module)
    (symbol SPACE, Module, Array)
    ambient
    (ambient, Module)
    (super, Module)
    cover
    (cover, Module)
    (coverMap, Module)
    (length, Module)
    (dim, Module)
    (symbol ++, Module, Module)
    (factor, Module)
    (hilbertPolynomial, Module)
    inducedMap
    (isSubset, Module, Module)
    (map, Module)
    (mingens, Module)
    (minimalPresentation, Module)
    (numgens, Module)
    (pdim, Module)
    (presentation, Module)
    (pushForward, RingMap, Module)
    (isSubquotient, Module, Module)
    (tensor, Module, Module)
    (symbol **, Module, Ring)
    (tensor, Matrix, Module)
    (symbol ^**, Module, ZZ)
    (wedgeProduct, ZZ, ZZ, Module)
    (symbol ^, Module, ZZ)
    (symbol ^, Module, Array)
    (symbol ^, Module, List)
    (symbol /, Module, Module)
    (symbol _, Module, List)
    (symbol _, ZZ, Module)
    (symbol _, Module, Array)
    truncate
///

doc ///
  Key
    gradedModule
  Headline
    make a graded module
  SeeAlso
    "Complexes :: gradedModule(Complex)"
    "OldChainComplexes :: gradedModule(List)"
///

doc ///
  Key
    module
  Headline
    make or get a module
  Subnodes
    (module, Ring)
    -- TO (module, Ideal), -- TODO
    (module, Vector)
///

doc ///
  Key
    (module, Ring)
    (module, RingFamily)
  Usage
    module R
  Inputs
    R:
  Outputs
    :
      the free module of rank 1 over the ring R
  Description
    Example
      ZZ
      module ZZ
///

doc ///
  Key
    (module, Vector)
  Headline
    the module of a vector
  Usage
    module v
  Inputs
    v:
  Outputs
    :
      the module that contains the vector @TT "v"@
  Description
    Text
      The class of @TT "v"@ is also equal to the module of @TT "v"@.
    Example
      F = ZZ^4
      v = F_2
      module v
      class v
///


doc ///
  Key
    (symbol SPACE, Module, Array)
  Headline
    make a chain complex from a module
  Description
    Text
      @TT "M[n]"@ -- create a chain complex with the module M concentrated in
      degree -n.
  SeeAlso
    "Complexes :: Complex"
    "OldChainComplexes :: ChainComplex"
///

doc ///
  Key
    relations
    (relations, Module)
  Headline
    the defining relations
  Description
    Text
      @TT "relations M"@ -- produce the relations defining a module M.

      The relations are represented as a matrix, and if not stored in the
      module under M.relations, the matrix is understood to be empty.
  SeeAlso
    "generators"
    "subquotient"
///

doc ///
  Key
    Vector
  Headline
    the class of all elements of modules that are handled by the engine
  Description
    Text
      If @TT "R"@ is a ring handled by the engine, and @TT "M"@ is a module
      over @TT "R"@, then M is a subclass of Vector.
  SeeAlso
    RingElement
    Module
    Matrix
  Subnodes
    vector
    (entries, Vector)
    (symbol **, Vector, Vector)
    (symbol ||, Vector, Vector)
///

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
