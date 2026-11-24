-- here we document the stub definitions in m2/shared.m2

doc ///
  Key
    height
  Headline
    height of an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Posets::height(Poset)"
///
doc ///
  Key
    depth
  Headline
    depth of an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Depth::Depth"
    "SLPexpressions::SLPexpressions"
///
doc ///
  Key
    width
  Headline
    width of an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
///
doc ///
  Key
    length
  Headline
    length of an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Complexes::length(Complex)"
    "Permutations::length(Permutation)"
///
doc ///
  Key
    extend
  Headline
    extend an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Permutations :: extend(Permutation,ZZ)"
    "Complexes :: extend(Complex,Complex,Matrix)"
    "OldChainComplexes :: extend(ChainComplex,ChainComplex,Matrix)"
///

doc ///
  Key
    complete
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "OldChainComplexes::complete(ChainComplex)"
///
doc ///
  Key
    minimize
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Complexes::minimize(Complex)"
///
doc ///
  Key
    decompose
  Headline
    decompose an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "MinimalPrimes::MinimalPrimes"
///
doc ///
  Key
    truncate
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Truncations::Truncations"
///
doc ///
  Key
    chi
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Schubert2::chi(AbstractSheaf)"
    "NormalToricVarieties::chi(CoherentSheaf)"
///
doc ///
  Key
    euler
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "HyperplaneArrangements::HyperplaneArrangements"
    "MultiprojectiveVarieties::euler(MultiprojectiveVariety)"
    "Schubert2::euler(AbstractVariety)"
    "Varieties::Varieties"
///
doc ///
  Key
    eulers
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
///
doc ///
  Key
    genera
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
///
doc ///
  Key
    genus
  Headline
    genus of an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
///
doc ///
  Key
    isExact
  Headline
    whether an object is exact
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "ChainComplexExtras :: isExact(ChainComplex)"
    "Complexes :: isExact(Complex)"
///
doc ///
  Key
    isSmooth
  Headline
    whether an object is smooth
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "WeilDivisors::isSmooth(Ideal)"
    "LatticePolytopes::isSmooth(Polyhedron)"
    "Varieties::isSmooth(Variety)"
    "SpaceCurves::isSmooth(Curve)"
    "Polyhedra::isSmooth(Cone)"
    "NormalToricVarieties::isSmooth(NormalToricVariety)"
    "SpecialFanoFourfolds::isSmooth(EmbeddedProjectiveVariety)"
///
doc ///
  Key
    isVeryAmple
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "WeilDivisors::isVeryAmple(WeilDivisor)"
    "Polyhedra::isVeryAmple(Polyhedron)"
    "PositivityToricBundles::isVeryAmple(ToricVectorBundleKlyachko)"
    "NormalToricVarieties::isVeryAmple(ToricDivisor)"
///
doc ///
  Key
    isNormal
  Headline
    whether an object is normal
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Polyhedra::isNormal(Polyhedron)"
    "IntegralClosure::isNormal(Ring)"
    "AssociativeAlgebras::isNormal(RingElement)"
///
doc ///
  Key
    normalCone
  Headline
    normal cone of an object
  Description
    Text
      This function is a method function, defined in the core so multiple
      packages can add methods to it.
  SeeAlso
    "Polyhedra::normalCone(Polyhedron,Polyhedron)"
    "ReesAlgebra::normalCone(Ideal)"
///

doc ///
  Key
    isEmpty
    (isEmpty, Thing)
  Headline
    whether an object is empty
  SeeAlso
    "Polyhedra::Polyhedra"
///

doc ///
  Key
    isIsomorphism
    (isIsomorphism, Matrix)
  Headline
    whether a map is an isomorphism
  Usage
    isIsomorphism f
  Description
    Text
      Whether the map $f$ is an isomorphism.
  SeeAlso
    "Cremona::isIsomorphism(RationalMap)"
    "GradedLieAlgebras::isIsomorphism(LieAlgebraMap)"
    "MultiprojectiveVarieties::isIsomorphism(MultirationalMap)"
    "Varieties::isIsomorphism(SheafMap)"
///

doc ///
  Key
    cone
  Headline
    mapping cone or polyhedral cone
  SeeAlso
    "Complexes::cone(ComplexMap)"
    "Polyhedra::cone(Polyhedron)"
///
doc ///
  Key
    rays
  Headline
    1-dimensional cones of a fan or polyhedral object
  SeeAlso
    "FourTiTwo::rays(Matrix)"
    "NormalToricVarieties::rays(NormalToricVariety)"
    "Polyhedra::rays(PolyhedralObject)"
///

doc ///
  Key
    pullback
  Headline
    compute the pullback morphism
  SeeAlso
    pushout
///

doc ///
  Key
    pushout
  Headline
    compute the pushout morphism
  SeeAlso
    pullback
///

-- also see functions/intersect-doc.m2
doc ///
  Key
    intersect
    (intersect, List)
    (intersect, Sequence)
  Headline
    compute an intersection
  Description
    Text
      This function calculates the intersection of a list or sequence of
      compatible objects.

      When a more efficient algorithm is available for computing the
      intersection of all inputs simultaneously rather than iteratively, for
      instance for intersecting @TO Module@s, a specialized function that takes
      a list or sequence may be installed on @TT "(symbol intersect, Type)"@.
    Example
      code lookup(symbol intersect, Module)
    Text
      This function may also be called using the synonym
      @M2CODE "intersection"@.
  SeeAlso
    -- add references to intersect methods installed in packages _other than Core_ here
    "M0nbar::intersect"
    "NAGtypes::intersect(PolySpace,PolySpace)"
    "Polyhedra::intersect"
///

undocumented { 1:union }
doc ///
  Key
    union
    (union, List)
    (union, Sequence)
  Headline
    compute the union
  Description
    Text
      This function returns the union of a list or sequence of compatible sets
      or varieties.
  SeeAlso
    "NumericalAlgebraicGeometry::union(NumericalVariety,NumericalVariety)"
    "Posets::union(Poset,Poset)"
///

-- also see functions/tensor-doc.m2
doc ///
  Key
    tensor
    (tensor, List)
    (tensor, Sequence)
  Headline
    tensor product
  Description
    Text
      This function calculates the tensor product of a list or sequence of
      compatible objects.

      This method is declared as a @TO MethodFunctionBinary@, so for two or
      more argument the product is computed iteratively from the binary tensor
      products, working from left to right. For two arguments, this is the same
      as @TT "A ** B"@ except that options are allowed.
    Example
      tensor(ZZ^2, ZZ^3, ZZ^4)
  SeeAlso
    -- add references to tensor methods installed in packages _other than Core_ here
    symbol**
///

doc ///
  Key
     directProduct
    (directProduct, List)
    (directProduct, Sequence)
  Headline
    direct product
  Description
    Text
      This function calculates the direct product of a list or sequence of
      compatible objects.

      This is a binary method, meaning it is defined for two inputs but can
      accept more. When three or more arguments are given, the direct product is
      evaluated from left to right: the direct product of the first two inputs
      is computed, then the result is used to compute the direct product with
      the third input, and so on.
  SeeAlso
    "Polyhedra::directProduct(Cone,Cone)"
    "Polyhedra::directProduct(Fan,Fan)"
    "Graphs::directProduct(Graph,Graph)"
///

doc ///
  Key
    status
  Headline
    get the status of a computation
  SeeAlso
    (status, GroebnerBasis)
    "OldChainComplexes :: status(ChainComplex)"
    "SubalgebraBases :: status(SAGBIBasis)"
    "SumsOfSquares :: status(SDPResult)"
    -- this one is used in a different sense
    -- "NAGtypes :: status(AbstractPoint)",
///
