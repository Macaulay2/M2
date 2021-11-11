newPackage(
    "Complexes",
    Version => "0.9999",
    Date => "3 Nov 2021",
    Authors => {
        {   Name => "Gregory G. Smith", 
            Email => "ggsmith@mast.queensu.ca", 
            HomePage => "http://www.mast.queensu.ca/~ggsmith"
            },
        {   Name => "Mike Stillman", 
            Email => "mike@math.cornell.edu", 
            HomePage => "http://www.math.cornell.edu/~mike"
            }},
    Headline => "development package for beta testing new version of chain complexes",
    PackageExports => {"Truncations"},
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

export {
    "component",
    -- types
    "Complex",
    "ComplexMap",
    -- functions/methods
    "augmentationMap",
    "canonicalMap",
    "canonicalTruncation",
    "complex",
    "concentration",
    "connectingMap",
    "connectingExtMap",
    "connectingTorMap",
    "cylinder",
    "freeResolution",
    "homotopyMap",
    "horseshoeResolution",
    "koszulComplex",
    "longExactSequence",
    "isComplexMorphism",
    "isExact",
    "isFree", -- TODO: move to Core, use for freemodules too
    "isQuasiIsomorphism",
    "isNullHomotopic",
    "isNullHomotopyOf",
    "isShortExactSequence",
    "liftMapAlongQuasiIsomorphism",
    "minimize",
    "minimizingMap",
    "nullHomotopy",
    "naiveTruncation",
    "randomComplexMap",
    "resolutionMap",
    "tensorCommutativity",
    "torSymmetry",
    "yonedaExtension",
    "yonedaExtension'",
    "yonedaMap",
    "yonedaMap'",
    "yonedaProduct",
    -- Option names
    "Concentration",
    "Cycle",
    "Boundary",
    "InternalDegree",
    "Base",
    "UseTarget"
    }

-- keys into the type `Complex`
protect modules

--------------------------------------------------------------------
-- code to be migrated to M2 Core ----------------------------------
--------------------------------------------------------------------
tensorCommutativity = method()
tensorCommutativity(Module, Module) := Matrix => (M,N) -> (
    -- implement the isomorphism M ** N --> N ** M
    MN := M ** N;
    NM := N ** M;
    m := numgens source gens M;
    n := numgens source gens N;
    perm := flatten for i from 0 to m - 1 list
      for j from 0 to n - 1 list (
          -- (i,j) (in M**N) to m*i + j
          -- map to column (j,i) <--> n*j + i
          m*j+i
          );
    FMN := source gens MN;
    f := ((id)_FMN)_perm;
    map(NM, MN, f)
    )

homTensorAdjoint = method()
homTensorAdjoint(Module, Module, Module) := (L, M, N) -> (
    -- returns the natural map: Hom(L ** M, N) --> Hom(L, Hom(M, N))
    -- phi -> (ell |-> (m |-> phi(ell ** m)))
    )

-- This belongs in M2 Core!!!
-- make this into a git issue.
single = v -> (
     if not same v 
     then error "incompatible objects in direct sum";
     v#0)
directSum Sequence := args -> (
    if #args === 0 then error "expected more than 0 arguments";
    y := youngest args;
    key := (directSum, args);
    if y =!= null and y.cache#?key then y.cache#key else (
        type := single apply(args, class);
        meth := lookup(symbol directSum, type);
        if meth === null then error "no method for direct sum";
        S := meth args;
        if y =!= null then y.cache#key = S;
        S))

Hom(Module, Module) := Module => (M,N) -> (
    Y := youngest(M.cache.cache,N.cache.cache);
    if Y#?(Hom,M,N) then return Y#(Hom,M,N);
    H := trim kernel (transpose presentation M ** N);
    H.cache.homomorphism = (f) -> map(N,M,adjoint'(f,M,N), Degree => first degrees source f + degree f);
    Y#(Hom,M,N) = H; -- a hack: we really want to type "Hom(M,N) = ..."
    H.cache.formation = FunctionApplication { Hom, (M,N) };
    H)

--------------------------------------------------------------------
-- package code ----------------------------------------------------
--------------------------------------------------------------------
load "Complexes/ChainComplex.m2"
load "Complexes/ChainComplexMap.m2"
load "Complexes/Tor.m2"

-- load "Complexes/Resolutions.m2"
-- load(currentFileDirectory | "Complexes/res.m2")

--------------------------------------------------------------------
-- interface code to legacy types ----------------------------------
--------------------------------------------------------------------
chainComplex Complex := ChainComplex => (cacheValue symbol ChainComplex) (C -> (
    (lo,hi) := concentration C;
    D := new ChainComplex;
    D.ring = ring C;
    for i from lo to hi do D#i = C_i;
    for i from lo+1 to hi do D.dd#i = dd^C_i;
    D
    ))

complex ChainComplex := Complex => opts -> (cacheValue symbol Complex)(D -> (
    (lo,hi) := (min D, max D);
    while lo < hi and (D_lo).numgens == 0 do lo = lo+1;
    while lo < hi and (D_hi).numgens == 0 do hi = hi-1;
    if lo === hi then
        complex D_lo
    else 
        complex hashTable for i from lo+1 to hi list i => D.dd_i
    ))

chainComplex ComplexMap := ChainComplexMap => f -> (
    g := new ChainComplexMap;
    g.cache = new CacheTable;
    g.source = chainComplex source f;
    g.target = chainComplex target f;
    g.degree = degree f;
    (lo,hi) := concentration f;
    for i from lo to hi do g#i = f_i;
    g
    )

complex ChainComplexMap := ComplexMap => opts -> g -> (
    map(complex target g, complex source g, i -> g_i, Degree => degree g)
    )

--------------------------------------------------------------------
-- package documentation -------------------------------------------
--------------------------------------------------------------------
beginDocumentation()

undocumented{
    (net, Complex),
    (net, ComplexMap),
    (texMath, Complex),
    (texMath, ComplexMap),
    (expression, ComplexMap),
    (component,Module,Thing),
    component
    }

load "Complexes/ChainComplexDoc.m2"
load "Complexes/ChainComplexMapDoc.m2"

--------------------------------------------------------------------
-- documentation for legacy type conversion ------------------------
--------------------------------------------------------------------
doc ///
    Key
        (complex, ChainComplex)
    Headline
        translate between data types for chain complexes
    Usage
        D = complex C
    Inputs
        C:ChainComplex
    Outputs
        D:Complex
    Description
        Text
            Both ChainComplex and Complex are Macaulay2 types that
            implement chain complexes of modules over rings.
            The plan is to replace ChainComplex with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/32003[a..d];
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            C = resolution M
            D = complex C
            D1 = freeResolution M
            assert(D == D1)
        Text
            In the following example, note that a different choice of sign
            is chosen in the new Complexes package.
        Example
            C1 = Hom(C, R^1)
            D1 = complex C1
            D2 = Hom(D, R^1)
            D1.dd_-1
            D2.dd_-1
            assert(D1 != D2)
    Caveat
        This is a temporary method to allow comparisons among the data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (chainComplex, Complex)
        (chainComplex, ComplexMap)
        (complex, ChainComplexMap)
///

doc ///
    Key
        (chainComplex, Complex)
    Headline
        translate between data types for chain complexes
    Usage
        C = chainComplex D
    Inputs
        D:Complex
    Outputs
        C:ChainComplex
    Description
        Text
            Both ChainComplex and Complex are Macaulay2 types that
            implement chain complexes of modules over rings.
            The plan is to replace ChainComplex with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/32003[a..d];
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            C = resolution M
            D = freeResolution M
            C1 = chainComplex D
            assert(C == C1)
        Text
            The tensor products make the same choice of signs.
        Example
            D2 = D ** D
            C2 = chainComplex D2
            assert(C2 == C1 ** C1)
    Caveat
        This is a temporary method to allow comparisons among the data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (complex, ChainComplex)
        (complex, ChainComplexMap)
        (chainComplex, ComplexMap)
///

doc ///
    Key
        (complex, ChainComplexMap)
    Headline
        translate between data types for chain complex maps
    Usage
        g = complex f
    Inputs
        f:ChainComplexMap
    Outputs
        g:ComplexMap
    Description
        Text
            Both ChainComplexMap and ComplexMap are Macaulay2 types that
            implement maps between chain complexes.
            The plan is to replace ChainComplexMap with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/32003[a..d];
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            C = resolution M
            f = C.dd
            g = complex f
            isWellDefined g
            D = freeResolution M
            assert(D.dd == g)
        Text
            The following two extension of maps between modules to
            maps between chain complexes agree.
        Example
            J = ideal vars R
            C1 = resolution(R^1/J)
            D1 = freeResolution(R^1/J)
            f = extend(C1, C, matrix{{1_R}})
            g = complex f
            g1 = extend(D1, D, matrix{{1_R}})
            assert(g == g1)
    Caveat
        This is a temporary method to allow comparisons among the data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (chainComplex, ComplexMap)
        (complex, ChainComplex)
        (chainComplex, Complex)
///

doc ///
    Key
        (chainComplex, ComplexMap)
    Headline
        translate between data types for chain complexes
    Usage
        f = chainComplex g
    Inputs
        g:ComplexMap
    Outputs
        f:ChainComplexMap
    Description
        Text
            Both ChainComplexMap and ComplexMap are Macaulay2 types that
            implement maps between chain complexes.
            The plan is to replace ChainComplexMap with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/101[a..d];
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            D = freeResolution M
            C = resolution M
            g = D.dd
            f = chainComplex g
            assert(f == C.dd)
        Text
            We construct a random morphism of chain complexes.
        Example
            J = ideal vars R
            C1 = resolution(R^1/J)
            D1 = freeResolution(R^1/J)
            g = randomComplexMap(D1, D, Cycle => true)
            f = chainComplex g
            assert(g == complex f)
            assert(isComplexMorphism g)
    Caveat
        This is a temporary method to allow comparisons among the data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (complex, ChainComplexMap)
        (complex, ChainComplex)
        (chainComplex, Complex)
///

--------------------------------------------------------------------
-- package tests ---------------------------------------------------
--------------------------------------------------------------------
load "Complexes/ChainComplexTests.m2"

end------------------------------------------------------------

restart
uninstallPackage "Complexes"

restart
installPackage "Complexes"
check "Complexes"
viewHelp Complexes
restart
needsPackage "Complexes"

doc ///
    Key
    Headline
    Usage
    Inputs
    Outputs
    Description
        Text
        Example
    Caveat
    SeeAlso
///


S = ZZ/101[a..d]
K = res coker vars S
L = K ** K
elapsedTime L**L;
