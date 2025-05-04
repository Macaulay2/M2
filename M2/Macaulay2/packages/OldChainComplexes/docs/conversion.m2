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
            needsPackage "OldChainComplexes"
            C = resolution M
            needsPackage "Complexes"
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
            needsPackage "OldChainComplexes"
            C = resolution M
            needsPackage "Complexes"
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
            needsPackage "OldChainComplexes"
            C = resolution M
            f = C.dd
            needsPackage "Complexes"
            g = complex f
            isWellDefined g
            D = freeResolution M
            assert(D.dd == g)
        Text
            The following two extension of maps between modules to
            maps between chain complexes agree.
        Example
            J = ideal vars R
            needsPackage "OldChainComplexes"
            C1 = resolution(R^1/J)
            needsPackage "Complexes"
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
            needsPackage "Complexes"
            D = freeResolution M
            needsPackage "OldChainComplexes"
            C = resolution M
            g = D.dd
            f = chainComplex g
            assert(f == C.dd)
        Text
            We construct a random morphism of chain complexes.
        Example
            J = ideal vars R
            needsPackage "OldChainComplexes"
            C1 = resolution(R^1/J)
            needsPackage "Complexes"
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
