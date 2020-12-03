--------------------------------------------------------------------
-- documentation for chain complex maps ----------------------------
--------------------------------------------------------------------

doc ///
    Key
        "Making maps between chain complexes"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (map, Complex, Complex, HashTable),
                TO (map, Complex, Complex, ZZ),
                TO (map, Complex, Complex, Function),
                TO (map, Complex, Complex, ComplexMap),
                TO (id, Complex),
                TO "differential of a chain complex",
                TO (symbol SPACE, ComplexMap, Array),
                TO (isWellDefined, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new complex maps"@
        Text 
            @UL {
                TO (resolutionMap, Complex),
                TO (homology, ComplexMap),
                TO (augmentationMap, Complex),
                TO (extend, Complex, Complex, Matrix)
           }@
    	Text
    	    @SUBSECTION "Canonical maps between complexes"@
        Text
            Some complexes come with canonical maps.
            To access the complex map, 
            one uses @TO (canonicalMap, Complex, Complex)@.
            The following operations have associated canonical maps.
	Text
    	    @UL {
                TO (kernel, ComplexMap),
                TO (cokernel, ComplexMap),
                TO (image, ComplexMap),
                TO (coimage, ComplexMap),
                TO (cone, ComplexMap),
                TO (cylinder, ComplexMap),
                TO (inducedMap, Complex, Complex)
            }@
    	Text
    	    @SUBSECTION "Random maps of chain complexes"@
        Text
            The method @TO (randomComplexMap, Complex, Complex)@
            allows one to construct random complex maps,
            random morphisms between complexes, and random
            null homotopies between complexes.
	Text
    	    @UL {
                TO (isCommutative, ComplexMap),
                TO (isComplexMorphism, ComplexMap),
                TO (isNullHomotopic, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Elementary operations on complex maps"@
        Text
    	    @UL {
                TO "arithmetic with complex maps",
                TO (symbol +, ComplexMap, ComplexMap),
                TO (symbol |, ComplexMap, ComplexMap),
                TO (symbol ||, ComplexMap, ComplexMap),
                TO (symbol ++, ComplexMap, ComplexMap),
                TO (symbol **, ComplexMap, ComplexMap),
                TO (Hom, ComplexMap, ComplexMap),
                TO (dual, ComplexMap),
                TO (symbol _, ComplexMap, Array),
                TO (symbol ^, ComplexMap, Array),
                TO (naiveTruncation, ComplexMap, Sequence),
                TO (canonicalTruncation, ComplexMap, Sequence),
                TO (part, List, ComplexMap),
                TO (truncate, List, ComplexMap),
                TO (symbol SPACE, RingMap, ComplexMap),
                TO (symbol **, RingMap, ComplexMap)
            }@
    SeeAlso
        "Making chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Towards computing in the derived category"
    Description
        Text
            @UL {
                TO (resolution, Complex),
                TO (resolutionMap, Complex),
                TO (isShortExactSequence, ComplexMap, ComplexMap),
                TO (isQuasiIsomorphism, ComplexMap),
                TO (liftMapAlongQuasiIsomorphism, ComplexMap, ComplexMap),
                TO (connectingMap, ComplexMap, ComplexMap),
                TO (horseshoeResolution, Complex)
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
///

doc ///
  Key
    ComplexMap
  Headline
    the class of all maps between chain complexes
  Description
    Text
      A map of chain complexes $f : C \rightarrow D$ of degree $d$ is a
      sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
      No relationship between the maps $f_i$ and 
      and the differentials of either $C$ or $D$ is assumed.
      
      The set of all maps from $C$ to $D$ form
      the complex $Hom(C,D)$ where $Hom(C,D)_d$ consists of the
      maps of degree $d$.

      The usual algebraic operations are available: addition,
      subtraction, scalar multiplication, and composition. The
      identity map from a chain complex to itself can be produced with
      @TO "id"@. An attempt to add (subtract, or compare) a ring
      element to a chain complex will result in the ring element being
      multiplied by the appropriate identity map.
  SeeAlso
    Complex
///

doc ///
    Key
        (map, Complex, Complex, HashTable)
    Headline
        make a map of chain complexes
    Usage
        f = map(D, C, H)
    Inputs
        C:Complex
        D:Complex
        H:HashTable
            whose keys are integers, and whose values are the maps between
            the corresponding terms
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
    Description
        Text
            A map of complexes $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and 
            and the differentials of either $C$ or $D$ is assumed.
            
            We construct a map of chain complexes by specifying the
            individual maps between the terms.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}
            D = freeResolution coker vars R
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{a*b^2, 0, 0}, {-a*c^2, a*c^3, 0}, {b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{a*b^2*c^3}})
                }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isComplexMorphism f
        Text
            The keys in the hash table index the terms in the source of the
            map.  If a key is missing, that map is taken to be the zero map.
            We illustrate by constructing a map of chain complexes
            having nonzero degree, and omitting one key in the hash table.
        Example
            E = D[-3]
            H = hashTable { 0 => map(E_3, C_0, 1),
                1 => map(E_4, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                3 => map(E_6, C_3, {{a*b^2*c^3}})
                }
            g = map(E, C, H, Degree => 3)
            g_2
            assert(g_1 == f_1)
            assert(g != f)
            assert isWellDefined g
            assert isHomogeneous g
            assert(degree g == 3)
            assert not isComplexMorphism g
            assert not isCommutative g
            assert(source g == C)
            assert(target g == E)
        Text
            This is the primary constructor used by all of the more user friendly
            methods for constructing a chain complex.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, ComplexMap)@.
    SeeAlso
        ComplexMap
        (map, Complex, Complex, Function)
        (isWellDefined, ComplexMap)
        (isHomogeneous, ComplexMap)
        (degree, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (isCommutative, ComplexMap)
        (source, ComplexMap)
        (target, ComplexMap)
///

doc ///
    Key
        (map, Complex, Complex, ZZ)
    Headline
        make the zero map or identity between chain complexes
    Usage
        f = map(D, C, 0)
        f = map(C, C, 1)
    Inputs
        C:Complex
        D:Complex
        0:ZZ
            or 1
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
            the zero map from $C$ to $D$ or the identity map from $C$ to $C$
    Description
        Text
            A map of complexes $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            
            We construct the zero map between two
            chain complexes.
        Example
            R = QQ[a,b,c]
            C = freeResolution coker vars R
            D = freeResolution coker matrix{{a^2, b^2, c^2}}
            f = map(D, C, 0)
            assert isWellDefined f
            assert isComplexMorphism f
            g = map(C, C, 0, Degree => 13)
            assert isWellDefined g
            assert(degree g == 13)
            assert not isComplexMorphism g
            assert isCommutative g
            assert isHomogeneous g
            assert(source g == C)
            assert(target g == C)
        Text
            Using this function to create the identity map
            is the same as using @TO (id, Complex)@.
        Example
            assert(map(C, C, 1) === id_C)
   SeeAlso
        ComplexMap
        (map, Complex, Complex, Function)
        (isWellDefined, ComplexMap)
        (isHomogeneous, ComplexMap)
        (degree, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (isCommutative, ComplexMap)
        (source, ComplexMap)
        (target, ComplexMap)
        (id, Complex)
///

doc ///
    Key
        (map, Complex, Complex, Function)
    Headline
        make a map of chain complexes
    Usage
        f = map(D, C, fcn)
    Inputs
        C:Complex
        D:Complex
        fcn:Function
            whose values at integers are the maps between
            the corresponding terms
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
    Description
        Text
            A map of complexes $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and 
            and the differentials of either $C$ or $D$ is assumed.
            
            We construct a map of chain complexes by specifying a
            function which determines the maps between the terms.
        Example
            R = ZZ/101[x]/x^3;
            M = coker vars R
            C = freeResolution(M, LengthLimit => 6)
            D = C[1]
            f = map(D, C, i -> 
                if odd i then 
                    map(D_i, C_i, {{x}})
                else map(D_i, C_i, {{x^2}})
                )
            assert isWellDefined f
            assert isCommutative f
            assert(source f == C)
            assert(target f == D)
    SeeAlso
        ComplexMap
        (isWellDefined, ComplexMap)
        (isCommutative, ComplexMap)
        (source, ComplexMap)
        (target, ComplexMap)
///


doc ///
    Key
        (map, Complex, Complex, ComplexMap)
    Headline
        make a new map of chain complexes from an existing one
    Usage
        g = map(D, C, f)
    Inputs
        C:Complex
        D:Complex
        f:ComplexMap
            regarded as providing matrices which induce maps between the terms of $C$ and $D$
        Degree => ZZ
            the degree $d$ of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        g:ComplexMap
    Description
        Text
            A map of complexes $f : C' \rightarrow D'$ is a
            sequence of maps $f_i : C'_i \rightarrow D'_{d'+i}$.  
            The new map $g : C \rightarrow D$ is the sequence of maps $g_i : C_i \rightarrow D_{d+i}$
            induced by the matrix of $f_i$.
            
            One use for this function is to get the new map of chain complexes obtained by shifting 
            the source or target of an existing chain map.  For example, one can regard the differential
            on a complex can be regarded as a map of degree zero between shifted complexes.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker vars R
            f = map(C[-1], C, dd^C, Degree => 0)
            assert isWellDefined f
            assert(degree f == 0)
            assert isCommutative f
            assert isComplexMorphism f
            assert not isComplexMorphism dd^C
    SeeAlso
        ComplexMap
        (map, Complex, Complex, Function)
        (isWellDefined, ComplexMap)
        (degree, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (isCommutative, ComplexMap)
        (symbol SPACE, Complex, Array)
///

doc ///
    Key
        (id, Complex)
    Headline
        the identity map of a chain complex
    Usage
        f = id_C
    Inputs
        C:Complex
    Outputs
        f:ComplexMap
          the identity map from $C$ to itself
    Description
        Text
            The chain complexes together with complex morphisms
            forms a category.  In particular, every chain 
            complex has an identity map.
        Example
            R = ZZ/101[x,y]/(x^3, y^3)
            C = freeResolution(coker vars R, LengthLimit=>6)
            f = id_C
            assert isWellDefined f
            assert isComplexMorphism f
        Text
            The identity map corresponds to an element of
            the Hom complex.
        Example
            R = ZZ/101[a,b,c]
            I = ideal(a^2, b^2, b*c, c^3)
            C = freeResolution I
            D = Hom(C, C)
            homomorphism' id_C
    SeeAlso
        (map, Complex, Complex, ZZ)
        (isWellDefined, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (Hom, Complex, Complex)
        (homomorphism', ComplexMap)
///

doc /// 
    Key
        (isWellDefined, ComplexMap)
    Headline
        whether a map of chain complexes is well-defined
    Usage
        isWellDefined f
    Inputs
        f:ComplexMap
    Outputs
        :Boolean
            that is true when {\tt f} determines a well defined complex map
    Description
        Text
            A map of chain complexes $f : C \to D$ of degree $d$ is a sequence of
            maps $f_i : C_i \to D_{d+i}$.  No relationship is required between
            these maps and the differentials in the source and target.

            This routine checks that $C$ and $D$ are well-defined
            chain complexes, and that, for each $f_i$, the source and
            target equal $C_i$ and $D_{d+i}$, respectively.  If the
            variable {\tt debugLevel} is set to a value greater than
            zero, then information about the nature of any failure is
            displayed.
        Text
            Unlike the @TO2((isWellDefined, Complex), 
                "corresponding function for Complexes")@,
            the basic constructors for complex maps are all but
            assured to be well defined. The only case that could cause
            a problem is if one constructs the source or target
            complex, and those are not well defined.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}
            D = freeResolution coker vars R
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{a*b^2, 0, 0}, {-a*c^2, a*c^3, 0}, {b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{a*b^2*c^3}})
                }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isComplexMorphism f
        Text
            We construct two random maps of chain complexes,
            and check to see that, as should be the case, 
            both are well defined.
        Example
            g = randomComplexMap(D,C)
            assert isWellDefined g
            assert not isCommutative g
        Example
            h = randomComplexMap(D,C, Cycle => true)
            assert isWellDefined h
            assert isComplexMorphism h
        Text
            This method also checks the following aspects of 
            the data structure:
        Text
            @UL {
                TEX "The underlying hash table has exactly the expected keys,
                namely, {\\tt source, target, degree, map, cache}",
                "The ring of the source and target are the same",
                "The source and target are well defined complexes",
                "The degree is an integer",
                TEX "All keys in the {\\tt map} field are integers,
                in the range of the concentration of the source",
                TEX "The source and target of each $f_i$ is as expected",
                TEX "If the {\\tt isCommutative} key is present in the cache
                table, then commutativity of the map with the differentials
                is checked"
                }@
    SeeAlso
        (isWellDefined, Complex)
        (isCommutative, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (map, Complex, Complex, HashTable)
///

doc ///
    Key
        (source, ComplexMap)
    Headline
        get the source of a map of chain complexes
    Usage
        C = source f
    Inputs
      f:ComplexMap
    Outputs
      C:Complex
    Description
        Text
            Given a complex map $f : C \to D$
            this method returns the chain complex $C$.
        Example
            R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = freeResolution I
            FJ = freeResolution J
            f = randomComplexMap(FJ, FI, Cycle=>true)
            source f
            assert isWellDefined f
            assert isComplexMorphism f
            assert(source f == FI)
            assert(target f == FJ)
        Text
            The differential in a complex is a map of chain complexes.
        Example
            kk = coker vars R
            F = freeResolution kk
            source dd^F == F
            target dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making chain complexes"
       (target, ComplexMap)
       (freeResolution, Ideal)
       (randomComplexMap, Complex, Complex)
///

doc ///
    Key
        (target, ComplexMap)
    Headline
        get the target of a map of chain complexes
    Usage
        C = target f
    Inputs
      f:ComplexMap
    Outputs
      C:Complex
    Description
        Text
            Given a complex map $f : C \to D$
            this method returns the chain complex $D$.
        Example
            R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = freeResolution I
            FJ = freeResolution J
            f = randomComplexMap(FJ, FI, Cycle=>true)
            target f
            assert isWellDefined f
            assert isComplexMorphism f
            assert(target f == FJ)
            assert(source f == FI)
        Text
            The differential in a complex is a map of chain complexes.
        Example
            kk = coker vars R
            F = freeResolution kk
            target dd^F == F
            source dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making chain complexes"
       (source, ComplexMap)
       (freeResolution, Ideal)
       (randomComplexMap, Complex, Complex)
///

doc ///
    Key
        (degree, ComplexMap)
    Headline
        get the degree of a map of chain complexes
    Usage
        degree f
    Inputs
        f:ComplexMap
    Outputs
        :ZZ
    Description
        Text
            A complex map $f : C \to D$ of degree $d$ is a sequence of
            of maps $f_i : C_i \to D_{i+d}$.
            This method returns $d$.
        Text
            The degree of the differential of a complex is always -1.
        Example
            R = ZZ/101[a..d];
            I = ideal(a^2, b^2, c^2)
            FI = freeResolution I
            assert(degree dd^FI == -1)
        Example
            FJ = freeResolution (I + ideal(a*b*c))
            f = randomComplexMap(FJ, FI, Cycle=>true, Degree => -2)
            assert(degree f == -2)
   SeeAlso
       "Basic invariants and properties"
       (source, ComplexMap)
       (target, ComplexMap)
       (freeResolution, Ideal)
       (randomComplexMap, Complex, Complex)
///

doc ///
    Key
        (concentration, ComplexMap)
    Headline
        indices on which a complex map may be non-zero
    Usage
        (lo,hi) = concentration f
    Inputs
        f:ComplexMap
    Outputs
        :Sequence
            a pair of integers {\tt lo}, {\tt hi} such that {\tt f_i = 0}
            for {\tt i < lo} or {\tt i > hi}.
    Description
        Text
            In this package, each map of complexes has a concentration {\tt (lo, hi)} 
            such that {\tt lo <= hi}.  When {\tt lo <= i <= hi}, the map
            {\tt f_i} might be zero.
      
            This function is mainly used in programming, to loop over all
            non-zero maps.  This should not be confused
            with the support of the source or target.
        Example
            S = ZZ/101[a..c]
            C = freeResolution coker vars S
            concentration id_C
            D = C ++ C[5]
            concentration id_D
            f = randomComplexMap(D, C)
            concentration f
        Text
            Indices that are outside of the concentration automatically
            return the zero object.
        Example
            f_-1
            (id_D)_4
        Text
            Maps inside the concentration may nevertheless be zero.
        Example
            (id_D)_-1
        Text
            The concentration of a zero complex can be arbitrary, however,
            after pruning, its concentration will be {\tt (0,0)}.
        Example      
            C0 = (complex S^0)[4]
            g = id_C0
            concentration g
            prune g
            concentration oo
    SeeAlso
        (concentration, Complex)
        (symbol _, ComplexMap, ZZ)
///


doc ///
    Key
        (symbol _, ComplexMap, ZZ)
    Headline
        access individual matrices in a complex map
    Usage
        f_i
    Inputs
        f:ComplexMap
        i:ZZ
            the homological index
    Outputs
        :Matrix
            the {\tt i}-th map
    Description
        Text
            A complex map $f : C \to D$ of degree $d$ is a sequence of maps $f_i : C_i \to D_{i+d}$.
            This method allows one to access the individual $f_i$.
        Example
            S = ZZ/101[a..c];
            C = freeResolution coker matrix{{a^2, b^2, c^2}}
            D = freeResolution coker vars S
            f = randomComplexMap(D, C)
            f_2
            f_0
        Text
            Indices that are outside of the concentration are automatically zero.
        Example
            concentration f
            f_-1
            f_3
            f_4
    SeeAlso
        (symbol_, Complex, ZZ)
        (concentration, ComplexMap)
///

doc ///
  Key
    (symbol*, ComplexMap, ComplexMap)
  Headline
    composition of homomorphisms of complexes
  Usage
    f = h * g
  Inputs
    h:ComplexMap
      if a ring element or integer, then we multiply the ring element
      by the appropriate identity map
    g:ComplexMap
  Outputs
    f:ComplexMap
      the composition of $g$ followed by $h$
  Description
    Text
      If $g_i : C_i \rightarrow D_{d+i}$, and $h_j : D_j \rightarrow E_{e+j}$,
      then the composition corresponds to 
      $f_i := h_{d+i} * g_i : C_i \rightarrow E_{i+d+e}$.  In particular,
      the degree of the composition $f$ is the sum of the degrees of
      $g$ and $h$.
    Example
      R = ZZ/101[a..d]
      C = freeResolution coker vars R
      3 * dd^C
      0 * dd^C
      dd^C * dd^C
  SeeAlso
      "Making maps between chain complexes"
      "arithmetic with complex maps"
///

doc ///
    Key
        (symbol ^, ComplexMap, ZZ)
    Headline
        the n-fold composition
    Usage
        f^n
    Inputs
        f:ComplexMap
            whose source and target are the same complex
        n:ZZ
    Outputs
        :ComplexMap
            the composition of $f$ with itself $n$ times.
    Description
        Text
            A complex map $f : C \to C$ can be composed with itself.
            This method produces these new maps of chain complexes.
        Text
            The differential on a chain complex always composes with itself to give the 
            zero map.
        Example
            S = ZZ/101[a..c];
            C = freeResolution coker matrix{{a^2, b^2, c^2}}
            f = dd^C
            f^2
            assert(source f == target f)
            assert(degree f == -1)
            assert(degree f^2 == -2)
        Example
            g = randomComplexMap(C, C, Degree => -1)
            g^2
            g^3
            assert(g^4 == 0)
        Text
            The zero-th power returns the identity map
        Example
            f^0 == id_C
            g^0 == id_C
        Text
            When $n$ is negative, the result is the $n$-fold power
            of the inverse complex map, if it exists.
        Example
            h = randomComplexMap(C, C)
            h^-1
            assert(h * h^-1 == id_C)
            h^-4
            assert(h^-4 * h^4 == id_C)
    SeeAlso
        (symbol^, Matrix, ZZ)
        (symbol^, Complex, ZZ)
///

doc ///
   Key
     (symbol ==, ComplexMap, ComplexMap)
     (symbol ==, ComplexMap, ZZ)
     (symbol ==, ZZ, ComplexMap)
   Headline
     whether two complex maps are equal
   Usage
     f == g
     f == 0
     f == 1
   Inputs
     f:ComplexMap
       or 0, or 1.
     g:ComplexMap
       or 0, or 1.
   Outputs
     :Boolean
       that is true when {\tt f} and {\tt g} are equal
   Description
     Text
       Two complex maps are equal if they have the same source,
       the same target, and $f_i = g_i$ for all $i$.
     Example
       S = ZZ/101[a..c]
       C = freeResolution coker vars S
       f = id_C
       assert(f == 1)
       f === id_C[-1][1]
       f == id_C[-1][1]
     Text
       A complex map is equal to zero if all the maps are zero.
       This could require computation to determine if something that
       is superficially not zero is in fact zero.
     Example
       assert(0 * id_C == 0)
     Example
       g = randomComplexMap(C, C)
       h = canonicalMap(coker g, target g)
       assert(h == 0)
     Text
       Testing whether a map is equal to 1 is a shorthand for determining
       if the complex map is the identity.
       Although the matrices may appear to be the identity, the map is not the
       identity when the source and target are not equal.
     Example
       g = randomComplexMap(C, C, InternalDegree=>1, Cycle=>true)
       h = canonicalMap(coker g, target g)
       assert(h != 1)
     Text
       Testing for equality is not the same testing for isomorphism.
       In particular, different presentations of a complex need not be equal.
     Example
       D = prune image g
       p = D.cache.pruningMap
       p == 1
       assert(coker p == 0 and ker p == 0)
       assert(prune p == 1)
   SeeAlso
     (symbol ==, Complex, Complex)
     (symbol SPACE, ComplexMap, Array)
     randomComplexMap
     canonicalMap
     (prune, Complex)
///

doc ///
  Key
    (isCommutative, ComplexMap)
  Headline
    whether a complex map commutes with the differentials
  Usage
    isCommutative f
  Inputs
    f:ComplexMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the differentials
  Description
    Text
      For a complex map $f : C \to D$ of degree $d$, this method
      checks whether, for all $i$, we have
      $dd^D_{i+d} * f_i = (-1)^d * (f_{i-1} * dd^C_i)$.
    Text
      We first construct a random complex map which commutes with the differential.
    Example
      S = ZZ/101[a,b,c];
      C = freeResolution coker vars S
      D = C ** C
      f1 = randomComplexMap(D, C, Boundary => true, InternalDegree => 1)
      isCommutative f1
      assert(degree f1 == 0)
      assert isNullHomotopic f1
      assert(source f1 == C and target f1 == D)
    Text
      We next generate a complex map that is commutative and (likely) 
      induces a nontrivial map on homology.
    Example
      f2 = randomComplexMap(D, C, Cycle => true)
      isCommutative f2
      assert(degree f2 == 0)
      assert isComplexMorphism f2
    Text
      When the degree of the complex map is odd, isCommutative determines
      whether the map is anti-commutative.  We illustrate
      this for one square.
    Example
      f3 = randomComplexMap(D, C, Cycle => true, Degree=>1, InternalDegree => 1)
      isCommutative f3
      assert(degree f3 == 1)
      part1 = dd^D_3 * f3_2
      part2 = f3_1 * dd^C_2
      assert(part1 + part2 == 0)
    Text
      If the @TO "debugLevel"@ is greater than zero, then
      the location of the first failure of commutativity is displayed.
    Example
      f4 = randomComplexMap(D, C)
      isCommutative f4
      debugLevel = 1
      isCommutative f4
  SeeAlso
    isComplexMorphism
    randomComplexMap
    freeResolution
///

doc ///
  Key
    (isComplexMorphism, ComplexMap)
    isComplexMorphism
  Headline
    whether a complex map is a morphism of complexes
  Usage
    isComplexMorphism f
  Inputs
    f:ComplexMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the differentials and has degree $0$
  Description
    Text
      For a complex map $f : C \to D$ of degree $d$, this method
      checks whether $d = 0$ and, for all $i$, we have
      $dd^D_{i+d} * f_i = (-1)^d * (f_{i-1} * dd^C_i)$.
    Text
      We first construct a random complex morphism.
    Example
      S = ZZ/101[a,b,c];
      C = freeResolution coker vars S
      D = C ** C
      f1 = randomComplexMap(D, C, Boundary => true, InternalDegree => 1)
      isComplexMorphism f1
      assert(degree f1 == 0)
      assert isNullHomotopic f1
      assert(source f1 == C and target f1 == D)
    Text
      We next generate a complex morphism that (likely) 
      induces a nontrivial map on homology.
    Example
      f2 = randomComplexMap(D, C, Cycle => true)
      isComplexMorphism f2
      assert(degree f2 == 0)
      assert isComplexMorphism f2
    Text
      When the degree is non-zero, the map is not a complex morphism.
      If the @TO "debugLevel"@ is greater than zero, then
      information about the failure is displayed.
    Example
      f3 = randomComplexMap(D, C, Cycle => true, Degree=>1, InternalDegree => 1)
      assert(degree f3 == 1)
      isComplexMorphism f3
      debugLevel = 1
      isComplexMorphism f3
      assert isCommutative f3
    Example
      f4 = randomComplexMap(D, C)
      assert(degree f4 == 0)
      debugLevel = 0
      isComplexMorphism f4
      debugLevel = 1
      isComplexMorphism f4
  SeeAlso
    (isCommutative, ComplexMap)
    randomComplexMap
    freeResolution
///

doc ///
    Key
        (Hom, ComplexMap, ComplexMap)
        (Hom, Complex, ComplexMap)
        (Hom, Complex, Matrix)
        (Hom, ComplexMap, Module)
        (Hom, ComplexMap, Matrix)
        (Hom, ComplexMap, Complex)
        (Hom, ComplexMap, Ring)
        (Hom, Matrix, Complex)
        (Hom, Matrix, ComplexMap)
        (Hom, Module, ComplexMap)
        (Hom, Ring, ComplexMap)
    Headline
        the map of complexes between Hom complexes
    Usage
        h = Hom(f,g)
    Inputs
        f:ComplexMap
        g:ComplexMap
    Outputs
        h:ComplexMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of chain complexes induces the map
            $h = Hom(f,g) : Hom(D,E) \to Hom(C,F)$ defined by $\phi \mapsto g \phi f$.
        Example
            S = ZZ/101[a..c];
            C = freeResolution coker vars S
            D = (freeResolution coker matrix{{a^2,a*b,b^3}})[-1]
            f = randomComplexMap(D,C)
            E = (dual C)[-3]
            F = (dual D)[-3]
            g = randomComplexMap(F,E)
            h = Hom(f,g)
            assert isWellDefined h
            assert(source h === Hom(D,E))
            assert(target h === Hom(C,F))
        Text
            We illustrate the defining property of the map $h$ on a random element $\phi$
            in degree zero.
        Example
            e = randomComplexMap(source h, complex(S^1))
            phi = homomorphism e
            psi = homomorphism'(g * phi * f)
            assert(h*e == psi)
        Text
            If either of the arguments is a @TO "Complex"@, that argument is
            understood to be the identity map on that complex.
        Example
            assert(Hom(f, C) == Hom(f, id_C))
            assert(Hom(C, f) == Hom(id_C, f))
        Text
            If either of the arguments is a @TO "Module"@ or a @TO "Ring"@, that argument is
            understood to be the identity map on the complex having a unique non-zero term in 
            in homological degree 0.  The ring must be the underlying ring of the map of complexes.
        Example
            assert(Hom(f, S) == Hom(f, id_(complex S)))
            assert(Hom(S, f) == Hom(id_(complex S), f))
            M = S^1/(a^2, b^2, c^2)
            assert(Hom(f, M) == Hom(f, id _ (complex M)))
            assert(Hom(M, f) == Hom(id _ (complex M), f))
        Text
            If either of the arguments is a @TO "Matrix"@, that argument is
            understood to be a map of complexes whose source and target have a unique non-zero entry
            in homological degree 0.
        Example
            m = vars S;
            h1 = Hom(f, m)
            assert(h1 == Hom(f, map(complex target m, complex source m, i -> m)))
            m = vars S;
            h2 = Hom(m, f)
            assert(h2 == Hom(map(complex target m, complex source m, i -> m), f))
        Text
            XXX write this text after writing doc for homomorphism and homomorphism'.
        Example
            e = randomComplexMap(source h, complex(S^1, Base => -1))
            phi = homomorphism e
            assert(degree phi == -1)
            psi = homomorphism'(g * phi * f)
            i = map(complex S^1, source e, id_(source e), Degree => 1)
            assert(h*e == psi*i)
            assert((degree h, degree e, degree psi, degree i) === (0, 0, -1, 1))
        Text
            This routine is functorial.
        Example
            D' = (freeResolution coker matrix{{a^2,a*b,c^3}})[-1]
            f' = randomComplexMap(D', D)
            Hom(f' * f, g) == Hom(f, id_F) * Hom(f', g)
            Hom(f' * f, g) == Hom(f, g) * Hom(f', id_E)
            F' = dual (freeResolution coker matrix{{a^2,a*b,a*c,b^3}})[-3]
            g' = randomComplexMap(F', F)
            Hom(f, g' * g) == Hom(f, g') * Hom(id_D, g)
            Hom(f, g' * g) == Hom(id_C, g') * Hom(f, g)
    SeeAlso
        (homomorphism, ComplexMap)
        (homomorphism', ComplexMap)
        (randomComplexMap, Complex, Complex)
        (Hom, Complex, Complex)
///

doc ///
    Key
        (dual, ComplexMap)
    Headline
        the dual of a map of complexes
    Usage
        h = dual f
    Inputs
        f:ComplexMap
    Outputs
        h:ComplexMap
    Description
        Text
            The map $f : C \to D$ of chain complexes over the ring $S$ induces the map
            $h = Hom(f, S^1) : Hom(D, S^1) \to Hom(C,S^1)$ defined by $\phi \mapsto \phi f$.
        Example
            S = ZZ/101[a..c]
            C = freeResolution coker vars S
            D = (freeResolution coker matrix{{a^2,a*b,b^3}})[-1]
            f = randomComplexMap(D,C)
            h = dual f
            assert isWellDefined h
            assert(h == Hom(f, S^1))
            assert(source h == Hom(D,S^1))
            assert(target h == Hom(C,S^1))
        Text
            This routine is functorial.
        Example
            D' = (freeResolution coker matrix{{a^2,a*b,c^3}})[-1]
            f' = randomComplexMap(D', D)
            dual(f' * f) == dual f * dual f'
    SeeAlso
        (Hom, ComplexMap, ComplexMap)
        (randomComplexMap, Complex, Complex)
        (dual, Matrix)
///

doc ///
    Key
        (symbol**, ComplexMap, ComplexMap)
        (tensor, ComplexMap, ComplexMap)
        (symbol**, Complex, ComplexMap)
        (symbol**, ComplexMap, Complex)
        (symbol**, ComplexMap, Module)
        (symbol**, Module, ComplexMap)
    Headline
        the map of complexes between tensor complexes
    Usage
        h = f ** g
        h = tensor(f, g)
    Inputs
        f:ComplexMap
        g:ComplexMap
    Outputs
        h:ComplexMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of chain complexes induces the map
            $h = f \otimes g : C \otimes E \to D \otimes F$ defined by $c \otimes e \mapsto f(c) \otimes g(e)$.
        Example
            S = ZZ/101[a..c]
            C = freeResolution coker vars S
            D = (freeResolution coker matrix{{a^2,a*b,b^3}})[-1]
            f = randomComplexMap(D,C)
            E = (dual C)[-3]
            F = (dual D)[-3]
            g = randomComplexMap(F,E)
            h = f ** g
            assert isWellDefined h
            assert(source h === C ** E)
            assert(target h === D ** F)
        Text
            If one argument is a Complex or Module,
            then the identity map of the corresponding complex is used.
        Example
            fE = f ** E
            assert(fE === f ** id_E)
            k = coker vars S
            gk = g ** k
            assert(gk == g ** id_(complex k))
        Text
            This routine is functorial.
        Example
            D' = (freeResolution coker matrix{{a^2,a*b,c^3}})[-1]
            f' = randomComplexMap(D', D)
            (f' * f) ** g == (f' ** g) * (f ** id_E)
            (f' * f) ** g == (f' ** id_F) * (f ** g)
            F' = dual (freeResolution coker matrix{{a^2,a*b,a*c,b^3}})[-3]
            g' = randomComplexMap(F', F)
            f ** (g' * g) == (f ** g') * (id_C ** g)
            f ** (g' * g) == (id_D ** g') * (f ** g)
    SeeAlso
        (symbol**, Complex, Complex)
        (randomComplexMap, Complex, Complex)
        (Hom, Complex, Complex)
///

doc ///
    Key
        (truncate, List, ComplexMap)
        (truncate, ZZ, ComplexMap)
    Headline
        truncation of a complex map at a specified degree or set of degrees
    Usage
        truncate(d, f)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        f:ComplexMap
            that is homogeneous over $R$
    Outputs
        :ComplexMap
            a complex map over $R$ whose terms in the source and target
            consist of all elements of component-wise degree at least {\tt d}.
    Description
        Text
            Truncation of homogeneous (graded) maps induces a natural
            operation on maps of chain complexes.
        Text
            In the singly graded case, the truncation of a homogeneous
            module $M$ at degree $d$ is generated by all homogeneous
            elements of degree at least $d$ in $M$.  The truncation of
            a map between homogeneous modules is the induced map
            between the truncation of the source and the truncation of
            the target.  This method applies this operation to each
            term in a map of chain complexes.
        Example
            R = QQ[a,b,c];
            C = freeResolution ideal(a*b, a*c, b*c)
            D = (freeResolution ideal(a*b, a*c, b*c, a^2-b^2))[-1]
            f = randomComplexMap(D,C, Cycle => true)
            g = truncate(3,f)
            assert isWellDefined g
            assert (source g == truncate(3, source f))
            assert (target g == truncate(3, target f))
        Text
            Truncating at a degree less than the minimal generators
            is the identity operation.
        Example
            assert(f == truncate(0, f))
        Text
            In the multi-graded case, the truncation of a homogeneous module at 
            a list of degrees is generated by all homogeneous elements of degree
            that are component-wise greater than or equal to at least one
            of the degrees.  As in the singly graded case, this induces a map between
            the truncations the source and target.
        Example
            A = ZZ/101[x_0, x_1, y_0, y_1, y_2, Degrees => {2:{1,0}, 3:{0,1}}];
            I = intersect(ideal(x_0, x_1), ideal(y_0, y_1, y_2))
            C = freeResolution I
            J = intersect(ideal(x_0^2, x_1^2), ideal(y_0^2, y_1^2, y_2^2))
            D = freeResolution J
            f = extend(C, D, id_(A^1))
            g1 = prune truncate({{1,1}}, f)
            g2 = truncate({{1,0}}, f)
            g3 = truncate({{0,1}}, f)
            g4 = truncate({{1,0},{0,1}}, f)
            g5 = truncate({{2,2}}, f)
            assert all({g1,g2,g3,g4,g5}, isWellDefined)
    SeeAlso
        "Making maps between chain complexes"
        (truncate, List, Matrix)
        (truncate, List, Complex)
        (canonicalTruncation, ComplexMap, Sequence)
        (naiveTruncation, ComplexMap, ZZ, ZZ)
        (part, List, ComplexMap)
///

doc ///
    Key
        (naiveTruncation, ComplexMap, ZZ, ZZ)
        (naiveTruncation, ComplexMap, InfiniteNumber, ZZ)
        (naiveTruncation, ComplexMap, Nothing, ZZ)
        (naiveTruncation, ComplexMap, Sequence)
        (naiveTruncation, ComplexMap, ZZ, InfiniteNumber)
        (naiveTruncation, ComplexMap, ZZ, Nothing)
        (naiveTruncation, ComplexMap, InfiniteNumber, InfiniteNumber)
        (naiveTruncation, ComplexMap, Sequence, Sequence)
    Headline
        drops all terms in the source of a complex outside a given interval
    Usage
        naiveTruncation(f, lo, hi)
        naiveTruncation(f, (lo, hi))
    Inputs
        f:ComplexMap
        lo:ZZ
            or {\tt -infinity} or {\tt null} (the latter two give no lower bound)
        hi:ZZ
            or {\tt infinity} or {\tt null} (the latter two give no upper bound)
    Outputs
        :ComplexMap
    Description
        Text
            Naive truncations of complexes are functorial.  Given a map of
            chain complexes, this method returns the canonical map
            from the naive truncation of the source to the naive truncation of the
            target.  The degree of {\tt f} is used to determine the appropriate naive 
            truncation of the target complex.

            First, we define some non-trivial maps of chain complexes.
        Example
            R = ZZ/101[a..d];
            C = (freeResolution coker matrix{{a,b,c}})[1]
            D = freeResolution coker matrix{{a*b,a*c,b*c}}
            E = freeResolution coker matrix{{a^2,b^2,c*d}}
            f = randomComplexMap(D, C)
            g = randomComplexMap(E, D)
            h = g * f
        Text
            We use these maps to illustrate naive truncation.
        Example
            tf = naiveTruncation(f, 0, 1)
            tg = naiveTruncation(g, (0, 1))
            th = naiveTruncation(h, (0, 1))
            assert all({tf, tg, th}, isWellDefined)
            assert(th == tg * tf)
        Example
            t2f = naiveTruncation(f, -infinity, 1)
            assert(t2f == naiveTruncation(f,, 1))
            assert(tf != t2f)
        Text
            It is also possible to truncate the source and target independently.
        Example
            t2f = naiveTruncation(f, (0,1), (1,2))
            assert(source t2f == naiveTruncation(C, (1,2)))
            assert(target t2f == naiveTruncation(D, (0,1)))
        Text
            There is another type of truncation, 
            @TO2 ((canonicalTruncation, ComplexMap, Sequence),
                "canonical truncation")@, which induces an isomorphism
                on (a range) of homology.
    SeeAlso
        "Making maps between chain complexes"
        (naiveTruncation, Complex, Sequence)
        (canonicalTruncation, Complex, ZZ, ZZ)
        (canonicalTruncation, ComplexMap, ZZ, ZZ)
        (truncate, List, ComplexMap)
///

doc ///
    Key
        (canonicalTruncation, ComplexMap, ZZ, ZZ)
        (canonicalTruncation, ComplexMap, InfiniteNumber, ZZ)
        (canonicalTruncation, ComplexMap, Nothing, ZZ)
        (canonicalTruncation, ComplexMap, Sequence)
        (canonicalTruncation, ComplexMap, ZZ, InfiniteNumber)
        (canonicalTruncation, ComplexMap, InfiniteNumber, InfiniteNumber)
        (canonicalTruncation, ComplexMap, ZZ, Nothing)
    Headline
        reducing the number of non-zero terms of a complex
    Usage
        canonicalTruncation(f, (lo, hi))
    Inputs
        f:ComplexMap
        lo:ZZ
            or {\tt -infinity} or {\tt null} (the latter two give no lower bound)
        hi:ZZ
            or {\tt infinity} or {\tt null} (the latter two give no upper bound)
    Outputs
        :ComplexMap
    Description
        Text
            Returns a new complex map which drops (sets to zero) all modules 
            outside the given range in the source, and modifies the ends to preserve homology
            in the given range.  The degree of the map {\tt f} is used to
            determine the truncation of the target.

            First, we define some non-trivial maps of chain complexes.
        Example
            R = ZZ/101[a..d];
            C = (freeResolution coker matrix{{a,b,c}})[1]
            D = freeResolution coker matrix{{a*b,a*c,b*c}}
            E = freeResolution coker matrix{{a^2,b^2,c*d}}
            f = randomComplexMap(D, C)
            g = randomComplexMap(E, D)
            h = g * f
        Text
            We use these maps to illustrate canonical truncation.
        Example
            tf = canonicalTruncation(f, (0, 1))
            tg = canonicalTruncation(g, (0, 1))
            th = canonicalTruncation(h, (0, 1))
            assert all({tf, tg, th}, isWellDefined)
            assert(th == tg * tf)
        Example
            t2f = canonicalTruncation(f, (-infinity, 1))
            assert(t2f == canonicalTruncation(f, (, 1)))
            assert(tf != t2f)
        Text
            There is another type of truncation, @TO2
            ((naiveTruncation, ComplexMap, Sequence), "naive
            truncation")@, which yields a short exact sequence of
            complexes.
    SeeAlso
        "Making maps between chain complexes"
        (canonicalTruncation, Complex, Sequence)
        (naiveTruncation, Complex, ZZ, ZZ)
        (naiveTruncation, ComplexMap, ZZ, ZZ)
        (truncate, List, ComplexMap)
///

doc ///
    Key
        (symbol SPACE, RingMap, ComplexMap)
    Headline
        apply a ring map to a map of complexes
    Usage
        phi f
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:ComplexMap
            over the ring $R$
    Outputs
        :ComplexMap
            over the ring $S$
    Description
        Text
            We illustrate the image of a complex map along a ring map.
        Example
            R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = freeResolution I
            CJ = freeResolution J
            f = extend(CJ, CI, map(CJ_0, CI_0, 1))
            assert isWellDefined f
            g = phi f
            assert isWellDefined g
            dd^(source g)
            dd^(target g)
            prune HH g
    SeeAlso
        (symbol SPACE, RingMap, Complex)
        (symbol **, RingMap, ComplexMap)
///

doc ///
    Key
        (symbol**, RingMap, ComplexMap)
        (tensor, RingMap, ComplexMap)
    Headline
        tensor a map of complexes along a ring map
    Usage
        phi ** f
        tensor(phi, f)
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:ComplexMap
            over the ring $R$
    Outputs
        :ComplexMap
            over the ring $S$
    Description
        Text
            We illustrate the tensor product of a map of complexes along a ring map.
        Example
            R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = freeResolution I
            CJ = freeResolution J
            f = extend(CJ, CI, map(CJ_0, CI_0, 1))
            assert isWellDefined f
            g = phi ** f
            assert isWellDefined g
            dd^(source g)
            dd^(target g)
            prune HH g
    SeeAlso
        (symbol **, RingMap, Complex)
        (symbol SPACE, RingMap, ComplexMap)
///
 

doc ///
    Key
        resolutionMap
        (resolutionMap, Complex)
    Headline
        map from a free resolution to the given complex
    Usage
        resolutionMap C
    Inputs
        C:Complex
        LengthLimit => ZZ
          this is used to limit somehow the computation where resolutions might be too long or infinite
        DegreeLimit =>
          unused
        FastNonminimal =>
          unused
        HardDegreeLimit =>
          unused
        PairLimit =>
          unused
        SortStrategy =>
          unused
        StopBeforeComputation =>
          unused
        Strategy =>
          unused
        SyzygyLimit => 
          unused
    Outputs
        :ComplexMap
            a quasi-isomorphism whose source is a free resolution and whose target is $C$
    Description
        Text
            Given a complex $C$, this method produces the natural quasi-isomorphism
            from a complex $F$ all of whose terms are free modules to the complex $C$.
            The algorithm used minimizes the ranks of the free modules in $F$.
        Example
            R = ZZ/101[a,b,c];
            I = ideal(a^2, a*b, b*c)
            C = Hom(freeResolution I, R^1/I)
            assert all(min C .. max C, i -> not isFreeModule C_i)
            fC = resolutionMap C
            FC = resolution C
            prune HH FC
            assert isQuasiIsomorphism fC
            assert isFree FC
            assert isWellDefined fC
            assert(0 == coker fC) -- showing that fC is surjective.
        Text
            The resolution of a short exact sequence is simply the 
            zero complex.
        Example
            J = ideal(a,b)
            K = ideal(b^2,c)
            g1 = map(R^1/(J+K), R^1/J ++ R^1/K, {{1,-1}})
            g2 = map(R^1/J ++ R^1/K, R^1/(intersect(J,K)), {{1},{1}})
            D = complex{g1, g2}
            assert isWellDefined D
            assert isShortExactSequence(g1,g2)
            fD = resolutionMap D
            assert isWellDefined fD
            assert isQuasiIsomorphism fD
            assert(0 == source fD) -- so fD is certainly not surjective!
        Text
            To avoid recomputation, this method caches its value.
    SeeAlso
        (resolution, Complex)
        (freeResolution, Module)
        (isQuasiIsomorphism, ComplexMap)
///

doc ///
    Key
        augmentationMap
        (augmentationMap, Complex)
    Headline
        map from a free resolution to a module regarded as a complex
    Usage
        augmentationMap C
    Inputs
        C:Complex
            constructed via @TO freeResolution@
    Outputs
        :ComplexMap
            a quasi-isomorphism whose source is $C$ and whose target is the module
            resolved by $C$
    Description
        Text
            Given a complex $C$, this method produces the natural quasi-isomorphism
            from a complex $F$ all of whose terms are free modules to the complex $C$.
            The algorithm used minimizes the ranks of the free modules in $F$.
        Example
            R = ZZ/101[a,b,c];
            I = ideal(a^2, a*b, b*c)
            C = freeResolution I
            f = augmentationMap C
            assert isWellDefined f
            assert isComplexMorphism f
            assert isQuasiIsomorphism f
        Example
            g = resolutionMap complex comodule I
            assert(f == g)
    SeeAlso
        (resolutionMap, Complex)
        (freeResolution, Module)
        (isQuasiIsomorphism, ComplexMap)
///

doc ///
    Key
        canonicalMap
        (canonicalMap, Complex, Complex)
        [canonicalMap, UseTarget]
    Headline
        gets the natural map arising from various constructions
    Usage
        g = canonicalMap(D, C)
    Inputs
        C:Complex
        D:Complex
        UseTarget => Boolean
            determines the choice of canonical map
            when $D$ is a cylinder of a map $f$
            and the source and target of $f$ are the same
    Outputs
        g:ComplexMap
    Description
        Text
            A canonical map, also called a natural map, is a 
            map that arises naturally from the definition or
            the construction of the object.
            
            The following six constructions are supported: kernel, 
            cokernel, image, coimage, cone, and cylinder.
        Text
            The @TO2((kernel, ComplexMap), "kernel of a complex map")@ 
            comes with a natural injection into the source complex.
            This natural map is always a complex morphism.
        Example
            R = ZZ/101[a,b,c,d];
            D = freeResolution coker vars R
            C = (freeResolution coker matrix"a,b,c")[1]
            f = randomComplexMap(D, C, Cycle=>true)
            assert isComplexMorphism f
            K1 = kernel f
            g = canonicalMap(source f, K1)
            degree g
            assert(isWellDefined g and isComplexMorphism g)
        Example
            f2 = randomComplexMap(D, C)
            assert not isComplexMorphism f2
            K2 = kernel f2
            g2 = canonicalMap(source f2, K2)
            assert(isWellDefined g2 and isComplexMorphism g2)
        Text
            The @TO2((cokernel, ComplexMap), "cokernel of a complex map")@
            comes with a natural surjection from the target complex.
        Example
            Q = cokernel f
            g3 = canonicalMap(Q, target f)
            assert(isWellDefined g3 and isComplexMorphism g3)
        Text
            The @TO2((image, ComplexMap), "image of a complex map")@ 
            comes with a natural injection into the target complex.
        Example
            I = image f
            g4 = canonicalMap(target f, I)
            assert(isWellDefined g4 and isComplexMorphism g4)
        Text
            The @TO2((coimage, ComplexMap), "coimage of a complex map")@
            comes with a natural surjection from the source complex.
            This natural map is always a complex morphism.
        Example
            J = coimage f
            g5 = canonicalMap(J, source f)
            assert(isWellDefined g5 and isComplexMorphism g5)
        Example
            J2 = coimage f2
            g6 = canonicalMap(J2, source f2)
            assert(isWellDefined g6 and isComplexMorphism g6)
        Text
            The @TO2((cone, ComplexMap), "cone of a complex morphism")@
            comes with two natural maps.  Given a map $f : C \to D$,
            let $E$ denote the cone of $f$.  The first is a natural
            injection from the target $D$ of $f$ into $E$.  The
            second is a natural surjection from $E$ to $C[-1]$.
            Together, these maps form a short exact sequence of
            complexes.
        Example
            E = cone f
            g = canonicalMap(E, target f)
            h = canonicalMap((source f)[-1], E)
            assert(isWellDefined g and isWellDefined h)
            assert(isComplexMorphism g and isComplexMorphism h)
            assert isShortExactSequence(h,g)
        Text
            The @TO2((cylinder, ComplexMap), "cylinder of a complex
            map")@ comes with four natural maps.  Given a map $f : C
            \to D$, let $F$ denote the cylinder of $f$.  The first is
            the natural injection from the source $C$ of $f$ into the
            cylinder $F$.  Together these two maps form a short exact
            sequence of complexes.
        Example
            F = cylinder f
            g = canonicalMap(F, source f)
            h = canonicalMap(E, F)
            assert(isWellDefined g and isWellDefined h)
            assert(isComplexMorphism g and isComplexMorphism h)
            assert isShortExactSequence(h,g)
        Text
            The third is the natural injection from the target $D$ of $F$
            into the cylinder $F$.
            The fourth is the natural surjection from the cylinder $F$ to the 
            target $D$ of $f$.
            However, these two maps do not form a short exact
            sequence of complexes.
        Example
            g' = canonicalMap(F, target f)
            h' = canonicalMap(target f, F)
            assert(isWellDefined g' and isWellDefined h')
            assert(isComplexMorphism g' and isComplexMorphism h')
            assert not isShortExactSequence(h',g')
        Text
            When $D == C$, the optional argument {\tt UseTarget} 
            selects the appropriate natural map.
        Example
            f' = id_C
            F' = cylinder f'
            g = canonicalMap(F', C, UseTarget=>true)
            h = canonicalMap(F', C, UseTarget=>false)
            assert(isWellDefined g and isWellDefined h)
            assert(g != h)
            assert(isComplexMorphism g and isComplexMorphism h)
    SeeAlso
        (inducedMap, Complex, Complex)
///

doc ///
    Key
        (inducedMap, Complex, Complex)
    Headline
        make the map of complexes induced at each term by the identity map
    Usage
        f = inducedMap(D, C)
    Inputs
        C:Complex
        D:Complex
        Degree => ZZ
            specify the degree of the map of complexes, if not 0
        Verify => Boolean
            if true, check that the resulting maps are well-defined
    Outputs
        f:ComplexMap
    Description
        Text
            Let $d$ be the value of the optional argument {\tt
            Degree}, or zero, if not given.  For each $i$, the terms
            $D_{i+d}$ and $C_i$ must be subquotients of the same
            ambient free module.  This method returns the complex map
            induced by the identity on each of these free modules.
            
            If {\tt Verify => true} is given, then this method
            also checks that these identity maps induced well-defined 
            maps.  This can be a relatively expensive computation.
        Text
            We illustrate this method by truncating a free resolution
            at two distinct internal degrees.  We check that 
            the various induced maps compose to give another
            induced map.
        Example
            kk = ZZ/32003
            R = kk[a,b,c]
            F = freeResolution (ideal gens R)^2
            C1 = truncate(3, F)
            C2 = truncate(4, F)
            assert isWellDefined C1
            assert isWellDefined C2
            f = inducedMap(C1, C2)
            assert isWellDefined f
            f1 = inducedMap(F, C1)
            f2 = inducedMap(F, C2)
            assert isWellDefined f1
            assert isWellDefined f2
            assert(f2 == f1 * f)
    SeeAlso
        (inducedMap, Module, Module)
///

doc ///
    Key
        "arithmetic with complex maps"
        (symbol+, ComplexMap, ComplexMap)
        (symbol+, RingElement, ComplexMap)
        (symbol+, Number, ComplexMap)
        (symbol+, ComplexMap, RingElement)
        (symbol+, ComplexMap, Number)
        (symbol-, ComplexMap)
        (symbol-, ComplexMap, ComplexMap)
        (symbol-, RingElement, ComplexMap)
        (symbol-, Number, ComplexMap)
        (symbol-, ComplexMap, RingElement)
        (symbol-, ComplexMap, Number)
        (symbol*, RingElement, ComplexMap)
    Headline
        perform arithmetic operations on complex maps
    Usage
        f + g
        a + f
        f + a
        -f
        f - g
        a - f
        f - a
        a * f
    Inputs
        f:ComplexMap
        g:ComplexMap
        a:RingElement
          that is, an element in the underlying ring or a number
    Outputs
        :ComplexMap
    Description
        Text
            The set of complex maps forms a module over the underlying @TO2((ring, ComplexMap), "ring")@.
            These methods implement the basic operations of addition, subtraction, and scalar multiplication.
        Example
            R = ZZ/101[a..d]
            C = freeResolution coker matrix{{a*b, a*c^2, b*c*d^3, a^3}}
            D = freeResolution coker matrix{{a*b, a*c^2, b*c*d^3, a^3, a*c*d}}
            f = randomComplexMap(D, C, Cycle => true)
            g = randomComplexMap(D, C, Boundary => true)
        Example
            f+g
            f-g
            -f
            3*f
            0*f
            a*f
            assert(0*f == 0)
            assert(1*f == f)
            assert((-1)*f == -f)
            assert(-(f-g) == g-f)
            assert((a+b)*f == a*f + b*f)
            assert(a*(f+g) == a*f + a*g)
            assert isComplexMorphism (f+g)
        Text
            Adding or subtracting a scalar is the same as adding or subtracting the
            scalar multiple of the identity.  In particular, the source and target must be equal.
        Example
            h = randomComplexMap(C, C)
            h+1
            assert(h+1 == h + id_C)
            assert(h+a == h + a*id_C)
            assert(1-h == id_C - h)
            assert(b-c*h == -c*h + b*id_C)
        Text
            Arithmetic on differentials can be a useful method
            for constructing new chain complexes.
        Example
            E = complex(-dd^C)
            isWellDefined E
            assert(dd^E == map(E, E, -dd^C))
    SeeAlso
        "Making maps between chain complexes"
        randomComplexMap
        (complex, ComplexMap)
        (map, Complex, Complex, ComplexMap)
///

doc ///
    Key
        (symbol|, ComplexMap, ComplexMap)
    Headline
        join or concatenate maps horizontally
    Usage
        f | g
    Inputs
        f:ComplexMap
        g:ComplexMap
    Outputs
        :ComplexMap
    Description
        Text
            Given complex maps with the same target,
            this method constructs the associated map
            from the direct sum of the sources to the target.

            First, we define some non-trivial maps of chain complexes.
        Example
            R = ZZ/101[a..d];
            C1 = (freeResolution coker matrix{{a,b,c}})[1]
            C2 = freeResolution coker matrix{{a*b,a*c,b*c}}
            D = freeResolution coker matrix{{a^2,b^2,c*d}}
            f = randomComplexMap(D, C1)
            g = randomComplexMap(D, C2)
        Example
            h = f|g
            assert isWellDefined h
            assert(source h === source f ++ source g)
            assert(target h === target f)
        Text
            This is really a shorthand for constructing complex maps via block matrices.
        Example
            assert(h === map(D, C1 ++ C2, {{f,g}}))
    SeeAlso
        (symbol++, Complex, Complex)
        (symbol++, ComplexMap, ComplexMap)
        (symbol||, ComplexMap, ComplexMap)
        (symbol|, Matrix, Matrix)
        (map, Complex, Complex, List)
///

doc ///
    Key
        (symbol||, ComplexMap, ComplexMap)
    Headline
        join or concatenate maps vertically
    Usage
        f || g
    Inputs
        f:ComplexMap
        g:ComplexMap
    Outputs
        :ComplexMap
    Description
        Text
            Given complex maps with the same source,
            this method constructs the associated map
            from the source to the direct sum of the targets.

            First, we define some non-trivial maps of chain complexes.
        Example
            R = ZZ/101[a..d];
            D1 = (freeResolution coker matrix{{a,b,c}})[1]
            D2 = freeResolution coker matrix{{a*b,a*c,b*c}}
            C = freeResolution coker matrix{{a^2,b^2,c*d}}
            f = randomComplexMap(D1, C)
            g = randomComplexMap(D2, C)
        Example
            h = f||g
            assert isWellDefined h
            assert(target h === target f ++ target g)
            assert(source h === source f)
        Text
            This is really a shorthand for constructing complex maps via block matrices.
        Example
            assert(h === map(D1 ++ D2, C, {{f},{g}}))
    SeeAlso
        (symbol++, Complex, Complex)
        (symbol++, ComplexMap, ComplexMap)
        (symbol|, ComplexMap, ComplexMap)
        (symbol||, Matrix, Matrix)
        (map, Complex, Complex, List)
///

doc ///
    Key
        (symbol++, ComplexMap, ComplexMap)
        (directSum, ComplexMap)
    Headline
        direct sum of complex maps
    Usage
        h = f ++ g
        h = directSum(f,g,...)
        h = directSum(name1 => f, name2 => g, ...)
    Inputs
        f:ComplexMap
        g:ComplexMap
    Outputs
        h:ComplexMap
          that is the direct sum of the input chain  complex maps
    Description
        Text
            The direct sum of two complex maps is a a complex map
            from the direct sum of the sources to the direct sum of
            the targets.

            First, we define some non-trivial maps of chain complexes.
        Example
            R = ZZ/101[a..d];
            C1 = (freeResolution coker matrix{{a,b,c}})[1]
            C2 = freeResolution coker matrix{{a*b,a*c,b*c}}
            D1 = (freeResolution coker matrix{{a,b,c}})
            D2 = freeResolution coker matrix{{a^2, b^2, c^2}}[-1]
            f = randomComplexMap(D1, C1, Cycle => true)
            g = randomComplexMap(D2, C2, Cycle => true)
        Example
            h = f ++ g
            assert isWellDefined h
            assert(h == map(D1 ++ D2, C1 ++ C2, {{f,0},{0,g}}))
        Text
            The direct sum of any sequence of complex maps can be 
            computed as follows.
        Example
            directSum(f, g, f[2])
            h2 = directSum(mike => f, greg => g, dan => f[2])
            h2_[greg,dan]
            assert(source oo == C2 ++ C1[2])
        Text
            One can easily obtain the compositions with canonical
            injections and surjections.
        Example
            h_[0]^[0] == f
            h_[1]^[1] == g
            h_[0]^[1] == 0
            h_[1]^[0] == 0
        Example
            h_[0] == h * (C1 ++ C2)_[0]
            h_[1] == h * (C1 ++ C2)_[1]
            h^[0] == (D1 ++ D2)^[0] * h
            h^[1] == (D1 ++ D2)^[1] * h
    SeeAlso
        (symbol++, Complex, Complex)
        (symbol**, ComplexMap, ComplexMap)
        (Hom, ComplexMap, ComplexMap)
        (symbol_, ComplexMap, Array)
///

-- TODO for the next 4 nodes:
-- make better examples
-- add text (when are these actually defined?  Maybe change code)
-- also add SeeAlso canonicalMap's.
doc ///
  Key
    (image, ComplexMap)
  Headline
    make the image of a map of complexes
  Usage
    E = image f
  Inputs
    f : ComplexMap
  Outputs
    E : Complex
  Description
    Text
      If $f : C \to D$ is a map of chain complexes of degree $d$,
      then the image is the complex $E$ whose $i-th$ is $image(f_{i-d})$,
      and whose differential is induced from the differential 
      on the target.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the image.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      prune image g == D
      prune image h == C[-1]
    Text
      There is a canonical map of complexes from the image to the target.
    Example
      g1 = canonicalMap(target g, image g)
      ker g1 == 0
      image g1 == image g
      h1 = canonicalMap(target h, image h)
      ker h1 == 0
      image h1 == image h
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    image
    (coimage, ComplexMap)
    (kernel, ComplexMap)
    (cokernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (coimage, ComplexMap)
  Headline
    make the coimage of a map of complexes
  Usage
    coimage f
  Inputs
    f : ComplexMap
  Outputs
    : Complex
  Description
    Text
      The coimage of a chain complex map $f : C \to D$
      is the complex $E$ whose $i-th$ term is $coimage(f_i)$,
      and whose differential is induced from the differential 
      on the source.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the coimage.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      coimage g == D
      prune coimage h == C[-1]
    Text
      There is a canonical map of complexes from the source to the coimage.
    Example
      g1 = canonicalMap(coimage g, source g)
      coimage g1 == coimage g
      coker g1 == 0
      h1 = canonicalMap(coimage h, source h)
      coimage h1 == coimage h
      coker h1 == 0
  Caveat
    The coimage is more computationally intensive than @TO (image, ComplexMap)@
    because, unlike {\tt image}, it computes kernels of maps of modules.
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    coimage
    (image, ComplexMap)
    (kernel, ComplexMap)
    (cokernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (kernel, ComplexMap)
  Headline
    make the kernel of a map of complexes
  Usage
    kernel f
    ker f
  Inputs
    f : ComplexMap
  Outputs
    : Complex
  Description
    Text
      The kernel of a chain complex map $f : C \to D$
      is the complex $E$ whose $i-th$ term is $kernel(f_i)$,
      and whose differential is induced from the differential 
      on the source.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the kernel.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      ker g == 0
      prune ker h == D
    Text
      There is a canonical map of complexes from the kernel to the source.
    Example
      h1 = canonicalMap(source h, ker h)
      ker h == image h1
      ker h1 == 0
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    ker
    (image, ComplexMap)
    (coimage, ComplexMap)
    (cokernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (cokernel, ComplexMap)
  Headline
    make the cokernel of a map of complexes
  Usage
    cokernel f
    coker f
  Inputs
    f : ComplexMap
  Outputs
    : Complex
  Description
    Text
      If $f : C \to D$ is a map of chain complexes of degree $d$,
      then the cokernel is the complex $E$ whose $i-th$ is $cokernel(f_{i-d})$,
      and whose differential is induced from the differential 
      on the target.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the kernel.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      prune coker g == C[-1]
      coker h == 0
    Text
      There is a canonical map of complexes from the target to the cokernel.
    Example
      g1 = canonicalMap(coker g, target g)
      coker g == image g1
      coker g1 == 0
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    cokernel
    (image, ComplexMap)
    (coimage, ComplexMap)
    (kernel, ComplexMap)
    canonicalMap
///


doc ///
  Key
    (cone, ComplexMap)
  Headline
    make the mapping cone of a morphism of chain complexes
  Usage
    cone f
  Inputs
    f:ComplexMap
      which is a morphism of complexes
  Outputs
    :Complex
  Description
    Text
      Given a morphism $f : B \to C$, the mapping cone is the complex
      whose $i$-th term is $B_{i-1} \oplus\ C_i$, and whose $i$-th 
      differential is given by
      {\tt matrix\{\{-dd^{B[-1]}, 0\}, \{f[-1], dd^C\}\}}.
    Text
      A map between modules induces a map between their free resolutions,
      and we compute the associated mapping cone.
    Example
      S = ZZ/32003[x,y,z];
      M = ideal vars S
      B = freeResolution(S^1/M^2)
      C = freeResolution(S^1/M)
      f = extend(C,B,id_(S^1))
      Cf = cone f
      dd^Cf
      prune HH Cf
      assert(prune HH_1 Cf == prune(M/M^2))
    Text
      The mapping cone fits into a canonical short exact
      sequence of chain complexes:
      $$0 \to C \to cone(f) \to B[-1] \to 0.$$
    Example
      g = canonicalMap(Cf,C)
      h = canonicalMap(B[-1],Cf)
      assert(isWellDefined g and isWellDefined h)
      assert(isShortExactSequence(h,g))
    Text
      The most important application of mapping cones is to 
      identify quasi-isomorphisms: $f$ is a quasi-isomorphism 
      if and only if the mapping cone is acyclic.
    Example
      aug = augmentationMap C
      assert isWellDefined aug
      cone aug
      assert(0 == prune HH cone aug)
      assert isQuasiIsomorphism aug
    Text
      Mapping cones can also be used to construct free resolutions
      of subschemes linked via a complete intersection to a
      arithmetically Cohen-Macaulay subscheme;
      see Peskine-Szpiro, Liaison des varieties algebrique I, 
          {\it Invent. math.} {\bf 26} (1974) 271-302.
    Text
      Here, we consider a random complete intersection of 2 cubics
      contained in the ideal of the twisted cubic curve, and we
      compute a free resolution of the linked curve of degree 6.
    Example
      S = ZZ/32003[a..d]
      I = monomialCurveIdeal(S, {1,2,3})
      K = ideal((gens I) * random(source gens I, S^{-3,-3}))
      C = freeResolution(S^1/I)
      B = freeResolution(S^1/K)
      f = dual extend(C,B,id_(S^1))
      Cf = (cone f)[-2]
      prune HH Cf
      Cf' = minimize Cf
      J = ideal dd^Cf'_1
      freeResolution J
      assert(degree J == 6)
  Caveat
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    (cylinder, ComplexMap)
    (augmentationMap, Complex)
    (extend, Complex, Complex, Matrix)
    (freeResolution, Module)
    canonicalMap
    isQuasiIsomorphism
    isShortExactSequence
///


doc ///
  Key
    cylinder
    (cylinder, ComplexMap)
  Headline
    make the mapping cylinder of a morphism of chain complexes
  Usage
    cylinder f
  Inputs
    f:ComplexMap
      which is a morphism of complexes
  Outputs
    :Complex
  Description
    Text
      Given a morphism $f : B \to C$, the mapping cylinder 
      is the complex whose the $i$-th term is $B_{i-1} \oplus B_i \oplus C_i$
      and whose $i$-th differential is given in block form by
              {\tt matrix \{\{ - dd^B_{i-1}, 0, 0 \}, 
                \{ -id_{B_{i-1}}, dd^B_i, 0 \},
                \{ f_{i-1}, 0, dd^C_i\}\}}.
      Alternatively, the cylinder is the
      mapping cone of the morphism $g : B \to B \oplus C$ given in block form
      by
        {\tt matrix\{\{-id_B\}, \{f\}\}}.
    Text
      A map between modules induces a map between their free resolutions,
      and we compute the associated mapping cylinder.
    Example
      S = ZZ/32003[x,y,z];
      M = ideal vars S
      B = freeResolution(S^1/M^2)
      C = freeResolution(S^1/M)
      f = extend(C,B,id_(S^1))
      cylf = cylinder f
      dd^cylf
      assert isWellDefined cylf
    Text
      The mapping cylinder fits into a canonical short exact
      sequence of chain complexes,
      $$0 \to B \to cyl(f) \to cone(f) \to 0.$$
    Example
      Cf = cone f
      g = canonicalMap(cylf, B)
      h = canonicalMap(Cf, cylf)
      assert(isWellDefined g and isWellDefined h)
      assert(isShortExactSequence(h,g))
    Text
      The alternative interpretation of the cylinder, defined above,
      can be demonstrated as follows.
    Example
      g = map(B ++ C, B, {{-id_B},{f}})
      cone g == cylf
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    (cone, ComplexMap)
    (extend, Complex, Complex, Matrix)
    (freeResolution, Module)
    canonicalMap
    isShortExactSequence
///

doc ///
    Key
        (symbol^, ComplexMap, Array)
        (symbol_, ComplexMap, Array)
    Headline
        the composition with the canonical inclusion or projection map
    Usage
        i = f_[name]
        p = f^[name]
    Inputs
        f:ComplexMap
        name:
    Outputs
        :ComplexMap
            {\tt i} is the composition of {\tt f} with the canonical inclusion and {\tt p} is
            the composition of the canonical projection with {\tt f}
    Description
        Text
            The direct sum is an n-ary operator with projection and
            inclusion maps from each component satisfying appropriate
            identities.

            One can access these maps as follows.  First, we define
            some non-trivial maps of chain complexes.
        Example
            R = ZZ/101[a..d];
            C1 = (freeResolution coker matrix{{a,b,c}})[1]
            C2 = freeResolution coker matrix{{a*b,a*c,b*c}}
            D1 = (freeResolution coker matrix{{a,b,c}})
            D2 = freeResolution coker matrix{{a^2, b^2, c^2}}[-1]
            f = randomComplexMap(D1, C1, Cycle => true)
            g = randomComplexMap(D2, C2, Cycle => true)
        Example
            h = f ++ g
        Text
            The four basic maps are the inclusion from each summand of the source
            and the projection to each summand of the target.
        Example
            h_[0] == h * (C1 ++ C2)_[0]
            h_[1] == h * (C1 ++ C2)_[1]
            h^[0] == (D1 ++ D2)^[0] * h
            h^[1] == (D1 ++ D2)^[1] * h
        Text
            These can be combined to obtain the blocks of the map of chain complexes.
        Example
            h_[0]^[0] == f
            h_[1]^[1] == g
            h_[0]^[1] == 0
            h_[1]^[0] == 0
            assert(h == map(D1 ++ D2, C1 ++ C2, {{f,0},{0,g}}))
        Text
            The default names for the components are the non-negative
            integers.  However, one can choose any name.
        Example
            h = (mike => f) ++ (greg => g)
            h_[mike]^[mike] == f
            h_[greg]^[greg] == g
    SeeAlso
        (symbol++, Complex, Complex)
        (symbol^, Complex, Array)
        (symbol_, Complex, Array)
        (directSum, Complex)
        (components, Complex)
        indices
///

doc ///
    Key
        (randomComplexMap, Complex, Complex)
        randomComplexMap
        [randomComplexMap, Boundary]
        [randomComplexMap, Cycle]
        [randomComplexMap, Degree]
        [randomComplexMap, InternalDegree]
        Cycle
        Boundary
        InternalDegree
    Headline
        a random map of chain complexes
    Usage
        f = randomComplexMap(C,D)
    Inputs
        C:Complex
        D:Complex
        Boundary => Boolean
            whether the constructed {\tt f} is a null homotopy
        Cycle => Boolean
            whether the constructed {\tt f} commutes with the differentials
        Degree => ZZ
            the degree of the constructed map of chain complexes
        InternalDegree => List
            or @ofClass ZZ@
    Outputs
        f:ComplexMap
    Description
        Text
            A random complex map $f : C \to D$ is obtained from a random element
            in the complex @TO2 ((Hom,Complex,Complex), "$Hom(C,D)$")@.
        Example
            S = ZZ/101[a..c]
            C = freeResolution coker matrix{{a*b, a*c, b*c}}
            D = freeResolution coker vars S
            f = randomComplexMap(D,C)
            assert isWellDefined f
            assert not isCommutative f
            assert not isNullHomotopic f
        Text
            When the random element in the complex $Hom(C,D)$ lies in the kernel
            of the differential, the associated map of complexes commutes
            with the differential.
        Example
            g = randomComplexMap(D,C, Cycle => true)
            assert isWellDefined g
            assert isCommutative g
            assert isComplexMorphism g
            assert not isNullHomotopic g
        Text
            When the random element in the complex $Hom(C,D)$ lies in the image
            of the differential, the associated map of complexes is a null
            homotopy.
        Example
            h = randomComplexMap(D,C, Boundary => true)
            assert isWellDefined h
            assert isCommutative h
            assert isComplexMorphism h
            assert isNullHomotopic h
            nullHomotopy h
        Text
            When the degree of the random element in the complex $Hom(C,D)$ is non-zero,
            the associated map of complexes has the same degree.
        Example
            p = randomComplexMap(D, C, Cycle => true, Degree => -1)
            assert isWellDefined p
            assert isCommutative p
            assert not isComplexMorphism p
            assert(degree p === -1)
        Text
            By default, the random element is constructed as a random linear combination of
            the basis elements in the appropriate degree of $Hom(C,D)$.  Given an internal
            degree, the random element is constructed as maps of modules with this degree.
        Example
            q = randomComplexMap(D, C, Boundary => true, InternalDegree => 2)
            assert all({0,1,2}, i -> degree q_i === {2})
            assert isHomogeneous q
            assert isWellDefined q
            assert isCommutative q
            assert isComplexMorphism q
            source q === C
            target q === D
            assert isNullHomotopic q
    SeeAlso
        (homomorphism, ComplexMap)
        (homomorphism', ComplexMap)
        (Hom, Complex, Complex)
///

doc ///
    Key
        (homology, ComplexMap)
        (homology, ZZ, ComplexMap)
        (cohomology, ZZ, ComplexMap)
    Headline
        induced map on homology or cohomology
    Usage
        h = HH f
    Inputs
        f:ComplexMap
    Outputs
        h:ComplexMap
    Description
        Text
            Homology defines a functor from the category of chain complexes
            to itself.  Given a map of chain complexes $f : C \to D$,
            this method returns the induced map $HH f : HH C \to HH D$.
        Text
            To directly obtain the $n$-th map in $h$, use {\tt HH_n f} or
            {\tt HH^n f}.  By definition {\tt HH^n f === HH_(-n) f}.
            This can be more efficient, as it will compute only the desired
            induced map.
        Text
            If $f$ commutes with the differentials, then these induced
            maps are well defined.
        Example
            S = ZZ/101[a..d]
            I = ideal(a*b, a*d, c*b, c*d)
            C = (dual freeResolution I)[1]
            D = dual complex for i from 0 to 4 list koszul(i,gens I)
            assert isWellDefined D
            f = randomComplexMap(D, C, Cycle => true)
            assert isCommutative f
            h = HH f
            assert isWellDefined h
            prune h
            assert(source h == HH C)
            assert(target h == HH D)
        Example
            f2 = randomComplexMap(D, C, Cycle => true, Degree => -1)
            h2 = HH f2
            assert isWellDefined h2
            prune h2
        Text
            A boundary will always induce the zero map.
        Example
            f3 = randomComplexMap(D, C, Boundary => true)
            h3 = HH f3
            assert isWellDefined h3
            assert(h3 == 0)
    SeeAlso
        (homology, Complex)
        (homology, ZZ, Complex)
        (cohomology, ZZ, Complex)
        (prune, ComplexMap)
///

doc ///
    Key
        (tensorCommutativity, Complex, Complex)
    Headline
        make the canonical isomorphism arising from commutativity
    Usage
        tensorCommutativity(C, D)
    Inputs
        C:Complex
        D:Complex
    Outputs
        :ComplexMap
            which is an isomorphism from {\tt C ** D} to 
            {\tt D ** C} 
    Description
        Text
            The commutativity of tensor products of modules induces
            the commutativity of tensor products of chain complexes.
            This method implements this isomorphism for chain complexes.
        Text
            Using two term complexes of small rank,
            we see that this isomorphism need not be the identity map.
        Example
            S = ZZ/101[x_0..x_8]
            C = complex{genericMatrix(S,x_0,2,1)}
            D = complex{genericMatrix(S,x_2,1,2)}
            F = C ** D
            G = D ** C
            f = tensorCommutativity(C,D)
            assert isWellDefined f
            assert(source f === F)
            assert(target f === G)
            assert(f_1 != id_(source f_1))
            assert(prune ker f == 0)
            assert(prune coker f == 0)
            g = f^-1
            assert isWellDefined g
            assert(g * f == 1)
            assert(f * g == 1)
        Text
            We illustrate this isomorphism on complexes, none
            of whose terms are free modules.
        Example
            ses = (I,J) -> (
                complex{
                    map(S^1/(I+J), S^1/I ++ S^1/J, {{1,1}}),
                    map(S^1/I ++ S^1/J, S^1/(intersect(I,J)), {{1},{-1}})
                    }
                )
            C = ses(ideal(x_0,x_1), ideal(x_1,x_2))
            D = ses(ideal(x_3,x_4,x_5), ideal(x_6,x_7,x_8))
            h = tensorCommutativity(C, D);
            assert isWellDefined h
            assert(ker h == 0)
            assert(coker h == 0)
            k = h^-1;
            assert(h*k == 1)
            assert(k*h == 1)
            h_2
            assert(source h_2 != target h_2)
        Text
            Interchanging the arguments gives the inverse map.
        Example
            h1 = tensorCommutativity(D, C)
            assert(h1*h == id_(C**D))
            assert(h*h1 == id_(D**C))
        Text
            Interchanging the factors in a tensor product of
            two complex maps can be accomplished as follows.
        Example
            C = freeResolution ideal(x_0^2, x_1^2)
            D = freeResolution ideal(x_0, x_1)
            f = extend(D, C, map(D_0, C_0, 1))
            E = freeResolution ideal(x_2^3, x_3^3, x_4^3)
            F = freeResolution ideal(x_2, x_3, x_4)
            g = extend(F, E, map(F_0, E_0, 1))
            assert(tensorCommutativity(D,F) * (f**g) == (g**f) * tensorCommutativity(C,E))
    SeeAlso
        "Working with Tor"
        (tensorCommutativity, Module, Module)
        (tensorAssociativity, Complex, Complex, Complex)
///

doc ///
    Key
        (tensorAssociativity, Complex, Complex, Complex)
    Headline
        make the canonical isomorphism arising from associativity
    Usage
        tensorAssociativity(C, D, E)
    Inputs
        C:Complex
        D:Complex
        E:Complex
    Outputs
        :ComplexMap
            which is an isomorphism from {\tt C ** (D ** E)} to 
            {\tt (C ** D) ** E} 
    Description
        Text
            The associativity of tensor products of modules induces
            the associativity of tensor products of chain complexes.
            This method implements this isomorphism for chain complexes.
        Text
            Using two term complexes of small rank,
            we see that this isomorphism need not be the identity map.
        Example
            S = ZZ/101[x_0..x_11]
            C = complex{genericMatrix(S,x_0,2,1)}
            D = complex{genericMatrix(S,x_4,1,2)}
            E = complex{genericMatrix(S,x_8,2,2)}
            F = (C ** D) ** E
            G = C ** (D ** E)
            f = tensorAssociativity(C,D,E)
            assert isWellDefined f
            assert(source f === G)
            assert(target f === F)
            f_1
            assert(f_1 != id_(source f_1))
            assert(prune ker f == 0)
            assert(prune coker f == 0)
            g = f^-1
            assert isWellDefined g
            assert(g * f == 1)
            assert(f * g == 1)
        Text
            We illustrate this isomorphism on complexes, none
            of whose terms are free modules.
        Example
            ses = (I,J) -> (
                complex{
                    map(S^1/(I+J), S^1/I ++ S^1/J, {{1,1}}),
                    map(S^1/I ++ S^1/J, S^1/(intersect(I,J)), {{1},{-1}})
                    }
                )
            C = ses(ideal(x_0,x_1), ideal(x_1,x_2))
            D = ses(ideal(x_3,x_4,x_5), ideal(x_6,x_7,x_8))
            E = ses(ideal(x_1^2, x_1*x_2), ideal(x_1*x_3,x_9^2))
            h = tensorAssociativity(C, D, E);
            assert isWellDefined h
            assert(ker h == 0)
            assert(coker h == 0)
            k = h^-1;
            assert(h*k == 1)
            assert(k*h == 1)
            h_2
            assert(source h_2 != target h_2)
    SeeAlso
        "Working with Tor"
        (tensorCommutativity, Complex, Complex)
        (tensorAssociativity, Module, Module, Module)
///

