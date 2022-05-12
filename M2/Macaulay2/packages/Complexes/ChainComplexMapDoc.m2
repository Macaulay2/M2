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
                TO (map, Complex, Complex, List),
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
                TO (symbol**, Complex, Matrix),
                TO (freeResolution, Matrix),
                TO (extend, Complex, Complex, Matrix),
                TO (nullHomotopy, ComplexMap)
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
                TO (longExactSequence, ComplexMap, ComplexMap),
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
      @LITERAL ////<script type="text/javascript"> macros["\\Hom"] = "\\operatorname{Hom}" </script>////@

      A map of chain complexes $f \colon C \rightarrow D$ of degree $d$ is a
      sequence of maps $f_i \colon C_i \rightarrow D_{d+i}$.  
      No relationship between the maps $f_i$ and 
      and the differentials of either $C$ or $D$ is assumed.
      
      The set of all maps from $C$ to $D$ form
      the complex $\Hom(C,D)$ where $\Hom(C,D)_d$ consists of the
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
        (map, Complex, Complex, List)
    Headline
        make a map of chain complexes
    Usage
        f = map(D, C, L)
    Inputs
        C:Complex
        D:Complex
        L:List
            consisting of either matrices, or lists of maps of complexes
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
            from $C$ to $D$
    Description
        Text
            A map of complexes $f \colon C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i \colon C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and 
            and the differentials of either $C$ or $D$ is assumed.
            
            This method has two very different usages.  The first is to 
            construct a chain complex map from a list of matrices.  The second
            constructs a chain complex map from essentially a block matrix
            whose entries are chain complex maps.
        Text
            In the first case, we construct a map of chain complexes
            by specifying the individual maps between the terms.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}
            D = freeResolution coker vars R
            L = {map(D_0, C_0, 1),
                map(D_1, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                map(D_2, C_2, {{a*b^2, 0, 0}, {-a*c^2, a*c^3, 0}, {b*c^2, -b*c^3, b^2*c^3}}),
                map(D_3, C_3, {{a*b^2*c^3}})
                }
            f = map(D, C, L)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isComplexMorphism f
        Text
            In the second, we construct a map of chain complexes via a block matrix
            whose individual entries are already maps of chain complexes.
            We illustrate by constructing a mapping cone.
        Example
            f = extend(D,C,id_(R^1))
            assert(degree f == 0)
            g = map(D, C[-1], f[-1], Degree => -1) -- a variant of f having degree -1
            cf = map(E = C[-1] ++ D, E, {
                    {dd^(C[-1]),    0}, 
                    {         g, dd^D}
                    })
            assert isWellDefined cf
            assert(degree cf == -1)
        Text
            We convert this map of complexes {\tt cf} into the differential of the mapping cone.
            For the following constructor, the source and target of
            the input must be identical, in this case the chain complex $E$.
        Example
            conef = complex cf 
            assert isWellDefined conef
            assert(conef == cone f)
    SeeAlso
        "Making maps between chain complexes"
        (map, Complex, Complex, HashTable)
        (degree, ComplexMap)
        (extend, Complex, Complex, Matrix)
        (cone, ComplexMap)
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
        (isHomogeneous, ComplexMap)
    Headline
         whether a map of complexes is homogeneous
    Usage
         isHomogeneous f
    Inputs
         f:ComplexMap
    Outputs
         :Boolean
             that is true when $f_i$ is homogeneous, for all $i$
    Description
        Text
            A map of complexes $f \colon C \to D$ is homogeneous
            (graded) if its underlying ring is graded, and all the
            component maps $f_i \colon C_i \to D_{d+i}$ are graded of
            degree zero, where $f$ has degree $d$.
        Example
            S = ZZ/101[a,b,c,d];
            I = minors(2, matrix{{a,b,c},{b,c,d}})
            C = freeResolution (S^1/I)
            assert isHomogeneous dd^C
            f = randomComplexMap(C, C, Degree => -1)
            assert isHomogeneous f
            f = randomComplexMap(C, C, InternalDegree => 2)
            assert isHomogeneous f
        Text
            A map of chain complexes may be homogeneous even if the
            source or the target is not homogeneous.
        Example
            phi = map(S, S, {1,b,c,d})
            D = phi C
            dd^D
            assert not isHomogeneous dd^D
            g = randomComplexMap(D, D, InternalDegree => 1)
            assert isHomogeneous g
    SeeAlso
        "Basic invariants and properties"
        isHomogeneous
        (isHomogeneous, ComplexMap)
        (randomComplexMap, Complex, Complex)
///

doc ///
    Key
        (components, ComplexMap)
    Headline
        list the components of a direct sum
    Usage
        components f
    Inputs
        f:ComplexMap
    Outputs
        :List
            the component maps of a direct sum of maps of complexes
    Description
        Text
            A map of complexes stores its component maps.
        Example
            S = ZZ/101[a,b,c];
            C = freeResolution coker vars S
            g1 = id_C
            g2 = randomComplexMap(C[1], C[2], Boundary => true)
            f = g1 ++ g2
            assert isWellDefined f
            L = components f
            L_0 === g1
            L_1 === g2
            indices f
            f' = (greg => g1) ++ (mike => g2)
            components f'
            indices f'
        Text
            The names of the components are called indices, and are
            used to access the relevant inclusion and projection maps.
        Example
            f'_[mike]
            f'^[greg]
            f^[0]
            f_[0]
    SeeAlso
        (directSum, ComplexMap)
        (components, Complex)
        indices
        (symbol_, ComplexMap, Array)
        (symbol^, ComplexMap, Array)
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
        (symbol**, Complex, Matrix)
        (symbol**, Matrix, Complex)
    Headline
        create the tensor product of a complex and a map of modules
    Usage
        h = C ** f
        h = f ** C
    Inputs
        C:Complex
            over a ring $R$
        f:Matrix
            defining a homomorphism from the $R$-module $M$ to the $R$-module $N$
    Outputs
        h:ComplexMap
            from $C \otimes M$ to $C \otimes N$
    Description
        Text
            For any chain complex $C$, a map $f \colon M \to N$ of $R$-modules induces a
            morphism $C \otimes f$ of chain complexes
            from $C \otimes M$ to $C \otimes N$.  This method returns this map of chain complexes.
        Example
            R = ZZ/101[a..d];
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            J = ideal(I_0, I_1)
            C = koszulComplex vars R
            f = map(R^1/I, R^1/J, 1)
            C ** f
            f ** C
            f' = random(R^2, R^{-1, -1, -1})
            C ** f'
            f' ** C
            assert isWellDefined(C ** f')
            assert isWellDefined(f' ** C)
        Text
            Tensoring with a complex defines a functor from the category
            of $R$-modules to the category of complexes over $R$.
        Example
            f'' = random(source f', R^{-2,-2})
            assert((C ** f') * (C ** f'') == C ** (f' * f''))
            assert(C ** id_(R^{-1,-2,-3}) == id_(C ** R^{-1,-2,-3}))
    SeeAlso
        "Making maps between chain complexes"
        (symbol**, Complex, Complex)
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
        (symbol**, Ring, ComplexMap)
        (symbol**, ComplexMap, RingMap)
        (symbol**, ComplexMap, Ring)
        (tensor, RingMap, ComplexMap)
        (tensor, ComplexMap, RingMap)
    Headline
        tensor a map of complexes along a ring map
    Usage
        phi ** f
        tensor(phi, f)
        S ** f
        f ** S
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
            These methods implement the base change of rings.  As input, one can either
            give a ring map $\phi$, or the ring $S$ (when there is a canonical map
                from $R$ to $S$).
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
        [resolutionMap, LengthLimit]
        [resolutionMap, DegreeLimit]
        [resolutionMap, FastNonminimal]
        [resolutionMap, HardDegreeLimit]
        [resolutionMap, PairLimit]
        [resolutionMap, SortStrategy]
        [resolutionMap, StopBeforeComputation]
        [resolutionMap, Strategy]
        [resolutionMap, SyzygyLimit]
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
        UseTarget
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
        (symbol*, Number, ComplexMap)
        (symbol*, ComplexMap, RingElement)
        (symbol*, ComplexMap, Number)
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
            R = ZZ/101[a..d];
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
            assert(b-h*c == -h*c + id_C*b)
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
      Given a morphism $f \colon B \to C$, the mapping cone is the complex
      whose $i$-th term is $B_{i-1} \oplus C_i$, and whose $i$-th 
      differential is given by
      \[ \begin{bmatrix} -\operatorname{dd}^{B[-1]} & 0 \\ f[-1] & \operatorname{dd}^C \end{bmatrix}. \]
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
      $$0 \to C \to \operatorname{cone}(f) \to B[-1] \to 0.$$
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
        (tensorCommutativity, Module, Module)
        tensorCommutativity
    Headline
        make the canonical isomorphism arising from commutativity
    Usage
        tensorCommutativity(M, N)
    Inputs
        M:Module
        N:Module
            both over the same ring $R$
    Outputs
        :Matrix
            that is an isomorphism from $M \otimes_R N$ to 
            $N \otimes_R M$.
    Description
        Text
            Given $R$-modules $M$ and $N$, there exists a canonical isomorphism
            $f \colon M \otimes_R N \to N \otimes_R M$ interchanging the factors.
            This method implements this isomorphism.
        Text
            Even for free modules, this map is not simply given by the identity matrix.
        Example
            R = ZZ/101[x,y];
            M = R^2
            N = R^3
            f = tensorCommutativity(M, N)
            assert isWellDefined f
            assert isIsomorphism f
        Text
            By giving the generators of $M$ and $N$ sufficiently 
            different degrees, we see that the 
            canonical generators for the two
            tensor products come in different orders.
            The isomorphism is given by the corresponding
            permutation matrix.
        Example
            M = R^{1,2}
            N = R^{100,200,300}
            M ** N
            N ** M
            tensorCommutativity(M, N)
        Text
            For completeness, we include an example when
            neither module is free.
        Example
            g = tensorCommutativity(coker vars R ++ coker vars R, image vars R)
            source g
            target g
            assert isWellDefined g
            assert isIsomorphism g
    SeeAlso
        "Working with Tor"
        (tensorCommutativity, Complex, Complex)
        (tensorAssociativity, Module, Module, Module)
        (isIsomorphism, Matrix)
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
            both over the same ring $R$
    Outputs
        :ComplexMap
            that is an isomorphism from $C \otimes_R D$ to 
            $D \otimes_R C$
    Description
        Text
            The commutativity of tensor products of modules induces
            the commutativity of tensor products of chain complexes.
            This method implements this isomorphism for chain complexes.
        Text
            Using two term complexes of small rank,
            we see that this isomorphism need not be the identity map.
        Example
            S = ZZ/101[x_0..x_8];
            C = complex{genericMatrix(S,x_0,2,1)}
            D = complex{genericMatrix(S,x_2,1,2)}
            F = C ** D
            G = D ** C
            f = tensorCommutativity(C,D)
            assert isWellDefined f
            assert isComplexMorphism f
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
            assert isComplexMorphism h
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
            assert isComplexMorphism h1
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
            assert isComplexMorphism tensorCommutativity(D,F)
            assert isComplexMorphism tensorCommutativity(C,E)
    SeeAlso
        "Working with Tor"
        (tensorCommutativity, Module, Module)
        (tensorAssociativity, Complex, Complex, Complex)
        (isComplexMorphism, ComplexMap)
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

doc ///
    Key
        (isShortExactSequence, ComplexMap, ComplexMap)
    Headline
        whether a pair of complex maps forms a short exact sequence
    Usage
        isShortExactSequence(g, f)
    Inputs
        f:ComplexMap
        g:ComplexMap
    Outputs
        :Boolean
            that is @TO true@ if these form a short exact sequence
    Description
        Text
            A short exact sequence of complexes 
            \[ 0 \to B \xrightarrow{f} C \xrightarrow{g} D \to 0\]
            consists of two morphisms of complexes
            $f \colon B \to C$ and $g \colon C \to D$ such that
            $g f = 0$, $\operatorname{image} f = \operatorname{ker} g$, 
            $\operatorname{ker} f = 0$, and $\operatorname{coker} g = 0$.
        Text
            From a complex morphism $h \colon B \to C$, one obtains a
            short exact sequence
            \[ 0 \to \operatorname{image} h \to C \to \operatorname{coker} h \to 0. \]
        Example
            R = ZZ/101[a,b,c];
            B = freeResolution coker matrix{{a^2*b, a*b*c, c^3}}
            C = freeResolution coker vars R
            h = randomComplexMap(C, B, Cycle => true)
            f = canonicalMap(C, image h)
            g = canonicalMap(coker h, C)
            assert isShortExactSequence(g,f)
        Text
            A short exact sequence of modules gives rise to a short
            exact sequence of complexes.  These complexes arise
            as free resolutions of the modules.
        Example
            I = ideal(a^3, b^3, c^3)
            J = I + ideal(a*b*c)
            K = I : ideal(a*b*c)
            SES = complex{
                map(comodule J, comodule I, 1),
                map(comodule I, (comodule K) ** R^{-3}, {{a*b*c}})
                }
            assert isWellDefined SES
            assert isShortExactSequence(dd^SES_1, dd^SES_2)
            (g,f) = horseshoeResolution SES
            assert isShortExactSequence(g,f)
    SeeAlso
        "Basic invariants and properties"
        (isShortExactSequence, Complex)
        canonicalMap
        (cone, ComplexMap)
        (horseshoeResolution, Complex)
        (longExactSequence, ComplexMap, ComplexMap)
///

doc ///
    Key
        (isShortExactSequence, Matrix, Matrix)
    Headline
        whether a pair of matrices forms a short exact sequence
    Usage
        isShortExactSequence(g, f)
    Inputs
        f:Matrix
        g:Matrix
    Outputs
        :Boolean
            that is @TO true@ if these form a short exact sequence
    Description
        Text
            A short exact sequence of modules
            \[ 0 \to L \xrightarrow{f} M \xrightarrow{g} N \to 0\]
            consists of two homomorphisms of modules
            $f \colon L \to M$ and $g \colon M \to N$ such that
            $g f = 0$, $\operatorname{image} f = \operatorname{ker} g$, 
            $\operatorname{ker} f = 0$, and $\operatorname{coker} g = 0$.
        Text
            From a homomorphism $h \colon M \to N$, one obtains a
            short exact sequence
            \[ 0 \to \operatorname{image} h \to N \to \operatorname{coker} h \to 0. \]
        Example
            R = ZZ/101[a,b,c];
            h = random(R^3, R^{4:-1})
            f = inducedMap(target h, image h)
            g = inducedMap(cokernel h, target h)
            assert isShortExactSequence(g,f)
        Text
            Ideal quotients also give rise to short exact sequences.
        Example
            I = ideal(a^3, b^3, c^3)
            J = I + ideal(a*b*c)
            K = I : ideal(a*b*c)
            g = map(comodule J, comodule I, 1)
            f = map(comodule I, (comodule K) ** R^{-3}, {{a*b*c}})
            assert isShortExactSequence(g,f)
    SeeAlso
        "Basic invariants and properties"
        (isShortExactSequence, Complex)
        (isShortExactSequence, ComplexMap, ComplexMap)
///

doc ///
    Key
        (isShortExactSequence, Complex)
        isShortExactSequence
    Headline
        whether a chain complex is a short exact sequence
    Usage
        isShortExactSequence C
    Inputs
        C:Complex
    Outputs
        :Boolean
            that is @TO true@ if $C$ is a short exact sequence
    Description
        Text
            A short exact sequence of modules is a complex
            \[ 0 \to L \xrightarrow{f} M \xrightarrow{g} N \to 0\]
            consisting of two homomorphisms of modules
            $f \colon L \to M$ and $g \colon M \to N$ such that
            $g f = 0$, $\operatorname{image} f = \operatorname{ker} g$, 
            $\operatorname{ker} f = 0$, and $\operatorname{coker} g = 0$.
        Text
            From a homomorphism $h \colon M \to N$, one obtains a
            short exact sequence
            \[ 0 \to \operatorname{image} h \to N \to \operatorname{coker} h \to 0. \]
        Example
            R = ZZ/101[a,b,c];
            h = random(R^3, R^{4:-1})
            f = inducedMap(target h, image h)
            g = inducedMap(cokernel h, target h)
            C = complex {g, f}
            isWellDefined C
            assert isShortExactSequence C
            assert isShortExactSequence(C[10])
            assert not isShortExactSequence(C ++ C[6])
            D = complex(R^1, Base=>4) ++ complex(R^1, Base=>2)
            assert not isShortExactSequence D
    SeeAlso
        "Basic invariants and properties"
        (isShortExactSequence, Matrix, Matrix)
        (isShortExactSequence, ComplexMap, ComplexMap)
///

doc ///
    Key
        (isQuasiIsomorphism, ComplexMap)
        [isQuasiIsomorphism, Concentration]
        isQuasiIsomorphism
    Headline
         whether a map of complexes is a quasi-isomorphism
    Usage
         isQuasiIsomorphism f
    Inputs
         f:ComplexMap
         Concentration => Sequence
             restricts attention to the induced maps indexed
             by elements in the given interval 
    Outputs
         :Boolean
             that is true when $f$ is a morphism of complexes
             such that the induced maps on homology are all
             isomorphisms
    Description
        Text
            The @TO2((cone, ComplexMap), "cone")@ of a 
            map $f \colon C \to D$ is acyclic
            exactly when $f$ is a quasi-isomorphism.
        Example
            S = ZZ/32003[x,y,z];
            C = freeResolution coker vars S
            f = augmentationMap C
            assert isQuasiIsomorphism f
            assert(0 == prune HH cone f)
            assert isIsomorphism HH_0 f
            assert isIsomorphism HH_1 f
        Text
            XXX TODO. Free resolutions of complexes produce quasi 
            isomorphisms. (use example to doc of (resolution, Complex)).
        Example
            D = complex{random(S^2, S^{-3,-3,-4})}
            prune HH D
    SeeAlso
        "Basic invariants and properties"
        (cone, ComplexMap)
        liftMapAlongQuasiIsomorphism
///

doc ///
    Key
        (isNullHomotopyOf, ComplexMap, ComplexMap)
        isNullHomotopyOf
    Headline
        whether the first map of chain complexes is a null homotopy for the second
    Usage
        isNullHomotopyOf(h, f)
    Inputs
        h:ComplexMap
        f:ComplexMap
    Outputs
        :Boolean
            that is true when $h$ is a null homotopy of $f$
    Description
        Text
            A map of chain complexes $f \colon C \to D$ is
            null-homopic if there exists a map of chain
            complexes $h : C \to D$ of degree $\deg(f)+1$,
            such that we have the equality 
            \[ f = \operatorname{dd}^D h 
              + (-1)^{\deg(f)} h \operatorname{dd}^C.
            \]
        Text
            As a first example, we construct a map of chain complexes
            in which the null homotopy is given by the identity.
        Example
            R = ZZ/101[x,y,z];
            M = cokernel matrix{{x,y,z^2}, {y^2,z,x^2}}
            C = complex {id_M}
            h = map(C, C, i -> if i == 0 then id_M, Degree => 1)
            isWellDefined h
            assert isNullHomotopyOf(h, id_C)
            assert isNullHomotopic id_C
        Text
            A random map of chain complexes, arising as a boundary
            in the associated Hom complex, is automatically
            null homotopic.  We use the method @TO nullHomotopy@
            to construct a witness and verify it is a null homotopy.
        Example
            C = (freeResolution M) ** R^1/ideal(x^3, z^3-x)
            f = randomComplexMap(C, C[1], Boundary => true)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
        Text
            By assigning @TO "debugLevel"@ a positive value,
            this method provides some information about the nature
            of the failure to be a null homotopy.
        Example
            g1 = randomComplexMap(C, C[1], Degree => 1)
            g2 = randomComplexMap(C, C[1], Degree => -1)
            debugLevel = 1
            assert not isNullHomotopyOf(g1, f)
            assert not isNullHomotopyOf(g2, f)
    SeeAlso
        "Basic invariants and properties"
        (isNullHomotopic, ComplexMap)
        (nullHomotopy, ComplexMap)
        randomComplexMap
        (Hom, Complex, Complex)
///

doc ///
    Key
        (isNullHomotopic, ComplexMap)
        isNullHomotopic
    Headline
        whether a map of complexes is null-homotopic
    Usage
        isNullHomotopic f
    Inputs
        f:ComplexMap
    Outputs
        :Boolean
            that is true when $f$ is null-homotopic
    Description
        Text
            A map of chain complexes $f \colon C \to D$ is
            null-homopic if there exists a map of chain
            complexes $h : C \to D$ of degree $\deg(f)+1$,
            such that we have the equality 
            \[ f = \operatorname{dd}^D h 
              + (-1)^{\deg(f)} h \operatorname{dd}^C.
            \]
        Text
            As a first example, we construct a map of chain complexes
            in which the null homotopy is given by the identity.
        Example
            R = ZZ/101[x,y,z];
            M = cokernel matrix{{x,y,z^2}, {y^2,z,x^2}}
            C = complex {id_M}
            assert isNullHomotopic id_C
            h = nullHomotopy id_C
            assert(h_0 == id_M)
            assert isNullHomotopyOf(h, id_C)
        Text
            A random map of chain complexes, arising as a boundary
            in the associated Hom complex, is automatically
            null homotopic.
        Example
            C = (freeResolution M) ** R^1/ideal(x^3, z^3-x)
            f = randomComplexMap(C, C[1], Boundary => true)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
            g = randomComplexMap(C, C[1])
            assert not isNullHomotopic g
        Text
            This procedure also works for complex maps
            whose degree is non-zero.
        Example
            f = randomComplexMap(C, C[2], Boundary => true, Degree => 1)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
    SeeAlso
        "Basic invariants and properties"
        (isNullHomotopyOf, ComplexMap, ComplexMap)
        (nullHomotopy, ComplexMap)
        randomComplexMap
        (Hom, Complex, Complex)
///

doc ///
    Key
        (nullHomotopy, ComplexMap)
        nullHomotopy
    Headline
        a map which is a candidate for being a null homotopy
    Usage
        h = nullHomotopy f
    Inputs
        f:ComplexMap
    Outputs
        h:ComplexMap
    Description
        Text
            A map of chain complexes $f \colon C \to D$ is
            null-homopic if there exists a map of chain
            complexes $h : C \to D$ of degree $\deg(f)+1$,
            such that we have the equality 
            \[ f = \operatorname{dd}^D h 
              + (-1)^{\deg(f)} h \operatorname{dd}^C.
            \]
            Given $f$, this method returns a map $h$ of chain complexes
            that will be a null-homotopy if one exists.
        Text
            As a first example, we construct a map of chain complexes
            in which the null homotopy is given by the identity.
        Example
            R = ZZ/101[x,y,z];
            M = cokernel matrix{{x,y,z^2}, {y^2,z,x^2}}
            C = complex {id_M}
            assert isNullHomotopic id_C
            h = nullHomotopy id_C
            assert(h_0 == id_M)
            assert isNullHomotopyOf(h, id_C)
        Text
            A random map of chain complexes, arising as a boundary
            in the associated Hom complex, is automatically
            null homotopic.
        Example
            C = (freeResolution M) ** R^1/ideal(x^3, z^3-x)
            f = randomComplexMap(C, C[1], Boundary => true)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
        Text
            When a map of chain complexes is not null-homotopic,
            this method nevertheless returns a map $h$ of
            chain complexes, having the correct source, target
            and degree, but cannot be a null homotopy.
        Example
            g = randomComplexMap(C, C[1])
            assert not isNullHomotopic g
            h' = nullHomotopy g
            assert isWellDefined h'
            assert(degree h' === degree g + 1)
            assert not isNullHomotopyOf(h', g)
        Text
            For developers: when the source of $f$ is a free complex,
            a procedure, that is often faster, is attempted.  In the
            general case this method uses the Hom complex.
    Caveat
        The output is only a null homotopy when one exists.
    SeeAlso
        "Making maps between chain complexes"
        (isNullHomotopic, ComplexMap)
        (isNullHomotopyOf, ComplexMap, ComplexMap)
        randomComplexMap
        (Hom, Complex, Complex)
///

doc ///
    Key
        (freeResolution, Matrix)
    Headline
        compute the induced map between free resolutions
    Usage
        freeResolution f
    Inputs
        f:Matrix
            defining a map from an $R$-module $M$ to an $R$-module $N$
        LengthLimit => ZZ
            this is used to limit somehow the computation where resolutions might be too long or infinite
        DegreeLimit => List
            or @ofClass ZZ@, an option that specifies that the computation stops at the given
            (slanted) degree 
        FastNonminimal => Boolean
            unused (TODO: probably used)
        HardDegreeLimit => List
            unused (TODO: used?)
        PairLimit => ZZ
            or @TO infinity@, an internal option which specifies that the computation should stop after a 
            certain number of s-pairs have computed
        SortStrategy => ZZ
            an internal option that specifies the strategy to be used for sorting S-pairs
        StopBeforeComputation => Boolean
            whether to start the computation. This can be useful when you want to obtain the 
            partially computed resolution contained in an interrupted computation.
        Strategy => ZZ
            TODO: perhaps needs its own page
        SyzygyLimit => ZZ
            or @TO infinity@, 
            an internal option which specifies that the computation should stop after a 
            certain number of syzygies have computed
    Outputs
        :ComplexMap
            an induced map of chain complexes from the free resolution of $M$ to the
            free resolution of $N$
    Description
        Text
            A homomorphism $f \colon M \to N$ of $R$-modules induces a morphism of chain complexes
            from any free resolution of $M$ to a free resolution of $N$.  This method 
            constructs this map of chain complexes.
        Example
            R = QQ[a..d];
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            J = ideal(I_0, I_1)
            M = R^1/J
            N = R^1/I
            f = map(N, M, 1)
            g = freeResolution f
            assert isWellDefined g
            assert isComplexMorphism g
            assert(source g === freeResolution M)
            assert(target g === freeResolution N)
        Text
            Taking free resolutions is a functor, up to homotopy, from
            the category of modules to the category of chain
            complexes.
            In the subsequent example, the composition of the induced
            chain maps $g$ and $g'$ happens to be equal to 
            the induced map of the composition.
        Example
            K = ideal(I_0)
            L = R^1/K
            f' = map(M, L, 1)
            g' = freeResolution f'
            g'' = freeResolution(f * f')
            assert(g'' === g * g')
            assert(freeResolution id_N === id_(freeResolution N))
        Text
            Over a quotient ring, free resolutions are often infinite.
            Use the optional argument {\tt LengthLimit} to obtain
            a truncation of the map between resolutions.
        Example
            S = ZZ/101[a,b]
            R = S/(a^3+b^3)
            f = map(R^1/(a,b), R^1/(a^2, b^2), 1)
            g = freeResolution(f, LengthLimit => 7)
            assert isWellDefined g
            assert isComplexMorphism g
    SeeAlso
        "Making maps between chain complexes"
        (freeResolution, Module)
        (isComplexMorphism, ComplexMap)
///

doc ///
    Key
        (extend, Complex, Complex, Matrix, Sequence)
        (extend, Complex, Complex, Matrix)
    Headline
        extend a map of modules to a map of chain complexes
    Usage
        g = extend(D, C, f, p)
        g = extend(D, C, f)
    Inputs
        C:Complex
        D:Complex
        f:Matrix
        p:Sequence
            consisting of a pair of integers $(j,i)$, such that the
            matrix $f$ defines a map from $C_i$ to $D_j$; the default 
            value is $i = j = 0$
        Verify => Boolean
            currently, this option is ignored
    Outputs
        :ComplexMap
    Description
        Text
            Let $C$ be a chain complex such that each term is a free
            module.  Let $D$ be a chain
            complex which is exact at the $k$-th term for all $k > j$.
            Given a map of modules $f \colon C_i \to D_j$ such that
            the image of $f \circ \operatorname{dd}^C_{i+1}$ is
            contained in the image of $\operatorname{dd}^D_{j+1}$,
            this method constructs a morphism of chain complexes $g
            \colon C \to D$ of degree $j-i$ such that $g_i = f$.

            $\phantom{WWWW}
            \begin{array}{cccccc}
            0 & \!\!\leftarrow\!\! & C_{i} & \!\!\leftarrow\!\! & C_{i+1} & \!\!\leftarrow\!\! & C_{i+2} & \dotsb \\
              &            & \downarrow \, {\scriptstyle f} & & \downarrow \, {\scriptstyle g_{i+1}} && \downarrow \, {\scriptstyle g_{i+2}} \\
            0 & \!\!\leftarrow\!\! & D_{j} & \!\!\leftarrow\!\! & D_{j+1} &  \!\!\leftarrow\!\! & D_{j+2} & \dotsb \\
            \end{array}
            $
        Text
            A map between modules extends to a map between their free resolutions.
        Example
            S = ZZ/101[a..d];
            I = ideal(a*b*c, b*c*d, a*d^2)
            C = S^{{-3}} ** freeResolution (I:a*c*d)
            D = freeResolution I
            f = map(D_0, C_0, matrix{{a*c*d}})
            g = extend(D, C, f)
            assert isWellDefined g
            assert isComplexMorphism g
            assert(g_0 == f)
            E = cone g
            dd^E
        Text
            Extension of maps to complexes is also useful in 
            constructing a free resolution of a linked ideal.
        Example
            I = monomialCurveIdeal(S, {1,2,3})
            K = ideal(I_1^2, I_2^2)
            FI = freeResolution I
            FK = freeResolution K
            f = map(FI_0, FK_0, 1)
            g = extend(FI, FK, f)
            assert isWellDefined g
            assert isComplexMorphism g
            assert(g_0 == f)
            C = cone (dual g)[- codim K]
            dd^C
            dd^(minimize C)
            assert(ideal relations HH_0 C == K:I)
        Text
            Inspired by a @TO yonedaMap@ computation, we extend a map
            of modules to a map between free resolutions having
            homological degree $-1$.
        Example
            f = map(FK_0, FI_1, matrix {{a*c^2-a*b*d, -b*c^2+a*c*d, -c^3+a*d^2}}, Degree => 1)
            assert isHomogeneous f
            assert isWellDefined f
            g = extend(FK, FI, f, (0,1))
            assert isWellDefined g
            assert isCommutative g
            assert(degree g === -1)
            assert isHomogeneous g
    SeeAlso
        "Making maps between chain complexes"
        (cone, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (minimize, Complex)
///

doc ///
    Key
        (liftMapAlongQuasiIsomorphism, ComplexMap, ComplexMap)
        liftMapAlongQuasiIsomorphism
        (symbol//, ComplexMap, ComplexMap)
        (quotient, ComplexMap, ComplexMap)
        homotopyMap
        (homotopyMap, ComplexMap)
    Headline
        lift a map of chain complexes along a quasi-isomorphism
    Usage
        f' = liftMapAlongQuasiIsomorphism(f, g)
        f' = f // g
    Inputs
        f:ComplexMap
            where each term in the source of $f$ is a free module
        g:ComplexMap
            a quasi-isomorphism having the same target as $f$
    Outputs
        f':ComplexMap
            a map from the source of $f$ to the source of $g$
    Consequences
        Item
            the homotopy relating $f$ and $g \circ f'$ is
            available as {\tt homotopyMap f'}.
    Description
        Text
            Let $f \colon P \to C$ be a morphism of chain 
            complexes, where each term in $P$ is a free module.
            Given a quasi-isomorphism $g \colon B \to C$,
            this method produces a morphism $f' \colon P \to B$
            such that there exists a map $h \colon P \to C$
            of chain complexes having degree $1$
            satisfying

            $f - g \circ f' = h \circ \operatorname{dd}^P +
             \operatorname{dd}^C \circ h$.
             
        Text
            Given a morphism between complexes, we can construct 
            the corresponding map 
            between their
            free resolutions using this method.
            
            To be more precise, 
            given a morphism $\phi \colon B \to C$ of complexes,
            let $\alpha \colon P \to B$ and 
            $\beta \colon F \to C$ denote the free resolutions
            of the source and target complexes.
            Lifting the composite map $\phi \circ \alpha$ along the
            quasi-isomorphism $\beta$ gives a commutative diagram
            $\phantom{WWWW}
            \begin{array}{ccc}
            P & \!\!\rightarrow\!\! & F \\
            \downarrow \, {\scriptstyle \alpha} & & \downarrow \, {\scriptstyle \beta} \\
            B & \xrightarrow{\phi} & C
            \end{array}
            $
        Example
            S = ZZ/101[a,b,c,d];
            J = ideal(a*b, a*d, b*c);
            I = J + ideal(c^3);
            C = prune Hom(S^{2} ** freeResolution I, S^1/I)
            D = prune Hom(freeResolution J, S^1/J)
            r = randomComplexMap(D,C,Cycle=>true)
            f = r * resolutionMap C
            g = resolutionMap D
            assert isQuasiIsomorphism g
            f' = liftMapAlongQuasiIsomorphism(f, g)
            assert(f' == f//g)
            assert isWellDefined f'
            assert isComplexMorphism f'
            h = homotopyMap f'
            isNullHomotopyOf(h, g * (f//g) - f)
        Text
            TODO: XXX start here. Do triangles, invert interesting quasi-isomorphism.
            isSemiFree, and add in an example or 2.  Include
            finding an inverse for a quasi-isomorphism.
            We need some kind of better example here.
    Caveat
        The following three assumptions are not checked:
        $f$ is a morphism, the source of $f$ is semifree,
        and $g$ is a quasi-isomorphism.
    SeeAlso
        "Towards computing in the derived category"
        isQuasiIsomorphism
        isComplexMorphism
///

doc ///
    Key
        (horseshoeResolution, Complex)
        (horseshoeResolution, Matrix, Matrix)
        horseshoeResolution
        [horseshoeResolution, LengthLimit]
    Headline
        make the horseshoe resolution
    Usage
        (beta, alpha) = horseshoeResolution C
        (beta, alpha) = horseshoeResolution(g, f)
    Inputs
        C:Complex
            (or a pair of maps) that is
            a short exact sequence of modules
        LengthLimit => ZZ
            limit the lengths of the constructed free resolutions
    Outputs
        :Sequence
            consisting of a pair of chain complex maps
            $\beta$ and $\alpha$
    Description
        Text
            Given a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow N \xleftarrow{g} M \xleftarrow{f} L \leftarrow 0,
            $

            the horsehoe lemma produces simultaneous free resolutions
            of $N$, $M$ and $L$, which form a short exact sequence of complexes.
            This method returns these two chain complex maps.
        Text
            We illustrate this method by constructing simultaneous
            free resolutions of three monomial ideals.
        Example
            S = ZZ/101[a,b,c];
            I = ideal(a^3, b^3, c^3)
            J = I + ideal(a*b*c)
            K = I : ideal(a*b*c)
            C = complex{
                map(comodule J, comodule I, 1),
                map(comodule I, (comodule K) ** S^{-3}, {{a*b*c}})
                }
            assert isShortExactSequence C
            (beta, alpha) = horseshoeResolution C
            assert isShortExactSequence(beta, alpha)
            assert(prune HH source alpha == complex C_2)
            assert(prune HH target alpha == complex C_1)
            assert(prune HH target beta == complex C_0)
            assert isFree target alpha
        Text
            The constructed resolution of the middle term $C_1$
            is almost never minimal.
        Example
            target alpha
            freeResolution C_1
            minimize target alpha
        Text
            The optional argument {\tt LengthLimit} allows one to
            truncate the constructed free resolutions.
        Example
            (beta, alpha) = horseshoeResolution(C, LengthLimit => 2)
            assert isShortExactSequence(beta, alpha)
            prune HH source alpha
            assert isFree target alpha
    SeeAlso
        "Towards computing in the derived category"
        (isShortExactSequence, Complex)
        (minimize, Complex)
        (connectingMap, ComplexMap, ComplexMap)
        connectingExtMap
///

doc ///
    Key
        (connectingMap, ComplexMap, ComplexMap)
        connectingMap
        [connectingMap, Concentration]
    Headline
        construct the connecting homomorphism on homology
    Usage
        connectingMap(g, f)
    Inputs
        f:ComplexMap
            an injective morphism $f \colon A \to B$
        g:ComplexMap
            a surjective morphism $g \colon B \to C$ 
            whose kernel is the same as the image of $f$
        Concentration => Sequence
            not yet implemented
    Outputs
        h:ComplexMap
            a complex morphism
            whose source is the homology of $C$ and whose target
            is the homology of $A$, shifted by $-1$
    Description
        Text
            Given a short exact sequence of chain complexes

            $\phantom{WWWW}
            0 \leftarrow C \xleftarrow{g} B \xleftarrow{f} A \leftarrow 0,
            $
            
            this function returns the unique morphism $h \colon H(C) \to H(A)[-1]$ of complexes
            that naturally fits into the long exact sequence

            $\phantom{WWWW}
            \dotsb \leftarrow H(C)[-1] \xleftarrow{H(g)[-1]} H(B)[-1] \xleftarrow{H(f)[-1]} H(A)[-1] \xleftarrow{h} H(C) \xleftarrow{H(g)} H(B) \xleftarrow{H(f)} H(A) \leftarrow \dotsb.
            $
            
            $\phantom{WWWW}$
            
        Text
            As a first example, consider a free resolution $F$ of $S/I$.
            Applying the Hom functor $\operatorname{Hom}(F, -)$ to a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/h \leftarrow S \xleftarrow{h} S(- \deg h) \leftarrow 0
            $

            gives rise to a short exact sequence of complexes.  The corresponding long exact sequence in homology
            has the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Ext}^{d+1}(S/I, S(-\deg h))
            \xleftarrow{\delta} 
            \operatorname{Ext}^d(S/I, S/h)
            \leftarrow \operatorname{Ext}^d(S/I, S)
            \leftarrow \operatorname{Ext}^d(S/I, S(-\deg h))
            \leftarrow \dotsb.
            $
        Example
            S = ZZ/101[a..d, Degrees=>{2:{1,0},2:{0,1}}];
            h = a*c^2 + a*c*d + b*d^2;
            I = (ideal(a,b) * ideal(c,d))^[2]
            F = freeResolution comodule I;
            g = Hom(F, map(S^1/h, S^1, 1))
            f = Hom(F, map(S^1, S^{-degree h}, {{h}}))
            assert isWellDefined g
            assert isWellDefined f
            assert isShortExactSequence(g, f)
            delta = connectingMap(g, f)
            assert isWellDefined delta
            assert(degree delta == 0)            
            assert(source delta_(-1) == Ext^1(comodule I, S^1/h))
            assert(target delta_(-1) == Ext^2(comodule I, S^{{-1,-2}}))
            L = longExactSequence(g,f)
            assert isWellDefined L
            assert(HH L == 0)
            assert(dd^L_-9 === delta_-3)
            assert(dd^L_-8 === HH_-3 g)
            assert(dd^L_-7 === HH_-3 f)
            assert(dd^L_-6 === delta_-2)
            assert(dd^L_-5 === HH_-2 g)
            assert(dd^L_-4 === HH_-2 f)
            assert(dd^L_-3 === delta_-1)
        Text
            Applying the Hom functor $\operatorname{Hom}(-, S)$ to the horseshoe resolution of
            a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/(I+J) \leftarrow S/I \oplus S/J  \leftarrow S/I \cap J \leftarrow 0
            $

            gives rise to a short exact sequence of complexes.  The corresponding long exact sequence in homology
            has the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Ext}^{d+1}(S/(I+J), S)
            \xleftarrow{\delta} 
            \operatorname{Ext}^d(S/I \cap J, S)
            \leftarrow \operatorname{Ext}^d(S/I \oplus S/J, S)
            \leftarrow \operatorname{Ext}^d(S/(I+J), S)
            \leftarrow \dotsb.
            $
        Example
            S = ZZ/101[a..d];
            I = ideal(c^3-b*d^2, b*c-a*d)
            J = ideal(a*c^2-b^2*d, b^3-a^2*c)
            ses = complex{
                map(S^1/(I+J), S^1/I ++ S^1/J, {{1,1}}),
                map(S^1/I ++ S^1/J, S^1/intersect(I,J), {{1},{-1}})
                }
            assert isWellDefined ses
            assert(HH ses == 0)
            (g,f) = horseshoeResolution ses
            assert isShortExactSequence(g,f)
            (Hf, Hg) = (Hom(f, S), Hom(g, S));
            assert isShortExactSequence(Hf, Hg)
            delta = connectingMap(Hf, Hg)
            assert isWellDefined delta
            assert isComplexMorphism delta
            assert(source delta_-2 == Ext^2(comodule intersect(I,J), S))
            assert(target delta_-2 == Ext^3(comodule (I+J), S))
            L = longExactSequence(Hf, Hg)
            assert isWellDefined L
            assert(HH L == 0)
            assert(dd^L_-6 === delta_-3)
            assert(dd^L_-5 === HH_-3 Hf)
            assert(dd^L_-4 === HH_-3 Hg)
            assert(dd^L_-3 === delta_-2)
            assert(dd^L_-2 === HH_-2 Hf)
            assert(dd^L_-1 === HH_-2 Hg)
            assert(dd^L_0 === delta_-1)
    SeeAlso
        "Towards computing in the derived category"
        (longExactSequence, ComplexMap, ComplexMap)
///

doc ///
    Key
        (longExactSequence, ComplexMap, ComplexMap)
        longExactSequence
        [(longExactSequence, ComplexMap, ComplexMap), Concentration]
    Headline
        make the long exact sequence in homology
    Usage
        L = longExactSequence(g, f)
    Inputs
        f:ComplexMap
        g:ComplexMap
        Concentration => Sequence
            this optional argument is not yet implemented
    Outputs
        L:Complex
    Description
        Text
            Every short exact sequence of complexes

            $\phantom{WWWW}
            0 \leftarrow C \xleftarrow{g} B  \xleftarrow{f} A \leftarrow 0
            $

            gives rise to a long exact sequence $L$ in homology having
            the form

            $\phantom{WWWW}
            \dotsb \leftarrow H_{i-1}(C) 
            \xleftarrow{H_{i-1}(g)} 
            H_{i-1}(B)
            \xleftarrow{H_{i-1}(f)}
            H_{i-1}(A)
            \xleftarrow{\delta_i}
            H_{i}(C) 
            \xleftarrow{H_{i}(g)} 
            H_{i}(B)
            \xleftarrow{H_{i}(f)}
            H_{i}(A)
            \leftarrow \dotsb.
            $
            
            This method returns the complex $L$ such that, for all integers $i$, we have
            $L_{3i} = H_i(C)$, $L_{3i+1} = H_i(B)$, and $L_{3i+2} = H_i(A)$.
            The differentials $\operatorname{dd}^L_{3i}$ are the connecting 
            homomorphisms $\delta_i \colon H_i(C) \to H_{i-1}(A)$.  Moreover, we have
            $\operatorname{dd}^L_{3i+1} = H_{i}(g)$ and 
            $\operatorname{dd}^L_{3i+2} = H_{i}(f)$.
        Text
            For example, consider a free resolution $F$ of $S/I$.
            Applying the Hom functor $\operatorname{Hom}(F, -)$ to a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/h \leftarrow S \xleftarrow{h} S(- \deg h) \leftarrow 0
            $

            gives rise to a short exact sequence of complexes.  The corresponding long exact sequence $L$ in homology
            has the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Ext}^{d+1}(S/I, S(-\deg h))
            \xleftarrow{\delta} 
            \operatorname{Ext}^d(S/I, S/h)
            \leftarrow \operatorname{Ext}^d(S/I, S)
            \leftarrow \operatorname{Ext}^d(S/I, S(-\deg h))
            \leftarrow \dotsb.
            $
        Example
            S = ZZ/101[a..d, Degrees=>{2:{1,0},2:{0,1}}];
            h = a*c^2 + a*c*d + b*d^2;
            I = (ideal(a,b) * ideal(c,d))^[2]
            F = freeResolution comodule I;
            g = Hom(F, map(S^1/h, S^1, 1))
            f = Hom(F, map(S^1, S^{-degree h}, {{h}}))
            assert isWellDefined g
            assert isWellDefined f
            assert isShortExactSequence(g, f)
            L = longExactSequence(g,f)
            assert isWellDefined L
            assert(HH L == 0)
        Text
            We verify that the indexing on $L$ in this example matches
            the description above.
        Example
            delta = connectingMap(g, f);
            assert(dd^L_-9 === delta_-3)
            assert(dd^L_-8 === HH_-3 g)
            assert(dd^L_-7 === HH_-3 f)
            assert(dd^L_-6 === delta_-2)
            assert(dd^L_-5 === HH_-2 g)
            assert(dd^L_-4 === HH_-2 f)
            assert(dd^L_-3 === delta_-1)
    SeeAlso
        "Towards computing in the derived category"
        connectingMap
        isShortExactSequence
        homology
///

doc ///
    Key
        (connectingExtMap, Module, Matrix, Matrix)
        (connectingExtMap, Matrix, Matrix, Module)
        connectingExtMap
        [connectingExtMap, Concentration]
    Headline
        makes the connecting maps in Ext
    Usage
        connectingExtMap(M, g, f)
        connectingExtMap(g, f, M)
    Inputs
        M:Module
            over a ring $S$
        f:Matrix
            over a ring $S$ defining an injective map $f \colon A \to B$
        g:Matrix
            over a ring $S$ defining a surjective map $g \colon B \to C$
            such that the image of $f$ equals the kernel of $g$
        Concentration => Sequence
            not yet implemented
    Outputs
        :ComplexMap
    Description
        Text
            Since Ext is a bifunctor, there are two different connecting maps.
            The first comes from applying the covariant functor
            $\operatorname{Hom}_S(M, -)$ to a short exact sequence of modules.  The
            second comes from applying the contravariant functor
            $\operatorname{Hom}_S(-, M)$ to a short exact sequence of modules.
            More explicitly, given the short exact sequence

            $\phantom{WWWW}
            0 \leftarrow C \xleftarrow{g} B  \xleftarrow{f} A \leftarrow 0,
            $
            
            the $(-i)$-th connecting homomorphism is, in the first case, 
            a map $\operatorname{Ext}_S^i(M, C) \to \operatorname{Ext}_S^{i+1}(M, A)$
            and, in the second case,  
            a map $\operatorname{Ext}_S^i(A, M) \to \operatorname{Ext}_S^{i+1}(C, M)$.  Observe that 
            the connecting homomorphism is indexed homologically, whereas
            Ext modules are indexed cohomologically, explaining the
            different signs for the index $i$.
        Text
            As a first example,
            applying the functor $\operatorname{Hom}(S/I, -)$ to a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/h \leftarrow S \xleftarrow{h} S(- \deg h) \leftarrow 0
            $

            gives rise to the long exact sequence in Ext modules having the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Ext}^{i+1}(S/I, S(-\deg h))
            \xleftarrow{\delta_{-i}} 
            \operatorname{Ext}^i(S/I, S/h)
            \leftarrow \operatorname{Ext}^i(S/I, S)
            \leftarrow \operatorname{Ext}^i(S/I, S(-\deg h))
            \leftarrow \dotsb.
            $
        Example
            S = ZZ/101[a..d, Degrees=>{2:{1,0},2:{0,1}}];
            h = a*c^2 + a*c*d + b*d^2;
            I = (ideal(a,b) * ideal(c,d))^[2]
            g = map(S^1/h, S^1, 1)
            f = map(S^1, S^{-degree h}, {{h}})
            assert isShortExactSequence(g,f)
            delta = connectingExtMap(S^1/I, g, f)
            assert isWellDefined delta
            assert(degree delta == 0)            
            assert(source delta_(-1) == Ext^1(comodule I, S^1/h))
            assert(target delta_(-1) == Ext^2(comodule I, S^{{-1,-2}}))
        Text
            As a second example, 
            applying the functor $\operatorname{Hom}(-, S)$ to the
            short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/(I+J) \leftarrow S/I \oplus S/J  \leftarrow S/I \cap J \leftarrow 0
            $

            gives rise to the long exact sequence of Ext modules
            having the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Ext}^{i+1}(S/(I+J), S)
            \xleftarrow{\delta_{-i}} 
            \operatorname{Ext}^i(S/I \cap J, S)
            \leftarrow \operatorname{Ext}^i(S/I \oplus S/J, S)
            \leftarrow \operatorname{Ext}^i(S/(I+J), S)
            \leftarrow \dotsb.
            $
        Example
            S = ZZ/101[a..d];
            I = ideal(c^3-b*d^2, b*c-a*d)
            J = ideal(a*c^2-b^2*d, b^3-a^2*c)
            g = map(S^1/(I+J), S^1/I ++ S^1/J, {{1,1}})
            f = map(S^1/I ++ S^1/J, S^1/intersect(I,J), {{1},{-1}})
            assert isShortExactSequence(g,f)
            delta = connectingExtMap(g, f, S^1)
            assert isWellDefined delta
            assert(source delta_-2 == Ext^2(comodule intersect(I,J), S))
            assert(target delta_-2 == Ext^3(comodule (I+J), S))
    SeeAlso
        "Working with Ext"
        connectingMap
        isShortExactSequence
        longExactSequence
        Ext
        "Working with Tor"
///

doc ///
    Key
        (Tor, ZZ, Matrix, Module)
        (Tor, ZZ, Module, Matrix)
    Headline
        make the induced map on Tor modules
    Usage
        g = Tor_i(f, M)
        h = Tor_i(M, f)
    Inputs
        i:ZZ
        f:Matrix
            defining a homomorphism from the $R$-module $L$ to
            the $R$-module $N$
        M:Module
            over the ring $R$
    Outputs
        g:Matrix
            defining the induced homomorphism from
            $\operatorname{Tor}_i^R(L, M)$ to 
            $\operatorname{Tor}_i^R(N, M)$,
            or the matrix $h$
            defining the induced homomorphism from
            $\operatorname{Tor}_i^R(M, L)$ to 
            $\operatorname{Tor}_i^R(M, N)$
    Description
        Text
            The $\operatorname{Tor}$ functors are derived functors of
            the tensor product functor.
            Given a homomorphism $f \colon L \to N$
            of $R$-modules and an $R$-module $M$, this method returns
            the induced homomorphism 
            $g \colon \operatorname{Tor}_i^R(L, M) \to
            \operatorname{Tor}_i^R(N, M)$.
            
        Example
            R = ZZ/101[a..d];
            L = R^1/ideal(a^2, b^2, c^2, a*c, b*d)
            N = R^1/ideal(a^2, b^2, c^2, a*c, b*d, a*b)
            f = map(N,L,1)
            M = coker vars R
            betti freeResolution L
            betti freeResolution N
            g1 = Tor_1(f, M)
            g2 = Tor_2(f, M)
            g3 = Tor_3(f, M)
            g4 = Tor_4(f, M)
            assert(source g2 === Tor_2(L, M))
            assert(target g2 === Tor_2(N, M))
            prune ker g3
            prune coker g3
        Text
            Although the $\operatorname{Tor}$ functors are symmetric,
            the actual matrices depend on the order of the arguments.
        Example
            M = R^1/ideal(a^2,b^2,c^3,b*d)
            h1 = Tor_1(M, f)
            h1' = Tor_1(f, M)
            Tor_1(L, M)
            Tor_1(M, L)
            assert(source h1 == Tor_1(M, L))
            assert(source h1' == Tor_1(L, M))
            h2 = Tor_2(M, f)
            h2' = Tor_2(f, M)
            prune h2
            prune h2'
    SeeAlso
        "Working with Tor"
///

doc ///
    Key
        (torSymmetry, ZZ, Module, Module)
        torSymmetry
    Headline
        makes the canonical isomorphism realizing the symmetry of Tor
    Usage
        torSymmetry(i, M, N)
    Inputs
        i:ZZ
        M:Module
        N:Module
            over the same ring $R$ as $M$
    Outputs
        :Matrix
            representing an $R$-module homomorphism from
            $\operatorname{Tor}_i^R(M, N)$ to $\operatorname{Tor}_i^R(N, M)$
    Description
        Text
            @TO2(tensorCommutativity, "Tensor commutativity")@
            gives rise to an isomorphism from
            $\operatorname{Tor}_i^R(M, N)$ to $\operatorname{Tor}_i^R(N, M)$.
            This method returns this isomorphism.
        Text
            We compute the Betti numbers of the Veronese surface
            in two ways: $\operatorname{Tor}(M, \ZZ/101)$ or
            via Koszul cohomology $\operatorname{Tor}(\ZZ/101, M)$.
        Example
            S = ZZ/101[a..f];
            I = trim minors(2, genericSymmetricMatrix(S, 3))
            M = S^1/I;
            N = coker vars S
            f1 = torSymmetry(1,M,N)
            f2 = torSymmetry(1,N,M)
            assert(f1 * f2 == 1)
            assert(f2 * f1 == 1)
            g1 = torSymmetry(2,M,N);
            g2 = torSymmetry(2,N,M);
            assert(g1 * g2 == 1)
            assert(g2 * g1 == 1)
            h1 = torSymmetry(3,M,N);
            h2 = torSymmetry(3,N,M);
            assert(h1 * h2 == 1)
            assert(h2 * h1 == 1)
        Text
            Although the Tor modules are isomorphic, they are
            presented with different numbers of generators.  As a
            consequence, the matrices need not be square.
            For example, after pruning the modules, $f_1$ and $f_2$
            are represented by the same matrix.
        Example
            p1 = prune f1
            p2 = prune f2
            assert(p1 * p2 == 1)
    SeeAlso
        "Working with Tor"
        Tor
///


-- XXX start here.  We just need to figure out symmetry of Tor
-- and relate to the connecting maps
doc ///
    Key
        (connectingTorMap, Module, Matrix, Matrix)
        (connectingTorMap, Matrix, Matrix, Module)
        connectingTorMap
        [connectingTorMap, Concentration]
    Headline
        makes the connecting maps in Tor
    Usage
        connectingTorMap(M, g, f)
        connectingTorMap(g, f, M)
    Inputs
        M:Module
            over a ring $S$
        f:Matrix
            over a ring $S$ defining an injective map $f \colon A \to B$
        g:Matrix
            over a ring $S$ defining a surjective map $g \colon B \to C$
            such that the image of $f$ equals the kernel of $g$
        Concentration => Sequence
            not yet implemented
    Outputs
        :ComplexMap
    Description
        Text
            Since Tor is a bifunctor, there are two different connecting maps.
            The first comes from applying the covariant functor
            $M \otimes_S -$ to a short exact sequence of modules.  The
            second comes from applying the covariant functor
            $- \otimes_S M$ to a short exact sequence of modules.
            More explicitly, given the short exact sequence

            $\phantom{WWWW}
            0 \leftarrow C \xleftarrow{g} B  \xleftarrow{f} A \leftarrow 0,
            $
            
            the $i$-th connecting homomorphism is, in the first case, 
            a map $\operatorname{Tor}^S_i(M, C) \to \operatorname{Tor}^S_{i-1}(M, A)$
            and, in the second case,  
            a map $\operatorname{Tor}^S_i(A, M) \to \operatorname{Tor}^S_{i-1}(C, M)$.
        Text
            As a first example, let $\Bbbk$ be the base field as an $S$-module.
            Applying the functor $\Bbbk \otimes_S -$ to a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/(I+J) \leftarrow S/I \oplus S/J  \leftarrow S/I \cap J \leftarrow 0
            $

            gives rise to the long exact sequence of Tor modules
            having the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Tor}_{i-1}(\Bbbk, S/(I+J))
            \xleftarrow{\delta_{i}} 
            \operatorname{Tor}_i(\Bbbk, S/I \cap J)
            \leftarrow \operatorname{Tor}_i(\Bbbk, S/I \oplus S/J)
            \leftarrow \operatorname{Tor}_i(\Bbbk, S/(I+J))
            \leftarrow \dotsb.
            $
        Example
            S = ZZ/101[a..d];
            I = ideal(c^3-b*d^2, b*c-a*d)
            J = ideal(a*c^2-b^2*d, b^3-a^2*c)
            g = map(S^1/(I+J), S^1/I ++ S^1/J, {{1,1}});
            f = map(S^1/I ++ S^1/J, S^1/intersect(I,J), {{1},{-1}});
            assert isShortExactSequence(g,f)
            kk = coker vars S
            delta = connectingTorMap(kk, g, f)
            assert isWellDefined delta
            assert(source delta_2 == Tor_2(kk, target g))
            assert(target delta_2 == Tor_1(kk, source f))
            prune delta
        Text
            The connecting homomorphism fits into a natural
            long exact sequence in Tor.
        Example
            F = freeResolution kk;
            LES = longExactSequence(F ** g, F ** f);
            assert all(3, i -> dd^LES_(3*(i+1)) == delta_(i+1))
            assert(HH LES == 0)
        Text
            As a second example,
            applying the functor $- \otimes_S \Bbbk$ to a short exact sequence of modules

            $\phantom{WWWW}
            0 \leftarrow S/(I+J) \leftarrow S/I \oplus S/J  \leftarrow S/I \cap J \leftarrow 0
            $

            gives rise to the long exact sequence of Tor modules
            having the form

            $\phantom{WWWW}
            \dotsb \leftarrow \operatorname{Tor}_{i-1}(S/(I+J), \Bbbk)
            \xleftarrow{\delta_{i}} 
            \operatorname{Tor}_i(S/I \cap J, \Bbbk)
            \leftarrow \operatorname{Tor}_i(S/I \oplus S/J, \Bbbk)
            \leftarrow \operatorname{Tor}_i(S/(I+J), \Bbbk)
            \leftarrow \dotsb.
            $
        Example
            delta' = connectingTorMap(g, f, kk)
            assert isWellDefined delta'
            (g',f') = horseshoeResolution(g,f);
            assert isShortExactSequence(g',f')
            LES' = longExactSequence(g' ** kk, f' ** kk);
            assert(HH LES' == 0)
            assert all(3, i -> dd^LES'_(3*(i+1)) == delta'_(i+1))
    SeeAlso
        "Working with Tor"
        connectingMap
        isShortExactSequence
        longExactSequence
        Tor
        "Working with Ext"
///

///
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


///
        Text
            XXX as a next example, we show a non-zero connecting homomorphism 
            in a Tor long exact sequence. (in connectingMap doc node)
        Text
            TODO: connectingExtMap(i, M, ses in N's), (ses in M's, N),
            TODO: connectingTorMap(i, M, ses in N's), (M, ses in N's)
            TODO: example: functoriality of LES, connecting map.
            TODO: given a map M --> M', ses in N's, ==> get complex map between LES's.
            TODO: email from Janina Letz on March 23, 2021, has some bugs in Complexes...
///

