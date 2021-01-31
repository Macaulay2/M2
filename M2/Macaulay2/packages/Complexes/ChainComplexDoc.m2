--------------------------------------------------------------------
-- documentation for chain complexes -------------------------------
--------------------------------------------------------------------
doc ///
    Key
        Complexes
    Headline
        development package for beta testing new version of chain complexes    
    Description
        Text
            This package develops new data types and routines for homological algebra.
            Eventually, it will replace the current facilities for homological algebra.
            We are making this available in order to get feedback from users before
            making this change.  Please email the authors with any and all comments or
            suggestions.
        Text
            The major change is replacing the @TO ChainComplex@ data type with @TO Complex@.
            The internal structure of this new data type is somewhat different, but more
            importantly, it has a richer set of constructors.  Use the functions
            @TO (complex, ChainComplex)@, @TO (complex, ChainComplexMap)@, @TO (chainComplex, Complex)@, @TO (chainComplex, ComplexMap)@, 
            to translate between these representations.
        Text
            The overarching goal is to make all of the homological algebra routines functorial.
            For instance, we have @TO2(canonicalMap, "canonical maps")@ associated to kernels,
            cokernels, images, coimages, cones, and cylinders.
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Making chain complexes"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (complex, HashTable),
                TO (complex, List),
                TO (complex, Module), 
                TO (complex, Complex),
                TO (symbol SPACE, Complex, Array),
                TO (isWellDefined, Complex)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new complexes"@
	Text
    	    @UL {
                TO (freeResolution, Module),
                TO (resolution, Complex),
                TO (homology, Complex)
            }@
    	Text
    	    @SUBSECTION "More advanced constructors"@
	Text
    	    @UL {
                TO (symbol++, Complex, Complex),
                TO (symbol**, Complex, Complex),
                TO (Hom, Complex, Complex),
                TO (dual, Complex),
                TO (symbol SPACE, RingMap, Complex),
                TO (symbol **, RingMap, Complex),
                TO (naiveTruncation, Complex, ZZ, ZZ),
                TO (canonicalTruncation, Complex, ZZ, ZZ),
                TO (minimalPresentation, Complex),
                TO (minimize, Complex),
                TO (gradedModule, Complex),
                TO (part, List, Complex),
                TO (truncate, List, Complex),
                TO (yonedaExtension, Matrix)
            }@
    	Text
    	    @SUBSECTION "Extracting complexes from complex maps"@
        Text
    	    @UL {
                TO (source, ComplexMap),
                TO (target, ComplexMap),
                TO (kernel, ComplexMap),
                TO (cokernel, ComplexMap),
                TO (image, ComplexMap),
                TO (coimage, ComplexMap),
                TO (cone, ComplexMap),
                TO (cylinder, ComplexMap)
            }@
    SeeAlso
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Basic invariants and properties"
    Headline
        information about accessing basic features
    Description
    	Text
    	    @SUBSECTION "Predicates for complexes and complex maps"@
        Text
    	    @UL {
                TO (isWellDefined, Complex),
                TO (isExact, Complex),
                TO (isHomogeneous, Complex),
                TO (isFree, Complex),
                TO (isWellDefined, ComplexMap),
                TO (isCommutative, ComplexMap),
                TO (isComplexMorphism, ComplexMap),
                TO (isShortExactSequence, ComplexMap, ComplexMap),
                TO (isQuasiIsomorphism, ComplexMap),
                TO (isNullHomotopic, ComplexMap),
                TO (isNullHomotopyOf, ComplexMap, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Other invariants for complexes"@
        Text
    	    @UL {
                TO (ring, Complex),
                TO (concentration, Complex),
                TO (max, Complex),
                TO (min, Complex),
                TO (length, Complex),
                TO (regularity, Complex),
                TO (betti, Complex),
                TO (poincare, Complex),
                TO (poincareN, Complex),
                TO (components, Complex)
            }@
    	Text
    	    @SUBSECTION "Other invariants for complex maps"@
        Text
    	    @UL {
                TO (source, ComplexMap),
                TO (target, ComplexMap),
                TO (degree, ComplexMap),
                TO (ring, ComplexMap),
                TO (concentration, ComplexMap),
                TO (isHomogeneous, ComplexMap),
                TO (components, ComplexMap),
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Working with Ext"
    Headline
        information about functorial properties
    Description
    	Text
    	    @SUBSECTION "Hom and Ext"@
        Text
    	    @UL {
                TO (Ext, ZZ, Module, Module),
                TO "(Ext, ZZ, Matrix, Matrix)",
                TO "(Ext, ZZ, Matrix, Module)",
                TO "(Ext, ZZ, Module, Matrix)",
                TO (Hom, Complex, Complex),
                TO (Hom, ComplexMap, ComplexMap),
                TO (homomorphism, ComplexMap),
                TO (homomorphism', ComplexMap)
            }@
        Text
    	    @SUBSECTION "Yoneda extensions and elements of Ext"@
        Text
    	    @UL {
                TO (yonedaMap, Matrix),
                TO (yonedaMap', ComplexMap),
                TO (yonedaExtension, Matrix),
                TO (yonedaExtension', Complex),
                TO (yonedaProduct, Matrix, Matrix),
                TO (yonedaProduct, Module, Module)
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Working with Tor"
    Headline
        information about functorial properties
    Description
        Text
    	    @UL {
                TO (Tor, ZZ, Module, Module),
                TO "(Tor, ZZ, Module, Matrix)",
                TO "(Tor, ZZ, Matrix, Module)",
                TO "(Tor, ZZ, Matrix, Matrix)",
                TO (tensor, Complex, Complex),
                TO (tensor, ComplexMap, ComplexMap),
                TO (tensorCommutativity, Complex, Complex),
                TO (tensorAssociativity, Complex, Complex, Complex),
                TO "symmetry of Tor"
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Towards computing in the derived category"
///

--------------------------------------------------------------------
-- chain complexes -------------------------------------------------
--------------------------------------------------------------------
doc ///
    Key
        Complex
    Headline
        the class of all chain complexes
    Description
        Text
            A complex is a sequence of objects $C_i$, connected by
            maps $dd^C_i : C_i \rightarrow C_{i-1}$ such that the
            composition of any two consecutive maps is zero.

            TODO: more needs to be added here explaining how to used complexes.
            and links to "landing pages".
///

doc ///
    Key
        (ring, Complex)
        (ring, ComplexMap)
    Headline
        access the ring of a complex or a complex map
    Usage
        ring C
    Inputs
        C:Complex
            or a @TO "ComplexMap"@
    Outputs
        :Ring
    Description
        Text
            Every complex or complex map has a base ring.  This
            function access that information.
        Example
            S = ZZ/101[a,b,c,d];
            C = freeResolution coker vars S
            ring C
            assert(ring C === S)
            ring id_C
            assert(ring id_C === S)
    SeeAlso
        "Basic invariants and properties"
        ring
///

doc ///
    Key
        concentration
        (concentration, Complex)
        (max, Complex)
        (min, Complex)
    Headline
        indices on which a complex may be non-zero
    Usage
        (lo,hi) = concentration C
    Inputs
        C:Complex
    Outputs
        lo:ZZ
        hi:ZZ
            a pair of integers {\tt lo}, {\tt hi} such that {\tt C_i = 0}
            for {\tt i < lo} or {\tt i > hi}.
    Description
        Text
            In this package, each complex has a concentration {\tt (lo, hi)} 
            such that {\tt lo <= hi}.  When {\tt lo <= i <= hi}, the module
            {\tt C_i} might be zero.  The methods {\tt max} and {\tt min} 
            applied to the complex {\tt C} return {\tt lo} and {\tt hi}, respectively.
      
            This function is mainly used in programming, to loop over all
            non-zero modules or maps in the complex.  This should not be confused
            with the support of a complex.
        Example
            S = ZZ/101[a..c];
            C = freeResolution coker vars S
            concentration C
            D = C ++ C[5]
            concentration D
            min D
            max D
            assert((min D, max D) === concentration D)
        Text
            Indices that are outside of the concentration automatically
            return the zero object.
        Example
            C_-1
            D_4
        Text
            The function {\tt concentration} does no computation.
            To eliminate extraneous zeros, use @TO (prune,Complex)@.
        Example
            f1 = a*id_C  
            E = ker f1
            concentration E
            concentration prune E
        Text
            The concentration of a zero complex can be arbitrary, however,
            after pruning, its concentration will be {\tt (0,0)}.
        Example      
            C0 = (complex S^0)[4]
            concentration C0
            prune C0
            concentration oo
    SeeAlso
        "Basic invariants and properties"
        (symbol _, Complex, ZZ)
        (concentration, ComplexMap)
///

doc ///
    Key
        (complex, HashTable)
    Headline
        make a chain complex
    Usage
        complex H
    Inputs
        H:HashTable
            each key is an integer indexing a differential, and the 
            value at that key is the map
        Base => ZZ
            ignored when the input is a hash table
    Outputs
        :Complex
    Description
        Text
            A complex is a sequence of objects (e.g. modules),
            connected by maps called differentials.  The composition
            of any two consecutive maps is zero.
      
            The same data type is used for both chain and cochain
            complexes.  If {\tt C} is a complex, then we have 
            {\tt C^i = C_{-i}}.

            We construct the Koszul complex on the generators for the
            ideal of the twisted cubic curve.
        Example
            S = ZZ/101[a..d]
            I = ideal(b^2-a*c, b*c-a*d, c^2-b*d)
            F1 = map(S^1,,matrix{{I_0, I_1, I_2}})
            F2 = map(source F1,,matrix{
                    {0, I_2, -I_1},
                    {-I_2, 0, I_0},
                    {I_1, -I_0, 0}
                    })
            F3 = map(source F2,,matrix{{I_0}, {I_1}, {I_2}})
            C = complex hashTable{1 => F1, 2 => F2, 3 => F3}
            isWellDefined C
        Text
            This is the primary constructor used by all of the more
            user friendly methods for constructing a chain complex.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, Complex)@.
    SeeAlso
        "Making chain complexes"
        (isWellDefined, Complex)
        (complex, List)
        (complex, Module)
///

doc ///
    Key
        complex
        (complex, List)
        Base
    Headline
        make a chain complex
    Usage
        complex L
    Inputs
        L:List
            of maps
        Base => ZZ
            the index of the target of the first map 
            in the differential.
    Outputs
        :Complex
    Description
        Text
            A complex is a sequence of objects (e.g. modules),
            connected by maps called differentials.  The composition
            of any two consecutive maps is zero.
      
            The same data type is used for both chain and cochain
            complexes.  If {\tt C} is a complex, then we have
            {\tt C^i = C_{-i}}.

            Often, a complex is most easily described by giving a list
            of consecutive maps which form the differential.

            We construct the Koszul complex on the generators for the
            ideal of the twisted cubic curve.
        Example
            S = ZZ/101[a..d]
            I = ideal(b^2-a*c, b*c-a*d, c^2-b*d)
            F1 = map(S^1,,matrix{{I_0, I_1, I_2}})
            F2 = map(source F1,,matrix{
                    {0, I_2, -I_1},
                    {-I_2, 0, I_0},
                    {I_1, -I_0, 0}
                    })
            F3 = map(source F2,,matrix{{I_0}, {I_1}, {I_2}})
            C = complex {F1, F2, F3}
            isWellDefined C
        Text
            To start a complex at a base different from zero, use the
            optional argument {\tt Base}.
        Example
            C1 = complex({F1, F2, F3}, Base => 1)
            isWellDefined C1
        Text
            Notice that this changes the homological degrees of the
            maps, but is not the same as the shift of the complex
            (which for odd shifts negates the maps).
        Example
            dd^C1
            dd^(C[-1])
        Text
            Having constructed this complex, we can access individual
            terms and maps.
        Example
            C_2
            C^(-1)
            C^(-1) == C_1
            C_7
            dd^C
            dd^C_2
            length C
        Text
            By computing the homology of this complex, we see that
            these generators do not form a regular sequence, because
            $H_1(C)$ is non-zero.
        Example
            HH C
            prune HH C
            prune HH_1 C
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, Complex)@.
    SeeAlso
        "Making chain complexes"
        (isWellDefined, Complex)
        (complex, HashTable)
        (complex, Module)
        (symbol SPACE, Complex, Array)
///

doc ///
    Key
        (complex, Module)
        (complex, Ideal)
        (complex, Ring)
    Headline
        make a chain complex of length zero
    Usage
        complex M
    Inputs
        M:Module
            or @TO "Ideal"@, or @TO "Ring"@.
        Base => ZZ
            index for {\tt M}
    Outputs
        :Complex
            returns the complex whose 0-th component is {\tt M}.
    Description
        Text
            In contrast to @TO (complex,HashTable)@ and @TO
            (complex,List)@, this constructor provides a convenient
            method to construct a complex with only one non-zero
            component.
      
            We illustrate this with a free module.
        Example
            S = ZZ/101[a..d]
            C0 = complex S^2
            f = dd^C0
            source f, target f
            f == 0
            isWellDefined C0
            C0 == 0
            length C0
        Example
            C1 = complex(S^2, Base=>3)
            C1 == C0[-3]
            C1_3
            C1_0
        Text
            A ring or an ideal will be converted to a module first.
        Example
            C2 = complex S
            I = ideal(a^2-b, c^3)
            C3 = complex I
            C4 = complex (S/I)
            (ring C3, ring C4)
        Text
            The zero complex over a ring {\tt S} is most conveniently
            created by giving the zero module.
        Example
            C5 = complex S^0
            C5 == 0
            dd^C5 == 0
            C5_0
    SeeAlso
        "Making chain complexes"
        (isWellDefined, Complex)
        (complex, HashTable)
///

doc ///
    Key
        (complex, Complex)
    Headline
        make a complex by reindexing the terms of the complex
    Usage
        D = complex(C, Base => i)
    Inputs
        C:Complex
        Base => ZZ
            the target of the lowest differential
    Outputs
        D:Complex
            where if $d$ is the smallest index of the target of the
            differentials of C, then for all integers $j$ we have
            $D_{i+j} = C_{d+j}$
    Description
        Text
            This returns an alteration of the input complex,
            reindexing the terms of the complex.
        Example
            S = ZZ/101[a..d]
            C = freeResolution coker vars S
            D = complex(C, Base => 1)
            E = complex(D, Base => -11)
            dd^D_2 == dd^C_1
            dd^E_-9 == dd^C_2
        Text
            Rather than specifying the homological degree of the
            lowest target, one can also shift the homological degree,
            which may simultaneously negate the maps.
        Example
            F = C[-1]
            for i from min F to max F list
                dd^F_i == - dd^D_i
    SeeAlso
        "Making chain complexes"
        (isWellDefined, Complex)
        (complex, HashTable)
        (complex, List)
        (symbol SPACE, Complex, Array)
///

-- TODO: add in doc for (complex, ComplexMap).

-- TODO: Add programming details
doc ///
   Key
     (isWellDefined, Complex)
   Headline
     whether a complex is well-defined
   Usage
     isWellDefined C
   Inputs
     C:Complex
   Outputs
     :Boolean
       that is true when {\tt C} determines a well defined complex
   Description
    Text
      This routine checks that the differential of {\tt C} composes to zero.
      Additionally, it checks that the underlying data in {\tt C} is a properly formed
      Complex object in Macaulay2. If the variable {\tt debugLevel} is set to a value greater than zero,
      then information about the nature of any failure is displayed.
    Text

      As a first example, we construct by hand the free resolution of the twisted
      cubic.  One must work with maps rather than matrices, because the source and the target
      of adjacent maps must be the same (including degree information).
    Example
      R = QQ[a..d];
      f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
      f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
      C = complex {f0, f1}
      isWellDefined C
      dd^C
      (dd^C)^2
    Text
      The zero complex is well-defined.
    Example
      C = complex R^0
      isWellDefined C
    Text
    
      The next example demonstrates the case when the sequence maps do not compose to 0.
    Example
      g1 = map(source f0,, {{-d, c}, {c, b}, {b, a}})
      C = complex {f0, g1}
      isWellDefined C
      debugLevel = 1
      isWellDefined C
      (dd^C)^2
   SeeAlso
     (isWellDefined, ComplexMap)
     map
///

doc ///
   Key
     (symbol _, Complex, ZZ)
     (symbol ^, Complex, ZZ)
   Headline
     access individual object in a complex
   Usage
     C_i
     C^i
   Inputs
     C:Complex
     i:ZZ
       either the homological or cohomological index
   Outputs
     :Module
       the {\tt i}-th object
   Description
    Text
       Complexes can be either chain complexes or cochain complexes.  Subscripts
       refer to homological indices, and superscripts refer to
       cohomological indices.
     
       In this package homological indices are used by default.  For
       example, the @TO "concentration"@ references homological indices.
       Nevertheless, we always have the equation $C^i = C_{-i}$.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      C_2
      C^(-2)
      C_2 == C^(-2)
    Text
      Indices that are outside of the concentration automatically
      return the zero object.
    Example
      C_-7
   SeeAlso
///

doc ///
   Key
     (symbol ==, Complex, Complex)
     (symbol ==, Complex, ZZ)
     (symbol ==, ZZ, Complex)
   Headline
     whether two complexes are equal
   Usage
     C == D
     C == 0
   Inputs
     C:Complex
     D:Complex
   Outputs
     :Boolean
       that is true when {\tt C} and {\tt D} are equal
   Description
    Text
      Two complexes are equal if the corresponding 
      objects and corresponding maps at each index are equal.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      D = C[3][-3]
      C === D
      C == D
    Text
      Both the maps and the objects must be equal.
    Example
      (lo,hi) = concentration C
      E = complex for i from lo+1 to hi list 0*dd^C_i
      dd^E
      C == E
      E == 0
    Text
      A complex is equal to zero if all the objects and maps are zero.
      This could require computation to determine if something that
      is superficially not zero is in fact zero.
    Example
      f = id_C
      D = coker f
      D == 0
    Example
      C0 = complex S^0
      C1 = C0[4]
      concentration C0 == concentration C1
      C0 == C1
      C0 == 0
      C1 == 0
    Text
      Testing for equality is not the same testing for isomorphism.
      In particular, different presentations of a complex need not be equal.
    Example
      R = QQ[a..d];
      f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
      f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
      C = complex {f0, f1}
      HH C != complex coker f0
      prune HH C == complex coker f0
   Caveat
   SeeAlso
///

-- XXX

doc ///
    Key
        "differential of a chain complex"
        (symbol^, Symbol, Complex)
    Headline
        get the maps between the terms in a complex
    Usage
        dd^C
    Inputs
        C:Complex
    Outputs
        :ComplexMap
            a map of degree -1
    Description
        Text
            A chain complex is a sequence of modules connected
            by homomorphisms, called differentials, such that
            the composition of any two consecutive maps is zero.
        Text
            One can access the differential of a complex as follows.
        Example
            R = QQ[a..d];
            I = ideal(a*d-b*c, b^2-a*c, c^2-b*d);
            C = freeResolution(R^1/I)
            dd^C
            C.dd
            assert(dd^C === C.dd)
            assert(source dd^C === C)
            assert(target dd^C === C)
            assert(degree dd^C === -1)
        Text
            The composition of the differential with itself is zero.
        Example
            (dd^C)^2 == 0
        Text
            The individual maps between terms are indexed by their
            source.
        Example
            dd^C_2
            assert(source dd^C_2 === C_2)
            assert(target dd^C_2 === C_1)
    SeeAlso
        "Making maps between chain complexes"
        (symbol_, ComplexMap, ZZ)
        (symbol_, Complex, ZZ)
        (source, ComplexMap)
        (target, ComplexMap)
        (degree, ComplexMap)
///

doc ///
   Key
     (symbol SPACE, Complex, Array)
     (symbol SPACE, ComplexMap, Array)
   Headline
     shift a complex or complex map
   Usage
     D = C[i]
     g = f[i]
   Inputs
     C:Complex
       or {\tt f}, a @TO ComplexMap@
     :Array
       {\tt [i]}, where {\tt i} is an integer
   Outputs
     D:Complex
       or {\tt g}, a @TO ComplexMap@.
   Description
    Text
      The shifted complex $D$ is defined by $D_j = C_{i+j}$ for all $j$
      and the sign of the differential is changed if $i$ is odd.
       
      The shifted complex map $g$ is defined by $g_j = f_{i+j}$ for all $j$.
    
      The shift defines a natural automorphism on the category of complexes. 
      Topologists often call the shifted complex $C[1]$ the {\it suspension} of $C$.
    Example
      S = ZZ/101[a..d]
      C = freeResolution coker vars S
      dd^C_3
      D = C[1]
      assert isWellDefined D
      assert(dd^D_2 == -dd^C_3)
    Text
      In order to shift the complex one step, and not change the differential, one
      can do the following.
    Example
      E = complex(C, Base => -1)
      assert isWellDefined E
      assert(dd^E_2 == dd^C_3)
    Text
      The shift operator is functorial, as illustrated below.
    Example
      C2 = freeResolution (S^1/(a^2, b^2, c^2, d^2))
      C3 = freeResolution (S^1/(a^2, b^3, c^4, d^5))
      f2 = extend(C, C2, map(C_0, C2_0, 1))
      f3 = extend(C2, C3, map(C2_0, C3_0, 1))
      assert((f2*f3)[1] == (f2[1]) * (f3[1]))
      assert(source(f2[1]) == C2[1])
      assert(target(f2[1]) == C[1])
   SeeAlso
     concentration
     (complex, Complex)
     (extend, Complex, Complex, Matrix)
///

doc ///
    Key
        (gradedModule, Complex)
    Headline
        a new complex in which the differential is zero
    Usage
        gradedModule C
    Inputs
        C:Complex
    Outputs
        :Complex
            whose differential is the zero map
    Description
        Text
            This routine isolates the terms in the complex
            and forgets the differentials
        Example
            R = ZZ/101[a,b,c,d,e];
            I = intersect(ideal(a,b),ideal(c,d,e))
            C = (dual freeResolution I)[-4]
            dd^C
            G = gradedModule C
            dd^G
            assert(isWellDefined G)
            assert(G != C)
        Text
            The homology of a complex already has zero differential.
        Example
            H = HH C
            prune H
            dd^H == 0
            assert(H == gradedModule H)
    SeeAlso
        (homology, Complex)
///

doc ///
    Key
        freeResolution
        (freeResolution, Module)
        (freeResolution, Ideal)
    Headline
        compute a free resolution of a module or ideal
    Usage
        freeResolution M
    Inputs
        M:Module
            or @ofClass Ideal@, an ideal {\tt I} in a ring {\tt R}
    Outputs
        :Complex
            a free resolution of the module {\tt M} or of the
            quotient module {\tt R^1/I}
    Description
        Text
            A free resolution of a module $M$ is a complex
            $ F_0 \leftarrow F_1 \leftarrow F_2 \leftarrow \ldots$
            of free modules, which is acyclic: the cokernel of the map
            to $F_0$ is $M$ and the complex is exact at all other 
            locations.
        Example
            R = QQ[a..d]
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            M = R^1/I
            C = freeResolution M
            betti C
            length C
            dd^C
            assert isWellDefined C
            assert(prune HH C == complex M)
        Text
            Giving an ideal as the input produces a free resolution
            not of the module {\tt I}, but of the module {\tt R^1/I}.
        Example
            assert(freeResolution I == C)
            resolution complex M == freeResolution M
        Text
            Over a quotient ring, free resolutions are often infinite.
            Use the optional argument {\tt LengthLimit} to obtain
            part of the resolution.
        Example
            S = ZZ/101[a,b]
            R = S/(a^3+b^3)
            C = freeResolution (coker vars R, LengthLimit => 7)
            dd^C
    SeeAlso
        (resolution, Complex)
        (resolutionMap, Complex)
        (betti, Complex)
///

doc ///
   Key
     (homology, Complex)
   Headline
     homology of a complex
   Usage
     H = HH C
   Inputs
     C:Complex
   Outputs
     H:Complex
   Description
    Text
      The homology complex $H$ is defined by {\tt ker dd^C}/{\tt image dd^C}.
      The differential of the homology complex is the zero map.
      
      The first example is the complex associated to
      a triangulation of the real projective plane, having
      6 vertices, 15 edges, and 10 triangles.
    Example
      d1 = matrix {
          {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
          {0, -1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0}, 
          {0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 1, 0}, 
          {0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, -1}}
      d2 = matrix {
          {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 0, -1, -1, 0, 0, 0, 0, 0, 0}, 
          {1, 0, 1, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 1, 0, 0, -1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 1, 1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, -1, -1, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 0, 0, -1, 0, 0}, 
          {0, -1, 0, 0, 0, 1, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 1, 1, 0, 0}, 
          {0, 0, -1, 0, 0, 0, 0, 0, -1, 0}, 
          {0, 0, 0, 0, 0, -1, 0, 0, 1, 0}, 
          {0, 0, 0, -1, 0, 0, -1, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 0, 0, -1, -1}, 
          {0, 0, 0, 0, 0, 0, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, 0, -1}}
      C = complex {d1,d2}
      dd^C
      H = HH C
      dd^H == 0
    Text
      To see that the first homology group has torsion,
      we compute a minimal presentation of the homology.
    Example
      Hpruned = prune HH C
      dd^Hpruned == 0
    Text
      By dualizing the minimal free resolution of a monomial ideal,
      we get a free complex with non-trivial homology.  This particular
      complex is related to the local cohomology supported at the
      monomial ideal.
    Example
      S = ZZ/101[a..d, DegreeRank=>4];
      I = intersect(ideal(a,b),ideal(c,d))
      C = freeResolution (S^1/I)
      prune HH C
      Cdual = dual C
      prune HH Cdual
      prune HH_(-2) Cdual
   SeeAlso
     (dual, Complex)
     (prune, Complex)
///



doc ///
   Key
     (homology,ZZ,Complex)
     (cohomology,ZZ,Complex)
   Headline
     homology or cohomology module of a complex
   Usage
     HH_i C
     HH^i C
   Inputs
     i:ZZ
     C:Complex
   Outputs
     :Module
       the $i$-th homology or cohomology of the complex
   Description
    Text
      The $i$-th homology of a complex $C$ is the quotient
      ({\tt ker dd^C_i/image dd^C_(i+1)}).

      The first example is the complex associated to
      a triangulation of the real projective plane, having
      6 vertices, 15 edges, and 10 triangles.
    Example
      d1 = matrix {
          {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
          {0, -1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0}, 
          {0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 1, 0}, 
          {0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, -1}};
      d2 = matrix {
          {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 0, -1, -1, 0, 0, 0, 0, 0, 0}, 
          {1, 0, 1, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 1, 0, 0, -1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 1, 1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, -1, -1, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 0, 0, -1, 0, 0}, 
          {0, -1, 0, 0, 0, 1, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 1, 1, 0, 0}, 
          {0, 0, -1, 0, 0, 0, 0, 0, -1, 0}, 
          {0, 0, 0, 0, 0, -1, 0, 0, 1, 0}, 
          {0, 0, 0, -1, 0, 0, -1, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 0, 0, -1, -1}, 
          {0, 0, 0, 0, 0, 0, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, 0, -1}};
      C = complex {d1,d2}
      dd^C
      HH C
      prune HH_0 C
      prune HH_1 C
      prune HH_2 C
    Text
      The $i$-th cohomology of a complex $C$ is the $(-i)$-th
      homology of $C$.
    Example
      S = ZZ/101[a..d, DegreeRank=>4];
      I = intersect(ideal(a,b),ideal(c,d))
      C = dual freeResolution (S^1/I)
      prune HH^1 C
      prune HH^2 C
      prune HH^3 C
   SeeAlso
     prune
     (dual, Complex)
///

doc ///
   Key
     (directSum, Complex)
     (symbol++, Complex, Complex)
   Headline
     direct sum of complexes
   Usage
     D = C1 ++ C2
     D = directSum(C1,C2,...)
     D = directSum(name1 => C1, name2 => C2, ...)
   Inputs
     Ci:Complex
   Outputs
     D:Complex
       the direct sum of the input complexes
   Description
    Text
      The direct sum of two complexes is another complex.
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C1 ++ complex(S^13)[-2]
      C2 = complex (ideal(a,b,c))
      C1 ++ C2
      assert isWellDefined(C1 ++ C2)
    Text
      The direct sum of a sequence of complexes can be computed as follows.
    Example
      C3 = directSum(C1,C2,C2[-2])
      assert isWellDefined C3
    Text
      The direct sum is an n-ary operator with projection and
      inclusion maps from each component satisfying appropriate
      identities.
    Example
      C4 = directSum(first => C1, second => C2)
      C4_[first] -- inclusion map C1 --> C4
      C4^[first] -- projection map C4 --> C1
      C4^[first] * C4_[first] == 1
      C4^[second] * C4_[second] == 1
      C4^[first] * C4_[second] == 0
      C4^[second] * C4_[first] == 0
      C4_[first] * C4^[first] + C4_[second] * C4^[second] == 1
    Text
      There are two short exact sequences associated to a direct sum.
    Example
      isShortExactSequence(C4^[first], C4_[second])
      isShortExactSequence(C4^[second], C4_[first])
    Text
      Given a complex which is a direct sum, we obtain the component
      complexes and their names (indices) as follows.
    Example
      components C3
      indices C3
      components C4
      indices C4
   SeeAlso
     (components,Complex)
     indices
     (symbol^, Complex, Array)
     (symbol_, Complex, Array)
     (isShortExactSequence, ComplexMap, ComplexMap)
///

doc ///
   Key
     (symbol_, Complex, Array)
     (symbol^, Complex, Array)
   Headline
     the canonical inclusion or projection map of a direct sum
   Usage
     i = C_[name]
     p = C^[name]
   Inputs
     C:Complex
     name:
   Outputs
     :ComplexMap
       {\tt i} is the canonical inclusion and {\tt p} is
       the canonical projection
   Description
    Text
      The direct sum is an n-ary operator with projection and
      inclusion maps from each component satisfying appropriate
      identities.

      One can access these maps as follows.      
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C2 = complex (ideal(a,b,c))
      D = C1 ++ C2
      D_[0]
      D_[1]
      D^[0] * D_[0] == 1
      D^[1] * D_[1] == 1
      D^[0] * D_[1] == 0
      D^[1] * D_[0] == 0
      D_[0] * D^[0] + D_[1] * D^[1] == 1
    Text
      The default names for the components are the non-negative
      integers.  However, one can choose any name.
    Example
      E = (mike => C1) ++ (greg => C2)
      E_[mike]
      E_[greg]
      E^[mike] * E_[mike] == 1
      E^[greg] * E_[greg] == 1
      E^[mike] * E_[greg] == 0
      E^[greg] * E_[mike] == 0
      E_[mike] * E^[mike] + E_[greg] * E^[greg] == 1
    Text
      One can also access inclusion and projection maps of sub-direct sums.
    Example
      F = directSum(C1, C2, (complex S^13)[-4])
      F^[0,1]
      F_[0,2]
   SeeAlso
     (directSum, Complex)
     (components, Complex)
     indices
///

doc ///
   Key
     (components,Complex)
   Headline
     list the components of a direct sum
   Usage
     components C
   Inputs
     C:Complex
   Outputs
     :List
       the component complexes of a direct sum (of complexes)
   Description
    Text
      A complex which has been constructed as a direct sum
      stores its component complexes.
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C2 = complex (ideal(a,b,c))
      D = C1 ++ C2
      L = components D
      L_0 === C1
      L_1 === C2
      E = (mike => C1) ++ (greg => C2)
      components E
    Text
      The names of the component complexes are called indices, 
      and are used to access the relevant inclusion and projection maps.
    Example
      indices D
      D^[0]
      indices E
      E_[greg]
   SeeAlso
     (directSum, Complex)
     indices
     (symbol_, Complex, Array)
     (symbol^, Complex, Array)
///

doc ///
   Key
     (length, Complex)
   Headline
     length of a complex
   Usage
     length C
   Inputs
     C:Complex
   Outputs
     :ZZ
   Description
    Text
      The length of a complex is the difference between the highest index
      of a non-zero object and the lowest index of a non-zero object.
      
      Typically, it counts the number of non-zero differentials, e.g. in
      a free resolution.
    Example
      S = ZZ/101[a,b,c,d];
      C1 = freeResolution coker vars S
      length C1
      C2 = C1[5]
      length C2
      C3 = C1 ++ C1[6]
      length C3
    Text      
      This function always prunes the input complex, so might involve 
      computation.
    Example
      M1 = S^1/(a*b, c*d, a*c, b*c)
      M2 = S^1/(a*b, c*d, a*c)
      C4 = freeResolution M1
      C5 = freeResolution M2
      f = map(M1, M2, 1)
      C6 = coker extend(C4, C5, matrix f)
      concentration C6
      length C6
      prune C6
      concentration prune C6
   SeeAlso
     (prune,Complex)
     (concentration,Complex)
///


doc ///
   Key
     (isHomogeneous, Complex)
   Headline
     whether a complex is homogeneous
   Usage
     isHomogeneous C
   Inputs
     C:Complex
   Outputs
     :Boolean
       that is true when {\tt C} is a homogeneous (i.e. graded) complex
   Description
    Text
      A complex is homogeneous (graded) if the base ring is graded,
      all of the component objects are graded, and
      all the component maps are graded of degree zero.
    Example
      S = ZZ/101[a,b,c,d];
      I = minors(2, matrix{{a,b,c},{b,c,d}})
      C = freeResolution (S^1/I)
      isHomogeneous C
      J = minors(2, matrix{{a,b,c},{b,c,d^2}})
      D = freeResolution (S^1/J)
      isHomogeneous D
   SeeAlso
     isHomogeneous
///

doc ///
   Key
     (symbol**, Complex, Complex)
     (symbol**, Complex, Module)
     (symbol**, Module, Complex)
     (tensor, Complex, Complex)
   Headline
     tensor product of complexes
   Usage
     D = C1 ** C2
   Inputs
     C1:Complex
       or @ofClass Module@
     C2:Complex
       or @ofClass Module@
   Outputs
     D:Complex
       tensor product of {\tt C1} and {\tt C2}
   Description
    Text
      The tensor product is a complex $D$ whose $i$th component is
      the direct sum of $C1_j \otimes C2_k$ over all $i = j+k$.
      The differential on $C1_j \otimes C2_k$ is the differential 
      $dd^{C1} \otimes id_{C2} + (-1)^j id_{C1} \otimes dd^{C2}$.
      
      As the next example illustrates, the Koszul complex can be constructed via iterated tensor products.
    Example
      S = ZZ/101[a..c]
      Ca = complex {matrix{{a}}}
      Cb = complex {matrix{{b}}}
      Cc = complex {matrix{{c}}}
      Cab = Cb ** Ca
      dd^Cab
      assert isWellDefined Cab
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      dd^Cabc
      assert isWellDefined Cabc
    Text
      If one of the arguments is a module, it is considered as a complex concentrated in homological degree 0.
    Example
      Cabc ** (S^1/(a,b,c))
      S^2 ** Cabc
    Text
      Because the tensor product can be regarded as the total complex of a double complex,
      each term of the tensor product comes with pairs of indices, labelling the summands.
    Example
      indices Cabc_1
      components Cabc_1
      Cabc_1_[{1,0}]
      indices Cabc_2
      components Cabc_2
      Cabc_2_[{0,2}]
   SeeAlso
     indices
     components
     directSum
///

doc ///
   Key
     (Hom, Complex, Complex)
     (Hom, Complex, Module)
     (Hom, Module, Complex)     
     (Hom, Complex, Ring)
     (Hom, Ring, Complex)     
   Headline
     the complex of homomorphisms between two complexes
   Usage
     D = Hom(C1,C2)
   Inputs
     C1:Complex
       or @ofClass Module@, or @ofClass Ring@
     C2:Complex
       or @ofClass Module@, or @ofClass Ring@
   Outputs
     D:Complex
       the complex of homomorphisms between {\tt C1} and {\tt C2}
   Description
    Text
      The complex of homomorphisms is a complex $D$ whose $i$th component is
      the direct sum of $Hom(C1_j, C2_{j+i})$ over all $j$.
      The differential on $Hom(C1_j, C2_{j+i})$ is the differential 
      $Hom(id_{C1}, dd^{C2}) + (-1)^j Hom(dd^{C1}, id_{C2})$.
      $dd^{C1} \otimes id_{C2} + (-1)^j id_{C1} \otimes dd^{C2}$.

      In particular, for this operation to be well-defined, both
      arguments must have the same underlying ring.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      D = Hom(C,C)
      dd^D
      assert isWellDefined D
    Text
      The homology of this complex is $Hom(C, ZZ/101)$
    Example
      prune HH D == Hom(C, coker vars S)
    Text
      If one of the arguments is a module or a ring, it is considered as a complex concentrated in homological degree 0.
    Example
      E = Hom(C, S^2)
      prune HH E
    Text
      There is a simple relationship between Hom complexes and @TO2 ((symbol SPACE, Complex, Array), "shifts")@.
      Specifically, shifting the first argument is the same as the negative shift of the result.  But
      shifting the second argument is only the same as the positive shift of the result
      up to a sign.
    Example
      Hom(C[3], C) == D[-3]
      Hom(C, C[-2]) == D[-2]
      Hom(C, C[-3]) != D[-3]
      Hom(C, C[-3]) == complex(- dd^(D[-3]))
    Text
      Specific maps and morphisms between complexes can be obtained
      with @TO (homomorphism, ComplexMap)@.
    Text
      Because the Hom complex can be regarded as the total complex of a double complex,
      each term comes with pairs of indices, labelling the summands.
    Example
      indices D_-1
      components D_-1
      indices D_-2
      components D_-2
   SeeAlso
     (homomorphism, ComplexMap)
     (homomorphism', ComplexMap)
     (randomComplexMap, Complex, Complex)
     indices
     components
     (Hom, ComplexMap, ComplexMap)
///

doc ///
   Key
     (betti,Complex)
   Headline
     display of degrees in a complex
   Usage
     betti C
   Inputs
     C:Complex
     Weights => List
	   a list of integers whose dot product with the multidegree of a basis
	   element is enumerated in the display returned.  The default is the
	   heft vector of the ring.  See @TO "heft vectors"@.
   Outputs
     :BettiTally
       a diagram showing the degrees of the generators of the components in {\tt C}
   Description
    Text
      Column $j$ of the top row of the diagram gives the rank of the
      $j$-th component $C_j$ of the complex $C$.  The entry in column $j$ in the row labelled
      $i$ is the number of basis elements of (weighted) degree $i+j$ in $C_j$.
      When the complex is the free resolution of a module the entries are
	  the total and the graded Betti numbers of the module.
      
      As a first example, we consider the ideal 
      in 18 variables which cuts out the variety
      of commuting 3 by 3 matrices.
    Example
      S = ZZ/101[vars(0..17)]
      m1 = genericMatrix(S,a,3,3)
      m2 = genericMatrix(S,j,3,3)
      J = ideal(m1*m2-m2*m1)
      C0 = freeResolution J
      betti C0
    Text
      From the display, we see that $J$ has 8 minimal generators, all
      in degree 2, and that there are 2 linear syzygies on these
      generators, and 31 quadratic syzygies.  
      Since this complex is the free resolution of $S/J$, 
      the projective dimension
      is 6, the index of the last column, and the regularity of $S/J$ is 4, 
      the index of the last row in the diagram.
    Example
      length C0
      pdim betti C0
      regularity betti C0
    Text
      The betti display still makes sense if the complex is not a free resolution.
    Example
      betti dual C0
      C1 = Hom(C0, image matrix{{a,b}});
      betti C1
      C1_-6
    Text
      This module has 10 generators, 2 in degree $-9=(-6)+(-3)$, and 8 in degree $-8=(-6)+(-2)$.
    Text
      In the multi-graded case, the heft vector is used, by default, as the weight vector for weighting the
	  components of the degree vectors of basis elements.
      
      The following example is a nonstandard $\mathbb{Z}$-graded polynomial ring.
    Example
      R = ZZ/101[a,b,c,Degrees=>{-1,-2,-3}];
      heft R
      C2 = freeResolution coker vars R
      betti C2
      betti(C2, Weights => {1})
    Text
      The following example is the Cox ring of the second Hirzebruch surface, and the complex
      is the free resolution of the irrelevant ideal.
    Example
      T = QQ[a,b,c,d,Degrees=>{{1,0},{-2,1},{1,0},{0,1}}];
      B = intersect(ideal(a,c),ideal(b,d))
      C3 = freeResolution B
      dd^C3
      heft T
      betti C3
      betti(C3, Weights => {1,0})
      betti(C3, Weights => {0,1})
      degrees C3_1
   SeeAlso
     betti
     BettiTally
///

-- TODO: once we have Hom evaluation map,
-- let's add in the map from C to dual dual C.
doc ///
   Key
     (dual, Complex)
   Headline
     make the dual of a complex
   Usage
     dual C
   Inputs
     C:Complex
   Outputs
     :Complex
   Description
    Text
      The dual of a complex $C$ is by definition $Hom(C, R)$, where $R$ is the ring of $C$.
    Example
      S = ZZ/101[a..d];
      B = intersect(ideal(a,c),ideal(b,d))
      C1 = freeResolution B
      C2 = dual C1
      assert(C2 == Hom(C1, S^1))
      C1 == dual dual C1
      prune HH C2
    Text
      The double dual is not necessarily isomorphic to the original complex.
    Example
      I = ideal(a^2, a*b, b^2)
      J = ideal(b^3, b*c, c^3)
      K = intersect(I,J)
      f = map(S^1/I ++ S^1/J, S^1/K, {{1},{1}})
      g = map(S^1/(I+J), S^1/I ++ S^1/J, {{1,-1}})
      C = complex{g,f}
      assert isWellDefined C
      assert isExact C
      assert(dual C == 0)
      assert(C != dual dual C)
   SeeAlso
     (Hom, Complex, Complex)
     (dual, Module)
///

doc ///
    Key
        (part, List, Complex)
        (part, ZZ, Complex)
    Headline
        extract a graded component of a complex
    Usage
        part(d, C)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        C:Complex
            that is homogeneous over $R$
    Outputs
        :Complex
          a complex over the coefficient ring of $R$
    Description
        Text
          If $C$ is a graded (homogeneous) complex over a ring $R$, and $d$ is a degree, this method
          computes the degree $d$ part of the complex over the coefficient ring of $R$.
        Text
          Taking parts of a graded (homogeneous) complex commutes with taking homology.
        Example
          R = QQ[a,b,c,d];
          I = ideal(a*b, a*c, b*c, a*d)
          C = freeResolution I
          D = part(4,C)
          prune HH D == part(4, HH C)
          prune HH D == part(4, complex(R^1/I))
        Text
          Given a squarefree monomial ideal corresponding to a
          simplicial complex, in a polynomial ring equipped with the
          fine grading, parts of the dual of the free resolution of the monomial
          ideal are the chain complexes of the induced simplicial
          subcomplexes.
        Example
          S = QQ[a..d, DegreeRank=>4];
          I = intersect(ideal(a,b), ideal(c,d))
          C = dual freeResolution I
          prune HH (part({-1,-1,-1,-1}, C)) -- empty quadrilateral
          prune HH part({-1,-1,0,0}, C) -- 2 points
          prune HH part({0,0,-1,-1}, C) -- 2 points
          prune HH part({0,0,0,0}, C) -- solid quadrilateral
    SeeAlso
        "Making chain complexes"
        (part, List, ComplexMap)
        (truncate, List, Complex)
        (truncate, List, ComplexMap)
        (canonicalTruncation, Complex, Sequence)
        (naiveTruncation, Complex, ZZ, ZZ)
///

-- TODO Obtain the implicit equation of a surface by using this method
--      on a resolution over Rees algebra Mike and/or Greg thinks
--      about an example here.

doc ///
    Key
        (part, List, ComplexMap)
        (part, ZZ, ComplexMap)
    Headline
        extract a graded component of a map of complexes
    Usage
        part(d, f)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        f:ComplexMap
            that is homogeneous over $R$
    Outputs
        :ComplexMap
          a complex map over the coefficient ring of $R$
    Description
        Text
          If $f$ is a graded (homogeneous) map of complexes over a ring $R$, and $d$ is a degree, this method
          computes the degree $d$ part of the complex map over the coefficient ring of $R$.
        Text
          Taking parts of a graded (homogeneous) complex commutes with taking homology.
        Example
          kk = ZZ/7
          R = kk[a,b,c,d];
          I = ideal(a*b, a*c, b*c, a*d)
          J = I + ideal(b^3)
          C = freeResolution I
          D = freeResolution ((R^1/J) ** R^{{1}})
          f = randomComplexMap(D,C, Cycle=>true)
          g = part(2,f)
          assert(part(2, HH f) ==  prune HH part(2, f))
    SeeAlso
        "Making maps between chain complexes"
        (part, List, Complex)
        (truncate, List, Complex)
        (truncate, List, ComplexMap)
        (naiveTruncation, Complex, Sequence)
        (canonicalTruncation, Complex, ZZ, ZZ)
///

-- truncate start
doc ///
    Key
        (truncate, List, Complex)
        (truncate, ZZ, Complex)
    Headline
        truncation of a complex at a specified degree or set of degrees
    Usage
        truncate(d, C)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        C:Complex
            that is homogeneous over $R$
    Outputs
        :Complex
          a complex whose terms consist of all elements of component-wise degree at least {\tt d}.
    Description
        Text
            Truncation of homogeneous (graded) modules induces a natural
            operation on chain complexes.
        Text
            In the singly graded case, the truncation of a homogeneous
            module $M$ at degree $d$ is generated by all homogeneous
            elements of degree at least $d$ in $M$.  This method applies
            this operation to each term in a chain complex.  
        Example
            R = QQ[a,b,c];
            I = ideal(a*b, a*c, b*c)
            C = freeResolution I
            D = truncate(3,C)
            assert isWellDefined D
            prune HH D
        Text
            Truncating at a degree less than the minimal generators
            is the identity operation.
        Example
            assert(C == truncate(0, C))
        Text
            In the multi-graded case, the truncation of a homogeneous module at 
            a list of degrees is generated by all homogeneous elements of degree
            that are component-wise greater than or equal to at least one
            of the degrees.
        Example
            A = ZZ/101[x_0, x_1, y_0, y_1, y_2, Degrees => {2:{1,0}, 3:{0,1}}];
            I = intersect(ideal(x_0, x_1), ideal(y_0, y_1, y_2))
            C = freeResolution I
            D1 = prune truncate({{1,1}}, C)
            D2 = truncate({{1,0}}, C)
            D3 = truncate({{0,1}}, C)
            D4 = truncate({{1,0},{0,1}}, C)
            D5 = truncate({{2,2}}, C)
            assert all({D1,D2,D3,D4,D5}, isWellDefined)
    SeeAlso
        "Making chain complexes"
        (truncate, List, Module)
        (truncate, List, ComplexMap)
        (canonicalTruncation, Complex, Sequence)
        (naiveTruncation, Complex, ZZ, ZZ)
        (part, List, Complex)
///

-- TODO Obtain the implicit equation of a surface by using this method
--      on a resolution over Rees algebra Mike and/or Greg thinks
--      about an example here.

-- TODO the following appears to be a bug in Truncations.m2
-*   R = QQ[a,b,c];
     I = ideal(a*b, a*c, b*c)
     C = freeResolution I
     D = truncate(3,C)
     assert isWellDefined D
     prune HH D
     truncate(3, HH C) -- BUG?
*-

doc ///
    Key
        (naiveTruncation, Complex, ZZ, ZZ)
        (naiveTruncation, Complex, InfiniteNumber, ZZ)
        (naiveTruncation, Complex, Nothing, ZZ)
        (naiveTruncation, Complex, Sequence)
        (naiveTruncation, Complex, ZZ, InfiniteNumber)
        (naiveTruncation, Complex, ZZ, Nothing)
        (naiveTruncation, Complex, InfiniteNumber, InfiniteNumber)
        naiveTruncation
    Headline
        drops all terms of a complex outside a given interval
    Usage
        naiveTruncation(C, lo, hi)
    Inputs
        C:Complex
        lo:ZZ
            or {\tt -infinity} or {\tt null} (the latter two give no lower bound)
        hi:ZZ
            or {\tt infinity} or {\tt null} (the latter two give no upper bound)
    Outputs
        :Complex
    Description
        Text
            Returns a new complex which drops (sets to zero) all modules 
            outside the given range.
        Example
            R = ZZ/101[a,b,c,d,e];
            I = intersect(ideal(a,b),ideal(c,d,e))
            C = freeResolution I
            naiveTruncation(C, 1, 2)
            C16 = naiveTruncation(C, 1, 6)
            isWellDefined C16
            naiveTruncation(C, 1, infinity)
            naiveTruncation(C, -13, 2)
            naiveTruncation(C, -infinity, 2)
            naiveTruncation(C, , 2)
        Text
            Truncation gives rise to a natural short exact sequence of complexes.
        Example
            C' = naiveTruncation(C,, 1)
            C'' = naiveTruncation(C, 2, infinity)
            f = inducedMap(C, C')
            g = inducedMap(C'', C)
            assert isShortExactSequence(g,f)
        Text
            There is another type of truncation, @TO "canonicalTruncation"@, which induces
            an isomorphism on (a range) of homology.
    SeeAlso
        "Making chain complexes"
        (naiveTruncation, ComplexMap, Sequence)
        (canonicalTruncation, Complex, ZZ, ZZ)
        (canonicalTruncation, ComplexMap, ZZ, ZZ)
        (truncate, List, Complex)
///

doc ///
    Key
        (canonicalTruncation, Complex, ZZ, ZZ)
        (canonicalTruncation, Complex, InfiniteNumber, ZZ)
        (canonicalTruncation, Complex, Nothing, ZZ)
        (canonicalTruncation, Complex, Sequence)
        (canonicalTruncation, Complex, ZZ, InfiniteNumber)
        (canonicalTruncation, Complex, ZZ, Nothing)
        (canonicalTruncation, Complex, InfiniteNumber, InfiniteNumber)
        canonicalTruncation
    Headline
        reducing the number of non-zero terms of a complex
    Usage
        canonicalTruncation(C, lo, hi)
    Inputs
        C:Complex
        lo:ZZ
            or {\tt -infinity} or {\tt null} (the latter two give no lower bound)
        hi:ZZ
            or {\tt infinity} or {\tt null} (the latter two give no upper bound)
    Outputs
        :Complex
    Description
        Text
            Returns a new complex which drops (sets to zero) all modules 
            outside the given range, and modifies the ends to preserve homology
            in the given range.
        Example
            R = ZZ/101[a,b,c,d,e];
            I = intersect(ideal(a,b),ideal(c,d,e))
            C = (dual freeResolution I)[-4]
            C1 = canonicalTruncation(C, 1, 2)
            assert isWellDefined C1
            HH C1
            naiveTruncation(HH C, 1, 2) == HH C1
            prune HH C1
        Text
            We illustrate various possibilities for the truncation interval.
        Example
            C2 = canonicalTruncation(C, 1, 6)
            assert isWellDefined C2
            C3 = canonicalTruncation(C, 1, infinity)
            C2 == C3
            C4 = canonicalTruncation(C, -13, 2)
            C5 = canonicalTruncation(C, -infinity, 2)
            C4 == C5
            C6 = canonicalTruncation(C, , 2)
            C4 == C6
        Text
            If the lower and upper bounds are equal in the canonical truncation, 
            the resulting complex has a single nonzero term consisting of the
            homology in that location.
        Example
            assert(canonicalTruncation(C, 1, 1) == naiveTruncation(HH C, 1, 1))
        Text
            If we truncate only from below, then we get an injection
            from the truncation into the original complex, whereas if
            we truncate only from above, we get a surjection onto
            the truncated complex.
        Example
            f = inducedMap(C, C3)
            assert isWellDefined f
            assert(ker f == 0)
            prune coker f
            C7 = canonicalTruncation(C, -infinity, 1)
            C7 != coker f
        Example
            g = inducedMap(C5, C)
            assert isWellDefined g
            assert(coker g == 0)
            C8 = canonicalTruncation(C, 2, infinity)
            prune C8
            prune ker g
        Text
            There is another type of truncation, @TO "naiveTruncation"@, which yields
            a short exact sequence of complexes.
    SeeAlso
        "Making chain complexes"
        (canonicalTruncation, ComplexMap, Sequence)
        (naiveTruncation, Complex, ZZ, ZZ)
        (naiveTruncation, ComplexMap, ZZ, ZZ)
        (truncate, List, Complex)
///

doc ///
    Key
        (symbol SPACE, RingMap, Complex)
    Headline
        apply a ring map
    Usage
        phi C
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        C:Complex
            over the ring $R$
    Outputs
        :Complex
            over the ring $S$
    Description
        Text
            We illustrate the image of a complex under a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = freeResolution I
            D = phi C
            isWellDefined D
            dd^D
            prune HH D
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the complex.
        Example
            R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            phi = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = freeResolution coker vars R
            D = phi C
            assert isWellDefined D
            assert isHomogeneous D
            prune HH D
    Caveat
        Every term in the complex must be free or a submodule of a free module.
        Otherwise, use @TO (tensor, RingMap, Complex)@.
    SeeAlso
        (symbol SPACE, RingMap, ComplexMap)
        (symbol **, RingMap, Complex)
///

doc ///
    Key
        (symbol**, RingMap, Complex)
        (symbol**, Complex, RingMap)
        (tensor, RingMap, Complex)
        (tensor, Complex, RingMap)
        (symbol**, Complex, Ring)
        (symbol**, Ring, Complex)
    Headline
        tensor a complex along a ring map
    Usage
        phi ** C
        tensor(phi, C)
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        C:Complex
            over the ring $R$
    Outputs
        :Complex
            over the ring $S$
    Description
        Text
            We illustrate the tensor product of a complex along a ring map.
        Example
            R = QQ[x,y,z];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = freeResolution I
            D = phi ** C
            assert isWellDefined D
            dd^D
            prune HH D
        Text
            If a ring is used rather than a ring map, then the implicit
            map from the underlying ring of the complex to the given ring
            is used.
        Example
            A = R/(x^2+y^2+z^2);
            C ** A
            assert(map(A,R) ** C == C ** A)
        Text
            The commutativity of tensor product is witnessed as follows.
        Example
            assert(D == C ** phi)
            assert(C ** A == A ** C)
        Text
            When the modules in the complex are not free modules,
            this is different than the image of a complex 
            under a ring map.
        Example
            use R
            I = ideal(x*y, x*z, y*z);
            J = I + ideal(x^2, y^2);
            g = inducedMap(module J, module I)
            assert isWellDefined g
            C = complex {g}
            D1 = phi C
            assert isWellDefined D1
            D2 = phi ** C
            assert isWellDefined D2
            prune D1
            prune D2
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the complex.
        Example
            R = ZZ/101[a..d];
            S = ZZ/101[s,t];
            f = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = freeResolution coker vars R
            D = f ** C
            D == f C
            assert isWellDefined D
            assert isHomogeneous D
            prune HH D
            C1 = Hom(C, image vars R)
            D1 = f ** C1
            isWellDefined D1
            assert isHomogeneous D1
    SeeAlso
        (symbol **, RingMap, ComplexMap)
        (symbol SPACE, RingMap, Complex)
///

doc ///
    Key
        (resolution, Complex)
    Headline
        minimal free resolution of a complex
    Usage
        resolution C
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
        :Complex
    Description
        Text
            Given a complex $C$, this method produces a quasi-isomorphic complex $F$ 
            all of whose terms are free modules.  The algorithm used minimizes the
            ranks of the free modules in $F$.
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
            This method just accesses the cached value from
            the method @TO (resolutionMap, Complex)@, which 
            does the actual computation.
    SeeAlso
        (resolutionMap, Complex)
        (freeResolution, Module)
        (isQuasiIsomorphism, ComplexMap)
///

doc ///
    Key
        (minimalPresentation, Complex)
        (prune, Complex)
    Headline
        minimal presentation of all terms in a complex
    Usage
        D = minimalPresentation C
        D = prune C
    Inputs
        C:Complex
        Exclude => 
            unused
    Outputs
        D:Complex
            isomorphic to the input, where each term is replaced
            by a minimally presented model
    Consequences
        Item
            The isomorphism $g : D \to C$ is available as 
            @TT "g = D.cache.pruningMap"@.  The inverse isomorphism
            can be obtained as @TT "g^-1"@
    Description
        Text
            This is frequently useful to make the output of certain
            operations readable or understandable.
        Text
            In particular, homology often needs to be pruned to
            be understood.  For instance, this is useful 
            for recognizing when terms given by subquotient modules 
            are actually zero.
        Example
            S = ZZ/101[a,b,c,d,e];
            I = ideal(a,b) * ideal(c,d,e)
            F = dual freeResolution I
            C = HH F
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isComplexMorphism g
            assert (target g == C)
            assert (source g == D)
            h = g^-1
            assert(g*h == 1 and h*g == 1)
        Text
            The image of a map of complexes also becomes more
            understandable via pruning.
        Example
            S = ZZ/101[a,b,c];
            I = ideal(a^2,b^2,c^2);
            J = I + ideal(a*b*c);
            FI = freeResolution I
            FJ = freeResolution J
            f = randomComplexMap(FJ, FI ** S^{-1}, Cycle => true)
            C = image f
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isComplexMorphism g
            assert (target g == C)
            assert (source g == D)
            h = g^-1
            assert(g*h == 1 and h*g == 1)
   SeeAlso
       (minimize, Complex)
       (minimalPresentation, Module)
       randomComplexMap
       freeResolution
       isComplexMorphism
///

doc ///
    Key
        (minimize, Complex)
        minimize
    Headline
        a quasi-isomorphic complex whose terms have minimal rank
    Usage
        D = minimize C
    Inputs
        C:Complex
            graded, whose terms are all free modules
    Outputs
        D:Complex
            graded, whose terms are all free modules of minimal rank
    Consequences
        Item
            The projection morphism $g : C \to D$ is available as 
            @TT "g = D.cache.minimizingMap"@.  
    Description
        Text
            This method essentially removes all scalar units 
            from the matrices in the differential of $C$.
            
            We illustrate this in a simple example.
        Example
            S = ZZ/32003[a,b];
            I = ideal(a^2-b^2, a*b)
            C = freeResolution(I, FastNonminimal=>true)
            betti C
            D = minimize C
            assert(isWellDefined D and isHomogeneous D)
            betti D
            g = D.cache.minimizingMap
            assert isWellDefined g
            assert(isComplexMorphism g and isQuasiIsomorphism g)
            assert(source g == C)
            assert(target g == D)
            assert(coker g == 0)
        Text
            The minimal complex $D$ is a direct summand of the
            original complex $C$.  The natural inclusion
            of $D$ into $C$ can be constructed as follows.
        Example
            f = liftMapAlongQuasiIsomorphism(id_D, g)
            g*f == id_D
            assert(source f == D)
            assert(target f == C)
            assert(ker f == 0)
            f*g
        Text
            The chain complex $D$ is a direct summand of $C$,
            giving rise to a split short exact sequence of
            chain complexes.
        Example
            h = prune canonicalMap(C, ker g)
            assert isShortExactSequence(g, h)
        Text
            Warning: If the input complex is not homogeneous, then
            the output is probably not what one would expect.
        Example
            S = ZZ/32003[a..d]
            J = ideal(a*b*c-b*c, a*d-c, a^3-d^2*c)
            CJ = freeResolution J
            assert not isHomogeneous CJ
            D = minimize CJ
            isWellDefined D
            prune HH D == prune HH CJ
   SeeAlso
       freeResolution
       (resolution, Complex)
       (resolutionMap, Complex)
       (minimalPresentation, Complex)
///

/// -- comment about the above node
-- The following fails due to 'prune' (May 2020, see git issue #1116)
            SZZ = ZZ (monoid S);
            CZZ = complex hashTable for i from min C + 1 to max C list i => sub(dd^C_i, SZZ)
            isWellDefined CZZ
            betti CZZ
            DZZ = minimize CZZ
            assert isWellDefined DZZ
            assert isHomogeneous DZZ -- !! BUG
            betti DZZ
            g = D.cache.minimizingMap
            assert isWellDefined g
            assert isComplexMorphism g
            assert isQuasiIsomorphism g
            assert(source g == C)
            assert(target g == D)
            assert(coker g == 0)
///

/// -- comment about minimize and pruneComplex:
  -- this code can be run for the example ini (minimize,Complex).
  needsPackage "PruneComplex"
  C' = chainComplex C
  D' = pruneComplex(C', UnitTest => isScalar)
  g' = D'.cache.pruningMap
  D = complex D'
  g = complex g'
  source g == D
  target g == C
  isComplexMorphism g
///

doc ///
   Key
     isExact
     (isExact, Complex)
     (isExact, Complex, InfiniteNumber, InfiniteNumber)
     (isExact, Complex, InfiniteNumber, Number)
     (isExact, Complex, Number, InfiniteNumber)
     (isExact, Complex, Number, Number)
   Headline
     whether a complex is exact
   Usage
     isExact C
     isExact(C, lo, hi)
   Inputs
     C:Complex
     lo:Number
       or -infinity
     hi:Number
       or infinity
   Outputs
     :Boolean
       that is true when {\tt C} is exact
   Description
    Text
      The complex $C$ is exact if and only if the homology group
      $H^i(C)$ is the zero module, for all $i$.  If bounds are given,
      then true is returned if $H^i(C) = 0$ for all $lo \le i \le
      hi$.
    Text
      A resolution $C$ is an exact complex except in homological degree 0. 
      The augmented complex $C'$ is exact everywhere.
    Example
      S = ZZ/101[a..d];
      I = monomialCurveIdeal(S, {1,3,4})
      C = freeResolution I
      prune HH C
      assert not isExact C
      assert isExact(C, 1, infinity)
      C' = cone inducedMap(complex(S^1/I), C)[1]
      prune HH C'
      assert isExact C'
   SeeAlso
     (homology, Complex)
     cone
     freeResolution
     prune
///
 
doc ///
  Key
    (isFree, Complex)
    isFree
  Headline
    whether a complex consists of free modules
  Usage
    isFree C
  Inputs
    C:Complex
  Outputs
    :Boolean
      that is true when each $C_i$ is a free module
  Description
    Text
      This method checks whether the given representation of each
      module $C_i$ is free. To determine whether the complex $C$ is
      isomorphic to a free complex, use @TO2("(prune,Complex)", "prune")@.
    Text
      The following example demonstrates that the presentation of a module
      might not reveal the property of being free.
    Example
      S = ZZ/101[a,b];
      M = kernel vars S
      assert not isFreeModule M
      assert isFreeModule prune M
    Text
      By definition, a free resolution $C$ consists of free modules.
      In contrast, the augmented complex $C'$ might or might not
      consist of free modules.
    Example
      C = freeResolution M
      assert isFree C
      C' = cone map(complex M, C, i -> map(M, C_0, 1))[1]
      isWellDefined C'
      assert not isFree C'
      prune C'
      assert isFree prune C'
  SeeAlso
    isFreeModule
    freeResolution
    (prune, Complex)
///

doc ///
    Key
        (yonedaExtension, Matrix)
        yonedaExtension
    Headline
        creates a chain complex representing an extension of modules
    Usage
        C = yonedaExtension f
    Inputs
        f:Matrix
            over a ring $R$, from $R^1$ to $\operatorname{Ext}^d(M,N)$,
            which represents an element in the Ext module
    Outputs
        C:Complex
            which represents the extension corresponding to 
            the element in the Ext module
    Description
        Text
            The module $\operatorname{Ext}^d(M,N)$ corresponds to equivalence classes
            of extensions of $N$ by $M$.  In particular, an element 
            of this module is represented by an exact sequence of the form
            \[
              0 \leftarrow M \leftarrow F_0 \leftarrow F_1 \leftarrow \dots
              \leftarrow F_{d-2} \leftarrow P \leftarrow N \leftarrow 0
            \]
            where $F_0 \leftarrow F_1 \leftarrow \dots$
            is a free resolution of $M$, and $P$ is the pushout of the maps
            $g : F_d \rightarrow N$ and $F_d \rightarrow F_{d-1}$.
            The element corresponding to $f$ in $\operatorname{Ext}^d(M,N)$ lifts to 
            the map $g$.
        Text
            In our first example, the module 
            $\operatorname{Ext}^1(M,R^1)$
            has one generator, in degree 0.
            The middle term in the corresponding short exact sequence
            determines an irreducible rank 2 vector bundle 
            on the elliptic curve, which can be verified by computing
            Fitting ideals.
        Example
            R = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z));
            M = truncate(1,R^1)
            f = basis(0, Ext^1(M, R^1))
            C = yonedaExtension f
            assert isWellDefined C
            assert isShortExactSequence(dd^C_1, dd^C_2)
            E = C_1
            fittingIdeal(1, E)
            saturate fittingIdeal(2, E)
        Text
            For higher Ext modules, we get longer exact sequences.
            When the map $f$ has degree 0, the corresponding exact sequence
            is homogeneous.
        Example
            x = symbol x;
            S = ZZ/101[x_0..x_5];
            I = borel monomialIdeal(x_2*x_3)
            E = Ext^4(S^1/I, S^{-5})
            f = E_{0}
            assert(isHomogeneous f and degree f === {0})
            C = yonedaExtension f
            assert isWellDefined C
            assert isHomogeneous C
            assert(HH C == 0)
        Text
            The inverse operation is given by @TO yonedaExtension'@.
        Example
            f' = yonedaExtension' C
            assert(f' == f)
    SeeAlso
        "Working with Ext"
        (yonedaMap, Matrix)
        (yonedaMap', ComplexMap)
        (yonedaExtension', Complex)
        (yonedaProduct, Module, Module)
        (homomorphism, ComplexMap)
        (homomorphism', ComplexMap)
        fittingIdeal
///

doc ///
    Key
        (yonedaExtension', Complex)
        yonedaExtension'
    Headline
        identifies the element of Ext corresponding to an extension
    Usage
        f = yonedaExtension' C
    Inputs
        C:Complex
          exact, of length $d$ over a ring $R$
    Outputs
        f:Matrix
          a map from $R^1$ to $\operatorname{Ext}^d(C_0, C_d)$
    Description
        Text
            The module $\operatorname{Ext}^d(M,N)$ corresponds to equivalence classes
            of extensions of $N$ by $M$.  In particular, an element 
            of this module is represented by an exact sequence of the form
            \[
              0 \leftarrow M \leftarrow C_1 \leftarrow C_2 \leftarrow \dots
              \leftarrow C_{d-1} \leftarrow N \leftarrow 0
            \]
            In particular, we have $M = C_0$ and $N = C_d$.
            For any such exact sequence, this method returns the map $f
            \colon R^1 \to \operatorname{Ext}^d(M,N)$
            corresponding to the element in the Ext module.
        Text
            In our first example, the module 
            $\operatorname{Ext}^1(M,R^1)$
            has one generator, in degree 0.
            The middle term in the corresponding short exact sequence
            determines an irreducible rank 2 vector bundle 
            on the elliptic curve.
        Example
            R = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z));
            M = truncate(1,R^1)
            N = R^1;
            E = coker map(R^{3:-1} ++ R^1,,{
                    {y, x, 0, 0}, 
                    {-z, 0, x, -y*z}, 
                    {0, -z, -y, x^2-3*x*z+2*z^2}, 
                    {x^2-3*x*z+2*z^2, y*z, 0, 0}
                    })
            d1 = map(M, E, (cover E)^[0])
            d2 = map(E, N, (cover E)_[1])
            C = complex{d1,d2}
            assert isWellDefined C
            assert isHomogeneous C
            assert(HH C == 0)
            f = yonedaExtension' C
        Text
            Although the complex representing $f$ is only defined up
            to equivalence of extensions, this method returns the same 
            complex in this example.
        Example
            assert(C == yonedaExtension f)
            assert(basis(0, Ext^1(M,N)) == f)
        Text
            The trivial extension corresponds to the zero element
            in the Ext module.
        Example
            R = ZZ/101[a,b,c,d,e];
            M = coker matrix"a,b,d,e"
            N = coker matrix"c,d,e"
            Ext^3(M,N)
            C = complex{id_M, map(M, R^0, 0), map(R^0, N, 0), id_N}
            assert isWellDefined C
            assert(HH C == 0)
            f = yonedaExtension' C
            assert(target f === Ext^3(M,N))
            assert(f == 0)
            D = yonedaExtension f
            assert(C != D)
    SeeAlso
        "Working with Ext"
        (yonedaMap, Matrix)
        (yonedaMap', ComplexMap)
        (yonedaExtension, Matrix)
        (yonedaProduct, Module, Module)
///

-- TODO: this doc node needs more text, another example.
doc ///
    Key
        (yonedaMap, Matrix)
        yonedaMap
    Headline
        creates a chain complex map representing an extension of modules
    Usage
        g = yonedaMap f
    Inputs
        f:Matrix
            over a ring $R$, from $R^1$ to $\operatorname{Ext}^d_R(M,N)$,
            which represents an element in the Ext module
        LengthLimit => ZZ
            determines the maximum length of the free resolutions used
    Outputs
        g:ComplexMap
            of degree $-d$ from the free resolution of $M$ to the free
            resolution of $N$ corresponding to the given element in the
            Ext module
    Description
        Text
            The module $\operatorname{Ext}^d_R(M,N)$ is constructed from
            a free resolution $F$ of $M$,
            \[
              0 \leftarrow M \leftarrow F_0 \leftarrow F_1 \leftarrow \dots
              \leftarrow F_d \leftarrow \ldots,
            \]
            by taking the homology of the complex $\operatorname{Hom}_R(F, N)$.
            An element of $\operatorname{Ext}^d_R(M,N)$ is represented by
            an element of $\operatorname{Hom}_R(F_d, N)$.  This map extends to a map
            of degree $-d$ from $F$ to the free resolution of $N$.
        Text
            We illustrate this method by choosing a random element
            in an Ext module.  This particular Ext module may be regarded
            as a possible obstruction space for deformations of the ideal $I$.
        Example
            S = ZZ/101[a..d]
            I = ideal"a2,ab,ac,b3"
            E = Ext^1(I, S^1/I)
            B = basis(0, E)
            f = B * random(S^16, S^1)
            g = yonedaMap f
            assert isWellDefined g
            assert(degree g === -1)
            assert isCommutative g            
            assert isHomogeneous g
            source g -- free resolution of I
            target g -- free resolution of S/I
            assert(yonedaMap' g == f)
        Text
            If the free resolutions are not finite in length,
            one needs to choose a truncation via the optional
            argument {\tt LengthLimit}.
        Example
            R = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z));
            M = truncate(1,R^1)
            prune Ext^3(M, M)
            B = basis(-4, Ext^3(M, M))
            f = B_{2}
            g = yonedaMap(f, LengthLimit => 8)
            assert isHomogeneous g
            assert isWellDefined g
            assert isCommutative g
            assert(degree g === -3)
            assert(yonedaMap' g == map(target f, R^1, f, Degree => -4))
            assert(isHomogeneous yonedaMap' g)
    SeeAlso
        "Working with Ext"
        (yonedaMap', ComplexMap)
        (yonedaExtension, Matrix)
        (yonedaExtension', Complex)
        (yonedaProduct, Matrix, Matrix)
        (yonedaProduct, Module, Module)
///

///
    Key
        (yonedaMap', ComplexMap)
        yonedaMap'
    Headline
        identifies the element of Ext corresponding to a map of free resolutions
    Usage
        f = yonedaMap' g
    Inputs
        g:ComplexMap
            of degree $-d$ from the free resolution of $M$ to the free
            resolution of $N$
    Outputs
        f:Matrix
            over a ring $R$, from $R^1$ to $\operatorname{Ext}^d_R(M,N)$,
            which represents the corresponding element in the Ext module
    Description
        Text
            The module $\operatorname{Ext}^d_R(M,N)$ is constructed from
            a free resolution $F$ of $M$,
            \[
              0 \leftarrow M \leftarrow F_0 \leftarrow F_1 \leftarrow \dots
              \leftarrow F_d \leftarrow \ldots,
            \]
            by taking the homology of the complex $\operatorname{Hom}_R(F, N)$.
            An element of $\operatorname{Ext}^d_R(M,N)$ is represented by
            an element of $\operatorname{Hom}_R(F_d, N)$.  Given a map $g$ extending
            a map of degree $-d$ from $F$ to the free resolution of $N$,
            this method returns the corresponding element in the Ext module.
        Text
            We illustrate this method by choosing a random element
            in an Ext module, constructing the corresponding map  $g$ between free resolutions.
        Example
            S = ZZ/101[a..d]
            I = ideal"a2,ab,ac,b3"
            E = Ext^1(I, S^1/I)
            B = basis(0, E)
            f0 = B * random(S^16, S^1)
            g = yonedaMap f0
            assert(degree g === -1)
            f = yonedaMap' g
            assert isWellDefined f
            assert(degree f == {0})
            assert isHomogeneous f
            source f === S^1
            target f === E
            assert(f == f0)
        Text
            The method @TO yonedaMap'@ is only a one-sided inverse
            to @TO yonedaMap@.
        Example
            R = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z));
            M = truncate(1,R^1)
            B = basis(-4, Ext^3(M, M))
            f0 = B_{2}
            g = yonedaMap(f0, LengthLimit => 8)
            f = yonedaMap' g
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f === {-4})
            assert(f != f0)
            assert(yonedaMap(f, LengthLimit => 8) == g)
    SeeAlso
        "Working with Ext"
        (yonedaMap, Matrix)
        (yonedaExtension, Matrix)
        (yonedaExtension', Complex)
        (yonedaProduct, Matrix, Matrix)
        (yonedaProduct, Module, Module)
///
