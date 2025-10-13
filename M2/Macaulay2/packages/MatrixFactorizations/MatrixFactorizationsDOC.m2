doc ///
    Key
        MatrixFactorizations
    Headline
        A package for creating and computing objects in the category of ZZ/d-graded factorizations, such as matrix factorizations
    Description
        Text
            A ZZ/d-graded factorization F  of a ring element f is a ZZ/d-graded complex of free R-modules equipped with a degree -1 (mod d) endomorphism $\text{d}^F$ such that $(\text{d}^F)^d = f * id_F$. 
	    In practice, a @TO ZZdFactorization@ may be visualized as a sequence of R-module maps:
	    
	    $$F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_{d-1}$$
	    
	    with the caveat that $d^F_0 : F_0 \to F_{d-1}$, since one should count degree modulo d. Any 2-periodic complex may be reinterpretted as a ZZ/2-graded factorization of 0, and 
	    likewise a matrix factorization of a ring element f is equivalently a ZZ/2-graded factorization of f. Because of their similarity with complexes, much of the functionality
	    and syntax for this package closely resembles the @TO Complexes@ package, with some key differences.

	    Firstly, the output string of a @TO ZZdFactorization@ and @TO ZZdFactorizationMap@ is meant to illustrate the periodic nature of
	    a ZZ/d-graded factorization.
	Example
	    S = ZZ/101[a..c];
	    K = koszulMF({a,b,c}, a^2 + b^2 + c^2)
	    diffs = K.dd
	    K.dd^2
	Text
            A ZZ/d-graded factorization does not have a concentration, but instead has a @TO period@. The period is a very important
	    invariant, since almost all of the methods implemented depend on the period of the factorization, and will give legitimately
	    different outputs as the period varies.
	Example
	    period K
	    K' = linearMF(a^3 + b^3 + c^3, t) --we have to adjoin a root of unity t
	    period K'
	    diffs' = K'.dd
	    (diffs')^3
	Text
	    As seen in the above, many constructions of factorizations with periods $>2$ require the user
	    to adjoin or specify a primitive root of unity. For an in-depth discussion on all the methods implemented
	    for accomplishing this, see @TO adjoinRoot@.

	    Finally, every factorization (assuming it is well-defined) has an associated @TO potential@, which is
	    the polynomial $f$ such that all $d$-fold compositions of the differentials are equal to $f$ times the identity.
	    This potential can be accessed as follows:
	Example
	    potential K
	    isdFactorization K
	    potential K'
	    isdFactorization K'
	Text    
	    Much of the syntax and functionality of the types ZZdFactorization and ZZdFactorizationMap are based on current functionalities for the analogous objects in the Complexes package,
	    so there should be essentially no learning curve for users already familiar with working with chain complexes.
    SeeAlso
         "Making ZZdFactorizations"
         "Making maps between factorizations"
         "Basic invariants and properties"
	 "differential of a ZZ/d-graded factorization"
	 "Preprogrammed examples and operations"
///

doc ///
  Key
    (isFactorizationMorphism, ZZdFactorizationMap)
    isFactorizationMorphism
  Headline
    whether a ZZdFactorizationMap is a morphism of factorizations
  Usage
    isFactorizationMorphism f
  Inputs
    f:ZZdFactorizationMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the differentials and has degree $0$
  Description
    Text
      For a factorization map $f : C \to D$ of degree $d$, this method
      checks whether $d = 0$ and, for all $i$, we have
      $dd^D_{i+d} * f_i = (-1)^d * (f_{i-1} * dd^C_i)$.
    Text
      We first construct a random factorization morphism.
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
        "Making ZZdFactorizations"
    Headline
        Information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (ZZdfactorization, HashTable),
                TO (ZZdfactorization, List), 
                TO (isWellDefined, ZZdFactorization),
		TO (isdFactorization, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Important constructors for creating ZZ/d-graded factorizations"@
	Text
    	    @UL {
                TO (koszulMF, List, RingElement),
		TO (linearMF, RingElement),
		TO (randomLinearMF, ZZ, Ring),
		TO (tailMF, Module),
		TO (randomTailMF, RingElement),
		TO (gradedModule, ZZdFactorization),
                TO (homology, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "More advanced constructors"@
	Text
    	    @UL {
                TO (symbol++, ZZdFactorization, ZZdFactorization),
                TO (symbol**, ZZdFactorization, ZZdFactorization),
                TO (Hom, ZZdFactorization, ZZdFactorization),
                TO (dual, ZZdFactorization),
                TO (symbol SPACE, RingMap, ZZdFactorization),
                TO (symbol **, RingMap, ZZdFactorization),
                TO (minimalPresentation, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Extracting ZZ/d-graded factorization from ZZ/d-graded factorization maps"@
        Text
    	    @UL {
                TO (source, ZZdFactorizationMap),
                TO (target, ZZdFactorizationMap),
                TO (kernel, ZZdFactorizationMap),
                TO (cokernel, ZZdFactorizationMap),
                TO (image, ZZdFactorizationMap),
                TO (coimage, ZZdFactorizationMap),
                TO (cone, ZZdFactorizationMap),
            }@
    SeeAlso
        "Making maps between factorizations"
        "Basic invariants and properties"
	"Preprogrammed examples and operations"  
///

doc ///
    Key
        "Basic invariants and properties"
    Headline
        information about accessing basic features
    Description
    	Text
    	    @SUBSECTION "Predicates for factorizations and factorization maps"@
        Text
    	    @UL {
                TO (isWellDefined, ZZdFactorization),
                TO (isFree,ZZdFactorization),
                TO (isWellDefined, ZZdFactorizationMap),
                TO (isCommutative, ZZdFactorizationMap),
                TO (isQuasiIsomorphism, ZZdFactorizationMap),
                TO (isShortExactSequence, ZZdFactorizationMap, ZZdFactorizationMap),
                --TO (isNullHomotopic, ZZdFactorizationMap),
                --TO (isNullHomotopyOf, ZZdFactorizationMap, ZZdFactorizationMap)
            }@
    	Text
    	    @SUBSECTION "Other invariants for ZZdFactorizations"@
        Text
    	    @UL {
                TO (ring,ZZdFactorization),
                TO (period,ZZdFactorization),
                TO (components, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Other invariants for factorization maps"@
        Text
    	    @UL {
                TO (source, ZZdFactorizationMap),
                TO (target, ZZdFactorizationMap),
                TO (degree, ZZdFactorizationMap),
                TO (ring, ZZdFactorizationMap),
                TO (components,ZZdFactorizationMap),
            }@
    SeeAlso
        "Making ZZdFactorizations"
        "Making maps between factorizations"
        "Preprogrammed examples and operations"
///


doc ///
    Key
        "Preprogrammed examples and operations"
    Headline
        Information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (ZZdfactorization, HashTable),
                TO (ZZdfactorization, List), 
                TO (isWellDefined, ZZdFactorization),
		TO (isdFactorization, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Important constructors for creating ZZ/d-graded factorizations"@
	Text
    	    @UL {
                TO (koszulMF, List, RingElement),
		TO (linearMF, RingElement),
		TO (randomLinearMF, ZZ, Ring),
		TO (tailMF, Module),
		TO (randomTailMF, RingElement),
		TO (gradedModule, ZZdFactorization),
                TO (homology, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "More advanced constructors"@
	Text
    	    @UL {
                TO (symbol++, ZZdFactorization, ZZdFactorization),
                TO (symbol**, ZZdFactorization, ZZdFactorization),
                TO (Hom, ZZdFactorization, ZZdFactorization),
                TO (dual, ZZdFactorization),
                TO (symbol SPACE, RingMap, ZZdFactorization),
                TO (symbol **, RingMap, ZZdFactorization),
                TO (minimalPresentation, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Extracting ZZ/d-graded factorization from ZZ/d-graded factorization maps"@
        Text
    	    @UL {
                TO (source, ZZdFactorizationMap),
                TO (target, ZZdFactorizationMap),
                TO (kernel, ZZdFactorizationMap),
                TO (cokernel, ZZdFactorizationMap),
                TO (image, ZZdFactorizationMap),
                TO (coimage, ZZdFactorizationMap),
                TO (cone, ZZdFactorizationMap),
            }@
    SeeAlso
        "Making maps between factorizations"
        "Basic invariants and properties"
	"Preprogrammed examples and operations"  
///

doc ///
    Key
        "Making maps between factorizations"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (map, ZZdFactorization, ZZdFactorization, HashTable),
                TO (map, ZZdFactorization, ZZdFactorization, ZZ),
                TO (map, ZZdFactorization, ZZdFactorization, ZZdFactorizationMap),
                TO (id, ZZdFactorization),
                TO (symbol SPACE, ZZdFactorizationMap, Array),
                TO (isWellDefined, ZZdFactorizationMap)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new factorization maps"@
        Text 
            @UL {
                TO (homology, ZZdFactorizationMap),
                TO (symbol**, ZZdFactorization, Matrix),
                TO (nullHomotopy, ZZdFactorizationMap)
           }@
    	Text
    	    @SUBSECTION "Canonical maps between factorizations"@
        Text
            Some factorizations come with canonical maps.
            To access the factorization map, 
            one uses @TO (inducedMap, ZZdFactorization, ZZdFactorization)@.
	Text
    	    @UL {
                TO (kernel, ZZdFactorizationMap),
                TO (cokernel, ZZdFactorizationMap),
                TO (image, ZZdFactorizationMap),
                TO (coimage, ZZdFactorizationMap),
                TO (cone, ZZdFactorizationMap),
                TO (inducedMap, ZZdFactorization, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Random maps of ZZ/d-graded factorizations"@
        Text
            The method @TO (randomFactorizationMap, ZZdFactorization, ZZdFactorization)@
            allows one to construct random factorization maps,
            random morphisms between factorizations, and random
            null homotopies between factorizations.
	Text
    	    @UL {
                TO (isCommutative, ZZdFactorizationMap),
                TO (isFactorizationMorphism, ZZdFactorizationMap),
                TO (isNullHomotopic, ZZdFactorizationMap)
            }@
    	Text
    	    @SUBSECTION "Elementary operations on factorization maps"@
        Text
    	    @UL {
                TO "arithmetic with ZZ/d-graded factorization maps",
                TO (symbol +, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol |, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol ||, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol ++, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol **, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (Hom, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (dual, ZZdFactorizationMap),
                TO (symbol _, ZZdFactorizationMap, Array),
                TO (symbol ^, ZZdFactorizationMap, Array),
                TO (symbol SPACE, RingMap, ZZdFactorizationMap),
                TO (symbol **, RingMap, ZZdFactorizationMap)
            }@
    SeeAlso
        "Making ZZdFactorizations"
        "Basic invariants and properties"
        "Preprogrammed examples and operations"
///


doc ///
    Key
        ZZdFactorization
    Headline
        the class of all ZZ/d-graded factorizations
    Description
        Text
            A ZZ/d-graded factorization is a sequence of objects $C_i$, connected by
            maps $dd^C_i : C_i \rightarrow C_{i-1}$ such that the
            composition of any d consecutive maps is equal to a fixed scalar multiple of the identity.

            In practice, a ZZdFactorization may be visualized as a sequence of R-module maps:
	    
	    $$F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_{d-1}$$
	    
	    with the caveat that $d^F_0 : F_0 \to F_{d-1}$, since one should count degree modulo d. Any 2-periodic complex may be reinterpretted as a ZZ/2-graded factorization of 0, and 
	    likewise a matrix factorization of a ring element f is equivalently a ZZ/2-graded factorization of f. Because of their similarity with complexes, much of the functionality
	    and syntax for this package closely resembles the "Complexes" package, with some key differences that we will highlight below.
	Example
            Q = QQ[x_1..x_3];
	    F1 = ZZdfactorization {x_1,x_2}
	    F1.dd
	    F2 = ZZdfactorization {x_1,x_2,x_3}
	    F2.dd
	Text
	    Notice that in the above, both the modules and the differentials are displayed modulo the period of the complex. Moreover, if the user tries to access the data of the modules
	    or differentials, this input is also taken modulo the period:
	Example
	    F1_0
	    F1_2
	    F1.dd_123
	Text
	    The package is implemented to not actually check for well-definedness of the factorization (that is, it does not check if all of the differentials actually compose to a scalar
	    multiple of the identity). The user can check this by using the isWellDefined and isdFactorization commands: 
	Example
	    isdFactorization F2
	Text
	    Much of the syntax and functionality of the types ZZdFactorization and ZZdFactorizationMap are based on current functionalities for the analogous objects in the Complexes package,
	    so there should be essentially no learning curve for users already familiar with working with chain complexes.
    SeeAlso
         "Making ZZdFactorizations"
         "Making maps between factorizations"
         "Basic invariants and properties"
	 "Preprogrammed examples and operations"
///

doc ///
    Key
        (ring, ZZdFactorization)
        (ring, ZZdFactorizationMap)
    Headline
        access the ring of a ZZ/d-graded factorization or a factorization map
    Usage
        ring C
    Inputs
        C:ZZdFactorization
            or a @TO "ZZdFactorizationMap"@
    Outputs
        :Ring
    Description
        Text
            Every ZZ/d-graded factorization or factorization map has a base ring.  This
            function accesses that information.
        Example
            S = ZZ/101[a,b,c,d];
            C = koszulMF({a,b,c,d}, a^2+b^2+c^2+d^2)
	    C.dd
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
        period
        (period, ZZdFactorization)
	(period, ZZdFactorizationMap)
    Headline
        the period of a ZZ/d-graded factorization or map
    Usage
        p = period C
    Inputs
        C:ZZdFactorization
    Outputs
        :ZZ
            the integer d for which C is a ZZ/d-graded factorization
    Description
        Text
            In this package, each factorization has a period d. This function is mainly used in programming, to loop over all
            non-zero modules or maps in the factorization.  This should not be confused
            with the support of a factorization (that is, all spots with a nonzero module).
        Example
            S = ZZ/101[a..c];
	    F = ZZdfactorization(S^0, 2)
	    period F -- even though all modules are 0, period is unchanged.
	    F == 0
            C = tailMF ideal vars (S/(a^2+b^2+c^2))
            period C
            D = C ++ C[5]
            period D
        Text
            Unlike the "concentration" command for complexes, indices outside the period are
	    taken modulo the period, and all of the standard operations on factorizations
	    leave the period unchanged. If you want to change the period of a factorization,
	    try using TO (unfold, ZZdFactorization, List) instead.
        Example
            C_-1
            D_4
        Text
            The function {\tt period} does no computation. Use prune to obtain the pruned outputs.
        Example
            f1 = a*id_C  
            E = coker f1
            Z = coker id_C
            Z == 0
	    period prune Z
        Text
            A module may be viewed as a ZZ/d-graded factorization with any period. We can construct
	    this factorization as follows. We can specify the base homological degree using the option
	    Base.
        Example      
            C0 = ZZdfactorization(coker vars S, 2)
	    C1 = ZZdfactorization(coker vars S, 2, Base => 1)
            period C0
	    period C1
            prune C0 == (prune C1)[1]
    SeeAlso
        "Basic invariants and properties"
        (symbol _, ZZdFactorization, ZZ)
        (period, ZZdFactorizationMap)
///


doc ///
    Key
        (ZZdfactorization, HashTable)
    Headline
        make a ZZ/d-graded factorization
    Usage
        ZZdfactorization H
    Inputs
        H:HashTable
            each key is an integer indexing a differential, and the 
            value at that key is the map
        Base => ZZ
            ignored when the input is a hash table
    Outputs
        :ZZdFactorization
    Description
        Text
            A ZZ/d-graded factorization is a sequence of objects $C_i$, connected by
            maps $dd^C_i : C_i \rightarrow C_{i-1}$ such that the
            composition of any d consecutive maps is equal to a fixed scalar multiple of the identity.

            In practice, a ZZdFactorization may be visualized as a sequence of R-module maps:
	    
	    $$F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_{d-1}$$
	    
	    with the caveat that $d^F_0 : F_0 \to F_{d-1}$, since one should count degree modulo d. 
        Example
            S = ZZ/101[a..d]
            F1 = map(S^2,S^2,matrix {{d, b}, {a, c}})
            F2 = map(source F1,S^2,matrix {{-c, b}, {a, -d}})
            C = ZZdfactorization hashTable{1 => F1, 2 => F2}
	    C.dd
            isWellDefined C
	    isdFactorization C
	    C.dd^2
        Text
            This is the primary constructor used by all of the more
            user friendly methods for constructing a ZZ/d-graded factorization.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, ZZdFactorization)@.
    SeeAlso
        "Making ZZdFactorizations"
        (isWellDefined, ZZdFactorization)
        (ZZdfactorization, List)
        (ZZdfactorization, Module, ZZ)
///


doc ///
    Key
        ZZdfactorization
        (ZZdfactorization, List)
    Headline
        make a ZZ/d-graded factorization
    Usage
        ZZdfactorization L
    Inputs
        L:List
            of maps
        Base => ZZ
            the index of the target of the first map 
            in the differential.
    Outputs
        :ZZdFactorization
    Description
        Text
            A ZZ/d-graded factorization is a sequence of objects $C_i$, connected by
            maps $dd^C_i : C_i \rightarrow C_{i-1}$ such that the
            composition of any d consecutive maps is equal to a fixed scalar multiple of the identity.

            In practice, a ZZdFactorization may be visualized as a sequence of R-module maps:
	    
	    $$F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_{d-1}$$
	    
	    with the caveat that $d^F_0 : F_0 \to F_{d-1}$, since one should count degree modulo d. 

	    Often, a factorization is most easily described by giving a list
            of consecutive maps which form the differentials.
            If the list consists entirely of ring elements, the constructor will recognize
	    that the user intends to use scalar multiplication as the maps in the factorization.
        Example
            S = ZZ/101[a..d]
            C1 = ZZdfactorization {a,b}
	    C2 = ZZdfactorization {a,b,c}
	    C3 = ZZdfactorization {a,b,c,d}
	    period C1
	    period C2
	    period C3
	    isdFactorization C1
	    isdFactorization C2
	    isdFactorization C3
        Text
            To start a factorization at a base different from zero, use the
            optional argument {\tt Base}.
        Example
            D1 = ZZdfactorization({a,b}, Base => 1)
            isWellDefined D1
	    isdFactorization D1
        Text
            Notice that this changes the homological degrees of the
            maps, but is not the same as the shift of the factorization
            (which will rescale the maps by roots of unity in general).
        Example
            dd^C1
            dd^(D1[-1])
        Text
            Having constructed this factorization, we can access individual
            terms and maps.
        Example
            C1_2
            C1^(-1)
            C1^(-1) == C1_1
            C1_7
            dd^C1
            dd^C1_2
            period C1
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a factorization
        is well constructed, use @TO (isWellDefined, ZZdFactorization)@.
    SeeAlso
        "Making ZZdFactorizations"
        (isWellDefined, ZZdFactorization)
        (ZZdfactorization, HashTable)
        (symbol SPACE, ZZdFactorization, Array)
///

doc ///
    Key
        (ZZdfactorization, ZZdFactorization)
    Headline
        make a ZZ/d-graded factorization with shifted base; used for permuting the modules without adding signs
    Usage
        ZZdfactorization X
    Inputs
        X:ZZdFactorization
            specifies maps
        Base => ZZ
            the index of the target of the first map 
            in the differential.
    Outputs
        :ZZdFactorization
    Description
        Text
            A ZZ/d-graded factorization is a sequence of objects $C_i$, connected by
            maps $dd^C_i : C_i \rightarrow C_{i-1}$ such that the
            composition of any d consecutive maps is equal to a fixed scalar multiple of the identity.

            In practice, a ZZdFactorization may be visualized as a sequence of R-module maps:
	    
	    $$F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_{d-1}$$
	    
	    with the caveat that $d^F_0 : F_0 \to F_{d-1}$, since one should count degree modulo d. 

	    If one wants to shift the differentials of a factorization without having to redefine or
	    add signs, this can be done as follows:
        Example
            S = ZZ/101[a..d]
            C1 = ZZdfactorization {a,b}
	    C1.dd
	    C1' = ZZdfactorization(C1, Base => 1)
	    C1'.dd
	    C2 = ZZdfactorization {a,b,c}
	    C2.dd
	    C2' = ZZdfactorization(C2, Base => 2)
	    C2'.dd
	    C3 = ZZdfactorization {a,b,c,d}
	    C3.dd
	    C3' = ZZdfactorization(C3, Base => 3)
	    C3'.dd
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a factorization
        is well constructed, use @TO (isWellDefined, ZZdFactorization)@.
    SeeAlso
        "Making ZZdFactorizations"
        (isWellDefined, ZZdFactorization)
        (ZZdfactorization, HashTable)
        (symbol SPACE, ZZdFactorization, Array)
///


doc ///
    Key
        (ZZdfactorization, Module, ZZ)
	(ZZdfactorization, Ring, ZZ)
	(ZZdfactorization, Ideal, ZZ)
    Headline
        convert a module/ring/ideal into a ZZ/d-graded factorization for specified period d
    Usage
        ZZdfactorization(M, d)
    Inputs
        M:Module
	d:ZZ
	    specifies the period of the resulting factorization
        Base => ZZ
            the index of the target of the first map 
            in the differential.
    Outputs
        :ZZdFactorization
    Description
        Text
            A ZZ/d-graded factorization is a sequence of objects $C_i$, connected by
            maps $dd^C_i : C_i \rightarrow C_{i-1}$ such that the
            composition of any d consecutive maps is equal to a fixed scalar multiple of the identity.

            In practice, a ZZdFactorization may be visualized as a sequence of R-module maps:
	    
	    $$F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_{d-1}$$
	    
	    with the caveat that $d^F_0 : F_0 \to F_{d-1}$, since one should count degree modulo d. 

	    Sometimes the user may wish to view a module as a ZZ/d-graded factorization with zeros in all
	    other spots. This can be accomplished with the following syntax:
        Example
            S = ZZ/101[a..c]
	    M = coker vars S
            C1 = ZZdfactorization(M, 2)
	    isWellDefined C1
	    isZZdComplex C1
	    C2 = ZZdfactorization(M, 3)
	    C2'= ZZdfactorization(M, 3, Base => 1)
	    C2'' = ZZdfactorization(M, 3, Base => 2)
        Text
            To start a factorization at a base different from zero, use the
            optional argument {\tt Base}.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a factorization
        is well constructed, use @TO (isWellDefined, ZZdFactorization)@.
    SeeAlso
        "Making ZZdFactorizations"
        (isWellDefined, ZZdFactorization)
        (ZZdfactorization, HashTable)
        (symbol SPACE, ZZdFactorization, Array)
///



doc ///
    Key
        isdFactorization
        (isdFactorization, ZZdFactorization)
    Headline
        Checks if the differentials of a factorization compose to a scalar multiple of the identity
    Usage
        isdFactorization(X)
    Inputs
        X: ZZdFactorization
            The input d-fold matrix factorization
    Outputs
        :Sequence
            A sequence specifying whether the differentials of the factorization compose to a scalar multiple of the identity,
            where the second term of the sequence is the scalar multiple if it exists, otherwise it outputs no potential.
    Description
        Text
            This method checks if the differentials of a factorization compose to a scalar multiple of the identity.
            It returns a sequence where the first element is a boolean value indicating whether the factorization is well-defined,
            and the second element is the scalar multiple if it exists, otherwise it outputs no potential.

	    This check is distinct from isWellDefined for factorizations, since it does not do any other checks
	    for well-definedness except for checking the differentials compose to a scalar multiple of the identity.
        Example
            Q = ZZ/101[x_1..x_3];
	    X = ZZdfactorization {x_1, x_2 , x_3}
	    isdFactorization(X)
	    f = x_1^3 + x_2^3 + x_3^3;
	    X2 = randomTailMF(f, 2, 4, 2)
	    X2.dd
	    isdFactorization X2
	    randomLinearMF(2,Q)
	    isdFactorization oo
	    L = linearMF(f, t) --must specify root of unity t for larger period
	    isdFactorization L
	    L.dd
	    L.dd^3
    Caveat
        This command is distinct from isWellDefined, since it only checks if the differentials compose
	to be a scalar multiple of the identity.
    SeeAlso
        (isWellDefined, ZZdFactorization)
	(tailMF, Module)
	(randomTailMF, RingElement)
	(linearMF, RingElement)
	(randomLinearMF, ZZ, Ring)
///




doc ///
   Key
     (symbol _, ZZdFactorization, ZZ)
     (symbol ^, ZZdFactorization, ZZ)
   Headline
     access individual object in a ZZ/d-graded factorization
   Usage
     C_i
     C^i
   Inputs
     C:ZZdFactorization
     i:ZZ
       either the homological or cohomological index
   Outputs
     :Module
       the {\tt i}-th object
   Description
    Text
       ZZ/d-graded factorizations can be either homologically or cohomologically graded.  Subscripts
       refer to homological indices, and superscripts refer to
       cohomological indices.
     
       In this package homological indices are used by default.  We always have the equation $C^i = C_{-i}$.
    Example
      S = ZZ/101[x_1..x_3]
      C = koszulMF({x_1,x_2,x_3}, x_1^2+x_2^2+x_3^2)
      C_2
      C^(-2)
      C_2 == C^(-2)
    Text
      Indices that are outside of the period are taken modulo the period.
    Example
      C_-7
   SeeAlso
     "Making ZZdFactorizations"
///

doc ///
   Key
     (symbol ==, ZZdFactorization, ZZdFactorization)
     (symbol ==, ZZdFactorization, ZZ)
     (symbol ==, ZZ, ZZdFactorization)
   Headline
     whether two ZZ/d-graded factorizations are equal
   Usage
     C == D
     C == 0
   Inputs
     C:ZZdFactorization
     D:ZZdFactorization
   Outputs
     :Boolean
       that is true when {\tt C} and {\tt D} are equal
   Description
    Text
      Two factorizations are equal if the corresponding 
      objects and corresponding maps at each index are equal.
    Example
      S = ZZ/101[a..c]/(a^2+b^2+c^2)
      C = tailMF coker vars S
      C.dd
      D = C[3][-3]
      C === D
      C == D
    Text
      Both the maps and the objects must be equal.
    Example
      E = ZZdfactorization {0*id_(C_0), 0*id_(C_1)}
      dd^E
      C == E
      E == 0
      dd^E == 0
    Text
      A complex is equal to zero if all the objects and maps are zero.
      This could require computation to determine if something that
      is superficially not zero is in fact zero.
    Example
      f = id_C
      D = coker f
      D == 0
    Text
      Testing for equality is not the same testing for isomorphism.
      In particular, different presentations of a factorization need not be equal.
    Example
      use ambient S
      K = koszulMF({a,b,c}, a^2+b^2+c^2)
      C == K
    Text
      The matrix factorizations $C$ and $K$ above are isomorphic, however.
   SeeAlso
     "Making ZZdFactorizations"
///

doc ///
    Key
        "differential of a ZZ/d-graded factorization"
        (symbol^, Symbol, ZZdFactorization)
    Headline
        get the maps between the terms in a ZZ/d-graded factorization
    Usage
        dd^C
        dd_C
    Inputs
        C:ZZdFactorization
    Outputs
        :ZZdFactorizationMap
            a map of degree -1
    Description
        Text
            A Z/d-graded factorization is a sequence of modules connected
            by homomorphisms, called differentials, such that any d-fold
            composition of the maps is a scalar multiple of the identity.
        Text
            One can access the differential of a factorization as follows.
        Example
            R = QQ[a..d]/(c^2-b*d+a^2);
	    m = ideal vars R
            C = tailMF (m^2) 
            dd^C
            assert(dd^C === C.dd)
            assert(source dd^C === C)
            assert(target dd^C === C)
            assert(degree dd^C === -1)
        Text
            The composition of the differential with itself should be a scalar multiple of the identity.
        Example
            (dd^C)^2 == (R.relations)_(0,0)
        Text
            The individual maps between terms are indexed by their
            source.
        Example
            dd^C_2
            assert(source dd^C_2 === C_2)
            assert(target dd^C_2 === C_1)
    SeeAlso
        "Making maps between ZZ/d-graded factorizations"
        (symbol_, ZZdFactorizationMap, ZZ)
        (symbol_, ZZdFactorization, ZZ)
        (source, ZZdFactorizationMap)
        (target, ZZdFactorizationMap)
        (degree, ZZdFactorizationMap)
///


doc ///
   Key
     (symbol SPACE, ZZdFactorization, Array)
     (symbol SPACE, ZZdFactorizationMap, Array)
   Headline
     shift a ZZ/d-graded factorization or map of ZZ/d-graded factorizations
   Usage
     D = C[i]
     g = f[i]
   Inputs
     C:ZZdFactorization
       or {\tt f}, a @TO ZZdFactorizationMap@
     :Array
       {\tt [i]}, where {\tt i} is an integer
   Outputs
     D:ZZdFactorization
       or {\tt g}, a @TO ZZdFactorizationMap@.
   Description
    Text
      The shifted factorization $D$ is defined by $D_j = C_{i+j}$ for all $j$
      and the $j$th differential is rescaled by $t^i$, where $t$ is some primitive root of unity.
      Note that a factorization of period > 2 must have a root of unity adjoined in order to
      define the shift.
       
      The shifted complex map $g$ is defined by $g_j = f_{i+j}$ for all $j$.
    
      The shift defines a natural automorphism on the category of ZZ/d-graded factorizations. 
      For factorizations with period greater than $2$, the shift is not necessarily the same as the
      suspension functor; see TO (suspension, ZZdFactorization) to see how these functors differ.
    Example
      R = QQ[a..d]/(c^2-b*d+a^2);
      m = ideal vars R
      C = tailMF (m^2) 
      dd^C_3
      D = C[1]
      assert isWellDefined D
      assert(dd^D_2 == -dd^C_3)
      Q = ZZ/101[a..c];
      K = linearMF(a^3 + b^3 + c^3, t)
      K.dd
      (K[1]).dd
      isWellDefined (K[1])
      potential (K[2])
      assert(K[3] == K)
    Text
      In order to shift the factorization by one step, and not change the differential, one
      can do the following.
    Example
      E = ZZdfactorization(C, Base => -1)
      assert isWellDefined E
      assert(dd^E_2 == dd^C_3)
    Text
      The shift operator is functorial, as illustrated below.
    Example
      i = id_E[1]
      not(id_E == i)
      i^2 == id_E
      dd^E[1]
      isdFactorization(E[1])
   SeeAlso
     period
     (ZZdfactorization, ZZdFactorization)
///



doc ///
    Key
        (gradedModule, ZZdFactorization)
    Headline
        a new ZZ/d-graded factorization in which the differential is zero
    Usage
        gradedModule C
    Inputs
        C:ZZdFactorization
    Outputs
        :ZZdFactorization
            whose differential is the zero map
    Description
        Text
            This routine isolates the terms in the factorization
            and forgets the differentials
        Example
            R = ZZ/101[a,b,c]/(a^3+b^3+c^3);
            C = tailMF ideal vars R
            dd^C
            G = gradedModule C
            dd^G
	    dd^G == 0
            assert((isdFactorization G)_0)
            assert(G != C)
	    assert(HH G == G)
    SeeAlso
        (homology, ZZdFactorization)
///


doc ///
   Key
     (homology, ZZdFactorization)
   Headline
     homology of a ZZ/d-graded factorization
   Usage
     H = HH C
   Inputs
     C:ZZdFactorization
   Outputs
     H:ZZdFactorization
   Description
    Text
      The homology of a 2-fold factorization $H$ is defined by {\tt ker dd^C}/{\tt image dd^C}.
      The differential of the homology complex is the zero map. If the differentials
      of the factorization do not compose to 0, then the homology is not defined. Similarly,
      in order to make sense of the homology of a d-fold factorization for d > 3, one should
      use the fullCollapse command.
      
      An easy way to construct complexes from factorizations is to use the End command, which computes
      the endomorphisms of the factorization. In general, if $F$ is a factorization of some polynomial
      $f$ and $G$ is a factorization of some polynomial $g$, then $\operatorname{Hom} (F,G)$ is a factorization
      of $g-f$. 
    Example
      Q = ZZ/11[x,y];
      F = randomLinearMF(2,Q)
      E = Hom(F,F)
      isZZdComplex E
    Text
      Thus $E$ is a $2$-periodic complex. This means that the homology is well-defined:
    Example
      prune HH E
      R = Q/(potential F);
      netList for i to 3 list prune Ext^i (coker vars R, coker vars R)
    Text
      In the above case, the homology of the endomorphism factorization is computing the stable Ext
      of the residue field over the hypersurface $R/(f)$, ie, the Ext module obtained by taking sufficiently
      high Ext values.
   SeeAlso
     (dual, ZZdFactorization)
     (prune, ZZdFactorization)
///


doc ///
   Key
     (homology,ZZ,ZZdFactorization)
     (homology, ZZ, ZZ, ZZdFactorization)
   Headline
     homology or cohomology module of a ZZ/d-graded factorization
   Usage
     HH_i C
   Inputs
     i:ZZ
     C:ZZdFactorization
   Outputs
     :Module
       the $i$-th homology or cohomology of the ZZ/d-graded factorization
   Description
    Text
      The homology of a 2-fold factorization $H$ is defined by {\tt ker dd^C}/{\tt image dd^C}.
      The differential of the homology complex is the zero map. If the differentials
      of the factorization do not compose to 0, then the homology is not defined. Similarly,
      in order to make sense of the homology of a d-fold factorization for d > 2, one should specify both
      the spot to take homology in, and also how many compositions of the differentials to use (since there
      are multiple ways to compose the differentials for a longer factorization)
      
      An easy way to construct complexes from factorizations is to compute
      the endomorphisms of the factorization. In general, if $F$ is a factorization of some polynomial
      $f$ and $G$ is a factorization of some polynomial $g$, then $\operatorname{Hom} (F,G)$ is a factorization
      of $g-f$. 
    Example
      Q = ZZ/11[x,y];
      F = randomLinearMF(2,Q)
      E = Hom(F,F)
      isZZdComplex E
    Text
      Thus $E$ is a $2$-periodic complex. This means that the homology is well-defined, and moreover
      the indices of homology are also taken modulo 2:
    Example
      prune HH_0 E
      prune HH_1 E
      prune HH_4 E
      R = Q/(potential F);
      netList for i to 3 list prune Ext^i (coker vars R, coker vars R)
    Text
      In the above case, the homology of the endomorphism factorization is computing the stable Ext
      of the residue field over the hypersurface $R/(f)$, ie, the Ext module obtained by taking sufficiently
      high Ext values.
   SeeAlso
     prune
     (dual, Complex)
///

doc ///
   Key
     (directSum, ZZdFactorization)
     (symbol++, ZZdFactorization, ZZdFactorization)
   Headline
     direct sum of ZZ/d-graded factorizations
   Usage
     D = C1 ++ C2
     D = directSum(C1,C2,...)
     D = directSum(name1 => C1, name2 => C2, ...)
   Inputs
     Ci:ZZdFactorization
   Outputs
     D:ZZdFactorization
       the direct sum of the input ZZ/d-graded factorizations
   Description
    Text
      The direct sum of two factorizations is another factorization, assuming the inputs factor the same ring element.
      The differentials are simply the direct sum of the constituent differentials.
    Example
      S = ZZ/101[x,y];
      C1 = randomLinearMF(2,S)
      C2 = randomLinearMF(2,S)
      C12 = C1 ++ C2
      potential(C1) == potential(C2)
      isdFactorization(C12)
      C12.dd^2
    Text
      As we can see in the above example, the problem stems from the fact that the direct sum of matrix factorizations
      with different potentials is no longer a matrix factorization. Constructing factorizations with the same potential
      will solve this:
    Example
      Q = ZZ/101[x,y]
      f = x^3+y^3;
      R = Q/(f)
      m = ideal vars R
      F1 = tailMF m
      F2 = tailMF (m^2)
      potential(F1) == potential(F2)
      F12 = F1++ F2
      isdFactorization F12
    Text
      The direct sum is an n-ary operator with projection and
      inclusion maps from each component satisfying appropriate
      identities.
    Example
      C4 = directSum(first => F1, second => F2)
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
      Given a factorization which is a direct sum, we obtain the component
      complexes and their names (indices) as follows (even if the resulting factorization
	  is not well-defined, we can still obtain the components).
    Example
      components C12
      indices C12
      components C4
      indices C4
   SeeAlso
     (components,ZZdFactorization)
     indices
     (symbol^, ZZdFactorization, Array)
     (symbol_, ZZdFactorization, Array)
     (isShortExactSequence, ZZdFactorizationMap, ZZdFactorizationMap)
     (sum, Complex)
///

doc ///
   Key
     (symbol_, ZZdFactorization, Array)
     (symbol^, ZZdFactorization, Array)
   Headline
     the canonical inclusion or projection map of a direct sum
   Usage
     i = C_[name]
     p = C^[name]
   Inputs
     C:ZZdFactorization
     name:
   Outputs
     :ZZdFactorizationMap
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
      f = a^3+b^3+c^3;
      C1 = randomTailMF(f, 2, 4, 4)
      C2 = randomTailMF(f, 2, 4, 4)
      D = C1 ++ C2
      isdFactorization D
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
      E = (cheese => C1) ++ (crackers => C2)
      E_[cheese]
      E_[crackers]
      E^[cheese] * E_[cheese] == 1
      E^[crackers] * E_[crackers] == 1
      E^[cheese] * E_[crackers] == 0
      E^[crackers] * E_[cheese] == 0
      E_[cheese] * E^[cheese] + E_[crackers] * E^[crackers] == 1
    Text
      One can also access inclusion and projection maps of sub-direct sums.
    Example
      F = directSum(C1, C2, randomTailMF(f))
      F^[0,1]
      F_[0,2]
   SeeAlso
     (directSum, Complex)
     (components, Complex)
     indices
///

doc ///
   Key
     (components, ZZdFactorization)
   Headline
     list the components of a direct sum
   Usage
     components C
   Inputs
     C:ZZdFactorization
   Outputs
     :List
       the component factorizations of a direct sum (of ZZ/d-graded factorizations)
   Description
    Text
      A ZZ/d-graded factorization which has been constructed as a direct sum
      stores its component factorizations.
    Example
      S = ZZ/101[a,b,c];
      f = a^3 + b^3 + c^3;
      C1 = randomTailMF(f)
      C2 = randomTailMF(f)
      D = C1 ++ C2
      L = components D
      L_0 === C1
      L_1 === C2
      E = (peanut => C1) ++ (butter => C2)
      components E
    Text
      The names of the component complexes are called indices, 
      and are used to access the relevant inclusion and projection maps.
    Example
      indices D
      D^[0]
      indices E
      E_[butter]
   SeeAlso
     (directSum, ZZdFactorization)
     indices
     (symbol_, ZZdFactorization, Array)
     (symbol^, ZZdFactorization, Array)
///





doc ///
   Key
     (symbol**, ZZdFactorization, ZZdFactorization)
     (symbol**, Complex, ZZdFactorization)
     (symbol**, ZZdFactorization, Complex)
     (symbol**, ZZdFactorization, Module)
     (symbol**, Module, ZZdFactorization)
     (tensor, ZZdFactorization, ZZdFactorization)
   Headline
     tensor product of ZZ/d-graded factorizations
   Usage
     D = C1 ** C2
   Inputs
     C1:ZZdFactorization
       or @ofClass Module@ or @ofClass ZZdFactorization@
     C2:ZZdFactorization
       or @ofClass Module@ or @ofClass ZZdFactorization@
   Outputs
     D:ZZdFactorization
       tensor product of {\tt C1} and {\tt C2}
   Description
    Text
      The tensor product is a ZZ/d-graded factorization $D$ whose $i$th component is
      the direct sum of $C1_j \otimes C2_k$ over all $i = j+k \mod d$.
      The differential on $C1_j \otimes C2_k$ is the differential 
      $dd^{C1} \otimes id_{C2} + t^j id_{C1} \otimes dd^{C2}$, where $t$ is some
      primitive $d$th root of unity (assuming both factorizations have period $d$).
      
      The use of a primitive $d$th root of unity adds some subtlety to this construction,
      since for $d > 2$ the user may need to adjoin a root of unity using the @TO adjoinRoot@ command.
      In general, if $C1$ is a factorizations of $f$ and $C2$ is a factorization of $g$, then the tensor product
      $C1 \otimes C2$ is a factorization of the sum $f + g$. As the next example illustrates, this allows one
      to always construct matrix factorizations by taking the tensor product of "trivial" factorizations.
    Example
      S = ZZ/101[a..c]
      Ca = ZZdfactorization {a,a}
      Cb = ZZdfactorization {b,b}
      Cc = ZZdfactorization {c,c}
      Cab = Cb ** Ca
      diffs = dd^Cab
      diffs^2
      potential Ca
      potential Cb
      potential Cab
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      diffs2 = dd^Cabc
      diffs2^2
    Text
      Let us see an example of a longer factorization and tensor products of these objects. In the following
      example, we use the notation $t$ to denote a primitive $3$rd root of unity:
    Example
      St = adjoinRoot(3,S,t)
      t^3
      St.rootOfUnity
      D1 = (ZZdfactorization {a,a,a})**St
      D2 = (ZZdfactorization {b,b,b})**St
      D12 = D1**D2
      diffs = D12.dd
      diffs^3
      potential D1
      potential D2
      potential D12
    Text
      If one of the arguments is a module, it is considered as a ZZ/d-graded factorization concentrated in homological degree 0.
    Example
      Cabc ** (S^1/(a,b,c))
      isdFactorization oo
      S^2 ** Cabc
    Text
      If one of the arguments is a complex, the function will automatically @TO Fold@ the complex
      and then take the tensor product. Since we are tensoring with a complex, the potential
      will remain unchanged.
    Example
      K = koszulComplex vars S;
      KCa = K**Ca
      assert isWellDefined KCa
      potential KCa
    Text
      Because the tensor product can be regarded as the totalization of a bifactorization,
      each term of the tensor product comes with pairs of indices, labelling the summands.
    Example
      indices Cabc_1
      components Cabc_1
      Cabc_1_[{1,0}]
      indices Cabc_2
      components Cabc_2
      indices D12_0
      indices D12_1
      indices D12_2
    Text
      Note in the above how the ZZ/d-graded tensor product differs from the standard tensor product of
      complexes. Since the indices are taken modulo the period, we see that both $\{ 0, 0 \}$ and $\{ 1, 1 \}$ are
      indices in degree $0$ (and likewise the indices for $D12$ are taken modulo $3$).
   SeeAlso
     indices
     components
     directSum
///



doc ///
   Key
     (Hom, ZZdFactorization, ZZdFactorization)
     (Hom, ZZdFactorization, Module)
     (Hom, Module, ZZdFactorization)     
     (Hom, ZZdFactorization, Ring)
     (Hom, Ring, ZZdFactorization)     
   Headline
     the ZZ/d-graded homomorphism factorization between two ZZ/d-graded factorizations
   Usage
     D = Hom(C1,C2)
   Inputs
     C1:ZZdFactorization
       or @ofClass Module@, or @ofClass Ring@
     C2:ZZdFactorization
       or @ofClass Module@, or @ofClass Ring@
   Outputs
     D:ZZdFactorization
       the ZZ/d-graded factorization of homomorphisms between {\tt C1} and {\tt C2}
   Description
    Text
      The factorization of homomorphisms is a ZZ/d-graded factorization $D$ whose $i$th component is
      the direct sum of $Hom(C1_j, C2_{k})$ over all $k-j = i \mod d$.
      The differential on $Hom(C1_j, C2_{k})$ is the differential 
      $Hom(id_{C1}, dd^{C2}) + t^j Hom(dd^{C1}, id_{C2})$ where $t$ is a primitive $d$th root of unity.

      The use of a primitive $d$th root of unity adds some subtlety to this construction,
      since for $d > 2$ the user may need to adjoin a root of unity using the \texttt{adjoinRoot} command.
      In general, if $C1$ is a factorizations of $f$ and $C2$ is a factorization of $g$, then the Hom factorization
      $\operatorname{Hom} (C1,C2)$ is a factorization of the difference $g-f$.
    Example
      S = ZZ/101[a..c];
      f = a^3 + b^3 + c^3
      C = randomTailMF(f)
      D = Hom(C,C)
      isZZdComplex D
      Q = ZZ/101[x,y];
      F = randomLinearMF(2,Q)
      E = Hom(F,F)
      diffs = E.dd
      diffs^2
    Text
      The homology of $E$ actually computes the stable Ext of the maximal Cohen-Macaulay module defined
      by the matrix factorization (up to twists).
    Example
      R = Q/(potential F)
      M = coker (F.dd_0**R)
      prune Ext^1(M,M)
      prune Ext^2(M,M)
      prune HH E
    Text
      If one of the arguments is a module or a ring, it is considered as a factorization concentrated in degree 0.
    Example
      E = Hom(C, S^2)
      isdFactorization E
    Text
      There is a simple relationship between Hom factorizations and @TO2 ((symbol SPACE, Complex, Array), "shifts")@.
      Specifically, shifting the first argument is the same as the negative shift of the result.  But
      shifting the second argument is only the same as the positive shift of the result
      up to a sign.
    Example
      Hom(C[3], C) == D[-3]
      Hom(C, C[-2]) == D[-2]
      Hom(C, C[-3]) != D[-3]
    Text
      Specific maps and morphisms between complexes can be obtained
      with the homomorphism command.
    Text
      Because the Hom factorization can be regarded as the totalization of a d-periodic double complex,
      each term comes with pairs of indices, labelling the summands.
    Example
      indices D_-1
      components D_-1
      indices D_-2
      components D_-2
   SeeAlso
     (homomorphism, ComplexMap)
     (homomorphism', ComplexMap)
     (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
     indices
     components
     (Hom, ZZdFactorizationMap, ZZdFactorizationMap)
///

doc ///
   Key
     (Hom, Complex, ZZdFactorization)
     (Hom, Complex, ZZdFactorization, Symbol)
     (Hom, Complex, ZZdFactorization, RingElement)
     (Hom, ZZdFactorization, Complex)
     (Hom, ZZdFactorization, Complex, Symbol)
     (Hom, ZZdFactorization, Complex, RingElement)
   Headline
     the ZZ/d-graded homomorphism factorization formed by folding the complex, then taking the ZZ/d-graded Hom factorization
   Usage
     D = Hom(C1,C2)
   Inputs
     C1:Complex
       or @ofClass ZZdFactorization@
     C2:ZZdFactorization
       or @ofClass Complex@, with an optional Symbol or RingElement input specifying a root of unity
   Outputs
     D:ZZdFactorization
       the ZZ/d-graded factorization of homomorphisms between {\tt C1} and {\tt C2}
   Description
    Text
      The factorization of homomorphisms is a ZZ/d-graded factorization $D$ whose $i$th component is
      the direct sum of $Hom(C1_j, C2_{k})$ over all $k-j = i \mod d$.
      The differential on $Hom(C1_j, C2_{k})$ is the differential 
      $Hom(id_{C1}, dd^{C2}) + t^j Hom(dd^{C1}, id_{C2})$ where $t$ is a primitive $d$th root of unity.

      The use of a primitive $d$th root of unity adds some subtlety to this construction,
      since for $d > 2$ the user may need to adjoin a root of unity using the \texttt{adjoinRoot} command.
      In general, if $C1$ is a factorizations of $f$ and $C2$ is a factorization of $g$, then the Hom factorization
      $\operatorname{Hom} (C1,C2)$ is a factorization of the difference $g-f$.

      If one of the inputs is a complex, the method will automatically @TO Fold@ the complex and take
      the d-periodic Hom.
    Example
      S = ZZ/101[a..c];
      f = a^3 + b^3 + c^3
      C = randomTailMF(f)
      K = koszulComplex vars S
      D = Hom(K,C)
      isWellDefined D
      potential C
      potential D
   SeeAlso
     (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
     indices
     components
     (Hom, ZZdFactorizationMap, ZZdFactorizationMap)
///


doc ///
   Key
     (Hom, ZZdFactorization, ZZdFactorization, Symbol)
     (Hom, ZZdFactorization, ZZdFactorization, RingElement)
     (Hom, ZZdFactorization, Module, Symbol)
     (Hom, ZZdFactorization, Module, RingElement)
   Headline
     the ZZ/d-graded homomorphism factorization between two ZZ/d-graded factorizations
   Usage
     D = Hom(C1,C2,t)
   Inputs
     C1:ZZdFactorization
       or @ofClass Module@
     C2:ZZdFactorization
       or @ofClass Module@
     t:Symbol
       or @ofClass RingElement@
   Outputs
     D:ZZdFactorization
       the ZZ/d-graded factorization of homomorphisms between {\tt C1} and {\tt C2}
   Description
    Text
      The factorization of homomorphisms is a ZZ/d-graded factorization $D$ whose $i$th component is
      the direct sum of $Hom(C1_j, C2_{k})$ over all $k-j = i \mod d$.
      The differential on $Hom(C1_j, C2_{k})$ is the differential 
      $Hom(id_{C1}, dd^{C2}) + t^j Hom(dd^{C1}, id_{C2})$ where $t$ is a primitive $d$th root of unity.

      The use of a primitive $d$th root of unity adds some subtlety to this construction,
      since for $d > 2$ the user may need to adjoin a root of unity using the \texttt{adjoinRoot} command.
      In general, if $C1$ is a factorizations of $f$ and $C2$ is a factorization of $g$, then the Hom factorization
      $\operatorname{Hom} (C1,C2)$ is a factorization of the difference $g-f$.
    Example
      S = ZZ/101[a..c];
      f = a^3 + b^3 + c^3
      C = randomTailMF(f)
      D = Hom(C,C)
      isZZdComplex D
      Q = ZZ/101[x,y];
      F = randomLinearMF(2,Q)
      E = Hom(F,F)
      diffs = E.dd
      diffs^2
    Text
      The homology of $E$ actually computes the stable Ext of the maximal Cohen-Macaulay module defined
      by the matrix factorization (up to twists).
    Example
      R = Q/(potential F)
      M = coker (F.dd_0**R)
      prune Ext^1(M,M)
      prune Ext^2(M,M)
      prune HH E
    Text
      If one of the arguments is a module or a ring, it is considered as a factorization concentrated in degree 0.
    Example
      E = Hom(C, S^2)
      isdFactorization E
    Text
      There is a simple relationship between Hom factorizations and @TO2 ((symbol SPACE, Complex, Array), "shifts")@.
      Specifically, shifting the first argument is the same as the negative shift of the result.  But
      shifting the second argument is only the same as the positive shift of the result
      up to a sign.
    Example
      Hom(C[3], C) == D[-3]
      Hom(C, C[-2]) == D[-2]
      Hom(C, C[-3]) != D[-3]
    Text
      Specific maps and morphisms between complexes can be obtained
      with the homomorphism command.
    Text
      Because the Hom factorization can be regarded as the totalization of a d-periodic double complex,
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


-*
doc ///
    Key
        (homomorphism, ZZdFactorizationMap)
    Headline
        get the homomorphism from an element of Hom
    Usage
        g = homomorphism f
    Inputs
        f:ZZdFactorizationMap
            a map of the form $f : R^1 \to Hom(C, D)$, where
            $C$ and $D$ are complexes,
            $Hom(C,D)$ has been previously computed, and $R$ is
            the underlying ring of these complexes
    Outputs
        g:ZZdFactorizationMap
            the corresponding map of chain complexes from $C$ to $D$
    Description
        Text
            As a first example, consider two Koszul complexes $C$ and $D$.
            From a random map $f : R^1 \to Hom(C, D)$, we construct 
            the corresponding map of chain complexes $g : C \to D$.
        Example
            R = ZZ/101[a,b,c]
            C = freeResolution ideal"a,b,c"
            D = freeResolution ideal"a2,b2,c2"
            H = Hom(C,D)
            f = randomFactorizationMap(H, complex R^{-2})
            isWellDefined f
            g = homomorphism f
            isWellDefined g
            assert not isCommutative g
        Text
            The map $g : C \to D$ corresponding to a random map into $Hom(C,D)$
            does not generally commute with the differentials.  However, if the
            element of $Hom(C,D)$ is a cycle, then the corresponding map does commute.
        Example
            f = randomComplexMap(H, complex R^{-2}, Cycle => true)
            isWellDefined f
            g = homomorphism f
            isWellDefined g
            assert isCommutative g
            assert(degree g === 0)
            assert(source g === C)
            assert(target g === D)
            assert(homomorphism' g == f)
        Text
            A homomorphism of non-zero degree can be encoded
            in (at least) two ways.
        Example
            f1 = randomComplexMap(H, complex R^1, Degree => -2)
            f2 = map(target f1, (source f1)[2], i -> f1_(i+2))
            assert isWellDefined f2
            g1 = homomorphism f1
            g2 = homomorphism f2
            assert(g1 == g2)
            assert isWellDefined g1
            assert isWellDefined g2
            homomorphism' g1 == f1
            homomorphism' g2 == f1
    SeeAlso
        (homomorphism, Matrix)
        (homomorphism, ZZ, Matrix, Complex)
        (homomorphism', ComplexMap)
        (Hom, Complex, Complex)
        (randomComplexMap, Complex, Complex)
///
*-

-*
doc ///
    Key
        (homomorphism', ZZdFactorizationMap)
    Headline
        get the element of Hom from a map of ZZ/d-graded factorizations
    Usage
        f = homomorphism g
    Inputs
        g:ZZdFactorizationMap
            from $C$ to $D$
    Outputs
        f:ZZdFactorizationMap
            a map of the form $f : R^1 \to Hom(C, D)$, where
            $R$ is the underlying ring of these ZZ/-graded factorizations
    Description
        Text
            As a first example, consider two Koszul complexes $C$ and $D$.
            From a random map $f : R^1 \to Hom(C, D)$, we construct 
            the corresponding map of chain complexes $g : C \to D$.
        Example
            R = ZZ/101[a,b,c]
            C = freeResolution ideal"a,b,c"
            D = freeResolution ideal"a2,b2,c2"
            g = randomComplexMap(D, C, InternalDegree => 2)
            isWellDefined g
            f = homomorphism' g
            isWellDefined f
        Text
            The map $g : C \to D$ corresponding to a random map into $Hom(C,D)$
            does not generally commute with the differentials.  However, if the
            element of $Hom(C,D)$ is a cycle, then the corresponding map does commute.
        Example
            g = randomComplexMap(D, C, Cycle => true, InternalDegree => 3)
            isWellDefined g
            f = homomorphism' g
            isWellDefined f
            assert isCommutative g
            assert(degree f === 0)
            assert(source f == complex(R^{-3}))
            assert(target g === D)
            assert(homomorphism f == g)
    SeeAlso
        "Working with Ext"
        (homomorphism', Matrix)
        (homomorphism, ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorization)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///
*-

-*
doc ///
    Key
        (homomorphism, ZZ, Matrix, ZZdFactorization)
    Headline
        get the homomorphism from an element of Hom
    Usage
        g = homomorphism(i, f, E)
    Inputs
        i:ZZ
        f:Matrix
            a map of the form $f \colon R^1 \to E_i$
        E:ZZdFactorization
            having the form
            $E = \operatorname{Hom}(C, D)$ for some ZZZ/d-graded factorizations $C$ and $D$
    Outputs
        g:ZZdFactorizationMap
            the corresponding map of chain complexes from $C$ to $D$ of degree $i$
    Description
        Text
            An element of the complex $\operatorname{Hom}(C, D)$ corresponds to a map of 
            ZZ/d-graded factorizations from $C$ to $D$.  Given an element in the $i$-th term, this
            method returns the corresponding map of factorizations of degree $i$.
        Text
            As a first example, consider two Koszul complexes $C$ and $D$.
            From a random map $f \colon R^1 \to Hom(C, D)$, we construct 
            the corresponding map of chain complexes $g \colon C \to D$.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution ideal"a,b,c"
            D = freeResolution ideal"a2,b2,c2"
            E = Hom(C,D)
            f = random(E_2, R^{-5})
            g = homomorphism(2, f, E)
            assert isWellDefined g
            assert not isCommutative g
        Text
            The map $g \colon C \to D$ corresponding to a random map into $Hom(C,D)$
            does not generally commute with the differentials.  However, if the
            element of $Hom(C,D)$ is a cycle, then the corresponding map does commute.
        Example
            h = randomComplexMap(E, complex R^{-2}, Cycle => true, Degree => -1)
            f = h_0
            g = homomorphism(-1, f, E)
            assert isWellDefined g
            assert isCommutative g
            assert(degree g === -1)
            assert(source g === C)
            assert(target g === D)
            assert(homomorphism' g == h)
    SeeAlso
        "Working with Ext"
        (homomorphism, Matrix)
        (homomorphism', ComplexMap)
        (Hom, Complex, Complex)
        (randomComplexMap, Complex, Complex)
///
*-


doc ///
   Key
     (dual, ZZdFactorization)
     (dual, ZZdFactorization, Symbol)
     (dual, ZZdFactorization, RingElement)
   Headline
     make the dual of a ZZ/d-graded factorization
   Usage
     dual C
   Inputs
     C:ZZdFactorization
   Outputs
     :ZZdFactorization
   Description
    Text
      The dual of a ZZ/d-graded factorization $C$ is by definition $Hom(C, R)$, where $R$ is the ring of $C$.
      If $C$ is a factorization of $f$, then the dual of $C$ is a factorization of $-f$.
    Example
      S = ZZ/101[a..d];
      f = a^2+b^2+c^2+d^2;
      R = S/(f)
      C1 = tailMF ideal vars R 
      C2 = dual C1
      isdFactorization C2
      potential C1
      potential C2
    Text
      The double dual of a ZZ/2-graded factorization is isomorphic to the original factorization.
      If the period of the factorization is $>2$, then one should adjoin a root of unity to the underlying
      ring or factorization. This can be done in a few different ways:
    Example
      Q = ZZ/101[a..c];
      f = a^3+b^3+c^3
      F = linearMF(f,t) --this syntax automatically adjoins a root
      Fd = dual F
      Fd.dd
      potential F
      potential dual F
      F' = ZZdfactorization {a,a,a} --no root of unity in this case
      Ft = adjoinRoot(F',t)
      dual Ft
      Fd' = dual(F', t) --this syntax adjoins the root for the user
      Fd'.dd
   SeeAlso
     (Hom, ZZdFactorization, ZZdFactorization)
     (dual, Module)
///

doc ///
    Key
        (symbol SPACE, RingMap, ZZdFactorization)
    Headline
        apply a ring map
    Usage
        phi C
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        C:ZZdFactorization
            over the ring $R$
    Outputs
        :ZZdFactorization
            over the ring $S$
    Description
        Text
            We illustrate the image of a ZZ/d-graded factorization under a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            Rf = R/(x^3+y^3+z^3)
            C = tailMF ideal vars Rf
            D = phi C
	    isdFactorization D
	    phi(potential C) == potential D
            dd^D
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the factorizations.
        Example
            R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            phi = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = koszulMF({a,b,c,d}, a^2+b^2+c^2+d^2)
            D = phi C
            isdFactorization D
	    potential D == phi(potential C)
    Caveat
        Every term in the ZZ/d-graded factorization must be free or a submodule of a free module.
        Otherwise, use @TO (tensor, RingMap, ZZdFactorization)@.
    SeeAlso
        (symbol SPACE, RingMap, ZZdFactorizationMap)
        (symbol **, RingMap, ZZdFactorization)
///

doc ///
    Key
        (symbol**, RingMap, ZZdFactorization)
        (symbol**, ZZdFactorization, RingMap)
        (tensor, RingMap, ZZdFactorization)
        (tensor, ZZdFactorization, RingMap)
        (symbol**, ZZdFactorization, Ring)
        (symbol**, Ring, ZZdFactorization)
    Headline
        tensor a ZZ/d-graded factorization along a ring map
    Usage
        phi ** C
        tensor(phi, C)
        S ** C
        C ** S
    Inputs
        phi:RingMap
            whose source is a ring $R$ and whose target is a ring $S$
        C:ZZdFactorization
            over the ring $R$
    Outputs
        :ZZdFactorization
            over the ring $S$
    Description
        Text
            These methods implement the base change of rings.  As input, one can either
            give a ring map $\phi$, or the ring $S$ (when there is a canonical map
                from $R$ to $S$).
        Text
            We illustrate the tensor product of a ZZ/d-graded factorization along a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            Rf = R/(x^3+y^3+z^3)
            C = tailMF ideal vars Rf
            D = phi**C
	    isdFactorization D
	    phi(potential C) == potential D
            dd^D
        Text
            If a ring is used rather than a ring map, then the implicit
            map from the underlying ring of the complex to the given ring
            is used.
        Example
	    use R;
            A = R/(x^2+y^2+z^2);
            C ** A
            assert(map(A,R) ** C == C ** A)
        Text
            The commutativity of tensor product is witnessed as follows.
        Example
            assert(D == C ** phi)
            assert(C ** A == A ** C)
        Text
            When the modules in the factorization are not free modules,
            this is different than the image of a factorization 
            under a ring map.
        Example
            use R
            C' = C ** coker matrix{{x^2+y^2+z^2}} 
            D1 = phi C
            isdFactorization D1
            D2 = phi ** C'
            isdFactorization D2
            prune D1
            prune D2
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the factorization.
        Example
            R = ZZ/101[a..d];
            S = ZZ/101[s,t];
            f = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = koszulMF({a,b,c,d}, a^2+b^2+c^2+d^2)
            D = f ** C
            isdFactorization D
	    potential D == f(potential C)
            D == f C
            C1 = Hom(C, image vars R)
            D1 = f ** C1
            isdFactorization C1
	    isdFactorization D1
    SeeAlso
        (symbol **, RingMap, ZZdFactorizationMap)
        (symbol SPACE, RingMap, ZZdFactorization)
///



doc ///
    Key
        (minimalPresentation, ZZdFactorization)
        (prune, ZZdFactorization)
        (prune, ZZdFactorizationMap)
        (minimalPresentation, ZZdFactorizationMap)
    Headline
        minimal presentation of all terms in a ZZ/d-graded factorization
    Usage
        D = minimalPresentation C
        D = prune C
        h = minimalPresentation f
        h = prune f
    Inputs
        C:ZZdFactorization
            or $f$ @ofClass ZZdFactorizationMap@
        Exclude => 
            unused
    Outputs
        D:ZZdFactorization
            isomorphic to the input, where each term is replaced
            by a minimally presented model, or $h$ @ofClass ZZdFactorizationMap@
            where the source and target are minimally presented
    Consequences
        Item
            The isomorphism $g : D \to C$ is available as 
            @TT "g = D.cache.pruningMap"@.  The inverse isomorphism
            can be obtained as @TT "g^-1"@
    Description
        Text
            This is frequently useful to make the output of certain
            operations readable or understandable.  This operation
            is functorial, applying both to ZZ/d-graded factorizations and factorization maps.
        Text
            In particular, homology often needs to be pruned to
            be understood.  For instance, this is useful 
            for recognizing when terms given by subquotient modules 
            are actually zero.
	    One can directly obtain the pruning map by accessing the cached {\tt pruningMap} key.
        Example
            S = ZZ/101[x,y,z,w];
            F = koszulMF({x,y,z,w}, y*w - x*z);
	    E = Hom(F,F)
            Ed = dual E
            C = HH E
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isFactorizationMorphism g
            assert (target g == C)
            assert (source g == D)
            g^-1
            assert(g*g^-1 == 1 and g^-1*g == 1)
   SeeAlso
       "Making ZZdFactorizations"
       (minimalPresentation, Module)
       randomFactorizationMap
       isFactorizationMorphism
///

-*
doc ///
    Key
        (sum, ZZdFactorizationMap)
    Headline
        make the direct sum of all terms
    Usage
        sum C
        sum f
    Inputs
        C:ZZdFactorization
            or {\tt f}, @ofClass ZZdFactorizationMap@
    Outputs
        :Module
            or @ofClass Matrix@, if the input is a ZZ/d-graded factorization map
    Description
        Text
            This is the forgetful functor from the
            category of chain complexes to the category of modules.
            A chain complex $C$ is sent to the direct sum 
            $\bigoplus_i C_i$ of its terms.
            A map of chain complexes $f \colon C \to D$ is sent to the
            direct sum $\bigoplus_i f_i \colon \bigoplus_i C_i \to \bigoplus_i D_i$.
        Example
            S = ZZ/101[a,b,c];
            C = koszulComplex {a,b,c}
            sum C
            assert(rank sum C == 2^3)
        Example
            f = randomFactorizationMap(C, C, InternalDegree => 1, Cycle => true)
            g = sum f
            assert(g^2 === sum f^2)
            assert(target g === sum target f)
            assert(source g === sum source f)
            h = sum dd^C
            assert(h^2 == 0)
    SeeAlso
        "Basic invariants and properties"
        (directSum, ZZdFactorization)
        randomFactorizationMap
///
*-
 




///
    Key
        (higherHomotopyFactorization, List, Complex)
	(higherHomotopyFactorization, RingElement, Complex)
    Headline
        Construct the matrix factorization induced by a system of higher homotopies
    Usage
        higherHomotopyFactorization(L,C)
	higherHomotopyFactorization(f,C)
    Inputs
        C:Complex
        L:List
	    A list of ring elements annihilating the homology of C
	f:RingElement
	    A ring element annihilating the homology of C
    Outputs
        :ZZdFactorization
	    A matrix factorization of f_1t_1 + ... + f_n t_n if L = {f_1,...,f_n} or a matrix factorization of f
	    induced by a system of higher homotopies with respect to these polynomials.
    Description
        Text
        Example
    Caveat
    SeeAlso
///

///
    Key
        (toBranchedCover, ZZdFactorization, Symbol)
	(toBranchedCover, ZZdFactorization, RingElement)
    Headline
        Convert a ZZ/d-graded factorization of a ring element f into a module over the
	d-fold branched cover of f
    Usage
        toBranchedCover(C,z)
    Inputs
        C:ZZdFactorization
	z:Symbol or RingElement
	    this specifies the name of the variable to use in the branched cover
    Outputs
        M:Module
	    A maximal Cohen-Macaulay module over the d-fold branched cover hypersurface
    Description
        Text
        Example
    Caveat
        In order to guarantee that this function behaves well with respect to pushing forward, one should
	give the variables of the ambient ring degree 0.
    SeeAlso
///

///
    Key
        (zeroOutDegrees,Ring)
	(zeroOutDegrees,ZZdFactorization)
    Headline
        Redefine a ring or ZZ/d-graded factorization so that all variables have degree 0
    Usage
        zeroOutDegrees(R)
	zeroOutDegrees(C)
    Inputs
        R:Ring
	C:ZZdFactorization
    Outputs
        :Ring
	    A ring that is isomorphic to the input ring, but with the variables viewed as having degree 0
	:ZZdFactorization
	    A ZZ/d-graded factorization that is isomorphic to the original, but viewed as over a ring
	    where all variables have degree 0
    Description
        This is a helper function for converting ZZ/d-graded factorizations into maximal Cohen-Macaulay modules over d-fold branched coverings,
	since functions such as PushFwd work better when the variables of the original ring are viewed as having degree 0.
        Text
        Example
    Caveat
    SeeAlso
///


///
    Key
        (degreeSorter,ZZ,Module)
	(degreeSorter,ZZ,ZZ,Module)
	(degreeSorter,ZZ,Matrix)
	(degreeSorter,ZZ,ZZ,Matrix)
    Headline
        Reorder the homogeneous basis elements of a free module or the rank/source of a matrix in terms of
	the congruence classes of their degrees.
    Usage
        degreeSorter(d,M)
	degreeSorter(d,offset,M)
    Inputs
        d:ZZ
	    the integer determining the congruence classes that the degrees will be ordered with respect to
	offset:ZZ
	    an offset value
	M:Module or Matrix
    Outputs
        L:List
	    if the input M is a module, then the output is the list of positions, indicating an ordering of 
	    the homogeneous basis elements grouped according to the congruence class of their degrees modulo d
	N:Matrix
	     if the input M is a matrix, then the output is the sae matrix M but with rows/columns reordered
	     to respect the ordering of the basis elements according to the congruence class of their degrees modulo d
    Description
        Text
        Example
    Caveat
    SeeAlso
///

///
    Key
        (branchedToMF,Module)
	(branchedToMF,Module,Ring)
	(branchedToMF,Matrix)
	(branchedToMF,Matrix,Ring)
    Headline
        Convert a maximal Cohen-Macaulay module over a d-fold branched covering into a matrix over
	the base ring which yields a well-defined ZZ/d-graded factorization
    Usage
        branchedToMF(M)
	branchedToMF(M,Q)
    Inputs
        M:Module or Matrix
	    a maximal Cohen-Macaulay module over a d-fold branched cover, or a matrix whose cokernel
	    yields a maximal Cohen-Macaulay module over a d-fold branched cover
	Q:Ring
	    optional argument to specify the base ring of the d-fold branched covering
    Outputs
        N:Matrix
	    A matrix over the base ring that yields a well-defined ZZ/d-graded factorization
    Description
        Text
        Example
    Caveat
    SeeAlso
///




doc ///
    Key
        "Making maps between ZZ/d-graded factorizations"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (map, ZZdFactorization, ZZdFactorization, HashTable),
                TO (map, ZZdFactorization, ZZdFactorization, ZZ),
                TO (map, ZZdFactorization, ZZdFactorization, ZZdFactorizationMap),
                TO (id, ZZdFactorization),
                TO "differential of a ZZ/d-graded factorization",
                TO (symbol SPACE, ZZdFactorizationMap, Array),
                TO (isWellDefined, ZZdFactorizationMap)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new ZZ/d-graded factorization maps"@
        Text 
            @UL {
                TO (homology, ZZdFactorizationMap),
                TO (symbol**, ZZdFactorization, Matrix),
                TO (nullHomotopy, ZZdFactorizationMap)
           }@
    	Text
    	    @SUBSECTION "Canonical maps between ZZ/d-graded factorizations"@
        Text
            Some ZZ/d-graded factorizations come with canonical maps.
            To access the ZZ/d-graded factorization map, 
            one uses 
	    @TO (inducedMap, ZZdFactorization, ZZdFactorization)@.
	Text
    	    @UL {
                TO (kernel, ZZdFactorizationMap),
                TO (cokernel, ZZdFactorizationMap),
                TO (image, ZZdFactorizationMap),
                TO (coimage, ZZdFactorizationMap),
                TO (cone, ZZdFactorizationMap),
                TO (inducedMap, ZZdFactorization, ZZdFactorization)
            }@
    	Text
    	    @SUBSECTION "Random maps of ZZ/d-graded factorizations"@
        Text
            The method @TO (randomFactorizationMap, ZZdFactorization, ZZdFactorization)@
            allows one to construct random ZZ/d-graded factorization maps,
            random morphisms between factorizations, and random
            null homotopies between factorizaions.
	Text
    	    @UL {
                TO (isCommutative, ZZdFactorizationMap),
                TO (isFactorizationMorphism, ZZdFactorizationMap),
                TO (isNullHomotopic, ZZdFactorizationMap)
            }@
    	Text
    	    @SUBSECTION "Elementary operations on factorization maps"@
        Text
    	    @UL {
                TO "arithmetic with complex maps",
                TO (symbol +, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol |, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol ||, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol ++, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (symbol **, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (Hom, ZZdFactorizationMap, ZZdFactorizationMap),
                TO (dual, ZZdFactorizationMap),
                TO (symbol _, ZZdFactorizationMap, Array),
                TO (symbol ^, ZZdFactorizationMap, Array),
                --TO (part, List, ZZdFactorizationMap),
                TO (symbol SPACE, RingMap, ZZdFactorizationMap),
                TO (symbol **, RingMap, ZZdFactorizationMap)
            }@
    SeeAlso
        "Making ZZdFactorizations"
        "Basic invariants and properties"
///


doc ///
  Key
    ZZdFactorizationMap
  Headline
    the class of all maps between ZZ/d-graded factorizations
  Description
    Text
      @LITERAL ////<script> macros["\\Hom"] = "\\operatorname{Hom}" </script>////@

      A map of ZZ/d-graded factorizations $f \colon C \rightarrow D$ of degree $d$ is a
      sequence of maps $f_i \colon C_i \rightarrow D_{d+i}$.  
      No relationship between the maps $f_i$ and 
      and the differentials of either $C$ or $D$ is assumed.
      
      The set of all maps from $C$ to $D$ form
      the another factorization $\Hom(C,D)$ where $\Hom(C,D)_d$ consists of the
      maps of degree $d$; this is a factorization of the differential of the respective potentials.

      The usual algebraic operations are available: addition,
      subtraction, scalar multiplication, and composition. The
      identity map from a factorization to itself can be produced with
      @TO "id"@. An attempt to add (subtract, or compare) a ring
      element to a factorization map will result in the ring element being
      multiplied by the appropriate identity map.
  SeeAlso
    "Making maps between ZZ/d-graded factorizations"
    ZZdFactorization
///


doc ///
    Key
        (map, ZZdFactorization, ZZdFactorization, HashTable)
    Headline
        make a map of ZZ/d-graded factorizations
    Usage
        f = map(D, C, H)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
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
        f:ZZdFactorizationMap
    Description
        Text
            A map of ZZ/d-graded factorizations $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and 
            and the differentials of either $C$ or $D$ is assumed.
            
            We construct a map of factorizations by specifying the
            individual maps between the terms.
        Example
	    S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    use S
            H = hashTable { 2 => map(C_0, D_0, matrix {{0, 0, -c, -b, 0, -c, 0, 0}, {0, 0, 0, 0, -c, 0, 0, 0}, {0, 0,
                     0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, b, 0, 0, 0}}),
                1 => map(C_1, D_1, matrix {{0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, -c, b, c, 0, 0}, {-c, 0, 0,
                     0, 0, 0, 0, 0}, {b, 0, 0, 0, 0, 0, 0, 0}})
                }
            f = map(C, D, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isFactorizationMorphism f
        Text
            The keys in the hash table index the terms in the source of the
            map.  If a key is missing, that map is taken to be the zero map.
            We illustrate by constructing a map of factorizations
            having nonzero degree, and omitting one key in the hash table.
        Example
            E = D[1]
            g = map(E, C, hashTable {}, Degree => 1)
            assert isWellDefined g
            assert isHomogeneous g
            assert(degree g == 1)
            assert not isFactorizationMorphism g --since the map has nonzero degree
            assert isCommutative g
            assert(source g == C)
            assert(target g == E)
        Text
            This is the primary constructor used by all of the more user friendly
            methods for constructing a chain complex.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, ZZdFactorizationMap)@.
    SeeAlso
        ZZdFactorizationMap
        (isWellDefined, ZZdFactorizationMap)
        (degree, ZZdFactorizationMap)
        (isFactorizationMorphism, ZZdFactorizationMap)
        (isCommutative, ZZdFactorizationMap)
        (source, ZZdFactorizationMap)
        (target, ZZdFactorizationMap)
///

-*
doc ///
    Key
        (map, ZZdFactorization, ZZdFactorization, List)
    Headline
        make a map of ZZ/d-graded factorizations
    Usage
        f = map(D, C, L)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
        L:List
            consisting of either matrices, or lists of maps of factorizations
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ZZdFactorizationMap
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
*-


doc ///
    Key
        (map, ZZdFactorization, ZZdFactorization, ZZ)
    Headline
        make the zero map or identity between ZZ/d-graded factorizations
    Usage
        f = map(D, C, 0)
        f = map(C, C, 1)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
        0:ZZ
            or 1
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ZZdFactorizationMap
            the zero map from $C$ to $D$ or the identity map from $C$ to $C$
    Description
        Text
            A map of ZZ/d-graded factorizations $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            
            We construct the zero map between two
            factorizations.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = map(D, C, 0)
            assert isWellDefined f
            assert isFactorizationMorphism f
            g = map(C, C, 0, Degree => 13)
            assert isWellDefined g
            assert(degree g == 13)
            assert not isFactorizationMorphism g
            assert isCommutative g
            assert isHomogeneous g
            assert(source g == C)
            assert(target g == C)
        Text
            Using this function to create the identity map
            is the same as using @TO (id, ZZdFactorization)@.
        Example
            assert(map(C, C, 1) === id_C)
   SeeAlso
        ZZdFactorizationMap
        (isWellDefined, ZZdFactorizationMap)
        (degree, ZZdFactorizationMap)
        (isCommutative, ZZdFactorizationMap)
        (source, ZZdFactorizationMap)
        (target, ZZdFactorizationMap)
        (id, ZZdFactorization)
///

-*
doc ///
    Key
        (map, ZZdFactorization, ZZdFactorization, Function)
    Headline
        make a map of ZZ/d-graded factorizations
    Usage
        f = map(D, C, fcn)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
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
        f:ZZdFactorizationMap
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
*-

doc ///
    Key
        (map, ZZdFactorization, ZZdFactorization, ZZdFactorizationMap)
    Headline
        make a new map of ZZ/d-graded factorizations from an existing one (for inducing maps)
    Usage
        g = map(D, C, f)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
        f:ZZdFactorizationMap
            regarded as providing matrices which induce maps between the terms of $C$ and $D$
        Degree => ZZ
            the degree $d$ of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        g:ZZdFactorizationMap
    Description
        Text
            A map of factorizations $f : C' \rightarrow D'$ is a
            sequence of maps $f_i : C'_i \rightarrow D'_{d'+i}$.  
            The new map $g : C \rightarrow D$ is the sequence of maps $g_i : C_i \rightarrow D_{d+i}$
            induced by the matrix of $f_i$.
            
            One use for this function is to get the new map of ZZ/d-graded factorizations obtained by shifting 
            the source or target of an existing chain map.  For example, one can regard the differential
            on a factorization can be regarded as a map of degree zero between shifted factorizations.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
	    D = ZZdfactorization(C, Base => 1)
            f = map(D, C, dd^C, Degree => 0)
            assert isWellDefined f
            assert(degree f == 0)
            assert isCommutative f
            assert isFactorizationMorphism f
            assert not isFactorizationMorphism dd^C
    SeeAlso
        ZZdFactorizationMap
        (isWellDefined, ZZdFactorizationMap)
        (degree, ZZdFactorizationMap)
        (isFactorizationMorphism, ZZdFactorizationMap)
        (isCommutative, ZZdFactorizationMap)
        (symbol SPACE, ZZdFactorization, Array)
///

doc ///
    Key
        (id, ZZdFactorization)
    Headline
        the identity map of a ZZ/d-graded factorization
    Usage
        f = id_C
    Inputs
        C:ZZdFactorization
    Outputs
        f:ZZdFactorizationMap
          the identity map from $C$ to itself
    Description
        Text
            The collection of ZZ/d-graded factorizations together with ZZ/d-graded factorization morphisms
            forms a category.  In particular, every ZZ/d-graded factorization has an identity map.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = id_C
            assert isWellDefined f
            assert isFactorizationMorphism f
	    g = id_D
	    assert isWellDefined g
            assert isFactorizationMorphism g
    SeeAlso
        (map, ZZdFactorization, ZZdFactorization, ZZ)
        (isWellDefined, ZZdFactorizationMap)
        (isFactorizationMorphism, ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorization)
///

doc /// 
    Key
        (isWellDefined, ZZdFactorizationMap)
    Headline
        whether a map of ZZ/d-graded factorizations is well-defined
    Usage
        isWellDefined f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        :Boolean
            that is true when {\tt f} determines a well defined ZZ/d-graded factorization map
    Description
        Text
            A map of factorizations $f : C \to D$ of degree $d$ is a sequence of
            maps $f_i : C_i \to D_{d+i}$.  No relationship is required between
            these maps and the differentials in the source and target.

            This routine checks that $C$ and $D$ are well-defined
            factorizations, and that, for each $f_i$, the source and
            target equal $C_i$ and $D_{d+i}$, respectively.  If the
            variable {\tt debugLevel} is set to a value greater than
            zero, then information about the nature of any failure is
            displayed.
        Text
            Unlike the @TO (isWellDefined, ZZdFactorization)@,
            the basic constructors for ZZ/d-graded factorization maps are all but
            assured to be well defined. The only case that could cause
            a problem is if one constructs the source or target
            complex, and those are not well defined.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    use S
            H = hashTable { 2 => map(C_0, D_0, matrix {{0, 0, -c, -b, 0, -c, 0, 0}, {0, 0, 0, 0, -c, 0, 0, 0}, {0, 0,
                     0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, b, 0, 0, 0}}),
                1 => map(C_1, D_1, matrix {{0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, -c, b, c, 0, 0}, {-c, 0, 0,
                     0, 0, 0, 0, 0}, {b, 0, 0, 0, 0, 0, 0, 0}})
                }
            f = map(C, D, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isFactorizationMorphism f
        Text
            We construct two random maps of factorizations,
            and check to see that, as should be the case, 
            both are well defined.
        Example
            g = randomFactorizationMap(C,D)
            assert isWellDefined g
            assert not isCommutative g
        Example
            h = randomFactorizationMap(C,D, Cycle => true)
            assert isWellDefined h
            assert isFactorizationMorphism h
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
        (isWellDefined, ZZdFactorization)
        (isCommutative, ZZdFactorizationMap)
        (map, ZZdFactorization, ZZdFactorization, HashTable)
///




doc ///
    Key
        (source, ZZdFactorizationMap)
    Headline
        get the source of a map of ZZ/d-graded factorizations
    Usage
        C = source f
    Inputs
      f:ZZdFactorizationMap
    Outputs
      C:ZZdFactorization
    Description
        Text
            Given a ZZ/d-graded factorization map $f : C \to D$
            this method returns the ZZ/d-graded factorization $C$.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C, D, Cycle=>true)
            source f
            assert isWellDefined f
            assert isFactorizationMorphism f
            assert(source f == D)
            assert(target f == C)
        Text
            The differential in a factorization is a map of factorizations.
        Example
            use S
	    F = randomTailMF(a^3 + b^3 + c^3)
            source dd^F == F
            target dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making ZZdFactorizations"
       (target, ZZdFactorizationMap)
       (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (target, ZZdFactorizationMap)
    Headline
        get the target of a map of ZZ/d-graded factorizations
    Usage
        C = target f
    Inputs
      f:ZZdFactorizationMap
    Outputs
      C:ZZdFactorization
    Description
        Text
            Given a ZZ/d-graded factorization map $f : C \to D$
            this method returns the ZZ/d-graded factorization $D$.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C, D, Cycle=>true)
            source f
            assert isWellDefined f
            assert isFactorizationMorphism f
            assert(source f == D)
            assert(target f == C)
        Text
            The differential in a factorization is a map of factorizations.
        Example
            use S
	    F = randomTailMF(a^3 + b^3 + c^3)
            source dd^F == F
            target dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making ZZdFactorizations"
       (source, ZZdFactorizationMap)
       (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (degree, ZZdFactorizationMap)
    Headline
        get the degree of a map of ZZ/d-graded factorizations
    Usage
        degree f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        :ZZ
    Description
        Text
            A ZZ/d-graded factorization map $f : C \to D$ of degree $d$ is a sequence of
            of maps $f_i : C_i \to D_{i+d}$.
            This method returns $d$.
        Text
            The degree of the differential of a factorization is always -1.
        Example
            S = ZZ/101[a,b,c];
	    F = randomTailMF(a^3 + b^3 + c^3)
            assert(degree dd^F == -1)
        Example
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C, D, Cycle=>true, Degree => -1)
            assert(degree f == -1)
   SeeAlso
       "Basic invariants and properties"
       (source, ZZdFactorizationMap)
       (target, ZZdFactorizationMap)
       (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///


doc ///
    Key
        (isHomogeneous, ZZdFactorizationMap)
    Headline
         whether a map of ZZ/d-graded factorizations is homogeneous
    Usage
         isHomogeneous f
    Inputs
         f:ZZdFactorizationMap
    Outputs
         :Boolean
             that is true when $f_i$ is homogeneous, for all $i$
    Description
        Text
            A map of factorizations $f \colon C \to D$ is homogeneous
            (graded) if its underlying ring is graded, and all the
            component maps $f_i \colon C_i \to D_{d+i}$ are graded of
            degree zero, where $f$ has degree $d$.
        Example
	    S = ZZ/101[a,b,c];
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            assert isHomogeneous dd^C
            f = randomFactorizationMap(C, C, Degree => -1)
            f = randomFactorizationMap(C, C, InternalDegree => 2)
        Text
            The image of a homogeneous factorization under a nonhomogeneous
	    ring map may fail to be homogeneous.
        Example
	    use S;
            phi = map(S, S, {1,b,c})
            D = phi C
            dd^D
            assert not isHomogeneous dd^D
            g = randomFactorizationMap(D, D, InternalDegree => 1)
    SeeAlso
        "Basic invariants and properties"
        isHomogeneous
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///



doc ///
    Key
        (symbol _, ZZdFactorizationMap, ZZ)
    Headline
        access individual matrices in a ZZ/d-graded factorization map
    Usage
        f_i
    Inputs
        f:ZZdFactorizationMap
        i:ZZ
            the homological index
    Outputs
        :Matrix
            the {\tt i}-th map
    Description
        Text
            A ZZ/d-graded factorization map $f : C \to D$ of degree $d$ is a sequence of maps $f_i : C_i \to D_{i+d}$.
            This method allows one to access the individual $f_i$.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C, D, Cycle=>true, InternalDegree => 1)
            f_1
            f_0
        Text
            Indices that are outside of the period are taken modulo the period.
        Example
            period f
            f_-1
            f_3
            f_4
    SeeAlso
        (symbol_, ZZdFactorization, ZZ)
        (period, ZZdFactorizationMap)
///



doc ///
    Key
        (components, ZZdFactorizationMap)
    Headline
        list the components of a direct sum
    Usage
        components f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        :List
            the component maps of a direct sum of maps of ZZ/d-graded factorizations
    Description
        Text
            A map of ZZ/d-graded factorizations stores its component maps.
        Example
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            g1 = id_C
            g2 = randomFactorizationMap(C[1], C[2], Boundary => true)
            f = g1 ++ g2
            assert isWellDefined f
            L = components f
            L_0 === g1
            L_1 === g2
            indices f
            f' = (chicken => g1) ++ (nuggets => g2)
            components f'
            indices f'
        Text
            The names of the components are called indices, and are
            used to access the relevant inclusion and projection maps.
        Example
            f'_[chicken]
            f'^[nuggets]
            f^[0]
            f_[0]
    SeeAlso
        (directSum, ZZdFactorizationMap)
        (components, ZZdFactorization)
        indices
        (symbol_, ZZdFactorizationMap, Array)
        (symbol^, ZZdFactorizationMap, Array)
///

doc ///
  Key
    (symbol*, ZZdFactorizationMap, ZZdFactorizationMap)
  Headline
    composition of homomorphisms of ZZ/d-graded factorizations
  Usage
    f = h * g
  Inputs
    h:ZZdFactorizationMap
      if a ring element or integer, then we multiply the ring element
      by the appropriate identity map
    g:ZZdFactorizationMap
  Outputs
    f:ZZdFactorizationMap
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
      C = koszulMF({a,b,c}, a^3 + b^3 + c^3)
      3 * dd^C
      0 * dd^C
      dd^C * dd^C
  SeeAlso
      "Making maps between factorizations"
      "arithmetic with ZZ/d-graded factorization maps"
///

doc ///
    Key
        (symbol ^, ZZdFactorizationMap, ZZ)
    Headline
        the n-fold composition
    Usage
        f^n
    Inputs
        f:ZZdFactorizationMap
            whose source and target are the same ZZ/d-graded factorization
        n:ZZ
    Outputs
        :ZZdFactorizationMap
            the composition of $f$ with itself $n$ times.
    Description
        Text
            A ZZ/d-graded factorization map $f : C \to C$ can be composed with itself.
            This method produces these new maps of ZZ/d-graded factorizations.
        Text
            The differential on a ZZ/d-graded factorization should compose with itself d times to give a  
            scalar multiple of the identity map.
        Example
            S = ZZ/101[a..c];
            C = koszulMF({a,b,c}, a^3 + b^3 + c^3)
            f = dd^C
            f^2
            assert(source f == target f)
            assert(degree f == -1)
            assert(degree f^2 == -2)
	    K' = linearMF(a^3+b^3+c^3, t)
	    g = K'.dd
	    g^3
        Example
            g = randomFactorizationMap(C, C, Degree => -1)
            g^2
            g^3
        Text
            The zero-th power returns the identity map
        Example
            f^0 == id_C
            g^0 == id_C
        Text
            When $n$ is negative, the result is the $n$-fold power
            of the inverse complex map, if it exists.
        Example
            h = randomFactorizationMap(C, C)
            h^-1
            assert(h * h^-1 == id_C)
            h^-4
            assert(h^-4 * h^4 == id_C)
    SeeAlso
        (symbol^, Matrix, ZZ)
        (symbol^, ZZdFactorization, ZZ)
///

doc ///
   Key
     (symbol ==, ZZdFactorizationMap, ZZdFactorizationMap)
     (symbol ==, ZZdFactorizationMap, ZZ)
     (symbol ==, ZZ, ZZdFactorizationMap)
     (symbol ==, ZZdFactorizationMap, RingElement)
     (symbol ==, RingElement, ZZdFactorizationMap)
   Headline
     whether two ZZ/d-graded factorization maps are equal
   Usage
     f == g
     f == 0
     f == 1
     f == r
   Inputs
     f:ZZdFactorizationMap
       or 0, or 1.
     g:ZZdFactorizationMap
       or 0, or 1, or any RingElement.
   Outputs
     :Boolean
       that is true when {\tt f} and {\tt g} are equal, or when {\tt f} is equal to the 0 map,
       the identity map, or a scalar multiple of the identity map.
   Description
     Text
       Two ZZ/d-graded factorization maps are equal if they have the same source,
       the same target, and $f_i = g_i$ for all $i$.
     Example
       S = ZZ/101[a,b,c];
       R = S/(a^2+b^2+c^2);
       m = ideal vars R
       C = tailMF m
       D = tailMF (m^2)
       f = id_C
       assert(f == 1)
       f === id_C[-1][1]
       f == id_C[-1][1]
     Text
       A factorization map is equal to zero if all the maps are zero.
       This could require computation to determine if something that
       is superficially not zero is in fact zero.
     Example
       assert(0 * id_C == 0)
     Example
       use S;
       E = koszulMF({a,b,c}, a^3 + b^3 + c^3)
       g = randomFactorizationMap(E, E)
       h = inducedMap(coker g, target g)
       assert(prune h == 0)
     Text
       Testing whether a map is equal to 1 is a shorthand for determining
       if the complex map is the identity.
       Although the matrices may appear to be the identity, the map is not the
       identity when the source and target are not equal.
     Example
       g = randomFactorizationMap(C, C, InternalDegree=>1, Cycle=>true)
       h = inducedMap(coker g, target g)
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
     (symbol ==, ZZdFactorization, ZZdFactorization)
     (symbol SPACE, ZZdFactorizationMap, Array)
     randomFactorizationMap
     (prune, ZZdFactorization)
///




doc ///
  Key
    (isCommutative, ZZdFactorizationMap)
  Headline
    whether a ZZ/d-graded factorization map commutes with the differentials
  Usage
    isCommutative f
  Inputs
    f:ZZdFactorizationMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the differentials
  Description
    Text
      For a ZZ/d-graded factorization map $f : C \to D$ of degree $d$, this method
      checks whether, for all $i$, we have
      $dd^D_{i+d} * f_i =  (f_{i-1} * dd^C_i)$.
    Text
      We first construct a random factorization map which commutes with the differential.
    Example
      S = ZZ/101[a,b];
      C = koszulMF({a,b}, a^3 + b^3)
      D = C ** C
      f1 = randomFactorizationMap(C,C, Boundary => true)
      f2 = randomFactorizationMap(D,D, Cycle => true, InternalDegree => 1, Degree => 1)
      isCommutative f1
      isCommutative f2
      assert(degree f1 == 0)
      assert(source f1 == C and target f1 == C)
    Text
      We can verify directly that maps commute with the differentials:
    Example
      f3 = randomFactorizationMap(C, C, Cycle => true, Degree=>1, InternalDegree => 1)
      isCommutative f3
      assert(degree f3 == 1)
      part1 = dd^C_3 * f3_2
      part2 = f3_1 * dd^C_2
      assert(part1 + part2 == 0)
    Text
      If the @TO "debugLevel"@ is greater than zero, then
      the location of the first failure of commutativity is displayed.
    Example
      f4 = randomFactorizationMap(D, C)
      isCommutative f4
      debugLevel = 1
      isCommutative f4
  SeeAlso
    isFactorizationMorphism
    randomFactorizationMap
///


doc ///
    Key
        (Hom, ZZdFactorizationMap, ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorizationMap)
        (Hom, ZZdFactorization, Matrix)
        (Hom, ZZdFactorizationMap, Module)
        (Hom, ZZdFactorizationMap, Matrix)
        (Hom, ZZdFactorizationMap, ZZdFactorization)
        (Hom, ZZdFactorizationMap, Ring)
        (Hom, Matrix, ZZdFactorization)
        (Hom, Matrix, ZZdFactorizationMap)
        (Hom, Module, ZZdFactorizationMap)
        (Hom, Ring, ZZdFactorizationMap)
    Headline
        the map of factorizations between Hom complexes
    Usage
        h = Hom(f,g)
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of ZZ/d-graded factorizations induces the map
            $h = Hom(f,g) : Hom(D,E) \to Hom(C,F)$ defined by $\phi \mapsto g \phi f$.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C,D)
            E = (dual C)
            F = (dual D)
            g = randomFactorizationMap(F,E)
            h = Hom(f,g)
            assert isWellDefined h
        Text
            We illustrate the defining property of the map $h$ on a random element $\phi$
            in degree zero.
        Example
            e = randomFactorizationMap(source h, ZZdfactorization(S^1,2))
            phi = homomorphism e
            e == homomorphism'(phi)
        Text
            If either of the arguments is a @TO "ZZdFactorization"@, that argument is
            understood to be the identity map on that factorization.
        Example
            assert(Hom(f, C) == Hom(f, id_C))
            assert(Hom(C, f) == Hom(id_C, f))
        Text
            XXX write this text after writing doc for homomorphism and homomorphism'.
        Example
            e = randomFactorizationMap(source h, ZZdfactorization(S^1, 2, Base => 1))
            phi = homomorphism e
            assert(degree phi == 1)
            psi = homomorphism'(g * phi * f)
            i = map(ZZdfactorization(S^1, 2), source e, id_(source e), Degree => 1)
            assert((degree h, degree e, degree psi, degree i) === (0, 0, 1, 1))
    SeeAlso
        (homomorphism, ZZdFactorizationMap)
        (homomorphism', ZZdFactorizationMap)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (Hom, ZZdFactorization, ZZdFactorization)
///



doc ///
    Key
        (Hom, ComplexMap, ZZdFactorizationMap)
	(Hom, ComplexMap, ZZdFactorizationMap, Symbol)
	(Hom, ComplexMap, ZZdFactorizationMap, RingElement)
        (Hom, ZZdFactorizationMap, ComplexMap)
	(Hom, ZZdFactorizationMap, ComplexMap, Symbol)
	(Hom, ZZdFactorizationMap, ComplexMap, RingElement)
    Headline
        the map of factorizations between Hom factorizations formed by first folding the relevant complex map
    Usage
        h = Hom(f,g)
    Inputs
        f:ComplexMap
	  or @ofClass ZZdFactorizationMap@
        g:ZZdFactorizationMap
	  or @ofClass ComplexMap@
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of ZZ/d-graded factorizations induces the map
            $h = Hom(f,g) : Hom(D,E) \to Hom(C,F)$ defined by $\phi \mapsto g \phi f$.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    K = koszulComplex vars S
	    H = Hom(K, C)
	    potential H
	    H' = Hom(D, K)
	    potential H'
	    isZZdComplex (H**H')
    SeeAlso
        (homomorphism, ZZdFactorizationMap)
        (homomorphism', ZZdFactorizationMap)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (Hom, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (Hom, ZZdFactorizationMap, ZZdFactorizationMap, Symbol)
	(Hom, ZZdFactorizationMap, ZZdFactorizationMap, RingElement)
        (Hom, ZZdFactorization, ZZdFactorizationMap, Symbol)
	(Hom, ZZdFactorization, ZZdFactorizationMap, RingElement)
        (Hom, ZZdFactorization, Matrix, Symbol)
	(Hom, ZZdFactorization, Matrix, RingElement)
        (Hom, ZZdFactorizationMap, Module, Symbol)
	(Hom, ZZdFactorizationMap, Module, RingElement)
        (Hom, ZZdFactorizationMap, Matrix, Symbol)
	(Hom, ZZdFactorizationMap, Matrix, RingElement)
        (Hom, ZZdFactorizationMap, ZZdFactorization, Symbol)
	(Hom, ZZdFactorizationMap, ZZdFactorization, RingElement)
        (Hom, ZZdFactorizationMap, Ring, Symbol)
	(Hom, ZZdFactorizationMap, Ring, RingElement)
    Headline
        the map of factorizations between Hom complexes
    Usage
        h = Hom(f,g,t)
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
	t:Symbol
	   optional input specifying the symbol used to represent the distinguished root of unity, 
	   required only if the underlying ring does not have a specified root of unity and the period is > 2
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of ZZ/d-graded factorizations induces the map
            $h = Hom(f,g) : Hom(D,E) \to Hom(C,F)$ defined by $\phi \mapsto g \phi f$. When the period
	    of a factorization is greater than $2$, one should adjoin a root of unity in order
	    for the Hom factorization to be defined.
        Example
            S = ZZ/101[a,b];
	    C = linearMF(a^3+b^3, t)
	    C.cache.?rootOfUnity
	    H = Hom(C,C) --no need to adjoin root of unity
	    A = ZZdfactorization {a,a,a}
	    A.cache.?rootOfUnity
	    H' = Hom(A,A,t) --need to adjoin root of unity
        Text
            If one instead wants to define a new factorization over the same ambient ring, use
	    the tensor product instead.
        Example
            St = ring C;
	    At = A**St
	    Ht = Hom(At,At)
	    Ht**H
	    isZZdComplex Ht
	Text
	    The homomorphism and homomorphism' commands are also implemented for factorizations
	    of longer length, as well as @TO randomFactorizationMap@.
	Example
	    e = randomFactorizationMap(C,C, Cycle => true)
            phi = homomorphism' e
        Text
            If either of the arguments is a @TO "ZZdFactorization"@, that argument is
            understood to be the identity map on the factorization.
        Example
            assert(Hom(e, C) == Hom(e, id_C))
            assert(Hom(C, e) == Hom(id_C, e))
    SeeAlso
        (homomorphism, ZZdFactorizationMap)
        (homomorphism', ZZdFactorizationMap)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (Hom, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (homomorphism, ZZ, Matrix, ZZdFactorization)
    Headline
        get the homomorphism from an element of Hom
    Usage
        g = homomorphism(i, f, E)
    Inputs
        i:ZZ
        f:Matrix
            a map of the form $f \colon R^1 \to E_i$
        E:ZZdFactorization
            having the form
            $E = \operatorname{Hom}(C, D)$ for some factorizations $C$ and $D$
    Outputs
        g:ZZdFactorizationMap
            the corresponding map of chain complexes from $C$ to $D$ of degree $i$
    Description
        Text
            An element of the factorization $\operatorname{Hom}(C, D)$ corresponds to a map of 
            factorizations from $C$ to $D$.  Given an element in the $i$-th term, this
            method returns the corresponding map of complexes of degree $i$.
        Text
            As a first example, consider two Koszul complexes $C$ and $D$.
            From a random map $f \colon R^1 \to Hom(C, D)$, we construct 
            the corresponding map of chain complexes $g \colon C \to D$.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            E = Hom(C,D)
            f = random(E_2, S^{-5})
            g = homomorphism(2, f, E)
            assert isWellDefined g
            assert not isCommutative g
        Text
            The map $g \colon C \to D$ corresponding to a random map into $Hom(C,D)$
            does not generally commute with the differentials.  However, if the
            element of $Hom(C,D)$ is a cycle, then the corresponding map does commute.
        Example
            h = randomFactorizationMap(E, ZZdfactorization(S^{-2}, 2), Cycle => true)
            f = h_0
            g = homomorphism(0, f, E)
            assert isWellDefined g
            assert isCommutative g
            assert(degree g === 0)
            assert(source g === C)
            assert(target g === D)
            assert(homomorphism' g == h)
    SeeAlso
        (homomorphism, Matrix)
        (homomorphism', ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorization)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (homomorphism, ZZdFactorizationMap)
    Headline
        get the homomorphism from an element of Hom
    Usage
        g = homomorphism f
    Inputs
        f:ZZdFactorizationMap
            a map of the form $f : R^1 \to Hom(C, D)$, where
            $C$ and $D$ are factorization,
            $Hom(C,D)$ has been previously computed, and $R$ is
            the underlying ring of these complexes
    Outputs
        g:ZZdFactorizationMap
            the corresponding map of chain complexes from $C$ to $D$
    Description
        Text
            As a first example, consider two Koszul complexes $C$ and $D$.
            From a random map $f : R^1 \to Hom(C, D)$, we construct 
            the corresponding map of chain complexes $g : C \to D$.
        Example
	    S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            H = Hom(C,D)
            f = randomFactorizationMap(H, ZZdfactorization( S^{-2}, 2))
            isWellDefined f
            g = homomorphism f
            isWellDefined g
            assert not isCommutative g
        Text
            The map $g : C \to D$ corresponding to a random map into $Hom(C,D)$
            does not generally commute with the differentials.  However, if the
            element of $Hom(C,D)$ is a cycle, then the corresponding map does commute.
        Example
            f = randomFactorizationMap(H, ZZdfactorization( S^{-2}, 2), Cycle => true)
            isWellDefined f
            g = homomorphism f
            isWellDefined g
            assert isCommutative g
            assert(degree g === 0)
            assert(source g === C)
            assert(target g === D)
            assert(homomorphism' g == f)
        Text
            A homomorphism of non-zero degree can be encoded
            in (at least) two ways.
        Example
            f1 = randomFactorizationMap(H, ZZdfactorization( S^1, 2), Degree => 1)
            f2 = map(target f1, (source f1)[1], i -> f1_(i+1))
            assert isWellDefined f2
            g1 = homomorphism f1
            g2 = homomorphism f2
            assert(g1 == g2)
            assert isWellDefined g1
            assert isWellDefined g2
            homomorphism' g1 == f1
            homomorphism' g2 == f1
    SeeAlso
        "Working with Ext"
        (homomorphism, Matrix)
        (homomorphism, ZZ, Matrix, ZZdFactorization)
        (homomorphism', ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorization)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (homomorphism', ZZdFactorizationMap)
    Headline
        get the element of Hom from a map of factorizations
    Usage
        f = homomorphism g
    Inputs
        g:ZZdFactorizationMap
            from $C$ to $D$
    Outputs
        f:ZZdFactorizationMap
            a map of the form $f : R^1 \to Hom(C, D)$, where
            $R$ is the underlying ring of these factorizations
    Description
        Text
            As a first example, consider two matrix factorizations $C$ and $D$.
            From a random map $f : R^1 \to Hom(C, D)$, we construct 
            the corresponding map of ZZ/d-graded factorizations $g : C \to D$.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            g = randomFactorizationMap(D, C, InternalDegree => 2)
            isWellDefined g
            f = homomorphism' g
            isWellDefined f
        Text
            The map $g : C \to D$ corresponding to a random map into $Hom(C,D)$
            does not generally commute with the differentials.  However, if the
            element of $Hom(C,D)$ is a cycle, then the corresponding map does commute.
        Example
            g = randomFactorizationMap(D, C, Cycle => true, InternalDegree => 3)
            isWellDefined g
            f = homomorphism' g
            isWellDefined f
            assert isCommutative g
            assert(degree f === 0)
            assert(source f == ZZdfactorization(S^{-3}, 2))
            assert(target g === D)
            assert(homomorphism f == g)
    SeeAlso
        (homomorphism', Matrix)
        (homomorphism, ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorization)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
///




doc ///
    Key
        (dual, ZZdFactorizationMap)
    Headline
        the dual of a map of ZZ/d-graded factorizations
    Usage
        h = dual f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The map $f : C \to D$ of ZZ/d-graded factorizations over the ring $S$ induces the map
            $h = Hom(f, S^1) : Hom(D, S^1) \to Hom(C,S^1)$ defined by $\phi \mapsto \phi f$.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C,D)
            h = dual f
            assert isWellDefined h
        Text
            This routine is functorial.
        Example
            D' = tailMF (trim m^3)
            f' = randomFactorizationMap(D, D')
            dual(f * f') == dual f' * dual f
    SeeAlso
        (Hom, ZZdFactorizationMap, ZZdFactorizationMap)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (dual, Matrix)
///




		
		

doc ///
    Key
        (dual, ZZdFactorizationMap, RingElement)
	(dual, ZZdFactorizationMap, Symbol)
    Headline
        the dual of a map of ZZ/d-graded factorizations
    Usage
        h = dual(f,t)
    Inputs
        f:ZZdFactorizationMap
	t:RingElement
	     optional input specifying the symbol used to represent the distinguished root of unity, 
	     required only if the underlying ring does not have a specified root of unity and the period is > 2 
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The map $f : C \to D$ of ZZ/d-graded factorizations over the ring $S$ induces the map
            $h = Hom(f, S^1) : Hom(D, S^1) \to Hom(C,S^1)$ defined by $\phi \mapsto \phi f$. If
	    the period of the factorization is >2, then there must be a root of unity adjoined
	    in order for the dual to be defined.

	    The dual is a factorization of the negative of the potential.
        Example
            S = ZZ/101[a..c]
            C = linearMF(a^3+b^3, t)
	    St = ring C;
	    D = (koszulComplex {a,b})**St
            f = randomFactorizationMap(C,D**C, Cycle => true)
            h = dual f
            assert isWellDefined h
        Text
            This routine is functorial.
        Example
            D' = C**D
            f' = randomFactorizationMap(D**C, D')
    SeeAlso
        (Hom, ZZdFactorizationMap, ZZdFactorizationMap)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (dual, Matrix)
///


doc ///
   Key
     (isWellDefined, ZZdFactorization)
   Headline
     whether a ZZ/d-graded factorization is well-defined
   Usage
     isWellDefined C
   Inputs
     C:ZZdFactorization
   Outputs
     :Boolean
       that is true when {\tt C} determines a well defined factorization
   Description
    Text
      This routine does NOT check that the differential of {\tt C} composes to a scalar multiple
      of the identity. Use @TO isdFactorization@ to check this.
      It checks that the underlying data in {\tt C} is a properly formed
      ZZdFactorization object in Macaulay2. If the variable {\tt debugLevel} is set to a value greater than zero,
      then information about the nature of any failure is displayed.
    Text
      Most of the basic constructors will by default yield a well-defined factorization.
    Example
      R = ZZ/101[a..d];
      K = koszulMF(ideal vars R, a^2+b^2+c^2+d^2)
      isWellDefined K
      K.dd
      K.dd^2
      isdFactorization K
    Text
      The direct sum of factorizations is considered well-defined, but it is NOT a ZZ/d-graded factorization
      since the differential does not compose to a scalar multiple of the identity.
    Example
      C1 = randomTailMF(a^3+b^3, 2,6,2)
      C2 = randomTailMF(a^3+b^3+c^3,2,6,2)
      isWellDefined (C1++C2)
      isdFactorization (C1++C2)
    Text
      The zero factorization is well-defined, regardless of the specified period.
    Example
      C = ZZdfactorization(R^0,2)
      isWellDefined C
      isWellDefined ZZdfactorization(R^0,5)
   SeeAlso
     (isWellDefined, ZZdFactorizationMap)
     map
///



doc ///
    Key
        (symbol**, ZZdFactorization, Matrix)
        (symbol**, Matrix, ZZdFactorization)
    Headline
        create the tensor product of a ZZ/d-graded factorization and a map of modules
    Usage
        h = C ** f
        h = f ** C
    Inputs
        C:ZZdFactorization
            over a ring $R$
        f:Matrix
            defining a homomorphism from the $R$-module $M$ to the $R$-module $N$
    Outputs
        h:ZZdFactorizationMap
            from $C \otimes M$ to $C \otimes N$
    Description
        Text
            For any ZZ/d-graded factorization map $C$, a map $f \colon M \to N$ of $R$-modules induces a
            morphism $C \otimes f$ of ZZ/d-graded factorizations
            from $C \otimes M$ to $C \otimes N$.  This method returns this map of factorizations.
        Example
            R = ZZ/101[a..d];
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            J = ideal(I_0, I_1)
            C = koszulMF({a,b,c,d}, a^3+b^3+c^3+d^3)
            f = map(R^1/I, R^1/J, 1)
            C ** f
            f ** C
            f' = random(R^2, R^{-1, -1, -1})
            C ** f'
            f' ** C
            assert isWellDefined(C ** f')
            assert isWellDefined(f' ** C)
        Text
            Tensoring with a factorization defines a functor from the category
            of $R$-modules to the category of factorizations over $R$ (that leaves the potential unchanged).
        Example
            f'' = random(source f', R^{-2,-2})
            assert((C ** f') * (C ** f'') == C ** (f' * f''))
            assert(C ** id_(R^{-1,-2,-3}) == id_(C ** R^{-1,-2,-3}))
    SeeAlso
        "Making maps between factorizations"
        (symbol**, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (symbol**, ZZdFactorizationMap, ZZdFactorizationMap)
        (tensor, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol**, ZZdFactorization, ZZdFactorizationMap)
        (symbol**, ZZdFactorizationMap, ZZdFactorization)
        (symbol**, ZZdFactorizationMap, Module)
        (symbol**, Module, ZZdFactorizationMap)
    Headline
        the map of ZZ/d-graded factorizations between tensor factorizations
    Usage
        h = f ** g
        h = tensor(f, g)
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of ZZ/d-graded factorizations induce the map
            $h = f \otimes g : C \otimes E \to D \otimes F$ defined by $c \otimes e \mapsto f(c) \otimes g(e)$.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C,D)
            E = (dual C)
            F = (dual D)
            g = randomFactorizationMap(F,E)
            h = f ** g
            assert isWellDefined h
            assert(source h === D ** E)
            assert(target h === C ** F)
        Text
            If one argument is a ZZdFactorization or Module,
            then the identity map of the corresponding factorization is used.
        Example
            fE = f ** E
            assert(fE === f ** id_E)
            k = coker vars S
            gk = g ** k
            assert(gk == g ** id_(complex k))
        Text
            This routine is functorial.
        Example
	    use S;
            D' = randomTailMF(a^3+b^3)
            f' = randomFactorizationMap(D, D',InternalDegree => 3)
            (f * f') ** g == (f ** g) * (f' ** id_E)
            (f * f') ** g == (f ** id_F) * (f' ** g)
    SeeAlso
        (symbol**, ZZdFactorization, ZZdFactorization)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (Hom, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (symbol**, ComplexMap, ZZdFactorizationMap)
	(symbol**, ZZdFactorizationMap, ComplexMap)
	(symbol**, Complex, ZZdFactorizationMap)
	(symbol**, ZZdFactorizationMap, Complex)
    Headline
        the map of ZZ/d-graded factorizations between tensor factorizations, obtained by first folding the relevant complex map
    Usage
        h = f ** g
        h = tensor(f, g)
    Inputs
        f:ComplexMap
	  or @ofClass ZZdFactorizationMap@
        g:ZZdFactorizationMap
	  or @ofClass ComplexMap@
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of ZZ/d-graded factorizations induce the map
            $h = f \otimes g : C \otimes E \to D \otimes F$ defined by $c \otimes e \mapsto f(c) \otimes g(e)$.
        Example
            S = ZZ/101[a..c]
            C = freeResolution coker vars S
            D = (freeResolution coker matrix{{a^2,a*b,b^3}})[-1]
            f = randomComplexMap(D,C)
	    m = ideal vars (S/(a^3+b^3))
	    g = randomFactorizationMap(tailMF m, tailMF (m^2), Cycle => true)
	    isWellDefined (g**f)
    SeeAlso
        (symbol**, ZZdFactorization, ZZdFactorization)
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        (Hom, ZZdFactorization, ZZdFactorization)
///

doc ///
    Key
        (symbol SPACE, RingMap, ZZdFactorizationMap)
    Headline
        apply a ring map to a map of ZZ/d-graded factorizations
    Usage
        phi f
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:ZZdFactorizationMap
            over the ring $R$
    Outputs
        :ZZdFactorizationMap
            over the ring $S$
    Description
        Text
            We illustrate the image of a ZZ/d-graded factorization map along a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            Rf = R/(x^3+y^3+z^3)
            C = tailMF ideal vars Rf
            D = phi C
	    isdFactorization D
	    phi(potential C) == potential D
            dd^D
            f = randomFactorizationMap(C, C)
            assert isWellDefined f
            g = phi f
            assert isWellDefined g
            dd^(source g)
            dd^(target g)
    SeeAlso
        (symbol SPACE, RingMap, ZZdFactorization)
        (symbol **, RingMap, ZZdFactorizationMap)
///

doc ///
    Key
        (symbol**, RingMap, ZZdFactorizationMap)
        (symbol**, Ring, ZZdFactorizationMap)
        (symbol**, ZZdFactorizationMap, RingMap)
        (symbol**, ZZdFactorizationMap, Ring)
        (tensor, RingMap, ZZdFactorizationMap)
        (tensor, ZZdFactorizationMap, RingMap)
    Headline
        tensor a map of ZZ/d-graded factorizations along a ring map
    Usage
        phi ** f
        tensor(phi, f)
        S ** f
        f ** S
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:ZZdFactorizationMap
            over the ring $R$
    Outputs
        :ZZdFactorizationMap
            over the ring $S$
    Description
        Text
            These methods implement the base change of rings.  As input, one can either
            give a ring map $\phi$, or the ring $S$ (when there is a canonical map
                from $R$ to $S$).
        Text
            We illustrate the tensor product of a map of ZZ/d-graded factorizations along a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            Rf = R/(x^3+y^3+z^3)
            C = tailMF ideal vars Rf
            D = phi ** C
	    isdFactorization D
	    phi(potential C) == potential D
            dd^D
            f = randomFactorizationMap(C, C)
            assert isWellDefined f
            g = phi ** f
            assert isWellDefined g
            dd^(source g)
            dd^(target g)
    SeeAlso
        (symbol **, RingMap, ZZdFactorization)
        (symbol SPACE, RingMap, ZZdFactorizationMap)
///
 




-*
(inducedMap, ZZdFactorization, ZZdFactorization, ZZdFactorizationMap)
*-

doc ///
    Key
        (inducedMap, ZZdFactorization, ZZdFactorization)
    Headline
        make the map of ZZ/d-graded factorizations induced at each term by the identity map
    Usage
        f = inducedMap(D, C)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
        Degree => ZZ
            specify the degree of the map of ZZ/d-graded factorizations, if not 0
        Verify => Boolean
            if true, check that the resulting maps are well-defined
    Outputs
        f:ZZdFactorizationMap
    Description
        Text
            Let $d$ be the value of the optional argument {\tt
            Degree}, or zero, if not given.  For each $i$, the terms
            $D_{i+d}$ and $C_i$ must be subquotients of the same
            ambient free module.  This method returns the factorization map
            induced by the identity on each of these free modules.
            
            If {\tt Verify => true} is given, then this method
            also checks that these identity maps induced well-defined 
            maps.  This can be a relatively expensive computation.
        Text
            We illustrate this method by inducing some of the natural inclusions and surjections
	    induced by taking kernels/cokernels of morphisms of factorizations.
        Example
            R = ZZ/101[a,b,c]
            C = koszulMF({a,b,c}, a^3+b^3+c^3)
	    f = randomFactorizationMap(C, C, Cycle => true, InternalDegree => 1)
	    isWellDefined coker f
	    (coker f).dd^2 == (a^3 + b^3 + c^3)*id_(coker f)
	    i1 = inducedMap(coker f, C)
	    i2 = inducedMap(C, image f)
	    ker i1 == image i2
    SeeAlso
        (inducedMap, Module, Module)
///


doc ///
    Key
        "arithmetic with ZZ/d-graded factorization maps"
        (symbol+, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol+, RingElement, ZZdFactorizationMap)
        (symbol+, Number, ZZdFactorizationMap)
        (symbol+, ZZdFactorizationMap, RingElement)
        (symbol+, ZZdFactorizationMap, Number)
        (symbol-, ZZdFactorizationMap)
        (symbol-, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol-, RingElement, ZZdFactorizationMap)
        (symbol-, Number, ZZdFactorizationMap)
        (symbol-, ZZdFactorizationMap, RingElement)
        (symbol-, ZZdFactorizationMap, Number)
        (symbol*, RingElement, ZZdFactorizationMap)
        (symbol*, Number, ZZdFactorizationMap)
        (symbol*, ZZdFactorizationMap, RingElement)
        (symbol*, ZZdFactorizationMap, Number)
    Headline
        perform arithmetic operations on ZZ/d-graded factorization maps
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
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
        a:RingElement
          that is, an element in the underlying ring or a number
    Outputs
        :ZZdFactorizationMap
    Description
        Text
            The set of ZZ/d-graded factorizations maps forms a module over the underlying @TO2((ring, ZZdFactorizationMap), "ring")@.
            These methods implement the basic operations of addition, subtraction, and scalar multiplication.
	Example
	    S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(D, C, Cycle => true, InternalDegree => 1)
            g = randomFactorizationMap(D, C, Boundary => true)
        Example
	    use S;
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
        Text
            Adding or subtracting a scalar is the same as adding or subtracting the
            scalar multiple of the identity.  In particular, the source and target must be equal.
    SeeAlso
        "Making maps between factorizations"
        randomFactorizationMap
        (map, ZZdFactorization, ZZdFactorization, ZZdFactorizationMap)
///




doc ///
    Key
        (symbol|, ZZdFactorizationMap, ZZdFactorizationMap)
    Headline
        join or concatenate maps horizontally
    Usage
        f | g
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
    Outputs
        :ZZdFactorizationMap
    Description
        Text
            Given ZZ/d-graded factorization maps with the same target,
            this method constructs the associated map
            from the direct sum of the sources to the target.

            First, we define some non-trivial maps of factorizations.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
	    use S;
            C1 = tailMF m
	    C2 = randomTailMF(a^3+b^3, 2, 4, 2)
            D = tailMF (m^2)
            f = randomFactorizationMap(D, C1, InternalDegree => 1)
            g = randomFactorizationMap(D, C2, InternalDegree => 3)
        Example
            h = f|g
            assert isWellDefined h
            assert(source h === source f ++ source g)
            assert(target h === target f)
    SeeAlso
        (symbol++, ZZdFactorization, ZZdFactorization)
        (symbol++, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol||, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol|, Matrix, Matrix)
        (map, ZZdFactorization, ZZdFactorization, List)
///



doc ///
    Key
        (symbol||, ZZdFactorizationMap, ZZdFactorizationMap)
    Headline
        join or concatenate maps vertically
    Usage
        f || g
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
    Outputs
        :ZZdFactorizationMap
    Description
        Text
            Given ZZ/d-graded factorization maps with the same source,
            this method constructs the associated map
            from the source to the direct sum of the targets.

            First, we define some non-trivial maps of factorizations.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
	    use S;
            C = tailMF m
	    D1 = randomTailMF(a^3+b^3, 2, 4, 2)
            D2 = tailMF (m^2)
            f = randomFactorizationMap(D1, C)
            g = randomFactorizationMap(D2, C, InternalDegree => 2)
        Example
            h = f||g
            assert isWellDefined h
            assert(target h === target f ++ target g)
            assert(source h === source f)
    SeeAlso
        (symbol++, ZZdFactorization, ZZdFactorization)
        (symbol++, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol|, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol||, Matrix, Matrix)
        (map, ZZdFactorization, ZZdFactorization, List)
///


doc ///
    Key
        (symbol++, ZZdFactorizationMap, ZZdFactorizationMap)
        (directSum, ZZdFactorizationMap)
    Headline
        direct sum of ZZ/d-graded factorization maps
    Usage
        h = f ++ g
        h = directSum(f,g,...)
        h = directSum(name1 => f, name2 => g, ...)
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
    Outputs
        h:ZZdFactorizationMap
          that is the direct sum of the input ZZ/d-graded factorization maps
    Description
        Text
            The direct sum of two ZZ/d-graded factorization maps is a a ZZ/d-graded factorization map
            from the direct sum of the sources to the direct sum of
            the targets.

            First, we define some non-trivial maps of ZZ/d-graded factorizations.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
	    use S;
            C1 = tailMF m
	    C2 = randomTailMF(a^3+b^3, 3, 5, 2)
	    D1 = randomTailMF(a^3+b^3, 2, 4, 2)
            D2 = tailMF (m^2)
            f = randomFactorizationMap(D1, C1, Cycle => true)
            g = randomFactorizationMap(D2, C2, Cycle => true, InternalDegree => 4)
        Example
            h = f ++ g
            assert isWellDefined h
            assert(h == map(D1 ++ D2, C1 ++ C2, {{f,0},{0,g}}))
        Text
            The direct sum of any sequence of factorization maps can be 
            computed as follows.
        Example
            directSum(f, g, f[2])
            h2 = directSum(peanut => f, butter => g, jelly => f[2])
            h2_[butter,jelly]
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
        (symbol++, ZZdFactorization, ZZdFactorization)
        (symbol**, ZZdFactorizationMap, ZZdFactorizationMap)
        (Hom, ZZdFactorizationMap, ZZdFactorizationMap)
        (symbol_, ZZdFactorizationMap, Array)
///

-- TODO for the next 4 nodes:
-- make better examples
-- add text (when are these actually defined?  Maybe change code)
-- also add SeeAlso canonicalMap's.
doc ///
  Key
    (image, ZZdFactorizationMap)
  Headline
    make the image of a map of ZZ/d-graded factorizations
  Usage
    E = image f
  Inputs
    f : ZZdFactorizationMap
  Outputs
    E : ZZdFactorization
  Description
    Text
      If $f : C \to D$ is a map of ZZ/d-graded factorizations of degree $d$,
      then the image is the ZZ/d-graded factorization $E$ whose $i-th$ is $image(f_{i-d})$,
      and whose differential is induced from the differential 
      on the target.
    Text
      In the following example, we first construct a random
      factorization morphism $f : C \to D$.  
    Example
      S = ZZ/101[a,b];
      R = S/(a^3+b^3);
      m = ideal vars R
      C = tailMF m
      D = tailMF (m^2)
      f = randomFactorizationMap(D, C, Cycle => true, InternalDegree => 2)
      isWellDefined image f
      potential image f
      g = inducedMap(D, image f)
      prune image f
      oo.dd
  SeeAlso
    "Making ZZdFactorizations"
    "Making maps between factorizations"
    image
    (coimage, ZZdFactorizationMap)
    (kernel, ZZdFactorizationMap)
    (cokernel, ZZdFactorizationMap)
///

doc ///
  Key
    (coimage, ZZdFactorizationMap)
  Headline
    make the coimage of a map of ZZ/d-graded factorizations
  Usage
    coimage f
  Inputs
    f : ZZdFactorizationMap
  Outputs
    : ZZdFactorization
  Description
    Text
      The coimage of a ZZ/d-graded factorization map $f : C \to D$
      is the ZZ/d-graded factorization $E$ whose $i-th$ term is $coimage(f_i)$,
      and whose differential is induced from the differential 
      on the source.
    Text
      In the following example, we first construct a random
      factorization morphism $f : C \to D$. 
    Example
      S = ZZ/101[a,b];
      R = S/(a^3+b^3);
      m = ideal vars R
      C = tailMF m
      D = tailMF (m^2)
      f = randomFactorizationMap(C, D, Cycle => true, InternalDegree => 2)
      isWellDefined coimage f
      inducedMap(coimage f, D)
      prune coimage f
      potential coimage f
  SeeAlso
    "Making ZZdFactorizations"
    "Making maps between factorizations"
    coimage
    (image, ZZdFactorizationMap)
    (kernel, ZZdFactorizationMap)
    (cokernel, ZZdFactorizationMap)
///

doc ///
  Key
    (kernel, ZZdFactorizationMap)
  Headline
    make the kernel of a map of ZZ/d-graded factorizations
  Usage
    kernel f
    ker f
  Inputs
    f : ZZdFactorizationMap
  Outputs
    : ZZdFactorization
  Description
    Text
      The kernel of a ZZ/d-graded factorization map $f : C \to D$
      is the factorization $E$ whose $i-th$ term is $kernel(f_i)$,
      and whose differential is induced from the differential 
      on the source.
    Text
      In the following example, we first construct a random
      factorization morphism $f : C \to D$.
    Example
      S = ZZ/101[a,b];
      R = S/(a^3+b^3);
      m = ideal vars R
      C = tailMF m
      D = tailMF (m^2)
      f = randomFactorizationMap(C, D, Cycle => true, InternalDegree => 2)
      g = inducedMap(D, ker f)
      ker g == 0
      prune ker f --since this is a 1x1 factorization of an irreducible,
      oo.dd       --it must be the trivial factorization
  SeeAlso
    "Making ZZdFactorizations"
    "Making maps between factorizations"
    ker
    (image, ZZdFactorizationMap)
    (coimage, ZZdFactorizationMap)
    (cokernel, ZZdFactorizationMap)
///

doc ///
  Key
    (cokernel, ZZdFactorizationMap)
  Headline
    make the cokernel of a map of ZZ/d-graded factorizations
  Usage
    cokernel f
    coker f
  Inputs
    f : ZZdFactorizationMap
  Outputs
    : ZZdFactorization
  Description
    Text
      If $f : C \to D$ is a map of ZZ/d-graded factorizations of degree $d$,
      then the cokernel is the factorization $E$ whose $i-th$ term is $cokernel(f_{i-d})$,
      and whose differential is induced from the differential 
      on the target.
    Text
      In the following example, we first construct a random
      factorization morphism $f : C \to D$. 
    Example
      S = ZZ/101[a,b];
      R = S/(a^3+b^3);
      m = ideal vars R
      use S;
      C = tailMF m
      D = tailMF (m^2)
      f = randomFactorizationMap(D, C, Cycle => true, InternalDegree => 2)
      g = inducedMap(coker f, D)
      coker g == 0
      prune coker f
      (coker f).dd^2 == a^3+b^3
  SeeAlso
    "Making ZZdFactorizations"
    "Making maps between factorizations"
    cokernel
    (image, ZZdFactorizationMap)
    (coimage, ZZdFactorizationMap)
    (kernel, ZZdFactorizationMap)
///


doc ///
  Key
    (cone, ZZdFactorizationMap)
  Headline
    make the mapping cone of a morphism of ZZ/d-graded factorizations
  Usage
    cone f
  Inputs
    f:ZZdFactorizationMap
      which is a morphism of ZZ/d-graded factorizations
  Outputs
    :ZZdFactorization
  Description
    Text
      Given a morphism $f \colon B \to C$, the mapping cone is the factorization
      whose $i$-th term is $B_{i-1} \oplus C_i$, and whose $i$-th 
      differential is given by
      \[ \begin{bmatrix} -\operatorname{dd}^{B[-1]} & 0 \\ f[-1] & \operatorname{dd}^C \end{bmatrix}. \]
    Text
      A map between modules induces a map between their free resolutions,
      and we compute the associated mapping cone.
    Example
      S = ZZ/101[a,b];
      R = S/(a^3+b^3);
      m = ideal vars R
      use S;
      C = tailMF m
      D = tailMF (m^2)
      f = randomFactorizationMap(D, C, Cycle => true, InternalDegree => 2)
      Cf = cone f
      dd^Cf
      isdFactorization Cf
      cone inducedMap(coker f, target f)
      isdFactorization oo
    Text
      The mapping cone fits into a canonical short exact
      sequence of factorizations:
      $$0 \to C \to \operatorname{cone}(f) \to B[1] \to 0.$$
  SeeAlso
    "Making ZZdFactorizations"
    "Making maps between factorizations"
    isQuasiIsomorphism
    isShortExactSequence
///



doc ///
    Key
        (symbol^, ZZdFactorizationMap, Array)
        (symbol_, ZZdFactorizationMap, Array)
    Headline
        the composition with the canonical inclusion or projection map
    Usage
        i = f_[name]
        p = f^[name]
    Inputs
        f:ZZdFactorizationMap
        name:
    Outputs
        :ZZdFactorizationMap
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
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
	    use S;
            C1 = tailMF m
	    C2 = randomTailMF(a^3+b^3, 3, 5, 2)
	    D1 = randomTailMF(a^3+b^3, 2, 4, 2)
            D2 = tailMF (m^2)
            f = randomFactorizationMap(D1, C1, Cycle => true)
            g = randomFactorizationMap(D2, C2, Cycle => true, InternalDegree => 4)
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
            h = (chicken => f) ++ (nuggets => g)
            h_[chicken]^[chicken] == f
            h_[nuggets]^[nuggets] == g
    SeeAlso
        (symbol++, ZZdFactorization, ZZdFactorization)
        (symbol^, ZZdFactorization, Array)
        (symbol_, ZZdFactorization, Array)
        (directSum, ZZdFactorization)
        (components, ZZdFactorization)
        indices
///


doc ///
    Key
        randomFactorizationMap
        (randomFactorizationMap, ZZdFactorization, ZZdFactorization)
        [randomFactorizationMap, Boundary]
        [randomFactorizationMap, Cycle]
        [randomFactorizationMap, Degree]
        [randomFactorizationMap, InternalDegree]
    Headline
        a random map of ZZ/d-graded factorizations
    Usage
        f = randomFactorizationMap(C,D)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
        Boundary => Boolean
            whether the constructed {\tt f} is a null homotopy
        Cycle => Boolean
            whether the constructed {\tt f} commutes with the differentials
        Degree => ZZ
            the degree of the constructed map of chain complexes
        InternalDegree => List
            or @ofClass ZZ@
    Outputs
        f:ZZdFactorizationMap
    Description
        Text
            A random ZZ/d-graded factorization map $f : C \to D$ is obtained from a random element
            in the ZZ/d-graded factorization @TO2 ((Hom,ZZdFactorization,ZZdFactorization), "$Hom(C,D)$")@.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    f = randomFactorizationMap(D,C)
            assert isWellDefined f
            assert not isCommutative f
            --assert not isNullHomotopic f
        Text
            When the random element in the factorization $Hom(C,D)$ lies in the kernel
            of the differential, the associated map of complexes commutes
            with the differential.
        Example
            g = randomFactorizationMap(D,C, Cycle => true, InternalDegree => 2)
            assert isWellDefined g
            assert isCommutative g
            assert isFactorizationMorphism g
            --assert not isNullHomotopic g
        Text
            Assume that $C$ and $D$ are factorizations of the same potential. Then the factorization
	    $Hom(C,D)$ is actually a ZZ/d-graded complex.
	    When the random element in the factorization $Hom(C,D)$ lies in the image
            of the differential, the associated map of complexes is a null
            homotopy.
        Example
            h = randomFactorizationMap(D,C, Boundary => true)
            assert isWellDefined h
            assert isCommutative h
            assert isFactorizationMorphism h
            --assert isNullHomotopic h
            --nullHomotopy h
        Text
            When the degree of the random element in the factorization $Hom(C,D)$ is non-zero,
            the associated map of factorizations has the same degree.
        Example
            p = randomFactorizationMap(D, C, Cycle => true, Degree => 1)
            assert isWellDefined p
            assert isCommutative p
            assert not isFactorizationMorphism p
            assert(degree p === 1)
        Text
            By default, the random element is constructed as a random linear combination of
            the basis elements in the appropriate degree of $Hom(C,D)$.  Given an internal
            degree, the random element is constructed as maps of modules with this degree.
        Example
            q = randomFactorizationMap(D, C, Boundary => true, InternalDegree => 2)
            assert all({0,1,2}, i -> degree q_i === {2})
            assert isWellDefined q
            assert isCommutative q
            assert isFactorizationMorphism q
            source q === C
            target q === D
            --assert isNullHomotopic q
    SeeAlso
        (homomorphism, ZZdFactorizationMap)
        (homomorphism', ZZdFactorizationMap)
        (Hom, ZZdFactorization, ZZdFactorization)
///



doc ///
    Key
        (homology, ZZdFactorizationMap)
        (homology, ZZ, ZZdFactorizationMap)
	(cohomology, ZZ, ZZdFactorizationMap)
    Headline
        induced map on homology or cohomology
    Usage
        h = HH f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            Homology defines a functor from the category of ZZ/d-graded complexes
            to itself.  Given a map of ZZ/d-graded complexes $f : C \to D$,
            this method returns the induced map $HH f : HH C \to HH D$.
        Text
            To directly obtain the $n$-th map in $h$, use {\tt HH_n f} or
            {\tt HH^n f}.  By definition {\tt HH^n f === HH_(-n) f}.
            This can be more efficient, as it will compute only the desired
            induced map. Note that this method does not check whether the
	    differentials in the factorization compose to 0, so the user should
	    verify this for themselves using @TO isZZdComplex@ or a direct check.
        Text
            If $f$ commutes with the differentials, then these induced
            maps are well defined.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    f = randomFactorizationMap(D,C, Cycle => true, InternalDegree => 1)
	    g = Hom(f,C)
	    assert isCommutative g
	    (isZZdComplex source g, isZZdComplex target g)
	    h = HH(g)
            assert isWellDefined h
            prune h
            assert(source h == HH Hom(D,C))
            assert(target h == HH Hom(C,C))
        Example
            f2 = randomFactorizationMap(C, D, Cycle => true, Degree => 1, InternalDegree => 1)
            h2 = HH Hom(C,f2)
            assert isWellDefined h2
            prune h2
        Text
            A boundary will always induce the zero map.
        Example
            f3 = randomFactorizationMap(D, C, Boundary => true)
            h3 = HH Hom(C,f3)
            assert isWellDefined h3
            assert(h3 == 0)
    SeeAlso
        (homology, ZZdFactorization)
        (homology, ZZ, ZZdFactorization)
        (prune, ZZdFactorizationMap)
///


doc ///
    Key
        (tensorCommutativity, ZZdFactorization, ZZdFactorization)
    Headline
        make the canonical isomorphism arising from commutativity of the tensor product operation
    Usage
        tensorCommutativity(C, D)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
            both over the same ring $R$
    Outputs
        :ZZdFactorizationMap
            that is an isomorphism from $C \otimes_R D$ to 
            $D \otimes_R C$, assuming factorizations of period 2
    Description
        Text
            The commutativity of tensor products of modules induces
            the commutativity of tensor products of ZZ/2-graded factorizations. The main difference
	    for factorizations is that there are coefficients depending on the degrees of the terms
	    being commuted.
            This method implements this isomorphism for ZZ/2-graded factorizations.
        Text
            Using two term complexes of small rank,
            we see that this isomorphism need not be the identity map.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    F = C**D
	    G = D**C
            f = tensorCommutativity(C,D)
            assert isWellDefined f
            assert isFactorizationMorphism f
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
            This isomorphism works even when the modules of the factorization
	    are not necessarily free.
        Example
            h2 = tensorCommutativity(C**(coker vars S), D**(coker vars S));
            assert isWellDefined h2
            assert isFactorizationMorphism h2
            assert(ker h2 == 0)
            assert(coker h2 == 0)
            k = h2^-1;
            assert(h2*k == 1)
            assert(k*h2 == 1)
            h2_2
            assert(source h2_2 != target h2_2)
        Text
            Interchanging the arguments gives the inverse map.
        Example
            h1 = tensorCommutativity(D, C)
            assert isFactorizationMorphism h1
            assert(h1*f == id_(C**D))
            assert(f*h1 == id_(D**C))
    Caveat
        This method will still run when the factorizations have period > 2, but the resulting
	map will not be a well-defined morphism of factorizations.
    SeeAlso
        (tensorCommutativity, Module, Module)
        (tensorAssociativity, ZZdFactorization, ZZdFactorization, ZZdFactorization)
        (isFactorizationMorphism, ZZdFactorizationMap)
///


doc ///
    Key
        (tensorAssociativity, ZZdFactorization, ZZdFactorization, ZZdFactorization)
    Headline
        make the canonical isomorphism arising from associativity
    Usage
        tensorAssociativity(C, D, E)
    Inputs
        C:ZZdFactorization
        D:ZZdFactorization
        E:ZZdFactorization
    Outputs
        :ZZdFactorizationMap
            which is an isomorphism from {\tt C ** (D ** E)} to 
            {\tt (C ** D) ** E} 
    Description
        Text
            The associativity of tensor products of modules induces
            the associativity of tensor products of ZZ/d-graded factorizations.
            This method implements this isomorphism for ZZ/d-graded factorizations. Note
	    that although @TO tensorCommutativity@ requires the factorizations to have
	    period 2 to be a well defined morphism, this function returns a well-defined
	    factorization morphism regardless of the period.
        Text
            Using two term complexes of small rank,
            we see that this isomorphism need not be the identity map.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    use S;
            E = randomTailMF(a^3+b^3, 1, 2, 3)
	    E.dd
            F = (C ** D) ** E
            G = C ** (D ** E)
            f = tensorAssociativity(C,D,E)
            assert isWellDefined f
	    assert isFactorizationMorphism f
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
    SeeAlso
        (tensorCommutativity, ZZdFactorization, ZZdFactorization)
        (tensorAssociativity, Module, Module, Module)
///


doc ///
    Key
        (isShortExactSequence, ZZdFactorizationMap, ZZdFactorizationMap)
    Headline
        whether a pair of ZZ/d-graded factorization maps forms a short exact sequence
    Usage
        isShortExactSequence(g, f)
    Inputs
        f:ZZdFactorizationMap
        g:ZZdFactorizationMap
    Outputs
        :Boolean
            that is @TO true@ if these form a short exact sequence
    Description
        Text
            A short exact sequence of ZZ/d-graded factorizations 
            \[ 0 \to B \xrightarrow{f} C \xrightarrow{g} D \to 0\]
            consists of two morphisms of factorizations
            $f \colon B \to C$ and $g \colon C \to D$ such that
            $g f = 0$, $\operatorname{image} f = \operatorname{ker} g$, 
            $\operatorname{ker} f = 0$, and $\operatorname{coker} g = 0$.
        Text
            From a factorization morphism $h \colon B \to C$, one obtains a
            short exact sequence
            \[ 0 \to \operatorname{image} h \to C \to \operatorname{coker} h \to 0. \]
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            h = randomFactorizationMap(C, D, Cycle => true)
            f = inducedMap(C, image h)
            g = inducedMap(coker h, C)
            assert isShortExactSequence(g,f)
    SeeAlso
        "Basic invariants and properties"
        (cone, ZZdFactorizationMap)
///




doc ///
    Key
        (isQuasiIsomorphism, ZZdFactorizationMap)
    Headline
         whether a map of ZZ/d-graded factorizations is a quasi-isomorphism
    Usage
         isQuasiIsomorphism f
    Inputs
         f:ZZdFactorizationMap
    Outputs
         :Boolean
             that is true when $f$ is a morphism of ZZ/d-graded factorizations
             such that the induced maps on homology are all
             isomorphisms
    Description
        Text
            The @TO2((cone, ZZdFactorizationMap), "cone")@ of a 
            map $f \colon C \to D$ is acyclic
            exactly when $f$ is a quasi-isomorphism.
        Example
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    f = randomFactorizationMap(C, C, Cycle => true) --only scalar mults of the identity
	    isQuasiIsomorphism(Hom(f,C))
	    g = randomFactorizationMap(D, D, Cycle => true, Degree => 1, InternalDegree => 2)
	    assert isCommutative g
	    isQuasiIsomorphism Hom(C,g)
	    h = randomFactorizationMap(D, D, Boundary => true)
	    assert( HH Hom(D,h) == 0 )
    SeeAlso
        "Basic invariants and properties"
        (cone, ZZdFactorizationMap)
///



doc ///
    Key
        (isNullHomotopyOf, ZZdFactorizationMap, ZZdFactorizationMap)
    Headline
        whether the first map of ZZ/d-graded factorizations is a null homotopy for the second
    Usage
        isNullHomotopyOf(h, f)
    Inputs
        h:ZZdFactorizationMap
        f:ZZdFactorizationMap
    Outputs
        :Boolean
            that is true when $h$ is a null homotopy of $f$
    Description
        Text
            A map of ZZ/d-graded factorizations $f \colon C \to D$ is
            null-homotopic if there exists a map of factorizations
	    $h : C \to D$ of degree $\deg(f)+1$,
            such that we have the equality 
            \[ f = \operatorname{dd}^D h 
              + (-1)^{\deg(f)} h \operatorname{dd}^C.
            \]
        Text
            As a first example, we verify that the identity
	    map on the "trivial" factorization is nullhomotopic.
        Example
            S = ZZ/101[a,b];
	    C = trivialMF(S^2, a^3+b^3)
            h = map(C, C, i -> if i == 1 then id_(S^2), Degree => 1)
            isWellDefined h
            assert isNullHomotopyOf(h, id_C)
            assert isNullHomotopic id_C
        Text
            A random map of factorizations, arising as a boundary
            in the associated Hom complex, is automatically
            null homotopic.  We use the method @TO nullHomotopy@
            to construct a witness and verify it is a null homotopy.
        Example
            D = randomTailMF(a^3+b^3, 1,2,4)
	    D.dd
            f = randomFactorizationMap(D, D[1], Boundary => true)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
        Text
            By assigning @TO "debugLevel"@ a positive value,
            this method provides some information about the nature
            of the failure to be a null homotopy.
        Example
            g1 = randomFactorizationMap(D, D[1], Degree => 1)
            g2 = randomFactorizationMap(D, D[1], Degree => -1)
            debugLevel = 1
            assert not isNullHomotopyOf(g1, f)
            assert not isNullHomotopyOf(g2, f)
    SeeAlso
        "Basic invariants and properties"
        (isNullHomotopic, ZZdFactorizationMap)
        (nullHomotopy, ZZdFactorizationMap)
        randomFactorizationMap
        (Hom, ZZdFactorization, ZZdFactorization)
///



doc ///
    Key
        (isNullHomotopic, ZZdFactorizationMap)
    Headline
        whether a map of ZZ/d-graded factorizations is null-homotopic
    Usage
        isNullHomotopic f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        :Boolean
           that is true when $f$ is null-homotopic
    Description
        Text
            A map of ZZ/d-graded factorizations $f \colon C \to D$ is
            null-homotopic if there exists a map of factorizations
            $h : C \to D$ of degree $\deg(f)+1$,
            such that we have the equality 
            \[ f = \operatorname{dd}^D h 
              + (-1)^{\deg(f)} h \operatorname{dd}^C.
            \]
        Text
            As a first example, we verify that the identity
	    map on the "trivial" factorization is nullhomotopic.
        Example
            S = ZZ/101[a,b];
	    C = trivialMF(S^2, a^3+b^3)
            h = map(C, C, i -> if i == 1 then id_(S^2), Degree => 1)
            isWellDefined h
            assert isNullHomotopyOf(h, id_C)
            assert isNullHomotopic id_C
        Text
            A random map of factorizations, arising as a boundary
            in the associated Hom complex, is automatically
            null homotopic.  We use the method @TO nullHomotopy@
            to construct a witness and verify it is a null homotopy.
        Example
            D = randomTailMF(a^3+b^3, 1,2,4)
	    D.dd
            f = randomFactorizationMap(D, D[1], Boundary => true)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
        Text
            This procedure also works for complex maps
            whose degree is non-zero.
        Example
            f = randomFactorizationMap(D, D[2], Boundary => true, Degree => 1)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
    SeeAlso
        "Basic invariants and properties"
        (isNullHomotopyOf, ZZdFactorizationMap, ZZdFactorizationMap)
        (nullHomotopy, ZZdFactorizationMap)
        randomFactorizationMap
        (Hom, ZZdFactorization, ZZdFactorization)
///



doc ///
    Key
        (nullHomotopy, ZZdFactorizationMap)
    Headline
        a map which is a candidate for being a null homotopy
    Usage
        h = nullHomotopy f
    Inputs
        f:ZZdFactorizationMap
    Outputs
        h:ZZdFactorizationMap
    Description
        Text
            A map of ZZ/d-graded factorizations $f \colon C \to D$ is
            null-homotopic if there exists a map of factorizations
            $h : C \to D$ of degree $\deg(f)+1$,
            such that we have the equality 
            \[ f = \operatorname{dd}^D h 
              + (-1)^{\deg(f)} h \operatorname{dd}^C.
            \]
        Text
            As a first example, we verify that the identity
	    map on the "trivial" factorization is nullhomotopic.
        Example
            S = ZZ/101[a,b];
	    C = trivialMF(S^2, a^3+b^3)
            h = map(C, C, i -> if i == 1 then id_(S^2), Degree => 1)
            isWellDefined h
            assert isNullHomotopyOf(h, id_C)
            assert isNullHomotopic id_C
        Text
            A random map of factorizations, arising as a boundary
            in the associated Hom complex, is automatically
            null homotopic.  We use the method @TO nullHomotopy@
            to construct a witness and verify it is a null homotopy.
        Example
            D = randomTailMF(a^3+b^3, 1,2,4)
	    D.dd
            f = randomFactorizationMap(D, D[1], Boundary => true)
            assert isNullHomotopic f
            h = nullHomotopy f
            assert isNullHomotopyOf(h, f)
        Text
            When a map of factorizations is not null-homotopic,
            this method nevertheless returns a map $h$ of
            factorizations, having the correct source, target
            and degree, but cannot be a null homotopy.
        Example
            g = randomFactorizationMap(D, D[1])
            assert not isNullHomotopic g
            h' = nullHomotopy g
            assert isWellDefined h'
            assert(degree h' === degree g + 1)
            assert not isNullHomotopyOf(h', g)
    Caveat
        The output is only a null homotopy when one exists.
    SeeAlso
        "Making maps between factorizations"
        (isNullHomotopic, ZZdFactorizationMap)
        (isNullHomotopyOf, ZZdFactorizationMap, ZZdFactorizationMap)
        randomFactorizationMap
        (Hom, ZZdFactorization, ZZdFactorization)
///







doc ///
    Key
       Fold
       (Fold, Complex, ZZ)
       (Fold, ComplexMap, ZZ)
    Headline
        Convert any complex or complex map into a ZZ/d-graded factorization (or map) for a fixed integer d
    Usage
        Fold(C,d)
	Fold(phi,d)
    Inputs
        C:Complex
	  or @ofClass ComplexMap@
	d:ZZ
	   an integer specifying the period of the resulting factorization or factorization map
    Outputs
        :ZZdFactorization
    Description
        Text
            Any complex can be viewed as a ZZ/d-graded factorization of 0 by
	    grouping all terms in homological degrees that are the same, modulo
	    the period.
        Example
	    Q = QQ[x_1..x_3];
	    K = koszulComplex vars Q
	    fK = Fold(K, 2)  --this groups the odd/even degrees together
	    fK.dd
	    isZZdComplex fK
	    f3K = Fold(K,3)
	    f3K.dd
	    f3K.dd^2
	    isZZdComplex(f3K)
	Text
	    When the specified period is longer than the actual complex, the
	    output will just be the complex but viewed with additional zero positions.
	Example
	    K' = koszulComplex {x_1,x_2}
	    Fold(K',4)
	    Fold(K',5)
	Text
	    The Fold function is also functorial and can be applied to complex maps.
	Example
	    Fold(id_K, 2)
	    oo == id_(Fold(K,2))
	    f = randomComplexMap(K, K', Cycle => true)
	    ff = Fold(f,2)
	    isCommutative Fold(f, 2)
	    isCommutative Fold(f, 3)
    SeeAlso
        "Making ZZdFactorizations"
///





doc /// 
    Key
       tailMF
       (tailMF, Module)
       (tailMF, Ideal)
    Headline
        Generate a ZZdFactorization from a module over a hypersurface ring
    Usage
        tailMF(Module)
    Inputs
        Module:Module
            A module over a hypersurface ring or @ofClass Ideal@ in a hypersurface ring
    Outputs
        :ZZdFactorization
    Description
        Text
            The function tailMF takes a module $M$ over a hypersurface ring $R = S/(f)$ and generates a ZZdFactorization.
	    This involves computing a resolution of $M$ over the hypersurface $R$, taking a high truncation
	    of this resolution, lifting it to the ambient ring $S$, then finding a nullhomotopy for multiplication by $f$.
        Example
            S = ZZ/101[a,b,c];
	    R = S/(a^3+b^3+c^3);
	    m = ideal vars R;
	    C1 = tailMF m
	    C1.dd
	    assert isWellDefined C1
	    C1.dd^2
	    C2 = tailMF m^2
	    C2.dd
	    C2.dd^2
    Caveat
        There are no checks implemented for whether the ambient ring is actually a hypersurface. The user
	should make sure the input module $M$ satisfies the conditions put forth above.
    SeeAlso
        "Making ZZdFactorizations"
        koszulMF
	randomTailMF
///

doc ///
    Key
        koszulMF
        (koszulMF, List, RingElement)
	(koszulMF, Ideal, RingElement)
	(koszulMF, RingElement)
    Headline
        Construct a Koszul matrix factorization from an ideal and a polynomial
    Usage
        koszulMF(List, RingElement)
    Inputs
        List:List
            A list of ring elements or an ideal I
        RingElement:RingElement
            A polynomial element
    Outputs
        :ZZdFactorization
    Description
        Text
            Constructs a Koszul matrix factorization from a list and a polynomial, where the factorization is built
	    by writing the ring element as a linear combination of the ring elements in the list. If the list is instead
	    and ideal, then the factorization is built with respect to a generating set of the ideal. If no list or ring is specified,
	    then the command constructs the factorization with respect the the homogeneous maximal ideal.
        Example
            R = QQ[x,y,z]
            I = ideal(x^2*y, y^3*z)
            f = x^2*y + y^3*z
	    K = koszulMF({x, y, z}, f)
	    K == koszulMF(f)
	    K.dd
	    K' = koszulMF({x, y^2, z}, f)
	    K'.dd
	    koszulMF(ideal(x^2,y^2,z^2), f) --factorization changes, depending on list
	    oo.dd
	    K3 = koszulMF(I, f)
	    oo.dd  --looks like tensor product of trivial factorizations
	    isNullHomotopic id_K3
	    nullHomotopy id_K3
    Caveat
        This method does not actually check if the ring element can be written as
	a linear combination of the elements in the list.
    SeeAlso
        "Making ZZdFactorizations"
        tailMF
	eulerMF
///

doc ///
    Key
        eulerMF
        (eulerMF, RingElement)
    Headline
        Construct a Koszul matrix factorization with respect to the Jacobian ideal
    Usage
        eulerMF(RingElement)
    Inputs
        RingElement:RingElement
            A polynomial element
    Outputs
        :ZZdFactorization
    Description
        Text
            Constructs a Koszul matrix factorization with respect to the Jacobian ideal. This method works best in characteristic 0.
            This method utilizes the \( \text{koszulMF} \) function to construct a Koszul matrix factorization with respect to the Jacobian ideal of the polynomial \( f \).
        Example
            R = ZZ/101[x,y,z];
            f = x^2*y - y^3;
            K = eulerMF(f)
	    K.dd
	    g = random(4,R)
	    K' = eulerMF g
	    K'.dd
	    potential K' == g
    Caveat
        Since this method involves computing the jacobian ideal, the characteristic
	of the underlying field should be sufficiently large with respect to the degree of f.
    SeeAlso
        "Making ZZdFactorizations"
        koszulMF
///


doc ///
  Key
    (isFree, ZZdFactorization)
    isFree
  Headline
    whether a ZZ/d-graded factorization consists of free modules
  Usage
    isFree C
  Inputs
    C:ZZdFactorization
  Outputs
    :Boolean
      that is true when each $C_i$ is a free module
  Description
    Text
      This method checks whether the given representation of each
      module $C_i$ is free. To determine whether the factorization $C$ is
      isomorphic to a free factorization, use @TO2((prune,ZZdFactorization), "prune")@.
    Text
      The following example demonstrates that the presentation of a module
      might not reveal the property of being free.
    Example
      S = ZZ/101[a,b];
      M = koszulMF(ideal vars S, a^3+b^3)
      M' = coker (M++M)_[0]
      prune M'
      assert not isFree M'
      assert isFree prune M
  SeeAlso
    isFreeModule
    (prune, ZZdFactorization)
///


doc ///
    Key
        adjoinRoot
        (adjoinRoot, ZZ, Ring, Symbol)
        (adjoinRoot, ZZ, Ring, RingElement)
        (adjoinRoot, ZZdFactorization, RingElement)
        (adjoinRoot, ZZdFactorization, Symbol)
        (adjoinRoot, ZZdFactorizationMap, RingElement)
        (adjoinRoot, ZZdFactorizationMap, Symbol)
	rootOfUnity
	RootOfUnity
    Headline
        Adjoin a root of unity to the underlying object input
    Usage
        adjoinRoot(ZZ, Ring, Symbol)
        adjoinRoot(ZZ, Ring, RingElement)
        adjoinRoot(ZZdFactorization, RingElement)
        adjoinRoot(ZZdFactorization, Symbol)
        adjoinRoot(ZZdFactorizationMap, RingElement)
        adjoinRoot(ZZdFactorizationMap, Symbol)
    Inputs
        ZZ:ZZ
            An integer representing the order of the root of unity
        Ring:Ring
            The ring object
        Symbol:Symbol
            A symbol or RingElement representing the name assigned to the new indeterminate
    Outputs
        :Ring
    Description
        Text
            When working with ZZ/d-graded factorizations of period > 2, the operations are often
	    dependent on the existence of a distinguished primitive root of unity. The {\tt adjoinRoot} command
	    allows the user to adjoin or specify a distinguished root of unity to either a ring or
	    a @TO ZZdFactorization@/@TO ZZdFactorizationMap@.
	Text
	    We will illustrate all of the methods of adjoining a distinguished root of unity, including
	    the advantages/disadvantages of using certain methods.
        Example
            Q = QQ[x_1..x_3];
            d = 3;
            Qt = adjoinRoot(d, Q, t)
	    Qt.rootOfUnity --stores the root of unity in the ring
	    {t, t^2, t^3}
	Text
	    Any factorization of length > 2 constructed over the ring $Qt$ will automatically
	    cache the distinguished root of unity $t$.
	Example
            Dt = ZZdfactorization {x_1*1_Qt,x_2*1_Qt,x_3*1_Qt}
	    Dt.cache.rootOfUnity
	Text
	    If the user forgets to adjoin a root of unity, then one can directly adjoin a root
	    to the factorization by either using {\tt adjoinRoot} or tensoring with a ring that
	    contains a root of unity.
	Example
	    S = QQ[x,y,z];
	    C = ZZdfactorization {x,y,z}
	    C.cache.?rootOfUnity
	    Ct = adjoinRoot(C, t)
	    (Ct.cache.?rootOfUnity, Ct.cache.rootOfUnity)
            St = adjoinRoot(3, S, t);
	    Ct' = C**St
	    Ct'.cache.rootOfUnity
	Text
	    Sometimes a ring already has a distinguished root of unity without the need to adjoin
	    a new variable. The user can specify such a distinguished root of unity in this case, and
	    all constructions for factorizations of larger period will go through using this
	    distinguished root of unifty.
	Example
	    R = ZZ/3[x,y,z]
	    R.rootOfUnity = 1_R
	    Cx = ZZdfactorization {x,x,x}
	    Cy = ZZdfactorization {y,y,y}
	    Cz = ZZdfactorization {z,z,z}
	    Cxyz = Cx**Cy**Cz
	    Cxyz.dd
	    Cxyz.dd^3
        Text
	    The user can also adjoin roots of unity directly to maps of factorizations, if needed.
	Example
	    use S;
	    C = ZZdfactorization {x,y,z}
	    newId = adjoinRoot(id_C, t)
	    tnewId = newId**newId
	    assert(source tnewId == (C**ring newId)**(C**ring newId))
    SeeAlso
        "Making ZZdFactorizations"
///

doc ///
    Key
        monomialMF
        (monomialMF, RingElement)
    Headline
        Construct a trivial d-fold factorization of a monomial
    Usage
        monomialMF(RingElement)
    Inputs
        RingElement:RingElement
            A monomial in some polynomial ring
    Outputs
        :ZZdFactorization
    Description
        Text
            Constructs a trivial d-fold factorization of a degree d monomial, which is given by multiplying
	    all variables of the monomial.
        Example
	    Q = QQ[x..z]
            f = x*y^2*z^3
	    degree f
            C = monomialMF(f)
	    isdFactorization C
	    period C
	    g = 2/3*x*y*z
	    C' = monomialMF(g)
	    C'.dd
    SeeAlso
        "Making ZZdFactorizations"
        linearMF
	randomLinearMF
///


doc ///
    Key
        linearMF
        (linearMF, RingElement)
        (linearMF, RingElement, RingElement)
        (linearMF, RingElement, Symbol)
    Headline
        Constructs a Koszul d-fold factorization of a homogeneous element of degree d
    Usage
        linearMF(RingElement)
    Inputs
        RingElement:RingElement
            A ring element and a Symbol of RingElement, specifying the name of a distinguished
	    root of unity if one needs to be adjoined.
    Outputs
        :ZZdFactorization
    Description
        Text
            This method constructs a Koszul d-fold factorization of a homogeneous element of degree $d$.
            The code first checks if the input element $f$ is homogeneous. If it is, it constructs a
	    Koszul d-fold factorization obtained by taking the tensor product of the "trivial" factorizations
	    of the constituent monomial terms (obtained by using the @TO monomialMF@ function).
        Example
	    Q = QQ[x..z];
            f = x*y^2*z^3 + x^3*y*z^2
            K = linearMF(f, t)
	    diffs = K.dd
	    diffs^6
	Text
	    These linear factorizations are interesting because they give an explicit construction of
	    nontrivial Ulrich modules over hypersurface rings. These Ulrich modules are constructed
	    by taking the cokernel of any of the matrices in the factorization, then tensoring with
	    the hypersurface ring.
    SeeAlso
        "Making ZZdFactorizations"
///


doc ///
    Key
        randomLinearMF
        (randomLinearMF, ZZ, Ring)
        (randomLinearMF, ZZ, Ring, RingElement)
        (randomLinearMF, ZZ, Ring, Symbol)
    Headline
        Generates a linear d-fold factorization of a random degree d polynomial
    Usage
        randomLinearMF(ZZ, Ring)
    Inputs
        ZZ:ZZ
            An integer representing the degree
        Ring:Ring
            The ring object
        RingElement:RingElement
            An element in the ring
        Symbol:Symbol
            A symbol representing the indeterminate
    Outputs
        :ZZdFactorization
    Description
        Text
            Generates a linear d-fold factorization of a random degree $d$ polynomial using the
	    @TO linearMF@ command. If the ring has too many variables the output may be a factorization
	    that is too large to be useful.
        Example
            Q = QQ[x,y,z];
            d = 3;
            K = randomLinearMF(d, Q, t) --way too many terms!
	    S = QQ[x,y]
	    K' = randomLinearMF(d, S, t)
            isdFactorization K'
///

-*
doc ///
    Key
        eulerChi
        (eulerChi, ZZdFactorization)
    Headline
        Calculates the Euler characteristic of a ZZdFactorization
    Usage
        eulerChi(ZZdFactorization)
    Inputs
        ZZdFactorization:ZZdFactorization
            A ZZdFactorization object representing a complex
    Outputs
        :ZZ
    Description
         Text
            Calculates the Euler characteristic of a ZZdFactorization. To be well-defined, the input should be a complex.
            This method calculates the Euler characteristic by computing
	    $$\ell_R (H_0 (C)) - \ell_R (H_1 (C))$$
	    where $C$ is some ZZ/2-graded complex. An easy way to construct such complexes is to take
	    the endomorphisms of a matrix factorization arising from an isolated singularity.
         Example
            S = ZZ/101[a..c]
	    C = koszulMF(a^3+b^3+c^3)
	    E = Hom(C,C)
	    prune HH E
	    eulerChi E
	    Q = ZZ/101[a,b,c,d]
	    D = koszulMF(a*b - c*d)
	    F = Hom(D,D)
	    eulerChi(F)
    Caveat
        In order to be well-defined, the input should be a complex (ie, the differential should square to 0).
    SeeAlso
        "Making ZZdFactorizations"
///
*-

doc ///
    Key
        (euler, ZZdFactorization)
    Headline
        Calculates the Euler characteristic of a ZZdFactorization
    Usage
        euler(ZZdFactorization)
    Inputs
        ZZdFactorization:ZZdFactorization
            A ZZdFactorization object representing a complex
    Outputs
        :ZZ
    Description
         Text
            Calculates the Euler characteristic of a ZZdFactorization. To be well-defined, the input should be a complex.
            This method calculates the Euler characteristic by computing
	    $$\ell_R (H_0 (C)) - \ell_R (H_1 (C))$$
	    where $C$ is some ZZ/2-graded complex. An easy way to construct such complexes is to take
	    the endomorphisms of a matrix factorization arising from an isolated singularity.
         Example
            S = ZZ/101[a..c]
	    C = koszulMF(a^3+b^3+c^3)
	    E = Hom(C,C)
	    prune HH E
	    euler E
	    Q = ZZ/101[a,b,c,d]
	    D = koszulMF(a*b - c*d)
	    F = Hom(D,D)
	    euler(F)
    Caveat
        In order to be well-defined, the input should be a complex (ie, the differential should square to 0).
    SeeAlso
        "Making ZZdFactorizations"
///

doc ///
    Key
        unfold
        (unfold, ZZdFactorization, List)
        (unfold, ZZdFactorization, ZZ)
	(unfold, ZZdFactorizationMap, List)
	(unfold, ZZdFactorizationMap, ZZ)
    Headline
        Converts a factorization into a complex
    Usage
        unfold(ZZdFactorization, L)
        unfold(ZZdFactorization, d)
    Inputs
        ZZdFactorization:ZZdFactorization
            A ZZdFactorization
        L:List
            A list specifying the endpoint homological degrees
        d:ZZ
            An integer representing the endpoint homological degree
    Outputs
        :Complex
    Description
        Text
            Converts a factorization into a complex, where the list specifies the endpoint homological degrees.
            This method converts a factorization into a complex by specifying the homological degrees.
	    If a single integer is provided, it takes homological endpoints from degrees 0 to d.
        Example
            S = ZZ/101[a..c];
	    C = koszulMF(a^3+b^3+c^3)
	    E = Hom(C,C)
	    Eu = unfold(E, {-3,3})
	    isWellDefined Eu
	    concentration Eu
	    prune HH_0 Eu == prune HH_0 E
	Text
	    If only one integer is specified, then the start point of the unfolded object is assumed
	    to be homological degree 0. Beware as well that unfolding a factorization of a
	    nonzero polynomial is not a well-defined complex.
	Example
	    unfold(E, 3)
	    Cu = unfold(C, 4)
	    assert not isWellDefined Cu
	Text
	    The @TO Fold@ and unfold functors are not inverses to each other and will in general
	    change the homology of a ZZ/d-graded complex.
	Example
	    E' = Fold(unfold(E,1), 2)
	    E'.dd_1 == E.dd_1
	    E'.dd_0 --the information of the 0th differential has been lost
	    Fold(unfold(E,2), 2)
	Text
	    This method is also functorial, applying to factorization maps with the same syntax.
	Example
	    Id = unfold(id_E, {-2,2})
	    Id == id_(source Id)
	    isCommutative Id
    SeeAlso
        Fold
	"Making ZZdFactorizations"
///


doc ///
    Key
        isZZdComplex
        (isZZdComplex, ZZdFactorization)
    Headline
        Checks if the differentials of a factorization compose to 0
    Usage
        isZZdComplex(ZZdFactorization)
    Inputs
        ZZdFactorization:ZZdFactorization
            A ZZdFactorization object representing a complex
    Outputs
        :Boolean
    Description
        Text
            This method evaluates whether all d-fold compositions of the differentials are equal to 0.
	    A simple way to obtain a ZZ/d-graded complex is to form the endomorphisms of a factorization:
        Example
            Q = ZZ/101[x_1..x_3];
	    F = randomTailMF(x_1^3+x_2^3 + x_3^3 , 2, 5, 2)
	    E = Hom(F,F)
	    isZZdComplex E
	    F' = linearMF(x_1^3 + x_2^3 , t)
	    E' = Hom(F', F')
	    isZZdComplex E'
	Text
	    Folding a complex is also another simple way to obtain a ZZ/d-graded complex:
	Example
	    K = koszulComplex vars Q
	    fK = Fold(K, 2)
	    isZZdComplex fK
	    Fold(K**K,4)
	    isZZdComplex oo
    SeeAlso
        "Making ZZdFactorizations"
	Fold
	randomTailMF
///


doc ///
    Key
        collapseMF
        (collapseMF, ZZdFactorization, ZZ)
    Headline
        Converts a d-fold factorization into a (d-1)-fold factorization by composing the kth differential with the (k+1)th differential
    Usage
        collapseMF(ZZdFactorization, ZZ)
    Inputs
        ZZdFactorization:ZZdFactorization
            A ZZdFactorization object representing a d-fold factorization
        ZZ:ZZ
            An integer representing the kth differential
    Outputs
        :ZZdFactorization
    Description
        Text
            This method collapses a d-fold factorization into a d-1-fold factorization by composing the kth differential with the (k+1)th differential. It then permutes the resulting factorization to ensure the correct ordering.
        Example
            Q = QQ[x_1..x_3];
	    C = ZZdfactorization {x_1,x_2,x_3}
	    C.dd
	    C1 = collapseMF(C, 1)
	    C1.dd
	    C2 = collapseMF(C, 2)
	    C2.dd
	    K = linearMF(x_1^4 + x_2^4, t)
	    collapseMF(K, 1)
	    oo.dd
	    collapseMF(K, 2)
	    oo.dd
	    collapseMF(K, 3)
	    oo.dd
///

doc ///
    Key
        trivialMF
        (trivialMF, ZZ, ZZ, Module, RingElement)
	(trivialMF, ZZ, ZZ, Module, ZZ)
	(trivialMF, ZZ, Module, RingElement)
	(trivialMF, Module, RingElement)
    Headline
        Constructs the trivial matrix factorization of an element f
    Usage
        trivialMF(d, i, M, f)
    Inputs
        d:ZZ
            The period of the desired factorization
	i:ZZ
	    The homological degree that the module M will appear in
        M:Module
            A module
        RingElement:RingElement
            An element in the ring of M
    Outputs
        :ZZdFactorization
    Description
        Text
            Constructs the trivial matrix factorization associated to M, which is the factorization:
	    $$ M \xrightarrow{1} M \xrightarrow{1} \cdots \xrightarrow{1} M \xrightarrow{f} M.$$
	    The user can specify using the index $i$ which differential should be multiplication
	    by the element $f$. If only one integer is specified, it is assumed that multiplication by
	    $f$ is the first differential in the factorization. If no integers are specified, then the output
	    is the length 2 factorization with first differential given by multiplication by $f$.
        Example
            Q = ZZ/101[a..c]
	    f = a^3 + b^3 + c^3
	    T = trivialMF(Q^3, f)
	    T.dd
	    isdFactorization T
	    isNullHomotopic id_T
	    nullHomotopy id_T
	    T1 = trivialMF(4, 2, Q^4, f)
	    T1.dd
    SeeAlso
        "Making ZZdFactorizations"
///

doc ///
    Key
        potential
        (potential, ZZdFactorization)
    Headline
        Outputs the polynomial $f$ such that the dth power of the differentials is equal to $ f \cdot \text{id}$
    Usage
        potential(C)
    Inputs
        C:ZZdFactorization
            A ZZ/d-graded factorization
    Outputs
        :RingElement
    Description
        Text
            This method determines the potential of the factorization $C$ by first checking if $C$ a well-defined factorization using the isdFactorization function.
	    If it is, it returns the potential \( f \); otherwise, it throws an error.
        Example
            Q = ZZ/101[a..d]
	    f = a^2+b^2+c^2+d^2
	    C = linearMF(f)
	    potential C
	    S = ZZ/101[a,b]
	    D = randomTailMF(a^3 + b^3, 2, 6, 3)
	    D.dd
	    D.dd^2
	    potential D
    SeeAlso
        "Making ZZdFactorizations"
///

doc ///
    Key
        projectiveCover
        (projectiveCover, ZZdFactorization)
    Headline
        Constructs the projective cover of a d-fold matrix factorization
    Usage
        projectiveCover(C)
    Inputs
        C:ZZdFactorization
            A ZZdFactorization object representing a d-fold matrix factorization
    Outputs
        :ZZdFactorizationMap
    Description
        Text
            This method constructs the projective cover of a d-fold matrix factorization. The output
	    is a ZZdFactorizationMap, which is the canonical surjection from the cover onto the
	    original factorization.
        Example
            Q = ZZ/101[a,b,c];
	    C = linearMF(a^3+b^3,t)
	    f = projectiveCover C
	    isCommutative f
	    prune coker f
	    prune ker f
	    oo.dd
	    D = koszulMF(a^3+b^3+c^3)
	    D.dd
	    g = projectiveCover D
	    isCommutative g
	    prune ker g
	    oo.dd
	Text
	    It turns out that the kernel of this projective when the period is 2 is isomorphic
	    to the shift of the original factorization. In general, the kernel of the projective
	    covering map gives a canonical method of defining the @TO suspension@ of a d-fold
	    factorization.
    SeeAlso
        suspension
	injectiveCover
///


doc ///
    Key
        injectiveCover
        (injectiveCover, ZZdFactorization)
    Headline
        Constructs the injective hull of a d-fold matrix factorization
    Usage
        injectiveCover(C)
    Inputs
        C:ZZdFactorization
            A ZZdFactorization object representing a d-fold matrix factorization
    Outputs
        :ZZdFactorizationMap
    Description
        Text
            This method constructs the injectice cover of a d-fold matrix factorization. The output
	    is a ZZdFactorizationMap, which is the canonical injection from the original factorization into the
	    injective cover.
        Example
            Q = ZZ/101[a,b,c];
	    C = linearMF(a^3+b^3,t)
	    f = injectiveCover C
	    isCommutative f
	    prune ker f
	    prune coker f
	    oo.dd
	    D = koszulMF(a^3+b^3+c^3)
	    D.dd
	    g = injectiveCover D
	    isCommutative g
	    F = prune coker g
	    oo.dd
	    F == D[1]
	Text
	    In this case, using the cokernel into the injective cover gives a factorization
	    that is directly equal to the shift of a 2-periodic factorization. In general the
	    cokernel of this inclusion natural defines a suspension functor on the category
	    of d-fold factorizations.
///

doc ///
    Key
        suspension
        (suspension, ZZdFactorization)
        (suspension, ZZ, ZZdFactorization)
        (suspension, ZZdFactorizationMap)
    Headline
        Constructs the suspension of a d-fold factorization
    Usage
        suspension(ZZdFactorization)
        suspension(ZZ, ZZdFactorization)
        suspension(ZZdFactorizationMap)
    Inputs
        ZZdFactorization:ZZdFactorization
            A ZZdFactorization object representing a d-fold factorization
        ZZ:ZZ
            An integer representing the dimension
        ZZdFactorizationMap:ZZdFactorizationMap
            A ZZdFactorizationMap object representing a map of factorizations
    Outputs
        :ZZdFactorization
    Description
        Text
	    The suspension of a d-fold factorization may be canonically defined by taking the cokernel
	    of the natural embedding of a factorization into its injective cover. When $d=2$, this recovers
	    the shift functor of a factorization, but when $d > 2$ this object is an indecomposable yet
	    nonminimal ZZ/d-graded factorization. 
        Example
           Q = ZZ/101[a,b,c];
	   D = koszulMF(a^3+b^3+c^3)
	   D.dd
	   suspension D
	   oo == D[1]
	   suspension(2, D) == D
	   C = linearMF(a^3+b^3,t)
	   sC = suspension(C)
	   sC.dd
	   sC.dd^3
	   suspension(3,C)
	   isdFactorization oo
    SeeAlso
        injectiveCover
	projectiveCover
        ker
        prune
///




doc ///
    Key
        randomTailMF
        (randomTailMF, RingElement, ZZ, ZZ, ZZ)
	(randomTailMF, RingElement, ZZ, ZZ)
	(randomTailMF, RingElement)
    Headline
        Construct a matrix factorization from a high syzygy of a random module over a hypersurface
    Usage
        randomTailMF(f, n, m, deg)
    Inputs
        f:RingElement
            A ring element representing a nonzerodivisor on the ambient ring
        n:ZZ
            \( n \): The bound controlling the size of the presentation of the module
	m:ZZ
            - \( m \): The bound controlling the size of the presentation of the module
	deg:ZZ
            - \( d \): The degree bound on the generators
    Outputs
        :ZZdFactorization
	    A matrix factorization built from a high syzygy of a random coker presented as the cokernel
	    of a map $R^m \to R^n$ with entries at most degree $d$.
    Description
        Text
            If no degree is specified, then a random list of degrees with values ranging from 1 to 10 is chosen.
	    If no parameters on the presentation matrix are specified, then random values between 1 and 10
	    are chosen. It is highly recommended that one specifies bounds when working over a ring with
	    more than 3 variables, since the computations may get out of hand if the presentation size
	    is randomly chosen to be too large.
	    This function is mostly meant as a helper function for generating interesting examples of
	    matrix factorizations en masse. Note that for certain parameters this function may take some time
	    to run, since the code will continue to generate examples until it obtains a well-defined
	    matrix factorization (if it does not do this, the ranks of the modules in resulting factorization
	    may not be equal).
        Example
            Q = ZZ/101[x_1..x_3]
            f = random(2,Q)
            C = randomTailMF(f, 4, 8, 5)
	    C.dd
	    C.dd^2
	    isWellDefined C
	    f == potential C
	    D = randomTailMF(f, 3, 5) --without specifying generator degrees
	    isdFactorization D 
	    S = ZZ/101[a,b,c];
	    g = a^3 + b^3 + c^3;
	    E = randomTailMF(g)  --without specifying presentation bounds or degree bounds
	    isdFactorization E
	Text
	    If the user does not care about whether the output is a well-defined factorization, use the option
	    {\tt WellDefined => false}.
	Example
	    E' = randomTailMF(g, 1, 2, 2, WellDefined => false)
    SeeAlso
        tailMF
///  

doc ///
    Key
        higherHomotopyFactorization
        (higherHomotopyFactorization, List, Complex)
	(higherHomotopyFactorization, RingElement, Complex)
    Headline
        Constructs the matrix factorization associated with a system of higher homotopies
    Usage
        higherHomotopyFactorization(L, C)
        higherHomotopyFactorization(f, C)
    Inputs
        L: List
            A list of ring elements, usually a regular sequence
        C: Complex
            A complex whose homology is annihilated by the elements of the list
        f: RingElement
            A single ring element annihilating the homology of C
    Outputs
        : ZZdFactorization
            The matrix factorization associated with the system of higher homotopies built from C, with
	    respect to the sequence of ring elements
    Description
        Text
            For each sequence of elements $f_1 , \dots , f_c \in S$ annihilating the homology of a complex,
	    there is an associated system of higher homotopies that can be used to construct a matrix
	    factorization of $f_1 t_1 + f_2 t_2 + \cdots + f_c t_c \in S[t_1, \dots , t_c]$. When $c= 1$,
	    this simply recovers a matrix factorization of $f = f_1$. This function uses existing
	    commands from the @TO CompleteIntersectionResolutions@ package.
        Example
            S = ZZ/101[x,y,z]
	    K = koszulComplex vars S
	    L = {x^2,y^2,z^2}
	    H = higherHomotopyFactorization(L, K)
	    H.dd
	    isdFactorization H
	    H' = higherHomotopyFactorization(x^3+y^3+z^3, K)
	    H'.dd
	Text
	    The terms of the higher homotopies can be accessed from the differentials. For instance,
	    each component of a higher homotopy is a map $C_i \to C_{i+t}$ for some odd integer $t$.
	    The user can access this component of the higher homotopies as follows:
	Example
	    dH0 = H'.dd_0;
	    dH1 = H'.dd_1;
	    dH0_[0]^[1] --the higher homotopy K_0 --> K_1
	    dH0_[2]^[3] --the higher homotopy K_2 --> K_3
	    dH0_[2]^[1] --the original differential of the Koszul complex
	    dH1_[1]^[2] --the higher homotopy K_1 --> K_2
	Text
	    Note that the construction of higher homotopies does not require that the starting
	    resolution is finite. One can attempt to build factorizations from any complex as long as the
	    ring elements annihilate the homology:
	Example
	    Q = ZZ/101[x,y]
	    K = koszulComplex vars Q;
	    C = complex {K.dd_1, map(source K.dd_1, target K.dd_2, K.dd_2*K.dd_1),  K.dd_2}
	    isWellDefined C
	    Cn = higherHomotopyFactorization(x^3+y^3, C)
	    Cn.dd
	    Cn.dd^2
	Text
	    This function can also be used to construct matrix factor
    SeeAlso
        "Making ZZdFactorizations"
	toBranchedCover
///

doc ///
    Key
        toBranchedCover
        (toBranchedCover, ZZdFactorization, Symbol)
	(toBranchedCover, ZZdFactorization, RingElement)
	zeroOutDegrees
	(zeroOutDegrees, Ring)
	(zeroOutDegrees, ZZdFactorization)
    Headline
        Converts a d-fold matrix factorization into a maximal Cohen-Macaulay module over a d-fold branched cover of the potential
    Usage
        toBranchedCover(C, z)
    Inputs
        C: ZZdFactorization
            The input d-fold matrix factorization
        z: Symbol
            The symbol representing the new variable in the branched cover
    Outputs
        :Matrix
	    A matrix, whose cokernel is a maximal Cohen-Macaulay module over the ring $R[z]/(z^d + f)$,
	    where $f$ is the potential of the original factorization.
    Description
        Text
            This method converts a d-fold matrix factorization into a d-fold branched cover of the potential,
            where the potential is the polynomial associated with the factorization.
            The Symbol z specifies the name of the new variable introduced in the branched cover.
            The input is assumed to be a well-defined factorization.
            In the following example, the zeroOutDegrees function is used to ensure that modules
	    over the resulting branched cover are well-behaved with respect to the pushforward
	    along the natural map $S \to S[z]/(z^d + f)$.
        Example
            S = zeroOutDegrees (ZZ/101[a..c])
	    C = koszulMF(a^3+b^3+c^3)
	    M = toBranchedCover(C,z)
	    liftM = sub(M, ambient ring M)
	    prune ker liftM == 0 --this implies that the cokernel is MCM over the hypersurface
	Text
	    This method works for factorizations of arbitrary period:
	Example
	    D = linearMF(a^4 + b^4, t)
	    N = toBranchedCover(D, z)
	    liftN = sub(N, ambient ring N)
	    prune ker liftN == 0 --again, must be MCM
	Text
	    The user can convert the MCM module over a branched cover back into a factorization
	    using the @TO branchedToMF@ function. Composing these functors yields a folded version
	    of the original factorization (with a negative sign), an insight originally noted by Knorrer.
	Example
	    bC = branchedToMF(M, S) --specifying S substitutes back into the original ring
	    bC.dd
	    C.dd
	    isdFactorization bC
	    bD = branchedToMF(N, ring D)
	    bD.dd --notice the block structure
	    D.dd
	    isdFactorization bD
    SeeAlso
        "Making ZZdFactorizations"
	branchedToMF
        higherHomotopyFactorization
///


doc ///
    Key
        branchedToMF
	(branchedToMF, Module)
	(branchedToMF, Module, Ring)
	(branchedToMF, Matrix)
	(branchedToMF, Matrix, Ring)
    Headline
        Converts a maximal Cohen-Macaulay module over a d-fold branched cover into a d-fold factorization
    Usage
        branchedToMF(M)
	branchedToMF(M, R)
    Inputs
        M: Module
            The input module, a MCM over the branched cover $S[z]/(z^d + f)$
    Outputs
        :ZZdFactorization
	    A ZZ/d-graded factorization with potential $-f$.
    Description
        Text
            This method converts a maximal Cohen-Macaulay module over a d-fold branched cover
	    into a ZZ/d-graded factorization. This method is designed to try to order the rows and
	    columns of the matrices in such a way that one can deduce the decomposable block structure,
	    should one exist. We first illustrate how to obtain a factorization from a high syzygy
	    of a module over a branched cover.
	    In the following, the zeroOutDegrees function is used to ensure that modules
	    over the resulting branched cover are well-behaved with respect to the pushforward
	    along the natural map $S \to S[z]/(z^d + f)$.
        Example
            S = zeroOutDegrees (ZZ/101[a..c]) 
            f = a^3 + b^3 + c^3;
	    bS = S[z]/(z^2 + f)
	    F = freeResolution(ideal(a,b,c,z), LengthLimit => 4)
	    C = branchedToMF F.dd_4
	    isdFactorization C
	    C.dd
	    K = higherHomotopyFactorization(f, koszulComplex vars S)
	    K.dd
	Text
	    In the above example, we see that the factorization obtained by taking a high
	    syzygy of the residue field over the branched cover is remarkably similar to
	    a fold of the factorization obtained by taking a high syzygy of the residue field
	    over the original ring. We can obtain other examples of factorizations by taking
	    other modules:
	Example
	    F2 = freeResolution(trim (ideal(a,b,c,z))^2, LengthLimit => 4)
	    C2 = branchedToMF F2.dd_4
	    C2.dd
	    M = toBranchedCover(C,z)
	    liftM = sub(M, ambient ring M)
	    prune ker liftM == 0 --this implies that the cokernel is MCM over the hypersurface
	Text
	    This method works for d-fold factorizations with d > 2 in the same way:
	Example
            Q = adjoinRoot(4, zeroOutDegrees (ZZ/101[a..b]), t)
	    g = a^4+b^4
	    bQ = Q[z]/(z^4 + g)
	    G = freeResolution(ideal(a,b,z), LengthLimit => 3)
	    D = branchedToMF G.dd_3
	    D.dd
	Text
	    In this case, we obtain something that looks like a fold of the Koszul factorization of the potential, but padded with
	    additional identity maps so that the resulting period is 4.
    SeeAlso
        "Making ZZdFactorizations"
	toBranchedCover
	zeroOutDegrees
        higherHomotopyFactorization
///






doc ///
    Key
        mooreMF
        (mooreMF, ZZ)
    Headline
        Constructs the matrix factorization of the Moore matrix and its adjoint
    Usage
        mooreMF(p)
    Inputs
        p: ZZ
            The characteristing of the underlying field (p = 0 for rational coefficients)
    Outputs
        : ZZdFactorization
            The Moore matrix factorization
    Description
        Text
            This method constructs the Moore matrix factorization corresponding in some characteristic
	    specified by the integer p. 
        Example
            F = mooreMF(0)
	    F.dd
	    potential F
	    E = Hom(F,F)
	    prune HH_1 E
	    prune HH_0 E
	    netList (ann oo)_* --recover entries of 0th differential
    SeeAlso
        rk1MCM2gen
	adjointFactorization
	"Preprogrammed examples and operations"
///

doc ///
    Key
        rk1MCM2gen
        (rk1MCM2gen, List, ZZ)
	(rk1MCM2gen, List)
    Headline
        Construct every possible rank 1, 2-generated maximal Cohen-Macaulay module over the cubic Fermat hypersurface in 4 variables
    Usage
        rk1MCM2gen(L,d)
    Inputs
        L:List
	    any permutation of the set {2,3,4}
	d:ZZ
	    any integer specifying the characteristic of the ambient field
    Outputs
        :ZZdFactorization
	    A matrix factorization corresponding to a rank 1, 2-generated maximal Cohen-Macaulay module over
	    the cubic Fermat hypersurface in 4 variables.
    Description
        Text
            This method constructs every possible rank 1, 2-generated maximal Cohen-Macaulay module
	    (up to isomorphism) over the cubic Fermat hypersurface in 4 variables. It is known that such
            MCM modules are parametrized by all permutations of $\{ 2, 3 ,4 \}$.
        Example
            H = hashTable for i in permutations {2,3,4} list i => rk1MCM2gen(i,0);
	    H#{2,3,4}.dd
	    H#{3,2,4}.dd
	    H#{2,4,3}.dd
	    H#{4,3,2}.dd
	    H#{4,2,3}.dd
	    H#{3,4,2}.dd
	    for i in permutations {2,3,4} list potential H#i
    SeeAlso
        mooreMF
        adjointFactorization
	"Preprogrammed examples and operations"
///

-*
doc ///
    Key
        rk1MCM3gen
        (rk1MCM3gen, ZZ, ZZ)
	(rk1MCM3gen, ZZ)
    Headline
        Constructs a rank 1, 3-generated maximal Cohen-Macaulay module
    Usage
        rk1MCM2gen(p, d)
    Inputs
        p: List
            Specifies characteristic of base field (assumes QQ if d is not specified)
        d: ZZ
            an integer, determining the type of factorization
    Outputs
        : ZZdFactorization
            The constructed factorization that determines a rank 1, 3-generated maximal Cohen-Macaulay module
    Description
        Text
            This method constructs a rank 1, 3-generated maximal Cohen-Macaulay module based on the input parameters specified by a list.
            The integer d specifies the characteristic of the base field (default is 0, representing QQ).
            It generates the module based on the input parameters and returns the corresponding factorization.
        Example
            F = rk1MCM2gen({2,2,2},0)
	    diffs = F.dd
	    diffs^2
	    F' = rk1MCM2gen({1,2,4},0)
	    isdFactorization(F')
    SeeAlso
        mooreMF
	rk1MCM2gen
	classicalAdjoint
	"Preprogrammed examples and operations"
///
*-



doc ///
    Key
        adjointFactorization
	(adjointFactorization, Matrix)
    Headline
        Constructs matrix factorization of a determinant of a matrix
    Usage
        adjointFactorization M
    Inputs
        M: Matrix
	    A square matrix
    Outputs
        : ZZdFactorization
            The matrix factorization of the determinant of M determined by the adjoint matrix
    Description
        Text
            Given a matrix $M$, the adjoint matrix $M'$ is the matrix satisfying $M \cdot M' = M' \cdot M = \det(M) id$.
	    This means that any square matrix induces a matrix factorization of its determinant, and this
	    function implements this induced matrix factorization.
        Example
            Q = QQ[x_(1,1)..x_(3,3)]
	    M = genericMatrix(Q,3,3)
	    F = adjointFactorization M
	    F.dd
	    potential F == determinant M
    SeeAlso
        mooreMF
        rk1MCM2gen
	"Preprogrammed examples and operations"
///


doc ///
    Key
        fullCollapse
        (fullCollapse, ZZdFactorization, ZZ, ZZ)
    Headline
        Converts a d-fold factorization into a 2-fold factorization
    Usage
        fullCollapse(X, n, r)
    Inputs
        X: ZZdFactorization
            The input d-fold factorization
        n: ZZ
            Starting position for composing maps
        r: ZZ
            Number of maps to compose
    Outputs
        : ZZdFactorization
            The resulting 2-fold factorization
    Description
        Text
            This method converts a d-fold factorization into a 2-fold factorization by composing the maps in the specified positions.
            It starts composing maps from position n and continues composing r maps.
            The resulting 2-fold factorization is returned.
        Example
	    Q = ZZ/101[a,b];
	    C = linearMF(a^4+b^4, t)
	    C.dd
	    C.dd^4
	    C' = fullCollapse(C,2,1)
	    isdFactorization C'
	    E = Hom(C,C)
	    prune HH_1 fullCollapse(E,2,1)
    SeeAlso
        "Making ZZdFactorizations"
///


--small MF
-*
matrix {{-a+35*c+17*d, b+17*c-35*d, -9*c^2+39*c*d-45*d^2, 3*c^2-13*c*d+15*d^2}, {b-17*c+35*d,
      a+35*c+17*d, 15*c^2+36*c*d-26*d^2, -3*c^2+13*c*d-15*d^2}, {0, 0, -17*b+46*c+25*d,
      a+4*b-45*c+33*d}, {0, 0, a-4*b+45*c-33*d, b+35*c+17*d}}

matrix {{-a-35*c-17*d, b+17*c-35*d, 3*c^2-13*c*d+15*d^2, -9*c^2+39*c*d-45*d^2}, {b-17*c+35*d,
      a-35*c-17*d, 3*c^2-13*c*d+15*d^2, -15*c^2-36*c*d+26*d^2}, {0, 0, -b-35*c-17*d, a+4*b-45*c+33*d},
      {0, 0, a-4*b+45*c-33*d, 17*b-46*c-25*d}}

  matrix {{-a+35*c+17*d, b+17*c-35*d}, {b-17*c+35*d, a+35*c+17*d}}

  matrix {{-a-35*c-17*d, b+17*c-35*d}, {b-17*c+35*d, a-35*c-17*d}}
*-
