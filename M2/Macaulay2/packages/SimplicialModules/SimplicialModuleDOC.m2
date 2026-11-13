doc ///
    Key
        SimplicialModules
    Headline
        A package for creating and computing objects in the category of Simplicial R-Modules
    Description
        Text
            A simplicial R-module is a presheaf on the so-called Simplex Category, with values in the category of R-modules. Concretely, such objects can be viewed as nonnegatively graded R-modules
	    equipped with certain face and degeneracy operators satisfying the simplicial identities. As an example, every R-module M can be converted into a simplicial R-module whose degree
	    n piece is equal to M for all n, and with face/degeneracy operators simply given by the identity.
        Example
            S = simplicialModule(ZZ^2,3,Degeneracy => true) --the integer 3 specifies a top degree, the option Degeneracy specifies whether or not to compute degeneracy maps
	Text
	    The output string for a simplicial module is meant to indicate that this object is infinite in general,
	    and the user can only compute a finite snapshot of the object.
	    The face/degeneracy maps can be accessed using the keys {\tt dd} and {\tt ss}, respectively. In order to verify that the resulting face/degeneracy maps satisfy the simplicial
	    identities, one can use the @TO isSimplicialModule@ command.
	Example
	    S.dd
	    S.ss
	    assert isSimplicialModule S
	Text
	    In general, simplicial objects are infinite objects. Because of this, the user can specify a top degree for the resulting simplicial object. If no top degree is specified, then
	    the default top degree is determined by the input object.
        Text
	    @SUBSECTION "The Dold-Kan Correspondence"@
	Text
	    The category of simplicial R-modules is equivalent to the category of nonnegatively graded
	    chain complexes via an equivalence known as the Dold-Kan correspondence. 
	    
	    This means that there is a functor that converts a chain complex into a simplicial object, known as the Dold-Kan functor. In practice, the image of the Dold-Kan functor is
	    highly nontrivial to compute by hand. However, this package uses an algorithm of Kock-Sakuranath to compute Dold-Kan images quite efficiently by using the @TO simplicialModule@
	    command:
	Example
	    R = ZZ/101[x_1..x_3];
	    K = koszulComplex vars R
	    simplicialModule(K) --defaults to top degree 3
	    elapsedTime simplicialModule(K,6) --specify top degree 6
	    elapsedTime S' = simplicialModule(K,6, Degeneracy => true)
	    isWellDefined S'  --this method checks the simplicial identities as well as many other basic checks
	Text
	    The other piece of the Dold-Kan correspondence is the functor that converts a simplicial module into a chain complex. This functor is often referred to as the normalization functor,
	    and is also implemented as the command @TO normalize@. If a simplicial R-module is computed as the Dold-Kan image of some complex, then this complex is cached and the @TO normalize@ command
	    returns this cached complex. If the user wants the normalization to be computed without accessing this cached value, one can use the {\tt CheckComplex => false} option.
	    By default, the normalization function does not prune the output, so the user should use @TO prune@ to get a nicer looking output.
	Example
	    R = ZZ/101[x_1..x_3];
	    K = koszulComplex vars R
	    Kn = normalize(simplicialModule(K,4), CheckComplex => false)
	    Kn.dd
	    K == prune Kn
	Text
	    As the above example shows, applying the normalization to the Dold-Kan image recovers the original complex.
	Text
	    @SUBSECTION "Canonical Extensions of Functors to the Category of Chain Complexes"@
	Text
	    One of the main utilities of the Dold-Kan correspondence from the perspective of a commutative algebraist is as a method of canonically extending any
	    endofunctor of R-modules to an endofunctor of nonnegatively chain complexes. This is because endofunctors of R-modules naturally extend to endofunctors
	    of simplicial R-modules by just applying the functor degree-wise. Since any nonnegatively graded chain complex can be converted into a simplicial R-module
	    (and vice versa) this yields a canonical way to extend endofunctors of R-modules to chain complexes in a way that preserves homotopy equivalence. 
	    
	    This extension is often called the Dold-Puppe extension, and was introduced in pioneering work on the theory of derived nonlinear functors.
	    In general, the Dold-Puppe extension of a functor looks quite different from the classically defined functor, and in most cases may not be quasi-isomorphic over arbitrary base rings!
	Example
	    Q = ZZ/101[x_1,x_2];
	    K1 = complex {matrix{{x_1}}};
	    K2 = complex {matrix{{x_2}}};
	    T1 = K1**K2
	    T1.dd
	    T2 = prune simplicialTensor({K1,K2})
	    T2.dd
	    phi1 = extend(T1,T2,id_(T1_0))
	    phi2 = extend(T2,T1,id_(T1_0))
	    phi1*phi2 == id_T1
	    isNullHomotopic(phi2*phi1 - id_T2)
	Text
	    The above example allows us to directly verify that the classically defined tensor product is homotopy equivalent to the Dold-Kan version. This is true over arbitrary base rings,
	    but there are other functors such as the symmetric/exterior power functors that have classical definitions for chain complexes that are not necessarily
	    homotopy equivalent to the Dold-Puppe extensions:
	Example
	    needsPackage "ChainComplexOperations"
	    Q = ZZ/2[x_1..x_3];
	    K = koszulComplex vars Q;
	    W1 = wedge2 K --the classical "naively" defined exterior power functor
	    W2 = prune extPower(2,K) --the simplicial version of the exterior power functor
	    prune HH W1
	    prune HH W2
	Text
	    Notice that the two definitions of exterior power yield complexes that are not even quasi-isomorphic, and hence certainly not homotopy equivalent. Moreover, the "naive"
	    definition yields a complex that does not even have finite length homology, while the Dold-Puppe extension applied to a complex with finite length homology is guaranteed
	    to have finite length homology. This preservation of "good behavior" for Dold-Puppe extensions of functors was the main motivation of the authors of CITE for using
	    simplicial techniques to prove the Total Rank Conjecture in characteristic 2. The @TO extPower@ command along with Schur functors in general have also been implemented in a 
	    functorial way and can take morphisms of chain complexes as inputs:
	Example
	    Q=ZZ/101[x_1,x_2];
	    K = koszulComplex vars Q;
	    F = complex res ((ideal vars Q)^2);
	    phi = extend(K,F,id_(K_0))
	    e2phi = prune extPower(2,phi) --induced map on the exterior powers of these complexes
	    assert isCommutative e2phi
	    ephi = prune extPower(3,phi,TopDegree => 4); --the TopDegree option specifies how many homological degrees to compute
	    assert isCommutative ephi;
	    ephi_1
	    ephi_2;
	    prune schurMap({2},phi) --the second symmetric power
	    sphi = schurMap({2,1},phi,TopDegree => 3);  --schur functor corresponding to partition {2,1} applied to phi
	    sphi_1
	Text    
	    This package thus lays the groundwork for computing with Dold-Puppe extensions of nonlinear functors, deriving nonlinear functors, and also allows any endofunctor of R-modules
	    implemented in Macaulay2 to immediately be canonically extended to an endofunctor at the level of chain complexes. Moreover, almost all methods
	    are implemented with an eye toward user accessibility and comprehension of the outputs. This is illustrated
	    in many of the documentation nodes below.
	Text
	    @SUBSECTION "Getting started:"@
	Text
	    @UL {
                TO "The Dold-Kan Correspondence in Macaulay2",
                TO "Making simplicial modules",
                TO "Making maps between simplicial modules",
                TO "Basic invariants and properties",
		TO "face/degeneracy maps of a simplicial module"
            }@
	Text
	    @SUBSECTION "Contributors"@
	Text
	    The following people have generously contributed code or improved existing code:
	Text
	    @SUBSECTION "Acknowledgments"@
	Text
	    This package began its life during the 2023 Macaulay2 workshop in Minneapolis. Many documentation
	    nodes for the basic constructors not specific to the {\tt SimplicialModule} type have been
	    repurposed from existing documentation coming from the @TO Complexes@ package.
///


doc ///
    Key
        SimplicialModule
	complexLength
	Degeneracy
    Headline
        the class of all simplicial modules
    Description
        Text
            A simplicial R-module is a presheaf on the so-called Simplex Category, with values in the category of R-modules. Concretely, such objects can be viewed as nonnegatively graded R-modules
	    equipped with certain face and degeneracy operators satisfying the simplicial identities. As an example, every R-module M can be converted into a simplicial R-module whose degree
	    n piece is equal to M for all n, and with face/degeneracy operators simply given by the identity.
        Example
            S = simplicialModule(ZZ^2,3,Degeneracy => true) --the integer 3 specifies a top degree, the option Degeneracy specifies whether or not to compute degeneracy maps
	Text
	    The output string for a simplicial module is meant to indicate that this object is infinite in general,
	    and the user can only compute a finite snapshot of the object.
	    The face/degeneracy maps can be accessed using the keys {\tt dd} and {\tt ss}, respectively. In order to verify that the resulting face/degeneracy maps satisfy the simplicial
	    identities, one can use the @TO isSimplicialModule@ command.
	Example
	    keys S
    SeeAlso
        "The Dold-Kan Correspondence in Macaulay2"
	"Making simplicial modules"
///


doc ///
    Key
        "The Dold-Kan Correspondence in Macaulay2"
        (simplicialModule, Complex, ZZ)
	(simplicialModule, Complex)
	summandSurjection
    Headline
        compute the image of a non-negatively graded complex under the Dold-Kan correspondence
    Usage
	simplicialModule(C,d)
	simplicialModule(C)
    Inputs
        C : Complex
        d : ZZ
	    an integer specifying the top degree of the output SimplicialModule (if d is not specified, the top degree is the length of the complex C)
    Description
        Text
            Given any non-negatively graded chain complex, the Dold-Kan correspondence is an equivalence of
	    categories:
	    $$\text{Ch}_{\geq 0} (R) \leftrightarrow \text{Simplicial R-modules}.$$
	    If the simplicial module is obtained as a Dold-Kan image of some complex $C$,
            then for each $i$ there is a decomposition
            $$S_i = C_0 \oplus C_1^{\binom{i}{1}} \oplus \cdots \oplus C_j^{\binom{i}{j}} \oplus \cdots.$$
            To be technically correct, each of the direct summands $C_j$ of $S_i$ should be thought of as being
	    parametrized by an order preserving surjection $f : [i] \to [j]$ (where $[n] := \{ 0 ,\dots  , n \}$). To deduce which surjection
	    corresponds to a given summand, first notice that order preserving surjections $f : [i] \to [j]$
	    are in bijection with compositions of $i+1$ into $j+1$ parts by just listing the sizes of the
	    fibers of the map $f$. For instance, the composition $(2,2,1)$ corresponds to the surjection
	    $f : [4] \to [2]$ defined via
	    $$f(0) = f(1) = 0, \quad f(2) = f(3) = 1, \quad f(4) = 2.$$
	    With this in mind, one can deduce the surjection corresponding to a summand as follows:
	Example
	    R = ZZ/101[a..c]
	    C = koszulComplex vars R
	    S = simplicialModule(C, 4, Degeneracy => true)
            S_2
	    components S_2 --these are all the modules showing up
	    S_(2,1)
	    components S_(2,1) --these are only the components of C_1^2
	    sort(select(compositions(2,3), i -> all(i, j -> j>0)))
	Text
	    The ordering of the surjections corresponding to a summand agrees with the lexicographic
	    ordering of compositions via the bijection mentioned above. Thus the first summand of $C_1$ appearing
	    in $S_2$ corresponds to the surjection $f : [2] \to [1]$ given by $f(0) = 0$ and $f(1) = f(2) = 1$,
	    and the second summand corresponds to the surjection $f: [2] \to [1]$ with $f(0) = f(1) = 0$ and $f(2) = 1$.
	    These surjections may be accessed more directly using the {\tt summandSurjection} command:
	Example
	    summandSurjection(2,0) --the surjection parametrizing C_0
	    summandSurjection(2,1) --the surjections parametrizing C_1
	    summandSurjection(2,2) --the surjection parametrizing C_2
	Text
	    Notice that the individual terms $C_j^{\binom{i}{j}}$ may be accessed as the $(i,j)$-component
            of the simplicial module. There are moreover explicit formulas for the face and degeneracy
	    maps in terms of the differentials of $C$. These can also be accessed:
	Example
	    S.dd --face maps
	    S.ss --degeneracy maps
	    S.dd_(2,0)
        Text
	    If you want to restrict/project a face/degeneracy map to a particular summand group, this can be done
	    using the array notation. The $k$-th summand group of $S_i$ corresponds to $C_k^{\binom{i}{k}}$:
	Example
	    (S.dd_(2,0))_[1]^[0]  --restrict to C_1 summand group of S_2, project onto C_0 summand of S_1
	    (S.dd_(2,0))_[1]^[1]  --restrict to C_1 summand group of S_2, project onto C_1 summand of S_1
	    (S.dd_(2,0))_[2]^[1]  --restrict to C_2 summand of S_2, project onto C_1 summand of S_1
        Text
	    The first computation tells us that the component of the face map $d_{2,0}$ mapping
	    $$C_1^{\oplus 2} \to  C_0$$
	    is given by the differential of the original complex $C$ applied to the first copy of $C_1$
	    (we use the compositions corresponding to the surjections to label the free modules).
	    The second computation shows the component $C_1^{\oplus 2} \to C_1$
	    is simply the identity map on the second copy of $C_1$.
	    The third computation shows the component
	    $$C_2 \to C_1$$
	    is also given by the differential of $C$.
    SeeAlso
        "Making simplicial modules"
	"Making maps between simplicial modules"
///


doc ///
    Key
        "Making simplicial modules"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (simplicialModule, HashTable, HashTable, HashTable, ZZ),
                TO (simplicialModule, Module, ZZ),
                TO (symbol SPACE, SimplicialModule, Array),
                TO (isWellDefined, SimplicialModule)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new simplicial modules"@
	Text
    	    @UL {
                TO (simplicialModule, Complex),
                TO (simplicialTensor, Complex, Complex)
            }@
    	Text
    	    @SUBSECTION "More advanced constructors"@
	Text
    	    @UL {
                TO (symbol++, SimplicialModule, SimplicialModule),
                TO (symbol**, SimplicialModule, SimplicialModule),
                TO (symbol SPACE, RingMap, SimplicialModule),
                TO (symbol **, RingMap, SimplicialModule),
                TO (minimalPresentation, SimplicialModule),
                TO (truncate, List, SimplicialModule)
            }@
    	Text
    	    @SUBSECTION "Extracting simplicial modules from simplicial maps"@
        Text
    	    @UL {
                TO (source, SimplicialModuleMap),
                TO (target, SimplicialModuleMap),
                TO (kernel, SimplicialModuleMap),
                TO (cokernel, SimplicialModuleMap),
                TO (image, SimplicialModuleMap),
                TO (coimage, SimplicialModuleMap)
            }@
    SeeAlso
        "Making maps between simplicial modules"
        "Basic invariants and properties"
///

doc ///
    Key
        "Basic invariants and properties"
    Headline
        information about accessing basic features
    Description
    	Text
    	    @SUBSECTION "Predicates for simplicial modules and simplicial module maps"@
        Text
    	    @UL {
                TO (isWellDefined, SimplicialModule),
                TO (isWellDefined, SimplicialModuleMap),
                TO (isCommutative, SimplicialModuleMap),
                TO (isSimplicialMorphism, SimplicialModuleMap),
                TO (isShortExactSequence, SimplicialModuleMap, SimplicialModuleMap)
            }@
    	Text
    	    @SUBSECTION "Other invariants for simplicial modules"@
        Text
    	    @UL {
                TO (ring, SimplicialModule),
                TO (topDegree, SimplicialModule),
                TO (components, SimplicialModule)
            }@
    	Text
    	    @SUBSECTION "Other invariants for simplicial module maps"@
        Text
    	    @UL {
                TO (source, SimplicialModuleMap),
                TO (target, SimplicialModuleMap),
                TO (degree, SimplicialModuleMap),
                TO (ring, SimplicialModuleMap),
                TO (isHomogeneous, SimplicialModuleMap),
                TO (components, SimplicialModuleMap)
            }@
    SeeAlso
        "Making simplicial modules"
        "Making maps between simplicial modules"
///



doc ///
    Key
        (ring, SimplicialModule)
        (ring, SimplicialModuleMap)
    Headline
        access the ring of a simplicial module or a simplicial module map
    Usage
        ring C
    Inputs
        C:SimplicialModule
            or a @TO "SimplicialModuleMap"@
    Outputs
        :Ring
    Description
        Text
            Every simplicial module or simplicial module map has a base ring.  This
            function accesses that information.
        Example
            S = ZZ/101[a,b,c,d];
            C = simplicialModule freeResolution coker vars S
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
        topDegree
        (topDegree, SimplicialModule)
	(topDegree, SimplicialModuleMap)
    Headline
        top degree of a simplicial module
    Usage
        topDegree C
    Inputs
        C:SimplicialModule
    Outputs
        d:ZZ
            an integer specifying the top degree to which the
	    face/degeneracy maps of a simplicial module have been computed
    Description
        Text
            In this package, each simplicial module necessarily has a top degree which is a
	    finite bound up to which Macaulay2 will compute the face and/or degeneracy maps.
	    This function is mainly used in programming, to loop over all
            non-zero modules or maps in the simplicial module.
        Example
            S = ZZ/101[a..c];
            C = simplicialModule(freeResolution coker vars S, 8)
            topDegree C
            C' = simplicialModule(freeResolution coker vars S, 6)
	    assert(topDegree C' == 6)
        Text
            Indices that are outside of the top degree automatically
            return the zero object.
        Example
            C_-1
	    C_9
            C'_7
        Text
            The function {\tt topDegree} does no computation, and just accesses
	    a cached value.
    SeeAlso
        "Basic invariants and properties"
        (symbol _, SimplicialModule, ZZ)
///


doc ///
    Key
        simplicialModule
        (simplicialModule, HashTable, HashTable, HashTable, ZZ)
	(simplicialModule, HashTable, HashTable, ZZ)
	(simplicialModule, Complex, HashTable, HashTable, ZZ)
	(simplicialModule, Complex, HashTable, ZZ)
    Headline
        make a simplicial module
    Usage
         simplicialModule(H1, H2, H3, d)
    Inputs
        H1:HashTable
            or @TO Complex@, used to determine the modules in each degree
	H2:HashTable
	    used to specify the face maps
	H3:HashTable
	    optional, used to specify degeneracy maps if needed
	d:ZZ
	    an integer, specifying the top degree to compute the simplicial module up to
	Degeneracy => Boolean
	    option specifying whether or not to also compute the degeneracy maps
    Outputs
        :SimplicialModule
    Description
        Text
            A simplicial module is a sequence of objects (e.g. modules),
            connected by maps called face/degeneracy maps denoted by $d$ and $s$, respectively.
	    These maps satisfy the simplicial identities:
            1. For face maps:
                \[ d_j d_i = d_i d_{j-1} \text{ for } 0 \leq i < j \leq n \]
            2. For face and degeneracy maps:
                \[ d_i s_j = s_{j-1} d_i \text{ for } i < j \]
                \[ d_j s_j = \text{id} \]
                \[ d_{j+1} s_j = \text{id} \]
                \[ d_k s_j = s_j d_{k-1} \text{ for } k > j+1 \]
            3. For degeneracy maps:
                \[ s_j s_i = s_i s_{j+1} \text{ for } i \leq j \]

            This constructor is the most basic constructor for building a simplicial module,
	    and is called by all of the more user friendly constructors. It is highly recommended
	    that the user sees @TO (simplicialModule, Complex)@ to quickly build simplicial modules.
        Example
            S = ZZ/101[a..d]
            moduleHash = hashTable { 0 => S^1,
		                     1 => S^1++S^2,
				     2 => S^1++S^2++S^2++S^1}
	    faceHash = hashTable {(1,0) => matrix {{1, a, b}},
		                  (1,1) => matrix {{1_S, 0, 0}},
				  (2,0) => matrix {{1, a, b, 0, 0, 0},
				                  {0, 0, 0, 1, 0, -b},
						  {0, 0, 0, 0, 1, a}},
				  (2,1) => matrix {{1_S, 0, 0, 0, 0, 0},
				                   {0, 1, 0, 1, 0,0},
				                   {0, 0, 1, 0, 1, 0}},
				  (2,2) => matrix {{1_S, 0, 0, 0, 0, 0},
				                   {0, 1, 0, 0, 0, 0},
						   {0, 0, 1, 0, 0, 0}}}
	    degenHash = hashTable {(0,0) => matrix {{1_S}, {0}, {0}},
		                   (1,0) => matrix {{1_S, 0, 0},
				                    {0, 0, 0},
						    {0, 0, 0},
						    {0, 1, 0},
						    {0, 0, 1},
						    {0, 0, 0}},
				   (1,1) => matrix {{1_S, 0, 0},
				                    {0, 1, 0},
						    {0, 0, 1},
						    {0, 0, 0},
						    {0, 0, 0},
						    {0, 0, 0}}}
	    T = simplicialModule(moduleHash, faceHash, degenHash, 2)
	    T.module
	    T.dd
	    T.ss
	    T' = simplicialModule(moduleHash, faceHash, 2)
	    T'.?ss
	Text
	    In the above, notice that if the user does not provide a hash table
	    specifying the degeneracy maps, then the simplicial module is still constructed
	    using only the data of the face maps. This feature is intentional since for the
	    purposes of efficiency, storing the data of the degeneracy maps may slow down
	    computations (and one can compute normalizations using only the face maps). In general,
	    the method @TO forgetDegeneracy@ allows the user to ignore the data of the degeneracy maps
	    if needed.
        Text
	    In the following example, we see that one can construct simplicial modules
	    that are not well-defined. The user should use @TO (isWellDefined, SimplicialModule)@
	    in order to check that a simplicial module is indeed well-defined.
	Example
	    H1 = hashTable {0 => S^1, 1 => S^1, 2 => S^1}
	    H2 = hashTable {(1,0) => map(S^1, S^1, 0),
		            (1,1) => map(S^1, S^1, 0),
			    (2,0) => map(S^1, S^1, 0),
			    (2,1) => map(S^1, S^1, 0),
			    (2,2) => map(S^1, S^1, 0)}
	    H3 = hashTable {(0,0) => map(S^1, S^1, 0),
		            (1,0) => map(S^1, S^1, 0),
			    (1,1) => map(S^1, S^1, 0)}
	    U = simplicialModule(H1,H2,H3,2)
	    U.dd
	    U.ss
	    isWellDefined U
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, SimplicialModule)@.
    SeeAlso
        "Making simplicial modules"
        (isWellDefined, SimplicialModule)
        (simplicialModule, HashTable, HashTable, HashTable, ZZ)
        (simplicialModule, Module, ZZ)
        (symbol SPACE, SimplicialModule, Array)
///


doc ///
    Key
        (simplicialModule, Module, ZZ)
        (simplicialModule, Ideal, ZZ)
        (simplicialModule, Ring, ZZ)
    Headline
        make a simplicial module associated to a complex concentrated in degree 0
    Usage
        simplicialModule(M, d)
    Inputs
        M:Module
            or @TO "Ideal"@, or @TO "Ring"@.
        d:ZZ
	    top degree to compute the simplicial module up to
    Outputs
        :SimplicialModule
            returns the complex whose 0-th component is {\tt M}.
    Description
        Text
            In contrast to @TO (simplicialModule,HashTable,HashTable,HashTable,ZZ)@ or @TO
            (simplicialModule,Complex,ZZ)@, this constructor provides a convenient
            method to construct a simplicial module from a single module/ring/ideal.
      
            We illustrate this with a free module.
        Example
            S = ZZ/101[a..d]
            C0 = simplicialModule( S^2, 6, Degeneracy => true)
            f = dd^C0
            source f, target f
            f == 0
            isWellDefined C0
            C0 == 0
            topDegree C0
        Example
            C1 = simplicialModule(complex(S^2, Base=>3), 6, Degeneracy => true)
            C1_3
            C1_0
        Text
            A ring or an ideal will be converted to a module first.
        Example
            C2 = simplicialModule( S, 5, Degeneracy => true)
            I = ideal(a^2-b, c^3)
            C3 = simplicialModule( I, 7, Degeneracy => true)
            C4 = simplicialModule( (S/I), 8, Degeneracy => true)
            (ring C3, ring C4)
        Text
            The zero simplicial module over a ring {\tt S} is most conveniently
            created by giving the zero module.
        Example
            C5 = simplicialModule(S^0, 8, Degeneracy => true)
            C5 == 0
            dd^C5 == 0
	    ss^C5 == 0
            C5_0
    SeeAlso
        "Making simplicial modules"
        (isWellDefined, SimplicialModule)
        (simplicialModule, HashTable, HashTable, HashTable, ZZ)
///

doc ///
   Key
     (isWellDefined, SimplicialModule)
   Headline
     whether a simplicial module is well-defined
   Usage
     isWellDefined C
   Inputs
     C:SimplicialModule
   Outputs
     :Boolean
       that is true when {\tt C} determines a well defined simplicial module
   Description
    Text
      This routine checks that the face/degeneracy maps of {\tt C} satisfy the simplicial identities
      (see @TO isSimplicialModule@ for a list of these identities).
      If there are no degeneracy maps stored, it instead checks that the naive normalization of C
      is a well-defined complex.
      Additionally, it checks that the underlying data in {\tt C} is a properly formed
      @TO SimplicialModule@ object in Macaulay2. If the variable {\tt debugLevel} is set to a value greater than zero,
      then information about the nature of any failure is displayed.
    Text
      As a first example, we construct the Dold-Kan image of a free resolution and verify that it is
      well-defined.
    Example
      R = QQ[a..d];
      f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
      f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
      C = simplicialModule(complex {f0, f1}, 3, Degeneracy => true)
      isWellDefined C
      dd^C
      ss^C
      dd^C*ss^C  --if C is well-defined, this should be all identity maps
    Text
      The zero simplicial module is well-defined.
    Example
      C = simplicialModule(R^0, 6, Degeneracy => true)
      isWellDefined C
   SeeAlso
     (isWellDefined, SimplicialModuleMap)
     map
///


doc ///
   Key
     (symbol _, SimplicialModule, ZZ)
     (symbol _, SimplicialModule, Sequence)
   Headline
     access individual objects of a simplicial module
   Usage
     C_i
   Inputs
     C:SimplicialModule
     i:ZZ
       or @TO Sequence@, indicating a term in some degree
   Outputs
     :Module
       the {\tt i}-th object
   Description
    Text
       The data of the modules of a simplicial module is stored in two different ways,
       depending on whether the simplicial module $S$ is obtained as the Dold-Kan image
       of some complex. If the simplicial module is obtained as a Dold-Kan image of some complex $C$,
       then for each $i$ there is a decomposition
       $$S_i = C_0 \oplus C_1^{\binom{i}{1}} \oplus \cdots \oplus C_j^{\binom{i}{j}} \oplus \cdots.$$
       Thus the individual terms $C_j^{\binom{i}{j}}$ may be accessed as the $(i,j)$-component
       of the simplicial module.
       If $S$ is not obtained as a Dold-Kan image, then each term is considered singly indexed with no additional components.
    Example
      S = ZZ/101[a..c]
      C = simplicialModule(freeResolution coker vars S, 4, Degeneracy => true)
      C.?complex
      C.complex
      C_(1,0) == C.complex_0
      C_(1,1) == C.complex_1
      C_(2,1) == C.complex_1 ++ C.complex_1
      tC = C ** C
      tC.?complex
      tC_2
    Text
      If the simplicial object is obtained as a Dold-Kan image, then using an integer subscript
      will automatically take the direct sum over all the terms of the complex appearing in that degree
      (and cache the components of that direct sum accordingly).
    Example
      C_3
      components C_3
      (C_3)_[1,2,3] --extract the inclusion of (C.complex_1)^3
    Text
      Indices that are outside of the top degree automatically
      return the zero object.
    Example
      C_(-1,3)
      C_-7
   SeeAlso
     (symbol _, SimplicialModuleMap, ZZ)
///


doc ///
   Key
     (symbol ==, SimplicialModule, SimplicialModule)
     (symbol ==, SimplicialModule, ZZ)
     (symbol ==, ZZ, SimplicialModule)
   Headline
     whether two simplicial modules are equal
   Usage
     C == D
     C == 0
   Inputs
     C:SimplicialModule
     D:SimplicialModule
   Outputs
     :Boolean
       that is true when {\tt C} and {\tt D} are equal
   Description
    Text
      Two simplicial modules are equal if the corresponding 
      objects and corresponding maps at each index are equal.
    Example
      S = ZZ/101[a..c]
      C = simplicialModule(K = freeResolution coker vars S, 4)
      D = image id_C;
      C === D
      C == D
    Text
      Both the maps and the objects must be equal.
    Example
      E = simplicialModule(complex for i from 1 to 3 list 0*dd^K_i, 4)
      dd^E
      C == E
      E == 0
    Text
      A simplicial module is equal to zero if all the objects and maps are zero.
      This could require computation to determine if something that
      is superficially not zero is in fact zero.
    Example
      f = id_C
      D = coker f
      D == 0
    Example
      C0 = simplicialModule( S^0, 5, Degeneracy => true)
      C1 = simplicialModule(complex(S^0, Base => 2), 5, Degeneracy => true)
      topDegree C0 == topDegree C1
      C0 == C1
      C0 == 0
      C1 == 0
   Caveat
     Testing for equality is not the same as testing for isomorphism.
   SeeAlso
///


doc ///
    Key
        "face/degeneracy maps of a simplicial module"
        (symbol^, Symbol, SimplicialModule)
	ss
    Headline
        access the face and degeneracy maps of a simplicial module
    Usage
        dd^C
        ss^C
    Inputs
        C:SimplicialModule
    Outputs
        :SimplicialModuleMap
            a map of degree -1 or 1, respectively
    Description
        Text
            A simplicial module is a sequence of modules connected
            by homomorphisms, called face/degeneracy maps, such that
            these maps satisfy a set of identities known as the simplicial identities.
            The face maps are accessed via the symbol @TT "dd"@ (the same symbol used
            for the differentials of a complex), and the degeneracy maps via @TT "ss"@,
            after the conventional letter $s$ (or $\sigma$) used for degeneracy maps
            in the literature.

	    More precisely we should have the following equalities:
	    1. For face maps:
                \[ d_j d_i = d_i d_{j-1} \text{ for } 0 \leq i < j \leq n \]
            2. For face and degeneracy maps:
                \[ d_i s_j = s_{j-1} d_i \text{ for } i < j \]
                \[ d_j s_j = \text{id} \]
                \[ d_{j+1} s_j = \text{id} \]
                \[ d_k s_j = s_j d_{k-1} \text{ for } k > j+1 \]
            3. For degeneracy maps:
                \[ s_j s_i = s_i s_{j+1} \text{ for } i \leq j \]
        Text
            The face/degeneracy maps are considered as double indexed in this package. This is because
	    for a fixed integer $n$, the module $S_n$ comes equipped with $(n+1)$ face and degeneracy maps:
	    $$d_{n,i} : S_n \to S_{n-1}, \quad \text{and} \quad s_{n,i} : S_n \to S_{n+1}.$$
	    The above maps can be accessed as follows:
        Example
            R = QQ[a..d];
            I = ideal(a*d-b*c, b^2-a*c, c^2-b*d);
            C = simplicialModule(freeResolution(R^1/I), 4, Degeneracy => true)
	    isWellDefined C
            dd^C
            C.dd
	    ss^C
	    C.ss
            assert(dd^C === C.dd)
            assert(source dd^C === C)
            assert(target dd^C === C)
            assert(degree dd^C === -1)
	    assert(source ss^C === C)
            assert(target ss^C === C)
            assert(degree ss^C === 1)
        Text
            The individual maps between terms are indexed by their
            source.
        Example
            dd^C_(2,0)
            assert(source dd^C_2 === C_2)
            assert(target dd^C_2 === C_1)
    SeeAlso
        "Making maps between simplicial modules"
        (symbol_, SimplicialModuleMap, ZZ)
        (symbol_, SimplicialModule, ZZ)
        (source, SimplicialModuleMap)
        (target, SimplicialModuleMap)
        (degree, SimplicialModuleMap)
///



doc ///
   Key
     (symbol_, SimplicialModule, Array)
     (symbol^, SimplicialModule, Array)
   Headline
     the canonical inclusion or projection map of a direct sum
   Usage
     i = C_[name]
     p = C^[name]
   Inputs
     C:SimplicialModule
     name:
   Outputs
     :SimplicialModuleMap
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
      C1 = simplicialModule(freeResolution coker vars S, 5, Degeneracy => true)
      C2 = simplicialModule(complex (ideal(a,b,c)) , 5, Degeneracy => true)
      D = C1 ++ C2
      D_[0]
      isCommutative D_[0]
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
      E = (chicken => C1) ++ (nuggets => C2)
      E_[chicken]
      E_[nuggets]
      E^[chicken] * E_[chicken] == 1
      E^[nuggets] * E_[nuggets] == 1
      E^[chicken] * E_[nuggets] == 0
      E^[nuggets] * E_[chicken] == 0
      E_[chicken] * E^[chicken] + E_[nuggets] * E^[nuggets] == 1
    Text
      One can also access inclusion and projection maps of sub-direct sums.
    Example
      F = directSum(C1, C2, simplicialModule(complex(S^2, Base => 1), 5, Degeneracy => true))
      prune (F^[0,1])
      isSimplicialMorphism oo
      prune (F_[0,2])
      isSimplicialMorphism oo
   SeeAlso
     (directSum, SimplicialModule)
     (components, SimplicialModule)
     indices
///

doc ///
   Key
     (components, SimplicialModule)
   Headline
     list the components of a direct sum
   Usage
     components C
   Inputs
     C:SimplicialModule
   Outputs
     :List
       the component simplicial modules of a direct sum (of simplicial modules)
   Description
    Text
      A simplicial module which has been constructed as a direct sum
      stores its component simplicial modules.
    Example
      S = ZZ/101[a,b,c];
      C1 = simplicialModule freeResolution coker vars S
      C2 = simplicialModule(complex (ideal(a,b,c)), 3)
      D = C1 ++ C2
      L = components D
      L_0 === C1
      L_1 === C2
      E = (peanut => C1) ++ (butter => C2)
      components E
    Text
      The names of the component simplicial modules are called indices, 
      and are used to access the relevant inclusion and projection maps.
    Example
      indices D
      D^[0]
      indices E
      E_[butter]
   SeeAlso
     (directSum, SimplicialModule)
     indices
     (symbol_, SimplicialModule, Array)
     (symbol^, SimplicialModule, Array)
///


doc ///
   Key
     (symbol**, SimplicialModule, SimplicialModule)
     (symbol**, Complex, SimplicialModule)
     (symbol**, SimplicialModule, Complex)
     (symbol**, SimplicialModule, Module)
     (symbol**, Module, SimplicialModule)
   Headline
     tensor product of simplicial modules
   Usage
     D = C1 ** C2
   Inputs
     C1:SimplicialModule
       or @ofClass Module@
     C2:SimplicialModule
       or @ofClass Module@
   Outputs
     D:SimplicialModule
       tensor product of {\tt C1} and {\tt C2}
   Description
    Text
      The tensor product is a simplicial module $D$ whose $i$th component is
      the tensor product of the degree i components of $C1$ and $C2$. The face/degeneracy maps
      are given by the tensor products of the face and degeneracy maps of the original objects.
      
      As the next example illustrates, the simplicial tensor product in general does not
      normalize to give an object that is isomorphic to the classically defined tensor product
      of complexes.
    Example
      S = ZZ/101[a..c]
      Ca = simplicialModule(complex {matrix{{a}}}, 3)
      Cb = simplicialModule(complex {matrix{{b}}}, 3)
      Cc = simplicialModule(complex {matrix{{c}}}, 3)
      Cab = Cb ** Ca
      dd^Cab
      (prune normalize Cab).dd
      assert isWellDefined Cab
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      dd^(nC = prune normalize Cabc)
      assert isWellDefined nC
    Text
      If one of the arguments is a module, it is considered as a complex concentrated in homological degree 0.
    Example
      Cabc ** (S^1/(a,b,c));
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
        (truncate, List, SimplicialModule)
        (truncate, ZZ, SimplicialModule)
    Headline
        truncation of a simplicial module at a specified degree or set of degrees
    Usage
        truncate(d, C)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        C:SimplicialModule
            that is homogeneous over $R$
    Outputs
        :SimplicialModule
          a simplicial module whose terms consist of all elements of component-wise degree at least {\tt d}.
    Description
        Text
            Truncation of homogeneous (graded) modules induces a natural
            operation on simplicial modules.
        Text
            In the singly graded case, the truncation of a homogeneous
            module $M$ at degree $d$ is generated by all homogeneous
            elements of degree at least $d$ in $M$.  This method applies
            this operation to each term in a simplicial module.
        Example
            R = QQ[a,b,c];
            I = ideal(a*b, a*c, b*c)
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D = truncate(3,C)
            assert isWellDefined D
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
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D1 = prune truncate({{1,1}}, C)
            D2 = truncate({{1,0}}, C)
            D3 = truncate({{0,1}}, C)
            D4 = truncate({{1,0},{0,1}}, C);
            D5 = truncate({{2,2}}, C);
            assert all({D1,D2,D3,D4,D5}, isWellDefined)
    SeeAlso
        "Making simplicial modules"
        "Truncations :: truncate(List,Module)"
        (truncate, List, SimplicialModuleMap)
///


doc ///
    Key
        (symbol SPACE, RingMap, SimplicialModule)
    Headline
        apply a ring map to a simplicial module
    Usage
        phi C
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        C:SimplicialModule
            over the ring $R$
    Outputs
        :SimplicialModule
            over the ring $S$
    Description
        Text
            We illustrate the image of a simplicial module under a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D = phi C
            isWellDefined D
            dd^D
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the simplicial module.
        Example
            R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            phi = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = simplicialModule(freeResolution coker vars R, 4, Degeneracy => true)
            D = phi C
            assert isWellDefined D
    Caveat
        Every term in the simplicial module must be free or a submodule of a free module.
        Otherwise, use @TO (tensor, RingMap, SimplicialModule)@.
    SeeAlso
        (symbol SPACE, RingMap, SimplicialModuleMap)
        (symbol **, RingMap, SimplicialModule)
///


doc ///
    Key
        (symbol**, RingMap, SimplicialModule)
        (symbol**, SimplicialModule, RingMap)
        (tensor, RingMap, SimplicialModule)
        (tensor, SimplicialModule, RingMap)
        (symbol**, SimplicialModule, Ring)
        (symbol**, Ring, SimplicialModule)
    Headline
        tensor a simplicial module along a ring map
    Usage
        phi ** C
        tensor(phi, C)
        S ** C
        C ** S
    Inputs
        phi:RingMap
            whose source is a ring $R$ and whose target is a ring $S$
        C:SimplicialModule
            over the ring $R$
    Outputs
        :SimplicialModule
            over the ring $S$
    Description
        Text
            These methods implement the base change of rings.  As input, one can either
            give a ring map $\phi$, or the ring $S$ (when there is a canonical map
                from $R$ to $S$).
        Text
            We illustrate the tensor product of a simplicial module along a ring map.
        Example
            R = QQ[x,y,z];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D = phi ** C
            assert isWellDefined D
            dd^D
	    ss^D
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
            C = simplicialModule(complex {g}, 3, Degeneracy => true)
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
            C = simplicialModule(freeResolution coker vars R, 3, Degeneracy => true)
            D = f ** C
            D == f C
            assert isWellDefined D
    SeeAlso
        (symbol **, RingMap, SimplicialModuleMap)
        (symbol SPACE, RingMap, SimplicialModule)
///



doc ///
    Key
        (minimalPresentation, SimplicialModule)
        (prune, SimplicialModule)
        (prune, SimplicialModuleMap)
        (minimalPresentation, SimplicialModuleMap)
    Headline
        minimal presentation of all terms in a simplicial module
    Usage
        D = minimalPresentation C
        D = prune C
        h = minimalPresentation f
        h = prune f
    Inputs
        C:SimplicialModule
            or $f$ @ofClass SimplicialModuleMap@
        Exclude => 
            unused
    Outputs
        D:SimplicialModule
            isomorphic to the input, where each term is replaced
            by a minimally presented model, or $h$ @ofClass SimplicialModuleMap@
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
            is functorial, applying both to simplicial modules and simplicial module maps.
        Text
            In particular, images/kernels/cokernels of maps need to be pruned to
            be understood.  For instance, this is useful 
            for recognizing when terms given by subquotient modules 
            are actually zero.
        Example
            S = ZZ/101[a,b,c,d,e];
            I = ideal(a,b) * ideal(c,d,e)
            F = simplicialModule((dual freeResolution I)[-4], 2, Degeneracy => true)
            C = HH F
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isSimplicialMorphism g
            assert (target g == C)
            assert (source g == D)
            g^-1
            assert(g*g^-1 == 1 and g^-1*g == 1)
        Text
            The image of a map of simplicial modules also becomes more
            understandable via pruning.
        Example
            S = ZZ/101[a,b,c];
            I = ideal(a^2,b^2,c^2);
            J = I + ideal(a*b*c);
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            FJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = randomSimplicialMap(FJ, FI ** S^{-1}, Cycle => true)
            C = image f
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isSimplicialMorphism g
            assert (target g == C)
            assert (source g == D)
            g^-1
            assert(g*g^-1 == 1 and g^-1*g == 1)
        Text
            One can directly prune the map of simplicial modules $f$.
        Example
            h = prune f
            assert(source h === prune source f)
            assert(target h === prune target f)
   SeeAlso
       "Making simplicial modules"
       (minimalPresentation, Module)
       randomSimplicialMap
       isSimplicialMorphism
///


doc ///
    Key
        "Making maps between simplicial modules"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (map, SimplicialModule, SimplicialModule, HashTable),
                TO (map, SimplicialModule, SimplicialModule, ZZ),
                TO (map, SimplicialModule, SimplicialModule, SimplicialModuleMap),
                TO (id, SimplicialModule),
                TO "face/degeneracy maps of a simplicial module",
                TO (symbol SPACE, SimplicialModuleMap, Array),
                TO (isWellDefined, SimplicialModuleMap)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new simplicial module maps"@
        Text 
            @UL {
                TO (symbol**, SimplicialModule, Matrix)
           }@
    	Text
    	    @SUBSECTION "Canonical maps between simplicial modules"@
        Text
            Some simplicial modules come with canonical maps.
            These are best accessed using @TO inducedMap@.
	Text
    	    @UL {
                TO (kernel, SimplicialModuleMap),
                TO (cokernel, SimplicialModuleMap),
                TO (image, SimplicialModuleMap),
                TO (coimage, SimplicialModuleMap),
                TO (inducedMap, SimplicialModule, SimplicialModule)
            }@
    	Text
    	    @SUBSECTION "Random maps of simplicial modules"@
        Text
            The method @TO (randomSimplicialMap, SimplicialModule, SimplicialModule)@
            allows one to construct random simplicial module maps,
            random morphisms between simplicial modules, and random
            null homotopies between simplicial modules.
	Text
    	    @UL {
                TO (isCommutative, SimplicialModuleMap),
                TO (isSimplicialMorphism, SimplicialModuleMap)
            }@
    	Text
    	    @SUBSECTION "Elementary operations on simplicial module maps"@
        Text
    	    @UL {
                TO "arithmetic with simplicial module maps",
                TO (symbol +, SimplicialModuleMap, SimplicialModuleMap),
                TO (symbol |, SimplicialModuleMap, SimplicialModuleMap),
                TO (symbol ||, SimplicialModuleMap, SimplicialModuleMap),
                TO (symbol ++, SimplicialModuleMap, SimplicialModuleMap),
                TO (symbol **, SimplicialModuleMap, SimplicialModuleMap),
                TO (symbol _, SimplicialModuleMap, Array),
                TO (symbol ^, SimplicialModuleMap, Array),
                TO (truncate, List, SimplicialModuleMap),
                TO (symbol SPACE, RingMap, SimplicialModuleMap),
                TO (symbol **, RingMap, SimplicialModuleMap)
            }@
    SeeAlso
        "Making simplicial modules"
        "Basic invariants and properties"
///


doc ///
  Key
    SimplicialModuleMap
    complexMap
  Headline
    the class of all maps between simplicial modules
  Description
    Text
      @LITERAL ////<script> macros["\\Hom"] = "\\operatorname{Hom}" </script>////@

      A map of simplicial modules $f \colon C \rightarrow D$ of degree $d$ is a
      sequence of maps $f_i \colon C_i \rightarrow D_{d+i}$.  
      No relationship between the maps $f_i$
      and the face/degeneracy maps of either $C$ or $D$ is assumed. If a simplicial module map
      is obtained as the image of a morphism of complexes under the Dold-Kan functor,
      the key {\tt complexMap} will be stored for more efficient normalization computations.
      
      The usual algebraic operations are available: addition,
      subtraction, scalar multiplication, and composition. The
      identity map from a simplicial module to itself can be produced with
      @TO "id"@. An attempt to add (subtract, or compare) a ring
      element to a simplicial module map will result in the ring element being
      multiplied by the appropriate identity map.
  SeeAlso
    SimplicialModule
    "Making maps between simplicial modules"
    "arithmetic with simplicial module maps"
///


doc ///
    Key
        (map, SimplicialModule, SimplicialModule, HashTable)
    Headline
        make a map of simplicial modules
    Usage
        f = map(D, C, H)
    Inputs
        C:SimplicialModule
        D:SimplicialModule
        H:HashTable
            whose keys are integers, and whose values are the maps between
            the corresponding terms
    Outputs
        f:SimplicialModuleMap
    Description
        Text
            A map of simplicial modules $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and
            the face/degeneracy maps of either $C$ or $D$ is assumed.
            
            We construct a map of simplicial modules by specifying the
            individual maps between the terms. Note that this constructor is typically
	    used more behind the scenes, and using the @TO simplicialModule@ or basic map constructors
	    is typically much more efficient than typing the maps by hand.
        Example
            R = ZZ/101[a,b,c];
            C = simplicialModule(F = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}, Degeneracy => true)
            D = simplicialModule(G = freeResolution coker vars R, Degeneracy => true)
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{1, 0, 0, 0}, {0, a, 0, 0}, {0, -b, b^2, 0}, {0, 0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			             {0, a, 0, 0, 0, 0,0, 0, 0, 0},
				     {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0},
				     {0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0},
				     {0, 0, 0, 0, a, 0, 0, 0, 0,0},
                                     {0, 0, 0, 0, -b, b^2, 0, 0, 0, 0},
                                     {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3, 0},
				     {0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
			            {0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0},
				    {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, a, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, -b, b^2, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0,0, 0, 0, 0, 0, 0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0, 0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-a*c^2, a*c^3, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0,0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0, 0, 0, 0},
                                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0, 0,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2,a*c^3, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2*c^3}})
			    }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isSimplicialMorphism f
        Text
            The keys in the hash table index the terms in the source of the
            map.  If a key is missing, that map is taken to be the zero map.
            We illustrate this by constructing the 0 map as follows:
	Example
            h = map(C, C, hashTable {})
	    h == 0
        Text
            This is the primary constructor used by all of the more user friendly
            methods for constructing a simplicial module map.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a simplicial module map
        is well constructed, use @TO (isWellDefined, SimplicialModuleMap)@.
    SeeAlso
        SimplicialModuleMap
        (isWellDefined, SimplicialModuleMap)
        (isHomogeneous, SimplicialModuleMap)
        (degree, SimplicialModuleMap)
        (isSimplicialMorphism, SimplicialModuleMap)
        (isCommutative, SimplicialModuleMap)
        (source, SimplicialModuleMap)
        (target, SimplicialModuleMap)
///


doc ///
    Key
        (map, SimplicialModule, SimplicialModule, ZZ)
    Headline
        make the zero map or identity between simplicial modules
    Usage
        f = map(D, C, 0)
        f = map(C, C, 1)
    Inputs
        C:SimplicialModule
        D:SimplicialModule
        0:ZZ
            or 1
    Outputs
        f:SimplicialModuleMap
            the zero map from $C$ to $D$ or the identity map from $C$ to $C$
    Description
        Text
            A map of simplicial modules $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            
            We construct the zero map between two
            simplicial modules.
        Example
            R = QQ[a,b,c]
            C = simplicialModule(freeResolution coker vars R, Degeneracy => true)
            D = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}, Degeneracy => true)
            f = map(D, C, 0)
            assert isWellDefined f
            assert isSimplicialMorphism f
            g = map(C, C, 0, Degree => 13)
            assert isWellDefined g
            assert(degree g == 13)
            assert not isSimplicialMorphism g
            assert isCommutative g
            assert isHomogeneous g
            assert(source g == C)
            assert(target g == C)
        Text
            Using this function to create the identity map
            is the same as using @TO (id, SimplicialModule)@.
        Example
            assert(map(C, C, 1) === id_C)
   SeeAlso
        SimplicialModuleMap
        (isWellDefined, SimplicialModuleMap)
        (isHomogeneous, SimplicialModuleMap)
        (degree, SimplicialModuleMap)
        (isSimplicialMorphism, SimplicialModuleMap)
        (isCommutative, SimplicialModuleMap)
        (source, SimplicialModuleMap)
        (target, SimplicialModuleMap)
        (id, SimplicialModule)
///

doc ///
    Key
        (map, SimplicialModule, SimplicialModule, SimplicialModuleMap)
    Headline
        make a new map of simplicial modules, induced from an existing one
    Usage
        g = map(D, C, f)
    Inputs
        C:SimplicialModule
        D:SimplicialModule
        f:SimplicialModuleMap
            regarded as providing matrices which induce maps between the terms of $C$ and $D$
    Outputs
        g:SimplicialModuleMap
    Description
        Text
            A map of simplicial modules $f : C' \rightarrow D'$ is a
            sequence of maps $f_i : C'_i \rightarrow D'_{d'+i}$.  
            The new map $g : C \rightarrow D$ is the sequence of maps $g_i : C_i \rightarrow D_{d+i}$
            induced by the matrix of $f_i$.
            
            One use for this function is to get the new map of simplicial modules induced by
	    forgetting the underlying complexes of the source and target, assuming that the
	    source and target are obtained as Dold-Kan images. 
        Example
            R = ZZ/101[a,b,c];
            C = simplicialModule(freeResolution coker vars R, Degeneracy => true)
            f = map(forgetComplex C, forgetComplex C, id_C)
            assert isWellDefined f
            assert(degree f == 0)
            assert isCommutative f
            assert isSimplicialMorphism f
            normalize f --notice how the normalization is not already pruned
	    normalize id_C
	    prune normalize f == normalize id_C
    SeeAlso
        SimplicialModuleMap
        (isWellDefined, SimplicialModuleMap)
        (degree, SimplicialModuleMap)
        (isSimplicialMorphism, SimplicialModuleMap)
        (isCommutative, SimplicialModuleMap)
        (symbol SPACE, SimplicialModuleMap, Array)
///



doc ///
    Key
        (id, SimplicialModule)
    Headline
        the identity map of a simplicial module
    Usage
        f = id_C
    Inputs
        C:SimplicialModule
    Outputs
        f:SimplicialModuleMap
          the identity map from $C$ to itself
    Description
        Text
            The simplicial modules together with simplicial morphisms
            forms a category.  In particular, every simplicial module has an identity map.
        Example
            R = ZZ/101[x,y]/(x^3, y^3)
            C = simplicialModule(freeResolution(coker vars R, LengthLimit=>6), 6, Degeneracy => true)
            f = id_C;
            assert isWellDefined f
            assert isSimplicialMorphism f
    SeeAlso
        (map, SimplicialModule, SimplicialModule, ZZ)
        (isWellDefined, SimplicialModuleMap)
        (isSimplicialMorphism, SimplicialModuleMap)
///

doc /// 
    Key
        (isWellDefined, SimplicialModuleMap)
    Headline
        whether a map of simplicial modules is well-defined
    Usage
        isWellDefined f
    Inputs
        f:SimplicialModuleMap
    Outputs
        :Boolean
            that is true when {\tt f} determines a well defined simplicial module map
    Description
        Text
            A map of simplicial modules $f : C \to D$ of degree $d$ is a sequence of
            maps $f_i : C_i \to D_{d+i}$.  No relationship is required between
            these maps and the face/degeneracy maps in the source and target.

            This routine checks that $C$ and $D$ are well-defined
            simplicial modules, and that, for each $f_i$, the source and
            target equal $C_i$ and $D_{d+i}$, respectively.  If the
            variable {\tt debugLevel} is set to a value greater than
            zero, then information about the nature of any failure is
            displayed.
        Text
            Unlike the @TO2((isWellDefined, SimplicialModule), 
                "corresponding function for SimplicialModules")@,
            the basic constructors for simplicial module maps are all but
            assured to be well defined. The only case that could cause
            a problem is if one constructs the source or target
            complex, and those are not well defined.
        Example
            R = ZZ/101[a,b,c];
            C = simplicialModule(F = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}, Degeneracy => true)
            D = simplicialModule(G = freeResolution coker vars R, Degeneracy => true)
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{1, 0, 0, 0}, {0, a, 0, 0}, {0, -b, b^2, 0}, {0, 0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			             {0, a, 0, 0, 0, 0,0, 0, 0, 0},
				     {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0},
				     {0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0},
				     {0, 0, 0, 0, a, 0, 0, 0, 0,0},
                                     {0, 0, 0, 0, -b, b^2, 0, 0, 0, 0},
                                     {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3, 0},
				     {0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
			            {0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0},
				    {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, a, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, -b, b^2, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0,0, 0, 0, 0, 0, 0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0, 0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-a*c^2, a*c^3, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0,0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0, 0, 0, 0},
                                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0, 0,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2,a*c^3, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2*c^3}})
			    }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isSimplicialMorphism f
        Text
            We construct two random maps of simplicial modules,
            and check to see that, as should be the case, 
            both are well defined.
        Example
            g = randomSimplicialMap(D,C); --outputs are large
	    normalize g
            assert isWellDefined g
            assert not isCommutative g
        Example
            h = randomSimplicialMap(D,C, Cycle => true);
	    normalize h
            assert isWellDefined h
            assert isSimplicialMorphism h
        Text
            This method also checks the following aspects of 
            the data structure:
        Text
            @UL {
                TEX "The underlying hash table has exactly the expected keys,
                namely, {\\tt source, target, degree, map, cache}",
                "The ring of the source and target are the same",
                "The source and target are well defined simplicial modules",
                "The degree is an integer",
                TEX "All keys in the {\\tt map} field are integers or sequences,
                in the range of the top degree of the source",
                TEX "The source and target of each $f_i$ is as expected",
                TEX "If the {\\tt isCommutative} key is present in the cache
                table, then commutativity of the map with the face/degeneracy maps
                is checked"
                }@
    SeeAlso
        (isWellDefined, SimplicialModule)
        (isCommutative, SimplicialModuleMap)
        (isSimplicialMorphism, SimplicialModuleMap)
        (map, SimplicialModule, SimplicialModule, HashTable)
///

doc ///
    Key
        (source, SimplicialModuleMap)
    Headline
        get the source of a map of simplicial modules
    Usage
        C = source f
    Inputs
      f:SimplicialModuleMap
    Outputs
      C:SimplicialModule
    Description
        Text
            Given a simplicial module map $f : C \to D$
            this method returns the simplicial module $C$.
        Example
            R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            FJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = randomSimplicialMap(FJ, FI, Cycle=>true)
            source f
            assert isWellDefined f
            assert isSimplicialMorphism f
            assert(source f == FI)
            assert(target f == FJ)
        Text
            The face/degeneracy map in a simplicial module is considered to have type SimplicialModuleMap. 
        Example
            kk = coker vars R
            F = simplicialModule(freeResolution kk, Degeneracy => true)
            source dd^F == F
            target dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making simplicial modules"
       (target, SimplicialModuleMap)
       (randomSimplicialMap, SimplicialModule, SimplicialModule)
///

doc ///
    Key
        (target, SimplicialModuleMap)
    Headline
        get the target of a map of simplicial modules
    Usage
        C = target f
    Inputs
      f:SimplicialModuleMap
    Outputs
      C:SimplicialModule
    Description
        Text
            Given a simplicial module map $f : C \to D$
            this method returns the simplicial module $D$.
        Example
            R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            FJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = randomSimplicialMap(FJ, FI, Cycle=>true)
            source f
            assert isWellDefined f
            assert isSimplicialMorphism f
            assert(source f == FI)
            assert(target f == FJ)
        Text
            The face/degeneracy map in a simplicial module is considered to have type SimplicialModuleMap. 
        Example
            kk = coker vars R
            F = simplicialModule(freeResolution kk, Degeneracy => true)
            source dd^F == F
            target dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making simplicial modules"
       (source, SimplicialModuleMap)
       (freeResolution, Ideal)
       (randomSimplicialMap, SimplicialModule, SimplicialModule)
///

doc ///
    Key
        (degree, SimplicialModuleMap)
    Headline
        get the degree of a map of simplicial modules
    Usage
        degree f
    Inputs
        f:SimplicialModuleMap
    Outputs
        :ZZ
    Description
        Text
            A simplicial module map $f : C \to D$ of degree $d$ is a sequence
            of maps $f_i : C_i \to D_{i+d}$.
            This method returns $d$.
        Text
            The degree of the face/degeneracy map of a simplicial module is -1 or 1, respectively.
        Example
            R = ZZ/101[a..d];
            I = ideal(a^2, b^2, c^2)
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            assert(degree dd^FI == -1)
	    assert(degree ss^FI == 1)
        Example
            FJ = simplicialModule(freeResolution (I + ideal(a*b*c)), Degeneracy => true)
            f = randomSimplicialMap(FJ, FI, Cycle=>true, Degree => -2);
	    normalize f
   SeeAlso
       "Basic invariants and properties"
       (source, SimplicialModuleMap)
       (target, SimplicialModuleMap)
       (randomSimplicialMap, SimplicialModule, SimplicialModule)
///


doc ///
    Key
        (symbol _, SimplicialModuleMap, ZZ)
	(symbol _, SimplicialModuleMap, Sequence)
    Headline
        access individual matrices in a simplicial module map
    Usage
        f_i
    Inputs
        f:SimplicialModuleMap
        i:ZZ
            the degree index or a sequence
    Outputs
        :Matrix
            the {\tt i}-th map
    Description
        Text
            A simplicial module map $f : C \to D$ of degree $d$ is a sequence of maps $f_i : C_i \to D_{i+d}$.
            This method allows one to access the individual $f_i$.
        Example
            S = ZZ/101[a..c];
            C = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}, Degeneracy => true)
            D = simplicialModule(freeResolution coker vars S, Degeneracy => true)
            f = randomSimplicialMap(D, C)
            f_2
            f_0
	Text
	    The face/degeneracy maps of a simplicial module are indexed by sequences $(a,b)$ of integers,
	    where $b \leq a$.
	Example
	    dd^C_(2,0)
	    dd^C_(2,1)
	    dd^C_(2,2)
	    ss^C_(0,0)
	    ss^C_(1,0)
	    ss^C_(1,1)
        Text
            Indices that are outside of the top degree or range of sequences return 0 by default.
        Example
            topDegree f
            f_-1
            f_3
            f_4
	    ss^D_(2,3)
	    ss^D_(4,0)
    SeeAlso
        (symbol_, SimplicialModule, ZZ)
///

doc ///
    Key
        (isHomogeneous, SimplicialModuleMap)
    Headline
         whether a map of simplicial modules is homogeneous
    Usage
         isHomogeneous f
    Inputs
         f:SimplicialModuleMap
    Outputs
         :Boolean
             that is true when $f_i$ is homogeneous, for all $i$
    Description
        Text
            A map of simplicial modules $f \colon C \to D$ is homogeneous
            (graded) if its underlying ring is graded, and all the
            component maps $f_i \colon C_i \to D_{d+i}$ are graded of
            degree zero, where $f$ has degree $d$.
        Example
            S = ZZ/101[a,b,c,d];
            I = minors(2, matrix{{a,b,c},{b,c,d}})
            C = simplicialModule(freeResolution (S^1/I), Degeneracy => true)
            assert isHomogeneous dd^C
            f = randomSimplicialMap(C, C, Degree => -1)
            assert isHomogeneous f
            f = randomSimplicialMap(C, C, InternalDegree => 2)
        Text
            A map of simplicial modules may be homogeneous even if the
            source or the target is not homogeneous.
        Example
            phi = map(S, S, {1,b,c,d})
            D = phi C
            dd^D
            assert not isHomogeneous dd^D
    SeeAlso
        "Basic invariants and properties"
        isHomogeneous
        (randomSimplicialMap, SimplicialModule, SimplicialModule)
///



doc ///
    Key
        (components, SimplicialModuleMap)
    Headline
        list the components of a direct sum
    Usage
        components f
    Inputs
        f:SimplicialModuleMap
    Outputs
        :List
            the component maps of a direct sum of maps of simplicial modules
    Description
        Text
            A map of simplicial modules stores its component maps.
        Example
            S = ZZ/101[a,b,c];
            C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
            g1 = id_C
            g2 = randomSimplicialMap(C[1], C[2], Boundary => true)
            f = g1 ++ g2
            assert isWellDefined f
            L = components f
            L_0 === g1
            L_1 === g2
            indices f
        Text
            The names of the components are called indices, and are
            used to access the relevant inclusion and projection maps.
        Example
            f^[0]
            f_[0]
    SeeAlso
        (directSum, SimplicialModuleMap)
        (components, SimplicialModule)
        indices
        (symbol_, SimplicialModuleMap, Array)
        (symbol^, SimplicialModuleMap, Array)
///

doc ///
  Key
    (symbol*, SimplicialModuleMap, SimplicialModuleMap)
  Headline
    composition of homomorphisms of simplicial modules
  Usage
    f = h * g
  Inputs
    h:SimplicialModuleMap
      if a ring element or integer, then we multiply the ring element
      by the appropriate identity map
    g:SimplicialModuleMap
  Outputs
    f:SimplicialModuleMap
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
      C = simplicialModule(freeResolution coker vars R, Degeneracy => true)
      3 * dd^C
      0 * dd^C
      dd^C * ss^C --if C is a well-defined simplicial object, these should all be identity maps
  SeeAlso
      "Making maps between simplicial modules"
      "arithmetic with simplicial module maps"
///

doc ///
    Key
        (symbol ^, SimplicialModuleMap, ZZ)
    Headline
        the n-fold composition
    Usage
        f^n
    Inputs
        f:SimplicialModuleMap
            whose source and target are the same simplicial module
        n:ZZ
    Outputs
        :SimplicialModuleMap
            the composition of $f$ with itself $n$ times.
    Description
        Text
            A simplicial module map $f : C \to C$ can be composed with itself.
            This method produces these new maps of simplicial modules.
        Text
            The face/degeneracy map always composes with itself to give the
            zero map.
        Example
            S = ZZ/101[a..c];
            C = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}, Degeneracy => true)
            f = dd^C
            f^2
            assert(source f == target f)
            assert(degree f == -1)
            assert(degree f^2 == -2)
        Example
            g = randomSimplicialMap(C, C)
            g^2
            g^3
        Text
            The zero-th power returns the identity map
        Example
	    f^0 == id_(C[1])
            g^0 == id_C
        Text
            When $n$ is negative, the result is the $n$-fold power
            of the inverse simplicial module map, if it exists.
        Example
            h = randomSimplicialMap(C, C)
            h^-1
            assert(h * h^-1 == id_C)
            h^-4
            assert(h^-4 * h^4 == id_C)
    SeeAlso
        (symbol^, Matrix, ZZ)
///

doc ///
   Key
     (symbol ==, SimplicialModuleMap, SimplicialModuleMap)
     (symbol ==, SimplicialModuleMap, ZZ)
     (symbol ==, ZZ, SimplicialModuleMap)
   Headline
     whether two simplicial module maps are equal
   Usage
     f == g
     f == 0
     f == 1
   Inputs
     f:SimplicialModuleMap
       or 0, or 1.
     g:SimplicialModuleMap
       or 0, or 1.
   Outputs
     :Boolean
       that is true when {\tt f} and {\tt g} are equal
   Description
     Text
       Two simplicial module maps are equal if they have the same source,
       the same target, and $f_i = g_i$ for all $i$.
     Example
       S = ZZ/101[a..c]
       C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
       f = id_C
       assert(f == 1)
     Text
       A simplicial module map is equal to zero if all the maps are zero.
       This could require computation to determine if something that
       is superficially not zero is in fact zero.
     Example
       assert(0 * id_C == 0)
     Example
       g = randomSimplicialMap(C, C)
       h = inducedMap(coker g, target g)
       assert(h == 0)
     Text
       Testing whether a map is equal to 1 is a shorthand for determining
       if the simplicial module map is the identity.
       Although the matrices may appear to be the identity, the map is not the
       identity when the source and target are not equal.
     Example
       g = randomSimplicialMap(C, C, InternalDegree=>1, Cycle=>true)
       h = inducedMap(coker g, target g)
       assert(h != 1)
     Text
       Testing for equality is not the same as testing for isomorphism.
       In particular, different presentations of a simplicial module need not be equal.
     Example
       D = prune image g
       p = D.cache.pruningMap
       p == 1
       assert(coker p == 0 and ker p == 0)
       assert(prune p == 1)
   SeeAlso
     (symbol ==, SimplicialModule, SimplicialModule)
     (symbol SPACE, SimplicialModuleMap, Array)
     randomSimplicialMap
     (prune, SimplicialModule)
///

doc ///
  Key
    (isCommutative, SimplicialModuleMap)
  Headline
    whether a simplicial module map commutes with the face/degeneracy maps
  Usage
    isCommutative f
  Inputs
    f:SimplicialModuleMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the face/degeneracy maps
  Description
    Text
      For a simplicial module map $f : C \to D$, this method
      checks whether, for all $i$, we have
      $dd^D_{i} * f_i =  f_{i-1} * dd^C_i$ and, if the source at target are equipped with
      degeneracy maps, it also checks the equality $ss^D_{i} * f_i =  f_{i+1} * ss^C_i$.
    Text
      We first construct a random simplicial module map which commutes with the face/degeneracy map.
    Example
      S = ZZ/101[a,b,c];
      C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      D = C ** C
      isWellDefined D
      f1 = prune randomSimplicialMap(D, C, Cycle => true, InternalDegree => 1);
      prune normalize f1
      isCommutative oo
      isCommutative f1
      assert(degree f1 == 0)
    Text
      We next generate a simplicial module map that is commutative and (likely) 
      induces a nontrivial map on homology.
    Example
      f2 = randomSimplicialMap(D, C, Cycle => true);
      isCommutative f2
      assert(degree f2 == 0)
      assert isSimplicialMorphism f2
    Text
      If the @TO "debugLevel"@ is greater than zero, then
      the location of the first failure of commutativity is displayed.
  SeeAlso
    isSimplicialMorphism
    randomSimplicialMap
///

doc ///
  Key
    (isSimplicialMorphism, SimplicialModuleMap)
    isSimplicialMorphism
  Headline
    whether a simplicial module map is a morphism of simplicial modules
  Usage
    isSimplicialMorphism f
  Inputs
    f:SimplicialModuleMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the face/degeneracy maps and has degree $0$
  Description
    Text
      For a simplicial module map $f : C \to D$ of degree $d$, this method
      checks whether $d = 0$ and, for all $i$, we have
      $dd^D_{i+d} * f_i =  f_{i-1} * dd^C_i$ and if the simplicial module
      also has degeneracy maps, it checks that $ss^D_{i} * f_i =  f_{i+1} * ss^C_i$.
    Text
      We first construct a random simplicial module morphism.
    Example
      S = ZZ/101[a,b,c];
      C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      D = C ** C
      f1 = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 1);
      isSimplicialMorphism f1
      assert(degree f1 == 0)
    Text
      We next generate a complex morphism that (likely) 
      induces a nontrivial map on homology.
    Example
      f2 = randomSimplicialMap(D, C, Cycle => true);
      isSimplicialMorphism f2
      assert(degree f2 == 0)
      assert isSimplicialMorphism f2
      prune HH f2
    Text
      If the @TO "debugLevel"@ is greater than zero, then
      information about the failure is displayed.
  SeeAlso
    (isCommutative, SimplicialModuleMap)
    randomSimplicialMap
///


doc ///
    Key
        (symbol**, SimplicialModule, Matrix)
        (symbol**, Matrix, SimplicialModule)
    Headline
        create the tensor product of a simplicial module and a map of modules
    Usage
        h = C ** f
        h = f ** C
    Inputs
        C:SimplicialModule
            over a ring $R$
        f:Matrix
            defining a homomorphism from the $R$-module $M$ to the $R$-module $N$
    Outputs
        h:SimplicialModuleMap
            from $C \otimes M$ to $C \otimes N$
    Description
        Text
            For any simplicial module $C$, a map $f \colon M \to N$ of $R$-modules induces a
            morphism $C \otimes f$ of simplicial modules
            from $C \otimes M$ to $C \otimes N$.  This method returns this map of simplicial modules.
        Example
            R = ZZ/101[a..d];
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            J = ideal(I_0, I_1)
            C = simplicialModule(koszulComplex vars R, Degeneracy => true)
            f = map(R^1/I, R^1/J, 1)
            C ** f;
	    isSimplicialMorphism oo
            f ** C;
	    isSimplicialMorphism oo
            f' = random(R^2, R^{-1, -1, -1})
            C ** f';
            f' ** C;
            assert isWellDefined(C ** f')
            assert isWellDefined(f' ** C)
        Text
            Tensoring with a simplicial module defines a functor from the category
            of $R$-modules to the category of simplicial modules over $R$.
        Example
            f'' = random(source f', R^{-2,-2})
            assert((C ** f') * (C ** f'') == C ** (f' * f''))
            assert(C ** id_(R^{-1,-2,-3}) == id_(C ** R^{-1,-2,-3}))
    SeeAlso
        "Making maps between simplicial modules"
        (symbol**, SimplicialModule, SimplicialModule)
///

doc ///
    Key
        (symbol**, SimplicialModuleMap, SimplicialModuleMap)
        (tensor, SimplicialModuleMap, SimplicialModuleMap)
        (symbol**, SimplicialModule, SimplicialModuleMap)
        (symbol**, SimplicialModuleMap, SimplicialModule)
	(symbol**, ComplexMap, SimplicialModuleMap)
	(symbol**, SimplicialModuleMap, ComplexMap)
	(symbol**, Complex, SimplicialModuleMap)
	(symbol**, SimplicialModuleMap, Complex)
        (symbol**, SimplicialModuleMap, Module)
        (symbol**, Module, SimplicialModuleMap)
    Headline
        the map of simplicial modules between tensor simplicial modules
    Usage
        h = f ** g
        h = tensor(f, g)
    Inputs
        f:SimplicialModuleMap
        g:SimplicialModuleMap
    Outputs
        h:SimplicialModuleMap
    Description
        Text
            The maps $f : C \to D$ and $g : E \to F$ of simplicial modules induces the map
            $h = f \otimes g : C \otimes E \to D \otimes F$ defined by $c \otimes e \mapsto f(c) \otimes g(e)$.
        Example
            S = ZZ/101[a..c]
            C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
            D = simplicialModule((freeResolution coker matrix{{a^2,a*b,b^3}})[-1], Degeneracy => true)
            f = randomSimplicialMap(D,C)
            E = simplicialModule((dual C.complex)[-3], Degeneracy => true)
            F = simplicialModule((dual D.complex)[-3], 3, Degeneracy => true)
            g = randomSimplicialMap(F,E)
            h = f ** g;
            assert isWellDefined h
            assert(prune source h == C ** E)
            assert(prune target h == D ** F)
        Text
            If one argument is a SimplicialModule or Module,
            then the identity map of the corresponding complex is used.
        Example
            fE = f ** E;
            assert(fE == f ** id_E)
            k = coker vars S
            gk = g ** k;
        Text
            This routine is functorial.
        Example
            D' = simplicialModule((freeResolution coker matrix{{a^2,a*b,c^3}})[-1], 3, Degeneracy => true)
            f' = randomSimplicialMap(D', D)
            (f' * f) ** g == (f' ** g) * (f ** id_E)
            (f' * f) ** g == (f' ** id_F) * (f ** g)
            F' = simplicialModule(dual (freeResolution coker matrix{{a^2,a*b,a*c,b^3}})[-3], Degeneracy => true)
            g' = randomSimplicialMap(F', F)
            f ** (g' * g) == (f ** g') * (id_C ** g)
            f ** (g' * g) == (id_D ** g') * (f ** g)
    SeeAlso
        (symbol**, SimplicialModule, SimplicialModule)
        (randomSimplicialMap, SimplicialModule, SimplicialModule)
///

doc ///
    Key
        (truncate, List, SimplicialModuleMap)
        (truncate, ZZ, SimplicialModuleMap)
    Headline
        truncation of a simplicial module map at a specified degree or set of degrees
    Usage
        truncate(d, f)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        f:SimplicialModuleMap
            that is homogeneous over $R$
    Outputs
        :SimplicialModuleMap
            a simplicial module map over $R$ whose terms in the source and target
            consist of all elements of component-wise degree at least {\tt d}.
    Description
        Text
            Truncation of homogeneous (graded) maps induces a natural
            operation on maps of simplicial modules.
        Text
            In the singly graded case, the truncation of a homogeneous
            module $M$ at degree $d$ is generated by all homogeneous
            elements of degree at least $d$ in $M$.  The truncation of
            a map between homogeneous modules is the induced map
            between the truncation of the source and the truncation of
            the target.  This method applies this operation to each
            term in a map of simplicial modules.
        Example
            R = QQ[a,b,c];
            C = simplicialModule(freeResolution ideal(a*b, a*c, b*c), 3, Degeneracy => true)
            D = simplicialModule((freeResolution ideal(a*b, a*c, b*c, a^2-b^2))[-1], 3, Degeneracy => true)
            f = randomSimplicialMap(D,C, Cycle => true)
            g = truncate(3,f);
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
            the truncations of the source and target.
        Example
            A = ZZ/101[x_0, x_1, y_0, y_1, y_2, Degrees => {2:{1,0}, 3:{0,1}}];
            I = intersect(ideal(x_0, x_1), ideal(y_0, y_1, y_2))
            C = simplicialModule(freeResolution I, 4, Degeneracy => true)
            J = intersect(ideal(x_0^2, x_1^2), ideal(y_0^2, y_1^2, y_2^2))
            D = simplicialModule(freeResolution J, 4, Degeneracy => true)
            f = simplicialModule(extend(C.complex, D.complex, id_(A^1)), 2)
            g1 = prune truncate({{1,1}}, f);
	    g1_0
	    g1_1
            g2 = truncate({{1,0}}, f);
	    g2_1
            g3 = truncate({{0,1}}, f);
            g4 = truncate({{1,0},{0,1}}, f);
	    g4_1
            g5 = truncate({{2,2}}, f);
            assert all({g1,g2,g3,g4,g5}, isWellDefined)
    SeeAlso
        "Making maps between simplicial modules"
        "Truncations :: truncate(List,Matrix)"
        (truncate, List, SimplicialModule)
///


doc ///
    Key
        (symbol SPACE, RingMap, SimplicialModuleMap)
    Headline
        apply a ring map to a map of simplicial modules
    Usage
        phi f
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:SimplicialModuleMap
            over the ring $R$
    Outputs
        :SimplicialModuleMap
            over the ring $S$
    Description
        Text
            We illustrate the image of a simplicial module map along a ring map.
        Example
            R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = simplicialModule(freeResolution I, 4, Degeneracy => true)
            CJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = simplicialModule(extend(CJ.complex, CI.complex, map(CJ_0, CI_0, 1)), Degeneracy => true)
            assert isWellDefined f
            g = phi f
            assert isWellDefined g
            dd^(source g)
	    ss^(source g)
            dd^(target g);
            prune HH normalize g
    SeeAlso
        (symbol SPACE, RingMap, SimplicialModule)
        (symbol **, RingMap, SimplicialModuleMap)
///

doc ///
    Key
        (symbol**, RingMap, SimplicialModuleMap)
        (symbol**, Ring, SimplicialModuleMap)
        (symbol**, SimplicialModuleMap, RingMap)
        (symbol**, SimplicialModuleMap, Ring)
        (tensor, RingMap, SimplicialModuleMap)
        (tensor, SimplicialModuleMap, RingMap)
    Headline
        tensor a map of simplicial modules along a ring map
    Usage
        phi ** f
        tensor(phi, f)
        S ** f
        f ** S
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:SimplicialModuleMap
            over the ring $R$
    Outputs
        :SimplicialModuleMap
            over the ring $S$
    Description
        Text
            These methods implement the base change of rings.  As input, one can either
            give a ring map $\phi$, or the ring $S$ (when there is a canonical map
                from $R$ to $S$).
        Text
            We illustrate the tensor product of a map of simplicial modules along a ring map.
        Example
            R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = simplicialModule(freeResolution I, 4, Degeneracy => true)
            CJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = simplicialModule(extend(CJ.complex, CI.complex, map(CJ_0, CI_0, 1)), Degeneracy => true)
            assert isWellDefined f
            g = phi ** f
            assert isWellDefined g
            dd^(source g)
	    ss^(source g)
            dd^(target g);
            simplicialModule prune HH normalize g
	    isSimplicialMorphism oo
    SeeAlso
        (symbol **, RingMap, SimplicialModule)
        (symbol SPACE, RingMap, SimplicialModuleMap)
///
 
doc ///
    Key
        (inducedMap, SimplicialModule, SimplicialModule)
    Headline
        make the map of simplicial modules induced at each term by the identity map
    Usage
        f = inducedMap(D, C)
    Inputs
        C:SimplicialModule
        D:SimplicialModule
    Outputs
        f:SimplicialModuleMap
    Description
        Text
            Let $d$ be the value of the optional argument {\tt
            Degree}, or zero, if not given.  For each $i$, the terms
            $D_{i+d}$ and $C_i$ must be subquotients of the same
            ambient free module.  This method returns the simplicial module map
            induced by the identity on each of these free modules.
            
            If {\tt Verify => true} is given, then this method
            also checks that these identity maps induce well-defined 
            maps.  This can be a relatively expensive computation.
        Text
            We illustrate this method by truncating a free resolution
            at two distinct internal degrees.  We check that 
            the various induced maps compose to give another
            induced map.
        Example
            needsPackage "Truncations"
            kk = ZZ/32003
            R = kk[a,b,c]
            F = simplicialModule(freeResolution (ideal gens R)^2, 2, Degeneracy => true)
            C1 = truncate(3, F);
	    prune normalize C1
            C2 = truncate(4, F);
	    prune normalize C2
            assert isWellDefined C1
            assert isWellDefined C2
            f = inducedMap(C1, C2);
	    prune normalize f
            assert isWellDefined f
            f1 = inducedMap(F, C1)
            f2 = inducedMap(F, C2)
            assert isWellDefined f1
            assert isWellDefined f2
            assert(f2 == f1 * f)
    SeeAlso
        (inducedMap, Module, Module)
        (truncate, List, SimplicialModule)
///


doc ///
    Key
        "arithmetic with simplicial module maps"
        (symbol+, SimplicialModuleMap, SimplicialModuleMap)
        (symbol+, RingElement, SimplicialModuleMap)
        (symbol+, Number, SimplicialModuleMap)
        (symbol+, SimplicialModuleMap, RingElement)
        (symbol+, SimplicialModuleMap, Number)
        (symbol-, SimplicialModuleMap)
        (symbol-, SimplicialModuleMap, SimplicialModuleMap)
        (symbol-, RingElement, SimplicialModuleMap)
        (symbol-, Number, SimplicialModuleMap)
        (symbol-, SimplicialModuleMap, RingElement)
        (symbol-, SimplicialModuleMap, Number)
        (symbol*, RingElement, SimplicialModuleMap)
        (symbol*, Number, SimplicialModuleMap)
        (symbol*, SimplicialModuleMap, RingElement)
        (symbol*, SimplicialModuleMap, Number)
    Headline
        perform arithmetic operations on simplicial module maps
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
        f:SimplicialModuleMap
        g:SimplicialModuleMap
        a:RingElement
          that is, an element in the underlying ring or a number
    Outputs
        :SimplicialModuleMap
    Description
        Text
            The set of simplicial module maps forms a module over the underlying @TO2((ring, SimplicialModuleMap), "ring")@.
            These methods implement the basic operations of addition, subtraction, and scalar multiplication.
        Example
            R = ZZ/101[a..d];
            C = simplicialModule(freeResolution coker matrix{{a*b, a*c^2, b*c*d^3, a^3}}, Degeneracy => true)
            D = simplicialModule(freeResolution coker matrix{{a*b, a*c^2, b*c*d^3, a^3, a*c*d}}, 3, Degeneracy => true)
            f = randomSimplicialMap(D, C, Cycle => true);
	    prune normalize f
            g = randomSimplicialMap(D, C, Boundary => true);
	    prune normalize g
        Example
            f+g;
	    isSimplicialMorphism oo
            f-g;
	    isSimplicialMorphism oo
            -f;
            3*f;
            0*f
            a*f;
            assert(0*f == 0)
            assert(1*f == f)
            assert((-1)*f == -f)
            assert(-(f-g) == g-f)
            assert((a+b)*f == a*f + b*f)
            assert(a*(f+g) == a*f + a*g)
            assert isSimplicialMorphism (f+g)
        Text
            Adding or subtracting a scalar is the same as adding or subtracting the
            scalar multiple of the identity.  In particular, the source and target must be equal.
        Example
            h = randomSimplicialMap(C, C);
	    prune normalize h
            prune normalize(h+1)
            assert(h+1 == h + id_C)
            assert(h+a == h + a*id_C)
            assert(1-h == id_C - h)
            assert(b-c*h == -c*h + b*id_C)
            assert(b-h*c == -h*c + id_C*b)
    SeeAlso
        "Making maps between simplicial modules"
        randomSimplicialMap
        (map, SimplicialModule, SimplicialModule, SimplicialModuleMap)
///

doc ///
    Key
        (symbol|, SimplicialModuleMap, SimplicialModuleMap)
    Headline
        join or concatenate maps horizontally
    Usage
        f | g
    Inputs
        f:SimplicialModuleMap
        g:SimplicialModuleMap
    Outputs
        :SimplicialModuleMap
    Description
        Text
            Given simplicial module maps with the same target,
            this method constructs the associated map
            from the direct sum of the sources to the target.

            First, we define some non-trivial maps of simplicial modules.
        Example
            R = ZZ/101[a..d];
            C1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], Degeneracy => true)
            C2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, Degeneracy => true)
            D = simplicialModule(freeResolution coker matrix{{a^2,b^2,c*d}}, Degeneracy => true)
            f = randomSimplicialMap(D, C1)
            g = randomSimplicialMap(D, C2)
        Example
            h = f|g
            assert isWellDefined h
            assert(source h === source f ++ source g)
            assert(target h === target f)
    SeeAlso
        (symbol++, SimplicialModule, SimplicialModule)
        (symbol++, SimplicialModuleMap, SimplicialModuleMap)
        (symbol||, SimplicialModuleMap, SimplicialModuleMap)
        (symbol|, Matrix, Matrix)
///

doc ///
    Key
        (symbol||, SimplicialModuleMap, SimplicialModuleMap)
    Headline
        join or concatenate maps vertically
    Usage
        f || g
    Inputs
        f:SimplicialModuleMap
        g:SimplicialModuleMap
    Outputs
        :SimplicialModuleMap
    Description
        Text
            Given simplicial module maps with the same source,
            this method constructs the associated map
            from the source to the direct sum of the targets.

            First, we define some non-trivial maps of simplicial modules.
        Example
            R = ZZ/101[a..d];
            D1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], Degeneracy => true)
            D2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, Degeneracy => true)
            C = simplicialModule(freeResolution coker matrix{{a^2,b^2,c*d}}, Degeneracy => true)
            f = randomSimplicialMap(D1, C)
            g = randomSimplicialMap(D2, C)
        Example
            h = f||g
            assert isWellDefined h
            assert(target h === target f ++ target g)
            assert(source h === source f)
    SeeAlso
        (symbol++, SimplicialModule, SimplicialModule)
        (symbol++, SimplicialModuleMap, SimplicialModuleMap)
        (symbol|, SimplicialModuleMap, SimplicialModuleMap)
        (symbol||, Matrix, Matrix)
///

doc ///
    Key
        (symbol++, SimplicialModuleMap, SimplicialModuleMap)
        (directSum, SimplicialModuleMap)
    Headline
        direct sum of simplicial module maps
    Usage
        h = f ++ g
        h = directSum(f,g,...)
        h = directSum(name1 => f, name2 => g, ...)
    Inputs
        f:SimplicialModuleMap
        g:SimplicialModuleMap
    Outputs
        h:SimplicialModuleMap
          that is the direct sum of the input simplicial module maps
    Description
        Text
            The direct sum of two simplicial module maps is a simplicial module map
            from the direct sum of the sources to the direct sum of
            the targets.

            First, we define some non-trivial maps of simplicial modules.
        Example
            R = ZZ/101[a..d];
            C1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], Degeneracy => true)
            C2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, Degeneracy => true)
            D1 = simplicialModule((freeResolution coker matrix{{a,b,c}}),2,  Degeneracy => true)
            D2 = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}[-1], 2, Degeneracy => true)
            f = randomSimplicialMap(D1, C1, Cycle => true)
            g = randomSimplicialMap(D2, C2, Cycle => true)
        Example
            h = f ++ g
            assert isWellDefined h
	    assert isSimplicialMorphism h
        Text
            The direct sum of any sequence of simplicial module maps can be 
            computed as follows.
        Example
            directSum(f, g, f[2])
            h2 = directSum(peanut => f, butter => g, jelly => f[2])
	    indices h2
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
        (symbol++, SimplicialModule, SimplicialModule)
        (symbol**, SimplicialModuleMap, SimplicialModuleMap)
        (symbol_, SimplicialModuleMap, Array)
///


doc ///
  Key
    (image, SimplicialModuleMap)
  Headline
    make the image of a map of simplicial modules
  Usage
    E = image f
  Inputs
    f : SimplicialModuleMap
  Outputs
    E : SimplicialModule
  Description
    Text
      If $f : C \to D$ is a map of simplicial modules of degree $d$,
      then the image is the simplicial module $E$ whose $i$-th term is $image(f_{i-d})$,
      and whose face/degeneracy map is induced from the face/degeneracy map 
      on the target.
    Text
      In the following example, we first construct a random
      simplicial morphism $f : C \to D$. 
    Example
      S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d),3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 0)
      prune image f
      prune normalize oo
      i = inducedMap(forgetComplex target f, image f)
      isSimplicialMorphism i
      normalize i
    Text
      There is a canonical map of simplicial modules from the image to the target.
    Example
      g1 = inducedMap(target f, image f)
      ker g1 == 0
      image g1 == image f
  SeeAlso
    "Making simplicial modules"
    "Making maps between simplicial modules"
    image
    (coimage, SimplicialModuleMap)
    (kernel, SimplicialModuleMap)
    (cokernel, SimplicialModuleMap)
///

doc ///
  Key
    (coimage, SimplicialModuleMap)
  Headline
    make the coimage of a map of simplicial modules
  Usage
    coimage f
  Inputs
    f : SimplicialModuleMap
  Outputs
    : SimplicialModule
  Description
    Text
      The coimage of a simplicial module map $f : C \to D$
      is the simplicial module $E$ whose $i$-th term is $coimage(f_i)$,
      and whose face/degeneracy map is induced from the face/degeneracy map 
      on the source.
    Text
      In the following example, we first construct a random
      simplicial morphism $f : C \to D$.  
    Example
      S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d), 3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 0)
      g1 = inducedMap(coimage f, source f)
      coimage g1 == coimage f
      coker g1 == 0
  Caveat
    The coimage is more computationally intensive than @TO (image, SimplicialModuleMap)@
    because, unlike {\tt image}, it computes kernels of maps of modules.
  SeeAlso
    "Making simplicial modules"
    "Making maps between simplicial modules"
    coimage
    (image, SimplicialModuleMap)
    (kernel, SimplicialModuleMap)
    (cokernel, SimplicialModuleMap)
///

doc ///
  Key
    (kernel, SimplicialModuleMap)
  Headline
    make the kernel of a map of simplicial modules
  Usage
    kernel f
    ker f
  Inputs
    f : SimplicialModuleMap
  Outputs
    : SimplicialModule
  Description
    Text
      The kernel of a simplicial module map $f : C \to D$
      is the simplicial module $E$ whose $i$-th term is $kernel(f_i)$,
      and whose face/degeneracy map is induced from the face/degeneracy map 
      on the source.
    Text
      In the following example, we first construct a random
      simplicial morphism $f : C \to D$. 
    Example
      S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d), 3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Boundary => true, InternalDegree => 0)
      prune ker f
      h1 = inducedMap(source f, ker f)
      ker f == image h1
      ker h1 == 0
  SeeAlso
    "Making simplicial modules"
    "Making maps between simplicial modules"
    ker
    (image, SimplicialModuleMap)
    (coimage, SimplicialModuleMap)
    (cokernel, SimplicialModuleMap)
///

doc ///
  Key
    (cokernel, SimplicialModuleMap)
  Headline
    make the cokernel of a map of simplicial modules
  Usage
    cokernel f
    coker f
  Inputs
    f : SimplicialModuleMap
  Outputs
    : SimplicialModule
  Description
    Text
      If $f : C \to D$ is a map of simplicial modules of degree $d$,
      then the cokernel is the simplicial module $E$ whose $i$-th term is $cokernel(f_{i-d})$,
      and whose face/degeneracy map is induced from the face/degeneracy map 
      on the target.
    Text
      In the following example, we first construct a random
      simplicial morphism $f : C \to D$. 
    Example
      S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d), 3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 0)
      prune coker f
      prune normalize oo
      prune HH coker f
      g1 = inducedMap(coker f, target f)
      coker f == image g1
      coker g1 == 0
  SeeAlso
    "Making simplicial modules"
    "Making maps between simplicial modules"
    cokernel
    (image, SimplicialModuleMap)
    (coimage, SimplicialModuleMap)
    (kernel, SimplicialModuleMap)
///


doc ///
    Key
        (symbol^, SimplicialModuleMap, Array)
        (symbol_, SimplicialModuleMap, Array)
    Headline
        the composition with the canonical inclusion or projection map
    Usage
        i = f_[name]
        p = f^[name]
    Inputs
        f:SimplicialModuleMap
        name:
    Outputs
        :SimplicialModuleMap
            {\tt i} is the composition of {\tt f} with the canonical inclusion and {\tt p} is
            the composition of the canonical projection with {\tt f}
    Description
        Text
            The direct sum is an n-ary operator with projection and
            inclusion maps from each component satisfying appropriate
            identities.

            One can access these maps as follows.  First, we define
            some non-trivial maps of simplicial modules.
        Example
            R = ZZ/101[a..d];
            C1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], 3, Degeneracy => true)
            C2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, 3, Degeneracy => true)
            D1 = simplicialModule((freeResolution coker matrix{{a,b,c}}), Degeneracy => true)
            D2 = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}[-1], 3, Degeneracy => true)
            f = randomSimplicialMap(D1, C1, Cycle => true)
            g = randomSimplicialMap(D2, C2, Cycle => true)
        Example
            h = f ++ g;
        Text
            The four basic maps are the inclusion from each summand of the source
            and the projection to each summand of the target.
        Example
            h_[0] == h * (C1 ++ C2)_[0]
            h_[1] == h * (C1 ++ C2)_[1]
            h^[0] == (D1 ++ D2)^[0] * h
            h^[1] == (D1 ++ D2)^[1] * h
        Text
            These can be combined to obtain the blocks of the map of simplicial modules.
        Example
            h_[0]^[0] == f
            h_[1]^[1] == g
            h_[0]^[1] == 0
            h_[1]^[0] == 0
        Text
            The default names for the components are the non-negative
            integers.  However, one can choose any name.
        Example
            h = (chicken => f) ++ (nuggets => g);
	    indices h
            h_[chicken]^[chicken] == f
            h_[nuggets]^[nuggets] == g
    SeeAlso
        (symbol++, SimplicialModule, SimplicialModule)
        (symbol^, SimplicialModule, Array)
        (symbol_, SimplicialModule, Array)
        (directSum, SimplicialModule)
        (components, SimplicialModule)
        indices
///

doc ///
    Key
        (randomSimplicialMap, SimplicialModule, SimplicialModule)
        randomSimplicialMap
        [randomSimplicialMap, Boundary]
        [randomSimplicialMap, Cycle]
        [randomSimplicialMap, Degree]
        [randomSimplicialMap, InternalDegree]
        Cycle
        Boundary
        InternalDegree
    Headline
        a random map of simplicial modules
    Usage
        f = randomSimplicialMap(C,D)
    Inputs
        C:SimplicialModule
        D:SimplicialModule
        Boundary => Boolean
            whether the constructed {\tt f} is a simplicial null homotopy
        Cycle => Boolean
            whether the constructed {\tt f} commutes with the face/degeneracy maps
        Degree => ZZ
            the degree of the constructed map of simplicial modules
        InternalDegree => List
            or @ofClass ZZ@
    Outputs
        f:SimplicialModuleMap
    Description
        Text
            A random simplicial module map $f : C \to D$ is obtained from choosing a random
	    map of the underlying normalizations, which uses the @TO randomComplexMap@ command.
        Example
            S = ZZ/101[a..c]
            C = simplicialModule(freeResolution coker matrix{{a*b, a*c, b*c}}, 3, Degeneracy => true)
            D = simplicialModule(freeResolution coker vars S, Degeneracy => true)
            f = randomSimplicialMap(D,C)
            assert isWellDefined f
            assert not isCommutative f
        Text
            When the random element is chosen with option {\tt Cycle => true}, the associated map of simplicial modules commutes
            with the face/degeneracy map.
        Example
            g = randomSimplicialMap(D,C, Cycle => true)
            assert isWellDefined g
            assert isCommutative g
            assert isSimplicialMorphism g
        Text
            When the random element is chosen with option {\tt Boundary => true}, the associated map of simplicial modules is a
	    simplicial null homotopy.
        Example
            h = randomSimplicialMap(D,C, Boundary => true)
            assert isWellDefined h
            assert isCommutative h
            assert isSimplicialMorphism h
            assert isNullHomotopic normalize h
            nullHomotopy normalize h
        Text
            When the degree of the random element is chosen with a specific degree,
            the associated map of simplicial modules will be a well-defined degree 0 simplicial
	    morphism mapping to the Dold-Kan image of the shift of the normalization. Thus,
	    even when specifying nonzero degree this constructor will still yield a simplicial
	    morphism.
        Example
            p = randomSimplicialMap(D, C, Cycle => true, Degree => -1)
            assert isWellDefined p
            assert isCommutative p
            assert isSimplicialMorphism p
        Text
            Given an internal degree, the random element is constructed as maps of modules with this degree.
        Example
            q = randomSimplicialMap(D, C, Boundary => true, InternalDegree => 2);
            assert isCommutative q
            assert isSimplicialMorphism q
            source q === C
            target q === D
            assert isNullHomotopic normalize q
///


doc ///
    Key
        (isShortExactSequence, SimplicialModuleMap, SimplicialModuleMap)
    Headline
        whether a pair of simplicial module maps forms a short exact sequence
    Usage
        isShortExactSequence(g, f)
    Inputs
        f:SimplicialModuleMap
        g:SimplicialModuleMap
    Outputs
        :Boolean
            that is @TO true@ if these form a short exact sequence
    Description
        Text
            A short exact sequence of simplicial modules 
            \[ 0 \to B \xrightarrow{f} C \xrightarrow{g} D \to 0\]
            consists of two morphisms of simplicial modules
            $f \colon B \to C$ and $g \colon C \to D$ such that
            $g f = 0$, $\operatorname{image} f = \operatorname{ker} g$, 
            $\operatorname{ker} f = 0$, and $\operatorname{coker} g = 0$.
        Text
            From a simplicial morphism $h \colon B \to C$, one obtains a
            short exact sequence
            \[ 0 \to \operatorname{image} h \to C \to \operatorname{coker} h \to 0. \]
        Example
            R = ZZ/101[a,b,c];
            B = simplicialModule(freeResolution coker matrix{{a^2*b, a*b*c, c^3}},  Degeneracy => true)
            C = simplicialModule(freeResolution coker vars R, 2, Degeneracy => true)
            h = randomSimplicialMap(C, B, Cycle => true)
            f = inducedMap(C, image h)
            g = inducedMap(coker h, C)
            assert isShortExactSequence(g,f)
        Text
            A short exact sequence of modules gives rise to a short
            exact sequence of simplicial modules.  These simplicial modules arise
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
            (g,f) = (horseshoeResolution SES)/simplicialModule;
            assert isShortExactSequence(g,f)
    SeeAlso
        "Basic invariants and properties"
///

doc ///
   Key
     (symbol SPACE, SimplicialModule, Array)
     (symbol SPACE, SimplicialModuleMap, Array)
   Headline
     shift a simplicial module or simplicial module map
   Usage
     D = C[i]
     g = f[i]
   Inputs
     C:SimplicialModule
       or {\tt f}, a @TO SimplicialModuleMap@
     :Array
       {\tt [i]}, where {\tt i} is an integer
   Outputs
     D:SimplicialModule
       or {\tt g}, a @TO SimplicialModuleMap@.
   Description
    Text
      The shifted simplicial module $D$ is not as simple to define as the
      shift in the category of chain complexes. This method naively normalizes the
      given simplicial module/map, applies the shift in the category of chain complexes,
      then applies the Dold-Kan functor to the result.

      As the following example shows, this is not the same thing as simply shifting
      all terms of the simplicial module.
    Example
      S = ZZ/101[a..d]
      D = simplicialModule(S^1, 5)
      D[-1]
      oo.dd
      D[-2]
      C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      dd^C_(3,0)
      ss^C_(2,0)
      D = C[1]
      assert isWellDefined D
      dd^D_(2,0)
      ss^D_(1,0)
    Text
      Notice that the face maps of the shifted simplicial module are not simply the negation
      of the face maps of the original. The shift operator is functorial, as illustrated below.
    Example
      C2 = simplicialModule(freeResolution (S^1/(a^2, b^2, c^2, d^2)), Degeneracy => true)
      C3 = simplicialModule(freeResolution (S^1/(a^2, b^3, c^4, d^5)), Degeneracy => true)
      f2 = simplicialModule(extend(C.complex, C2.complex, map(C_0, C2_0, 1)));
      f3 = simplicialModule(extend(C2.complex, C3.complex, map(C2_0, C3_0, 1)));
      isWellDefined (f2[-1])
      isSimplicialMorphism (f2[-1])
      isSimplicialMorphism (f3[-2])
   SeeAlso
      "Making simplicial modules"
///

doc ///
   Key
     (directSum, SimplicialModule)
     (symbol++, SimplicialModule, SimplicialModule)
   Headline
     direct sum of simplicial modules
   Usage
     D = C1 ++ C2
     D = directSum(C1,C2,...)
     D = directSum(name1 => C1, name2 => C2, ...)
   Inputs
     Ci:SimplicialModule
   Outputs
     D:SimplicialModule
       the direct sum of the input simplicial modules
   Description
    Text
      The direct sum of two simplicial modules is another simplicial module.
    Example
      S = ZZ/101[a,b,c];
      C1 = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      D1 = C1 ++ simplicialModule(complex(S^13)[-2], 3, Degeneracy => true)
      isWellDefined D1
      D1.?ss --knows to cache degeneracy maps if inputs have degeneracy maps
      C2 = simplicialModule(ideal(a,b,c), 3, Degeneracy => true)
      C1 ++ C2
      assert isWellDefined(C1 ++ C2)
    Text
      The direct sum of a sequence of simplicial modules can be computed as follows.
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
      Given a simplicial module which is a direct sum, we obtain the component
      simplicial modules and their names (indices) as follows.
    Example
      components C3
      indices C3
      components C4
      indices C4
   SeeAlso
     (components,SimplicialModule)
     indices
     (symbol^, SimplicialModule, Array)
     (symbol_, SimplicialModule, Array)
     (isShortExactSequence, SimplicialModuleMap, SimplicialModuleMap)
///



doc /// 
    Key
        (isSimplicialModule, SimplicialModule)
	isSimplicialModule
    Headline
        check if the simplicial identities hold for a simplicial object with degeneracy map keys
    Usage
        isSimplicialModule(S)
    Inputs
        S:SimplicialModule
            A simplicial module with face maps `dd` and degeneracy maps `ss`.
    Outputs
        :Boolean
            true if the simplicial identities hold, false otherwise
    Description
        Text
            This function checks if the simplicial identities hold for a given simplicial object `S` with face maps `dd` and degeneracy maps `ss`. The simplicial identities are a set of equations that need to be satisfied by the face and degeneracy maps of the simplicial object.
        Text
            Specifically, the function verifies the following identities:
            1. For face maps:
                \[ d_j d_i = d_i d_{j-1} \text{ for } 0 \leq i < j \leq n \]
            2. For face and degeneracy maps:
                \[ d_i s_j = s_{j-1} d_i \text{ for } i < j \]
                \[ d_j s_j = \text{id} \]
                \[ d_{j+1} s_j = \text{id} \]
                \[ d_k s_j = s_j d_{k-1} \text{ for } k > j+1 \]
            3. For degeneracy maps:
                \[ s_j s_i = s_i s_{j+1} \text{ for } i \leq j \]
        Text
            If any of these identities fail, the function returns `false`. If all identities are satisfied, the function returns `true`.
        Example
            R = QQ[a..d];
            f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
            f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
            C = simplicialModule(complex {f0, f1}, 3, Degeneracy => true)
            isSimplicialModule C
            dd^C
            ss^C
            dd^C*ss^C  --if C is simplicial, this should be all identity maps
        Text
            The zero simplicial module is well-defined.
        Example
            C = simplicialModule(R^0, 6, Degeneracy => true)
            isSimplicialModule C
    SeeAlso
        isWellDefined
///


doc /// 
    Key
        (forgetComplex, SimplicialModule)
	forgetComplex
	RememberSummands
    Headline
        forget the underlying complex data of a simplicial module obtained as a Dold-Kan image
    Usage
        forgetComplex(S)
    Inputs
        S:SimplicialModule
            A simplicial module, which is assumed to be obtained as a Dold-Kan image.
        RememberSummands => Boolean
                Default value is true. Indicates whether to remember the component summands of the simplicial module when creating the new simplicial module.
    Outputs
        :SimplicialModule
            A new simplicial module that is no longer viewed as a Dold-Kan image of some complex
    Description
        Text
            This function removes the data of the underlying complex from a simplicial module $S$ that is obtained as a Dold-Kan image.
	    The function checks if the simplicial module has an associated complex and, if so, it reconstructs the simplicial module without the complex data
	    while preserving the face and degeneracy maps.
        Text
            If the option `RememberSummands` is set to true (the default), the function will remember the summands
	    of the simplicial module when reconstructing it. The face and degeneracy maps of the original simplicial module
	    are preserved in the new simplicial module.
	    This function is good for testing that the normalization of the Dold-Kan functor recovers the original complex,
	    since the @TO normalize@ command by default first checks if a simplicial module is obtained as a Dold-Kan image
	    before attempting a more costly computation.
        Example
            R = ZZ/101[x_1..x_3];
	    K = koszulComplex vars R
	    S = simplicialModule(K,4, Degeneracy => true)
	    S.?complex
	    fS = forgetComplex S
	    components fS_3
	    ffS = forgetComplex(S, RememberSummands => false)
	    components ffS_3
	    Kn = normalize fS
	    Knn = normalize ffS
	    Kn.dd
	    K == prune Kn
    SeeAlso
        normalize
	forgetDegeneracy
///



doc /// 
    Key
        (forgetDegeneracy, SimplicialModule)
	forgetDegeneracy
    Headline
        forget the data of degeneracy maps of a simplicial object
    Usage
        forgetDegeneracy(S)
    Inputs
        S:SimplicialModule
            A simplicial module whose degeneracy maps are to be forgotten.
    Outputs
        :SimplicialModule
            The simplicial module S, but with no degeneracy maps stored.
    Description
        Text
            This function removes the data of degeneracy maps from a simplicial module `S`.
	    It is useful when the user wants to ignore degeneracy maps for the purpose of speeding up computations or simplifying the simplicial object.
        Example
	    Q = ZZ/101[a..d]
	    K = koszulComplex vars Q
	    S = simplicialModule(K, 6, Degeneracy => true)
	    elapsedTime S**S
	    fS = forgetDegeneracy S
	    elapsedTime fS**fS --faster when degeneracy is ignored
	Text
	    The change in speed becomes much more noticeable as ranks get larger.
    SeeAlso
        forgetComplex
///



 
doc /// 
    Key
        tensorwithComponents
        (tensorwithComponents, Module, Module)
	(tensorwithComponents, Matrix, Matrix)
	(tensorwithComponents, List)
    Headline
        compute the tensor product of direct summands, caching the components for easy access
    Usage
        tensorwithComponents(M, N)
    Inputs
        M:Module
	N:Module
            Two modules whose tensor product of direct summands is to be computed.
    Outputs
        :Module
            The tensor product of M and N, with cached components for easier access
    Description
        Text
            This function computes the tensor product of two modules M and N, but additionally caches the components based on the component indices of the original modules.
	    This caching is particularly useful for easily accessing induced maps on components of tensor products of direct sums.
        Text
            If M and N have cached index components, then this function will use those indices. This function
	    is mostly used for extracting components of the simplicial tensor product, since the full
	    face/degeneracy maps are typically much too large to be displayed on their own.
        Example
            Q = ZZ/101[a..b]
	    M = Q^2++Q^3
	    N = Q^3++Q^4
	    components(M**N) --all components of M and N have been forgotten
	    T = tensorwithComponents(M,N)
	    components T
	    indices T
	Text
	    This method also works for lists of modules, for purposes of iterating:
	Example
	    tensorwithComponents {M,M,M}
	    indices oo
	Text
	    This method is also functorial, which makes it easy to restrict maps
	    to tensor summands:
	Example
	    L = directSum {Q^2, Q^3, Q^4}
            f = L_[0,2]
	    phi = tensorwithComponents(f, f)
	    phi^[{0,2}]_[{0,1}]
///


 
doc /// 
    Key
        (naiveNorm, SimplicialModule, ZZ)
	(naiveNorm, SimplicialModule)
	(naiveNorm, Complex, ZZ)
	naiveNorm
    Headline
        compute the naive normalization of a simplicial object
    Usage
        naiveNorm(S, n)
    Inputs
        S:SimplicialModule
	    A simplicial module or @TO Complex@
	n:ZZ
             and an integer n specifying the top degree for normalization.
    Outputs
        :Complex
            The naive normalization complex of S, where the differential is the alternating sum of face maps.
    Description
        Text
            This function computes the naive normalization of a simplicial object S. The naive normalization is a complex built from the modules of the simplicial object,
	    with a differential that is the alternating sum of the face maps. In general the naive normalization
	    is homotopy equivalent to the normalization (see @TO normalize@), but is much bigger in general.
	Example
	    Q = ZZ/101[a..d]
	    K = koszulComplex vars Q
	    S = simplicialModule(K, Degeneracy => true)
	    nK = naiveNorm(S)
	    isWellDefined nK
	    prune HH nK
	Text
	    Note that in the above, the naive normalization will always be an infinite complex,
	    so there will always be extraneous homology at the tail end. Note in this case that
	    the homology of the naive normalization is precisely the homology of {\tt K}, as it should
	    be (in fact, it is homotopy equivalent to {\tt K}).
    SeeAlso
        normalize
///


 
doc /// 
    Key
        schurMap
        (schurMap, List, SimplicialModule)
	(schurMap, List, SimplicialModuleMap)
	(schurMap, List, Complex)
	(schurMap, List, ComplexMap)
	TopDegree
    Headline
        construct the Dold-Puppe extension of the Schur functor
    Usage
        schurMap(lambda, S, Options => {Degeneracy => false, TopDegree => null})
    Inputs
        lambda: List
            A list representing a partition.
        S: SimplicialModule
            or @TO SimplicialModuleMap@, or @TO Complex@, or @TO ComplexMap@ to apply the Schur functor extension.
        Degeneracy => Boolean
                Default value is false. Indicates whether to include degeneracy maps in the construction.
        TopDegree => ZZ
                Default is null. If provided, specifies the top degree for the construction of the simplicial module.
    Outputs
        : SimplicialModule
            The Dold-Puppe extension of the Schur functor applied to the simplicial module S.
    Description
        Text
            This function constructs the Dold-Puppe extension of the Schur functor to the category of simplicial modules,
	    applied to a simplicial module S or a complex (or maps thereof). The construction involves
	    creating a simplicial module where each component is a Schur functor applied to the corresponding component of S.
        Text
	    By default, degeneracy maps are not computed in this method since it adds additional computation time
	    that is not necessary for computing the normalization or many of the invariants of interest. However,
	    if the user is interested in having degeneracy maps, use the option {\tt Degeneracy => true}.
	Example
	    Q = ZZ/101[a..b]
	    K = koszulComplex vars Q;
	    S = simplicialModule(K, 4, Degeneracy => true)
	    S2S = elapsedTime schurMap({2}, S)
	    elapsedTime schurMap({2}, S, Degeneracy => true)
	Text
	    In general, if a polynomial functor has degree $d$, the Dold-Puppe extension of a functor
	    applied to a chain complex of length $t$ will have length at most $d \cdot t$. We can see this
	    explicitly in the following example:
	Example
	    S = simplicialModule(K, 5)
	    S2 = schurMap({2}, S)
	    prune normalize S2 --notice the output has length 4
	    minimize oo
	Text
	    If the input is a complex, then the default top degree is taken to be the degree of the
	    Schur functor multiplied by the length of the complex. Computationally, this upper bound
	    is often too big to be computed at the moment, so the user may need to specify a top degree
	    by using the {\tt TopDegree => d} option.
	Example
            Q = ZZ/101[a..c]
	    K = koszulComplex vars Q;
	    S2K = elapsedTime prune schurMap({2}, K, TopDegree => 4)
	    (minimize S2K).dd --ignore the last differential
	    --S21K = elapsedTime prune schurMap({2,1}, K, TopDegree => 3) --top degree 4 takes ~1 minute
	Text
	    These functors are particularly interesting in the modular setting, i.e., when the characteristic
	    of the underlying field is small relative to the degree of the Schur functor. In this case,
	    the induced complexes will have different homotopy classes as the characteristic varies.
	Example
            needsPackage "ChainComplexOperations"
	    Q = ZZ/2[a,b]
	    K = koszulComplex vars Q;
	    S2K = minimize prune schurMap({2}, K)
	    S2K' = sym2 K --the "naive" sym2 functor
	    S2K.dd
	    S2K'.dd
	    prune HH S2K
	    prune HH S2K' --not quasi-isomorphic!
	    Q = ZZ/3[a,b];
	    K = koszulComplex vars Q;
	    prune HH schurMap({2}, K)
	    prune HH sym2 K --quasi-isomorphic in all other characteristics
	    --S3K = elapsedTime minimize prune schurMap({3}, K) --takes 23 seconds
	    --S21K = elapsedTime minimize prune schurMap({2,1}, K) --takes 17 seconds
	    --S21K.dd
	    --prune HH S21K
	Text
	    This method is also implemented in a functorial way.
	Example
            Q = ZZ/2[a,b]
	    K = koszulComplex vars Q;
	    F = freeResolution( (ideal vars Q)^2)
	    phi = extend(K, F, id_(K_0))
            f = elapsedTime prune schurMap({2}, phi)
	    isCommutative f
	    isWellDefined f
	    prune HH source f
	    prune HH target f
	    prune HH f
    Caveat
        As many of the above examples show, this method can quickly become
	computationally infeasible when the modules in the complex have too big of rank.
	This seems to stem more from the efficiency of the @TO schur@ method, since methods such
	as the tensor product still run very quickly even with huge matrices.
    SeeAlso
        extPower
	simplicialTensor
	symmetricQuotient
///


 
doc /// 
    Key
        extPower
        (extPower, ZZ, SimplicialModule)
	(extPower, ZZ, SimplicialModuleMap)
	(extPower, ZZ, Complex)
	(extPower, ZZ, ComplexMap)
    Headline
        compute the Dold-Puppe extension of the exterior power functor to simplicial modules
    Usage
        extPower(d, S, Options => {Degeneracy => false, TopDegree => null})
    Inputs
        d: ZZ
            An integer representing the degree of the exterior power.
        S: SimplicialModule
            A simplicial module to which the exterior power functor extension is applied.
        Degeneracy => Boolean
            Default value is false. Indicates whether to include degeneracy maps in the construction.
        TopDegree => ZZ
            Default is null. If provided, specifies the top degree for the construction of the simplicial module.
    Outputs
        : SimplicialModule
            The Dold-Puppe extension of the exterior power functor applied to the simplicial module S.
    Description
        Text
            This function computes the Dold-Puppe extension of the exterior power functor to the category of chain complexes.
	    It can be applied to a simplicial module, a complex, or maps thereof. This method is equivalent
	    to using @TO schurMap@ for the partition $(1, 1, \dots , 1)$, but is generally much faster.
	Example
	    Q = ZZ/2[a,b]
	    K = koszulComplex vars Q
	    w3K = elapsedTime prune extPower(3, K) 
	    --elapsedTime prune schurMap({1,1,1}, K) --takes much longer
	    (minimize w3K).dd
	    prune HH w3K
	Text
	    This method is also implemented in a functorial way:
	Example
	    F = freeResolution( (ideal vars Q)^2)
	    phi = extend(K, F, id_(K_0))
            f = elapsedTime prune extPower(2, phi)
	    prune HH f
	Text
	    Since this method runs significantly faster than using {\tt schurMap}, we can compute the
	    full exterior power complex of a longer complex:
	Example
	    Q = ZZ/2[a..c];
	    K = koszulComplex vars Q
	    w2K = prune extPower(2, K)
	    (minimize w2K).dd
	    prune HH w2K --has more interesting homology than in the nonmodular case
	    needsPackage "ChainComplexOperations"
	    Q = ZZ/3[a..c]
	    K = koszulComplex vars Q
	    prune HH wedge2 K --notice: homology concentrated in odd degrees
    Caveat
        This method may take a very long time to run if the user inputs a large/long complex.
	Use the option {\tt TopDegree => d} to only run the computation up to a certain degree.
    SeeAlso
        schurMap
	exteriorInclusion
	symmetricQuotient
	simplicialTensor
///



doc /// 
    Key
        simplicialTensor
        (simplicialTensor, List)
	(simplicialTensor, Complex, Complex)
	(simplicialTensor, ComplexMap, ComplexMap)
	(simplicialTensor, ZZ, Complex)
	(simplicialTensor, ZZ, SimplicialModule)
    Headline
        compute the simplicial tensor product and cache direct sum indices for easy access
    Usage
        simplicialTensor(T, Options => {Degeneracy => false, TopDegree => null})
    Inputs
        T: List
            A list of simplicial modules or complexes to compute the tensor product.
        Degeneracy => Boolean
            Default value is false. Indicates whether to include degeneracy maps in the construction.
        TopDegree => ZZ
            Default is null. If provided, specifies the top degree for the construction of the simplicial module.
    Outputs
        : SimplicialModule
            The simplicial tensor product of the components in T, with cached direct sum indices.
    Description
        Text
            This function computes the simplicial tensor product of a list T, which can consist of simplicial modules or complexes.
	    It caches direct sum indices using @TO tensorwithComponents@ so that the user can easily access
	    components of the resulting face/degeneracy maps on particular direct summands of the tensor product.

	    The simplicial tensor product of complexes is built as the Dold-Kan extension of the tensor product
	    functor on the category of R-modules. In general, this complex looks quite different from the
	    standard tensor product of complexes. In fact, the simplicial tensor product is always homotopy
	    equivalent to the classically defined tensor product.
        Example
	    Q = ZZ/101[x_1,x_2];
	    K1 = complex {matrix{{x_1}}};
	    K2 = complex {matrix{{x_2}}};
	    T1 = K1**K2
	    T1.dd
	    T2 = prune simplicialTensor({K1,K2})
	    T2.dd
	    phi1 = extend(T1,T2,id_(T1_0))
	    phi2 = extend(T2,T1,id_(T1_0))
	    phi1*phi2 == id_T1
	    isNullHomotopic(phi2*phi1 - id_T2)
	Text
	    Here is how to access specific components of the face maps for a tensor product of simplicial
	    modules:
	Example
	    S1 = simplicialModule(K1, 4)
	    S2 = simplicialModule(K2, 4)
	    S12 = S1**S2
            indices S12_4 --lists the indices of the summands
	    netList flatten for i in indices S12_4 list (
		for j in indices S12_3 list ( if (dd^S12_(4,0))_[i]^[j]==0 then continue else
		    horizontalJoin {net (dd^S12_(4,0))_[i]^[j], " : ", net i, " --> " , net j} ))
        Text
	    One reason for using the simplicial tensor product
	    is that its components as a simplicial module are more canonically built, and thus naturally
	    extend functorial maps on the category of R-modules to the category of chain complexes.

	    We can see actually see this in an example. The exterior power functor admits a canonical
	    comultiplication map
	    $$\bigwedge^{i+j} \to \bigwedge^i \otimes \bigwedge^j.$$
	    This means that for any chain complex $C$ there is a canonical inclusion of complexes
	    $$\bigwedge^{i+j} C \hookrightarrow \bigwedge^i C \otimes \bigwedge^j C.$$
	    Constructing this inclusion directly using the naive definition of the exterior power functor
	    on complexes would be extremely unnatural, but filtering through the simplicial category
	    makes this a very easy task:
	Example
	    Q = ZZ/101[a,b];
	    K = koszulComplex vars Q
	    SK = simplicialModule(K,6) --want top degree 6 since the resulting complexes should have length 6
	    w21K = extPower(2, SK) ** SK
	    w3K = extPower(3, SK)
	    H = hashTable for i from 0 to 6 list i => dual wedgeProduct(2,1, dual SK_i);
	    inclusion = map(w21K, w3K, H);
	    isWellDefined inclusion
	    isCommutative inclusion
	    prune ker inclusion --should be 0 since it is an inclusion
	    S21' = complex { matrix {{a, b, 0, 0, -b}, {0, 0, a, b, a}}, matrix {{-b, b, 0, 0,
                                             b, 0}, {a, -a, 0, 0, 0, b}, {0, 0, -b, b, -a, 0}, {0, 0, a, -a, 0, -a}, {0, 0, 0, 0, a, b}}, 
                                            matrix {{a, b, 0, -b, -b}, {a, b, 0, 0, -2*b}, {0, a, b, a, 0}, {0, a, b, 0, a}, {0, 0, 0,
                                              -b, b}, {0, 0, 0, a, -a}}, matrix {{3*b, 0}, {-a, 2*b}, {0, -3*a}, {a, b}, {a, b}}}
	    S21 = S21'[-1]  --this is the S^(2,1) schur functor; note the ranks
	    prune HH S21 --note the homology
	Text
	    Just for sake of illustration, let us see how the above example changes in the modular
	    setting; note that the only characteristic for which the inclusion
	    $$\bigwedge^3 \hookrightarrow \bigwedge^2 \otimes \bigwedge^1$$
	    is not canonically split is for characteristic 3, so we should expect the Dold-Kan
	    extension of the Schur functor $\mathbb{S}^{(2,1)} (K)$ to look different in characteristic 3.
	    (the resulting minimization of $\mathbb{S}^{(2,1)} (K)$ should not have coefficients, for instance).
	Example
	    Q = ZZ/3[a,b];
	    K = koszulComplex vars Q
	    SK = simplicialModule(K,6) --want top degree 6 since the resulting complexes should have length 6
	    w21K = extPower(2, SK) ** SK
	    w3K = extPower(3, SK)
	    H = hashTable for i from 0 to 6 list i => dual wedgeProduct(2,1, dual SK_i);
	    inclusion = map(w21K, w3K, H);
	    --inc = prune normalize inclusion;
	    --next commands are commented out since they are longer computations
	    --S21K = prune coker inc --the char 3 simplicial schur functor
	    --(minimize S21K).dd --complex is longer! All other cases have ranks (2,5,6,5,2)
	    --prune HH S21K --more homology as well!
	Text
	    Note that the above method of computing the Schur functor $\mathbb{S}^{(2,1)} (K)$ is
	    significantly faster than the {\tt schurMap} command.

	    This method is also implemented functorially, so it can be applied to simplicial maps
	    and complex maps.
	Example
	    F = freeResolution( (ideal vars Q)^2)
	    phi = extend(K, F, id_(K_0))
	    prune simplicialTensor(phi, phi)
    Caveat
        The user should remember to prune the output upon normalizing a simplicial
	tensor product.
    SeeAlso
        extPower
	symmetricQuotient
///



doc ///
    Key
        (normalize, SimplicialModule, ZZ)
	(normalize, SimplicialModule)
	(normalize, SimplicialModuleMap, ZZ)
	(normalize, SimplicialModuleMap)
	CheckSum
	CheckComplex
        [(normalize, SimplicialModule, ZZ), CheckSum]
        [(normalize, SimplicialModule, ZZ), CheckComplex]
        [(normalize, SimplicialModule), CheckSum]
        [(normalize, SimplicialModule), CheckComplex]
        [(normalize, SimplicialModuleMap, ZZ), CheckSum]
        [(normalize, SimplicialModuleMap, ZZ), CheckComplex]
        [(normalize, SimplicialModuleMap), CheckSum]
        [(normalize, SimplicialModuleMap), CheckComplex]
    Headline
        normalization functor from simplicial modules to nonnegatively-graded chain complexes
    Usage
        normalize(S, d)
        normalize S
    Inputs
        S: SimplicialModule
            or @TO SimplicialModuleMap@, the object to be normalized.
        d: ZZ
            An integer specifying the degree up to which the normalization should be computed.
        CheckSum => Boolean
            Default value is true. If true and S has more than one component, computes the direct sum of normalized components.
        CheckComplex => Boolean
            Default value is true. If true and S contains a cached simplicial module or map, it outputs this cached value.
    Outputs
        : Complex
            or @TO ComplexMap@, the normalized nonnegatively-graded chain complex resulting from the normalization process.
    Description
        Text
            This function computes the normalization functor from the category of simplicial modules
	    to the category of nonnegatively-graded chain complexes. It is implemented in a functorial way,
	    applying both to simplicial modules and simplicial module maps.

	    The normalization of a simplicial module $S$ is by definition equal to the complex $N (S)$ with:
	    $$N(S)_n := \bigcap_{i=1}^n \ker d^S_{(n,i)},$$
	    with differential induced by the face map $d_{(n,0)}^S$. As currently implemented, the normalization
	    does not prune the output; the user should use {\tt prune} to obtain the best looking output.

	    The {\tt normalize} command is implemented so that it first checks whether a simplicial module
	    has been obtained as the Dold-Kan image of some complex. If it has, then it returns that complex
	    without doing any additional computation. If the user prefers that the normalization is computed
	    by definition instead of accessing this cached value, use the option {\tt CheckComplex => false}.
	Example
	    R = ZZ/101[x_1..x_3];
	    K = koszulComplex vars R
	    S = simplicialModule(K,10, Degeneracy => true)
	    keys S
	    K == normalize S
	    Kn = normalize(S, CheckComplex => false)
	    Kn.dd
	    K == Kn
	    K == prune Kn
	Text
	    For computational efficiency, this method also checks if the simplicial module has been constructed as a direct sum of simplicial
	    modules. If it has, then it returns the direct sum of the normalizations of each component. If the
	    user prefers that this shortcut is not taken, use {\tt CheckSum => false}.
	Example
	    S10 = directSum toList(10: forgetComplex S)
	    elapsedTime prune normalize S10
	    elapsedTime prune normalize(S10, CheckSum => false) --about 3-4 times slower; becomes significant for larger ranks
        Text
            The user may also specify the top homological degree to compute the normalization up to. Note that
	    this can help speed up computational time; if the user knows the normalization should have
	    a shorter length, then they should specify this upper bound in the syntax:
	Example
	    elapsedTime prune normalize(S10, 3, CheckSum => false) --MUCH FASTER!
        Text
            Again, this method is functorial, and when combined with other methods in this package
	    can be a particularly powerful way of obtaining nontrivial morphisms of complexes. We use this
	    method to obtain the image of the inclusion
	    $$\bigwedge^3 K \to \bigwedge^2 K \otimes K$$
	    for a Koszul complex K. Constructing this map directly using the naive definitions of
	    tensor products/exterior powers of complexes is not possible in full generality. Taking
	    advantage of the Dold-Kan correspondence and simplicial methods allows us to obtain
	    this inclusion explicitly.
	Example
	    Q = ZZ/3[a,b];
	    K = koszulComplex vars Q
	    SK = simplicialModule(K,6) --want top degree 6 since the resulting complexes should have length 6
	    w21K = extPower(2, SK) ** SK
	    w3K = extPower(3, SK)
	    H = hashTable for i from 0 to 6 list i => dual wedgeProduct(2,1, dual SK_i);
	    inclusion = map(w21K, w3K, H);
	    inc = prune normalize(inclusion,3);
	    isWellDefined inc
	    isCommutative inc
	Text
	    Taking the cokernel of the above morphism yields a canonically
	    defined Schur functor $\mathbb{S}^{(2,1)} (K)$; constructing this complex
	    using the classical Schur complex definition will yield a complex that does not
	    have finite length homology (and hence isn't even homotopically equivalent).
	Example
	    S21K = prune coker inc --this is only a snapshot; should go up to degree 6
	    (minimize S21K).dd
	    prune HH S21K --free part is because we truncated
    SeeAlso
	(directSum, SimplicialModule)
        naiveNorm
	extPower
        simplicialTensor
///


doc /// 
    Key
        exteriorInclusion
	(exteriorInclusion, SimplicialModule)
	(exteriorInclusion, Complex, ZZ)
	(exteriorInclusion, Complex)
        (exteriorInclusion, Module)
    Headline
        computes the image of the 2nd exterior power into the tensor product
    Usage
        exteriorInclusion(S)
    Inputs
        M:SimplicialModule
            or @TO Complex@ or @TO Module@, optional integer argument specifies top degree
    Outputs
        :SimplicialModuleMap
            or @TO Matrix@, the map representing the image of the 2nd exterior power of $S$ into the tensor product $S \otimes S$.
    Description
        Text
            Given a simplicial module $S$ (or a complex), this function computes the map
	    $$\bigwedge^2 S \to S \otimes S,$$
	    The cokernel of
	    this map is by definition the second symmetric power of $S$. This method
	    is mainly used in conjunction with the @TO tensorLES@ command to compute induced
	    maps on homology for canonical short exact sequences.
	Example
	    Q = ZZ/101[a,b,c]
	    K = koszulComplex vars Q
	    --elapsedTime schurMap({2}, K) --takes some time
	    phi = elapsedTime exteriorInclusion(K,3); --specify top degree 3
	    isWellDefined phi
	    isCommutative phi
	    prune coker phi
        Text
	    All of the homology of the second symmetric/exterior powers are guaranteed to be concentrated
	    in degrees $0$ to $3$ in the above example, so it suffices to compute only $4$ terms to
	    understand all of the homology.
	Example
	    for i to 3 list prune HH_i source phi
	    for i to 3 list prune HH_i (coker phi)
	Text
	    Notice that since the tensor square splits outside of characteristic 2, the symmetric power
	    picks up the even degree homology and the exterior square picks up the odd homology. In
	    characteristic 2 this changes:
	Example
	    Q = ZZ/2[a,b,c]
	    K = koszulComplex vars Q
	    phi = elapsedTime exteriorInclusion(K,3); --specify top degree 3
	    isWellDefined phi
	    isCommutative phi
	    for i to 2 list prune HH_i source phi
	    for i to 2 list prune HH_i (coker phi)
    SeeAlso
        symmetricQuotient
	extPower
	simplicialTensor
///

 

 
doc /// 
    Key
        symmetricQuotient
	(symmetricQuotient, SimplicialModule)
	(symmetricQuotient, Complex, ZZ)
	(symmetricQuotient, Complex)
        (symmetricQuotient, Module)
    Headline
        computes the image of the surjection from the simplicial tensor product onto the second symmetric power of a simplicial module
    Usage
        symmetricQuotient(S)
    Inputs
        S: SimplicialModule
            or @TO Complex@ or @TO Module@, optional integer argument specifies top degree
    Outputs
        : SimplicialModuleMap
            The induced map representing the image of the surjection from the simplicial tensor product onto the second symmetric power of S.
    Description
        Text
            This function computes the induced map on the cokernel of the map constructed by
	    the @TO exteriorInclusion@ method, which is explicitly giving the map
	    $$S \otimes S \to \operatorname{Sym}^2 (S).$$
	    Let us see some examples:
	Example
	    Q = ZZ/2[a,b];
	    K = koszulComplex vars Q;
	    phi = prune symmetricQuotient(K,4) 
	    isWellDefined phi
	    isCommutative phi
	    prune coker phi == 0
	    prune HH phi
	    prune coker oo --the induced map on homology is NOT surjective, in contrast to the case when 2 is a unit
    SeeAlso
        tensorLES
	exteriorInclusion
	simplicialTensor
///

 


doc /// 
    Key
        tensorLES
        (tensorLES, Complex, ZZ)
    Headline
        computes the long exact sequence of homology induced by the canonical short exact sequence of complexes
    Usage
        tensorLES(C, d)
    Inputs
        C: Complex
            A complex.
        d: ZZ
            An integer specifying the top degree for constructing the simplicial module.
    Outputs
        : Complex
            The long exact sequence of homology induced by the canonical short exact sequence \( 0 \to \wedge^2 C \to T^2 C \to Sym^2 C \to 0 \).
    Description
        Text
            This function computes the long exact sequence of homology associated with the canonical short exact sequence of complexes:
            \( 0 \to \bigwedge^2 C \to C \otimes C \to \operatorname{Sym}^2 C \to 0 \).
        Text
            This function first computes the exterior inclusion map on the complex C up to degree d using the @TO exteriorInclusion@ function.
            Then it constructs the induced map on the cokernel of this exterior inclusion map, which is the surjection to the second symmetric power of C.
            Finally, it computes the long exact sequence of homology using the @TO longExactSequence@ function on the pruned maps of the induced maps.

	    This long exact sequence is only interesting in characteristic 2, since the above sequence is split whenever 2 is a unit,
	    so there are no interesting connecting homomorphisms. Let's see some examples in characteristic 2 of the
	    connecting homomorphisms:
	Example
	    Q = ZZ/2[a,b]
	    K = koszulComplex vars Q
	    prune tensorLES(K,4)
	    oo.dd_6 --nontrivial connecting homomorphism
	    F = freeResolution( (ideal vars Q)^3)
	    prune tensorLES(F,4)
	    L = complex {K.dd_1, map(source K.dd_1,target K.dd_2 ,K.dd_2*K.dd_1), K.dd_2}
	    hL = elapsedTime prune tensorLES(L,4)
	    netList {hL.dd_3, hL.dd_6, hL.dd_9, hL.dd_12, hL.dd_15} --two nontrivial connecting homs
    SeeAlso
        exteriorInclusion
	simplicialTensor
 ///

-- Option doc stubs

doc ///
    Key
        [simplicialModule, Degeneracy]
    Headline
        whether to compute degeneracy maps
    Usage
        simplicialModule(..., Degeneracy => true)
    Description
        Text
            If {\tt Degeneracy => true}, the degeneracy maps are also computed
            alongside the face maps. By default, degeneracy maps are not computed.
    SeeAlso
        simplicialModule
///

doc ///
    Key
        [forgetComplex, RememberSummands]
    Headline
        whether to remember direct sum structure
    Usage
        forgetComplex(..., RememberSummands => true)
    Description
        Text
            If {\tt RememberSummands => true}, the direct sum structure
            of the terms is remembered when forgetting the underlying complex.
    SeeAlso
        forgetComplex
///

doc ///
    Key
        [extPower, Degeneracy]
    Headline
        whether to compute degeneracy maps
    Usage
        extPower(..., Degeneracy => true)
    Description
        Text
            If {\tt Degeneracy => true}, the degeneracy maps are also computed
            alongside the face maps. By default, degeneracy maps are not computed.
    SeeAlso
        extPower
///

doc ///
    Key
        [extPower, TopDegree]
    Headline
        top degree for the construction
    Usage
        extPower(..., TopDegree => n)
    Description
        Text
            Specifies the top simplicial degree for the resulting simplicial module.
    SeeAlso
        extPower
///

doc ///
    Key
        [schurMap, Degeneracy]
    Headline
        whether to compute degeneracy maps
    Usage
        schurMap(..., Degeneracy => true)
    Description
        Text
            If {\tt Degeneracy => true}, the degeneracy maps are also computed
            alongside the face maps. By default, degeneracy maps are not computed.
    SeeAlso
        schurMap
///

doc ///
    Key
        [schurMap, TopDegree]
    Headline
        top degree for the construction
    Usage
        schurMap(..., TopDegree => n)
    Description
        Text
            Specifies the top simplicial degree for the resulting simplicial module.
    SeeAlso
        schurMap
///

doc ///
    Key
        [simplicialTensor, Degeneracy]
    Headline
        whether to compute degeneracy maps
    Usage
        simplicialTensor(..., Degeneracy => true)
    Description
        Text
            If {\tt Degeneracy => true}, the degeneracy maps are also computed
            alongside the face maps. By default, degeneracy maps are not computed.
    SeeAlso
        simplicialTensor
///

doc ///
    Key
        [simplicialTensor, TopDegree]
    Headline
        top degree for the construction
    Usage
        simplicialTensor(..., TopDegree => n)
    Description
        Text
            Specifies the top simplicial degree for the resulting simplicial module.
    SeeAlso
        simplicialTensor
///


doc ///
    Key
        [minimalPresentation, Exclude]
    Headline
        exclude option for minimal presentation
    Description
        Text
            This option is inherited from the core @TO minimalPresentation@ method.
    SeeAlso
        (minimalPresentation, SimplicialModule)
///

doc ///
    Key
        [map, Degree]
    Headline
        specify the degree of a map of simplicial modules
    Description
        Text
            When constructing a map of simplicial modules, this option
            specifies the degree $d$ of the resulting map.  A map of
            degree $d$ consists of component maps $f_i : C_i \to D_{i+d}$.
            By default, the degree is 0.
    SeeAlso
        (map, SimplicialModule, SimplicialModule, HashTable)
///

doc ///
    Key
        [map, DegreeLift]
    Headline
        unused option for maps of simplicial modules
    Description
        Text
            This option is inherited from the core map method
            and is not used for maps of simplicial modules.
///

doc ///
    Key
        [map, DegreeMap]
    Headline
        unused option for maps of simplicial modules
    Description
        Text
            This option is inherited from the core map method
            and is not used for maps of simplicial modules.
///

doc ///
    Key
        [inducedMap, Degree]
    Headline
        specify the degree of an induced map of simplicial modules
    Description
        Text
            When constructing an induced map of simplicial modules,
            this option specifies the degree of the resulting map.
            By default, the degree is 0.
    SeeAlso
        (inducedMap, SimplicialModule, SimplicialModule)
///


