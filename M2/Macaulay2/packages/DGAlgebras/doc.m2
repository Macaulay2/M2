doc ///
  Key
    DGAlgebras
  Headline
    Data types and basic functions on differential graded (DG) Algebras.
  Description
    Text
      This package is used to define and manipulate DG algebras.
  Contributors
    Some documentation was added by Daniel Rostamloo and David Eisenbud.
  Subnodes
    "Basic operations on DG Algebras"
    "The Koszul complex as a DG Algebra"
    "Basic operations on DG Algebra Maps"
    "Basic operations on DG Module Maps"
    "Base change and tensor with non-DG types"
    "Operations on DG Ideals"
    "Building DG modules, submodules, and quotients"
    "Module-like operations on DG modules"
    "Operations on DG Submodules"
    "Image, kernel, and cokernel of DG module maps"
    "Homology of DG modules and DG module maps"
    "Pruning DG modules, submodules, quotients, and maps"
    "Well-definedness, acyclicity, and quasi-isomorphism"
    "Semifree resolutions of DG modules"
    "Computing module differentials and visualizing DG modules"
    "Building DG algebras from existing DG algebras"
    "Low-level differential computations and validity checks"
    "Accessors and cache management"
    "DG module primitives and chain-complex export"
///

doc ///
  Key
    "Basic operations on DG Algebras"
  Headline
    Outlines some basic operations on DG Algebras
  Description
    Text
      There are several ways to define a DGAlgebra.  One can start by defining one 'from scratch'.  One does
      this by specifying the ring over which the DGAlgebra is defined and the degrees of the generators.  The
      name of the generators of the DGAlgebra by default is $T_i$, but one may change this by specifying the
      optional (string) argument 'Variable'.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{1,1}})
    Text
      The command freeDGAlgebra only defines the underlying algebra of A, and not the differential.  To set the differential of A,
      one uses the command setDiff.
    Example
      setDiff(A, gens R)
    Text
      Note that the above is the (graded) Koszul complex on a set of generators of R.  A much easier way to define this is to use the
      function koszulComplexDGA.
    Example
      B = koszulComplexDGA(R, Variable=>"S")
    Text
      One can compute the homology algebra of a DGAlgebra using the homology (or HH) command.
    Example
      HB = HH B
      describe HB
      degrees HB
    Text
      Note that since R is a complete intersection, its Koszul homology algebra is an exterior algebra, which is a
      free graded commutative algebra.  Note that the internal degree is preserved in the computation of the homology algebra
      of B.
    Text
      One can also adjoin variables to kill cycles in homology.  The command killCycles looks for the first positive degree
      nonzero homology (say i), and adjoins variables in homological degree i+1 that differentiate to a minimal generating set of this homology, so that the
      resulting DGAlgebra now only has homology in degree greater than i (note of course this could introduce new homology in higher degrees).
      The command adjoinVariables allows finer control over this procedure.  See @ TO adjoinVariables @ for an example.
    Example
      HB.cache.cycles
      C = adjoinVariables(B,{first HB.cache.cycles})
      homologyAlgebra(C,GenDegreeLimit=>4,RelDegreeLimit=>4)
      C = killCycles(B)
      homologyAlgebra(C,GenDegreeLimit=>4,RelDegreeLimit=>4)
    Text
      Again, note that since R is a complete intersection, once we adjoin the variables in homological degree two to kill the cycles in degree one,
      we obtain a minimal DG Algebra resolution of the residue field of R.  Also, note that since C has generators in even degree, one must specify the
      optional arguments GenDegreeLimit and RelDegreeLimit to specify the max degree of the computation.  To do this, one uses the homologyAlgebra command
      rather than the HH command.
    Text
      This computation could have also been done with the command acyclicClosure.  The command acyclicClosure performs the command killCycles sequentially to ensure that the
      result has homology in higher and higher degrees, thereby computing (part of) a minimal DG Algebra resolution of the residue field.  acyclicClosure has an optional
      argument EndDegree that allows the user to specify the maximum homological degree with which to perform this adjunction of variables.  The default value of this is 3, since if there
      are any variables of degree 3 that need to be added, then each subsequent homological degree will require some variables to be adjoined (Halperin's rigidity theorem).
    Example
      D = acyclicClosure R
      R' = ZZ/101[x,y,z]/ideal{x^2,y^2,z^2,x*y*z}
      E = acyclicClosure(R',EndDegree=>5)
      tally degrees E.natural
    Text
      As you can see, since R' is not a complete intersection, the acyclic closure of E requires infinitely many variables; we display the degrees of the first 6 here.
      The tally that is displayed gives the deviations of the ring R.  One can compute the deviations directly from any minimal free resolution of the residue field
      of R', so that using the one provided by res coker vars R is faster.  To do this, use the command @ TO deviations @.
    Example
      deviations(R,DegreeLimit=>6)
      deviations(R',DegreeLimit=>6)
    Text
      As a brief warning, the command @ TO poincareN @ which is used in @ TO deviations @ uses the symbols S and T internally, and may cause problems referring to such rings at the Macaulay2 prompt.
///

doc ///
  Key
    "The Koszul complex as a DG Algebra"
  Headline
    an example
  Description
    Text
      The Koszul complex on a sequence of elements $f_1,\dots,f_r$ is a complex of R-modules whose underlying graded R-module
      is the exterior algebra on R^r generated in homological degree one.  This algebra structure also respects the boundary map
      of the complex in the sense that it satisfies the Leibniz rule.  That is, $d(ab) = d(a)b + (-1)^{deg a}ad(b)$.  When one
      speaks of 'the' Koszul complex of a ring, one means the Koszul complex on a minimal set of generators of the maximal ideal of R.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      KR = koszulComplexDGA R
    Text
      One can specify the name of the variable to easily handle multiple Koszul complexes at once.
    Example
      S = ZZ/101[x,y,z]/ideal{x^3,y^3,z^3,x^2*y^2,y^2*z^2}
      KS = koszulComplexDGA(S,Variable=>"U")
    Text
      To obtain the chain complex associated to the Koszul complex, one may use toComplex.  One can also obtain this complex
      directly without using the DGAlgebras package by using the command @ TO koszul @.
    Example
      cxKR = toComplex KR
      prune HH cxKR
    Text
      Since the Koszul complex is a DG algebra, its homology is itself an algebra.  One can obtain this algebra using the command
      homology, homologyAlgebra, or HH (all commands work).  This algebra structure can detect whether or not the ring is a complete
      intersection or Gorenstein.
    Example
      HKR = HH KR
      ideal HKR
      R' = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a*c,a*d,b*c,b*d,a^2*b^2-c^2*d^2}
      HKR' = HH koszulComplexDGA R'
      numgens HKR'
      ann ideal gens HKR'
    Text
      Note that since the socle of HKR' is one dimensional, HKR' has Poincare duality, and hence R' is Gorenstein.
    Text
      One can also consider the Koszul complex of an ideal, or a sequence of elements.
    Example
      Q = ambient R
      I = ideal {a^3,b^3,c^3,d^3}
      KI = koszulComplexDGA I
      HKI = HH KI
      describe HKI
      use Q
      I' = I + ideal{a^2*b^2*c^2*d^2}
      KI' = koszulComplexDGA I'
      HKI' = HH KI'
      describe HKI'
      HKI'.cache.cycles
    Text
      Note that since I is a Q-regular sequence, the Koszul complex is acyclic, and that both homology algebras are algebras over the zeroth homology
      of the Koszul complex.
///

doc ///
  Key
    "Basic operations on DG Algebra Maps"
  Headline
    Outlines some basic operations on DGAlgebraMaps
  Description
    Text
      An algebra map between the underlying graded algebras that satisfies the Leibniz rule is a morphism of DG algebras.  Such objects
      are created using the DGAlgebraMap class.  As with DGAlgebras, one can define a DGAlgebraMap 'from scratch' using @ TO dgAlgebraMap @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
      K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
      K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
      f = dgAlgebraMap(K2,K1,matrix{{0,T_(1,1),T_(1,2)}})
    Text
      Once we define the DGAlgebraMap, it is a good idea to check to see if it indeed satisfies the Leibniz rule.  This can be checked by using
      isWellDefined.
    Example
      isWellDefined f
    Text
      Oops!  Let's try that again.
    Example
      g = dgAlgebraMap(K1,K2,matrix{{Y_(1,2),Y_(1,3)}})
      isWellDefined g
    Text
      One can lift a ring homomorphism in degree zero to a map of DGAlgebras (up to a specified degree) using liftToDGMap.  This is helpful
      in some of the internal functions of the DGAlgebras package, such as computing the map induced on Tor algebras by a RingMap.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      S = R/ideal{a^2*b^2*c^2}
      f = map(S,R)
      A = acyclicClosure(R,EndDegree=>3)
      B = acyclicClosure(S,EndDegree=>3)
      phi = liftToDGMap(B,A,f)
    Text
      Once one has a DGAlgebraMap, one can also obtain the underlying map of complexes via toComplexMap.
    Example
      cmPhi = toComplexMap(phi,EndDegree=>3)
    Text
      There are also some auxiliary commands associated with DGAlgebraMaps
    Example
      source phi
      target phi
    Text
      One can also obtain the map on homology induced by a DGAlgebra map.
    Example
      HHg = HH g
      matrix HHg
///

doc ///
  Key
    "Basic operations on DG Module Maps"
  Headline
    Outlines some basic operations on DGModuleMaps
  Description
    Text
      A morphism of DG modules over a fixed DG algebra A is an A-linear map of underlying
      graded A.natural-modules that commutes with the differentials.  Such objects are
      created using the DGModuleMap class.  The workhorse constructor is @ TO dgModuleMap @,
      which accepts either a full Matrix encoding of the map or a list of image Vectors
      (one per natural generator of the source).
    Example
      R = QQ[x,y]/ideal(x^2,y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      idM = identityDGModuleMap Mdg
      isWellDefined idM
    Text
      As with DGAlgebraMaps, once you have a candidate map it is a good idea to check
      @ TO isWellDefined @.  The check verifies (per natural generator of the source) both
      the hom-degree condition and the chain-map condition $d_N \circ f = f \circ d_M$.
    Example
      natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
      xMap = dgModuleMap(Mdg, Mdg, apply(natGens, g -> x * g))
      isWellDefined xMap
    Text
      A DGModuleMap induces a chain map between the underlying complexes via
      @ TO toComplexMap @, and a module map on every homology degree via
      @ TO homology @.
    Example
      H0 = homology(xMap, 0)
      H0 == map(target H0, source H0, 0)
    Text
      Classical construction: given a hom-degree-0 seed (the images of the hom-deg-0
      generators of M in N.natural), @ TO liftToDGModuleMap @ inductively solves
      $d_N(x) = f(d_M(e))$ degree by degree, producing a full chain map M -> N.
      Requires N to be acyclic up to the requested EndDegree.
    Example
      f = liftToDGModuleMap(Mdg, Mdg, {(Mdg.natural)_0}, EndDegree => 2)
      isWellDefined f
      h0 = homology(f, 0)
      h0 == id_(source h0)
    Text
      Auxiliary operations: addition, subtraction, scalar multiplication by elements of
      A.ring, negation, and composition via $*$ are all supported, mirroring the
      matrix-level operations on f.natural.
    Example
      (2_R * idM).natural == 2 * idM.natural
      (idM * idM).natural == idM.natural
  SeeAlso
    DGModuleMap
    dgModuleMap
    identityDGModuleMap
    zeroDGModuleMap
    liftToDGModuleMap
    isWellDefined
    toComplexMap
    homology
///

doc ///
  Key
    DGModuleMap
  Headline
    The class of morphisms of DG modules over a common DG algebra
  Description
    Text
      A DGModuleMap encodes an A-linear chain map between two DGModules sharing the
      same underlying DG algebra A.  Create one via @ TO dgModuleMap @.  See
      @ TO "Basic operations on DG Module Maps" @ for a guided tour.
  SeeAlso
    DGModule
    dgModuleMap
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    dgModuleMap
    (dgModuleMap, DGModule, DGModule, Matrix)
    (dgModuleMap, DGModule, DGModule, List)
  Headline
    Construct a DGModuleMap from a matrix or from a list of image Vectors
  Usage
    f = dgModuleMap(N, M, img)
  Inputs
    N:DGModule
      The target DGModule.
    M:DGModule
      The source DGModule; must share the same DG algebra as N.
    img:
      Either a Matrix whose i-th column encodes the image of the i-th natural
      generator of M in N.natural, or a List of Vectors of the same length as
      M.Degrees (one Vector per source generator).
  Outputs
    f:DGModuleMap
  Description
    Text
      The map @ TT "f" @ satisfies the @ TT "A" @-linearity constraint by
      construction: the image of a general element @ TT "\\sum a_i e_i" @ is
      @ TT "\\sum a_i f(e_i)" @, where @ TT "f(e_i)" @ is the @ TT "i" @-th entry
      supplied.  Whether @ TT "f" @ is also a chain map (i.e.\ commutes with
      the differentials) can be verified with @ TO isWellDefined @.
    Text
      A particularly useful application is to build multiplication-by-a-ring-element
      chain maps on a minimal semifree resolution, whose induced action on
      homology recovers the action of the element on @ TT "Tor" @.  Over the
      complete intersection @ TT "R = k[x, y]/(x^2, y^2)" @, the variable
      @ TT "y" @ acts as zero on the residue field, and indeed the induced map
      on homology of the multiplication-by-@ TT "y" @ chain map is zero in
      every degree:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
      natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
      fy = dgModuleMap(Mdg, Mdg, apply(natGens, g -> y * g))
      isWellDefined fy
      cmy = toComplexMap fy
      apply(0..3, n -> prune HH_n cmy)
    Text
      The same constructor also accepts a @ TO Matrix @ whose columns give
      the images of the natural generators.  For instance, supplying the
      identity matrix of @ TT "Mdg.natural" @ reproduces the identity chain
      map:
    Example
      idmat = id_(Mdg.natural)
      g = dgModuleMap(Mdg, Mdg, idmat)
      isWellDefined g
      g == identityDGModuleMap Mdg
  SeeAlso
    identityDGModuleMap
    zeroDGModuleMap
    liftToDGModuleMap
    (isWellDefined, DGModuleMap)
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    identityDGModuleMap
    (identityDGModuleMap, DGModule)
  Headline
    The identity DGModuleMap on a DG module
  Usage
    idM = identityDGModuleMap M
  Inputs
    M:DGModule
  Outputs
    idM:DGModuleMap
  Description
    Text
      Returns the identity endomorphism of M as a DGModuleMap.  Its underlying
      natural matrix is $\mathrm{id}_{M.natural}$ and its induced map on every
      homology degree is the identity.
    Text
      The shorthand @ TT "id_M" @ (mirroring the @ TO "Complexes::Complexes" @
      convention @ TT "id_C" @ for a Complex @ TT "C" @) is an alias, and both
      forms compare equal to the scalar @ TT "1" @:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      id_KM == identityDGModuleMap KM
      id_KM == 1
    Text
      The identity is the neutral element for composition of DG module maps.
      On a semifree resolution of the residue field, the mult-by-y chain map
      is idempotent under pre- and post-composition with the identity:
    Example
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      natGens = apply(rank Mdg.natural, i -> (Mdg.natural)_i)
      fy = dgModuleMap(Mdg, Mdg, apply(natGens, v -> y * v))
      idMdg = identityDGModuleMap Mdg
      idMdg * fy == fy
      fy * idMdg == fy
      idMdg * idMdg == idMdg
  SeeAlso
    dgModuleMap
    zeroDGModuleMap
///

doc ///
  Key
    zeroDGModuleMap
    (zeroDGModuleMap, DGModule, DGModule)
  Headline
    The zero DGModuleMap between two DG modules over a common algebra
  Usage
    zM = zeroDGModuleMap(N, M)
  Inputs
    N:DGModule
    M:DGModule
  Outputs
    zM:DGModuleMap
      The zero map M -> N.  Its natural matrix is zero, and its induced map on
      every homology degree is zero.
  Description
    Text
      The zero endomorphism is the neutral element for addition and the
      absorbing element for composition of DG module maps:
    Example
      R = ZZ/101[x] / ideal(x^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x), EndDegree => 2)
      idM = identityDGModuleMap Mdg
      zM  = zeroDGModuleMap(Mdg, Mdg)
      idM + zM == idM
      zM * idM == zM
      idM * zM == zM
    Text
      Taking the cokernel of the zero endomorphism recovers the original DG
      module, while taking its kernel yields the full module as a DG
      submodule:
    Example
      Q = cokernel zM
      rank Q.natural == rank Mdg.natural
      K = kernel zM
      instance(K, DGSubmodule)
  SeeAlso
    dgModuleMap
    identityDGModuleMap
///

doc ///
  Key
    liftToDGModuleMap
    (liftToDGModuleMap, DGModule, DGModule, Matrix)
    (liftToDGModuleMap, DGModule, DGModule, List)
    (liftToDGModuleMap, DGModule, DGModule, Vector)
    [liftToDGModuleMap, EndDegree]
  Headline
    Lift an image of hom-degree-0 generators to a full DGModuleMap
  Usage
    f = liftToDGModuleMap(N, M, h0)
  Inputs
    N:DGModule
      The target DGModule.  Must be acyclic in positive hom-degrees up to
      EndDegree so that lifts exist.
    M:DGModule
      The source DGModule.  Must be semifree (M.natural a free module).
    h0:
      A Matrix with one column per hom-degree-0 generator of M and
      (rank N.natural)-many rows whose entries lie in A.natural, or a List of
      Vectors (in N.natural), or a single Vector when M has exactly one
      hom-degree-0 generator.
  Outputs
    f:DGModuleMap
  Description
    Text
      Starting from the supplied hom-degree-0 assignment, the routine inductively
      constructs images for each M-generator of hom-degree $d \geq 1$ by solving
      $d_N(x) = f(d_M(e))$ in N.natural at hom-degree $d$.  Existence of a lift
      at every step is what requires N to be acyclic.  The lift is not unique in
      general: different choices of preimage at each step produce chain-homotopic
      lifts.
    Example
      R = QQ[x,y]/ideal(x^2,y^2)
      A = koszulComplexDGA R
      Mmin = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      Mnon = semifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      quism = liftToDGModuleMap(Mnon, Mmin, {(Mnon.natural)_0}, EndDegree => 2)
      isWellDefined quism
  SeeAlso
    dgModuleMap
    identityDGModuleMap
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    (homology, DGModuleMap)
  Headline
    The induced map on graded homology modules over HH(A)
  Usage
    h = homology f
  Inputs
    f:DGModuleMap
      A morphism M -> N of DG modules over a common DG algebra A.  Both
      M.natural and N.natural must be free (the free / semifree case).
  Outputs
    h:
      The induced A.homology-module map H_*(M) -> H_*(N), where H_*(M) and
      H_*(N) carry their natural HH(A)-module structures via the action of
      cycles of A on M and N.  The result lives over the ring HH(A).
  Description
    Text
      The per-degree restrictions of h are given by @ TO (homology, DGModuleMap, ZZ) @.
      The present method assembles them into a single map of HH(A)-modules by
      forming the direct sum of the per-degree pieces and imposing the
      cycle-action relations on both sides.  Functorial in f.
    Example
      R = QQ[x,y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x,y), EndDegree => 2)
      idM = identityDGModuleMap Mdg
      h = homology idM
      ring source h === HH A
  SeeAlso
    (homology, DGModuleMap, ZZ)
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    (symbol ==, DGModuleMap, DGModuleMap)
    (isHomogeneous, DGModuleMap)
    (map, DGModule, DGModule, ZZ)
    (isQuasiIsomorphism, DGModuleMap)
  Headline
    Elementary predicates and constructors for DGModuleMaps
  Description
    Text
      Auxiliary operations mirroring those on ComplexMap.
      For maps f, g between DG modules over a common DG algebra:
    Text
      @ TT "f == g" @ -- equality of source, target, and underlying natural matrix.
    Text
      @ TT "isHomogeneous f" @ -- agrees with @ TT "isHomogeneous f.natural" @.
    Text
      @ TT "map(N, M, 0)" @ -- returns the zero map M -> N.
      @ TT "map(M, M, 1)" @ -- returns the identity on M.
    Text
      @ TT "isQuasiIsomorphism f" @ -- returns true iff the induced chain map on
      @ TO toComplexMap @ is a quasi-isomorphism.
    Example
      R = QQ[x,y]/ideal(x^2, y^2)
      Mdg = minimalSemifreeResolution(koszulComplexDGA R, R^1 / ideal(x,y), EndDegree => 2)
      idM = identityDGModuleMap Mdg
      zM = zeroDGModuleMap(Mdg, Mdg)
      idM == idM
      map(Mdg, Mdg, 0) == zM
      map(Mdg, Mdg, 1) == idM
      isQuasiIsomorphism idM
  SeeAlso
    DGModuleMap
    identityDGModuleMap
    zeroDGModuleMap
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    identityDGAlgebraMap
    (identityDGAlgebraMap, DGAlgebra)
  Headline
    The identity DGAlgebraMap on a DG algebra
  Usage
    idA = identityDGAlgebraMap A
  Inputs
    A:DGAlgebra
  Outputs
    idA:DGAlgebraMap
  Description
    Text
      Returns the identity self-map of A as a DGAlgebraMap.  Under the hood the
      underlying RingMap A.natural -> A.natural is the identity map on every
      generator (including the base-ring generators).
    Example
      R = QQ[x,y]/ideal(x^2,y^2)
      A = koszulComplexDGA R
      idA = identityDGAlgebraMap A
      (idA * idA) == idA
  SeeAlso
    dgAlgebraMap
    (symbol *, DGAlgebraMap, DGAlgebraMap)
///

doc ///
  Key
    (symbol *, DGAlgebraMap, DGAlgebraMap)
  Headline
    Composition of DG algebra maps
  Usage
    h = g * f
  Inputs
    g:DGAlgebraMap
      A map with source = target of f.
    f:DGAlgebraMap
  Outputs
    h:DGAlgebraMap
      The composite DGAlgebraMap whose underlying RingMap is g.natural * f.natural.
  SeeAlso
    identityDGAlgebraMap
    dgAlgebraMap
///

doc ///
  Key
    (symbol **, DGAlgebra, DGAlgebra)
    tensorFactors
    (tensorFactors, DGAlgebra)
    tensorInclusions
    (tensorInclusions, DGAlgebra)
  Headline
    Exterior tensor product of DG algebras over a common ground ring
  Usage
    C = A ** B
    (A1, A2) = tensorFactors C
    (iotaA, iotaB) = tensorInclusions C
  Inputs
    A:DGAlgebra
    B:DGAlgebra
      Must satisfy A.ring === B.ring.
  Outputs
    C:DGAlgebra
      A DGAlgebra over A.ring whose underlying algebra has #A.Degrees + #B.Degrees
      generators, with the obvious multi-degrees, and whose differential is
      d_A on the A-generators and d_B on the B-generators (transported via the
      canonical inclusions).
  Description
    Text
      This is the exterior tensor product of A and B as DGAs: both are viewed
      as augmented DG algebras over their common ground ring, and the tensor
      is taken componentwise in multi-degrees.  Internally the result is cached
      on A.cache, so C1 = A ** B and C2 = A ** B return the SAME DGAlgebra
      object, which is essential for composition identities to hold downstream.
    Text
      The helpers @ TO tensorFactors @ and @ TO tensorInclusions @ recover the
      pair (A, B) and the canonical inclusions A -> C and B -> C (as DGAlgebraMaps).
    Example
      R = QQ[x,y]
      A = koszulComplexDGA R
      C = A ** A
      numgens C.natural
      (iotaA, iotaB) = tensorInclusions C
  SeeAlso
    (symbol **, DGAlgebraMap, DGAlgebraMap)
    (symbol **, DGModule, DGModule)
///

doc ///
  Key
    (symbol **, DGAlgebraMap, DGAlgebraMap)
  Headline
    Tensor product of DG algebra maps
  Usage
    h = f ** g
  Inputs
    f:DGAlgebraMap
      A:A -> A'
    g:DGAlgebraMap
      B:B -> B'
  Outputs
    h:DGAlgebraMap
      The map A ** B -> A' ** B' that restricts to f on the A-generators and
      g on the B-generators, transported via the canonical inclusions.
  Description
    Text
      Functorial with the DGA tensor product: identityDGAlgebraMap(A) ** identityDGAlgebraMap(B)
      equals identityDGAlgebraMap(A ** B).
  SeeAlso
    (symbol **, DGAlgebra, DGAlgebra)
    identityDGAlgebraMap
///

doc ///
  Key
    (symbol **, DGModule, DGModule)
  Headline
    Exterior tensor product of DG modules over DG algebras sharing a ground ring
  Usage
    P = M ** N
  Inputs
    M:DGModule
      A (free) DG module over some DG algebra A.
    N:DGModule
      A (free) DG module over some DG algebra B, with A.ring === B.ring.
  Outputs
    P:DGModule
      A (free) DG module over A ** B whose natural generators are indexed
      lexicographically by (i, j) with i ranging over M-generators and j over
      N-generators.  The multi-degree of (i, j) is M.Degrees#i + N.Degrees#j
      coordinate-wise, and the differential is
      $d(e_i \otimes f_j) = d_M(e_i) \otimes f_j + (-1)^{|e_i|}\, e_i \otimes d_N(f_j)$.
  Description
    Text
      This is the EXTERIOR tensor product: we do not quotient by an A- or B-action.
      The resulting DGModule lives over A ** B via the canonical inclusions.
      The result is cached on M.cache, so M ** N returns the same object on repeat.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      ng = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * ng#0})
      P = M ** M
      P.dgAlgebra === (A ** A)
  SeeAlso
    (symbol **, DGAlgebra, DGAlgebra)
    (symbol **, DGModuleMap, DGModuleMap)
    freeDGModule
///

doc ///
  Key
    (symbol **, DGModuleMap, DGModuleMap)
  Headline
    Tensor product of DG module maps
  Usage
    h = F ** G
  Inputs
    F:DGModuleMap
      M -> M'
    G:DGModuleMap
      N -> N'
  Outputs
    h:DGModuleMap
      The map M ** N -> M' ** N' acting by F on the first factor and G on the second factor.
  Description
    Text
      Functorial: idM ** idN equals identityDGModuleMap(M ** N), and tensor with
      a zero map is a zero map.
  SeeAlso
    (symbol **, DGModule, DGModule)
    identityDGModuleMap
    zeroDGModuleMap
///

doc ///
  Key
    DGAlgebra
  Headline
    The class of all DGAlgebras
  Description
    Text
      Some common ways to create DGAlgebras include @ TO koszulComplexDGA @, @ TO freeDGAlgebra @, @ TO setDiff @, and @ TO acyclicClosure @.
  SeeAlso
    "Basic operations on DG Algebras"
///

doc ///
  Key
    freeDGAlgebra
    (freeDGAlgebra,Ring,List)
    [freeDGAlgebra,Variable]
  Headline
    Construct a free DG algebra with given generator degrees
  Usage
    A = freeDGAlgebra(R, degreeList)
    A = freeDGAlgebra(R, degreeList, Variable => "U")
  Inputs
    R:Ring
      The base ring over which the DG algebra is defined.
    degreeList:List
      One degree per algebra generator.  Each degree is itself a
      @ TO List @ of integers; the @ EM "first" @ entry is interpreted
      as the homological degree, and any remaining entries are internal
      gradings carried through to the underlying algebra.
    Variable => String
      Either a base name (@ TO String @ or @ TO Symbol @) or an explicit
      list of symbols.  Defaults to @ TT "\"T\"" @.  See below.
  Outputs
    A:DGAlgebra
      A DG algebra over @ TT "R" @ whose @ TT "A.natural" @ is a
      graded-commutative polynomial ring on @ TT "#degreeList" @
      generators.  The differential is not set by this constructor; call
      @ TO setDiff @ on the result to install it.
  Description
    Text
      The output is a @ TO DGAlgebra @ whose underlying ring is a
      polynomial ring in graded-commutative generators: generators of
      @ EM "odd" @ homological degree are skew-commutative and square
      to zero (i.e.\ exterior), while generators of @ EM "even" @
      homological degree are fully commutative.  The resulting algebra
      is always free as a graded @ TT "R" @-algebra; the current version
      of the package does @ EM "not" @ handle quotient DG algebras whose
      underlying ring is a non-polynomial quotient.
    Text
      @ BOLD "Variable-naming convention." @  Generators are named
      @ TT "base_(i, j)" @, where @ TT "i" @ is the homological degree
      (the first entry of the degree vector) and @ TT "j" @ is a
      @ EM "1-indexed" @ counter among generators at that hom-degree.
      So @ TT "freeDGAlgebra(R, {{1}, {1}, {1}, {3}})" @ produces four
      generators @ TT "T_(1,1), T_(1,2), T_(1,3), T_(3,1)" @.  The
      @ TT "Variable" @ option changes the base name:
    Example
      R = ZZ/101[x, y, z]
      A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}}, Variable => "U")
      gens A.natural
    Text
      Passing a @ TO List @ of symbols for @ TT "Variable" @ overrides
      the doubly-indexed naming entirely and uses the given names
      verbatim.  The list length must match @ TT "#degreeList" @:
    Example
      B = freeDGAlgebra(R, {{1}, {1}, {1}}, Variable => {getSymbol "P", getSymbol "Q", getSymbol "RR"})
      gens B.natural
    Text
      This is the mode used internally by @ TO adjoinVariables @ and
      @ TO acyclicClosure @ to preserve the identities of existing
      generators when extending a DG algebra.
    Text
      @ BOLD "Multi-grading." @  Any degree entries beyond the first are
      carried through as additional gradings of the underlying ring
      @ TT "A.natural" @.  To make the differential homogeneous with
      respect to both the homological and internal gradings, pair each
      hom-degree with the matching internal-degree of its image under
      @ TO setDiff @.  Here, @ TT "T_(1,i)" @ has degree @ TT "(1,1)" @
      (hom-degree 1, internal-degree 1) matching @ TT "x, y, z" @, and
      @ TT "T_(3,1)" @ has degree @ TT "(3,3)" @ matching its
      cubic-monomial differential:
    Example
      Bmulti = freeDGAlgebra(R, {{1, 1}, {1, 1}, {1, 1}, {3, 3}})
      degrees Bmulti.natural
      setDiff(Bmulti, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
      isHomogeneous Bmulti
    Text
      Dropping the internal grading (hom-degree only) breaks homogeneity
      because the differential mixes @ EM "different" @ internal
      polynomial degrees of @ TT "R" @:
    Example
      Bhomo = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
      setDiff(Bhomo, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
      isHomogeneous Bhomo
    Text
      The differential is set after construction (via @ TO setDiff @)
      rather than passed to @ TT "freeDGAlgebra" @ directly: the
      differential's ring is @ TT "A.natural" @, which does not exist
      until after the constructor runs.  For common DG algebras arising
      in commutative algebra (Koszul complex, Tate resolution,
      acyclic closure, etc.) dedicated constructors are available; see
      @ TO koszulComplexDGA @, @ TO acyclicClosure @, @ TO minimalModel @.
  Caveat
    There is currently a bug handling DG algebras that have no
    generators in some homological degree but some in a later
    homological degree; for example @ TT "freeDGAlgebra(R, {{1}, {5}})" @
    may produce incorrect behavior if no hom-degree 3 generators are
    adjoined first.  The safer workflow is @ TT "freeDGAlgebra" @ with
    hom-degrees only up to what is needed, followed by
    @ TO adjoinVariables @ to extend incrementally.
  SeeAlso
    koszulComplexDGA
    acyclicClosure
    minimalModel
    adjoinVariables
    setDiff
    DGAlgebra
///

doc ///
  Key
    koszulComplexDGA
    (koszulComplexDGA,Ring)
    [koszulComplexDGA,Variable]
  Headline
    The Koszul complex on the variables of a ring, as a DG algebra
  Usage
    A = koszulComplexDGA R
    A = koszulComplexDGA(R, Variable => "S")
  Inputs
    R:Ring
      A polynomial ring or quotient of one; the Koszul complex is built on
      @ TT "gens R" @.
    Variable => String
      Base name for the Koszul generators.  Defaults to @ TT "\"T\"" @.
      Both @ TT "String" @ and @ TT "Symbol" @ values are accepted.
  Outputs
    A:DGAlgebra
      The Koszul complex on @ TT "gens R" @ equipped with the structure of
      a DG algebra over @ TT "R" @.
  Description
    Text
      Given a ring @ TT "R" @ with @ TT "n = numgens R" @ generators
      @ TT "x_1, ..., x_n" @, the returned DG algebra has underlying
      graded-commutative polynomial ring
      @ TT "R[T_(1,1), ..., T_(1,n)]" @ on @ TT "n" @ exterior (odd,
      square-zero) generators in hom-degree 1, with differential
      @ TT "d(T_(1,i)) = x_i" @ extended by the Leibniz rule.  In the
      @ TT "R" @-module direction, the complex is identical to the
      classical Koszul complex @ TT "K_\\bullet(x_1, ..., x_n; R)" @.
    Text
      @ BOLD "Variable naming convention." @  Generators are named
      @ TT "T_(i, j)" @, where @ TT "i" @ is the homological degree (here
      always @ TT "1" @) and @ TT "j" @ is a @ EM "1-indexed" @ counter
      among generators at that hom-degree.  Pass @ TT "Variable => \"S\"" @
      to use base name @ TT "S" @ instead.
    Example
      R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
      A = koszulComplexDGA R
      gens A.natural
      flatten entries matrix A.diff
    Text
      Converting to a @ TO Complex @ gives the ordinary Koszul complex on
      the variables (up to a choice of monomial order on the exterior
      product); taking homology recovers the classical Koszul homology:
    Example
      complexA = toComplex A
      complexA.dd_1
      ranks = apply(4, i -> numgens prune HH_i complexA)
      ranks == apply(4, i -> numgens prune HH_i koszul vars R)
    Text
      The homology can be computed directly from @ TT "A" @ via
      @ TO (homology,ZZ,DGAlgebra) @, or as an algebra via
      @ TO homologyAlgebra @.
    Text
      To rename the Koszul generators, pass the @ TT "Variable" @ option:
    Example
      S = ZZ/101[a, b]
      AS = koszulComplexDGA(S, Variable => "U")
      gens AS.natural
  Caveat
    The Koszul complex is built on @ EM "all" @ elements of @ TT "gens R" @,
    not on a minimal generating set of a possibly non-irrelevant ideal.
    To build a Koszul complex on a specific sequence of ring elements, use
    @ TO (koszulComplexDGA, List) @; for a full generating set of an ideal,
    use @ TO (koszulComplexDGA, Ideal) @.  Also, @ TO toComplex @ uses a
    different monomial order than the @ TO "Complexes::koszul" @ command,
    so individual differentials may differ by a basis permutation even
    though the complexes are isomorphic.
  SeeAlso
    (koszulComplexDGA, Ideal)
    (koszulComplexDGA, List)
    freeDGAlgebra
    acyclicClosure
    koszulComplexDGM
    (homology, ZZ, DGAlgebra)
    homologyAlgebra
///

doc ///
  Key
    (koszulComplexDGA,Ideal)
  Headline
    The Koszul complex on the generators of an ideal, as a DG algebra
  Usage
    A = koszulComplexDGA I
  Inputs
    I:Ideal
      An ideal of a ring @ TT "R" @.
  Outputs
    A:DGAlgebra
      The Koszul complex over @ TT "R" @ on the chosen generators of
      @ TT "I" @.
  Description
    Text
      Given generators @ TT "f_1, ..., f_r" @ of @ TT "I" @, the output is
      a DG algebra over @ TT "R" @ whose underlying graded algebra is
      @ TT "R[T_(1,1), ..., T_(1,r)]" @ on @ TT "r" @ exterior generators
      in hom-degree 1, with differential @ TT "d(T_(1,i)) = f_i" @
      extended by the Leibniz rule.  The variable naming convention and
      @ TT "Variable" @ option mirror @ TO (koszulComplexDGA, Ring) @.
    Example
      R = ZZ/101[a, b, c]
      I = ideal(a^3, b^3, c^3, a^2 * b^2 * c^2)
      A = koszulComplexDGA I
      gens A.natural
      flatten entries matrix A.diff
    Text
      The resulting complex coincides with @ TO "Complexes::koszul" @
      applied to @ TT "gens I" @, up to monomial order:
    Example
      complexA = toComplex A
      ranks = apply(5, i -> numgens prune HH_i complexA)
      ranks == apply(5, i -> numgens prune HH_i koszul gens I)
    Text
      In particular, @ TT "I" @ contains the redundant generator
      @ TT "a^2 b^2 c^2" @ (it is in @ TT "(a^3, b^3, c^3)" @), so the
      Koszul complex is not exact at @ TT "H_1" @; nonzero higher
      Koszul homology encodes the relations and syzygies between the
      chosen generators.
  SeeAlso
    (koszulComplexDGA, Ring)
    (koszulComplexDGA, List)
    (homology, ZZ, DGAlgebra)
///

doc ///
  Key
    (koszulComplexDGA,List)
  Headline
    The Koszul complex on a list of ring elements, as a DG algebra
  Usage
    A = koszulComplexDGA diffList
  Inputs
    diffList:List
      A nonempty list of @ TO RingElement @ s, all lying in a common ring
      @ TT "R" @.  These become the images of the Koszul generators under
      the differential.
  Outputs
    A:DGAlgebra
      The Koszul complex over @ TT "R" @ on the sequence @ TT "diffList" @.
  Description
    Text
      This is the lowest-level form: the generators of @ TT "I" @ or
      @ TT "R" @ are replaced by an arbitrary user-supplied list of ring
      elements.  Useful for building a Koszul complex on a partial
      regular sequence or on arbitrary elements that may not generate an
      ideal of interest.  The variable naming convention follows
      @ TO (koszulComplexDGA, Ring) @: generators are named
      @ TT "T_(1, j)" @ with @ TT "j = 1, ..., #diffList" @.
    Example
      R = ZZ/101[x, y, z]
      A = koszulComplexDGA({x^2, y*z})
      gens A.natural
      flatten entries matrix A.diff
      apply(3, i -> numgens prune HH_i toComplex A)
    Text
      Here the sequence @ TT "(x^2, y z)" @ is regular in
      @ TT "ZZ/101[x, y, z]" @, so the Koszul complex is acyclic in
      positive homology and @ TT "H_0" @ is the quotient
      @ TT "R/(x^2, y z)" @ (of dimension 1 as an @ TT "R" @-module).
  SeeAlso
    (koszulComplexDGA, Ring)
    (koszulComplexDGA, Ideal)
///

doc ///
  Key
    (homology,ZZ,DGAlgebra)
  Headline
    Computes the homology of a DG algebra as a module
  Usage
    H = homology(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra 
  Outputs
    H:Module
      The nth homology of A.
  Description
    Example
      R = ZZ/32003[x,y,z]
      A = koszulComplexDGA(R)
      apply(numgens R+1, i -> numgens prune homology(i,A))
///

doc ///
  Key
    setDiff
    (setDiff,DGAlgebra,List)
    InitializeComplex
    [setDiff,InitializeComplex]
    InitializeDegreeZeroHomology
    [setDiff,InitializeDegreeZeroHomology]
  Headline
    Install or replace the differential on a DG algebra
  Usage
    A = setDiff(A, diffList)
    A = setDiff(A, diffList, InitializeComplex => false)
  Inputs
    A:DGAlgebra
    diffList:List
      A list of elements of @ TT "A.natural" @, one per algebra generator,
      given in the @ EM "same order" @ as @ TT "gens A.natural" @ (which is
      the order of @ TT "A.Degrees" @).  The @ TT "i" @-th entry becomes
      @ TT "d((gens A.natural)_i)" @ and is extended to all of
      @ TT "A.natural" @ by @ TT "R" @-linearity and the Leibniz rule.
      Each entry must have homological degree one less than its generator.
    InitializeComplex => Boolean
      Whether to eagerly compute all differential matrices up to
      @ TT "maxDegree A" @.  Defaults to @ TT "true" @; set to
      @ TT "false" @ to skip the precomputation when only a low-degree
      fragment is needed.
    InitializeDegreeZeroHomology => Boolean
      Whether to compute the quotient ring @ TT "H_0(A)" @ (needed by
      @ TO homologyAlgebra @).  Defaults to @ TT "true" @; set to
      @ TT "false" @ to skip the Grobner-basis computation when not
      needed.
  Outputs
    A:DGAlgebra
      The same @ TT "A" @, now with its @ TT "diff" @ field populated;
      returned for convenience in a chained call.
  Description
    Text
      Because the ring @ TT "A.natural" @ does not exist until after
      @ TO freeDGAlgebra @ runs, the differential cannot be passed to
      the constructor and must be set afterwards with @ TT "setDiff" @.
      The list @ TT "diffList" @ supplies the image of each algebra
      generator; Leibniz and @ TT "R" @-linearity extend this to every
      element of @ TT "A.natural" @.
    Example
      R = ZZ/101[x, y, z]
      A = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
      gens A.natural
      setDiff(A, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)})
      flatten entries matrix A.diff
    Text
      Here @ TT "d(T_(1,i)) = x_i" @ (so the Koszul piece on
      @ TT "T_(1,*)" @ is installed) and @ TT "d(T_(3,1))" @ is the
      Koszul-relation 2-cycle @ TT "x T_(1,2) T_(1,3) - y T_(1,1) T_(1,3)
      + z T_(1,1) T_(1,2)" @, turning @ TT "A" @ into a Tate-resolution
      fragment for @ TT "R/(x, y, z)" @.  Converting to a @ TO Complex @
      shows the installed differentials:
    Example
      Acomplex = toComplex A
      Acomplex.dd_1
      Acomplex.dd_2
    Text
      @ BOLD "Validity." @  The package does @ EM "not" @ check that the
      installed differential squares to zero; a user-supplied
      @ TT "diffList" @ that violates @ TT "d^2 = 0" @ yields a malformed
      DG algebra with silently wrong downstream behavior.  The chain-map
      conditions can be checked after the fact via
      @ TO (isWellDefined, DGAlgebra) @.
    Text
      @ BOLD "Options." @  @ TT "InitializeComplex" @ controls whether
      @ TT "setDiff" @ eagerly builds all differential matrices up to
      @ TT "maxDegree A" @ (the sum of the hom-degrees of the
      odd-hom-degree generators).  Set it to @ TT "false" @ to defer this
      computation when only low-degree information is needed:
    Example
      Abig = freeDGAlgebra(R, {{1}, {1}, {1}, {3}})
      setDiff(Abig, {x, y, z, x*T_(1,2)*T_(1,3) - y*T_(1,1)*T_(1,3) + z*T_(1,1)*T_(1,2)}, InitializeComplex => false)
    Text
      @ TT "InitializeDegreeZeroHomology" @ controls whether the quotient
      ring @ TT "H_0(A) = A_0 / image(d_1)" @ is computed on the spot
      (a Grobner-basis computation, deferred to @ TO homologyAlgebra @
      if turned off).  Default: @ TT "true" @.  Turn it off if you have
      many hom-degree-1 generators and are not about to compute
      @ TT "HH A" @ as a DG algebra.
  Caveat
    The order of entries in @ TT "diffList" @ must match the order of
    generators in @ TT "gens A.natural" @ (equivalently, of
    @ TT "A.Degrees" @).  For constructors that adjoin generators
    incrementally (@ TO adjoinVariables @, @ TO acyclicClosure @,
    @ TO killCycles @), the new generators come after all existing
    ones in this ordering.
  SeeAlso
    freeDGAlgebra
    koszulComplexDGA
    adjoinVariables
    (isWellDefined, DGAlgebra)
    homologyAlgebra
///

doc ///
  Key
    (isHomogeneous, DGAlgebra)
  Headline
    Determine if the DGAlgebra respects the gradings of the ring it is defined over.
  Usage
    isHom = isHomogeneous(A)
  Inputs
    A:DGAlgebra
  Outputs
    isHom:Boolean
      Whether or not the DGA respects the grading
  Description
    Example
      R = ZZ/101[x,y,z]
      A = freeDGAlgebra(R,{{1},{1},{1},{3}})
      setDiff(A,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
      isHomogeneous A
      B = freeDGAlgebra(R,{{1,1},{1,1},{1,1},{3,3}})
      setDiff(B,{x,y,z,x*T_(1,2)*T_(1,3)-y*T_(1,1)*T_(1,3)+z*T_(1,1)*T_(1,2)})
      isHomogeneous B
///

doc ///
  Key
    natural
  Headline
    The underlying graded-commutative ring of a DGAlgebra or DGModule
  Usage
    Anat = A.natural
  Description
    Text
      For a @ TO DGAlgebra @ @ TT "A" @, the key @ TT "A.natural" @ holds
      the underlying graded-commutative polynomial ring of @ TT "A" @
      with the differential @ EM "forgotten" @.  This is the ambient ring
      in which algebra generators live, in which cycles and boundaries
      are expressed, and against which differentials are compared.  It is
      always a polynomial ring over @ TT "A.ring" @ (with the
      skew-commutative flag set on odd-hom-degree generators), never a
      quotient.
    Example
      R = ZZ/101[a, b, c, d]
      A = koszulComplexDGA R
      A.natural
      A.ring === R
      gens A.natural
    Text
      For a @ TO DGModule @ @ TT "M" @, the same key holds the underlying
      free @ TT "A.natural" @-module with the differential forgotten.  The
      differential is recorded separately in @ TT "M.diff" @ and the
      multi-degrees of the natural generators in @ TT "M.Degrees" @:
    Example
      B = freeDGModule(A, {0, 1, 2})
      B.natural
      rank B.natural
  SeeAlso
    DGAlgebra
    DGModule
    ring
///

doc ///
  Key
    cycles
  Headline
    Chosen cycle representatives for homology generators of a DG algebra
  Usage
    A.cycles
  Description
    Text
      When @ TO acyclicClosure @, @ TO killCycles @, or
      @ TO homologyAlgebra @ adjoin new generators to kill homology of a
      DG algebra @ TT "A" @, the chosen cycle representatives are cached
      on the output so the exact Tate construction can be inspected
      afterwards.  The key is @ TT "A.cycles" @ on a DG algebra, or
      @ TT "HA.cache.cycles" @ on the output of @ TO homologyAlgebra @.
    Example
      R = ZZ/101[a, b, c, d] / ideal(a^3, b^4, c^5, d^6)
      A = koszulComplexDGA R
      apply(maxDegree A + 1, i -> numgens prune homology(i, A))
      HA = homologyAlgebra A
      numgens HA
      HA.cache.cycles
    Text
      The entries of @ TT "HA.cache.cycles" @ are elements of
      @ TT "A.natural" @: cycles chosen to represent the generators of
      @ TT "HA" @ as an algebra.  These are the same representatives
      that would be used as the differentials of newly adjoined
      generators in @ TO acyclicClosure @.
  SeeAlso
    homologyAlgebra
    acyclicClosure
    killCycles
///

doc ///
  Key
    getBasis
    (getBasis,ZZ,DGAlgebra)
  Headline
    Get a basis for a particular homological degree of a DG algebra.
  Usage
    M = getBasis(n,A)
  Inputs
    n:ZZ
    A:DGAlgebra
  Outputs
    M:Matrix
      The basis of the desired homological degree of the DG Algebra.
  Description
    Text
      This function is to allow for the retrieval of a basis of a particular homological degree of a @ TO DGAlgebra @
      when the underlying algebra A.natural is multigraded.  In the code, the homological grading is always the first
      integer in the degree tuple, and so this function returns a matrix consisting of all monomials in homological
      degree n.  
    Example
      R = ZZ/101[a..d, Degrees=>{1,1,1,2}]
      A =  koszulComplexDGA(R)
      getBasis(3,A)
///

doc ///
  Key
    (getBasis,ZZ,Ring)
  Headline
    Get a basis for a degree of a ring.
  Usage
    M = getBasis(n,R)
  Inputs
    n:ZZ
    R:Ring
  Outputs
    M:Matrix
      The basis of the desired degree
  Description
    Text
      This function was not meant for general use, but it fixes the first degree in the degree tuple
      of the ring R, and finds a basis of that 'slice' of the ring.  It does this by using a cached
      version of the ring that forgets all other degrees.  A Ring object in Macaulay2 will not have this
      cached ring by default, but the rings used internally in the DGAlgebras package will.
///

doc ///
  Key
    toComplex
    (toComplex,DGAlgebra)
  Headline
    Converts a DGAlgebra to a Complex
  Usage
    C = toComplex A or C = toComplex(A,n)
  Inputs
    A:DGAlgebra
  Outputs
    C:Complex
      The DG algebra A as a Complex
  Description
    Example
      R = ZZ/101[x_1..x_10]
      A = koszulComplexDGA(R)
      C = toComplex A
    Text
      Warning:  The term order that the internal command koszul uses to order the monomials is not GRevLex, and so the differentials
      used in koszul and koszulComplexDGA will not match up exactly.  Also, this command will only execute if all of the variables
      of the @ TO DGAlgebra @ A are of odd homological degree.  Otherwise, you need to use the function @ TO (toComplex, DGAlgebra, ZZ) @.
///

doc ///
  Key
    (toComplex,DGAlgebra,ZZ)
  Headline
    Converts a DGAlgebra to a Complex
  Usage
    C = toComplex A or C = toComplex(A,n)
  Inputs
    A:DGAlgebra
    n:ZZ
  Outputs
    C:Complex
      The DG algebra A as a Complex
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = acyclicClosure(R,EndDegree=>3)
    Text
      The above will be a resolution of the residue field over R, since R is a complete intersection.
    Example
      C = toComplex(A, 10)
      apply(10, i -> prune HH_i(C))
///

doc ///
  Key
    acyclicClosure
    (acyclicClosure,DGAlgebra)
  Headline
    Tate's construction of a semifree acyclic DG algebra extension
  Usage
    B = acyclicClosure A
    B = acyclicClosure(A, EndDegree => n)
  Inputs
    A:DGAlgebra
      The starting DG algebra.
    StartDegree => ZZ
      First hom-degree at which to begin killing homology.  Defaults to
      @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which homology is killed.  Defaults to
      @ TT "3" @.
    Variable => String
      Base name used for any new adjoined generators (Strings and Symbols
      are both accepted).  Defaults to reusing the base symbol already in
      use by @ TT "A" @, or @ TT "\"T\"" @ if A has no generators.
  Outputs
    B:DGAlgebra
      A semifree extension of @ TT "A" @ with the same ring and the same
      degree-zero homology, whose positive-degree homology has been killed
      in every hom-degree from @ TT "StartDegree" @ through @ TT "EndDegree" @.
  Description
    Text
      @ BOLD "Tate's construction." @  Starting from @ TT "A" @,
      @ TT "acyclicClosure" @ iterates @ TO killCycles @ from
      @ TT "StartDegree" @ up through @ TT "EndDegree" @.  At each step
      @ TT "n" @, a minimal set of cycle representatives of @ TT "H_n(A)" @
      is computed and a new DG algebra generator is adjoined for each,
      placed in hom-degree @ TT "n + 1" @ and whose differential is the
      chosen cycle.  Adjoining follows the graded-commutative convention:
      generators of odd hom-degree are exterior (skew-commutative,
      square-zero) while generators of even hom-degree are polynomial, so
      the result is a free DG algebra in the sense of Tate/Gulliksen.  If
      @ TT "A" @ is the Koszul complex on a regular sequence, no
      homology exists to kill and @ TT "acyclicClosure" @ returns @ TT "A" @
      unchanged; otherwise new generators appear, recording the deviations
      of @ TT "A" @.
    Text
      @ BOLD "Variable-naming convention." @  Both the existing generators of
      @ TT "A" @ and the newly adjoined generators use a doubly-indexed
      naming scheme @ TT "base_(i, j)" @, where @ TT "i" @ is the
      homological degree (the first entry of the multi-degree) and
      @ TT "j" @ is a @ EM "1-indexed" @ counter among generators at that
      hom-degree.  Counters continue past those already present in
      @ TT "A" @.  For example, if @ TT "A" @ already has three generators
      @ TT "T_(1,1), T_(1,2), T_(1,3)" @ in hom-degree 1 and two new
      hom-degree-2 cycles need killing, the output has new generators
      @ TT "T_(2,1)" @ and @ TT "T_(2,2)" @.  The @ TT "Variable" @ option
      only affects the base symbol for @ EM "new" @ generators; existing
      names are preserved.
    Text
      Over a complete intersection @ TT "R = k[a, b, c]/(a^3, b^3, c^3)" @,
      the Koszul complex has three nonzero hom-degree-2 cycles (one per
      relation) which must be killed, introducing three new polynomial
      generators @ TT "T_(2,1), T_(2,2), T_(2,3)" @:
    Example
      R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
      A = koszulComplexDGA R
      gens A.natural
      B = acyclicClosure(A, EndDegree => 2)
      gens B.natural
      take(flatten entries matrix B.diff, 6)
    Text
      Here the new generators satisfy
      @ TT "d(T_(2,i)) = a_i^2 T_(1,i)" @, the cycle representatives of
      @ TT "H_2" @ for each element of the regular sequence.  The result
      is quasi-isomorphic to @ TT "R/(a,b,c)" @ in hom-degrees
      @ TT "<= 2" @.
    Text
      For larger @ TT "EndDegree" @, the construction keeps going; over a
      c.i. no further generators appear because the Koszul-plus-polynomial
      DG algebra is already a resolution of the residue field (this is
      Tate's theorem):
    Example
      B2 = acyclicClosure(A, EndDegree => 4)
      numgens B2.natural == numgens B.natural
    Text
      Over a non-c.i., higher deviations are nontrivial and
      @ TT "acyclicClosure" @ adjoins further generators at each stage.
      Tallying by hom-degree gives the deviation sequence of the ring
      (excluding hom-degree 0, where the ring itself sits):
    Example
      S = QQ[x, y] / ideal(x^2, x*y, y^2)
      C = acyclicClosure(koszulComplexDGA S, EndDegree => 3)
      tally apply(C.Degrees, d -> first d)
    Text
      The hom-degree-1 count of @ TT "2" @ comes from the two Koszul
      exterior generators (one per variable of @ TT "S" @); hom-degree
      @ TT "2" @ has three polynomial generators killing the three
      relations; and hom-degrees @ TT "3" @ and beyond have nontrivial
      deviations, signaling that @ TT "S" @ is @ EM "not" @ a complete
      intersection.  Over a complete intersection, all hom-degrees
      @ TT ">= 3" @ would be empty.
    Text
      To choose a different base symbol for the newly adjoined generators,
      use the @ TT "Variable" @ option.  Existing generators of @ TT "A" @
      keep their names:
    Example
      D = acyclicClosure(A, EndDegree => 2, Variable => "U")
      gens D.natural
  Caveat
    For a DG algebra @ TT "A" @ whose degree-zero part already has
    inhomogeneous structure (e.g. one built via @ TO freeDGAlgebra @
    with a hand-written differential that is not square-zero), the
    package assumes d^2 = 0 without checking; the resulting
    @ TT "acyclicClosure" @ is only meaningful when this holds.
  SeeAlso
    (acyclicClosure,Ring)
    killCycles
    adjoinVariables
    minimalModel
    koszulComplexDGA
    StartDegree
    EndDegree
///

doc ///
  Key
    (acyclicClosure,Ring)
  Headline
    Tate's acyclic closure of the residue field, starting from the Koszul complex
  Usage
    A = acyclicClosure R
    A = acyclicClosure(R, EndDegree => n)
  Inputs
    R:Ring
      A local or graded ring (in practice, a quotient of a polynomial ring
      by a homogeneous ideal).
  Outputs
    A:DGAlgebra
      A semifree DG algebra over @ TT "R" @ whose degree-zero homology is
      @ TT "R/m = R_0" @ (the residue field, or more generally
      @ TT "R/" @ @ TO (ideal, Ring) @@ TT " R" @) and whose positive
      homology is killed through hom-degree @ TT "EndDegree" @.
  Description
    Text
      This form is shorthand for
      @ TT "acyclicClosure(koszulComplexDGA R, ...)" @: the Koszul complex
      on a chosen generating set of the maximal/irrelevant ideal is used
      as the starting DG algebra, and @ TO (acyclicClosure, DGAlgebra) @
      iteratively adjoins new generators to kill homology up to
      @ TT "EndDegree" @.  The output is a truncation of Avramov's
      acyclic closure of @ TT "R" @ over itself -- equivalently, a free
      DG algebra resolution of the residue field in hom-degrees
      @ TT "0" @ through @ TT "EndDegree" @.
    Text
      @ BOLD "Variable convention." @  The hom-degree-1 exterior generators
      come from @ TO koszulComplexDGA @ and are named @ TT "T_(1,j)" @,
      one for each generator of the maximal/irrelevant ideal.  Higher
      generators (adjoined by @ TO killCycles @ at each stage) are named
      @ TT "T_(n,j)" @, with @ TT "n" @ the homological degree and
      @ TT "j" @ a 1-indexed counter.  Generators of odd hom-degree are
      exterior; generators of even hom-degree are polynomial.  Pass
      @ TT "Variable => \"S\"" @ (or any string/symbol) to rename the base.
    Example
      R = ZZ/101[a, b, c, d] / ideal(a^3, b^3, c^4 - d^3)
      A = acyclicClosure(R, EndDegree => 3)
      tally apply(A.Degrees, d -> first d)
      take(flatten entries matrix A.diff, numgens A.natural)
    Text
      Four hom-degree-1 exterior generators @ TT "T_(1,1), ..., T_(1,4)" @
      (one per variable of @ TT "R" @) and three hom-degree-2 polynomial
      generators (one for each defining relation, since @ TT "R" @ is a
      complete intersection of codimension 3).  In the resulting DG
      algebra the residue field has been resolved through hom-degree 3.
    Text
      For a complete intersection, the Koszul complex already has zero
      higher homology beyond what the Tate polynomial closure kills at
      hom-degree 2, so the number of generators stabilizes quickly:
    Example
      S = QQ[x, y, z] / ideal(x^2, y^2, z^2)
      C = acyclicClosure(S, EndDegree => 4)
      tally apply(C.Degrees, d -> first d)
    Text
      For a non-c.i., further deviations appear:
    Example
      T = QQ[x, y] / ideal(x^2, x*y, y^2)
      D = acyclicClosure(T, EndDegree => 3)
      tally apply(D.Degrees, d -> first d)
  SeeAlso
    (acyclicClosure, DGAlgebra)
    koszulComplexDGA
    minimalModel
    killCycles
    StartDegree
    EndDegree
///

doc ///
  Key
    minimalModel
    (minimalModel, Ideal)
    (minimalModel, QuotientRing)
    [minimalModel, StartDegree]
    [minimalModel, EndDegree]
    [minimalModel, Variable]
  Headline
    Build the minimal DG algebra resolution of a quotient ring
  Usage
    A = minimalModel I
    A = minimalModel R
    A = minimalModel(I, EndDegree => n)
  Inputs
    I:Ideal
      An ideal in a polynomial ring @ TT "Q" @.
    R:QuotientRing
      A quotient @ TT "Q/I" @, where @ TT "Q" @ is a polynomial ring.
      The defining ideal @ TT "ideal R" @ (in @ TT "ambient R = Q" @)
      is used.
    StartDegree => ZZ
      First hom-degree at which to start killing homology.  Defaults
      to @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which to kill homology.  Defaults to
      @ TT "3" @.
    Variable => String
      Name of the divided-power generators; defaults to
      @ TT "\"T\"" @.
  Outputs
    A:DGAlgebra
      A DG algebra over @ TT "Q" @ whose degree-zero homology is
      @ TT "Q/I" @ and whose positive-degree homology is killed
      through hom-degree @ TT "EndDegree" @.  In other words, a
      truncation of the minimal DG algebra resolution of
      @ TT "Q/I" @ as a @ TT "Q" @-algebra.
  Description
    Text
      Given an ideal @ TT "I" @ in a polynomial ring @ TT "Q" @,
      @ TT "minimalModel I" @ is a shorthand for
    Text
      @ TT "acyclicClosure(koszulComplexDGA I)" @ .
    Text
      The Koszul DG algebra on a generating set of @ TT "I" @ has
      @ TT "H_0 = Q/I" @, and acyclicClosure adjoins divided-power
      variables in successively higher hom-degrees to kill all
      positive homology, following Tate's construction.  The result
      is a semifree DG algebra resolution of @ TT "Q/I" @ over
      @ TT "Q" @ in the sense of Avramov, minimal in each hom-degree.
      When @ TT "R = Q/I" @ is passed, the defining ideal of
      @ TT "R" @ inside its ambient polynomial ring is used.
    Text
      For a complete intersection, the Koszul complex on a minimal
      generating set is already acyclic in positive degrees, so
      @ TT "minimalModel" @ returns just that Koszul DG algebra:
    Example
      Q = ZZ/101[x, y]
      I = ideal(x^2, y^2)
      A = minimalModel(I, EndDegree => 3)
      apply(0..3, n -> numcols getBasis(n, A))
    Text
      The @ TT "QuotientRing" @ form is equivalent:
    Example
      R = Q / I
      A' = minimalModel(R, EndDegree => 3)
      numgens A.natural == numgens A'.natural
    Text
      For a non-complete-intersection ideal the acyclic closure
      genuinely adjoins higher divided-power generators:
    Example
      J = ideal(x^2, x*y, y^2)
      B = minimalModel(J, EndDegree => 3)
      apply(0..3, n -> numcols getBasis(n, B))
    Text
      As with @ TO acyclicClosure @, @ TT "EndDegree" @ truncates
      the construction and @ TT "Variable" @ renames the adjoined
      generators:
    Example
      C = minimalModel(I, EndDegree => 2, Variable => "S")
      gens C.natural
    Text
      @ BOLD "Deviations and Gulliksen's theorem." @  The rank of
      @ TT "A" @ in hom-degree @ TT "n" @ is the @ TT "n" @-th
      @ EM "deviation" @ of @ TT "Q/I" @, a classical invariant
      which vanishes for @ TT "n >= 2" @ if and only if @ TT "Q/I" @
      is a complete intersection (Gulliksen-Avramov).  Compare the
      two shapes:
    Example
      A1 = minimalModel(ideal(x^2, y^2), EndDegree => 4);
      apply(0..4, n -> numcols getBasis(n, A1))
      A2 = minimalModel(ideal(x^2, x*y, y^2), EndDegree => 4);
      apply(0..4, n -> numcols getBasis(n, A2))
    Text
      @ TT "A1" @ concentrates in hom-degrees @ TT "0, 1" @ (the
      hallmark of a c.i.), while @ TT "A2" @ keeps acquiring
      divided-power generators, showing that @ TT "Q/(x^2, xy, y^2)" @
      is not a c.i.  Running
      @ TO minimalSemifreeResolution @ on the residue field over the
      minimal model then gives a test of Gulliksen's theorem, which
      states that over a codimension-@ TT "c" @ complete intersection
      the Betti numbers of any finitely generated module are
      eventually polynomial in the hom-degree, of degree at most
      @ TT "c - 1" @:
    Example
      A = minimalModel(ideal(x^2, y^2), EndDegree => 8);
      k = Q^1 / ideal(x, y);
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 8);
      b = apply(0..8, n -> numcols moduleDifferential(n, Mdg))
      apply(0..7, n -> b#(n+1) - b#n)
      apply(0..6, n -> b#(n+2) - 2*b#(n+1) + b#n)
    Text
      The Betti sequence is @ TT "(1, 4, 8, 12, 16, 20, 24, 28, 32)" @;
      first differences stabilize to a constant and second
      differences are eventually @ TT "0" @, so the numbers are
      eventually linear -- a degree-@ TT "1" @ polynomial, matching
      codimension @ TT "2" @.  A codimension-@ TT "3" @ c.i. such as
      @ TT "(x^2, y^2, z^2)" @ would instead show second differences
      stabilizing nonzero and third differences vanishing.
  Caveat
    The ring of @ TT "A" @ is always the polynomial ring containing
    the relations -- that is, @ TT "ring I" @ or @ TT "ambient R" @,
    not the quotient itself.  This matches the standard convention
    that the minimal DG algebra resolution of @ TT "Q/I" @ lives
    over @ TT "Q" @.
  SeeAlso
    acyclicClosure
    (acyclicClosure, Ring)
    koszulComplexDGA
    (koszulComplexDGA, Ideal)
///

doc ///
  Key
    (symbol **, DGAlgebra, Ring)
  Headline
    Base change of a DG algebra to another ring
  Usage
    B = A ** S
  Inputs
    A:DGAlgebra
    S:Ring
      A ring that admits substitution from A.ring.  The common case is
      @ TT "S = A.ring / I" @ for some ideal I, but any ring into which the
      variables of A.natural substitute is allowed.
  Outputs
    B:DGAlgebra
      A DG algebra over S with the same generator degrees as A and with
      differential obtained by substituting every entry of the differential
      matrix of A into B.natural.
  Description
    Text
      Base change: the underlying graded algebra is the same exterior / symmetric
      shape over S, and the differential is transported by @ TO substitute @.
      When S = A.ring / I, this models the DG algebra "A mod I" used, for
      instance, in building the Koszul complex of a quotient ring.
    Text
      The result is cached on @ TT "A.cache" @: repeated calls
      @ TT "A ** S" @ with the same pair (A, S) return the SAME DGAlgebra
      object.  This identity is essential for functoriality of base change on
      DGAlgebraMaps and DGModules — see @ TO "Base change and tensor with non-DG types" @.
    Example
      R = ZZ/101[a,b,c,d]
      A = koszulComplexDGA(R)
      S = R/ideal{a^3,a*b*c}
      B = A ** S
      B === A ** S
      Bdd = toComplex B
      Bdd.dd
    Text
      Base change along the trivial quotient @ TT "R / ideal 0_R" @ is structure-
      preserving: the result is a well-defined DGAlgebra over a ring isomorphic
      to R.
    Example
      R' = ZZ/101[x]
      A' = koszulComplexDGA R'
      QR' = R' / ideal(0_(R'))
      isWellDefined(A' ** QR')
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    (symbol **, DGAlgebraMap, Ring)
///

doc ///
  Key
    "Base change and tensor with non-DG types"
  Headline
    Tensoring DG objects with ordinary rings and modules
  Description
    Text
      The DGAlgebras package extends @ TO2((symbol **, DGAlgebra, Ring), "** ") @
      so that every DG object (@ TO DGAlgebra @, @ TO DGModule @, @ TO DGSubmodule @,
      @ TO DGQuotientModule @, @ TO DGIdeal @, @ TO DGAlgebraMap @, @ TO DGModuleMap @)
      can be base-changed along any ring map out of the ground ring.  It also defines
      the exterior tensor of a DGModule with an ordinary @ TO Module @, subsuming the
      usual ring-of-scalars base change as the special case @ TT "M ** S^1" @.
    Text
      Two design choices make these operations usable in larger constructions.
    Text
      @BOLD "Caching (object identity)."@  Both @ TO (symbol **, DGAlgebra, Ring) @ and
      @ TO (symbol **, DGModule, Ring) @ cache their result, so repeated calls with the
      same operands return the SAME object.  This matters because a DGAlgebraMap or
      DGModuleMap is tagged by its source and target DG-objects, and without identity
      the source of @ TT "id_M ** S" @ and the target of @ TT "id_M ** S" @ would be two
      distinct (though isomorphic) DGModules.
    Text
      @BOLD "Functoriality."@  These operations are functorial: if @ TT "id" @ is the
      identity DGModuleMap on M, then @ TT "id ** S" @ is the identity DGModuleMap on
      @ TT "M ** S" @; likewise for DGAlgebraMaps.  Composition is preserved.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      QR = R / ideal(x^2)
      MQ = M ** QR
      MQ === M ** QR
      idM = identityDGModuleMap M
      (idM ** QR).source === MQ
      (idM ** QR).target === MQ
      isWellDefined(idM ** QR)
    Text
      For a DGModule M and an ordinary free module N, @ TT "M ** N" @ forms the
      exterior tensor product over @ TT "A.natural" @.  This lets you tensor by
      copies of a ring, by direct sums, or by arbitrary graded shifts without
      leaving the DG world.
    Example
      N = R^{0, -1}
      MN = M ** N
      class MN
      numgens MN
      rank MN.natural == (rank M.natural) * (rank N)
    Text
      The tensor is associative in the natural way: you can chain
      @ TT "(M ** N) ** K" @ for a sequence of ordinary free modules, producing
      a DGModule whose natural rank is the product of the ranks.
    Example
      K = R^3
      MNK = (M ** R^2) ** K
      isWellDefined MNK
      numgens MNK == 2 * 2 * 3
    Text
      A deliberate omission: there is no @ TT "** Ideal" @ method.  For an ideal
      I, the underlying module @ TT "module I" @ is almost never free, so exterior
      tensor with a DGModule is not well-defined in our framework.  Use
      @ TT "** (R/I)" @ for base change along the quotient instead.
  Subnodes
    (symbol **, DGAlgebra, Ring)
    (symbol **, DGModule, Ring)
    (symbol **, DGSubmodule, Ring)
    (symbol **, DGQuotientModule, Ring)
    (symbol **, DGIdeal, Ring)
    (symbol **, DGAlgebraMap, Ring)
    (symbol **, DGModuleMap, Ring)
    (symbol **, DGModule, Module)
    (symbol **, DGSubmodule, Module)
    (symbol **, DGQuotientModule, Module)
///

doc ///
  Key
    (symbol **, DGModule, Ring)
  Headline
    Base change of a DG module along a ring map
  Usage
    MS = M ** S
  Inputs
    M:DGModule
      A (free) DG module over some DG algebra A.
    S:Ring
      A ring admitting substitution from A.ring.  Typically S = A.ring / I.
  Outputs
    MS:DGModule
      A DG module over @ TT "A ** S" @ with the same generator multi-degrees as M
      and with differential obtained by substituting every entry of each
      differential column into the base-changed algebra.
  Description
    Text
      This is the base change of M along the ring map A.ring -> S, lifted to the
      DG level.  Since A ** S and M ** S are both cached, repeated tensoring
      with S is idempotent up to object identity:
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      QR = R / ideal(x^2)
      MS = M ** QR
      MS === M ** QR
      MS.dgAlgebra === A ** QR
      isWellDefined MS
    Text
      When M is the Koszul complex on a sequence and the map A.ring -> S is a
      quotient map, @ TT "M ** S" @ is the Koszul complex of the same sequence
      computed over S.
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGAlgebra, Ring)
    (symbol **, DGModuleMap, Ring)
///

doc ///
  Key
    (symbol **, DGSubmodule, Ring)
  Headline
    Base change of a DG submodule along a ring map
  Usage
    SubS = Sub ** S
  Inputs
    Sub:DGSubmodule
      A DG submodule of some ambient DG module M.
    S:Ring
      A ring admitting substitution from the ground ring of Sub.
  Outputs
    SubS:DGSubmodule
      The DG submodule of @ TT "M ** S" @ whose inclusion matrix is obtained by
      substituting every entry of the original inclusion into the base-changed
      ring.
  Description
    Text
      The ambient is @ TT "(Sub.ambient) ** S" @, so that for any DGQuotientModule
      built from Sub the quotient @ TT "(M/Sub) ** S" @ equals
      @ TT "(M ** S) / (Sub ** S) " @ up to canonical equality.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      QR = R / ideal(x^2)
      SubQ = Sub ** QR
      isWellDefined SubQ
      SubQ.ambient === M ** QR
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    (symbol **, DGQuotientModule, Ring)
///

doc ///
  Key
    (symbol **, DGQuotientModule, Ring)
  Headline
    Base change of a DG quotient module along a ring map
  Usage
    QS = Q ** S
  Inputs
    Q:DGQuotientModule
    S:Ring
  Outputs
    QS:DGQuotientModule
      The quotient @ TT "(M ** S) / (Sub ** S)" @ where M = Q.ambient and
      Sub = Q.subDGModule.
  Description
    Text
      Internally, the submodule is rebuilt directly inside the base-changed
      ambient (rather than constructing @ TT "Sub ** S" @ separately and then
      quotienting); this is what ensures the ambient DGModule of the result is
      the cached @ TT "M ** S" @, not a distinct isomorphic copy.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat}})
      Q = M / Sub
      QR = R / ideal(x^2)
      QS = Q ** QR
      isWellDefined QS
      QS.ambient === M ** QR
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGSubmodule, Ring)
    (symbol **, DGModule, Ring)
///

doc ///
  Key
    (symbol **, DGIdeal, Ring)
  Headline
    Base change of a DG ideal along a ring map
  Usage
    IS = I ** S
  Inputs
    I:DGIdeal
      A DG ideal of some DG algebra A.
    S:Ring
  Outputs
    IS:DGIdeal
      The DG ideal of @ TT "A ** S" @ generated by the images of the original
      generators of I under @ TO substitute @.
  Description
    Text
      Unlike @ TT "** Ideal" @ (which is not defined — see the note in
      @ TO "Base change and tensor with non-DG types" @), this operation does
      land inside the DG world: DGIdeal is always generated by elements, so
      substitution along the ring map of base change produces a well-formed
      DGIdeal of @ TT "A ** S" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      QR = R / ideal(x^2)
      IQ = I ** QR
      isWellDefined IQ
      IQ.dgAlgebra === A ** QR
    Text
      Base change preserves the zero and unit DG ideals: the zero ideal stays
      zero, and the unit ideal stays the whole algebra.
    Example
      Z = dgIdeal(A, {})
      ZQ = Z ** QR
      isWellDefined ZQ and isZero ZQ
      U = dgIdeal(A, {1_Anat})
      UQ = U ** QR
      isWellDefined UQ and not isZero UQ
    Text
      Multivariate quotients and multi-generator DG ideals interact as expected.
    Example
      R3 = ZZ/101[x,y,z]
      A3 = koszulComplexDGA R3
      Anat3 = A3.natural
      I3 = dgIdeal(A3, {x_Anat3, y_Anat3, z_Anat3})
      QR3 = R3 / ideal(x*y, y*z)
      isWellDefined(I3 ** QR3)
  SeeAlso
    "Base change and tensor with non-DG types"
    dgIdeal
    (symbol **, DGAlgebra, Ring)
///

doc ///
  Key
    (symbol **, DGAlgebraMap, Ring)
  Headline
    Base change of a DG algebra map along a ring map
  Usage
    phiS = phi ** S
  Inputs
    phi:DGAlgebraMap
      A DG algebra map A -> B.
    S:Ring
  Outputs
    phiS:DGAlgebraMap
      A DG algebra map @ TT "A ** S -> B ** S" @ whose underlying RingMap
      sends each generator of A.natural to the image under phi, substituted
      into @ TT "(B ** S).natural" @.
  Description
    Text
      Functorial and compatible with object identity: because @ TT "A ** S" @
      and @ TT "B ** S" @ are cached, repeated evaluations of @ TT "phi ** S" @
      land in the same source/target, and
      @ TT "identityDGAlgebraMap(A) ** S" @ equals
      @ TT "identityDGAlgebraMap(A ** S)" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      idA = identityDGAlgebraMap A
      QR = R / ideal(x^2)
      idAQ = idA ** QR
      isWellDefined idAQ
      (idAQ.source) === (A ** QR)
      (idAQ.target) === (A ** QR)
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGAlgebra, Ring)
    identityDGAlgebraMap
///

doc ///
  Key
    (symbol **, DGModuleMap, Ring)
  Headline
    Base change of a DG module map along a ring map
  Usage
    fS = f ** S
  Inputs
    f:DGModuleMap
      A DG module map M -> N over a DG algebra A.
    S:Ring
  Outputs
    fS:DGModuleMap
      A DG module map @ TT "M ** S -> N ** S" @ (over @ TT "A ** S" @) whose
      underlying matrix is obtained entry-wise by substituting each entry of
      f.natural into the base-changed ring.
  Description
    Text
      Functorial: @ TT "identityDGModuleMap(M) ** S" @ equals
      @ TT "identityDGModuleMap(M ** S)" @, and composition is preserved.
      Cache-identity is what ensures the source and target of the result are
      the same cached DG modules as @ TT "M ** S" @ and @ TT "N ** S" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      idM = identityDGModuleMap M
      QR = R / ideal(x^2)
      idMQ = idM ** QR
      isWellDefined idMQ
      (idMQ.source) === (M ** QR)
      (idMQ.target) === (M ** QR)
    Text
      Functoriality is literal at the matrix level: the natural matrix of
      @ TT "idM ** QR" @ is the identity matrix on @ TT "(M ** QR).natural" @.
    Example
      MQ = M ** QR
      idMQ.natural == id_(MQ.natural)
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    identityDGModuleMap
///

doc ///
  Key
    (symbol **, DGModule, Module)
    (symbol **, Module, DGModule)
  Headline
    Exterior tensor product of a DG module with an ordinary free module
  Usage
    P = M ** N
    P = N ** M
  Inputs
    M:DGModule
      Must be semifree: M.natural must be a free A.natural-module.
    N:Module
      An ordinary module over A.ring (or a compatible ring) that is also free.
  Outputs
    P:DGModule
      A DG module over the same DG algebra A, with
      @ TT "rank P.natural = (rank M.natural) * (rank N)" @ generators.  The
      multi-degree of the (i, j)-th natural generator is
      @ TT "M.Degrees#i + (0, (degrees N)#j)" @ (homological degree from M,
      internal degrees shifted by the internal degree of the j-th generator of N).
      The differential is @ TT "d_M \\otimes id_N" @.
  Description
    Text
      This is the exterior tensor product of DG-modules, where @ TT "N" @ is
      viewed as a complex concentrated in homological degree 0 with zero
      differential.  The commuted form @ TT "N ** M" @ is defined as
      @ TT "M ** N" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      N = R^2
      MN = M ** N
      class MN
      numgens MN
      isWellDefined MN
    Text
      The special case @ TT "M ** R^1" @ is canonically isomorphic to M, and
      @ TT "M ** R^{d}" @ is a shift of M by the internal degree d.
    Text
      Multi-degree M tensors predictably: if M has multiple homological degrees
      and N is free of rank r, the result has @ TT "(rank M.natural) * r" @
      natural generators and is well-defined.
    Example
      M2m = freeDGModule(A, {0, 1})
      N3 = R^3
      M2N = M2m ** N3
      numgens M2N == 2 * 3
      isWellDefined M2N
    Text
      Edge case: tensoring with the zero module @ TT "R^0" @ gives a well-defined
      zero DGModule.
    Example
      MZ = M ** R^0
      isWellDefined MZ
      numgens MZ == 0
  Caveat
    Both M and N must be free (semifree on the DG side).  For the non-free case,
    take an initial free resolution and then tensor.
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Ring)
    (symbol **, DGModule, DGModule)
///

doc ///
  Key
    (symbol **, DGSubmodule, Module)
    (symbol **, Module, DGSubmodule)
  Headline
    Exterior tensor product of a DG submodule with an ordinary free module
  Usage
    SubN = Sub ** N
    SubN = N ** Sub
  Inputs
    Sub:DGSubmodule
    N:Module
      A free module over a compatible ring.
  Outputs
    SubN:DGSubmodule
      The DG submodule of @ TT "(Sub.ambient) ** N" @ whose inclusion acts as
      @ TT "inclusion(Sub) \\otimes id_N" @ on natural generators indexed by
      (i, j).
  Description
    Text
      Equivalently: for each natural generator of N, embed a copy of Sub's
      inclusion matrix into the corresponding block of the tensored ambient.
      The commuted form @ TT "N ** Sub" @ is defined as @ TT "Sub ** N" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      N = R^2
      SubN = Sub ** N
      isWellDefined SubN
      SubN.ambient === M ** N
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Module)
    (symbol **, DGSubmodule, Ring)
///

doc ///
  Key
    (symbol **, DGQuotientModule, Module)
    (symbol **, Module, DGQuotientModule)
  Headline
    Exterior tensor product of a DG quotient module with an ordinary free module
  Usage
    QN = Q ** N
    QN = N ** Q
  Inputs
    Q:DGQuotientModule
    N:Module
      A free module over a compatible ring.
  Outputs
    QN:DGQuotientModule
      The DG quotient module @ TT "(Q.ambient ** N) / (Q.subDGModule ** N)" @,
      built directly inside the tensored ambient so that the ambient of the
      result equals @ TT "Q.ambient ** N" @ on the nose.
  Description
    Text
      The commuted form @ TT "N ** Q" @ is defined as @ TT "Q ** N" @.
    Example
      R = ZZ/101[x,y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      Sub = dgSubmodule(M, matrix {{x_Anat}})
      Q = M / Sub
      N = R^2
      QN = Q ** N
      isWellDefined QN
      QN.ambient === M ** N
  SeeAlso
    "Base change and tensor with non-DG types"
    (symbol **, DGModule, Module)
    (symbol **, DGQuotientModule, Ring)
///

doc ///
  Key
    "Operations on DG Ideals"
  Headline
    Sum, product, power, intersection, colon, and other ideal-algebra operations on DG ideals
  Description
    Text
      A DG ideal of a DG algebra A is a graded ideal of @ TT "A.natural" @ that is
      closed under the differential d of A.  The @ TO DGIdeal @ type wraps such an
      ideal and exposes the standard M2 @ TO Ideal @ operations, each of which
      preserves d-closure and returns a new @ TO DGIdeal @.
    Text
      Constructor: @ TO dgIdeal @ takes a List, Matrix, or Ideal of elements of
      @ TT "A.natural" @ and iteratively d-saturates the ideal they generate
      until it is closed under d.  Warning: if an input element is not a cycle,
      the d-saturation enlarges the ideal (possibly to the unit ideal).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      isWellDefined I
      isDGIdeal(A, I.natural)
    Text
      Ideal-algebra operations all preserve d-closure by elementary arguments
      (sum: @ TT "d(I+J) \\subset dI + dJ \\subset I + J" @; product: by the
      Leibniz rule; intersection: direct; power: induction; colon: Leibniz +
      @ TT "f J \\subset I" @ implies @ TT "(df) J \\subset I + f(dJ) \\subset I" @).
      Concretely, they all return well-defined DGIdeals.
    Example
      isWellDefined(I + J)
      isWellDefined(I * J)
      isWellDefined(I^3)
      isWellDefined intersect(I, J)
      isWellDefined(I : J)
    Text
      Equality, containment, and reduction mirror the usual behavior on @ TO Ideal @.
    Example
      isSubset(I, I + J)
      I != I + J
      (x_Anat^2 % I) == 0
      (y_Anat^2 % I) == y_Anat^2
    Text
      Quotient DG algebras: @ TT "A / I" @ builds the DG algebra whose
      underlying ring is @ TT "A.natural / I.natural" @, with differential
      descending from d (well-defined precisely because I is d-closed).
    Example
      B = A / I
      class B
      isWellDefined B
    Text
      Compatibility: any operation that takes an @ TO Ideal @ and lives on
      @ TT "I.natural" @ can be reached via @ TT "I.natural" @, but the
      wrapped operations on DGIdeal return DGIdeals (so d-closure is
      certified on the result by running through @ TO dgIdeal @ once more).
  Subnodes
    DGIdeal
    dgIdeal
    isDGIdeal
    (isWellDefined, DGIdeal)
    (symbol +, DGIdeal, DGIdeal)
    (symbol *, DGIdeal, DGIdeal)
    (symbol ^, DGIdeal, ZZ)
    (intersect, DGIdeal, DGIdeal)
    (symbol :, DGIdeal, DGIdeal)
    (symbol ==, DGIdeal, DGIdeal)
    (symbol %, RingElement, DGIdeal)
    (mingens, DGIdeal)
    (prune, DGIdeal)
    (symbol /, DGAlgebra, DGIdeal)
    (isHomogeneous, DGIdeal)
  SeeAlso
    DGIdeal
    dgIdeal
    (symbol **, DGIdeal, Ring)
///

doc ///
  Key
    DGIdeal
  Headline
    The class of d-closed graded ideals of a DG algebra
  Description
    Text
      A @ TT "DGIdeal" @ wraps a graded ideal of @ TT "A.natural" @ that is
      closed under the differential d of A.  It records the ambient DG
      algebra, the underlying Ideal, and a chosen set of generators.  Create
      one via @ TO dgIdeal @.
    Text
      The standard ideal-algebra operations (@ TO (symbol +, DGIdeal, DGIdeal) @,
      @ TO (symbol *, DGIdeal, DGIdeal) @, @ TO (symbol ^, DGIdeal, ZZ) @,
      @ TO (intersect, DGIdeal, DGIdeal) @, @ TO (symbol :, DGIdeal, DGIdeal) @)
      all preserve d-closure and return new DGIdeals.  See
      @ TO "Operations on DG Ideals" @ for a guided tour.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat * y_Anat})
      class I
      ambient I === A
      ring I === R
      numgens I
  SeeAlso
    dgIdeal
    "Operations on DG Ideals"
    (symbol /, DGAlgebra, DGIdeal)
///

doc ///
  Key
    dgIdeal
    (dgIdeal, DGAlgebra, List)
    (dgIdeal, DGAlgebra, Matrix)
    (dgIdeal, DGAlgebra, Ideal)
  Headline
    Construct a DG ideal from a list, matrix, or ideal of generators
  Usage
    I = dgIdeal(A, gs)
  Inputs
    A:DGAlgebra
    gs:
      A List of elements of @ TT "A.natural" @, a Matrix of such, or an
      @ TO Ideal @ of @ TT "A.natural" @.
  Outputs
    I:DGIdeal
      The smallest d-closed ideal of @ TT "A.natural" @ containing the given
      generators.  Constructed by iteratively adjoining derivatives until the
      ideal stabilizes.
  Description
    Text
      If the inputs are already cycles, no new generators are introduced and
      @ TT "I.natural" @ is the ordinary ideal they generate.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      xA = sub(x, Anat)
      I = dgIdeal(A, {xA})
      numgens I.natural == 1
      isDGIdeal(A, I.natural)
    Text
      If a seed element is not a cycle, d-saturation may enlarge the ideal.
      For the Koszul complex on {x}, @ TT "d(T_1) = x" @, so
      @ TT "dgIdeal(A, {T_1}) " @ ends up containing both @ TT "T_1" @ and
      @ TT "x" @.
    Example
      R' = ZZ/101[x]
      A' = koszulComplexDGA R'
      T = (A'.natural)_0
      I' = dgIdeal(A', {T})
      numgens I'.natural
      isDGIdeal(A', I'.natural)
    Text
      A more interesting multi-variable example: over the Koszul algebra of
      @ TT "QQ[x, y]" @, the top-degree exterior product @ TT "T_1 T_2" @ has
      @ TT "d(T_1 T_2) = -y T_1 + x T_2" @, and d-saturation records both the
      seed and its differential as minimal generators of the resulting
      d-closed ideal:
    Example
      R2 = QQ[x, y]
      A2 = koszulComplexDGA R2
      T1 = (A2.natural)_0
      T2 = (A2.natural)_1
      polyDifferential(A2, T1 * T2)
      J = dgIdeal(A2, {T1 * T2})
      mingens J.natural
    Text
      An empty seed or a list of zero seeds yields the zero DG ideal.
    Example
      Z = dgIdeal(A, {})
      isWellDefined Z
      isZero Z
      Z' = dgIdeal(A, {0_Anat, 0_Anat})
      isZero Z'
    Text
      A seed containing @ TT "1" @ gives the unit DG ideal.
    Example
      U = dgIdeal(A, {1_Anat, x_Anat})
      U.natural == ideal 1_Anat
  SeeAlso
    DGIdeal
    isDGIdeal
    "Operations on DG Ideals"
///

doc ///
  Key
    isDGIdeal
    (isDGIdeal, DGAlgebra, Ideal)
  Headline
    Test whether an ideal of A.natural is closed under the differential of A
  Usage
    b = isDGIdeal(A, I)
  Inputs
    A:DGAlgebra
    I:Ideal
      An ideal of @ TT "A.natural" @.
  Outputs
    b:Boolean
      True iff every minimal generator g of I satisfies
      @ TT "polyDifferential(A, g) % I == 0" @.
  Description
    Text
      Unlike @ TO dgIdeal @ (which d-saturates), @ TT "isDGIdeal" @ simply
      checks d-closure without modifying the input.  Use it to validate that
      an externally-produced Ideal is already a DG ideal.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      isDGIdeal(A, ideal(x_Anat, y_Anat))
      T = Anat_0   -- first Koszul variable T_{1,1}; d(T) = x
      isDGIdeal(A, ideal(T))
    Text
      The second test returns false: the ideal @ TT "(T)" @ is not d-closed
      because @ TT "d(T) = x" @ is not in it.  Passing the same seed to
      @ TO dgIdeal @ produces the d-closure.
    Example
      J = dgIdeal(A, {T})
      isDGIdeal(A, J.natural)
  SeeAlso
    dgIdeal
    DGIdeal
///

doc ///
  Key
    (isWellDefined, DGIdeal)
  Headline
    Verify that a DGIdeal has correct structure and is d-closed
  Usage
    b = isWellDefined I
  Inputs
    I:DGIdeal
  Outputs
    b:Boolean
  Description
    Text
      Performs (1) a key-shape check — the expected fields (@ TT "dgAlgebra, ring,
      natural, generators, cache" @) are present and have the correct types —
      and (2) the semantic d-closure check via @ TO isDGIdeal @.  Set
      @ TT "debugLevel > 0" @ to see a diagnostic message on failure.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      isWellDefined I
  SeeAlso
    dgIdeal
    isDGIdeal
    DGIdeal
///

doc ///
  Key
    (symbol +, DGIdeal, DGIdeal)
  Headline
    Sum of two DG ideals
  Usage
    K = I + J
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = I.natural + J.natural" @.
  Description
    Text
      Preserves d-closure because @ TT "d(I + J) \\subset dI + dJ \\subset I + J" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      K = I + J
      isWellDefined K
      (I + J) == (J + I)
    Text
      The zero DGIdeal is a two-sided identity for @ TT "+" @.
    Example
      Z = dgIdeal(A, {})
      (Z + I) == I
  SeeAlso
    "Operations on DG Ideals"
    (symbol *, DGIdeal, DGIdeal)
    (intersect, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol *, DGIdeal, DGIdeal)
  Headline
    Product of two DG ideals
  Usage
    K = I * J
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = I.natural * J.natural" @.
  Description
    Text
      Preserves d-closure by the Leibniz rule: @ TT "d(ab) = d(a) b \\pm a d(b)" @
      shows @ TT "d(IJ) \\subset (dI) J + I (dJ) \\subset IJ" @.  When the ambient
      ring is commutative, multiplication is commutative.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      K = I * J
      isWellDefined K
      (I * J) == (J * I)
    Text
      The unit DGIdeal is a two-sided identity for @ TT "*" @ (as ideals).
    Example
      U = dgIdeal(A, {1_Anat})
      (U * I) == I
  SeeAlso
    "Operations on DG Ideals"
    (symbol +, DGIdeal, DGIdeal)
    (symbol ^, DGIdeal, ZZ)
///

doc ///
  Key
    (symbol ^, DGIdeal, ZZ)
  Headline
    Nonnegative integer power of a DG ideal
  Usage
    K = I^n
  Inputs
    I:DGIdeal
    n:ZZ
      A nonnegative integer.
  Outputs
    K:DGIdeal
      @ TT "I^0" @ is the unit DGIdeal @ TT "(1)" @; @ TT "I^n" @ for
      @ TT "n >= 1" @ is the n-fold product @ TT "I * I * ... * I" @.
  Description
    Text
      Distributes with multiplication: @ TT "I^2 * I == I^3" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      isWellDefined(I^2)
      isWellDefined(I^3)
      (I^2 * I) == I^3
    Text
      @ TT "I^0" @ always returns the unit DGIdeal @ TT "(1)" @.  Taking
      powers of the zero DG ideal is well-defined at every exponent.
    Example
      I^0
      Z = dgIdeal(A, {})
      isWellDefined(Z^0) and isWellDefined(Z^5)
  Caveat
    Negative exponents are rejected with an error.
  SeeAlso
    "Operations on DG Ideals"
    (symbol *, DGIdeal, DGIdeal)
///

doc ///
  Key
    (intersect, DGIdeal, DGIdeal)
  Headline
    Intersection of two DG ideals
  Usage
    K = intersect(I, J)
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = intersect(I.natural, J.natural)" @.
  Description
    Text
      Intersections preserve d-closure directly: if g lies in both I and J,
      then d(g) lies in both (by d-closure of each), hence in their intersection.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {y_Anat})
      K = intersect(I, J)
      isWellDefined K
    Text
      Intersecting with the zero DGIdeal yields the zero DGIdeal.
    Example
      Z = dgIdeal(A, {})
      ZI = intersect(Z, I)
      isWellDefined ZI
      numgens ZI.natural == 0
  SeeAlso
    "Operations on DG Ideals"
    (symbol +, DGIdeal, DGIdeal)
    (symbol *, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol :, DGIdeal, DGIdeal)
  Headline
    Ideal quotient (colon) of two DG ideals
  Usage
    K = I : J
  Inputs
    I:DGIdeal
    J:DGIdeal
      With @ TT "I.dgAlgebra === J.dgAlgebra" @.
  Outputs
    K:DGIdeal
      The DGIdeal with @ TT "K.natural = I.natural : J.natural" @ — i.e.
      @ TT "{ f \\in A.natural | f J \\subset I }" @.
  Description
    Text
      Preserves d-closure: if @ TT "f J \\subset I" @ then by Leibniz
      @ TT "d(f) J = d(fJ) - f d(J) \\subset dI + f J \\subset I + I = I" @,
      so @ TT "d(f) \\in I : J" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat * y_Anat})
      J = dgIdeal(A, {y_Anat})
      K = I : J
      isWellDefined K
  SeeAlso
    "Operations on DG Ideals"
    (symbol *, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol ==, DGIdeal, DGIdeal)
    (isSubset, DGIdeal, DGIdeal)
  Headline
    Equality and containment of DG ideals
  Usage
    b = I == J
    b = isSubset(I, J)
  Inputs
    I:DGIdeal
    J:DGIdeal
  Outputs
    b:Boolean
  Description
    Text
      Both compare DGIdeals via their underlying Ideals @ TT "I.natural" @
      and @ TT "J.natural" @.  Equality additionally requires the DG algebras
      to be the same object (@ TT "=== " @ — object identity, not just
      isomorphism).  Containment also asserts that I and J share an ambient
      DG algebra.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      J = dgIdeal(A, {x_Anat, y_Anat})
      isSubset(I, J)
      I != J
      I == I
  SeeAlso
    "Operations on DG Ideals"
    (symbol +, DGIdeal, DGIdeal)
///

doc ///
  Key
    (symbol %, RingElement, DGIdeal)
    (symbol %, ZZ, DGIdeal)
  Headline
    Reduce a ring element modulo a DG ideal
  Usage
    r = f % I
  Inputs
    f:
      A RingElement of @ TT "I.dgAlgebra.natural" @, or an integer (coerced
      into @ TT "I.dgAlgebra.natural" @ before reduction).
    I:DGIdeal
  Outputs
    r:RingElement
      The normal form of f modulo @ TT "I.natural" @.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat})
      (x_Anat^2 % I) == 0
      (y_Anat^2 % I) == y_Anat^2
  SeeAlso
    "Operations on DG Ideals"
    (symbol ==, DGIdeal, DGIdeal)
///

doc ///
  Key
    (mingens, DGIdeal)
    (numgens, DGIdeal)
    (module, DGIdeal)
    (ring, DGIdeal)
    (generators, DGIdeal)
  Headline
    Basic accessors on a DG ideal
  Description
    Text
      Thin wrappers around the corresponding @ TO Ideal @ operations on
      @ TT "I.natural" @, with the exception of @ TT "ambient I" @, which
      returns the ambient @ TO DGAlgebra @ (not its underlying ring).
    Text
      @ TT "mingens I" @ returns the minimal-generator matrix of
      @ TT "I.natural" @.  Note that the d-saturation in @ TO dgIdeal @ may
      have produced redundant generators; @ TT "mingens I" @ re-extracts a
      minimal set.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, x_Anat * y_Anat})
      mingens I
      numgens I
    Text
      @ TT "numgens I" @ equals @ TT "numgens I.natural" @ — the number of
      (stored, possibly redundant) generators of the underlying ideal.
    Example
      numgens I == numgens I.natural
    Text
      @ TT "module I" @ returns the underlying Module-view of the ideal, and
      @ TT "ring I, ambient I, generators I" @ expose the ground ring, the
      ambient DG algebra, and the chosen generator matrix, respectively.
    Example
      ring I === R
      ambient I === A
      module I
  SeeAlso
    "Operations on DG Ideals"
    dgIdeal
    (prune, DGIdeal)
///

doc ///
  Key
    (prune, DGIdeal)
    (minimalPresentation, DGIdeal)
  Headline
    Trim a DG ideal down to a minimal generating set
  Usage
    J = prune I
    J = minimalPresentation I
  Inputs
    I:DGIdeal
  Outputs
    J:DGIdeal
      A DGIdeal with @ TT "J.natural == I.natural" @ (as ideals) but with a
      possibly smaller generator list obtained by @ TO trim @.
  Description
    Text
      @ TT "prune" @ computes @ TT "trim I.natural" @ and wraps the result.
      It caches a @ TT "pruningMap" @ in @ TT "J.cache" @ giving the canonical
      comparison map @ TT "J -> I" @; since DGIdeals have the same ambient
      DG algebra before and after pruning, this is recorded as the identity
      DGAlgebraMap on @ TT "I.dgAlgebra" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, x_Anat * y_Anat, x_Anat^2})
      J = prune I
      isWellDefined J
      J.natural == I.natural
    Text
      Idempotent: pruning an already-minimal DGIdeal is a no-op at the
      ideal level (the stored generator list already agrees with
      @ TT "mingens" @).  Repeated pruning is safe.
    Example
      isWellDefined prune prune prune J
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @ on
      DGIdeals.
  SeeAlso
    "Operations on DG Ideals"
    (mingens, DGIdeal)
    dgIdeal
///

doc ///
  Key
    (symbol /, DGAlgebra, DGIdeal)
  Headline
    Quotient DG algebra by a DG ideal
  Usage
    B = A / I
  Inputs
    A:DGAlgebra
    I:DGIdeal
      A DG ideal of A.  Must satisfy @ TT "I.dgAlgebra === A" @.
  Outputs
    B:DGAlgebra
      A DG algebra whose underlying ring is @ TT "A.natural / I.natural" @
      and whose differential is the (well-defined) descent of d_A.
  Description
    Text
      The descent is well-defined precisely because I is d-closed: for any
      lift g' of a class g in B, the difference @ TT "g - g'" @ lies in I,
      so @ TT "d_A(g) - d_A(g') \\in d_A(I) \\subset I" @ and the image
      @ TT "[d_A(g)] \\in B" @ is independent of the chosen lift.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      Anat = A.natural
      T = Anat_0
      I = dgIdeal(A, {T})
      B = A / I
      isWellDefined B
      B.natural
  SeeAlso
    "Operations on DG Ideals"
    DGIdeal
    dgIdeal
///

doc ///
  Key
    (isHomogeneous, DGIdeal)
    (isZero, DGIdeal)
  Headline
    Predicates on a DG ideal
  Description
    Text
      @ TT "isHomogeneous I" @ returns @ TT "isHomogeneous I.natural" @ —
      i.e. tests whether the underlying ideal is homogeneous with respect to
      the grading of @ TT "A.natural" @.
    Text
      @ TT "isZero I" @ returns true iff @ TT "I.natural" @ is the zero
      ideal (equivalently, @ TT "numgens I.natural == 0" @).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      I = dgIdeal(A, {x_Anat, y_Anat})
      isHomogeneous I
      isZero I
      Z = dgIdeal(A, {})
      isZero Z
  SeeAlso
    "Operations on DG Ideals"
    DGIdeal
///

doc ///
  Key
    "Building DG modules, submodules, and quotients"
  Headline
    The DGModule / DGSubmodule / DGQuotientModule hierarchy and its constructors
  Description
    Text
      The package provides three related types of DG-module-like objects
      over a fixed @ TO DGAlgebra @ A:
    Text
      @ TO DGModule @ — a free graded @ TT "A.natural" @-module equipped
      with a differential that satisfies the Leibniz rule against A.
      Built via @ TO freeDGModule @, with differentials set by
      @ TO setDiff @.
    Text
      @ TO DGSubmodule @ — a subobject of a DGModule M, represented by a
      matrix of generators whose column span is closed under the
      differential of M.  Built via @ TO dgSubmodule @, which
      @ TT "d" @-saturates the seed generators automatically.
    Text
      @ TO DGQuotientModule @ — the cokernel M / S of the inclusion
      S → M, with the induced differential.  Built via @ TO dgQuotientModule @
      or the shorthand @ TT "M / S" @.
    Text
      Every object in this hierarchy carries an underlying graded
      @ TT "A.natural" @-module accessible via @ TT "M.natural" @, a list of
      multi-degrees @ TT "M.Degrees" @, and a list of differentials
      @ TT "M.diff" @ (one per natural generator for DGModule and
      DGSubmodule; per quotient-presentation generator for
      DGQuotientModule).  Structural correctness can be checked with
      @ TO isWellDefined @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      isWellDefined M
      Anat = A.natural
      S = dgSubmodule(M, (id_(M.natural))_{0})
      isWellDefined S
      Q = M / S
      isWellDefined Q
  Subnodes
    DGModule
    DGSubmodule
    DGQuotientModule
    freeDGModule
    dgSubmodule
    dgQuotientModule
    (symbol /, DGModule, DGSubmodule)
  SeeAlso
    "Module-like operations on DG modules"
    "Operations on DG Submodules"
    DGAlgebra
///

doc ///
  Key
    DGModule
  Headline
    The class of DG modules over a DG algebra
  Description
    Text
      A @ TT "DGModule" @ M over a @ TO DGAlgebra @ A is a free graded
      @ TT "A.natural" @-module together with a differential that squares
      to zero and satisfies the Leibniz rule against A.  Internally M is
      a hashtable with keys:
    Text
      @ TT "M.dgAlgebra" @ — the ambient @ TO DGAlgebra @.
    Text
      @ TT "M.natural" @ — the underlying graded @ TT "A.natural" @-module.
    Text
      @ TT "M.Degrees" @ — the list of multi-degrees of the natural generators.
    Text
      @ TT "M.diff" @ — a list of differentials, one per natural generator,
      each living in @ TT "M.natural" @ in the appropriate homological degree.
    Text
      New DGModules are constructed by @ TO freeDGModule @; the differential
      on generators is set via @ TO setDiff @.  Every
      @ TO DGSubmodule @ is also a DGModule (as a subtype); by contrast a
      @ TO DGQuotientModule @ is a separate type with a compatible presentation.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      isWellDefined M
      M.Degrees
  SeeAlso
    "Building DG modules, submodules, and quotients"
    freeDGModule
    setDiff
    DGSubmodule
    DGQuotientModule
///

doc ///
  Key
    DGSubmodule
  Headline
    The class of d-closed submodules of a DG module
  Description
    Text
      A @ TT "DGSubmodule" @ S of a @ TO DGModule @ M is a free
      A.natural-module together with an inclusion
      @ TT "S.inclusion : S → M" @ of DGModules whose column span in
      @ TT "M.natural" @ is closed under the differential of M.
      @ TT "DGSubmodule" @ is a subtype of @ TO DGModule @, so every
      DG-module operation applies to submodules as well.
    Text
      Beyond the DGModule keys, a DGSubmodule carries:
    Text
      @ TT "S.ambient" @ — the enclosing DGModule M.
    Text
      @ TT "S.inclusion" @ — a @ TO DGModuleMap @ S → M; use
      @ TO inclusion @ to access it.
    Text
      New DGSubmodules are constructed by @ TO dgSubmodule @, which
      takes either an inclusion matrix or a list of generators, and
      d-saturates the input automatically.  See
      @ TO "Operations on DG Submodules" @ for the lattice operations
      (sum, intersection, equality, containment).
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, id_(M.natural))
      ambient S === M
      source inclusion S === S
      target inclusion S === M
  SeeAlso
    "Building DG modules, submodules, and quotients"
    "Operations on DG Submodules"
    dgSubmodule
    DGModule
    inclusion
///

doc ///
  Key
    DGQuotientModule
  Headline
    The class of quotient DG modules M / S
  Description
    Text
      A @ TT "DGQuotientModule" @ Q = M / S is the cokernel of the
      inclusion @ TT "S.inclusion : S → M" @, equipped with the induced
      differential.  Unlike @ TO DGSubmodule @, @ TT "DGQuotientModule" @
      is NOT a subtype of @ TO DGModule @: its @ TT ".natural" @ is a
      cokernel module, which does not support the free-module operations
      (rank, basis by index) that DG algorithms assume on a generic
      DGModule.
    Text
      Beyond the DGModule-like keys (@ TT ".dgAlgebra" @, @ TT ".natural" @,
      @ TT ".Degrees" @, @ TT ".diff" @), a DGQuotientModule carries:
    Text
      @ TT "Q.ambient" @ — the DGModule M from which Q was built;
      accessible via @ TO ambient @.
    Text
      @ TT "Q.subDGModule" @ — the killed submodule S; accessible via
      @ TO subDGModule @.
    Text
      @ TT "Q.projection" @ — the quotient map M → Q as a
      @ TO DGModuleMap @; accessible via @ TO projection @.
    Text
      New DGQuotientModules are built by @ TO dgQuotientModule @ or the
      shorthand @ TT "M / S" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Z = dgSubmodule(M, {})
      Q = M / Z
      instance(Q, DGQuotientModule)
      ambient Q === M
      subDGModule Q === Z
      source projection Q === M
      target projection Q === Q
  SeeAlso
    "Building DG modules, submodules, and quotients"
    "Module-like operations on DG modules"
    dgQuotientModule
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    freeDGModule
    (freeDGModule, DGAlgebra, List)
  Headline
    Construct a free DG module over a DG algebra
  Usage
    M = freeDGModule(A, degList)
  Inputs
    A:DGAlgebra
    degList:List
      of multi-degrees.  Each entry is either an integer (interpreted as
      a homological degree with trivial internal degree) or a list whose
      first entry is the homological degree and whose remaining entries
      are internal degrees.  Short lists are right-padded with zeros to
      match the length of a degree in @ TT "degrees A.natural" @.
  Outputs
    M:DGModule
      A DGModule whose @ TT ".natural" @ is the graded free
      @ TT "A.natural" @-module of rank @ TT "#degList" @ with the
      specified generator degrees, and whose differential is initialized
      to zero on every generator.
  Description
    Text
      The differential is initially set to zero on every generator, and is
      customized afterward via @ TO setDiff @.  Because the constructor
      returns a free @ TO DGModule @, the result can serve as a building block
      for semifree resolutions (see @ TO adjoinGenerators @ and @ TO semifreeResolution @)
      or as the starting point for a hand-built DG module, as illustrated below.
    Text
      Here we build the 2-periodic minimal free resolution of the residue field
      over the hypersurface @ TT "R = k[x]/(x^2)" @ — the prototypical Eisenbud
      matrix factorization — as an infinite semifree DG module over the Koszul
      DG algebra:
    Example
      R = QQ[x] / ideal(x^2)
      A = koszulComplexDGA R
      F = freeDGModule(A, toList(0..4))
      natGens = apply(rank F.natural, i -> (F.natural)_i)
      setDiff(F, {0, x * natGens#0, x * natGens#1, x * natGens#2, x * natGens#3})
      apply(1..4, n -> moduleDifferential(n, F))
    Text
      The differential alternates between multiplication by @ TT "x" @ in
      every degree, matching the classical 2-periodic resolution of the
      residue field.  Its homology is concentrated in degree zero:
    Example
      apply(0..3, n -> prune homology(n, F))
    Text
      More elaborate differentials are possible once multiple generators
      sit in overlapping hom-degrees.  Here is a rank-3 toy example where
      @ TT "F_1" @ has two generators and @ TT "d_1" @ sends them to
      @ TT "x" @ and @ TT "y" @ respectively, recovering the truncation of
      the Koszul complex on @ TT "(x, y)" @:
    Example
      S = QQ[x, y]
      AS = koszulComplexDGA S
      G = freeDGModule(AS, {0, 1, 1})
      natG = apply(rank G.natural, i -> (G.natural)_i)
      setDiff(G, {0, x * natG#0, y * natG#0})
      moduleDifferential(1, G)
    Text
      Internal degrees from a multi-graded base ring are inherited; a
      plain homological degree is padded with zeros to match.
    Example
      Rg = ZZ/101[x, y, Degrees => {{1, 0}, {0, 1}}]
      Ag = koszulComplexDGA Rg
      Mg = freeDGModule(Ag, {{0, 0, 0}, {1, 0, 0}})
      degrees Mg
    Text
      The empty-list form gives the zero DG module.
    Example
      M0 = freeDGModule(A, {})
      isZero M0
  SeeAlso
    "Building DG modules, submodules, and quotients"
    DGModule
    setDiff
    adjoinGenerators
    semifreeResolution
    (isWellDefined, DGModule)
///

doc ///
  Key
    dgSubmodule
    (dgSubmodule, DGModule, Matrix)
    (dgSubmodule, DGModule, List)
  Headline
    Construct a d-closed submodule of a DG module
  Usage
    S = dgSubmodule(M, incMat)
    S = dgSubmodule(M, gs)
  Inputs
    M:DGModule
    incMat:Matrix
      with target @ TT "M.natural" @, whose columns are proposed
      generators of the submodule.
    gs:List
      of elements of @ TT "M.natural" @ (as Vectors, 1-column Matrices,
      or anything coercible to @ TT "M.natural" @).  The empty list gives
      the zero submodule.
  Outputs
    S:DGSubmodule
      The smallest d-closed submodule of M containing the columns of
      @ TT "incMat" @ (respectively the elements of @ TT "gs" @).
  Description
    Text
      The constructor iteratively adjoins the M-differentials of the
      current generators until the column span is closed, then builds
      the lifted differential on the resulting free module.  If d-closure
      fails to stabilize within 200 iterations, an error is raised — in
      practice this only happens if the ambient module has unbounded
      homological degree and the seed is chosen poorly.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, id_(M.natural))
      instance(S, DGSubmodule)
      ambient S === M
      isWellDefined S
    Text
      The list form coerces each entry to an element of
      @ TT "M.natural" @; passing the empty list produces the zero
      submodule.
    Example
      Z = dgSubmodule(M, {})
      numcols (inclusion Z).natural == 0
      isZero Z
    Text
      d-closure is performed automatically.  Seeding with a non-closed
      generator pulls in its M-differential:
    Example
      M2 = freeDGModule(A, {0, 1})
      natGens = apply(rank M2.natural, i -> (M2.natural)_i)
      setDiff(M2, {0, x * natGens#0})
      Anat = A.natural
      seed = (id_(M2.natural))_{1}
      S2 = dgSubmodule(M2, seed)
      xe0 = (id_(M2.natural))_{0} * matrix {{x_Anat}}
      incMat = (inclusion S2).natural
      (xe0 - incMat * (xe0 // incMat)) == 0
    Text
      In a richer setting, consider the minimal semifree resolution of the
      residue field over the complete intersection
      @ TT "R = QQ[x, y]/(x^2, y^2)" @.  Seeding the submodule with the
      hom-degree-2 natural generators, d-closure automatically pulls in the
      hom-degree-4 generators that sit below them in the Koszul-style
      differential:
    Example
      S = QQ[x, y] / ideal(x^2, y^2)
      B = koszulComplexDGA S
      Mdg = minimalSemifreeResolution(B, S^1 / ideal(x, y), EndDegree => 3)
      homDegs = apply(Mdg.Degrees, d -> d#0)
      pos2 = positions(Mdg.Degrees, d -> d#0 == 2)
      seedHi = (id_(Mdg.natural))_pos2
      Shi = dgSubmodule(Mdg, seedHi)
      apply(Shi.Degrees, d -> d#0)
      isWellDefined Shi
  Caveat
    The target of the inclusion matrix must be @ TT "M.natural" @ on the
    nose (object identity, not just isomorphism).  This catches common
    shape mistakes up front — for example, building
    @ TT "matrix {{a, b}}" @ for a rank-n ambient produces a 1×2 matrix
    whose target is rank 1, not rank n.
  SeeAlso
    "Building DG modules, submodules, and quotients"
    "Operations on DG Submodules"
    DGSubmodule
    inclusion
    (isWellDefined, DGModule)
///

doc ///
  Key
    dgQuotientModule
    (dgQuotientModule, DGModule, DGSubmodule)
  Headline
    Construct the quotient DG module M / S
  Usage
    Q = dgQuotientModule(M, S)
  Inputs
    M:DGModule
    S:DGSubmodule
      With @ TT "S.ambient === M" @.
  Outputs
    Q:DGQuotientModule
      The cokernel of @ TT "S.inclusion.natural" @ equipped with the
      induced differential.
  Description
    Text
      The shorthand @ TT "M / S" @ calls this constructor.  The resulting
      Q records the ambient M (via @ TO ambient @), the killed submodule
      S (via @ TO subDGModule @), and the quotient map M → Q (via
      @ TO projection @).
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Z = dgSubmodule(M, {})
      Q = dgQuotientModule(M, Z)
      instance(Q, DGQuotientModule)
      ambient Q === M
      subDGModule Q === Z
    Text
      Quotienting by the zero submodule gives a DGQuotientModule whose
      underlying cokernel is isomorphic to @ TT "M.natural" @; quotienting
      by the whole ambient yields the zero DGQuotientModule.
    Example
      Sfull = dgSubmodule(M, id_(M.natural))
      Qzero = M / Sfull
      isZero Qzero
  Caveat
    @ TT "S.ambient" @ must equal @ TT "M" @ as an object (===), not
    merely be isomorphic.  This enforces a strict lattice-of-submodules
    discipline on the resulting quotient.
  SeeAlso
    "Building DG modules, submodules, and quotients"
    (symbol /, DGModule, DGSubmodule)
    DGQuotientModule
    projection
    subDGModule
///

doc ///
  Key
    (symbol /, DGModule, DGSubmodule)
  Headline
    Quotient DG module: M / S
  Usage
    Q = M / S
  Inputs
    M:DGModule
    S:DGSubmodule
      With @ TT "S.ambient === M" @.
  Outputs
    Q:DGQuotientModule
  Description
    Text
      Shorthand for @ TT "dgQuotientModule(M, S)" @.  See
      @ TO dgQuotientModule @ for semantics and invariants.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Z = dgSubmodule(M, {})
      Q = M / Z
      ambient Q === M
      isWellDefined Q
    Text
      The zero and top-dimensional boundary cases are handled
      consistently: @ TT "M / 0" @ is a DGQuotientModule wrapping a
      cokernel isomorphic to M, and @ TT "M / M" @ is the zero
      DGQuotientModule.
    Example
      Sfull = dgSubmodule(M, id_(M.natural))
      isZero (M / Sfull)
  SeeAlso
    "Building DG modules, submodules, and quotients"
    dgQuotientModule
    DGQuotientModule
///

doc ///
  Key
    ambient
    (ambient, DGIdeal)
    (ambient, DGSubmodule)
    (ambient, DGQuotientModule)
    inclusion
    (inclusion, DGSubmodule)
    projection
    (projection, DGQuotientModule)
    subDGModule
    (subDGModule, DGQuotientModule)
  Headline
    Accessors for the ambient object, inclusion, projection, and killed submodule
  Description
    Text
      These accessors navigate between the DG types in the
      DGModule / DGSubmodule / DGQuotientModule hierarchy:
    Text
      @ TT "ambient" @ on a @ TO DGIdeal @ returns the enclosing
      @ TO DGAlgebra @; on a @ TO DGSubmodule @ or
      @ TO DGQuotientModule @ it returns the enclosing @ TO DGModule @.
    Text
      @ TT "inclusion" @ on a @ TO DGSubmodule @ S returns the
      @ TO DGModuleMap @ S → ambient(S).
    Text
      @ TT "projection" @ on a @ TO DGQuotientModule @ Q returns the
      @ TO DGModuleMap @ ambient(Q) → Q.
    Text
      @ TT "subDGModule" @ on a @ TO DGQuotientModule @ Q returns the
      @ TO DGSubmodule @ that was killed to form Q.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, id_(M.natural))
      Q = M / S
      ambient S === M
      ambient Q === M
      source inclusion S === S
      target inclusion S === M
      source projection Q === M
      target projection Q === Q
      subDGModule Q === S
    Text
      For a DGIdeal the analogous accessor returns the DGAlgebra:
    Example
      I = dgIdeal(A, {x_(A.natural)})
      ambient I === A
  SeeAlso
    "Building DG modules, submodules, and quotients"
    DGSubmodule
    DGQuotientModule
    DGIdeal
///

doc ///
  Key
    "Module-like operations on DG modules"
  Headline
    Basic queries (generators, rank, degrees, homogeneity) extended to DG modules and their sub- and quotient modules
  Description
    Text
      Every @ TO DGModule @ (and its @ TO DGSubmodule @ / @ TO DGQuotientModule @ variants)
      carries an underlying graded @ TT "A.natural" @-module, accessible via
      @ TT "M.natural" @.  The standard @ TO Module @ methods — @ TO numgens @,
      @ TO rank @, @ TO degrees @, @ TO isHomogeneous @, @ TO super @,
      @ TO cover @, @ TO relations @ — are lifted directly onto the DG types,
      so code written against ordinary modules can often be reused on DG
      objects unchanged.
    Text
      Two DG-specific predicates are added:
    Text
      @ TO isFreeDGModule @ — is the underlying @ TT "M.natural" @ a free
      graded @ TT "A.natural" @-module?  Every DGModule built via
      @ TO freeDGModule @ satisfies this; user-built DGModules with
      non-free @ TT ".natural" @ slots are tolerated by @ TO isWellDefined @
      as long as the differential closes.
    Text
      @ TO isZero @ — does the DG object have zero natural generators?
      Consistent across DGModule, DGSubmodule, DGQuotientModule, and
      DGIdeal, so predicates can be written generically.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1, 2})
      numgens M
      rank M
      degrees M
      isHomogeneous M
      isFreeDGModule M
      isZero M
    Text
      On quotient DG modules, @ TT "super" @ and @ TT "cover" @ both return
      the ambient DGModule, and @ TT "relations" @ returns the matrix of
      the killed inclusion — mirroring M2's cokernel encoding of a quotient.
    Example
      Anat = A.natural
      Mone = freeDGModule(A, {0})
      S = dgSubmodule(Mone, matrix {{x_Anat, y_Anat}})
      Q = Mone / S
      super Q === Mone
      cover Q === Mone
      relations Q
  Subnodes
    (numgens, DGModule)
    (rank, DGModule)
    (degrees, DGModule)
    (isHomogeneous, DGModule)
    isFreeDGModule
    (isZero, DGModule)
    (super, DGSubmodule)
  SeeAlso
    "Operations on DG Submodules"
    "Operations on DG Ideals"
    DGModule
///

doc ///
  Key
    (numgens, DGModule)
    (numgens, DGSubmodule)
    (numgens, DGQuotientModule)
  Headline
    Number of natural generators of a DG module (or sub- or quotient module)
  Usage
    n = numgens M
  Inputs
    M:
      A DGModule, DGSubmodule, or DGQuotientModule.
  Outputs
    n:ZZ
      The number of natural generators of the underlying graded @ TT "A.natural" @-module,
      equal to the length of its list of degrees.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1, 2})
      numgens M == 3
    Text
      For a DGSubmodule the count is the number of columns of the inclusion
      matrix (= number of generators being included).  For a DGQuotientModule
      it is the number of natural generators of the quotient presentation,
      which by construction equals @ TT "numgens (cover Q).natural" @.
    Example
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
      numgens S == 2
      Q = M1 / S
      numgens Q == numgens M1
    Text
      Boundary case: a DGModule freely generated on an empty list has zero
      generators and is @ TO isZero @.
    Example
      M0 = freeDGModule(A, {})
      numgens M0 == 0
      isZero M0
  SeeAlso
    "Module-like operations on DG modules"
    (rank, DGModule)
    (degrees, DGModule)
    isZero
///

doc ///
  Key
    (rank, DGModule)
    (rank, DGSubmodule)
  Headline
    Rank of the underlying free A.natural-module
  Usage
    r = rank M
  Inputs
    M:
      A DGModule or DGSubmodule whose @ TT ".natural" @ is free.
  Outputs
    r:ZZ
      @ TT "rank M.natural" @.
  Description
    Text
      For a free DGModule, @ TT "rank" @ and @ TO numgens @ coincide.
      For a DGSubmodule, @ TT "rank" @ reports the rank of the included
      free module (which is @ TT "numgens S" @ when the inclusion is
      injective on natural generators — always the case for submodules
      built by @ TO dgSubmodule @).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      rank M == numgens M
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
      rank S
    Text
      After pruning a DGSubmodule with redundant inclusions, @ TT "rank" @
      equals @ TT "numgens" @ on the pruned object.
    Example
      Sbig = dgSubmodule(M1, matrix {{1_Anat, x_Anat, y_Anat}})
      Sp = prune Sbig
      numgens Sp == rank Sp
  Caveat
    @ TT "rank" @ is not defined on @ TO DGQuotientModule @: the rank of a
    cokernel module triggers codim computations that fail over non-affine
    graded ambients.  Use @ TO numgens @ for the generator count.
  SeeAlso
    "Module-like operations on DG modules"
    (numgens, DGModule)
///

doc ///
  Key
    (degrees, DGModule)
    (degrees, DGSubmodule)
    (degrees, DGQuotientModule)
  Headline
    Multi-degrees of the natural generators of a DG module
  Usage
    ds = degrees M
  Inputs
    M:
      A DGModule, DGSubmodule, or DGQuotientModule.
  Outputs
    ds:List
      A list of multi-degrees, one per natural generator.  Each multi-degree
      is a list whose first entry is the homological degree and whose
      remaining entries are the internal degrees.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1, 2})
      degrees M == {{0,0},{1,0},{2,0}}
    Text
      Internal degrees come from the ambient ring's grading and are
      concatenated onto the homological degree.
    Example
      Rg = ZZ/101[x, y, Degrees => {{1,0}, {0,1}}]
      Ag = koszulComplexDGA Rg
      Mg = freeDGModule(Ag, {{0,0,0}, {1,0,0}})
      degrees Mg
  SeeAlso
    "Module-like operations on DG modules"
    (numgens, DGModule)
    (isHomogeneous, DGModule)
///

doc ///
  Key
    (isHomogeneous, DGModule)
    (isHomogeneous, DGSubmodule)
    (isHomogeneous, DGQuotientModule)
  Headline
    Test whether the underlying graded module is homogeneous
  Usage
    b = isHomogeneous M
  Inputs
    M:
      A DGModule, DGSubmodule, or DGQuotientModule.
  Outputs
    b:Boolean
      @ TT "isHomogeneous M.natural" @.
  Description
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 2})
      isHomogeneous M
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat, y_Anat}})
      isHomogeneous S
      Q = M1 / S
      isHomogeneous Q
  SeeAlso
    "Module-like operations on DG modules"
    (degrees, DGModule)
///

doc ///
  Key
    isFreeDGModule
    (isFreeDGModule, DGModule)
    (isFreeDGModule, DGSubmodule)
  Headline
    Is the underlying A.natural-module free?
  Usage
    b = isFreeDGModule M
  Inputs
    M:
      A DGModule or DGSubmodule.
  Outputs
    b:Boolean
      @ TT "isFreeModule M.natural" @.
  Description
    Text
      Every DGModule built via @ TO freeDGModule @ (and every DGSubmodule of
      one) has free @ TT ".natural" @, so this returns true in the typical
      case.  The check exists because it is possible to build a DGModule
      directly whose underlying @ TT ".natural" @ module is non-free, and @ TO isWellDefined @ will
      tolerate such an object as long as the differential closes.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 2})
      isFreeDGModule M
      Anat = A.natural
      M1 = freeDGModule(A, {0})
      S = dgSubmodule(M1, matrix {{x_Anat}})
      isFreeDGModule S
    Text
      Tensor products, base changes, and direct sums constructed by this
      package all land in the free case, so @ TT "isFreeDGModule" @ remains
      true after any of those operations.
    Example
      MNK = (M ** R^2) ** R^2
      isFreeDGModule MNK
  SeeAlso
    "Module-like operations on DG modules"
    freeDGModule
    (isWellDefined, DGModule)
///

doc ///
  Key
    (isZero, DGModule)
    (isZero, DGSubmodule)
    (isZero, DGQuotientModule)
    isZero
  Headline
    Does the DG object have no natural generators?
  Usage
    b = isZero X
  Inputs
    X:
      A DGModule, DGSubmodule, DGQuotientModule, or @ TO DGIdeal @.
  Outputs
    b:Boolean
      True iff the natural presentation is zero.  Concretely:
      @ TT "numgens M.natural == 0" @ for a DGModule, no inclusion columns
      (or a zero inclusion matrix) for a DGSubmodule, and
      @ TT "Q.natural == 0" @ for a DGQuotientModule.  For a DGIdeal,
      see @ TO (isZero, DGIdeal) @.
  Description
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M0 = freeDGModule(A, {})
      isZero M0
      M = freeDGModule(A, {0})
      not isZero M
    Text
      A zero DGSubmodule can be built explicitly via a zero inclusion matrix.
    Example
      Anat = A.natural
      Z = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      isZero Z
    Text
      Quotienting a DGModule by its whole ambient yields a zero
      DGQuotientModule.
    Example
      Full = dgSubmodule(M, id_(M.natural))
      Q = M / Full
      isZero Q
  SeeAlso
    "Module-like operations on DG modules"
    (numgens, DGModule)
    (isZero, DGIdeal)
///

doc ///
  Key
    (super, DGSubmodule)
    (super, DGQuotientModule)
    (cover, DGQuotientModule)
    (relations, DGQuotientModule)
  Headline
    Ambient DGModule, cover, and relation matrix of a DG sub- or quotient module
  Description
    Text
      These mirror the analogous M2 Module accessors.
    Text
      @ TT "super S" @ (for a DGSubmodule S) and @ TT "super Q, cover Q" @
      (for a DGQuotientModule Q) all return the ambient DGModule M from
      which the sub- or quotient was built.
    Text
      @ TT "relations Q" @ returns the inclusion matrix of the killed
      submodule, encoding the cokernel presentation of Q.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      super S === M
      Q = M / S
      super Q === M
      cover Q === M
      numcols relations Q == 2
  SeeAlso
    "Module-like operations on DG modules"
    DGSubmodule
    DGQuotientModule
///

doc ///
  Key
    "Operations on DG Submodules"
  Headline
    Sum, intersection, equality, and containment of DG submodules
  Description
    Text
      DGSubmodules of a common ambient DGModule M form a lattice under sum,
      intersection, and inclusion — each operation preserves d-closure by
      elementary arguments paralleling the @ TO "Operations on DG Ideals" @
      story:
    Text
      @BOLD "Sum"@: @ TT "d(s + t) = d(s) + d(t) \\in S + T" @ when
      @ TT "s \\in S" @ and @ TT "t \\in T" @.
    Text
      @BOLD "Intersection"@: if @ TT "x \\in S \\cap T" @ then
      @ TT "d(x) \\in S" @ (by d-closure of S) and @ TT "d(x) \\in T" @
      (by d-closure of T), so @ TT "d(x) \\in S \\cap T" @.
    Text
      @BOLD "Containment / equality"@: read off directly from the images of
      the inclusions in @ TT "M.natural" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      T = dgSubmodule(M, matrix {{y_Anat}})
      ST = S + T
      isWellDefined ST
      isSubset(S, ST) and isSubset(T, ST)
      cap = intersect(S, T)
      isWellDefined cap
      isSubset(cap, S) and isSubset(cap, T)
    Text
      Sum is idempotent (@ TT "S + S == S" @) and intersecting with the
      zero submodule yields the zero submodule.
    Example
      (S + S) == S
      Z = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      isZero intersect(S, Z)
    Text
      All four operations require the DGSubmodules to share an ambient
      DGModule (via @ TT "===" @ — object identity, not just isomorphism),
      and raise an error otherwise.
  Subnodes
    (symbol +, DGSubmodule, DGSubmodule)
    (intersect, DGSubmodule, DGSubmodule)
    (symbol ==, DGSubmodule, DGSubmodule)
  SeeAlso
    DGSubmodule
    "Operations on DG Ideals"
    "Module-like operations on DG modules"
///

doc ///
  Key
    (symbol +, DGSubmodule, DGSubmodule)
  Headline
    Sum of two DG submodules of a common ambient
  Usage
    U = S + T
  Inputs
    S:DGSubmodule
    T:DGSubmodule
      With @ TT "S.ambient === T.ambient" @.
  Outputs
    U:DGSubmodule
      The DGSubmodule of @ TT "S.ambient" @ whose inclusion matrix is the
      horizontal concatenation of @ TT "S.inclusion.natural" @ and
      @ TT "T.inclusion.natural" @.
  Description
    Text
      Preserves d-closure and contains both summands.  Idempotent:
      @ TT "S + S == S" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      T = dgSubmodule(M, matrix {{y_Anat}})
      ST = S + T
      isWellDefined ST
      isSubset(S, ST) and isSubset(T, ST)
      (S + S) == S
  Caveat
    If the two ambients are distinct objects (even if isomorphic), this
    raises an error — the operation is about lattice structure within a
    fixed ambient.
  SeeAlso
    "Operations on DG Submodules"
    (intersect, DGSubmodule, DGSubmodule)
    (isSubset, DGSubmodule, DGSubmodule)
///

doc ///
  Key
    (intersect, DGSubmodule, DGSubmodule)
  Headline
    Intersection of two DG submodules of a common ambient
  Usage
    U = intersect(S, T)
  Inputs
    S:DGSubmodule
    T:DGSubmodule
      With @ TT "S.ambient === T.ambient" @.
  Outputs
    U:DGSubmodule
      The DGSubmodule of @ TT "S.ambient" @ whose image is
      @ TT "image(S.inclusion.natural) \\cap image(T.inclusion.natural)" @.
  Description
    Text
      Preserves d-closure directly.  Intersecting with the zero submodule
      yields the zero submodule; intersecting with the same submodule is
      idempotent (@ TT "intersect(S, S) == S" @ as submodules).
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      T = dgSubmodule(M, matrix {{x_Anat * y_Anat}})
      cap = intersect(S, T)
      isWellDefined cap
      isSubset(cap, S) and isSubset(cap, T)
    Text
      Boundary case: intersection with the zero submodule is zero.
    Example
      Z = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      isZero intersect(S, Z)
  SeeAlso
    "Operations on DG Submodules"
    (symbol +, DGSubmodule, DGSubmodule)
///

doc ///
  Key
    (symbol ==, DGSubmodule, DGSubmodule)
    (isSubset, DGSubmodule, DGSubmodule)
  Headline
    Equality and containment of DG submodules
  Usage
    b = S == T
    b = isSubset(S, T)
  Inputs
    S:DGSubmodule
    T:DGSubmodule
  Outputs
    b:Boolean
  Description
    Text
      Equality requires the ambients to be the same object
      (@ TT "===" @) and the images of the inclusions to agree.
      Containment further requires the DGSubmodules share an ambient and
      tests the image inclusions.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      Anat = A.natural
      S = dgSubmodule(M, matrix {{x_Anat}})
      U = dgSubmodule(M, matrix {{x_Anat, y_Anat}})
      isSubset(S, U)
      not isSubset(U, S)
      S == S
    Text
      Two DGSubmodules built from distinct ambient DGModules are never
      equal, even if the ambient modules are abstractly isomorphic.
    Example
      M' = freeDGModule(A, {0})
      S' = dgSubmodule(M', matrix {{x_Anat}})
      not (S == S')
  SeeAlso
    "Operations on DG Submodules"
    (symbol +, DGSubmodule, DGSubmodule)
    (intersect, DGSubmodule, DGSubmodule)
///

doc ///
  Key
    "Image, kernel, and cokernel of DG module maps"
  Headline
    Building DG submodules and DG quotient modules from a DG module map
  Description
    Text
      For a DG module map @ TT "f : M -> N" @ of DG modules over a common
      DG algebra A, the package provides three chain-level constructions:
    Text
      @ TO (image, DGModuleMap) @ — the image of f, built as a
      @ TO DGSubmodule @ of the target N.  A chain map sends cycles to
      cycles and boundaries to boundaries, so the column span of
      @ TT "f.natural" @ is automatically d-closed in @ TT "N.natural" @;
      no d-saturation is needed.
    Text
      @ TO (kernel, DGModuleMap) @ — the kernel of f, built as a
      @ TO DGSubmodule @ of the source M.  Again chain-map-ness gives
      d-closure for free.
    Text
      @ TO (cokernel, DGModuleMap) @ — the cokernel of f, built as a
      @ TO DGQuotientModule @ equal to @ TT "target f / image f" @.
    Text
      These three operations fit together in the usual short exact
      sequence: for any f, we have
      @ TT "rank(source f) == numgens(kernel f) + numgens(image f)" @
      and @ TT "rank(target f) == numgens(image f) + numgens(cokernel f)" @
      whenever the natural modules are free.
    Text
      A genuinely informative example: let
      @ TT "R = k[x, y]/(x^2, y^2)" @ and let
      @ TT "KM = koszulComplexDGM R^1" @ be its Koszul DG module.
      Multiplication by @ TT "x" @ is a degree-0 endomorphism of
      @ TT "KM" @ because @ TT "x" @ is central in the underlying
      ring:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      isWellDefined mx
    Text
      Since @ TT "x^2 = 0" @ in @ TT "R" @, the composition
      @ TT "mx o mx" @ is the zero map, which chain-level is the
      statement @ TT "image mx \\subseteq kernel mx" @.  The three
      constructions expose this directly:
    Example
      imgMx = image mx;
      kerMx = kernel mx;
      cokMx = cokernel mx;
      all({imgMx, kerMx, cokMx}, isWellDefined)
      numcols (inclusion imgMx).natural
      numcols (inclusion kerMx).natural
      isSubset(imgMx, kerMx)
    Text
      The cokernel is the quotient DG module
      @ TT "KM / x \\cdot KM" @; at the natural level it is
      @ TT "KM \\otimes_R R/(x) = k[y]/(y^2) \\otimes_k \\Lambda(T_1, T_2)" @,
      presented by the @ TT "1 x 1" @ matrix @ TT "[x]" @ per
      generator:
    Example
      presentation cokMx.natural
    Text
      The three operations are used throughout the package -- notably
      by @ TO2((homology, DGModuleMap, ZZ), "homology at degree n") @,
      which computes @ TT "kernel(d_n) / image(d_{n+1})" @ via these
      primitives.
    Text
      Finally, each of @ TT "image mx" @, @ TT "kernel mx" @,
      @ TT "cokernel mx" @ is itself a DG module, so
      @ TO (toComplex, DGModule) @ (or its
      @ TO DGSubmodule @ / @ TO DGQuotientModule @ variants) converts
      them into ordinary chain complexes one can hand to
      @ TO "Complexes::Complexes" @.  For the cokernel in particular,
      the underlying complex is @ TT "KM / x \\cdot KM" @ and its
      homology picks up the classes that become new cycles when the
      @ TT "x" @-direction is killed:
    Example
      CcokMx = toComplex cokMx
      apply(0..(maxDegree cokMx), n -> prune HH_n CcokMx)
  Subnodes
    (image, DGModuleMap)
    (kernel, DGModuleMap)
    (cokernel, DGModuleMap)
  SeeAlso
    DGModuleMap
    "Building DG modules, submodules, and quotients"
    "Basic operations on DG Module Maps"
///

doc ///
  Key
    (image, DGModuleMap)
  Headline
    Image of a DG module map as a DG submodule of its target
  Usage
    S = image f
  Inputs
    f:DGModuleMap
      A morphism @ TT "M -> N" @ of DG modules over a common DG algebra.
  Outputs
    S:DGSubmodule
      The DG submodule of @ TT "N = target f" @ whose column span in
      @ TT "N.natural" @ is the image of @ TT "f.natural" @.  Zero
      columns of @ TT "f.natural" @ are dropped, so the number of
      generators of the result reflects the rank of the image rather
      than the width of the presentation.
  Description
    Text
      Since f is a chain map, the column span of @ TT "f.natural" @ is
      already d-closed in @ TT "N.natural" @; @ TO dgSubmodule @ is
      called without a d-saturation loop.
    Text
      A substantive example: over
      @ TT "R = k[x, y]/(x^2, y^2)" @ take the Koszul DG module
      @ TT "KM = koszulComplexDGM R^1" @ and the endomorphism
      "multiplication by @ TT "x" @."  Its image is a proper
      DG submodule -- it cannot be all of @ TT "KM" @ because
      @ TT "x" @ annihilates the class of @ TT "1 \\in R" @ modulo
      its own image (since @ TT "x^2 = 0" @):
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      imgMx = image mx
      isWellDefined imgMx
      numcols (inclusion imgMx).natural
      isSubset(imgMx, dgSubmodule(KM, id_(KM.natural)))
    Text
      The image of the inclusion of a DG submodule
      @ TT "S \\subset M" @ recovers @ TT "S" @ itself:
    Example
      Sfull = dgSubmodule(KM, id_(KM.natural));
      (image (inclusion Sfull)) == Sfull
  SeeAlso
    "Image, kernel, and cokernel of DG module maps"
    (kernel, DGModuleMap)
    (cokernel, DGModuleMap)
    DGSubmodule
///

doc ///
  Key
    (kernel, DGModuleMap)
  Headline
    Kernel of a DG module map as a DG submodule of its source
  Usage
    S = kernel f
  Inputs
    f:DGModuleMap
      A morphism @ TT "M -> N" @ of DG modules over a common DG algebra.
  Outputs
    S:DGSubmodule
      The DG submodule of @ TT "M = source f" @ generated by the kernel
      of @ TT "f.natural" @ as a module over @ TT "M.dgAlgebra.natural" @.
  Description
    Text
      A chain map sends cycles to cycles, so the kernel of
      @ TT "f.natural" @ at the module level is already d-closed.  The
      constructor takes the natural-level kernel generators and wraps
      them with @ TO dgSubmodule @.
    Text
      Mathematically interesting example: over
      @ TT "R = k[x, y]/(x^2, y^2)" @ the endomorphism
      "multiplication by @ TT "x" @" of the Koszul DG module
      @ TT "KM" @ squares to zero, so its kernel contains its image:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      kerMx = kernel mx
      imgMx = image mx
      isWellDefined kerMx
      numcols (inclusion kerMx).natural
      isSubset(imgMx, kerMx)
    Text
      The inclusion of a DG submodule is injective, so its kernel is
      zero:
    Example
      Sfull = dgSubmodule(KM, id_(KM.natural));
      isZero kernel (inclusion Sfull)
  SeeAlso
    "Image, kernel, and cokernel of DG module maps"
    (image, DGModuleMap)
    (cokernel, DGModuleMap)
    DGSubmodule
///

doc ///
  Key
    (cokernel, DGModuleMap)
  Headline
    Cokernel of a DG module map as a DG quotient module of its target
  Usage
    Q = cokernel f
  Inputs
    f:DGModuleMap
      A morphism @ TT "M -> N" @ of DG modules over a common DG algebra.
  Outputs
    Q:DGQuotientModule
      The DG quotient module @ TT "N / image(f)" @ with the induced
      differential.
  Description
    Text
      Equivalently, this is @ TT "dgQuotientModule(target f, image f)" @.
      The @ TT "coker" @ spelling is an alias for @ TT "cokernel" @ and
      invokes the same method.
    Text
      A nontrivial example: over @ TT "R = k[x, y]/(x^2, y^2)" @,
      multiplication by @ TT "x" @ on the Koszul DG module
      @ TT "KM" @ has cokernel equal to @ TT "KM / x \\cdot KM" @,
      which at the natural level is @ TT "KM \\otimes_R R/(x)" @ --
      the base change of @ TT "KM" @ mod @ TT "x" @:
    Example
      R = ZZ/101[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      mx = dgModuleMap(KM, KM, x * id_(KM.natural))
      Q = cokernel mx
      isWellDefined Q
      ambient Q === KM
      presentation Q.natural
    Text
      The induced differential on @ TT "Q" @ is well-defined because
      @ TT "mx" @ commutes with the differential, and it can be
      realized as an ordinary chain complex via
      @ TO (toComplex, DGQuotientModule) @:
    Example
      C = toComplex Q
      apply(0..(maxDegree Q), n -> prune HH_n C)
    Text
      For a DG submodule inclusion @ TT "S \\hookrightarrow M" @, the
      cokernel is exactly the DG quotient module @ TT "M / S" @:
    Example
      Sfull = dgSubmodule(KM, id_(KM.natural));
      isZero cokernel (inclusion Sfull)
  SeeAlso
    "Image, kernel, and cokernel of DG module maps"
    (image, DGModuleMap)
    (kernel, DGModuleMap)
    DGQuotientModule
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    "Homology of DG modules and DG module maps"
  Headline
    Computing H_n and H_* for DG modules, DG submodules, DG quotient modules, and DG module maps
  Description
    Text
      For a DG module @ TT "M" @ over a DG algebra @ TT "A" @, the package
      supports two parallel views of homology:
    Text
      @ TO (homology, ZZ, DGModule) @ returns a single degree
      @ TT "H_n(M)" @ as a module over the base ring @ TT "A.ring" @.
      This is the natural per-degree shape and matches the convention of
      @ TO (homology, ZZ, DGAlgebra) @.
    Text
      @ TO (homology, DGModule) @ (equivalently @ TO homologyModule @)
      returns the full graded object @ TT "H_*(M)" @ assembled as a
      module over the homology algebra @ TT "HH(A)" @, with the
      @ TT "HH(A)" @-action coming from multiplication by cycle classes.
    Text
      Maps @ TT "f : M -> N" @ of DG modules induce maps on homology at
      each level: @ TO (homology, DGModuleMap, ZZ) @ returns the induced
      map @ TT "H_n(M) -> H_n(N)" @ as a map of @ TT "A.ring" @-modules,
      and @ TO (homology, DGModuleMap) @ assembles those pieces into a
      map of @ TT "HH(A)" @-modules.
    Text
      For quotients, @ TO (homology, ZZ, DGQuotientModule) @ and
      @ TO (homology, DGQuotientModule) @ (equivalently
      @ TO (homologyModule, DGQuotientModule) @) work the same way,
      using @ TO toComplex @ to reduce to an
      underlying complex.  A @ TO DGSubmodule @ is a @ TO DGModule @,
      so its homology is computed by the DG module methods directly.
    Text
      To extract an explicit representative of a given homology class,
      use @ TO (homologyClass, DGModule, Vector) @; given a cycle in
      @ TT "M.natural" @ it returns the corresponding element of
      @ TT "prune homology(n, M)" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      M = minimalSemifreeResolution(A, k, EndDegree => 2)
      H0 = prune homology(0, M)
      numgens H0 == 1
      idM = identityDGModuleMap M
      h0 = homology(idM, 0)
      h0 == id_(source h0)
    Text
      The identity on @ TT "M" @ induces the identity on every
      @ TT "H_n(M)" @, a baseline sanity check that each of the pieces
      fits together.
  Subnodes
    (homology, ZZ, DGModule)
    (homology, DGModule)
    (homology, DGModuleMap, ZZ)
    (homology, ZZ, DGQuotientModule)
    (homology, DGQuotientModule)
    (homologyClass, DGModule, Vector)
  SeeAlso
    (homology, DGModuleMap)
    (homology, ZZ, DGAlgebra)
    (homology, DGAlgebra)
    homologyModule
    homologyClass
///

doc ///
  Key
    (homology, ZZ, DGModule)
  Headline
    The degree-n homology of a DG module as a module over the base ring
  Usage
    H = homology(n, M)
  Inputs
    n:ZZ
      A homological degree.
    M:DGModule
      A DG module over a DG algebra @ TT "A" @.
  Outputs
    H:Module
      The module @ TT "H_n(M) = ker(d_n) / image(d_{n+1})" @, presented
      as a module over @ TT "A.ring" @.
  Description
    Text
      This is the per-degree homology: the differential
      @ TT "d_n : M_n -> M_{n-1}" @ is built via
      @ TT "moduleDifferential" @, and @ TT "H_n(M)" @ is obtained by
      applying the built-in M2 @ TT "homology" @ method to the adjacent
      pair @ TT "(d_n, d_{n+1})" @.  The resulting module lives over
      @ TT "A.ring" @; use @ TO (prune, Module) @ to minimize the
      presentation.
    Text
      Because @ TO DGSubmodule @ is a subtype of @ TO DGModule @, this
      method also handles DG submodules directly.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      prune homology(0, KM)
      prune homology(1, KM) == 0
      prune homology(2, KM) == 0
    Text
      For the Koszul complex on a regular sequence, only
      @ TT "H_0 = R/(x,y,z)" @ is nonzero; this recovers the standard
      fact.
    Example
      R2 = ZZ/101[x]
      A2 = koszulComplexDGA R2
      M = freeDGModule(A2, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      numgens prune homology(0, M)
      numgens prune homology(1, M)
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, DGModule)
    (homology, DGModuleMap, ZZ)
    (homology, ZZ, DGQuotientModule)
    (homology, ZZ, DGAlgebra)
    moduleDifferential
///

doc ///
  Key
    (homology, DGModule)
    (homologyModule, DGModule)
  Headline
    The graded homology of a DG module as a module over HH(A)
  Usage
    HM = homology M
    HM = homologyModule M
  Inputs
    M:DGModule
      A DG module over a DG algebra @ TT "A" @.
  Outputs
    HM:Module
      The graded homology @ TT "H_*(M)" @ packaged as a module over the
      homology algebra @ TT "HH(A)" @.  The @ TT "HH(A)" @-action comes
      from multiplication by cycle classes of @ TT "A" @ on @ TT "M" @.
  Description
    Text
      The two names @ TT "homology M" @ and @ TT "homologyModule M" @
      produce the same object and are provided as aliases, matching the
      analogous convention for DG algebras in which
      @ TO (homology, DGAlgebra) @ returns @ TO homologyAlgebra @.
    Text
      For per-degree homology as an @ TT "A.ring" @-module, use
      @ TO (homology, ZZ, DGModule) @.
    Text
      A fast path is taken when @ TT "M" @ came from
      @ TO koszulComplexDGM @ and has vanishing generator differentials:
      in that case the result is computed directly from
      @ TT "M.module" @ via @ TO (homologyModule, DGAlgebra, Module) @.
      Otherwise the method builds the @ TT "HH(A)" @-module presentation
      degree by degree, using the action of each cycle class of
      @ TT "A" @ on @ TT "M" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      M = minimalSemifreeResolution(A, k, EndDegree => 2)
      HM = homology M
      ring HM === HH A
      HM' = homologyModule M
      HM == HM'
    Text
      The ring of the result is @ TT "HH(A)" @, the homology algebra of
      the underlying DG algebra.
  Caveat
    The general path currently requires @ TT "M.natural" @ to be a free
    @ TT "A.natural" @-module (or @ TT "M" @ to be a
    @ TO koszulComplexDGM @ with zero generator differentials).
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, ZZ, DGModule)
    (homology, DGAlgebra)
    homologyModule
    homologyAlgebra
///

doc ///
  Key
    (homology, DGModuleMap, ZZ)
  Headline
    The induced map on degree-n homology of a DG module map
  Usage
    h = homology(f, n)
  Inputs
    f:DGModuleMap
      A map @ TT "M -> N" @ of DG modules over a common DG algebra
      @ TT "A" @.
    n:ZZ
      A homological degree.
  Outputs
    h:Matrix
      The induced map @ TT "H_n(f) : H_n(M) -> H_n(N)" @, presented as
      a morphism of @ TT "A.ring" @-modules.
  Description
    Text
      Computed via @ TO inducedMap @ from the chain map
      @ TT "toComplexMap(f, n)" @ applied to the pair of per-degree
      homology modules.  Since a chain map sends cycles to cycles and
      boundaries to boundaries, the induced map is well defined.
    Text
      The identity DG module map induces the identity on each
      @ TT "H_n" @; the zero DG module map induces the zero map on each
      @ TT "H_n" @.  Composition respects homology:
      @ TT "homology(g * f, n) == homology(g, n) * homology(f, n)" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      k = R^1 / ideal(x, y)
      M = minimalSemifreeResolution(A, k, EndDegree => 3)
      idM = identityDGModuleMap M
      zM = zeroDGModuleMap(M, M)
      h0id = homology(idM, 0)
      h0id == id_(source h0id)
      h0z = homology(zM, 0)
      h0z == map(target h0z, source h0z, 0)
    Text
      Multiplication by @ TT "x" @ on a minimal semifree resolution of
      @ TT "k = R/(x, y)" @ induces the zero map on @ TT "H_0 = k" @,
      because @ TT "x" @ kills @ TT "k" @.
    Example
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      xMap = dgModuleMap(M, M, apply(natGens, g -> x * g))
      h0x = homology(xMap, 0)
      h0x == map(target h0x, source h0x, 0)
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, DGModuleMap)
    (homology, ZZ, DGModule)
    toComplexMap
///

doc ///
  Key
    (homology, ZZ, DGQuotientModule)
  Headline
    The degree-n homology of a DG quotient module
  Usage
    H = homology(n, Q)
  Inputs
    n:ZZ
      A homological degree.
    Q:DGQuotientModule
      A DG quotient module @ TT "M / S" @ over a DG algebra @ TT "A" @.
  Outputs
    H:Module
      The module @ TT "H_n(Q)" @, presented as a module over
      @ TT "A.ring" @.
  Description
    Text
      Implemented via @ TT "HH_n (toComplex Q)" @.  Since the underlying
      natural module is a cokernel rather than a free module, the
      differentials of @ TT "Q" @ are expressed through
      @ TO toComplex @ and the standard M2
      @ TT "HH_n" @ is applied.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      h0 = prune homology(0, Q)
      h1 = prune homology(1, Q)
      h2 = prune homology(2, Q)
      numgens h0 == 1
      numgens h1 == 1
      h2 == 0
    Text
      With @ TT "S" @ acyclic and @ TT "H_0(M) = R/x" @, the quotient
      @ TT "Q = M/S" @ has @ TT "H_0(Q) = R/x" @ and
      @ TT "H_1(Q) = R/x" @, both equal to the residue field
      @ TT "k = ZZ/101" @ (one generator each, torsion), and
      @ TT "H_2(Q) = 0" @.
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, DGQuotientModule)
    (homology, ZZ, DGModule)
    (toComplex, DGQuotientModule)
///

doc ///
  Key
    (homology, DGQuotientModule)
    (homologyModule, DGQuotientModule)
  Headline
    The graded homology of a DG quotient module
  Usage
    HQ = homology Q
    HQ = homologyModule Q
  Inputs
    Q:DGQuotientModule
      A DG quotient module @ TT "M / S" @ over a DG algebra @ TT "A" @.
  Outputs
    HQ:Module
      The graded homology @ TT "H_*(Q)" @ packaged as a module over
      @ TT "HH(A)" @.
  Description
    Text
      Implemented by taking @ TO toComplex @ and
      assembling the per-degree homologies
      @ TT "HH_i(toComplex Q)" @ into a direct sum, then tensoring each
      piece with @ TT "HH(A)" @ along @ TT "A.ring -> HH(A)" @.
      The two names @ TT "homology Q" @ and @ TT "homologyModule Q" @
      are aliases, matching @ TO (homology, DGModule) @.
    Text
      Because @ TT "Q.natural" @ is a cokernel rather than a free
      @ TT "A.natural" @-module, the general cycle-action construction
      used for @ TO (homologyModule, DGModule) @ does not apply; the
      result captures the R-module structure and the @ TT "HH(A)" @
      action coming from @ TT "A.ring -> HH(A)" @ only.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      HQ = homology Q
      ring HQ === HH A
      HQ' = homologyModule Q
      HQ == HQ'
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homology, ZZ, DGQuotientModule)
    (homology, DGModule)
    (toComplex, DGQuotientModule)
    homologyModule
///

doc ///
  Key
    (homologyClass, DGModule, Vector)
  Headline
    The homology class of a cycle in a DG module
  Usage
    h = homologyClass(M, z)
  Inputs
    M:DGModule
      A free DG module over a DG algebra @ TT "A" @.
    z:Vector
      A cycle in @ TT "M.natural" @; that is, an element @ TT "z" @ of
      homological degree @ TT "d" @ with @ TT "diff(M, z) == 0" @.
  Outputs
    h:
      The corresponding element of @ TT "prune homology(d, M)" @.
      Boundaries go to zero; the zero vector returns
      @ TT "0_(prune homology(0, M))" @ by convention.
  Description
    Text
      This is the DG module analogue of
      @ TO (homologyClass, DGAlgebra, RingElement) @.  It expresses the
      cycle @ TT "z" @ in the basis of @ TT "prune homology(d, M)" @ by
      solving @ TT "zCol = pruneMat * hCoef + boundaryMat * t" @, where
      @ TT "pruneMat" @ is the pruning map of @ TT "H_d(M)" @ and
      @ TT "boundaryMat" @ is the degree-@ TT "d+1" @ differential.
    Text
      An input that is not a cycle raises an error.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      z = natGens#0
      H = prune homology(0, M)
      h = homologyClass(M, z)
      h != 0_H
    Text
      On a boundary, @ TT "homologyClass" @ returns zero.
    Example
      b = x * natGens#0
      hb = homologyClass(M, b)
      hb == 0_(prune homology(0, M))
  Caveat
    Requires @ TT "M" @ to be a free DG module.
  SeeAlso
    "Homology of DG modules and DG module maps"
    (homologyClass, DGAlgebra, RingElement)
    (homology, ZZ, DGModule)
///

doc ///
  Key
    "Pruning DG modules, submodules, quotients, and maps"
  Headline
    Minimizing presentations of DG types via prune and minimalPresentation
  Description
    Text
      For each DG type @ TT "T" @ (DG algebra, DG module, DG submodule,
      DG quotient module, DG ideal, DG algebra map, DG module map), the
      package provides @ TT "prune" @ and its synonym
      @ TT "minimalPresentation" @.  The result is an object of the
      same type whose underlying presentation has had redundancy
      removed, where possible.
    Text
      Every call to @ TT "prune" @ caches a @ TT "pruningMap" @ in the
      @ TT "cache" @ table of the pruned output, encoding the canonical
      comparison map @ TT "(pruned) -> (original)" @.  For types where
      pruning does nothing (see below), the cached pruningMap is the
      identity; for types where it does something, the pruningMap is a
      well-defined @ TO DGModuleMap @ (or @ TO DGAlgebraMap @) whose
      source is the pruned object and whose target is the original.
      This mirrors the convention used by
      @ TT "prune" @ applied to @ TT "Complex" @ objects from the Complexes package for @ TT "Complex" @ objects.
    Text
      The two active cases are:
    Text
      @ TO (prune, DGSubmodule) @ trims the inclusion matrix to a
      minimal generating set of its column span.  When the inclusion
      matrix is already minimal, @ TT "prune" @ is the identity.
    Text
      @ TO (prune, DGQuotientModule) @ first prunes the relations
      submodule and then additionally prunes
      @ TT "toComplex Qp" @ at the complex level, caching the result so
      that @ TT "toComplex Qp" @ returns a minimally presented complex.
    Text
      The no-op cases, installed so that pruning always commutes with
      every constructor, are @ TO (prune, DGModule) @,
      @ TO (prune, DGAlgebra) @, @ TO (prune, DGAlgebraMap) @,
      @ TO (prune, DGModuleMap) @, and (already documented)
      @ TO (prune, DGIdeal) @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
      Sp = prune S
      numcols (inclusion S).natural
      numcols (inclusion Sp).natural
      image (inclusion S).natural == image (inclusion Sp).natural
    Text
      The image submodule of @ TT "M.natural" @ is unchanged; the
      presentation of @ TT "S" @ is reduced from three generators to
      one.
  Subnodes
    (prune, DGSubmodule)
    (prune, DGQuotientModule)
    (prune, DGModule)
    (prune, DGAlgebra)
    (prune, DGAlgebraMap)
    (prune, DGModuleMap)
  SeeAlso
    (prune, DGIdeal)
    (mingens, DGIdeal)
///

doc ///
  Key
    (prune, DGSubmodule)
    (minimalPresentation, DGSubmodule)
  Headline
    Trim a DG submodule to a minimal generating set of its inclusion
  Usage
    Sp = prune S
    Sp = minimalPresentation S
  Inputs
    S:DGSubmodule
  Outputs
    Sp:DGSubmodule
      A DG submodule of the same ambient DG module whose inclusion
      matrix has had @ TT "A.natural" @-linearly redundant columns
      removed.
  Description
    Text
      @ TT "prune S" @ computes
      @ TT "mingens image S.inclusion.natural" @ and builds a new
      DG submodule from that reduced inclusion matrix.  The
      @ TT "A.natural" @-span of the new inclusion matrix equals the
      original: @ TT "image Sp.inclusion.natural == image S.inclusion.natural" @.
    Text
      A @ TT "pruningMap" @ is cached in @ TT "Sp.cache" @ as a
      @ TO DGModuleMap @ from @ TT "Sp" @ to @ TT "S" @; when the
      inclusion matrix is already minimal this is the identity on
      @ TT "S" @ (and @ TT "prune S === S" @).
    Text
      Well-definedness (d-closure) and the underlying natural-level
      image are preserved by pruning.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
      Sp = prune S
      numcols (inclusion Sp).natural
      numcols (inclusion S).natural
      image (inclusion S).natural == image (inclusion Sp).natural
      isWellDefined Sp
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @ on
      DG submodules.
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGQuotientModule)
    (prune, DGModule)
    dgSubmodule
///

doc ///
  Key
    (prune, DGQuotientModule)
    (minimalPresentation, DGQuotientModule)
  Headline
    Minimize the presentation of a DG quotient module
  Usage
    Qp = prune Q
    Qp = minimalPresentation Q
  Inputs
    Q:DGQuotientModule
  Outputs
    Qp:DGQuotientModule
      A DG quotient module of the same ambient DG module whose
      relations submodule has been pruned, and whose
      @ TT "toComplex Qp" @ has additionally been pruned at the
      complex level.
  Description
    Text
      @ TT "prune Q" @ proceeds in two steps.  First, it prunes the
      relations submodule @ TT "Q.subDGModule" @ using
      @ TO (prune, DGSubmodule) @; this trims redundant columns from
      the inclusion matrix.  If the inclusion was already minimal,
      @ TT "Qp === Q" @.
    Text
      Second, it computes @ TT "prune (toComplex Qp)" @ at the
      complex level and caches the result in @ TT "Qp.cache" @ under
      the symbol @ TT "prunedComplex" @.  After this, calling
      @ TT "toComplex Qp" @ returns a minimally presented complex and
      pruning commutes with @ TT "toComplex" @: the complex-level
      prune of @ TT "toComplex Qp" @ is idempotent.
    Text
      A @ TT "pruningMap" @ from @ TT "Qp" @ to @ TT "Q" @ is cached
      in @ TT "Qp.cache" @ as a @ TO DGModuleMap @ induced by the
      identity on @ TT "M.natural" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat, x_Anat, y_Anat}})
      Q = M / S
      Qp = prune Q
      image (inclusion Qp.subDGModule).natural == image (inclusion S).natural
    Text
      If the cokernel is secretly zero, pruning collapses
      @ TT "toComplex Qp" @ to the zero complex, matching
      @ TT "prune" @ applied to @ TT "Complex" @ objects from the Complexes package on @ TT "Complex" @.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @ on
      DG quotient modules.
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGSubmodule)
    dgQuotientModule
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    (prune, DGModule)
    (minimalPresentation, DGModule)
  Headline
    Pruning a DG module is the identity
  Usage
    Mp = prune M
    Mp = minimalPresentation M
  Inputs
    M:DGModule
  Outputs
    Mp:DGModule
      The same DG module @ TT "M" @.  A @ TT "pruningMap" @ equal to
      the identity DG module map on @ TT "M" @ is cached in
      @ TT "M.cache" @.
  Description
    Text
      DG modules as stored in this package (built from
      @ TO freeDGModule @ or @ TO koszulComplexDGM @) carry no
      auxiliary presentation data that @ TT "prune" @ could minimize;
      the method is installed as the identity so that calling code can
      use @ TT "prune" @ uniformly across DG types.
    Text
      Calling @ TT "prune M" @ has the side effect of caching an
      identity @ TO DGModuleMap @ under the symbol @ TT "pruningMap" @
      in @ TT "M.cache" @.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      (prune M) === M
      (minimalPresentation M) === M
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGSubmodule)
    (prune, DGQuotientModule)
///

doc ///
  Key
    (prune, DGAlgebra)
    (minimalPresentation, DGAlgebra)
  Headline
    Pruning a DG algebra is the identity
  Usage
    Ap = prune A
    Ap = minimalPresentation A
  Inputs
    A:DGAlgebra
  Outputs
    Ap:DGAlgebra
      The same DG algebra @ TT "A" @.
  Description
    Text
      DG algebras as stored in this package carry a fixed underlying
      graded algebra and differential; there is no redundancy for
      @ TT "prune" @ to eliminate.  The method is installed as the
      identity so that pruning commutes uniformly with every
      constructor.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      (prune A) === A
      (minimalPresentation A) === A
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGIdeal)
///

doc ///
  Key
    (prune, DGAlgebraMap)
    (minimalPresentation, DGAlgebraMap)
  Headline
    Pruning a DG algebra map is the identity
  Usage
    fp = prune f
    fp = minimalPresentation f
  Inputs
    f:DGAlgebraMap
  Outputs
    fp:DGAlgebraMap
      The same DG algebra map @ TT "f" @.
  Description
    Text
      Since @ TO (prune, DGAlgebra) @ is the identity, and a
      @ TO DGAlgebraMap @ is stored as a ring map between the
      underlying graded algebras of its source and target, there is
      nothing for @ TT "prune" @ to minimize.  The method is installed
      as the identity so that pruning commutes uniformly.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      phi = identityDGAlgebraMap A
      (prune phi) === phi
      (minimalPresentation phi) === phi
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGAlgebra)
    DGAlgebraMap
///

doc ///
  Key
    (prune, DGModuleMap)
    (minimalPresentation, DGModuleMap)
  Headline
    Pruning a DG module map is the identity
  Usage
    fp = prune f
    fp = minimalPresentation f
  Inputs
    f:DGModuleMap
  Outputs
    fp:DGModuleMap
      The same DG module map @ TT "f" @.
  Description
    Text
      Since @ TO (prune, DGModule) @ is the identity, and a
      @ TO DGModuleMap @ is stored as an @ TT "A.natural" @-linear
      matrix between the naturals of its source and target, there is
      nothing for @ TT "prune" @ to minimize.  The method is installed
      as the identity so that pruning commutes uniformly.
    Text
      @ TT "minimalPresentation" @ is a synonym for @ TT "prune" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      idM = identityDGModuleMap M
      (prune idM) === idM
      (minimalPresentation idM) === idM
  SeeAlso
    "Pruning DG modules, submodules, quotients, and maps"
    (prune, DGModule)
    DGModuleMap
///

doc ///
  Key
    "Well-definedness, acyclicity, and quasi-isomorphism"
  Headline
    Predicates that check DG-theoretic conditions on DG types
  Description
    Text
      The package exposes three families of predicates for checking
      conditions that distinguish DG objects from their underlying
      graded / module-theoretic data.
    Text
      @ TO isWellDefined @ — verifies that an object satisfies the DG
      axioms appropriate to its type.  For a @ TO DGAlgebra @ or
      @ TO DGModule @, the central check is that the stored
      differential squares to zero on every generator.  For a
      @ TO DGSubmodule @ or @ TO DGQuotientModule @, the check is
      d-closure of the defining submodule; for a @ TO DGAlgebraMap @
      or @ TO DGModuleMap @, the check is the chain-map condition
      @ TT "d \\circ f = f \\circ d" @ on every generator.
    Text
      @ TO isAcyclic @ — returns true when a DG algebra or DG module
      has @ TT "H_i = 0" @ for all @ TT "i >= 1" @ up to a finite
      bound.  For objects with infinite hom-degree the user must
      supply @ TO EndDegree @.
    Text
      @ TO isQuasiIsomorphism @ — returns true when a DG algebra map
      or DG module map induces an isomorphism on @ TT "H_*" @.  The
      underlying check is performed on @ TO toComplexMap @ and
      inherits the @ TT "Concentration" @ option from the Complexes
      package.
    Text
      All three predicates accept the option @ TO EndDegree @ where
      applicable; for finite-degree objects the default bound is
      @ TT "maxDegree" @ of the object and no option is required.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      isWellDefined A
      isAcyclic A
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{1_Anat}})
      Q = M / S
      isWellDefined S
      isWellDefined Q
      phi = identityDGAlgebraMap A
      isWellDefined phi
      isQuasiIsomorphism phi
    Text
      For a regular polynomial ring the Koszul DG algebra is acyclic
      and its identity map is a quasi-isomorphism.  Both
      @ TT "S" @ and @ TT "Q = M / S" @ are well-defined because
      @ TT "S" @ is d-closed.
  Subnodes
    (isWellDefined, DGAlgebra)
    (isWellDefined, DGModule)
    (isWellDefined, DGSubmodule)
    (isWellDefined, DGQuotientModule)
    (isWellDefined, DGModuleMap)
    (isAcyclic, DGModule)
    (isAcyclic, DGQuotientModule)
    (isQuasiIsomorphism, DGAlgebraMap)
    (annihilator, DGSubmodule)
    (annihilator, DGQuotientModule)
  SeeAlso
    (isWellDefined, DGIdeal)
    (isAcyclic, DGAlgebra)
    (isQuasiIsomorphism, DGModuleMap)
///

doc ///
  Key
    (isWellDefined, DGAlgebra)
  Headline
    Check that a DG algebra has correct structure and a differential that squares to zero
  Usage
    b = isWellDefined A
  Inputs
    A:DGAlgebra
  Outputs
    b:Boolean
      True when @ TT "A" @ has all expected structural keys with the
      correct types, the base ring of @ TT "A.natural" @ is compatible
      with @ TT "A.ring" @, the differential lowers homological
      degree by one on every generator, and @ TT "d^2 = 0" @ holds on
      every generator of @ TT "A.natural" @.
  Description
    Text
      Checks three conditions in order.  First, a structural
      validation: the DG algebra has keys @ TT "ring" @,
      @ TT "natural" @, @ TT "diff" @, and @ TT "Degrees" @ with the
      right types, and the length of @ TT "A.Degrees" @ equals the
      number of generators of @ TT "A.natural" @.  Second, every
      generator @ TT "g" @ of @ TT "A.natural" @ has
      @ TT "d(g)" @ of homological degree @ TT "|g| - 1" @.  Third,
      @ TT "d^2(g) = 0" @ on every generator — Leibniz then extends
      this to all of @ TT "A.natural" @.
    Text
      When @ TT "debugLevel > 0" @ the routine prints a diagnostic
      line for each failing check.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      isWellDefined A
    Text
      A DG algebra with @ TT "A.diff == {}" @ (no differential set)
      passes the check trivially — the second and third conditions
      apply only to generators with a stored differential.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGAlgebra
    setDiff
    (isWellDefined, DGIdeal)
    (isAcyclic, DGAlgebra)
///

doc ///
  Key
    (isWellDefined, DGModule)
  Headline
    Check that a DG module has correct structure and that its differential squares to zero
  Usage
    b = isWellDefined M
  Inputs
    M:DGModule
  Outputs
    b:Boolean
      True when @ TT "M" @ has all expected structural keys, its base
      DG algebra @ TT "M.dgAlgebra" @ is itself well-defined, the
      ring of @ TT "M" @ matches that of its DG algebra, each stored
      differential is a module element of @ TT "M.natural" @, and
      @ TT "d_M^2 = 0" @ on every generator.
  Description
    Text
      Performs a structural check (required keys and list-length
      agreement @ TT "#M.diff == #M.Degrees" @), then verifies that
      @ TT "M.dgAlgebra" @ is a well-defined DG algebra, checks that
      each entry of @ TT "M.diff" @ is a vector or matrix over
      @ TT "M.natural" @, and finally calls the internal
      @ TT "isWellDefinedDifferential" @ routine to verify
      @ TT "d_M^2 = 0" @ up to the natural bound.
    Text
      Diagnostic messages are emitted when @ TT "debugLevel > 0" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      isWellDefined M
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGModule
    freeDGModule
    setDiff
    (isWellDefined, DGSubmodule)
    (isWellDefined, DGModuleMap)
///

doc ///
  Key
    (isWellDefined, DGSubmodule)
  Headline
    Check that a DG submodule is d-closed in its ambient DG module
  Usage
    b = isWellDefined S
  Inputs
    S:DGSubmodule
  Outputs
    b:Boolean
      True when @ TT "S" @ has all expected structural keys, its
      ambient is a well-defined DG module, and the column span of
      @ TT "S.inclusion.natural" @ in @ TT "S.ambient.natural" @ is
      stable under @ TT "d_{S.ambient}" @.
  Description
    Text
      After a structural key check, verifies that
      @ TT "S.ambient" @ is a well-defined DG module, then checks
      d-closure: for every column @ TT "v" @ of the inclusion matrix,
      @ TT "d_M(v)" @ lies in the column span of the inclusion
      matrix modulo further columns.  This is the one nontrivial
      condition that distinguishes a DG submodule from a general
      A.natural-submodule of @ TT "M.natural" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      isWellDefined S
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGSubmodule
    dgSubmodule
    (isWellDefined, DGModule)
    (isWellDefined, DGQuotientModule)
///

doc ///
  Key
    (isWellDefined, DGQuotientModule)
  Headline
    Check that a DG quotient module has a well-defined differential
  Usage
    b = isWellDefined Q
  Inputs
    Q:DGQuotientModule
  Outputs
    b:Boolean
      True when @ TT "Q" @ has all expected structural keys, its
      projection map has the right source and target, and its
      relations submodule @ TT "Q.subDGModule" @ is itself a
      well-defined DG submodule of @ TT "Q.ambient" @.
  Description
    Text
      After a structural key-shape check, verifies that
      @ TT "Q.ambient" @ is a @ TO DGModule @, that
      @ TT "Q.subDGModule" @ is a @ TO DGSubmodule @ with ambient
      equal to @ TT "Q.ambient" @, and that @ TT "Q.projection" @ is
      a @ TO DGModuleMap @ from @ TT "Q.ambient" @ to @ TT "Q" @.
      The substantive check then delegates to
      @ TO (isWellDefined, DGSubmodule) @ on the relations
      submodule: the quotient is well-defined exactly when the
      relations are d-closed in the ambient.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      isWellDefined Q
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGQuotientModule
    dgQuotientModule
    (isWellDefined, DGSubmodule)
    (symbol /, DGModule, DGSubmodule)
///

doc ///
  Key
    (isWellDefined, DGModuleMap)
  Headline
    Check that a DG module map has correct structure and is a chain map
  Usage
    b = isWellDefined f
  Inputs
    f:DGModuleMap
  Outputs
    b:Boolean
      True when @ TT "f" @ has all expected structural keys, source
      and target are DG modules (or DG quotient modules) over a
      common DG algebra, the natural matrix has the correct source
      and target, @ TT "f" @ preserves hom-degree, and
      @ TT "d_N(f(e_i)) = f(d_M(e_i))" @ on every natural generator
      @ TT "e_i" @ of the source.
  Description
    Text
      Six checks in order: structural key shape; source and target
      types are @ TO DGModule @ or @ TO DGQuotientModule @; source
      and target share the same DG algebra;
      @ TT "f.natural" @ is a module map with the correct source and
      target; the hom-degree of @ TT "f" @ is zero; and the chain-map
      condition holds on every generator of the source.
    Text
      Modeled on @ TT "isWellDefined ComplexMap" @ from the
      Complexes package.  Diagnostic messages are emitted when
      @ TT "debugLevel > 0" @.
    Example
      R = QQ[x, y]/ideal(x^2, y^2)
      A = koszulComplexDGA R
      M = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 2)
      idM = identityDGModuleMap M
      zM = zeroDGModuleMap(M, M)
      isWellDefined idM
      isWellDefined zM
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    DGModuleMap
    dgModuleMap
    identityDGModuleMap
    zeroDGModuleMap
    (isWellDefined, DGModule)
///

doc ///
  Key
    (isAcyclic, DGModule)
    [(isAcyclic, DGModule), EndDegree]
  Headline
    Determine whether a DG module has vanishing positive-degree homology
  Usage
    b = isAcyclic M
    b = isAcyclic(M, EndDegree => n)
  Inputs
    M:DGModule
    EndDegree => ZZ
      An upper bound for the range of degrees checked.  Required when
      @ TT "M" @ has infinite @ TO maxDegree @ (for example, when its
      DG algebra is an @ TO acyclicClosure @).  When omitted and
      @ TT "M" @ is finite-dimensional, @ TT "maxDegree M" @ is used.
  Outputs
    b:Boolean
      True when @ TT "H_i(M) = 0" @ for all @ TT "1 <= i <= n" @.
  Description
    Text
      Checks per-degree homology via @ TO (homology, ZZ, DGModule) @
      and @ TO prune @.  Degree zero is intentionally excluded:
      acyclicity concerns only positive-degree homology.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      KM = koszulComplexDGM R^1
      isAcyclic KM
    Text
      For the Koszul DG module on a regular sequence, all
      positive-degree homology vanishes (@ TT "H_0 = R / (x, y, z)" @
      is the only nonzero piece).
    Example
      R' = QQ[x, y]/ideal(x^2, y^2)
      KM' = koszulComplexDGM R'^1
      not isAcyclic(KM', EndDegree => 3)
    Text
      Over a non-regular ring the Koszul DG module picks up nontrivial
      positive-degree homology.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (isAcyclic, DGAlgebra)
    (isAcyclic, DGQuotientModule)
    (homology, ZZ, DGModule)
    EndDegree
///

doc ///
  Key
    (isAcyclic, DGQuotientModule)
    [(isAcyclic, DGQuotientModule), EndDegree]
  Headline
    Determine whether a DG quotient module has vanishing positive-degree homology
  Usage
    b = isAcyclic Q
    b = isAcyclic(Q, EndDegree => n)
  Inputs
    Q:DGQuotientModule
    EndDegree => ZZ
      An upper bound for the range of degrees checked.  Required when
      @ TT "Q.ambient" @ has infinite @ TO maxDegree @.
  Outputs
    b:Boolean
      True when @ TT "H_i(Q) = 0" @ for all @ TT "1 <= i <= n" @.
  Description
    Text
      Semantics match @ TO (isAcyclic, DGModule) @: positive-degree
      homology only, computed via @ TO (homology, ZZ, DGQuotientModule) @
      and @ TO prune @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      Sfull = dgSubmodule(M, id_(M.natural))
      Q0 = M / Sfull
      isAcyclic Q0
    Text
      When the quotient is the zero DG module, @ TT "isAcyclic" @
      holds trivially.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (isAcyclic, DGModule)
    (homology, ZZ, DGQuotientModule)
    EndDegree
///

doc ///
  Key
    (isQuasiIsomorphism, DGAlgebraMap)
  Headline
    Determine whether a DG algebra map induces an isomorphism on homology
  Usage
    b = isQuasiIsomorphism phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    b:Boolean
      True when the induced chain map @ TT "toComplexMap phi" @ is a
      quasi-isomorphism on the underlying complexes.
  Description
    Text
      Computed by forwarding to @ TT "isQuasiIsomorphism(toComplexMap phi)" @
      from the Complexes package.  The underlying check inspects
      @ TT "H_n" @ of the induced chain map at every degree in the
      concentration of the source and target.  The
      @ TT "Concentration" @ option of the Complexes version is
      accepted and passed through.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      phi = identityDGAlgebraMap A
      isQuasiIsomorphism phi
    Text
      The identity DG algebra map is trivially a quasi-isomorphism.
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (isQuasiIsomorphism, DGModuleMap)
    identityDGAlgebraMap
    toComplexMap
///

doc ///
  Key
    (annihilator, DGSubmodule)
  Headline
    The DG ideal of A annihilating a DG submodule
  Usage
    I = annihilator S
  Inputs
    S:DGSubmodule
  Outputs
    I:DGIdeal
      The DG ideal of @ TT "S.ambient.dgAlgebra" @ whose underlying
      ideal is the annihilator of @ TT "image S.inclusion.natural" @
      as a submodule of @ TT "S.ambient.natural" @.
  Description
    Text
      Computes @ TT "annihilator(image S.inclusion.natural)" @ at the
      @ TT "A.natural" @ level, then wraps the result as a DG ideal.
      The result is a DG ideal: if @ TT "a" @ annihilates @ TT "S" @
      and @ TT "s \\in S" @, then
      @ TT "d(a s) = d(a) s \\pm a d(s)" @ vanishes because both
      terms vanish (the second since @ TT "d(s) \\in S" @), so
      @ TT "d(a) s = 0" @ for every @ TT "s \\in S" @ and
      @ TT "d(a)" @ is again in the annihilator.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{x_Anat}})
      I = annihilator S
      isWellDefined I
    Text
      The annihilator of the zero submodule is the unit ideal.
    Example
      S0 = dgSubmodule(M, map(M.natural, (Anat)^0, 0))
      annihilator S0 == dgIdeal(A, {1_Anat})
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (annihilator, DGQuotientModule)
    DGIdeal
    dgIdeal
    DGSubmodule
///

doc ///
  Key
    (annihilator, DGQuotientModule)
  Headline
    The DG ideal of A annihilating a DG quotient module
  Usage
    I = annihilator Q
  Inputs
    Q:DGQuotientModule
  Outputs
    I:DGIdeal
      The DG ideal of @ TT "Q.dgAlgebra" @ whose underlying ideal is
      the annihilator of @ TT "Q.natural" @ (a cokernel presentation)
      as an @ TT "A.natural" @-module.
  Description
    Text
      Computes @ TT "annihilator Q.natural" @ and wraps it as a DG
      ideal.  As with @ TO (annihilator, DGSubmodule) @ the result is
      automatically d-closed: the argument showing closure uses
      Leibniz together with the fact that @ TT "d" @ on the ambient
      descends to @ TT "d" @ on the quotient.
    Example
      R = ZZ/101[x, y]
      A = koszulComplexDGA R
      Anat = A.natural
      M = freeDGModule(A, {0})
      S = dgSubmodule(M, matrix {{x_Anat}})
      Q = M / S
      I = annihilator Q
      isWellDefined I
      isSubset(dgIdeal(A, {x_Anat}), I)
  SeeAlso
    "Well-definedness, acyclicity, and quasi-isomorphism"
    (annihilator, DGSubmodule)
    DGIdeal
    dgQuotientModule
    DGQuotientModule
///

doc ///
  Key
    "Semifree resolutions of DG modules"
  Headline
    Building semifree DG module resolutions via koszulComplexDGM, adjoinGenerators, killCycles, and semifreeResolution
  Description
    Text
      A @ TO DGModule @ @ TT "F" @ over a @ TO DGAlgebra @ @ TT "A" @ is
      @ EM "semifree" @ if its underlying graded @ TT "A.natural" @-module
      is free with a homogeneous basis.  Semifree DG modules play the
      role of free modules in ordinary homological algebra: every
      @ TT "A" @-module (or @ TT "A.ring" @-module lifted along the
      augmentation @ TT "A -> A.ring" @) admits a semifree resolution,
      unique up to DG module homotopy.
    Text
      The package provides four building blocks for assembling semifree
      resolutions by hand or automatically:
    Text
      @ TO koszulComplexDGM @ constructs the Koszul DG module
      @ TT "K^R \\otimes_R M" @ as a DG module over
      @ TT "koszulComplexDGA(ring M)" @.  On a regular ring this is
      already a free resolution of @ TT "M" @; over a singular ring it
      provides a starting point that needs further generators adjoined
      to kill higher homology.
    Text
      @ TO adjoinGenerators @ takes a free DG module @ TT "M" @ and a
      list of cycles in @ TT "M.natural" @ and returns a new free DG
      module with one additional generator per cycle, whose differential
      is that cycle.  This is the module-theoretic analog of
      @ TO adjoinVariables @ on a DG algebra.
    Text
      @ TO (killCycles, DGModule) @ scans for the smallest hom-degree
      @ TT "n" @ in the requested range at which @ TT "H_n(M)" @ is
      nonzero, then adjoins one new hom-degree-@ TT "(n+1)" @ generator
      per minimal homology class.  Iterating this from
      @ TT "StartDegree" @ up to @ TT "EndDegree" @ produces a DG module
      with vanishing homology in that range.
    Text
      @ TO semifreeResolution @ automates the full construction: build a
      free cover of @ TT "M" @, adjoin hom-degree-1 generators for the
      relations of @ TT "M" @, then iterate
      @ TO (killCycles, DGModule) @.  Its refined cousin
      @ TO minimalSemifreeResolution @ applies
      @ TO getBoundaryPreimage @ at each stage to subtract off
      boundaries before adjoining, producing a resolution that is
      minimal over @ TT "A" @ in the sense that every generator's
      differential lands in the augmentation ideal of
      @ TT "A.natural" @.  The predicate
      @ TO isMinimalSemifreeResolution @ tests this minimality
      condition.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      isMinimalSemifreeResolution Mdg
      apply(0..3, n -> prune homology(n, Mdg))
    Text
      Over a complete intersection of codimension two, the rank of
      @ TT "F_n" @ in the minimal semifree resolution of the residue
      field matches the @ TT "n" @-th Betti number @ TT "n+1" @, as
      expected from the Tate construction.
  Subnodes
    koszulComplexDGM
    adjoinGenerators
    (killCycles, DGModule)
    semifreeResolution
    [semifreeResolution, StartDegree]
    [semifreeResolution, EndDegree]
    minimalSemifreeResolution
    [minimalSemifreeResolution, StartDegree]
    [minimalSemifreeResolution, EndDegree]
    isMinimalSemifreeResolution
  SeeAlso
    acyclicClosure
    killCycles
    adjoinVariables
    getBoundaryPreimage
///

doc ///
  Key
    koszulComplexDGM
    (koszulComplexDGM, Module)
    (koszulComplexDGM, DGAlgebra, Module)
  Headline
    The Koszul complex of a module as a DG module
  Usage
    KM = koszulComplexDGM M
    KM = koszulComplexDGM(A, M)
  Inputs
    A:DGAlgebra
      A DG algebra over @ TT "R = ring M" @.  If omitted,
      @ TT "koszulComplexDGA(ring M)" @ is used.
    M:Module
      A module over a polynomial or quotient ring @ TT "R" @.
  Outputs
    KM:DGModule
      The Koszul DG module @ TT "K^R \\otimes_R M" @ viewed as a DG
      module over @ TT "A" @.
  Description
    Text
      The module generators of @ TT "KM" @ are one copy of each
      generator of @ TT "M" @, placed in hom-degree @ TT "0" @.  The
      differential vanishes on these generators, so the full
      differential of @ TT "KM" @ is determined by the Leibniz rule
      together with the differential on @ TT "A.natural" @.  In the
      one-argument form the DG algebra is built on demand via
      @ TT "koszulComplexDGA(ring M)" @; in the two-argument form the
      caller supplies @ TT "A" @, whose @ TT "ring" @ must equal
      @ TT "ring M" @.
    Text
      On a regular ring @ TT "R" @, @ TT "K^R" @ resolves the residue
      field @ TT "k" @, so @ TT "koszulComplexDGM M" @ resolves @ TT "M" @
      via @ TT "k \\otimes_R M" @-style homology.  In particular
      @ TT "koszulComplexDGM R^1" @ recovers the Koszul complex of
      @ TT "R" @ itself.
    Text
      Over a regular ring, the Koszul DG module on @ TT "R^1" @ is a
      free resolution of the residue field, so its higher homology
      vanishes:
    Example
      R = QQ[x, y, z]
      KM = koszulComplexDGM R^1
      apply(0..3, n -> prune homology(n, KM))
    Text
      Over a complete intersection, the higher homology of
      @ TT "koszulComplexDGM R^1" @ is no longer zero — it is the Koszul
      homology algebra of @ TT "R" @, which records the deviations of
      @ TT "R" @.  For the codimension-2 complete intersection
      @ TT "k[x,y]/(x^2, y^2)" @, @ TT "HH_i(KM)" @ has rank equal to the
      @ TT "i" @-th Betti number of the residue field:
    Example
      S = QQ[x, y] / ideal(x^2, y^2)
      KS = koszulComplexDGM S^1
      apply(0..3, n -> numgens prune homology(n, KS))
    Text
      Applying @ TO koszulComplexDGM @ to a nontrivial quotient module
      produces its Koszul resolution, now tensored against the given
      module.  Here we resolve @ TT "S / (x)" @ over @ TT "S = k[x, y]" @
      and read off its free ranks:
    Example
      S = QQ[x, y]
      KQ = koszulComplexDGM(S^1 / ideal(x))
      C = toComplex KQ
      apply(0..2, n -> rank C_n)
      apply(0..2, n -> prune homology(n, KQ))
    Text
      In the two-argument form the caller-supplied DG algebra must share
      the base ring of the module.  This is the usual way to build a
      Koszul DG module that lives over the @ TO DGAlgebra @ you will pass
      to @ TO semifreeResolution @ or @ TO minimalSemifreeResolution @:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, R^1 / ideal(x, y), EndDegree => 3)
      apply(0..3, n -> rank (toComplex Mdg)_n)
  Caveat
    The two-argument form raises an error when
    @ TT "A.ring =!= ring M" @.
  SeeAlso
    "Semifree resolutions of DG modules"
    koszulComplexDGA
    DGModule
    freeDGModule
    semifreeResolution
    minimalSemifreeResolution
///

doc ///
  Key
    adjoinGenerators
    (adjoinGenerators, DGModule, List)
  Headline
    Adjoin new free generators to a DG module with prescribed differentials
  Usage
    Mnew = adjoinGenerators(M, cycleList)
  Inputs
    M:DGModule
      A free DG module (built by @ TO freeDGModule @ or an earlier call
      to @ TO adjoinGenerators @).
    cycleList:List
      A list of Vectors in @ TT "M.natural" @; each entry should be a
      cycle under the differential of @ TT "M" @, homogeneous with
      respect to the ambient multi-grading.
  Outputs
    Mnew:DGModule
      A free DG module whose generator list is @ TT "M.Degrees" @
      followed by one new generator per cycle in @ TT "cycleList" @,
      shifted up by one hom-degree, with differential equal to the
      corresponding cycle.
  Description
    Text
      This is the module-theoretic analog of @ TO adjoinVariables @ on a
      DG algebra.  For each @ TT "z" @ in @ TT "cycleList" @ of
      hom-degree @ TT "d" @, the output has a fresh generator in
      hom-degree @ TT "d + 1" @ whose differential is @ TT "z" @.  If
      @ TT "z" @ is indeed a cycle then the new generator witnesses its
      homology class as a boundary, so homology classes supported on
      @ TT "cycleList" @ are killed.
    Text
      Existing generator indices and their differentials are preserved
      verbatim: the first @ TT "#M.Degrees" @ generators of
      @ TT "Mnew" @ correspond to those of @ TT "M" @, so code that
      references @ TT "(M.natural)_i" @ for old indices @ TT "i" @
      continues to work when lifted to @ TT "Mnew" @.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      Mnew = adjoinGenerators(M, {x * natGens#0})
      #Mnew.Degrees
      first Mnew.Degrees#1
    Text
      The new generator sits in hom-degree @ TT "1" @ and its
      differential is the cycle @ TT "x * e_0" @ in @ TT "M.natural" @.
  Caveat
    Only supported when @ TT "M.natural" @ is a free
    @ TT "A.natural" @-module (i.e. @ TT "M" @ was built by
    @ TO freeDGModule @ or a previous @ TO adjoinGenerators @ call).
    Each entry of @ TT "cycleList" @ must be a cycle; otherwise the
    output differential will not satisfy @ TT "d^2 = 0" @.
  SeeAlso
    "Semifree resolutions of DG modules"
    freeDGModule
    (killCycles, DGModule)
    adjoinVariables
///

doc ///
  Key
    (killCycles, DGModule)
    [(killCycles, DGModule), StartDegree]
    [(killCycles, DGModule), EndDegree]
  Headline
    Adjoin free generators to kill the lowest nonvanishing homology of a DG module
  Usage
    Mnew = killCycles M
    Mnew = killCycles(M, StartDegree => s, EndDegree => e)
  Inputs
    M:DGModule
      A free DG module.
    StartDegree => ZZ
      The smallest hom-degree at which to look for nontrivial homology.
      Defaults to @ TT "1" @.
    EndDegree => ZZ
      The largest hom-degree at which to look; defaults to
      @ TT "StartDegree" @ (so by default a single degree is scanned).
  Outputs
    Mnew:DGModule
      Either @ TT "M" @ itself (if homology vanishes throughout the
      requested range) or a free DG module with additional hom-degree
      @ TT "n+1" @ generators, where @ TT "n" @ is the smallest degree
      in the range with @ TT "H_n(M) != 0" @; the new generators'
      differentials are representative cycles of a minimal generating
      set for @ TT "H_n(M)" @.
  Description
    Text
      The construction mirrors @ TO (killCycles, DGAlgebra) @ for DG
      modules.  Scanning from @ TT "StartDegree" @ upwards, the method
      computes @ TT "prune homology(n, M)" @ until it finds the first
      degree @ TT "n" @ with nontrivial homology.  It then lifts the
      minimal homology generators provided by the pruning map back to
      cycles in @ TT "M_n" @ and hands them to @ TO adjoinGenerators @,
      producing a new DG module in which those classes become
      boundaries.
    Text
      A single call kills homology at one degree.  Iterating with
      successive @ TT "StartDegree" @ values is how
      @ TO semifreeResolution @ and
      @ TO minimalSemifreeResolution @ build resolutions.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      M2 = adjoinGenerators(M, {x * natGens#0})
      prune homology(0, M2)
      M3 = killCycles(M2, StartDegree => 1, EndDegree => 2)
      prune homology(1, M3) == 0
    Text
      @ TT "M2" @ has @ TT "H_0 = R/(x) = k" @ and no higher homology
      to kill, so @ TT "M3" @ agrees with @ TT "M2" @ in this example.
      In settings where @ TT "H_1" @ is nontrivial, @ TT "killCycles" @
      adjoins one hom-degree-2 generator per minimal @ TT "H_1" @
      class.
  Caveat
    Raises an error when @ TT "StartDegree > EndDegree" @.  The input
    must be a free DG module for the underlying homology computation
    to extract representative cycles.
  SeeAlso
    "Semifree resolutions of DG modules"
    adjoinGenerators
    (killCycles, DGAlgebra)
    semifreeResolution
    minimalSemifreeResolution
    StartDegree
    EndDegree
///

doc ///
  Key
    semifreeResolution
    (semifreeResolution, DGAlgebra, Module)
    (semifreeResolution, Module)
  Headline
    Build a semifree DG module resolving a module over the base ring
  Usage
    F = semifreeResolution M
    F = semifreeResolution(A, M)
    F = semifreeResolution(A, M, EndDegree => n)
  Inputs
    A:DGAlgebra
      A DG algebra over @ TT "R = ring M" @.  If omitted,
      @ TT "koszulComplexDGA(ring M)" @ is used.
    M:Module
      A module over @ TT "A.ring" @.
    StartDegree => ZZ
      First hom-degree at which to start killing homology.  Defaults
      to @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which to kill homology.  Defaults to
      @ TT "3" @.  Homology of @ TT "F" @ in the range
      @ TT "[StartDegree, EndDegree]" @ will vanish.
  Outputs
    F:DGModule
      A free DG module over @ TT "A" @ with
      @ TT "H_0(F) = M" @ and @ TT "H_n(F) = 0" @ for
      @ TT "StartDegree <= n <= EndDegree" @.
  Description
    Text
      Starts from a free DG module with one hom-degree-0 generator per
      generator of @ TT "M" @, adjoins hom-degree-1 generators whose
      differentials are the columns of @ TT "presentation M" @ lifted
      to @ TT "A.natural" @ (killing the presentation relations), and
      then iterates @ TO (killCycles, DGModule) @ from
      @ TT "StartDegree" @ through @ TT "EndDegree" @.
    Text
      This is the module-theoretic analog of
      @ TO acyclicClosure @ on a DG algebra.  The output is a semifree
      DG module but is not guaranteed to be minimal over @ TT "A" @:
      relation columns are adjoined even when they are already
      boundaries under the DG algebra differential.  For a minimal
      resolution, use @ TO minimalSemifreeResolution @.
    Example
      R = QQ[x]
      M = R^1 / ideal(x)
      Mdg = semifreeResolution(koszulComplexDGA R, M, EndDegree => 3)
      prune homology(0, Mdg)
      all(1..3, n -> prune homology(n, Mdg) == 0)
    Text
      Over a complete intersection, the resolution is infinite but the
      truncation up to any requested @ TT "EndDegree" @ is acyclic in
      positive hom-degrees up to that bound.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      Mdg = semifreeResolution(koszulComplexDGA R, k, EndDegree => 3)
      all(1..3, i -> prune homology(i, Mdg) == 0)
    Text
      The @ TT "Module" @-only form infers the DG algebra from
      @ TT "ring M" @:
    Example
      R = QQ[x]
      M = R^1 / ideal(x)
      Mdg = semifreeResolution(M, EndDegree => 2)
      Mdg.ring === R
      Mdg.dgAlgebra.ring === R
  Caveat
    The output is not minimal in general; use
    @ TO minimalSemifreeResolution @ when a minimal presentation is
    required.  The base ring of @ TT "M" @ must agree with
    @ TT "A.ring" @ in the two-argument form.
  SeeAlso
    "Semifree resolutions of DG modules"
    minimalSemifreeResolution
    (killCycles, DGModule)
    acyclicClosure
    koszulComplexDGM
///

doc ///
  Key
    [semifreeResolution, StartDegree]
  Headline
    Option to specify the degree at which semifreeResolution starts killing cycles
  Usage
    semifreeResolution(..., StartDegree => n)
  Description
    Text
      Controls the first hom-degree at which
      @ TO semifreeResolution @ attempts to kill homology.
      Hom-degrees strictly below @ TT "StartDegree" @ are populated
      only by the initial free cover and the adjoinment of
      presentation relations at hom-degree @ TT "1" @.  The default
      value is @ TT "1" @.
  SeeAlso
    semifreeResolution
    [semifreeResolution, EndDegree]
///

doc ///
  Key
    [semifreeResolution, EndDegree]
  Headline
    Option to specify the last degree at which semifreeResolution kills cycles
  Usage
    semifreeResolution(..., EndDegree => n)
  Description
    Text
      Controls the last hom-degree at which
      @ TO semifreeResolution @ iterates
      @ TO (killCycles, DGModule) @.  Homology of the output in the
      range @ TT "[StartDegree, EndDegree]" @ vanishes; higher degrees
      are not resolved.  The default value is @ TT "3" @.
  SeeAlso
    semifreeResolution
    [semifreeResolution, StartDegree]
///

doc ///
  Key
    minimalSemifreeResolution
    (minimalSemifreeResolution, DGAlgebra, Module)
    (minimalSemifreeResolution, Module)
  Headline
    Build a minimal semifree DG module resolution over a DG algebra
  Usage
    F = minimalSemifreeResolution M
    F = minimalSemifreeResolution(A, M)
    F = minimalSemifreeResolution(A, M, EndDegree => n)
  Inputs
    A:DGAlgebra
      A DG algebra over a (graded-)local ring @ TT "R = ring M" @.  If
      omitted, @ TT "koszulComplexDGA(ring M)" @ is used.
    M:Module
      A module over @ TT "A.ring" @.
    StartDegree => ZZ
      First hom-degree at which to start killing homology.  Defaults
      to @ TT "1" @.
    EndDegree => ZZ
      Last hom-degree at which to kill homology.  Defaults to
      @ TT "3" @.
  Outputs
    F:DGModule
      A semifree DG module resolving @ TT "M" @ over @ TT "A" @,
      minimal in the sense that every generator's differential lands
      in the augmentation ideal of @ TT "A.natural" @.
  Description
    Text
      Produces a semifree resolution whose generator count in each
      hom-degree is minimal among all semifree resolutions of
      @ TT "M" @ over @ TT "A" @.  Two ingredients make the output
      minimal:
    Text
      First, @ TT "M" @ is replaced by @ TT "prune M" @, giving a
      minimal presentation over the (graded-)local base ring.  The
      number of hom-degree-0 generators is therefore the minimal
      number of generators of @ TT "M" @ over @ TT "R" @.
    Text
      Second, each column of @ TT "presentation(prune M)" @ is tested
      with @ TO getBoundaryPreimage @ against the DG-algebra-induced
      differential.  If a relation is already a boundary, it is not
      adjoined; if only a residue is a non-boundary, the adjoined
      generator's differential is that residue rather than the
      original relation.  This is what makes the output minimal
      @ EM "over A" @, not merely over @ TT "R" @.
    Text
      The iterative @ TO (killCycles, DGModule) @ passes then use the
      canonical pruning map of @ TT "homology(n, F)" @ to pick a
      minimal generating set of representative cycles at each stage.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      isMinimalSemifreeResolution Mdg
      all(1..3, i -> prune homology(i, Mdg) == 0)
    Text
      Over the c.i. @ TT "R = k[x, y]/(x^2, y^2)" @ the minimal
      semifree resolution of @ TT "k" @ has @ TT "rank F_n = n+1" @,
      matching the Betti numbers of @ TT "k" @ over @ TT "R" @.
    Example
      apply(0..3, n -> numcols moduleDifferential(n, Mdg))
    Text
      As with @ TO semifreeResolution @, the @ TT "Module" @-only
      form infers @ TT "koszulComplexDGA(ring M)" @ as the DG
      algebra:
    Example
      R = QQ[x]
      M = R^1 / ideal(x)
      Mdg = minimalSemifreeResolution(M, EndDegree => 2)
      Mdg.ring === R
      isMinimalSemifreeResolution Mdg
    Text
      A more substantial example: take the Koszul DG algebra on a
      regular sequence cutting out a complete intersection, and
      resolve a module over the resulting DG algebra.  Here
      @ TT "A = koszulComplexDGA(I_*)" @ with
      @ TT "I = (x^2, y^2)" @ is a DG algebra over
      @ TT "Q = ZZ/101[x, y]" @ whose degree-zero homology is the
      complete intersection @ TT "Q/I" @, and
      @ TT "A" @ itself is the minimal DG algebra resolution of
      @ TT "Q/I" @ over @ TT "Q" @.  We resolve the
      @ TT "Q/I" @-module @ TT "M = Q/(x^2, x*y, y^2)" @ as a DG
      @ TT "A" @-module:
    Example
      Q = ZZ/101[x, y]
      I = ideal(x^2, y^2)
      A = koszulComplexDGA(I_*)
      M = Q^1 / ideal(x^2, x*y, y^2)
      Mdg = minimalSemifreeResolution(A, M, EndDegree => 5)
      isMinimalSemifreeResolution Mdg
      all(1..5, i -> prune homology(i, Mdg) == 0)
      apply(0..5, n -> numcols moduleDifferential(n, Mdg))
    Text
      @ BOLD "Matrix factorizations on hypersurfaces." @  Over a
      hypersurface @ TT "R = Q/(f)" @ every maximal Cohen-Macaulay
      module's free resolution is eventually @ EM "2-periodic" @
      with differentials forming a matrix factorization
      @ TT "(phi, psi)" @ of @ TT "f" @ (Eisenbud).  The minimal
      semifree resolution over @ TT "A = koszulComplexDGA(ideal f)" @
      exhibits this periodicity directly in its printed
      differentials.  Here is the classic example @ TT "f = x^5" @:
    Example
      Q = ZZ/101[x]
      A = koszulComplexDGA(ideal(x^5))
      Mdg = minimalSemifreeResolution(A, Q^1/ideal(x), EndDegree => 6)
      apply(1..6, n -> flatten entries moduleDifferential(n, Mdg))
    Text
      The 2x2 blocks alternate between an @ TT "x" @-pattern and an
      @ TT "x^4" @-pattern; their product is @ TT "x^5" @ times an
      identity matrix, which is exactly the matrix factorization
      @ TT "x * x^4 = x^5" @.  Similarly, for the quadric form
      @ TT "f = x^2 + y^2" @ the module
      @ TT "coker [[x, y], [y, -x]]" @ is already a matrix
      factorization of @ TT "f" @, so its minimal semifree
      resolution stabilizes in hom-degree one:
    Example
      Q = ZZ/101[x, y]
      A = koszulComplexDGA(ideal(x^2 + y^2))
      M = coker matrix{{x, y}, {y, -x}}
      Mdg = minimalSemifreeResolution(A, M, EndDegree => 6)
      apply(0..6, n -> numcols moduleDifferential(n, Mdg))
    Text
      For the three-variable quadric @ TT "f = x^2 + y^2 + z^2" @,
      resolving the residue field stabilizes at rank @ TT "8" @,
      reflecting the 8-dimensional Clifford algebra of a
      nondegenerate ternary quadratic form:
    Example
      Q = ZZ/101[x, y, z]
      A = koszulComplexDGA(ideal(x^2 + y^2 + z^2))
      Mdg = minimalSemifreeResolution(A, Q^1/ideal(x, y, z), EndDegree => 6)
      apply(0..6, n -> numcols moduleDifferential(n, Mdg))
  Caveat
    Minimality relies on @ TT "A.ring" @ being (graded-)local so that
    @ TT "prune" @ returns a minimal presentation.  For
    @ TT "A = koszulComplexDGA R" @ with @ TT "R" @ a standard graded
    quotient of a polynomial ring over a field, this is satisfied.
  SeeAlso
    "Semifree resolutions of DG modules"
    semifreeResolution
    isMinimalSemifreeResolution
    getBoundaryPreimage
    (killCycles, DGModule)
///

doc ///
  Key
    [minimalSemifreeResolution, StartDegree]
  Headline
    Option to specify the degree at which minimalSemifreeResolution starts killing cycles
  Usage
    minimalSemifreeResolution(..., StartDegree => n)
  Description
    Text
      Controls the first hom-degree at which
      @ TO minimalSemifreeResolution @ attempts to kill homology.
      The default is @ TT "1" @.
  SeeAlso
    minimalSemifreeResolution
    [minimalSemifreeResolution, EndDegree]
///

doc ///
  Key
    [minimalSemifreeResolution, EndDegree]
  Headline
    Option to specify the last degree at which minimalSemifreeResolution kills cycles
  Usage
    minimalSemifreeResolution(..., EndDegree => n)
  Description
    Text
      Controls the last hom-degree at which
      @ TO minimalSemifreeResolution @ iterates
      @ TO (killCycles, DGModule) @.  Homology of the output in the
      range @ TT "[StartDegree, EndDegree]" @ vanishes.  The default
      is @ TT "3" @.
  SeeAlso
    minimalSemifreeResolution
    [minimalSemifreeResolution, StartDegree]
///

doc ///
  Key
    isMinimalSemifreeResolution
    (isMinimalSemifreeResolution, DGModule)
  Headline
    Test whether a semifree DG module is minimal over its DG algebra
  Usage
    b = isMinimalSemifreeResolution M
  Inputs
    M:DGModule
      A free DG module (the intended use is on the output of
      @ TO semifreeResolution @ or
      @ TO minimalSemifreeResolution @).
  Outputs
    b:Boolean
      @ TT "true" @ when each generator's differential lies in the
      augmentation ideal of @ TT "A.natural" @, i.e. its reduction
      along @ TT "A -> k" @ (sending @ TT "T_i -> 0" @ and then mod
      @ TT "(vars R)" @) is zero.
  Description
    Text
      Minimality over a DG algebra @ TT "A" @ means: after base change
      along the augmentation @ TT "A -> k" @, every differential of
      @ TT "F" @ becomes the zero map.  Equivalently, each
      @ TT "M.diff#i" @ has all of its coefficients (in the free
      basis of @ TT "M.natural" @) lying in the augmentation ideal of
      @ TT "A.natural" @ generated by @ TT "(vars R)" @ and the
      positive-hom-degree generators @ TT "T_i" @.
    Text
      This predicate checks only the structural minimality condition;
      it does not verify acyclicity.  Use
      @ TO (isAcyclic, DGModule) @ with an @ TT "EndDegree" @ option
      for that.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      isMinimalSemifreeResolution Mdg
    Text
      A DG module built with a unit differential between free
      generators is not minimal:
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      Mbad = freeDGModule(A, {0, 1})
      natGens = apply(rank Mbad.natural, i -> (Mbad.natural)_i)
      setDiff(Mbad, {0, natGens#0})
      isMinimalSemifreeResolution Mbad
  Caveat
    Returns @ TT "false" @ on DG modules whose underlying
    @ TT "A.natural" @-module is not free (in that case the
    generator differentials are not the right data to test).
  SeeAlso
    "Semifree resolutions of DG modules"
    minimalSemifreeResolution
    semifreeResolution
    (isAcyclic, DGModule)
///

doc ///
  Key
    "Computing module differentials and visualizing DG modules"
  Headline
    Differential matrices, element-wise differentials, and visualization helpers for DG modules
  Description
    Text
      Once a @ TO DGModule @ @ TT "M" @ over a @ TO DGAlgebra @
      @ TT "A" @ is built, the package supports two levels of access
      to the differential.
    Text
      The @ EM "matrix-level" @ view is @ TO moduleDifferential @: the
      call @ TT "moduleDifferential(n, M)" @ returns the hom-degree-@ TT "n" @
      piece of @ TT "d_M" @ as a homogeneous matrix over
      @ TT "A.ring" @ whose source and target are the free
      @ TT "A.ring" @-modules on the respective monomial bases of
      @ TT "M_n" @ and @ TT "M_{n-1}" @.  The block-structured variant
      @ TO moduleBlockDiff @ partitions this matrix into one summand
      per @ TT "(i, v)" @ label, where @ TT "i" @ indexes a generator
      of @ TT "M.natural" @ and @ TT "v" @ is a chunk-degree vector
      on the variables of @ TT "A.natural" @.  The pretty-printer
      @ TO displayModuleBlockDiff @ renders this labeled block matrix
      with one label per row and column.
    Text
      The @ EM "element-level" @ view is @ TT "diff(M, v)" @: apply the
      differential directly to a homogeneous element
      @ TT "v" @ of @ TT "M.natural" @ via the Leibniz rule, returning
      a Vector in @ TT "M.natural" @ of hom-degree one less.  This is
      the entry point used internally by chain-map checks and by
      @ TO (homologyClass, DGModule, Vector) @.
    Text
      Two lightweight inspection helpers summarize the generator
      layout: @ TO generatorTable @ prints a one-row-per-generator
      table with hom-degree, external degree, and differential; and
      @ TO dgModuleSummary @ tabulates, for each hom-degree in a
      requested range, the number of freshly adjoined generators at
      that degree and the rank of @ TT "F_n" @ as an
      @ TT "A.ring" @-module.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      d2 = moduleDifferential(2, Mdg)
      d1 = moduleDifferential(1, Mdg)
      d1 * d2 == 0
    Text
      Over a complete intersection, the semifree resolution of the
      residue field has well-defined differentials matching
      @ TT "d^2 = 0" @ in every degree.
  Subnodes
    moduleDifferential
    (diff, DGModule, Vector)
    (diff, DGQuotientModule, Vector)
    generatorTable
    dgModuleSummary
    moduleBlockDiff
    displayModuleBlockDiff
  SeeAlso
    "Semifree resolutions of DG modules"
    (homology, ZZ, DGModule)
///

doc ///
  Key
    moduleDifferential
    (moduleDifferential, ZZ, DGModule)
  Headline
    The hom-degree-n differential of a DG module as a matrix over the base ring
  Usage
    dn = moduleDifferential(n, M)
  Inputs
    n:ZZ
      A hom-degree.
    M:DGModule
      A DG module over a DG algebra @ TT "A" @; must be free (as in
      @ TO freeDGModule @ output) or in @ TO koszulComplexDGM @
      form.
  Outputs
    dn:Matrix
      The component @ TT "d_n : F_n -> F_{n-1}" @ of the
      differential of @ TT "M" @, as a homogeneous matrix over
      @ TT "A.ring" @.  Columns are indexed by the pairs
      @ TT "(i, m)" @ of @ TT "moduleBasisPairs(M, n)" @; rows by
      the pairs of @ TT "moduleBasisPairs(M, n-1)" @.
  Description
    Text
      Two code paths are used internally:
    Text
      When @ TT "M" @ comes from @ TO koszulComplexDGM @ (so
      @ TT "M.module" @ is set) and every generator differential is
      zero, @ TT "d_n" @ is the tensor product
      @ TT "polyDifferential(n, A) \\otimes id_{M.module}" @.  This
      is the fast path.
    Text
      For general free DG modules, @ TT "d_n" @ is computed
      monomial-by-monomial via the Leibniz rule, extracting
      @ TT "A.ring" @-linear coefficients in the target basis.
      Results are cached per hom-degree in
      @ TT "M.cache.diffs" @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      d1 = moduleDifferential(1, Mdg)
      d2 = moduleDifferential(2, Mdg)
      d1 * d2 == 0
    Text
      Every column of @ TT "d_{n-1} d_n" @ is zero, which is
      the @ TT "d^2 = 0" @ condition.
  Caveat
    Raises an error on non-free DG modules (for example, the
    natural module of a @ TO DGQuotientModule @).  In that setting,
    compute the differential at the element level via
    @ TT "diff(Q, v)" @ or work with @ TO dgComplex @.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    (diff, DGModule, Vector)
    moduleBlockDiff
    polyDifferential
    (homology, ZZ, DGModule)
///

doc ///
  Key
    (diff, DGModule, Vector)
  Headline
    Apply the DG module differential to an element
  Usage
    dv = diff(M, v)
  Inputs
    M:DGModule
      A DG module.
    v:Vector
      A homogeneous element of @ TT "M.natural" @.
  Outputs
    dv:Vector
      The image @ TT "d_M(v)" @ in @ TT "M.natural" @, computed via
      the Leibniz rule.
  Description
    Text
      Applies the Leibniz rule coordinate by coordinate.  If
      @ TT "v = sum_i f_i \\cdot e_i" @ where @ TT "e_i" @ are the
      free generators of @ TT "M.natural" @ and
      @ TT "f_i \\in A.natural" @, then
      @ TT "d_M(v) = sum_i (d_A(f_i) \\cdot e_i + (-1)^{|f_i|} f_i \\cdot d_M(e_i))" @
      where @ TT "|f_i|" @ is the hom-degree of @ TT "f_i" @.
    Text
      This element-level interface is the entry point for checking
      the cycle condition on a candidate homology class and for
      defining @ TT "DGModuleMap" @ objects without committing to the
      matrix-level bookkeeping of
      @ TO moduleDifferential @.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      diff(M, natGens#1)
      diff(M, natGens#0)
    Text
      Here @ TT "d(e_1) = x \\cdot e_0" @ and @ TT "d(e_0) = 0" @.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    moduleDifferential
    (diff, DGQuotientModule, Vector)
    (diff, DGAlgebra, RingElement)
///

doc ///
  Key
    (diff, DGQuotientModule, Vector)
  Headline
    Apply the induced differential on a DG quotient module to an element
  Usage
    dv = diff(Q, v)
  Inputs
    Q:DGQuotientModule
      A DG quotient module @ TT "Q = M / S" @.
    v:Vector
      A homogeneous element of @ TT "Q.natural" @.
  Outputs
    dv:Vector
      The image @ TT "d_Q(v)" @ in @ TT "Q.natural" @.
  Description
    Text
      Because the inclusion of @ TT "S" @ into @ TT "M" @ is a DG
      submodule (its columns are d-closed in @ TT "M" @), the
      differential of @ TT "M" @ descends to @ TT "Q" @.  This method
      applies the Leibniz rule to a representative in
      @ TT "Q.natural" @ without going through a matrix-level
      differential, which is convenient because @ TT "Q.natural" @ is
      a cokernel rather than a free module.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      S = dgSubmodule(M, (id_(M.natural))_{1})
      Q = M / S
      projection Q
      -- apply d to a natural-module generator of Q
      diff(Q, (Q.natural)_0)
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    (diff, DGModule, Vector)
    DGQuotientModule
    dgQuotientModule
///

doc ///
  Key
    generatorTable
    (generatorTable, DGModule)
  Headline
    Display the generator list of a DG module with hom-degrees and differentials
  Usage
    t = generatorTable M
  Inputs
    M:DGModule
  Outputs
    t:Net
      A centered table with one row per generator.  Columns are the
      generator index @ TT "i" @, the hom-degree, the remaining
      external degree, and the differential
      @ TT "M.diff#i" @.
  Description
    Text
      A convenient overview of the generator structure of a
      semifree DG module.  Particularly useful for inspecting the
      output of @ TO minimalSemifreeResolution @ or
      @ TO semifreeResolution @, where each iteration of
      @ TO (killCycles, DGModule) @ appends further generators.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      instance(generatorTable M, Net)
    Text
      When @ TT "M" @ has no generators, the result is a single-row
      placeholder.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    dgModuleSummary
    moduleBlockDiff
///

doc ///
  Key
    dgModuleSummary
    (dgModuleSummary, DGModule)
    (dgModuleSummary, DGModule, ZZ)
  Headline
    Tabulate hom-degree-wise generator counts and free-rank counts for a DG module
  Usage
    t = dgModuleSummary M
    t = dgModuleSummary(M, n)
  Inputs
    M:DGModule
      A free DG module (for example from @ TO freeDGModule @ or
      @ TO minimalSemifreeResolution @).
    n:ZZ
      Upper bound on the hom-degree range to display.  When omitted,
      @ TT "maxDegree M" @ is used; if that is @ TT "infinity" @ the
      method raises an error asking for an explicit bound.
  Outputs
    t:Net
      A centered table with one row per hom-degree in @ TT "0..n" @.
      Columns are the hom-degree @ TT "k" @, the number of
      generators of @ TT "M" @ placed at hom-degree @ TT "k" @ (i.e.
      adjoined in that degree), and the rank of @ TT "F_k" @ as an
      @ TT "A.ring" @-module.
  Description
    Text
      A one-call overview of how the generators of @ TT "M" @
      distribute across hom-degrees and how that translates into
      base-ring ranks.  Matches the data fed into
      @ TO moduleDifferential @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      instance(dgModuleSummary(Mdg, 3), Net)
  Caveat
    The one-argument form is only applicable when the underlying
    hom-grading of @ TT "M" @ is bounded (for example when
    @ TT "M" @ is a @ TO koszulComplexDGM @ output over a polynomial
    ring).  For arbitrary semifree DG modules, pass an explicit
    upper bound.
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    generatorTable
    moduleBlockDiff
    moduleDifferential
///

doc ///
  Key
    moduleBlockDiff
    (moduleBlockDiff, DGModule, ZZ)
  Headline
    The hom-degree-n differential of a DG module as a labeled block matrix
  Usage
    b = moduleBlockDiff(M, n)
  Inputs
    M:DGModule
      A DG module over a DG algebra @ TT "A" @; must be free.
    n:ZZ
      A hom-degree.
  Outputs
    b:Matrix
      The hom-degree-@ TT "n" @ differential of @ TT "M" @, assembled
      as a map between direct sums indexed by pairs
      @ TT "{i, v}" @ (generator index plus variable-chunk-degree
      vector).
  Description
    Text
      A block-structured refinement of @ TO moduleDifferential @.
      The source of the resulting map is a direct sum of small free
      @ TT "A.ring" @-modules, one per pair
      @ TT "{i, v}" @, where @ TT "i" @ is the generator index of
      @ TT "M.natural" @ and @ TT "v" @ is a tuple of exponents
      giving the chunk-degrees of @ TT "A.natural" @-monomials
      contributing to @ TT "F_n" @.  Composing with
      @ TT "matrix" @ recovers exactly
      @ TT "moduleDifferential(n, M)" @.
    Text
      Results are cached in
      @ TT "M.cache#\"moduleBlockDiffs\"" @ per @ TT "n" @, so
      repeated calls are cheap.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 3)
      b2 = moduleBlockDiff(Mdg, 2)
      matrix b2 == moduleDifferential(2, Mdg)
      -- Each source/target index is a {genIdx, chunkVec} pair.
      srcIdx = indices source b2
      all(srcIdx, l -> #l == 2 and instance(l#0, ZZ) and instance(l#1, List))
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    displayModuleBlockDiff
    moduleDifferential
///

doc ///
  Key
    displayModuleBlockDiff
    (displayModuleBlockDiff, DGModule, ZZ)
    (displayModuleBlockDiff, DGModule, List, List)
  Headline
    Pretty-print the labeled block matrix of a DG module differential
  Usage
    tab = displayModuleBlockDiff(M, n)
    entry = displayModuleBlockDiff(M, srcLbl, tgtLbl)
  Inputs
    M:DGModule
      A DG module; must be free.
    n:ZZ
      A hom-degree.
    srcLbl:List
      A pair @ TT "{i, v}" @ identifying a single source block (a
      generator index and a chunk-degree vector).
    tgtLbl:List
      A pair @ TT "{j, w}" @ identifying a single target block.
  Outputs
    tab:Net
      A centered table whose first row lists source labels and whose
      first column lists target labels; inner entries are the
      corresponding blocks of @ TT "moduleBlockDiff(M, n)" @.
    entry:Matrix
      In the three-argument form, returns the single block
      @ TT "b^[tgtLbl]_[srcLbl]" @ extracted from the labeled block
      matrix.
  Description
    Text
      The two-argument form lays out @ TO moduleBlockDiff @ as a
      labeled table, parallel to @ TT "displayBlockDiff" @ for DG
      algebras.  The three-argument form bypasses the table and
      returns a single block directly, which is handy for
      programmatic inspection.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      k = R^1 / ideal(x, y)
      A = koszulComplexDGA R
      Mdg = minimalSemifreeResolution(A, k, EndDegree => 2)
      instance(displayModuleBlockDiff(Mdg, 2), Net)
  SeeAlso
    "Computing module differentials and visualizing DG modules"
    moduleBlockDiff
    moduleDifferential
///

doc ///
  Key
    "Building DG algebras from existing DG algebras"
  Headline
    Base change, generator restriction, truncation, and targeted cycle-killing for DG algebras
  Description
    Text
      Given an existing @ TO DGAlgebra @ @ TT "A" @, the package
      supports several ways to derive new DG algebras from it.  All of
      these constructions preserve the graded-commutative and Leibniz
      structure by design; most perform safety checks before returning.
    Text
      @ TO baseChange @ moves a DG algebra along a ring map on its
      base: given @ TT "A" @ over @ TT "R" @ and a ring map
      @ TT "phi: R -> S" @ (or simply a target ring @ TT "S" @, in
      which case @ TT "map(S, R)" @ is used), @ TT "baseChange" @
      returns a DG algebra over @ TT "S" @ whose generator list and
      differentials are obtained by transporting scalars along
      @ TT "phi" @.
    Text
      @ TO subDGAlgebra @ produces a sub-DG algebra of @ TT "A" @
      generated by a chosen subset of the DG generators.  The subset
      must be d-closed: it is an error if the differential of a kept
      generator uses a dropped one.
      @ TO restrictDifferential @ is a synonym.
      @ TO truncateGenerators @ is a convenience wrapper:
      @ TT "truncateGenerators(A, n)" @ calls
      @ TT "subDGAlgebra" @ with the generators of hom-degree
      strictly greater than @ TT "n" @.
    Text
      @ TO killHomologyAtDegree @ targets a specific hom-degree
      @ TT "n" @: it computes minimal representatives of
      @ TT "H_n(A)" @ and adjoins variables in hom-degree
      @ TT "n+1" @ whose differentials are those representatives,
      producing a new DG algebra with @ TT "H_n = 0" @.
    Text
      All four constructions take optional arguments
      @ TO InitializeDegreeZeroHomology @ and
      @ TO InitializeComplex @ controlling how much of the
      downstream cache is eagerly populated by the internal call to
      @ TO setDiff @.
    Text
      Two low-level predicates round out the family:
      @ TO isValidDGAlgebra @ performs a structural-invariant check
      (keys and types), and
      @ TO isWellDefinedDifferential @ performs the semantic
      @ TT "d^2 = 0" @ check on every generator.  Both are called
      internally by @ TO (isWellDefined, DGAlgebra) @.
    Example
      R = QQ[x, y]
      S = R / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = baseChange(A, S)
      underlyingRing B === S
      isWellDefinedDifferential B
    Text
      Moving a Koszul DG algebra from a polynomial ring to a
      complete intersection factor preserves
      @ TT "d^2 = 0" @ because the differential expressions
      @ TT "d(T_i) = x_i" @ remain valid in @ TT "S" @.
  Subnodes
    baseChange
    subDGAlgebra
    restrictDifferential
    truncateGenerators
    killHomologyAtDegree
    isValidDGAlgebra
    isWellDefinedDifferential
  SeeAlso
    freeDGAlgebra
    koszulComplexDGA
    setDiff
    (isWellDefined, DGAlgebra)
    adjoinVariables
    killCycles
///

doc ///
  Key
    baseChange
    (baseChange, DGAlgebra, Ring)
    (baseChange, DGAlgebra, RingMap)
    [baseChange, InitializeDegreeZeroHomology]
    [baseChange, InitializeComplex]
  Headline
    Transport a DG algebra along a ring map on its base
  Usage
    B = baseChange(A, S)
    B = baseChange(A, phi)
  Inputs
    A:DGAlgebra
      A DG algebra over some ring @ TT "R" @.
    S:Ring
      A target ring.  Shorthand for @ TT "baseChange(A, map(S, R))" @.
    phi:RingMap
      A ring map with source @ TT "R = underlyingRing A" @; its target
      becomes the base ring of @ TT "B" @.
    InitializeDegreeZeroHomology => Boolean
      Whether to compute @ TT "H_0" @ of the result eagerly.  Default
      @ TT "true" @.
    InitializeComplex => Boolean
      Whether to build the underlying complex of @ TT "B" @
      eagerly.  Default @ TT "false" @.
  Outputs
    B:DGAlgebra
      A DG algebra over @ TT "target phi" @ with the same generator
      hom-degrees as @ TT "A" @, and whose differentials are the
      images of @ TT "A.diff" @ under the obvious extension of
      @ TT "phi" @ that sends each @ TT "A" @-generator to the
      corresponding @ TT "B" @-generator.
  Description
    Text
      Useful for lifting a Koszul DG algebra over a polynomial ring
      to one over a factor ring, or for specialization (via a ring
      map that kills some variables).
    Example
      R = QQ[x, y]
      A = koszulComplexDGA R
      S = R / ideal(x^2, y^2)
      B = baseChange(A, S)
      underlyingRing B === S
      isWellDefinedDifferential B
    Text
      An explicit ring map gives more flexibility:
    Example
      R = QQ[x, y]
      A = koszulComplexDGA R
      phi = map(R, R, {x, 0})
      Bphi = baseChange(A, phi)
      underlyingRing Bphi === R
  Caveat
    Raises an error if @ TT "source phi" @ is not equal to
    @ TT "underlyingRing A" @.
  SeeAlso
    "Building DG algebras from existing DG algebras"
    (symbol **, DGAlgebra, Ring)
    InitializeDegreeZeroHomology
    InitializeComplex
///

doc ///
  Key
    subDGAlgebra
    (subDGAlgebra, DGAlgebra, List)
    [subDGAlgebra, InitializeDegreeZeroHomology]
    [subDGAlgebra, InitializeComplex]
  Headline
    The sub-DG algebra generated by a chosen subset of DG generators
  Usage
    B = subDGAlgebra(A, keepIdx)
  Inputs
    A:DGAlgebra
    keepIdx:List
      A list of integers giving the indices (into
      @ TT "0 .. numgens A.natural - 1" @) of the DG generators to
      keep.  Duplicates and out-of-range indices raise an error.
    InitializeDegreeZeroHomology => Boolean
      Whether to compute @ TT "H_0" @ of the result eagerly.
      Default @ TT "true" @.
    InitializeComplex => Boolean
      Whether to build the underlying complex eagerly.  Default
      @ TT "false" @.
  Outputs
    B:DGAlgebra
      A DG algebra over @ TT "A.ring" @ whose DG generators are the
      @ TT "keepIdx" @-indexed generators of @ TT "A" @ in sorted
      order, with their differentials restricted accordingly.
  Description
    Text
      The subset @ TT "keepIdx" @ must be d-closed: if the
      differential of a kept generator uses a dropped generator,
      @ TT "subDGAlgebra" @ raises an error rather than silently
      returning a non-DG object.  This protects downstream code
      from accidentally producing inconsistent structures.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = subDGAlgebra(A, {0})
      numgens B.natural
    Text
      Dropping a generator whose image is referenced in a kept
      generator's differential is caught:
    Example
      R = QQ[x] / ideal(x^2)
      A = koszulComplexDGA R
      A' = adjoinVariables(A, {x * (gens A.natural)#0})
      try subDGAlgebra(A', {1}) else "error: kept differential uses dropped generator"
  Caveat
    Duplicate or out-of-range entries in @ TT "keepIdx" @ raise
    an error.  @ TT "keepIdx" @ is internally sorted, so the
    generator order of the output depends only on which indices
    appear.
  SeeAlso
    "Building DG algebras from existing DG algebras"
    restrictDifferential
    truncateGenerators
///

doc ///
  Key
    restrictDifferential
    (restrictDifferential, DGAlgebra, List)
    [restrictDifferential, InitializeDegreeZeroHomology]
    [restrictDifferential, InitializeComplex]
  Headline
    Synonym for subDGAlgebra, named to match the restricted-differential vocabulary
  Usage
    B = restrictDifferential(A, keepIdx)
  Inputs
    A:DGAlgebra
    keepIdx:List
      List of integers giving DG generator indices to keep.
    InitializeDegreeZeroHomology => Boolean
      Default @ TT "true" @.
    InitializeComplex => Boolean
      Default @ TT "false" @.
  Outputs
    B:DGAlgebra
      Identical to @ TT "subDGAlgebra(A, keepIdx)" @.
  Description
    Text
      Calls directly into @ TO subDGAlgebra @.  The two are
      interchangeable; @ TT "restrictDifferential" @ is provided so
      that calling code emphasizing the restricted-differential
      aspect reads more naturally.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = restrictDifferential(A, {0})
      numgens B.natural
  SeeAlso
    "Building DG algebras from existing DG algebras"
    subDGAlgebra
///

doc ///
  Key
    truncateGenerators
    (truncateGenerators, DGAlgebra, ZZ)
    [truncateGenerators, InitializeDegreeZeroHomology]
    [truncateGenerators, InitializeComplex]
  Headline
    Restrict a DG algebra to its generators of hom-degree strictly above a bound
  Usage
    B = truncateGenerators(A, n)
  Inputs
    A:DGAlgebra
    n:ZZ
      A hom-degree bound; generators of hom-degree @ TT "<= n" @ are
      dropped.
    InitializeDegreeZeroHomology => Boolean
      Default @ TT "true" @.
    InitializeComplex => Boolean
      Default @ TT "false" @.
  Outputs
    B:DGAlgebra
      The sub-DG algebra of @ TT "A" @ generated by the DG
      generators whose first-hom-degree component is strictly
      greater than @ TT "n" @.
  Description
    Text
      Computes @ TT "keepIdx" @ as
      @ TT "select(0 .. #A.Degrees - 1, i -> first A.Degrees#i > n)" @
      and delegates to @ TO subDGAlgebra @.  As with
      @ TO subDGAlgebra @, the subset must be d-closed; if the
      differential of a generator @ TT "T_i" @ of hom-degree
      @ TT "> n" @ uses a generator of hom-degree @ TT "<= n" @ that
      would be dropped, an error is raised.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = truncateGenerators(A, 0)
      numgens B.natural
    Text
      The Koszul algebra has all generators in hom-degree @ TT "1" @
      so truncating below degree @ TT "0" @ is a no-op in this
      case.
  SeeAlso
    "Building DG algebras from existing DG algebras"
    subDGAlgebra
///

doc ///
  Key
    killHomologyAtDegree
    (killHomologyAtDegree, DGAlgebra, ZZ)
  Headline
    Adjoin variables killing the homology of a DG algebra at a prescribed degree
  Usage
    B = killHomologyAtDegree(A, n)
  Inputs
    A:DGAlgebra
    n:ZZ
      The hom-degree at which to kill homology.
  Outputs
    B:DGAlgebra
      Either @ TT "A" @ itself (if @ TT "H_n(A) = 0" @) or a new DG
      algebra obtained from @ TT "A" @ by adjoining variables in
      hom-degree @ TT "n + 1" @ whose differentials are minimal
      representatives of @ TT "H_n(A)" @.
  Description
    Text
      Unlike @ TO killCycles @, which searches for the first
      nontrivial degree upward from @ TT "StartDegree" @, this
      method targets a specific degree.  It computes the minimal
      generators of @ TT "H_n(A)" @ using the pruning map of
      @ TT "prune homology(n, A)" @, lifts them to cycles in
      @ TT "A_n" @, and adjoins via @ TO adjoinVariables @.
    Example
      R = QQ[x, y]
      A = koszulComplexDGA R
      killHomologyAtDegree(A, 1) === A
    Text
      Over a regular ring, @ TT "H_1" @ vanishes already, so the
      method short-circuits.  Over a complete intersection, it
      adjoins new generators:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      B = killHomologyAtDegree(A, 1)
      numgens B.natural > numgens A.natural
      prune homology(1, B)
  SeeAlso
    "Building DG algebras from existing DG algebras"
    killCycles
    adjoinVariables
    acyclicClosure
///

doc ///
  Key
    isValidDGAlgebra
    (isValidDGAlgebra, DGAlgebra)
  Headline
    Structural invariant check for a DG algebra
  Usage
    b = isValidDGAlgebra A
  Inputs
    A:DGAlgebra
  Outputs
    b:Boolean
      @ TT "true" @ when @ TT "A" @ has all required keys of the
      expected types (ring, natural algebra, degree list,
      differential list, cache table).
  Description
    Text
      A lightweight structural check, intended to guard
      downstream code against hand-assembled or corrupted
      @ TT "DGAlgebra" @ hash tables.  It does @ EM "not" @ check
      the @ TT "d^2 = 0" @ condition; for that, use
      @ TO isWellDefinedDifferential @ or the user-facing
      @ TO (isWellDefined, DGAlgebra) @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      isValidDGAlgebra A
  SeeAlso
    "Building DG algebras from existing DG algebras"
    isWellDefinedDifferential
    (isWellDefined, DGAlgebra)
///

doc ///
  Key
    isWellDefinedDifferential
    (isWellDefinedDifferential, DGAlgebra)
    (isWellDefinedDifferential, DGModule)
  Headline
    Semantic check that d^2 = 0 for a DG algebra or DG module
  Usage
    b = isWellDefinedDifferential A
    b = isWellDefinedDifferential M
  Inputs
    A:DGAlgebra
      A DG algebra.
    M:DGModule
      A DG module.
  Outputs
    b:Boolean
      @ TT "true" @ when the composition
      @ TT "d_{n-1} * d_n" @ is zero for every relevant
      @ TT "n" @.
  Description
    Text
      The @ EM "semantic" @ half of the well-definedness check: it
      verifies that the differential squares to zero on every
      generator.  The structural half
      (@ TO isValidDGAlgebra @, and the corresponding
      @ TT "isValidDGModule" @ for modules) is called first as a
      pre-condition.
    Text
      This is the primitive called internally by
      @ TO (isWellDefined, DGAlgebra) @ and the user-facing
      @ TO (isWellDefined, DGModule) @.  It is exported for
      low-level use when only the @ TT "d^2 = 0" @ half of
      well-definedness needs to be checked.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      isWellDefinedDifferential A
      M = freeDGModule(A, {0})
      isWellDefinedDifferential M
  SeeAlso
    "Building DG algebras from existing DG algebras"
    "Well-definedness, acyclicity, and quasi-isomorphism"
    isValidDGAlgebra
    (isWellDefined, DGAlgebra)
    (isWellDefined, DGModule)
///

doc ///
  Key
    "Low-level differential computations and validity checks"
  Headline
    Primitives behind DG algebra and DG module differentials
  Description
    Text
      The methods documented here implement the raw computations
      that higher-level DG algebra and DG module routines rely on.
      They split into three groups.

      The first group -- @ TO polyDifferential @ and
      @ TO polyDiffMonomial @ -- applies the Leibniz rule directly
      to elements of @ TT "A.natural" @ to produce the DG algebra
      differential.  @ TT "polyDifferential(n, A)" @ returns the
      per-degree matrix @ TT "A_n --> A_{n-1}" @ over the base ring
      and caches it inside @ TT "A" @; @ TT "polyDifferential(A, f)" @
      applies @ TT "d" @ to a single element by summing over its
      terms; @ TT "polyDiffMonomial(A, m)" @ is the monomial kernel
      that powers both.

      The second group -- @ TO dgComplex @ -- packages the per-degree
      differentials of a DG algebra or DG module as a
      @ TO2{Complex, "Complex"} @ of free modules over the base ring.
      The result is cached and invalidated by
      @ TO invalidateDGAlgebraCache @.

      The third group -- @ TO isValidDGModule @ and
      @ TO isDGSubmodule @ -- checks structural invariants.
      @ TT "isValidDGModule" @ parallels @ TO isValidDGAlgebra @ and
      confirms that the required keys are present with the expected
      types; @ TT "isDGSubmodule" @ tests whether a given inclusion
      matrix has @ TT "d" @-closed image without performing the
      @ TT "d" @-saturation that @ TO dgSubmodule @ would.

      Finally, @ TO2{(getBoundaryPreimage, DGModule, Vector), "getBoundaryPreimage"} @
      lifts boundaries through the module differential, returning a
      preimage when one exists and a residue otherwise.
  Subnodes
    polyDifferential
    polyDiffMonomial
    dgComplex
    isValidDGModule
    isDGSubmodule
    (getBoundaryPreimage, DGModule, Vector)
    (getBoundaryPreimage, DGModule, List)
  SeeAlso
    "Basic operations on DG Algebras"
    "Building DG modules, submodules, and quotients"
    "Well-definedness, acyclicity, and quasi-isomorphism"
///

doc ///
  Key
    polyDifferential
    (polyDifferential, ZZ, DGAlgebra)
    (polyDifferential, DGAlgebra, RingElement)
  Headline
    Matrix and element-level differentials of a DG algebra
  Usage
    d = polyDifferential(n, A)
    b = polyDifferential(A, f)
  Inputs
    n:ZZ
      A homological degree.
    A:DGAlgebra
    f:RingElement
      An element of @ TT "A.natural" @.
  Outputs
    d:Matrix
      The per-degree map @ TT "A_n --> A_{n-1}" @ over
      @ TT "A.ring" @ (first form).
    b:RingElement
      The image @ TT "d(f)" @ in @ TT "A.natural" @ (second form).
  Description
    Text
      The two-argument form @ TT "polyDifferential(n, A)" @ returns
      the matrix of the DG algebra differential in hom-degree
      @ TT "n" @, expressed in the monomial basis of @ TT "A_n" @
      and @ TT "A_{n-1}" @.  It is cached inside @ TT "A" @ and
      reused on subsequent calls; @ TO invalidateDGAlgebraCache @
      clears the cache.  Out-of-range degrees are handled
      uniformly: degree @ TT "0" @ gives the conventional zero map
      @ TT "R^1 --> R^0" @, and degrees past @ TT "maxDegree A" @
      give zero maps of the appropriate shape.
    Example
      R = QQ[x, y, z]
      A = koszulComplexDGA R
      d1 = polyDifferential(1, A)
      d2 = polyDifferential(2, A)
      d3 = polyDifferential(3, A)
      assert(d1 * d2 == 0)
      assert(d2 * d3 == 0)
    Text
      The zero-degree map is the conventional @ TT "R^1 --> R^0" @:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      d0 = polyDifferential(0, A)
      assert(source d0 == R^1 and target d0 == R^0)
    Text
      The element form @ TT "polyDifferential(A, f)" @ applies
      @ TT "d" @ to an arbitrary element by summing over its
      monomial terms:
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      gs = gens A.natural
      polyDifferential(A, gs#0 * gs#1)
  SeeAlso
    "Low-level differential computations and validity checks"
    polyDiffMonomial
    (diff, DGAlgebra, RingElement)
    invalidateDGAlgebraCache
///

doc ///
  Key
    polyDiffMonomial
    (polyDiffMonomial, DGAlgebra, RingElement)
  Headline
    Leibniz rule on a single monomial of a DG algebra
  Usage
    b = polyDiffMonomial(A, m)
  Inputs
    A:DGAlgebra
    m:RingElement
      A monomial of @ TT "A.natural" @ (or the zero element).
  Outputs
    b:RingElement
      The image @ TT "d(m)" @ in @ TT "A.natural" @.
  Description
    Text
      @ TT "polyDiffMonomial" @ is the monomial kernel of the
      DG algebra differential.  It splits @ TT "m" @ into its
      coefficient and its support powers and then applies the
      graded Leibniz rule variable by variable, keeping track of
      the sign produced when @ TT "d" @ passes an odd-degree
      generator.  The routine handles the zero element without
      raising an exception, returning @ TT "0" @ in @ TT "A.natural" @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      assert(polyDiffMonomial(A, 0_(A.natural)) == 0_(A.natural))
      gs = gens A.natural
      polyDiffMonomial(A, gs#0 * gs#1)
    Text
      On a two-variable Koszul generator product, the Leibniz rule
      gives @ TT "d(T_1 T_2) = d(T_1) T_2 - T_1 d(T_2) = x T_2 - y T_1" @:
    Example
      r = polyDiffMonomial(A, gs#0 * gs#1)
      assert(r == x * gs#1 - y * gs#0)
  SeeAlso
    "Low-level differential computations and validity checks"
    polyDifferential
    (diff, DGAlgebra, RingElement)
///

doc ///
  Key
    dgComplex
    (dgComplex, DGAlgebra)
    (dgComplex, DGModule)
  Headline
    Package a DG algebra or DG module as a Complex of free base-ring modules
  Usage
    C = dgComplex A
    C = dgComplex M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    C:Complex
      A @ TO Complex @ of free @ TT "A.ring" @-modules whose
      @ TT "i" @-th module is @ TT "A_i" @ (resp.\ @ TT "M_i" @)
      and whose differential is @ TT "polyDifferential" @
      (resp.\ @ TO moduleDifferential @).
  Description
    Text
      @ TT "dgComplex" @ assembles the per-degree differentials of
      a DG algebra or DG module into a single
      @ TO2{Complex, "Complex"} @ object.  The result is cached on
      the input; calling @ TT "dgComplex" @ again returns exactly
      the same complex, and @ TO invalidateDGAlgebraCache @ clears
      the cache so that the next call rebuilds the complex.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      C1 = dgComplex A
      C2 = dgComplex A
      assert(C1 === C2)
      invalidateDGAlgebraCache A
      assert(not A.cache#?(symbol dgComplex))
      C3 = dgComplex A
      assert(instance(C3, Complex))
    Text
      When @ TT "maxDegree" @ is infinite -- for example on the
      acyclic closure of a non-regular ring -- @ TT "dgComplex" @
      truncates at a canonical finite bound so that a
      @ TO Complex @ is always produced.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = acyclicClosure(R, EndDegree => 2)
      assert(instance(dgComplex A, Complex))
    Text
      The DG module form is parallel: it builds the complex whose
      terms are @ TT "M_i" @ and whose differentials are the maps
      returned by @ TO moduleDifferential @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      D1 = dgComplex KM
      D2 = dgComplex KM
      assert(D1 === D2)
  SeeAlso
    "Low-level differential computations and validity checks"
    polyDifferential
    moduleDifferential
    invalidateDGAlgebraCache
    toComplex
///

doc ///
  Key
    isValidDGModule
    (isValidDGModule, DGModule)
  Headline
    Structural-invariant check on a DG module
  Usage
    b = isValidDGModule M
  Inputs
    M:DGModule
  Outputs
    b:Boolean
      @ TT "true" @ if the stored @ TT "DGModule" @ has all
      required keys with the expected types; @ TT "false" @
      otherwise.
  Description
    Text
      @ TT "isValidDGModule" @ parallels @ TO isValidDGAlgebra @:
      it confirms that @ TT "M" @ carries the keys
      @ TT "dgAlgebra" @, @ TT "ring" @, @ TT "natural" @,
      @ TT "Degrees" @, and @ TT "diff" @ with the expected types,
      that the referenced @ TT "DGAlgebra" @ is itself structurally
      valid, and that the generator-degree list has the same length
      as the differential list.  It does not verify the semantic
      property @ TT "d_M^2 = 0" @; that is the job of
      @ TO isWellDefinedDifferential @, and the two together are
      what the user-facing @ TO (isWellDefined, DGModule) @ calls.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      assert(isValidDGModule KM)
  SeeAlso
    "Low-level differential computations and validity checks"
    "Well-definedness, acyclicity, and quasi-isomorphism"
    isValidDGAlgebra
    isWellDefinedDifferential
    (isWellDefined, DGModule)
///

doc ///
  Key
    isDGSubmodule
    (isDGSubmodule, DGModule, Matrix)
  Headline
    Test whether the image of an inclusion matrix is d-closed
  Usage
    b = isDGSubmodule(M, incMat)
  Inputs
    M:DGModule
    incMat:Matrix
      A matrix whose target is @ TT "M.natural" @.
  Outputs
    b:Boolean
      @ TT "true" @ if the column span of @ TT "incMat" @ is
      closed under the differential of @ TT "M" @; @ TT "false" @
      otherwise.
  Description
    Text
      @ TT "isDGSubmodule" @ checks whether a matrix of candidate
      generators spans a DG submodule: it assembles
      @ TT "d_M" @ applied column-wise and then verifies that the
      result lies in the column span of @ TT "incMat" @ via a
      single @ TT "//" @ solve.  Unlike @ TO dgSubmodule @, it
      does not saturate: a submodule that is not @ TT "d" @-closed
      is reported as such rather than enlarged.  The empty matrix
      is always @ TT "d" @-closed.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      idM = identityDGModuleMap M
      imF = image idM
      assert(isDGSubmodule(target idM, (inclusion imF).natural))
      kerF = kernel idM
      assert(isDGSubmodule(source idM, (inclusion kerF).natural))
  SeeAlso
    "Low-level differential computations and validity checks"
    dgSubmodule
    (image, DGModuleMap)
    (kernel, DGModuleMap)
///

doc ///
  Key
    (getBoundaryPreimage, DGModule, Vector)
  Headline
    Lift a boundary in a DG module to a preimage under the differential
  Usage
    (lifted, myLift) = getBoundaryPreimage(M, b)
  Inputs
    M:DGModule
      A free DG module over a @ TO DGAlgebra @.
    b:Vector
      A homogeneous element of @ TT "M.natural" @.
  Outputs
    seq:Sequence
      A pair @ TT "(lifted, myLift)" @.  If @ TT "lifted" @ is
      @ TT "true" @, then @ TT "myLift" @ is an element with
      @ TT "d_M(myLift) = b" @; otherwise @ TT "myLift" @ is the
      residue @ TT "b - d_M(liftAttempt)" @, which records how far
      @ TT "b" @ fails to be a boundary.
  Description
    Text
      @ TT "getBoundaryPreimage" @ for DG modules is the module
      analogue of @ TO (getBoundaryPreimage, DGAlgebra, RingElement) @:
      it attempts to solve @ TT "d_M(x) = b" @ for @ TT "x" @, using
      the matrix @ TT "moduleDifferential(homDegree(b) + 1, M)" @
      and a single @ TT "//" @ solve.  Over a general ring such a
      solve may fail to find a preimage even when one exists, so a
      @ TT "true" @ answer is a guarantee but a @ TT "false" @
      answer only says "the particular preimage candidate we tried
      left a residue".  The returned residue is
      @ TT "b - d_M(liftAttempt)" @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      -- d(e_1) = x*e_0, so x*e_0 is a boundary with preimage e_1.
      b = x * natGens#0
      (ok, pre) = getBoundaryPreimage(M, b)
      assert(ok === true)
      assert(diff(M, pre) == b)
    Text
      On a non-boundary, the first coordinate is @ TT "false" @
      and the second coordinate records the residue:
    Example
      M2 = freeDGModule(A, {0})
      ng2 = apply(rank M2.natural, i -> (M2.natural)_i)
      (ok2, residue) = getBoundaryPreimage(M2, ng2#0)
      assert(ok2 === false)
      assert(residue != 0)
  SeeAlso
    "Low-level differential computations and validity checks"
    (getBoundaryPreimage, DGModule, List)
    getBoundaryPreimage
    moduleDifferential
    homologyClass
///

doc ///
  Key
    (getBoundaryPreimage, DGModule, List)
  Headline
    Lift a list of boundaries sharing a hom-degree through the module differential
  Usage
    (lifted, lifts) = getBoundaryPreimage(M, boundaryList)
  Inputs
    M:DGModule
      A free DG module over a @ TO DGAlgebra @.
    boundaryList:List
      A list of elements of @ TT "M.natural" @, each either
      @ TT "0" @ or homogeneous of a common hom-degree.
  Outputs
    seq:Sequence
      A pair @ TT "(lifted, lifts)" @.  If @ TT "lifted" @ is
      @ TT "true" @, then @ TT "lifts#i" @ satisfies
      @ TT "d_M(lifts#i) = boundaryList#i" @ for every @ TT "i" @;
      otherwise @ TT "lifts" @ is the list of per-entry residues.
  Description
    Text
      The list form of @ TO (getBoundaryPreimage, DGModule, Vector) @
      solves multiple preimage problems in a single
      @ TT "//" @ call.  Zero entries are permitted and contribute
      zero columns; all nonzero entries must share a hom-degree.
      When every boundary lifts, the returned list preserves the
      order of @ TT "boundaryList" @.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x * natGens#0})
      b = x * natGens#0
      (ok, lifts) = getBoundaryPreimage(M, {b, 0_(M.natural)})
      assert(ok === true)
      assert(diff(M, lifts#0) == b)
      assert(diff(M, lifts#1) == 0)
  SeeAlso
    "Low-level differential computations and validity checks"
    (getBoundaryPreimage, DGModule, Vector)
    getBoundaryPreimage
    moduleDifferential
///

doc ///
  Key
    "Accessors and cache management"
  Headline
    Reading state out of a DG algebra or DG module and maintaining its caches
  Description
    Text
      A @ TO DGAlgebra @ or @ TO DGModule @ stores its defining
      data in a small collection of keys -- the base ring, the
      graded-commutative algebra, the generator degrees, and the
      list of generator differentials -- together with a cache of
      derived information (per-degree differential matrices,
      homology, the homology algebra, a @ TO Complex @
      presentation).  This page collects the two groups of
      utilities that manage that state.

      The first group -- @ TO underlyingRing @,
      @ TO underlyingAlgebra @, @ TO differential @, and
      @ TO generatorDegrees @ -- returns the four defining
      ingredients.  Because both DG algebras and DG modules carry
      the same names, code can remain agnostic about which one it
      is handling.

      The second group -- @ TO ensureDGAlgebraCaches @ and
      @ TO invalidateDGAlgebraCache @ -- maintains the cache
      sub-tables.  @ TT "ensureDGAlgebraCaches" @ is idempotent
      and guarantees that the standard cache keys are present
      with the expected types; @ TO invalidateDGAlgebraCache @
      wipes every cached computed value so that the next call
      recomputes from scratch.  Routines such as @ TO setDiff @
      that mutate the differential call
      @ TO invalidateDGAlgebraCache @ internally so that stale
      homology and stale differential matrices are never served.
  Subnodes
    underlyingRing
    underlyingAlgebra
    differential
    generatorDegrees
    ensureDGAlgebraCaches
    invalidateDGAlgebraCache
  SeeAlso
    "Basic operations on DG Algebras"
    "Building DG modules, submodules, and quotients"
    "Low-level differential computations and validity checks"
///

doc ///
  Key
    underlyingRing
    (underlyingRing, DGAlgebra)
    (underlyingRing, DGModule)
  Headline
    The commutative base ring of a DG algebra or DG module
  Usage
    S = underlyingRing A
    S = underlyingRing M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    S:Ring
      The commutative base ring @ TT "A.ring" @ or @ TT "M.ring" @
      over which the DG structure is defined.
  Description
    Text
      @ TT "underlyingRing" @ returns the base ring.  This is the
      ring @ TT "R" @ in the DG algebra @ TT "A = R\\langle T_1, T_2, \\ldots \\rangle" @
      or the DG module @ TT "M" @, and it is the ring over which
      every matrix returned by @ TO polyDifferential @ or
      @ TO moduleDifferential @ is built.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      assert(underlyingRing A === R)
    Example
      M = koszulComplexDGM R^1
      assert(underlyingRing M === R)
  SeeAlso
    "Accessors and cache management"
    underlyingAlgebra
    differential
    generatorDegrees
///

doc ///
  Key
    underlyingAlgebra
    (underlyingAlgebra, DGAlgebra)
    (underlyingAlgebra, DGModule)
  Headline
    The graded-commutative algebra carrying the DG structure
  Usage
    N = underlyingAlgebra A
    N = underlyingAlgebra M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    N:Ring
      The graded-commutative ring @ TT "A.natural" @
      (the DG algebra's own ring) or, for a DG module,
      the graded free module @ TT "M.natural" @
      (as a @ TO Module @ over @ TT "A.natural" @).
  Description
    Text
      For a @ TO DGAlgebra @, @ TT "underlyingAlgebra" @ returns the
      graded-commutative ring @ TT "A.natural" @ -- the ring whose
      elements one manipulates when writing polynomials in the DG
      generators.  For a @ TO DGModule @ it returns the underlying
      @ TT "A.natural" @-module @ TT "M.natural" @ on which the
      differential acts.  In both cases the result is what you
      typically @ TT "use" @ before referring to DG generators by
      name.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      assert(underlyingAlgebra A === A.natural)
      numgens underlyingAlgebra A
    Example
      M = koszulComplexDGM R^1
      assert(underlyingAlgebra M === M.natural)
  SeeAlso
    "Accessors and cache management"
    underlyingRing
    differential
    generatorDegrees
///

doc ///
  Key
    differential
    (differential, DGAlgebra)
    (differential, DGModule)
  Headline
    The list of generator differentials stored in a DG algebra or DG module
  Usage
    d = differential A
    d = differential M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    d:List
      The stored list of differentials: for a @ TO DGAlgebra @,
      one element of @ TT "A.natural" @ per DG generator; for a
      @ TO DGModule @, one element of @ TT "M.natural" @ per
      natural generator.
  Description
    Text
      @ TT "differential" @ returns the raw list of generator
      differentials that was installed by @ TO setDiff @.  The
      @ TT "i" @-th entry is the image of the @ TT "i" @-th DG
      generator under @ TT "d" @, and every other differential is
      computed from it by linearity and the graded Leibniz rule.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      differential A
      assert(differential A === A.diff)
    Example
      M = koszulComplexDGM R^1
      differential M
  SeeAlso
    "Accessors and cache management"
    setDiff
    polyDifferential
    moduleDifferential
///

doc ///
  Key
    generatorDegrees
    (generatorDegrees, DGAlgebra)
    (generatorDegrees, DGModule)
  Headline
    The hom-degrees (and optional internal degrees) of the DG generators
  Usage
    degs = generatorDegrees A
    degs = generatorDegrees M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    degs:List
      A list of degree vectors, one per DG generator (for a DG
      algebra) or one per natural generator (for a DG module).
  Description
    Text
      @ TT "generatorDegrees" @ returns the degree data that was
      supplied at construction.  Each entry is a list whose first
      coordinate is the homological degree and whose remaining
      coordinates record the internal (multi-)grading of the base
      ring.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      generatorDegrees A
    Example
      M = freeDGModule(A, {0, 1, 3})
      generatorDegrees M
  SeeAlso
    "Accessors and cache management"
    underlyingRing
    underlyingAlgebra
    differential
///

doc ///
  Key
    ensureDGAlgebraCaches
    (ensureDGAlgebraCaches, DGAlgebra)
    (ensureDGAlgebraCaches, DGModule)
  Headline
    Guarantee that the standard cache sub-tables are present
  Usage
    A = ensureDGAlgebraCaches A
    M = ensureDGAlgebraCaches M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    :{DGAlgebra,DGModule}
      The same object, with any missing standard cache sub-table
      created (the method mutates the input and returns it).
  Description
    Text
      @ TT "ensureDGAlgebraCaches" @ is idempotent: it creates the
      cache sub-tables that the rest of the package expects (the
      @ TT "homology" @, @ TT "homologyAlgebra" @, and
      @ TT "diffs" @ sub-tables for a DG algebra; the @ TT "diffs" @
      sub-table for a DG module), and leaves any that already exist
      untouched.  Code that reads from the cache can call this
      method once up front and thereafter assume the sub-tables are
      present with the right types, rather than defensively
      checking each access.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      ensureDGAlgebraCaches A
      assert(instance(A.cache, CacheTable))
      -- idempotent: calling twice leaves the cache intact
      ensureDGAlgebraCaches A
      assert(instance(A.cache, CacheTable))
    Example
      KM = koszulComplexDGM R^1
      ensureDGAlgebraCaches KM
      assert(instance(KM.cache, CacheTable))
  SeeAlso
    "Accessors and cache management"
    invalidateDGAlgebraCache
///

doc ///
  Key
    invalidateDGAlgebraCache
    (invalidateDGAlgebraCache, DGAlgebra)
    (invalidateDGAlgebraCache, DGModule)
  Headline
    Discard cached derived data so that it is recomputed from scratch
  Usage
    A = invalidateDGAlgebraCache A
    M = invalidateDGAlgebraCache M
  Inputs
    A:DGAlgebra
    M:DGModule
  Outputs
    :{DGAlgebra,DGModule}
      The same object with every cached derived value cleared
      (the method mutates the input and returns it).
  Description
    Text
      @ TT "invalidateDGAlgebraCache" @ wipes every cache entry
      that records a value derived from the differential: per-degree
      differential matrices, per-degree homology, the homology
      algebra, and any @ TO Complex @ or block-diff summary cached
      by @ TO dgComplex @ or @ TO displayBlockDiff @.  The cache
      sub-tables themselves are recreated empty, so subsequent
      lookups recompute the values rather than returning stale
      results.  Routines that mutate the differential (most
      notably @ TO setDiff @) call this function internally.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      A = koszulComplexDGA R
      C1 = dgComplex A
      invalidateDGAlgebraCache A
      assert(not A.cache#?(symbol dgComplex))
      C2 = dgComplex A
      assert(instance(C2, Complex))
    Example
      KM = koszulComplexDGM R^1
      D1 = dgComplex KM
      invalidateDGAlgebraCache KM
      assert(not KM.cache#?(symbol dgComplex))
      D2 = dgComplex KM
      assert(instance(D2, Complex))
  SeeAlso
    "Accessors and cache management"
    ensureDGAlgebraCaches
    setDiff
    dgComplex
///

doc ///
  Key
    "DG module primitives and chain-complex export"
  Headline
    Installing differentials, reading degree data, and exporting to Complex
  Description
    Text
      This page collects the remaining DG module primitives
      (installing a differential, asking for the largest
      hom-degree, reading off a per-degree basis) together with
      the methods that export a DG algebra, DG module, DG module
      map, or DG ideal to the ordinary
      @ TO2{Complex, "Complex"} @ / @ TO2{ComplexMap, "ComplexMap"} @
      world.

      Installing a differential.  @ TO (setDiff, DGModule, List) @
      is the DG module analogue of @ TO (setDiff, DGAlgebra, List) @:
      it records the image of each natural generator under
      @ TT "d_M" @ and invalidates every cached derived value.

      Reading degree data.  @ TO (maxDegree, DGModule) @ and
      @ TO (maxDegree, DGQuotientModule) @ report the largest
      hom-degree present (or @ TT "infinity" @ if the underlying
      DG algebra has an even-degree generator).
      @ TO (getBasis, ZZ, DGModule) @ returns an
      @ TT "A.ring" @-basis of the hom-degree-@ TT "n" @ piece of
      @ TT "M.natural" @.

      Exporting to @ TO Complex @.  The @ TO toComplex @ family
      packages per-degree differentials as a plain
      @ TO Complex @ of free base-ring modules; the
      @ TO toComplexMap @ family packages per-degree induced maps
      as a @ TO ComplexMap @.  Together they let the chain-level
      results of the DG machinery be handed off to the rest of
      the @ TO2{Complexes, "Complexes"} @ package.

      Block-diagonal display.  @ TO displayBlockDiff @ prints the
      per-internal-multidegree block decomposition of a DG
      algebra differential at a given homological degree,
      convenient for inspecting the acyclic closure of a
      multigraded quotient.
  Subnodes
    (setDiff, DGModule, List)
    (maxDegree, DGModule)
    (maxDegree, DGQuotientModule)
    (getBasis, ZZ, DGModule)
    (toComplex, DGModule)
    (toComplex, DGModule, ZZ)
    (toComplex, DGQuotientModule)
    (toComplex, DGIdeal)
    (toComplexMap, DGModuleMap)
    (toComplexMap, DGModuleMap, ZZ)
    displayBlockDiff
  SeeAlso
    "Basic operations on DG Algebras"
    "Building DG modules, submodules, and quotients"
    "Low-level differential computations and validity checks"
///

doc ///
  Key
    (setDiff, DGModule, List)
  Headline
    Install the list of generator differentials on a free DG module
  Usage
    setDiff(M, diffList)
  Inputs
    M:DGModule
      A free DG module (from @ TO freeDGModule @).
    diffList:List
      One entry per natural generator of @ TT "M" @.  Each entry
      is a vector of @ TT "M.natural" @ (or @ TT "0" @, or a
      one-column matrix that will be folded into a vector).
  Outputs
    :DGModule
      The same @ TT "M" @, mutated to carry the new differentials.
  Description
    Text
      @ TT "setDiff" @ is the DG module analogue of
      @ TO (setDiff, DGAlgebra, List) @: it records the image of
      the @ TT "i" @-th natural generator under the differential in
      the @ TT "i" @-th slot of @ TT "M.diff" @, and clears every
      cached derived value so that later calls to
      @ TO moduleDifferential @ or @ TO homology @ will recompute
      from the newly installed data.  Only free DG modules (those
      produced by @ TO freeDGModule @) are accepted; the list
      length must match the number of natural generators.
    Example
      R = QQ[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      natGens = apply(rank M.natural, i -> (M.natural)_i)
      setDiff(M, {0, x^2 * natGens#0})
      d1 = moduleDifferential(1, M)
      d2 = moduleDifferential(2, M)
      assert(d1 * d2 == 0)
  SeeAlso
    "DG module primitives and chain-complex export"
    freeDGModule
    (setDiff, DGAlgebra, List)
    moduleDifferential
    invalidateDGAlgebraCache
///

doc ///
  Key
    (maxDegree, DGModule)
  Headline
    Largest hom-degree present in a DG module
  Usage
    n = maxDegree M
  Inputs
    M:DGModule
  Outputs
    n:{ZZ,InfiniteNumber}
      @ TT "maxDegree A + max(first d | d in M.Degrees)" @ when
      @ TT "A" @ has finite max hom-degree; @ TT "infinity" @
      otherwise.
  Description
    Text
      @ TT "maxDegree" @ of a DG module combines the hom-degree
      bound of the ambient @ TO DGAlgebra @ with the largest
      hom-degree shift of a natural generator.  When the
      underlying DG algebra has an even-degree generator it has
      @ TT "infinity" @ as its max degree, and so does every DG
      module over it.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      assert(maxDegree KM == maxDegree KM.dgAlgebra)
    Example
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 3})
      maxDegree M
  SeeAlso
    "DG module primitives and chain-complex export"
    maxDegree
    (maxDegree, DGQuotientModule)
    (maxDegree, DGAlgebra)
///

doc ///
  Key
    (maxDegree, DGQuotientModule)
  Headline
    Largest hom-degree present in a DG quotient module
  Usage
    n = maxDegree Q
  Inputs
    Q:DGQuotientModule
  Outputs
    n:{ZZ,InfiniteNumber}
      The @ TT "maxDegree" @ of the ambient DG module
      @ TT "Q.ambient" @.
  Description
    Text
      Passing a DG quotient module to @ TO maxDegree @ returns
      the max hom-degree of its ambient DG module.  Because
      taking a quotient cannot introduce elements of higher
      hom-degree than are already in the ambient, the ambient's
      bound is valid for the quotient.  The value is what
      @ TO (toComplex, DGQuotientModule) @ and
      @ TO (toComplexMap, DGModuleMap) @ use to decide where to
      truncate when no @ TO EndDegree @ is supplied.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0, 1})
      z = zeroDGModuleMap(M, M)
      Q = cokernel z
      assert(maxDegree Q == maxDegree M)
  SeeAlso
    "DG module primitives and chain-complex export"
    maxDegree
    (maxDegree, DGModule)
    (toComplex, DGQuotientModule)
///

doc ///
  Key
    (getBasis, ZZ, DGModule)
  Headline
    Basis of the hom-degree-n piece of a DG module
  Usage
    b = getBasis(n, M)
  Inputs
    n:ZZ
      A homological degree.
    M:DGModule
  Outputs
    b:Matrix
      A one-row matrix over @ TT "M.natural" @ whose entries are
      the basis elements of @ TT "F_n(M)" @, the hom-degree-@ TT "n" @
      piece of @ TT "M.natural" @ over @ TT "A.ring" @.
  Description
    Text
      @ TT "getBasis(n, M)" @ delegates to
      @ TO2{(basis, ZZ, Module), "basis"} @ on the underlying
      @ TT "A.natural" @-module @ TT "M.natural" @, restricting to
      the variables of @ TT "A.natural" @.  The result is the
      piece of @ TT "M" @ on which @ TT "moduleDifferential(n, M)" @
      acts as its source-side basis.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      b = getBasis(1, KM)
      assert(instance(b, Matrix))
      assert(numcols b == 2)
  SeeAlso
    "DG module primitives and chain-complex export"
    getBasis
    (getBasis, ZZ, DGAlgebra)
    moduleDifferential
///

doc ///
  Key
    (toComplex, DGModule)
  Headline
    Export a DG module to a Complex of free base-ring modules
  Usage
    C = toComplex M
  Inputs
    M:DGModule
  Outputs
    C:Complex
      A @ TO Complex @ of free @ TT "A.ring" @-modules whose
      @ TT "i" @-th differential is @ TT "moduleDifferential(i+1, M)" @.
  Description
    Text
      @ TT "toComplex" @ packages the per-degree module
      differentials as an ordinary @ TO Complex @ so that the
      result can flow through the rest of the
      @ TO2{Complexes, "Complexes"} @ package.  The one-argument
      form uses @ TT "maxDegree M" @ as the truncation bound and
      errors out if that is @ TT "infinity" @; use
      @ TO (toComplex, DGModule, ZZ) @ to supply an explicit bound.
      The result is not cached by this method (use
      @ TO dgComplex @ for a cached variant).
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      KM = koszulComplexDGM R^1
      C = toComplex KM
      assert(instance(C, Complex))
      assert(C.dd_1 == moduleDifferential(1, KM))
      assert(C.dd_2 == moduleDifferential(2, KM))
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplex
    (toComplex, DGModule, ZZ)
    (toComplex, DGAlgebra)
    dgComplex
    moduleDifferential
///

doc ///
  Key
    (toComplex, DGModule, ZZ)
  Headline
    Export a DG module to a Complex with an explicit hom-degree bound
  Usage
    C = toComplex(M, n)
  Inputs
    M:DGModule
    n:ZZ
      A hom-degree upper bound.
  Outputs
    C:Complex
      A @ TO Complex @ of free @ TT "A.ring" @-modules, truncated at
      hom-degree @ TT "n" @.
  Description
    Text
      The explicit-bound form of @ TO (toComplex, DGModule) @.
      Supplying a bound is required when the ambient DG algebra has
      infinite hom-degree (as for an @ TO acyclicClosure @), and
      convenient when a low bound suffices.
    Example
      R = QQ[x, y, z]
      KM = koszulComplexDGM R^1
      C = toComplex(KM, 2)
      assert(instance(C, Complex))
      assert(max C == 2)
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplex
    (toComplex, DGModule)
    dgComplex
    moduleDifferential
///

doc ///
  Key
    (toComplex, DGQuotientModule)
  Headline
    Export a DG quotient module to a Complex
  Usage
    C = toComplex Q
  Inputs
    Q:DGQuotientModule
  Outputs
    C:Complex
      The cokernel of the chain map induced by the inclusion
      @ TT "Q.subDGModule.inclusion" @; equivalently, the
      per-degree quotient complex of the ambient.
  Description
    Text
      For a DG quotient module @ TT "Q = M / S" @,
      @ TT "toComplex Q" @ builds the cokernel at the
      @ TO Complex @ level: it exports the ambient DG module
      @ TT "M" @ via @ TO (toComplex, DGModule) @, exports the
      inclusion of @ TT "S" @ via @ TO (toComplexMap, DGModuleMap) @,
      and takes the cokernel of the resulting @ TO ComplexMap @.
      If @ TT "Q" @ was previously passed through
      @ TO (prune, DGQuotientModule) @ the cached pruned complex
      is returned instead.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      M = freeDGModule(A, {0})
      z = zeroDGModuleMap(M, M)
      Q = cokernel z
      CM = toComplex M
      CQ = toComplex Q
      assert(rank CM_0 == rank CQ_0)
      assert(rank CM_1 == rank CQ_1)
  SeeAlso
    "DG module primitives and chain-complex export"
    (toComplex, DGModule)
    (toComplexMap, DGModuleMap)
    (prune, DGQuotientModule)
///

doc ///
  Key
    (toComplex, DGIdeal)
  Headline
    Export a DG ideal to a Complex (not yet implemented)
  Usage
    C = toComplex I
  Inputs
    I:DGIdeal
  Outputs
    C:Complex
  Description
    Text
      @ TT "toComplex" @ on a @ TO DGIdeal @ is not yet
      implemented.  The natural definition views @ TT "I" @ as a
      DG submodule of the regular DG module
      @ TT "A.natural" @ over @ TT "A" @, but a canonical wrapper
      for the regular-representation view of @ TT "A" @ as a DG
      module over itself is not yet in the package, so the method
      raises an informative error pointing at the currently
      available alternatives.

      Use @ TT "toComplex(ambient I / I)" @ to obtain the
      @ TO Complex @ of the quotient DG algebra, or read the
      underlying data via @ TT "ideal I" @ and @ TT "gens I" @.
    Example
      R = ZZ/101[x]
      A = koszulComplexDGA R
      T = (gens A.natural)#0
      I = dgIdeal(A, {T})
      errored = try (toComplex I; false) else true
      assert(errored)
  SeeAlso
    "DG module primitives and chain-complex export"
    "Operations on DG Ideals"
    DGIdeal
    dgIdeal
    (symbol /, DGAlgebra, DGIdeal)
///

doc ///
  Key
    (toComplexMap, DGModuleMap)
  Headline
    Export a DG module map to a ComplexMap
  Usage
    cm = toComplexMap f
  Inputs
    f:DGModuleMap
      A DG module map @ TT "M --> N" @.
  Outputs
    cm:ComplexMap
      The induced @ TO ComplexMap @
      @ TT "toComplex M --> toComplex N" @.
  Description
    Text
      @ TT "toComplexMap f" @ assembles the per-degree pieces of
      @ TT "f" @ into a @ TO ComplexMap @.  When either side has
      infinite hom-degree, an @ TO EndDegree @ bound must be
      supplied; the option @ TO AssertWellDefined @ (default
      @ TT "true" @) causes @ TT "isWellDefined f" @ to be checked
      up front.  When the target is a DG quotient module only the
      canonical projection is supported; the result is the induced
      quotient chain map.  Use
      @ TO (toComplexMap, DGModuleMap, ZZ) @ to extract a single
      per-degree component instead.
    Example
      R = QQ[a, b] / ideal(a^2, b^2)
      A = koszulComplexDGA R
      phi = dgAlgebraMap(A, A, matrix {gens A.natural})
      cm = toComplexMap phi
      assert(instance(cm, ComplexMap))
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplexMap
    (toComplexMap, DGModuleMap, ZZ)
    (toComplexMap, DGAlgebraMap)
    (toComplex, DGModule)
    identityDGModuleMap
///

doc ///
  Key
    (toComplexMap, DGModuleMap, ZZ)
  Headline
    The per-hom-degree component of a DG module map as a matrix
  Usage
    m = toComplexMap(f, n)
  Inputs
    f:DGModuleMap
      A DG module map @ TT "M --> N" @.
    n:ZZ
      A homological degree.
  Outputs
    m:Matrix
      The hom-degree-@ TT "n" @ component
      @ TT "F_n(M) --> F_n(N)" @ as an @ TT "A.ring" @-linear
      matrix.
  Description
    Text
      Builds the @ TT "A.ring" @-linear matrix that @ TT "f" @
      induces on the hom-degree-@ TT "n" @ pieces of
      @ TT "M.natural" @ and @ TT "N.natural" @.  This is the
      per-degree primitive used by
      @ TO (toComplexMap, DGModuleMap) @ when assembling a full
      @ TO ComplexMap @.
    Example
      R = QQ[x, y] / ideal(x^2, y^2)
      Mdg = minimalSemifreeResolution(koszulComplexDGA R, R^1 / ideal(x, y), EndDegree => 3)
      idM = identityDGModuleMap Mdg
      all(0..3, n -> (
          cm := toComplexMap(idM, n);
          source cm == target cm and cm == id_(source cm)
          ))
  SeeAlso
    "DG module primitives and chain-complex export"
    toComplexMap
    (toComplexMap, DGModuleMap)
    (toComplex, DGModule)
    identityDGModuleMap
///

doc ///
  Key
    killCycles
    (killCycles,DGAlgebra)
  Headline
    Adjoin variables to kill non-bounding cycles in the lowest positive degree
  Usage
    B = killCycles A
  Inputs
    A:DGAlgebra
  Outputs
    B:DGAlgebra
      a new DG algebra with extra generators adjoined to make every cycle in the
      first positive degree where @ TT "HH_i A" @ is nonzero into a boundary
  Description
    Text
      This is the basic building block of Tate's acyclic closure construction.
      Given a DG algebra @ TT "A" @, @ TT "killCycles" @ locates the smallest positive
      homological degree @ TT "i" @ in which @ TT "HH_i A" @ is nonzero, picks a
      generating set of cycles, and adjoins new DG algebra generators in degree
      @ TT "i+1" @ mapping to those cycles.  The result is a DG algebra whose
      homology agrees with @ TT "A" @ below degree @ TT "i" @ and is killed in
      degree @ TT "i" @.  Iterating this procedure produces the acyclic closure.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3, b^3, c^3 - d^4}
      A = koszulComplexDGA R
      A.diff
      B = killCycles A
      B.diff
    Text
      The new generator @ TT "T_(2,1)" @ in @ TT "B" @ has differential equal to
      a chosen nonzero cycle representative in homological degree 1 of @ TT "A" @.
  SeeAlso
    DGAlgebra
    acyclicClosure
    adjoinVariables
///

doc ///
  Key
    adjoinVariables
    (adjoinVariables,DGAlgebra,List)
  Headline
    Extend a DG algebra by a user-chosen list of cycles to be killed
  Usage
    B = adjoinVariables(A, cycleList)
    B = adjoinVariables(A, cycleList, Variable => "U")
  Inputs
    A:DGAlgebra
    cycleList:List
      Homogeneous cycles of @ TT "A" @, i.e.\ elements @ TT "z" @ in
      @ TT "A.natural" @ with @ TT "(A.diff)(z) = 0" @ (no check is
      performed).  Each cycle must have the same multi-degree shape as
      @ TT "A.Degrees" @.
    Variable => String
      Base name for the newly adjoined generators.  Defaults to the base
      symbol already used by @ TT "A" @, or @ TT "\"T\"" @ if @ TT "A" @
      has no generators.
  Outputs
    B:DGAlgebra
      A semifree extension of @ TT "A" @ with one new generator per entry
      of @ TT "cycleList" @.  The new generator for a cycle @ TT "z" @ of
      hom-degree @ TT "n" @ has hom-degree @ TT "n + 1" @ and differential
      @ TT "z" @.
  Description
    Text
      Whereas @ TO killCycles @ chooses representatives of a basis of the
      first nonzero homology of @ TT "A" @ automatically,
      @ TT "adjoinVariables" @ lets the user specify exactly which cycles
      become boundaries.  This is the primitive used by
      @ TO acyclicClosure @, @ TO killCycles @, and @ TO minimalModel @,
      but can also be called directly to build DG algebra resolutions by
      hand.
    Text
      @ BOLD "Variable convention." @  The new generators extend the
      existing doubly-indexed naming scheme @ TT "base_(i, j)" @: for
      each adjoined cycle of hom-degree @ TT "n" @, a new generator named
      @ TT "base_(n+1, k)" @ is created, where @ TT "k" @ continues the
      1-indexed counter already in use at hom-degree @ TT "n + 1" @ in
      @ TT "A" @.  Existing generator names are preserved verbatim.
      Passing @ TT "Variable => \"U\"" @ switches the base symbol for
      @ EM "the new generators only" @:
    Example
      R = ZZ/101[a, b, c] / ideal(a^3, b^3, c^3)
      A = koszulComplexDGA R
      z = a^2 * (gens A.natural)_0
      polyDifferential(A, z) == 0
      B = adjoinVariables(A, {z})
      gens B.natural
      B' = adjoinVariables(A, {z}, Variable => "U")
      gens B'.natural
    Text
      Killing a single cycle lowers the size of the corresponding
      homology:
    Example
      apply(3, i -> numgens prune homology(i, A))
      apply(3, i -> numgens prune homology(i, B))
    Text
      Over a non-c.i.\ example with a non-regular relation, one can
      inspect the first homology, pick a representative, and kill just
      that class (leaving other @ TT "H_1" @ classes intact):
    Example
      S = ZZ/101[a, b, c, d] / ideal(a^3, b^3, c^3 - d^4)
      AS = koszulComplexDGA S
      prune homology(1, AS)
      BS = adjoinVariables(AS, {a^2 * (gens AS.natural)_0})
      prune homology(1, BS)
  Caveat
    The package does @ EM "not" @ verify that the supplied elements are
    actually cycles; passing a non-cycle produces a malformed DG algebra.
    Use @ TT "polyDifferential(A, z) == 0" @ to check before calling
    (the shorthand @ TT "(A.diff)(z)" @ applies @ TT "A.diff" @ as a
    ring map, which only agrees with the differential on monomials that
    contain at most one algebra generator and hence is @ EM "not" @ a
    valid cycle check for products).
  SeeAlso
    DGAlgebra
    killCycles
    acyclicClosure
    minimalModel
    setDiff
///

doc ///
  Key
    homologyAlgebra
    (homologyAlgebra,DGAlgebra)
  Headline
    Compute the homology algebra of a DGAlgebra.
  Usage
    HA = homologyAlgebra(A)
  Inputs
    A:DGAlgebra
  Outputs
    HA:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
    Text
      Note that HA is a graded commutative polynomial ring (i.e. an exterior algebra) since R is a complete intersection.
    Example  
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4,a^3*b^3*c^3*d^3}
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
      numgens HA
      HA.cache.cycles
    Example
      Q = ZZ/101[x,y,z]
      I = ideal{y^3,z*x^2,y*(z^2+y*x),z^3+2*x*y*z,x*(z^2+y*x),z*y^2,x^3,z*(z^2+2*x*y)}
      R = Q/I
      A = koszulComplexDGA(R)
      apply(maxDegree A + 1, i -> numgens prune homology(i,A))
      HA = homologyAlgebra(A)
    Text
      One can check that HA has Poincare duality since R is Gorenstein.
    Text
      If your DGAlgebra has generators in even degrees, then one must specify the options GenDegreeLimit and RelDegreeLimit.
    Example
      R = ZZ/101[a,b,c,d]
      S = R/ideal{a^4,b^4,c^4,d^4}
      A = acyclicClosure(R,EndDegree=>3)
      B = A ** S
      HB = homologyAlgebra(B,GenDegreeLimit=>7,RelDegreeLimit=>14)
///

doc ///
  Key
    (homology,DGAlgebra)
  Headline
    The homology algebra of a DG algebra
  Usage
    HA = homology A
    HA = HH A
  Inputs
    A:DGAlgebra
  Outputs
    HA:Ring
      the homology algebra @ TT "HH_*(A)" @, as a graded-commutative ring
  Description
    Text
      Returns the homology algebra of @ TT "A" @, caching the result inside @ TT "A" @
      so subsequent calls are fast.  This is a convenience wrapper around
      @ TO homologyAlgebra @ that supports the @ TT "HH" @ shorthand.
    Example
      R = ZZ/101[a,b,c] / ideal(a^3, b^3, c^3, a^2*b^2*c^2)
      A = koszulComplexDGA R
      HA = HH A
      numgens HA
      apply(maxDegree A + 1, i -> numgens prune homology(i, A))
    Text
      For DG algebras with even-degree generators (such as acyclic closures of non-regular
      rings), call @ TO homologyAlgebra @ directly and supply
      @ TO GenDegreeLimit @ and @ TO RelDegreeLimit @ explicitly.
  SeeAlso
    homologyAlgebra
    zerothHomology
    (homology, ZZ, DGAlgebra)
///

doc ///
  Key
    torAlgebra
    (torAlgebra,Ring)
  Headline
    Computes the Tor algebra of a ring
  Usage
    torR = torAlgebra(R)
  Inputs
    R:Ring
  Outputs
    torR:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]
      TorR = torAlgebra(R)
      S = R/ideal{a^3,b^3,c^3,d^5}
      TorS = torAlgebra(S, GenDegreeLimit => 3)
    Text
      The above example calculates the Tor algebra of R and S up to degree 3, by default.  One can also specify the maximum degree
      to compute generators of the Tor algebra by specifying the GenDegreeLimit option.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a^2*b^2*c^3*d^2}
      TorR = torAlgebra(R,GenDegreeLimit=>5)
///

doc ///
  Key
    (torAlgebra,Ring,Ring)
  Headline
    Computes Tor_R(S,k) up to a specified generating and relating degree.
  Usage
    TorRS = torAlgebra(R,S,GenDegreeLimit=>m,RelDegreeLimit=>n)
  Inputs
    R:Ring
    S:Ring
  Outputs
    TorRS:Ring
  Description
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      M = coker matrix {{a^3*b^3*c^3*d^3}};
      S = R/ideal{a^3*b^3*c^3*d^3}
      HB = torAlgebra(R,S,GenDegreeLimit=>4,RelDegreeLimit=>8)
      numgens HB
      apply(5,i -> #(flatten entries getBasis(i,HB)))      
      Mres = freeResolution(M, LengthLimit=>8)
    Text
      Note that in this example, $Tor_*^R(S,k)$ has trivial multiplication, since the
      map from R to S is a Golod homomorphism by a theorem of Levin and Avramov.
///

doc ///
  Key
    maxDegree
    (maxDegree,DGAlgebra)
  Headline
    Computes the maximum homological degree of a DGAlgebra
  Usage
    mDegree = maxDegree(A)
  Inputs
    A:DGAlgebra
  Outputs
    mDegree:ZZ
      The maximum degree of the DGAlgebra A (this can be infinite).
  Description
    Text
      Note that if the DGAlgebra A has any generators of even degree, then maxDegree returns infinity.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3}
      A = koszulComplexDGA(R)
      B = acyclicClosure(A,EndDegree=>3)
      maxDegree(A)
      maxDegree(B)
///

doc ///
  Key
    isHomologyAlgebraTrivial
    (isHomologyAlgebraTrivial,DGAlgebra)
  Headline
    Determines if the homology algebra of a DGAlgebra is trivial
  Usage
    isTriv = isHomologyAlgebraTrivial(A) 
  Inputs
    A:DGAlgebra
  Outputs
    isTriv:Boolean
  Description
    Text
      This function computes the homology algebra of the DGAlgebra A and determines if the multiplication on H(A) is trivial.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      S = R/ideal{a^3*b^3*c^3*d^3}
      A = acyclicClosure(R,EndDegree=>3)
      B = A ** S
      isHomologyAlgebraTrivial(B,GenDegreeLimit=>6)
    Text
      The command returns true since R --> S is Golod.  Notice we also used the option GenDegreeLimit here.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      A = koszulComplexDGA(R)
      isHomologyAlgebraTrivial(A)
    Text
      The command returns false, since R is Gorenstein, and so HA has Poincare Duality, hence the multiplication
      is far from trivial.
///

doc ///
  Key
    isAcyclic
    (isAcyclic,DGAlgebra)
  Headline
    Determine whether a DG algebra has no higher homology
  Usage
    isAcyc = isAcyclic A
  Inputs
    A:DGAlgebra
  Outputs
    isAcyc:Boolean
      @ TT "true" @ if @ TT "HH_i A = 0" @ for all @ TT "i > 0" @, and @ TT "false" @ otherwise
  Description
    Text
      A DG algebra is acyclic (in the convention used by this package) if it has
      zero homology in all positive degrees, so that @ TT "A -> HH_0 A" @ is a
      quasi-isomorphism.  @ TT "isAcyclic" @ checks homology degree by degree up
      through @ TT "maxDegree A" @; for unbounded DG algebras an @ TO EndDegree @
      option bounds the search.
    Text
      The Koszul complex of a regular sequence is acyclic:
    Example
      Q = ZZ/101[a,b,c,d]
      I = ideal(a^4, b^4, c^4, d^4)
      isAcyclic(koszulComplexDGA I)
    Text
      But the Koszul complex of a ring with nontrivial relations in homology is not:
    Example
      R = ZZ/101[a,b,c,d] / ideal(a^4 + b^4 + c^4 + d^4)
      isAcyclic(koszulComplexDGA R)
    Text
      An acyclic closure built to sufficient degree is acyclic in the checked range:
    Example
      R = ZZ/101[a,b,c,d] / ideal(a^3, b^3, c^3, d^3)
      A = acyclicClosure(R, EndDegree => 3)
      isAcyclic(A, EndDegree => 3)
  SeeAlso
    DGAlgebra
    (homology, ZZ, DGAlgebra)
    acyclicClosure
///

doc ///
  Key
    isGolod
    (isGolod,Ring)
  Headline
    Determines if a ring is Golod
  Usage
    isGol = isGolod(R)
  Inputs
    R:Ring
  Outputs
    isGol:Boolean
  Description
    Text
      This function determines if the Koszul complex of a ring R admits a trivial Massey operation.  If one exists, then R is Golod.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4+b^4+c^4+d^4}
      isGolod(R)
    Text
      Hypersurfaces are Golod, but
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      isGolod(R)
    Text
      complete intersections of higher codimension are not.  Here is another example:
    Example
      Q = ZZ/101[a,b,c,d]
      R = Q/(ideal vars Q)^2
      isGolod(R)
    Text
      The above is a (CM) ring minimal of minimal multiplicity, hence Golod.  The next example was found
      by Lukas Katthan, and appears in his arXiv paper 1511.04883.  It is the first known example
      of an algebra that is not Golod, but whose Koszul complex has a trivial homology product.
    Example
      Q = ZZ/101[x_1,x_2,y_1,y_2,z,w]
      I = ideal {x_1*x_2^2,z^2*w,y_1*y_2^2,x_2^2*z*w,y_2^2*z^2,x_1*x_2*y_1*y_2,x_2^2*y_2^2*z,x_1*y_1*z}
      R = Q/I
      isHomologyAlgebraTrivial koszulComplexDGA R
      isGolod R
    Text
      Note that since the Koszul complex is zero in homological degree beyond the embedding dimension, there are only finitely
      many Massey products that one needs to check to verify that a ring is Golod.
///

doc ///
  Key
    isGolodHomomorphism
    (isGolodHomomorphism,QuotientRing)
  Headline
    Determines if the canonical map from the ambient ring is Golod
  Usage
    isGol = isGolodHomomorphism(R)
  Inputs
    R:QuotientRing
  Outputs
    isGol:Boolean
  Description
    Text
      This function determines if the canonical map from ambient R --> R is Golod.  It does this by computing an acyclic closure of
      ambient R (which is a @ TO DGAlgebra @), then tensors this with R, and determines if this DG Algebra has a trivial Massey operation
      up to a certain homological degree provided by the option GenDegreeLimit.
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^4+b^4+c^4+d^4}
      isGolodHomomorphism(R,GenDegreeLimit=>5)
    Text
      If R is a Golod ring, then ambient R $\rightarrow$ R is a Golod homomorphism. 
    Example
      Q = ZZ/101[a,b,c,d]/ideal{a^4,b^4,c^4,d^4}
      R = Q/ideal (a^3*b^3*c^3*d^3)
      isGolodHomomorphism(R,GenDegreeLimit=>5,TMOLimit=>3)
    Text
      The map from Q to R is Golod by a result of Avramov and Levin; we can only find the trivial Massey operations out to a given degree.
///

doc ///
  Key
    getGenerators
    (getGenerators,DGAlgebra)
  Headline
    Cycles whose classes generate the homology algebra of a DG algebra
  Usage
    cycleList = getGenerators A
  Inputs
    A:DGAlgebra
  Outputs
    cycleList:List
      a list of cycles in @ TT "A.natural" @ whose homology classes generate
      @ TT "HH_*(A)" @ as a graded-commutative algebra over @ TT "A.ring" @
  Description
    Text
      Walks up the homological degrees of @ TT "A" @, computing a basis of each
      homology module and selecting minimal new generators not already reachable
      by products of lower-degree cycles.  The returned list is exactly the
      generating set used internally by @ TO homologyAlgebra @ to build its
      presentation.
    Text
      This version of the function assumes all algebra generators of @ TT "A" @
      lie in odd homological degree.  When @ TT "A" @ has generators in even
      degree, a finite generating set need not exist, and the search depth must
      be bounded with @ TT "EndDegree" @ or @ TO GenDegreeLimit @.
    Example
      R = ZZ/101[a,b,c] / ideal(a^3, b^3, c^3, a^2*b^2*c^2)
      A = koszulComplexDGA R
      netList getGenerators A
    Text
      The generators above are the cycles used to present @ TT "homologyAlgebra A" @;
      their classes span @ TT "HH_i A" @ in each positive degree.
    Example
      apply(maxDegree A + 1, i -> numgens prune homology(i, A))
  SeeAlso
    DGAlgebra
    homologyAlgebra
    (homology, ZZ, DGAlgebra)
    homologyClass
///

doc ///
  Key
    deviations
    (deviations,Ring)
    (deviations,Complex)
    (deviations,RingElement,List)
  Headline
    Computes the deviations of the input ring, complex, or power series.
  Usage
    devTally = deviations(R)
  Inputs
    R:Ring
  Outputs
    devTally:Tally
  Description
    Text
      This command computes the deviations of a @ TO Ring @, a @ TO Complex @, or a power series in the form of a @ TO RingElement @.
      The deviations are the same as the degrees of the generators of the acyclic closure of R, or the degrees of the generators of the
      Tor algebra of R.  This function takes an option called Limit (default value 3) that specifies the largest deviation to compute.
    Example
      R = ZZ/101[a,b,c,d]/ideal {a^3,b^3,c^3,d^3}
      deviations(R)
      deviations(R,DegreeLimit=>4)
      S = R/ideal{a^2*b^2*c^2*d^2}
      deviations(S,DegreeLimit=>4)
      T = ZZ/101[a,b]/ideal {a^2-b^3}
      deviations(T,DegreeLimit=>4)
    Text
      Note that the deviations of T are not graded, since T is not graded.  When calling deviations on a Complex, the
      zeroth free module must be cyclic, and this is checked.  The same goes for the case
      of a RingElement.
    Example
      R = ZZ/101[a,b,c,d]/ideal {a^3,b^3,c^3,d^3}
      A = degreesRing R
      kRes = freeResolution(coker vars R, LengthLimit => 4)
      pSeries = poincareN kRes
      devA = deviations(R,DegreeLimit=>5)
      devB = deviations(kRes,DegreeLimit=>5)
      devC = deviations(pSeries,degrees R, DegreeLimit=>5)
      devA === devB and devB === devC
///

doc ///
  Key
    deviationsToPoincare
    (deviationsToPoincare,HashTable)
    [deviationsToPoincare,DegreeLimit]
  Headline
    Computes the power series corresponding to a set of deviations.
  Usage
    pSeries = deviationsToPoincare(devHash)
  Inputs
    devHash:HashTable
      HashTable of the same form as the output from @ TO deviations @
  Outputs
    pSeries:RingElement
  Description
    Text
      This command takes a HashTable of the same form output from @ TO deviations @ and produces the Poincare series corresponding to it.
      The (key,value) pairs must be of the form homologicalDegree=>number or (homologicalDegree,internalDegree)=>number.
      Because 
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      RDevs = deviations(R,DegreeLimit=>6)
      devPSeries = deviationsToPoincare(RDevs,DegreeLimit=>6)
      pSeries = poincareN (freeResolution(coker vars R, LengthLimit=>6))
      substitute(devPSeries,ring pSeries) == pSeries
///

doc ///
  Key
    findTrivialMasseyOperation
    findNaryTrivialMasseyOperation
    (findTrivialMasseyOperation,DGAlgebra)
    (findNaryTrivialMasseyOperation,DGAlgebra,List,HashTable,ZZ)
  Headline
    Finds a trivial Massey operation on a set of generators of H(A)
  Usage
    tmo = findTrivialMasseyOperation(A)
  Inputs
    A:DGAlgebra
  Outputs
    seq:Sequence
      A sequence seq whose first entry reports whether a trivial Massey operation has been found, and the second
      entry is a hash table with keys given by monomials in a generating set of the positive degree homology of
      A and values the element that bounds the Massey product corresponding to that monomial.
  Description
    Text
      This function the element that bounds all potentially nonzero Massey products (before taking homology class).
      The maximum degree of a generating cycle is specified in the option GenDegreeLimit, if needed.
    Text
      Golod rings are defined by being those rings whose Koszul complex K^R has a trivial Massey operation.
      Also, the existence of a trivial Massey operation on a DG algebra A forces the multiplication on H(A)
      to be trivial.  An example of a ring R such that H(K^R) has trivial multiplication, yet K^R does not admit
      a trivial Massey operation is unknown.  Such an example cannot be monomially defined, by a result of
      Jollenbeck and Berglund. 
    Text
      This is an example of a Golod ring.  It is Golod since it is the Stanley-Reisner ideal of a flag complex
      whose 1-skeleton is chordal [Jollenbeck-Berglund].
    Example
      Q = ZZ/101[x_1..x_6]
      I = ideal (x_3*x_5,x_4*x_5,x_1*x_6,x_3*x_6,x_4*x_6)
      R = Q/I
      A = koszulComplexDGA(R)
      isHomologyAlgebraTrivial(A,GenDegreeLimit=>3)
      cycleList = getGenerators(A)
      (hasTMO, tmoSoFar) = findTrivialMasseyOperation(A)
      assert(hasTMO)
    Text
      Below is an example of a Teter ring (Artinian Gorenstein ring modulo its socle), and the computation in Avramov and Levin's
      paper shows that H(A) does not have trivial multiplication, hence no trivial Massey operation can exist.
    Example
      Q = ZZ/101[x,y,z]
      I = ideal (x^3,y^3,z^3,x^2*y^2*z^2)
      R = Q/I
      A = koszulComplexDGA(R)
      isHomologyAlgebraTrivial(A)
      cycleList = getGenerators(A)
      assert(not first findTrivialMasseyOperation(A))
    Text
      The related function @ TO findNaryTrivialMasseyOperation @ find only the nth order trivial Massey operations.
///

doc ///
  Key
    masseyTripleProduct
    (masseyTripleProduct,DGAlgebra,RingElement,RingElement,RingElement)
  Headline
    Computes the Massey triple product of a set of cycles or homology classes
  Usage
    h = masseyTripleProduct(A,h1,h2,h3)
  Inputs
    A:DGAlgebra
    h1:RingElement
    h2:RingElement
    h3:RingElement
  Outputs
    h:RingElement
      The return value is either the homology class of the Massey triple product defined
      by the inputs or a cycle representing the homology class.
  Description
    Text
       These functions compute the Massey triple product of either three homology classes
       or three cycles that represent nonzero homology classes for which the Massey triple product
       is defined.
    Text
       For an example, we return to an example due to Lukas Katthan which was discussed in @ TO isGolod @.
       First, we define the algebra:
    Example
       Q = QQ[x_1,x_2,y_1,y_2,z]
       I = ideal (x_1*x_2^2,y_1*y_2^2,z^3,x_1*x_2*y_1*y_2,y_2^2*z^2,x_2^2*z^2,x_1*y_1*z,x_2^2*y_2^2*z)
       R = Q/I
       KR = koszulComplexDGA R
    Text
       The following are cycles:
    Example
       z1 = z^2*T_(1,5)
       z2 = y_2^2*T_(1,3)
       z3 = x_2^2*T_(1,1)
    Text
       and z1*z2, z2*z3 vanish in homology:
    Example
       (lifted12,lift12) = getBoundaryPreimage(KR,z1*z2)
       (lifted23,lift23) = getBoundaryPreimage(KR,z2*z3)
    Text
       Note that the first return value of @ TO getBoundaryPreimage @ indicates that the inputs
       are indeed boundaries, and the second value is the lift of the boundary along the differential.
    Text
       Given cycles z1,z2,z3 such that z1*z2 and z2*z3 are boundaries, 
       the Massey triple product of the homology classes represented by z1,z2 and z3 
       is the homology class of lift12*z3 + z1*lift23.  To see this, we compute and check:
    Example
       z123 = masseyTripleProduct(KR,z1,z2,z3)
       z123 == lift12*z3 + z1*lift23
    Text
       One may also compute Massey triple products directly on elements of the homology
       algebra itself, as is seen with the command masseyTripleProduct:
    Example
       H = HH(KR)
       h1 = homologyClass(KR,z1)
       h2 = homologyClass(KR,z2)
       h3 = homologyClass(KR,z3)
       h123 = masseyTripleProduct(KR,h1,h2,h3)
       h123 == homologyClass(KR,z123)
///

doc ///
  Key
    (masseyTripleProduct,DGAlgebra,ZZ,ZZ,ZZ)
  Headline
    Computes the matrix representing all triple Massey operations.
  Usage
    mat = masseyTripleProduct(A,l,m,n)
  Inputs
    A:DGAlgebra
    l:ZZ
    m:ZZ
    n:ZZ
  Outputs
    mat:Matrix
  Description
    Text
      Given a triple of homology classes h1,h2,h3, such that h1h2 = h2h3 = 0,
      the Massey triple product of h1,h2 and h3 may be defined as in
      @ TO masseyTripleProduct @.  This command computes a basis of the homology
      algebra of A in degrees l,m and n respectively, and expresses the triple
      Massey operation of each triple, provided it is defined.  If a triple product
      is not defined (i.e. if either h1h2 or h2h3 is not zero) then the triple
      product is reported as zero in the matrix.
    Text
      The following example appears in "On the Hopf algebra of a Local Ring" by Avramov
      as an example of a nonvanishing Massey operation which an algebra generator:
    Example
      Q = QQ[t_1,t_2,t_3,t_4]
      I = ideal (t_1^3,t_2^3,t_3^3-t_1*t_2^2,t_1^2*t_3^2,t_1*t_2*t_3^2,t_2^2*t_4,t_4^2)
      R = Q/I
      KR = koszulComplexDGA R
      H = HH(KR)
      masseys = masseyTripleProduct(KR,1,1,1);
      rank masseys
    Text
      As you can see, this command is useful to determine the number of linearly independent
      elements that arise as triple Massey products.
    Text
      For example, the following Massey triple product is nonvanishing and is an
      algebra generator:
    Example
      masseyTripleProduct(KR,X_2,X_4,X_1)
///

doc ///
  Key
    expandGeomSeries
    (expandGeomSeries,List,ZZ)
    (expandGeomSeries,RingElement,ZZ)
  Headline
    Expand a geometric series to a specified degree.
  Usage
    pSeries = expandGeomSeries(f,n)
  Inputs
    f:RingElement
      Ratio of the geometric series to be expanded.
    n:ZZ
      Degree which to expand the geometric series.
  Outputs
    pSeries:RingElement
      Power series representation of the geometric series.
  Description
    Text
      If the user supplies a list instead of a RingElement as the first argument, the return
      value is the product of all the each of the geometric series expanded to degree n obtained
      by calling expandGeomSeries on each element of the list.
    Example
      A = ZZ[S,T_0,T_1]
      f = expandGeomSeries(S^2*T_0^8,10)
      g = expandGeomSeries(S^4*T_1^15,10)
      h = expandGeomSeries({S^2*T_0^8,S^4*T_1^15},10)
      B = A/(first gens A)^11
      substitute(f*g,B) == h
///

doc ///
  Key
    torMap
    (torMap,RingMap)
    [torMap,GenDegreeLimit]
  Headline
    Compute the map of Tor algebras associated to a RingMap.
  Usage
    torPhi = torMap(phi)
  Inputs
    phi:RingMap
  Outputs
    torPhi:RingMap
  Description
    Text
      The functor Tor_R(M,N) is also functorial in the ring argument.  Therefore, a ring map phi from A to B induces an algebra map
      from the Tor algebra of A to the Tor algebra of B.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3,a^2*b^2*c^2}
      S = R/ideal{a*b^2*c^2,a^2*b*c^2,a^2*b^2*c}
      f = map(S,R)
      fTor = torMap(f,GenDegreeLimit=>3)
      matrix fTor
    Text
      In the following example, the map on Tor is surjective, which means that the ring homomorphism is large (Dress-Kramer).
    Example
      R = ZZ/101[a,b,c,d]/ideal{a^3,b^3,c^3,d^3,a*c,a*d,b*c,b*d}
      S = ZZ/101[a,b]/ideal{a^3,b^3}
      f = map(S,R,matrix{{a,b,0,0}})
      fTor = torMap(f,GenDegreeLimit=>4)
      matrix fTor      
///

doc ///
  Key
    (diff,DGAlgebra,RingElement)
  Headline
    Apply the differential of a DGAlgebra to a ring element
  Usage
    b = diff(A, a)
  Inputs
    A:DGAlgebra
    a:RingElement
      an element of @ TT "A.natural" @
  Outputs
    b:RingElement
      the image @ TT "d_A(a)" @
  Description
    Text
      Returns the image of @ TT "a" @ under the differential of @ TT "A" @.
      The element @ TT "a" @ must be a ring element of the underlying graded-commutative
      algebra @ TT "A.natural" @; products, sums, and polynomial expressions in the
      DG generators are all valid inputs.  The differential is applied by the Leibniz rule,
      so homogeneous components are processed independently.
    Example
      R = ZZ/101[x,y,z]
      A = koszulComplexDGA R
      diff(A, T_(1,1) * T_(1,2))
      diff(A, diff(A, T_(1,1) * T_(1,2) * T_(1,3)))
    Text
      When @ TT "a" @ is a cycle, the result is zero:
    Example
      z = x*T_(1,2) - y*T_(1,1)
      diff(A, z)
    Text
      To solve the equation @ TT "diff(A, b) == z" @ for a boundary @ TT "z" @,
      use @ TO getBoundaryPreimage @.
  SeeAlso
    DGAlgebra
    getBoundaryPreimage
    homologyClass
///

doc ///
  Key
    getBoundaryPreimage
    (getBoundaryPreimage,DGAlgebra,RingElement)
    (getBoundaryPreimage,DGAlgebra,List)
  Headline
    Attempt to find a preimage of a boundary under the differential of a DGAlgebra.
  Usage
    (lifted,myLift) = getBoundaryPreimage(A,z)
  Inputs
    A:DGAlgebra
    z:RingElement
  Outputs
    seq:Sequence
  Description
    Text
      The first element in the return value is a boolean value indicating whether the
      lift was possible.  If true, the second coordinate of the return value is the lift.
      If false, then the second coordinate of the return value is the reduction of the
      input modulo the image.
    Example
       Q = QQ[x_1,x_2,y_1,y_2,z]
       I = ideal (x_1*x_2^2,y_1*y_2^2,z^3,x_1*x_2*y_1*y_2,y_2^2*z^2,x_2^2*z^2,x_1*y_1*z,x_2^2*y_2^2*z)
       R = Q/I
       KR = koszulComplexDGA R
    Text
       The following are cycles:
    Example
       z1 = z^2*T_(1,5)
       z2 = y_2^2*T_(1,3)
       z3 = x_2^2*T_(1,1)
       {diff(KR,z1),diff(KR,z1),diff(KR,z1)}
    Text
       and z1*z2, z2*z3 vanish in homology:
    Example
       (lifted12,lift12) = getBoundaryPreimage(KR,z1*z2)
       (lifted23,lift23) = getBoundaryPreimage(KR,z2*z3)
    Text
       We can check that the differential of the lift is the supposed boundary:
    Example
       diff(KR,lift23) == z2*z3
///

doc ///
  Key
    homologyClass
    (homologyClass,DGAlgebra,RingElement)
  Headline
    Computes the element of the homology algebra corresponding to a cycle in a DGAlgebra.
  Usage
    h = homologyClass(A,z)
  Inputs
    A:DGAlgebra
    z:RingElement
  Outputs
    h:RingElement
  Description
    Text
      This function computes the element in the homology algebra of a cycle in a @ TO DGAlgebra @.
      In order to do this, the @ TO homologyAlgebra @ is retrieved (or computed, if it hasn't been
      already).
    Example
      Q = QQ[x,y,z]
      I = ideal (x^3,y^3,z^3)
      R = Q/I
      KR = koszulComplexDGA R
      z1 = x^2*T_(1,1)
      z2 = y^2*T_(1,2)
      H = HH(KR)
      homologyClass(KR,z1*z2)
///

doc ///
  Key
    zerothHomology
    (zerothHomology,DGAlgebra)
  Headline
    The zeroth homology of a DG algebra as a ring
  Usage
    HA0 = zerothHomology A
  Inputs
    A:DGAlgebra
  Outputs
    HA0:Ring
      @ TT "HH_0(A)" @, presented as a quotient of @ TT "A.ring" @
  Description
    Text
      Returns @ TT "HH_0(A)" @ as a ring, computed by taking @ TT "A.ring" @ modulo the image
      of the degree-one part of the differential.  For a Koszul complex
      @ TT "KR = koszulComplexDGA I" @ over @ TT "R" @, this recovers the quotient ring @ TT "R/I" @.
    Example
      Q = ZZ/101[x,y,z]
      I = ideal(x^2, y^2, z^2, x*y*z)
      KQ = koszulComplexDGA I
      H0 = zerothHomology KQ
      describe H0
    Text
      When @ TT "A" @ is an acyclic closure, @ TT "zerothHomology A" @ returns the quotient
      ring on which the DG algebra resolves the residue field.
    Example
      R = Q / I
      A = acyclicClosure(R, EndDegree => 2)
      zerothHomology A
  SeeAlso
    (homology, DGAlgebra)
    homologyAlgebra
///

doc ///
  Key
    getDegNModule
    (getDegNModule,ZZ,Ring,Ring)
  Headline
    Extract the degree-n strand of a graded algebra as a module over its degree-zero piece
  Usage
    MN = getDegNModule(N, R, A)
  Inputs
    N:ZZ
      the homological degree to extract
    R:Ring
      the degree-zero piece of @ TT "A" @, i.e.\ @ TT "A_0" @
    A:Ring
      a graded @ TT "R" @-algebra with @ TT "A_0 = R" @
  Outputs
    MN:Module
      the degree-@ TT "N" @ component of @ TT "A" @, presented as an @ TT "R" @-module
  Description
    Text
      Given a graded algebra @ TT "A" @ over @ TT "R" @ with @ TT "A_0 = R" @, the strand
      @ TT "A_N" @ is an @ TT "R" @-module.  This function returns that strand with a
      minimal presentation by monomials of internal degree @ TT "N" @ modulo the relations
      of @ TT "A" @.  It is most often applied to a homology algebra @ TT "HA = HH A" @
      together with its degree-zero subring @ TT "HA_0 = zerothHomology A" @.
    Example
      Q = ZZ/101[x,y,z]
      I = ideal(x^3, y^3, z^3)
      R = Q / I
      KR = koszulComplexDGA R
      HA = HH KR;
      H0 = zerothHomology KR
      M1 = getDegNModule(1, H0, HA)
      M2 = getDegNModule(2, H0, HA)
    Text
      The ranks of these strands are the Koszul Betti numbers of @ TT "R" @:
    Example
      apply(0..3, n -> numgens getDegNModule(n, H0, HA))
  SeeAlso
    zerothHomology
    homologyAlgebra
    homologyModule
///

doc ///
  Key
    DGAlgebraMap
  Headline
    The class of DG algebra maps
  Description
    Text
      A @ TT "DGAlgebraMap" @ represents a morphism of @ TO2{DGAlgebra, "DG algebras"} @.
      It carries four pieces of data: a @ TT "source" @ and @ TT "target" @ (each a
      @ TO DGAlgebra @), an underlying ring map @ TT "f.natural" @ on the graded-commutative
      algebras, and a degree-zero ring map @ TT "f.ringMap" @ between the underlying rings.
      A DG algebra map is a ring map that commutes with the differentials, so that
      @ TT "d_B \\circ f = f \\circ d_A" @ where @ TT "d_A" @ and @ TT "d_B" @ are the
      differentials of the source and target.
    Text
      There are two principal ways to construct a DG algebra map:
      @ TO dgAlgebraMap @ builds one from a matrix specifying where the DG generators go,
      and @ TO liftToDGMap @ lifts a ring map on the degree-zero part to a morphism of
      acyclic closures or Koszul complexes.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 3)
      B = acyclicClosure(S, EndDegree => 3)
      phi = liftToDGMap(B, A, map(S, R))
      source phi === A
      target phi === B
    Text
      Once constructed, a DG algebra map can be checked with @ TO (isWellDefined, DGAlgebraMap) @,
      converted to a @ TO ComplexMap @ via @ TO toComplexMap @, or pushed through homology
      with @ TO (homology, DGAlgebraMap) @.
  SeeAlso
    dgAlgebraMap
    liftToDGMap
    (isWellDefined, DGAlgebraMap)
    toComplexMap
    (homology, DGAlgebraMap)
    ringMap
///

doc ///
  Key
    ringMap
  Headline
    The degree-zero ring map underlying a DGAlgebraMap
  Usage
    phi.ringMap
  Description
    Text
      Every @ TO DGAlgebraMap @ @ TT "phi" @ from @ TT "A" @ to @ TT "B" @ includes a key
      @ TT "ringMap" @: a @ TO RingMap @ from @ TT "A.ring" @ to @ TT "B.ring" @ giving the
      restriction of @ TT "phi" @ to degree zero.  When the two DG algebras live over the
      same ring, @ TT "phi.ringMap" @ is the identity; in general it is the degree-zero
      ring map used to lift @ TT "phi" @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 2)
      B = acyclicClosure(S, EndDegree => 2)
      phi = liftToDGMap(B, A, map(S, R))
      phi.ringMap
    Text
      The @ TT "ringMap" @ is the bridge used internally by @ TO toComplexMap @ to
      pushforward complexes between different rings.
  SeeAlso
    DGAlgebraMap
    liftToDGMap
    toComplexMap
///

doc ///
  Key
    dgAlgebraMap
    (isWellDefined,DGAlgebraMap)
    (dgAlgebraMap,DGAlgebra,DGAlgebra,Matrix)
  Headline
    Define a DG algebra map between DG algebras.
  Usage
    phi = dgAlgebraMap(B,A,M)
  Inputs
    A:DGAlgebra
       Source
    B:DGAlgebra
       Target
    M:Matrix
       A matrix representing where the generators of A should be mapped to (akin to ringMap)
  Outputs
    phi:DGAlgebraMap
  Description
    Example
      R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
      K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
      K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
      g = dgAlgebraMap(K1,K2,matrix{{Y_(1,2),Y_(1,3)}})
      isWellDefined g
    Text
      The function does not check that the DG algebra map is well defined, however.
    Example
      f = dgAlgebraMap(K2,K1,matrix{{0,T_(1,1),T_(1,2)}})
      isWellDefined f
///

doc ///
  Key
    toComplexMap
    (toComplexMap,DGAlgebraMap)
    (toComplexMap,DGAlgebraMap,ZZ)
    [toComplexMap,AssertWellDefined]
    [toComplexMap,EndDegree]
  Headline
    Construct the ComplexMap associated to a DGAlgebraMap
  Usage
    psi = toComplexMap phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    psi:ComplexMap
  Description
    Example
       R = ZZ/101[a,b,c]/ideal{a^3+b^3+c^3,a*b*c}
       K1 = koszulComplexDGA(ideal vars R,Variable=>"Y")
       K2 = koszulComplexDGA(ideal {b,c},Variable=>"T")
       g = dgAlgebraMap(K1,K2,matrix{{Y_(1,2),Y_(1,3)}})
       g' = toComplexMap g
    Text
      The option @ TO EndDegree @ must be specified if the source of phi has any algebra generators of even degree.  The option @ TO AssertWellDefined @
      is used if one wishes to assert that the result of this computation is indeed a chain map.  One can construct just the nth map in the
      chain map by providing the second @ TO ZZ @ parameter.
    Text
      This function also works when working over different rings, such as the case when the @ TO DGAlgebraMap @ is produced via
      @ TO liftToDGMap @ and in the next example.  In this case, the target module is produced via @ TO pushForward @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      S = R/ideal{a^2*b^2*c^2}
      f = map(S,R)
      A = acyclicClosure(R,EndDegree=>3)
      B = acyclicClosure(S,EndDegree=>3)
      phi = liftToDGMap(B,A,f)
      toComplexMap(phi,EndDegree=>3)
///

doc ///
  Key
    liftToDGMap
    (liftToDGMap,DGAlgebra,DGAlgebra,RingMap)
    [liftToDGMap,EndDegree]
  Headline
    Lift a ring homomorphism in degree zero to a DG algebra morphism
  Usage
    phiTilde = liftToDGMap(B,A,phi)
  Inputs
    B:DGAlgebra
      Target
    A:DGAlgebra
      Source
    phi:RingMap
      Map from A in degree zero to B in degree zero
  Outputs
    phiTilde:DGAlgebraMap
      DGAlgebraMap lifting phi to a map of DGAlgebras.
  Description
    Text
      In order for phiTilde to be defined, phi of the image of the differential of A in degree 1 must lie in the image of the
      differential of B in degree 1.  At present, this condition is not checked.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3,b^3,c^3}
      S = R/ideal{a^2*b^2*c^2}
      f = map(S,R)
      A = acyclicClosure(R,EndDegree=>3)
      B = acyclicClosure(S,EndDegree=>3)
      phi = liftToDGMap(B,A,f)
      toComplexMap(phi,EndDegree=>3)
///

doc ///
  Key
    (homology,DGAlgebraMap)
  Headline
    The induced ring map on homology algebras
  Usage
    homologyPhi = homology phi
    homologyPhi = HH phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    homologyPhi:RingMap
      the induced map @ TT "HH(source phi) -> HH(target phi)" @
  Description
    Text
      A @ TO DGAlgebraMap @ @ TT "phi: A -> B" @ is a chain map, so it descends to a ring
      map on homology algebras.  This function computes that induced map, returning a
      @ TO RingMap @ from @ TO homologyAlgebra @ of the source to @ TO homologyAlgebra @
      of the target.  The homology algebras are computed via @ TO homologyAlgebra @ if
      they have not been cached.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3 + b^3 + c^3, a*b*c}
      K1 = koszulComplexDGA(ideal vars R, Variable => "Y")
      K2 = koszulComplexDGA(ideal{b, c}, Variable => "T")
      g = dgAlgebraMap(K1, K2, matrix{{Y_(1,2), Y_(1,3)}})
      HHg = HH g
      source HHg === homologyAlgebra K1
      target HHg === homologyAlgebra K2
  SeeAlso
    DGAlgebraMap
    homologyAlgebra
    toComplexMap
///

-- moved this out of the documentation since it is not complete
--    (homology,DGAlgebraMap,ZZ)

--    Text
--      One can also supply the second argument (a ZZ) in order to obtain the map on homology in a specified degree.
--      (This is currently not available).

doc ///
  Key
    dgAlgebraMultMap
    (dgAlgebraMultMap,DGAlgebra,RingElement)
  Headline
    Returns the chain map corresponding to multiplication by a cycle.
  Usage
    phi = dgAlgebraMultMap(A,z)
  Inputs
    A:DGAlgebra
    z:RingElement
  Outputs
    phi:ComplexMap
  Description
    Text
      If A is a DGAlgebra, and z is a cycle of A, then left multiplication of A by z gives
      a chain map from A to A.  This command converts A to a complex using @ TO toComplex @,
      and constructs a @ TO ComplexMap @ that represents left multiplication by z.
      This command is used to determine the module structure that is computed in
      @ TO homologyModule @.
    Example
      R = QQ[x,y,z]/ideal{x^3,y^3,z^3}
      KR = koszulComplexDGA R
      z1 = x^2*T_(1,1)
      phi = dgAlgebraMultMap(KR,z1)
    Text
      As you can see, the degree of phi is the homological degree of z:
    Example
      degree phi == first degree z
    Text
      Care is also taken to ensure the resulting map is homogeneous if R and z are:
    Example
      isHomogeneous phi
    Text
      One may then view the action of multiplication by the homology class of z upon
      taking the induced map in homology:
    Example
      Hphi = prune HH(phi); (Hphi_0,Hphi_1,Hphi_2)
///

doc ///
  Key
    homologyModule
    (homologyModule,DGAlgebra,Module)
  Headline
    Compute the homology of a DGModule as a module over a DGAlgebra.
  Usage
    HM = homologyModule(A,M)
  Inputs
    A:DGAlgebra
    M:Module
  Outputs
    HM:Module
  Description
    Text
      Given a DGAlgebra A over a ring R, and an R-module M, A ** M carries the structure
      of a left DG module over A.  It follows that H(A ** M) is a module over H(A).
      Although DGModules have yet to be implemented as objects in Macaulay2 in their own right,
      the current infrastructure (with a little extra work) allows us to determine the module structure
      of this type of DG module as a module over the homology algebra of A.
    Text
      Currently, this code will only work on DGAlgebras that are finite over their ring
      of definition, such as Koszul complexes.  (Truncations of) module structures in case
      of non-finite DGAlgebras may be made available in a future update.
    Text
      For an example, we will compute the module structure of the Koszul homology of
      the canonical module over the Koszul homology algebra.
    Example
      Q = QQ[x,y,z,w]
      I = ideal (w^2, y*w+z*w, x*w, y*z+z^2, y^2+z*w, x*y+x*z, x^2+z*w)
      R = Q/I
      KR = koszulComplexDGA R
      cxKR = toComplex KR
      HKR = HH(KR)
    Text
      The following is the graded canonical module of R:
    Example
      degList = first entries vars Q / degree / first
      M = Ext^4(Q^1/I,Q^{-(sum degList)}) ** R
    Text
      We obtain the Koszul homology module using the following command:
    Example
      HKM = homologyModule(KR,M);
    Text
      One may notice the duality of HKR and HKM by considering their Hilbert series:
    Example
      hsHKR = value numerator reduceHilbert hilbertSeries HKR
      hsHKM = value numerator reduceHilbert hilbertSeries HKM
      AA = ring hsHKR
      e = numgens Q
      hsHKR == T_0^e*T_1^e*sub(hsHKM, {T_0 => T_0^(-1), T_1 => T_1^(-1)})
///

doc ///
  Key
    (source,DGAlgebraMap)
  Headline
    The source of a DG algebra map
  Usage
    A = source phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    A:DGAlgebra
      the source DG algebra of @ TT "phi" @
  Description
    Text
      Returns the @ TO DGAlgebra @ from which @ TT "phi" @ is defined.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 2)
      B = acyclicClosure(S, EndDegree => 2)
      phi = liftToDGMap(B, A, map(S, R))
      source phi === A
  SeeAlso
    DGAlgebraMap
    (target, DGAlgebraMap)
    liftToDGMap
///

doc ///
  Key
    (target,DGAlgebraMap)
  Headline
    The target of a DG algebra map
  Usage
    B = target phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    B:DGAlgebra
      the target DG algebra of @ TT "phi" @
  Description
    Text
      Returns the @ TO DGAlgebra @ into which @ TT "phi" @ maps.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3, b^3, c^3}
      S = R / ideal{a^2*b^2*c^2}
      A = acyclicClosure(R, EndDegree => 2)
      B = acyclicClosure(S, EndDegree => 2)
      phi = liftToDGMap(B, A, map(S, R))
      target phi === B
  SeeAlso
    DGAlgebraMap
    (source, DGAlgebraMap)
    liftToDGMap
///

doc ///
  Key
    AssertWellDefined
  Headline
    Option to verify that a constructed DG algebra map respects differentials
  Usage
    liftToDGMap(..., AssertWellDefined => true)
    toComplexMap(..., AssertWellDefined => true)
  Description
    Text
      When set to @ TT "true" @, the constructor checks that the output
      @ TO DGAlgebraMap @ or the resulting @ TO ComplexMap @ actually commutes with the
      differentials up to the working degree.  This is a @ TT "false" @ by default
      because the check can be expensive for large acyclic closures; set it to
      @ TT "true" @ when debugging a DG map that behaves unexpectedly.
  SeeAlso
    liftToDGMap
    toComplexMap
    dgAlgebraMap
///

doc ///
  Key
    StartDegree
  Headline
    Option to specify the first homological degree processed by an iterative DG construction
  Usage
    acyclicClosure(..., StartDegree => n)
    killCycles(..., StartDegree => n)
    getGenerators(..., StartDegree => n)
  Description
    Text
      Several routines in this package build DG algebras one homological degree at a
      time, starting from degree @ TT "StartDegree" @ and incrementing up to an
      appropriate endpoint.  Raising @ TT "StartDegree" @ can be useful when a partial
      acyclic closure has already been cached and you wish to continue the construction
      from where it left off.  The default value is @ TT "1" @.
  SeeAlso
    EndDegree
    acyclicClosure
    killCycles
    getGenerators
///

doc ///
  Key
    EndDegree
  Headline
    Option to specify the last homological degree processed by an iterative DG construction
  Usage
    acyclicClosure(..., EndDegree => n)
    killCycles(..., EndDegree => n)
    isAcyclic(..., EndDegree => n)
    toComplexMap(..., EndDegree => n)
  Description
    Text
      Bounds how far an iterative DG algebra computation should proceed.  In @ TO acyclicClosure @
      this limits the homological degree of new generators adjoined; in @ TO isAcyclic @
      it is the highest degree in which homology is checked; in @ TO toComplexMap @ it
      governs how many homological degrees of the resulting chain map are computed.
    Text
      The default value varies by function and is documented on each specific bracketed
      option node.  This option is required whenever a DG algebra has generators in
      even homological degree, since the algebra is otherwise unbounded.
  SeeAlso
    StartDegree
    acyclicClosure
    killCycles
    isAcyclic
    toComplexMap
///

doc ///
  Key
    GenDegreeLimit
  Headline
    Option to bound the homological degree searched for algebra generators
  Usage
    homologyAlgebra(..., GenDegreeLimit => n)
    torAlgebra(..., GenDegreeLimit => n)
    findTrivialMasseyOperation(..., GenDegreeLimit => n)
    isHomologyAlgebraTrivial(..., GenDegreeLimit => n)
    isGolodHomomorphism(..., GenDegreeLimit => n)
  Description
    Text
      Routines that assemble a homology algebra — such as @ TO homologyAlgebra @ and
      @ TO torAlgebra @ — must, in general, walk up all homological degrees to find
      algebra generators.  When the DG algebra has generators in even degree, that
      process is infinite; @ TT "GenDegreeLimit" @ caps the search.  The answer is a
      truncation of the full homology algebra below the specified degree, and is
      exact in the bounded case.
  SeeAlso
    RelDegreeLimit
    homologyAlgebra
    torAlgebra
    findTrivialMasseyOperation
    isHomologyAlgebraTrivial
    isGolodHomomorphism
///

doc ///
  Key
    RelDegreeLimit
  Headline
    Option to bound the homological degree searched for algebra relations
  Usage
    homologyAlgebra(..., RelDegreeLimit => n)
    torAlgebra(..., RelDegreeLimit => n)
  Description
    Text
      Once generators have been chosen (see @ TO GenDegreeLimit @), a homology algebra
      computation must determine relations among them.  @ TT "RelDegreeLimit" @ bounds
      the homological degree up to which those relations are searched.  Typical practice
      is to set @ TT "RelDegreeLimit" @ to roughly twice @ TT "GenDegreeLimit" @, since
      relations can be degree-sums of generator pairs.
  SeeAlso
    GenDegreeLimit
    homologyAlgebra
    torAlgebra
///

doc ///
  Key
    TMOLimit
  Headline
    Option to bound the arity of Massey operations searched for by findTrivialMasseyOperation
  Usage
    findTrivialMasseyOperation(..., TMOLimit => n)
    isGolodHomomorphism(..., TMOLimit => n)
  Description
    Text
      A trivial Massey operation on a DG algebra is a system of null-homotopies that
      simultaneously trivialize every Massey product.  @ TT "TMOLimit" @ caps the arity
      of Massey products considered: only @ TT "k" @-fold products with @ TT "k <= n" @
      are tested for bounding.  This makes the search finite even when the full system
      would be infinite.  In practice, @ TT "TMOLimit" @ values of @ TT "3" @ or @ TT "4" @
      suffice for small examples.
  SeeAlso
    findTrivialMasseyOperation
    isGolodHomomorphism
    isGolod
///

doc ///
  Key
    [isAcyclic,EndDegree]
  Headline
    Option to specify the degree to finish checking acyclicity
  Usage
    isAcyclic(...,EndDegree=>n)
///

doc ///
  Key
    [acyclicClosure,StartDegree]
  Headline
    Option to specify the degree to start computing the acyclic closure. 
  Usage
    acyclicClosure(...,StartDegree=>n)
  Description 
   Text
    The default value is 1.
///

doc ///
  Key
    [acyclicClosure,EndDegree]
  Headline
    Option to specify the degree to stop computing the acyclic closure
  Usage
    A =  acyclicClosure(B,EndDegree=>n)
  Inputs
    B: DGAlgebra
    n: ZZ
  Description
   Text
    All generators of the acyclic closure will be found up to homological degree n.
    Note that B can be an ordinary ring, a factor ring of a polynomial ring, treated as
    a DGAlgebra generated in homological degree 0, as in the following example:
    
    The default value of EndDegree is 3.
   Example
    R = ZZ/101[x,y,z,w]/(ideal"x3,y3,z3,x2yz")
    acyclicClosure R
    acyclicClosure(R,EndDegree => 3)
///

doc ///
  Key
    [getGenerators,StartDegree]
  Headline
    First homological degree in which to search for new algebra generators
  Usage
    getGenerators(..., StartDegree => n)
  Description
    Text
      Degrees below @ TT "n" @ are skipped when walking @ TO getGenerators @ up through
      the DG algebra.  Raising @ TT "StartDegree" @ is useful when earlier degrees have
      been inspected separately.  See @ TO StartDegree @.
  SeeAlso
    getGenerators
    StartDegree
    [getGenerators,DegreeLimit]
///

doc ///
  Key
    [getGenerators,DegreeLimit]
  Headline
    Last homological degree in which to search for algebra generators
  Usage
    getGenerators(..., DegreeLimit => n)
  Description
    Text
      Caps the homological degree of candidates considered by @ TO getGenerators @.
      When the underlying DG algebra has generators in even degree, this option
      is required to make the search finite.
  SeeAlso
    getGenerators
    [getGenerators,StartDegree]
///

doc ///
  Key
    [killCycles,StartDegree]
  Headline
    First homological degree from which killCycles begins searching for cycles
  Usage
    killCycles(..., StartDegree => n)
  Description
    Text
      @ TO killCycles @ searches upward from @ TT "StartDegree" @ for the first
      degree containing a non-bounding cycle, then adjoins generators to kill
      that cycle.  See @ TO StartDegree @.
  SeeAlso
    killCycles
    StartDegree
    [killCycles,EndDegree]
///

doc ///
  Key
    [killCycles,EndDegree]
  Headline
    Last homological degree processed by killCycles
  Usage
    killCycles(..., EndDegree => n)
  Description
    Text
      Bounds the search depth of @ TO killCycles @.  By default @ TT "EndDegree => -1" @
      tells @ TO killCycles @ to stop after processing the first degree with nontrivial
      homology.  See @ TO EndDegree @.
  SeeAlso
    killCycles
    EndDegree
    [killCycles,StartDegree]
///

doc ///
  Key
    [killCycles,Variable]
  Headline
    Option to name the DG algebra generators adjoined by killCycles
  Usage
    killCycles(..., Variable => "T")
  Description
    Text
      @ TO killCycles @ adjoins new DG generators that become boundaries of previously
      non-bounding cycles.  The @ TT "Variable" @ option specifies the base name used
      for these new generators, so that subsequent calls produce readable and
      non-colliding names.
  SeeAlso
    killCycles
    acyclicClosure
///

doc ///
  Key
    [adjoinVariables,Variable]
  Headline
    Option to name the DG algebra generators adjoined by adjoinVariables
  Usage
    adjoinVariables(..., Variable => "T")
  Description
    Text
      @ TO adjoinVariables @ adjoins a new DG generator for each cycle in its input list.
      The @ TT "Variable" @ option specifies the base name used for these new generators.
  SeeAlso
    adjoinVariables
    killCycles
///

doc ///
  Key
    [acyclicClosure,Variable]
  Headline
    Option to name the DG algebra generators adjoined during acyclic closure
  Usage
    acyclicClosure(..., Variable => "T")
  Description
    Text
      As @ TO acyclicClosure @ iteratively @ TO2{killCycles, "kills cycles"} @, it adjoins
      new DG algebra generators indexed by @ TT "T_(i, j)" @ (or the given base name).
      Use the @ TT "Variable" @ option to control that base name.
  SeeAlso
    acyclicClosure
    killCycles
    Variable
///

doc ///
  Key
    [getBasis,Limit]
  Headline
    Maximum number of basis elements to return from getBasis
  Usage
    getBasis(..., Limit => n)
  Description
    Text
      Caps the size of the returned basis, which is useful when only the first few
      elements are needed (for example, when enumerating cycles degree by degree).
  SeeAlso
    getBasis
///

doc ///
  Key
    [homologyAlgebra,GenDegreeLimit]
  Headline
    Maximum homological degree searched for homology-algebra generators
  Usage
    homologyAlgebra(..., GenDegreeLimit => n)
  Description
    Text
      Required when the DG algebra has generators in even degree; see @ TO GenDegreeLimit @.
  SeeAlso
    homologyAlgebra
    GenDegreeLimit
    [homologyAlgebra,RelDegreeLimit]
///

doc ///
  Key
    [homologyAlgebra,RelDegreeLimit]
  Headline
    Maximum homological degree searched for homology-algebra relations
  Usage
    homologyAlgebra(..., RelDegreeLimit => n)
  Description
    Text
      Bounds the degree up to which relations are computed; see @ TO RelDegreeLimit @.
  SeeAlso
    homologyAlgebra
    RelDegreeLimit
    [homologyAlgebra,GenDegreeLimit]
///

doc ///
  Key
    [torAlgebra,GenDegreeLimit]
  Headline
    Maximum homological degree searched for Tor-algebra generators
  Usage
    torAlgebra(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    torAlgebra
    GenDegreeLimit
    [torAlgebra,RelDegreeLimit]
///

doc ///
  Key
    [torAlgebra,RelDegreeLimit]
  Headline
    Maximum homological degree searched for Tor-algebra relations
  Usage
    torAlgebra(..., RelDegreeLimit => n)
  Description
    Text
      See @ TO RelDegreeLimit @.
  SeeAlso
    torAlgebra
    RelDegreeLimit
    [torAlgebra,GenDegreeLimit]
///

doc ///
  Key
    [findTrivialMasseyOperation,GenDegreeLimit]
  Headline
    Maximum homological degree of generators used in the trivial Massey operation search
  Usage
    findTrivialMasseyOperation(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    findTrivialMasseyOperation
    GenDegreeLimit
    TMOLimit
///

doc ///
  Key
    [findTrivialMasseyOperation,TMOLimit]
  Headline
    Maximum arity of Massey products searched for bounding
  Usage
    findTrivialMasseyOperation(..., TMOLimit => n)
  Description
    Text
      See @ TO TMOLimit @.
  SeeAlso
    findTrivialMasseyOperation
    TMOLimit
///

doc ///
  Key
    [isHomologyAlgebraTrivial,GenDegreeLimit]
  Headline
    Maximum homological degree of generators used by isHomologyAlgebraTrivial
  Usage
    isHomologyAlgebraTrivial(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    isHomologyAlgebraTrivial
    GenDegreeLimit
///

doc ///
  Key
    [isGolodHomomorphism,GenDegreeLimit]
  Headline
    Maximum homological degree of generators used by isGolodHomomorphism
  Usage
    isGolodHomomorphism(..., GenDegreeLimit => n)
  Description
    Text
      See @ TO GenDegreeLimit @.
  SeeAlso
    isGolodHomomorphism
    GenDegreeLimit
    TMOLimit
///

doc ///
  Key
    [deviations,DegreeLimit]
  Headline
    Maximum homological degree to which deviations are computed
  Usage
    deviations(..., DegreeLimit => n)
  Description
    Text
      Bounds the homological degree of generators reported by @ TO deviations @.
      The default value is @ TT "3" @.
  SeeAlso
    deviations
    deviationsToPoincare
///

doc ///
  Key
    [isGolodHomomorphism,TMOLimit]
  Headline
    Option to cap the arity of the trivial Massey operation used in isGolodHomomorphism
  Usage
    isGolodHomomorphism(...,TMOLimit=>n)
  Description
    Text
      Passes through to @ TO findTrivialMasseyOperation @: only products of
      up to @ TT "n" @ cycle representatives are considered when searching
      for a trivial Massey operation on the Koszul homology.  See
      @ TO TMOLimit @.
  SeeAlso
    isGolodHomomorphism
    TMOLimit
    findTrivialMasseyOperation
///

doc ///
  Key
    Verbosity
  Headline
    Option controlling the verbosity level of DGAlgebras computations
  Usage
    homologyAlgebra(..., Verbosity => n)
    getGenerators(..., Verbosity => n)
  Description
    Text
      Several iterative routines in this package accept a @ TT "Verbosity" @
      option whose value is a nonnegative integer.  Higher values print more
      diagnostic output about the steps being performed (cycle searches, relation
      searches, degree-by-degree progress, etc.).  The default value is @ TT "0" @,
      which suppresses all progress output.
  SeeAlso
    homologyAlgebra
    getGenerators
///

doc ///
  Key
    [homologyAlgebra,Verbosity]
  Headline
    Option to control progress output from homologyAlgebra
  Usage
    homologyAlgebra(...,Verbosity=>n)
  Description
    Text
      Larger values of @ TT "n" @ print progress messages about the
      generator and relation searches carried out by
      @ TO homologyAlgebra @.  The default is @ TT "0" @ (no output).
  SeeAlso
    homologyAlgebra
///

doc ///
  Key
    [getGenerators,Verbosity]
  Headline
    Option to control progress output from getGenerators
  Usage
    getGenerators(...,Verbosity=>n)
  Description
    Text
      Larger values of @ TT "n" @ print progress messages about the
      cycle enumeration and independence checks in @ TO getGenerators @.
      The default is @ TT "0" @ (no output).
  SeeAlso
    getGenerators
///

doc ///
  Key
    (net,DGAlgebra)
  Headline
    Short-form display of a DG algebra
  Usage
    net A
  Inputs
    A:DGAlgebra
  Outputs
    :Net
      a one-line summary of @ TT "A" @
  Description
    Text
      Produces the text Macaulay2 prints when a @ TO DGAlgebra @ appears at the top
      level.  The display names the underlying ring, the generators of @ TT "A.natural" @,
      and any differentials known so far, giving a compact identifier rather than a
      full data dump.  Use @ TT "peek A" @ or @ TT "displayBlockDiff" @ for a detailed
      view of the internal state.
    Example
      R = ZZ/101[x,y,z] / ideal(x^3, y^3, z^3)
      A = koszulComplexDGA R
      net A
      A
  SeeAlso
    DGAlgebra
    displayBlockDiff
///

doc ///
  Key
    (net,DGAlgebraMap)
  Headline
    Short-form display of a DG algebra map
  Usage
    net phi
  Inputs
    phi:DGAlgebraMap
  Outputs
    :Net
      a short summary of @ TT "phi" @
  Description
    Text
      Produces the text Macaulay2 prints when a @ TO DGAlgebraMap @ is displayed at
      the top level.  The output lists the action of @ TT "phi" @ on the generators
      of the underlying algebra, using the information stored in @ TT "phi.natural" @.
    Example
      R = ZZ/101[a,b,c]/ideal{a^3 + b^3 + c^3, a*b*c}
      K1 = koszulComplexDGA(ideal vars R, Variable => "Y")
      K2 = koszulComplexDGA(ideal{b, c}, Variable => "T")
      g = dgAlgebraMap(K1, K2, matrix{{Y_(1,2), Y_(1,3)}})
      net g
      g
  SeeAlso
    DGAlgebraMap
    dgAlgebraMap
///

doc///
 Key 
  displayBlockDiff
  (displayBlockDiff, DGAlgebra, ZZ)
  (displayBlockDiff, DGAlgebra, Array, Array)
  (displayBlockDiff, DGAlgebra, List, List)
  (displayBlockDiff, DGAlgebra, VisibleList)  
 Headline
  Shows natural decomposition of a map in the Tate resolution
 Usage
  displayBlockDiff (A, n)
  displayBlockDiff (A, LA, LA)
  displayBlockDiff (A, L, L)
  displayBlockDiff (A, V)
 Inputs
  A:DGAlgebra
   of the sort produced by @TO acyclicClosure@ or @TO killCycles@
  n:ZZ
  LA:Array
     of the form [L] where L is a list representing a multi-index in the complex
  L:List
   of indices as above
  V:VisibleList
   a list, array or sequence representing a (single) multi-index in the complex defined by A
 Description
  Text
   For example, consider the first five steps in the resolution of the residue field
   in the following example:
  Example
   R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
   G = betti freeResolution (coker vars R, LengthLimit => 5)
  Text
   It is a free graded-commutative divided power algebra on generator of each degree starting with 1.
   We can compute the beginning of the complex corresponding to the first 3 factors with as
  Example
   A = acyclicClosure(R,EndDegree => 2)
   F = toComplex(A, 5);
   betti F
   betti F
  Text
   Since we gave @TO acyclicClosure @ the EndDegree 2, the complex produced is exact up to
   step 2; that is betti F and betti G agree up to the column 3. There are 3 chunks of generators
   in A, of homological degrees 1,2,3. 
  
   From the betti table of F 
   we see the three generators of the exterior algebra K as the linear part of the resolution.
   The second strand, 4,12,12,4 is the tensor product of K with the 4 generators of A that have
   homological degree 2 and correspond to the 4 generators of I; thus they have internal degree 3.
   We also see that A has three generators in homological degree 3 and internal degree 5.
   K multiplied by these generators accounts for 3,9,9 of the 3rd strand, while the symmetric
   square of the 4 generators of homological degree 2,  account for 0,10,0 and the product of
   these with the generators of degree 1 account for the remaining 0,0,30. Finally the 12
   in the 4th row represents the product of the 4 generators of A in homological degree with
   the 3 in degree 3.
  
   The native display of the differentials of this complex does not distinguish these pieces,
   but displayBlockDiff allows one to look at them in various ways:
  Example
   F.dd_3
   displayBlockDiff(A,3)
  Text
   Here the triples of numbers represent the number of factors from the generators of each of the three chunks of
   variables: the first index gives the number from the Koszul complex, the second from the 4 variable in homological degree
   2 and the third the number from the 3 variables of homological degree 3. Thus the sum of the
   three indices is the homological degree. The sources of the blocks are listed on the top row, the targets
   are given by the columns.
   We can see the lists of indices of the source and target with
    Example
     indices source blockDiff(A,5)
     indices target blockDiff(A,5)
  Text
   We can extract one or several blocks from F.dd_3 as follows:
  Example
   R = QQ[x,y,z]/(ideal(x^3,y^3,z^3,x*y*z))
   A = acyclicClosure(R,EndDegree => 2)
  Text
   We can specify a lists of source and target degrees, either as lists representing an 
   index for th source and an index for the target:
  Example
   displayBlockDiff(A,  {0,2,3}, {0,4,0})
  Text
   or as a pair of arrays of such lists
  Example
   displayBlockDiff(A,  [{0,2,3}], [{1,0,3},{0,4,0}])
  Text
   or look at all the blocks with a given target summand with:
  Example
   displayBlockDiff(A,  (1,0,3) )
 SeeAlso
  blockDiff
  acyclicClosure
///

doc ///
 Key
  blockDiff
  (blockDiff, DGAlgebra, ZZ)
 Headline
  prepares a map for display
 Usage
  X = blockDiff(A,n)
 Inputs
  A: DGAlgebra
  n: ZZ
 Outputs
  X:Matrix
   map between labeled direct sums
 Description
  Text
   Use @TO displayBlockDiff@ to display the blocks in various ways.
 SeeAlso
  displayBlockDiff 
///
