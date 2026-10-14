doc ///
Key
  WeylModules
Headline
  Weyl modules in positive characteristic, after S. Doty
Description
  Text
    @TT "WeylModules"@ is a Macaulay2 package for computations with Weyl
    modules for simply connected simple algebraic groups of finite type.
    It constructs integral Weyl modules and computes composition factors,
    maximal vectors, submodules, and quotients after reduction modulo a prime.
    The construction algorithms are based on S. R. Doty's GAP package
    @TT "WeylModulesNew"@.
  Text
    The Macaulay2 version also computes characters of tensor products,
    duals, and Frobenius twists.  It gives explicit quotient maps and
    chosen linear sections for quotients and subquotients, and computes
    submodule sums and intersections.  @TO reductionProfile@ compares
    reductions of the same integral module in several characteristics, and
    @TO verifyRepresentation@ checks the Chevalley--Serre relations on its
    action matrices.  Matrices for the divided powers are also available.
  Text
    @TO socleAnalysis@ records the factors and dimensions of each socle layer
    and reports when the search may be incomplete.  Decomposition matrices
    include their row and column labels, while @TO weightGraph@ and
    @TO socleDiagram@ display the weights, actions, and socle layers.
  Text
    @SUBSECTION "Example"@
  Text
    For the Cartan matrix @TT "cartanA 3"@ of
    @TEX "$A_2=\\mathfrak{sl}_3$"@, the following constructs the integral
    Weyl module @TEX "$V(2\\omega_1)$"@ and computes composition
    multiplicities, the socle, and the simple head after reduction modulo 2.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})
    dim W
    compositionFactors(W, 2)
    S = socleWeyl(W, 2);                 dim S
    Q = simpleQuotient(W, 2);            dim Q
  Text
    The reduction has socle @TEX "$L(0,1)=\\Lambda^2V$"@ and head
    @TEX "$L(2,0)=F^*V$"@, both of dimension 3.
  Text
    @SUBSECTION "Principal constructors"@
  Text
    @UL {
      {TO weylModuleZ, " constructs an integral ", TO WeylModuleZ,
       "; decomposition numbers, radicals, socles, and explicit action matrices are computed from this module."},
      {TO reduction, " reduces an already constructed integral module in a chosen characteristic; ",
       TO reductionProfile, " compares several reductions of the same integral module."},
      {TO weylModule, " constructs the reduction modulo p as a ",
       TO QuotientWeylModuleFp, " for computations with weights, characters, actions, and maximal vectors."},
      {TO simpleQuotient, " constructs the simple head directly, either from an integral module or from a characteristic and highest weight."},
      {TO subWeylModule, " and ", TO quotientWeylModule,
       " construct submodules and quotients of a given Weyl module; ",
       TO subquotientWeylModule, " constructs a quotient T/S of two submodules."}
    }@
  Text
    The constructors taking only a characteristic and a weight default to
    type A; @TT "LieType"@ selects
    another family.  With partition input, a type-A list of length
    @TT "n"@ describes @TEX "$A_{n-1}$"@, while type B, C, and D lists
    have length equal to the rank.  The coordinate conventions are
    documented under @TO WeightFormat@.
  Text
    @SUBSECTION "A nonsplit extension in type A2"@
  Text
    In type @TEX "$A_2$"@ the module @TEX "$V(2\\omega_1)$"@ is the
    divided square @TEX "$D^2V=\\Gamma^2V$"@ of the standard module.
    In characteristic 2 it fits into the @EM "nonsplit"@ short exact
    sequence
  Text
    @TEX "$0 \\longrightarrow \\Lambda^2 V \\longrightarrow D^2 V \\longrightarrow F^*V \\longrightarrow 0$"@.
  Text
    Thus @TEX "$[D^2V]=[\\Lambda^2V]+[F^*V]$"@ in the Grothendieck group,
    but @EM "not"@ @TEX "$D^2V\\cong\\Lambda^2V\\oplus F^*V$"@ as a module.
    @TO decompositionNumbers@ records only the Grothendieck-group
    multiplicities.  The calls to @TO socleWeyl@ and @TO simpleQuotient@
    above identify the bottom and top of the extension.
  Text
    @SUBSECTION "Mathematical background"@
  Text
    @SUBSECTION "Integral Weyl modules"@
  Text
    Fix a complex semisimple Lie algebra @TEX "$\\mathfrak{g}$"@ with a
    chosen triangular decomposition
    @TEX "$\\mathfrak{g}=\\mathfrak{n}^-\\oplus\\mathfrak{h}\\oplus\\mathfrak{n}^+$"@,
    Cartan subalgebra @TEX "$\\mathfrak{h}$"@, simple roots
    @TEX "$\\alpha_1,\\ldots,\\alpha_n$"@, and a Chevalley basis
    @TEX "$\\{E_\\alpha,F_\\alpha,H_i\\}$"@ in the universal enveloping
    algebra @TEX "$U(\\mathfrak{g})$"@.  For each dominant integral
    weight @TEX "$\\lambda\\in P^+$"@ there is a finite-dimensional
    irreducible @TEX "$\\mathfrak{g}_\\mathbb{C}$"@-module
    @TEX "$L(\\lambda)_\\mathbb{C}$"@ of highest weight @TEX "$\\lambda$"@.
    The corresponding Weyl module over @TEX "$\\mathbb{Z}$"@ is the lattice
    @TEX "$V_\\mathbb{Z}(\\lambda):=U_\\mathbb{Z}(\\mathfrak{g})v_\\lambda$"@
    obtained by acting on a highest-weight vector
    @TEX "$v_\\lambda\\in L(\\lambda)_\\mathbb{C}$"@ by Kostant's
    @TEX "$\\mathbb{Z}$"@-form @TEX "$U_\\mathbb{Z}(\\mathfrak{g})$"@.
  Text
    For any field @TEX "$k$"@, the module
    @TEX "$V_k(\\lambda):=V_\\mathbb{Z}(\\lambda)\\otimes_\\mathbb{Z}k$"@
    is the Weyl module of highest weight @TEX "$\\lambda$"@ over
    @TEX "$k$"@.  Its formal character is independent of @TEX "$k$"@
    and is given by Weyl's character formula.  Over @TEX "$\\mathbb{C}$"@
    it is irreducible; over a field of positive characteristic
    @TEX "$p$"@ it generally is not.  The package computes its simple
    quotient, composition factors, radical, and maximal vectors, and
    searches for its socle filtration, with the limitation described under
    @TO "WeylModules: conventions and limitations"@.
  Text
    In type @TEX "$A_{n-1}$"@, these are the Weyl functors of
    Akin--Buchsbaum--Weyman evaluated on @TEX "$k^n$"@.  More precisely,
    for a partition @TEX "$\\mu=(\\mu_1\\geq\\cdots\\geq\\mu_n\\geq0)$"@,
    the Weyl functor @TEX "$K_\\mu(k^n)$"@ restricts from
    @TEX "$\\mathrm{GL}_n$"@ to the @TEX "$\\mathrm{SL}_n$"@-module
    @TEX "$V_k(\\lambda)$"@, where
    @TEX "$\\lambda=\\sum_{i=1}^{n-1}(\\mu_i-\\mu_{i+1})\\omega_i$"@.
  Text
    Weyl functors are the Kuhn duals of Schur functors:
    @TEX "$K_\\mu(E)\\cong(S_\\mu(E^*))^*$"@ for a finite-dimensional
    vector space @TEX "$E$"@.  Here @TEX "$S_\\mu$"@ is the Schur
    functor indexed by its highest weight; it is denoted
    @TEX "$L_{\\mu'}$"@ in Akin--Buchsbaum--Weyman's notation, with
    @TEX "$\\mu'$"@ the conjugate partition.  In particular,
    @TEX "$K_{(d)}(E)=D^dE\\cong(\\operatorname{Sym}^d(E^*))^*$"@,
    while @TEX "$K_{(1^d)}(E)=\\bigwedge^dE$"@.  The divided square
    in the example above is the case @TEX "$\\mu=(2)$"@.
  Text
    @SUBSECTION "Kostant's integral form"@
  Text
    The @TEX "$\\mathbb{Z}$"@-subalgebra
    @TEX "$U_\\mathbb{Z}(\\mathfrak{g})\\subset U(\\mathfrak{g}_\\mathbb{C})$"@
    is generated by the divided powers
    @TEX "$E_\\alpha^{(r)}:=E_\\alpha^r/r!$"@ and
    @TEX "$F_\\alpha^{(r)}:=F_\\alpha^r/r!$"@, and by the binomial
    elements @TEX "$\\binom{H_i}{r}$"@, for all positive roots
    @TEX "$\\alpha$"@, simple coroots @TEX "$H_i$"@, and
    @TEX "$r\\geq0$"@.  These elements lie in
    @TEX "$U(\\mathfrak{g}_\\mathbb{C})$"@, and their PBW products have
    integral structure constants.  The reduction
    @TEX "$U_p(\\mathfrak{g}):=U_\\mathbb{Z}(\\mathfrak{g})\\otimes\\mathbb{F}_p$"@
    is the hyperalgebra of @TEX "$\\mathfrak{g}$"@.  It contains divided
    powers such as @TEX "$E_\\alpha^{(p^j)}$"@ and
    @TEX "$F_\\alpha^{(p^j)}$"@ for @TEX "$j\\geq1$"@, which are absent from the ordinary
    enveloping algebra @TEX "$U(\\mathfrak{g}_{\\mathbb{F}_p})$"@.  These
    divided powers can act nontrivially on
    @TEX "$V_\\mathbb{Z}(\\lambda)\\otimes\\mathbb{F}_p$"@ and enter the
    definition of the maximal vectors computed by @TO maximalVectors@.
  Text
    @SUBSECTION "Composition factors and the simple head"@
  Text
    Every Weyl module
    @TEX "$V_\\mathbb{Z}(\\lambda)\\otimes\\mathbb{F}_p$"@ has a unique
    simple quotient @TEX "$L(\\lambda)$"@.  The kernel is the radical
    @TEX "$\\operatorname{rad}V(\\lambda)$"@ computed by
    @TO maximalSubmodule@.  Its composition factors are the simple modules
    @TEX "$L(\\mu)$"@ with multiplicities
    @TEX "$[V(\\lambda):L(\\mu)]\\geq0$"@.
  Text
    Dominance and linkage constrain the possible composition factors:
    @TEX "$\\mu\\leq\\lambda$"@, and @TEX "$\\mu$"@ lies in the linkage
    class of @TEX "$\\lambda$"@.  Under the hypotheses of Lusztig's
    character formula, affine Kazhdan--Lusztig polynomials determine these
    multiplicities; the formula is not valid uniformly in small
    characteristic.  The integers @TEX "$[V(\\lambda):L(\\mu)]$"@ are
    the decomposition numbers returned by @TO decompositionNumbers@.
    They determine the Grothendieck-group class of the module, not a
    splitting, Loewy series, or extension class.
  Text
    @SUBSECTION "Weight coordinates"@
  Text
    The weight-handling functions accept a
    @TT "WeightFormat"@ option matching the convention used in
    @TO KacMoody@.  For classical types (A, B, C, D), weights may also be
    specified in partition/epsilon coordinates; spin weights in types B
    and D may be
    half-integral, and the last type-D coordinate may be negative.
  Example
    -- Build the 20-dimensional sl_4 module with partition (2,1,0,0)
    -- and Dynkin highest weight (1,1,0):
    A = cartanA 4
    W = weylModuleZ(A, {2, 1, 0, 0}, WeightFormat => "Partition")
    dim W
    -- Spin representation of so(5), with half-integral epsilon coordinates:
    WB = weylModuleZ(cartanB 2, {1/2, 1/2}, WeightFormat => "Partition")
    dim WB
  Text
    The dimensions are 20 and 4.  The first is the rank of the type-A Weyl lattice of highest weight @TEX "$\\omega_1+\\omega_2$"@, indexed by the partition @TEX "$(2,1,0,0)$"@; the second is the rank of the type-@TEX "$B_2$"@ spin lattice.  Half-integral epsilon coordinates describe an integral weight of the simply connected group.
  Text
    @SUBSECTION "Representative examples"@
  Text
    For @TEX "$\\mathfrak{sl}_2$"@ in characteristic 2,
    @TEX "$V(2)$"@ is the nonsplit extension
    @TEX "$0\\to L(0)\\to V(2)\\to L(2)\\to0$"@.  For @TEX "$G_2$"@,
    the 7-dimensional module @TEX "$V(\\omega_1)$"@ in characteristic 2
    has socle @TEX "$L(0,0)$"@ and a 6-dimensional simple quotient
    @TEX "$L(\\omega_1)$"@.  The type-@TEX "$B_2$"@ module
    @TEX "$V(2\\omega_1)$"@, whose characteristic-zero realization is
    the 14-dimensional traceless symmetric square, is irreducible modulo 3
    and reducible modulo 2.
  Text
    The type-A functions include the Schur-algebra decomposition matrix
    @TT "schurAlgebraDecompositionMatrix(3, 4, 3)"@ for Doty's
    characteristic-3 @TEX "$S(4,3)$"@ example and the symmetric-group
    decomposition matrix @TT "symmetricGroupDecompositionMatrix(2, 4)"@
    for @TEX "$S_4$"@ in characteristic 2.
  Text
    @SUBSECTION "Conventions for modules and weights"@
  Text
    Calculations use
    Dynkin (fundamental-weight) coordinates @TEX "$(m_1, \\dots, m_n)$"@
    with @TEX "$m_i = \\langle\\lambda, \\alpha_i^\\vee\\rangle$"@.  Roots are expressed
    in simple-root coordinates.  The Cartan matrix convention is
    @TT "KacMoody"@'s: column @TT "j"@ of @TT "A"@ is the Dynkin
    expansion of @TEX "$\\alpha_j$"@.
  Text
    @TO WeylModuleZ@ is the Weyl
    module @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ over @TEX "$\\mathbb{Z}$"@.
    Reduction modulo @TEX "$p$"@ gives the Weyl module over
    @TEX "$\\mathbb{F}_p$"@.  @TO SubWeylModuleFp@ is a @TEX "$U_p$"@-stable submodule of
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@, generated by a list of
    @TT "(weight, F_p-coefficient-vector)"@ pairs.
    @TO QuotientWeylModuleFp@ represents the quotient
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p / S$"@, used for socle and
    radical computations.  @TO SubquotientWeylModuleFp@ represents the
    quotient @TT "T/S"@ for submodules @TT "S <= T <= V"@.
    Functions such as @TO weights@ and @TO character@ apply to integral
    modules, modular submodules, quotients, and subquotients.
  Text
    @SUBSECTION "Principal operations"@
  Text
    @UL {
      {"Weights and bases: ", TO weights, ", ", TO character, ", ", TO weightSpace,
       ", ", TO basisVecs, ", ", TO highestWeight, ", ", TO moduleMetadata, "."},
      {"Reduction modulo p: ", TO maximalVectors, ", ", TO simpleQuotient,
       ", ", TO decompositionNumbers, ", ", TO reductionProfile, ", ", TO maximalSubmodule, "."},
      {"Socles and extensions: ", TO socleWeyl, ", ", TO socleSeries,
       ", ", TO socleAnalysis, ", ", TO extensionData, ", ", TO twoFactorQuotientsContaining, "."},
      {"Characters and tables: ", TO WeylCharacter, ", ", TO characterTerms,
       ", ", TO DecompositionTable, "."},
      {"Actions and relations: ", TO chevalleyActionMatrices, ", ",
       TO dividedPowerActionE, ", ", TO checkWeylModule, "."},
      {"Type-A Schur and symmetric-group functions: ", TO schurAlgebraDecompositionMatrix,
       ", ", TO symmetricGroupDecompositionMatrix, ", ", TO mullineux, "."},
      {"Roots and PBW bases: ", TO chevalleyConstants, ", ", TO verifyJacobi,
       ", ", TO pbwTuples, ", ", TO applyChevalley, "."}
    }@
  Text
    @SUBSECTION "References"@
  Text
    K. Akin, D. A. Buchsbaum, and J. Weyman,
    @HREF {"https://jerzyweyman.webflow.io/research/schur", "Schur Functors and Schur Complexes"}@,
    @EM "Advances in Mathematics"@ 44 (1982), 207--278;
    S. Doty, @HREF {"https://github.com/srdoty/WeylModulesNew", "WeylModulesNew"}@ (GAP package,
    version 2.1, 2024); W. de Graaf, @EM "Lie Algebras: Theory and
    Algorithms"@ (North-Holland, 2000); V. Kac, @EM "Infinite-dimensional
    Lie Algebras"@ (3rd edition, Cambridge UP, 1990); J. Jantzen,
    @EM "Representations of Algebraic Groups"@ (2nd edition, AMS, 2003);
    G. James and A. Kerber, @EM "The Representation Theory of the
    Symmetric Group"@ (Addison-Wesley, 1981).
Subnodes
  WeightFormat
  "Tutorial: Weyl modules in classical types"
  "Tutorial: socle structure and Doty's two-factor extensions"
  "Tutorial: the Schur algebra and the symmetric group"
  "WeylModules: conventions and limitations"
  "WeylModules: function index"
SeeAlso
  KacMoody
  weylModuleZ
  simpleQuotient
  decompositionNumbers
  schurAlgebraDecompositionMatrix
  "Tutorial: Weyl modules in classical types"
  "Tutorial: socle structure and Doty's two-factor extensions"
  "Tutorial: the Schur algebra and the symmetric group"
  "WeylModules: function index"
///

doc ///
Key
  "WeylModules: conventions and limitations"
Headline
  Coordinate conventions, interpretation, and computational limitations
Description
  Text
    @SUBSECTION "Coordinates, type, and rank"@
  Text
    @TO weylModuleZ@ uses Dynkin coordinates by default.  The constructors
    @TO weylModule@ and @TO simpleQuotient@ use classical
    epsilon/partition coordinates by default and select type A unless
    @TT "LieType"@ says otherwise.  In @TT "KacMoody"@ notation,
    @TT "cartanA n"@ is type @TEX "$A_{n-1}$"@, whereas
    @TT "cartanB n"@, @TT "cartanC n"@, and @TT "cartanD n"@ have rank
    @TT "n"@.  List lengths are interpreted relative to the selected
    coordinate format and Cartan type; the complete conventions are given
    under @TO WeightFormat@.
  Text
    @SUBSECTION "Primes and coefficient vectors"@
  Text
    Modular constructors require a prime @TT "p"@ and raise an error for
    composite input.  A generator passed to @TO subWeylModule@ or
    @TO isWithin@ is a coefficient list in the package's chosen basis of
    one weight space; its length must equal that weight-space dimension.
    The corresponding basis is returned by @TO weightSpace@.
  Text
    @SUBSECTION "Empty results, null results, and basis coordinates"@
  Text
    An empty list means that an enumeration found no elements.  @TT "null"@
    is used when an operation is not defined at the requested weight or no
    next term exists; the individual help pages state their convention.
    Quotient vectors returned by @TO maximalVectors@ use the quotient basis,
    while generators returned by the socle-filtration routines are lifts
    written in the ambient Weyl-module weight-space basis.
    @TO quotientProjection@ and @TO quotientSection@ give the coordinate
    maps explicitly.  The section selects ambient representatives; it is
    noncanonical and need not be a homomorphism of representations.
  Text
    For a quotient @TEX "$Q=V/S$"@, @TO socleSeries@ returns cumulative
    preimages inside @TT "V"@.  Consequently the displayed dimensions
    include @TEX "$\\dim S$"@.  Thus the dimension of the corresponding
    subspace of @TT "Q"@ is @TT "dim T - dim definingKernel Q"@ for each
    returned preimage @TT "T"@.
  Text
    @SUBSECTION "Composition factors are not direct summands"@
  Text
    @TO decompositionNumbers@ and @TO decomposeCharacter@ determine a
    Grothendieck-group class.  They do not determine a submodule lattice,
    a Loewy series, a splitting, or an extension class.  Structural
    information is provided by @TO maximalSubmodule@, @TO socleWeyl@,
    @TO socleSeries@, and @TO twoFactorQuotientsContaining@.
  Text
    @SUBSECTION "Socle-search limitation"@
  Text
    At a weight with a multidimensional maximal-vector space, the underlying
    socle search tests the vectors in one chosen basis separately.  It
    does not search every nonzero linear combination.  When all relevant
    maximal-vector spaces are one-dimensional, the computed result is the
    full socle filtration.  @TO socleAnalysis@ checks this condition in every
    successive quotient and records whether the full filtration has been
    determined.  If this condition fails, the result is marked
    @TT "socle search may be incomplete"@.
    The option @TT "RequireComplete => true"@ makes
    @TO socleAnalysis@, @TO socleWeyl@, @TO socleSeries@, and the two-factor
    search raise an error when completeness has not been established.
  Text
    @SUBSECTION "Checking the representation relations"@
  Text
    @TO verifyRepresentation@ checks the dimensions of the weight spaces,
    their chosen bases, and the action matrices.  With its default setting,
    it also checks all the ordinary Chevalley--Serre relations.
    These calculations do not prove that the constructor is correct for
    every root datum and highest weight, nor do they check all straightening
    identities in Kostant's integral form for arbitrary divided powers.
    The divided-power matrices can be inspected independently with
    @TO dividedPowerActionE@, @TO dividedPowerActionF@, and
    @TO binomialCartanAction@.
  Text
    @SUBSECTION "Size of the calculations and integral bases"@
  Text
    The package constructs explicit PBW bases and action matrices, so time
    and memory can grow rapidly with rank and highest weight.  Even a
    fundamental-weight module in an exceptional type can be substantially
    slower than the classical examples in this guide.  The option
    @TT "Verbose => true"@ to @TO weylModuleZ@ reports construction-stage
    timings.
  Text
    The constructor computes rational kernels and chooses PBW representatives.
    It returns a module only if the quotient projections and action matrices
    have integral entries.  If this fails, it raises an error; it does not
    yet find a saturated lattice using Smith or Hermite normal form.
    The PBW module constructor for types B, C, F, and G also requires the
    canonical node ordering supplied by the corresponding @TT "KacMoody"@
    Cartan constructor.  This restriction is separate from classical weight
    conversion: @TO weightAsPartition@ and @TO weightAsDynkin@ accept
    simultaneously permuted Cartan data in types B, C, and D.
SeeAlso
  WeylModules
  WeightFormat
  weylModuleZ
  weightSpace
  decompositionNumbers
  socleWeyl
  socleSeries
///

doc ///
Key
  "Tutorial: Weyl modules in classical types"
Headline
  Classical types and epsilon-coordinate input
Description
  Text
    For classical types A, B, C, and D, a highest weight
    @TEX "$\\lambda$"@ may be specified in @TEX "$\\varepsilon$"@-coordinates
    rather than by Dynkin labels.  Select these coordinates with
    @TT "WeightFormat => \"Partition\""@, following the convention in
    @TT "KacMoody"@.  In type D and for spin weights, the entries may be
    signed or half-integral, so the input need not be an ordinary partition.
  Text
    @SUBSECTION "Type A"@
  Text
    A partition labels a polynomial @TEX "$GL_n$"@ representation.  The
    partition @TEX "$\\lambda = (\\lambda_1, \\dots, \\lambda_n)$"@ for
    @TEX "$\\mathrm{sl}_n$"@ corresponds to the Dynkin label
    @TEX "$(\\lambda_1 - \\lambda_2, \\dots, \\lambda_{n-1} - \\lambda_n)$"@.
    The integral Weyl module @TEX "$V(k\\omega_1)$"@ is the divided power
    @TEX "$\\Gamma^k V$"@ of the standard representation.  It becomes
    the symmetric power after extending scalars to characteristic zero:
  Example
    A = cartanA 3                                 -- sl_3
    W = weylModuleZ(A, {3, 0, 0}, WeightFormat => "Partition")
    dim W                                          -- 10
    highestWeight W                                -- Dynkin: (3, 0)
    highestWeight(W, WeightFormat => "Partition")  -- (3, 0, 0)
    sortedCharacter character(W, WeightFormat => "Partition")
  Text
    The output identifies the rank-ten lattice @TEX "$D^3(\\mathbb Z^3)$"@.  The two highest-weight lists represent the same weight: @TEX "$3\\omega_1$"@ in Dynkin coordinates and @TEX "$(3,0,0)$"@ in partition coordinates.  Each character entry gives the rank of the corresponding weight space.
  Text
    @SUBSECTION "Type B: spin representations"@
  Text
    In the
    epsilon-basis, the spin module of @TEX "$\\mathrm{so}(5)$"@ has
    highest weight @TEX "$(1/2, 1/2)$"@ -- @TT "WeightFormat"@ accepts
    rationals here.
  Example
    B = cartanB 2                                 -- so(5)
    Wspin = weylModuleZ(B, {1/2, 1/2}, WeightFormat => "Partition")
    dim Wspin                                      -- 4 = dimension of the spin representation
    highestWeight(Wspin, WeightFormat => "Partition")
    -- The 5-dimensional standard representation is V(omega_1):
    Wstd = weylModuleZ(B, {1, 0}, WeightFormat => "Partition")
    dim Wstd
    -- In characteristic zero, Sym^2 of the standard with its trace removed
    -- is V(2 omega_1):
    Wsym2 = weylModuleZ(B, {2, 0}, WeightFormat => "Partition")
    dim Wsym2                                      -- 14
  Text
    The three dimensions @TEX "$4,5,14$"@ belong respectively to the spin module, the natural module, and the Weyl module of highest weight @TEX "$2\\omega_1$"@.  The half-integral spin label is an epsilon-coordinate expression for the fundamental weight @TEX "$\\omega_2$"@.
  Text
    @SUBSECTION "Output in epsilon coordinates"@
  Text
    When the
    output of @TO decompositionNumbers@ is requested in partition
    format, the keys are the partition labels of the simple constituents:
  Example
    A = cartanA 4
    W = weylModuleZ(A, {2, 1, 0, 0}, WeightFormat => "Partition")
    dim W                                          -- = 20
    decompositionNumbers(W, 2)                     -- Dynkin keys
    decompositionNumbers(W, 2, WeightFormat => "Partition")
  Text
    Both tables describe the same composition factors in characteristic 2.  A key @TEX "$\\mu$"@ with value @TEX "$m$"@ means that @TEX "$L(\\mu)$"@ occurs @TEX "$m$"@ times in a composition series of the rank-twenty Weyl module.  Changing weight coordinates changes the labels, not the multiplicities.
SeeAlso
  weylModuleZ
  WeightFormat
  decompositionNumbers
  weightAsPartition
  weightAsDynkin
///

doc ///
Key
  "Tutorial: socle structure and Doty's two-factor extensions"
Headline
  Socle filtrations and two-factor quotients
Description
  Text
    Let @TEX "$S \\subseteq V(\\lambda) \\otimes \\mathbb{F}_p$"@.  The
    following functions compute the socle filtration of the quotient
    @TEX "$(V(\\lambda) \\otimes \\mathbb{F}_p)/S$"@ and selected
    two-factor quotients.
  Text
    @TO socleSeries@ computes a chain of submodules
    @TEX "$S \\subseteq S_1 \\subseteq S_2 \\subseteq \\cdots \\subseteq V$"@.
    Subject to the completeness condition below, @TEX "$S_k/S$"@ is the
    @TT "k"@th term of the socle filtration of @TEX "$V/S$"@.
    @TO gensSocleLayers@ records the
    generators added at each step.  @TO socleLengthTwoQuotient@ enlarges
    @TT "S"@ by lower socle layers to produce a quotient of socle length
    at most 2.  At a weight with a multidimensional maximal-vector space,
    the routines test only the chosen basis directions.  The returned chain
    is known to give the full socle filtration when all relevant maximal-vector
    spaces are one-dimensional, as specified under @TO socleWeyl@.
  Text
    @TO socleAnalysis@ checks for ambiguity in every successive quotient
    and records the factor multiplicities and dimension checks for each
    layer.  With @TT "RequireComplete => true"@ it raises an error if the
    search does not determine the full socle filtration.
  Example
    A = cartanA 4                                  -- sl_4
    W = weylModuleZ(A, {3, 0, 0})                  -- V(3 omega_1) = Gamma^3 std
    decompositionNumbers(W, 2)                     -- in char 2
    series = socleSeries(W, 2);
    apply(series, S -> dim S)
    layers = gensSocleLayers(W, 2);
    apply(layers, layer -> apply(layer, x -> x#0))
    R = socleAnalysis(W, 2)
    isCertified R
    socleLayerFactors R
  Text
    The cumulative dimensions @TEX "$\\{4,20\\}$"@ give a four-dimensional socle @TEX "$L(0,0,1)$"@ and a sixteen-dimensional head @TEX "$L(3,0,0)$"@.  Thus the two socle layers have dimensions 4 and 16.  The value @TT "true"@ says that the maximal-vector search has determined the full socle filtration in this example.
  Text
    @SUBSECTION "The G_2 module V(3,0)"@
  Text
    At @TEX "$p = 2$"@, the computation using the chosen maximal-vector bases for
    @TEX "$V(3 \\omega_1)$"@ of @TEX "$G_2$"@ returns six layers and a
    42-dimensional two-layer candidate labeled
    @TEX "$L(\\omega_1) | L(3\\omega_1)$"@.  This example has a
    maximal-vector space of dimension greater than one, so these commands
    do not by themselves determine the full socle filtration.  These
    values agree with those reported by Doty:
  Example
    G = cartanG2()
    WG = weylModuleZ(G, {3, 0});
    dim WG                                          -- 77
    S0 = subWeylModule(WG, 2, {});
    Q = socleLengthTwoQuotient S0;
    dim Q                                           -- 42
    apply(gensSocleLayers Q, layer -> apply(layer, x -> x#0))
    RG = socleAnalysis(WG, 2)
    isCertified RG                                  -- false
    ambiguousWeightsByLayer RG
  Text
    The quotient found has dimension 42, with recorded bottom and top labels @TEX "$(1,0)$"@ and @TEX "$(3,0)$"@.  The value @TT "false"@ for the ambient module warns that the six recorded layers need not be its full socle filtration: the first two quotient stages encounter an ambiguous maximal-vector space at weight @TEX "$(2,0)$"@.
  Text
    @SUBSECTION "Two-factor quotients"@
  Text
    @TO twoFactorQuotientsContaining@ searches inside @TEX "$V/S$"@ for
    quotients with two composition factors.  A module
    with two composition factors can still be a direct sum.  Here the
    retained quotients have computed socle length two and a simple socle;
    when the socle search is complete, they are nonsplit extensions.
    The routine is not an exhaustive computation or parametrization of an
    @TEX "$\\mathrm{Ext}^1$"@ group.
    Passing @TT "RequireComplete => true"@ makes the search raise an error
    when it encounters a maximal-vector space of dimension greater than one.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})                      -- V(2 omega_1) for sl_3
    S0 = subWeylModule(W, 2, {});
    tfqs = twoFactorQuotientsContaining S0;
    #tfqs                                            -- one quotient in this example
    Q = first tfqs;
    apply(gensSocleLayers Q, layer -> apply(layer, x -> x#0))
  Text
    There is one quotient in the returned list.  Its layers have simple factors @TEX "$L(0,1)$"@ below @TEX "$L(2,0)$"@, so it is the nonsplit extension @TEX "$0\\to\\Lambda^2 k^3\\to D^2 k^3\\to(k^3)^{(1)}\\to0$"@ in characteristic 2.  These layer labels assert no direct-sum decomposition.
SeeAlso
  socleSeries
  gensSocleLayers
  socleLengthTwoQuotient
  twoFactorQuotientsContaining
  socleWeyl
  socleAnalysis
  extensionData
///

doc ///
Key
  "Tutorial: the Schur algebra and the symmetric group"
Headline
  Decomposition matrices for S(n,r) and S_n in characteristic p
Description
  Text
    For each partition @TEX "$\\mu$"@ of @TT "r"@ with at most @TT "n"@
    parts, let @TEX "$V(\\mu)$"@ be the corresponding Weyl module for
    @TEX "$\\mathrm{sl}_n$"@ in characteristic @TEX "$p$"@.  The
    decomposition matrix of the Schur
    algebra @TEX "$S(n, r)$"@ records the multiplicities of the simple
    factors @TEX "$L(\\nu)$"@ in @TEX "$V(\\mu)$"@.  The columns indexed
    by @TT "p"@-restricted partitions give the decomposition matrix of
    the symmetric group @TEX "$S_r$"@ on the dual Specht modules.
  Text
    @SUBSECTION "S(4, 3) in characteristic 3"@
  Text
    Rows and columns are indexed
    by the partitions @TEX "$(3),(2,1),(1,1,1)$"@, in that order:
  Example
    T = schurAlgebraDecompositionTable(3, 4, 3)     -- S(4,3), char 3
    rowLabels T
    columnLabels T
    matrix T
  Text
    The entry in row @TEX "$\\lambda$"@, column @TEX "$\\mu$"@ is @TEX "$[V(\\lambda):L(\\mu)]$"@ for the Schur algebra @TEX "$S(4,3)$"@ in characteristic 3.  Thus the first row gives one copy each of the simples labeled @TEX "$(3,0,0,0)$"@ and @TEX "$(2,1,0,0)$"@; the last row says that the exterior cube is simple.
  Text
    @SUBSECTION "S_4 in characteristic 2"@
  Example
    ST = symmetricGroupDecompositionTable(2, 4)
    rows = rowLabels ST
    columns = columnLabels ST
    SM = matrix ST
    -- 5 partitions of 4 (all rows), 2 columns (= 2-restricted partitions)
    rows, columns
    numrows SM, numcols SM
    -- A single sparse decomposition row:
    symmetricGroupDecompositionNumbers(2, {3, 1})
  Text
    The five rows are labeled by all partitions of 4, while the two columns use the package's 2-restricted labeling of the simple @TEX "$k[S_4]$"@-modules.  The row labeled @TEX "$(3,1)$"@, and the sparse table printed last, give one copy of each simple.  The table records composition multiplicities and does not assert that the corresponding Specht module splits.
  Text
    @TO mullineux@ is the @TT "p"@-Mullineux involution; together with
    @TO conjugatePartition@, @TO pRestrictedPartitions@, and
    @TO pRegularPartitions@, it relates the @TT "p"@-regular and
    @TT "p"@-restricted labeling conventions for modular irreducibles.
SeeAlso
  schurAlgebraDecompositionMatrix
  schurAlgebraDecompositionTable
  symmetricGroupDecompositionMatrix
  symmetricGroupDecompositionTable
  symmetricGroupDecompositionNumbers
  schurAlgebraWeylModule
  mullineux
  pRestrictedPartitions
  compositionToWeight
///

doc ///
Key
  WeightFormat
Headline
  Input and output of weights in partition (epsilon-basis) coordinates
Description
  Text
    Most weight-handling functions in @TT "WeylModules"@ accept the
    option @TT "WeightFormat"@, which normally takes one of two values:
    @TT "\"Dynkin\""@ (the default), in which case weights are tuples
    @TEX "$(m_1, \\ldots, m_n)$"@ of fundamental-weight coefficients;
    or @TT "\"Partition\""@, in which case weights use classical
    @TEX "$\\varepsilon$"@-coordinates: length @TT "n+1"@ in type
    @TEX "$A_n$"@ and length @TT "n"@ in types @TEX "$B_n,C_n,D_n$"@.
    Calculations use Dynkin coordinates.  The option selects input coordinates for
    @TO weylModuleZ@, @TO subWeylModule@, @TO maximalVectors@, and related
    functions, and output coordinates for @TO character@, @TO weights@,
    @TO decompositionNumbers@, and related functions.
    Half-integer (spin) weights are allowed in types B and D.  The
    @TO weylModule@ and @TO simpleQuotient@ constructors also
    accept @TT "\"Auto\""@ (their default), meaning partition coordinates
    for classical types and Dynkin coordinates for exceptional types.
  Text
    @SUBSECTION "Coordinate conventions"@
  Text
    Dynkin coordinates express a weight in the basis of fundamental
    weights; the Cartan matrix and Weyl character formula use these
    coordinates.  Epsilon coordinates express the same weight
    in the standard orthogonal realization of a classical root system;
    in type A these are the partition coordinates used by Schur--Weyl and
    symmetric-group functions.
  Text
    @SUBSECTION "Dynkin coordinates"@
  Text
    These are the rank-@TT "n"@ tuples
    @TEX "$(m_1, \\ldots, m_n)$"@ with
    @TEX "$m_i = \\langle\\mu, \\alpha_i^\\vee\\rangle$"@.  This is the
    coefficient vector in the basis of fundamental weights
    @TEX "$\\omega_1, \\ldots, \\omega_n$"@.  The package stores weights
    in these coordinates.  In the @TT "KacMoody"@ convention, the columns
    of the Cartan matrix are the Dynkin coordinates of the simple roots.
  Text
    @SUBSECTION "Partition / epsilon coordinates"@
  Text
    These are tuples giving the
    expansion of @TEX "$\\mu$"@ in the orthogonal basis
    @TEX "$\\varepsilon_1, \\ldots, \\varepsilon_N$"@, where
    @TEX "$N=n+1$"@ for @TEX "$A_n$"@ and @TEX "$N=n$"@ for
    @TEX "$B_n,C_n,D_n$"@.  Type A uses length @TT "n+1"@ (with one
    common-shift direction quotiented away); types B/C/D use length
    @TT "n"@.  Spin weights in B/D have half-integer entries.
    For type A, the canonical dominant representative is an ordinary
    partition.  In types B and C, dominant epsilon coordinates are weakly
    decreasing and nonnegative, with half-integers possible in type B.  In
    type D they satisfy
    @TEX "$\\lambda_1\\geq\\cdots\\geq\\lambda_{n-1}\\geq|\\lambda_n|$"@
    and may be half-integral.  Thus the option name @TT "Partition"@ is
    shorthand for classical epsilon coordinates; the output is not always
    an ordinary integer partition.  General weight-space labels need not be
    dominant, nonnegative, or weakly decreasing.
  Text
    Conversion is defined on the entire weight lattice, not only on its
    dominant cone.  In types B, C, and D the package obtains the
    fundamental-weight-to-epsilon change-of-basis matrix from the supplied
    Cartan datum.  Consequently the conversion routines also respect a
    simultaneous permutation of the rows and columns of a classical Cartan
    matrix; Dynkin coordinates are then understood in that permuted node
    order.  The formulas below are written in the standard @TT "KacMoody"@
    node order.
  Text
    @SUBSECTION "Conversion formulas"@
  Text
    Let
    @TEX "$(m_1, \\ldots, m_n)$"@ denote Dynkin coordinates and
    @TEX "$(\\lambda_1, \\lambda_2, \\ldots)$"@ the epsilon coordinates.
  Text
    @SUBSECTION "Type A_n"@
  Text
    For @TT "cartanA(n+1)"@,
    @TEX "$\\lambda_i - \\lambda_{i+1} = m_i$"@ for @TT "i = 1..n"@.
    Equivalently, @TEX "$\\lambda_i = m_i + m_{i+1} + \\cdots + m_n + \\lambda_{n+1}$"@,
    with the canonical choice @TEX "$\\lambda_{n+1} = 0$"@.
    Tuples differing by a multiple of @TEX "$(1,1,\\ldots,1)$"@ represent
    the same @TEX "$\\mathrm{SL}_{n+1}$"@ weight.
  Text
    @SUBSECTION "Type B_n"@
  Text
    With the last node short and the spin representation at the last node,
    @TEX "$\\lambda_i - \\lambda_{i+1} = m_i$"@ for @TT "i = 1..n-1"@,
    and @TEX "$2 \\lambda_n = m_n$"@.  For a dominant integral weight,
    even @TT "m_n"@ gives an ordinary partition, while odd @TT "m_n"@ gives
    @TEX "$\\lambda \\in (\\mathbb{Z} + 1/2)^n$"@ -- a half-integer spin
    weight.
  Text
    @SUBSECTION "Type C_n"@
  Text
    With the last node long, @TEX "$\\lambda_i - \\lambda_{i+1} = m_i$"@
    for @TT "i = 1..n-1"@, and @TEX "$\\lambda_n = m_n$"@.
    A dominant integral weight gives an ordinary integer partition.
  Text
    @SUBSECTION "Type D_n"@
  Text
    With the fork at the end,
    @TEX "$\\lambda_i - \\lambda_{i+1} = m_i$"@ for @TT "i = 1..n-2"@,
    @TEX "$\\lambda_{n-1} - \\lambda_n = m_{n-1}$"@, and
    @TEX "$\\lambda_{n-1} + \\lambda_n = m_n$"@.  The two half-spin representations
    are distinguished by the sign of @TEX "$\\lambda_n$"@.
  Text
    @SUBSECTION "Example: the standard representation of sl_3"@
  Text
    The three weights are @TEX "$\\varepsilon_1, \\varepsilon_2, \\varepsilon_3$"@.
    In Dynkin coordinates these are @TEX "$(1,0)$"@, @TEX "$(-1,1)$"@,
    @TEX "$(0,-1)$"@ -- i.e. @TEX "$\\omega_1$"@,
    @TEX "$-\\omega_1 + \\omega_2$"@, and @TEX "$-\\omega_2$"@
    (drop @TEX "$\\alpha_1 = (2,-1)$"@ then @TEX "$\\alpha_2 = (-1,2)$"@
    starting from the highest weight).  In partition form they are
    @TEX "$(1,0,0)$"@, @TEX "$(0,1,0)$"@, and @TEX "$(-1,-1,0)$"@: the
    first two are @TEX "$\\varepsilon_1, \\varepsilon_2$"@ verbatim, and
    the third is the canonical SL-representative of @TEX "$\\varepsilon_3$"@
    (since for @TEX "$\\mathrm{sl}_3$"@ we have
    @TEX "$\\varepsilon_1 + \\varepsilon_2 + \\varepsilon_3 = 0$"@,
    so @TEX "$\\varepsilon_3 = -\\varepsilon_1 - \\varepsilon_2 = (-1,-1,0)$"@).
  Example
    A = cartanA 3
    W = weylModuleZ(A, {1, 0})
    weights W                                          -- Dynkin
    weights(W, WeightFormat => "Partition")            -- partition / epsilon-basis
  Text
    The lists describe the same three weights of the natural @TEX "$\\mathrm{SL}_3$"@-module.  The Dynkin labels are @TEX "$(1,0),(-1,1),(0,-1)$"@.  Their partition representatives differ from the usual epsilon vectors by a common coordinate shift, which has no effect on an @TEX "$\\mathrm{SL}_3$"@ weight.
  Text
    @SUBSECTION "Example: type A3"@
  Text
    For
    @TEX "$\\mathrm{sl}_4$"@ pass the partition @TEX "$(2, 1, 0, 0)$"@
    (Dynkin label @TEX "$(1, 1, 0)$"@, whose dimension is 20):
  Example
    A = cartanA 4
    W = weylModuleZ(A, {2, 1, 0, 0}, WeightFormat => "Partition")
    dim W
    highestWeight W                                    -- Dynkin
    highestWeight(W, WeightFormat => "Partition")
  Text
    The dimension is 20.  The lists @TEX "$(1,1,0)$"@ and @TEX "$(2,1,0,0)$"@ name the same highest weight of @TEX "$\\mathrm{SL}_4$"@, since consecutive differences of the partition give its Dynkin labels.
  Example
    -- Type C: integer entries; epsilon-basis matches partition.
    C = cartanC 3
    Wstd = weylModuleZ(C, {1, 0, 0}, WeightFormat => "Partition")
    dim Wstd                                         -- 6 = dimension of the standard representation of sp(6)
    -- Type D: half-integer (spin) input.
    D = cartanD 4
    Wspin = weylModuleZ(D, {1/2, 1/2, 1/2, 1/2}, WeightFormat => "Partition")
    dim Wspin                                        -- 8 (one of the half-spin reps)
  Text
    The dimensions 6 and 8 are those of the natural @TEX "$\\mathrm{Sp}_6$"@-module and a half-spin module for @TEX "$\\mathrm{Spin}_8$"@.  Integer epsilon coordinates are required in type C; the half-integral type-D tuple is a weight of the simply connected spin group.
  Example
    -- Conversion is not restricted to dominant weights.
    weightAsDynkin(cartanB 2, {-1/2, 1/2})
    weightAsDynkin(cartanC 3, {0, -1, 1})
    weightAsDynkin(cartanD 4, {1, -1, 2, -2})
    -- Simultaneously permuted Cartan data use the corresponding node order.
    B = cartanB 3
    perm = {2, 0, 1}
    Bperm = submatrix(B, perm, perm)
    mu = {2, -1, 1}
    weightAsDynkin(Bperm, weightAsPartition(Bperm, mu)) == mu
  Text
    The first three outputs are Dynkin labels of weights that need not be dominant.  The final @TT "true"@ shows that conversion to epsilon coordinates and back preserves the weight even after a simultaneous permutation of the Cartan rows and columns.
  Text
    @TO weightAsPartition@, @TO weightAsDynkin@,
    @TO characterAsPartition@, and @TO characterAsDynkin@ convert
    individual weights and characters between the two coordinate systems.
  Example
    W = weylModuleZ(cartanA 4, {2, 0, 0})
    weightAsPartition(W, {2, 0, 0})
    weightAsDynkin(W, {2, 1, 1, 0})
  Text
    The highest weight @TEX "$2\\omega_1$"@ becomes @TEX "$(2,0,0,0)$"@.  Conversely, consecutive differences of @TEX "$(2,1,1,0)$"@ give @TEX "$(1,0,1)$"@, a different weight in the same type-A weight lattice.
SeeAlso
  weylModuleZ
  weights
  character
  weightAsPartition
  weightAsDynkin
  characterAsPartition
  characterAsDynkin
///

doc ///
Key
  classifyFiniteCartan
  (classifyFiniteCartan, Matrix)
Headline
  Classify a finite-type Cartan matrix as one of A_n / B_n / ... / G_2
Usage
  cls = classifyFiniteCartan A
Inputs
  A:Matrix
    a Cartan matrix
Outputs
  cls:Sequence
    a pair @TT "(\"X\", n)"@ where @TT "\"X\""@ is one of @TT "\"A\""@,
    @TT "\"B\""@, @TT "\"C\""@, @TT "\"D\""@, @TT "\"E\""@,
    @TT "\"F\""@, @TT "\"G\""@ and @TT "n"@ is the rank, or @TT "null"@
    if @TT "A"@ does not match any standard finite-type Cartan in
    KacMoody's convention
Description
  Text
    Compares @TT "A"@ against the standard Cartan-matrix constructors
    @TT "cartanA(n+1)"@, @TT "cartanB n"@, @TT "cartanC n"@,
    @TT "cartanD n"@, @TT "cartanE k"@, @TT "cartanF4()"@,
    @TT "cartanG2()"@ from @TT "KacMoody"@.
    @TO chevalleyConstants@ uses this classification to choose its
    construction of the structure constants.
  Example
    classifyFiniteCartan cartanA 3
    classifyFiniteCartan cartanB 3
    classifyFiniteCartan cartanG2()
    classifyFiniteCartan cartanF4()
  Text
    The pairs give the Dynkin family and its rank: @TEX "$A_2,B_3,G_2,F_4$"@.  In particular @TT "cartanA 3"@ describes @TEX "$\\mathfrak{sl}_3$"@, whose rank is 2, rather than type @TEX "$A_3$"@.
SeeAlso
  chevalleyConstants
///

doc ///
Key
  isSimplyLaced
  (isSimplyLaced, Matrix)
Headline
  Test whether a Cartan matrix is simply-laced (type ADE)
Usage
  isSimplyLaced A
Inputs
  A:Matrix
    a generalized Cartan matrix
Outputs
  :Boolean
    true if @TT "A"@ is finite type and simply-laced
Description
  Text
    Returns @TT "true"@ when @TT "A"@ is a finite-type Cartan matrix all
    of whose simple roots have the same norm -- equivalently, when the
    diagonal symmetrizer of @TT "A"@ is a scalar multiple of the
    identity.  This corresponds to the types @TEX "$A_n$"@,
    @TEX "$D_n$"@, @TEX "$E_6$"@, @TEX "$E_7$"@, @TEX "$E_8$"@.
  Example
    isSimplyLaced cartanA 4         -- type A_3
    isSimplyLaced cartanD 4
    isSimplyLaced cartanE 6
    isSimplyLaced cartanB 3
    isSimplyLaced cartanG2()
  Text
    The three @TT "true"@ values correspond to types A, D, and E, whose roots all have the same length.  Types B and G have two root lengths, giving @TT "false"@.
SeeAlso
  chevalleyConstants
///

doc ///
Key
  chevalleyEpsilonMatrix
  (chevalleyEpsilonMatrix, Matrix)
Headline
  The epsilon matrix of an ADE-type Cartan matrix
Usage
  M = chevalleyEpsilonMatrix A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  M:Matrix
    an @TEX "$r \\times r$"@ matrix of @TEX "$\\pm 1$"@'s
Description
  Text
    Returns the @TEX "$r \\times r$"@ matrix @TT "M"@ used to construct the
    Frenkel-Kac 2-cocycle @TEX "$\\varepsilon$"@ that supplies the signs
    of the Chevalley structure constants.  The convention follows GAP's
    @TT "lib/algliess.gi"@: @TEX "$M_{i,i} = -1$"@ on the diagonal,
    @TEX "$M_{i,j} = +1$"@ for @TEX "$j < i$"@ (lower triangle), and
    @TEX "$M_{i,j} = (-1)^{A_{i,j}}$"@ for @TEX "$j > i$"@ (upper
    triangle).
  Text
    For ADE types, @TEX "$A_{i,j} \\in \\{-1, 0\\}$"@ when @TEX "$i \\ne j$"@,
    so the upper-triangular entries are @TEX "$-1$"@ when @TEX "$i$"@ and
    @TEX "$j$"@ are connected in the Dynkin diagram and @TEX "$+1$"@
    otherwise.
  Example
    chevalleyEpsilonMatrix cartanA 3   -- type A_2
    chevalleyEpsilonMatrix cartanA 4   -- type A_3
    chevalleyEpsilonMatrix cartanD 4
  Text
    For type @TEX "$A_2$"@, the matrix is @TEX "$\\left(\\begin{smallmatrix}-1&-1\\\\1&-1\\end{smallmatrix}\\right)$"@.  The upper off-diagonal entry is negative because the two simple roots are adjacent; the lower entry is positive by the chosen cocycle convention.  The larger matrices apply the same rule to the @TEX "$A_3$"@ and @TEX "$D_4$"@ diagrams.
  Text
    An error is raised for non-simply-laced types.
SeeAlso
  chevalleyConstants
///

doc ///
Key
  chevalleyConstants
  (chevalleyConstants, Matrix)
Headline
  Chevalley structure constants of a finite-type simple Lie algebra
Usage
  N = chevalleyConstants A
Inputs
  A:Matrix
    a finite-type Cartan matrix (any of A, B, C, D, E, F, G in KacMoody convention)
Outputs
  N:HashTable
    an immutable hash table keyed on pairs of roots, valued in integers
Description
  Text
    Returns a hash table @TT "N"@ such that, for every ordered pair
    @TT "(alpha, beta)"@ of roots with @TT "alpha + beta"@ a nonzero
    root, the entry @TT "N#(alpha, beta)"@ is the integer
    @TEX "$N(\\alpha,\\beta)$"@ -- the structure
    constant of a Chevalley basis:
    @TEX "$[E_\\alpha, E_\\beta] = N(\\alpha,\\beta) E_{\\alpha+\\beta}$"@.
    Roots are given as @TT "List"@s of integers in the simple-root
    basis (so positive roots have non-negative entries, negative roots
    have non-positive entries).
  Text
    The construction follows GAP's @TT "SimpleLieAlgebraTypeA_G"@ in
    @TT "lib/algliess.gi"@.  For simply-laced types (A, D, E), a 2-cocycle
    @TEX "$\\varepsilon : Q \\times Q \\to \\{\\pm 1\\}$"@ on the root
    lattice is built from the matrix returned by
    @TO chevalleyEpsilonMatrix@; the structure constants are
    @TEX "$N(\\alpha,\\beta) = c \\cdot \\varepsilon(\\alpha_0, \\beta_0)$"@
    where @TEX "$\\alpha_0, \\beta_0$"@ are the positive versions of
    @TEX "$\\alpha, \\beta$"@ and @TEX "$c \\in \\{\\pm 1\\}$"@ depends on
    the signs of @TEX "$\\alpha, \\beta, \\alpha+\\beta$"@.
  Text
    For non-simply-laced types (B, C, F, G), the construction uses the
    folding of root systems in characteristic zero: each non-ADE type is
    related to the @TEX "$\\sigma$"@-fixed Lie subalgebra of an ambient
    ADE algebra (B_n in
    D_{n+1}, C_n in A_{2n-1}, F_4 in E_6, G_2 in D_4).  The structure
    constants are
    @TEX "$N(\\alpha,\\beta) = \\varepsilon(r, r') \\cdot (q + 1)$"@,
    where @TEX "$r, r'$"@ are ambient representatives in the
    @TEX "$\\sigma$"@-orbits of @TEX "$\\alpha, \\beta$"@ with
    @TEX "$r + r'$"@ an ambient root, @TEX "$\\varepsilon$"@ is a
    @TEX "$\\sigma$"@-invariant ambient cocycle, and @TEX "$q$"@ is the
    chain depth in non-ADE roots.  This folding chooses integral Chevalley
    constants before reduction; it is not an assertion that fixed points
    commute with reduction in bad characteristic.  Magnitudes can exceed 1 in non-ADE
    (e.g., G_2 has structure constants of absolute value up to 3).
  Text
    The folding construction requires the canonical node ordering used by
    @TT "cartanB n"@, @TT "cartanC n"@, @TT "cartanF4()"@, and
    @TT "cartanG2()"@.  A simultaneously permuted non-simply-laced Cartan
    matrix describes the same root system, but this routine does not
    currently allow a different node ordering in the folding construction.
  Example
    A = cartanA 3                  -- type A_2 (= sl_3)
    N = chevalleyConstants A
    -- Bracket of e_{alpha_1} with e_{alpha_2}:
    N#({1,0}, {0,1})
    -- Antisymmetry:
    N#({0,1}, {1,0})
    -- Bracket of f_{alpha_1} with f_{alpha_2} = -N(alpha_1, alpha_2) f_{alpha_1+alpha_2}:
    N#({-1,0}, {0,-1})
  Text
    The values @TEX "$-1,1,1$"@ mean @TEX "$[e_{\\alpha_1},e_{\\alpha_2}]=-e_{\\alpha_1+\\alpha_2}$"@, the reversed bracket has the opposite sign, and @TEX "$[f_{\\alpha_1},f_{\\alpha_2}]=f_{\\alpha_1+\\alpha_2}$"@.  The signs reflect the package's chosen Chevalley basis.
  Text
    @TO chevalleyN@ gives
    the structure constant for a single pair and returns @TT "0"@ when
    @TEX "$\\alpha + \\beta$"@ is not a root.  @TO verifyJacobi@ checks
    the root-vector part of the Jacobi identity.
  Example
    verifyJacobi cartanA 4         -- type A_3
    verifyJacobi cartanD 4
  Text
    Both values are @TT "true"@: the root-vector Jacobi checks vanish for the type-@TEX "$A_3$"@ and type-@TEX "$D_4$"@ structure constants.  The scope of this check is described on @TO verifyJacobi@.
SeeAlso
  chevalleyN
  chevalleyEpsilonMatrix
  verifyJacobi
  isSimplyLaced
///

doc ///
Key
  chevalleyN
  (chevalleyN, Matrix, List, List)
Headline
  The Chevalley structure constant for a pair of roots
Usage
  n = chevalleyN(A, alpha, beta)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  alpha:List
    a root in simple-root coordinates
  beta:List
    a root in simple-root coordinates
Outputs
  n:ZZ
    the integer @TEX "$N(\\alpha,\\beta)$"@, or @TT "0"@ if
    @TT "alpha+beta"@ is not a (nonzero) root
Description
  Text
    Returns the entry for @TT "(alpha, beta)"@ in the table computed by
    @TO chevalleyConstants@.  It returns @TT "0"@ when the pair
    @TT "(alpha, beta)"@ is not in the table -- typically because
    @TT "alpha+beta"@ is zero or not a root.
  Example
    A = cartanA 3                     -- type A_2
    chevalleyN(A, {1,0}, {0,1})
    chevalleyN(A, {0,1}, {1,0})
    chevalleyN(A, {1,0}, {1,0})       -- alpha+beta = 2 alpha_1, not a root
    chevalleyN(A, {1,0}, {-1,0})      -- alpha+beta = 0
  Text
    The first two values, @TEX "$-1$"@ and @TEX "$1$"@, are opposite structure constants for the two orders of the simple roots.  The third is zero because @TEX "$2\\alpha_1$"@ is not a root.  The fourth is also zero in this function: the bracket of opposite root vectors lies in the Cartan subalgebra and is not encoded as a root-vector coefficient.
SeeAlso
  chevalleyConstants
///

doc ///
Key
  verifyJacobi
  (verifyJacobi, Matrix)
Headline
  Verify the Jacobi identity for the Chevalley structure constants
Usage
  verifyJacobi A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  :Boolean
    true if the tested root-vector Jacobi relations hold, false otherwise
Description
  Text
    Iterates over every unordered triple
    @TEX "$\\{\\alpha, \\beta, \\gamma\\}$"@ of roots with
    @TEX "$\\alpha + \\beta + \\gamma \\ne 0$"@ and tests
  Text
    @TEX "$N(\\alpha,\\beta)\\,N(\\alpha+\\beta,\\gamma) +
           N(\\beta,\\gamma)\\,N(\\beta+\\gamma,\\alpha) +
           N(\\gamma,\\alpha)\\,N(\\gamma+\\alpha,\\beta) = 0$"@.
  Text
    This tests the Jacobi identity in the linear span of root vectors.
    Triples with @TEX "$\\alpha+\\beta+\\gamma = 0$"@ are skipped, as are
    triples for which one of the three pairwise sums is zero: in either case
    an intermediate bracket is a Cartan element rather than a root vector.
    Thus this routine is a consistency check for the root-vector structure
    constants, not a verification of every Chevalley-basis Jacobi relation.
    Failures, if any, are printed.
  Example
    verifyJacobi cartanA 3            -- type A_2
    verifyJacobi cartanA 4            -- type A_3
    verifyJacobi cartanD 4
    verifyJacobi cartanE 6
  Text
    Each result is @TT "true"@.  For these four root systems the routine finds no failure in the root-vector Jacobi identities included in its check; this is not an additional assertion about brackets with Cartan basis elements.
SeeAlso
  chevalleyConstants
///

----------------------------------------------------------------------
-- Layer 4 documentation
----------------------------------------------------------------------

doc ///
Key
  WeylModuleZ
Headline
  Type of an integral Weyl module V_Z(lambda)
Description
  Text
    @TT "WeylModuleZ"@ is the Macaulay2 type representing the Weyl module
    @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ -- the integral form of a Weyl
    module over @TEX "$\\mathbb{Z}$"@ for a simple finite-type Lie
    algebra @TEX "$\\mathfrak{g}$"@.  Its reduction modulo a prime
    @TEX "$p$"@ is the Weyl module over @TEX "$\\mathbb{F}_p$"@.
  Text
    Let @TEX "$U_{\\mathbb{Z}}(\\mathfrak{g})$"@
    be Kostant's @TEX "$\\mathbb{Z}$"@-form of the universal enveloping
    algebra (generated by the divided powers @TEX "$E_\\alpha^{(k)}, F_\\alpha^{(k)}, \\binom{H_i}{k}$"@).
    The Weyl module @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ is the
    @TEX "$U_{\\mathbb{Z}}(\\mathfrak{g})$"@-module generated by a single
    highest-weight vector @TEX "$v_\\lambda$"@ of weight @TEX "$\\lambda$"@,
    annihilated by @TEX "$E_\\alpha^{(r)}$"@ for every positive root
    @TEX "$\\alpha$"@ and @TEX "$r>0$"@, with
    @TEX "$\\binom{H_i}{k} v_\\lambda = \\binom{\\langle\\lambda, \\alpha_i^\\vee\\rangle}{k} v_\\lambda$"@
    and @TEX "$F_i^{(\\lambda_i+1)}v_\\lambda=0$"@ for every simple root.
    Equivalently, it is the universal finite-dimensional integrable
    highest-weight module
    @TEX "$V(\\lambda)$"@ for @TEX "$\\mathfrak{g}_{\\mathbb{C}}$"@,
    realised on a chosen integral lattice -- it has the same character
    as the irreducible @TEX "$L(\\lambda)_{\\mathbb{C}}$"@, namely
    Weyl's character formula.  Reduction modulo @TEX "$p$"@ produces the
    @EM "Weyl module mod p"@: typically reducible, with a unique simple
    quotient @TEX "$L(\\lambda)$"@ at the top.
  Text
    @SUBSECTION "Bases and actions"@
  Text
    A @TT "WeylModuleZ"@ specifies
    the Cartan matrix, the highest weight @TEX "$\\lambda$"@ in Dynkin
    coordinates, the positive roots in a fixed height-then-lex ordering,
    Chevalley structure constants @TEX "$N(\\alpha,\\beta)$"@,
    @EM "PBW exponent tuples"@ at each weight, the chosen representatives
    forming an integral basis after the checks described under
    @TO weylModuleZ@, and
    @TEX "$\\mathbb{Z}$"@-valued matrices for the actions of
    @TEX "$f_i$"@ and @TEX "$e_i$"@ between adjacent weight spaces,
    and the scalar action of @TEX "$h_i$"@ on each weight space.
  Text
    @TO weylModuleZ@ constructs the module.  Its dimension, weights,
    character, bases, and actions are computed by @TO (dim, WeylModuleZ)@,
    @TO weights@, @TO weightSpaces@, @TO weightSpace@, @TO basisVecs@,
    @TO character@, @TO actionF@, @TO actionE@, @TO actionH@,
    @TO highestWeight@, @TO ambientCartan@.  For a mod-@TEX "$p$"@
    reduction, see @TO weylModule@ or @TO quotientWeylModule@.
  Example
    -- The 8-dimensional adjoint of sl_3 (Dynkin highest weight (1,1)).
    W = weylModuleZ(cartanA 3, {1, 1})
    dim W
    pairs character W
    -- Rank of the action matrix F_alpha_1 from V_(1,1) to V_(-1,2):
    rank actionF(W, {-1, 2}, 1)
  Text
    The character has six root weights of multiplicity one and the zero weight of multiplicity two, hence total rank eight.  These are the root spaces and Cartan subalgebra of the adjoint representation after extension to characteristic zero.  The last output says that @TEX "$f_1$"@ has rank one from weight @TEX "$(1,1)$"@ to weight @TEX "$(-1,2)$"@.
SeeAlso
  weylModuleZ
  weylModule
  SubWeylModuleFp
  QuotientWeylModuleFp
///

doc ///
Key
  weylModuleZ
  (weylModuleZ, Matrix, List)
  [weylModuleZ, WeightFormat]
  [weylModuleZ, Verbose]
Headline
  Construct the Weyl module V_Z(lambda) over a finite-type Cartan matrix
Usage
  W = weylModuleZ(A, lambda)
  W = weylModuleZ(A, lambda, WeightFormat => "Partition")
  W = weylModuleZ(A, lambda, Verbose => true)
Inputs
  A:Matrix
    a finite-type Cartan matrix (any of A, B, C, D, E, F, G in @TT "KacMoody"@'s convention)
  lambda:List
    dominant integral highest weight, by default in Dynkin (fundamental-weight) coordinates
Outputs
  W:WeylModuleZ
    the integral form @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ together with action matrices
    for @TEX "$f_i, e_i, h_i$"@ on every weight space
Description
  Text
    Constructs the Weyl module @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ for the
    finite-type Lie algebra @TEX "$\\mathfrak{g}(A)$"@.  All seven Cartan
    types (A, B, C, D, E, F, G) are allowed.  The non-simply-laced
    construction uses divided-power commutation formulas for the longer
    root strings.
  Text
    For types B, C, F, and G, pass the Cartan matrix in the canonical node
    ordering returned by the corresponding @TT "KacMoody"@ constructor.
    Simultaneous row-and-column permutations of these matrices are not
    currently allowed in the folding construction.
  Text
    See @TO WeylModules@ for the underlying definitions.
    The lattice @TEX "$V_{\\mathbb{Z}}(\\lambda)=U_{\\mathbb{Z}}(\\mathfrak{g})v_\\lambda$"@
    is generated by a highest-weight vector in
    @TEX "$L(\\lambda)_\\mathbb{C}$"@ under Kostant's
    @TEX "$\\mathbb{Z}$"@-form.  Its character is given by Weyl's
    formula -- the same as the irreducible
    @TEX "$L(\\lambda)_\\mathbb{C}$"@.  Reduction mod @TEX "$p$"@
    produces the @EM "Weyl module mod p"@,
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@, with simple top
    @TEX "$L(\\lambda)$"@.  Its radical is the unique maximal proper
    submodule; it should not be confused with the socle, which is the sum
    of the simple submodules.
  Text
    @SUBSECTION "Construction algorithm"@
  Text
    Step (1): realise the Verma module
    @TEX "$M(\\lambda) := U_{\\mathbb{Z}}(\\mathfrak{n}^-) v_\\lambda$"@
    on its PBW basis (over the height-then-lex ordered positive roots)
    restricted to the weights of @TEX "$V(\\lambda)$"@.  At each weight
    @TEX "$\\mu$"@, the PBW basis size is the Kostant partition number
    @TEX "$K(\\lambda - \\mu)$"@.
  Text
    Step (2): compute the action of @TEX "$f_i$"@ on
    @TEX "$M(\\lambda)$"@ by PBW commutation, using the structure
    constants computed in @TO chevalleyConstants@.  The fundamental
    identity
    @TEX "$[f_\\alpha, f_\\beta^{(k)}] = -N(\\alpha,\\beta) f_\\beta^{(k-1)} f_{\\alpha+\\beta}$"@
    together with the further root-string terms in non-ADE types, gives
    the required commutation formulas.
  Text
    Step (3): compute the Verma kernel
    @TEX "$\\ker(M(\\lambda) \\to V(\\lambda))$"@ by closing the seed
    vectors @TEX "$\\{f_{\\alpha_i}^{(\\lambda_i+1)} v_\\lambda\\}$"@
    under successive @TEX "$f_j$"@ actions in PBW coordinates.  At each
    V-weight, compute the kernel over @TEX "$\\mathbb{Q}$"@ and greedily
    pick PBW representatives that are linearly independent modulo it.
    The constructor accepts this choice only when the resulting quotient
    projections and action matrices have integral coordinates.  Otherwise it
    stops with an error; it does not currently compute a saturated
    replacement by Smith or Hermite normal form.
  Text
    Step (4): solve for the @TEX "$e_i$"@ action using the incoming
    @TEX "$f_j$"@ maps at each weight.  It is determined by the recurrence
    @TEX "$e_i (f_j w) = f_j (e_i w) + \\delta_{ij} h_i w$"@ together
    with @TEX "$e_i v_\\lambda = 0$"@.
  Text
    The weights, character, maximal vectors, simple head, and socle series
    are computed by @TO weights@, @TO character@, @TO maximalVectors@, @TO simpleQuotient@,
    and @TO socleSeries@, respectively.
  Text
    Explicit PBW spaces can grow quickly with rank and highest weight.
    @TT "Verbose"@ defaults to @TT "false"@;
    @TT "Verbose => true"@ prints construction-stage timings.
  Text
    @SUBSECTION "Example: the standard representation of sl_3"@
  Text
    The Weyl module
    @TEX "$V(\\omega_1) = V$"@ of @TEX "$\\mathrm{sl}_3$"@ is the
    3-dimensional standard representation, with weights
    @TEX "$\\varepsilon_1, \\varepsilon_2, \\varepsilon_3$"@ (in the
    epsilon basis), each appearing with multiplicity one.  This module
    is irreducible over any field, so reduction mod @TEX "$p$"@ gives
    the simple module @TEX "$L(\\omega_1)$"@ for every prime.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {1, 0})
    dim W                                          -- 3
    weights W                                       -- 3 weights, multiplicity 1 each
    pairs character W
    -- f_1 : V_(1,0) -> V_(-1,1), the lowering by alpha_1 = e_1 - e_2:
    actionF(W, {-1, 1}, 1)
  Text
    The character assigns multiplicity one to the three weights @TEX "$(1,0),(-1,1),(0,-1)$"@.  The matrix @TEX "$(1)$"@ means that @TEX "$f_1$"@ takes the chosen highest-weight basis vector to the chosen vector of weight @TEX "$(-1,1)$"@.  Together these describe the integral natural representation.
  Text
    @SUBSECTION "Example: D^2 of the standard representation at p = 2"@
  Text
    In
    type @TEX "$A_{n-1}$"@, the Weyl module
    @TEX "$V(2 \\omega_1)$"@ is the divided square
    @TEX "$D^2 V = \\Gamma^2(V)$"@.  For a free module it may be realized
    as the invariant submodule @TEX "$(V \\otimes V)^{S_2}$"@, or
    equivalently as the dual of @TEX "$\\mathrm{Sym}^2(V^*)$"@.  (The
    tensor quotient by @TEX "$v\\otimes w-w\\otimes v$"@ is instead
    @TEX "$\\mathrm{Sym}^2 V$"@.)  When 2 is invertible,
    @TEX "$\\Gamma^2V$"@ is naturally isomorphic to
    @TEX "$\\mathrm{Sym}^2V$"@; in characteristic 2 they need not be
    isomorphic.  At @TEX "$p = 2$"@ for
    @TEX "$\\mathrm{sl}_3$"@ one has the nonsplit short exact
    sequence
  Text
    @TEX "$0 \\to \\Lambda^2 V \\to D^2 V \\to F^* V \\to 0$"@,
  Text
    where @TEX "$F^* V$"@ is the Frobenius twist (= the simple top
    @TEX "$L(2 \\omega_1)$"@) and @TEX "$\\Lambda^2 V \\cong V^*$"@ is
    the dual standard representation, which is the simple socle
    @TEX "$L(\\omega_2)$"@.
    The following computes the two composition factors, the head, and the
    socle.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})           -- V(2 omega_1) = D^2 of std, dim 6
    dim W
    -- Decomposition factors:
    pairs decompositionNumbers(W, 2)     -- {(2,0) : 1 (= F^* V), (0,1) : 1 (= Lambda^2 V)}
    -- Simple top L(2,0) = F^* V, dim 3:
    dim simpleQuotient(W, 2)
    -- Simple socle L(0,1) = Lambda^2 V, dim 3:
    dim socleWeyl(W, 2)
  Text
    The dimension is 6 and the two decomposition entries are both 1.  The simple head @TEX "$L(2,0)$"@ and simple socle @TEX "$L(0,1)$"@ each have dimension 3; they are the two ends of the nonsplit exact sequence above, not summands of @TEX "$D^2V$"@.
  Text
    @SUBSECTION "Partition input"@
  Text
    For classical types, epsilon
    coordinates are often more natural than Dynkin labels.  Pass
    @TT "WeightFormat => \"Partition\""@:
  Example
    -- The divided-cube Weyl module for sl_4: partition (3, 0, 0, 0).
    A = cartanA 4
    W = weylModuleZ(A, {3, 0, 0, 0}, WeightFormat => "Partition")
    dim W                                          -- 20 = dim D^3 of C^4
    -- Spin representation of so(5): half-integral epsilon coordinates (1/2, 1/2).
    Wspin = weylModuleZ(cartanB 2, {1/2, 1/2}, WeightFormat => "Partition")
    dim Wspin                                      -- 4
  Text
    The ranks 20 and 4 identify the divided-cube Weyl lattice for @TEX "$\\mathrm{SL}_4$"@ and the spin Weyl lattice for @TEX "$\\mathrm{Spin}_5$"@.  The tuple @TEX "$(3,0,0,0)$"@ has Dynkin label @TEX "$(3,0,0)$"@, while @TEX "$(1/2,1/2)$"@ has type-@TEX "$B_2$"@ label @TEX "$(0,1)$"@.
  Text
    @SUBSECTION "Non-ADE: G_2 and F_4"@
  Text
    The divided-power commutation formulas include the terms arising
    from root strings of length 1 in ADE, up to 2 in B/C/F, and up to 3
    in G_2.
  Example
    -- The 14-dimensional adjoint of G_2 = V(omega_2):
    G = cartanG2()
    Wadj = weylModuleZ(G, {0, 1})
    dim Wadj
  Text
    The output is 14, the rank of the integral adjoint Weyl module of type @TEX "$G_2$"@.  Its highest weight is the second fundamental weight, @TEX "$\\omega_2$"@.
SeeAlso
  WeylModuleZ
  actionF
  actionE
  basisVecs
  character
  weights
  WeightFormat
  weylModule
  "Tutorial: Weyl modules in classical types"
///

doc ///
Key
  weylModule
  (weylModule, ZZ, List)
  (weylModule, WeylModuleZ, ZZ)
  (weylModule, Matrix, ZZ, List)
  [weylModule, LieType]
  [weylModule, WeightFormat]
Headline
  Weyl module V(lambda) mod p, with type selected by LieType and inferred rank
Usage
  weylModule(p, lambda)
  weylModule(p, lambda, LieType => "B")
  weylModule(p, lambda, WeightFormat => "Dynkin")
Inputs
  p:ZZ
    a prime
  lambda:List
    the highest weight; by default this is in classical epsilon coordinates
    for types A/B/C/D (the option is called @TT "Partition"@, although
    type-B spin weights can be half-integral, and signed or half-integral
    type-D weights need not be ordinary partitions)
    and in Dynkin coordinates for exceptional types
Outputs
  :QuotientWeylModuleFp
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@, the Weyl module over the
    prime field
Description
  Text
    The output is the Weyl module
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@ -- a finite-length
    representation of the simply connected simple algebraic group
    @TEX "$G$"@, expressed through the action of its hyperalgebra over
    @TEX "$\\mathbb{F}_p$"@, of the same dimension as the irreducible
    @TEX "$L(\\lambda)_\\mathbb{C}$"@ in characteristic zero.  In
    classical types, @TEX "$\\lambda$"@ is given in epsilon coordinates
    (ordinary partitions in type A and for many tensor representations,
    but possibly half-integral in B/D, with a possibly negative final
    coordinate in type D); for
    exceptional types use Dynkin coordinates.  See @TO WeylModules@ for the
    full mathematical definition of @TEX "$V(\\lambda)$"@ via
    Kostant's @TEX "$\\mathbb{Z}$"@-form.
  Text
    The constructor selects the Cartan matrix from @TT "LieType"@.  With the
    default partition input, a type-A tuple of length @TT "n"@ selects
    @TEX "$A_{n-1}$"@ (rank @TT "n-1"@), whereas types B, C, and D have
    rank @TT "#lambda"@.  Passing
    @TT "WeightFormat => \"Dynkin\""@ makes a type-A tuple of length
    @TT "n"@ select rank @TT "n"@ instead.  Accepted @TT "LieType"@
    values: @TT "\"A\""@, @TT "\"B\""@, @TT "\"C\""@, @TT "\"D\""@
    (partition length 2 or more for A, length 2 or more for B/C,
    and length 4 or more for D), and the exceptional @TT "\"E_6\""@,
    @TT "\"E_7\""@, @TT "\"E_8\""@, @TT "\"F_4\""@, @TT "\"G_2\""@
    (rank fixed by the type; @TT "#lambda"@ must equal that rank).
    The exceptional types take Dynkin coordinates only.
  Text
    @SUBSECTION "Type A example: V(2,1,0) for sl_3"@
  Text
    The Weyl module @TEX "$V(2,1,0) = V(\\omega_1 + \\omega_2)$"@ in
    type @TEX "$A_2$"@ is the @TEX "$8$"@-dimensional adjoint
    representation @TEX "$\\mathfrak{sl}_3$"@.  It is irreducible in
    characteristic 2 in this example:
  Example
    W = weylModule(2, {2, 1, 0})       -- partition (2,1,0) = adjoint
    dim W                              -- 8
    highestWeight W                    -- Dynkin: {1, 1}
    highestWeight(W, WeightFormat => "Partition")
  Text
    The dimension is 8 and the highest-weight outputs @TEX "$(1,1)$"@ and @TEX "$(2,1,0)$"@ describe the same type-@TEX "$A_2$"@ weight.  The returned quotient has zero defining kernel: it represents the entire Weyl module over @TEX "$\\mathbb F_2$"@.
  Text
    @SUBSECTION "Type A example: V(3,0,0,0) for sl_4"@
  Text
    The divided cube @TEX "$D^3 V=V(3\\omega_1)$"@ of the standard
    4-dimensional representation has dimension 20.  It is irreducible
    away from characteristic 2 and 3:
  Example
    W = weylModule(2, {3, 0, 0, 0})
    dim W                              -- 20
  Text
    The value 20 is the dimension of @TEX "$D^3(\\mathbb F_2^4)$"@.  Reduction preserves the rank of the integral lattice, even though it can introduce proper submodules.
  Text
    @SUBSECTION "Type B / C / D: epsilon-coordinate input"@
  Text
    Type @TEX "$B_n$"@ admits half-integer epsilon coordinates for the spin
    representation; @TEX "$D_n$"@ has two half-spin representations distinguished by
    the sign of the last epsilon coordinate.  Type @TEX "$C_n$"@ requires
    integral epsilon coordinates.
  Example
    -- B_2 standard representation, partition (1, 0), dimension 5.
    Wb = weylModule(2, {1, 0}, LieType => "B")
    dim Wb
    -- B_2 spin representation, epsilon coordinates (1/2, 1/2), dimension 4.
    Wspin = weylModule(2, {1/2, 1/2}, LieType => "B")
    dim Wspin
    -- D_4 half-spin representation, dimension 8.
    Whalfspin = weylModule(2, {1/2, 1/2, 1/2, 1/2}, LieType => "D")
    dim Whalfspin
  Text
    The dimensions @TEX "$5,4,8$"@ identify the natural type-@TEX "$B_2$"@ Weyl module, its spin module, and a type-@TEX "$D_4$"@ half-spin module over @TEX "$\\mathbb F_2$"@.  These are dimensions of whole Weyl modules; dimension alone does not determine their composition factors.
  Text
    @SUBSECTION "Exceptional types: Dynkin input only"@
  Example
    -- G_2 adjoint = V(omega_2) in Dynkin.  Dim 14.
    Wg = weylModule(2, {0, 1}, LieType => "G_2")
    dim Wg
  Text
    The output gives the fourteen-dimensional type-@TEX "$G_2$"@ Weyl module of highest weight @TEX "$\\omega_2$"@ over @TEX "$\\mathbb F_2$"@.  The construction uses Dynkin labels because no partition-coordinate convention is supplied for exceptional types.
  Text
    The output is a @TO QuotientWeylModuleFp@ with empty defining
    kernel: it is the Weyl module realised as the quotient
    @TEX "$V(\\lambda) / 0$"@ over @TEX "$\\mathbb{F}_p$"@.  Functions
    such as @TO character@, @TO weights@, and @TO maximalVectors@ apply to
    this quotient.  Functions whose documented input is @TO WeylModuleZ@,
    including @TO simpleQuotient@, are called on the integral module returned
    by @TO weylModuleZ@.  @TO decompositionNumbers@ also accepts a quotient
    or subquotient directly.
SeeAlso
  weylModuleZ
  simpleQuotient
  WeightFormat
  "Tutorial: Weyl modules in classical types"
///

doc ///
Key
  LieType
Headline
  Choose the Dynkin family for a highest-weight constructor
Description
  Text
    This option specifies the family of the simply connected group when
    a constructor is given only a characteristic and highest weight.
    The classical values are @TT "\"A\""@, @TT "\"B\""@,
    @TT "\"C\""@, and @TT "\"D\""@.  The exceptional values
    @TT "\"E_6\""@, @TT "\"E_7\""@, @TT "\"E_8\""@,
    @TT "\"F_4\""@, and @TT "\"G_2\""@ fix the rank.
    Classical constructors infer the rank from the number of weight
    coordinates; exceptional constructors use Dynkin coordinates.
    The default family is type A.  See @TO WeightFormat@ for the
    coordinate and rank conventions.
  Example
    M = weylModule(2, {1/2, 1/2}, LieType => "B")
    dim M
    highestWeight M
  Text
    The outputs are 4 and @TT "{0,1}"@.  The epsilon-coordinate weight
    @TEX "$(1/2,1/2)$"@ is the second fundamental weight of type
    @TEX "$B_2$"@, so this is the four-dimensional spin Weyl module
    over @TEX "$\\mathbb F_2$"@.
SeeAlso
  weylModule
  simpleQuotient
  WeightFormat
///

doc ///
Key
  highestWeight
  (highestWeight, WeylModuleZ)
  (highestWeight, SubWeylModuleFp)
  (highestWeight, QuotientWeylModuleFp)
  (highestWeight, SubquotientWeylModuleFp)
  [highestWeight, WeightFormat]
Headline
  The highest weight of a Weyl module (or sub/quotient)
Usage
  highestWeight W
  highestWeight(W, WeightFormat => "Partition")
Inputs
  W:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@,
    @TO QuotientWeylModuleFp@, or @TO SubquotientWeylModuleFp@
Outputs
  :List
    the unique dominance-maximal support weight in the chosen format, or
    @TT "null"@ for the zero submodule or quotient
Description
  Text
    Returns the highest weight @TEX "$\\lambda$"@ of @TEX "$V(\\lambda)$"@.
    By default it is reported in Dynkin (fundamental-weight) coordinates
    @TEX "$(m_1, \\ldots, m_n)$"@, where
    @TEX "$m_i = \\langle\\lambda, \\alpha_i^\\vee\\rangle = \\text{coefficient of } \\omega_i$"@.
    Pass @TT "WeightFormat => \"Partition\""@ to recover the
    @TEX "$\\varepsilon$"@-basis form (a partition for type A, with last
    entry 0 by convention; half-integral values are allowed in B/D spin
    weights, and the final D coordinate may be negative).
    For a submodule, quotient, or subquotient, the method
    computes the dominance-maximal weight present in that module.
    It returns @TT "null"@ when the module is zero and raises an error if
    there is more than one incomparable maximal weight; use
    @TO dominantWeights@ when all dominant support weights are wanted.
  Example
    -- A_2: ω_1 + 2 ω_2 in Dynkin = partition (3, 2, 0).
    W = weylModuleZ(cartanA 3, {1, 2})
    highestWeight W
    highestWeight(W, WeightFormat => "Partition")
  Text
    The outputs @TEX "$(1,2)$"@ and @TEX "$(3,2,0)$"@ are two coordinate expressions for @TEX "$\\omega_1+2\\omega_2$"@.  The consecutive differences of the partition recover the Dynkin labels.
  Example
    -- B_2 spin: highest weight ω_2 in Dynkin = (1/2, 1/2) in epsilon basis.
    W = weylModuleZ(cartanB 2, {0, 1})
    highestWeight W
    highestWeight(W, WeightFormat => "Partition")
  Text
    The outputs @TEX "$(0,1)$"@ and @TEX "$(1/2,1/2)$"@ express the type-@TEX "$B_2$"@ spin highest weight in Dynkin and epsilon coordinates, respectively.
  Example
    -- A quotient reports the highest weight actually present in the quotient.
    W = weylModuleZ(cartanA 3, {2, 0})
    Q = simpleQuotient(W, 2)
    highestWeight Q
  Text
    The output @TEX "$(2,0)$"@ is the highest weight of the simple head @TEX "$L(2\\omega_1)$"@.  Quotienting by the radical removes lower weights but preserves the highest-weight line.
SeeAlso
  weights
  character
  WeightFormat
  ambientCartan
///

doc ///
Key
  ambientCartan
  (ambientCartan, WeylModuleZ)
  (ambientCartan, SubWeylModuleFp)
  (ambientCartan, QuotientWeylModuleFp)
  (ambientCartan, SubquotientWeylModuleFp)
  (ambientCartan, WeylCharacter)
Headline
  The Cartan matrix of the underlying Lie algebra
Usage
  ambientCartan M
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@, or @TO QuotientWeylModuleFp@
Outputs
  :Matrix
    the Cartan matrix of the simple Lie algebra @TEX "$\\mathfrak{g}$"@
    that @TT "M"@ is a representation of
Description
  Text
    Returns the Cartan matrix associated to @TT "M"@.  In the convention
    of @TO KacMoody@, column @TT "j"@
    of the Cartan matrix gives the Dynkin coordinates of the simple root
    @TEX "$\\alpha_j$"@.
  Example
    W = weylModuleZ(cartanA 3, {1, 0})
    A = ambientCartan W                            -- 2x2 sl_3 Cartan
    -- Same Cartan recovered from a sub or quotient:
    soc = socleWeyl(weylModuleZ(cartanA 3, {2, 0}), 2)
    ambientCartan soc
  Text
    Both outputs are the type-@TEX "$A_2$"@ matrix @TEX "$\\left(\\begin{smallmatrix}2&-1\\\\-1&2\\end{smallmatrix}\\right)$"@.  The socle is a representation of the same group as its ambient Weyl module, so passing to the submodule changes the character but not the root datum.
SeeAlso
  highestWeight
  ambientWeylModule
///

doc ///
Key
  weights
  (weights, WeylModuleZ)
  (weights, SubWeylModuleFp)
  (weights, QuotientWeylModuleFp)
  (weights, SubquotientWeylModuleFp)
  [weights, WeightFormat]
  [weights, WeightOrder]
  (weights, WeylCharacter)
Headline
  Weights in the support of a module or formal character
Usage
  weights M
  weights(M, WeightFormat => "Partition")
  weights(M, WeightOrder => "Lex")
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@,
    @TO QuotientWeylModuleFp@, @TO SubquotientWeylModuleFp@, or @TO WeylCharacter@
Outputs
  wts:List
    the distinct support weights, without multiplicities
Description
  Text
    Returns the distinct weights @TEX "$\\mu$"@ at which the module
    @TT "M"@ has non-zero weight space.  @TT "M"@ may be a
    @TO WeylModuleZ@, a @TO SubWeylModuleFp@, or a
    @TO QuotientWeylModuleFp@.  Each weight occurs once in this list;
    @TO character@ gives its multiplicity.
  Text
    @SUBSECTION "Coordinate convention"@
  Text
    By default each weight is a tuple
    @TEX "$(m_1, \\ldots, m_n)$"@ in Dynkin (fundamental-weight)
    coordinates -- the rank @TT "n"@ equals the number of simple roots,
    with @TEX "$m_i = \\langle\\mu, \\alpha_i^\\vee\\rangle$"@.  In type
    @TEX "$A_{n-1}$"@ this tuple has one fewer entry than the corresponding
    @TEX "$\\varepsilon$"@-basis representative.
    For example, the standard 3-dim representation of
    @TEX "$\\mathfrak{sl}_3$"@ has weights @TEX "$\\varepsilon_1, \\varepsilon_2, \\varepsilon_3$"@,
    which appear in Dynkin form as
    @TEX "$(1,0)$"@, @TEX "$(-1,1)$"@, @TEX "$(0,-1)$"@ -- exactly the
    expansions @TEX "$\\varepsilon_1 = \\omega_1$"@,
    @TEX "$\\varepsilon_2 = -\\omega_1 + \\omega_2$"@,
    @TEX "$\\varepsilon_3 = -\\omega_2$"@.
  Example
    -- Standard representation of sl_3.  Three weights, all multiplicity 1.
    W = weylModuleZ(cartanA 3, {1, 0})
    weights W                                          -- Dynkin
    weights(W, WeightFormat => "Partition")            -- epsilon-basis (last coord = 0)
  Text
    Each list has three entries because the natural module has three distinct weight spaces.  The Dynkin and partition lists express the same weights, each of multiplicity one.  The partition convention chooses an epsilon representative with final coordinate zero.
  Text
    @SUBSECTION "Further examples"@
  Text
    The 8-dimensional adjoint
    representation @TEX "$\\mathfrak{sl}_3 \\cong V(1,1)$"@ has 7
    distinct weights: the 6 roots (each with multiplicity 1) and the
    zero weight (multiplicity 2 -- the Cartan).  @TT "weights"@ returns
    the 7 distinct labels:
  Example
    Wadj = weylModuleZ(cartanA 3, {1, 1})
    dim Wadj                                            -- 8
    weights Wadj
    -- The character records the multiplicity-2 zero weight:
    pairs character Wadj
  Text
    There are seven distinct weights but dimension eight: the zero-weight entry of the character has multiplicity two.  The other six entries are the roots of @TEX "$\\mathfrak{sl}_3$"@, each of multiplicity one.
  Text
    @SUBSECTION "Type B with a nontrivial multiplicity"@
  Text
    The 14-dimensional
    @TEX "$\\mathfrak{so}(5)$"@-module @TEX "$V(2 \\omega_1)$"@ has the
    zero weight with multiplicity 2.  In partition form, the highest
    weight is @TEX "$(2, 0)$"@.
  Example
    Wb = weylModuleZ(cartanB 2, {2, 0})
    dim Wb                                              -- 14
    pairs character(Wb, WeightFormat => "Partition")
  Text
    The displayed multiplicities sum to 14.  In particular @TEX "$(0,0)$"@ occurs with multiplicity two in the type-@TEX "$B_2$"@ epsilon-coordinate character.
  Text
    @SUBSECTION "Submodules and quotients"@
  Text
    The same option applies to submodules and quotients, including the
    head and socle of @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    soc = socleWeyl(W, 2)                               -- L(0,1) inside V(2,0) mod 2
    weights soc                                         -- Dynkin: 3 weights of L(0,1)
    weights(soc, WeightFormat => "Partition")
  Text
    The three weights belong to the simple socle @TEX "$L(\\omega_2)=\\Lambda^2\\mathbb F_2^3$"@.  They are @TEX "$(0,1),(1,-1),(-1,0)$"@ in Dynkin coordinates; the second list gives equivalent epsilon-coordinate representatives.
  Text
    For a @TO WeylCharacter@, return the support of the formal character.
    A weight with a negative coefficient in a virtual character is still in
    its support; a weight with coefficient zero is omitted.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    T = weylCharacter weylModuleZ(cartanA 2, {0})
    weights(C - 3*T)
  Text
    The three weights are 1, 0, and -1. The weights 1 and -1 come from the
    natural module, while weight zero has coefficient -3 from subtracting
    three trivial characters. This support describes a virtual character,
    not the weight spaces of an actual module.
SeeAlso
  character
  weightSpace
  weightSpaces
  WeightFormat
  weightAsPartition
  weightAsDynkin
///

doc ///
Key
  character
  (character, WeylModuleZ)
  (character, SubWeylModuleFp)
  (character, QuotientWeylModuleFp)
  (character, SubquotientWeylModuleFp)
  [character, WeightFormat]
  (character, WeylCharacter)
Headline
  Weight coefficients of a module or formal character
Usage
  character M
  character(M, WeightFormat => "Partition")
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@,
    @TO QuotientWeylModuleFp@, @TO SubquotientWeylModuleFp@, or @TO WeylCharacter@
Outputs
  ch:HashTable
    support weights mapped to their weight-space ranks over
    @TEX "$\\mathbb{Z}$"@ or dimensions over @TEX "$\\mathbb{F}_p$"@
Description
  Text
    For each weight @TEX "$\\mu$"@ of the module @TT "M"@ (a
    @TO WeylModuleZ@, @TO SubWeylModuleFp@, or @TO QuotientWeylModuleFp@),
    returns the formal character @TEX "$\\mathrm{ch}(M)(\\mu) = \\dim M_\\mu$"@
    as a @TT "HashTable"@ keyed by weight.
  Text
    @SUBSECTION "Coordinate convention"@
  Text
    As for @TO weights@, keys
    are by default in Dynkin (fundamental-weight) coordinates: a tuple
    @TEX "$(m_1, \\ldots, m_n)$"@ of length @TT "n"@ = rank, with
    @TEX "$m_i = \\langle\\mu, \\alpha_i^\\vee\\rangle$"@.  Pass
    @TT "WeightFormat => \"Partition\""@ for keys in the
    @TEX "$\\varepsilon$"@-basis (a tuple of length @TT "n+1"@ for
    type A with last coordinate set to @TEX "$0$"@; half-integers appear
    for spin weights in types B/D).  See @TO WeightFormat@ for
    the conversion formulas.
  Example
    -- sl_3 V(2 omega_1) = Gamma^2 of the standard, dim 6.  Six distinct weights,
    -- all multiplicity 1.
    W = weylModuleZ(cartanA 3, {2, 0})
    pairs character W
    pairs character(W, WeightFormat => "Partition")
  Text
    Each pair @TEX "$(\\mu,1)$"@ contributes the term @TEX "$e^\\mu$"@ to the formal character of @TEX "$D^2(\\mathbb Z^3)$"@.  There are six terms, giving rank six.  The two outputs differ only in their coordinate expressions for @TEX "$\\mu$"@.
  Example
    -- sl_3 adjoint = V(1,1), dim 8.  The zero weight has multiplicity 2.
    Wadj = weylModuleZ(cartanA 3, {1, 1})
    dim Wadj
    pairs character Wadj
    -- Note (0,0) -> 2 in the output.
  Text
    The entry @TEX "$((0,0),2)$"@ means that the zero-weight space has rank two.  The six root weights have rank one, so the multiplicities sum to eight.
  Example
    -- Over characteristic zero, type B_2 V(2 omega_1) is the traceless
    -- symmetric-square representation of so(5).  This constructs its
    -- 14-dimensional integral Weyl form; the zero weight has multiplicity 2.
    Wb = weylModuleZ(cartanB 2, {2, 0})
    pairs character(Wb, WeightFormat => "Partition")
  Text
    The zero-weight multiplicity is two and the total multiplicity is 14.  This is the formal character of the integral type-@TEX "$B_2$"@ Weyl module of highest weight @TEX "$2\\omega_1$"@; its reduction has this same formal character in every characteristic.
  Example
    -- Character of a quotient in the same coordinates.
    W = weylModuleZ(cartanA 3, {2, 0})
    Q = simpleQuotient(W, 2)                            -- L(2,0) mod 2, dim 3
    pairs character Q
    pairs character(Q, WeightFormat => "Partition")
  Text
    The head @TEX "$L(2\\omega_1)$"@ has the three weights @TEX "$2\\varepsilon_1,2\\varepsilon_2,2\\varepsilon_3$"@, each of multiplicity one.  It is the Frobenius twist of the natural module in characteristic 2.  The tables express these three terms in Dynkin and canonical partition coordinates.
  Text
    The character is the natural input to @TO decomposeCharacter@,
    @TO differenceCharacter@, @TO productCharacter@, and the family
    of decomposition-matrix routines.
  Text
    For a @TO WeylCharacter@, return its coefficients as a weight-indexed
    table. A virtual character may have negative coefficients. These are
    signed formal coefficients, not dimensions of weight spaces.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    T = weylCharacter weylModuleZ(cartanA 2, {0})
    character(C - 3*T)
  Text
    The entries at weights 1 and -1 are 1, and the entry at weight zero is
    -3. The formal sum is @TEX "$e^{\\omega_1}+e^{-\\omega_1}-3e^0$"@,
    the natural character minus three trivial characters.
SeeAlso
  weights
  decomposeCharacter
  decompositionNumbers
  characterDim
  productCharacter
  differenceCharacter
  WeightFormat
///

doc ///
Key
  weightSpace
  (weightSpace, WeylModuleZ, List)
  (weightSpace, SubWeylModuleFp, List)
  (weightSpace, QuotientWeylModuleFp, List)
  (weightSpace, SubquotientWeylModuleFp, List)
  [weightSpace, WeightFormat]
Headline
  A basis of a given weight space
Usage
  data = weightSpace(M, mu)
  data = weightSpace(M, mu, WeightFormat => "Partition")
Inputs
  M:
    an integral module, modular submodule, quotient, or subquotient
  mu:List
    the requested weight in the chosen coordinate format
Outputs
  data:
    for an integral module, a list of chosen PBW exponent tuples; for a
    submodule, a basis matrix over @TEX "$\\mathbb{F}_p$"@ in ambient
    coordinates; for a quotient or subquotient, the identity matrix in its
    chosen weight-space basis
Description
  Text
    For a @TO WeylModuleZ@, this returns the list of PBW exponent tuples
    chosen as the @TEX "$\\mathbb{Z}$"@-basis of its
    @TEX "$\\mu$"@-weight space.  Each tuple @TT "(c_1,...,c_N)"@
    represents the basis
    vector @TEX "$f_{\\beta_1}^{(c_1)} \\cdots f_{\\beta_N}^{(c_N)} v_\\lambda$"@,
    with @TEX "$\\beta_1, \\ldots, \\beta_N$"@ the positive roots in the
    height-then-lex ordering.  The weight
    @TT "mu"@ may be supplied in either Dynkin (default) or partition
    form via @TT "WeightFormat"@.  For a @TO SubWeylModuleFp@, the
    return value is instead a matrix over @TEX "$\\mathbb{F}_p$"@ whose
    columns form a basis of the submodule weight space, written in the
    ambient Weyl module's chosen basis.  For a quotient or subquotient the
    return value is an identity matrix in the chosen quotient coordinates;
    @TO quotientSection@ gives the corresponding ambient representatives.
    A missing integral-module weight returns @TT "{}"@; a missing modular
    weight returns @TT "null"@.
  Example
    -- Standard sl_3 representation: each weight space has dimension 1.  Each PBW tuple
    -- has one entry per positive root.
    W = weylModuleZ(cartanA 3, {1, 0})
    weightSpace(W, {1, 0})                         -- Dynkin = highest weight
    weightSpace(W, {-1, 1})
    weightSpace(W, {0, 0, 0}, WeightFormat => "Partition")  -- not a weight, returns {}
    weightSpace(W, {0, 1, 0}, WeightFormat => "Partition")  -- = (-1, 1) in Dynkin
  Text
    The first two nonempty lists each contain one PBW exponent tuple, representing a basis vector in a rank-one weight space of the natural module.  The zero weight is absent, hence the empty list.  The partition label @TEX "$(0,1,0)$"@ describes the same weight as the Dynkin label @TEX "$(-1,1)$"@.
  Example
    -- Adjoint representation V(1,1): the zero weight space has dimension 2
    -- (it is the Cartan), so two PBW tuples are returned.
    Wadj = weylModuleZ(cartanA 3, {1, 1})
    weightSpace(Wadj, {0, 0})                      -- two basis tuples
  Text
    The two returned exponent tuples represent independent vectors of weight zero.  In the characteristic-zero adjoint representation this weight space is the two-dimensional Cartan subalgebra.
SeeAlso
  weightSpaces
  basisVecs
  WeightFormat
///

doc ///
Key
  weightSpaces
  (weightSpaces, WeylModuleZ)
  (weightSpaces, SubWeylModuleFp)
  (weightSpaces, QuotientWeylModuleFp)
  (weightSpaces, SubquotientWeylModuleFp)
  [weightSpaces, WeightFormat]
Headline
  Bases of the weight spaces, indexed by weight
Usage
  H = weightSpaces M
  H = weightSpaces(M, WeightFormat => "Partition")
Inputs
  M:
    an integral module, modular submodule, quotient, or subquotient
Outputs
  H:HashTable
    weights mapped to PBW-tuple lists for an integral module, ambient basis
    matrices for a submodule, or identity matrices in quotient coordinates
Description
  Text
    Returns a @TT "HashTable"@ whose keys are the weights of @TT "M"@.
    For a @TO WeylModuleZ@, each value is the list of PBW exponent tuples
    forming the chosen @TEX "$\\mathbb{Z}$"@-basis.  For a
    @TO SubWeylModuleFp@, each value is a matrix over
    @TEX "$\\mathbb{F}_p$"@ whose columns are a basis in ambient
    Weyl-module coordinates.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})              -- adjoint, dim 8
    pairs weightSpaces W
    -- The (0,0) entry has two tuples: the Cartan is 2-dim.
    pairs weightSpaces(W, WeightFormat => "Partition")
  Text
    Each key names a weight and its value lists the corresponding PBW basis vectors.  The zero-weight key has two tuples, while each root-weight key has one.  Thus the table describes six rank-one root spaces and a rank-two zero-weight space, in either coordinate convention.
SeeAlso
  weightSpace
  basisVecs
  WeightFormat
///

doc ///
Key
  basisVecs
  (basisVecs, WeylModuleZ)
  (basisVecs, SubWeylModuleFp)
  (basisVecs, QuotientWeylModuleFp)
  (basisVecs, SubquotientWeylModuleFp)
  [basisVecs, WeightFormat]
  [basisVecs, WeightOrder]
Headline
  Flat list of the module's chosen basis vectors
Usage
  B = basisVecs M
  B = basisVecs(M, WeightFormat => "Partition")
  B = basisVecs(M, WeightOrder => "Lex")
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@,
    @TO QuotientWeylModuleFp@, or @TO SubquotientWeylModuleFp@
Outputs
  B:List
    pairs consisting of a weight and a PBW tuple, a one-column submodule
    basis matrix, or a quotient-coordinate basis vector
Description
  Text
    For a @TO WeylModuleZ@, returns a flat list of pairs
    @TT "(weight, tuple)"@ forming the chosen PBW
    @TEX "$\\mathbb{Z}$"@-basis of @TEX "$V(\\lambda)$"@.  For a
    @TO SubWeylModuleFp@, the second entry in each pair is a one-column
    matrix over @TEX "$\\mathbb{F}_p$"@, expressed in the ambient
    weight-space basis.  For quotients and subquotients it is a standard
    basis column in the chosen quotient coordinates; @TO quotientSection@
    gives its ambient representative.  The order is consistent with
    @TO weights@ and is controlled by @TO WeightOrder@.
  Example
    -- Standard representation: 3 weights, 3 basis vectors.
    W = weylModuleZ(cartanA 3, {1, 0})
    basisVecs W
  Text
    The three pairs record one basis vector in each weight space.  In a pair @TEX "$(\\mu,a)$"@, the first list is the weight and the second is the exponent tuple for the ordered divided-power PBW monomial acting on the highest-weight vector.
  Example
    -- Adjoint: 7 distinct weights but 8 basis vectors (zero appears twice).
    Wadj = weylModuleZ(cartanA 3, {1, 1})
    #(basisVecs Wadj)
    -- In partition coords, the zero weight is (0,0,0):
    select(basisVecs(Wadj, WeightFormat => "Partition"), p -> p#0 == {0, 0, 0})
  Text
    The total basis has eight vectors.  The selected two pairs both have weight zero but distinct PBW exponent tuples, accounting for the rank-two Cartan weight space.  Repeated weight labels therefore denote different basis vectors, not duplicate records.
SeeAlso
  weightSpace
  weightSpaces
  WeightFormat
///

doc ///
Key
  actionF
  (actionF, WeylModuleZ, List, ZZ)
  (actionF, SubWeylModuleFp, List, ZZ)
  (actionF, QuotientWeylModuleFp, List, ZZ)
  (actionF, SubquotientWeylModuleFp, List, ZZ)
  [actionF, WeightFormat]
Headline
  Matrix of f_i between adjacent weight spaces
Usage
  M = actionF(W, mu, i)
Inputs
  W:
    an integral module, modular submodule, quotient, or subquotient
  mu:List
    the target weight, in the coordinates selected by @TT "WeightFormat"@
  i:ZZ
    1-based index of a simple root
Outputs
  M:Matrix
    the matrix of @TEX "$f_i : W_{\\mu+\\alpha_i} \\to W_\\mu$"@
    in the chosen @TEX "$\\mathbb{Z}$"@-bases (or @TEX "$\\mathbb{F}_p$"@-bases
    for a module in characteristic @TT "p"@), or @TT "null"@ if either
    @TT "mu"@ or @TT "mu + alpha_i"@ is not a weight of
    @TT "W"@
Description
  Text
    The lowering operator @TEX "$f_i \\in \\mathfrak{g}$"@ corresponding
    to the negative simple root @TEX "$-\\alpha_i$"@ acts on the
    integral form @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ as a sum of
    @TEX "$\\mathbb{Z}$"@-linear maps
    @TEX "$f_i : V_{\\mu+\\alpha_i} \\to V_\\mu$"@ between adjacent
    weight spaces.  This routine returns the matrix of one such
    component in the chosen Z-basis (see @TO weightSpace@).
  Text
    The entries are integers because the Chevalley generator @TEX "$f_i$"@
    belongs to Kostant's @TEX "$\\mathbb{Z}$"@-form.  More generally, the
    divided powers @TEX "$f_i^{(k)}=f_i^k/k!$"@ preserve the integral
    lattice even though their expression inside the rational enveloping
    algebra contains a factorial denominator.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {1, 0})
    actionF(W, {-1, 1}, 1)         -- f_1 : V_lambda -> V_{lambda - alpha_1}
    actionF(W, {0, -1}, 2)         -- f_2 : V_{(-1,1)} -> V_{(0,-1)}
  Text
    The matrices @TEX "$(1)$"@ and @TEX "$(-1)$"@ give the two nonzero lowering maps between the three rank-one weight spaces of the natural module.  Thus @TEX "$f_1v_{(1,0)}=v_{(-1,1)}$"@ and @TEX "$f_2v_{(-1,1)}=-v_{(0,-1)}$"@ in the chosen PBW bases.  The second sign depends on that basis convention.
  Example
    -- Adjoint of sl_3: the f_1 step from V_(1,1) to V_(-1,2).
    Wadj = weylModuleZ(cartanA 3, {1, 1})
    actionF(Wadj, {-1, 2}, 1)
  Text
    The matrix @TEX "$(1)$"@ means that @TEX "$f_1$"@ carries the adjoint highest-weight vector to the chosen basis vector of weight @TEX "$(-1,2)=(1,1)-\\alpha_1$"@.
  Example
    -- The induced action on a quotient over F_2.
    W = weylModuleZ(cartanA 3, {1, 0})
    Q = simpleQuotient(W, 2)
    actionF(Q, {-1, 1}, 1)
  Text
    The entry 1 is now in @TEX "$\\mathbb F_2$"@.  It describes the induced lowering map on the simple natural module after reduction; the source and target weights are unchanged.
SeeAlso
  actionE
  actionH
  weightSpace
  applyChevalley
///

doc ///
Key
  actionE
  (actionE, WeylModuleZ, List, ZZ)
  (actionE, SubWeylModuleFp, List, ZZ)
  (actionE, QuotientWeylModuleFp, List, ZZ)
  (actionE, SubquotientWeylModuleFp, List, ZZ)
  [actionE, WeightFormat]
Headline
  Matrix of e_i between adjacent weight spaces
Usage
  M = actionE(W, mu, i)
Inputs
  W:
    an integral module, modular submodule, quotient, or subquotient
  mu:List
    the source weight, in the coordinates selected by @TT "WeightFormat"@
  i:ZZ
    1-based index of a simple root
Outputs
  M:Matrix
    the matrix of @TEX "$e_i : W_\\mu \\to W_{\\mu+\\alpha_i}$"@
    in the chosen @TEX "$\\mathbb{Z}$"@-bases (or the reduction's
    @TEX "$\\mathbb{F}_p$"@-bases), or @TT "null"@ if either
    @TT "mu"@ or @TT "mu + alpha_i"@ is not a weight of
    @TT "W"@
Description
  Text
    Builds the @TEX "$e_i$"@ action by induction on weight depth using
    the relation @TEX "$[e_i, f_j] = \\delta_{ij} h_i$"@.  Concretely, for
    each weight @TEX "$\\mu < \\lambda$"@ each basis vector @TT "v"@ of
    @TEX "$V(\\lambda)_\\mu$"@ is decomposed as
    @TEX "$v = \\sum_l c_l f_{j_l} w_l$"@ (with @TT "w_l"@ at higher
    weight) by solving a linear system for the incoming @TEX "$f_j$"@
    maps, using @TT "//"@; then
    @TEX "$e_i v = \\sum_l c_l (f_{j_l}(e_i w_l) + \\delta_{i,j_l} h_i w_l)$"@,
    using the inductively known @TT "e_i w_l"@ at higher weight.
  Text
    For a @TO QuotientWeylModuleFp@, this returns the induced action on
    quotient weight spaces over its prime field.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {1, 0})
    actionE(W, {-1, 1}, 1)         -- e_1 : V_{lambda - alpha_1} -> V_lambda
    actionE(W, {0, -1}, 2)         -- e_2 : V_{lambda - alpha_1 - alpha_2} -> V_{lambda - alpha_1}
  Text
    The matrices @TEX "$(1)$"@ and @TEX "$(-1)$"@ are the raising maps from weights @TEX "$(-1,1)$"@ to @TEX "$(1,0)$"@, and from @TEX "$(0,-1)$"@ to @TEX "$(-1,1)$"@, in the chosen PBW bases.  Both maps have rank one.
  Example
    -- The quotient action is a matrix over F_2.
    Q = simpleQuotient(W, 2)
    actionE(Q, {-1, 1}, 1)
    -- Highest weight: e_i v_lambda = 0 (target not a V-weight).
    actionE(W, {1, 0}, 1)
  Text
    The quotient matrix @TEX "$(1)$"@ describes the same raising map over @TEX "$\\mathbb F_2$"@.  The final call returns @TT "null"@ because the weight above the highest weight is absent; mathematically @TEX "$e_1$"@ annihilates the highest-weight line.
SeeAlso
  actionF
  actionH
///

doc ///
Key
  actionH
  (actionH, WeylModuleZ, ZZ, List)
  (actionH, SubWeylModuleFp, ZZ, List)
  (actionH, QuotientWeylModuleFp, ZZ, List)
  (actionH, SubquotientWeylModuleFp, ZZ, List)
  [actionH, WeightFormat]
Headline
  Scalar action of the Cartan element h_i on V(lambda)_mu
Usage
  c = actionH(W, i, mu)
Inputs
  W:
    an integral module, modular submodule, quotient, or subquotient
  i:ZZ
    1-based index of a simple root
  mu:List
    a weight (Dynkin coordinates) of @TEX "$V(\\lambda)$"@
Outputs
  c:ZZ
    the integer @TEX "$\\langle \\mu, \\alpha_i^\\vee \\rangle$"@
Description
  Text
    The Cartan element @TEX "$h_i := \\alpha_i^\\vee \\in \\mathfrak{h}$"@
    acts on a weight vector @TEX "$v \\in V(\\lambda)_\\mu$"@ by the
    scalar @TEX "$\\mu(h_i) = \\langle\\mu, \\alpha_i^\\vee\\rangle$"@, which is
    exactly the @TT "i"@th Dynkin coordinate of @TEX "$\\mu$"@.
  Text
    Together with the matrices returned by @TO actionE@ and @TO actionF@,
    this scalar gives the Cartan action in the
    @TEX "$\\mathfrak{sl}_2$"@ relation @TEX "$[e_i, f_i] = h_i$"@.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})
    actionH(W, 1, {2, 0})         -- = lambda_1
    actionH(W, 2, {0, 1})         -- = 1
    actionH(W, 1, {-2, 2})        -- = -2
  Text
    The scalars @TEX "$2,1,-2$"@ are the indicated Dynkin coordinates of the three weights.  They mean that @TEX "$h_1$"@ acts as @TEX "$2$"@ on the highest-weight space, @TEX "$h_2$"@ acts as @TEX "$1$"@ on the weight-@TEX "$(0,1)$"@ space, and @TEX "$h_1$"@ acts as @TEX "$-2$"@ on the weight-@TEX "$(-2,2)$"@ space.
SeeAlso
  actionF
  actionE
///

doc ///
Key
  SubWeylModuleFp
Headline
  A U_p-stable subspace of V(lambda) tensor F_p
Description
  Text
    A @TT "SubWeylModuleFp"@ is a @TEX "$U_p$"@-stable
    @TEX "$\\mathbb{F}_p$"@-subspace
    @TEX "$S \\subseteq V(\\lambda) \\otimes \\mathbb{F}_p$"@, where
    @TEX "$U_p$"@ is the @TEX "$\\mathbb{F}_p$"@-form of the
    hyperalgebra (the algebra generated by all divided powers
    @TEX "$E_\\alpha^{(p^k)}, F_\\alpha^{(p^k)}$"@ and the @TEX "$\\binom{H_i}{p^k}$"@'s,
    reduced mod @TEX "$p$"@).  At each weight @TEX "$\\mu$"@,
    the columns of a matrix over @TEX "$\\mathbb{F}_p$"@ span
    @TEX "$S \\cap V(\\lambda)_\\mu$"@ in the chosen integral basis
    of @TEX "$V_\\mu$"@.  The ambient integral module, characteristic,
    weight-space dimensions, and chosen generators are also retained.
  Text
    Radicals, terms of the socle filtration, and kernels of quotient maps
    are submodules of this type.  They occur in @TO socleSeries@,
    @TO maximalSubmodule@, and @TO socleLengthTwoQuotient@, including
    the quotients used to study two-factor extensions.
  Text
    @TO subWeylModule@ constructs the submodule generated by one or more
    weight vectors, and @TO subWeylModuleDirectSum@ forms an internal
    direct sum.  Its dimension, weights, character, and weight-space bases
    are given by @TO (dim, SubWeylModuleFp)@,
    @TO (weights, SubWeylModuleFp)@, @TO (character, SubWeylModuleFp)@,
    @TO (weightSpace, SubWeylModuleFp, List)@, and
    @TO (basisVecs, SubWeylModuleFp)@.  @TO ambientWeylModule@ gives
    the ambient integral module, and @TO isWithin@ tests membership.
  Example
    -- The non-trivial simple submodule L(0,1) inside V(2 omega_1) for sl_3 mod 2.
    W = weylModuleZ(cartanA 3, {2, 0})
    soc = socleWeyl(W, 2)            -- a SubWeylModuleFp
    dim soc
    weights soc
  Text
    The submodule has dimension 3 and the three weights of @TEX "$\\Lambda^2\\mathbb F_2^3$"@.  It is the simple socle @TEX "$L(\\omega_2)$"@ inside the six-dimensional divided square.  The ambient integral module still has highest weight @TEX "$2\\omega_1$"@.
SeeAlso
  subWeylModule
  weylModuleZ
  socleWeyl
  socleSeries
  QuotientWeylModuleFp
///

doc ///
Key
  subWeylModule
  (subWeylModule, WeylModuleZ, ZZ, List, List)
  (subWeylModule, WeylModuleZ, ZZ, List)
  (subWeylModule, SubWeylModuleFp, List, List)
  [subWeylModule, WeightFormat]
Headline
  Cyclic or finitely-generated submodule of V(lambda) mod p
Usage
  S = subWeylModule(W, p, mu, vec)
  S = subWeylModule(W, p, gens)
  S = subWeylModule(S0, mu, vec)
Inputs
  W:WeylModuleZ
    a Weyl module
  p:ZZ
    a prime
  mu:List
    a weight (Dynkin or Partition format) of @TT "W"@
  vec:List
    a coefficient list (length @TEX "$\\dim V_\\mu$"@) over
    @TEX "$\\mathbb{F}_p$"@ in the chosen Z-basis at @TT "mu"@
  gens:List
    a list of @TT "(mu, vec)"@ pairs
  S0:SubWeylModuleFp
    an existing submodule, to be extended
Outputs
  S:SubWeylModuleFp
    the @TEX "$U_p$"@-stable submodule of
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@ generated by the given weight vectors
Description
  Text
    The hyperalgebra
    @TEX "$U_p(\\mathfrak{g}) := U_\\mathbb{Z}(\\mathfrak{g}) \\otimes \\mathbb{F}_p$"@
    is the algebra of all integer-coefficient
    "divided-power expressions" in the Chevalley generators, reduced
    mod @TT "p"@.  Every subspace represented by this package is
    weight-graded, so stability under the Cartan binomial operators is
    automatic.  For such a subspace, being a rational @TT "G"@-submodule
    is equivalent to stability under all relevant
    @TEX "$E_{\\alpha_j}^{(p^k)}$"@ and @TEX "$F_{\\alpha_j}^{(p^k)}$"@.
  Text
    The first form returns the cyclic @TEX "$U_p$"@-submodule of
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@ generated by the
    weight vector @TT "vec"@ at weight @TT "mu"@:
    @TEX "$\\langle v \\rangle_{U_p} := U_p \\cdot v$"@.  The second
    form returns the submodule generated by a list of weight vectors,
    @TEX "$\\sum_l U_p \\cdot v_l$"@.  The third form extends an
    existing submodule @TEX "$S_0$"@ to
    @TEX "$S_0 + U_p \\cdot v$"@.
  Text
    @SUBSECTION "Algorithm"@
  Text
    Apply the divided-power simple-root operators
    @TEX "$e_{\\alpha_j}^{(p^k)}$"@ and
    @TEX "$f_{\\alpha_j}^{(p^k)}$"@ for @TT "j = 1..rank"@ and
    @TT "k = 0..height"@ to the generators, retaining each image that
    increases the span at its weight.  Repeat until every image lies in
    the span already obtained.  This is Doty's procedure in
    @TT "lib/submodule.gi:8"@ and is also used in the socle computations.
  Example
    -- A_2 V(2,0) = D^2 V at p = 2: the maximal vector at weight (0, 1)
    -- generates the submodule L(0,1) = Lambda^2 V, the simple socle.
    W = weylModuleZ(cartanA 3, {2, 0})
    mvecs = maximalVectors(W, 2)
    m01 = first select(mvecs, x -> x#0 == {0, 1})
    S = subWeylModule(W, 2, m01#0, m01#1)
    dim S                          -- 3
    pairs character S              -- character of L(0,1) = std-dual of sl_3
  Text
    The selected maximal vector generates the three-dimensional simple submodule @TEX "$L(\\omega_2)$"@.  Its character has the three weights of @TEX "$\\Lambda^2\\mathbb F_2^3$"@, each with multiplicity one.
  Example
    -- The submodule generated by v_lambda is all of V(lambda).
    Sfull = subWeylModule(W, 2, {2, 0}, {1})
    dim Sfull                       -- = dim V = 6
  Text
    The output is 6: the highest-weight vector generates the entire Weyl module under the hyperalgebra, including its divided-power operators.
  Example
    -- Generators in partition coordinates: same module via partition (1,1,0)
    -- (= Dynkin (0, 1)) for sl_3.
    Spart = subWeylModule(W, 2, {1, 1, 0}, {1}, WeightFormat => "Partition")
    dim Spart                       -- 3
  Text
    The dimension is again 3.  The partition label @TEX "$(1,1,0)$"@ and Dynkin label @TEX "$(0,1)$"@ specify the same generator weight, so these two constructions give the same submodule.
SeeAlso
  SubWeylModuleFp
  subWeylModuleDirectSum
  maximalVectors
  isWithin
  WeightFormat
///

doc ///
Key
  isWithin
  (isWithin, SubWeylModuleFp, List, List)
  [isWithin, WeightFormat]
Headline
  Test whether a weight vector lies in a submodule
Usage
  isWithin(S, mu, vec)
  isWithin(S, mu, vec, WeightFormat => "Partition")
Inputs
  S:SubWeylModuleFp
  mu:List
    a weight (Dynkin or Partition format)
  vec:List
    an @TEX "$\\mathbb{F}_p$"@-coefficient list of length
    @TEX "$\\dim V(\\lambda)_\\mu$"@ in the chosen Z-basis
Outputs
  :Boolean
    true iff the indicated weight vector lies in @TT "S"@
Description
  Text
    Given a weight vector
    @TEX "$v \\in V(\\lambda)_\\mu$"@ presented as the @TEX "$\\mathbb{F}_p$"@-linear
    combination @TT "vec"@ of the chosen Z-basis at @TT "mu"@, decide
    whether @TEX "$v \\in S$"@.  Membership reduces to a rank
    comparison of @TT "(S basis at mu) | vec"@ against the
    @TT "S"@ basis matrix at @TT "mu"@.
  Example
    -- L(0,1) inside V(2,0) for sl_3 mod 2: the generator at (0,1) is in,
    -- but the highest-weight vector of V is not.
    W = weylModuleZ(cartanA 3, {2, 0})
    S = subWeylModule(W, 2, {0, 1}, {1})
    isWithin(S, {0, 1}, {1})           -- generator itself: in S
    isWithin(S, {2, 0}, {1})           -- v_lambda: not in S
    -- Same generator queried via partition coordinates:
    isWithin(S, {1, 1, 0}, {1}, WeightFormat => "Partition")
  Text
    The results are @TT "true"@, @TT "false"@, and @TT "true"@.  The socle contains its weight-@TEX "$\\omega_2$"@ generator but not the ambient highest-weight vector of weight @TEX "$2\\omega_1$"@.  The final query merely expresses the generator's weight in partition coordinates.
SeeAlso
  subWeylModule
  WeightFormat
///

doc ///
Key
  subWeylModuleDirectSum
  (subWeylModuleDirectSum, WeylModuleZ, ZZ, List)
Headline
  Form an internal direct sum of submodules
Usage
  S = subWeylModuleDirectSum(W, p, modules)
Inputs
  W:WeylModuleZ
  p:ZZ
    a prime
  modules:List
    a list of @TO SubWeylModuleFp@ in @TT "W"@ over @TEX "$\\mathbb{F}_p$"@
Outputs
  S:SubWeylModuleFp
    the @EM "internal"@ direct sum
    @TEX "$S = S_1 \\oplus \\cdots \\oplus S_r$"@ inside @TT "V"@,
    realized as the submodule generated by the union of all generators
Description
  Text
    Every summand must lie in the reduction of @TT "W"@ modulo @TT "p"@.
    Their sum is direct precisely when its dimension equals the sum of
    their dimensions; otherwise the function raises an error.
    @TO socleWeyl@ uses this sum for the simple submodules found in its
    maximal-vector search.
  Example
    -- Two independent simple submodules of B_2 V(0,4) mod 3.
    W = weylModuleZ(cartanB 2, {0, 4})
    M = maximalVectors(W, 3);
    m20 = first select(M, x -> x#0 == {2, 0});
    m10 = first select(M, x -> x#0 == {1, 0});
    S20 = subWeylModule(W, 3, m20#0, m20#1);
    S10 = subWeylModule(W, 3, m10#0, m10#1);
    S = subWeylModuleDirectSum(W, 3, {S20, S10});
    (dim S, dim S20 + dim S10)                 -- (19, 19)
  Text
    The pair @TEX "$(19,19)$"@ says that the sum has dimension equal to the sum of the two submodule dimensions.  Their intersection is zero, so this is an internal direct sum @TEX "$S_{20}\\oplus S_{10}$"@ inside the reduction of @TEX "$V(4\\omega_2)$"@ modulo 3.  It says nothing about a complementary summand in the ambient module.
SeeAlso
  subWeylModule
  socleWeyl
///

doc ///
Key
  ambientWeylModule
  (ambientWeylModule, SubWeylModuleFp)
  (ambientWeylModule, QuotientWeylModuleFp)
  (ambientWeylModule, WeylModuleZ)
  (ambientWeylModule, SubquotientWeylModuleFp)
Headline
  The ambient integral Weyl module of a submodule or quotient
Usage
  ambientWeylModule S
  ambientWeylModule Q
Inputs
  S:SubWeylModuleFp
    or @TO QuotientWeylModuleFp@
Outputs
  W:WeylModuleZ
    the integral Weyl module @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@ from
    whose reduction contains the submodule or maps onto the quotient
Description
  Text
    For a @TO SubWeylModuleFp@ @TT "S"@ this returns the ambient
    @TO WeylModuleZ@ such that @TEX "$S \\subseteq V_{\\mathbb{F}_p}(\\lambda)$"@.
    For a @TO QuotientWeylModuleFp@ @TT "Q"@ it returns the
    integral form of the source of the quotient map.  In both cases this
    is the original @TT "WeylModuleZ"@, with its chosen PBW bases,
    weights, and integral action matrices.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    S = subWeylModule(W, 2, {0, 1}, {1})           -- a SubWeylModuleFp
    ambientWeylModule S === W                       -- true
    Q = quotientWeylModule S
    ambientWeylModule Q === W                       -- true
  Text
    Both values are @TT "true"@: the submodule and quotient retain the very same integral ambient lattice @TEX "$V_{\\mathbb Z}(2\\omega_1)$"@.  This records their construction from its reduction; it does not identify either modular module with that lattice.
SeeAlso
  subWeylModule
  quotientWeylModule
  ambientCartan
  definingKernel
///

doc ///
Key
  QuotientWeylModuleFp
Headline
  A quotient V/S of V(lambda) tensor F_p by a SubWeylModuleFp
Description
  Text
    A @TT "QuotientWeylModuleFp"@ is the
    @TEX "$\\mathbb{F}_p$"@-vector space
    @TEX "$Q := (V(\\lambda) \\otimes \\mathbb{F}_p) / S$"@ for some
    @TO SubWeylModuleFp@ @TT "S"@, equipped with the induced
    @TEX "$U_p$"@-action.  A basis of each weight space @TT "Q_mu"@
    consists of cosets of selected vectors in the chosen basis of
    @TEX "$V_\\mu$"@.  Their positions in that basis are the
    @EM "complement indices"@.  The quotient retains the ambient
    @TO WeylModuleZ@, kernel @TT "S"@, characteristic @TT "p"@,
    change-of-basis matrices for the natural projection, and the induced
    @TEX "$f_i$"@- and @TEX "$e_i$"@-action matrices.
  Text
    When @TT "S = (0)"@ the quotient is
    @TEX "$V \\otimes \\mathbb{F}_p$"@ itself -- this is what
    @TO weylModule@ returns.  When @TT "S = \\mathrm{rad}(V)"@ the
    quotient is the irreducible @TEX "$L(\\lambda)$"@ -- this is what
    @TO simpleQuotient@ returns.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    mvecs = maximalVectors(W, 2)
    m01 = first select(mvecs, x -> x#0 == {0, 1})
    S = subWeylModule(W, 2, m01#0, m01#1)
    Q = quotientWeylModule S
    dim Q                        -- = dim V - dim S = 6 - 3 = 3
    weights Q
    pairs character Q
  Text
    The quotient has dimension @TEX "$6-3=3$"@.  Its three weight spaces form the character of @TEX "$L(2\\omega_1)=(\\mathbb F_2^3)^{(1)}$"@, the Frobenius twist of the natural module.  The killed submodule is @TEX "$\\Lambda^2\\mathbb F_2^3$"@.
SeeAlso
  quotientWeylModule
  SubWeylModuleFp
  weylModule
  simpleQuotient
  definingKernel
///

doc ///
Key
  quotientWeylModule
  (quotientWeylModule, SubWeylModuleFp)
Headline
  The quotient V / S
Usage
  Q = quotientWeylModule S
Inputs
  S:SubWeylModuleFp
    a U_p-stable subspace of V(lambda) tensor F_p
Outputs
  Q:QuotientWeylModuleFp
    the quotient V(lambda) tensor F_p / S, with U_p-action
Description
  Text
    Returns the quotient
    @TEX "$Q := (V(\\lambda) \\otimes \\mathbb{F}_p) / S$"@ from a
    @TO SubWeylModuleFp@.  At each weight @TEX "$\\mu$"@, select
    vectors in the chosen basis of @TEX "$V_\\mu$"@ whose cosets form
    a basis of @TEX "$V_\\mu / S_\\mu$"@.  The induced
    @TEX "$f_i, e_i$"@-actions on @TT "Q"@ are expressed in these bases.
  Example
    -- A_2 V(2,0) mod 2: dim 6, with maximal sub L(0,1) (dim 3).
    W = weylModuleZ(cartanA 3, {2, 0})
    mvecs = maximalVectors(W, 2)
    m01 = first select(mvecs, x -> x#0 == {0, 1})
    S = subWeylModule(W, 2, m01#0, m01#1)         -- the 3-dim sub
    dim S
    Q = quotientWeylModule S                       -- the 3-dim quotient
    dim Q
  Text
    The submodule and quotient both have dimension 3, while the ambient Weyl module has dimension 6.  The quotient is the simple head @TEX "$L(2\\omega_1)$"@ of the nonsplit divided-square extension in characteristic 2.
  Example
    -- The whole V mod p as a "quotient by zero":
    W = weylModuleZ(cartanA 3, {2, 0})
    Z = subWeylModule(W, 2, {})                    -- zero submodule
    Q = quotientWeylModule Z
    dim Q                                           -- = dim W = 6
  Text
    The quotient by zero has dimension 6 and is the whole reduction @TEX "$V_{\\mathbb F_2}(2\\omega_1)$"@.  The quotient construction does not itself impose simplicity.
SeeAlso
  SubWeylModuleFp
  QuotientWeylModuleFp
  definingKernel
  simpleQuotient
  weylModule
///

doc ///
Key
  (dim, WeylModuleZ)
  (dim, SubWeylModuleFp)
  (dim, QuotientWeylModuleFp)
  (dim, SubquotientWeylModuleFp)
  (dim, WeylCharacter)
Headline
  Rank, dimension, or virtual character dimension
Usage
  d = dim M
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@,
    @TO QuotientWeylModuleFp@, @TO SubquotientWeylModuleFp@, or @TO WeylCharacter@
Outputs
  d:ZZ
    the integral rank for @TO WeylModuleZ@ or the
    @TEX "$\\mathbb{F}_p$"@-dimension for a submodule or quotient
Description
  Text
    For a quotient @TEX "$Q=V/S$"@, this satisfies
    @TEX "$\\dim Q=\\dim V-\\dim S$"@.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    S = socleWeyl(W, 2);
    Q = quotientWeylModule S;
    (dim W, dim S, dim Q)
  Text
    The triple @TEX "$(6,3,3)$"@ expresses rank over @TEX "$\\mathbb Z$"@ for the integral Weyl module and dimensions over @TEX "$\\mathbb F_2$"@ for its socle and quotient.  It verifies the dimension identity @TEX "$6=3+3$"@ for the short exact sequence after reduction.
  Text
    For a subquotient @TEX "$T/S$"@, return
    @TEX "$\\dim T-\\dim S$"@ over its prime field. For a @TO WeylCharacter@,
    @TT "dim"@ sums its formal coefficients, as @TO characterDim@ does;
    the result can be negative for a virtual character.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    T = weylCharacter weylModuleZ(cartanA 2, {0})
    dim(C - 3*T)
  Text
    The result -1 is a virtual dimension: two dimensions from the natural
    character minus three from the trivial characters. It does not describe
    the dimension of a vector space.
SeeAlso
  characterDim
  ambientWeylModule
  definingKernel
///

doc ///
Key
  (generators, SubWeylModuleFp)
  [generators, CoefficientRing]
Headline
  Recorded generators of a modular submodule
Usage
  gens = generators S
Inputs
  S:SubWeylModuleFp
Outputs
  gens:List
    recorded pairs @TT "(mu, v)"@ of a Dynkin-coordinate weight and a
    one-column matrix over @TEX "$\\mathbb{F}_p$"@
Description
  Text
    Returns the chosen generators of @TT "S"@, expressed in the ambient
    Weyl-module weight-space bases.  This generating set need not be minimal
    or canonical.  The weights are always in Dynkin coordinates.
    The @TT "CoefficientRing"@ option has no effect on this method.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    S = socleWeyl(W, 2);
    generators S
  Text
    Each returned pair consists of a weight and the coordinates of a recorded generator in that ambient weight space.  Here the weight-@TEX "$(0,1)$"@ vector generates the simple socle @TEX "$L(\\omega_2)$"@; its entire three-dimensional basis is obtained by the hyperalgebra action, rather than by listing three generators.
SeeAlso
  subWeylModule
  basisVecs
  ambientWeylModule
///

doc ///
Key
  (net, WeylModuleZ)
  (net, SubWeylModuleFp)
  (net, QuotientWeylModuleFp)
  (net, ReductionProfile)
  (net, SubquotientWeylModuleFp)
  (net, WeylCharacter)
  (net, DecompositionTable)
  (net, SocleAnalysis)
  (net, ExtensionData)
  (net, ChevalleyActionData)
  (net, WeylModuleCheck)
  (net, WeylGraph)
  (net, SocleDiagram)
Headline
  Display of a Weyl module, submodule, or quotient
Usage
  net M
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@
Outputs
  :Net
    the displayed form of the module
Description
  Text
    Displays the type of module and its defining parameters.
  Example
    W = weylModuleZ(cartanA 3, {1, 0})
    net W
  Text
    The display identifies an integral type-@TEX "$A_2$"@ module of highest weight @TEX "$(1,0)$"@ and rank 3 over @TEX "$\\mathbb Z$"@.  It is the natural @TEX "$\\mathrm{SL}_3$"@ Weyl lattice.  The display summarizes the module; @TO character@ and @TO weightSpaces@ give its individual weight spaces.
SeeAlso
  WeylModuleZ
  SubWeylModuleFp
  QuotientWeylModuleFp
///

doc ///
Key
  simpleQuotient
  (simpleQuotient, WeylModuleZ, ZZ)
  (simpleQuotient, ZZ, List)
  [simpleQuotient, LieType]
  [simpleQuotient, WeightFormat]
Headline
  The simple quotient L(lambda) of V(lambda) tensor F_p
Usage
  simpleQuotient(W, p)
  simpleQuotient(p, lambda)
  simpleQuotient(p, lambda, LieType => "B")
  simpleQuotient(p, lambda, WeightFormat => "Dynkin")
Inputs
  W:WeylModuleZ
    a previously constructed integral Weyl module
  p:ZZ
    a prime
  lambda:List
    a dominant highest weight; the direct characteristic-weight form uses @TT "Auto"@
    coordinates as described below
Outputs
  Q:QuotientWeylModuleFp
    the simple head @TEX "$L(\\lambda)$"@, represented as a quotient of
    its integral Weyl module reduced modulo @TT "p"@
Description
  Text
    Every Weyl module
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@ has a unique simple
    quotient @TEX "$L(\\lambda)$"@, namely
    @TEX "$L(\\lambda) := V(\\lambda) / \\mathrm{rad}(V(\\lambda))$"@,
    where the @EM "radical"@ @TEX "$\\mathrm{rad}(V)$"@ is the
    intersection of all maximal proper submodules.  For a split simply
    connected group, the collection
    @TEX "$\\{L(\\lambda) : \\lambda \\in P^+\\}$"@ is a complete set of
    pairwise non-isomorphic simple finite-dimensional rational
    @TEX "$G$"@-modules in characteristic @TT "p"@.
  Text
    @SUBSECTION "Algorithm"@
  Text
    Following Doty (lib/weylmodule.gi:395), start with the
    zero submodule @TEX "$S = (0) \\subseteq V(\\lambda)$"@.  While
    @TEX "$V/S$"@ has a maximal vector at a weight strictly below
    @TEX "$\\lambda$"@, add the @TEX "$U_p$"@-submodule generated by it
    to @TT "S"@.  Iterate.  The final @TT "S"@ is
    @TEX "$\\mathrm{rad}(V)$"@ and @TEX "$V/S = L(\\lambda)$"@.
  Text
    The two-argument form @TT "simpleQuotient(W, p)"@ takes a previously
    constructed integral form, while @TT "simpleQuotient(p, lambda)"@
    constructs it from the highest weight.  The @TT "LieType"@ option
    selects the Cartan family (default
    @TT "\"A\""@).  Classical input defaults to partition coordinates:
    a type-A partition of length @TT "n"@ selects rank @TT "n-1"@,
    while types B, C, and D select rank @TT "n"@.  Exceptional input is
    Dynkin.  Use @TT "WeightFormat => \"Dynkin\""@ to override the
    classical default.  This is the same convention as for @TO weylModule@.
  Example
    -- A_2 V(2 omega_1) = D^2 V at p = 2.  Recall the SES
    --   0 -> Lambda^2 V -> D^2 V -> F^* V -> 0,
    -- so L(2,0) = F^* V is 3-dim (Frobenius twist of std).
    W = weylModuleZ(cartanA 3, {2, 0})
    Q = simpleQuotient(W, 2)
    dim Q
    pairs character Q
  Text
    The dimension is 3, and the character consists of the three weights of the Frobenius twist of the natural @TEX "$\\mathrm{SL}_3$"@-module.  This is @TEX "$L(2\\omega_1)$"@, obtained by killing the simple socle @TEX "$L(\\omega_2)$"@ of the divided square.
  Example
    -- The same example in partition coordinates: V(2,0,0) for sl_3
    -- at p = 2 has L(2,0,0) = F^* V of dim 3.
    Q = simpleQuotient(2, {2, 0, 0})
    dim Q
  Text
    The dimension is again 3.  The partition @TEX "$(2,0,0)$"@ names the same highest weight @TEX "$2\\omega_1$"@, so this is the same simple head constructed without first specifying the integral lattice.
  Example
    -- B_2 V(omega_1) at p = 2: V is the 5-dim defining rep of so(5),
    -- but in char 2 there's a 1-dim trivial submodule and L(omega_1)
    -- has dim 4 -- this is the so(5) ~ sp(4) "low char" exception.
    dim simpleQuotient(2, {1, 0}, LieType => "B")
  Text
    The dimension 4 is that of the simple head @TEX "$L(\\omega_1)$"@ in type @TEX "$B_2$"@ in characteristic 2.  The five-dimensional natural Weyl module has a one-dimensional trivial radical, so its head is smaller than its Weyl module.
  Example
    -- G_2 V(omega_1) at p = 2: 7-dim short rep degenerates to a 6-dim
    -- simple quotient with a 1-dim trivial socle.
    dim simpleQuotient(2, {1, 0}, LieType => "G_2")
  Text
    The dimension 6 is that of the simple head @TEX "$L(\\omega_1)$"@ in type @TEX "$G_2$"@ in characteristic 2.  The seven-dimensional Weyl module has a trivial one-dimensional socle and radical.
  Example
    -- D_4 half-spin rep is irreducible mod 2: L = V, dim 8.
    dim simpleQuotient(2, {1/2, 1/2, 1/2, 1/2}, LieType => "D")
  Text
    The output 8 equals the Weyl dimension of the chosen type-@TEX "$D_4$"@ half-spin module.  Thus the radical is zero in characteristic 2, and the simple head is the whole Weyl module.
SeeAlso
  simpleCharacter
  simpleDimension
  socleWeyl
  decompositionNumbers
  maximalSubmodule
  weylModule
  "Tutorial: socle structure and Doty's two-factor extensions"
///

doc ///
Key
  simpleCharacter
  (simpleCharacter, Matrix, ZZ, List)
  [simpleCharacter, WeightFormat]
Headline
  Character of the simple module L(lambda) in characteristic p
Usage
  ch = simpleCharacter(A, p, lambda)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  p:ZZ
    a prime
  lambda:List
    a dominant highest weight in the chosen coordinate format
Outputs
  ch:HashTable
    the character, mapping weights in the chosen format to multiplicities
Description
  Text
    The character
    @TEX "$\\mathrm{ch}(L(\\lambda))$"@ of the simple rational
    @TEX "$G$"@-module, equivalently the corresponding simple integrable
    weight module for the hyperalgebra, of highest weight @TEX "$\\lambda$"@ in
    characteristic @TEX "$p$"@.  These characters classify the
    irreducible rational representations of the split simply connected simple
    algebraic group @TEX "$G$"@ in characteristic @TEX "$p$"@.
    The character of every finite-dimensional rational module is an integral
    sum of simple characters, with coefficients equal to its composition
    multiplicities.
  Text
    @SUBSECTION "Steinberg's tensor product theorem"@
  Text
    Steinberg's tensor product theorem reduces the calculation
    of @TEX "$\\mathrm{ch}(L(\\lambda))$"@ for arbitrary
    @TEX "$\\lambda$"@ to a product of simple characters at
    @TEX "$p$"@-restricted weights.  Write
    @TEX "$\\lambda = \\sum_{k \\geq 0} p^k \\lambda^{(k)}$"@ in base
    @TEX "$p$"@, with each @TEX "$\\lambda^{(k)} = \\sum_i m_i^{(k)} \\omega_i$"@
    satisfying @TEX "$0 \\leq m_i^{(k)} < p$"@ -- so each
    @TEX "$\\lambda^{(k)}$"@ is @TEX "$p$"@-restricted.  Then
  Text
    @TEX "$L(\\lambda) \\;\\cong\\; L(\\lambda^{(0)}) \\otimes \\mathrm{Fr}^*(L(\\lambda^{(1)})) \\otimes \\mathrm{Fr}^{2*}(L(\\lambda^{(2)})) \\otimes \\cdots$"@,
  Text
    where @TEX "$\\mathrm{Fr}^*$"@ is pullback along the Frobenius
    isogeny @TEX "$F : G \\to G$"@.  A weight @TEX "$\\nu$"@ in the
    Frobenius twist @TEX "$\\mathrm{Fr}^*(L(\\mu))$"@ scales by
    @TEX "$p$"@ relative to its weight in @TEX "$L(\\mu)$"@; combined
    with multiplicativity of characters under tensor product, this
    yields the recurrence
    @TEX "$\\mathrm{ch}(L(\\lambda)) = \\mathrm{ch}(L(\\lambda_0)) \\cdot p^* \\mathrm{ch}(L(\\lambda_1))$"@
    used by @TT "simpleCharacter"@, with @TEX "$\\lambda_0 = \\lambda \\bmod p$"@
    and @TEX "$\\lambda_1 = \\lfloor \\lambda / p \\rfloor$"@.
  Text
    The @TEX "$p$"@-restricted base case (@TEX "$0 \\leq m_i < p\\,\\forall i$"@)
    is computed by @TO simpleQuotient@ from the Weyl module
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@.  Repeated calculations
    for the same @TEX "$(A, p, \\lambda)$"@ reuse the character already computed.
  Example
    sort pairs simpleCharacter(cartanA 2, 2, {2})
  Text
    The two weights are @TEX "$2\\omega_1$"@ and @TEX "$-2\\omega_1$"@,
    each with multiplicity one.  Thus @TEX "$L(2\\omega_1)$"@ is the
    Frobenius twist of the two-dimensional natural
    @TEX "$\\mathrm{SL}_2$"@-module.  Its character has no zero-weight term,
    whereas the Weyl module @TEX "$V(2\\omega_1)$"@ has dimension three.
  Example
    A = cartanA 3
    sort pairs simpleCharacter(A, 2, {2, 0})
  Text
    The weights @TEX "$(2,0),(-2,2),(0,-2)$"@, each with multiplicity one,
    are twice the weights of the natural @TEX "$\\mathrm{SL}_3$"@-module.
    This is @TEX "$L(2\\omega_1)\\cong\\mathrm{Fr}^*L(\\omega_1)$"@ in
    characteristic two.
  Example
    sort pairs simpleCharacter(A, 2, {2, 0, 0}, WeightFormat => "Partition")
  Text
    The same three weights are written in epsilon coordinates, with the
    last coordinate chosen to be zero.  In particular,
    @TEX "$(-2,2)$"@ in Dynkin coordinates becomes @TEX "$(0,2,0)$"@,
    and @TEX "$(0,-2)$"@ becomes @TEX "$(-2,-2,0)$"@.  The negative
    entries reflect this choice of representative for an
    @TEX "$\\mathrm{SL}_3$"@ weight; they do not mean negative weight
    multiplicities.
SeeAlso
  simpleDimension
  simpleTopFactorCharacter
  simpleQuotient
  decompositionNumbers
  WeightFormat
///

doc ///
Key
  simpleDimension
  (simpleDimension, Matrix, ZZ, List)
  [simpleDimension, WeightFormat]
Headline
  Dimension of the simple module L(lambda) in characteristic p
Usage
  d = simpleDimension(A, p, lambda)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  p:ZZ
    a prime
  lambda:List
    a dominant highest weight in the chosen coordinate format
Outputs
  d:ZZ
    the dimension of the simple rational module of highest weight lambda
Description
  Text
    The simple module @TEX "$L(\\lambda)$"@ is the unique simple quotient
    of the Weyl module @TEX "$V(\\lambda)$"@ in characteristic
    @TEX "$p$"@.  Its dimension is the sum of the weight multiplicities
    in @TO simpleCharacter@.  Weyl's dimension formula gives the
    dimension of @TEX "$V(\\lambda)$"@; it gives the dimension of
    @TEX "$L(\\lambda)$"@ only when that Weyl module is simple.
  Text
    If @TEX "$\\lambda=\\sum_k p^k\\lambda^{(k)}$"@ is its expansion in
    p-restricted weights, Steinberg's tensor product theorem gives
    @TEX "$\\dim L(\\lambda)=\\prod_k\\dim L(\\lambda^{(k)})$"@, because a
    Frobenius twist preserves dimension.
  Example
    A = cartanA 3
    simpleDimension(A, 2, {2, 0})
    dim weylModuleZ(A, {2, 0})
  Text
    The answers are three and six.  In characteristic two,
    @TEX "$L(2\\omega_1)$"@ is the three-dimensional Frobenius twist of
    the natural module, whereas @TEX "$V(2\\omega_1)=D^2(k^3)$"@ is
    its six-dimensional divided square.
  Example
    simpleDimension(A, 2, {3, 0})
    simpleDimension(A, 2, {3, 0, 0}, WeightFormat => "Partition")
  Text
    Both calls return nine.  The first uses Dynkin coordinates and the
    second uses the partition @TEX "$(3,0,0)$"@.  Since
    @TEX "$3\\omega_1=\\omega_1+2\\omega_1$"@, the simple module is
    @TEX "$L(\\omega_1)\\otimes\\mathrm{Fr}^*L(\\omega_1)$"@ and has
    dimension @TEX "$3\\cdot3=9$"@.
SeeAlso
  simpleCharacter
  simpleTopFactorDim
  simpleQuotient
  WeightFormat
///

doc ///
Key
  simpleTopFactorCharacter
  (simpleTopFactorCharacter, Matrix, ZZ, List)
  [simpleTopFactorCharacter, WeightFormat]
Headline
  Character of the simple head of a Weyl module
Usage
  ch = simpleTopFactorCharacter(A, p, lambda)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  p:ZZ
    a prime
  lambda:List
    a dominant highest weight in the chosen coordinate format
Outputs
  ch:HashTable
    the weights of the simple head, mapped to their multiplicities
Description
  Text
    The head @TEX "$V(\\lambda)/\\mathrm{rad}(V(\\lambda))$"@ of a Weyl
    module is the simple module @TEX "$L(\\lambda)$"@.  This function
    returns its formal character in characteristic @TEX "$p$"@.
    It agrees with @TO simpleCharacter@ and retains the name
    @TT "SimpleTopFactorCharacter"@ used in Doty's GAP package.
  Example
    sort pairs simpleTopFactorCharacter(cartanA 2, 2, {2})
  Text
    The head of the three-dimensional divided square for
    @TEX "$\\mathrm{SL}_2$"@ has just two weights, @TEX "$2\\omega_1$"@
    and @TEX "$-2\\omega_1$"@, both with multiplicity one.
    It is @TEX "$L(2\\omega_1)=\\mathrm{Fr}^*L(\\omega_1)$"@; the
    zero-weight line lies in the radical and is absent from this character.
SeeAlso
  simpleCharacter
  simpleTopFactorDim
  simpleQuotient
  maximalSubmodule
  WeightFormat
///

doc ///
Key
  simpleTopFactorDim
  (simpleTopFactorDim, Matrix, ZZ, List)
  [simpleTopFactorDim, WeightFormat]
Headline
  Dimension of the simple head of a Weyl module
Usage
  d = simpleTopFactorDim(A, p, lambda)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  p:ZZ
    a prime
  lambda:List
    a dominant highest weight in the chosen coordinate format
Outputs
  d:ZZ
    the dimension of V(lambda)/rad(V(lambda)) in characteristic p
Description
  Text
    A Weyl module @TEX "$V(\\lambda)$"@ has a unique simple head
    @TEX "$L(\\lambda)$"@.  Its dimension is the sum of the
    multiplicities returned by @TO simpleTopFactorCharacter@.
    This function agrees with @TO simpleDimension@ and retains
    Doty's GAP name @TT "SimpleTopFactorDim"@.
  Example
    simpleTopFactorDim(cartanA 3, 2, {2, 0})
  Text
    The answer is three: the head of
    @TEX "$V(2\\omega_1)=D^2(k^3)$"@ in characteristic two is
    @TEX "$L(2\\omega_1)=\\mathrm{Fr}^*(k^3)$"@.  The remaining
    three dimensions form the radical @TEX "$\\Lambda^2(k^3)$"@.
SeeAlso
  simpleDimension
  simpleTopFactorCharacter
  simpleQuotient
  maximalSubmodule
  WeightFormat
///

doc ///
Key
  socleWeyl
  (socleWeyl, WeylModuleZ, ZZ)
  (socleWeyl, WeylModuleZ, ZZ, Option)
Headline
  Search for the socle of V(lambda) tensor F_p by testing maximal-vector basis elements
Usage
  s = socleWeyl(W, p)
  s = socleWeyl(W, p, RequireComplete => true)
Inputs
  W:WeylModuleZ
  p:ZZ
    a prime
Outputs
  s:SubWeylModuleFp
    the semisimple submodule found by the search; it is the full socle when
    the completeness condition below holds
Description
  Text
    The @EM "socle"@ of a module
    @TT "V"@ is its largest semisimple submodule
    @TEX "$\\mathrm{soc}(V) := \\sum L$"@ where the sum is over all
    simple submodules @TT "L"@ of @TT "V"@.  The head (or top) is
    @TEX "$\\mathrm{top}(V) := V/\\mathrm{rad}(V)$"@.  Under vector-space
    duality one has
    @TEX "$\\mathrm{soc}(V^*) \\cong \\mathrm{top}(V)^*$"@.  For Weyl modules the
    head is always the unique simple @TEX "$L(\\lambda)$"@.  When the
    Weyl module is reducible, its socle is a sum of simple modules
    @TEX "$L(\\mu)$"@ with @TEX "$\\mu < \\lambda$"@.
  Text
    @SUBSECTION "Algorithm and completeness"@
  Text
    If the highest-weight vector is the only maximal vector up to scalars,
    the Weyl module is simple and the function returns the whole module.
    Otherwise, proceed as follows.
  Text
    For each chosen maximal (singular) vector
    @TEX "$m$"@ at a weight @TEX "$\\mu$"@ below the highest weight, form the cyclic
    submodule @TEX "$U_p \\cdot m$"@.  Its simple head is
    @TEX "$L(\\mu)$"@, so this submodule is simple precisely when its
    dimension equals
    @TEX "$\\dim L(\\mu)$"@ (computed via @TO simpleCharacter@); in that
    case @TEX "$L(\\mu)$"@ embeds into @TEX "$V(\\lambda)$"@ at
    @TEX "$\\mu$"@, and it is added to the semisimple sum.
    The function tests the basis of each maximal-vector space returned
    by @TO maximalVectors@ one vector at a time.  If every relevant
    maximal-vector space is one-dimensional, or more generally if each
    simple cyclic summand is generated by a tested basis element, the result
    is the full socle.  At an ambiguous weight it does not exhaust all
    nonzero linear combinations, so the returned submodule may be properly
    contained in the socle.
  Text
    Passing @TT "RequireComplete => true"@ requires the calculation
    to establish the full socle filtration before returning its first
    term.  An ambiguous or otherwise incomplete search raises an error.
  Example
    -- A_2 V(2,0) at p=2: V is dim 6 with composition factors
    -- L(2,0) (top) and L(0,1) (socle).  Socle is dim 3.
    W = weylModuleZ(cartanA 3, {2, 0})
    soc = socleWeyl(W, 2, RequireComplete => true)
    dim soc
    pairs character soc
  Text
    The answer has dimension three and the character of
    @TEX "$L(\\omega_2)=\\Lambda^2(k^3)$"@.  Thus the socle of the
    six-dimensional divided square is its exterior-square submodule,
    and the quotient is @TEX "$L(2\\omega_1)=\\mathrm{Fr}^*(k^3)$"@.
    Each relevant maximal-vector space is one-dimensional here, so
    the search finds the full socle.
  Example
    -- A_2 V(omega_1) is irreducible mod every p, so soc = V.
    Wstd = weylModuleZ(cartanA 3, {1, 0})
    dim socleWeyl(Wstd, 2) == dim Wstd
  Text
    The equality is true: the natural three-dimensional module is
    simple, and its largest semisimple submodule is the whole module.
SeeAlso
  simpleCharacter
  subWeylModule
  socleSeries
  maximalSubmodule
  decompositionNumbers
///

doc ///
Key
  decomposeCharacter
  (decomposeCharacter, Matrix, ZZ, HashTable)
  (decomposeCharacter, WeylCharacter, ZZ)
  [decomposeCharacter, WeightFormat]
Headline
  Express a formal character as a sum of simple characters
Usage
  d = decomposeCharacter(A, p, ch)
  d = decomposeCharacter(C, p)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  p:ZZ
    a prime
  ch:HashTable
    a weight-to-multiplicity table in the nonnegative integral span of simple characters
  C:WeylCharacter
    such a formal character together with its Cartan matrix
Outputs
  d:HashTable
    dominant highest weights mapped to the coefficients of their simple characters
Description
  Text
    If @TEX "$M$"@ is a finite-dimensional rational module in
    characteristic @TEX "$p$"@, then
    @TEX "$\\mathrm{ch}(M)=\\sum_\\mu[M:L(\\mu)]\\mathrm{ch}(L(\\mu))$"@.
    This function recovers the coefficients in that expansion.
    The characters of the simple modules are linearly independent, so
    those coefficients are uniquely determined.
  Text
    Choose a maximal dominant weight @TEX "$\\mu$"@ of positive
    multiplicity @TEX "$m$"@, subtract
    @TEX "$m\\,\\mathrm{ch}(L(\\mu))$"@, and repeat.
    The simple characters are computed by @TO simpleCharacter@,
    using Steinberg's tensor product theorem.  The input must be a
    nonnegative integral sum of simple characters: arbitrary nonnegative
    weight multiplicities need not be the character of a module.
    The function raises an error if a subtraction leaves a negative
    multiplicity or a nonzero remainder without a dominant highest weight.
  Text
    For a @TO WeylCharacter@, the Cartan matrix is already part of the
    input.  The @TO WeightFormat@ option specifies the coordinates of a
    hash-table input and of the output highest weights.  The terms of
    a @TO WeylCharacter@ are stored in Dynkin coordinates; for that
    form the option changes only the output highest-weight labels.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})
    sort pairs decomposeCharacter(A, 2, character W)
  Text
    The two entries, @TT "{2,0} => 1"@ and @TT "{0,1} => 1"@,
    express @TEX "$\\mathrm{ch}(D^2(k^3))$"@ as
    @TEX "$\\mathrm{ch}(L(2\\omega_1))+\\mathrm{ch}(L(\\omega_2))$"@
    in characteristic two.  These are the characters of the Frobenius
    twist of the natural module and of its exterior square.
    This equality determines the composition multiplicities, but
    does not assert a direct-sum decomposition.
  Example
    C = weylCharacter W;
    sort pairs decomposeCharacter(C, 2)
  Text
    The result is the same two-factor table.  The character object
    carries the type @TEX "$A_2$"@ Cartan matrix, so the prime is
    the only additional datum needed to determine the simple
    characters in this expansion.
  Example
    ch = productCharacter(character weylModuleZ(cartanA 2, {1}),
                          character weylModuleZ(cartanA 2, {1}));
    sort pairs decomposeCharacter(cartanA 2, 2, ch)
  Text
    For the tensor square of the natural @TEX "$\\mathrm{SL}_2$"@-module,
    the result is @TT "{0} => 2"@ and @TT "{2} => 1"@.
    Thus its four-dimensional character is
    @TEX "$2\\,\\mathrm{ch}(L(0))+\\mathrm{ch}(L(2\\omega_1))$"@.
    The two trivial composition factors account for the zero-weight
    multiplicity two; the Frobenius twist accounts for the weights
    @TEX "$\\pm2\\omega_1$"@.
SeeAlso
  decompositionNumbers
  simpleCharacter
  productCharacter
  WeightFormat
///

doc ///
Key
  decompositionNumbers
  (decompositionNumbers, WeylModuleZ, ZZ)
  (decompositionNumbers, SubWeylModuleFp)
  (decompositionNumbers, QuotientWeylModuleFp)
  (decompositionNumbers, SubquotientWeylModuleFp)
  [decompositionNumbers, WeightFormat]
Headline
  Composition multiplicities of a module in characteristic p
Usage
  d = decompositionNumbers(W, p)
  d = decompositionNumbers M
Inputs
  W:WeylModuleZ
    an integral Weyl module
  p:ZZ
    a prime, used to reduce W
  M:
    a submodule, quotient, or subquotient over a prime field
Outputs
  d:HashTable
    the nonzero composition multiplicities, indexed by highest weights
Description
  Text
    A composition series of a finite-length rational module
    @TEX "$M$"@ has simple quotients @TEX "$L(\\mu)$"@.
    The multiplicity @TEX "$[M:L(\\mu)]$"@ is independent of the chosen
    series by the Jordan--Holder theorem.  The function returns the
    table @TEX "$\\mu\\mapsto[M:L(\\mu)]$"@ by decomposing the formal
    character of @TEX "$M$"@ into simple characters.
    For @TT "(W,p)"@ it takes
    @TEX "$M=V_{\\mathbb Z}(\\lambda)\\otimes\\mathbb F_p$"@;
    a submodule, quotient, or subquotient already specifies the prime.
  Text
    The result determines the Grothendieck-group equality
    @TEX "$[M]=\\sum_\\mu[M:L(\\mu)][L(\\mu)]$"@.
    It does not determine the submodule lattice, socle filtration, or
    whether an extension splits.  The @TO WeightFormat@ option changes
    the coordinates of the highest-weight labels.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})
    sort pairs decompositionNumbers(W, 2)
  Text
    Each of @TEX "$L(2\\omega_1)$"@ and @TEX "$L(\\omega_2)$"@ occurs
    once.  Here @TEX "$V(2\\omega_1)=D^2(k^3)$"@ fits into the
    nonsplit exact sequence
    @TEX "$0\\to\\Lambda^2(k^3)\\to D^2(k^3)\\to\\mathrm{Fr}^*(k^3)\\to0$"@.
    The decomposition table records its two factors; the location of
    these factors follows from the socle and head calculations.
  Example
    sort pairs decompositionNumbers(W, 3)
  Text
    Only @TT "{2,0} => 1"@ remains, so this Weyl module is simple in
    characteristic three.  In general, a p-restricted highest weight
    alone does not imply simplicity of its Weyl module.
  Example
    sort pairs decompositionNumbers(weylModuleZ(A, {3, 0}), 2)
  Text
    The entries @TT "{0,0} => 1"@ and @TT "{3,0} => 1"@ give
    one trivial factor and one copy of @TEX "$L(3\\omega_1)$"@.
    The latter has dimension nine by Steinberg's theorem, so the
    dimensions add to ten, the dimension of @TEX "$D^3(k^3)$"@.
    In this example the trivial factor is the socle
    @TEX "$\\Lambda^3(k^3)$"@.
  Example
    sort pairs decompositionNumbers(weylModuleZ(cartanB 2, {1, 0}), 2)
    sort pairs decompositionNumbers(weylModuleZ(cartanG2(), {1, 0}), 2)
  Text
    In both cases there is one trivial factor and one factor of the
    original highest weight.  For type @TEX "$B_2$"@ the dimensions
    are @TEX "$1+4=5$"@, and for type @TEX "$G_2$"@ they are
    @TEX "$1+6=7$"@.  These are characteristic-two reductions of
    Weyl modules that are irreducible in characteristic zero.
  Example
    sort pairs decompositionNumbers(
        weylModuleZ(cartanA 4, {2, 1, 0, 0}, WeightFormat => "Partition"),
        2, WeightFormat => "Partition")
  Text
    The labels are partitions of three with at most four parts, rather
    than Dynkin coordinates.  The partition @TEX "$(2,1,0,0)$"@
    gives the highest weight @TEX "$(1,1,0)$"@ of
    @TEX "$\\mathrm{SL}_4$"@.  The sole entry has this partition as
    its label and multiplicity one, so the Weyl module is simple in
    characteristic two.  Changing coordinates leaves its composition
    multiplicities unchanged.
SeeAlso
  decomposeCharacter
  compositionFactors
  simpleCharacter
  simpleQuotient
  WeightFormat
  symmetricGroupDecompositionMatrix
///

doc ///
Key
  maximalVectors
  (maximalVectors, WeylModuleZ, List, ZZ)
  (maximalVectors, WeylModuleZ, ZZ)
  (maximalVectors, QuotientWeylModuleFp, List)
  (maximalVectors, QuotientWeylModuleFp)
  [maximalVectors, WeightFormat]
Headline
  Maximal (singular) vectors of V(lambda) reduced mod p (or of a quotient Q)
Usage
  vecs = maximalVectors(W, mu, p)
  pairs = maximalVectors(W, p)
  vecs = maximalVectors(Q, mu)
  pairs = maximalVectors Q
Inputs
  W:WeylModuleZ
    a Weyl module @TEX "$V_{\\mathbb{Z}}(\\lambda)$"@; the quotient forms
    take a @TO QuotientWeylModuleFp@ instead and read the prime from it
  mu:List
    a dominant weight in the chosen coordinate format -- only used in
    the per-weight forms
  p:ZZ
    a prime
Outputs
  :List
    in the per-weight form, a list of coefficient lists over
    @TEX "$\\mathbb{F}_p$"@, of length @TEX "$\\dim V(\\lambda)_\\mu$"@
    for @TT "W"@ or @TEX "$\\dim Q_\\mu$"@ for @TT "Q"@; in the all-weight
    form, a list of pairs @TT "(mu, v)"@ over all dominant @TT "mu"@
Description
  Text
    A weight vector
    @TEX "$v \\in V_\\mu$"@ in a @TEX "$U_p(\\mathfrak{g})$"@-module is
    @EM "maximal"@ (or @EM "singular"@, or @EM "primitive"@) if it is
    annihilated by every positive divided-power generator:
    @TEX "$e_{\\alpha_j}^{(p^k)} v = 0$"@ for every simple root
    @TEX "$\\alpha_j$"@ and every @TT "k = 0, 1, ..."@.  When
    @TEX "$\\mu$"@ is dominant, such a vector determines a homomorphism
    @TEX "$V(\\mu)\\to M$"@ sending the highest-weight vector to @TT "v"@.
    Its cyclic image is a highest-weight quotient of @TEX "$V(\\mu)$"@;
    it need not itself be a full Weyl module or a simple module.
    In characteristic @TT "p"@ the divided powers
    @TEX "$e_{\\alpha_j}^{(p^k)}$"@ act non-trivially even when
    @TEX "$e_{\\alpha_j}$"@ does not, so testing maximality requires
    using all relevant divided powers.  The calculation stops after the
    first @TEX "$p$"@-power above the largest simple-coroot coordinate
    occurring among the weights of the ambient module, which bounds every
    simple-root string.
  Text
    Maximal vectors at a weight @TEX "$\\mu$"@
    classify highest-weight homomorphisms
    @TEX "$V(\\mu) \\to V(\\lambda) \\otimes \\mathbb{F}_p$"@: by the
    universal property of Weyl modules, every such homomorphism is uniquely
    determined by the image of @TEX "$v_\\mu$"@, which must be a
    maximal vector of weight @TEX "$\\mu$"@.  Counting independent
    maximal vectors at @TEX "$\\mu$"@ thus computes
    @TEX "$\\dim \\mathrm{Hom}_G(V(\\mu), V(\\lambda) \\otimes \\mathbb{F}_p)$"@.
    These spaces of maximal vectors are used in Doty's algorithms for
    @TO socleWeyl@, @TO simpleQuotient@, and @TO socleSeries@.
  Text
    @SUBSECTION "Algorithm"@
  Text
    Build the matrix whose rows record the action
    of @TEX "$e_{\\alpha_j}^{(p^k)}$"@ on each chosen
    @TEX "$\\mathbb{F}_p$"@-basis vector of
    @TEX "$V(\\lambda)_\\mu$"@; the kernel of this matrix is exactly
    the @TEX "$\\mathbb{F}_p$"@-space of maximal vectors of weight
    @TEX "$\\mu$"@.  For an integral Weyl module, the output lists use
    its chosen @TEX "$\\mathbb{Z}$"@-basis (see @TO weightSpace@), reduced
    modulo @TT "p"@.  For a quotient, they use the quotient basis selected
    when @TT "Q"@ was constructed, not ambient @TEX "$V_\\mu$"@ coordinates.
  Example
    -- A_1 V(2) mod 2: this is the divided square D^2 of the std sl_2 rep.
    -- The SES 0 -> L(0) -> D^2 -> L(2) -> 0 gives two maximal vectors:
    -- v_lambda (weight 2, generates the whole Weyl module; its image gives
    -- the highest-weight line of the head L(2) = F^* V) and
    -- f^(1) v_lambda (weight 0, generates the trivial socle).
    W = weylModuleZ(cartanA 2, {2})
    maximalVectors(W, 2)
  Text
    The output has one pair at weight @TEX "$2\\omega_1$"@ and one
    at weight zero.  The highest-weight vector generates the whole
    three-dimensional Weyl module; the zero-weight vector generates
    its trivial socle.  They therefore describe two independent
    highest-weight homomorphisms with different source weights,
    rather than two simple direct summands.
  Example
    -- A_2 V(2,0) = D^2 of std at p=2.  Two maximal vectors: at lambda
    -- (giving F^* V on top) and at (0,1) (generating Lambda^2 V on bottom).
    W = weylModuleZ(cartanA 3, {2, 0})
    maximalVectors(W, 2)
  Text
    There is a maximal vector at @TEX "$(2,0)$"@ and another at
    @TEX "$(0,1)$"@.  The first generates @TEX "$D^2(k^3)$"@,
    while the second generates the simple submodule
    @TEX "$\\Lambda^2(k^3)$"@.  The coefficient lists are written
    relative to the chosen bases of those two weight spaces.
  Example
    -- A_2 V(omega_1) is the standard rep, irreducible mod every prime.
    -- Only the highest-weight maximal vector exists.
    W = weylModuleZ(cartanA 3, {1, 0})
    maximalVectors(W, 2)
    Q = simpleQuotient(W, 2)
    maximalVectors Q                -- maximal vectors in the simple quotient
  Text
    Both lists contain only the highest-weight pair at
    @TEX "$\\omega_1$"@.  The natural module is already simple, so
    taking its simple quotient changes neither the module nor its
    space of maximal vectors.
SeeAlso
  isMaximalVector
  ambiguousMaxVecs
  weylModuleZ
  weightSpace
  actionE
  highestPrimePower
///

doc ///
Key
  socleLengthTwoQuotient
  (socleLengthTwoQuotient, SubWeylModuleFp)
  (socleLengthTwoQuotient, WeylModuleZ)
  (socleLengthTwoQuotient, SubWeylModuleFp, Option)
Headline
  Truncate lower socle layers to obtain a quotient of socle length at most 2
Usage
  Q = socleLengthTwoQuotient S
  Q = socleLengthTwoQuotient(S, RequireComplete => true)
Inputs
  S:SubWeylModuleFp
    a submodule of @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@
  W:WeylModuleZ
    an integral module; the function raises an error because @TT "W"@
    does not specify a characteristic
Outputs
  Q:QuotientWeylModuleFp
    a quotient @TEX "$V/S'$"@ with @TEX "$S' \\supseteq S$"@ and
    computed socle length at most two; this is the actual socle length when
    the socle search is complete
Description
  Text
    A finite-length module @TT "M"@ has
    @EM "socle length"@ equal to the smallest @TT "l"@ such that
    @TEX "$\\mathrm{soc}^l(M) = M$"@.  When @TT "l = 2"@, the two
    socle layers are semisimple: @TEX "$\\mathrm{soc}(M)$"@ and
    @TEX "$M/\\mathrm{soc}(M)$"@.  The latter is not, in general, the
    head @TEX "$M/\\mathrm{rad}(M)$"@.  If both layers are single simple
    modules, then @TT "M"@ represents an extension class between those
    two simples.
  Text
    Given a submodule @TT "S"@ of @TEX "$V \\otimes \\mathbb{F}_p$"@,
    the function constructs a larger kernel
    @TEX "$S' \\supseteq S$"@ for which the computed filtration of
    @TEX "$V/S'$"@ has length at most 2.  Algorithm: while the computed
    socle length of @TEX "$V/S$"@ is greater than 2,
    extend @TT "S"@ by the cumulative filtration term two layers below
    the top (the term containing every layer except the top two), then
    iterate.  In effect, the selected lower layers are moved into the
    kernel.  This is Doty's
    truncation procedure; the returned kernel is not claimed to be minimal
    among all kernels that give a quotient of socle length at most 2.
    The calculation uses @TO socleWeyl@, so an ambiguous
    maximal-vector space can make the computed filtration incomplete; in
    that case the calculation does not establish that the returned quotient has
    socle length at most two.
  Text
    The characteristic is essential.  Calling this method directly on a
    @TO WeylModuleZ@ raises an error.  A prime is specified by taking
    the zero submodule @TT "subWeylModule(W,p,{})"@ as input.
    Passing @TT "RequireComplete => true"@ requires every socle
    calculation used in the truncation to establish the full socle
    filtration; an incomplete search then raises an error.
  Example
    -- A_2 V(2, 0) at p = 2 already has socle length 2:
    --   bottom = L(0,1) (= Lambda^2 V),  top = L(2,0) (= F^* V).
    -- So S' = S = (0) and Q = V is already a length-2 quotient.
    W = weylModuleZ(cartanA 3, {2, 0})
    S0 = subWeylModule(W, 2, {})
    Q = socleLengthTwoQuotient S0
    dim Q
    apply(gensSocleLayers Q, layer -> apply(layer, x -> x#0))
  Text
    The quotient still has dimension six, and the layer labels are
    @TT "{{{0,1}},{{2,0}}}"@.  Its kernel is zero because the divided
    square already has socle length two:
    @TEX "$0\\to L(\\omega_2)\\to Q\\to L(2\\omega_1)\\to0$"@.
  Example
    -- A_2 V(3, 0) at p = 2 also has socle length 2 already:
    --   bottom = L(0,0) trivial, top = L(3,0).
    -- Here L(3,0) = L(1,0) tensor Fr^*L(1,0), of dimension 9.
    W = weylModuleZ(cartanA 3, {3, 0})
    S0 = subWeylModule(W, 2, {})
    Q = socleLengthTwoQuotient S0
    apply(gensSocleLayers Q, layer -> apply(layer, x -> x#0))
  Text
    The layer labels are @TT "{{{0,0}},{{3,0}}}"@, so no lower
    term needs to be removed.  The quotient is the original divided
    cube, with trivial socle and the nine-dimensional simple head
    @TEX "$L(3\\omega_1)=L(\\omega_1)\\otimes\\mathrm{Fr}^*L(\\omega_1)$"@.
SeeAlso
  twoFactorQuotientsContaining
  socleSeries
  gensSocleLayers
///

doc ///
Key
  twoFactorQuotientsContaining
  (twoFactorQuotientsContaining, SubWeylModuleFp)
  (twoFactorQuotientsContaining, SubWeylModuleFp, Option)
Headline
  Search for selected two-composition-factor quotients whose kernels contain S
Usage
  Qs = twoFactorQuotientsContaining S
  Qs = twoFactorQuotientsContaining(S, RequireComplete => true)
Inputs
  S:SubWeylModuleFp
    a submodule of @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@
Outputs
  Qs:List
    a list of candidate @TO QuotientWeylModuleFp@'s, each a quotient
    @TEX "$V/S'$"@ with @TEX "$S' \\supseteq S$"@ whose computed
    filtration has two simple layers
Description
  Text
    A module with exactly two composition factors can be either split or
    nonsplit.  The candidates retained here have computed socle length
    exactly two and a simple bottom layer.  When the socle computation is
    complete, such a quotient fits into a nonsplit sequence
    @TEX "$0\\to L(\\mu)\\to Q\\to L(\\nu)\\to0$"@ and hence a nonzero
    class in @TEX "$\\mathrm{Ext}^1_G(L(\\nu), L(\\mu))$"@.
    The function searches
    for such quotients inside @TEX "$V/S$"@.  It does not enumerate every
    two-factor quotient and does not compute or parametrize the full
    @TEX "$\\mathrm{Ext}^1$"@ group.
  Text
    @SUBSECTION "Algorithm and limitation"@
  Text
    The algorithm first applies @TO socleLengthTwoQuotient@ to
    @TT "S"@.  If the result has two socle layers and a single simple
    bottom layer, return it.  Otherwise, for each generator @TT "gen"@
    of the bottom socle layer, form
    @TT "U = subWeylModule(S, gen)"@, apply
    @TT "socleLengthTwoQuotient"@ to @TT "U"@, and retain the candidates
    that pass the same two-layer, simple-bottom test.
  Text
    The socle search has the limitation described under @TO socleWeyl@:
    at an ambiguous weight, only the chosen basis vectors are tested.
    In that case the returned quotients need not have the asserted socle
    structure, since some simple submodules may have been missed.
  Text
    With @TT "RequireComplete => true"@, every socle calculation used
    in the search must establish the full socle filtration; otherwise
    the function raises an error.
  Example
    -- A_2 V(2, 0) mod 2: V is itself a two-factor module
    -- (the SES 0 -> Lambda^2 V -> D^2 V -> F^* V -> 0).  So there
    -- the search returns V itself.
    W = weylModuleZ(cartanA 3, {2, 0})
    S0 = subWeylModule(W, 2, {})
    tfqs = twoFactorQuotientsContaining S0
    #tfqs
    Q = first tfqs;
    apply(gensSocleLayers Q, layer -> apply(layer, x -> x#0))
  Text
    The search returns one quotient, whose bottom and top labels
    are @TEX "$\\omega_2$"@ and @TEX "$2\\omega_1$"@.  In this
    case it is @TEX "$D^2(k^3)$"@ itself, with its nonsplit
    exterior-square submodule and Frobenius-twisted quotient.  The
    result gives a two-factor extension found by the search.
SeeAlso
  socleLengthTwoQuotient
  socleSeries
///

doc ///
Key
  socleSeries
  (socleSeries, WeylModuleZ, ZZ)
  (socleSeries, SubWeylModuleFp)
  (socleSeries, QuotientWeylModuleFp)
  (socleSeries, WeylModuleZ, ZZ, Option)
  (socleSeries, SubWeylModuleFp, Option)
  (socleSeries, QuotientWeylModuleFp, Option)
Headline
  Cumulative terms of the computed socle filtration
Usage
  series = socleSeries(W, p)
  series = socleSeries S
  series = socleSeries Q
  series = socleSeries(W, p, RequireComplete => true)
Inputs
  W:WeylModuleZ
    an integral Weyl module
  p:ZZ
    a prime
  S:SubWeylModuleFp
    a kernel inside an ambient Weyl module
  Q:QuotientWeylModuleFp
    a quotient with its defining kernel
Outputs
  series:List
    cumulative submodule preimages in the ambient Weyl module
Description
  Text
    The socle filtration of a finite-length module @TEX "$M$"@ is the
    ascending chain
    @TEX "$0=M_0\\subset M_1\\subset\\cdots\\subset M_\\ell=M$"@,
    defined by
    @TEX "$M_k/M_{k-1}=\\mathrm{soc}(M/M_{k-1})$"@.
    Each quotient is the largest semisimple submodule of the module
    remaining after the preceding term is removed.
    The smallest @TEX "$\\ell$"@ is the socle length.
    In particular @TEX "$M_1=\\mathrm{soc}(M)$"@.
  Text
    For a kernel @TEX "$S\\subseteq V$"@, the function concerns
    @TEX "$M=V/S$"@ and returns submodules
    @TEX "$S_1\\subset S_2\\subset\\cdots$"@ of @TEX "$V$"@,
    with @TEX "$S$"@ itself omitted.  When the search is complete,
    @TEX "$S_k/S=M_k$"@; thus the returned dimensions are dimensions
    of the preimages, and @TEX "$\\dim M_k=\\dim S_k-\\dim S$"@.
    The quotient form reads @TEX "$S$"@ from @TO definingKernel@,
    while @TT "(W,p)"@ starts with the zero kernel after reduction
    modulo @TEX "$p$"@.
  Text
    A Weyl module has a simple head @TEX "$L(\\lambda)$"@, which forms
    its final socle layer.  The preceding layers record the positions
    of its other simple constituents.  Constituents in different
    layers need not be joined by a direct extension, and the filtration
    alone does not determine the full submodule lattice.
  Text
    The search tests chosen bases of maximal-vector spaces, as described
    under @TO socleWeyl@ and @TO gensNextSocle@.
    If every relevant space is one-dimensional, it finds the true socle
    filtration.  At an ambiguous weight it can miss a simple cyclic
    submodule generated by a linear combination of the tested vectors.
    The returned chain is then a semisimple filtration found by this
    search, even if it reaches the whole module.
    Passing @TT "RequireComplete => true"@ requires the calculation to
    establish the full socle filtration and raises an error otherwise.
    This option is also accepted by the submodule and quotient forms.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    apply(socleSeries(W, 2, RequireComplete => true), dim)
  Text
    The dimensions @TT "{3,6}"@ give the chain
    @TEX "$0\\subset\\Lambda^2(k^3)\\subset D^2(k^3)$"@ in
    characteristic two.  Its first layer is @TEX "$L(\\omega_2)$"@
    and its second is @TEX "$L(2\\omega_1)=\\mathrm{Fr}^*(k^3)$"@.
    The two layers are quotients of consecutive terms, not a
    direct-sum decomposition of the divided square.
  Example
    S = socleWeyl(W, 2);
    Q = quotientWeylModule S;
    series = socleSeries(Q, RequireComplete => true);
    apply(series, dim)
    apply(series, T -> dim T - dim definingKernel Q)
  Text
    The ambient preimage has dimension six, but the corresponding
    submodule of @TEX "$Q$"@ has dimension three after subtracting
    the dimension of @TEX "$S$"@.  Thus
    @TEX "$Q=L(2\\omega_1)$"@ is simple and has a single socle layer.
  Example
    W = weylModuleZ(cartanA 3, {3, 0})
    apply(socleSeries(W, 2, RequireComplete => true), dim)
  Text
    The dimensions @TT "{1,10}"@ show that @TEX "$D^3(k^3)$"@ has
    a one-dimensional socle and a nine-dimensional simple quotient.
    The layers are @TEX "$L(0)=\\Lambda^3(k^3)$"@ and
    @TEX "$L(3\\omega_1)$"@, respectively.
SeeAlso
  gensSocleLayers
  socleWeyl
  nextSocle
  socleSeriesAndPrims
  socleAnalysis
  socleLengthTwoQuotient
  printSocleLayers
///

doc ///
Key
  gensSocleLayers
  (gensSocleLayers, WeylModuleZ, ZZ)
  (gensSocleLayers, SubWeylModuleFp)
  (gensSocleLayers, QuotientWeylModuleFp)
Headline
  Highest-weight generators in successive computed socle layers
Usage
  layers = gensSocleLayers(W, p)
  layers = gensSocleLayers S
  layers = gensSocleLayers Q
Inputs
  W:WeylModuleZ
    an integral Weyl module
  p:ZZ
    a prime
  S:SubWeylModuleFp
    a kernel inside an ambient Weyl module
  Q:QuotientWeylModuleFp
    a quotient with its defining kernel
Outputs
  layers:List
    lists of (mu, vec) generators for successive layers, in ambient weight-space coordinates
Description
  Text
    For @TEX "$V/S$"@, the function returns highest-weight generators
    of the simple submodules found successively in the remaining quotients.
    At stage @TEX "$k$"@, the vector is maximal modulo the preceding
    term and generates a simple cyclic submodule there.
    The pairs in @TT "layers#(k-1)"@ give its highest weight
    @TEX "$\\mu$"@ and a lift to the ambient weight space
    @TEX "$V_\\mu$"@.  Consequently a later-layer vector need not
    be maximal in @TEX "$V$"@ itself.
  Text
    When the search is complete, these generators give the simple
    constituents of @TEX "$\\mathrm{soc}^k(V/S)/\\mathrm{soc}^{k-1}(V/S)$"@,
    counted with multiplicity.
    The @TO SubWeylModuleFp@ form computes layers of @TEX "$V/S$"@,
    and the @TO QuotientWeylModuleFp@ form uses its defining kernel.
    The form @TT "(W,p)"@ sets @TEX "$S=0$"@ after reduction modulo
    @TEX "$p$"@.
  Text
    As under @TO gensNextSocle@, an ambiguous maximal-vector space can
    make this search incomplete.  In that case the lists describe the
    semisimple layers found by the search, which need not be the true
    socle layers.  @TO socleAnalysis@ records whether the complete
    socle filtration has been established.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    layers = gensSocleLayers(W, 2);
    apply(layers, layer -> apply(layer, x -> x#0))
    apply(layers, layer -> apply(layer, x -> x#1))
  Text
    The highest-weight lists are @TT "{{{0,1}},{{2,0}}}"@.
    They describe @TEX "$L(\\omega_2)$"@ at the bottom and
    @TEX "$L(2\\omega_1)$"@ above it in @TEX "$D^2(k^3)$"@
    in characteristic two.  The accompanying coefficient lists
    represent highest-weight generators of those layers in the
    chosen ambient weight-space bases.
  Example
    Q = simpleQuotient(W, 2);
    apply(gensSocleLayers Q, layer -> apply(layer, x -> x#0))
  Text
    Only @TT "{{{2,0}}}"@ remains.  The quotient
    @TEX "$Q=L(2\\omega_1)$"@ is simple, so its highest-weight vector
    generates its single socle layer.
SeeAlso
  socleSeries
  gensNextSocle
  maximalVectors
  weightSpace
  socleSeriesAndPrims
  socleAnalysis
///

doc ///
Key
  pbwTuples
  (pbwTuples, List, List)
Headline
  Enumerate PBW exponent tuples summing to a given root-lattice element
Usage
  T = pbwTuples(positiveRoots, delta)
Inputs
  positiveRoots:List
    list of positive roots, each a list of integers in simple-root coords
  delta:List
    a non-negative-integer combination of simple roots (in simple-root coords)
Outputs
  T:List
    the list of all tuples @TT "(c_1,...,c_N)"@ of non-negative integers
    with @TT "sum_i c_i * positiveRoots#i = delta"@
Description
  Text
    Enumerates expressions for @TT "delta"@ as nonnegative integral
    combinations of the supplied roots.  These tuples are used to construct
    the PBW monomials in @TO weylModuleZ@.
    When that list is the complete positive-root system, with each positive
    root appearing once, the cardinality is Kostant's partition number
    @TEX "$K(\\delta)$"@.
  Example
    -- type A_2 positive roots in simple-root coords:
    R = {{1,0}, {0,1}, {1,1}}
    pbwTuples(R, {1, 1})           -- two ways to write alpha_1 + alpha_2
    #(pbwTuples(R, {2, 2}))        -- K(2 alpha_1 + 2 alpha_2)
  Text
    For @TEX "$\\alpha_1+\\alpha_2$"@ the exponent tuples are
    @TEX "$(1,1,0)$"@ and @TEX "$(0,0,1)$"@: one uses the two
    simple roots, and the other uses their sum.  For
    @TEX "$2\\alpha_1+2\\alpha_2$"@ there are three expressions,
    according as the coefficient of @TEX "$\\alpha_1+\\alpha_2$"@
    is zero, one, or two.  Thus its Kostant partition number is three.
SeeAlso
  weylModuleZ
///

----------------------------------------------------------------------
-- Submodules and socle layers
----------------------------------------------------------------------

doc ///
Key
  definingKernel
  (definingKernel, QuotientWeylModuleFp)
Headline
  The submodule S used to construct a quotient V/S
Usage
  S = definingKernel Q
Inputs
  Q:QuotientWeylModuleFp
Outputs
  S:SubWeylModuleFp
    the @TEX "$U_p$"@-stable submodule such that @TT "Q = V/S"@
Description
  Text
    Returns the kernel of the natural projection
    @TEX "$V \\to V/S$"@.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    Q = simpleQuotient(W, 2)              -- L(2,0), dim 3
    K = definingKernel Q                   -- = the maximal submodule
    dim K                                   -- = 3 = dim V - dim L
    K === maximalSubmodule(W, 2)
  Text
    The kernel has dimension three, and the final equality is true.
    It is the unique maximal submodule of @TEX "$D^2(k^3)$"@,
    namely @TEX "$\\Lambda^2(k^3)$"@.  The quotient is the
    three-dimensional simple module @TEX "$L(2\\omega_1)$"@ in
    characteristic two.
SeeAlso
  quotientWeylModule
  maximalSubmodule
  simpleQuotient
///

doc ///
Key
  ambiguousMaxVecs
  (ambiguousMaxVecs, WeylModuleZ, ZZ)
  [ambiguousMaxVecs, WeightFormat]
Headline
  Maximal (singular) vectors at a weight where several appear
Usage
  out = ambiguousMaxVecs(W, p)
Inputs
  W:WeylModuleZ
  p:ZZ
    a prime
Outputs
  out:List
    a list of @TT "(mu, vec)"@ pairs giving the chosen basis vectors of
    each maximal-vector space whose dimension is greater than one
Description
  Text
    A weight @TEX "$\\mu$"@ in
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@ is called
    @EM "ambiguous"@ if @TEX "$\\dim \\{ v \\in V_\\mu : e_i^{(p^k)} v = 0 \\,\\forall i, k\\} \\geq 2$"@,
    i.e., the space of maximal (singular) vectors at @TEX "$\\mu$"@
    has dimension at least 2.  In that case there is no canonical
    choice of "the" maximal vector at @TEX "$\\mu$"@, and different
    choices can generate different cyclic submodules of @TT "V"@.
    Such phenomena underlie Doty's two-factor extensions.
  Text
    Returns a basis of each maximal-vector space of dimension greater than
    one, as @TT "(mu, v)"@ pairs ordered as in @TO maximalVectors@.
    @TO isAmbiguous@ tests whether any such weight occurs.
  Example
    -- A_3 V(2,0,0) at p=2 has maximal vectors at distinct weights, but
    -- no weight has a maximal-vector space of dimension greater than one.
    W = weylModuleZ(cartanA 4, {2, 0, 0})
    #(ambiguousMaxVecs(W, 2))
  Text
    The answer is zero.  Although this divided square has distinct
    maximal-vector weights, each such space is one-dimensional.
    Ambiguity concerns several independent maximal vectors at the
    same weight, not merely the existence of several maximal weights.
  Example
    -- G_2 V(3,0) at p=2: ambiguity at weight (2,0).
    Wg = weylModuleZ(cartanG2(), {3, 0})
    amb = ambiguousMaxVecs(Wg, 2);
    apply(amb, x -> x#0)         -- list of weights (with duplicates)
  Text
    The weight @TT "{2,0}"@ occurs more than once because its
    maximal-vector space has dimension greater than one.  Each repeated
    label accompanies a different basis vector of that space.
    Linear combinations of these vectors can generate cyclic
    submodules different from those generated by the chosen basis.
SeeAlso
  isAmbiguous
  maximalVectors
  twoFactorQuotientsContaining
  WeightFormat
///

doc ///
Key
  isAmbiguous
  (isAmbiguous, WeylModuleZ, ZZ)
Headline
  Test whether V(lambda) tensor F_p has an ambiguous weight
Usage
  isAmbiguous(W, p)
Inputs
  W:WeylModuleZ
  p:ZZ
    a prime
Outputs
  :Boolean
    true iff some weight has multiple linearly independent maximal vectors
Description
  Text
    Returns @TT "true"@ iff
    @TT "ambiguousMaxVecs(W, p)"@ is non-empty.  Ambiguity can reveal
    branching in the submodule lattice, but its absence does not imply
    that the lattice is a chain.
  Example
    not isAmbiguous(weylModuleZ(cartanA 3, {2, 0}), 2)         -- true: no ambiguity
    isAmbiguous(weylModuleZ(cartanG2(), {3, 0}), 2)             -- true: G_2 example
  Text
    Both answers are true.  The first says that the type
    @TEX "$A_2$"@ divided square has no maximal-vector space of
    dimension greater than one.  The second says that
    @TEX "$V(3\\omega_1)$"@ in type @TEX "$G_2$"@ does have such
    a space in characteristic two, at weight @TEX "$2\\omega_1$"@.
SeeAlso
  ambiguousMaxVecs
  twoFactorQuotientsContaining
  socleSeries
///

doc ///
Key
  isMaximalVector
  (isMaximalVector, WeylModuleZ, ZZ, List, List)
  [isMaximalVector, WeightFormat]
Headline
  Test whether a vector at weight mu is a maximal (singular) vector
Usage
  isMaximalVector(W, p, mu, vec)
  isMaximalVector(W, p, mu, vec, WeightFormat => "Partition")
Inputs
  W:WeylModuleZ
  p:ZZ
    a prime
  mu:List
    a weight (in the chosen format)
  vec:List
    an @TEX "$\\mathbb{F}_p$"@-coefficient list of length
    @TEX "$\\dim V(\\lambda)_\\mu$"@
Outputs
  :Boolean
Description
  Text
    A nonzero vector
    @TEX "$v \\in V(\\lambda)_\\mu \\otimes \\mathbb{F}_p$"@ is called
    @EM "maximal"@ (or "singular", or "primitive") if
    @TEX "$e_{\\alpha_i}^{(p^k)} v = 0$"@ for every simple root and every
    relevant @TEX "$p$"@-power.  The calculation uses the uniform
    simple-root-string bound described under @TO maximalVectors@.
    For dominant @TEX "$\\mu$"@, maximal vectors identify
    @TEX "$\\mathrm{Hom}_G(V(\\mu), V(\\lambda))$"@ through the universal
    property of the Weyl module.
    The zero vector returns @TT "false"@.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    -- v_lambda is maximal:
    isMaximalVector(W, 2, {2, 0}, {1})
    -- The L(0,1) generator is also maximal:
    mvecs = maximalVectors(W, 2);
    m01 = first select(mvecs, x -> x#0 == {0, 1});
    isMaximalVector(W, 2, m01#0, m01#1)
  Text
    Both tests return true.  The highest-weight vector generates the
    whole divided square, whereas the vector of weight
    @TEX "$\\omega_2$"@ generates its exterior-square socle.
    Maximality means annihilation by all positive divided powers; it
    does not mean that the resulting cyclic submodule is simple.
SeeAlso
  maximalVectors
  ambiguousMaxVecs
  WeightFormat
///

doc ///
Key
  gensNextSocle
  (gensNextSocle, SubWeylModuleFp)
Headline
  Generators of the next layer of the socle filtration
Usage
  gens = gensNextSocle S
Inputs
  S:SubWeylModuleFp
Outputs
  gens:List
    a list of @TT "(mu, vec)"@ pairs that, when added to @TT "S"@,
    extend it to the computed preimage in @TT "V"@ of the socle of
    @TT "V/S"@
Description
  Text
    Given a submodule @TT "S"@ of
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@, the true socle of
    @TEX "$V/S$"@ is its maximal semisimple submodule.  Its preimage
    in @TT "V"@ is the smallest submodule @TT "S'"@ with
    @TEX "$S \\subseteq S' \\subseteq V$"@ and @TT "S'/S = soc(V/S)"@.
    @TT "gensNextSocle"@ returns the @TT "(mu, v)"@ pairs that, taken
    together with the existing generators of @TT "S"@, produce
    @TT "S'"@ whenever the maximal-vector search is complete.
  Text
    The returned generators are lifts of the chosen maximal vectors in
    @TEX "$V/S$"@ that generate @EM "simple"@ submodules.  They are used
    by @TO socleSeries@, @TO socleLengthTwoQuotient@, and
    @TO socleSeriesAndPrims@.
  Text
    As explained under @TO socleWeyl@, the function tests the chosen
    basis vectors of an ambiguous maximal-vector space separately; it does
    not exhaust all their nonzero linear combinations.  In such a case the
    returned generators may span a proper submodule of the next socle layer.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    S0 = subWeylModule(W, 2, {})             -- the zero submodule
    g = gensNextSocle S0;                     -- generators of soc(V)
    apply(g, x -> x#0)
  Text
    The list contains @TT "{0,1}"@.  The returned lift therefore
    generates @TEX "$L(\\omega_2)=\\Lambda^2(k^3)$"@, the first socle
    layer of @TEX "$D^2(k^3)$"@ in characteristic two.
    Since the starting submodule is zero, no preceding layer must be
    factored out in this calculation.
SeeAlso
  nextSocle
  socleSeries
  gensSocleLayers
  socleSeriesAndPrims
///

doc ///
Key
  nextSocle
  (nextSocle, SubWeylModuleFp)
Headline
  Lift the next computed socle layer to the ambient module
Usage
  Snext = nextSocle S
Inputs
  S:SubWeylModuleFp
Outputs
  :SubWeylModuleFp
    the submodule generated by the computed next-layer vectors; it is the
    true next socle preimage when the search is complete, or @TT "null"@
    if no next vector is found
Description
  Text
    The computed next element in the socle filtration of @TT "V/S"@, lifted
    back into @TT "V"@.  Equivalently
    @TT "nextSocle S = S + sum_v U_p . v"@ where @TT "v"@ ranges over
    the generators returned by @TO gensNextSocle@.  It therefore inherits
    the limitation described there: at an ambiguous weight, it may be
    properly contained in the next term of the socle filtration.
  Example
    -- A_2 V(2,0) at p = 2: socle filtration is L(0,1) | L(2,0).
    W = weylModuleZ(cartanA 3, {2, 0})
    S0 = subWeylModule(W, 2, {})
    S1 = nextSocle S0;        dim S1            -- = dim soc = 3
    S2 = nextSocle S1;        dim S2            -- = dim V = 6 (full)
    nextSocle S2                                 -- null
  Text
    The dimensions three and six give
    @TEX "$S_1=\\Lambda^2(k^3)$"@ and @TEX "$S_2=D^2(k^3)$"@.
    The quotient @TEX "$S_2/S_1$"@ is the three-dimensional simple
    head @TEX "$L(2\\omega_1)$"@.  At the last call the remaining
    quotient is zero, so there is no further layer and the result
    is @TT "null"@.
SeeAlso
  gensNextSocle
  socleSeries
  socleLengthTwoQuotient
///

doc ///
Key
  printSocleLayers
  (printSocleLayers, WeylModuleZ, ZZ)
  (printSocleLayers, QuotientWeylModuleFp)
Headline
  Print the highest weights found in each computed socle layer
Usage
  printSocleLayers(W, p)
  printSocleLayers Q
Inputs
  W:WeylModuleZ
    an integral Weyl module
  p:ZZ
    a prime
  Q:QuotientWeylModuleFp
    a quotient over a prime field
Description
  Text
    Prints the highest weights of the simple constituents found in each
    socle layer, counted with multiplicity and written in Dynkin coordinates.
    The function returns @TT "null"@.  The submodules and generators are
    returned by @TO socleSeries@ and @TO gensSocleLayers@; the same
    limitation at ambiguous maximal-vector weights applies here.
  Example
    W = weylModuleZ(cartanA 3, {3, 0})
    printSocleLayers(W, 2)
    Q = simpleQuotient(W, 2)
    printSocleLayers Q
  Text
    The first display lists @TT "{0,0}"@ at the bottom and
    @TT "{3,0}"@ above it.  Thus the divided cube has trivial
    socle and simple head @TEX "$L(3\\omega_1)$"@ in characteristic
    two.  The simple quotient has only the latter layer.  Each
    printed list gives highest weights of constituents in one
    layer, counted with multiplicity.
SeeAlso
  socleSeries
  gensSocleLayers
  socleSeriesAndPrims
///

doc ///
Key
  socleSeriesAndPrims
  (socleSeriesAndPrims, SubWeylModuleFp)
  (socleSeriesAndPrims, QuotientWeylModuleFp)
  (socleSeriesAndPrims, WeylModuleZ, ZZ)
Headline
  Socle filtration and primitive vectors in each layer
Usage
  (series, prims) = socleSeriesAndPrims S
  (series, prims) = socleSeriesAndPrims Q
  (series, prims) = socleSeriesAndPrims(W, p)
Inputs
  W:WeylModuleZ
    an integral Weyl module
  p:ZZ
    a prime
  S:SubWeylModuleFp
    a kernel inside an ambient Weyl module
  Q:QuotientWeylModuleFp
    a quotient with its defining kernel
Outputs
  series:List
    the socle series, as returned by @TO socleSeries@
  prims:List
    the generators added at each layer, as returned by
    @TO gensSocleLayers@ (the "primitive vectors")
Description
  Text
    Returns @TO socleSeries@ and @TO gensSocleLayers@ in a single
    call, as in Doty's GAP @TT "SocleSeriesAndPrims"@.
    The limitation at ambiguous maximal-vector weights described under
    @TO socleSeries@ applies to both lists.
  Example
    W = weylModuleZ(cartanA 3, {2, 0})
    (series, prims) = socleSeriesAndPrims(W, 2);
    apply(series, dim)
    apply(prims, layer -> apply(layer, x -> x#0))
    Q = simpleQuotient(W, 2);
    (qSeries, qPrims) = socleSeriesAndPrims Q;
    apply(qSeries, dim)
  Text
    The first dimension list is @TT "{3,6}"@, and the primitive
    weights are @TT "{{{0,1}},{{2,0}}}"@.  These describe the
    exterior-square socle and Frobenius-twisted head of
    @TEX "$D^2(k^3)$"@ in characteristic two.  For the simple
    quotient the sole ambient preimage has dimension six; its
    dimension inside the quotient is three after subtracting the
    three-dimensional kernel.
SeeAlso
  socleSeries
  gensSocleLayers
  printSocleLayers
///

doc ///
Key
  dominantWeights
  (dominantWeights, WeylModuleZ)
  (dominantWeights, SubWeylModuleFp)
  (dominantWeights, QuotientWeylModuleFp)
  [dominantWeights, WeightFormat]
  (dominantWeights, SubquotientWeylModuleFp)
Headline
  Weights of M whose Dynkin coordinates are all non-negative
Usage
  ws = dominantWeights M
  ws = dominantWeights(M, WeightFormat => "Partition")
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@
Outputs
  ws:List
    the weights @TEX "$\\mu$"@ of @TT "M"@ that are dominant
    @TEX "$(m_i \\geq 0 \\,\\forall i)$"@
Description
  Text
    Restricts the list returned by @TO weights@ to dominant weights.
    These are the candidates for highest weights of simple constituents
    of @TEX "$V \\otimes \\mathbb{F}_p$"@.
  Example
    -- A_2 adjoint (1,1): the dominant weights are (1,1) and (0,0).
    W = weylModuleZ(cartanA 3, {1, 1})
    dominantWeights W
  Text
    The result contains @TEX "$\\omega_1+\\omega_2$"@ and zero.
    These are the dominant representatives among the roots and zero
    weight of the adjoint module.  The presence of zero as a dominant
    weight does not itself imply a trivial composition factor.
SeeAlso
  weights
  dominantWeightSpaces
  isDominantWeight
  decompositionNumbers
///

doc ///
Key
  dominantWeightSpaces
  (dominantWeightSpaces, WeylModuleZ)
  (dominantWeightSpaces, SubWeylModuleFp)
  (dominantWeightSpaces, QuotientWeylModuleFp)
  [dominantWeightSpaces, WeightFormat]
  (dominantWeightSpaces, SubquotientWeylModuleFp)
Headline
  Weight spaces at dominant weights
Usage
  H = dominantWeightSpaces M
  H = dominantWeightSpaces(M, WeightFormat => "Partition")
Inputs
  M:
    a @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@
Outputs
  H:HashTable
    dominant weights mapped to PBW-tuple lists for an integral module,
    basis matrices for a submodule, or dimensions for a quotient
Description
  Text
    Like @TO weightSpaces@, but restricted to dominant weights.  For a
    @TO WeylModuleZ@, each value is a list of chosen PBW tuples.  For a
    @TO SubWeylModuleFp@, it is a finite-field matrix whose columns give
    a basis in ambient coordinates.  For a @TO QuotientWeylModuleFp@,
    it is the dimension of the quotient weight space.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    pairs dominantWeightSpaces W
  Text
    At highest weight @TT "{1,1}"@ there is one PBW tuple,
    @TT "{0,0,0}"@, for the highest-weight vector.
    At weight zero there are two tuples, @TT "{0,0,1}"@ and
    @TT "{1,1,0}"@, so the zero-weight space has rank two.
    They correspond to the negative-root monomials
    @TEX "$f_{\\alpha_1+\\alpha_2}$"@ and
    @TEX "$f_{\\alpha_2}f_{\\alpha_1}$"@ in the package's root order.
SeeAlso
  weightSpaces
  dominantWeights
  WeightFormat
///

----------------------------------------------------------------------
-- Weight / character / partition utilities
----------------------------------------------------------------------

doc ///
Key
  isDominantWeight
  (isDominantWeight, List)
Headline
  Test whether a weight is dominant in Dynkin coordinates
Usage
  isDominantWeight wt
Inputs
  wt:List
    a weight in Dynkin (fundamental-weight) coordinates
Outputs
  :Boolean
    true iff every entry of @TT "wt"@ is a non-negative integer
Description
  Text
    A weight
    @TEX "$\\lambda \\in P$"@ is @EM "dominant integral"@ if
    @TEX "$\\langle\\lambda, \\alpha_i^\\vee\\rangle \\geq 0$"@ for every simple
    coroot @TEX "$\\alpha_i^\\vee$"@.  Equivalently, in Dynkin
    coordinates @TEX "$\\lambda = \\sum m_i \\omega_i$"@, all
    @TEX "$m_i$"@ are non-negative integers.  Inputs with rational or
    other nonintegral entries return @TT "false"@.  The dominant weights index the
    simple rational modules of the corresponding split simply connected
    semisimple group.
  Text
    To test dominance against an arbitrary Cartan in a different
    coordinate system, see @TO (isDominant, Matrix, List)@ in
    @TO KacMoody@.
  Example
    isDominantWeight {2, 1, 0}        -- true
    isDominantWeight {1, -1, 1}       -- false
    isDominantWeight {0, 0}           -- true (the zero weight)
  Text
    The first and third weights lie in the closed dominant chamber;
    the second does not, since its pairing with the second simple
    coroot is @TEX "$-1$"@.  In particular the zero weight is
    dominant and indexes the trivial highest-weight module.
SeeAlso
  isRestrictedWeight
  dominantWeights
///

doc ///
Key
  isRestrictedWeight
  (isRestrictedWeight, ZZ, List)
Headline
  Test whether a weight is p-restricted in Dynkin coordinates
Usage
  isRestrictedWeight(p, wt)
Inputs
  p:ZZ
    a prime; a nonprime input raises an error
  wt:List
    a weight in Dynkin coordinates
Outputs
  :Boolean
    true iff every Dynkin coordinate satisfies @TEX "$0 \\leq m_i < p$"@
Description
  Text
    A dominant integral weight
    @TEX "$\\lambda = \\sum m_i \\omega_i$"@ is called
    @EM "p-restricted"@ if each @TEX "$m_i$"@ is an integer satisfying
    @TEX "$0 \\leq m_i < p$"@ for every
    @TT "i"@.  The simple modules @TEX "$L(\\lambda)$"@ for
    @TEX "$\\lambda$"@ @TEX "$p$"@-restricted determine all simple
    modules through Steinberg's tensor product
    theorem
    @TEX "$L(\\lambda_0 + p \\lambda_1) \\cong L(\\lambda_0) \\otimes \\mathrm{Fr}^*(L(\\lambda_1))$"@.
  Example
    isRestrictedWeight(2, {0, 1})       -- true
    isRestrictedWeight(2, {2, 0})       -- false
    isRestrictedWeight(3, {2, 0, 1})    -- true
    isRestrictedWeight(3, {3, 0, 1})    -- false
  Text
    In characteristic two, @TEX "$\\omega_2$"@ is restricted but
    @TEX "$2\\omega_1$"@ is not.  In characteristic three,
    @TEX "$2\\omega_1+\\omega_3$"@ is restricted, whereas
    @TEX "$3\\omega_1+\\omega_3$"@ is not.  A coordinate equal to
    @TEX "$p$"@ already requires a nonzero higher digit in the
    expansion used by Steinberg's theorem.
SeeAlso
  isDominantWeight
  pRestrictedQ
  simpleCharacter
///

doc ///
Key
  highestPrimePower
  (highestPrimePower, ZZ, ZZ)
Headline
  Smallest k with p^k > n
Usage
  k = highestPrimePower(p, n)
Inputs
  p:ZZ
    a prime (@TEX "$p \\geq 2$"@)
  n:ZZ
    a non-negative integer
Outputs
  k:ZZ
    the smallest non-negative integer with @TEX "$p^k>n$"@; for
    @TEX "$n>0$"@ this is @TEX "$1 + \\lfloor \\log_p n \\rfloor$"@
Description
  Text
    Returns the smallest @TT "k"@ with @TEX "$p^k > n$"@, equal to
    @TEX "$\\lceil \\log_p (n+1) \\rceil$"@ for @TEX "$n>0$"@; the
    value at @TEX "$n=0$"@ is zero.  The modular calculations use a sharper
    root-string bound from
    the weights of the ambient module.
  Example
    highestPrimePower(2, 1)             -- 1
    highestPrimePower(2, 2)             -- 2
    highestPrimePower(2, 7)             -- 3
    highestPrimePower(3, 9)             -- 3 (since 3^2 = 9, 3^3 = 27 > 9)
  Text
    The answers @TT "1,2,3,3"@ give the first exponents beyond
    the stated bounds: @TEX "$2>1$"@, @TEX "$4>2$"@,
    @TEX "$8>7$"@, and @TEX "$27>9$"@.  The strict inequality
    matters when the bound itself is a power of the prime.
SeeAlso
  maximalVectors
///

doc ///
Key
  sortedCharacter
  (sortedCharacter, HashTable)
  (sortedCharacter, Matrix, HashTable)
  (sortedCharacter, WeylCharacter)
Headline
  Order the weights of a character
Usage
  pairs = sortedCharacter ch
  pairs = sortedCharacter(A, ch)
Inputs
  A:Matrix
    the Cartan matrix, when intrinsic root-depth order is required
  ch:
    a @TT "HashTable"@ or @TO WeylCharacter@
    a character (weight @TT "->"@ multiplicity)
Outputs
  pairs:List
    an ordered list of @TT "(weight, multiplicity)"@ pairs
Description
  Text
    A bare @TT "HashTable"@ contains no Cartan datum, so
    @TT "sortedCharacter ch"@ uses reverse lexicographic order.  Supplying
    the Cartan matrix orders weights by their depth below the highest weight,
    with weights at the same depth ordered lexicographically.  This is
    the order used by @TO WeylCharacter@.  Both forms return the same
    weight multiplicities, arranged in the specified order.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})
    sortedCharacter character W
    sortedCharacter(A, character W)
  Text
    Both lists contain the same six weights of
    @TEX "$D^2(k^3)$"@, each with multiplicity one.
    The first arranges their Dynkin coordinates in reverse
    lexicographic order.  The second begins with @TEX "$2\\omega_1$"@
    and orders the remaining weights by the sum of the simple-root
    coefficients in @TEX "$2\\omega_1-\\mu$"@, resolving equal depths
    lexicographically.
  Text
    For a @TO WeylCharacter@, its Cartan matrix supplies the root-depth
    ordering, so no separate matrix argument is needed.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    sortedCharacter C
  Text
    The first pair is the highest weight 1 with coefficient one, and the
    second is weight -1 with coefficient one. The latter lies one simple
    root below the former, since the simple root of type A1 is
    @TEX "$2\\omega_1$"@.
SeeAlso
  character
  characterDim
  productCharacter
  differenceCharacter
///

doc ///
Key
  applyChevalley
Headline
  Expand a negative simple-root generator on a PBW monomial
Usage
  out = applyChevalley(W, "f", simpleIdx, c, lambdaDynkin)
Inputs
  W:WeylModuleZ
    an integral Weyl module with its root order and
    Chevalley constants
  kind:String
    currently must be @TT "\"f\""@
  simpleIdx:ZZ
    1-based simple-root index
  c:List
    a PBW exponent tuple at some weight
  lambdaDynkin:List
    the highest weight of the ambient Verma module; this argument does not
    affect the @TT "\"f\""@ calculation
Outputs
  out:HashTable
    a hash table @TT "newC -> coefficient"@ representing the result of
    applying the Chevalley generator to the PBW monomial
Description
  Text
    Expands @TEX "$f_{\\alpha_i} \\cdot f_{\\beta_1}^{(c_1)} \\cdots f_{\\beta_N}^{(c_N)} v_\\lambda$"@
    in the PBW basis using the divided-power Serre relations
    (chain length 1 for ADE, chain length 2 for B/C/F, chain length 3
    for G_2).  The result is a hash table whose keys are new PBW
    tuples and whose values are integer coefficients.  This is the PBW
    commutation expansion before projection to the chosen Weyl-module
    quotient basis.  The module action matrix is returned by @TO actionF@.
  Text
    @TO weylModuleZ@ uses these expansions to construct the action
    matrices.  Simple roots are numbered starting at one.  Any action
    string other than @TT "\"f\""@ raises an error.
  Example
    W = weylModuleZ(cartanA 3, {1, 0})
    c = first weightSpace(W, highestWeight W)
    applyChevalley(W, "f", 1, c, highestWeight W)
    -- HashTable{{0, 1, 0} => 1}
  Text
    The zero exponent tuple represents the highest-weight vector.
    The single term @TT "{0,1,0} => 1"@ therefore says that
    @TEX "$f_{\\alpha_1}v_{\\omega_1}$"@ is the divided-power PBW
    monomial with exponent one at @TEX "$\\alpha_1$"@, with
    coefficient one.  It has weight
    @TEX "$\\omega_1-\\alpha_1=(-1,1)$"@.
SeeAlso
  weylModuleZ
  actionF
  pbwTuples
  chevalleyConstants
///

----------------------------------------------------------------------
-- Partition combinatorics (used in Schur algebra and S_n)
----------------------------------------------------------------------

doc ///
Key
  allPartitions
  (allPartitions, ZZ)
Headline
  All partitions of a non-negative integer
Usage
  ps = allPartitions n
Inputs
  n:ZZ
    a non-negative integer
Outputs
  ps:List
    a list of all partitions of @TT "n"@, in descending order
Description
  Text
    A partition of @TT "n"@ is a weakly decreasing sequence of positive
    integers whose sum is @TT "n"@.  This function lists the partitions
    with the largest parts first and omits trailing zeros.  The empty
    list is the unique partition of zero.
  Example
    allPartitions 4
    #(allPartitions 7)
  Text
    The first list contains the five partitions
    @TEX "$(4),(3,1),(2,2),(2,1,1),(1,1,1,1)$"@, hence the five
    Young diagram shapes with four boxes.
    The second answer is fifteen, the number of partitions of seven.
SeeAlso
  boundedPartitions
  conjugatePartition
  pRestrictedPartitions
  pRegularPartitions
///

doc ///
Key
  boundedPartitions
  (boundedPartitions, ZZ, ZZ)
  (boundedPartitions, ZZ, ZZ, ZZ)
Headline
  Partitions of r with at most n parts and each part at most s
Usage
  ps = boundedPartitions(n, r)
  ps = boundedPartitions(n, r, s)
Inputs
  n:ZZ
    maximum number of (zero-padded) parts
  r:ZZ
    the size being partitioned
  s:ZZ
    optional upper bound on each part (default @TT "r"@)
Outputs
  ps:List
    a list of length-@TT "n"@ tuples
    @TEX "$(\\lambda_1, \\ldots, \\lambda_n)$"@ with
    @TEX "$\\lambda_1 \\geq \\cdots \\geq \\lambda_n \\geq 0$"@,
    @TEX "$\\sum \\lambda_i = r$"@, and
    @TEX "$\\lambda_i \\leq s$"@
Description
  Text
    Every output has length exactly @TT "n"@, so shorter partitions are
    padded with zeros.  The two-argument form sets @TT "s = r"@, which
    imposes no additional bound.  Partitions with larger parts appear
    first; after trailing zeros are removed, this agrees with the order
    used by @TO allPartitions@.  @TO schurAlgebraDecompositionMatrix@
    uses this order for both its rows and its columns.
  Example
    boundedPartitions(3, 4)            -- partitions of 4 with at most 3 parts
    boundedPartitions(5, 3, 2)         -- 3 = 1+1+1 = 2+1; both have parts <= 2
  Text
    The first list is @TT "{{4,0,0},{3,1,0},{2,2,0},{2,1,1}}"@:
    the partition @TEX "$(1,1,1,1)$"@ is excluded by the bound on
    the number of parts.  The second consists of
    @TT "{2,1,0,0,0}"@ and @TT "{1,1,1,0,0}"@, since the part
    three is excluded by the bound @TEX "$s=2$"@.
SeeAlso
  allPartitions
  schurAlgebraDecompositionMatrix
///

doc ///
Key
  conjugatePartition
  (conjugatePartition, List)
Headline
  The conjugate (transpose) of a partition
Usage
  mu = conjugatePartition lambda
Inputs
  lambda:List
    a partition (weakly decreasing list of non-negative integers)
Outputs
  mu:List
    the conjugate partition: @TEX "$\\mu_i = \\#\\{ j : \\lambda_j \\geq i\\}$"@
Description
  Text
    The conjugate, or transpose, @TEX "$\\lambda'$"@ is obtained by
    reflecting the Young diagram of @TEX "$\\lambda$"@ across its
    diagonal.  Equivalently, the rows of @TEX "$\\lambda'$"@ are the
    column lengths of @TEX "$\\lambda$"@.  The output is normalized by
    removing trailing zeros, so applying the function twice recovers the
    input after its trailing zeros have been removed.  Conjugation
    exchanges @TEX "$p$"@-restricted and @TEX "$p$"@-regular
    partitions.
  Example
    conjugatePartition {4}                  -- {1, 1, 1, 1}
    conjugatePartition {2, 1, 1, 1}         -- {4, 1}
    conjugatePartition conjugatePartition {3, 2, 1}   -- back to {3, 2, 1}
  Text
    A row of four boxes becomes a column of four boxes.
    The diagram of @TEX "$(2,1,1,1)$"@ has columns of lengths four
    and one, giving @TEX "$(4,1)$"@.  The final partition
    @TEX "$(3,2,1)$"@ is self-conjugate, so the double transpose
    recovers it.
SeeAlso
  pRestrictedQ
  pRegularQ
  mullineux
///

doc ///
Key
  pRestrictedQ
  (pRestrictedQ, ZZ, List)
Headline
  Test whether a partition is p-restricted
Usage
  pRestrictedQ(p, mu)
Inputs
  p:ZZ
    an integer @TT "p >= 2"@; representation-theoretic applications
    normally take @TT "p"@ prime
  mu:List
    a partition (weakly decreasing list of non-negative integers)
Outputs
  :Boolean
Description
  Text
    A partition @TEX "$\\mu = (\\mu_1, \\ldots, \\mu_k)$"@ is
    @EM "p-restricted"@ when
    @TEX "$\\mu_i-\\mu_{i+1}<p$"@ for every @TT "i"@, with the
    convention @TEX "$\\mu_{k+1}=0$"@.  Thus the last condition is
    @TEX "$\\mu_k<p$"@.  In the p-restricted convention for symmetric
    groups, these partitions label the simple modules @TEX "$D_\\mu$"@.
    The more common p-regular labels are explained under @TO pRegularQ@
    and @TO mullineux@.
  Text
    The function checks that @TT "mu"@ is a weakly decreasing list of
    non-negative integers.  For symmetric-group applications @TT "p"@
    is prime, although the predicate itself accepts any @TT "p >= 2"@.
  Example
    pRestrictedQ(2, {2, 1, 1})               -- true
    pRestrictedQ(2, {1, 1, 1, 1})            -- true
    pRestrictedQ(2, {2, 0})                  -- false: 2 - 0 = 2 not < 2
    pRestrictedQ(3, {3, 1})                  -- true: 3 - 1 = 2 and 1 are both < 3
    pRestrictedQ(3, {4, 1})                  -- false: 4 - 1 = 3 is not < 3
  Text
    The first two partitions have successive differences zero or
    one, including the final difference to zero, so they are
    2-restricted.  The partition @TEX "$(2)$"@ fails because its
    final difference is two.  For @TEX "$p=3$"@ the difference
    @TEX "$3-1=2$"@ is allowed, whereas @TEX "$4-1=3$"@ is not.
SeeAlso
  pRegularQ
  pRestrictedPartitions
  pRegularPartitions
  isRestrictedWeight
  mullineux
///

doc ///
Key
  pRegularQ
  (pRegularQ, ZZ, List)
Headline
  Test whether a partition is p-regular
Usage
  pRegularQ(p, mu)
Inputs
  p:ZZ
    an integer @TT "p >= 2"@; representation-theoretic applications
    normally take @TT "p"@ prime
  mu:List
    a partition
Outputs
  :Boolean
Description
  Text
    A partition is @EM "p-regular"@ if no positive part occurs @TT "p"@
    or more times.  Equivalently, its conjugate is p-restricted.  For a
    p-regular partition @TEX "$\\rho$"@, the ordinary Specht module
    @TEX "$S^\\rho$"@ has simple head @TEX "$D^\\rho$"@, and the
    modules @TEX "$D^\\rho$"@ give the usual p-regular labelling of the
    simple @TEX "$\\mathbb{F}_p S_n$"@-modules.  The p-restricted labels
    used by the Schur-algebra functions are related to these through the
    Mullineux map; see @TO mullineux@.
  Text
    The input must be a partition; this is checked by @TO conjugatePartition@.
    For symmetric-group applications @TT "p"@ is prime, although the
    predicate itself accepts any @TT "p >= 2"@.
  Example
    pRegularQ(2, {3, 2})              -- true: parts (3, 2) appear once each
    pRegularQ(2, {3, 1, 1})           -- false: 1 appears twice
    pRegularQ(3, {2, 2, 1})           -- true (each part has multiplicity < 3)
    pRegularQ(3, {1, 1, 1})           -- false (1 appears 3 times)
  Text
    Distinct parts make @TEX "$(3,2)$"@ 2-regular, but the repeated
    part one makes @TEX "$(3,1,1)$"@ fail.  In characteristic
    three a part may occur twice, as in @TEX "$(2,2,1)$"@, but
    not three times, as in @TEX "$(1,1,1)$"@.
SeeAlso
  pRestrictedQ
  pRegularPartitions
  conjugatePartition
  mullineux
///

doc ///
Key
  pRegularPartitions
  (pRegularPartitions, ZZ, ZZ)
Headline
  All p-regular partitions of n
Usage
  ps = pRegularPartitions(p, n)
Inputs
  p:ZZ
    a prime
  n:ZZ
    a non-negative integer
Outputs
  ps:List
    the list of all @TT "p"@-regular partitions of @TT "n"@
Description
  Text
    This is the filtered list
    @TT "select(allPartitions n, mu -> pRegularQ(p, mu))"@, so its order
    is inherited from @TO allPartitions@.  For @TT "n = 0"@ the result
    is the singleton list containing the empty partition.
  Example
    pRegularPartitions(2, 5)
    -- The three 2-regular partitions of 5 are {5}, {4,1}, and {3,2}.
  Text
    The result is @TT "{{5},{4,1},{3,2}}"@.
    For @TEX "$p=2$"@ regularity means that every part is distinct,
    so these are exactly the partitions of five that index simple
    modules in the usual 2-regular convention.
SeeAlso
  pRestrictedPartitions
  pRegularQ
  conjugatePartition
  mullineux
///

doc ///
Key
  weightToComposition
  (weightToComposition, ZZ, List)
Headline
  Inverse of compositionToWeight at a given degree
Usage
  comp = weightToComposition(r, wt)
Inputs
  r:ZZ
    the desired total degree (sum of parts of the composition)
  wt:List
    a weight in Dynkin coordinates of length @TT "n - 1"@
Outputs
  comp:List
    a length-@TT "n"@ composition of @TT "r"@ whose consecutive differences
    are @TT "wt"@; or @TT "null"@ if no such composition exists
Description
  Text
    Given a Dynkin weight @TEX "$(m_1, \\ldots, m_{n-1})$"@ of
    @TEX "$\\mathfrak{sl}_n$"@ and a degree @TEX "$r$"@,
    returns the unique non-negative composition
    @TEX "$(\\lambda_1, \\ldots, \\lambda_n)$"@ with
    @TEX "$\\lambda_i - \\lambda_{i+1} = m_i$"@ and
    @TEX "$\\sum \\lambda_i = r$"@, when one exists.  Set
    @TEX "$s_i=m_i+m_{i+1}+\\cdots+m_{n-1}$"@ for @TEX "$i<n$"@ and
    @TEX "$s_n=0$"@.  The only possible answer is
    @TEX "$\\lambda_i=s_i+c$"@, where
    @TEX "$c=(r-\\sum_i s_i)/n$"@.  The function returns this list when
    @TEX "$c$"@ is an integer and every entry is non-negative; otherwise
    it returns @TT "null"@.  In particular, the integrality condition is
    @TEX "$r \\equiv m_1+2m_2+\\cdots+(n-1)m_{n-1} \\pmod n$"@.  The
    result is a partition precisely when @TT "wt"@ is dominant.
  Example
    -- Lift Dynkin weight (-1, 2, -1) of sl_4 to a degree-4 composition.
    weightToComposition(4, {-1, 2, -1})
    weightToComposition(8, {-1, 2, -1})
    -- Not always solvable in a given degree:
    weightToComposition(6, {-1, 2, -1}) === null
  Text
    The two compositions are @TT "{1,2,0,1}"@ and
    @TT "{2,3,1,2}"@.  Their consecutive differences are both
    @TT "{-1,2,-1}"@; adding one to every part raises the degree
    by four and leaves the @TEX "$\\mathrm{SL}_4$"@ weight unchanged.
    Degree six fails the required congruence modulo four, so the
    final test is true.
SeeAlso
  compositionToWeight
///


----------------------------------------------------------------------
-- Character utilities (Doty's Chapter 1)
----------------------------------------------------------------------

doc ///
Key
  characterDim
  (characterDim, HashTable)
  (characterDim, WeylCharacter)
Headline
  Total dimension carried by a character
Usage
  d = characterDim ch
Inputs
  ch:
    a @TT "HashTable"@ or @TO WeylCharacter@
    a character (weight @TT "->"@ multiplicity)
Outputs
  d:ZZ
    @TEX "$\\sum_\\mu \\mathrm{ch}(\\mu)$"@, the total dimension
Description
  Text
    The character of a finite-length
    @TEX "$G$"@-module @TT "M"@ is the formal sum
    @TEX "$\\mathrm{ch}(M) = \\sum_\\mu \\dim M_\\mu \\cdot e^\\mu$"@,
    and @TT "characterDim"@ recovers @TEX "$\\dim M = \\sum_\\mu \\dim M_\\mu$"@
    by summing the multiplicities, equivalently by specializing every
    formal exponential to 1.
  Example
    -- Adjoint of sl_3 is 8-dimensional; characterDim verifies this.
    W = weylModuleZ(cartanA 3, {1, 1})
    characterDim character W
  Text
    The answer is eight: the adjoint module has six nonzero
    root weights, each with multiplicity one, and a zero-weight
    space of dimension two.  Summing these multiplicities gives
    @TEX "$6+2=8$"@.
  Text
    A @TO WeylCharacter@ is also accepted. For a virtual character this
    specialization is a signed integer and need not be the dimension of
    an actual module.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    T = weylCharacter weylModuleZ(cartanA 2, {0})
    characterDim(C - 3*T)
  Text
    The result is -1: the coefficients sum to @TEX "$2-3=-1$"@.
    This is the virtual dimension of the natural character minus three
    trivial characters, not a negative-dimensional module.
SeeAlso
  character
  productCharacter
  differenceCharacter
  sortedCharacter
///

doc ///
Key
  productCharacter
  (productCharacter, HashTable, HashTable)
  (productCharacter, WeylCharacter, WeylCharacter)
Headline
  Tensor-product character of two G-modules
Usage
  ch = productCharacter(ch1, ch2)
Inputs
  ch1:
    a character as a @TT "HashTable"@ or @TO WeylCharacter@
  ch2:
    a character of the same kind and in the same weight coordinates
Outputs
  ch:
    a @TT "HashTable"@ or @TO WeylCharacter@, matching the inputs
    the character of @TEX "$M_1 \\otimes M_2$"@ given the characters of @TT "M_1, M_2"@
Description
  Text
    For
    @TEX "$M_1, M_2$"@ rational @TEX "$G$"@-modules, the character of
    @TEX "$M_1 \\otimes M_2$"@ is the convolution of characters:
    @TEX "$\\mathrm{ch}(M_1 \\otimes M_2)(\\nu) = \\sum_{\\mu_1 + \\mu_2 = \\nu} \\mathrm{ch}(M_1)(\\mu_1) \\mathrm{ch}(M_2)(\\mu_2)$"@.
    This is also the character multiplication used in Steinberg's
    tensor-product theorem; see @TO simpleCharacter@.
  Example
    -- At the character (equivalently, K_0) level,
    -- ch(V tensor V) = ch(Gamma^2 V) + ch(Lambda^2 V).
    -- This does not assert a direct-sum decomposition in characteristic 2.
    A = cartanA 3
    chV = character weylModuleZ(A, {1, 0})
    chTensor = productCharacter(chV, chV)
    -- Total dimension is 9 (= 3 * 3):
    characterDim chTensor
  Text
    The tensor-product weight multiplicities give total dimension
    nine, the product @TEX "$3\\cdot3$"@.  Weights are added in
    forming the tensor product, so a repeated sum contributes the
    product of its two multiplicities to the corresponding weight
    space.  The resulting character equals
    @TEX "$\\mathrm{ch}(D^2(k^3))+\\mathrm{ch}(\\Lambda^2(k^3))$"@;
    that equality holds also in characteristic two, where it does
    not assert a splitting of the tensor square.
  Text
    With two @TO WeylCharacter@ arguments, return a @TO WeylCharacter@
    carrying the same convolution and the common Cartan matrix.
    It agrees with @TO tensorProductCharacter@.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    P = productCharacter(C, C)
    characterDim P
  Text
    The character has weights 2 and -2 with coefficient one and weight
    zero with coefficient two. Its total dimension is 4, the dimension of
    the tensor square of the two-dimensional natural module.
SeeAlso
  character
  differenceCharacter
  characterDim
///

doc ///
Key
  differenceCharacter
  (differenceCharacter, HashTable, HashTable)
Headline
  Subtract two weight-multiplicity tables, returning null on a negative entry
Usage
  ch = differenceCharacter(ch1, ch2)
Inputs
  ch1:HashTable
  ch2:HashTable
Outputs
  ch:HashTable
    the difference @TT "ch1 - ch2"@, or @TT "null"@ if some entry would be negative
Description
  Text
    If @TEX "$N \\subseteq M$"@ is already known, then
    the character of @TEX "$M/N$"@ is @TEX "$\\mathrm{ch}(M) - \\mathrm{ch}(N)$"@
    (subtract weight by weight).  The function returns a coefficientwise
    nonnegative weight table exactly when no difference is negative.
    Nonnegativity by itself does not prove Weyl invariance, realizability as
    the character of a rational module, or the existence of a submodule
    with character @TT "ch2"@.  A negative entry does prove that such a
    submodule cannot exist.
  Example
    -- For V(2,0) of sl_3: V has char ch_V; soc has char ch_socle;
    -- the head V/socle has char ch_V - ch_socle.
    W = weylModuleZ(cartanA 3, {2, 0})
    chV = character W
    soc = socleWeyl(W, 2)
    chSoc = character soc
    chHead = differenceCharacter(chV, chSoc)
    characterDim chHead                     -- = 3 = dim L(2,0)
  Text
    The difference has the character of
    @TEX "$L(2\\omega_1)=\\mathrm{Fr}^*(k^3)$"@, with the three
    weights @TEX "$(2,0),(-2,2),(0,-2)$"@ of multiplicity one.
    Its dimension is therefore three.
    Here subtraction has a module-theoretic meaning because
    @TEX "$\\Lambda^2(k^3)$"@ was constructed as an actual
    submodule of @TEX "$D^2(k^3)$"@.
SeeAlso
  character
  productCharacter
  characterDim
///

doc ///
Key
  characterAsPartition
  (characterAsPartition, Matrix, HashTable)
  (characterAsPartition, WeylModuleZ, HashTable)
  (characterAsPartition, SubWeylModuleFp, HashTable)
  (characterAsPartition, QuotientWeylModuleFp, HashTable)
  (characterAsPartition, SubquotientWeylModuleFp, HashTable)
Headline
  Express a character in partition coordinates
Usage
  ch' = characterAsPartition(M, ch)
Inputs
  M:
    a Cartan @TT "Matrix"@, @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@ -- providing a connected classical Cartan
    matrix of type A, B, C, or D
  ch:HashTable
    a character with Dynkin-coordinate keys
Outputs
  ch':HashTable
    the same character with its keys written in epsilon-basis coordinates
Description
  Text
    Each Dynkin-coordinate key is converted with @TO KacMoody@'s
    @TT "fundamentalToPartition"@.  In types A and C a dominant integral
    weight gives an ordinary partition; type-B spin weights may be
    half-integral, and type-D dominant weights satisfy the signed condition
    @TEX "$\\lambda_{n-1}\\geq|\\lambda_n|$"@.  A general weight may
    produce an epsilon-coordinate list that is not weakly decreasing.  In
    type A, the representative with last coordinate zero is used.  In types
    B, C, and D the change of basis is determined by the supplied Cartan
    datum, including a simultaneous permutation of its nodes.  See
    @TO WeightFormat@ for the type-by-type formulas.
  Example
    A = cartanA 3
    W = weylModuleZ(A, {2, 0})            -- = D^2 V for sl_3
    ch = character W                       -- Dynkin keys
    pairs ch
    pairs characterAsPartition(W, ch)      -- partition keys
  Text
    Each displayed coefficient is unchanged.  For example the
    highest weight @TEX "$(2,0)$"@ becomes @TEX "$(2,0,0)$"@,
    while the weight @TEX "$(-1,0)$"@ becomes
    @TEX "$(-1,0,0)$"@.  These are epsilon-coordinate representatives
    of the same @TEX "$\\mathrm{SL}_3$"@ weights, with last entry
    zero; they need not be partitions or have total degree two.
SeeAlso
  characterAsDynkin
  weightAsPartition
  weightAsDynkin
  WeightFormat
///

doc ///
Key
  characterAsDynkin
  (characterAsDynkin, Matrix, HashTable)
  (characterAsDynkin, WeylModuleZ, HashTable)
  (characterAsDynkin, SubWeylModuleFp, HashTable)
  (characterAsDynkin, QuotientWeylModuleFp, HashTable)
  (characterAsDynkin, SubquotientWeylModuleFp, HashTable)
Headline
  Express a character in Dynkin coordinates
Usage
  ch' = characterAsDynkin(M, ch)
Inputs
  M:
    a Cartan @TT "Matrix"@, @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@ whose Cartan matrix has connected classical
    type A, B, C, or D
  ch:HashTable
    a character with partition-coordinate keys
Outputs
  ch':HashTable
    the character with its keys converted to Dynkin coordinates
Description
  Text
    This converts the weights from epsilon coordinates to Dynkin
    coordinates.  It is defined for arbitrary weight-lattice
    elements, so the keys need not be dominant, nonnegative, or ordered.  In
    types B, C, and D the change of basis is recovered from the supplied
    Cartan datum and therefore respects simultaneous permutations of its
    rows and columns.  It is an inverse of @TO characterAsPartition@ on
    characters produced by that function.
    In type A, lists that differ by a multiple of
    @TT "{1,1,...,1}"@ represent the same Dynkin weight.  When several
    input keys therefore convert to the same Dynkin key, their
    multiplicities are added.
  Example
    A = cartanA 3
    -- Build a character with canonical epsilon-coordinate keys.
    chPart = new HashTable from {{2, 0, 0} => 1, {1, 1, 0} => 1, {0, 0, 0} => 2}
    pairs characterAsDynkin(A, chPart)    -- now keyed by Dynkin
  Text
    The three terms have Dynkin weights @TEX "$(2,0)$"@,
    @TEX "$(0,1)$"@, and @TEX "$(0,0)$"@, with respective
    coefficients one, one, and two.  This is a change of
    coordinates in a formal weight table; the individual weights
    are not asserted to be highest weights of composition factors.
  Example
    -- These two type-A keys differ by the determinant direction, so
    -- both become {1,0}; their multiplicities are added.
    chShifted = new HashTable from {{1, 0, 0} => 2, {2, 1, 1} => 3}
    pairs characterAsDynkin(A, chShifted)
  Text
    The answer is @TT "{1,0} => 5"@.  The two input epsilon
    weights differ by @TEX "$(1,1,1)$"@, the determinant direction,
    which restricts to zero on @TEX "$\\mathrm{SL}_3$"@.
    They therefore contribute to the same weight with combined
    multiplicity @TEX "$2+3=5$"@.
SeeAlso
  characterAsPartition
  weightAsDynkin
  weightAsPartition
  WeightFormat
///

doc ///
Key
  weightAsPartition
  (weightAsPartition, Matrix, List)
  (weightAsPartition, WeylModuleZ, List)
  (weightAsPartition, SubWeylModuleFp, List)
  (weightAsPartition, QuotientWeylModuleFp, List)
  (weightAsPartition, SubquotientWeylModuleFp, List)
Headline
  Convert a single weight from Dynkin to partition coordinates
Usage
  lambda = weightAsPartition(M, mu)
Inputs
  M:
    a Cartan @TT "Matrix"@, @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@ whose Cartan matrix has connected classical
    type A, B, C, or D
  mu:List
    a weight in Dynkin (fundamental-weight) coordinates
Outputs
  lambda:List
    the epsilon-coordinate form of @TT "mu"@; for type A this has length
    one more than @TT "#mu"@, while for B/C/D the length is unchanged
Description
  Text
    For @TEX "$\\mathrm{sl}_n$"@, a Dynkin weight
    @TEX "$(m_1, \\ldots, m_{n-1})$"@ is represented by
    @TEX "$(\\lambda_1, \\ldots, \\lambda_n)$"@ with
    @TEX "$\\lambda_i-\\lambda_{i+1}=m_i$"@ and
    @TEX "$\\lambda_n=0$"@.  This representative is a partition only
    when the weight is dominant.  Types B, C, and D use their standard
    epsilon-coordinate formulas; spin weights in types B and D may have
    half-integer entries.  The change of basis is read from the supplied
    Cartan matrix, so it respects simultaneous node permutations.  See
    @TO WeightFormat@ for the formulas in each type.
  Example
    -- sl_3 standard rep: Dynkin (1,0) <-> partition (1,0,0) = epsilon_1
    weightAsPartition(cartanA 3, {1, 0})
    -- sl_4 hook V(2,1,0) <-> partition (3,1,0,0)
    weightAsPartition(cartanA 4, {2, 1, 0})
  Text
    The outputs @TT "{1,0,0}"@ and @TT "{3,1,0,0}"@ have
    consecutive differences @TT "{1,0}"@ and @TT "{2,1,0}"@.
    They represent the fundamental weight @TEX "$\\omega_1$"@
    for @TEX "$\\mathrm{SL}_3$"@ and
    @TEX "$2\\omega_1+\\omega_2$"@ for @TEX "$\\mathrm{SL}_4$"@.
  Example
    -- so(5) spin: Dynkin (0,1) <-> partition (1/2, 1/2)
    weightAsPartition(cartanB 2, {0, 1})
    -- so(8) half-spin: Dynkin (0,0,1,0) <-> (1/2, 1/2, 1/2, -1/2)
    weightAsPartition(cartanD 4, {0, 0, 1, 0})
  Text
    The outputs are @TEX "$(1/2,1/2)$"@ and
    @TEX "$(1/2,1/2,1/2,-1/2)$"@.  They are the spin highest weight
    in type @TEX "$B_2$"@ and one of the half-spin highest weights
    in type @TEX "$D_4$"@.  Their half-integral epsilon coordinates
    are integral weights of the simply connected groups.
SeeAlso
  weightAsDynkin
  characterAsPartition
  WeightFormat
///

doc ///
Key
  weightAsDynkin
  (weightAsDynkin, Matrix, List)
  (weightAsDynkin, WeylModuleZ, List)
  (weightAsDynkin, SubWeylModuleFp, List)
  (weightAsDynkin, QuotientWeylModuleFp, List)
  (weightAsDynkin, SubquotientWeylModuleFp, List)
Headline
  Convert a single weight from partition to Dynkin coordinates
Usage
  mu = weightAsDynkin(M, lambda)
Inputs
  M:
    a Cartan @TT "Matrix"@, @TO WeylModuleZ@, @TO SubWeylModuleFp@, or
    @TO QuotientWeylModuleFp@ whose Cartan matrix has connected classical
    type A, B, C, or D
  lambda:List
    a weight in partition / epsilon-basis coordinates
Outputs
  mu:List
    the Dynkin-coordinate form of @TT "lambda"@; length is rank
Description
  Text
    This is a left inverse to @TO weightAsPartition@: converting a
    Dynkin weight to epsilon coordinates and back recovers the original
    weight.  In type A the reverse composition returns the canonical
    representative with last coordinate zero, since adding a constant
    to every epsilon coordinate does not change the Dynkin weight.  For
    types B, C, and D the conversion applies to every integral weight,
    including nondominant epsilon-coordinate lists, and is computed from the
    supplied Cartan matrix; simultaneous row-and-column permutations are
    therefore respected.  See @TO WeightFormat@ for the type-by-type formulas.
  Example
    -- Type A: differences between consecutive partition entries.
    weightAsDynkin(cartanA 4, {3, 1, 0, 0})         -- {2, 1, 0}
    -- General epsilon-coordinate weights need not be dominant.
    weightAsDynkin(cartanA 3, {0, 1, 0})             -- {-1, 1}
    -- Type B spin weight: epsilon coordinates (1/2, 1/2) -> Dynkin (0, 1).
    weightAsDynkin(cartanB 2, {1/2, 1/2})
    -- Type C standard partition (1, 0, 0) -> Dynkin (1, 0, 0).
    weightAsDynkin(cartanC 3, {1, 0, 0})
  Text
    The outputs are @TT "{2,1,0}"@, @TT "{-1,1}"@,
    @TT "{0,1}"@, and @TT "{1,0,0}"@.
    They record pairings with the simple coroots: the second weight
    lies outside the dominant chamber, the third is the type
    @TEX "$B_2$"@ spin weight, and the fourth is the highest weight
    of the natural type @TEX "$C_3$"@ module.
SeeAlso
  weightAsPartition
  characterAsDynkin
  WeightFormat
///

----------------------------------------------------------------------
-- Submodule and quotient operations
----------------------------------------------------------------------

doc ///
Key
  maximalSubmodule
  (maximalSubmodule, WeylModuleZ, ZZ)
Headline
  The unique maximal proper submodule of V(lambda) tensor F_p
Usage
  K = maximalSubmodule(W, p)
Inputs
  W:WeylModuleZ
  p:ZZ
    a prime
Outputs
  K:SubWeylModuleFp
    the radical @TEX "$\\mathrm{rad}(V(\\lambda) \\otimes \\mathbb{F}_p)$"@
Description
  Text
    The radical
    @TEX "$\\mathrm{rad}(M)$"@ of a module is the intersection of all
    maximal proper submodules; equivalently, it is the smallest
    submodule whose quotient is semisimple.  For a Weyl module
    @TEX "$V(\\lambda) \\otimes \\mathbb{F}_p$"@, the head
    @TEX "$V/\\mathrm{rad}$"@ is the simple module
    @TEX "$L(\\lambda)$"@, so @TEX "$\\mathrm{rad}(V(\\lambda))$"@ is
    the unique maximal proper submodule.
  Text
    By construction, @TT "maximalSubmodule(W, p) === definingKernel(simpleQuotient(W, p))"@.
  Example
    -- A_2 V(2,0) = D^2 V at p = 2: the SES 0 -> Lambda^2 V -> D^2 V -> F^* V -> 0
    -- shows rad = Lambda^2 V = L(0,1), dim 3.
    W = weylModuleZ(cartanA 3, {2, 0})
    K = maximalSubmodule(W, 2)
    dim K
    -- Identify the radical with the socle here (V has length 2):
    K === socleWeyl(W, 2)
  Text
    The radical has dimension three and agrees with the socle in
    this example.  It is @TEX "$\\Lambda^2(k^3)=L(\\omega_2)$"@
    inside @TEX "$D^2(k^3)$"@.  The quotient by it is the simple
    three-dimensional Frobenius twist @TEX "$L(2\\omega_1)$"@;
    the divided square is a nonsplit extension of these two modules.
  Example
    -- Irreducible Weyl modules have rad = 0.
    Wstd = weylModuleZ(cartanA 3, {1, 0})
    dim maximalSubmodule(Wstd, 2)            -- 0
  Text
    The answer is zero.  The natural module is simple in
    characteristic two, so it has no nonzero proper submodule and
    its radical vanishes.
SeeAlso
  simpleQuotient
  definingKernel
  socleWeyl
///

----------------------------------------------------------------------
-- Partition combinatorics (Doty's Chapter 4)
----------------------------------------------------------------------

doc ///
Key
  pRestrictedPartitions
  (pRestrictedPartitions, ZZ, ZZ)
Headline
  All p-restricted partitions of n
Usage
  ps = pRestrictedPartitions(p, n)
Inputs
  p:ZZ
    a prime
  n:ZZ
    a non-negative integer
Outputs
  ps:List
    the @TT "p"@-restricted partitions of @TT "n"@, in the order
    inherited from @TO allPartitions@
Description
  Text
    A partition is p-restricted when every consecutive difference is
    less than @TT "p"@, including the difference from its final part to
    zero; see @TO pRestrictedQ@.  In the convention used by the
    symmetric-group functions, a p-restricted partition @TEX "$\\mu$"@
    labels the simple module @TEX "$D_\\mu$"@.
  Example
    pRestrictedPartitions(2, 5)        -- p = 2 restricted partitions of 5
    pRestrictedPartitions(3, 6)        -- p = 3 restricted partitions of 6
  Text
    For @TEX "$p=2$"@ the labels are
    @TEX "$(2,2,1),(2,1,1,1),(1,1,1,1,1)$"@.
    For @TEX "$p=3$"@ the seven partitions of six are
    @TEX "$(4,2),(3,2,1),(3,1,1,1),(2,2,2),(2,2,1,1),(2,1,1,1,1),(1,1,1,1,1,1)$"@.
    Each list consists of the partitions with every successive
    difference, including the last difference to zero, smaller than
    the given prime.  These are the column labels used by the
    symmetric-group decomposition functions.
SeeAlso
  pRegularPartitions
  pRestrictedQ
  symmetricGroupDecompositionMatrix
  mullineux
///

doc ///
Key
  mullineux
  (mullineux, ZZ, List)
Headline
  The Mullineux involution on p-regular partitions
Usage
  nu = mullineux(p, mu)
Inputs
  p:ZZ
    a prime
  mu:List
    a @TT "p"@-regular partition
Outputs
  nu:List
    the @EM "p-Mullineux"@ image of @TT "mu"@
Description
  Text
    The Mullineux involution @TEX "$\\mathrm{m}_p$"@ is the bijection on
    p-regular partitions characterized by
    @TEX "$D^{\\mathrm{m}_p(\\mu)} \\cong D^\\mu \\otimes \\mathrm{sgn}$"@,
    where @TEX "$D^\\mu$"@ is the simple
    @TEX "$\\mathbb{F}_p S_n$"@-module labeled by @TT "mu"@ and
    @TEX "$\\mathrm{sgn}$"@ is the sign representation.  At
    @TT "p = 2"@ the sign representation is trivial, so the involution
    is the identity.  When @TT "p > n"@ the group algebra is semisimple
    and the Mullineux map is ordinary conjugation, but these maps differ
    in general.
  Text
    The map also translates between the two standard labellings of
    simple symmetric-group modules.  If @TEX "$\\nu$"@ is p-restricted,
    then its p-restricted label and the usual p-regular label satisfy
    @TEX "$D_\\nu \\cong D^{\\mathrm{m}_p(\\nu')}$"@.
  Example
    mullineux(2, {3})              -- {3}: p=2 is identity
    mullineux(3, {3})              -- {2, 1}
    mullineux(3, {2, 1})           -- {3}: involution
    mullineux(5, {4, 1})           -- {3, 1, 1}
  Text
    In characteristic two the sign twist leaves the simple module
    labelled @TEX "$(3)$"@ unchanged.  In characteristic three it
    exchanges the labels @TEX "$(3)$"@ and @TEX "$(2,1)$"@,
    giving the trivial and sign modules of @TEX "$S_3$"@.
    The last output is @TEX "$(3,1,1)$"@, so the sign twist of
    @TEX "$D^{(4,1)}$"@ in characteristic five is
    @TEX "$D^{(3,1,1)}$"@.
SeeAlso
  pRegularPartitions
  pRegularQ
  conjugatePartition
  symmetricGroupDecompositionMatrix
///

----------------------------------------------------------------------
-- Schur algebra and symmetric-group representations (Doty's Chapter 4)
----------------------------------------------------------------------

doc ///
Key
  schurAlgebraWeylModule
  (schurAlgebraWeylModule, List)
  (schurAlgebraWeylModule, ZZ, List)
Headline
  Build the type-A Weyl module attached to a Schur-algebra partition
Usage
  W = schurAlgebraWeylModule mu
  W = schurAlgebraWeylModule(p, mu)
Inputs
  p:ZZ
    when supplied, a prime retained for compatibility with Doty's constructor
  mu:List
    a non-empty partition; its length, including trailing zeros,
    determines @TT "n"@ (a one-part partition is padded to length two)
Outputs
  W:WeylModuleZ
    the Weyl module @TEX "$V_\\mathbb{Z}(\\lambda)$"@ with
    @TEX "$\\lambda$"@ the Dynkin label corresponding to the
    partition @TT "mu"@
Description
  Text
    Polynomial representations of @TEX "$\\mathrm{GL}_n$"@ of
    homogeneous degree @TT "r"@ are modules for the Schur algebra
    @TEX "$S(n, r) = \\mathrm{End}_{\\mathbb{F}_p S_r}(V^{\\otimes r})$"@,
    where @TEX "$V$"@ is the natural @TEX "$n$"@-dimensional module.
    A partition @TEX "$\\mu$"@ of @TT "r"@ with at most @TT "n"@
    parts determines a Schur-algebra Weyl module.  After restriction to
    @TEX "$\\mathrm{SL}_n$"@, its highest weight is the consecutive-
    differences tuple
    @TEX "$(\\mu_1-\\mu_2,\\ldots,\\mu_{n-1}-\\mu_n)$"@.
  Text
    This function returns that integral @TEX "$\\mathrm{SL}_n$"@ Weyl
    module as a @TO WeylModuleZ@; it does not return a separate
    characteristic-p Schur-algebra module.  The one-argument form therefore
    suffices to construct the module.  In the two-argument form
    @TT "(p,mu)"@, @TT "p"@ is checked to be prime but does not change
    the returned module.
    Trailing zeros in @TT "mu"@ are significant because they choose @TT "n"@.
  Example
    -- Padding (2,1) to length 3 chooses sl_3 and gives V(1,1), dimension 8.
    W = schurAlgebraWeylModule {2, 1, 0}
    dim W
    highestWeight W                     -- {1, 1} = (mu_1 - mu_2, mu_2 - mu_3)
  Text
    The module has dimension eight and highest weight
    @TEX "$\\omega_1+\\omega_2$"@.  It is the restriction to
    @TEX "$\\mathrm{SL}_3$"@ of the degree-three Weyl functor
    @TEX "$K_{(2,1)}(k^3)$"@ in the Akin--Buchsbaum--Weyman sense.
    The final zero in the input chooses the three-dimensional
    natural module; omitting it would choose a different group.
SeeAlso
  weylModuleZ
  schurAlgebraDecompositionMatrix
  compositionToWeight
  "Tutorial: the Schur algebra and the symmetric group"
///

doc ///
Key
  schurAlgebraDecompositionMatrix
  (schurAlgebraDecompositionMatrix, ZZ, ZZ, ZZ)
Headline
  Decomposition matrix of the Schur algebra S(n, r) in characteristic p
Usage
  M = schurAlgebraDecompositionMatrix(p, n, r)
Inputs
  p:ZZ
    a prime
  n:ZZ
    a positive integer, the dimension of the natural module
  r:ZZ
    a non-negative homogeneous degree
Outputs
  M:Matrix
    the integer matrix @TEX "$([V(\\mu) : L(\\nu)])_{\\mu, \\nu}$"@
    with rows and columns indexed, in the same order, by
    @TT "boundedPartitions(n, r, r)"@
Description
  Text
    The entry in row @TEX "$\\mu$"@ and column @TEX "$\\nu$"@ is the
    composition multiplicity @TEX "$[V(\\mu):L(\\nu)]$"@ for the Schur
    algebra @TEX "$S(n,r)$"@ in characteristic @TT "p"@.  Each label is
    a length-@TT "n"@, zero-padded partition returned by
    @TO boundedPartitions@.  Keeping the labels in that returned order
    determines the meaning of its rows and columns.  For @TT "n = 1"@
    there is one partition and the function returns the one-by-one
    matrix with entry 1.
  Text
    For @TEX "$S(4,3)$"@ in characteristic 3, the label order is
    @TT "{3,0,0,0}, {2,1,0,0}, {1,1,1,0}"@.  In that order the matrix is
    upper unitriangular: @TEX "$V(3)$"@ has factors @TEX "$L(3)$"@ and
    @TEX "$L(2,1)$"@, @TEX "$V(2,1)$"@ has factors
    @TEX "$L(2,1)$"@ and @TEX "$L(1,1,1)$"@, and
    @TEX "$V(1,1,1)$"@ is simple.
  Example
    ptns = boundedPartitions(4, 3, 3)
    M = schurAlgebraDecompositionMatrix(3, 4, 3)
    entries M                                  -- {{1,1,0},{0,1,1},{0,0,1}}
  Text
    The labels are @TEX "$(3),(2,1),(1,1,1)$"@, padded to
    length four.  The first row @TT "{1,1,0}"@ says that
    @TEX "$V(3)$"@ has one copy each of @TEX "$L(3)$"@ and
    @TEX "$L(2,1)$"@.  The second row @TT "{0,1,1}"@ gives
    @TEX "$L(2,1)$"@ and @TEX "$L(1,1,1)$"@ as the factors
    of @TEX "$V(2,1)$"@.  The last row says that the exterior
    cube @TEX "$V(1,1,1)$"@ is simple.
SeeAlso
  schurAlgebraWeylModule
  symmetricGroupDecompositionMatrix
  decompositionNumbers
  "Tutorial: the Schur algebra and the symmetric group"
///

doc ///
Key
  symmetricGroupDecompositionMatrix
  (symmetricGroupDecompositionMatrix, ZZ, ZZ)
Headline
  Decomposition matrix of S_n in characteristic p
Usage
  M = symmetricGroupDecompositionMatrix(p, n)
Inputs
  p:ZZ
    a prime
  n:ZZ
    a positive integer
Outputs
  M:Matrix
    the integer matrix @TEX "$([S_\\mu:D_\\nu])_{\\mu,\\nu}$"@ for
    dual Specht modules, with rows indexed by all partitions of @TT "n"@
    and columns by p-restricted partitions
Description
  Text
    Write @TEX "$S_\\mu=(S^\\mu)^*$"@ for the contragredient dual of
    the ordinary Specht module.  In characteristic @TT "p"@ it may be
    reducible.  For a p-restricted partition @TEX "$\\nu$"@, write
    @TEX "$D_\\nu$"@ for the corresponding simple module.  The matrix
    entry @TEX "$[S_\\mu:D_\\nu]$"@ is its multiplicity as a composition
    factor of @TEX "$S_\\mu$"@.
  Text
    For @TEX "$N\\ge r$"@, the exact Schur functor
    @TEX "$f(M)=M_{(1^r,0^{N-r})}$"@ from @TEX "$S(N,r)$"@-modules to
    @TEX "$\\mathbb{F}_p S_r$"@-modules.  With the conventions used here,
    @TEX "$f(V(\\mu))=S_\\mu$"@, while @TEX "$f(L(\\nu))=D_\\nu$"@
    for p-restricted @TEX "$\\nu$"@ and is zero for the other simple
    Schur-algebra modules.  Therefore the symmetric-group matrix is
    obtained from the @TEX "$S(n,n)$"@ matrix by retaining precisely the
    p-restricted columns.
  Text
    Rows are ordered by @TO allPartitions@, and columns are ordered by
    @TO pRestrictedPartitions@.  Equivalently, these are the labels from
    @TT "boundedPartitions(n,n,n)"@ after trailing zeros are removed,
    with the non-p-restricted column labels discarded.  The case
    @TT "n = 1"@ returns the one-by-one matrix with entry 1.
  Example
    -- S_4 char 2: 5 partitions of 4 (rows), 2 are 2-restricted (cols).
    M = symmetricGroupDecompositionMatrix(2, 4)
    numrows M, numcols M
    entries M                         -- {{0,1},{1,1},{1,0},{1,1},{0,1}}
  Text
    The matrix has five rows, labelled
    @TEX "$(4),(3,1),(2,2),(2,1,1),(1,1,1,1)$"@, and two
    columns, labelled @TEX "$(2,1,1),(1,1,1,1)$"@ in the
    2-restricted convention.  Thus the row @TT "{1,1}"@ for
    @TEX "$(3,1)$"@ gives one copy of each simple module,
    while the row @TT "{1,0}"@ for @TEX "$(2,2)$"@ gives
    just @TEX "$D_{(2,1,1)}$"@.
    The first and last rows both give the one-dimensional simple
    @TEX "$D_{(1,1,1,1)}$"@, since sign is trivial in
    characteristic two.
SeeAlso
  symmetricGroupDecompositionNumbers
  schurAlgebraDecompositionMatrix
  pRestrictedPartitions
  mullineux
  "Tutorial: the Schur algebra and the symmetric group"
///

doc ///
Key
  symmetricGroupDecompositionNumbers
  (symmetricGroupDecompositionNumbers, ZZ, List)
Headline
  Decomposition numbers of a single dual Specht module
Usage
  dn = symmetricGroupDecompositionNumbers(p, mu)
Inputs
  p:ZZ
    a prime
  mu:List
    a partition of a positive integer @TT "n"@; trailing zeros are allowed
Outputs
  dn:HashTable
    the nonzero entries @TEX "$\\nu \\mapsto [S_\\mu:D_\\nu]$"@,
    keyed by p-restricted partitions @TEX "$\\nu$"@ of
    @TEX "$n=\\sum \\mu$"@
Description
  Text
    This gives the nonzero entries of one row of
    @TO symmetricGroupDecompositionMatrix@.
    Zero multiplicities are omitted from the hash table.  The
    function pads @TT "mu"@ to length @TT "n"@, constructs the
    @TEX "$\\mathfrak{sl}_n$"@ Weyl module with the corresponding
    consecutive-differences highest weight, computes its decomposition
    numbers, and keeps the p-restricted labels.
  Text
    Trailing zeros are removed before the ambient type-A rank is chosen,
    so different zero-padded representations of the same partition give
    the same answer.  For the partition @TT "{1}"@ the function returns
    the single entry @TT "{1} => 1"@.
  Example
    -- The nonzero entries in the dual Specht row for (3,1) at p = 2.
    dn = symmetricGroupDecompositionNumbers(2, {3, 1})
    pairs dn
  Text
    The two nonzero entries are @TT "{2,1,1} => 1"@ and
    @TT "{1,1,1,1} => 1"@.  Hence the dual Specht module
    @TEX "$S_{(3,1)}$"@ has one copy each of the two simple
    modules in characteristic two.  Their dimensions are two
    and one, accounting for the three-dimensional Specht module.
SeeAlso
  symmetricGroupDecompositionMatrix
  decompositionNumbers
  pRestrictedPartitions
///

----------------------------------------------------------------------
-- compositionToWeight: the partition <-> Dynkin formula in type A
----------------------------------------------------------------------

doc ///
Key
  compositionToWeight
  (compositionToWeight, List)
Headline
  Convert a length-n composition to the corresponding Dynkin weight in type A
Usage
  wt = compositionToWeight mu
Inputs
  mu:List
    a length-@TT "n"@ list of integers (a partition or composition)
Outputs
  wt:List
    the consecutive-differences sequence @TT "(mu_1 - mu_2, mu_2 - mu_3, ..., mu_{n-1} - mu_n)"@
    of length @TT "n - 1"@
Description
  Text
    In type @TEX "$A_{n-1}$"@
    (= @TEX "$\\mathfrak{sl}_n$"@), partitions of length @TT "n"@ index
    Weyl modules through the formula
    @TEX "$\\lambda_{\\mathrm{Dynkin}} = (\\mu_1 - \\mu_2, \\mu_2 - \\mu_3, \\ldots, \\mu_{n-1} - \\mu_n)$"@.
    This is the "consecutive-differences" map, a special case of
    @TO partitionToFundamental@ (in @TO KacMoody@).
  Text
    The inverse, modulo addition of a constant to every entry, is
    @TO weightToComposition@, which fixes a degree and returns a composition of that
    degree.
  Example
    compositionToWeight {3, 1, 0, 0}              -- {2, 1, 0}
    compositionToWeight {2, 1}                     -- {1}
    compositionToWeight {5, 4, 4, 2, 0}            -- {1, 0, 2, 2}
  Text
    The outputs are @TT "{2,1,0}"@, @TT "{1}"@, and
    @TT "{1,0,2,2}"@.  They give the corresponding highest weights
    for @TEX "$\\mathrm{SL}_4$"@, @TEX "$\\mathrm{SL}_2$"@,
    and @TEX "$\\mathrm{SL}_5$"@.  Since all three inputs are
    partitions, the differences are nonnegative and the weights
    are dominant.
SeeAlso
  weightToComposition
  schurAlgebraWeylModule
  weightAsDynkin
  WeightFormat
///

----------------------------------------------------------------------
-- Further module, character, and submodule computations
----------------------------------------------------------------------

doc ///
Key
  "WeylModules: function index"
Headline
  Functions grouped by mathematical purpose
Description
  Text
    Each link leads to the function's own explanation, examples, and
    mathematical interpretation of the outputs.
  Text
    @SUBSECTION "Chevalley structure constants"@
  Text
    @UL {
      {TO isSimplyLaced, ", ", TO classifyFiniteCartan, ", ", TO chevalleyEpsilonMatrix, ", ", TO chevalleyConstants},
      {TO chevalleyN, ", ", TO verifyJacobi}
    }@
  Text
    @SUBSECTION "Integral Weyl modules, weights, and actions"@
  Text
    @UL {
      {TO weylModuleZ, ", ", TO weylModule, ", ", TO highestWeight, ", ", TO ambientCartan},
      {TO weights, ", ", TO weightSpaces, ", ", TO weightSpace, ", ", TO basisVecs},
      {TO character, ", ", TO actionF, ", ", TO actionE, ", ", TO actionH},
      {TO applyChevalley, ", ", TO pbwTuples, ", ", TO maximalVectors}
    }@
  Text
    @SUBSECTION "Modular submodules, quotients, and socle filtrations"@
  Text
    @UL {
      {TO subWeylModule, ", ", TO subWeylModuleDirectSum, ", ", TO ambientWeylModule, ", ", TO isWithin},
      {TO quotientWeylModule, ", ", TO definingKernel, ", ", TO simpleQuotient, ", ", TO simpleCharacter},
      {TO simpleDimension, ", ", TO socleWeyl, ", ", TO decomposeCharacter, ", ", TO decompositionNumbers},
      {TO maximalSubmodule, ", ", TO isAmbiguous, ", ", TO ambiguousMaxVecs, ", ", TO gensNextSocle},
      {TO nextSocle, ", ", TO socleSeries, ", ", TO gensSocleLayers, ", ", TO printSocleLayers},
      {TO socleLengthTwoQuotient, ", ", TO twoFactorQuotientsContaining}
    }@
  Text
    @SUBSECTION "Characters and weight predicates"@
  Text
    @UL {
      {TO characterDim, ", ", TO productCharacter, ", ", TO differenceCharacter, ", ", TO sortedCharacter},
      {TO isDominantWeight, ", ", TO isRestrictedWeight, ", ", TO highestPrimePower, ", ", TO dominantWeights},
      {TO dominantWeightSpaces, ", ", TO isMaximalVector, ", ", TO simpleTopFactorCharacter, ", ", TO simpleTopFactorDim},
      {TO socleSeriesAndPrims}
    }@
  Text
    @SUBSECTION "Weight-coordinate conversions"@
  Text
    @UL {
      {TO weightAsPartition, ", ", TO weightAsDynkin, ", ", TO characterAsPartition, ", ", TO characterAsDynkin}
    }@
  Text
    @SUBSECTION "Schur algebras, partitions, and symmetric groups"@
  Text
    @UL {
      {TO compositionToWeight, ", ", TO weightToComposition, ", ", TO allPartitions, ", ", TO boundedPartitions},
      {TO conjugatePartition, ", ", TO pRestrictedQ, ", ", TO pRegularQ, ", ", TO pRestrictedPartitions},
      {TO pRegularPartitions, ", ", TO mullineux, ", ", TO schurAlgebraWeylModule, ", ", TO schurAlgebraDecompositionMatrix},
      {TO symmetricGroupDecompositionMatrix, ", ", TO symmetricGroupDecompositionNumbers}
    }@
  Text
    @SUBSECTION "Quotient maps, subquotients, and submodule operations"@
  Text
    @UL {
      {TO quotientProjection, ", ", TO quotientSection, ", ", TO quotientBasis, ", ", TO projectToQuotient},
      {TO liftFromQuotient, ", ", TO characteristic, ", ", TO rootSystemType, ", ", TO lieType},
      {TO lieRank, ", ", TO moduleMetadata, ", ", TO constructionData, ", ", TO reduceWeylModule},
      {TO reduction, ", ", TO isSubmoduleOf, ", ", TO containsSubmodule, ", ", TO equalSubmodules},
      {TO submoduleCodimension, ", ", TO submoduleSum, ", ", TO submoduleIntersection, ", ", TO subquotientWeylModule},
      {TO lowerSubmodule, ", ", TO upperSubmodule, ", ", TO subquotientProjection, ", ", TO subquotientSection},
      {TO subquotientBasis, ", ", TO projectToSubquotient, ", ", TO liftFromSubquotient}
    }@
  Text
    @SUBSECTION "Formal characters and labeled decomposition tables"@
  Text
    @UL {
      {TO weylCharacter, ", ", TO coordinateConvention, ", ", TO sourceWeightFormat, ", ", TO isVirtualCharacter},
      {TO characterMetadata, ", ", TO rawCharacter, ", ", TO characterTerms, ", ", TO tensorProductCharacter},
      {TO tensorProductFactors, ", ", TO dualCharacter, ", ", TO frobeniusTwistCharacter, ", ", TO decompositionTable},
      {TO rowLabels, ", ", TO columnLabels, ", ", TO prime, ", ", TO algebra},
      {TO tableMetadata, ", ", TO schurAlgebraDecompositionTable, ", ", TO symmetricGroupDecompositionTable}
    }@
  Text
    @SUBSECTION "Composition factors and socle analysis"@
  Text
    @UL {
      {TO compositionFactors, ", ", TO isIrreducible, ", ", TO socleAnalysis, ", ", TO socleLayerFactors},
      {TO socleLayerGenerators, ", ", TO cumulativeSocleLayers, ", ", TO socleLayerDimensions, ", ", TO cumulativeSocleDimensions},
      {TO successiveQuotientDimensions, ", ", TO ambiguousWeights, ", ", TO ambiguousWeightsByLayer, ", ", TO analysisIssues},
      {TO isCertified, ", ", TO socleAnalysisComplete, ", ", TO socleAnalysisPrime, ", ", TO socleAnalysisModuleDimension},
      {TO socleLength, ", ", TO reductionProfile, ", ", TO reductionRows, ", ", TO reductionPrimes},
      {TO reductionRow, ", ", TO reductionWeylModule, ", ", TO reductionHighestWeight, ", ", TO reducibleCharacteristics},
      {TO extensionData, ", ", TO isTwoFactorExtension}
    }@
  Text
    @SUBSECTION "Action matrices, divided powers, homomorphisms, and diagrams"@
  Text
    @UL {
      {TO pbwRoots, ", ", TO chevalleyActionMatrices, ", ", TO raisingMatrices, ", ", TO loweringMatrices},
      {TO cartanMatrices, ", ", TO globalBasis, ", ", TO dividedPowerActionE, ", ", TO dividedPowerActionF},
      {TO binomialCartanAction, ", ", TO checkWeylModule, ", ", TO verifyRepresentation, ", ", TO isValid},
      {TO verificationChecks, ", ", TO verificationFailures, ", ", TO weylHomDimension, ", ", TO weylHomGenerators},
      {TO weightGraph, ", ", TO graphVertices, ", ", TO graphEdges, ", ", TO toDot},
      {TO toSVG, ", ", TO viewGraph, ", ", TO socleDiagram, ", ", TO diagramLayers}
    }@
SeeAlso
  WeylModules
  "WeylModules: conventions and limitations"
///

doc ///
Key
  ReductionProfile
Headline
  Reductions of one integral Weyl module in several characteristics
Description
  Text
    A @TO ReductionProfile@ records the dimensions and composition factors of
    the reductions of a fixed integral Weyl module at specified primes. Its
    rows refer to the same highest weight and integral lattice, so changes
    between rows arise from the characteristic. Structural information can
    also be included; its meaning and limitations are described under @TO reductionProfile@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2, 3})
    P
  Text
    The module has dimension 3 in both characteristics. Its reduction in
    characteristic 2 has simple head of dimension 2 and radical of dimension 1;
    in characteristic 3 it is irreducible. An uncomputed socle is displayed as
    unspecified, not as zero.
SeeAlso
  reductionProfile
  reductionRows
  reducibleCharacteristics
///

doc ///
Key
  reduction
  (reduction, WeylModuleZ, ZZ)
Headline
  Reduce an integral Weyl module modulo a prime
Description
  Text
    For an integral Weyl module @TEX "$W=V_{\\mathbb Z}(\\lambda)$"@ and
    a prime @TT "p"@, this returns @TEX "$W\\otimes_{\\mathbb Z}\\mathbb F_p$"@.
    It is represented as a @TO QuotientWeylModuleFp@ with zero defining
    kernel. The stored integral PBW basis and divided-power actions are reduced
    modulo @TT "p"@. This is the same construction as @TO reduceWeylModule@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    V = reduction(W, 2)
    dim V
    character V
  Text
    The reduction has the three weights 2, 0, and -2, each of multiplicity 1.
    These multiplicities agree with those of the integral module even though
    the reduction in characteristic 2 is reducible.
SeeAlso
  reduceWeylModule
  reductionProfile
  weylModuleZ
///

doc ///
Key
  reduceWeylModule
  (reduceWeylModule, WeylModuleZ, ZZ)
Headline
  Form the prime-field reduction of an integral Weyl module
Description
  Text
    For a prime @TT "p"@, @TT "reduceWeylModule(W,p)"@ forms
    @TEX "$V_{\\mathbb Z}(\\lambda)\\otimes_{\\mathbb Z}\\mathbb F_p$"@ using the
    integral basis of @TT "W"@. It agrees with @TO reduction@. The result is the
    whole reduced Weyl module, rather than its simple quotient.
  Example
    W = weylModuleZ(cartanA 2, {2})
    V = reduceWeylModule(W, 2)
    dim V
    simpleDimension(cartanA 2, 2, {2})
  Text
    The Weyl module has dimension 3, while its simple quotient has dimension
    2. The missing dimension is the one-dimensional radical.
SeeAlso
  reduction
  simpleDimension
  maximalSubmodule
///

doc ///
Key
  reductionProfile
  (reductionProfile, WeylModuleZ, List)
  (reductionProfile, WeylModuleZ, ZZ)
  [reductionProfile, IncludeStructure]
  [reductionProfile, IncludeSocle]
  [reductionProfile, RequireComplete]
  [reductionProfile, WeightFormat]
Headline
  Compare dimensions and composition factors across characteristics
Description
  Text
    For @TEX "$W=V_{\\mathbb Z}(\\lambda)$"@, @TT "reductionProfile(W,primes)"@
    records the prime, Weyl and simple-head dimensions, radical dimension,
    composition length, composition factors, and irreducibility of each
    reduction. An integer instead of a list requests all primes at most that
    integer; repeated primes are omitted after their first occurrence.
    @TT "IncludeStructure => true"@ adds maximal-vector and initial ambiguity
    data. @TT "IncludeSocle => true"@ also computes successive socle layers and
    records their dimensions, length, ambiguity, and whether the search
    determines the socle filtration. @TT "RequireComplete => true"@ requests
    these computations and raises an error if the socle filtration is not
    determined. Unrequested structural values are @TT "null"@. The limitation
    of the maximal-vector search remains the one described under @TO socleAnalysis@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2, 3}, IncludeSocle => true)
    apply(reductionRows P, r -> (r#"prime", r#"simpleDimension", r#"radicalDimension", r#"socleLength"))
  Text
    The tuples are (2,2,1,2) and (3,3,0,1). Thus in characteristic 2 the module
    has a one-dimensional radical and two socle layers; in characteristic 3
    it is simple and has one socle layer.
SeeAlso
  ReductionProfile
  reductionRow
  socleAnalysis
///

doc ///
Key
  reductionRows
  (reductionRows, ReductionProfile)
Headline
  Return the rows of a reduction profile
Description
  Text
    This returns one hash table for each recorded prime, in the order used by
    @TO reductionProfile@. The numerical fields describe dimensions and
    composition multiplicities in that characteristic; structural fields are
    @TT "null"@ unless the corresponding computation was requested.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2, 3})
    apply(reductionRows P, r -> (r#"prime", r#"compositionFactors"))
  Text
    In characteristic 2 the factors are @TEX "$L(2)$"@ and @TEX "$L(0)$"@,
    each once. In characteristic 3 the only factor is @TEX "$L(2)$"@.
    These are composition-factor statements and do not assert a direct-sum
    decomposition.
SeeAlso
  reductionProfile
  reductionRow
///

doc ///
Key
  reductionPrimes
  (reductionPrimes, ReductionProfile)
Headline
  Return the characteristics recorded in a reduction profile
Description
  Text
    This returns the list of primes indexing the rows of a
    @TO ReductionProfile@. It preserves their order of first occurrence.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {3, 2, 3})
    reductionPrimes P
  Text
    The list @TT "{3,2}"@ means that the first row concerns characteristic 3
    and the second characteristic 2. The repeated request for characteristic
    3 contributes no additional row.
SeeAlso
  reductionProfile
  reductionRows
///

doc ///
Key
  reductionRow
  (reductionRow, ReductionProfile, ZZ)
Headline
  Return the data for one characteristic in a reduction profile
Description
  Text
    For a prime @TT "p"@ recorded in @TT "P"@, @TT "reductionRow(P,p)"@
    returns the hash table describing the reduction in characteristic @TT "p"@.
    It returns @TT "null"@ if that characteristic was not included.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2, 3})
    r = reductionRow(P, 2)
    r#"simpleDimension", r#"radicalDimension"
    reductionRow(P, 5)
  Text
    The pair (2,1) gives the dimensions of the simple head and radical in
    characteristic 2. The last result says only that characteristic 5 was not
    computed in this profile.
SeeAlso
  reductionProfile
  reductionRows
///

doc ///
Key
  reductionWeylModule
  (reductionWeylModule, ReductionProfile)
Headline
  Recover the integral module underlying a reduction profile
Description
  Text
    This returns the integral Weyl module from which all rows of the
    @TO ReductionProfile@ were computed. It does not select a reduction in
    one characteristic.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2, 3})
    U = reductionWeylModule P
    highestWeight U
    dim U
  Text
    The recovered lattice has highest weight 2 and rank 3 over the integers.
    Its reductions supply the characteristic-dependent rows of @TT "P"@.
SeeAlso
  reductionHighestWeight
  reduction
///

doc ///
Key
  reductionHighestWeight
  (reductionHighestWeight, ReductionProfile)
Headline
  Return the highest weight of a reduction profile
Description
  Text
    This returns the highest weight of the common integral Weyl module, in
    the coordinate convention selected when @TO reductionProfile@ was called.
    The highest weight does not change with the prime.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2, 3})
    reductionHighestWeight P
  Text
    The weight @TT "{2}"@ is @TEX "$2\\omega_1$"@ for the root system
    @TEX "$A_1$"@. Both rows therefore concern reductions of the same
    highest-weight lattice.
SeeAlso
  reductionWeylModule
  highestWeight
  WeightFormat
///

doc ///
Key
  reducibleCharacteristics
  (reducibleCharacteristics, WeylModuleZ, List)
  (reducibleCharacteristics, WeylModuleZ, ZZ)
  (reducibleCharacteristics, ReductionProfile)
Headline
  Find the requested characteristics in which a Weyl module is reducible
Description
  Text
    This returns the primes among those supplied for which the reduction of
    the integral Weyl module is reducible. An integer gives an inclusive bound
    on the primes to test. Applied to a @TO ReductionProfile@, it selects from
    the already recorded rows. No assertion is made about primes outside the
    requested range.
  Example
    W = weylModuleZ(cartanA 2, {2})
    reducibleCharacteristics(W, 5)
    P = reductionProfile(W, {2, 3})
    reducibleCharacteristics P
  Text
    Both lists contain only 2. The reduction of @TEX "$V_{\\mathbb Z}(2)$"@
    is reducible in characteristic 2 and irreducible in characteristics 3 and
    5.
SeeAlso
  reductionProfile
  isIrreducible
///

doc ///
Key
  quotientProjection
  (quotientProjection, QuotientWeylModuleFp, List)
  (quotientProjection, SubquotientWeylModuleFp, List)
  [quotientProjection, WeightFormat]
Headline
  Matrix of the quotient map on a weight space
Description
  Text
    For @TEX "$Q=V/S$"@, fix a weight @TEX "$\\mu$"@.
    All matrices are over the defining prime field. For a subquotient
    @TEX "$T/S$"@, the same functions use ambient coordinates: the projection
    restricts to the intrinsic quotient map on @TEX "$T_\\mu$"@, while its
    chosen extension to vectors outside @TEX "$T_\\mu$"@ has no intrinsic
    representation-theoretic meaning.
    The result is the matrix of
    @TEX "$V_\\mu\\longrightarrow Q_\\mu$"@ in the stored ambient and quotient
    bases. For @TEX "$Q=V/S$"@, its kernel is @TEX "$S_\\mu$"@.
    An absent ambient weight gives
    @TT "null"@; a weight annihilated in the quotient gives a matrix with zero
    rows.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    Q = quotientWeylModule S
    quotientProjection(Q, {2})
    quotientProjection(Q, {0})
    quotientProjection(Q, {4})
  Text
    The identity matrix at weight 2 says that the highest-weight line survives
    in the quotient. The matrix at weight 0 has no rows because the
    one-dimensional socle is exactly that weight space. Weight 4 is absent
    even in the ambient Weyl module.
SeeAlso
  quotientSection
  projectToQuotient
  subquotientProjection
///

doc ///
Key
  quotientSection
  (quotientSection, QuotientWeylModuleFp, List)
  (quotientSection, SubquotientWeylModuleFp, List)
  [quotientSection, WeightFormat]
Headline
  Chosen linear section of a quotient weight space
Description
  Text
    This gives a matrix @TEX "$J_\\mu:Q_\\mu\\longrightarrow V_\\mu$"@ whose
    columns are the ambient representatives chosen for the quotient basis.
    If @TEX "$P_\\mu$"@ is @TO quotientProjection@, then
    @TEX "$P_\\mu J_\\mu=I$"@. The section is a noncanonical linear map; it
    need not be a homomorphism of representations. For @TEX "$T/S$"@ its
    image lies in @TEX "$T_\\mu$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    Q = quotientWeylModule S
    J = quotientSection(Q, {2})
    quotientProjection(Q, {2}) * J
  Text
    The product is the identity on the one-dimensional quotient weight space.
    Thus the chosen ambient vector represents the quotient basis vector.
    This identity says nothing about whether the sections commute with the
    root operators between different weights.
SeeAlso
  quotientProjection
  quotientBasis
  liftFromQuotient
///

doc ///
Key
  quotientBasis
  (quotientBasis, QuotientWeylModuleFp, List)
  (quotientBasis, SubquotientWeylModuleFp, List)
  [quotientBasis, WeightFormat]
Headline
  Representatives of a quotient weight-space basis
Description
  Text
    The columns are chosen ambient representatives of a basis of the
    quotient weight space. Consequently this matrix equals @TO quotientSection@.
    These representatives depend on the chosen complement to the kernel.
    For a subquotient @TEX "$T/S$"@ they lie in @TEX "$T_\\mu$"@.
    By contrast, @TT "weightSpace(Q,mu)"@ expresses its basis in quotient
    coordinates.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    Q = quotientWeylModule S
    B = quotientBasis(Q, {2})
    quotientProjection(Q, {2}) * B
  Text
    The ambient highest-weight vector maps to the quotient basis vector.
    The resulting identity matrix proves that its coset is a basis of
    @TEX "$Q_2$"@; it does not choose a canonical complement as a
    representation.
SeeAlso
  quotientSection
  weightSpace
  basisVecs
///

doc ///
Key
  projectToQuotient
  (projectToQuotient, QuotientWeylModuleFp, List, List)
  (projectToQuotient, QuotientWeylModuleFp, List, Matrix)
  (projectToQuotient, SubquotientWeylModuleFp, List, List)
  (projectToQuotient, SubquotientWeylModuleFp, List, Matrix)
  [projectToQuotient, WeightFormat]
Headline
  Express ambient vectors in quotient coordinates
Description
  Text
    This applies @TO quotientProjection@ to a vector in one weight space.
    A coefficient list is treated as a column; a matrix supplies one vector
    per column. The result is always a matrix over the defining prime field.
    For a subquotient @TEX "$T/S$"@, the supplied columns must lie in
    @TEX "$T_\\mu$"@ for the result to have the meaning of a quotient class.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    Q = quotientWeylModule S
    projectToQuotient(Q, {2}, {1})
    projectToQuotient(Q, {0}, {1})
  Text
    The highest-weight vector has nonzero quotient coordinate 1. The
    weight-zero vector maps to the zero-dimensional space, because that
    vector belongs to the socle used as the kernel.
SeeAlso
  quotientProjection
  liftFromQuotient
  projectToSubquotient
///

doc ///
Key
  liftFromQuotient
  (liftFromQuotient, QuotientWeylModuleFp, List, List)
  (liftFromQuotient, QuotientWeylModuleFp, List, Matrix)
  (liftFromQuotient, SubquotientWeylModuleFp, List, List)
  (liftFromQuotient, SubquotientWeylModuleFp, List, Matrix)
  [liftFromQuotient, WeightFormat]
Headline
  Choose ambient representatives of quotient vectors
Description
  Text
    This applies the linear section @TO quotientSection@ to a list of
    quotient coefficients or a matrix of quotient columns. It returns ambient
    coordinate columns over the prime field. The lift depends on the
    complement used to construct the quotient. For @TEX "$T/S$"@ the returned
    columns lie in @TEX "$T_\\mu$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    Q = quotientWeylModule S
    v = liftFromQuotient(Q, {2}, {1})
    projectToQuotient(Q, {2}, v)
  Text
    The lift is the ambient highest-weight vector, and projecting it gives
    the original quotient coordinate 1. Any other representative differs
    from this lift by a vector of the kernel in the same weight space.
SeeAlso
  quotientSection
  projectToQuotient
  liftFromSubquotient
///

doc ///
Key
  characteristic
  (characteristic, WeylModuleZ)
  (characteristic, SubWeylModuleFp)
  (characteristic, QuotientWeylModuleFp)
  (characteristic, SubquotientWeylModuleFp)
  (characteristic, WeylCharacter)
Headline
  Characteristic attached to a module or formal character
Description
  Text
    For an integral @TO WeylModuleZ@ the result is 0; for a modular
    submodule, quotient, or subquotient it is the defining prime.
    A @TO WeylCharacter@ obtained from a module inherits this value.
    For a character supplied directly as a weight-multiplicity sum, the
    characteristic is @TT "null"@ unless a later operation specifies it.
    The value 0 for an integral module refers to its integral construction.
  Example
    W = weylModuleZ(cartanA 2, {2})
    characteristic W
    characteristic reduction(W, 2)
    characteristic weylCharacter(cartanA 2, new HashTable from {{1} => 1, {-1} => 1})
  Text
    The first two values distinguish the integral lattice from its reduction
    over @TEX "$\\mathbb F_2$"@. The last sum has no specified field, so its
    characteristic is unspecified.
SeeAlso
  coefficientRing
  weylCharacter
  reduction
///

doc ///
Key
  (coefficientRing, WeylModuleZ)
  (coefficientRing, SubWeylModuleFp)
  (coefficientRing, QuotientWeylModuleFp)
  (coefficientRing, SubquotientWeylModuleFp)
Headline
  Coefficient ring of a Weyl module, submodule, or quotient
Description
  Text
    The coefficient ring is @TT "ZZ"@ for an integral Weyl module and the
    prime field @TT "ZZ/p"@ for a modular submodule, quotient, or subquotient.
    The integer basis vectors describe a free lattice; after reduction their
    coordinates lie in the indicated field.
  Example
    W = weylModuleZ(cartanA 2, {2})
    coefficientRing W
    coefficientRing reduction(W, 2)
  Text
    The lattice is defined over the integers, and its reduction is a
    three-dimensional vector space over @TEX "$\\mathbb F_2$"@.
    The characteristic and the coefficient ring therefore distinguish the
    two constructions.
SeeAlso
  characteristic
  reduction
///

doc ///
Key
  rootSystemType
  (rootSystemType, WeylModuleZ)
  (rootSystemType, SubWeylModuleFp)
  (rootSystemType, QuotientWeylModuleFp)
  (rootSystemType, SubquotientWeylModuleFp)
Headline
  Finite Cartan type and rank of the ambient root system
Description
  Text
    This returns the finite Cartan type and rank as a pair. For a modular
    submodule or quotient it describes the root system of the ambient Weyl
    module, irrespective of which weights remain in the submodule or quotient.
  Example
    W = weylModuleZ(cartanB 2, {1, 0})
    rootSystemType W
  Text
    The pair @TT "(B,2)"@ identifies a rank-two root system of type
    @TEX "$B_2$"@. It describes the Cartan matrix, rather than the dimension
    of the representation.
SeeAlso
  lieType
  lieRank
  ambientCartan
///

doc ///
Key
  lieType
  (lieType, WeylModuleZ)
  (lieType, SubWeylModuleFp)
  (lieType, QuotientWeylModuleFp)
  (lieType, SubquotientWeylModuleFp)
Headline
  Cartan letter of the ambient root system
Description
  Text
    This returns the type letter of the finite root system, such as
    @TT "\"A\""@ or @TT "\"B\""@. Submodules and quotients inherit the
    ambient Cartan type.
  Example
    W = weylModuleZ(cartanB 2, {1, 0})
    lieType W
  Text
    The value @TT "\"B\""@ means that the root datum is of type
    @TEX "$B$"@. Its rank is supplied separately by @TO lieRank@.
SeeAlso
  rootSystemType
  lieRank
///

doc ///
Key
  lieRank
  (lieRank, WeylModuleZ)
  (lieRank, SubWeylModuleFp)
  (lieRank, QuotientWeylModuleFp)
  (lieRank, SubquotientWeylModuleFp)
Headline
  Rank of the ambient root system
Description
  Text
    This is the number of simple roots, equivalently the size of the
    Cartan matrix. It is also the number of entries in a Dynkin-coordinate
    weight. It does not mean the dimension of the Weyl module.
  Example
    W = weylModuleZ(cartanB 2, {1, 0})
    lieRank W
    dim W
  Text
    There are two simple roots, while the representation has dimension 5.
    The highest weight therefore has two Dynkin coordinates, regardless of
    the dimension of the module.
SeeAlso
  lieType
  rootSystemType
  ambientCartan
///

doc ///
Key
  moduleMetadata
  (moduleMetadata, WeylModuleZ)
  (moduleMetadata, SubWeylModuleFp)
  (moduleMetadata, QuotientWeylModuleFp)
  (moduleMetadata, SubquotientWeylModuleFp)
  [moduleMetadata, WeightFormat]
Headline
  Collect the defining data and dimensions of a module
Description
  Text
    This returns a hash table containing the ambient Cartan type and matrix,
    coefficient ring, characteristic, highest weight when available, and
    dimension. Submodules, quotients, and subquotients also record their
    ambient dimension and the dimensions of the relevant kernel or enclosing
    submodules. If the module has no unique maximal weight, its
    @TT "highestWeight"@ entry is @TT "null"@. Weight entries use the requested
    @TO WeightFormat@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    Q = quotientWeylModule S
    d = moduleMetadata Q
    d#"dimension", d#"kernelDimension", d#"ambientHighestWeight"
  Text
    The tuple (2,1,{2}) describes a quotient of the three-dimensional
    highest-weight-2 Weyl module by a one-dimensional kernel. These values
    are consistent with the weight-space dimensions of
    @TEX "$V(2)/L(0)$"@ in characteristic 2.
SeeAlso
  constructionData
  highestWeight
  characteristic
///

doc ///
Key
  constructionData
  (constructionData, WeylModuleZ)
  (constructionData, SubWeylModuleFp)
  (constructionData, QuotientWeylModuleFp)
  (constructionData, SubquotientWeylModuleFp)
Headline
  Record the integral basis and quotient conventions
Description
  Text
    This gives the Cartan matrix, ambient Dynkin highest weight,
    characteristic, dimensions, ordered positive roots, and basis convention
    used in the construction. For quotient objects it records that the
    chosen section is noncanonical. It also records the package, Macaulay2,
    and construction-method versions, which identify the calculation.
    The root ordering determines the order of entries in a PBW exponent
    tuple.
  Example
    W = weylModuleZ(cartanA 2, {2})
    d = constructionData W
    d#"highestWeightDynkin"
    d#"positiveRootOrder"
    d#"basisConvention"
  Text
    The highest weight is 2 and there is one positive root, so each PBW
    exponent tuple has one entry. The basis consists of the three divided-power
    cosets represented by exponents 0, 1, and 2.
SeeAlso
  pbwRoots
  basisVecs
  moduleMetadata
///

doc ///
Key
  pbwRoots
  (pbwRoots, WeylModuleZ)
  (pbwRoots, SubWeylModuleFp)
  (pbwRoots, QuotientWeylModuleFp)
  (pbwRoots, SubquotientWeylModuleFp)
Headline
  Ordered positive roots indexing PBW exponents
Description
  Text
    This returns the positive roots in simple-root coordinates, in the
    order used by the divided-power PBW construction: increasing root height,
    then lexicographic order. For a modular submodule, quotient, or
    subquotient it returns the ordering of the ambient integral Weyl module.
    It is an ordering of roots, not a list of module weights.
  Example
    W = weylModuleZ(cartanB 2, {1, 0})
    pbwRoots W
  Text
    The roots are @TT "{0,1}"@, @TT "{1,0}"@, @TT "{1,1}"@, and
    @TT "{1,2}"@. A PBW exponent tuple has four entries in this order,
    describing the divided powers of the corresponding negative-root vectors.
SeeAlso
  basisVecs
  constructionData
///

doc ///
Key
  (weightMultiplicity, WeylModuleZ, List)
  (weightMultiplicity, SubWeylModuleFp, List)
  (weightMultiplicity, QuotientWeylModuleFp, List)
  (weightMultiplicity, SubquotientWeylModuleFp, List)
  (weightMultiplicity, WeylCharacter, List)
  [weightMultiplicity, WeightFormat]
Headline
  Multiplicity of one weight
Description
  Text
    For a module this returns the dimension of the specified weight space,
    or its rank for an integral Weyl lattice. An absent weight has
    multiplicity 0. For a @TO WeylCharacter@ it returns the coefficient of
    that weight; a virtual character may have negative coefficients.
    The weight is interpreted according to @TO WeightFormat@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    weightMultiplicity(W, {0})
    weightMultiplicity(W, {4})
    S = socleWeyl(W, 2)
    weightMultiplicity(S, {0})
  Text
    The integral module has a one-dimensional weight-zero space and no
    weight-four space. In characteristic 2 its socle is precisely the
    weight-zero line, so that line retains multiplicity 1 in the socle.
SeeAlso
  weightSpace
  weights
  character
///

doc ///
Key
  WeightOrder
Headline
  Order of weights in lists and characters
Description
  Text
    The default @TT "WeightOrder => \"RootDepth\""@ orders weights by the
    height of the difference from the highest weight in simple-root
    coordinates, then by reverse lexicographic Dynkin coordinates.
    @TT "\"Lex\""@ requests reverse lexicographic order alone.
    For a formal character with several maximal weights, depth is the least
    height of its difference from a maximal support weight above it in the
    root order. A bare hash table does not specify a Cartan matrix and hence
    does not determine root depth.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    weights W
    weights(W, WeightOrder => "Lex")
  Text
    The first list arranges the adjoint weights by their distance below the
    highest root; the second arranges their Dynkin coordinates lexicographically.
    The weight-zero space has multiplicity 2, but appears only once in either
    weight list. Changing the order does not change the character.
SeeAlso
  weights
  basisVecs
  characterTerms
///

doc ///
Key
  isSubmoduleOf
  (isSubmoduleOf, SubWeylModuleFp, SubWeylModuleFp)
Headline
  Test containment of modular submodules
Description
  Text
    This tests @TEX "$S\\subseteq T$"@ by comparing their weight-space column
    spans. The two submodules must have the same integral ambient object and
    the same prime; incompatible ambient data give @TT "false"@. The order of
    arguments is the order in the containment relation.
  Example
    W = weylModuleZ(cartanA 2, {2})
    Z = subWeylModule(W, 2, {})
    S = socleWeyl(W, 2)
    isSubmoduleOf(Z, S)
    isSubmoduleOf(S, Z)
  Text
    The zero module is contained in the one-dimensional socle, while the
    socle is not contained in the zero module. Thus the results are true and
    false respectively.
SeeAlso
  containsSubmodule
  equalSubmodules
  isWithin
///

doc ///
Key
  containsSubmodule
  (containsSubmodule, SubWeylModuleFp, SubWeylModuleFp)
Headline
  Test whether the first submodule contains the second
Description
  Text
    This tests @TEX "$S\\supseteq T$"@, equivalently
    @TT "isSubmoduleOf(T,S)"@. Containment concerns subspaces of the identical
    integral ambient Weyl module reduced at the same prime. Incompatible
    ambient modules or characteristics give @TT "false"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    Z = subWeylModule(W, 2, {})
    S = socleWeyl(W, 2)
    containsSubmodule(S, Z)
    containsSubmodule(Z, S)
  Text
    The socle contains the zero submodule, but the zero submodule does not
    contain the socle. This illustrates the reversed argument order relative
    to @TO isSubmoduleOf@.
SeeAlso
  isSubmoduleOf
  equalSubmodules
///

doc ///
Key
  equalSubmodules
  (equalSubmodules, SubWeylModuleFp, SubWeylModuleFp)
Headline
  Compare two submodules as ambient subspaces
Description
  Text
    This tests equality of weight-space spans in the same integral ambient
    module and characteristic. It compares the submodules themselves, rather
    than their chosen bases or merely their abstract isomorphism types.
    Incompatible ambient data give @TT "false"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    Z = subWeylModule(W, 2, {})
    S = socleWeyl(W, 2)
    equalSubmodules(S, submoduleSum(S, Z))
    equalSubmodules(S, Z)
  Text
    Adding the zero module changes no weight-space span, so the first test
    is true. The second is false because the socle has dimension 1.
SeeAlso
  submoduleSum
  isSubmoduleOf
///

doc ///
Key
  submoduleCodimension
  (submoduleCodimension, SubWeylModuleFp)
Headline
  Codimension in the full reduced Weyl module
Description
  Text
    For @TEX "$S\\subseteq V_{\\mathbb F_p}(\\lambda)$"@ this returns
    @TEX "$\\dim V_{\\mathbb F_p}(\\lambda)-\\dim S$"@. It measures codimension
    in the full ambient Weyl module, even if another submodule has been used
    to describe @TT "S"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    submoduleCodimension S
  Text
    The socle has dimension 1 in a module of dimension 3, so its codimension
    is 2. This is also the dimension of the quotient by the socle.
SeeAlso
  quotientWeylModule
  dim
///

doc ///
Key
  submoduleSum
  (submoduleSum, SubWeylModuleFp, SubWeylModuleFp)
  (submoduleSum, List)
Headline
  Sum of modular submodules
Description
  Text
    This constructs the sum by taking the span of the input basis columns
    in each weight space over the common prime field. A nonempty list gives
    the sum of all its entries. Every input must have the identical integral
    ambient module and characteristic.
  Example
    W = weylModuleZ(cartanA 2, {2})
    Z = subWeylModule(W, 2, {})
    S = socleWeyl(W, 2)
    U = submoduleSum({Z, S, S})
    dim U
    equalSubmodules(U, S)
  Text
    The sum has dimension 1 and equals the socle: repetitions and zero
    summands add no new vectors. This equality is equality of subspaces
    inside the fixed ambient module.
SeeAlso
  submoduleIntersection
  equalSubmodules
///

doc ///
Key
  submoduleIntersection
  (submoduleIntersection, SubWeylModuleFp, SubWeylModuleFp)
  (submoduleIntersection, List)
Headline
  Intersection of modular submodules
Description
  Text
    This intersects the input column spans weight by weight over the
    common prime field. A nonempty list gives the intersection of all its
    entries. The integral ambient module and characteristic must agree.
  Example
    W = weylModuleZ(cartanA 2, {2})
    Z = subWeylModule(W, 2, {})
    S = socleWeyl(W, 2)
    U = submoduleIntersection(S, Z)
    dim U
    equalSubmodules(submoduleIntersection({S, S}), S)
  Text
    The intersection of the socle with the zero module is zero.
    Intersecting the socle with itself preserves its one-dimensional
    weight-zero space.
SeeAlso
  submoduleSum
  isSubmoduleOf
///

doc ///
Key
  SubquotientWeylModuleFp
Headline
  A modular subquotient represented in weight-space coordinates
Description
  Text
    An object of this type represents @TEX "$T/S$"@ for submodules
    @TEX "$S\\subseteq T\\subseteq V_{\\mathbb F_p}(\\lambda)$"@.
    Its matrices describe the induced divided-power action on the quotient
    weight spaces. The integral ambient Weyl module, the two submodules,
    and chosen ambient representatives of the quotient basis are retained.
    These linear representatives need not define a section as a representation.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    dim M
    character M
  Text
    The result has dimension 2 with weights 2 and -2, each of multiplicity
    1. It is @TEX "$V(2)/L(0)\\cong L(2)$"@ in characteristic 2; the
    one-dimensional weight-zero socle has been factored out.
SeeAlso
  subquotientWeylModule
  lowerSubmodule
  upperSubmodule
///

doc ///
Key
  subquotientWeylModule
  (subquotientWeylModule, SubWeylModuleFp, SubWeylModuleFp)
Headline
  Construct the subquotient T/S
Description
  Text
    Given modular submodules @TEX "$S\\subseteq T$"@, this constructs
    @TEX "$T/S$"@ with the induced divided-power action. The inputs must have
    the identical integral ambient module and characteristic, and the
    constructor checks containment. Quotient coordinates are chosen
    separately in each weight space.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    compositionFactors M
    subquotientProjection(M, {2}) * subquotientSection(M, {2})
  Text
    The only composition factor is @TEX "$L(2)$"@ with multiplicity 1.
    The identity matrix at weight 2 confirms that the chosen section is a
    right inverse to the projection on that weight space.
SeeAlso
  SubquotientWeylModuleFp
  quotientWeylModule
  subquotientProjection
///

doc ///
Key
  lowerSubmodule
  (lowerSubmodule, SubquotientWeylModuleFp)
Headline
  Kernel submodule in the presentation T/S
Description
  Text
    For @TEX "$M=T/S$"@ this returns the stored lower submodule
    @TEX "$S$"@, as a submodule of the original reduced Weyl module.
    It is the kernel of @TEX "$T\\longrightarrow T/S$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    L = lowerSubmodule M
    dim L
    character L
  Text
    The lower submodule is the one-dimensional weight-zero socle
    @TEX "$L(0)$"@. These are exactly the vectors killed when forming the
    present subquotient.
SeeAlso
  upperSubmodule
  subquotientWeylModule
///

doc ///
Key
  upperSubmodule
  (upperSubmodule, SubquotientWeylModuleFp)
Headline
  Numerator submodule in the presentation T/S
Description
  Text
    For @TEX "$M=T/S$"@ this returns the stored upper submodule
    @TEX "$T$"@ inside the reduced ambient Weyl module. Together with
    @TO lowerSubmodule@ it specifies the short exact sequence
    @TEX "$0\\to S\\to T\\to M\\to0$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    U = upperSubmodule M
    dim U
    dim M + dim lowerSubmodule M
  Text
    Both dimensions are 3. Here the upper submodule is the whole reduction
    of @TEX "$V_{\\mathbb Z}(2)$"@, and its dimension is the sum of the
    dimensions of the kernel and subquotient.
SeeAlso
  lowerSubmodule
  subquotientWeylModule
///

doc ///
Key
  subquotientProjection
  (subquotientProjection, SubquotientWeylModuleFp, List)
  [subquotientProjection, WeightFormat]
Headline
  Projection onto a subquotient weight space
Description
  Text
    For @TEX "$M=T/S$"@ the returned matrix uses ambient coordinates and
    restricts on @TEX "$T_\\mu$"@ to
    @TEX "$T_\\mu\\longrightarrow T_\\mu/S_\\mu$"@. Its kernel on
    @TEX "$T_\\mu$"@ is @TEX "$S_\\mu$"@.
    Its extension outside @TEX "$T_\\mu$"@ depends on the row reduction and
    has no intrinsic meaning as a quotient map. This is the subquotient form
    of @TO quotientProjection@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    subquotientProjection(M, {2})
    subquotientProjection(M, {0})
  Text
    At weight 2 the line survives, so the projection is the identity.
    At weight 0 the numerator equals the denominator, so the quotient
    weight space has dimension zero and the projection has no rows.
SeeAlso
  subquotientSection
  projectToSubquotient
///

doc ///
Key
  subquotientSection
  (subquotientSection, SubquotientWeylModuleFp, List)
  [subquotientSection, WeightFormat]
Headline
  Choose representatives of subquotient weight-space vectors
Description
  Text
    For @TEX "$M=T/S$"@ this returns the matrix of a chosen linear section
    @TEX "$M_\\mu\\longrightarrow T_\\mu$"@, expressed in ambient coordinates.
    Its columns are complementary columns from the stored numerator basis.
    The section is noncanonical and need not commute with the root operators.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    J = subquotientSection(M, {2})
    subquotientProjection(M, {2}) * J
  Text
    The product is the identity on the highest-weight line of @TT "M"@.
    Thus the chosen column represents a basis of that quotient weight space,
    although this is only a linear section.
SeeAlso
  subquotientProjection
  subquotientBasis
  liftFromSubquotient
///

doc ///
Key
  subquotientBasis
  (subquotientBasis, SubquotientWeylModuleFp, List)
  [subquotientBasis, WeightFormat]
Headline
  Ambient representatives of a subquotient weight-space basis
Description
  Text
    The columns are chosen vectors of @TEX "$T_\\mu$"@ whose classes form
    a basis of @TEX "$T_\\mu/S_\\mu$"@. The matrix equals
    @TO subquotientSection@. The representatives depend on the complement to
    @TEX "$S_\\mu$"@ inside @TEX "$T_\\mu$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    B = subquotientBasis(M, {2})
    subquotientProjection(M, {2}) * B
  Text
    The highest-weight representative maps to quotient coordinate 1.
    Its coset therefore spans the weight-2 space of the subquotient.
SeeAlso
  subquotientSection
  basisVecs
  weightSpace
///

doc ///
Key
  projectToSubquotient
  (projectToSubquotient, SubquotientWeylModuleFp, List, List)
  (projectToSubquotient, SubquotientWeylModuleFp, List, Matrix)
  [projectToSubquotient, WeightFormat]
Headline
  Take classes of numerator vectors in subquotient coordinates
Description
  Text
    This applies @TO subquotientProjection@ to a coefficient list or to
    a matrix of ambient-coordinate columns at one weight. For the result
    to represent quotient classes, the supplied columns must belong to
    @TEX "$T_\\mu$"@. Vectors of @TEX "$S_\\mu$"@ map to zero.
    The result is a matrix over the defining prime field.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    projectToSubquotient(M, {2}, {1})
    projectToSubquotient(M, {0}, {1})
  Text
    The highest-weight vector survives with coordinate 1. The weight-zero
    vector belongs to the denominator and hence has zero class.
SeeAlso
  subquotientProjection
  liftFromSubquotient
///

doc ///
Key
  liftFromSubquotient
  (liftFromSubquotient, SubquotientWeylModuleFp, List, List)
  (liftFromSubquotient, SubquotientWeylModuleFp, List, Matrix)
  [liftFromSubquotient, WeightFormat]
Headline
  Lift subquotient coordinates to chosen numerator representatives
Description
  Text
    This applies the chosen linear section to a coefficient list or a
    matrix of columns in a subquotient weight space. The output is in the
    original ambient coordinates and belongs to @TEX "$T_\\mu$"@.
    Changing the section changes each lift by an element of
    @TEX "$S_\\mu$"@; the section need not be a homomorphism of representations.
  Example
    W = weylModuleZ(cartanA 2, {2})
    S = socleWeyl(W, 2)
    T = subWeylModule(W, 2, {2}, {1})
    M = subquotientWeylModule(S, T)
    v = liftFromSubquotient(M, {2}, {1})
    projectToSubquotient(M, {2}, v)
  Text
    The representative is the highest-weight vector, and taking its class
    recovers coordinate 1. This checks the section identity at weight 2.
SeeAlso
  subquotientSection
  projectToSubquotient
///

doc ///
Key
  WeylCharacter
Headline
  A finite formal character together with its root datum
Description
  Text
    A @TO WeylCharacter@ is a finite sum
    @TEX "$\\sum_\\mu c_\\mu e^\\mu$"@ with integer coefficients and a specified
    Cartan matrix. Weights are stored in Dynkin coordinates. Its
    characteristic and original coordinate convention are also recorded.
    Addition and subtraction add and subtract coefficients; multiplication
    convolves weights and corresponds to tensor product for module characters.
    These operations require equal Cartan matrices. Equality compares the
    Cartan matrices and all coefficients, independently of the recorded
    characteristic or construction. A directly supplied sum need not be
    Weyl invariant or the character of a representation.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    characterTerms C
    characterTerms(C * C)
    isVirtualCharacter(C - 2 * C)
  Text
    The natural @TEX "$\\mathrm{SL}_2$"@ character is
    @TEX "$e^{\\omega_1}+e^{-\\omega_1}$"@. Its square is
    @TEX "$e^{2\\omega_1}+2e^0+e^{-2\\omega_1}$"@.
    The subtraction yields the negative of the natural character, so it is
    virtual.
SeeAlso
  weylCharacter
  characterTerms
  tensorProductCharacter
///

doc ///
Key
  weylCharacter
  (weylCharacter, Matrix, HashTable)
  (weylCharacter, WeylModuleZ)
  (weylCharacter, SubWeylModuleFp)
  (weylCharacter, QuotientWeylModuleFp)
  (weylCharacter, SubquotientWeylModuleFp)
  [weylCharacter, WeightFormat]
Headline
  Construct a formal character with a Cartan matrix
Description
  Text
    Applied to a module, this takes its weight multiplicities and ambient
    Cartan matrix. Applied to a Cartan matrix and hash table, it interprets
    the keys as weights in the selected @TO WeightFormat@ and the values as
    integer coefficients. Zero terms are omitted and the result is stored in
    Dynkin coordinates. A supplied sum need not be the character of a module.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    D = weylCharacter(cartanA 2, new HashTable from {{1} => 1, {-1} => 1})
    C == D
    characterTerms D
  Text
    The two constructions give the same formal sum
    @TEX "$e^{\\omega_1}+e^{-\\omega_1}$"@. Equality concerns that sum and its
    Cartan matrix, so it holds even though the directly supplied character
    has no recorded characteristic.
SeeAlso
  WeylCharacter
  rawCharacter
  characteristic
///

doc ///
Key
  coordinateConvention
  (coordinateConvention, WeylCharacter)
Headline
  Coordinates in which a formal character is stored
Description
  Text
    This returns @TT "\"Dynkin\""@ for every @TO WeylCharacter@.
    In these coordinates, a weight is the list of its pairings with the
    simple coroots. The input coordinates are recorded separately by
    @TO sourceWeightFormat@.
  Example
    C = weylCharacter(cartanA 3, new HashTable from {{1, 0, 0} => 1}, WeightFormat => "Partition")
    coordinateConvention C
    rawCharacter C
  Text
    The partition (1,0,0) becomes the Dynkin weight (1,0), namely
    @TEX "$\\omega_1$"@ of @TEX "$A_2$"@. The stored convention is therefore
    Dynkin even though the sum was supplied in partition coordinates.
SeeAlso
  sourceWeightFormat
  rawCharacter
  WeightFormat
///

doc ///
Key
  sourceWeightFormat
  (sourceWeightFormat, WeylCharacter)
Headline
  Coordinate convention supplied when constructing a character
Description
  Text
    This returns the weight format selected when @TO weylCharacter@
    was called. It records how the input was written; it does not change
    the internal Dynkin coordinates or determine the output format of
    @TO rawCharacter@. Character operations create sums in Dynkin coordinates.
  Example
    C = weylCharacter(cartanA 3, new HashTable from {{1, 0, 0} => 1}, WeightFormat => "Partition")
    sourceWeightFormat C
    coordinateConvention C
  Text
    The values are @TT "\"Partition\""@ and @TT "\"Dynkin\""@.
    The first describes the supplied weight (1,0,0); the second describes
    its stored Dynkin representative (1,0).
SeeAlso
  coordinateConvention
  weylCharacter
///

doc ///
Key
  isVirtualCharacter
  (isVirtualCharacter, WeylCharacter)
Headline
  Test whether a formal character has a negative coefficient
Description
  Text
    This is true exactly when at least one weight coefficient is negative.
    A nonnegative sum may still fail to be Weyl invariant or to be the
    character of a representation, so a false result makes no such assertion.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    isVirtualCharacter C
    isVirtualCharacter(C - 2 * C)
  Text
    The natural module character has two positive coefficients. Its
    negative has coefficient -1 at both weights, so the second character
    is virtual.
SeeAlso
  WeylCharacter
  characterTerms
  decomposeCharacter
///

doc ///
Key
  characterMetadata
  (characterMetadata, WeylCharacter)
Headline
  Coordinates, characteristic, and origin of a formal character
Description
  Text
    This returns a hash table recording the internal and supplied
    coordinate conventions, the characteristic when specified, whether a
    coefficient is negative, and the module or operation from which the
    character was obtained. These values describe the formal sum; they do not
    supply additional module structure.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    d = characterMetadata C
    d#"coordinateConvention", d#"characteristic", d#"isVirtual"
  Text
    The tuple says that the weights are stored in Dynkin coordinates,
    the character came from an integral module, and all coefficients are
    nonnegative.
SeeAlso
  coordinateConvention
  sourceWeightFormat
  characteristic
  isVirtualCharacter
///

doc ///
Key
  rawCharacter
  (rawCharacter, WeylCharacter)
  [rawCharacter, WeightFormat]
Headline
  Return a weight-multiplicity hash table from a formal character
Description
  Text
    This returns the nonzero terms as an ordinary hash table, with weights
    as keys and integer coefficients as values. The requested
    @TO WeightFormat@ determines the output coordinates; it does not alter
    the stored Dynkin-coordinate sum.
  Example
    C = weylCharacter weylModuleZ(cartanA 3, {1, 0})
    rawCharacter C
    rawCharacter(C, WeightFormat => "Partition")
  Text
    The three Dynkin weights are (1,0), (-1,1), and (0,-1), each once.
    Their partition-coordinate representatives are (1,0,0), (0,1,0), and
    (-1,-1,0). The last differs from the polynomial weight (0,0,1) by
    subtracting (1,1,1), which does not change its restriction to
    the diagonal torus of the special linear group. The representative
    convention sets the final coordinate to zero.
SeeAlso
  characterTerms
  weylCharacter
  WeightFormat
///

doc ///
Key
  characterTerms
  (characterTerms, WeylCharacter)
  [characterTerms, WeightFormat]
  [characterTerms, WeightOrder]
Headline
  List the nonzero terms of a formal character
Description
  Text
    This returns pairs @TT "(weight, coefficient)"@ for the nonzero terms,
    using the requested coordinates and @TO WeightOrder@.
    The default orders by root depth and then reverse lexicographic Dynkin
    coordinates. Each weight occurs once, even when its coefficient exceeds
    1.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    characterTerms(C * C)
  Text
    The list gives weight 2 once, weight 0 with multiplicity 2, and weight
    -2 once. It is the tensor-square character
    @TEX "$e^{2\\omega_1}+2e^0+e^{-2\\omega_1}$"@.
SeeAlso
  rawCharacter
  WeightOrder
  support
///

doc ///
Key
  (support, WeylCharacter)
Headline
  Weights with nonzero coefficients in a formal character
Description
  Text
    This returns the support of the formal sum in Dynkin coordinates,
    ordered by the default root-depth order. A weight with a negative
    coefficient is still in the support. Use @TO weights@ when a different
    coordinate convention or weight order is wanted.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    support(C * C)
  Text
    The support consists of 2, 0, and -2. The weight-zero coefficient is 2,
    but support lists only the weights, so it appears once.
SeeAlso
  characterTerms
  weights
  weightMultiplicity
///

doc ///
Key
  tensorProductCharacter
  (tensorProductCharacter, WeylCharacter, WeylCharacter)
Headline
  Multiply formal characters by convolution of weights
Description
  Text
    For @TEX "$C=\\sum_\\mu a_\\mu e^\\mu$"@ and
    @TEX "$D=\\sum_\\nu b_\\nu e^\\nu$"@, this returns
    @TEX "$\\sum_{\\mu,\\nu}a_\\mu b_\\nu e^{\\mu+\\nu}$"@.
    For characters of modules over the same field, this is the character
    of their tensor product. The Cartan matrices must agree.
    It also equals the product @TT "C * D"@.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    E = tensorProductCharacter(C, C)
    characterTerms E
    dim E
  Text
    The tensor square has weight multiplicities 1, 2, 1 at weights
    2, 0, -2, and total dimension 4. These are formal character data;
    they alone do not determine how the composition factors are joined.
SeeAlso
  tensorProductFactors
  productCharacter
  WeylCharacter
///

doc ///
Key
  tensorProductFactors
  (tensorProductFactors, WeylCharacter, WeylCharacter, ZZ)
  [tensorProductFactors, WeightFormat]
Headline
  Composition multiplicities of a tensor-product character
Description
  Text
    This multiplies the input characters and decomposes the resulting
    formal sum into simple characters in the specified prime characteristic.
    The result consists of highest-weight and multiplicity pairs.
    For actual module characters in that characteristic, these are the
    composition factors of the tensor product. The result is a
    Grothendieck-group identity and does not assert a direct-sum
    decomposition.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    tensorProductFactors(C, C, 2)
  Text
    The pairs ({2},1) and ({0},2) mean
    @TEX "$[E\\otimes E]=[L(2)]+2[L(0)]$"@ in characteristic 2, where
    @TEX "$E$"@ is the natural two-dimensional module.
    The dimensions are 2 and 1, so the multiplicities account for all four
    dimensions.
SeeAlso
  tensorProductCharacter
  decomposeCharacter
  compositionFactors
///

doc ///
Key
  dualCharacter
  (dualCharacter, WeylCharacter)
Headline
  Formal character of the contragredient dual
Description
  Text
    This sends @TEX "$\\sum_\\mu c_\\mu e^\\mu$"@ to
    @TEX "$\\sum_\\mu c_\\mu e^{-\\mu}$"@. For a module character it is the
    character of the linear dual representation. Coefficients and the
    recorded characteristic are preserved.
  Example
    C = weylCharacter weylModuleZ(cartanA 3, {1, 0})
    characterTerms dualCharacter C
  Text
    The weights are (0,1), (1,-1), and (-1,0). These are the negatives of
    the weights of the natural @TEX "$\\mathrm{SL}_3$"@ module. The maximal
    weight is @TEX "$\\omega_2$"@, as expected for its dual.
SeeAlso
  weylCharacter
  tensorProductCharacter
///

doc ///
Key
  frobeniusTwistCharacter
  (frobeniusTwistCharacter, WeylCharacter, ZZ)
  (frobeniusTwistCharacter, WeylCharacter, ZZ, ZZ)
Headline
  Scale weights by a prime power
Description
  Text
    For a prime @TT "p"@ and an integer @TEX "$e\\geq0$"@, this replaces
    each weight @TEX "$\\mu$"@ by @TEX "$p^e\\mu$"@ without changing its
    coefficient. For a module in characteristic @TT "p"@, this is the
    character of its @TEX "$e$"@th Frobenius twist. The two-argument form
    uses @TEX "$e=1$"@. A conflicting recorded positive characteristic is
    rejected. A character of an integral module may be used to specify the
    formal sum to twist after reduction.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    characterTerms frobeniusTwistCharacter(C, 2, 2)
    dim frobeniusTwistCharacter(C, 2, 2)
  Text
    The weights 1 and -1 become 4 and -4; the dimension stays 2.
    This is the character of the second Frobenius twist of the natural
    module in characteristic 2, whose highest weight is
    @TEX "$4\\omega_1$"@.
SeeAlso
  weylCharacter
  characteristic
  simpleCharacter
///

doc ///
Key
  DecompositionTable
Headline
  A decomposition matrix together with its row and column labels
Description
  Text
    An object of this type stores a nonnegative integer matrix, its
    row and column labels, the prime characteristic, and the algebra.
    An entry is read using the specified meanings of its row and column.
    The printed table aligns labels with entries. The labeling convention
    is retained so that a matrix can be interpreted as composition
    multiplicities.
  Example
    T = schurAlgebraDecompositionTable(2, 2, 2)
    T
  Text
    The row (2,0) has entries 1 and 1: the degree-two Weyl module for
    @TEX "$\\mathrm{GL}_2$"@ in characteristic 2 has factors
    @TEX "$L(2,0)$"@ and @TEX "$L(1,1)$"@ once each.
    The row (1,1) has only @TEX "$L(1,1)$"@. These rows describe
    composition factors, not splittings.
SeeAlso
  decompositionTable
  rowLabels
  columnLabels
  tableMetadata
///

doc ///
Key
  decompositionTable
  (decompositionTable, Matrix, List, List, HashTable)
Headline
  Attach representation labels to a decomposition matrix
Description
  Text
    This constructs a @TO DecompositionTable@ from a matrix over
    @TT "ZZ"@, distinct row labels, distinct column labels, and a hash table
    containing a prime under @TT "\"prime\""@ and an algebra name under
    @TT "\"algebra\""@. Entries must be nonnegative, and the number of
    labels must match the matrix dimensions. The constructor attaches the
    supplied interpretation; it does not verify that a proposed matrix is
    the decomposition matrix of the named algebra.
  Example
    M = matrix {{1, 1}, {0, 1}}
    T = decompositionTable(M, {{2, 0}, {1, 1}}, {{2, 0}, {1, 1}}, new HashTable from {"prime" => 2, "algebra" => "Schur algebra S(2,2)"})
    T
  Text
    With Weyl modules labeling rows and simple modules labeling columns,
    the first row expresses
    @TEX "$[\\Delta(2,0)]=[L(2,0)]+[L(1,1)]$"@.
    The second expresses @TEX "$[\\Delta(1,1)]=[L(1,1)]$"@.
    These meanings come from the chosen labels and algebra.
SeeAlso
  DecompositionTable
  schurAlgebraDecompositionTable
///

doc ///
Key
  rowLabels
  (rowLabels, DecompositionTable)
Headline
  Labels of the modules whose composition factors form the rows
Description
  Text
    This returns the row labels in matrix order. For Schur-algebra tables
    the rows are Weyl modules indexed by bounded partitions. For symmetric
    group tables they are dual Specht modules indexed by all partitions.
  Example
    T = schurAlgebraDecompositionTable(2, 2, 2)
    rowLabels T
  Text
    The rows are indexed by (2,0) and (1,1), the two partitions of 2 with
    at most two parts. Thus the first row concerns
    @TEX "$\\Delta(2,0)$"@ and the second @TEX "$\\Delta(1,1)$"@.
SeeAlso
  columnLabels
  tableMetadata
  schurAlgebraDecompositionTable
///

doc ///
Key
  columnLabels
  (columnLabels, DecompositionTable)
Headline
  Labels of simple modules forming the columns
Description
  Text
    This returns the column labels in matrix order. In Schur-algebra
    tables the simple polynomial modules are indexed by bounded partitions.
    In symmetric group tables the columns use p-restricted partitions in
    the dual-Specht convention.
  Example
    T = symmetricGroupDecompositionTable(2, 3)
    columnLabels T
  Text
    The columns are (2,1) and (1,1,1), the 2-restricted partitions of 3.
    They label the two simple modules in the convention used here.
    The partition (3) labels a row but is not a 2-restricted column label.
SeeAlso
  rowLabels
  pRestrictedPartitions
  symmetricGroupDecompositionTable
///

doc ///
Key
  prime
  (prime, DecompositionTable)
Headline
  Prime characteristic of a decomposition table
Description
  Text
    This returns the prime recorded for the decomposition multiplicities.
    The same partition labels can have different multiplicities in
    different characteristics, so the prime is part of the interpretation
    of the table.
  Example
    T = schurAlgebraDecompositionTable(2, 2, 2)
    prime T
  Text
    The value 2 places this table in characteristic 2. In particular,
    the two factors in the row (2,0) concern modular polynomial
    representations in that characteristic.
SeeAlso
  algebra
  tableMetadata
///

doc ///
Key
  algebra
  (algebra, DecompositionTable)
Headline
  Algebra named by a decomposition table
Description
  Text
    This returns the algebra name recorded with the table.
    The name distinguishes the representation category in which the
    row-to-column composition multiplicities are interpreted.
  Example
    T = schurAlgebraDecompositionTable(2, 2, 2)
    algebra T
  Text
    The table concerns the Schur algebra @TEX "$S(2,2)$"@, equivalently
    homogeneous polynomial representations of @TEX "$\\mathrm{GL}_2$"@
    of degree 2 in the recorded characteristic.
SeeAlso
  prime
  tableMetadata
  schurAlgebraDecompositionTable
///

doc ///
Key
  tableMetadata
  (tableMetadata, DecompositionTable)
Headline
  Characteristic and labeling convention of a decomposition table
Description
  Text
    This returns the recorded algebra, prime, and any additional supplied
    information. Tables constructed by this package record the family,
    degree, row meaning, and column meaning; symmetric group tables also
    state the p-restricted dual-Specht labeling convention.
  Example
    T = symmetricGroupDecompositionTable(2, 3)
    d = tableMetadata T
    d#"prime", d#"degree", d#"rowMeaning", d#"columnMeaning"
  Text
    The tuple identifies characteristic 2 and degree 3. Rows refer to dual
    Specht modules, while columns refer to simples labeled by 2-restricted
    partitions; this determines how each printed entry is read.
SeeAlso
  rowLabels
  columnLabels
  prime
  algebra
///

doc ///
Key
  (matrix, DecompositionTable)
Headline
  Underlying integer matrix of a decomposition table
Description
  Text
    This returns the integer matrix in the order specified by
    @TO rowLabels@ and @TO columnLabels@. The labels and recorded
    characteristic are required to interpret its entries as composition
    multiplicities.
  Example
    T = schurAlgebraDecompositionTable(2, 2, 2)
    matrix T
  Text
    The first row (1,1) belongs to partition (2,0), and the second row
    (0,1) belongs to partition (1,1). With columns (2,0) and (1,1), this
    says that the former Weyl module has both simple factors once, while
    the latter is simple.
SeeAlso
  DecompositionTable
  rowLabels
  columnLabels
///

doc ///
Key
  schurAlgebraDecompositionTable
  (schurAlgebraDecompositionTable, ZZ, ZZ, ZZ)
Headline
  Labelled decomposition matrix of a Schur algebra
Description
  Text
    For a prime @TT "p"@, a positive integer @TT "n"@, and degree
    @TEX "$r\\geq0$"@, this returns the decomposition table for
    @TEX "$S(n,r)$"@ in characteristic @TT "p"@. Both axes use partitions
    of @TT "r"@ with at most @TT "n"@ parts. Rows are Weyl modules and
    columns are simple polynomial modules. The entry at
    @TEX "$(\\lambda,\\mu)$"@ is
    @TEX "$[\\Delta(\\lambda):L(\\mu)]$"@.
  Example
    T = schurAlgebraDecompositionTable(2, 2, 2)
    T
    matrix T
  Text
    For @TEX "$S(2,2)$"@ in characteristic 2, the degree-two divided-power
    module has factors @TEX "$L(2,0)$"@ and the determinant module
    @TEX "$L(1,1)$"@, each once. The exterior-square Weyl module is the
    determinant module and is simple.
SeeAlso
  schurAlgebraDecompositionMatrix
  DecompositionTable
  symmetricGroupDecompositionTable
///

doc ///
Key
  symmetricGroupDecompositionTable
  (symmetricGroupDecompositionTable, ZZ, ZZ)
Headline
  Labelled decomposition matrix for a symmetric group
Description
  Text
    For a prime @TT "p"@ and @TEX "$n\\geq1$"@, this returns the
    decomposition table for @TEX "$k[\\mathfrak S_n]$"@ in characteristic
    @TT "p"@. Rows use all partitions of @TT "n"@ and represent dual Specht
    modules. Columns use p-restricted partitions and represent simple modules.
    This is the p-restricted dual-Specht convention used by
    @TO symmetricGroupDecompositionMatrix@.
  Example
    T = symmetricGroupDecompositionTable(2, 3)
    T
  Text
    The rows (3) and (1,1,1) both have the single factor labeled (1,1,1).
    In characteristic 2 the one-dimensional trivial and sign modules agree.
    The row (2,1) has only the simple factor labeled (2,1), the
    two-dimensional simple module.
SeeAlso
  symmetricGroupDecompositionMatrix
  pRestrictedPartitions
  columnLabels
///

doc ///
Key
  SocleAnalysis
Headline
  A computation of successive socle layers
Description
  Text
    A @TO SocleAnalysis@ records a filtration of a modular Weyl module or one
    of its quotients, together with the simple factors and dimensions of the
    successive layers. When the maximal-vector search is complete, this is the
    socle filtration @TEX "$0=\\operatorname{soc}^0M\\subseteq\\operatorname{soc}^1M\\subseteq\\cdots\\subseteq M$"@.
    The function @TO isCertified@ records whether the whole filtration was
    determined without ambiguous maximal-vector spaces or inconsistent dimensions.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    R
  Text
    For @TEX "$M=V_{\\mathbb F_2}(2)$"@ of @TEX "$\\mathrm{SL}_2$"@,
    the result has dimension three and socle length two. Its socle is @TEX "$L(0)$"@,
    and its quotient by the socle is @TEX "$L(2)$"@. The layers therefore have
    dimensions one and two.
SeeAlso
  socleAnalysis
  socleLayerFactors
  socleLayerDimensions
  isCertified
///

doc ///
Key
  socleAnalysis
  (socleAnalysis, WeylModuleZ, ZZ)
  (socleAnalysis, SubWeylModuleFp)
  (socleAnalysis, QuotientWeylModuleFp)
  [socleAnalysis, WeightFormat]
  [socleAnalysis, RequireComplete]
Headline
  Compute successive socle layers
Description
  Text
    @TT "socleAnalysis(W,p)"@ computes successive socle layers of
    @TEX "$V_{\\mathbb F_p}(\\lambda)$"@. A quotient argument gives the
    layers of that quotient. A submodule argument @TT "S"@ specifies
    @TEX "$V/S$"@; the cumulative terms are returned as their preimages in @TEX "$V$"@.
  Text
    In each successive quotient the algorithm finds maximal vectors whose
    generated submodules are simple. If a maximal-vector space has dimension
    greater than one, testing its chosen basis directions need not find every
    simple submodule. Such weights are recorded by @TO ambiguousWeightsByLayer@.
    The filtration found may then be smaller than the socle filtration, even if
    it eventually reaches the whole module.
  Text
    @TT "RequireComplete => true"@ raises an error if the search stalls,
    a maximal-vector space is ambiguous, or the proposed simple-factor dimensions
    do not sum to the dimension of their layer. Without this option the computed
    filtration is returned, together with the reasons that completeness is uncertain.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleLayerFactors R
    socleLayerDimensions R
    cumulativeSocleDimensions R
    isCertified R
  Text
    The first layer is one copy of @TEX "$L(0)$"@ and the second is one
    copy of @TEX "$L(2)$"@. Their dimensions are @TEX "$1$"@ and @TEX "$2$"@,
    while the cumulative dimensions are @TEX "$1$"@ and @TEX "$3$"@.
    The final @TT "true"@ says that the full socle filtration was determined.
SeeAlso
  SocleAnalysis
  socleSeries
  socleWeyl
  maximalVectors
  socleDiagram
///

doc ///
Key
  RequireComplete
Headline
  Require a complete socle computation
Description
  Text
    This option requires the socle search to reach the whole module with no
    multidimensional maximal-vector space and with the expected dimensions of
    all simple layers. It is accepted by @TO socleAnalysis@, @TO extensionData@,
    @TO socleWeyl@, @TO socleSeries@, @TO socleLengthTwoQuotient@, and
    @TO twoFactorQuotientsContaining@. The default is @TT "true"@ for
    @TO extensionData@ and @TT "false"@ for @TO socleAnalysis@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    isCertified R
  Text
    The value @TT "true"@ confirms that the two layers of
    @TEX "$V_{\\mathbb F_2}(2)$"@ were determined completely. Had one of the
    successive quotients contained an ambiguous maximal-vector space, the call
    with this option would have raised an error.
SeeAlso
  socleAnalysis
  isCertified
///

doc ///
Key
  socleLayerFactors
  (socleLayerFactors, SocleAnalysis)
  (socleLayerFactors, SocleAnalysis, ZZ)
Headline
  Simple factors in each computed socle layer
Description
  Text
    Return the multiplicities of the simple modules in each computed layer.
    An entry @TEX "$\\mu\\Rightarrow m$"@ denotes
    @TEX "$L(\\mu)^{\\oplus m}$"@. With a second argument @TT "j"@, return the
    factor table for layer @TT "j"@, numbering layers from one. For a complete
    computation this is @TEX "$\\operatorname{soc}^jM/\\operatorname{soc}^{j-1}M$"@.
    Otherwise it describes the semisimple layer found by the search.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleLayerFactors R
    socleLayerFactors(R, 1)
  Text
    The first table contains @TT "{0} => 1"@ and the second
    @TT "{2} => 1"@: the layers are @TEX "$L(0)$"@ and @TEX "$L(2)$"@.
    The final table selects only the socle. These are direct-sum decompositions
    within the semisimple layers; they do not give a direct-sum decomposition of @TEX "$M$"@.
SeeAlso
  socleAnalysis
  socleLayerDimensions
  socleDiagram
///

doc ///
Key
  socleLayerGenerators
  (socleLayerGenerators, SocleAnalysis)
Headline
  Generators of the computed socle layers
Description
  Text
    Return, for each layer, the maximal vectors used to generate its simple
    summands in the appropriate successive quotient. Each generator is given
    by its weight and a coefficient vector in that quotient's chosen weight-space
    basis. The coordinates of later layers are quotient coordinates, rather
    than coordinates in the original module.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleLayerGenerators R
  Text
    The first list gives a weight-zero vector generating the one-dimensional
    socle @TEX "$L(0)$"@. The second gives a weight-two vector in
    @TEX "$M/\\operatorname{soc}M$"@ generating the simple quotient @TEX "$L(2)$"@.
    The two generator lists refer to different modules.
SeeAlso
  socleAnalysis
  cumulativeSocleLayers
  maximalVectors
///

doc ///
Key
  cumulativeSocleLayers
  (cumulativeSocleLayers, SocleAnalysis)
Headline
  Submodules forming the computed socle filtration
Description
  Text
    Return the successive cumulative submodules inside the ambient Weyl
    module. For a computation on @TEX "$V/S$"@, these are the inverse images
    @TEX "$T_j\\subseteq V$"@ of the computed layers, so every @TEX "$T_j$"@
    contains @TEX "$S$"@. Their dimensions as submodules of @TEX "$V$"@ include
    @TEX "$\\dim S$"@; @TO cumulativeSocleDimensions@ instead gives
    @TEX "$\\dim(T_j/S)$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    cumulativeSocleLayers R
    apply(cumulativeSocleLayers R, dim)
  Text
    Here the initial kernel is zero. The returned submodules therefore
    are @TEX "$\\operatorname{soc}M\\subseteq M$"@ themselves, of dimensions
    one and three. Their successive quotient dimensions are one and two.
SeeAlso
  socleAnalysis
  cumulativeSocleDimensions
  socleLayerDimensions
///

doc ///
Key
  socleLayerDimensions
  (socleLayerDimensions, SocleAnalysis)
Headline
  Dimensions of the successive computed socle layers
Description
  Text
    Return the dimensions of the semisimple layers, rather than those of
    the cumulative submodules. For a complete computation the @TT "j"@th entry
    is @TEX "$\\dim(\\operatorname{soc}^jM/\\operatorname{soc}^{j-1}M)$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleLayerDimensions R
  Text
    The list @TT "{1, 2}"@ gives the dimensions of @TEX "$L(0)$"@ and
    @TEX "$L(2)$"@ in the two layers of @TEX "$V_{\\mathbb F_2}(2)$"@.
    Their sum is the dimension three of the module.
SeeAlso
  socleLayerFactors
  cumulativeSocleDimensions
///

doc ///
Key
  cumulativeSocleDimensions
  (cumulativeSocleDimensions, SocleAnalysis)
Headline
  Dimensions reached by the computed socle filtration
Description
  Text
    Return the dimensions of the cumulative terms in the module being
    analyzed. If that module is @TEX "$V/S$"@ and the returned preimages are
    @TEX "$T_j\\subseteq V$"@, the entries are @TEX "$\\dim(T_j/S)$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    cumulativeSocleDimensions R
  Text
    The list @TT "{1, 3}"@ says that the socle has dimension one and
    the next term is the full three-dimensional module. Subtracting consecutive
    dimensions gives layer dimensions one and two.
SeeAlso
  cumulativeSocleLayers
  socleLayerDimensions
///

doc ///
Key
  successiveQuotientDimensions
  (successiveQuotientDimensions, SocleAnalysis)
Headline
  Dimensions of the quotients in which socles are sought
Description
  Text
    Return the dimension of each successive quotient before its socle is
    computed. If the filtration terms are @TEX "$T_0=0,T_1,\\ldots$"@ inside
    @TEX "$M$"@, the entries are @TEX "$\\dim(M/T_{j-1})$"@. No terminal zero
    is appended after the search reaches the whole module.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    successiveQuotientDimensions R
  Text
    The list @TT "{3, 2}"@ records the original module
    @TEX "$V_{\\mathbb F_2}(2)$"@ of dimension three, followed by its
    two-dimensional quotient by @TEX "$L(0)$"@.
SeeAlso
  socleAnalysis
  socleLayerDimensions
///

doc ///
Key
  ambiguousWeights
  (ambiguousWeights, SocleAnalysis)
Headline
  Weights with multidimensional maximal-vector spaces
Description
  Text
    Return all weights at which a maximal-vector space of dimension greater
    than one occurs in any successive quotient. The algorithm tests a chosen
    basis of each such space; another linear combination may generate a simple
    submodule that is not found by those tests. Thus a nonempty result prevents
    the computation from asserting that the full socle filtration was found.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    ambiguousWeights R
  Text
    The empty list means that every maximal-vector space encountered in
    this computation is at most one-dimensional. In this example the search also
    reaches the whole module with the correct dimensions, so it determines the
    full socle filtration.
SeeAlso
  ambiguousWeightsByLayer
  isCertified
  maximalVectors
///

doc ///
Key
  ambiguousWeightsByLayer
  (ambiguousWeightsByLayer, SocleAnalysis)
Headline
  Ambiguous maximal-vector weights in each successive quotient
Description
  Text
    Return one list for each stage of the socle search. The @TT "j"@th
    list contains the weights whose maximal-vector spaces have dimension greater
    than one in the quotient in which the @TT "j"@th layer is sought. A weight
    may appear at several stages.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    ambiguousWeightsByLayer R
  Text
    The two empty lists refer to @TEX "$M$"@ and
    @TEX "$M/\\operatorname{soc}M$"@. Neither quotient has a multidimensional
    maximal-vector space, so neither stage has this source of ambiguity.
SeeAlso
  ambiguousWeights
  analysisIssues
  socleAnalysis
///

doc ///
Key
  analysisIssues
  (analysisIssues, SocleAnalysis)
Headline
  Reasons a socle computation may be incomplete
Description
  Text
    Return the explanations recorded by the socle search: ambiguous
    maximal-vector spaces, failure to find a simple socle generator in a nonzero
    quotient, failure to enlarge the filtration, or a discrepancy between layer
    dimensions and the dimensions of their proposed simple factors.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    analysisIssues R
  Text
    The empty list says that none of these obstructions occurred for
    @TEX "$V_{\\mathbb F_2}(2)$"@. Combined with the fact that the filtration
    reaches the whole module, it gives @TT "isCertified R == true"@.
SeeAlso
  isCertified
  socleAnalysisComplete
  ambiguousWeights
///

doc ///
Key
  socleAnalysisPrime
  (socleAnalysisPrime, SocleAnalysis)
Headline
  Characteristic of a socle computation
Description
  Text
    Return the prime @TEX "$p$"@ over which the socle layers were computed.
    This is the characteristic of every submodule and quotient in the analysis.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleAnalysisPrime R
  Text
    The returned prime is two, so the layers belong to
    @TEX "$V_{\\mathbb F_2}(2)$"@. They do not describe the irreducible
    characteristic-zero module @TEX "$V_{\\mathbb Q}(2)$"@.
SeeAlso
  socleAnalysis
  socleAnalysisModuleDimension
///

doc ///
Key
  socleAnalysisModuleDimension
  (socleAnalysisModuleDimension, SocleAnalysis)
Headline
  Dimension of the module whose socle is analyzed
Description
  Text
    Return the dimension of the module being analyzed. A submodule argument
    @TT "S"@ to @TO socleAnalysis@ analyzes @TEX "$V/S$"@, so this function then
    returns @TEX "$\\dim V-\\dim S$"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleAnalysisModuleDimension R
  Text
    The value three is @TEX "$\\dim V_{\\mathbb F_2}(2)$"@. It equals
    the sum of the two layer dimensions @TEX "$1+2$"@.
SeeAlso
  socleAnalysis
  socleLayerDimensions
///

doc ///
Key
  socleAnalysisComplete
  (socleAnalysisComplete, SocleAnalysis)
Headline
  Whether the computed filtration reaches the whole module
Description
  Text
    Return @TT "true"@ when the computed filtration reaches the whole module.
    This alone does not assert that its terms are the full socle filtration:
    the search can reach the whole module even when multidimensional maximal-vector
    spaces make individual layers uncertain. Use @TO isCertified@ to include
    those conditions.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleAnalysisComplete R
    isCertified R
  Text
    Both values are @TT "true"@: the two computed layers exhaust
    @TEX "$V_{\\mathbb F_2}(2)$"@ and each is the full socle of the appropriate
    successive quotient.
SeeAlso
  socleAnalysis
  isCertified
///

doc ///
Key
  socleLength
  (socleLength, SocleAnalysis)
  (socleLength, WeylModuleZ, ZZ)
  (socleLength, SubWeylModuleFp)
  (socleLength, QuotientWeylModuleFp)
Headline
  Number of computed socle layers
Description
  Text
    Return the number of layers in a @TO SocleAnalysis@. With a Weyl module
    and a prime, or with a quotient, first perform @TO socleAnalysis@. A submodule
    @TT "S"@ again specifies @TEX "$V/S$"@. If @TO isCertified@ is true, this
    is the socle length of the module; otherwise it is the length of the filtration
    found by the search.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    socleLength R
    socleLength(W, 2)
  Text
    Both calls return two. The socle is @TEX "$L(0)$"@, and the quotient
    by it is the simple module @TEX "$L(2)$"@; two successive socle steps
    therefore exhaust @TEX "$V_{\\mathbb F_2}(2)$"@.
SeeAlso
  socleAnalysis
  socleLayerFactors
  isCertified
///

doc ///
Key
  isCertified
  (isCertified, SocleAnalysis)
  (isCertified, ExtensionData)
  (isCertified, SocleDiagram)
Headline
  Whether the full socle filtration was determined
Description
  Text
    For a @TO SocleAnalysis@, return @TT "true"@ exactly when the computed
    filtration reaches the whole module, no successive quotient has an ambiguous
    multidimensional maximal-vector space, and the dimensions of the simple factors
    sum to the dimension of every layer. For @TO ExtensionData@ and
    @TO SocleDiagram@, return the same conclusion from their underlying socle
    analysis.
  Example
    W = weylModuleZ(cartanA 2, {2})
    R = socleAnalysis(W, 2, RequireComplete => true)
    isCertified R
    isCertified extensionData(W, 2)
    isCertified socleDiagram R
  Text
    All three values are @TT "true"@ because they refer to the same
    complete socle filtration @TEX "$0\\subset L(0)\\subset V_{\\mathbb F_2}(2)$"@.
    This conclusion determines the layers; it does not determine extension arrows
    between individual factors in a general diagram.
SeeAlso
  socleAnalysisComplete
  analysisIssues
  ambiguousWeights
///

doc ///
Key
  compositionFactors
  (compositionFactors, WeylModuleZ, ZZ)
  (compositionFactors, SubWeylModuleFp)
  (compositionFactors, QuotientWeylModuleFp)
  (compositionFactors, SubquotientWeylModuleFp)
  [compositionFactors, WeightFormat]
Headline
  Highest weights and multiplicities of the composition factors
Description
  Text
    Return an ordered list of @TT "(highestWeight,multiplicity)"@ pairs.
    An entry @TEX "$(\\mu,m)$"@ denotes @TEX "$m$"@ occurrences of
    @TEX "$L(\\mu)$"@ in a composition series. Maximal weights precede lower
    weights, with lexicographic order among incomparable weights. An integral
    Weyl module requires an explicit prime; modular submodules, quotients, and
    subquotients already carry their characteristic.
  Example
    W = weylModuleZ(cartanA 2, {2})
    compositionFactors(W, 2)
  Text
    The pairs @TT "({2}, 1)"@ and @TT "({0}, 1)"@ give one copy of
    @TEX "$L(2)$"@ and one copy of @TEX "$L(0)$"@. Their dimensions are two
    and one. The list specifies composition multiplicities and does not imply
    that their nonsplit extension is a direct sum.
SeeAlso
  decompositionNumbers
  isIrreducible
  socleAnalysis
///

doc ///
Key
  isIrreducible
  (isIrreducible, WeylModuleZ, ZZ)
  (isIrreducible, SubWeylModuleFp)
  (isIrreducible, QuotientWeylModuleFp)
  (isIrreducible, SubquotientWeylModuleFp)
Headline
  Test whether a nonzero module is simple
Description
  Text
    Return @TT "true"@ precisely when the module is nonzero and its
    composition length is one. This uses the decomposition of its formal character
    into simple characters. The zero module returns @TT "false"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    isIrreducible(W, 2)
    isIrreducible(W, 3)
    isIrreducible simpleQuotient(W, 2)
  Text
    In characteristic two, @TEX "$V(2)$"@ has the two factors
    @TEX "$L(2)$"@ and @TEX "$L(0)$"@, so the first value is @TT "false"@.
    In characteristic three, @TEX "$V(2)=L(2)$"@, so the second is
    @TT "true"@. The last value is @TT "true"@ for the simple quotient
    @TEX "$L(2)$"@ in characteristic two.
SeeAlso
  compositionFactors
  simpleDimension
  simpleQuotient
///

doc ///
Key
  ExtensionData
Headline
  Composition factors and socle layers of a supplied extension
Description
  Text
    An @TO ExtensionData@ describes a Weyl module reduction or a quotient:
    its composition length, highest-weight head factor, bottom factors, and
    successive socle layers. The construction is @TO extensionData@.
    The field @TT "\"isNonSplit\""@ records the same two-factor conclusion as
    @TO isTwoFactorExtension@; it does not decide splitting for arbitrary
    composition length.
  Example
    W = weylModuleZ(cartanA 2, {2})
    E = extensionData(W, 2)
    E#"compositionFactors"
    E#"bottomFactors"
  Text
    The module has composition length two. The bottom factor is @TEX "$L(0)$"@
    and the other factor is @TEX "$L(2)$"@, giving
    @TEX "$0\\longrightarrow L(0)\\longrightarrow V_{\\mathbb F_2}(2)\\longrightarrow L(2)\\longrightarrow0$"@.
    The socle length two shows that this exact sequence does not split.
SeeAlso
  extensionData
  isTwoFactorExtension
  socleAnalysis
///

doc ///
Key
  extensionData
  (extensionData, WeylModuleZ, ZZ)
  (extensionData, SubWeylModuleFp)
  (extensionData, QuotientWeylModuleFp)
  [extensionData, WeightFormat]
  [extensionData, RequireComplete]
Headline
  Describe the simple factors and socle filtration of an extension
Description
  Text
    Compute composition factors, composition length, and socle layers of the
    supplied modular quotient. The arguments @TT "(W,p)"@ give
    @TEX "$V_{\\mathbb F_p}(\\lambda)$"@. A submodule argument @TT "S"@ gives
    @TEX "$V/S$"@. The default @TT "RequireComplete => true"@ raises an error
    if the full socle filtration cannot be determined. This describes an individual
    module and does not compute an entire @TEX "$\\operatorname{Ext}^1$"@ group.
  Example
    W = weylModuleZ(cartanA 2, {2})
    E = extensionData(W, 2)
    E#"topFactor"
    E#"bottomFactors"
    isTwoFactorExtension E
  Text
    The head has highest weight two and the socle is one copy of @TEX "$L(0)$"@.
    The final @TT "true"@ identifies a nonsplit extension of @TEX "$L(2)$"@
    by @TEX "$L(0)$"@: its composition length and socle length are both two.
SeeAlso
  ExtensionData
  isTwoFactorExtension
  socleAnalysis
///

doc ///
Key
  isTwoFactorExtension
  (isTwoFactorExtension, ExtensionData)
Headline
  Recognize a nonsplit extension with two simple factors
Description
  Text
    Return @TT "true"@ when the module has composition length two, its
    socle filtration is fully determined, its socle is simple, and its socle length
    is two. Such a module is a nonsplit extension: a direct sum of the two simple
    factors would be semisimple and have socle length one.
  Example
    W = weylModuleZ(cartanA 2, {2})
    isTwoFactorExtension extensionData(W, 2)
    isTwoFactorExtension extensionData(simpleQuotient(W, 2))
  Text
    The first value is @TT "true"@ for the nonsplit extension with socle
    @TEX "$L(0)$"@ and quotient @TEX "$L(2)$"@. The second is @TT "false"@
    because the simple quotient @TEX "$L(2)$"@ has composition length one.
SeeAlso
  extensionData
  compositionFactors
  socleLength
///

doc ///
Key
  ChevalleyActionData
Headline
  The Chevalley generators in a common basis
Description
  Text
    A @TO ChevalleyActionData@ contains the matrices of the simple
    @TEX "$e_i,f_i,h_i$"@ in a single basis for the whole module. The basis
    concatenates the chosen weight-space bases in root-depth order, starting
    with the highest weight. Matrix columns are images of the corresponding
    basis vectors. Construct these matrices with @TO chevalleyActionMatrices@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = chevalleyActionMatrices W
    D
    globalBasis D
  Text
    For @TEX "$V_{\\mathbb Z}(2)$"@ the basis labels have weights
    @TEX "$2,0,-2$"@, each with local index zero. Thus every action matrix
    is a three-by-three matrix, with the same row and column order.
SeeAlso
  chevalleyActionMatrices
  raisingMatrices
  loweringMatrices
  cartanMatrices
///

doc ///
Key
  chevalleyActionMatrices
  (chevalleyActionMatrices, WeylModuleZ)
  (chevalleyActionMatrices, SubWeylModuleFp)
  (chevalleyActionMatrices, QuotientWeylModuleFp)
  (chevalleyActionMatrices, SubquotientWeylModuleFp)
Headline
  Matrices of the simple Chevalley generators on the whole module
Description
  Text
    Return square matrices for @TEX "$e_i,f_i,h_i$"@, for all simple roots,
    on an integral Weyl module or a modular submodule, quotient, or subquotient.
    The common basis concatenates the chosen weight-space bases in root-depth
    order. The functions @TO raisingMatrices@, @TO loweringMatrices@,
    @TO cartanMatrices@, and @TO globalBasis@ give the separate lists and basis
    labels. The coefficient ring is @TEX "$\\mathbb Z$"@ for an integral
    module and the module's prime field otherwise.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = chevalleyActionMatrices W
    raisingMatrices D
    loweringMatrices D
    cartanMatrices D
  Text
    Writing @TEX "$v_j=f^{(j)}v_0$"@, the basis is
    @TEX "$(v_0,v_1,v_2)$"@. The matrices say
    @TEX "$ev_1=2v_0$"@, @TEX "$ev_2=v_1$"@,
    @TEX "$fv_0=v_1$"@, and @TEX "$fv_1=2v_2$"@.
    The Cartan matrix is diagonal with entries @TEX "$2,0,-2$"@,
    the weights of these vectors.
SeeAlso
  ChevalleyActionData
  globalBasis
  dividedPowerActionE
  dividedPowerActionF
///

doc ///
Key
  raisingMatrices
  (raisingMatrices, ChevalleyActionData)
Headline
  Matrices of the simple raising operators
Description
  Text
    Return the list of matrices for @TEX "$e_1,\\ldots,e_r$"@ in the
    basis given by @TO globalBasis@. The @TT "i"@th simple-root operator
    appears at list index @TT "i-1"@. Columns describe images of basis vectors.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = chevalleyActionMatrices W
    raisingMatrices D
  Text
    The single matrix is the action of @TEX "$e$"@ on
    @TEX "$V_{\\mathbb Z}(2)$"@. Its nonzero entries express
    @TEX "$ev_1=2v_0$"@ and @TEX "$ev_2=v_1$"@; the highest-weight vector
    @TEX "$v_0$"@ is annihilated.
SeeAlso
  chevalleyActionMatrices
  globalBasis
  dividedPowerActionE
///

doc ///
Key
  loweringMatrices
  (loweringMatrices, ChevalleyActionData)
Headline
  Matrices of the simple lowering operators
Description
  Text
    Return the list of matrices for @TEX "$f_1,\\ldots,f_r$"@ in the
    basis given by @TO globalBasis@. The @TT "i"@th simple-root operator
    appears at list index @TT "i-1"@. Columns describe images of basis vectors.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = chevalleyActionMatrices W
    loweringMatrices D
  Text
    The matrix expresses @TEX "$fv_0=v_1$"@ and @TEX "$fv_1=2v_2$"@,
    with @TEX "$fv_2=0$"@. The coefficient two explains why the second lowering
    map vanishes after reduction modulo two.
SeeAlso
  chevalleyActionMatrices
  globalBasis
  dividedPowerActionF
///

doc ///
Key
  cartanMatrices
  (cartanMatrices, ChevalleyActionData)
Headline
  Matrices of the simple coroots
Description
  Text
    Return the matrices for @TEX "$h_1,\\ldots,h_r$"@ in the common
    weight basis. On a weight space @TEX "$M_\\mu$"@,
    @TEX "$h_i$"@ acts as @TEX "$\\langle\\mu,\\alpha_i^\\vee\\rangle$"@
    times the identity. These scalars are the fundamental-weight coordinates
    of @TEX "$\\mu$"@, reduced in the prime field for a modular module.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = chevalleyActionMatrices W
    cartanMatrices D
  Text
    The diagonal entries @TEX "$2,0,-2$"@ are the three weights of
    @TEX "$V_{\\mathbb Z}(2)$"@. All become zero modulo two, although the
    rational group module still has three distinct weights.
SeeAlso
  chevalleyActionMatrices
  globalBasis
  binomialCartanAction
///

doc ///
Key
  globalBasis
  (globalBasis, ChevalleyActionData)
Headline
  Weight and local index of each vector in the common basis
Description
  Text
    Return the ordered list of pairs @TT "(weight,localIndex)"@ specifying
    the row and column order of the Chevalley matrices. Local indices start at
    zero within each chosen weight-space basis. Weight spaces are listed in
    root-depth order, and the weight coordinates are Dynkin coordinates.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = chevalleyActionMatrices W
    globalBasis D
  Text
    The labels @TT "({2},0)"@, @TT "({0},0)"@, and @TT "({-2},0)"@
    represent the unique chosen basis vector in each rank-one weight space.
    Consequently row three and column one of an action matrix describe the
    coefficient from weight two to weight minus two.
SeeAlso
  chevalleyActionMatrices
  weightBasis
  dividedPowerActionF
///

doc ///
Key
  dividedPowerActionE
  (dividedPowerActionE, WeylModuleZ, ZZ, ZZ)
  (dividedPowerActionE, SubWeylModuleFp, ZZ, ZZ)
  (dividedPowerActionE, QuotientWeylModuleFp, ZZ, ZZ)
  (dividedPowerActionE, SubquotientWeylModuleFp, ZZ, ZZ)
Headline
  Matrix of a simple raising divided power
Description
  Text
    @TT "dividedPowerActionE(M,i,n)"@ gives the matrix of
    @TEX "$e_i^{(n)}=e_i^n/n!$"@ for a nonnegative integer @TEX "$n$"@.
    It first computes the integral operator and then induces its action on
    the supplied modular submodule, quotient, or subquotient. Thus it is defined
    even when @TEX "$n!$"@ vanishes in the prime field. The basis order agrees
    with @TO chevalleyActionMatrices@; exponent zero gives the identity.
  Example
    W = weylModuleZ(cartanA 2, {2})
    M = reduction(W, 2)
    dividedPowerActionE(M, 1, 2)
  Text
    In the weight order @TEX "$2,0,-2$"@, the nonzero entry is in
    row one and column three: @TEX "$e^{(2)}v_2=v_0$"@. The ordinary square
    @TEX "$e^2$"@ vanishes in characteristic two, so this matrix cannot be
    recovered by division in @TEX "$\\mathbb F_2$"@.
SeeAlso
  dividedPowerActionF
  raisingMatrices
  globalBasis
///

doc ///
Key
  dividedPowerActionF
  (dividedPowerActionF, WeylModuleZ, ZZ, ZZ)
  (dividedPowerActionF, SubWeylModuleFp, ZZ, ZZ)
  (dividedPowerActionF, QuotientWeylModuleFp, ZZ, ZZ)
  (dividedPowerActionF, SubquotientWeylModuleFp, ZZ, ZZ)
Headline
  Matrix of a simple lowering divided power
Description
  Text
    @TT "dividedPowerActionF(M,i,n)"@ gives the matrix of
    @TEX "$f_i^{(n)}=f_i^n/n!$"@ for a nonnegative integer @TEX "$n$"@.
    The operator is computed integrally before reduction, so the formula remains
    meaningful when @TEX "$n!$"@ is zero in the prime field. It is then restricted
    or induced on the supplied modular module. Exponent zero gives the identity,
    and the basis order is that of @TO chevalleyActionMatrices@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    M = reduction(W, 2)
    dividedPowerActionF(M, 1, 2)
  Text
    The entry in row three and column one is one: the divided power
    takes the highest-weight vector @TEX "$v_0$"@ to the lowest-weight vector
    @TEX "$v_2$"@. This operator is nonzero although @TEX "$f^2=0$"@ in
    characteristic two.
SeeAlso
  dividedPowerActionE
  loweringMatrices
  weightGraph
///

doc ///
Key
  binomialCartanAction
  (binomialCartanAction, WeylModuleZ, ZZ, ZZ)
  (binomialCartanAction, SubWeylModuleFp, ZZ, ZZ)
  (binomialCartanAction, QuotientWeylModuleFp, ZZ, ZZ)
  (binomialCartanAction, SubquotientWeylModuleFp, ZZ, ZZ)
Headline
  Matrix of a binomial coroot operator
Description
  Text
    @TT "binomialCartanAction(M,i,n)"@ gives the Kostant-form operator
    @TEX "$\\binom{h_i}{n}$"@ for @TEX "$n\\geq0$"@. On a weight
    @TEX "$\\mu$"@ it acts by the integer
    @TEX "$\\binom{\\langle\\mu,\\alpha_i^\\vee\\rangle}{n}$"@,
    including the usual generalized binomial coefficients for negative integers.
    These integers are reduced in the prime field for a modular module. The basis
    order agrees with @TO chevalleyActionMatrices@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    binomialCartanAction(W, 1, 2)
    binomialCartanAction(reduction(W, 2), 1, 2)
  Text
    The integral diagonal is @TEX "$1,0,3$"@, since
    @TEX "$\\binom{2}{2}=1$"@, @TEX "$\\binom{0}{2}=0$"@, and
    @TEX "$\\binom{-2}{2}=3$"@. Modulo two it becomes @TEX "$1,0,1$"@.
    In particular this operator distinguishes the weight-zero space even though
    the ordinary @TEX "$h$"@ acts as zero on the entire reduction.
SeeAlso
  cartanMatrices
  dividedPowerActionE
  dividedPowerActionF
///

doc ///
Key
  WeylModuleCheck
Headline
  Results of checks on a Weyl-module representation
Description
  Text
    A @TO WeylModuleCheck@ contains the results of dimension and action
    checks, together with any failures. The functions @TO checkWeylModule@
    and @TO verifyRepresentation@ produce these objects.
    @TO verificationChecks@ lists the assertions and their truth values,
    and @TO verificationFailures@ gives the discrepancies.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    C = checkWeylModule(W, CheckLevel => "Serre")
    C
  Text
    The result states that the requested checks passed for the
    eight-dimensional type @TEX "$A_2$"@ module of highest weight
    @TEX "$\\omega_1+\\omega_2$"@. At the Serre level these include the
    Chevalley--Serre relations on the stored matrices.
SeeAlso
  checkWeylModule
  verifyRepresentation
  isValid
///

doc ///
Key
  checkWeylModule
  (checkWeylModule, WeylModuleZ)
  (checkWeylModule, SubWeylModuleFp)
  (checkWeylModule, QuotientWeylModuleFp)
  (checkWeylModule, SubquotientWeylModuleFp)
  [checkWeylModule, CheckLevel]
Headline
  Check dimensions and the Chevalley action of a module
Description
  Text
    Return a @TO WeylModuleCheck@ for an integral or modular module.
    At @TT "CheckLevel => \"Basic\""@, check the character dimension,
    matrix dimensions, and integral coefficients where applicable. Stored action
    blocks must have valid weights and root indices, the correct source and
    target dimensions, and coefficients in the stated ring. The Cartan action
    on @TEX "$M_\\mu$"@ must be
    @TEX "$\\langle\\mu,\\alpha_i^\\vee\\rangle\\operatorname{id}$"@.
    The weightwise @TO actionE@ and @TO actionF@ are compared with the
    corresponding blocks in the full matrices, including zero adjacent weight spaces.
  Text
    The level @TT "\"Relations\""@ also checks
    @TEX "$[h_i,h_j]=0$"@,
    @TEX "$[e_i,f_j]=\\delta_{ij}h_i$"@,
    @TEX "$[h_i,e_j]=a_{ij}e_j$"@, and
    @TEX "$[h_i,f_j]=-a_{ij}f_j$"@.
    The level @TT "\"Serre\""@ additionally checks the positive and negative
    Serre relations.
  Text
    For a submodule, check that the Chevalley maps preserve its weight spaces.
    For a quotient or subquotient, compare the stored actions with those induced
    from the ambient integral module and check dimension additivity in the
    defining exact sequence. These checks concern the Chevalley generators; in
    positive characteristic they are not by themselves a verification of every
    higher divided-power relation.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    C = checkWeylModule(W, CheckLevel => "Serre")
    isValid C
    verificationFailures C
  Text
    The values @TT "true"@ and @TT "{}"@ mean that all requested identities
    hold for @TEX "$V_{\\mathbb Z}(\\omega_1+\\omega_2)$"@ and that no
    dimension or matrix discrepancy was found.
SeeAlso
  CheckLevel
  verifyRepresentation
  verificationChecks
  verificationFailures
///

doc ///
Key
  CheckLevel
Headline
  Which relations to check on the action matrices
Description
  Text
    The allowed levels are @TT "\"Basic\""@, @TT "\"Relations\""@, and
    @TT "\"Serre\""@. Basic checks dimensions and the consistency of the
    stored actions. Relations additionally checks the Cartan and mixed commutator
    relations. Serre also checks the positive and negative Serre relations.
    The default is Basic for @TO checkWeylModule@ and Serre for
    @TO verifyRepresentation@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    isValid checkWeylModule(W, CheckLevel => "Basic")
    isValid checkWeylModule(W, CheckLevel => "Relations")
    isValid checkWeylModule(W, CheckLevel => "Serre")
  Text
    All three values are @TT "true"@. For this rank-one representation,
    the commutator relation is @TEX "$[e,f]=h$"@ and there are no Serre
    relations between distinct simple roots; the Serre level adds no such pairs.
SeeAlso
  checkWeylModule
  verifyRepresentation
///

doc ///
Key
  verifyRepresentation
  (verifyRepresentation, WeylModuleZ)
  (verifyRepresentation, SubWeylModuleFp)
  (verifyRepresentation, QuotientWeylModuleFp)
  (verifyRepresentation, SubquotientWeylModuleFp)
  [verifyRepresentation, CheckLevel]
Headline
  Check the Chevalley--Serre relations
Description
  Text
    Perform @TO checkWeylModule@ with default @TT "CheckLevel => \"Serre\""@.
    This checks the dimension and weightwise actions, the Cartan and mixed
    commutator identities, and the positive and negative Serre relations. The
    result is a @TO WeylModuleCheck@. Other levels may be selected explicitly.
    In positive characteristic these matrix identities concern the Chevalley
    generators and do not assert all relations among higher divided powers.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    C = verifyRepresentation W
    isValid C
    verificationFailures C
  Text
    The value @TT "true"@ and the empty list say that the stored
    Chevalley matrices give the required type @TEX "$A_2$"@ action on the
    eight-dimensional Weyl module, with no failed assertion.
SeeAlso
  checkWeylModule
  verificationChecks
  verificationFailures
///

doc ///
Key
  isValid
  (isValid, WeylModuleCheck)
Headline
  Whether all requested representation checks pass
Description
  Text
    Return @TT "true"@ when the @TO WeylModuleCheck@ has no failed
    assertion. Its meaning depends on the @TO CheckLevel@ used to produce the
    result; passing Basic checks does not assert the Serre relations.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    C = checkWeylModule(W, CheckLevel => "Serre")
    isValid C
  Text
    The returned @TT "true"@ means that the dimension and action checks,
    the commutator relations, and the Serre relations all pass for the type
    @TEX "$A_2$"@ Weyl module of highest weight @TEX "$(1,1)$"@.
SeeAlso
  checkWeylModule
  verificationChecks
  verificationFailures
///

doc ///
Key
  verificationChecks
  (verificationChecks, WeylModuleCheck)
Headline
  Truth values of the requested representation checks
Description
  Text
    Return the table of check names and truth values in a
    @TO WeylModuleCheck@. Relation names identify the matrix identity tested.
    Only assertions requested at the chosen @TO CheckLevel@ appear.
  Example
    W = weylModuleZ(cartanA 2, {2})
    C = verifyRepresentation W
    (verificationChecks C)#"dimension"
    (verificationChecks C)#"[e1,f1]"
  Text
    The first @TT "true"@ says that the sum of the weight multiplicities
    equals the dimension three of @TEX "$V_{\\mathbb Z}(2)$"@.
    The second says that its matrices satisfy @TEX "$[e,f]=h$"@.
SeeAlso
  verificationFailures
  isValid
  CheckLevel
///

doc ///
Key
  verificationFailures
  (verificationFailures, WeylModuleCheck)
Headline
  Discrepancies found in a representation check
Description
  Text
    Return one entry for each failed assertion. An entry gives the check,
    the weights or indices involved, and the difference from the asserted
    identity. For a failed matrix relation this difference is a matrix; another
    check may have a scalar difference or @TT "null"@.
  Example
    W = weylModuleZ(cartanA 3, {1, 1})
    C = checkWeylModule(W, CheckLevel => "Serre")
    verificationFailures C
  Text
    The empty list means that no discrepancy was found in the requested
    checks on @TEX "$V_{\\mathbb Z}(\\omega_1+\\omega_2)$"@. A nonempty list
    would identify precisely which dimension, action block, or relation failed.
SeeAlso
  verificationChecks
  isValid
  checkWeylModule
///

doc ///
Key
  weylHomDimension
  (weylHomDimension, WeylModuleZ, ZZ, List)
  (weylHomDimension, QuotientWeylModuleFp, List)
  [weylHomDimension, WeightFormat]
Headline
  Dimension of homomorphisms from a Weyl module
Description
  Text
    For a dominant integral weight @TEX "$\\mu$"@, a homomorphism
    @TEX "$V(\\mu)\\to M$"@ is determined by the image
    of its highest-weight vector, which lies in the maximal-vector space of
    weight @TEX "$\\mu$"@. This function returns the dimension of that space.
    For an integral ambient module the prime is explicit; a modular quotient
    already stores its characteristic. The weight is given in the convention
    selected by @TT "WeightFormat"@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    weylHomDimension(W, 2, {0})
    weylHomDimension(simpleQuotient(W, 2), {0})
  Text
    The first value is one: the unique weight-zero maximal direction
    gives the inclusion @TEX "$V(0)=L(0)\\hookrightarrow V_{\\mathbb F_2}(2)$"@.
    The second is zero, since the simple quotient @TEX "$L(2)$"@ has no
    weight-zero space.
SeeAlso
  weylHomGenerators
  maximalVectors
  isMaximalVector
///

doc ///
Key
  weylHomGenerators
  (weylHomGenerators, WeylModuleZ, ZZ, List)
  (weylHomGenerators, QuotientWeylModuleFp, List)
  [weylHomGenerators, WeightFormat]
Headline
  Images of highest-weight vectors for a basis of Weyl homomorphisms
Description
  Text
    For a dominant integral weight @TEX "$\\mu$"@, return a basis of
    its maximal-vector space in the target module. These vectors specify homomorphisms
    @TEX "$V(\\mu)\\to M$"@ by the image of the highest-weight vector.
    Vectors are expressed in the target's chosen weight-space basis.
    For an integral module supply a prime; a quotient already stores it.
  Text
    The entire vector space is computed. If its dimension is greater than
    one, examining only the displayed basis vectors need not exhaust all submodules
    that can arise from their linear combinations.
  Example
    W = weylModuleZ(cartanA 2, {2})
    weylHomGenerators(W, 2, {0})
  Text
    The output is one coefficient vector spanning the weight-zero maximal
    space. It sends the generator of @TEX "$V(0)$"@ to a nonzero scalar
    multiple of @TEX "$v_1=fv_0$"@, whose span is the trivial socle
    @TEX "$L(0)$"@ of @TEX "$V_{\\mathbb F_2}(2)$"@.
SeeAlso
  weylHomDimension
  maximalVectors
  socleAnalysis
///

doc ///
Key
  WeylGraph
Headline
  Weight spaces and nonzero simple lowering maps
Description
  Text
    A @TO WeylGraph@ records weight spaces and the ranks of ordinary simple
    lowering operators. Its vertices are weight spaces, with multiplicities,
    and its directed edges are the nonzero maps
    @TEX "$f_i:M_\\mu\\to M_{\\mu-\\alpha_i}$"@.
    The constructor is @TO weightGraph@. A vertex is not a chosen basis vector,
    and the graph is not a crystal graph.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    G
  Text
    The module @TEX "$V_{\\mathbb Z}(2)$"@ has rank-one weight spaces at
    @TEX "$2,0,-2$"@. The two displayed edges are the rank-one lowering maps
    @TEX "$2\\longrightarrow0\\longrightarrow-2$"@.
    The graph records their ranks over @TEX "$\\mathbb Q$"@, so it does not
    display the index two of the second map's image in its target lattice.
SeeAlso
  weightGraph
  graphVertices
  graphEdges
  toSVG
  viewGraph
///

doc ///
Key
  weightGraph
  (weightGraph, WeylModuleZ)
  (weightGraph, SubWeylModuleFp)
  (weightGraph, QuotientWeylModuleFp)
  (weightGraph, SubquotientWeylModuleFp)
Headline
  Construct the graph of ordinary simple lowering maps
Description
  Text
    @SUBSECTION "Weights and lowering maps"@
  Text
    A vertex of @TO weightGraph@ represents an entire weight space
    @TEX "$M_\\mu$"@, not an individual basis vector.  Its multiplicity is
    @TEX "$\\dim M_\\mu$"@, or @TEX "$\\operatorname{rank}_{\\mathbb Z}M_\\mu$"@
    for an integral module.  Weights are written in fundamental-weight
    coordinates.  An edge from @TEX "$\\mu$"@ to
    @TEX "$\\mu-\\alpha_i$"@, labeled @TT "f_i (rank r)"@, means that
    @TEX "$f_i:M_\\mu\\to M_{\\mu-\\alpha_i}$"@ has rank
    @TEX "$r>0$"@.  Here @TEX "$\\alpha_i$"@ is the
    @TEX "$i$"@th column of the Cartan matrix.  Zero maps give no edge;
    isolated weights are still vertices.
  Text
    For an integral module, an edge rank is computed over
    @TEX "$\\mathbb Q$"@; it does not describe the index of the image
    in the target lattice.  For a modular module, it is computed over
    its prime field and can therefore be smaller.  The graph records only
    the ordinary @TEX "$f_i$"@, not the higher divided powers
    @TEX "$f_i^{(r)}$"@.  In positive characteristic these higher
    operators can act nontrivially even when @TEX "$f_i=0$"@.
    Thus disconnectedness of this graph does not imply that the module is
    reducible or decomposable.  This is not a crystal graph.
  Text
    @TO graphVertices@ gives the weights and their multiplicities;
    @TO graphEdges@ gives the source, target, simple-root index, and rank
    of every edge.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    graphVertices G
    graphEdges G
  Text
    @TT "viewGraph G"@ draws:
  Text
    @LITERAL toSVG weightGraph weylModuleZ(cartanA 2, {2})@
  Text
    Here @TEX "$W=V_{\\mathbb Z}(2)=D^2(\\mathbb Z^2)$"@ for
    @TEX "$\\mathrm{SL}_2$"@.  The three vertices are its rank-one
    weight spaces of weights @TEX "$2,0,-2$"@.  If
    @TEX "$v_j=f^{(j)}v_0$"@, then
    @TEX "$fv_0=v_1$"@ and @TEX "$fv_1=2v_2$"@.
    These give the two rank-one edges
    @TEX "$2\\longrightarrow0\\longrightarrow-2$"@.
    The second map has image @TEX "$2\\mathbb Z v_2$"@:
    it has rank one but is not surjective over @TEX "$\\mathbb Z$"@.
    The entries of @TT "graphEdges G"@ describe these two maps.
  Example
    M = reduction(W, 2)
    weightGraph M
    dividedPowerActionF(M, 1, 2)
  Text
    After reduction modulo @TEX "$2$"@, all three weight spaces remain
    one-dimensional, but @TEX "$fv_1=0$"@.  The only edge is now
    @TEX "$2\\longrightarrow0$"@, and the vertex @TEX "$-2$"@
    is isolated.  Nevertheless @TEX "$f^{(2)}v_0=v_2$"@.
    In the displayed matrix, whose basis is ordered by weights
    @TEX "$2,0,-2$"@, the entry in row three and column one is
    @TEX "$1$"@.  This action is not represented by an edge.
  Example
    Q = simpleQuotient(W, 2)
    weightGraph Q
    dividedPowerActionF(Q, 1, 2)
  Text
    The simple quotient @TEX "$Q=L(2)$"@ has weights
    @TEX "$2,-2$"@, each with multiplicity one.  Its ordinary
    lowering operator is zero, so its graph has two vertices and no edges.
    The nonzero entry in row two and column one shows that
    @TEX "$f^{(2)}$"@ still takes the weight @TEX "$2$"@ vector
    to the weight @TEX "$-2$"@ vector.  In fact
    @TEX "$L(2)\\cong(k^2)^{(1)}$"@, the Frobenius twist of the
    natural module over @TEX "$k=\\mathbb F_2$"@.  This gives
    a simple module whose ordinary lowering graph is disconnected.
  Example
    U = weylModuleZ(cartanA 3, {2, 1})
    H = weightGraph U;
    new VerticalList from select(graphVertices H, v -> v#"multiplicity" == 2)
    new VerticalList from select(graphEdges H, e -> e#"rank" == 2)
  Text
    For @TEX "$U=V_{\\mathbb Z}(2\\omega_1+\\omega_2)$"@
    in type @TEX "$A_2$"@, the weights
    @TEX "$(1,0),(-1,1),(0,-1)$"@ have multiplicity two.
    The final output lists the rank-two maps
    @TEX "$f_1:U_{(1,0)}\\to U_{(-1,1)}$"@ and
    @TEX "$f_2:U_{(-1,1)}\\to U_{(0,-1)}$"@.
    Both become isomorphisms after tensoring with @TEX "$\\mathbb Q$"@.
    Each is recorded by one edge between two weight spaces, not by two
    edges between chosen basis vectors.
SeeAlso
  WeylGraph
  graphVertices
  graphEdges
  dividedPowerActionF
  toSVG
  viewGraph
///

doc ///
Key
  graphVertices
  (graphVertices, WeylGraph)
Headline
  Weights and multiplicities of the vertices
Description
  Text
    Return the list of vertex tables in a weight graph. Each contains
    @TT "\"weight\""@ in fundamental-weight coordinates,
    @TT "\"multiplicity\""@ equal to the weight-space dimension, and a
    printed @TT "\"label\""@. For an integral module the multiplicity is the
    rank of its weight lattice. Isolated weight spaces remain in this list.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    apply(graphVertices G, v -> (v#"weight", v#"multiplicity"))
  Text
    The three pairs give weights @TEX "$2,0,-2$"@, each of multiplicity
    one. Their multiplicities sum to the rank three of
    @TEX "$V_{\\mathbb Z}(2)$"@; each vertex represents the whole corresponding
    rank-one weight space.
SeeAlso
  weightGraph
  graphEdges
  weightMultiplicity
///

doc ///
Key
  graphEdges
  (graphEdges, WeylGraph)
Headline
  Source, target, and rank of each nonzero lowering map
Description
  Text
    Return one table per nonzero ordinary lowering map. Its entries
    @TT "\"from\""@, @TT "\"to\""@, @TT "\"simpleRoot\""@, and
    @TT "\"rank\""@ describe
    @TEX "$f_i:M_\\mu\\to M_{\\mu-\\alpha_i}$"@.
    Integral ranks are computed over @TEX "$\\mathbb Q$"@; modular ranks
    are computed over the module's prime field. Zero maps give no edge.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    apply(graphEdges G, e -> (e#"from", e#"to", e#"simpleRoot", e#"rank"))
    graphEdges weightGraph simpleQuotient(W, 2)
  Text
    The integral graph has two rank-one maps, from weight two to zero
    and from zero to minus two. Both use the only simple root, indexed one.
    The final list is empty: @TEX "$L(2)$"@ in characteristic two has zero
    ordinary lowering action. It is nevertheless simple, because
    @TEX "$f^{(2)}$"@ connects its two weights.
SeeAlso
  weightGraph
  graphVertices
  dividedPowerActionF
///

doc ///
Key
  SocleDiagram
Headline
  Simple factors arranged by socle layer
Description
  Text
    A @TO SocleDiagram@ arranges the semisimple factors of a
    @TO SocleAnalysis@ from bottom to top. Each layer records its factor
    multiplicities, its own dimension, and any ambiguous maximal-vector weights.
    The constructor is @TO socleDiagram@. A diagram describes layers rather
    than individual extension arrows.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = socleDiagram socleAnalysis(W, 2, RequireComplete => true)
    D
  Text
    The bottom layer is the one-dimensional simple @TEX "$L(0)$"@.
    The upper layer is the two-dimensional simple @TEX "$L(2)$"@.
    These are the socle and its quotient in @TEX "$V_{\\mathbb F_2}(2)$"@;
    the layers do not split off as a direct sum.
SeeAlso
  socleDiagram
  diagramLayers
  isCertified
  toSVG
///

doc ///
Key
  socleDiagram
  (socleDiagram, SocleAnalysis)
  (socleDiagram, WeylModuleZ, ZZ)
  (socleDiagram, SubWeylModuleFp)
  (socleDiagram, QuotientWeylModuleFp)
Headline
  Arrange the computed socle layers in a diagram
Description
  Text
    @SUBSECTION "Socle layers"@
  Text
    For a complete socle computation, layer @TEX "$j$"@ of
    @TO socleDiagram@ means
    @TEX "$\\operatorname{soc}^j M/\\operatorname{soc}^{j-1}M$"@,
    with @TEX "$\\operatorname{soc}^0 M=0$"@.  Its dimension is
    the dimension of that quotient, not of the cumulative submodule.
    A factor entry @TEX "$\\mu\\Rightarrow m$"@ means
    @TEX "$m$"@ copies of @TEX "$L(\\mu)$"@ in this semisimple
    layer.  Sums within a layer are direct sums; the successive layers
    need not split off from the module.
  Text
    When the maximal-vector search may be incomplete, the displayed
    semisimple layers describe the filtration found by @TO socleAnalysis@;
    they are not asserted to be the full socle layers.  This warning
    remains necessary even if the filtration reaches the whole module.
    To require a complete socle computation, pass
    @TT "socleAnalysis(..., RequireComplete => true)"@ to
    @TO socleDiagram@.  Factor labels retain the weight convention
    chosen in @TO socleAnalysis@.
  Text
    The overload @TT "socleDiagram(W,p)"@ concerns the reduction of
    @TT "W"@ modulo @TT "p"@, and @TT "socleDiagram Q"@ concerns
    the quotient @TT "Q"@.  As in @TO socleAnalysis@, a submodule
    argument @TT "S"@ specifies the quotient @TEX "$V/S$"@,
    not the socle filtration of @TT "S"@ itself.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = socleDiagram socleAnalysis(W, 2, RequireComplete => true)
    diagramLayers D
  Text
    @TT "viewGraph D"@ draws the two layers with the socle at the bottom:
  Text
    @LITERAL toSVG socleDiagram socleAnalysis(
      weylModuleZ(cartanA 2, {2}), 2, RequireComplete => true)@
  Text
    The output says that @TEX "$\\operatorname{soc}M=L(0)=kv_1$"@
    has dimension one and that
    @TEX "$M/\\operatorname{soc}M\\cong L(2)$"@ has dimension two.
    Thus @TEX "$V_{\\mathbb F_2}(2)$"@ fits into the nonsplit
    exact sequence
    @TEX "$0\\longrightarrow L(0)\\longrightarrow V_{\\mathbb F_2}(2)\\longrightarrow L(2)\\longrightarrow0$"@.
    It is not @TEX "$L(0)\\oplus L(2)$"@: that direct sum would
    be semisimple, with three-dimensional socle.  Equivalently,
    @TEX "$fv_0=v_1$"@ prevents the two other weight spaces from
    forming a complementary submodule.
    In @TT "diagramLayers D"@, the entries @TT "{0} => 1"@
    and @TT "{2} => 1"@ give one copy of each simple in its respective
    layer; the two dimensions are @TEX "$1$"@ and @TEX "$2$"@,
    not @TEX "$1$"@ and @TEX "$3$"@.
  Text
    In this two-factor example the socle computation implies that the extension
    is nonsplit, but a socle filtration in general does not determine
    extension arrows between individual simple factors.  Such arrows
    are therefore not drawn.
  Text
    Invisible edges in the socle export only arrange the layers vertically
    and assert no extensions.
SeeAlso
  SocleDiagram
  socleAnalysis
  diagramLayers
  toSVG
  viewGraph
///

doc ///
Key
  diagramLayers
  (diagramLayers, SocleDiagram)
Headline
  Factors and dimensions in each layer of a socle diagram
Description
  Text
    Return the layer tables of a @TO SocleDiagram@, in order from bottom
    to top. The entries @TT "\"layer\""@, @TT "\"factors\""@,
    @TT "\"dimension\""@, and @TT "\"ambiguousWeights\""@ give the
    one-based layer number, simple multiplicities, layer dimension, and weights
    where the maximal-vector search was ambiguous. The dimensions are not
    cumulative dimensions.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = socleDiagram socleAnalysis(W, 2, RequireComplete => true)
    apply(diagramLayers D, L -> (L#"layer", L#"factors", L#"dimension"))
  Text
    Layer one is @TEX "$L(0)$"@ with dimension one; layer two is
    @TEX "$L(2)$"@ with dimension two. The second dimension is that of the
    quotient by the socle, rather than the dimension three of the full module.
SeeAlso
  socleDiagram
  socleLayerFactors
  socleLayerDimensions
///

doc ///
Key
  toDot
  (toDot, WeylGraph)
  (toDot, SocleDiagram)
  (toDot, WeylGraph, String)
  (toDot, SocleDiagram, String)
Headline
  Write a Graphviz description of a weight graph or socle diagram
Description
  Text
    With one argument, return the DOT string describing the supplied
    @TO WeylGraph@ or @TO SocleDiagram@. With a filename, write that description
    and return the absolute filename. Existing files cause an error.
    Weight graphs include every weight multiplicity and the rank of each
    ordinary lowering map. Socle diagrams arrange the simple factors by layer;
    their invisible layout edges assert no extension relations.
  Text
    For example, @TT "toDot(G, \"weights.dot\")"@ writes a file which
    Graphviz renders with @TT "dot -Tsvg weights.dot -o weights.svg"@.
    For a drawing directly from the package, use @TO toSVG@ or @TO viewGraph@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    filename = toDot(G, temporaryFileName() | ".dot");
    fileExists filename
    get filename == toDot G
  Text
    The two values @TT "true"@ confirm that the file exists and contains
    the same graph as the returned DOT string: three rank-one weight spaces and
    two rank-one lowering maps for @TEX "$V_{\\mathbb Z}(2)$"@.
    Rendering it draws @TEX "$2\\longrightarrow0\\longrightarrow-2$"@.
SeeAlso
  toSVG
  viewGraph
  weightGraph
  socleDiagram
///

doc ///
Key
  toSVG
  (toSVG, WeylGraph)
  (toSVG, SocleDiagram)
  (toSVG, WeylGraph, String)
  (toSVG, SocleDiagram, String)
Headline
  Draw a weight graph or socle diagram as a vector image
Description
  Text
    With one argument, return the SVG image as a string. With a filename,
    save the image and return the absolute filename; existing files cause an
    error. SVG images can be opened in a browser or included in a document.
    For a weight graph the drawing labels weights, multiplicities, and lowering-map
    ranks. For a socle diagram it labels each layer and its dimension, placing
    the socle at the bottom. No Graphviz installation is needed.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = socleDiagram socleAnalysis(W, 2, RequireComplete => true)
    filename = toSVG(D, temporaryFileName() | ".svg");
    fileExists filename
    get filename == toSVG D
  Text
    The two values @TT "true"@ say that the saved vector image is the
    same as the returned image. It draws @TEX "$L(0)$"@ in the bottom layer
    of dimension one and @TEX "$L(2)$"@ above it in a layer of dimension two:
  Text
    @LITERAL toSVG socleDiagram socleAnalysis(
      weylModuleZ(cartanA 2, {2}), 2, RequireComplete => true)@
  Text
    These are the layers of the nonsplit extension
    @TEX "$0\\to L(0)\\to V_{\\mathbb F_2}(2)\\to L(2)\\to0$"@.
    Their vertical arrangement does not assert any additional extension arrows.
SeeAlso
  toDot
  viewGraph
  weightGraph
  socleDiagram
///

doc ///
Key
  viewGraph
  (viewGraph, WeylGraph)
  (viewGraph, SocleDiagram)
Headline
  Open the drawing of a weight graph or socle diagram
Description
  Text
    Save the drawing as a temporary SVG image, open it with the configured
    viewer, and return its filename. Both @TO WeylGraph@ and @TO SocleDiagram@
    are accepted. Weight multiplicities, lowering ranks, and socle-layer dimensions
    appear in the drawing. The saved vector image remains available for inclusion
    in a document.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    G
  Text
    This graph has three rank-one weight spaces and two nonzero lowering
    maps. Evaluating @TT "viewGraph G"@ opens its drawing, displaying the
    chain @TEX "$2\\longrightarrow0\\longrightarrow-2$"@.
    The rank labels are over @TEX "$\\mathbb Q$"@; the second map is
    multiplication by two on the integral lattice.
  Text
    @LITERAL toSVG weightGraph weylModuleZ(cartanA 2, {2})@
SeeAlso
  toSVG
  toDot
  weightGraph
  socleDiagram
///

doc ///
Key
  (show, WeylGraph)
  (show, SocleDiagram)
Headline
  Open a weight graph or socle diagram
Description
  Text
    For these diagram types, @TT "show G"@ is the same as @TT "viewGraph G"@:
    save a temporary SVG drawing, open it with the configured viewer, and return
    the filename.
  Example
    W = weylModuleZ(cartanA 2, {2})
    D = socleDiagram socleAnalysis(W, 2, RequireComplete => true)
    D
  Text
    The printed diagram identifies the socle @TEX "$L(0)$"@ of dimension
    one and the quotient @TEX "$L(2)$"@ of dimension two.
    Evaluating @TT "show D"@ opens a drawing with precisely these two layers,
    placing the socle at the bottom.
SeeAlso
  viewGraph
  toSVG
  socleDiagram
///

doc ///
Key
  (html, WeylGraph)
  (html, SocleDiagram)
Headline
  The SVG drawing for insertion in HTML
Description
  Text
    Return the same SVG string as @TO toSVG@. It describes the weight
    spaces and lowering maps of a @TO WeylGraph@, or the simple factors and
    dimensions in a @TO SocleDiagram@. The string may be inserted directly
    into an HTML document.
  Example
    W = weylModuleZ(cartanA 2, {2})
    G = weightGraph W
    html G == toSVG G
  Text
    The equality is @TT "true"@: both functions produce the same drawing
    of the three weights @TEX "$2,0,-2$"@ of @TEX "$V_{\\mathbb Z}(2)$"@
    and the two rank-one ordinary lowering maps between them.
SeeAlso
  toSVG
  viewGraph
///

doc ///
Key
  IncludeStructure
Headline
  Include maximal-vector information in a reduction profile
Description
  Text
    For @TO reductionProfile@, this option adds the number of vectors in the
    chosen maximal-vector bases and the weights of initially ambiguous maximal
    spaces. It defaults to @TT "false"@. It does not by itself request a socle
    filtration; @TO IncludeSocle@ includes that further computation.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2}, IncludeStructure => true)
    r = reductionRow(P, 2);
    r#"maximalVectorCount", r#"initialAmbiguousWeights", r#"socleLength"
  Text
    There are two maximal-vector basis vectors, of weights 2 and 0, and no
    ambiguous maximal-vector spaces. The socle length is @TT "null"@ because
    the filtration was not requested; this does not mean that the socle is zero.
SeeAlso
  reductionProfile
  IncludeSocle
  maximalVectors
///

doc ///
Key
  IncludeSocle
Headline
  Include computed socle layers in a reduction profile
Description
  Text
    For @TO reductionProfile@, this option also computes socle dimensions,
    socle length, and ambiguity in successive quotients. It defaults to
    @TT "false"@ and implies @TO IncludeStructure@. The completeness
    qualifications are the same as for @TO socleAnalysis@.
  Example
    W = weylModuleZ(cartanA 2, {2})
    P = reductionProfile(W, {2}, IncludeSocle => true)
    r = reductionRow(P, 2);
    r#"socleDimension", r#"socleLength", r#"socleCertified"
  Text
    The values are 1, 2, and @TT "true"@. The socle is the one-dimensional
    trivial module @TEX "$L(0)$"@, and its quotient is the two-dimensional
    simple @TEX "$L(2)$"@. Thus two socle layers exhaust the Weyl module
    @TEX "$V_{\\mathbb F_2}(2)$"@.
SeeAlso
  reductionProfile
  IncludeStructure
  socleAnalysis
///

doc ///
Key
  (symbol +, WeylCharacter, WeylCharacter)
Headline
  Add two formal characters
Description
  Text
    Add the coefficients at each weight in the common Cartan datum.
    For characters of modules over one field, this is the character of
    their direct sum. For arbitrary or virtual sums it is addition in the
    integral group ring of the weight lattice.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    T = weylCharacter weylModuleZ(cartanA 2, {0})
    characterTerms(C + T)
  Text
    The coefficients at weights 1, 0, and -1 are all one. This is the
    character of the natural two-dimensional module plus a trivial module,
    with total dimension three.
SeeAlso
  WeylCharacter
  weylCharacter
  characterTerms
///

doc ///
Key
  (symbol -, WeylCharacter, WeylCharacter)
  (symbol -, WeylCharacter)
Headline
  Subtract formal characters or take an additive inverse
Description
  Text
    Binary subtraction subtracts the coefficient at each weight, and unary
    negation changes every coefficient's sign. Binary subtraction requires
    the same Cartan datum. Negative coefficients represent a virtual
    difference and are not dimensions of weight spaces.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    T = weylCharacter weylModuleZ(cartanA 2, {0})
    characterTerms(C - T)
    dim(-C)
  Text
    The difference has coefficient one at weights 1 and -1 and coefficient
    -1 at weight zero. It is the natural character minus the trivial
    character. The value -2 is the virtual dimension of the negative
    natural character.
SeeAlso
  WeylCharacter
  isVirtualCharacter
  characterDim
///

doc ///
Key
  (symbol *, WeylCharacter, WeylCharacter)
  (symbol *, ZZ, WeylCharacter)
  (symbol *, WeylCharacter, ZZ)
Headline
  Multiply formal characters or multiply by an integer
Description
  Text
    The product of two characters is convolution of their weight
    coefficients and requires a common Cartan datum. For module characters
    over one field, it gives the tensor-product character. Multiplication
    by an integer instead scales every coefficient: it represents repeated
    addition or a virtual additive inverse, rather than scaling the weights.
  Example
    C = weylCharacter weylModuleZ(cartanA 2, {1})
    characterTerms(C * C)
    characterTerms(2 * C)
  Text
    The tensor square has weights 2 and -2 with coefficient one and weight
    zero with coefficient two. In contrast, the scalar multiple has weights
    1 and -1, each with coefficient two. Both have total dimension four,
    but they are different formal characters.
SeeAlso
  tensorProductCharacter
  productCharacter
  frobeniusTwistCharacter
///

doc ///
Key
  (symbol ==, WeylCharacter, WeylCharacter)
Headline
  Compare formal characters in the same Cartan datum
Description
  Text
    Equality means that the Cartan matrices agree and every weight has the
    same integer coefficient. The recorded characteristic, original weight
    convention, and manner of construction do not affect this equality.
    Equality of characters does not imply that arbitrary modules having
    those characters are isomorphic.
  Example
    W = weylModuleZ(cartanA 2, {2})
    C = weylCharacter W
    D = weylCharacter reduction(W, 2)
    C == D
    characteristic C, characteristic D
  Text
    The equality is @TT "true"@ although the recorded characteristics are
    zero and two. Both characters have weights 2, 0, and -2 with coefficient
    one, since reduction preserves the formal character. The integral
    lattice and its modular reduction are nevertheless different modules.
SeeAlso
  WeylCharacter
  character
  characteristic
///
