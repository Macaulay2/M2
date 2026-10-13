-- KacMoody documentation.  Loaded by KacMoody.m2 after beginDocumentation().
-- Show compact mathematical inputs and intermediate answers in examples.
-- Suppress routine setup, oversized tables, and values displayed explicitly later.

doc ///
Key
  KacMoody
Headline
  Computations with symmetrizable Kac-Moody algebras and their representations
Description
  Text
    @BOLD "KacMoody"@ provides tools for studying symmetrizable Kac--Moody
    algebras and their representations, using a generalized Cartan matrix
    as input.  Roots and weights are recorded together with their underlying
    Cartan data.  When the Cartan matrix is singular, weights also retain
    the coordinates that Dynkin labels alone do not determine.  Formal
    characters indicate whether they are exact or partial, together with
    the range of coefficients that has been determined.
  Text
    @SUBSECTION "An example: exceptional symmetry in seven dimensions"@

    The exceptional Lie algebra $G_2$ acts on a seven-dimensional vector
    space.  We can use characters to split its tensor spaces into
    irreducible representations:
  Example
    D = rootDatum "G2";
    C = finiteCharacter(D,{1,0})
    characterDimension C
  Text
    Let $V$ denote this representation.  The input @TT "{1,0}"@ is its
    highest weight in Dynkin labels; the two coordinates follow the node
    order shown by @TT "dynkinDiagram D"@.  The character @TT "C"@, returned
    by @TO finiteCharacter@, records the weights of $V$ and their
    multiplicities.  @TO characterDimension@ adds these multiplicities;
    the final output says that $\dim V=7$.
  Text
    Multiplying characters gives the character of a tensor product.
    @TO decomposeCharacter@ then finds its irreducible summands:
  Example
    decomposeCharacter(C*C)
  Text
    The keys are highest-weight Dynkin tuples.  An entry
    @TT "(a,b) => m"@ means $m$ copies of the irreducible representation
    with highest weight $a\omega_1+b\omega_2$, written $V(a,b)$ below.
    In particular, $V(0,0)=\mathbb C$ is the
    trivial representation and $V(1,0)=V$.  Thus the output means
    $$V\otimes V\cong
      \mathbb C\oplus V\oplus V(0,1)\oplus V(2,0),
      \qquad 49=1+7+14+27.$$
    Here $V(0,1)$ is the fourteen-dimensional adjoint representation:
    the Lie algebra $\mathfrak g_2$ acting on itself.
  Text
    We can separate the symmetric and alternating tensors in this same
    tensor product.  First, use @TO symmetricPowerDecomposition@ for the
    symmetric square:
  Example
    symmetricPowerDecomposition(C,2)
  Text
    This is the decomposition
    $$\operatorname{Sym}^2 V\cong\mathbb C\oplus V(2,0),
      \qquad 28=1+27.$$
    The single trivial summand implies that $V$ has an invariant symmetric
    bilinear form, unique up to scalar.  In particular, $V$ is isomorphic
    to its dual.
  Text
    Next, use @TO exteriorPowerDecomposition@ for the alternating square:
  Example
    exteriorPowerDecomposition(C,2)
  Text
    The output means
    $$\bigwedge^2 V\cong V\oplus\mathfrak g_2,
      \qquad 21=7+14.$$
    The symmetric and alternating pieces together recover all
    $28+21=49$ dimensions of $V\otimes V$.
  Text
    Finally, an alternating trilinear form on $V$ is an element of
    $\bigwedge^3 V^*$.  Use @TO dualCharacter@ to pass to the dual, then
    ask for the third exterior power:
  Example
    exteriorPowerDecomposition(dualCharacter C,3)
  Text
    This translates to
    $$\bigwedge^3 V^*\cong\mathbb C\oplus V\oplus V(2,0),
      \qquad 35=1+7+27.$$
    The trivial representation occurs once, so the space of
    $G_2$-invariant alternating trilinear forms on $V$ is one-dimensional.
    A nonzero such form is therefore unique up to scalar.
  Text
    Many of the same commands apply in finite, affine, and indefinite
    type.  The package determines the Cartan type from the matrix and
    computes roots and their multiplicities, characters of finite-dimensional
    highest-weight representations, restrictions to reductive Levi
    subalgebras, and Kostant cohomology for arbitrary standard parabolics.
    Weight multiplicities can also be computed by several independent
    formulas.  Kazhdan--Lusztig polynomials are available in every
    symmetrizable type, since each Bruhat interval is finite.  In infinite
    type, one can compute a BGG resolution through a specified homological
    degree.  Further finite-type computations include structural
    invariants, Borel--Weil--Bott cohomology, Demazure characters, symmetric
    and exterior powers, and highest-weight crystals.
  Text
    In finite type, @TO toLieAlgebra@ and @TO toRootSystem@ convert the
    same Cartan matrix to objects from @TT "LieAlgebraRepresentations"@
    and @TT "WeylGroups"@.  This gives direct access to their explicit
    matrices, Hasse diagrams, Gelfand-Tsetlin patterns, and related
    constructions.
  Text
    For the definitions underlying the package, begin with
    @TO "Mathematical background"@.  For concrete input conventions,
    continue with @TO "Reading diagrams and coordinates"@, which explains
    how the displayed node labels, Cartan-matrix entries, roots, weights,
    and parabolic inputs fit together.
Subnodes
  "Mathematical background"
  "Command reference"
  :Data and conventions
    "Root data and representations"
    "Related packages"
    "Conventions and sign rules"
    "Reading diagrams and coordinates"
    "Building a Cartan matrix and root system"
  :Finite representations
    "Working with formal characters"
    "Representation-theoretic invariants"
    "Tensor product decompositions and Littlewood-Richardson"
  :Weyl groups and parabolics
    "The Weyl group"
    "Parabolic decompositions and the nilradical grading"
    "Kostant homology and restricted cohomology"
  :Affine and indefinite type
    "Affine and indefinite Kac-Moody algebras"
  :Examples and references
    "Interpreting outputs: Irrep, Tally, HashTable"
    "Extended examples"
    "KacMoody references"
SeeAlso
  cartanA
  isCartanMatrix
  cartanType
  weylDimension
  weightsOfRep
  weightMultiplicity
  tensorDecomposition
  weylGroupElements
  weylLengthGenerating
  rootSnapshot
  leviHighestWeights
  kostantLieAlgCohomology
  Irrep
  LeviIrrep
  RootSnapshot
  cartanData
  parabolicData
  WeightFormat
  MaxDegree
///

doc ///
Key
  "Related packages"
Headline
  Related Macaulay2 packages
Description
  Text
    Several Macaulay2 packages overlap with parts of @TT "KacMoody"@.
    The following summary describes their main focus and their relation
    to the computations here:
  Text
    @UL {
      {TT "LieAlgebraRepresentations", " (Zinn-Justin, JSAG 2018): ",
       "specializes in finite type (A/B/C/D/E/F/G), with explicit ",
       "matrix representations via de Graaf bases, Gelfand-Tsetlin ",
       "patterns and polytopes (type A), spin and half-spin matrices ",
       "(type D), fusion products (quantum-group level), universal ",
       "enveloping algebras, and a character-ring formalism.  Use ",
       TO toLieAlgebra, " to pass a finite-type Cartan matrix to it."},
      {TT "WeylGroups", ": works in finite type from a ",
       TT "DynkinType", " and provides Bruhat-interval Hasse diagrams ",
       "with SVG export, ", TT "WeylGroupElement", " / ",
       TT "Parabolic", " / ", TT "WeylGroupDoubleCoset",
       " objects, and minimal coset representatives.  Use ",
       TO toRootSystem, " to pass a finite-type Cartan matrix to it."},
      {TT "HighestWeights", " (Galetto, JSAG 2015): given a graded ",
       "Macaulay2 object (free resolution, ",
       "ideal, module) whose pieces already carry a Lie-theoretic ",
       "grading, it reads off and decomposes the underlying ",
       "highest-weight modules.  This complements the forward ",
       "root-system and representation computations here."},
      {TT "GradedLieAlgebras", ": abstract ", TEX "$\\mathbb{Z}$",
       "-graded Lie algebras presented by generators and relations, ",
       "for Koszul-dual constructions, holonomy Lie algebras of ",
       "hyperplane arrangements, and minimal models in rational ",
       "homotopy theory."}
    }@
  Text
    @BOLD "KacMoody"@ focuses on the following computations:
  Text
    @UL {
      "computations from any symmetrizable generalized Cartan matrix,
       with its finite, affine, or indefinite type determined from the matrix,",
      "roots through a specified height and parabolic decompositions in affine and
       indefinite Kac-Moody algebras, including the named untwisted
       cartanAffineA/B/C/D/E/F/G commands, arbitrary twisted
       affine matrices, and overextended / very-extended diagrams,",
      {"the Kac--Moody Kostant formula for restricted, weight-graded ",
       "cohomology ", TEX "$H^\\bullet(\\mathfrak n^+;V(\\lambda))$",
       " for any standard parabolic nilradical, including an exact ",
       "degree-bounded computation for infinite quotients (see ",
       TO kostantLieAlgCohomology, "),"},
      {"the graded parabolic decomposition ",
       TEX "$\\mathfrak g = \\bigoplus_d \\mathfrak g_d$",
       " with each ", TEX "$\\mathfrak g_d$",
       " restricted to derived-Levi irreducibles (see ",
       TO leviHighestWeights, "),"},
      {"Peterson recursion for imaginary-root multiplicities in ",
       "affine and indefinite types (see ",
       TO rootSnapshot, "),"},
      {"Weyl-group descent sets, inversions written as positive roots, and ",
       "the length generating function (",
       TO leftDescentSet, ", ", TO rightDescentSet, ", ",
       TO weylInversionData, ", ", TO weylLengthGenerating,
       ") --- none of which appear in ", TT "WeylGroups", "."}
    }@
  Text
    These conversions retain the finite Cartan type, but not every
    choice of realization or coordinates.  @TO toLieAlgebra@ passes
    the finite Cartan matrix to @TT "LieAlgebraRepresentations"@.
    @TO toRootSystem@ determines the Dynkin type of each connected
    component, accepting reducible matrices and simultaneous row and
    column permutations.  Its result uses the node labeling chosen by
    @TT "WeylGroups"@.
SeeAlso
  toLieAlgebra
  toRootSystem
///

doc ///
Key
  "Mathematical background"
Headline
  Generalized Cartan matrices, roots, weights, Kac-Moody Lie algebras, parabolic subalgebras
Description
  Text
    This section collects the background needed to read the examples.
    Standard references are
    Humphreys, @EM "Introduction to Lie Algebras and Representation
    Theory"@, and Kac, @EM "Infinite Dimensional Lie Algebras"@.
  Text
    @BOLD "Generalized Cartan matrices."@
    A @EM "generalized Cartan matrix"@ (GCM) is an integer matrix
    $A=(a_{ij})_{i,j=1}^r$ with $a_{ii}=2$, $a_{ij}\le 0$ for $i\ne j$,
    and $a_{ij}=0 \iff a_{ji}=0$.  A GCM is @EM "symmetrizable"@ if
    there is a diagonal matrix $D$ of positive integers with $DA$
    symmetric.  Commands that use the invariant form require
    symmetrizability and report an error when it is absent.
    GCMs split into three disjoint classes:
  Text
    @UL {
      {BOLD "finite type", ": the symmetrized form is positive definite.  Its connected components are the classical Cartan matrices ", TEX "$A_r,B_r,C_r,D_r,E_{6,7,8},F_4,G_2$", "."},
      {BOLD "affine type", ": every connected component is finite or affine and at least one is affine; an indecomposable affine component has positive-semidefinite symmetrized form and one-dimensional kernel.  These arise from loop algebras (possibly twisted)."},
      {BOLD "indefinite type", ": everything else.  Hyperbolic (", TEX "$A$", " indefinite, every proper principal submatrix of finite or affine type) is the best-understood subclass."}
    }@
  Text
    @TO cartanType@ determines which case occurs; commands
    @TO cartanA@, @TO cartanB@, $\dots$, @TO cartanG2@ build the classical
    finite types, and any GCM (affine or indefinite) can be given
    directly as a matrix.
  Text
    @BOLD "Roots, coroots, Weyl group."@
    Fix a GCM $A$ of size $r$.  A realization consists of a Cartan
    subalgebra $\mathfrak h$, linearly independent simple roots
    $\alpha_1,\dots,\alpha_r\in\mathfrak h^*$, and linearly independent
    simple coroots $\alpha_1^\vee,\dots,\alpha_r^\vee\in\mathfrak h$.
    For singular $A$, the simple roots need not be a basis of all of
    $\mathfrak h^*$ and $\dim\mathfrak h$ is larger than $r$ in a
    minimal realization.  The package nevertheless records root-lattice
    elements by their coefficients in the free basis
    $\alpha_1,\dots,\alpha_r$.  The defining pairing is
    $\langle\alpha_j,\alpha_i^\vee\rangle = a_{ij}$.  The
    @EM "simple reflection"@ $s_i$ is the linear map
    $s_i(\lambda) = \lambda - \langle\lambda,\alpha_i^\vee\rangle\alpha_i$.
    The subgroup $W$ of $\mathrm{GL}(\mathfrak{h}^*_{\mathbb{R}})$
    generated by the $s_i$ is the @EM "Weyl group"@.  The Cartan
    matrix determines all relations among these generators: $W$ has
    the Coxeter presentation
    $$W=\left\langle s_1,\dots,s_r\ \middle|
      s_i^2=1,\ (s_is_j)^{m_{ij}}=1\text{ when }m_{ij}<\infty
      \right\rangle,$$
    where $m_{ii}=1$ and, for $i\ne j$,
    $$m_{ij}=\begin{cases}
      2,&a_{ij}a_{ji}=0,\\
      3,&a_{ij}a_{ji}=1,\\
      4,&a_{ij}a_{ji}=2,\\
      6,&a_{ij}a_{ji}=3,\\
      \infty,&a_{ij}a_{ji}\geq4.
    \end{cases}$$
    The command @TO coxeterMatrix@ computes this entire table, or one
    specified entry, directly from $A$.
    Thus disconnected nodes give commuting reflections, while a single
    edge gives the braid relation
    $s_is_js_i=s_js_is_j$.  Because only the products
    $a_{ij}a_{ji}$ occur, $A$ and its transpose determine the same
    abstract Coxeter group, although their actions on roots and coroots
    are dual realizations.  The Weyl group is finite iff
    $A$ is of finite type; otherwise it is infinite.  An element
    $w = s_{i_1}\cdots s_{i_k}$ of minimal such $k$ has
    @EM "length"@ $\ell(w) = k$, and the sequence $(i_1,\dots,i_k)$ is
    a @EM "reduced word"@ for $w$.
  Text
    The @EM "real roots"@ are the $W$-translates of the simple roots.
    In affine and indefinite type there are additional @EM "imaginary
    roots"@, which are not Weyl translates of simple roots.  All roots
    lie in the root lattice $Q = \bigoplus_i \mathbb{Z}\alpha_i$ and
    the @EM "height"@ of
    $\alpha=\sum_i c_i\alpha_i$ is
    $\operatorname{ht}(\alpha)=\sum_i c_i$.  Roots
    split as $\Phi = \Phi^+\sqcup\Phi^-$ (positive / negative),
    depending on the sign of their simple-root coefficients.
    In finite type $|\Phi^+|$ equals the number of positive roots
    (e.g.\ $120$ for $E_8$); in affine / indefinite type $\Phi^+$ is
    infinite and also contains imaginary roots (those of nonpositive
    squared norm for a symmetrizable GCM; in affine type their norm is
    zero).  Use @TO rootSnapshot@ uniformly: @TT "rootSnapshot A"@
    returns the complete positive root system in finite type, while
    @TT "rootSnapshot(A,H)"@ returns every positive root through height
    $H$ in finite, affine, or indefinite type.
  Text
    @BOLD "Weights, fundamental weights, the Weyl vector."@
    The evaluations
    $\lambda_i:=\langle\lambda,\alpha_i^\vee\rangle$ are the
    @EM "Dynkin labels"@ of a weight.  Unless a page says otherwise, a
    list $\{\lambda_1,\dots,\lambda_r\}$ gives these labels.
    They determine a weight modulo the subspace of $\mathfrak h^*$ that
    annihilates every simple coroot.  Such a list does not specify
    coordinates in the additional realization directions; @TO kmWeight@
    can retain these coordinates together with the Cartan datum.
    For singular $A$, the set of all
    $\lambda\in\mathfrak h^*$ taking integral values on the simple
    coroots has additional realization directions, so it is not a
    rank-$r$ lattice with a unique fundamental-weight basis.  When $A$
    is invertible (in particular, in finite type), the fundamental
    weights $\omega_i$ defined by
    $\langle\omega_i,\alpha_j^\vee\rangle=\delta_{ij}$ form the usual
    basis and have unique simple-root coordinates; this is the setting
    of @TO fundamentalWeights@.  The @EM "Weyl vector"@ $\rho$ is
    characterized by
    $\langle\rho,\alpha_i^\vee\rangle=1$ for every $i$; in finite type
    it is also
    $\rho=\tfrac12\sum_{\alpha\in\Phi^+}\alpha$ and is computed by
    @TO weylVector@.  A Dynkin-label weight is dominant integral iff
    all labels are nonnegative integers.
  Text
    @BOLD "Kac-Moody Lie algebras."@
    To a symmetrizable GCM $A$ one associates the @EM "Kac-Moody Lie
    algebra"@ $\mathfrak g(A)$.  For a realization as above, it is
    generated by the full Cartan algebra $\mathfrak h$ together with
    $e_i,f_i$ ($i=1,\dots,r$), subject to
    $$[h,h']=0,\quad [h,e_i]=\alpha_i(h)e_i,\quad
      [h,f_i]=-\alpha_i(h)f_i,\quad
      [e_i,f_j]=\delta_{ij}\alpha_i^\vee,$$
    for $h,h'\in\mathfrak h$, together with the Serre relations
    $(\mathrm{ad}\,e_i)^{1-a_{ij}}e_j = 0$ for $i\ne j$ (and likewise
    for $f_i$).  Writing $h_i=\alpha_i^\vee$ recovers the usual
    Chevalley-generator notation.  When $A$ is of finite type this recovers the classical
    finite-dimensional simple Lie algebras.  When $A$ is of affine type
    $\mathfrak g(A)$ is (up to a one-dimensional central extension and
    a derivation) the loop algebra of a finite-dimensional Lie algebra.
    In indefinite type the resulting Lie algebra is usually
    infinite-dimensional and its imaginary-root multiplicities can be
    considerably more complicated.  In every case
    $\mathfrak g(A)$ carries a root-space decomposition
    $\mathfrak g(A) = \mathfrak h \oplus \bigoplus_{\alpha\in\Phi}\mathfrak g_\alpha$
    with $\dim\mathfrak g_\alpha = 1$ for every real root $\alpha$.
  Text
    An irreducible @EM "integrable highest-weight module"@ $V(\lambda)$
    is attached to each dominant integral $\lambda$.  It is generated
    by a highest-weight vector $v_\lambda$ satisfying
    $$e_i v_\lambda=0,\qquad
      h v_\lambda=\lambda(h)v_\lambda\ \ (h\in\mathfrak h),\qquad
      f_i^{\lambda_i+1}v_\lambda=0.$$
    In particular $h_i v_\lambda=\lambda_i v_\lambda$ for
    $h_i=\alpha_i^\vee$; these eigenvalues are the Dynkin labels.
    In finite type $V(\lambda)$ is
    finite-dimensional and $\dim V(\lambda)$ is given by the Weyl
    dimension formula (@TO weylDimension@).  In affine / indefinite
    type $V(\lambda)$ may be infinite-dimensional but still has a
    well-defined formal character governed by the Weyl--Kac character
    formula in the appropriate completed group algebra.  The zero highest
    weight gives the
    one-dimensional trivial module; nontrivial positive-level affine
    integrable modules are infinite-dimensional.
  Text
    @BOLD "Parabolic subalgebras and the Levi grading."@
    In the examples, let $J\subseteq\{1,\dots,r\}$ be the
    list @TT "parabolic"@ of @EM "removed"@ nodes and let
    $I=\{1,\dots,r\}\setminus J$ be the Levi nodes.  The standard
    Levi root subsystem is
    $\Phi_I=\Phi\cap\bigoplus_{i\in I}\mathbb Z\alpha_i$, with
    $\Phi_I^+=\Phi_I\cap\Phi^+$.
    The standard
    parabolic is $\mathfrak p_I=\mathfrak l_I\oplus\mathfrak n_I^+$,
    where
    $\mathfrak l_I=\mathfrak h\oplus
    \bigoplus_{\alpha\in\Phi_I}\mathfrak g_\alpha$ and
    $\mathfrak n_I^+=\bigoplus_{\alpha\in\Phi^+\setminus\Phi_I^+}
    \mathfrak g_\alpha$.  The package uses the total
    $\mathbb Z$-degree
    $\deg(\sum c_k\alpha_k)=\sum_{j\in J}c_j$.
    Each graded piece is finite-dimensional when the derived Levi is
    finite type.  The @TO Irrep@ type records a highest-weight module
    for $[\mathfrak l_I,\mathfrak l_I]$: for an infinite-type Levi this
    is a symbolic integrable highest-weight module and is generally
    infinite-dimensional.  Levi-center characters are
    forgotten, so summands differing only by central character can
    merge.  Computing these derived-Levi highest weights is the central
    output of @TO leviHighestWeights@, which detects finite, affine, and
    indefinite type and determines any required root-height bound.  See also
    @TO kostantLieAlgCohomology@ for the general Kac--Moody Kostant
    formula.  Its default output likewise forgets central characters;
    set @TT "IncludeCentralCharacter => true"@ to retain the additional
    full-Levi central data in @TO LeviIrrep@ records.
  Text
    @BOLD "One extended type-A example."@  We now realize every definition
    above in one running example.  The call @TT "cartanA 4"@ means type
    $A_3$, so throughout this example
    $$A=A_3,\qquad \mathfrak g(A)=\mathfrak{sl}_4.$$
    We begin with the matrix, its symmetrizer, and its displayed node labels:
  Example
    A = cartanA 4;
    A
    isCartanMatrix A
    symmetrizer A
    print dynkinDiagram A
  Text
    Thus
    $$A=\begin{pmatrix}2&-1&0\\-1&2&-1\\0&-1&2\end{pmatrix}.$$
    It is a GCM because its diagonal entries are $2$, its off-diagonal
    entries are nonpositive, and its zero pattern is symmetric.  Here
    $D=I_3$ symmetrizes $A$; in fact $A$ itself is symmetric.  The diagram
    fixes the package labels $1$--$2$--$3$ used in every call below.
  Text
    The finite, affine, and indefinite alternatives can all be seen without
    leaving the type-$A$ family.  Attach the affine node and then the
    overextended node:
  Example
    Aaff = cartanAffineA 3
    Aover = cartanOverExtended A
    (cartanType A, cartanType Aaff, cartanType Aover)
    (det A, det Aaff, det Aover)
    dataAff = cartanData Aaff;
    (dataAff#"rank", dataAff#"matrixRank", dataAff#"corank")
  Text
    The three types are respectively @TT "finite"@, @TT "affine"@, and
    @TT "indefinite"@.  Their determinants are $4$, $0$, and $-4$.
    The affine matrix $A_3^{(1)}$ has four simple roots but matrix rank
    $3$ and corank $1$; its minimal realization therefore has
    $\dim\mathfrak h=2(4)-3=5$, so the simple roots do not span all of
    $\mathfrak h^*$.  The overextension $A_3^{++}$ is hyperbolic:
    deleting its overextended node leaves $A_3^{(1)}$, while deleting
    any other node leaves finite-type components.  In Lie-theoretic terms,
    $\mathfrak g(A_3^{(1)})$ is the centrally extended loop algebra of
    $\mathfrak{sl}_4$ with its derivation, whereas
    $\mathfrak g(A_3^{++})$ is indefinite.
  Text
    @BOLD "The realization and the defining pairing."@  In simple-root
    coordinates the basis vectors $\alpha_1,\alpha_2,\alpha_3$ are the
    standard rows.  The coroot rows record all pairings
    $\langle\alpha_j,\alpha_i^\vee\rangle=A_{ij}$:
  Example
    simpleRoots A
    coroots A
    toFundamental(A, {1,0,0})
    toFundamental(A, {0,1,0})
  Text
    The vector @TT "{1,0,0}"@ means $\alpha_1\in
    Q=\mathbb Z\alpha_1\oplus\mathbb Z\alpha_2\oplus\mathbb
    Z\alpha_3$.  Its Dynkin labels are $(2,-1,0)$, the first column of
    $A$; similarly $\alpha_2$ has labels $(-1,2,-1)$.  This is the
    defining root-coroot pairing written in coordinates.
  Text
    @BOLD "Reflections, Coxeter relations, words, and length."@  Apply
    the second simple reflection first to $\alpha_1$ and then to
    $\alpha_2$, and then verify every defining relation for the finite
    and affine Weyl groups:
  Example
    S2 = reflectionMatrix(A, 2)
    S2 * transpose matrix{{1,0,0}}
    S2 * transpose matrix{{0,1,0}}
    coxeterMatrix A
    finiteReflections = apply(3, i -> reflectionMatrix(A, i+1));
    all(3, i -> finiteReflections#i^2 == id_(ZZ^3))
    ((finiteReflections#0*finiteReflections#1)^3 == id_(ZZ^3),
     (finiteReflections#1*finiteReflections#2)^3 == id_(ZZ^3),
     (finiteReflections#0*finiteReflections#2)^2 == id_(ZZ^3))
    affineReflections = apply(4, i -> reflectionMatrix(Aaff, i+1));
    affineEdges = {{0,1},{1,2},{2,3},{3,0}};
    affineOpposites = {{0,2},{1,3}};
    coxeterMatrix Aaff
    all(4, i -> affineReflections#i^2 == id_(ZZ^4))
    apply(affineEdges, p ->
        (affineReflections#(p#0)*affineReflections#(p#1))^3
            == id_(ZZ^4))
    apply(affineOpposites, p ->
        (affineReflections#(p#0)*affineReflections#(p#1))^2
            == id_(ZZ^4))
    weylInversionData(A, {1,2,1})
    reducedWord(A, {1,2,1})
    weylGroupOrder A
    Caff = weylElementMatrix(Aaff, {1,2,3,4})
    Naff = Caff^3 - id_(ZZ^4)
    (Naff == 0, Naff*Naff == 0,
     apply(toList(1..5), k ->
         Caff^(3*k) == id_(ZZ^4) + k*Naff))
  Text
    The two column outputs say
    $$s_2(\alpha_1)=\alpha_1+\alpha_2,\qquad
      s_2(\alpha_2)=-\alpha_2,$$
    exactly as
    $s_i(\lambda)=\lambda-\langle\lambda,\alpha_i^\vee\rangle\alpha_i$
    prescribes.  The first Coxeter table is
    $$M(A_3)=\begin{pmatrix}1&3&2\\3&1&3\\2&3&1\end{pmatrix}.$$
    Indeed, the three relevant Cartan-entry products are
    $(a_{12}a_{21},a_{23}a_{32},a_{13}a_{31})=(1,1,0)$, which give
    $(m_{12},m_{23},m_{13})=(3,3,2)$.  The first @TT "true"@ verifies
    $s_i^2=1$ for all three generators, and the following three do the
    same for
    $$(s_1s_2)^3=(s_2s_3)^3=(s_1s_3)^2=1.$$
    Equivalently, adjacent generators satisfy
    $s_1s_2s_1=s_2s_1s_2$ and
    $s_2s_3s_2=s_3s_2s_3$, while $s_1s_3=s_3s_1$.
    For the affine four-cycle $A_3^{(1)}$, the displayed Coxeter table
    has $3$ on each edge and $2$ on each opposite pair: the four edge
    products are all $1$ and the two opposite-node products are $0$.  The next
    outputs verify all four involutions, all four order-$3$ edge
    relations, and both order-$2$ opposite-node relations.  Hence the
    computation checks every Coxeter relation prescribed by the affine
    Cartan matrix, not just a representative pair.  The word
    $s_1s_2s_1$ has the three inversions
    $\alpha_1,\alpha_1+\alpha_2,\alpha_2$, hence length $3$ and is
    reduced.  The finite Weyl group $W(A_3)\cong S_4$ has order $24$.
    For the affine Coxeter element $C=s_1s_2s_3s_4$, set
    $N=C^3-I$.  The final output shows that $N\ne0$, $N^2=0$, and
    verifies the first five instances of
    $$C^{3k}=(I+N)^k=I+kN.$$
    The equality holds for every $k$ by the binomial theorem, so these
    powers are all distinct.  This gives a direct computational proof
    that $W(A_3^{(1)})$ is infinite.
  Text
    @BOLD "Positive, negative, real, and imaginary roots."@  The complete
    finite positive root system is small enough to display:
  Example
    rootsA3 = rootSnapshot A;
    rootSnapshotByHeight rootsA3
    negativeRoots A
    (rootMultiplicity(rootsA3, {1,1,1}),
     rootHeight {1,1,1},
     rootNormSquared(A, {1,1,1}))
  Text
    The table is
    $$\Phi^+=\{\alpha_1,\alpha_2,\alpha_3,\,
      \alpha_1+\alpha_2,\,\alpha_2+\alpha_3,\,
      \alpha_1+\alpha_2+\alpha_3\}.$$
    The negative roots are precisely their negatives.  Every root shown is
    real.  In particular the highest root
    $\theta=\alpha_1+\alpha_2+\alpha_3$ has height $3$, squared norm $2$,
    and multiplicity $1$, so
    $\dim\mathfrak g_\theta=1$.
  Text
    The affine extension supplies the imaginary-root definition missing in
    finite type.  At height $4$ its primitive null root first appears:
  Example
    rootsAff = rootSnapshot(Aaff, 4);
    rootSnapshotRoots(rootsAff, Kind => "Imaginary")
    (rootMultiplicity(rootsAff, {1,1,1,1}),
     rootNormSquared(Aaff, {1,1,1,1}))
  Text
    The row $(1,1,1,1)$ means
    $\delta=\alpha_1+\alpha_2+\alpha_3+\alpha_4$ in the package's
    1-based affine labeling.  It has norm $0$ and multiplicity $3$.
    Unlike the real roots, $\delta$ is not a Weyl translate of a simple
    root.
  Text
    @BOLD "Dynkin labels, fundamental weights, dominance, and the Weyl
    vector."@  Return now to finite $A_3$.  Since $A$ is invertible, its
    fundamental weights have unique simple-root coordinates:
  Example
    fundamentalWeights A
    (A ** QQ) * transpose fundamentalWeights A
    omega2 = toSimple(A, {0,1,0});
    rho = weylVector A;
    omega2
    rho
    toFundamental(A, rho)
    rho == apply(sum entries cartanPositiveRoots A, x -> x/2)
    (isDominant(A, {0,1,0}), isDominant(A, {1,-1,0}))
  Text
    The identity matrix in the second output verifies
    $\langle\omega_i,\alpha_j^\vee\rangle=\delta_{ij}$.  The second
    row of the fundamental-weight matrix gives
    $\omega_2=\tfrac12\alpha_1+\alpha_2+\tfrac12\alpha_3$, whose
    Dynkin labels are $(0,1,0)$.  Summing the three fundamental weights
    gives
    $\rho=\tfrac32\alpha_1+2\alpha_2+\tfrac32\alpha_3$, with Dynkin
    labels $(1,1,1)$.  Thus $(0,1,0)$ is dominant integral, while
    $(1,-1,0)$ is not.
  Text
    @BOLD "The Chevalley--Serre presentation and root spaces."@  For
    $\mathfrak{sl}_4$, take
    $e_i=E_{i,i+1}$, $f_i=E_{i+1,i}$, and
    $h_i=[e_i,f_i]$.  The following literal $4$-by-$4$ matrices verify
    the relations:
  Example
    unitMatrix = (i,j) -> matrix(QQ, table(4,4, (p,q) ->
        if p == i and q == j then 1 else 0));
    bracket = (X,Y) -> X*Y-Y*X;
    es = {unitMatrix(0,1), unitMatrix(1,2), unitMatrix(2,3)};
    fs = {unitMatrix(1,0), unitMatrix(2,1), unitMatrix(3,2)};
    hs = apply(3, i -> bracket(es#i,fs#i));
    hs#0
    all(3, i -> all(3, j ->
        bracket(hs#i,hs#j) == 0))
    all(3, i -> all(3, j ->
        bracket(hs#i,es#j) == A_(i,j)*es#j))
    all(3, i -> all(3, j ->
        bracket(hs#i,fs#j) == -A_(i,j)*fs#j))
    all(3, i -> all(3, j ->
        bracket(es#i,fs#j) == (if i == j then hs#i else 0)))
    iteratedAd = (X,Y,n) -> (
        Z := Y;
        for k from 1 to n do Z = bracket(X,Z);
        Z);
    all(3, i -> all(3, j -> i == j or
        iteratedAd(es#i,es#j,1-A_(i,j)) == 0))
    all(3, i -> all(3, j -> i == j or
        iteratedAd(fs#i,fs#j,1-A_(i,j)) == 0))
    bracket(es#0,es#1)
    bracket(bracket(es#0,es#1),es#2)
  Text
    Here $h_1=\operatorname{diag}(1,-1,0,0)$.  The six
    @TT "true"@ values verify, for every $i,j$,
    $$[h_i,h_j]=0,\quad [h_i,e_j]=A_{ij}e_j,\quad
      [h_i,f_j]=-A_{ij}f_j,\quad
      [e_i,f_j]=\delta_{ij}h_i,$$
    followed by every $e$- and $f$-Serre relation.  These include
    $[e_1,e_3]=0$ because $A_{13}=0$ and
    $[e_1,[e_1,e_2]]=0$ because $1-A_{12}=2$.  The final matrices
    are $E_{13}\in\mathfrak g_{\alpha_1+\alpha_2}$ and
    $E_{14}\in\mathfrak g_{\alpha_1+\alpha_2+\alpha_3}$, making the
    root-space decomposition concrete.
  Text
    @BOLD "An integrable highest-weight module."@  Take
    $\lambda=\omega_2$, given by its Dynkin labels $(0,1,0)$:
  Example
    lambda = {0,1,0}
    (isDominant(A, lambda), weylDimension(A, lambda))
    weightsOfRep(A, lambda)
    apply(weightsOfRep(A, lambda),
        mu -> weightMultiplicity(A, lambda, mu))
    weylDimension(A, {0,0,0})
  Text
    Thus
    $$V(\omega_2)\cong\bigwedge\nolimits^2\mathbb C^4,\qquad
      \dim V(\omega_2)=6.$$
    Its six displayed weights all have multiplicity $1$.  Concretely take
    $v_\lambda=e_1\wedge e_2$.  The matrices above give
    $e_i v_\lambda=0$ for every $i$,
    $h_1v_\lambda=0$, $h_2v_\lambda=v_\lambda$, and
    $h_3v_\lambda=0$.  The defining
    integrability relations are
    $f_1v_\lambda=0$, $f_2^2v_\lambda=0$, and
    $f_3v_\lambda=0$, exactly the exponents
    $\lambda_i+1$.  The final output is $1$, confirming that
    $V(0)$ is the one-dimensional trivial module.
  Text
    The adjoint highest weight $(1,0,1)$ reconnects this module language
    with the root-space decomposition:
  Example
    adjointWeights = weightMultiplicityTable(A, {1,0,1});
    (weylDimension(A, {1,0,1}),
     adjointWeights#{0,0,0},
     sum values adjointWeights)
  Text
    The output $(15,3,15)$ says that the adjoint module is
    $\mathfrak{sl}_4$ itself: its twelve nonzero weights are the twelve
    roots, each with multiplicity $1$, and its zero-weight space is
    $\mathfrak h$, of dimension $3$.
  Text
    @BOLD "A parabolic, its Levi, and its grading."@  Remove the middle
    node $J=\{2\}$.  Its complement is $I=\{1,3\}$:
  Example
    print parabolicDiagram(A, {2})
    P = parabolicData(A, {2});
    (P#"removedNodes", P#"leviNodes", P#"gradingVector")
    P#"nilradicalRoots"
    parabolicDegrees(A, {2})
  Text
    This is the Grassmannian parabolic for
    $\operatorname{Gr}(2,4)$.  Its derived Levi is
    $\mathfrak{sl}_2\oplus\mathfrak{sl}_2$.  Concretely,
    $\Phi_I=\{\pm\alpha_1,\pm\alpha_3\}$ and
    $$\mathfrak l_I=\mathfrak h\oplus
      \mathfrak g_{\alpha_1}\oplus\mathfrak g_{-\alpha_1}\oplus
      \mathfrak g_{\alpha_3}\oplus\mathfrak g_{-\alpha_3}.$$
    The complementary positive root spaces form
    $$\mathfrak n_I^+
      =\mathfrak g_{\alpha_2}\oplus
       \mathfrak g_{\alpha_1+\alpha_2}\oplus
       \mathfrak g_{\alpha_2+\alpha_3}\oplus
       \mathfrak g_{\alpha_1+\alpha_2+\alpha_3}.$$
    Every listed root has middle coefficient $1$, so every root space
    has parabolic degree $1$.  Consequently
    $\mathfrak n_I^+=\mathfrak g_1$ is four-dimensional and abelian,
    and there is no positive piece $\mathfrak g_d$ for $d\geq2$.
  Example
    leviHighestWeights(A, 0, {2})
    leviHighestWeights(A, 1, {2})
    leviHighestWeights(A, 2, {2})
  Text
    Written without package notation, the first two outputs mean
    $$\mathfrak g_0\big|_{\mathfrak{sl}_2\oplus\mathfrak{sl}_2}
      \cong (V(2)\otimes V(0))\oplus
             (V(0)\otimes V(2))\oplus
             (V(0)\otimes V(0)),$$
    and
    $$\mathfrak g_1
      \cong V(1)\otimes V(1).$$
    The trivial summand in degree $0$ is the one-dimensional Levi center.
    The empty degree-$2$ Tally confirms that the grading has depth $1$.
    The @TO Irrep@ objects remember the two derived-Levi factors but, by
    default, not that central character.
  Text
    @BOLD "Kostant cohomology and the Levi center."@  Finally use the
    trivial ambient highest weight $\lambda=0$:
  Example
    H = kostantLieAlgCohomology(A, {2}, {0,0,0});
    H
    cohomologyDimensions(A, {2}, H)
    Hfull = kostantLieAlgCohomology(A, {2}, {0,0,0},
        IncludeCentralCharacter => true);
    Hfull#1
  Text
    The output @TT "H"@ is the complete derived-Levi decomposition of
    $H^q(\mathfrak n_I^+;\mathbb C)$ for $0\leq q\leq4$.  Its dimensions
    are $1,4,6,4,1$.  This agrees with
    $H^q(\mathfrak n_I^+;\mathbb C)\cong
    \bigwedge^q(\mathfrak n_I^+)^*$ because the four-dimensional
    nilradical is abelian.  The degree-$1$ output with central characters retains
    central character $(-1)$, in the package's removed-node
    fundamental-coweight normalization, in addition to the semisimple module
    $V(1)\otimes V(1)$, illustrating exactly what the default
    @TO Irrep@ output forgets.
  Text
    To see two different full-Levi summands merge after that
    information is forgotten, remove both end nodes $J=\{1,3\}$.  The
    surviving derived Levi is the middle $\mathfrak{sl}_2$:
  Example
    Jends = {1,3};
    leviHighestWeights(A, 1, Jends)
    leviHighestWeights(A, 2, Jends)
    Hend = kostantLieAlgCohomology(A, Jends, {0,0,0});
    HendFull = kostantLieAlgCohomology(A, Jends, {0,0,0},
        IncludeCentralCharacter => true);
    Hend#1
    HendFull#1
  Text
    In the Levi grading this means
    $$\mathfrak g_1\cong V(1)^{\oplus2},\qquad
      \mathfrak g_2\cong V(0).$$
    The default cohomology output likewise reports two copies of $V(1)$.
    Including central characters separates them as
    $V(1)_{(-1,0)}\oplus V(1)_{(0,-1)}$: their two-component
    central-character labels differ even though their restrictions to the derived
    $\mathfrak{sl}_2$ agree.
  Text
    Finally, the same type-$A$ tower illustrates what an @TO Irrep@ means
    for an infinite-type Levi.  In the hyperbolic algebra $A_3^{++}$,
    removing its overextended node leaves the affine Levi $A_3^{(1)}$:
  Example
    kostantLieAlgCohomology(
        Aover, {1}, {0,0,0,0,0}, MaxDegree => 2)
  Text
    Mathematically the output is
    $$H^0\cong V(0,0,0,0),\qquad
      H^1\cong V(1,0,0,0),\qquad
      H^2\cong V(0,1,0,1).$$
    The first affine-Levi module is trivial.  The latter two have positive
    affine level and are infinite-dimensional integrable highest-weight
    modules; here @TO Irrep@ records their highest weights symbolically
    rather than claiming they are finite-dimensional vector spaces.
SeeAlso
  cartanType
  cartanData
  cartanAffineA
  cartanOverExtended
  symmetrizer
  reflectionMatrix
  weylInversionData
  fundamentalWeights
  weylVector
  rootSnapshot
  rootSnapshotByHeight
  weylDimension
  parabolicData
  leviHighestWeights
  kostantLieAlgCohomology
///

doc ///
Key
  "Conventions and sign rules"
Headline
  Bourbaki convention for Cartan matrices, weight bases, arrow direction on Dynkin diagrams
Description
  Text
    All Cartan matrices use rows indexed by simple coroots:
    $$A_{ij} \;=\; \langle\alpha_j,\alpha_i^\vee\rangle
        \;=\; 2(\alpha_i,\alpha_j)/(\alpha_i,\alpha_i).$$
    Thus column $j$ is the Dynkin-label vector of the simple root
    $\alpha_j$.  When $A$ is invertible, the transpose-inverse
    $(A^T)^{-1}$ has rows giving the fundamental weights $\omega_i$ in
    the basis of simple roots.  For singular $A$ those simple-root
    coordinates are not determined by $A$ alone.  A
    simple root $\alpha_j$ is longer than $\alpha_i$ iff
    $|A_{ij}| > |A_{ji}|$; accordingly, in our Dynkin diagrams the
    arrow on a multi-edge points from the @EM "long"@ node to the
    @EM "short"@ node.
  Example
    A = matrix{{2,-3},{-1,2}};
    A
    dynkinDiagram A
    toFundamental(A, {1,0}) -- column 1: Dynkin labels of alpha_1
    toFundamental(A, {0,1}) -- column 2: Dynkin labels of alpha_2
  Text
    In the settings where the corresponding realization is available,
    weights can be expressed in three coordinate systems:
  Text
    @UL {
      TT "\"Dynkin\" labels / fundamental basis when A is invertible",
      TT "\"simple\" / alpha basis",
      TT "\"epsilon\" / a chosen Euclidean ambient realization"
    }@
  Text
    with the conversion commands @TO toFundamental@ / @TO toSimple@ /
    @TO toEpsilon@ / @TO fromEpsilon@.  Conversion from Dynkin labels
    to simple-root coordinates requires invertible $A$; epsilon
    coordinates are the chosen classical realizations, not data
    determined by every GCM.  Weights input to
    @TO weylDimension@, @TO weightMultiplicity@, @TO weightsOfRep@,
    @TO weightOrbit@, @TO tensorDecomposition@,
    @TO leviHighestWeights@, and @TO kostantLieAlgCohomology@
    use the Dynkin basis (the coefficients
    $\langle\lambda,\alpha_i^\vee\rangle$) by default.  Commands that
    accept the option @TO WeightFormat@ also accept classical partition
    coordinates.
SeeAlso
  toFundamental
  toSimple
  toEpsilon
  fromEpsilon
  WeightFormat
///

doc ///
Key
  "Reading diagrams and coordinates"
Headline
  Node labels, Cartan entries, root coordinates, and parabolic input
Description
  Text
    Start by printing the Dynkin diagram.  Its labels are the labels used by
    every command that asks for nodes or simple reflections.  For
    example, node $2$ is the central node in the package's $D_4$ labeling:
  Example
    A = cartanD 4
    print dynkinDiagram A
  Text
    A @TT "parabolic"@ list contains the @EM "removed"@ nodes, or
    equivalently the simple roots assigned degree $1$.  The complementary
    nodes form the Levi diagram.  Thus removing node $2$ above leaves the
    three outer nodes $\{1,3,4\}$, a Levi of type $A_1\times A_1\times A_1$:
  Example
    removed = {2};
    L = leviSubalgebra(A, removed);
    L#"nodes"
    print dynkinDiagram (L#"cartanMatrix")
    leviHighestWeights(A, 1, removed)
  Text
    The last line reads
    $$\mathfrak g_1\big|_{A_1\times A_1\times A_1}
      \cong V(1)\otimes V(1)\otimes V(1).$$
    The Tally value $1$ is the multiplicity of this tensor-product
    summand.

    @TT "L#\"nodes\""@ gives the ambient labels.  A diagram drawn from
    @TT "L#\"cartanMatrix\""@ uses local labels $1,2,3$ in that order,
    so those local nodes correspond here to ambient nodes $1,3,4$.

    Node lists start at @BOLD "1"@, exactly as displayed by
    @TO dynkinDiagram@.  Macaulay2 matrix positions start at @BOLD "0"@.
    The @TT "nodes0"@ field gives positions for operations such as
    @TO submatrix@; do not pass it to a command expecting node
    labels:
  Example
    L#"nodes"                  -- node labels: {1,3,4}
    L#"nodes0"                 -- matrix positions: {0,2,3}
    submatrix(A, L#"nodes0", L#"nodes0") == L#"cartanMatrix"
  Text
    Cartan matrices use rows indexed by coroots:
    $A_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$.  Mathematical node
    $i$ therefore corresponds to row and column $i$, while the same entry
    is addressed in Macaulay2 using position $i-1$.  In the following
    $G_2$ matrix, for example, @TT "G_(0,1)"@ is $A_{12}$:
  Example
    G = matrix {{2,-3},{-1,2}}
    print dynkinDiagram G
    G_(0,1)                    -- <alpha_2, alpha_1^vee> = -3
    G_(1,0)                    -- <alpha_1, alpha_2^vee> = -1
  Text
    Root computations return @EM "simple-root coordinates"@: the list
    $\{c_1,\ldots,c_r\}$ means $\sum_i c_i\alpha_i$.  Representation
    computations instead use @EM "Dynkin labels"@ by default.  Convert between
    the two with @TO toFundamental@ and @TO toSimple@.  In particular,
    column $j$ of $A$ is the Dynkin-label vector of $\alpha_j$:
  Example
    simpleRoots G
    toFundamental(G, {1,0})    -- alpha_1: first column of G
    toFundamental(G, {0,1})    -- alpha_2: second column of G
    theta = highestRoot G      -- {3,2}, meaning 3 alpha_1 + 2 alpha_2
    thetaDynkin = toFundamental(G, theta)
    weylDimension(G, thetaDynkin) -- 14, the adjoint representation
  Text
    Common outputs can be inspected with ordinary Macaulay2 operations.
    Root matrices have one root per row, a @TO Tally@ has irreducibles as
    keys and multiplicities as values, and the named fields of other outputs are accessed
    by their documented string keys:
  Example
    R = nilradicalRoots(A, removed);
    numrows R
    entries R
    T = leviHighestWeights(A, 1, removed);
    keys T
    values T
    L#"cartanMatrix"
  Text
    Most parabolic commands use removed nodes.  The two coset commands
    @TO parabolicCosetReps@ and @TO parabolicDotImages@ are
    exceptions: their argument @TT "I"@ is the complementary set of
    @EM "Levi"@ nodes.
SeeAlso
  dynkinDiagram
  dynkinDiagramData
  leviSubalgebra
  toFundamental
  toSimple
  nilradicalRoots
  leviHighestWeights
///

doc ///
Key
  "Building a Cartan matrix and root system"
Headline
  Cartan matrices, their types, and associated root systems
Description
  Text
    The commands @TO cartanA@, @TO cartanB@, @TO cartanC@,
    @TO cartanD@, @TO cartanE@, @TO cartanF4@, @TO cartanG2@ build
    Cartan matrices of the classical and exceptional finite types.
    Note the offset: @TT "cartanA n"@ returns the $(n-1)\times(n-1)$
    Cartan matrix of type $A_{n-1}$ (so @TT "cartanA 4"@ is
    $\mathfrak{sl}_4$, of rank $3$).  @TO dynkinDiagram@ draws the
    diagram in text; on a multi-edge the arrow points from the
    @EM "long"@ node to the @EM "short"@ node:
  Example
    dynkinDiagram cartanA 5
    dynkinDiagram cartanB 4
    dynkinDiagram cartanC 4
    dynkinDiagram cartanD 5
    dynkinDiagram cartanE 6
    dynkinDiagram cartanF4()
    dynkinDiagram cartanG2()
  Text
    Cartan type, Coxeter numbers, and the Cartan-matrix conditions can be checked
    across all finite types.  For $B_4 = \mathfrak{so}_9$:
  Example
    A = cartanB 4
    isCartanMatrix A
    cartanType A
    coxeterNumber A
    dualCoxeterNumber A
  Text
    From a finite-type Cartan matrix @TT "A"@ one can compute
    @TO simpleRoots@, @TO coroots@,
    @TO fundamentalWeights@, @TO weylVector@ (the sum of the fundamental
    weights, equal in finite type to the half-sum of positive roots,
    in @EM "simple-root"@ coordinates),
    @TO cartanPositiveRoots@, @TO highestRoot@ (also returned in
    simple-root coordinates, which is why the highest root of $B_4$
    comes out as $(1,2,2,2)$ rather than $(0,1,0,0)$).
  Example
    cartanPositiveRoots A
    weylVector A
    highestRoot A
    toFundamental(A, highestRoot A)
    rootInnerProduct(A, {1,0,0,0}, {0,0,0,1})
  Text
    In particular, @TT "highestRoot A"@ returns @TT "{1,2,2,2}"@,
    which means
    $$\theta=\alpha_1+2\alpha_2+2\alpha_3+2\alpha_4.$$
    It is not a highest-weight label.  Converting it gives
    @TT "toFundamental(A, highestRoot A) == {0,1,0,0}"@, the Dynkin
    labels of the adjoint highest weight.
  Text
    The @TO rootInnerProduct@ here is the symmetrized Cartan form
    $B=DA$.  The diagonal symmetrizer $D$ is normalized independently
    on each connected component by setting its first entry to
    $1$; consequently $\lVert\alpha_i\rVert^2=2D_{ii}$.  This records
    relative root lengths but does not globally normalize long roots
    to squared length $2$ (for example, the long $G_2$ roots have
    squared length $6$):
  Example
    rootInnerProduct(A, {0,0,0,1}, {0,0,0,1})
    rootInnerProduct(A, {1,0,0,0}, {1,0,0,0})
SeeAlso
  cartanA
  cartanType
  dynkinDiagram
  cartanPositiveRoots
  highestRoot
///

doc ///
Key
  "Representation-theoretic invariants"
Headline
  Dimensions, weights, and multiplicities of integrable highest-weight modules
Description
  Text
    For a dominant integral weight $\lambda$ (given as a list of
    non-negative Dynkin labels of length equal to the rank), the
    package computes the following invariants of the irreducible
    highest-weight representation $V(\lambda)$:
  Text
    @UL {
      (TO weylDimension, ": ", TEX "$\\dim V(\\lambda)$ via the Weyl dimension formula."),
      (TO weightsOfRep, ": the distinct weights in the support of ", TEX "$V(\\lambda)$", ", enumerated by the string method."),
      (TO dominantWeightsInRep, ": the dominant weights occurring in ", TEX "$V(\\lambda)$", "."),
      (TO weightMultiplicity, ": ", TEX "$\\dim V(\\lambda)_\\mu$", " for a specified weight, via Freudenthal's recursion."),
      (TO weightOrbit, ": the Weyl-group orbit of any weight (not necessarily dominant), under the linear action.")
    }@
  Text
    A collection of classical dimensions across types $B$, $C$, $D$,
    $G_2$, $F_4$: the vector ($7$) and spin ($8$) representations of
    $\mathfrak{so}_7$, the standard ($6$) and adjoint ($21$) of
    $\mathfrak{sp}_6$, the vector ($8$) and two half-spinors
    ($8$, $8$) of $\mathfrak{so}_8$ (the three representations related
    by triality), the $7$-
    and $14$-dimensional representations of $G_2$, and the $26$-dimensional
    quasi-minuscule representation and $52$-dimensional adjoint of $F_4$.
    Here @EM "quasi-minuscule"@ means that all nonzero weights form
    one Weyl orbit; the zero weight may also occur:
  Example
    weylDimension(cartanB 3, {1,0,0})
    weylDimension(cartanB 3, {0,0,1})
    weylDimension(cartanC 3, {1,0,0})
    weylDimension(cartanC 3, {2,0,0})
    weylDimension(cartanD 4, {1,0,0,0})
    weylDimension(cartanD 4, {0,0,1,0})
    weylDimension(cartanD 4, {0,0,0,1})
    weylDimension(cartanG2(), {1,0})
    weylDimension(cartanG2(), {0,1})
    weylDimension(cartanF4(), {0,0,0,1})
    weylDimension(cartanF4(), {1,0,0,0})
  Text
    Some representative exceptional dimensions of $E_6$, $E_7$, $E_8$ are
    the $27$-dimensional fundamental representation of $E_6$, the $56$-dimensional
    minuscule representation of $E_7$, and the $248$-, $3875$- and
    $147250$-dimensional representations of $E_8$.  A highest weight is
    called @EM "minuscule"@ when all weights of its irreducible
    representation form one Weyl-group orbit; each weight space then has
    dimension $1$:
  Example
    weylDimension(cartanE 6, {1,0,0,0,0,0})
    weylDimension(cartanE 7, {0,0,0,0,0,0,1})
    weylDimension(cartanE 8, {0,0,0,0,0,0,0,1}) -- 248, adjoint
    weylDimension(cartanE 8, {1,0,0,0,0,0,0,0}) -- 3875
    weylDimension(cartanE 8, {0,1,0,0,0,0,0,0}) -- 147250
  Text
    Weight-space data and multiplicities work identically outside
    type A.  The adjoint $V(0,1,0)$ of $\mathfrak{so}_7 = B_3$ has
    dimension $21$; its dominant weights and the rank-$3$ zero-weight
    multiplicity are:
  Example
    weylDimension(cartanB 3, {0,1,0})
    dominantWeightsInRep(cartanB 3, {0,1,0})
    weightMultiplicity(cartanB 3, {0,1,0}, {0,0,0})
  Text
    For a type-A example, take the adjoint representation $V(1,0,1)$ of
    $\mathfrak{sl}_4$ (dimension $15$, zero-weight multiplicity equal
    to the rank $3$):
  Example
    weylDimension(cartanA 4, {1,0,1})
    dominantWeightsInRep(cartanA 4, {1,0,1})
    weightMultiplicity(cartanA 4, {1,0,1}, {0,0,0})
  Text
    For any finite-dimensional simple Lie algebra, the adjoint has
    highest weight equal to the highest root $\theta$.  Its nonzero
    weights are the roots, each with multiplicity $1$; its zero-weight
    space is the Cartan subalgebra.  Thus
    $$\dim V(\theta)=|\Phi|+r,\qquad
      \dim V(\theta)_0=r,$$
    where $r$ is the rank.  For $\mathfrak{sl}_5$, first convert the
    highest root from simple-root coordinates to Dynkin labels:
  Example
    highestRoot cartanA 5
    toFundamental(cartanA 5, highestRoot cartanA 5)
    weylDimension(cartanA 5, {1,0,0,1})
    weightMultiplicity(cartanA 5, {1,0,0,1}, {0,0,0,0})
  Text
    The first two outputs describe the same highest weight:
    $\theta=\alpha_1+\alpha_2+\alpha_3+\alpha_4$ has Dynkin labels
    $(1,0,0,1)$.  The last two outputs give dimension $24=20+4$
    and zero-weight multiplicity $4$.  Concretely, this zero-weight
    space consists of diagonal $5\times5$ matrices of trace zero.
    For $G_2$, the corresponding numbers are $14=12+2$ and $2$:
  Example
    weylDimension(cartanG2(), {0,1})
    weightMultiplicity(cartanG2(), {0,1}, {0,0})
  Text
    A tensor decomposition can be checked by summing the dimensions of
    its summands, with multiplicity; the result should be
    $\dim V(\lambda)\,\dim V(\mu)$.
SeeAlso
  weylDimension
  weightsOfRep
  weightMultiplicity
  dominantWeightsInRep
  weightOrbit
///

doc ///
Key
  "Tensor product decompositions and Littlewood-Richardson"
Headline
  Tensor product decompositions by the Racah--Speiser formula
Description
  Text
    @TO tensorDecomposition@ computes the Clebsch-Gordan decomposition
    $$V(\lambda)\otimes V(\mu) \;=\; \bigoplus_\nu c^\nu_{\lambda,\mu}\,V(\nu)$$
    by straightening the shifted weights from the complete character
    of the smaller tensor factor.  A regular shifted weight contributes
    with the sign of the Weyl element taking it to the dominant chamber;
    a singular shifted weight contributes zero.  The output is a
    @TO Tally@ whose keys are @TO Irrep@ objects and
    whose values are the multiplicities $c^\nu_{\lambda,\mu}$.
  Text
    For any of the classical types A, B, C, D the Dynkin-label basis
    and the partition basis are related by
    @TO fundamentalToPartition@, which detects the classical type and
    node ordering automatically.  With
    @TT "WeightFormat => \"Partition\""@ the keys of the decomposition
    are shown as partitions.  In types B and D a "spin" summand
    appears as a partition with half-integer entries (so
    @TT "{1/2,1/2,1/2}"@ for the 8-dimensional spin representation of
    $\mathfrak{so}_7$); in type D the second half-spin representation
    is marked by a negative final entry (so
    @TT "{1/2,1/2,1/2,-1/2}"@ in $\mathfrak{so}_8$).
  Text
    The @TEX "$\\mathfrak{sl}_3$"@ standard $\otimes$ dual-standard
    decomposition is $V(\omega_1)\otimes V(\omega_2)= V(\omega_1+\omega_2)
    \oplus V(0)$ ($3\otimes\bar 3 = 8+1$), which in partition form is
    $(1,0,0)\otimes(1,1,0) = (2,1,0)+(0,0,0)$:
  Example
    tensorDecomposition(cartanA 3, {1,0,0}, {1,1,0}, WeightFormat => "Partition")
  Text
    Thus the two Tally entries translate literally to
    $$V(1,0,0)\otimes V(1,1,0)
      \;\cong\; V(2,1,0)\oplus V(0,0,0).$$
    The number after @TT "=>"@ is the multiplicity of that summand;
    both multiplicities are $1$ here.
  Text
    For a larger Littlewood-Richardson example in
    @TEX "$\\mathfrak{sl}_5$"@, decompose
    $(3,1,0,0,0)\otimes(3,1,0,0,0)$:
  Example
    tensorDecomposition(cartanA 5, {3,1,0,0,0}, {3,1,0,0,0}, WeightFormat => "Partition")
  Text
    For adjoint $\otimes$ adjoint of @TEX "$\\mathfrak{sl}_3$"@, the
    adjoint is the partition $(2,1,0)$ of shape $(2,1)$.  It appears
    with multiplicity $2$ in the product
    ($8\otimes 8 = 27+10+\bar{10}+8+8+1$):
  Example
    tensorDecomposition(cartanA 3, {2,1,0}, {2,1,0}, WeightFormat => "Partition")
  Text
    Reading the Tally as a direct sum gives
    $$V(2,1,0)^{\otimes 2}\cong
      V(4,2,0)\oplus V(3,3,0)\oplus V(3,0,0)
      \oplus V(2,1,0)^{\oplus 2}\oplus V(0,0,0).$$
    In particular, @TT "V(2,1,0) => 2"@ means two copies of the
    adjoint representation, not a weight with final coordinate $2$.
  Text
    For $B_3$ ($\mathfrak{so}_7$), $V(\omega_1)\otimes V(\omega_1) =
    V(2\omega_1)\oplus V(\omega_2)\oplus V(0)$ with $7\otimes 7 =
    27+21+1$.  Here the invariant symmetric form of $\mathfrak{so}_7$
    sits inside $\mathrm{Sym}^2 V$, so
    $\mathrm{Sym}^2 V = V(2\omega_1)\oplus V(0)$
    (traceless symmetric tensors and the invariant, dimensions $27+1=28$) and
    $\wedge^2 V = V(\omega_2)$ (the adjoint, of dimension $21$).  In partition
    form the three irreducible summands are $(2,0,0)$, $(1,1,0)$,
    and $()$:
  Example
    tensorDecomposition(cartanB 3, {1,0,0}, {1,0,0}, WeightFormat => "Partition")
  Text
    Two copies of the $B_3$ spin representation tensor to $8\otimes 8
    = 35+21+7+1$.  The spin representation of $\mathfrak{so}_7$ is orthogonal
    self-dual, so the invariant pairing lies in $\mathrm{Sym}^2
    (\text{spin})$: $\mathrm{Sym}^2(\text{spin}) =
    V(2\omega_3)\oplus V(0)$ (dimensions $35+1=36$), and
    $\wedge^2(\text{spin}) = V(\omega_2)\oplus V(\omega_1)$
    (dimensions $21+7=28$).  With both inputs in partition form as
    $(1/2,1/2,1/2)$, the summands are $(1,1,1)$ (traceless
    $\mathrm{Sym}^2$ of spin, of dimension $35$), $(1,1,0)$ (the adjoint, part of
    $\wedge^2$, of dimension $21$), $(1,0,0)$ (the vector representation, the other summand of
    $\wedge^2$, of dimension $7$), and $()$ (the invariant pairing).  All
    integer, since $m_3$ is even on both sides:
  Example
    tensorDecomposition(cartanB 3, {1/2,1/2,1/2}, {1/2,1/2,1/2}, WeightFormat => "Partition")
  Text
    Tensoring a nonspin representation with a spin representation gives
    half-integer partitions: $V(\omega_1)\otimes V(\omega_3)=
    V(\omega_3)\oplus V(\omega_1+\omega_3)$ is $7\otimes 8=8+48$, and
    partition coordinates express the summands as the half-integer tuples
    $(1/2,1/2,1/2)$ and $(3/2,1/2,1/2)$:
  Example
    tensorDecomposition(cartanB 3, {1,0,0}, {1/2,1/2,1/2}, WeightFormat => "Partition")
  Text
    Converting the Dynkin labels of a spin highest weight gives the
    half-integer output explicitly:
  Example
    fundamentalToPartition(cartanB 3, {0,0,1})           -- so_7 spin
    fundamentalToPartition(cartanD 4, {0,0,0,1})         -- so_8 half-spin (+)
    fundamentalToPartition(cartanD 4, {0,0,1,0})         -- so_8 half-spin (-)
  Text
    In contrast, for $\mathfrak{sp}_6 = C_3$, $V(\omega_1)\otimes
    V(\omega_1) = V(2\omega_1)\oplus V(\omega_2)\oplus V(0)$ has
    $6\otimes 6 = 21+14+1$, but the invariant (symplectic) form now
    lives inside $\wedge^2 V$, not $\mathrm{Sym}^2 V$.  Thus
    $\mathrm{Sym}^2 V = V(2\omega_1)$ (the adjoint, of dimension $21$) and
    $\wedge^2 V = V(\omega_2)\oplus V(0)$ (primitive $2$-forms plus
    the symplectic invariant, dimensions $14+1=15$).  In partition form the
    summands are the same shapes $(2,0,0) + (1,1,0) + ()$ as in the
    $B_3$ case, but their interpretation is dual:
  Example
    tensorDecomposition(cartanC 3, {1,0,0}, {1,0,0}, WeightFormat => "Partition")
  Text
    For $D_4$ ($\mathfrak{so}_8$), tensoring the two half-spin
    representations of opposite chirality gives $V(\omega_3)\otimes
    V(\omega_4)= V(\omega_1)\oplus V(\omega_3+\omega_4)$, i.e.\
    $8\otimes 8 = 8_v+56$.  The two half-spinors in partition form
    are $(1/2,1/2,1/2,\pm 1/2)$ (sign chooses the spinor chirality),
    and the output summands are the vector partition $(1,0,0,0)$ and
    the shape $(1,1,1,0)$ (the $56$-dimensional $\wedge^3 V_v$, where
    $V_v$ is the $8$-dimensional vector of $\mathfrak{so}_8$):
  Example
    tensorDecomposition(cartanD 4, {1/2,1/2,1/2,1/2}, {1/2,1/2,1/2,-1/2}, WeightFormat => "Partition")
  Text
    Half-spin tensored with itself in $D_4$ (same chirality) stays
    inside integer partitions, because the parity of $m_{n-1}+m_n$ is
    preserved by tensor: $V(\omega_4)\otimes V(\omega_4) = V(0)
    \oplus V(\omega_2)\oplus V(2\omega_4)$, i.e.\ $8\otimes 8 =
    1+28+35$.  The half-spin of $\mathfrak{so}_8$ is orthogonal
    self-dual (with symmetric invariant), so the trivial summand
    sits inside $\mathrm{Sym}^2$: $\mathrm{Sym}^2(\text{half-spin})
    = V(2\omega_4)\oplus V(0)$ (dimensions $35+1$) and
    $\wedge^2(\text{half-spin}) = V(\omega_2)$ (the adjoint, of dimension $28$):
  Example
    tensorDecomposition(cartanD 4, {1/2,1/2,1/2,1/2}, {1/2,1/2,1/2,1/2}, WeightFormat => "Partition")
  Text
    For the exceptional $G_2$, the decomposition of
    $\mathrm{ad}\otimes\mathrm{ad}$ into irreducibles picks up five
    summands of dimensions $1 + 14 + 27 + 77 + 77 = 196 = 14^2$:
  Example
    tensorDecomposition(cartanG2(), {0,1}, {0,1})
  Text
    In Dynkin labels, the last output says
    $$V(0,1)\otimes V(0,1)\cong
      V(0,0)\oplus V(0,1)\oplus V(2,0)\oplus V(3,0)\oplus V(0,2).$$
    The dimensions are respectively $1,14,27,77,77$.
SeeAlso
  tensorDecomposition
  tensorCoefficient
  fundamentalToPartition
  WeightFormat
///

doc ///
Key
  "The Weyl group"
Headline
  Reflections, length, descents, Bruhat order, Poincaré polynomial
Description
  Text
    The Weyl group is represented concretely by sequences of
    simple-reflection indices (words in $\{1,\dots,r\}$) and by the
    matrices they produce acting on the simple-root basis.
  Text
    @UL {
      (TO coxeterMatrix, ": the Coxeter exponents ", TEX "$m_{ij}$", " determined by the Cartan entries."),
      (TO reflectionMatrix, ": the matrix of ", TEX "$s_i$", " in the simple-root basis."),
      (TO weylElementMatrix, ": the matrix of the element ", TEX "$w = s_{i_1}\\cdots s_{i_k}$", " for a given word."),
      (TO weylLength, ": Coxeter length ", TEX "$\\ell(w)$", "."),
      (TO reducedWord, ": a reduced word for the same Weyl-group element."),
      (TO leftDescentSet, " and ", TO rightDescentSet, "."),
      (TO weylInversionData, ": inversions of ", TEX "$w$", " as positive roots (in simple-root coordinates) together with the word and length."),
      (TO longElement, " and ", TO weylGroupOrder, " for the longest element and ", TEX "$|W|$", "."),
      (TO weylGroupElements, ": full enumeration of ", TEX "$W$", " (finite type only), indexed by length."),
      (TO bruhatLessOrEqual, ": the Bruhat order."),
      (TO weylLengthGenerating, ": the Poincaré polynomial ", TEX "$\\sum_{w\\in W} q^{\\ell(w)}$", " in ", TEX "$\\mathbb Z[q]$", ".")
    }@
  Example
    A = cartanA 4
    weylGroupOrder A
    P = weylLengthGenerating A;
    P
    factor P
    w = longElement A
    weylLength(A, w)
  Text
    The coefficient of $q^k$ counts Weyl-group elements of length $k$.
    For this example the polynomial is
    $$1+3q+5q^2+6q^3+5q^4+3q^5+q^6.$$

    The factorization output means
    $$P(q)=(q+1)^2(q^2+q+1)(q^2+1).$$
    These are the irreducible factors over $\mathbb Z$.  They also recover
    the usual $q$-integer formula
    $$P(q)=[4]_q!=[1]_q[2]_q[3]_q[4]_q,$$
    since $[4]_q=(q+1)(q^2+1)$.
  Text
    The same commands work in any finite type.  $W(G_2)$ is the
    dihedral group of order $12$ with longest element of length $6$;
    $W(F_4)$ has order $1152$; $W(D_4)$ has order $192$:
  Example
    weylGroupOrder cartanG2()
    PG2 = weylLengthGenerating cartanG2();
    PG2
    factor PG2
    longElement cartanG2()
    weylGroupOrder cartanF4()
    weylGroupOrder cartanD 4
  Text
    For $G_2$, the factorization is
    $$(q+1)^2(q^2+q+1)(q^2-q+1)=[2]_q[6]_q.$$
    Evaluating at $q=1$ gives $12$, the Weyl-group order.
  Text
    The number of roots and the size of the Weyl group can be very
    different.  In type $E_8$:
  Example
    A = cartanE 8;
    (numrows cartanPositiveRoots A, weylGroupOrder A,
     length longElement A)
  Text
    The output $(120,696729600,120)$ says that $E_8$ has $120$
    positive roots, its Weyl group has $696729600$ elements, and its
    longest element has length $120$.  The last number equals the
    number of positive roots because the longest element sends all
    of them to negative roots.  The order, Poincaré polynomial,
    and longest element can be computed without listing the entire
    group; @TO weylGroupElements@ actually lists elements, subject to
    @TO MaxElements@.
  Text
    Bruhat order, descent sets and inversion data are available
    uniformly.  For $B_3$:
  Example
    B = cartanB 3
    u = {1,2,3,2,1}
    reducedWord(B, u)
    leftDescentSet(B, u)
    rightDescentSet(B, u)
    weylInversionData(B, u)
  Text
    The last hash table says that $u$ has length $5$ and inversion set
    $$\{\alpha_1,\ \alpha_1+\alpha_2,\
      \alpha_1+\alpha_2+\alpha_3,\
      \alpha_1+\alpha_2+2\alpha_3,\
      \alpha_1+2\alpha_2+2\alpha_3\}.$$
    Its five roots are the five simple-root-coordinate rows shown in
    the @TT "inversions"@ field.
SeeAlso
  weylGroupOrder
  weylLengthGenerating
  longElement
  weylInversionData
  bruhatLessOrEqual
  leftDescentSet
  rightDescentSet
///

doc ///
Key
  "Parabolic decompositions and the nilradical grading"
Headline
  Derived-Levi decomposition of every graded piece of the nilradical
Description
  Text
    The @TT "parabolic"@ input is the list of @EM "removed"@ Dynkin
    nodes.  Its entries are the 1-based labels printed by
    @TO dynkinDiagram@.  The removed nodes have degree $1$ and the
    remaining Levi nodes have degree $0$.  For example, removing node
    $2$ from $A_5$ leaves ambient nodes $\{1,3,4,5\}$ in the Levi:
  Example
    A = cartanA 6;
    dynkinDiagram A
    L = leviSubalgebra(A, {2});
    L#"nodes"
  Text
    This choice grades $\mathfrak g = \bigoplus_d \mathfrak g_d$ and
    determines the standard parabolic
    $\mathfrak p = \mathfrak l \oplus \mathfrak n$ whose Levi
    subalgebra $\mathfrak l$ sits in degree $0$ and whose nilradical
    $\mathfrak n = \bigoplus_{d\geq 1} \mathfrak g_d$ sits in positive
    degree.  In finite type, each $\mathfrak g_d$ is a finite-dimensional
    $\mathfrak l$-module.  The output records its restriction to the
    semisimple derived algebra $[\mathfrak l,\mathfrak l]$; the
    central character is omitted.

    @TO leviHighestWeights@ returns that module decomposition.
    Continuing with @TEX "$\\mathfrak{sl}_6/P_2$"@ (the Grassmannian
    $\mathrm{Gr}(2,6)$): removing node $2$ leaves a disconnected Levi
    $A_1 \times A_3$, and the tangent space $\mathfrak g_1$ is the
    defining representation of $\mathfrak{gl}_2$ tensored with
    $\wedge^3$ of the defining representation of $\mathfrak{gl}_4$.  In partition
    form the two factors are the partitions $(1)$ and $(1,1,1)$:
  Example
    dynkinDiagram (L#"cartanMatrix")
    leviHighestWeights(A, 1, {2}, WeightFormat => "Partition")
  Text
    The output
    @TT "Tally{V(1,0) ** V(1,1,1,0) => 1}"@ means
    $$\mathfrak g_1\big|_{\mathfrak{sl}_2\oplus\mathfrak{sl}_4}
      \;\cong\; \mathbb C^2\otimes\bigwedge^3\mathbb C^4.$$
    It is one irreducible summand, of dimension $2\cdot4=8$; the
    displayed weight lists are partition coordinates.

    The key of the Tally is an @TO Irrep@ recording the tensor product
    structure: when the Levi is disconnected, each connected component
    contributes one tensor factor, printed as
    @TT "V(a,...) ** V(b,...)"@.  The Levi diagram is relabeled locally;
    @TT "L#\"nodes\""@ records how its nodes sit in the ambient diagram.

    A @EM "Heisenberg-type"@ nilradical is two-step nilpotent with a
    one-dimensional center.  In its grading
    $\mathfrak n=\mathfrak g_1\oplus\mathfrak g_2$, the center is
    $\mathfrak g_2$, and the bracket on $\mathfrak g_1$ is a nondegenerate
    alternating form with values in $\mathfrak g_2$.
    The $E_7$ grading at node $1$ is an example: the
    coefficient of $\alpha_1$ in the highest root is $2$, so the
    filtration has depth $2$; the residual Levi is $D_6$, with
    $\mathfrak g_1$ a $32$-dimensional half-spin representation
    of that $D_6$ and $\mathfrak g_2$ the $1$-dimensional center of
    the nilradical.  (Check: $|\Phi^+(E_7)|-|\Phi^+(D_6)| = 63-30 =
    33 = 32 + 1$.)
  Example
    leviHighestWeights(cartanE 7, 1, {1})
    leviHighestWeights(cartanE 7, 2, {1})
  Text
    The two outputs mean
    $$\mathfrak g_1\cong V_{D_6}(0,1,0,0,0,0),
      \qquad \mathfrak g_2\cong V_{D_6}(0,0,0,0,0,0)=\mathbb C.$$
    These coordinates use the inherited Levi order on ambient nodes
    $\{2,3,4,5,6,7\}$, not the standalone @TT "cartanD 6"@ order.  In
    this diagram the first module is the $32$-dimensional half-spin
    representation; the second is the $1$-dimensional trivial module.
  Text
    In $D_4$, remove the central node
    $2$.  The Levi is $A_1\times A_1\times A_1$ (three disjoint ends),
    and $\mathfrak g_1$ is the external tensor product $V(1)\otimes
    V(1)\otimes V(1)$ --- the three outer nodes permuted by the
    triality automorphism:
  Example
    leviHighestWeights(cartanD 4, 1, {2})
  Text
    Here @TT "V(1) ** V(1) ** V(1) => 1"@ means
    $$\mathfrak g_1\big|_{A_1^3}
      \;\cong\; V_{A_1}(1)\otimes V_{A_1}(1)\otimes V_{A_1}(1).$$
    Each factor is $2$-dimensional, so $\dim\mathfrak g_1=8$.
  Text
    For $D_5$, the (complex) smooth quadric corresponds to the
    parabolic $\{1\}$; $\mathfrak g_1$ is the defining vector representation of
    $D_4$, and there is no $\mathfrak g_2$:
  Example
    leviHighestWeights(cartanD 5, 1, {1})
    leviHighestWeights(cartanD 5, 2, {1})
  Text
    These outputs say
    $$\mathfrak g_1\cong V_{D_4}(1,0,0,0),\qquad
      \mathfrak g_2=0.$$
    An empty @TT "Tally{}"@ means that the graded piece vanishes.
  Text
    For $B_3$, the parabolic $\{2\}$ has Levi $A_1 \times A_1$:
  Example
    leviHighestWeights(cartanB 3, 1, {2})
    leviHighestWeights(cartanB 3, 2, {2})
  Text
    Here the two lines translate to
    $$\mathfrak g_1\cong V_{A_1}(1)\otimes V_{A_1}(2),\qquad
      \mathfrak g_2\cong V_{A_1}(0)\otimes V_{A_1}(0)=\mathbb C.$$
    Their dimensions are $6$ and $1$.
  Text
    @BOLD "The full graded filtration."@  For a standard parabolic in a finite-dimensional simple Lie algebra,
    the grading is @EM "finite"@ in depth: if $S\subset\{1,\dots,r\}$
    is the set of removed (degree-$1$) nodes, then the depth of the
    filtration equals $\sum_{i\in S} c_i(\theta)$, where $c_i(\theta)$
    is the coefficient of the simple root $\alpha_i$ in the highest
    root $\theta$ expressed in the simple-root basis.  Iterating
    @TO leviHighestWeights@ over $d = 1, 2, 3, \dots$ gives
    the full derived-Levi-module structure of the nilradical
    $\mathfrak n = \bigoplus_{d\geq 1}\mathfrak g_d$, and the sum of
    the dimensions of the graded pieces equals $\dim \mathfrak n =
    |\Phi^+| - |\Phi^+_L|$.  Thus the nilradical of any finite-type
    standard parabolic can be read as a direct sum of derived-Levi
    irreducibles.

    @BOLD "$G_2$ with short-node parabolic."@  The highest root of
    $G_2$ in the simple-root basis is $\theta = 3\alpha_1+2\alpha_2$;
    removing node $1$ (depth $3$, Levi $A_1$ at node $2$) produces a
    three-step filtration with pieces of dimensions $2, 1, 2$:
  Example
    leviHighestWeights(cartanG2(), 1, {1})
    leviHighestWeights(cartanG2(), 2, {1})
    leviHighestWeights(cartanG2(), 3, {1})
  Text
    Here $\mathfrak g_1 = V(1)$ (the standard $A_1$ representation, of dimension $2$),
    $\mathfrak g_2 = V(0)$ (the trivial $A_1$ representation, of dimension $1$), and
    $\mathfrak g_3 = V(1)$ again; total $5 = |\Phi^+(G_2)|
    -|\Phi^+(A_1)| = 6 - 1$.

    @BOLD "$F_4$ with a middle-node parabolic."@  The highest root
    of $F_4$ in the simple-root basis is
    $\theta = 2\alpha_1+3\alpha_2+4\alpha_3+2\alpha_4$, so removing
    node $3$ gives a depth-$4$ filtration.  The Levi is
    $A_2\times A_1$:
  Example
    leviHighestWeights(cartanF4(), 1, {3})
    leviHighestWeights(cartanF4(), 2, {3})
    leviHighestWeights(cartanF4(), 3, {3})
    leviHighestWeights(cartanF4(), 4, {3})
  Text
    Written as representations of $A_2\times A_1$, these four outputs
    are
    $$\begin{aligned}
      \mathfrak g_1&\cong V_{A_2}(1,0)\otimes V_{A_1}(1),\\
      \mathfrak g_2&\cong V_{A_2}(0,1)\otimes V_{A_1}(2),\\
      \mathfrak g_3&\cong V_{A_2}(0,0)\otimes V_{A_1}(1),\\
      \mathfrak g_4&\cong V_{A_2}(1,0)\otimes V_{A_1}(0).
    \end{aligned}$$
    Thus @TT "**"@ means tensor product across Levi components, while
    separate Tally keys would mean direct sum.

    @BOLD "$E_7$ Heisenberg-type grading."@  Removing node $1$ of
    $E_7$ (coefficient $2$ in the highest root) gives a depth-$2$
    filtration whose $\mathfrak g_1$ is a single Levi irreducible
    and whose $\mathfrak g_2$ is $1$-dimensional --- the center of
    a classical Heisenberg-type nilradical.  Here we display
    @EM "all"@ graded pieces to see where the filtration stops:
  Example
    for d from 1 to 3 list (d, leviHighestWeights(cartanE 7, d, {1}))
  Text
    The empty @TT "Tally{}"@ at $d=3$ shows that the filtration
    has terminated.

    @BOLD "$E_8$ with a deep filtration."@  Removing a node with a
    large $\theta$-coefficient in $E_8$ produces a filtration of
    depth greater than two.  Iterating @TO leviHighestWeights@
    over successive degrees gives the full derived-Levi-module structure
    one degree at a time:
  Example
    netList prepend({"degree", "Levi decomposition"},
        for d from 1 to 7 list {d, leviHighestWeights(cartanE 8, d, {4})})
  Text
    Each row gives the degree and its derived-Levi decomposition.  The
    nonzero pieces occur in degrees $1$ through $6$; the empty
    @TT "Tally{}"@ in degree $7$ means $\mathfrak g_7=0$.  Each
    nonzero graded piece here is a tensor product of irreducibles of
    the simple Levi factors.

    @BOLD "$D_6$ with the spinor parabolic."@  Removing the spinor
    node $6$ of $D_6$: the coefficient of $\alpha_6$ in the highest
    root is exactly $1$, which (by the general principle above)
    forces the filtration to be depth $1$.  A parabolic with this
    property is called @EM "cominuscule"@: the removed simple root has
    coefficient $1$ in the highest root, and the nilradical is abelian.
    The single graded piece
    $\mathfrak g_1$ is $\wedge^2$ of the defining representation of
    the $A_5$ Levi:
  Example
    for d from 1 to 3 list leviHighestWeights(cartanD 6, d, {6}, WeightFormat => "Partition")
  Text
    In partition form, $\mathfrak g_1$ has highest weight $(1,1)$, so
    it is $\wedge^2 \mathbb C^6$ and has dimension $\binom62 = 15 =
    |\Phi^+(D_6)| - |\Phi^+(A_5)|$.

    @BOLD "Multiple removed nodes."@  A parabolic with several
    removed nodes induces a multi-step grading whose depth is the
    sum of the coefficients $c_i(\theta)$ over all removed $i$.
    For $\mathrm{Fl}(1,3,5) \subset \mathrm{SL}_5$, remove nodes
    $\{1,3\}$ of $A_4$ (Levi $A_1\times A_1$):
  Example
    for d from 1 to 3 list leviHighestWeights(cartanA 5, d, {1,3}, WeightFormat => "Partition")
  Text
    The two keys in degree $1$ are direct summands:
    $$\mathfrak g_1\cong
      \bigl(V(1,0)\otimes V(0,0)\bigr)
      \oplus\bigl(V(1,0)\otimes V(1,0)\bigr).$$
    The next two outputs mean
    $\mathfrak g_2\cong V(0,0)\otimes V(1,0)$ and
    $\mathfrak g_3=0$.  Their dimensions are $6$ and $2$, totaling
    $8=\dim\mathfrak n$.

    Further parabolic computations: @TO leviSubalgebra@,
    @TO leviConnectedComponents@, @TO nilradicalRoots@,
    @TO parabolicDegrees@, @TO parabolicCosetReps@ (minimal-length
    coset representatives of $W/W_L$), @TO dotActionOnWeight@ and
    @TO parabolicDotImages@ (the affine, or "dot", action
    $w\cdot\lambda = w(\lambda+\rho)-\rho$).
SeeAlso
  leviHighestWeights
  dynkinDiagram
  leviSubalgebra
  nilradicalRoots
  parabolicDegrees
  parabolicCosetReps
  Irrep
  WeightFormat
///

doc ///
Key
  LeviIrrep
  (symbol ==,LeviIrrep,LeviIrrep)
Headline
  A Levi irreducible with its Cartan matrix and optional central character
Description
  Text
    A @TT "LeviIrrep"@ records an irreducible highest-weight
    representation of a Levi subalgebra.  Along with its @TO Irrep@,
    it gives the derived-Levi Cartan matrix, the order of the connected
    components, the weight coordinates, and whether the central character
    is known.  These fields can be read using their names.
  Text
    A value produced by @TO irrepData@ has fields
    @TT "semisimple"@, @TT "parentCartan"@, @TT "components"@,
    @TT "weightFormat"@, @TT "centralCharacter"@,
    @TT "centralCharacterStatus"@, and
    @TT "centralCharacterBasis"@.  Because an @TO Irrep@ contains
    no central character, this output has @TT "centralCharacter => null"@
    and status @TT "unknown"@.
  Text
    With @TT "IncludeCentralCharacter => true"@,
    @TO kostantLieAlgCohomology@ also records the central character.  Its additional
    fields include @TT "semisimpleDynkinWeight"@,
    @TT "ambientComponents"@, @TT "ambientCartan"@,
    @TT "ambientWeight"@, @TT "leviNodes"@, and
    @TT "removedNodes"@.  The known @TT "centralCharacter"@ list is in
    removed-node order; see @TO IncludeCentralCharacter@ for its basis.
    The derived Levi may be affine or indefinite; in that case
    the field named @TT "semisimple"@
    denotes a symbolic integrable highest-weight Kac--Moody module.
  Example
    A = cartanA 3;
    H = kostantLieAlgCohomology(A, {1}, {0,0},
        IncludeCentralCharacter => true);
    V = first keys H#1;
    toString V
    (V#"semisimple", V#"centralCharacter", V#"removedNodes")
    irrepDimension V
  Text
    The printed bracket specifies the Cartan type and central character.  For
    example, @TT "V(1) [A_1; central=(-1)]"@ means the semisimple
    $A_1$ module $V(1)$ together with its one-dimensional center
    character.
  Text
    Equality compares the complete stored record, not just the displayed
    highest-weight tuple.  In particular, the Cartan matrix and any known
    central character participate in the comparison.
SeeAlso
  irrepData
  IncludeCentralCharacter
  Irrep
  irrepDimension
///

doc ///
Key
  irrepData
  (irrepData,StandardParabolic,Irrep)
  (irrepData,Matrix,Irrep)
  [irrepData,WeightFormat]
Headline
  Record the Cartan matrix acting on an Irrep
Usage
  W = irrepData(P, V)
  W = irrepData(AL, V)
Inputs
  P:StandardParabolic
    a standard parabolic whose derived Levi is the acting algebra
  AL:Matrix
    a possibly disconnected finite-type semisimple Cartan matrix
  V:Irrep
    one highest-weight factor for each connected component
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@
Outputs
  W:LeviIrrep
    the same irreducible with its Cartan matrix and an unknown central character
Description
  Text
    This command checks that the highest weights specify a
    finite-dimensional irreducible for each connected component of
    @TT "AL"@, then records @TT "AL"@ and the component order with
    @TT "V"@.  It cannot recover
    a reductive-center character from an @TO Irrep@ alone.  The
    @TT "P"@ form obtains @TT "AL"@ from @TO parabolicLevi@ automatically.
  Example
    V = new Irrep from {{1,0}}
    W = irrepData(cartanA 3, V);
    (W#"weightFormat", W#"components", W#"centralCharacterStatus")
    irrepDimension W
  Text
    The component table gives the $A_2$ highest weight $(1,0)$
    in Dynkin coordinates.  Its dimension is $3$, so this is the
    defining representation of $\mathfrak{sl}_3$.  The central-character
    status records whether additional central coordinates have been
    specified; it does not add a further irreducible factor.

SeeAlso
  LeviIrrep
  irrepDimension
  WeightFormat
///

doc ///
Key
  IncludeCentralCharacter
Headline
  Option retaining full Levi central characters in Kostant cohomology
Description
  Text
    Used by @TO kostantLieAlgCohomology@ and false by default.  By default,
    the output is @TT "Tally{Irrep => ZZ}"@ and the center is omitted.  With
    @TT "IncludeCentralCharacter => true"@, Tally keys are
    @TO LeviIrrep@ records and distinct full-Levi summands remain
    distinct.
  Text
    If the removed nodes are $j_1,\ldots,j_s$, the central-character
    list is in that order.  For an ambient Dynkin-label row $\mu$, its
    $j$-th entry is
    $$\chi_j=\sum_i\mu_i\bigl(A^T\bigr)^{-1}_{ij}.$$
    The record describes this as the removed-node ambient fundamental-
    coweight basis.  The exact basis string is also stored in
    @TT "centralCharacterBasis"@, and @TT "ambientWeight"@
    retains $w\mathbin{\cdot}\lambda$ in ambient Dynkin coordinates.
    This weight is Levi-dominant but need not be ambient-dominant, so it
    should not be described as an ambient highest weight.
  Text
    When the ambient Cartan matrix is singular, Dynkin labels do not
    determine every realization/derivation direction needed for a
    complete full-Levi highest weight.  They do determine the action of
    the intrinsic center inside the derived algebra, but the current
    record has no field for the missing realization coordinates.  The
    command therefore rejects
    @TT "IncludeCentralCharacter => true"@ rather than inventing missing
    data.  The default derived-Levi output remains available for every
    supported symmetrizable matrix.
  Example
    A = cartanA 4;
    H = kostantLieAlgCohomology(A, {1}, {0,0,0},
        IncludeCentralCharacter => true);
    apply(keys H, k -> apply(keys H#k, V -> V#"centralCharacter"))
  Text
    Kostant's theorem is multiplicity-free for the full Levi.
    Apparent multiplicities created by forgetting central characters
    disappear when the central characters are included.
SeeAlso
  kostantLieAlgCohomology
  LeviIrrep
  irrepData
///

doc ///
Key
  RootSnapshot
Headline
  Positive roots and multiplicities through a specified height
Description
  Text
    A @TT "RootSnapshot"@ is a hash-table record returned by
    @TO rootSnapshot@.  It has the same shape in finite, affine, and
    indefinite type.  Printing it shows a table of positive roots,
    with their heights, numerical simple-root coordinates, multiplicities,
    and real or imaginary type.  The multiplicity is the dimension of
    the corresponding root space.  At most $20$ roots are shown by
    default; if more were computed, the display states exactly how
    many are not shown.  @TO rootSnapshotByHeight@ displays the full
    table.  This display limit does not change the roots stored in the
    snapshot or its height bound.
    Its fields can also be read using their names:
  Text
    @UL {
      (TT "\"real\"", " and ", TT "\"imag\"", " => matrices with one positive root per row, in simple-root coordinates"),
      (TT "\"mult\"", " => a hash table from integral coordinate lists to root multiplicities"),
      (TT "\"cartanMatrix\"", ", ", TT "\"rank\"", ", and ", TT "\"basis\"", " => the Cartan matrix, its size, and the coordinate convention"),
      (TT "\"type\"", " and ", TT "\"algorithm\"", " => the Cartan type and the formula used to compute the multiplicities"),
      (TT "\"heightBound\"", " and ", TT "\"completeThroughHeight\"", " => the height through which every positive root and its multiplicity have been computed"),
      (TT "\"globallyComplete\"", " => whether the record contains the entire positive root system; this can be true only in finite type"),
      (TT "\"delta\"", " => the primitive null root, when the snapshot is affine")
    }@
  Example
    S = rootSnapshot(cartanAffineA 2, 8);
    S
    (S#"type", S#"algorithm", S#"basis", S#"completeThroughHeight")
    S#"delta"
    rootSnapshotRoots(S, Kind => "Imaginary")
  Text
    The key @TT "beta"@ is present in @TT "S#\"mult\""@ exactly
    when that root lies within the computed range.  Use
    @TO rootMultiplicity@ for queries: it returns zero for an absent
    in-range root and refuses to treat an out-of-range absence as zero.
SeeAlso
  rootSnapshot
  rootSnapshotRoots
  rootMultiplicity
  isRootInSnapshot
  rootSnapshotByHeight
  rootSnapshotCacheInfo
///

doc ///
Key
  LeviData
Headline
  A Levi Cartan matrix and its embedding in the ambient root lattice
Description
  Text
    A @TT "LeviData"@ is the hash-table record returned by
    @TO leviSubalgebra@.  Its @TT "nodes"@, @TT "removedNodes"@, and
    @TT "components"@ fields use the ambient node labels, starting at $1$.
    @TT "nodes0"@ and @TT "components0"@ give the corresponding Macaulay2
    matrix positions, starting at $0$.  The
    @TT "embedding"@ matrix sends a row of Levi simple-root
    coordinates to the corresponding row in ambient simple-root
    coordinates.
  Example
    A = cartanD 4;
    print dynkinDiagram A
    L = leviSubalgebra(A, {2});
    (L#"nodes", L#"removedNodes", L#"components")
    L#"embedding"
    L#"centerDimension"
  Text
    These decompositions record only the derived Levi;
    @TT "centralCharactersKept"@ is therefore @TT "false"@.  Summands
    with different full-Levi central characters can merge after this
    restriction.
SeeAlso
  leviSubalgebra
  leviConnectedComponents
  leviHighestWeights
///

doc ///
Key
  WeylElement
  (symbol ==,WeylElement,WeylElement)
Headline
  A Weyl-group element with its reduced word and action on roots
Description
  Text
    @TO weylElement@ constructs a @TT "WeylElement"@ belonging to a
    specified Weyl group.  Its @TT "parent"@ field is the
    @TO KMWeylGroup@, @TT "word"@ is a reduced word in the numbered
    simple reflections, @TT "length"@ is its Coxeter length, and
    @TT "rootActionEntries"@ gives its action on simple-root coordinates.
    The commands @TO weylParent@, @TO weylWord@, @TO weylLength@, and
    @TO weylElementMatrix@ return these quantities.  Multiplication and
    comparison take the Weyl group into account: the same sequence of
    simple reflections can represent different elements for different
    Cartan matrices.
  Example
    D = cartanDatum("A3", NodeLabels => {"left","middle","right"});
    W = weylGroup D;
    u = weylElement(W, {"left","middle","left"});
    v = weylElement(W, {"middle","left","middle"})
    u
    (u == v, u != simpleWeylReflection(W,"right"))
  Text
    The element itself prints as
    @TT "s_left s_middle s_left"@.  The identity prints as @TT "1_W"@.
    This is a reduced expression for the element.  Use @TO weylWord@,
    @TO weylLength@, and @TO weylElementMatrix@ to obtain the word, length,
    and faithful action on the root lattice.
  Text
    Equality compares the specified Weyl group and the action on
    the root lattice.  It therefore recognizes different reduced words
    for the same element, as in the $A_2$ braid relation above, while
    elements belonging to different specified Weyl groups are unequal.
    A reduced word can be entered using node labels; @TO weylWord@
    reports their numbered positions, starting at $1$.
  Text
    When given a Cartan matrix, @TO weylGroupElements@ returns elements
    with fields @TT "word"@, @TT "matrix"@, and @TT "length"@, without a
    @TT "parent"@ field.  Some parabolic computations also include
    @TT "inverse"@.  To compare these elements with @TT "=="@, act on
    weights, or multiply them, first use @TO weylElement@ to specify their
    Weyl group and reduced word.
  Text
    A word is only one reduced expression for the element; it is not a
    unique name.  For a fixed Cartan matrix, @TO weylElementMatrix@
    distinguishes the elements independently of the chosen reduced word.
    The number of elements listed is limited by @TO MaxElements@.
SeeAlso
  weylElement
  weylParent
  weylWord
  weylGroupElements
  parabolicCosetReps
  weylElementMatrix
  MaxElements
///

doc ///
Key
  GradedDecomposition
Headline
  Irreducible decompositions indexed by degree
Description
  Text
    A @TT "GradedDecomposition"@ has integer degree keys and
    @TT "Tally{Irrep => ZZ}"@ values by default, or
    @TT "Tally{LeviIrrep => ZZ}"@ values when central characters are
    requested.  For
    @TO kostantLieAlgCohomology@, the key $k$ is cohomological degree
    and the value is the derived-Levi decomposition of $H^k$.  When
    @TT "MaxDegree => d"@ was supplied, the table is exact in every
    degree through $d$; the absent higher degrees were not requested.
    With no bound, the table gives every cohomological degree when
    the parabolic Weyl quotient is finite.
  Example
    H = kostantLieAlgCohomology(cartanA 4, {1}, {0,0,0});
    H#2
    cohomologyDimensions(cartanA 4, {1}, H)
  Text
    Thus an entry @TT "2 => Tally{V(0,1) => 1}"@ means
    $H^2\cong V_{A_2}(0,1)$ after restriction to the derived Levi.
SeeAlso
  kostantLieAlgCohomology
  cohomologyDimensions
  decompositionDimension
  MaxDegree
///

doc ///
Key
  weightMultiplicityTable
  (weightMultiplicityTable,Matrix,List)
  [weightMultiplicityTable,WeightFormat]
Headline
  Complete formal character of a finite-dimensional irreducible representation
Usage
  T = weightMultiplicityTable(A, lambda)
  T = weightMultiplicityTable(A, lambda, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a dominant integral highest weight in the selected coordinates
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@
Outputs
  T:Tally
    all weights of $V(\lambda)$ as keys and their multiplicities as values
Description
  Text
    Computes every weight multiplicity in $V(\lambda)$ using
    Freudenthal's recursion.  The table is retained for subsequent
    @TO weightMultiplicity@ computations with the same Cartan matrix
    and highest weight.
  Example
    T = weightMultiplicityTable(cartanA 3, {1,1});
    T#{0,0}
    sum apply(keys T, mu -> T#mu)
  Text
    Here $V(1,1)$ is the adjoint representation of $\mathfrak{sl}_3$.
    The zero weight has multiplicity $2$, and the sum of all
    multiplicities is $8=\dim\mathfrak{sl}_3$.
SeeAlso
  weightMultiplicity
  weightsOfRep
  representationCacheInfo
  clearRepresentationCache
///

doc ///
Key
  irrepDimension
  (irrepDimension,StandardParabolic,Irrep)
  (irrepDimension,Matrix,Irrep)
  (irrepDimension,Matrix,LeviIrrep)
  (irrepDimension,LeviIrrep)
  [irrepDimension,WeightFormat]
Headline
  Dimension of an Irrep relative to its semisimple Cartan matrix
Usage
  d = irrepDimension(P, V)
  d = irrepDimension(AL, V)
  d = irrepDimension W
Inputs
  P:StandardParabolic
    a standard parabolic whose derived Levi acts on @TT "V"@
  AL:Matrix
    a possibly disconnected finite-type Cartan matrix
  V:Irrep
    one highest-weight factor for each connected component of @TT "AL"@
  W:LeviIrrep
    an irreducible with its Cartan matrix recorded
  WeightFormat=>String
    the coordinate convention for the component weights
Outputs
  d:ZZ
    the product of the component Weyl dimensions
Description
  Text
    Passing @TT "P"@ reads the finite derived-Levi Cartan matrix from the
    parabolic, so it can be used together with
    @TT "leviHighestWeights(P,k)"@.
  Example
    AL = matrix{{2,0,0},{0,2,-1},{0,-1,2}}
    V = new Irrep from {{2},{1,0}}
    irrepDimension(AL, V)
  Text
    This computes
    $\dim(V_{A_1}(2)\otimes V_{A_2}(1,0))=3\cdot3=9$.
    If @TT "W"@ is a @TO LeviIrrep@, its Cartan matrix and weight coordinates
    are already recorded, so the one-argument form is available:
  Example
    W = irrepData(AL, V);
    irrepDimension W
  Text
    Central characters do not change vector-space dimension.  In the
    two-argument form with a @TO LeviIrrep@, @TT "AL"@ must match its
    @TT "parentCartan"@.
SeeAlso
  decompositionDimension
  weylDimension
  Irrep
  LeviIrrep
  irrepData
///

doc ///
Key
  decompositionDimension
  (decompositionDimension,StandardParabolic,Tally)
  (decompositionDimension,Matrix,Tally)
  (decompositionDimension,Tally)
  [decompositionDimension,WeightFormat]
Headline
  Total dimension of a direct-sum decomposition
Usage
  d = decompositionDimension(P, T)
  d = decompositionDimension(AL, T)
  d = decompositionDimension T
Inputs
  P:StandardParabolic
    a standard parabolic whose derived Levi acts on every summand
  AL:Matrix
    the finite-type Cartan matrix acting on the summands
  T:Tally
    a Tally with @TO Irrep@ or @TO LeviIrrep@ keys
Outputs
  d:ZZ
    the multiplicity-weighted sum of irreducible dimensions
Description
  Text
    The @TT "P"@ form can be used directly after
    @TT "T = leviHighestWeights(P,k)"@: both commands use the derived
    Levi determined by @TT "P"@.
  Example
    T = new Tally from {(new Irrep from {{1}}) => 2,
                        (new Irrep from {{0}}) => 1};
    decompositionDimension(cartanA 2, T)
  Text
    The result is $2\dim V_{A_1}(1)+\dim V_{A_1}(0)=2\cdot2+1=5$.
    If every key is a @TO LeviIrrep@, its Cartan matrix is already known,
    so the one-argument form is available:
  Example
    H = kostantLieAlgCohomology(cartanA 3, {1}, {0,0},
        IncludeCentralCharacter => true);
    decompositionDimension H#1
SeeAlso
  irrepDimension
  cohomologyDimensions
  LeviIrrep
///

doc ///
Key
  cohomologyDimensions
  (cohomologyDimensions,StandardParabolic,HashTable)
  (cohomologyDimensions,Matrix,HashTable)
  (cohomologyDimensions,Matrix,List,HashTable)
  (cohomologyDimensions,Matrix,Set,HashTable)
  (cohomologyDimensions,GradedDecomposition)
  [cohomologyDimensions,WeightFormat]
Headline
  Convert a graded irreducible decomposition to graded dimensions
Usage
  D = cohomologyDimensions(P, H)
  D = cohomologyDimensions(AL, H)
  D = cohomologyDimensions(A, parabolic, H)
  D = cohomologyDimensions H
Inputs
  P:StandardParabolic
    the parabolic whose derived Levi acts on the cohomology
  A:Matrix
    an ambient Cartan matrix whose selected derived Levi is finite type
  parabolic:List
    removed 1-based nodes; a @TO Set@ is accepted as well
  H:HashTable
    cohomological degrees with their decomposition Tallies
Outputs
  D:HashTable
    the total vector-space dimension in each of the same degrees
Description
  Text
    The two-argument form with @TT "P"@ obtains the acting derived Levi
    from the same @TO StandardParabolic@ used to compute @TT "H"@.  In the
    two-argument form with a matrix, @TT "AL"@ is already the derived-Levi
    Cartan matrix; the three-argument matrix form obtains it from
    @TO leviSubalgebra@.  The one-argument form requires every Tally key
    to be a @TO LeviIrrep@ with its Cartan matrix recorded, as produced by
    @TT "IncludeCentralCharacter => true"@.  A
    @TO GradedDecomposition@ with @TO Irrep@ keys requires a parabolic
    or a matrix as an additional argument.
  Text
    This command applies when the cohomology is finite-dimensional.  If the acting
    Levi has an affine or indefinite connected component, the Kostant
    summands are generally infinite-dimensional and no integer total
    dimension exists; in that case @TO kostantLieAlgCohomology@ remains
    valid but @TT "cohomologyDimensions"@ reports an error.
  Example
    D = cartanDatum("A3",
        NodeLabels => {"projective","middle","right"});
    P = standardParabolic(D, RemovedNodes => {"projective"});
    H = kostantLieAlgCohomology(P, {0,0,0})
    cohomologyDimensions(P, H)
    Hfull = kostantLieAlgCohomology(P, {0,0,0},
        IncludeCentralCharacter => true)
    cohomologyDimensions Hfull
  Text
    The displayed decompositions have one irreducible summand in each
    degree. Both dimension tables have values $1,3,3,1$, the dimensions of
    $H^0,H^1,H^2,H^3$ for the three-dimensional abelian nilradical.
SeeAlso
  decompositionDimension
  kostantLieAlgCohomology
  LeviIrrep
  IncludeCentralCharacter
///

doc ///
Key
  representationCacheInfo
  (representationCacheInfo,Matrix)
Headline
  Count previously computed character and tensor-product tables
Usage
  H = representationCacheInfo A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  H:HashTable
    counts of retained multiplicity tables and tensor decompositions
Description
  Text
    Reports how many previously computed tables are retained for the
    Cartan matrix.  Tables are retained after computations with
    @TO weightMultiplicityTable@, @TO weightMultiplicity@, and the
    tensor-product commands.  Later calculations with the same Cartan
    matrix reuse these tables.  A full multiplicity table has one
    entry per distinct weight, so large representations may require
    substantial space.  Use @TO clearRepresentationCache@ to discard
    previously computed tables when they are no longer needed.
  Example
    A = cartanA 3;
    representationCacheInfo A
    weightMultiplicityTable(A, {1,1});
    representationCacheInfo A
  Text
    After computing the adjoint weight table, the saved-table count
    records that this highest weight has been computed.  The table
    lets a later call reuse the same multiplicities.  It is information
    about retained calculations, rather than an invariant of the
    $\mathfrak{sl}_3$ adjoint representation.

SeeAlso
  clearRepresentationCache
  weightMultiplicityTable
  tensorDecomposition
///

doc ///
Key
  clearRepresentationCache
  (clearRepresentationCache,Matrix)
Headline
  Discard retained character and tensor-product tables
Usage
  clearRepresentationCache A
Inputs
  A:Matrix
    the Cartan matrix whose previously computed representation tables are to be discarded
Description
  Text
    Discards the character and tensor-decomposition tables retained for
    @TT "A"@.  The Cartan matrix and previously computed root tables are
    unchanged.
  Example
    A = cartanA 3;
    weightMultiplicityTable(A,{1,1});
    clearRepresentationCache A
    (representationCacheInfo A)#"multiplicityTables"
  Text
    Clearing the saved calculation leaves zero retained weight tables
    for this Cartan matrix.  A new computation of the adjoint character
    will give the same mathematical answer; only the saved result is removed.

SeeAlso
  representationCacheInfo
  clearRootSnapshotCache
///


doc ///
Key
  "Kostant homology and restricted cohomology"
Headline
  Kostant decomposition for arbitrary symmetrizable Kac--Moody type
Description
  Text
    Let $I$ be the Levi nodes and put
    $W^I_{\rm r}=\{v\in W:v(\alpha_i)\in\Phi^+
      \text{ for every }i\in I\}$,
    the right-minimal representatives of $W/W_I$.  For an
    arbitrary symmetrizable generalized Cartan matrix and a dominant
    integral $\lambda$, @TO kostantLieAlgCohomology@ computes
    $$H^k_{\rm res}(\mathfrak n_I^+,L(\lambda))
      \;\cong\!
      \bigoplus_{\substack{v\in W^I_{\rm r}\\\ell(v)=k}}
      L_I(v^{-1}\!\cdot\lambda).$$
    This is also Kumar's opposite-nilradical homology formula
    $H_k(\overline{\mathfrak n}_I^-,L(\lambda))$ under the standard
    Chevalley/contragredient identification.  In infinite type,
    ``res'' specifies weight-graded (restricted) cohomology; the
    unrestricted algebraic dual would involve products over weights
    and is not the cohomology computed here.
  Text
    Every fixed Coxeter-length stratum is finite.  If $W/W_I$ is
    infinite, give @TT "MaxDegree => d"@ and the result is exact in
    every degree $0\leq k\leq d$.  If the
    quotient is finite, omitting @TO MaxDegree@ returns it completely.
    This remains possible in an infinite ambient algebra when each
    infinite-type connected component of the Dynkin diagram lies wholly
    inside the Levi.  Equivalently, every removed node lies in a
    finite-type connected component.
  Text
    The output is a hash table indexed by degree.  The theorem is
    multiplicity-free as a statement about the full Levi.  In the
    default @TO Irrep@ output,
    @TO Irrep@ forgets the central character, so full-Levi summands that
    differ only centrally can merge into a key of multiplicity greater
    than one.  For nonsingular @TT "A"@, use
    @TT "IncludeCentralCharacter => true"@ to obtain @TO LeviIrrep@
    keys that retain and distinguish those characters; see
    @TO IncludeCentralCharacter@ for the singular case.
  Text
    The affine algebra $A_1^{(1)}$ already shows the infinite
    behavior.  Remove node $1$ and keep the $A_1$ Levi at node $2$:
  Example
    Aaff = matrix{{2,-2},{-2,2}};
    print dynkinDiagram Aaff
    kostantLieAlgCohomology(Aaff, {1}, {0,0}, MaxDegree => 4)
  Text
    The result says, exactly through degree $4$,
    $$H^k_{\rm res}\cong V_{A_1}(2k)
      \qquad(0\leq k\leq4).$$
    In familiar terms, these are
    $$\mathbb C,\quad\operatorname{Sym}^2\mathbb C^2,\quad
      \operatorname{Sym}^4\mathbb C^2,\quad
      \operatorname{Sym}^6\mathbb C^2,\quad
      \operatorname{Sym}^8\mathbb C^2,$$
    of dimensions $1,3,5,7,9$.
  Text
    Why should this be the answer?  In the loop-algebra description,
    this parabolic has nilradical
    $\mathfrak n=\mathfrak{sl}_2\otimes t\mathbb C[t]$, with
    $$[x\otimes t^a,y\otimes t^b]=[x,y]\otimes t^{a+b}.$$
    The finite $A_1$ Levi acts by the adjoint action on each copy of
    $\mathfrak{sl}_2$.  For example,
    $[\mathfrak n,\mathfrak n]=\mathfrak{sl}_2\otimes t^2\mathbb C[t]$,
    so $H^1_{\rm res}\cong(\mathfrak n/[\mathfrak n,\mathfrak n])^*
    \cong V_{A_1}(2)$: the first nontrivial output is just the
    three-dimensional adjoint representation.
  Text
    There is also a concrete class in every homological degree.
    Let $e$ be a raising operator in $\mathfrak{sl}_2$.  In the
    Chevalley--Eilenberg homology complex, the vector
    $$z_k=(e\otimes t)\wedge(e\otimes t^2)\wedge\cdots
      \wedge(e\otimes t^k)$$
    is a cycle because its factors commute.  It is a highest-weight
    vector of weight $2k$, with total $t$-degree $k(k+1)/2$.
    It cannot be a boundary: a $(k+1)$-fold wedge of weight $2k$
    would need $k$ distinct raising-operator factors and one Cartan
    factor, so its total $t$-degree is at least $k(k+1)/2+1$.
    Thus $V_{A_1}(2k)$ really occurs in homology.  Restricted
    cohomology is its graded dual, and these finite-dimensional
    $\mathfrak{sl}_2$ modules are self-dual.
  Text
    Kostant's theorem explains why there are no other summands.
    The affine Weyl group is the infinite dihedral group: its two
    simple reflections have no finite braid relation.  With the Levi
    at node $2$, the right-minimal representatives are
    $$1,\ s_1,\ s_2s_1,\ s_1s_2s_1,\ s_2s_1s_2s_1,\ldots,$$
    exactly one in each length.  Their inverse dot-action weights
    can be inspected directly:
  Example
    netList prepend({"degree", "inverse word", "dot-action weight"},
        apply(toList(0..4), k -> (
            image := first parabolicDotImages(Aaff,k,{2},{0,0});
            {k,first image,last image})))
  Text
    The last column is $(0,0),(-2,2),(-4,4),(-6,6),(-8,8)$.
    Restricting to the Levi means reading the second coordinate,
    which gives precisely $0,2,4,6,8$.  This pattern continues in
    every degree, not just through the bound used in the example.
  Text
    This does have a free-resolution interpretation, but it is
    @EM "not periodic"@.  Write $U=U(\mathfrak n)$ for the universal
    enveloping algebra.  Its augmentation sends every element of
    $\mathfrak n$ to zero and makes $\mathbb C$ the trivial module.
    Since $H_k(\mathfrak n,\mathbb C)
    =\operatorname{Tor}_k^U(\mathbb C,\mathbb C)$, the minimal
    graded free resolution of $\mathbb C$ has the form
    $$\cdots\longrightarrow U(-6)^7\longrightarrow U(-3)^5
      \longrightarrow U(-1)^3\longrightarrow U
      \longrightarrow\mathbb C\longrightarrow0.$$
    Here $U(-d)$ has its generator in $t$-degree $d$; the degree-$k$
    generators form $V_{A_1}(2k)$ in $t$-degree $k(k+1)/2$.
    Thus both the ranks $1,3,5,7,\ldots$ and the generator degrees
    $0,1,3,6,\ldots$ grow.  The cohomological degree and the
    $t$-degree are different gradings; dualizing reverses the latter.
    The command reports the Levi modules, not the differential maps
    in this resolution.  For the underlying parabolic resolution,
    see @HREF{"https://www.numdam.org/item/10.24033/asens.1365.pdf",
      "Lepowsky, Generalized Verma modules, loop space cohomology and MacDonald-type identities, Section 5"}@.
  Text
    The @TEX "$\\mathfrak{sl}_4$"@ case with trivial coefficients on
    $\mathrm{Gr}(1,4) = \mathbb{P}^3$: here $|W/W_L| = 24/6 = 4$,
    so there are four Kostant summands.  Their dimensions are
    $1,3,3,1$ (total $8$).  We show the derived $A_2$ weights in
    partition coordinates:
  Example
    kostantLieAlgCohomology(cartanA 4, {1}, {0,0,0}, WeightFormat => "Partition")
  Text
    The outer hash-table keys are the cohomological degrees.  Thus this
    output means, after restriction to the derived $A_2$ Levi,
    $$H^0\cong V(0,0,0),\quad
      H^1\cong V(1,0,0),\quad
      H^2\cong V(1,1,0),\quad
      H^3\cong V(0,0,0).$$
    In familiar terms these are
    $\mathbb C,\mathbb C^3,\bigwedge^2\mathbb C^3,\mathbb C$.
  Text
    Adjoint-style action on the cohomology of the $\mathrm{Gr}(1,4)$
    flag, with $\lambda=\omega_1$:
  Example
    kostantLieAlgCohomology(cartanA 4, {1}, {1,0,0}, WeightFormat => "Partition")
  Text
    Here the output reads
    $$H^0\cong\mathbb C,\quad
      H^1\cong\operatorname{Sym}^2\mathbb C^3,\quad
      H^2\cong S_{(2,1)}\mathbb C^3,\quad
      H^3\cong\mathbb C^3,$$
    with dimensions $1,6,8,3$.
  Text
    Kostant's theorem on the $5$-dimensional smooth quadric $\mathrm{Gr}(1,B_3)$
    (i.e.\ $B_3$ with parabolic $\{1\}$): the Levi is $B_2$, the
    nilradical has dimension $5$, and $|W/W_L| = 48/8 = 6$, so the
    cohomology with trivial coefficients $H^*(\mathfrak n)$ splits into six
    summands indexed by the coset representatives of $W(B_3)/W(B_2)$:
  Example
    kostantLieAlgCohomology(cartanB 3, {1}, {0,0,0})
  Text
    For the derived $B_2$ Levi, the last hash table means
    $$\begin{aligned}
      H^0&\cong\mathbb C,& H^1&\cong V(1,0),& H^2&\cong V(0,2),\\
      H^3&\cong V(0,2),& H^4&\cong V(1,0),& H^5&\cong\mathbb C.
    \end{aligned}$$
    Here $V(1,0)$ is the $5$-dimensional vector module and $V(0,2)$
    is the $10$-dimensional adjoint module.  In general, a line
    @TT "k => Tally{V(a,b) => m, ...}"@ puts $m$ copies of
    $V(a,b)$ in $H^k$.
SeeAlso
  kostantLieAlgCohomology
  IncludeCentralCharacter
  LeviIrrep
  parabolicCosetReps
  dotActionOnWeight
///

doc ///
Key
  "Affine and indefinite Kac-Moody algebras"
Headline
  Levi grading, imaginary roots, and Peterson recursion beyond finite type
Description
  Text
    The same Levi grading extends to every symmetrizable Kac-Moody
    algebra.  In affine and indefinite type, three features require
    attention:
  Text
    @UL {
      {BOLD "the filtration can be infinite.",
       "  The nilradical ",
       TEX "$\\mathfrak n = \\bigoplus_{d\\geq 1}\\mathfrak g_d$",
       " can be infinite-dimensional, with no top degree.  ",
       EM "Each ",
       TEX "$\\mathfrak g_d$",
       " is still finite-dimensional when the derived Levi is of finite type; the package reports its derived-Levi restriction."},
      {BOLD "imaginary roots enter.",
       "  In affine type, roots of the form ",
       TEX "$n\\delta$",
       " (",
       TEX "$\\delta$",
       " the minimal imaginary root, ",
       TEX "$n\\in\\mathbb Z_{>0}$",
       ") have multiplicity $r-1$ for an indecomposable untwisted rank-$r$ affine Cartan matrix.  In twisted affine type the exact multiplicity can vary periodically with $n$ and is computed by Peterson's recursion.  In indefinite type, root multiplicities can grow rapidly."},
      {BOLD "a height cutoff is required.",
       "  Because the root system is infinite, computations use a height bound.  The integer ",
       TT "H",
       " bounds the height ",
       TEX "$\\sum_j a_j$",
       " of the positive roots ",
       TEX "$\\sum_j a_j\\alpha_j$",
       " that are enumerated.  The answer for fixed parabolic degree ",
       TEX "$d$",
       " is computed by the Levi commands only when ",
       TT "H",
       " meets a sufficient completeness bound; a smaller cutoff raises an error."}
    }@
  Text
    The same commands apply in all three types:
  Text
    @UL {
      {TO rootSnapshot,
       ": positive roots and multiplicities; the Cartan type is determined from the matrix."},
      {TO leviHighestWeights,
       ": the restriction of ", TEX "$\\mathfrak g_d$", " to derived-Levi irreducibles; the command determines the Cartan type and a height bound sufficient to include the entire graded piece.  The result is a ",
       TT "Tally{Irrep => ZZ}", " exactly as in the finite-type case."}
    }@
  Text
    Take affine @TEX "$\\widehat{\\mathfrak{sl}}_2$"@ with parabolic
    $\{1\}$: the Levi is the single node $2$ (type $A_1$), and the
    first few graded pieces show how the real-root strings and the
    imaginary root spaces combine into $A_1$-modules:
  Example
    A = matrix{{2,-2},{-2,2}}
    leviHighestWeights(A, 1, {1})
    leviHighestWeights(A, 2, {1})
    leviHighestWeights(A, 3, {1})
  Text
    Each line is @TT "Tally{V(2) => 1}"@, so after restriction to the
    derived $A_1$ Levi,
    $$\mathfrak g_1\cong V_{A_1}(2),\qquad
      \mathfrak g_2\cong V_{A_1}(2),\qquad
      \mathfrak g_3\cong V_{A_1}(2).$$
    Each is the three-dimensional adjoint $A_1$-module.  In every
    positive degree the two real-root spaces provide its weights
    $\pm2$, while the imaginary root $d\delta$ provides its zero-weight
    line.  The modules occupy different grading degrees even though
    forgetting the Levi-center character makes them look identical.

    Unlike the finite case, the depth does not terminate: there is a
    nontrivial $\mathfrak g_d$ for every $d \geq 1$.  The
    @TO ImaginaryMultiplicity@ option is an optional consistency check
    in untwisted type; twisted periodic multiplicities are computed
    automatically and do not accept a constant override.
  Text
    An indefinite example: the rank-$2$ hyperbolic Kac-Moody algebra
    with Cartan matrix $\left(\begin{smallmatrix} 2 & -3 \\ -3 & 2
    \end{smallmatrix}\right)$ has infinitely many positive roots
    with rapidly growing multiplicities.  @TO rootSnapshot@
    tabulates all positive roots up to a given height together with
    their root multiplicities, computed by Peterson's recursion:
  Example
    Ahyp = matrix{{2,-3},{-3,2}}
    cartanType Ahyp
    rootSnapshot(Ahyp, 4)
  Text
    Each tuple $(a,b)$ denotes the root $a\alpha_1+b\alpha_2$;
    the height is $a+b$, and the multiplicity is its root-space
    dimension.  For example, $(1,2)$ is the imaginary root
    $\alpha_1+2\alpha_2$, while $(1,3)$ is the real root
    $\alpha_1+3\alpha_2$.  Both have multiplicity $1$.
    The table contains every positive root through height $4$.
    Increasing the bound to $5$ reveals the first root spaces of
    dimension $2$:
  Example
    S5 = rootSnapshot(Ahyp, 5);
    (rootMultiplicity(S5,{2,3}),rootMultiplicity(S5,{3,2}))
  Text
    Thus $\dim\mathfrak g_{2\alpha_1+3\alpha_2}
    =\dim\mathfrak g_{3\alpha_1+2\alpha_2}=2$.
  Text
    Now use the same Cartan matrix with
    @TO leviHighestWeights@.  Removing node $1$ leaves a
    Levi of type $A_1$ (the single surviving node), and we decompose
    the first two graded pieces as Levi-modules:
  Example
    leviHighestWeights(Ahyp, 1, {1})
    leviHighestWeights(Ahyp, 2, {1})
  Text
    The two Tally outputs translate to
    $$\mathfrak g_1\cong V_{A_1}(3),\qquad
      \mathfrak g_2\cong V_{A_1}(4)\oplus V_{A_1}(0).$$
    Their dimensions are $4$ and $5+1$.  The imaginary root
    $2\alpha_1+3\alpha_2$ has multiplicity $2$ and Levi weight zero;
    its extra root-space dimension appears as the trivial summand in
    degree $2$.

    The same @TT "WeightFormat => \"Partition\""@ option works in
    these commands whenever every surviving Levi component is of
    classical type $A$, $B$, $C$, or $D$.
SeeAlso
  rootSnapshot
  leviHighestWeights
  affineRootSnapshot
  indefiniteRootSnapshot
  ImaginaryMultiplicity
///

doc ///
Key
  "Interpreting outputs: Irrep, Tally, HashTable"
Headline
  How to read irreducibles, direct sums, graded records, and root snapshots
Description
  Text
    The following outputs record representations, direct-sum
    decompositions, and the quantities used to compute them:
  Text
    @UL {
      (TO Irrep, ": a single highest-weight irreducible, or a tensor product of such (one factor per connected derived component).  For an infinite-type Levi this symbol denotes an integrable, generally infinite-dimensional module.  Prints as ", TT "V(a,...)", " or ", TT "V(a,...) ** V(b,...)", "."),
      (TT "Tally{Irrep => ZZ}", ": a direct-sum decomposition into irreducibles, with multiplicities.  Returned by ", TO leviHighestWeights, " and ", TO tensorDecomposition, "."),
      (TO GradedDecomposition, ": a hash table whose integer keys are degrees and whose values are decomposition Tallies; returned by ", TO kostantLieAlgCohomology, ", exactly through ", TO MaxDegree, " when a bound is supplied."),
      (TO RootSnapshot, ": positive roots through a specified height, with separate real and imaginary root matrices, a multiplicity table, and the Cartan matrix."),
      (TO LeviData, ": the Levi Cartan matrix, ambient node labels, embedding, connected components, rank, and center dimension."),
      (TO WeylElement, ": a Weyl-group element with its reduced word, length, and action on roots.")
    }@
  Text
    The separator @TT "**"@ lists the simple Levi factors in component
    order.  For example,
    @TT "Tally{V(1) ** V(1) => 2, V(0) ** V(2) => 1}"@ represents
    $$2\bigl(V(1)\otimes V(1)\bigr)
      \oplus \bigl(V(0)\otimes V(2)\bigr).$$
    A @TT "HashTable"@ adds one more layer: an entry
    @TT "k => Tally{...}"@ gives the decomposition in degree $k$.
    The empty @TO Irrep@ @TT "V()"@ is the unique representation of a
    rank-zero derived Levi, hence a one-dimensional trivial vector
    space; it is not missing data.
    When @TO IncludeCentralCharacter@ is enabled, each @TO LeviIrrep@
    key prints the semisimple module followed by its Cartan type and
    central character in brackets.  Distinct central characters are
    distinct full-Levi summands even when the displayed @TT "V(...)"@
    part agrees.
    @TO Tally@ and @TO HashTable@ displays are unordered: line order is
    not a mathematical ordering and may vary.  Use @TT "sort keys H"@
    for graded degrees or explicitly sort coordinate lists before
    displaying them in a fixed order.
  Text
    For a Levi returned by @TO leviSubalgebra@, factor order is the
    order of @TO leviConnectedComponents@ applied to
    @TT "L#\"cartanMatrix\""@.  Use @TT "L#\"nodes\""@ to map those
    local components back to ambient diagram labels.
  Text
    Root matrices such as @TO cartanPositiveRoots@ have one root per
    row in simple-root coordinates.  Representation-weight lists from
    @TO weightsOfRep@ and @TO weightOrbit@ use Dynkin labels unless
    @TT "WeightFormat => \"Partition\""@ is requested.  Each command's page
    states its coordinate convention in the output description.
  Text
    A root snapshot's @TT "mult"@ table contains exactly the roots
    represented in that bounded snapshot.  An absent key at height at
    most @TT "completeThroughHeight"@ means multiplicity zero.  Beyond
    that bound, absence means "not computed" rather than "not a root";
    @TO rootMultiplicity@ raises an error there to prevent the two cases
    from being confused.
SeeAlso
  Irrep
  leviHighestWeights
  rootSnapshot
  tensorDecomposition
  kostantLieAlgCohomology
///


doc ///
Key
  "Extended examples"
Headline
  Examples connecting diagrams, roots, Levi modules, Weyl groups, and cohomology
Description
  Text
    The examples on this page follow a Cartan matrix through several
    related constructions.  Roots, representation dimensions,
    Weyl-group combinatorics, and cohomology give independent checks
    on the resulting identities.
  Text
    The page is organized as a sequence of named examples: the
    Grassmannian $\mathrm{Gr}(2,6)$; $D_4$ triality; $F_4/P_3$; the
    $E_8$ dimension identity; affine $A_2^{(1)}$; the chain
    $E_8\subset E_9\subset E_{10}$; hyperbolic $A_1^{++}$;
    twisted affine periodicity; and the
    $B_3$ Borel.  Each bold title below begins a self-contained example.
  Text
    The command @TO decompositionDimension@ converts a
    @TT "Tally{Irrep => ZZ}"@ into its total dimension relative to a
    finite Levi Cartan matrix:
  Example
    decompositionDimension(cartanA 2, new Tally from {
        (new Irrep from {{3}}) => 2})
  Text
    @SUBSECTION "The Grassmannian Gr(2,6): a complete tour"@

    Start with the type $A_5$ diagram for $\mathfrak{sl}_6$ and remove
    node $2$.  The complementary nodes form a derived Levi of type
    $A_1\times A_3$.  The nilradical has dimension $8$ and is
    irreducible as a derived-Levi module:
  Example
    A = cartanA 6;
    removed = {2};
    print dynkinDiagram A
    L = leviSubalgebra(A, removed);
    L#"nodes"
    print dynkinDiagram (L#"cartanMatrix")
    tally ((parabolicDegrees(A, removed)) / last)
    numrows nilradicalRoots(A, removed)
    g1 = leviHighestWeights(A, 1, removed)
    decompositionDimension(L#"cartanMatrix", g1)
  Text
    The output says
    $$\mathfrak n=\mathfrak g_1
      \cong V_{A_1}(1)\otimes V_{A_3}(0,0,1)
      \cong \mathbb C^2\otimes(\mathbb C^4)^*,$$
    so $\dim\mathfrak n=2\cdot4=8$.  The removed simple root has
    coefficient $1$ in the highest root, so this parabolic is
    @EM "cominuscule"@.  There is no degree-$2$ piece, and its
    nilradical is abelian.
  Text
    Kostant cohomology and the minimal coset representatives now
    recover two familiar rows of numbers independently:
  Example
    H = kostantLieAlgCohomology(A, removed, toList(5:0));
    H#2
    cellCounts = apply(toList(0..8), k -> sum values H#k)
    sum cellCounts == binomial(6,2)
    cohomDims = apply(toList(0..8), k ->
        decompositionDimension(L#"cartanMatrix", H#k))
    cohomDims == apply(toList(0..8), k -> binomial(8,k))
  Text
    Thus the numbers of Kostant summands are
    $$(1,1,2,2,3,2,2,1,1),$$
    whose sum is $15=\binom62$, the number of Schubert cells in
    $\operatorname{Gr}(2,6)$.  Their total dimensions in each degree are
    $$(1,8,28,56,70,56,28,8,1)
      =\left(\binom80,\binom81,\ldots,\binom88\right),$$
    exactly as expected from
    $H^k(\mathfrak n,\mathbb C)\cong\bigwedge^k\mathfrak n^*$.
    For example, the displayed degree-$2$ output means
    $$H^2\cong
      V_{A_1}(0)\otimes V_{A_3}(2,0,0)
      \oplus
      V_{A_1}(2)\otimes V_{A_3}(0,1,0),$$
    with dimension $10+3\cdot6=28$.
  Text
    @SUBSECTION "$D_4$ triality: a Heisenberg nilradical"@

    The diagram shows that node $2$ is central.  Removing it leaves
    three interchangeable $A_1$ components:
  Example
    A = cartanD 4;
    removed = {2};
    print dynkinDiagram A
    L = leviSubalgebra(A, removed);
    print dynkinDiagram (L#"cartanMatrix")
    pieces = apply(toList(1..2), d ->
        leviHighestWeights(A, d, removed))
    apply(pieces, T -> decompositionDimension(L#"cartanMatrix", T))
    H = kostantLieAlgCohomology(A, removed, toList(4:0));
    H#2
    d4dims = apply(toList(0..9), k ->
        decompositionDimension(L#"cartanMatrix", H#k))
    d4dims == reverse d4dims
    sum apply(toList(0..9), k -> sum values H#k)
  Text
    In ordinary mathematical notation the two graded pieces are
    $$\mathfrak g_1\cong
      V(1)\otimes V(1)\otimes V(1),\qquad
      \mathfrak g_2\cong\mathbb C,$$
    of dimensions $8$ and $1$.  The three tensor factors make the
    triality symmetry visible in the output.  The nilradical is
    @EM "Heisenberg"@: $\mathfrak g_2=\mathbb Cz$ is its center and
    $[x,y]=\omega(x,y)z$ for $x,y\in\mathfrak g_1$, where $\omega$
    is a nondegenerate alternating form.  The cohomology dimensions
    are
    $$(1,8,27,48,42,42,48,27,8,1),$$
    a palindrome expressing Poincaré duality.  There are $24$ Kostant
    summands, agreeing with
    $|W(D_4)|/|W(A_1)^3|=192/8=24$.
  Text
    @SUBSECTION "$F_4/P_3$: a deeper exceptional grading"@

    This example has four nonzero positive degrees but remains quick
    enough for an interactive session:
  Example
    A = cartanF4();
    removed = {3};
    print dynkinDiagram A
    L = leviSubalgebra(A, removed);
    print dynkinDiagram (L#"cartanMatrix")
    degreeCounts = tally ((parabolicDegrees(A, removed)) / last)
    pieces = apply(toList(1..4), d ->
        leviHighestWeights(A, d, removed))
    pieceDims = apply(pieces, T ->
        decompositionDimension(L#"cartanMatrix", T))
    sum pieceDims == numrows nilradicalRoots(A, removed)
    H = kostantLieAlgCohomology(A, removed, toList(4:0));
    H#2
    sum apply(toList(0..20), k -> sum values H#k)
  Text
    For the derived Levi $A_2\times A_1$, the decomposition is
    $$\begin{aligned}
      \mathfrak g_1&\cong V(1,0)\otimes V(1),& \dim\mathfrak g_1&=6,\\
      \mathfrak g_2&\cong V(0,1)\otimes V(2),& \dim\mathfrak g_2&=9,\\
      \mathfrak g_3&\cong V(0,0)\otimes V(1),& \dim\mathfrak g_3&=2,\\
      \mathfrak g_4&\cong V(1,0)\otimes V(0),& \dim\mathfrak g_4&=3.
      \end{aligned}$$
    Consequently $\dim\mathfrak n=6+9+2+3=20$ and
    $$\dim\mathfrak f_4=52=12+2\cdot20.$$
    The computation finds $96$ Kostant summands, as predicted by
    $|W(F_4)|/(|W(A_2)|\,|W(A_1)|)=1152/(6\cdot2)=96$.
    In degree $2$ it gives
    $$H^2\cong V(0,2)\otimes V(0)
      \oplus V(1,1)\otimes V(3),$$
    of total dimension $6+8\cdot4=38$.
  Text
    @SUBSECTION "$E_8/P_8$: reconstructing all 248 dimensions"@

    The highest root controls the grading depth, while the remaining
    diagram is $E_7$:
  Example
    A = cartanE 8;
    removed = {8};
    print dynkinDiagram A
    highestRoot A
    L = leviSubalgebra(A, removed);
    print dynkinDiagram (L#"cartanMatrix")
    degreeCounts = tally ((parabolicDegrees(A, removed)) / last)
    pieces = apply(toList(1..2), d ->
        leviHighestWeights(A, d, removed))
    apply(pieces, T -> decompositionDimension(L#"cartanMatrix", T))
    dimFromRoots = numrows A + 2 * numrows cartanPositiveRoots A
    dimFromGrading = (numrows A + 2 * degreeCounts#0
        + 2 * numrows nilradicalRoots(A, removed))
    dimFromRoots == dimFromGrading
  Text
    The $120$ positive roots give $8+2\cdot120=248$.  The parabolic
    calculation instead gives
    $$\mathfrak g_1\cong V_{E_7}(\omega_7),\quad
      \dim\mathfrak g_1=56,\qquad
      \mathfrak g_2\cong\mathbb C,$$
    and hence the five-step decomposition
    $$248=1+56+(133+1)+56+1=134+57+57.$$
    The root and grading computations give this identity without
    listing the $696729600$ elements of $W(E_8)$.
  Text
    @SUBSECTION "Affine $A_2^{(1)}$: the loop algebra one degree at a time"@

    Removing the affine node leaves the finite algebra
    $\mathfrak{sl}_3$.  At each positive loop degree, six real-root
    spaces and the two-dimensional imaginary-root space should assemble
    into its eight-dimensional adjoint representation:
  Example
    A = cartanAffineA 2;
    print dynkinDiagram A
    S = rootSnapshot(A, 12);
    rootAccounting = apply(toList(1..3), d -> (
        nr := #select(entries(S#"real"), v -> v#0 == d);
        im := sum(select(entries(S#"imag"), v -> v#0 == d),
            v -> (S#"mult")#v);
        (d, nr, im, nr + im)
        ))
    affinePieces = apply(toList(1..3), d ->
        leviHighestWeights(A, d, {1}))
    weylDimension(cartanA 3, {1,1})
  Text
    Each tuple in @TT "rootAccounting"@ has the form
    @TT "(degree, real dimension, imaginary dimension, total)"@, so the
    output $(d,6,2,8)$ appears in degrees $1,2,3$.  Each corresponding
    Levi decomposition is @TT "Tally{V(1,1) => 1}"@.  In other words,
    $$\mathfrak g_d\big|_{\mathfrak{sl}_3}
      \cong V_{A_2}(1,1)\cong\mathfrak{sl}_3,\qquad d=1,2,3,$$
    and the same pattern continues at every positive loop degree.
  Text
    @SUBSECTION "From $E_8$ to affine $E_9$ to indefinite $E_{10}$"@

    The standard extension commands preserve the old Cartan matrix
    as a principal submatrix while moving through all three types:
  Example
    E8 = cartanE 8;
    E9 = cartanExtendUntwisted E8;
    E10 = cartanOverExtended E8;
    apply({E8,E9,E10}, M -> (numrows M, cartanType M, det M))
    submatrix(E10, toList(1..9), toList(1..9)) == E9
    delta9 = flatten entries ((rootSnapshot(E9, 30))#"delta")
    beta10 = prepend(0, delta9);
    S10 = rootSnapshot(E10, 30);
    ((S10#"mult")#beta10, rootNormSquared(E10, beta10))
  Text
    The first output is
    $$\{(8,\text{finite},1),(9,\text{affine},0),
       (10,\text{indefinite},-1)\}.$$
    The affine null root is
    $\delta=(1,2,3,4,6,5,4,3,2)$.  After embedding it into $E_{10}$,
    the root computation gives squared norm $0$ and multiplicity
    $8$, exactly matching its affine multiplicity.
  Text
    @SUBSECTION "Hyperbolic $A_1^{++}$: root multiplicities versus Levi modules"@

    Here removing node $2$ leaves a disconnected $A_1\times A_1$ Levi.
    We can compute the dimensions of the first four graded pieces in
    two unrelated ways: by summing root multiplicities and by summing
    the dimensions of the irreducible Levi summands.
  Example
    A = cartanOverExtended cartanA 2;
    print dynkinDiagram A
    S = rootSnapshot(A, 16);
    allPositive = entries(S#"real") | entries(S#"imag");
    rootDims = apply(toList(1..4), d ->
        sum(select(allPositive, v -> v#1 == d),
            v -> (S#"mult")#v))
    hyperbolicPieces = apply(toList(1..4), d ->
        leviHighestWeights(A, d, {2}))
    a1xA1Dimension = T -> sum(keys T, V ->
        T#V * product(toList V, w -> (toList w)#0 + 1));
    leviDims = apply(hyperbolicPieces, a1xA1Dimension)
    rootDims == leviDims
  Text
    Both routes give
    $$(6,10,20,51).$$
    For example, the first two decompositions are
    $$\mathfrak g_1\cong V(1)\otimes V(2),$$
    $$\mathfrak g_2\cong
      V(0)\otimes V(0)\oplus V(2)\otimes V(2),$$
    with dimensions $2\cdot3=6$ and $1+3\cdot3=10$.  The final
    @TT "true"@ confirms that the root multiplicities and the
    representation-theoretic decomposition give the same dimensions.
  Text
    @SUBSECTION "Twisted affine periodicity"@

    For a twisted affine matrix, imaginary-root multiplicities need not
    be the constant @TT "rank - 1"@.  They are computed from the matrix:
  Example
    A = transpose cartanAffineB 3;
    print dynkinDiagram A
    S = rootSnapshot(A, 30);
    delta = flatten entries S#"delta";
    apply(toList(1..6), k ->
        (k, (S#"mult")#(apply(delta, x -> k*x))))
  Text
    Here $\delta=(1,1,2,1)$ and
    $$\operatorname{mult}(k\delta)=2,3,2,3,2,3\qquad(1\leq k\leq6).$$
    This alternating pattern shows why the untwisted formula does not
    apply here.  Peterson's recursion computes the multiplicity of
    each imaginary root from the Cartan matrix.
  Text
    @SUBSECTION "The $B_3$ Borel: Kostant cohomology is the Weyl polynomial"@

    At the Borel, every node is removed and the number of Kostant
    summands in degree $k$ is the number of Weyl-group elements of
    length $k$:
  Example
    A = cartanB 3;
    H = kostantLieAlgCohomology(A, {1,2,3}, toList(3:0));
    P = weylLengthGenerating A;
    q = (ring P)_0;
    cohomPoincare = sum for k in keys H list (sum values H#k)*q^k;
    cohomPoincare
    cohomPoincare == P
    sub(cohomPoincare,q=>1) == weylGroupOrder A
  Text
    The coefficients are
    $$(1,3,5,7,8,8,7,5,3,1),$$
    and they sum to $48=|W(B_3)|$.  Equivalently,
    $$P_{W(B_3)}(q)
      =(1+q)(1+q+q^2+q^3)(1+q+\cdots+q^5),$$
    reflecting the exponents $1,3,5$.
SeeAlso
  dynkinDiagram
  cartanPositiveRoots
  rootSnapshot
  leviHighestWeights
  kostantLieAlgCohomology
///


doc ///
Key
  "KacMoody references"
Headline
  Standard sources for the conventions and theorems used in KacMoody
Description
  Text
    @UL {
      {EM "J. E. Humphreys", ", ", EM "Introduction to Lie Algebras and Representation Theory", ", Graduate Texts in Mathematics 9, Springer-Verlag, 1972.  Standard reference for the finite-type theory: simple roots, Weyl groups, highest-weight representations."},
      {EM "V. G. Kac", ", ", EM "Infinite Dimensional Lie Algebras", ", 3rd edition, Cambridge University Press, 1990, ", HREF{"https://doi.org/10.1017/CBO9780511626234", "DOI:10.1017/CBO9780511626234"}, ".  Standard reference for generalized Cartan matrices, affine and indefinite Kac-Moody algebras, integrable highest-weight modules, and root spaces."},
      {EM "N. Bourbaki", ", ", EM "Lie Groups and Lie Algebras", ", Chapters 4-6, Springer, 2002.  Source of the sign and arrow conventions on Dynkin diagrams used here."},
      {EM "B. Kostant", ", \"Lie algebra cohomology and the generalized Borel-Weil theorem,\" ", EM "Annals of Mathematics", " 74 (1961), 329-387, ", HREF{"https://doi.org/10.2307/1970237", "DOI:10.2307/1970237"}, ".  The finite-dimensional source of the nilradical cohomology formula."},
      {EM "H. Garland and J. Lepowsky", ", \"Lie algebra homology and the Macdonald--Kac formulas,\" ", EM "Inventiones Mathematicae", " 34 (1976), 37-76, ", HREF{"https://doi.org/10.1007/BF01418970", "DOI:10.1007/BF01418970"}, ".  The symmetrizable Kac--Moody homology formula with a finite-type Levi."},
      {EM "S. Kumar", ", ", EM "Kac--Moody Groups, their Flag Varieties and Representation Theory", ", Progress in Mathematics 204, Birkhäuser, 2002, Theorem 3.2.7, ", HREF{"https://doi.org/10.1007/978-1-4612-0105-2", "DOI:10.1007/978-1-4612-0105-2"}, ".  The arbitrary symmetrizable Kac--Moody opposite-nilradical homology formula and parabolic quotient convention used by ", TO kostantLieAlgCohomology, "."},
      {EM "L. Liu", ", \"Kostant's formula for Kac--Moody Lie algebras,\" ", EM "Journal of Algebra", " 149 (1992), 155-178, ", HREF{"https://doi.org/10.1016/0021-8693(92)90010-J", "DOI:10.1016/0021-8693(92)90010-J"}, ".  Homology and cohomology formulas without a finite-type assumption on the Levi."},
      {EM "A. Backus, P. Connick, and J. Lin", ", \"An algorithm for computing root multiplicities in Kac-Moody algebras,\" ", HREF{"https://arxiv.org/abs/1912.04540", "arXiv:1912.04540"}, " (2019).  Root multiplicities computed by Peterson's recursion in the fundamental chamber, together with Weyl-group symmetry."},
      {EM "R. W. Carter", ", ", EM "Lie Algebras of Finite and Affine Type", ", Cambridge Studies in Advanced Mathematics 96, Cambridge University Press, 2005.  Uniform treatment covering both finite and affine types."}
    }@
  Text
    @BOLD "Acknowledgments."@
    Thanks to the developers of
    @TO "LieAlgebraRepresentations::LieAlgebraRepresentations"@ and
    @TO "WeylGroups::WeylGroups"@.  Wherever the packages offer
    comparable finite-type computations, their outputs were used for
    cross-checks.
///

------------------------------------------------------------
-- Type-specific Cartan matrices
------------------------------------------------------------

doc ///
Key
  cartanA
Headline
  Cartan matrix of type $A_{n-1}$
Usage
  A = cartanA n
Inputs
  n:ZZ
    an integer $\geq 2$
Outputs
  A:Matrix
    the $(n-1)\times(n-1)$ Cartan matrix of type $A_{n-1}$
Description
  Text
    Returns the Cartan matrix of the simple Lie algebra $\mathfrak{sl}_n$
    of type $A_{n-1}$.  The diagonal entries are $2$, the two
    off-diagonals along the tridiagonal are $-1$, and all other entries
    are zero.
  Example
    cartanA 4
    dynkinDiagram cartanA 4
  Text
    The matrix is the Cartan matrix of $\mathfrak{sl}_4$, of rank $3$.
    Its two adjacent off-diagonal entries $-1$ give the chain
    $1$--$2$--$3$ displayed by the diagram.  Row $i$, column $j$ records
    $\langle\alpha_j,\alpha_i^\vee\rangle$.

SeeAlso
  cartanB
  cartanC
  cartanD
  cartanE
  cartanF4
  cartanG2
  dynkinDiagram
///

doc ///
Key
  cartanB
Headline
  Cartan matrix of type $B_\ell$
Usage
  A = cartanB ell
Inputs
  ell:ZZ
    an integer $\geq 2$
Outputs
  A:Matrix
    the $\ell\times\ell$ Cartan matrix of type $B_\ell$
Description
  Text
    Returns the Cartan matrix of $\mathfrak{so}_{2\ell+1}$ in the
    Bourbaki convention:
    $A_{\ell-1,\ell} = -1$ and $A_{\ell,\ell-1} = -2$, reflecting that
    the last simple root $\alpha_\ell$ is short.
  Example
    cartanB 3
    dynkinDiagram cartanB 4
  Text
    The matrix describes type $B_3$, or $\mathfrak{so}_7$.  Its last
    simple root is short: the entries $a_{23}=-1$ and $a_{32}=-2$
    distinguish it from the preceding long root.  The following diagram
    is the analogous rank-$4$ system $B_4$.

SeeAlso
  simpleRootsB
  positiveRootsB
  cartanA
  cartanC
  dynkinDiagram
///

doc ///
Key
  cartanC
Headline
  Cartan matrix of type $C_\ell$
Usage
  A = cartanC ell
Inputs
  ell:ZZ
    an integer $\geq 2$
Outputs
  A:Matrix
    the $\ell\times\ell$ Cartan matrix of type $C_\ell$
Description
  Text
    Returns the Cartan matrix of $\mathfrak{sp}_{2\ell}$ in the
    Bourbaki convention.  The last simple root $\alpha_\ell$ is long;
    concretely $A_{\ell-1,\ell} = -2$ and $A_{\ell,\ell-1} = -1$.
  Example
    cartanC 3
    dynkinDiagram cartanC 4
  Text
    The matrix describes type $C_3$, or $\mathfrak{sp}_6$.  Here
    $a_{23}=-2$ and $a_{32}=-1$, so the last simple root is long.
    The following diagram shows the same convention for $C_4$.

SeeAlso
  cartanB
  simpleRootsC
  positiveRootsC
  dynkinDiagram
///

doc ///
Key
  cartanD
Headline
  Cartan matrix of type $D_\ell$
Usage
  A = cartanD ell
Inputs
  ell:ZZ
    an integer $\geq 3$
Outputs
  A:Matrix
    the $\ell\times\ell$ Cartan matrix of type $D_\ell$
Description
  Text
    Returns the Cartan matrix of $\mathfrak{so}_{2\ell}$.  Type $D$ is
    simply laced; the Dynkin diagram is a path of length $\ell-2$ with a
    branch of length one attached near the end.
  Example
    cartanD 5
    dynkinDiagram cartanD 5
  Text
    The matrix and diagram both describe $D_5=\mathfrak{so}_{10}$.
    All simple roots have the same length; the two final vertices
    $4$ and $5$ both join vertex $3$ and do not join one another.

SeeAlso
  simpleRootsD
  positiveRootsD
  dynkinDiagram
///

doc ///
Key
  cartanE
Headline
  Cartan matrices of types $E_6$, $E_7$, $E_8$
Usage
  A = cartanE ell
Inputs
  ell:ZZ
    one of $6$, $7$, or $8$
Outputs
  A:Matrix
    the Cartan matrix of type $E_\ell$
Description
  Text
    Returns the simply laced Cartan matrix of type $E_\ell$.
    The resulting Dynkin diagram is a path with a single branch of
    length one attached to an interior node.
  Example
    cartanE 6
    dynkinDiagram cartanE 6
  Text
    The output describes the rank-$6$ exceptional root system $E_6$.
    Every bond is simple, so adjacent simple roots pair to $-1$
    and nonadjacent simple roots pair to $0$.  The diagram displays the
    numbering to use for weights and parabolic subsets.

SeeAlso
  simpleRootsE6
  simpleRootsE7
  simpleRootsE8
  dynkinDiagram
///

doc ///
Key
  cartanF4
Headline
  Cartan matrix of type $F_4$
Usage
  A = cartanF4()
Outputs
  A:Matrix
    the $4\times 4$ Cartan matrix of type $F_4$
Description
  Text
    Returns the Cartan matrix of the exceptional Lie algebra of type
    $F_4$.  The diagram is a path with a double bond in the middle
    whose arrow points from $\alpha_2$ (long) to $\alpha_3$ (short).
  Example
    cartanF4()
    dynkinDiagram cartanF4()
  Text
    The four rows correspond to the four simple roots of $F_4$.
    The middle bond joins the long root at node $2$ to the short root
    at node $3$; its unequal Cartan entries are $-1$ and $-2$.

SeeAlso
  simpleRootsF4
  positiveRootsF4
  dynkinDiagram
///

doc ///
Key
  cartanG2
Headline
  Cartan matrix of type $G_2$
Usage
  A = cartanG2()
Outputs
  A:Matrix
    the $2\times 2$ Cartan matrix of type $G_2$
Description
  Text
    Returns the Cartan matrix of the exceptional Lie algebra of type
    $G_2$ in the Bourbaki convention.  The simple root $\alpha_1$ is
    short and $\alpha_2$ is long, so the arrow in the Dynkin diagram
    points from $\alpha_2$ to $\alpha_1$ with a triple bond.
  Example
    cartanG2()
    dynkinDiagram cartanG2()
  Text
    The matrix is $\left(\begin{smallmatrix}2&-3\\-1&2\end{smallmatrix}\right)$.
    Thus $\langle\alpha_2,\alpha_1^\vee\rangle=-3$ and
    $\langle\alpha_1,\alpha_2^\vee\rangle=-1$.  Node $1$ is short,
    node $2$ is long, and the triple bond points toward node $1$.

SeeAlso
  simpleRootsG2
  positiveRootsG2
  dynkinDiagram
///

------------------------------------------------------------
-- Tripod / star / rank-2 / affine / extended Cartan matrices
------------------------------------------------------------

doc ///
Key
  cartanT
Headline
  Cartan matrix of a tripod (T-shaped) Dynkin diagram $T_{p,q,r}$
Usage
  A = cartanT(p, q, r)
Inputs
  p:ZZ
    length of the first arm (counting the central trivalent node), $p\geq 1$
  q:ZZ
    length of the second arm, $q\geq 1$
  r:ZZ
    length of the third arm, $r\geq 1$
Outputs
  A:Matrix
    the $(p+q+r-2)\times(p+q+r-2)$ simply-laced generalized Cartan matrix
    of the tripod diagram $T_{p,q,r}$
Description
  Text
    When $p,q,r\geq2$, the tripod diagram $T_{p,q,r}$ has three
    nontrivial arms of lengths $p$, $q$,
    $r$ emanating from a common trivalent vertex; the three integers
    count all nodes on each arm, including the center.  Node $1$ is the
    leaf of the first arm; nodes $2,\ldots,p$ walk inward to the center
    (node $p$); the second arm occupies nodes $p+1,\ldots,p+q-1$ and the
    third arm nodes $p+q,\ldots,p+q+r-2$.
  Text
    For $p,q,r\geq2$ its type is determined by:
  Text
    @UL {
      {"of finite type iff ", TEX "$1/p + 1/q + 1/r > 1$", " (Coxeter);"},
      {"of affine type iff ", TEX "$1/p + 1/q + 1/r = 1$", ";"},
      {"Lorentzian indefinite iff ", TEX "$1/p + 1/q + 1/r < 1$", "; only a subfamily is hyperbolic."}
    }@
  Text
    Among tripods with $p,q,r\geq2$, the finite cases, up to permuting
    the three arms, are exactly $T_{2,2,n} = D_{n+1}$, $T_{2,3,3} =
    E_6$, $T_{2,3,4} = E_7$, $T_{2,3,5} = E_8$; the affine cases are
    $T_{3,3,3} = \widetilde E_6$, $T_{2,4,4} = \widetilde E_7$,
    $T_{2,3,6} = \widetilde E_8$; and $T_{2,3,7}$ is the hyperbolic
    Kac-Moody algebra $E_{10}$.
    If one of $p,q,r$ is $1$, that arm contributes no edge and the
    diagram degenerates to a path; the displayed reciprocal criterion
    and the word "tripod" are intended for the nondegenerate case
    $p,q,r\geq2$.
  Example
    cartanT(2,3,3)
    dynkinDiagram cartanT(2,3,3)
    cartanType cartanT(2,3,3)
    cartanType cartanT(3,3,3)
    cartanType cartanT(2,3,7)
  Text
    Varying the arm lengths produces several familiar exceptional
    diagrams.  For instance, $T_{2,3,n}$ gives $E_6$, $E_7$, $E_8$,
    the affine $\widetilde E_8 = E_9$, and then the hyperbolic
    $E_{10}$ as $n$ runs from $3$ to $7$:
  Example
    for n from 3 to 7 list (n, cartanType cartanT(2,3,n))
SeeAlso
  cartanStar
  cartanE
  cartanOverExtended
  cartanType
  dynkinDiagram
///

doc ///
Key
  cartanStar
Headline
  Cartan matrix of a star-shaped Dynkin diagram
Usage
  A = cartanStar armLengths
Inputs
  armLengths:List
    a list $\{p_1,\ldots,p_k\}$ of positive integers; each $p_i$ is the
    length of the $i$-th arm counting the central node
Outputs
  A:Matrix
    the $n\times n$ simply-laced generalized Cartan matrix of the
    $k$-armed star with $n = 1 + \sum_i (p_i-1)$
Description
  Text
    Builds a simply-laced Cartan matrix whose Dynkin diagram is a star
    with $k$ arms emanating from a single central node.  The center
    corresponds to simple root $\alpha_1$, and successive arms fill the
    remaining node indices in order.  One-node arms ($p_i=1$) contribute
    nothing beyond the center; the center is trivalent or higher
    whenever at least three $p_i \geq 2$.
  Text
    A star with at most two
    nontrivial arms is a linear chain, giving type $A$; a star with
    exactly three nontrivial arms of lengths $p,q,r$ coincides with
    @TO cartanT@$(p,q,r)$ up to a reordering of the simple roots.
    Four arms of length $2$, counting the center, give the affine diagram
    $D_4^{(1)}$; four arms with any longer arm, or five or more
    nontrivial arms, are indefinite.
  Example
    cartanStar {2,2,2}
    dynkinDiagram cartanStar {2,2,2}
    cartanType cartanStar {2,2,2}
    cartanType cartanStar {2,2,2,2}
    cartanType cartanStar {2,2,2,2,2}
  Text
    Here @TT "cartanStar {2,2,2}"@ is $D_4$, which has a
    $4$-node diagram with a trivalent center; the Weyl group has order
    $192 = 2^{4-1} \cdot 4!$, in agreement with the abstract $D_4$.
  Example
    weylGroupOrder cartanStar {2,2,2}    -- 192
    weylGroupOrder cartanD 4             -- 192
  Text
    A star with three nontrivial arms is a tripod.  When
    $1/p + 1/q + 1/r > 1$, it has finite type.  The following
    examples give finite $E_6$ and affine $E_6$:
  Example
    cartanType cartanStar {2,3,3}        -- E_6 (finite)
    cartanType cartanStar {3,3,3}        -- affine E_6
  Text
    The unique four-arm affine exception and the next indefinite case
    are:
  Example
    A = cartanStar {2,2,2,2};            -- 5 nodes
    cartanType A                         -- affine (a D_4^{(1)} variant)
    B = cartanStar {2,2,2,2,2};          -- 6 nodes
    cartanType B                         -- indefinite
SeeAlso
  cartanT
  cartanD
  cartanType
  dynkinDiagram
  weylGroupOrder
///

doc ///
Key
  cartanRank2
Headline
  Generic rank-$2$ generalized Cartan matrix $[[2,-a],[-b,2]]$
Usage
  A = cartanRank2(a, b)
Inputs
  a:ZZ
    a positive integer
  b:ZZ
    a positive integer
Outputs
  A:Matrix
    the $2\times 2$ generalized Cartan matrix with off-diagonal entries
    $-a$ and $-b$
Description
  Text
    Returns the rank-$2$ generalized Cartan matrix
    $\begin{pmatrix} 2 & -a \\ -b & 2 \end{pmatrix}$.  Its type is
    determined by the product $ab$:
  Text
    @UL {
      {TEX "$ab = 1$", " gives $A_2$;"},
      {TEX "$ab = 2$", " gives $B_2 = C_2$;"},
      {TEX "$ab = 3$", " gives $G_2$;"},
      {TEX "$ab = 4$", " gives an affine Kac-Moody algebra (if $a=b=2$,
        the untwisted $A_1^{(1)}$; otherwise the twisted $A_2^{(2)}$
        diagram);"},
      {TEX "$ab \\geq 5$", " gives a rank-$2$ hyperbolic Kac-Moody algebra."}
    }@
  Example
    cartanRank2(1,1)
    cartanType cartanRank2(1,1)   -- A_2
    cartanRank2(1,3)
    cartanType cartanRank2(1,3)   -- G_2
    cartanRank2(2,2)
    cartanType cartanRank2(2,2)   -- affine A_1^{(1)}
    cartanType cartanRank2(2,3)   -- indefinite
  Text
    The Weyl group is finite exactly in the first three rows of the
    classification.  It is the dihedral group $I_2(m)$, where the
    order of $s_1s_2$ is $m\in\{3,4,6\}$.  For the crystallographic
    finite rank-$2$ cases this $m$ is also the Coxeter number $h$, and
    $|W|=2m=2h$:
  Example
    weylGroupOrder cartanRank2(1,1)      -- 6 = 2*3 (Coxeter m = 3)
    weylGroupOrder cartanRank2(1,2)      -- 8 = 2*4 (Coxeter m = 4)
    weylGroupOrder cartanRank2(1,3)      -- 12 = 2*6 (Coxeter m = 6)
  Text
    For $ab \geq 4$ the Weyl group is infinite.  The affine case
    $ab = 4$ has a positive-semidefinite symmetrized form with a
    one-dimensional radical (spanned in the root lattice by the null
    root); the hyperbolic cases $ab \geq 5$ have negative determinant:
  Example
    det cartanRank2(2,2)                   -- 0 (affine)
    det cartanRank2(2,3)                   -- < 0 (hyperbolic)
    cartanRank2(2,3)
SeeAlso
  cartanG2
  cartanAffineA
  cartanType
  cartanPositiveRootsDegree
///

doc ///
Key
  cartanExtendUntwisted
Headline
  Untwisted affine extension of a finite-type Cartan matrix
Usage
  Aaff = cartanExtendUntwisted A
Inputs
  A:Matrix
    a connected generalized Cartan matrix of finite type
Outputs
  Aaff:Matrix
    the untwisted affine Cartan matrix $A^{(1)}$, an $(r+1)\times(r+1)$
    generalized Cartan matrix of affine type, with the new affine node
    in the first row and column (node $1$ in @TO dynkinDiagram@)
Description
  Text
    Adjoins the affine simple root $\alpha_0 = \delta-\theta$, where
    $\theta$ is the highest root of the finite root system and $\delta$
    is the null root.  Its projection to the finite root space is
    $-\theta$, so the new Cartan entries are computed from the inner product:
    $A^{(1)}_{0j} = -2(\theta,\alpha_j)/(\theta,\theta)$ and
    $A^{(1)}_{j0} = -2(\theta,\alpha_j)/(\alpha_j,\alpha_j)$.  The
    resulting matrix has affine type.  Its symmetrized form is positive
    semidefinite, with one-dimensional radical spanned by $\delta$.
  Text
    There are two numbering conventions.  In traditional
    affine notation the new root is $\alpha_0$; it occupies the first
    row and column, indexed in Macaulay2 by $0$.
    @TO dynkinDiagram@ displays that same position as node $1$, and a
    parabolic or reflection command selects it with the integer
    $1$.  More generally, traditional $\alpha_i$ occupies matrix
    position $i$ and is displayed as node $i+1$.
  Example
    cartanExtendUntwisted cartanA 4
    cartanType cartanExtendUntwisted cartanA 4
    cartanExtendUntwisted cartanG2()
    cartanType cartanExtendUntwisted cartanG2()
  Text
    The first matrix has four nodes and type $A_3^{(1)}$; the following type output is
    affine. The second matrix is the three-node $G_2^{(1)}$ matrix, also affine. In each
    case the first node is the newly adjoined root $\alpha_0=\delta-\theta$.

    The affine matrices returned by @TO cartanAffineA@, @TO cartanAffineB@,
    @TO cartanAffineC@, @TO cartanAffineD@, @TO cartanAffineF4@,
    @TO cartanAffineG2@ agree with @TT "cartanExtendUntwisted"@ applied
    to the corresponding finite-type matrix:
  Example
    (cartanExtendUntwisted cartanA 5) == cartanAffineA 4
    (cartanExtendUntwisted cartanB 3) == cartanAffineB 3
    (cartanExtendUntwisted cartanG2()) == cartanAffineG2()
SeeAlso
  cartanAffineA
  cartanAffineB
  cartanAffineC
  cartanAffineD
  cartanAffineE
  cartanAffineF4
  cartanAffineG2
  cartanOverExtended
  highestRoot
  cartanType
///

doc ///
Key
  cartanAffineA
Headline
  Untwisted affine Cartan matrix $A_n^{(1)}$
Usage
  A = cartanAffineA n
Inputs
  n:ZZ
    the finite-type rank, $n\geq 1$
Outputs
  A:Matrix
    the $(n+1)\times(n+1)$ affine Cartan matrix of type $A_n^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine Kac-Moody algebra
    $A_n^{(1)}$, whose Dynkin diagram is an $(n+1)$-cycle for $n\geq 2$
    and the special two-node diagram $[[2,-2],[-2,2]]$ for $n=1$.
    In traditional notation, $\alpha_0$ is at matrix position $0$
    and $\alpha_1,\ldots,\alpha_n$ occupy positions $1,\ldots,n$.
    The displayed node numbers are one larger, so the affine node is
    labeled $1$ by @TO dynkinDiagram@.
  Example
    cartanAffineA 1
    cartanAffineA 3
    dynkinDiagram cartanAffineA 3
    cartanType cartanAffineA 3
  Text
    The two matrices are $A_1^{(1)}$ and $A_3^{(1)}$, of sizes $2$ and $4$. In the first,
    the off-diagonal entries are both $-2$; in the second, the four simple roots form the
    displayed cycle. The final output confirms that the four-node matrix is affine.

    The null root $\delta = \alpha_0 + \alpha_1 + \cdots + \alpha_n$
    (with coefficients $(1,1,\ldots,1)$) generates the one-dimensional
    radical of the Cartan matrix, consistent with the affine type:
  Example
    A = cartanAffineA 3;
    det A
    -- the kernel of A is 1-dimensional, spanned by the all-ones vector
    kerA := mingens ker A;
    numcols kerA
  Text
    The untwisted affinization coincides with @TO cartanExtendUntwisted@
    applied to the corresponding finite type, so one obtains
    $A_n^{(1)}$ from $\mathfrak{sl}_{n+1}$ in a natural way:
  Example
    (cartanExtendUntwisted cartanA 4) == cartanAffineA 3
    (cartanExtendUntwisted cartanA 6) == cartanAffineA 5
SeeAlso
  cartanA
  cartanExtendUntwisted
  cartanAffineB
  cartanAffineC
  cartanAffineD
  cartanOverExtended
  cartanType
///

doc ///
Key
  cartanAffineB
Headline
  Untwisted affine Cartan matrix $B_n^{(1)}$
Usage
  A = cartanAffineB n
Inputs
  n:ZZ
    the finite-type rank, $n\geq 3$
Outputs
  A:Matrix
    the $(n+1)\times(n+1)$ affine Cartan matrix of type $B_n^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine algebra
    $B_n^{(1)} = \widetilde{\mathfrak{so}}_{2n+1}$.  In traditional
    notation, $\alpha_0$ (matrix position $0$, displayed node $1$) is
    attached by a single bond to $\alpha_2$ (matrix position $2$,
    displayed node $3$).  The finite $B_n$ chain occupies the remaining
    positions and carries the double bond at its short-root end.
  Example
    cartanAffineB 3
    dynkinDiagram cartanAffineB 4
    cartanType cartanAffineB 4
  Text
    The matrix is the four-node system $B_3^{(1)}$, obtained from the rank-$3$ algebra
    $\mathfrak{so}_7$. The next diagram instead shows $B_4^{(1)}$ with five nodes, and
    the final output identifies that matrix as affine.

    The null root of $B_n^{(1)}$ has coefficients $(1,1,2,2,\ldots,2)$
    (one $\alpha_0$, one $\alpha_1$, twos on the rest), reflecting
    the asymmetric attachment of the affine node to interior node
    $\alpha_2$:
  Example
    det cartanAffineB 4
    mingens ker cartanAffineB 4
SeeAlso
  cartanB
  cartanExtendUntwisted
  cartanType
///

doc ///
Key
  cartanAffineC
Headline
  Untwisted affine Cartan matrix $C_n^{(1)}$
Usage
  A = cartanAffineC n
Inputs
  n:ZZ
    the finite-type rank, $n\geq 2$
Outputs
  A:Matrix
    the $(n+1)\times(n+1)$ affine Cartan matrix of type $C_n^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine algebra
    $C_n^{(1)} = \widetilde{\mathfrak{sp}}_{2n}$.  In traditional
    $\alpha_0,\ldots,\alpha_n$ notation, both end bonds are double:
    $\alpha_0$ (long) connects to $\alpha_1$ (short), and
    $\alpha_{n-1}$ (short) connects to $\alpha_n$ (long).  These roots
    appear as displayed nodes $1,\ldots,n+1$; all interior bonds are
    single.
  Example
    cartanAffineC 3
    dynkinDiagram cartanAffineC 3
    cartanType cartanAffineC 3
  Text
    The matrix and diagram describe the four-node $C_3^{(1)}$ system, the untwisted
    affinization of $\mathfrak{sp}_6$. The end roots at displayed positions $1$ and $4$
    are long and both end bonds are double. The type output is affine.

    The transpose is the Langlands-dual affine Cartan matrix and is
    generally the twisted type $D_{n+1}^{(2)}$, not another copy of
    $C_n^{(1)}$ with the same labels.
  Example
    A = cartanAffineC 3;
    dualCartanMatrix A
    -- the kernel is spanned by the null root with coefficients
    -- (1,2,2,...,2,1)
    mingens ker A
SeeAlso
  cartanC
  cartanExtendUntwisted
  cartanType
///

doc ///
Key
  cartanAffineD
Headline
  Untwisted affine Cartan matrix $D_n^{(1)}$
Usage
  A = cartanAffineD n
Inputs
  n:ZZ
    the finite-type rank, $n\geq 4$
Outputs
  A:Matrix
    the $(n+1)\times(n+1)$ affine Cartan matrix of type $D_n^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine algebra
    $D_n^{(1)} = \widetilde{\mathfrak{so}}_{2n}$.  In traditional
    affine notation the diagram has a fork at each end: roots
    $\alpha_0,\alpha_1$ attach to $\alpha_2$, and
    $\alpha_{n-1},\alpha_n$ attach to $\alpha_{n-2}$.  Their
    displayed node numbers are $1$ larger.  All bonds are single.
  Example
    cartanAffineD 4
    dynkinDiagram cartanAffineD 5
    cartanType cartanAffineD 5
  Text
    For $n = 4$ the diagram is the symmetric $4$-armed star with all
    arms of edge-length $1$ (lengths $\{2,2,2,2\}$ when counting
    the center).  This is the four-armed star diagram of $D_4^{(1)}$:
  Example
    cartanAffineD 4 == cartanStar {2,2,2,2}
  Text
    The comparison is false because the two matrices use different node orders. They
    describe isomorphic four-arm diagrams: the central node occupies position $3$ in @TT
    "cartanAffineD 4"@ and position $1$ in @TT "cartanStar {2,2,2,2}"@. Equality of the
    matrices would require the same simultaneous row-and-column ordering.
  Text
    The null root of $D_n^{(1)}$ has coefficients $(1,1,2,2,\ldots,2,1,1)$:
    both pairs of fork-leaves get coefficient $1$, interior chain
    $\alpha_i$ gets coefficient $2$.
  Example
    mingens ker cartanAffineD 5
SeeAlso
  cartanD
  cartanExtendUntwisted
  cartanType
///

doc ///
Key
  cartanAffineE
Headline
  Untwisted affine Cartan matrices $E_n^{(1)}$ for $n\in\{6,7,8\}$
Usage
  A = cartanAffineE n
Inputs
  n:ZZ
    one of $6$, $7$, or $8$
Outputs
  A:Matrix
    the $(n+1)\times(n+1)$ affine Cartan matrix of type $E_n^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine exceptional
    algebra $E_n^{(1)}$, realized as a tripod:
  Text
    @UL {
      {TEX "$E_6^{(1)} = T_{3,3,3}$", " (affine $\\widetilde E_6$);"},
      {TEX "$E_7^{(1)} = T_{2,4,4}$", ";"},
      {TEX "$E_8^{(1)} = T_{2,3,6}$", "."}
    }@
  Text
    The node numbering is inherited from @TO cartanT@; it differs from
    Kac's Table Aff 1, but the underlying Kac-Moody algebra is
    unchanged.  Permuting the simple roots preserves the type and
    permutes the coordinates of roots at every height.
  Example
    cartanAffineE 6
    dynkinDiagram cartanAffineE 6
    cartanType cartanAffineE 8
  Text
    The first matrix and diagram describe the seven-node affine system $E_6^{(1)}$, here
    numbered as the tripod $T_{3,3,3}$. The final type output is affine for the nine-node
    $E_8^{(1)}$ matrix.

    The marks (null-root coefficients) match the classical $E$-type
    marks up to the $T_{p,q,r}$ relabeling.  For $\widetilde E_8 =
    T_{2,3,6}$ the package ordering gives
    $(3,6,4,2,5,4,3,2,1)$.  Their sum is the Coxeter number
    $h(E_8)=30$:
  Example
    mingens ker cartanAffineE 8
    coxeterNumber cartanE 8
SeeAlso
  cartanE
  cartanT
  cartanExtendUntwisted
  cartanOverExtended
///

doc ///
Key
  cartanAffineF4
Headline
  Untwisted affine Cartan matrix $F_4^{(1)}$
Usage
  A = cartanAffineF4()
Outputs
  A:Matrix
    the $5\times 5$ affine Cartan matrix of type $F_4^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine $F_4^{(1)}$.
    In traditional notation the Dynkin diagram is the linear chain
    $\alpha_0-\alpha_1-\alpha_2\Rightarrow\alpha_3-\alpha_4$, with
    the double bond pointing from the long root $\alpha_2$ to the short
    root $\alpha_3$.  @TO dynkinDiagram@ displays these as nodes
    $1-2-3\Rightarrow4-5$.
  Example
    cartanAffineF4()
    dynkinDiagram cartanAffineF4()
    cartanType cartanAffineF4()
  Text
    The marks of $F_4^{(1)}$ are $(1,2,3,4,2)$ and sum to
    $h(F_4)=12$.  The comarks sum to the dual Coxeter number
    $h^\vee(F_4)=9$:
  Example
    coxeterNumber cartanF4(), dualCoxeterNumber cartanF4()
SeeAlso
  cartanF4
  cartanExtendUntwisted
  coxeterNumber
///

doc ///
Key
  cartanAffineG2
Headline
  Untwisted affine Cartan matrix $G_2^{(1)}$
Usage
  A = cartanAffineG2()
Outputs
  A:Matrix
    the $3\times 3$ affine Cartan matrix of type $G_2^{(1)}$
Description
  Text
    Returns the Cartan matrix of the untwisted affine $G_2^{(1)}$.
    In traditional notation the affine root $\alpha_0$ is attached by
    a single bond to the long root $\alpha_2$ of finite $G_2$; the
    triple bond joins the short $\alpha_1$ to $\alpha_2$.  These are
    matrix positions $0,1,2$ and displayed nodes $1,2,3$.
  Example
    cartanAffineG2()
    dynkinDiagram cartanAffineG2()
    cartanType cartanAffineG2()
  Text
    The null root of $G_2^{(1)}$ has coefficients $(1,3,2)$ in this
    node ordering;
    consistent with the Coxeter number $h(G_2) = 6$ and
    $h^\vee(G_2) = 4$, its entries are exactly the marks of the
    extended $G_2$ diagram:
  Example
    mingens ker cartanAffineG2()
    (coxeterNumber cartanG2(), dualCoxeterNumber cartanG2())
SeeAlso
  cartanG2
  cartanExtendUntwisted
  coxeterNumber
///

doc ///
Key
  cartanOverExtended
Headline
  Over-extended (Lorentzian) Kac-Moody Cartan matrix $A^{++}$
Usage
  Aplusplus = cartanOverExtended A
Inputs
  A:Matrix
    a connected finite-type generalized Cartan matrix
Outputs
  Aplusplus:Matrix
    the $(r+2)\times(r+2)$ Cartan matrix of the over-extended Kac-Moody
    algebra $A^{++}$
Description
  Text
    Takes the untwisted affinization $A^+ = A^{(1)}$ (see
    @TO cartanExtendUntwisted@) and places one additional simple root first,
    attached by a single bond to the affine node $\alpha_0$.  The
    result is the "over-extended" or Lorentzian Kac-Moody Cartan
    matrix $A^{++}$, which has signature $(r+1, 1)$ and is in
    particular of indefinite (hyperbolic or Lorentzian) type.
  Text
    For $A$ of type $E_8$, the sequence
    $E_8 \subset E_8^{(1)} = E_9 \subset E_8^{++} = E_{10} \subset
    E_8^{+++} = E_{11}$
    gives $E_{10}$, and the very-extension gives $E_{11}$.  These
    Lorentzian algebras occur in work on supergravity and string
    theory; $E_{10}$ has been conjectured to encode the dynamics of
    $11$-dimensional supergravity near a spacelike singularity.
  Example
    cartanOverExtended cartanE 8
    numrows cartanOverExtended cartanE 8
    cartanType cartanOverExtended cartanE 8
  Text
    Every over-extension is indefinite; for finite-type $A$ we get
    a Lorentzian Kac-Moody algebra.  The rank tower starting from
    $E_8$ produces $E_9$, $E_{10}$, $E_{11}$:
  Example
    (numrows cartanE 8, numrows cartanExtendUntwisted cartanE 8,
     numrows cartanOverExtended cartanE 8,
     numrows cartanVeryExtended cartanE 8)
  Text
    The over-extension of $A$ contains both $A$ itself and the
    untwisted affinization $A^{(1)}$ as submatrices (removing the
    new leftmost node gives $A^{(1)}$, removing the affine node in
    addition gives $A$):
  Example
    M := cartanOverExtended cartanA 4;     -- A_3^{++}, rank 5
    submatrix(M, {1,2,3,4}, {1,2,3,4}) == cartanAffineA 3
    submatrix(M, {2,3,4}, {2,3,4}) == cartanA 4
SeeAlso
  cartanExtendUntwisted
  cartanVeryExtended
  cartanType
///

doc ///
Key
  cartanVeryExtended
Headline
  Very-extended Kac-Moody Cartan matrix $A^{+++}$
Usage
  Aplusplusplus = cartanVeryExtended A
Inputs
  A:Matrix
    a connected finite-type generalized Cartan matrix
Outputs
  Aplusplusplus:Matrix
    the $(r+3)\times(r+3)$ Cartan matrix of the very-extended
    Kac-Moody algebra $A^{+++}$
Description
  Text
    Returns the very-extended Cartan matrix $A^{+++}$, obtained from
    @TO cartanOverExtended@ by attaching another simple root by a
    single bond to the over-extended node.  Applied to $E_8$, this
    produces $E_{11}$,
    which has been conjectured by West to encode the full algebraic
    structure of $M$-theory.
  Example
    E11 = cartanVeryExtended cartanE 8;
    numrows E11
    cartanType E11
  Text
    The chain of ranks is $r \to r+1 \to r+2 \to r+3$ as we go from
    finite type through affine, over-extended, and very-extended:
  Example
    apply({cartanE 8, cartanExtendUntwisted cartanE 8,
     cartanOverExtended cartanE 8, cartanVeryExtended cartanE 8},
     M -> (numrows M, cartanType M))
  Text
    Every very-extended matrix obtained this way is a symmetrizable
    Kac-Moody generalized Cartan matrix.  For the classical $A$, $D$,
    $E$ series the symmetrized form has Lorentzian signature:
  Example
    isCartanMatrix cartanVeryExtended cartanA 3
    isCartanMatrix cartanVeryExtended cartanD 5
    isCartanMatrix cartanVeryExtended cartanE 8
SeeAlso
  cartanOverExtended
  cartanExtendUntwisted
  cartanType
///

------------------------------------------------------------
-- Simple roots (epsilon basis)
------------------------------------------------------------

doc ///
Key
  simpleRootsA
Headline
  Simple roots of type $A_{n-1}$ in Euclidean coordinates
Usage
  S = simpleRootsA n
Inputs
  n:ZZ
    an integer $n\geq2$; the defining representation has dimension $n$
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses $\alpha_i=\varepsilon_i-\varepsilon_{i+1}$ for $1\leq i<n$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsA 4
  Text
    The rows are $\varepsilon_1-\varepsilon_2$, $\varepsilon_2-\varepsilon_3$, and $\varepsilon_3-\varepsilon_4$.
    Thus there are 3 simple roots in 4 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsA
  fromEpsilon
  toEpsilon
///

------------------------------------------------------------
-- Positive roots
------------------------------------------------------------

doc ///
Key
  positiveRootsA
Headline
  Positive roots of type $A_{n-1}$ in Euclidean coordinates
Usage
  R = positiveRootsA n
Inputs
  n:ZZ
    an integer $n\geq2$; the defining representation has dimension $n$
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    The positive roots are $\varepsilon_i-\varepsilon_j$ for $i<j$.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsA 4;
    (numrows R,numcols R)
    R
  Text
    The pair $(6,4)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    The six roots are the differences $\varepsilon_i-\varepsilon_j$
    with $1\leq i<j\leq4$, the positive roots of $\mathfrak{sl}_4$.
SeeAlso
  simpleRootsA
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  cartanPositiveRoots
  (cartanPositiveRoots, Matrix)
Headline
  Positive roots of a Cartan matrix
Usage
  R = cartanPositiveRoots A
Inputs
  A:Matrix
    a generalized Cartan matrix of finite type
Outputs
  R:Matrix
    a matrix whose rows are the positive roots, each expressed as a
    non-negative integer combination of simple roots
Description
  Text
    Returns every positive root of $A$ in simple-root coordinates.
    For a standard named Cartan matrix, this gives the same roots as the
    corresponding @TT "positiveRootsX"@ function after changing from the
    $\varepsilon$-basis to the simple-root basis.  These coordinates are
    useful for computing root heights and parabolic degrees.
  Example
    numrows cartanPositiveRoots cartanD 4
    cartanPositiveRoots cartanG2()
  Example
    -- The complete B_3 positive system in simple-root coordinates.
    cartanPositiveRoots(cartanB 3)
  Text
    The first output is $12$, the number of positive roots of $D_4$. In the $G_2$ matrix
    a row $(a,b)$ means the root $a\alpha_1+b\alpha_2$; for example $(3,2)$ is its
    highest root $3\alpha_1+2\alpha_2$. The following $B_3$ matrix likewise lists all
    nine positive roots in simple-root coordinates.

    The number of positive roots matches the classical count
    $|\Phi^+|$ for each Lie type: $\binom{n+1}{2}$ for $A_n$, $n^2$
    for $B_n$ and $C_n$, $n(n-1)$ for $D_n$, and $(36, 63, 120)$ for
    $(E_6, E_7, E_8)$.  It also equals the length of the longest
    Weyl-group element, illustrated here for a small case:
  Example
    numrows cartanPositiveRoots cartanA 3             -- 3
    weylLength(cartanA 3, longElement cartanA 3)      -- 3
    numrows cartanPositiveRoots cartanE 6             -- 36
  Text
    The rows need not be ordered by height.  For a connected root
    system, use @TO highestRoot@ to select the unique root of maximal
    height:
  Example
    highestRoot cartanA 4
  Text
    Sorting by height recovers the standard graded structure:
  Example
    tally apply(entries cartanPositiveRoots cartanD 4, rootHeight)
SeeAlso
  positiveRootsA
  negativeRoots
  allRoots
  highestRoot
  rootHeight
///

------------------------------------------------------------
-- General Cartan utilities
------------------------------------------------------------

doc ///
Key
  isCartanMatrix
Headline
  Test whether a matrix is a generalized Cartan matrix
Usage
  b = isCartanMatrix A
Inputs
  A:Matrix
Outputs
  b:Boolean
    true if $A$ satisfies the Cartan axioms (diagonal $2$,
    off-diagonal nonpositive integers, $A_{ij}=0 \Leftrightarrow
    A_{ji}=0$)
Description
  Text
    A @EM "generalized"@ Cartan matrix is permitted to be of affine
    or indefinite type (e.g.\ $\det A \leq 0$), hence this test does
    not require finite type.  For a finite-type test use
    @TO cartanType@.
  Example
    isCartanMatrix cartanA 3                  -- finite type
    isCartanMatrix matrix{{2,-2},{-2,2}}      -- affine sl_2 hat
    isCartanMatrix matrix{{2,-3},{-3,2}}      -- rank-2 hyperbolic
    isCartanMatrix matrix{{2,-1},{-1,1}}      -- fails (diag not 2)
    isCartanMatrix matrix{{2,-1},{0,2}}       -- fails (asymmetric zero pattern)
  Text
    The first three answers are true: finite, affine, and indefinite
    matrices can all satisfy the generalized Cartan axioms.  The last
    two answers are false because one diagonal entry is not $2$,
    and because a zero entry is not matched by a zero in the transpose
    position, respectively.

///

doc ///
Key
  symmetrizer
Headline
  Diagonal symmetrizer of a Cartan matrix
Usage
  D = symmetrizer A
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
Outputs
  D:Matrix
    a positive diagonal matrix over @TT "QQ"@ such that $DA$ is
    symmetric; each connected component is normalized by assigning
    its first node diagonal entry $1$
Description
  Text
    The symmetrizer encodes the relative squared lengths of the simple
    roots.  Its diagonal entries are rational and need not be integers.
    For example, type $B_n$
    gives diagonal entries $(1,\ldots,1,1/2)$, type $C_n$ gives
    $(1,\ldots,1,2)$, and $G_2$ gives $(1,3)$.
  Example
    symmetrizer cartanA 4                -- simply laced
    symmetrizer cartanB 3
    symmetrizer cartanC 3
    symmetrizer cartanG2()
    symmetrizer cartanF4()
  Text
    The $B_3$ and $C_3$ outputs have diagonal entries
    $(1,1,1/2)$ and $(1,1,2)$, respectively.  They give simple-root
    squared lengths $(2,2,1)$ and $(2,2,4)$ in the first-node
    normalization.  For $G_2$ the diagonal $(1,3)$ gives the
    short-to-long squared-length ratio $1:3$.

SeeAlso
  rootNormSquared
  isCartanMatrix
///

doc ///
Key
  cartanType
Headline
  Identify the type of a Cartan matrix (finite / affine / indefinite)
Usage
  t = cartanType A
Inputs
  A:Matrix
    a generalized Cartan matrix
Outputs
  t:String
    one of @TT "\"finite\""@, @TT "\"affine\""@,
    @TT "\"indefinite\""@, or @TT "\"nonsymmetrizable\""@
Description
  Text
    Classification is componentwise and therefore invariant under a
    simultaneous permutation of rows and columns.  A connected
    component is finite when its symmetrized form $DA$ is positive
    definite; it is affine when its determinant is zero and every
    proper principal minor is positive; otherwise it is indefinite.
    A disconnected matrix is affine when every component is finite or
    affine and at least one component is affine.  Thus a direct sum of
    several affine components is reported as affine, even though its
    full form has corank
    greater than one.
    Non-symmetrizable matrices are
    reported as @TT "\"nonsymmetrizable\""@.
  Example
    cartanType cartanA 5             -- finite
    cartanType cartanAffineA 5       -- affine
    cartanType cartanT(2,3,7)        -- indefinite (hyperbolic E_10)
  Text
    The family of tripod diagrams $T_{2,3,n}$ illustrates all three
    cases: as $n$ increases from $3$ to $7$, the diagrams are
    $E_6, E_7, E_8,$ affine $E_8,$ and hyperbolic $E_{10}$:
  Example
    for n from 3 to 7 list (n, cartanType cartanT(2,3,n))
  Text
    Over-extending $E_8$ twice produces the Lorentzian algebra
    $E_{11}$:
  Example
    cartanType cartanVeryExtended cartanE 8
SeeAlso
  isCartanMatrix
  symmetrizer
  cartanT
  cartanOverExtended
///

doc ///
Key
  cartanRank
  (cartanRank,Matrix)
Headline
  Number of simple roots in a Cartan matrix
Usage
  r = cartanRank A
Inputs
  A:Matrix
    a square integral generalized Cartan matrix
Outputs
  r:ZZ
    the number of rows, equivalently the number of simple-root labels
Description
  Text
    This is the size of the Cartan matrix, not its linear-algebra rank.
    For a singular affine matrix these differ; @TO cartanData@ reports
    both values and the corank.
  Example
    A = cartanAffineA 2
    (cartanRank A, rank(A ** QQ), (cartanData A)#"corank")
  Text
    The output $(3,2,1)$ distinguishes three simple-root labels from
    the linear-algebra rank $2$.  The affine Cartan matrix has a
    one-dimensional kernel, hence corank $1$.

SeeAlso
  cartanData
  cartanType
///

doc ///
Key
  cartanData
  (cartanData,Matrix)
Headline
  Type, rank, and connected components of a generalized Cartan matrix
Usage
  H = cartanData A
Inputs
  A:Matrix
    an integral generalized Cartan matrix
Outputs
  H:HashTable
    type, size, matrix rank, corank, symmetrizer, and connected components
Description
  Text
    The entries include @TT "rank"@ (number of nodes),
    @TT "matrixRank"@, @TT "corank"@, @TT "type"@,
    @TT "symmetrizable"@, @TT "symmetrizer"@, @TT "components"@,
    and @TT "nodeConvention"@.  For each connected component,
    @TT "nodes"@ gives its node positions in the original matrix,
    counted from $1$; @TT "label"@ gives its type when recognized;
    and @TT "canonicalNodeOrder"@ gives the node order used to identify
    that type.
  Example
    H = cartanData matrix{{2,-1,0},{-1,2,0},{0,0,2}};
    (H#"type", H#"rank", H#"components")
  Text
    Here the matrix is finite of type $A_2\times A_1$.  The
    @TT "components"@ entry describes the two factors and identifies
    the nodes belonging to each.
SeeAlso
  cartanRank
  cartanType
  dynkinDiagramData
///

doc ///
Key
  simpleRoots
Headline
  Simple roots associated to a Cartan matrix (as rows of an identity matrix)
Usage
  S = simpleRoots A
Inputs
  A:Matrix
    a Cartan matrix of rank $r$
Outputs
  S:Matrix
    the $r\times r$ identity matrix, representing the simple roots
    $\alpha_1,\ldots,\alpha_r$ as the standard basis vectors in the
    simple-root basis
Description
  Text
    The $i$-th simple
    root has coordinate vector $e_i$ in the basis of simple roots.
    For concrete $\varepsilon$-basis realizations see the type-specific
    functions @TO simpleRootsA@, @TO simpleRootsB@, etc.
  Example
    simpleRoots cartanA 3
    simpleRoots cartanD 4
  Text
    The first output is the $2\times2$ identity matrix: its rows are the simple-root
    coordinate vectors of $\alpha_1$ and $\alpha_2$ in $A_2$. The second output is the
    $4\times4$ identity matrix, recording the four simple roots of $D_4$ in their own
    basis.

    The simple roots are the first @TT "rank A"@ rows of the positive
    roots returned by @TO cartanPositiveRoots@:
  Example
    cartanPositiveRoots cartanB 3
    (cartanPositiveRoots cartanB 3)^{0,1,2} == simpleRoots cartanB 3
  Text
    Since @TT "simpleRoots A"@ is the identity matrix, the compatible
    row-coordinate identity is:
  Example
    A = cartanG2()
    coroots A == (A ** QQ) * simpleRoots A
SeeAlso
  simpleRootsA
  fundamentalWeights
  cartanPositiveRoots
///

doc ///
Key
  coroots
Headline
  Coroot coordinates (rows of the Cartan matrix)
Usage
  C = coroots A
Inputs
  A:Matrix
    a generalized Cartan matrix; its rows record simple-coroot evaluations
Outputs
  C:Matrix
    the matrix whose $i$-th row is $\alpha_i^\vee$ expressed as a list
    of evaluations $\langle\alpha_j,\alpha_i^\vee\rangle = A_{ij}$
Description
  Text
    Over $\mathbf Q$, @TT "coroots A"@ is just $A \otimes \mathbf Q$.
    The $i$-th row lists the evaluations of the simple coroot
    $\alpha_i^\vee$ on all simple roots.  In other words,
    @TT "(coroots A)_(i,j) = <alpha_j, alpha_i^vee>"@.  In a
    nonsymmetric type, the Dynkin-label vector of $\alpha_j$ is
    therefore @EM "column"@ $j$, not row $j$.
  Example
    coroots cartanA 3
    coroots cartanG2()
  Text
    The $G_2$ rows $(2,-3)$ and $(-1,2)$ are the evaluations of $\alpha_1^\vee$ and
    $\alpha_2^\vee$ on the two simple roots. For example the entry $-3$ is
    $\langle\alpha_2,\alpha_1^\vee\rangle$. These rows are not the Dynkin labels of the
    simple roots, which are the columns of the same matrix.

    When $A$ is invertible, coroots and fundamental weights are dual
    bases: pairing each simple
    coroot with each fundamental weight is the identity matrix.  This
    is the matrix identity $A \cdot A^{-1} = I$, since the
    fundamental-weight matrix is $(A^{-1})^T$:
  Example
    A = cartanB 3
    (coroots A) * transpose fundamentalWeights A
  Text
    The coroot of a nonisotropic real root
    $\alpha = \sum c_i \alpha_i$ is
    @BOLD "not"@ given by $\sum c_i \alpha_i^\vee$; it is instead
    $2\alpha/(\alpha,\alpha)$, which can be computed by combining the
    simple roots with the inner product @TO rootInnerProduct@.
SeeAlso
  fundamentalWeights
  simpleRoots
  rootInnerProduct
///

doc ///
Key
  fundamentalWeights
Headline
  Fundamental weights in the simple-root basis
Usage
  W = fundamentalWeights A
Inputs
  A:Matrix
    an invertible Cartan matrix
Outputs
  W:Matrix
    the matrix $(A^{-1})^T$; its $i$-th row is $\omega_i$ expressed as
    a rational combination of simple roots
Description
  Text
    The fundamental weights satisfy
    $\langle \omega_i, \alpha_j^\vee\rangle = \delta_{ij}$.  When
    expressed as a row vector in simple-root coordinates, the $i$-th
    fundamental weight is the $i$-th row of $(A^{-1})^T$ under the
    Bourbaki convention used throughout this package.
    For a singular GCM, fundamental weights exist only after choosing a
    realization with additional Cartan directions; they cannot be
    recovered as unique simple-root coordinate rows from $A$ alone.
    Accordingly the function reports an error when $A$ is singular.
  Example
    fundamentalWeights(cartanA 3)
  Text
    The two $A_2$ rows mean $\omega_1=(2\alpha_1+\alpha_2)/3$ and
    $\omega_2=(\alpha_1+2\alpha_2)/3$. These fractions are simple-root coefficients of
    fundamental weights, not fractional Dynkin labels.

    In finite type, the half-sum of positive roots expressed in simple-root
    coordinates must equal the sum of the fundamental weights (both
    equal the Weyl vector $\rho$):
  Example
    A = cartanD 4;
    rhoSimple = (1/2) * transpose matrix {sum entries cartanPositiveRoots A}
    sumOfOmegas = transpose matrix {sum entries fundamentalWeights A}
    rhoSimple == sumOfOmegas
  Text
    The entries of the fundamental-weight matrix are the coefficients
    expressing $\omega_i$ as a rational combination of simple roots;
    for $A_3 = \mathfrak{sl}_4$ these coefficients encode the familiar
    fractions $\omega_1 = \tfrac{1}{4}(3\alpha_1+2\alpha_2+\alpha_3)$,
    etc.
  Text
    In type $A_{n-1}$ the coefficient of $\alpha_j$ in $\omega_i$ is
    $\min(i,j)-ij/n$.  Summing the rows therefore gives $\rho$, while
    pairing them with the simple coroots gives the identity matrix:
  Example
    W = fundamentalWeights(cartanA 4);
    matrix {sum entries W}     -- rho of A_3 in simple-root coordinates
    (cartanA 4 ** QQ) * transpose W
  Text
    Fundamental weights can be computed in the same way in the
    non-simply-laced classical families.  For $B_3=\mathfrak{so}_7$,
    $\omega_1$, $\omega_2$, and $\omega_3$ are respectively the vector,
    adjoint, and spin highest weights, of dimensions $7$, $21$, and $8$.
    The last fundamental weight of $B_n$ is
    $(\tfrac12,\ldots,\tfrac12)$ in the $\varepsilon$-basis and has
    half-integral coefficients in its simple-root expansion:
  Example
    fundamentalWeights(cartanB 3)
    apply(3, i -> (
      v := toList(i:0) | {1} | toList(2-i:0);
      weylDimension(cartanB 3, v)))
    last entries fundamentalWeights(cartanB 4)
  Text
    For $C_n=\mathfrak{sp}_{2n}$, the fundamental representations are
    the traceless exterior powers
    $V,\wedge^2V/\mathbb C,\ldots$.  In type $C_3$ their dimensions
    are $6$, $14$, and $14$.  Multiplying the Cartan matrix by the
    transpose of the fundamental-weight matrix again displays the
    defining root-coroot duality:
  Example
    WC = fundamentalWeights(cartanC 3);
    WC
    apply(3, i -> (
      v := toList(i:0) | {1} | toList(2-i:0);
      weylDimension(cartanC 3, v)))
    (cartanC 3 ** QQ) * transpose WC
  Text
    In type $D_n$, the last two fundamental weights are the two
    half-spin highest weights
    $(\tfrac12,\ldots,\tfrac12,\pm\tfrac12)$ in the
    $\varepsilon$-basis, and both representations have dimension
    $2^{n-1}$.  For $D_4$, triality makes the vector representation
    and both half-spin representations eight-dimensional:
  Example
    fundamentalWeights(cartanD 4)
    weylDimension(cartanD 4, {1,0,0,0})    -- vector
    weylDimension(cartanD 4, {0,0,1,0})    -- one half-spin representation
    weylDimension(cartanD 4, {0,0,0,1})    -- the other half-spin representation
SeeAlso
  toFundamental
  toSimple
  weylVector
  weylDimension
  cartanA
  cartanB
  cartanC
  cartanD
///

doc ///
Key
  toFundamental
Headline
  Change of basis: simple-root coordinates to fundamental-weight coordinates
Usage
  w = toFundamental(A, lambdaSimple)
Inputs
  A:Matrix
    a Cartan matrix
  lambdaSimple:List
    coordinates of a weight in the basis of simple roots
Outputs
  w:List
    the Dynkin labels $\langle\lambda,\alpha_i^\vee\rangle$; when
    $A$ is invertible these are coordinates in the fundamental-weight basis
Description
  Text
    Computes $\lambda \cdot A^T$ where $A$ is the Cartan matrix.  The
    Dynkin labels are the eigenvalues $\langle\lambda,\alpha_i^\vee
    \rangle$ of a weight under the coroots and determine whether a
    weight is dominant (see @TO isDominant@).
  Example
    A = cartanA 5;                       -- sl_5, rank 4
    toFundamental(A, {1,1,0,0})          -- alpha_1 + alpha_2 in Dynkin labels
    toFundamental(A, {1,1,1,1})          -- the highest root of sl_5
    toFundamental(A, weylVector A)       -- rho has Dynkin labels (1,...,1)
  Text
    The first output is $(1,1,-1,0)$: these are the pairings of
    $\alpha_1+\alpha_2$ with the four simple coroots.
    The next output $(1,0,0,1)$ is the highest weight of the adjoint
    representation of $\mathfrak{sl}_5$.  Finally $(1,1,1,1)$
    expresses $\rho$ as the sum of the fundamental weights.

SeeAlso
  toSimple
  fundamentalWeights
  isDominant
///

doc ///
Key
  toSimple
Headline
  Change of basis: fundamental-weight coordinates to simple-root coordinates
Usage
  w = toSimple(A, lambdaFund)
Inputs
  A:Matrix
    an invertible Cartan matrix
  lambdaFund:List
    coordinates of a weight in the basis of fundamental weights
Outputs
  w:List
    the same weight in the basis of simple roots
Description
  Text
    The inverse of @TO toFundamental@: the fundamental weights
    $\omega_i$ expressed in simple-root coordinates are the rows of
    $(A^{-1})^T$ and this function performs that change of basis.
    A singular Cartan matrix does not determine unique simple-root
    coordinates from Dynkin labels, so the function requires $A$ to
    be invertible.  Applying @TT "toFundamental"@ after
    @TT "toSimple"@ recovers the original Dynkin labels:
  Example
    A = cartanA 5;                       -- sl_5, rank 4
    toSimple(A, {1,0,0,0})               -- omega_1 in simple-root coords
    toSimple(A, {1,1,1,1})               -- rho
    toFundamental(A, toSimple(A, {2,0,1,0}))
  Text
    The first output $(4/5,3/5,2/5,1/5)$ gives
    $\omega_1=(4\alpha_1+3\alpha_2+2\alpha_3+\alpha_4)/5$.
    The next output $(2,3,3,2)$ gives $\rho$ in the same basis.
    The final output $(2,0,1,0)$ recovers the original Dynkin labels
    after changing basis in both directions.

SeeAlso
  toFundamental
  fundamentalWeights
///

doc ///
Key
  toEpsilon
Headline
  Convert simple-root coordinates to $\varepsilon$-basis coordinates
Usage
  v = toEpsilon(S, lambda)
Inputs
  S:Matrix
    a simple-root matrix whose rows express the simple roots in the
    $\varepsilon$-basis (e.g. the output of @TO simpleRootsB@)
  lambda:List
    coordinates of a weight in the simple-root basis
Outputs
  v:List
    the same weight expressed in the $\varepsilon$-basis
Description
  Text
    Multiplies the simple-root coordinate row on the left by the
    simple-root matrix: @TT "v = lambda * S"@.  Useful when a weight
    arises intrinsically (in simple-root coordinates) but the user
    wants to see its explicit $\varepsilon$-basis expression, for
    instance to compare with Bourbaki tables.
  Example
    S = simpleRootsB 3
    toEpsilon(S, {1,0,0})    -- alpha_1 = epsilon_1 - epsilon_2
    toEpsilon(S, {0,0,1})    -- alpha_3 = epsilon_3
    toEpsilon(S, {1,1,1})    -- alpha_1+alpha_2+alpha_3 = epsilon_1
  Text
    The three converted coordinate lists $(1,-1,0)$, $(0,0,1)$, and $(1,0,0)$ mean
    $\alpha_1=\varepsilon_1-\varepsilon_2$,
    $\alpha_3=\varepsilon_3$, and
    $\alpha_1+\alpha_2+\alpha_3=\varepsilon_1$.

SeeAlso
  fromEpsilon
  simpleRootsB
///

doc ///
Key
  fromEpsilon
Headline
  Convert $\varepsilon$-basis coordinates to simple-root coordinates
Usage
  lambda = fromEpsilon(S, v)
Inputs
  S:Matrix
    a simple-root matrix whose rows express the simple roots in the
    $\varepsilon$-basis
  v:List
    coordinates of a weight in the $\varepsilon$-basis
Outputs
  lambda:List
    the same weight in the simple-root basis
Description
  Text
    Inverse of @TO toEpsilon@: solves @TT "lambda * S == v"@ for the
    simple-root coordinate vector @TT "lambda"@, where $S$ is the
    simple-root matrix.  For a rectangular $S$, the input must lie in
    the row span of $S$; otherwise the function reports an error.
    Applying @TO toEpsilon@ to the result recovers $v$.
  Example
    S = simpleRootsB 3
    fromEpsilon(S, {1,1,0})   -- epsilon_1 + epsilon_2, a long root
    fromEpsilon(S, {1,0,0})   -- epsilon_1, a short root
    toEpsilon(S, fromEpsilon(S, {2,-1,0}))
  Text
    The first two converted coordinate lists $(1,2,2)$ and $(1,1,1)$ express
    $\varepsilon_1+\varepsilon_2$ and $\varepsilon_1$ in the
    $B_3$ simple-root basis.  The final $(2,-1,0)$ verifies that
    the two coordinate changes recover the original Euclidean vector.

SeeAlso
  toEpsilon
///

doc ///
Key
  rootHeight
Headline
  Height of a root in the simple-root basis
Usage
  h = rootHeight v
Inputs
  v:List
    coordinates of a root in the simple-root basis
Outputs
  h:ZZ
    the sum of the coordinates
Description
  Text
    The height of the highest root $\theta$ is $h - 1$ where $h$ is
    the @TO coxeterNumber@; heights are used throughout the package
    to specify height bounds.  A parabolic degree is the related sum over only
    the removed nodes.
  Example
    rootHeight highestRoot cartanA 4    -- 3 for A_3 (= rank)
    rootHeight highestRoot cartanE 6    -- 11
    rootHeight highestRoot cartanE 8    -- 29 (Coxeter number 30)
  Text
    Positive roots of a finite-type algebra are graded by height; in
    type $A_n$ there are exactly $n+1-k$ positive roots of height $k$
    (the superdiagonals of the strictly upper triangular matrices in
    $\mathfrak{sl}_{n+1}$):
  Example
    tally apply(entries cartanPositiveRoots cartanA 4, rootHeight)
  Text
    In $E_8$ heights range up to $h(E_8) - 1 = 29$, and the root of
    maximum height is unique -- the highest root $\theta$:
  Example
    T := tally apply(entries cartanPositiveRoots cartanE 8, rootHeight);
    max keys T
    T # 29
SeeAlso
  highestRoot
  coxeterNumber
  cartanPositiveRoots
///

doc ///
Key
  rootInnerProduct
Headline
  Bilinear form on simple-root coordinates induced by a Cartan matrix
Usage
  x = rootInnerProduct(A, v, w)
Inputs
  A:Matrix
    a Cartan matrix
  v:List
    coordinates of a root-lattice vector in the simple-root basis
  w:List
    coordinates of a root-lattice vector in the simple-root basis
Outputs
  x:QQ
    the value $(v,w)$ under the symmetric bilinear form $B=DA$
Description
  Text
    The package normalizes each connected component of the
    symmetrizer independently by setting its first entry to $1$;
    hence simple root $\alpha_i$ has squared length $2D_{ii}$, and
    scales across disconnected components are unrelated.  In type
    $G_2$ this gives long roots length$^2$
    $=6$ and short roots length$^2=2$; in type $B_n$ the long roots
    have length$^2=2$ and short roots length$^2=1$.  This form
    also determines @TO rootNormSquared@ and enters Peterson's
    recursion for the root multiplicities in @TO indefiniteRootSnapshot@.
  Text
    This first-node normalization is not the finite representation-theoretic
    convention used by @TO quadraticCasimir@.  For that purpose,
    @TO normalizedInvariantForm@ rescales every finite simple component so
    that its long roots have squared norm $2$.  The two forms differ only by
    a positive scalar on each connected component, but that scalar matters
    when quoting an absolute Casimir eigenvalue.
  Example
    A = cartanG2()
    rootInnerProduct(A, {1,0}, {0,1})  -- alpha_1 . alpha_2
    rootInnerProduct(A, {1,0}, {1,0})  -- |alpha_1|^2 (short)
    rootInnerProduct(A, {0,1}, {0,1})  -- |alpha_2|^2 (long, = 3|short|^2)
    -- B_3: alpha_3 is short, alpha_1, alpha_2 are long
    rootInnerProduct(cartanB 3, {0,0,1}, {0,0,1})
    rootInnerProduct(cartanB 3, {1,0,0}, {1,0,0})
  Text
    The $G_2$ outputs are $-3$, $2$, and $6$.  They are
    $(\alpha_1,\alpha_2)$, $(\alpha_1,\alpha_1)$, and
    $(\alpha_2,\alpha_2)$, respectively, so the long root has three
    times the squared length of the short root.  The $B_3$ outputs
    $1$ and $2$ give the short and long squared lengths in that type.

SeeAlso
  rootNormSquared
  symmetrizer
  normalizedInvariantForm
  quadraticCasimir
///

doc ///
Key
  rootNormSquared
Headline
  Squared norm of a root under the Weyl-invariant inner product
Usage
  x = rootNormSquared(A, v)
Inputs
  A:Matrix
    a Cartan matrix
  v:List
    coordinates of a root in the simple-root basis
Outputs
  x:QQ
    the squared norm of $v$
Description
  Text
    Shorthand for @TT "rootInnerProduct(A,v,v)"@.  Useful for
    distinguishing long from short roots in non-simply-laced types and for
    the package's root-multiplicity recursions.  Its first-node-normalized
    scale is not necessarily the long-root-length-$2$ scale used by
    @TO normalizedInvariantForm@ and @TO quadraticCasimir@.  For example,
    in type $C_3$ this command gives long roots squared norm $4$, whereas the
    normalized finite form rescales them to $2$.
  Example
    -- B_3: long roots have norm^2 = 2, short roots have norm^2 = 1
    rootNormSquared(cartanB 3, {1,0,0})
    rootNormSquared(cartanB 3, {0,0,1})
    -- G_2: long roots norm^2 = 6, short roots norm^2 = 2
    rootNormSquared(cartanG2(), {0,1})
    rootNormSquared(cartanG2(), {1,0})
  Text
    The answers $2,1,6,2$ give the squared lengths of a long and
    a short $B_3$ root, followed by a long and a short $G_2$ root.
    The absolute scale is fixed by the first simple root, not by
    requiring every long root to have squared length $2$.

SeeAlso
  rootInnerProduct
  symmetrizer
  normalizedInvariantForm
  quadraticCasimir
///

doc ///
Key
  dualCartanMatrix
Headline
  The transpose of a Cartan matrix, i.e. its Langlands dual
Usage
  B = dualCartanMatrix A
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
Description
  Text
    Returns $A^T$.  In the package's Bourbaki convention this swaps
    the roles of roots and coroots, i.e. the dual root system of
    $B_\ell$ is $C_\ell$, $F_4$ is self-dual (diagram unchanged up to
    reflection), and $G_2$ is self-dual up to reversal of its two
    nodes.
  Example
    dualCartanMatrix cartanB 3 == cartanC 3
    dualCartanMatrix cartanG2()
  Text
    Langlands duality interchanges long and short roots, so the
    Coxeter number is invariant but the dual Coxeter number is not:
  Example
    (coxeterNumber cartanB 3, coxeterNumber cartanC 3)
    (dualCoxeterNumber cartanB 3, dualCoxeterNumber cartanC 3)
  Text
    On nonsymmetric affine Cartan matrices, transpose generally swaps
    an untwisted type with its twisted Langlands dual.  The simply
    laced types $A_n^{(1)}$, $D_n^{(1)}$, and $E_n^{(1)}$ are
    self-dual.
  Example
    dualCartanMatrix cartanAffineA 3 == cartanAffineA 3
    dualCartanMatrix cartanAffineD 4 == cartanAffineD 4
SeeAlso
  symmetrizer
  coxeterNumber
  dualCoxeterNumber
///

------------------------------------------------------------
-- Root system data
------------------------------------------------------------

doc ///
Key
  negativeRoots
  (negativeRoots, Matrix)
Headline
  List of negative roots of a Cartan matrix
Usage
  R = negativeRoots A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  R:Matrix
    a matrix whose rows are the negative roots in the simple-root basis
Description
  Text
    Returns the negatives of the positive roots as a matrix.  The
    number of rows is the number of positive roots, which for a
    finite type is $(\dim\mathfrak g - r)/2$ where $r$ is the rank.
  Example
    negativeRoots cartanA 2
    numrows negativeRoots cartanD 4    -- 12 negative roots in so_8
  Text
    Each negative root is just the negation of the corresponding
    positive root, so @TT "numrows negativeRoots A == numrows cartanPositiveRoots A"@.
    For the exceptional types:
  Example
    numrows negativeRoots cartanE 6    -- 36
    numrows negativeRoots cartanE 7    -- 63
  Text
    Sorting negative roots by the absolute value of their height
    recovers the height grading "upside down":
  Example
    tally apply(entries negativeRoots cartanD 4, v -> - rootHeight v)
SeeAlso
  cartanPositiveRoots
  allRoots
  (negativeRoots,Matrix,ZZ)
///

doc ///
Key
  allRoots
  (allRoots, Matrix)
Headline
  List of all roots (positive and negative) of a Cartan matrix
Usage
  R = allRoots A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  R:Matrix
    a matrix whose rows are all roots (positive followed by negative)
    in the simple-root basis
Description
  Text
    Equivalent to @TT "cartanPositiveRoots A || negativeRoots A"@.
    The total count $|\Phi|$ equals $\dim\mathfrak g - r$.
  Example
    allRoots cartanA 3                 -- 6 roots of sl_3
    numrows allRoots cartanE 6         -- 72 roots of e_6
  Text
    The dimension of the Lie algebra is recovered as $|\Phi| + r$:
  Example
    numrows allRoots cartanE 7 + 7         -- dim e_7 = 133
    numrows allRoots cartanF4() + 4        -- dim f_4 = 52
    numrows allRoots cartanG2() + 2        -- dim g_2 = 14
  Text
    The root system is symmetric under negation: for every positive
    root $\alpha$, $-\alpha$ is also in $\Phi$.  In particular the
    positive-height and negative-height parts balance:
  Example
    # select(entries allRoots cartanB 3, v -> rootHeight v > 0)
    # select(entries allRoots cartanB 3, v -> rootHeight v < 0)
SeeAlso
  cartanPositiveRoots
  negativeRoots
  (allRoots,Matrix,ZZ)
///

doc ///
Key
  highestRoot
Headline
  The highest root of a finite-type root system
Usage
  theta = highestRoot A
Inputs
  A:Matrix
    a connected finite-type Cartan matrix
Outputs
  theta:List
    the unique positive root of maximal height, in the simple-root basis
Description
  Text
    In the simple-root basis, the highest root $\theta$ of $A_n$ is
    $(1,1,\dots,1)$ (sum of all simple roots); of $E_8$ it is
    $(2,3,4,6,5,4,3,2)$, the marks of the extended Dynkin diagram.
    The Dynkin labels of $\theta$ give the highest weight
    of the adjoint representation:
  Example
    highestRoot cartanA 4              -- sum of all simple roots of A_3
    highestRoot cartanE 8              -- marks of affine E_8
    toFundamental(cartanA 5, highestRoot cartanA 5)
    -- adjoint of sl_5 has highest weight (1,0,0,1) in Dynkin labels
  Text
    The first tuple $(1,1,1)$ means
    $\theta=\alpha_1+\alpha_2+\alpha_3$ in $A_3$.
    The $E_8$ tuple similarly gives the coefficients of its highest
    root, one coefficient for each displayed Dynkin node.  The final
    $(1,0,0,1)$ describes the highest root of $A_4$ in Dynkin labels,
    hence the highest weight of its adjoint representation.

SeeAlso
  highestShortRoot
  coxeterNumber
  rootHeight
///

doc ///
Key
  highestShortRoot
Headline
  The highest short root of a connected finite-type root system
Usage
  theta = highestShortRoot A
Inputs
  A:Matrix
    a connected finite-type Cartan matrix
Outputs
  theta:List
    the positive root of maximal height among roots of minimal length
Description
  Text
    For simply laced types ($A$, $D$, $E$) every root has the same
    length, so this coincides with @TO highestRoot@.  For non-simply
    laced types, the highest root and highest short root differ.  The
    coroot attached to the highest long root of $A$ corresponds to the highest short root of
    $A^T$, which is what enters the @TO dualCoxeterNumber@ formula.
  Example
    highestShortRoot cartanB 3
    highestRoot cartanB 3
    highestShortRoot cartanG2()
    highestRoot cartanG2()
  Text
    In $B_3$ the two outputs are $(1,1,1)$ and $(1,2,2)$: the highest short root is
    $\alpha_1+\alpha_2+\alpha_3$, whereas the highest root is
    $\alpha_1+2\alpha_2+2\alpha_3$. In $G_2$ the outputs $(2,1)$ and $(3,2)$ similarly
    distinguish $2\alpha_1+\alpha_2$ from $3\alpha_1+2\alpha_2$.

    In the simply-laced cases the two agree:
  Example
    highestShortRoot cartanE 6 == highestRoot cartanE 6
    highestShortRoot cartanD 5 == highestRoot cartanD 5
SeeAlso
  highestRoot
  dualCoxeterNumber
  rootNormSquared
///

doc ///
Key
  weylVector
Headline
  The Weyl vector $\rho$
Usage
  rho = weylVector A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  rho:List
    the sum of the fundamental weights in simple-root coordinates; in
    finite type this is the half-sum of the positive roots
Description
  Text
    The Weyl vector is defined here by
    $\rho=\omega_1+\cdots+\omega_r$, so its Dynkin labels are all $1$.
    In finite type it also satisfies
    $\rho = \frac12\sum_{\alpha\in\Phi^+}\alpha$.  In affine and
    indefinite type the positive roots are infinite, so the half-sum
    is not a finite sum.  This function requires finite type,
    where $A$ is invertible and $\rho$ has unambiguous simple-root
    coordinates.  The Weyl vector enters the
    Weyl character / Weyl dimension formulas and the affine (``dot'')
    action $w\cdot\lambda = w(\lambda+\rho)-\rho$ used in
    @TO kostantLieAlgCohomology@.
  Example
    weylVector cartanA 3
    toFundamental(cartanA 3, weylVector cartanA 3)  -- all 1s
    -- rho for E_8 in simple-root basis: a large integer vector
    weylVector cartanE 8
  Text
    In $A_2$ the output $(1,1)$ means
    $\rho=\alpha_1+\alpha_2$; its Dynkin labels are also $(1,1)$.
    Each entry of the final $E_8$ tuple is the coefficient of the
    corresponding simple root in half the sum of all positive roots.

SeeAlso
  isDominant
  coxeterNumber
  dotActionOnWeight
///

doc ///
Key
  isDominant
  (isDominant,Matrix,List)
  (isDominant,Matrix,Matrix)
  [isDominant,WeightFormat]
Headline
  Test whether a weight is dominant
Usage
  b = isDominant(A, lambda)
  b = isDominant(A, lambda, WeightFormat => fmt)
Inputs
  A:Matrix
    a Cartan matrix
  lambda:List
    a weight, in the format selected by @TT "WeightFormat"@
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; see
    @TO WeightFormat@
Outputs
  b:Boolean
    true if all Dynkin labels of $\lambda$ (in Dynkin coordinates)
    are non-negative
Description
  Text
    A weight is dominant if
    $\langle\lambda,\alpha_i^\vee\rangle\geq0$ for every simple
    coroot.  In finite type, dominant integral weights parameterize
    the finite-dimensional irreducible highest-weight representations.
    When @TT "WeightFormat => \"Partition\""@ is given,
    @TT "lambda"@ is converted to Dynkin labels by
    @TO partitionToFundamental@ before testing these inequalities.
  Example
    isDominant(cartanA 4, {1,0,1})      -- dominant
    isDominant(cartanA 4, {1,-1,2})     -- not dominant
    isDominant(cartanE 6, {0,0,0,0,0,0}) -- the trivial rep
  Example
    -- Partition-form test on so_7: (1,1,0) is the adjoint.
    isDominant(cartanB 3, {1,1,0}, WeightFormat => "Partition")
  Text
    The first three answers are true, false, and true: a negative
    Dynkin label prevents dominance, whereas the zero weight is dominant.
    The partition $(1,1,0)$ is also dominant for $B_3$; it describes
    its adjoint highest weight, whose Dynkin labels are $(0,1,0)$.

SeeAlso
  WeightFormat
  weylVector
  weylDimension
///

doc ///
Key
  coxeterNumber
Headline
  Coxeter number of a connected finite-type Cartan matrix
Usage
  h = coxeterNumber A
Inputs
  A:Matrix
    a connected finite-type Cartan matrix
Outputs
  h:ZZ
    one plus the height of the highest root
Description
  Text
    The Coxeter number $h$ is the order of a Coxeter element of the
    Weyl group and equals $\frac{|\Phi|}{r} = (\text{ht}\,\theta)+1$.
    The values are $h(A_n) = n+1$, $h(B_n) = h(C_n) = 2n$,
    $h(D_n) = 2n-2$, $h(G_2) = 6$, $h(F_4) = 12$, $h(E_6) = 12$,
    $h(E_7) = 18$, $h(E_8) = 30$.
  Example
    coxeterNumber cartanA 4              -- 4 (for A_3 = sl_4)
    coxeterNumber cartanE 6              -- 12
    coxeterNumber cartanE 8              -- 30
    coxeterNumber cartanG2()             -- 6
  Text
    The outputs $4,12,30,6$ are the Coxeter numbers of
    $A_3,E_6,E_8,G_2$.  A product of the simple reflections, each used
    once, has exactly this order in the corresponding Weyl group.

SeeAlso
  dualCoxeterNumber
  highestRoot
  rootHeight
///

doc ///
Key
  dualCoxeterNumber
Headline
  Dual Coxeter number $h^\vee$ of a connected finite type
Usage
  hv = dualCoxeterNumber A
Inputs
  A:Matrix
    a connected finite-type Cartan matrix
Description
  Text
    Defined as $\langle \rho, \theta^\vee\rangle + 1$ where $\theta$ is
    the highest root.  Equivalently, one plus the height of the
    highest short root of the dual root system.  For simply-laced
    types $h^\vee = h$.  For the non-simply-laced types,
    $h^\vee(B_n) = 2n-1$, $h^\vee(C_n) = n+1$, $h^\vee(G_2) = 4$,
    $h^\vee(F_4) = 9$.
  Example
    dualCoxeterNumber cartanE 8        -- 30 (= h for simply laced)
    dualCoxeterNumber cartanB 3        -- 5
    dualCoxeterNumber cartanC 3        -- 4
    dualCoxeterNumber cartanG2()       -- 4
  Text
    The outputs $30,5,4,4$ are the dual Coxeter numbers of
    $E_8,B_3,C_3,G_2$.  In the normalization with long roots of
    squared length $2$, the adjoint Casimir eigenvalue is twice
    this number; for example it is $8$ for $G_2$.

SeeAlso
  coxeterNumber
  highestShortRoot
///

doc ///
Key
  dynkinDiagramData
  (dynkinDiagramData,Matrix)
  (dynkinDiagramData,CartanDatum)
  (dynkinDiagramData,Realization)
  (dynkinDiagramData,RootDatum)
  [dynkinDiagramData,NodeFormat]
Headline
  Nodes and bonds of a Dynkin diagram
Usage
  d = dynkinDiagramData A
  d = dynkinDiagramData D
Inputs
  A:Matrix
    a Cartan matrix, or the Cartan data of a specified root datum
Outputs
  d:HashTable
    nodes, bonds, and rank; for a Cartan datum or realization, both
    node positions and chosen labels
Description
  Text
    Describes the nodes and bonds of the diagram.  For a matrix,
    nodes are numbered
    $1,\dots,r$.
    @TT "d#\"edges\""@ contains one list @TT "{i,j,Aij,Aji}"@ for
    every connected pair $i<j$; the two Cartan entries determine bond
    multiplicity and arrow direction.
    For a @TO CartanDatum@, @TO Realization@, or @TO RootDatum@,
    @TT "\"positions\""@ and
    @TT "\"positionEdges\""@ always retain this form,
    @TT "\"nodeLabels\""@ and @TT "\"labeledEdges\""@ use the chosen
    labels, and @TT "\"nodeRecords\""@ and @TT "\"recordEdges\""@ give
    both.  The @TO NodeFormat@ option specifies which convention appears
    in @TT "\"nodes\""@ and @TT "\"edges\""@.
  Example
    dynkinDiagramData cartanA 3
    dynkinDiagramData cartanB 3
    dynkinDiagramData cartanG2()
    D = cartanDatum(cartanB 3,NodeLabels=>{"left","middle","short"});
    data = dynkinDiagramData D;
    (data#"nodeFormat",data#"positions",data#"nodeLabels",
        data#"positionEdges")
  Text
    For $B_3$, the edge $(2,3,-1,-2)$ joins the long root at position $2$ to the short
    root at position $3$ by a double bond. For $G_2$, $(1,2,-3,-1)$ gives a triple bond
    pointing toward the short root at position $1$. The last tuple retains positions
    $(1,2,3)$ and their names left, middle, short; its edge list still uses matrix
    positions.

    For branched types like $D$ and $E$, the branch node appears as
    an endpoint of three edges, making the graph structure of the
    diagram directly readable from the list:
  Example
    dynkinDiagramData cartanD 5
    dynkinDiagramData cartanE 6
  Text
    The edge count equals rank minus the number of connected components
    for every tree-shaped diagram.  Finite Dynkin diagrams are trees.
    Among affine diagrams, type $A_n^{(1)}$ is a cycle (with the
    familiar rank-two multiple-edge exception), while the other
    connected affine diagrams are trees.  Thus there is no single
    affine edge-count formula:
  Example
    #((dynkinDiagramData cartanE 8)#"edges")        -- 7
    #((dynkinDiagramData cartanAffineA 4)#"edges") -- 5: a cycle
    #((dynkinDiagramData cartanAffineE 8)#"edges") -- 8: a tree
SeeAlso
  dynkinDiagram
  CartanDatum
  Realization
  RootDatum
  NodeLabels
  NodeFormat
///

doc ///
Key
  dynkinDiagram
  (dynkinDiagram, Matrix)
  (dynkinDiagram, String)
  (dynkinDiagram, String, ZZ)
  (dynkinDiagram, CartanDatum)
  (dynkinDiagram, Realization)
  (dynkinDiagram, RootDatum)
  [dynkinDiagram,NodeFormat]
Headline
  Display a Dynkin diagram with its node numbers and labels
Usage
  s = dynkinDiagram A
  s = dynkinDiagram D
  s = dynkinDiagram "A4"
  s = dynkinDiagram("B", 3)
Inputs
  A:Matrix
    a (generalized) Cartan matrix in the Bourbaki convention
    $A_{ij} = 2(\alpha_i,\alpha_j)/(\alpha_i,\alpha_i)$
  D:CartanDatum
    a Cartan datum with chosen node labels
Outputs
  s:String
    a text drawing of the Dynkin diagram
Description
  Text
    For matrix input, each node is represented by its $1$-based position.
    A @TO CartanDatum@, @TO Realization@, or @TO RootDatum@ with custom
    labels displays both pieces of information by default, in the form
    @TT "position[label]"@.  Thus the diagram remains directly usable in
    commands that take node numbers while also showing the chosen root
    names.  Single, double,
    and triple bonds appear as @TT "---"@, @TT "==="@,
    and @TT "###"@ respectively; non-simply-laced bonds carry an arrow
    pointing from the long root to the short root.  Branch nodes of
    degree $3$ (types $D$ and $E$) are drawn with a one-node side
    branch underneath the main path.  Disconnected Dynkin diagrams are
    displayed component by component.  If a branch is too complicated
    for the compact layout (for example, a multiple bond on a side
    branch), the diagram is described by a list of all its bonds.

    The function can also be called with a type-and-rank specification:
    a single string like @TT "\"A4\""@ or @TT "\"E6\""@, or a
    @TO String@/@TO ZZ@ pair such as @TT "(\"D\", 5)"@.  In @TT "\"An\""@
    the integer $n$ is the rank (so the Lie algebra is $\mathfrak{sl}_{n+1}$).
    The string must be a supported type letter followed by its rank,
    as in the examples below.
  Example
    dynkinDiagram cartanA 4
    dynkinDiagram cartanB 4
    dynkinDiagram cartanG2()
    dynkinDiagram "E6"
    dynkinDiagram("F", 4)
  Text
    The first drawing is the chain $1$--$2$--$3$ of $A_3=\mathfrak{sl}_4$. The $B_4$
    drawing has a double bond pointing from long node $3$ to short node $4$, while the
    $G_2$ triple bond points from node $2$ to short node $1$. These are the node
    positions to use in Weyl and parabolic commands.

    The @TO NodeFormat@ option accepts @TT "\"Positions\""@,
    @TT "\"Labels\""@, or @TT "\"Records\""@ for a Cartan datum or
    realization.  @TT "\"Records\""@ is the default when labels have been
    chosen by the user and displays
    both position and label.  This is unambiguous even when an integer label
    is also a valid position.
  Example
    D = cartanDatum(
        cartanAffineA 2,
        NodeLabels=>{"affine","left","right"});
    print dynkinDiagram D
    print dynkinDiagram(D,NodeFormat=>"Labels")
    E = cartanDatum(cartanA 4,NodeLabels=>{2,1,3});
    print dynkinDiagram E
  Text
    The same diagram can be read as a list of bonds with
    @TO dynkinDiagramData@.  This also makes it accessible to screen
    readers: each edge @TT "{i,j,Aij,Aji}"@ gives its endpoints and both
    Cartan entries, without relying on spacing or arrows:
  Example
    (dynkinDiagramData cartanD 4)#"edges"
    (dynkinDiagramData cartanG2())#"edges"
    data = dynkinDiagramData D;
    (data#"positions",data#"nodeLabels",data#"positionEdges")
  Text
    The node numbers specify which simple roots a command uses.
    For example, the diagram shows that node $2$ is central in $D_4$;
    passing @TT "{2}"@
    as the parabolic list removes that node and leaves the three outer
    nodes in the Levi:
  Example
    A = cartanD 4;
    print dynkinDiagram A
    (leviSubalgebra(A, {2}))#"nodes"
  Text
    For a Cartan matrix that is a direct sum of finite-type blocks,
    each connected component is displayed separately.
SeeAlso
  dynkinDiagramData
  CartanDatum
  Realization
  RootDatum
  NodeLabels
  NodeFormat
  "Reading diagrams and coordinates"
  leviSubalgebra
  cartanA
  cartanB
  cartanC
  cartanD
  cartanE
  cartanF4
  cartanG2
///

------------------------------------------------------------
-- Representation theory basics
------------------------------------------------------------

doc ///
Key
  weylDimension
  (weylDimension,Matrix,List)
  [weylDimension,WeightFormat]
Headline
  Dimension of the irreducible representation of a given highest weight
Usage
  d = weylDimension(A, lambda)
  d = weylDimension(A, lambda, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a dominant integral highest weight.  In the default
    @TT "\"Dynkin\""@ format, @TT "lambda"@ is a list of Dynkin
    labels (coefficients in the basis of fundamental weights) and
    @TT "#lambda"@ must equal the rank.  In @TT "\"Partition\""@
    format, @TT "lambda"@ is a partition (in the
    $\varepsilon$-basis) and $A$ must be a classical-type Cartan
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; see
    @TO WeightFormat@
Outputs
  d:ZZ
    the dimension of the irreducible representation $V(\lambda)$ of
    the semisimple Lie algebra with Cartan matrix $A$
Description
  Text
    Computes the dimension by the Weyl dimension formula
    $$\dim V(\lambda) = \prod_{\alpha > 0}
        \frac{(\lambda+\rho,\alpha)}{(\rho,\alpha)},$$
    where the product runs over positive roots and $\rho$ is the Weyl
    vector.  The invariant form is the symmetrized Cartan form.
  Example
    weylDimension(cartanA 3, {1,1})              -- adjoint of sl_3
    weylDimension(cartanA 4, {0,1,0})            -- Lambda^2 of sl_4 standard
    weylDimension(cartanE 8, {0,0,0,0,0,0,0,1})  -- adjoint of E_8 (248)
  Text
    The answers are $8$, $6$, and $248$: the dimensions of the $A_2$ adjoint
    representation, $\bigwedge^2\mathbb C^4$ in type $A_3$, and the $E_8$ adjoint
    representation. The input tuples are highest-weight Dynkin labels, so each output is
    the dimension of one irreducible module, not its number of distinct weights.

    The same weights may be specified as partitions by setting
    @TT "WeightFormat => \"Partition\""@.  For instance, the
    adjoint of $\mathfrak{sl}_4$ has partition $(2,1,1,0)$ (Dynkin
    $(1,0,1)$), the spin representation of $\mathfrak{so}_7$ has
    partition $(1/2,1/2,1/2)$ (Dynkin $(0,0,1)$), and the two
    half-spin representations of $\mathfrak{so}_8$ have partitions
    $(1/2,1/2,1/2,\pm 1/2)$:
  Example
    weylDimension(cartanA 4, {2,1,1,0},        WeightFormat => "Partition")  -- 15
    weylDimension(cartanB 3, {1/2,1/2,1/2},    WeightFormat => "Partition")  -- 8
    weylDimension(cartanD 4, {1/2,1/2,1/2, 1/2}, WeightFormat => "Partition")  -- 8
    weylDimension(cartanD 4, {1/2,1/2,1/2,-1/2}, WeightFormat => "Partition")  -- 8
SeeAlso
  WeightFormat
  weightMultiplicity
  weightsOfRep
  tensorDecomposition
  partitionToFundamental
  fundamentalToPartition
///

doc ///
Key
  weightsOfRep
  (weightsOfRep,Matrix,List)
  [weightsOfRep,WeightFormat]
Headline
  All weights appearing in an irreducible representation
Usage
  L = weightsOfRep(A, lambda)
  L = weightsOfRep(A, lambda, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a dominant integral highest weight.  In default @TT "\"Dynkin\""@
    format, Dynkin labels; in @TT "\"Partition\""@ format, a
    partition (requires $A$ of classical type)
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; see
    @TO WeightFormat@.  The option governs @EM "both"@ the input
    format of @TT "lambda"@ and the output format of the returned
    weights
Outputs
  L:List
    the list of all weights of $V(\lambda)$, in the format selected
    by @TT "WeightFormat"@, sorted in reverse lexicographic order.
    This order need not place the highest weight first.  Multiplicities are not
    reported here; use @TO weightMultiplicity@ for those
Description
  Text
    Finds all the weights of $V(\lambda)$ by the
    simple-root string method: starting from the highest
    weight, each root string through a known weight extends the set
    to include all strings predicted by the integrality condition
    $\langle\mu,\alpha_i^\vee\rangle \in \mathbb Z$.
  Example
    -- The sl_3 adjoint has seven distinct weights: six roots and zero.
    L = weightsOfRep(cartanA 3, {1,1});
    #L
    L
  Text
    Each pair is a Dynkin-label vector: @TT "{a,b}"@ means
    $a\omega_1+b\omega_2$.  The six nonzero entries are the roots and
    @TT "{0,0}"@ is the zero weight.  This function lists each distinct
    weight once; in the adjoint, the zero-weight space has dimension
    $2$, which can be checked with @TO weightMultiplicity@.
  Example
    -- The spin rep of so_7 (partition (1/2,1/2,1/2)) has 8 weights,
    -- each with all entries +/- 1/2.
    weightsOfRep(cartanB 3, {1/2,1/2,1/2}, WeightFormat => "Partition")
SeeAlso
  WeightFormat
  weightMultiplicity
  dominantWeightsInRep
  weightOrbit
///

doc ///
Key
  weightMultiplicity
  (weightMultiplicity,Matrix,List,List)
  [weightMultiplicity,WeightFormat]
Headline
  Multiplicity of a single weight in an irreducible representation
Usage
  m = weightMultiplicity(A, lambda, mu)
  m = weightMultiplicity(A, lambda, mu, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    the highest weight of $V(\lambda)$, in the format selected by
    @TT "WeightFormat"@
  mu:List
    the weight whose multiplicity is requested, in the format
    selected by @TT "WeightFormat"@
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; see
    @TO WeightFormat@.  Both @TT "lambda"@ and @TT "mu"@ are
    interpreted in the chosen convention
Outputs
  m:ZZ
    the dimension of the $\mu$-weight space of $V(\lambda)$; zero if
    $\mu$ is not a weight of $V(\lambda)$
Description
  Text
    Computes the multiplicity via Freudenthal's recursion
    $$\bigl[(\lambda+\rho,\lambda+\rho) - (\mu+\rho,\mu+\rho)\bigr]\,
         m_\lambda(\mu)
       = 2 \sum_{\alpha > 0} \sum_{k \geq 1}
          (\mu + k\alpha, \alpha)\, m_\lambda(\mu + k\alpha),$$
    processing weights in decreasing order of height.  Inner products
    are taken with the symmetrized Cartan form.  The first calculation
    finds all multiplicities for $V(\lambda)$; later calls with the
    same highest weight reuse them.  Use @TO weightMultiplicityTable@
    to obtain the entire table of weights and multiplicities.
  Example
    -- Adjoint of sl_3: weight zero has multiplicity 2 (Cartan dim).
    weightMultiplicity(cartanA 3, {1,1}, {0,0})
  Example
    -- In the sl_4 adjoint, the zero-weight space has dimension 3.
    weightMultiplicity(cartanA 4, {1,0,1}, {0,0,0})
  Text
    Thus the output $3$ means
    $$\dim V(1,0,1)_{(0,0,0)}=3,$$
    as expected for the Cartan subalgebra of $\mathfrak{sl}_4$.
  Example
    -- Same adjoint-at-zero computation, but specified as partitions:
    -- Dynkin (1,0,1) <-> partition (2,1,1,0), and the zero weight
    -- is partition (0,0,0,0).  Answer is still 3 (Cartan dimension).
    weightMultiplicity(cartanA 4, {2,1,1,0}, {0,0,0,0}, WeightFormat => "Partition")
SeeAlso
  WeightFormat
  weightsOfRep
  dominantWeightsInRep
  tensorDecomposition
///

doc ///
Key
  dominantWeightsInRep
  (dominantWeightsInRep,Matrix,List)
  [dominantWeightsInRep,WeightFormat]
Headline
  Dominant weights occurring in an irreducible representation
Usage
  L = dominantWeightsInRep(A, lambda)
  L = dominantWeightsInRep(A, lambda, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a dominant integral highest weight, in the format selected by
    @TT "WeightFormat"@
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; see
    @TO WeightFormat@.  The option governs both the input format of
    @TT "lambda"@ and the output format of the returned weights
Outputs
  L:List
    the list of those weights of $V(\lambda)$ that are dominant (all
    Dynkin labels $\geq 0$), sorted in reverse lexicographic order
Description
  Text
    Every Weyl orbit of weights in $V(\lambda)$ meets the dominant
    chamber in exactly one point; this function returns that
    representative for each orbit.
  Example
    -- Adjoint of sl_3: dominant weights of V(1,1) are (1,1) and (0,0).
    dominantWeightsInRep(cartanA 3, {1,1})
  Example
    -- Adjoint of sl_4: V(1,0,1).
    dominantWeightsInRep(cartanA 4, {1,0,1})
  Example
    -- Same adjoint-of-sl_4 computation in partition form: V(1,0,1)
    -- has partition (2,1,1,0); its dominant weights are (2,1,1,0)
    -- and (0,0,0,0), the trivial weight.
    dominantWeightsInRep(cartanA 4, {2,1,1,0}, WeightFormat => "Partition")
  Text
    For the $\mathfrak{sl}_3$ adjoint representation, the dominant
    weights are $(1,1)$ and $(0,0)$, representing the highest root
    and the zero weight.  The $\mathfrak{sl}_4$ outputs give the same
    statement as $(1,0,1)$ and $(0,0,0)$ in Dynkin labels, or
    $(2,1,1,0)$ and $(0,0,0,0)$ in partition coordinates.  These
    are dominant weights occurring inside the representation, not
    a decomposition into irreducible summands.

SeeAlso
  WeightFormat
  weightsOfRep
  weightMultiplicity
///

doc ///
Key
  weightOrbit
  (weightOrbit,Matrix,List)
  [weightOrbit,WeightFormat]
Headline
  Weyl group orbit of a weight
Usage
  O = weightOrbit(A, lambda)
  O = weightOrbit(A, lambda, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a weight (not required to be dominant), in the format selected
    by @TT "WeightFormat"@
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; see
    @TO WeightFormat@.  Both the input @TT "lambda"@ and the
    returned orbit elements are in the chosen format
Outputs
  O:List
    the list of all weights in the Weyl-group orbit $W\cdot\lambda$
    (linear action, not the dot action), sorted in reverse
    lexicographic order
Description
  Text
    Finds the orbit by repeatedly applying the simple reflections
    $s_i(\mu) = \mu - \langle\mu,\alpha_i^\vee\rangle\alpha_i$
    expressed in Dynkin coordinates.
  Example
    -- Orbit of the highest weight of the standard sl_5 rep: the 5 weights
    -- of the standard representation.
    weightOrbit(cartanA 5, {1,0,0,0})
  Text
    These are the five weights $\varepsilon_1,\ldots,\varepsilon_5$
    of the standard $\mathfrak{sl}_5$ representation, written in
    Dynkin labels.  For example, @TT "{-1,1,0,0}"@ is another weight
    in the orbit, not a negative multiplicity; only the highest weight
    @TT "{1,0,0,0}"@ is dominant.
  Example
    #(weightOrbit(cartanA 4, {1,0,0}))
  Example
    -- The same sl_5 standard orbit viewed in partition coordinates:
    -- outputs are epsilon/GL-coordinate representatives normalized
    -- to have final coordinate zero; arbitrary orbit weights can
    -- therefore have negative entries.
    weightOrbit(cartanA 5, {1,0,0,0,0}, WeightFormat => "Partition")
  Text
    Here @TT "{-1,-1,-1,-1,0}"@ is the fifth standard weight after
    adding a common constant so that the final coordinate is zero.
    Thus @TT "Partition"@ names the coordinate convention for an
    arbitrary orbit weight; returned tuples need not be nonnegative
    partitions.
  Example
    -- Orbit of the spin highest weight of so_7 (partition
    -- (1/2,1/2,1/2)) has 8 elements: all +/-1/2 combinations.
    weightOrbit(cartanB 3, {1/2,1/2,1/2}, WeightFormat => "Partition")
SeeAlso
  WeightFormat
  weylGroupOrder
  weylGroupElements
  tensorDecomposition
///

doc ///
Key
  tensorDecomposition
  (tensorDecomposition,Matrix,List,List)
  [tensorDecomposition,WeightFormat]
Headline
  Decompose a tensor product of two irreducible representations
Usage
  T = tensorDecomposition(A, lambda, mu)
  T = tensorDecomposition(A, lambda, mu, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a dominant integral highest weight in the selected format
  mu:List
    a dominant integral highest weight in the selected format
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@.  In
    @TT "\"Partition\""@ coordinates, input partitions are converted to
    Dynkin labels by @TO partitionToFundamental@ for the computation,
    and output Dynkin labels are converted back by
    @TO fundamentalToPartition@.  This convention requires connected
    classical type $A$, $B$, $C$, or $D$.
Outputs
  T:Tally
    a tally whose keys are @TO Irrep@ objects giving the dominant
    highest weights $\nu$ appearing in
    $V(\lambda)\otimes V(\mu) = \bigoplus c^\nu_{\lambda,\mu} V(\nu)$,
    and whose values are the Littlewood-Richardson-type multiplicities
    $c^\nu_{\lambda,\mu}$
Description
  Text
    Computes the decomposition by Racah--Speiser straightening.  For
    each weight $\eta$ of the factor of smaller dimension, with
    multiplicity $m(\eta)$, move
    $\lambda' + \eta + \rho$ to the dominant chamber, where
    $\lambda'$ is the larger factor.  A singular shifted weight
    contributes zero; a regular one contributes
    $\operatorname{sgn}(w)m(\eta)$ to
    $V(w(\lambda'+\eta+\rho)-\rho)$.  Summing these signed
    contributions gives the multiplicity of each irreducible summand.
    The character and decomposition are retained for later calls;
    exchanging @TT "lambda"@ and @TT "mu"@ reuses the same result.

    With Dynkin coordinates, $A$ may also be a disconnected finite-type
    Cartan matrix.  Each key then has one @TO Irrep@ factor per
    connected component.  For a
    connected matrix it therefore prints as @TT "V(...)"@.  The
    @TT "WeightFormat => \"Partition\""@
    option interprets @TT "lambda"@ and @TT "mu"@ as partition-form
    highest weights (via @TO partitionToFundamental@) and also
    converts the output summands to partition form (via
    @TO fundamentalToPartition@).  It works for any of the classical
    types $A$, $B$, $C$, $D$; in type $B$ or $D$, spin (half-integer)
    inputs and summands appear as partitions with rational entries.
  Example
    -- sl_3 standard x standard: V(1,0) ** V(1,0) = V(2,0) + V(0,1)
    tensorDecomposition(cartanA 3, {1,0}, {1,0})
  Text
    The output @TT "Tally{V(0,1) => 1, V(2,0) => 1}"@ is the
    decomposition
    $$V(1,0)\otimes V(1,0)\cong V(2,0)\oplus V(0,1).$$
    A Tally key names a summand and the integer after @TT "=>"@ gives
    its multiplicity.  Equivalently,
    $$\mathbb C^3\otimes\mathbb C^3\cong
      \operatorname{Sym}^2\mathbb C^3\oplus
      \bigwedge^2\mathbb C^3,$$
    with dimensions $6+3=9$.
  Example
    -- sl_3 standard x dual standard: V(1,0) ** V(0,1) = V(1,1) + V(0,0)
    tensorDecomposition(cartanA 3, {1,0}, {0,1})
  Text
    Similarly, this reads
    $$V(1,0)\otimes V(0,1)\cong V(1,1)\oplus V(0,0)
      =\mathfrak{sl}_3\oplus\mathbb C.$$
  Example
    -- Same sl_4 standard-x-standard decomposition in partition form
    -- (Littlewood-Richardson): (1,0,0) (x) (1,0,0) = (2,0,0) + (1,1,0).
    tensorDecomposition(cartanA 4, {1,0,0}, {1,0,0}, WeightFormat => "Partition")
  Example
    -- so_7 vector tensor vector (both factors partition (1,0,0)):
    -- (1,0,0) (x) (1,0,0) = () + (1,1,0) + (2,0,0),
    -- i.e. 7 (x) 7 = 1 + 21 + 27.
    tensorDecomposition(cartanB 3, {1,0,0}, {1,0,0}, WeightFormat => "Partition")
  Text
    In direct-sum notation, the displayed partition-form Tally means
    $$\mathbb C^7\otimes\mathbb C^7\cong
      V(2,0,0)\oplus V(1,1,0)\oplus V(0,0,0),$$
    with dimensions $27+21+1=49$.
  Example
    -- Two so_7 spin reps tensor: 8 (x) 8 = 1 + 7 + 21 + 35; in
    -- partition form each spin input is (1/2, 1/2, 1/2), and the
    -- summands are (), (1), (1,1), (1,1,1) (all integer, since the
    -- parity of m_n is preserved and is even on both sides).
    tensorDecomposition(cartanB 3, {1/2,1/2,1/2}, {1/2,1/2,1/2}, WeightFormat => "Partition")
  Example
    -- sp_6 standard x standard (partition (1,0,0) on both sides):
    -- 6 (x) 6 = 1 + 14 + 21, partitions (), (1,1), (2).
    tensorDecomposition(cartanC 3, {1,0,0}, {1,0,0}, WeightFormat => "Partition")
  Example
    -- so_8: half-spin(+) tensor half-spin(-) in partition form as
    -- (1/2,1/2,1/2,1/2) and (1/2,1/2,1/2,-1/2), summing to the
    -- vector (1,0,0,0) and the 56-dim wedge^3 (1,1,1,0).
    tensorDecomposition(cartanD 4, {1/2,1/2,1/2,1/2}, {1/2,1/2,1/2,-1/2}, WeightFormat => "Partition")
SeeAlso
  weylDimension
  weightsOfRep
  weightMultiplicity
  tensorCoefficient
  Irrep
  WeightFormat
  fundamentalToPartition
  partitionToFundamental
///

doc ///
Key
  tensorCoefficient
  (tensorCoefficient,Matrix,List,List,List)
  [tensorCoefficient,WeightFormat]
Headline
  Multiplicity of one irreducible summand in a tensor product
Usage
  c = tensorCoefficient(A, lambda, mu, nu)
  c = tensorCoefficient(A, lambda, mu, nu, WeightFormat => fmt)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  lambda:List
    a dominant integral highest weight in the selected format
  mu:List
    a dominant integral highest weight in the selected format
  nu:List
    a dominant integral highest weight in the selected format
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; in
    @TT "\"Partition\""@ format all three highest weights are
    interpreted in partition form by
    @TO partitionToFundamental@.  Requires $A$ to be a connected
    Cartan of classical type $A$, $B$, $C$, or $D$.
Outputs
  c:ZZ
    the multiplicity $c^\nu_{\lambda,\mu}$ of $V(\nu)$ in
    $V(\lambda)\otimes V(\mu)$
Description
  Text
    Returns the multiplicity of $V(\nu)$ in the decomposition computed
    by @TO tensorDecomposition@.  The first call for a pair of highest
    weights computes the full decomposition by Racah--Speiser
    straightening; subsequent calls reuse it, including calls in which
    @TT "lambda"@ and @TT "mu"@ are exchanged.

    In this calculation, a shifted weight on a Weyl wall contributes
    zero.  Otherwise its contribution has the sign of the Weyl element
    carrying it to the dominant chamber.  See @TO tensorDecomposition@
    for the formula.
  Example
    -- sl_3 standard tensor dual: V(1,0) ** V(0,1) = V(1,1) + V(0,0)
    tensorCoefficient(cartanA 3, {1,0}, {0,1}, {1,1})
    tensorCoefficient(cartanA 3, {1,0}, {0,1}, {0,0})
    tensorCoefficient(cartanA 3, {1,0}, {0,1}, {2,0})   -- not a constituent
  Example
    -- so_7 vector tensor vector in partition form: 7 (x) 7 contains
    -- the trivial, the 21-dim (1,1,0), and the 27-dim (2,0,0).
    tensorCoefficient(cartanB 3, {1,0,0}, {1,0,0}, {0,0,0}, WeightFormat => "Partition")
    tensorCoefficient(cartanB 3, {1,0,0}, {1,0,0}, {1,1,0}, WeightFormat => "Partition")
    tensorCoefficient(cartanB 3, {1,0,0}, {1,0,0}, {2,0,0}, WeightFormat => "Partition")
  Example
    -- G_2 adjoint tensor adjoint contains the trivial with multiplicity 1
    tensorCoefficient(cartanG2(), {0,1}, {0,1}, {0,0})
  Text
    The first three answers $1,1,0$ say that $V(1,0)\otimes V(0,1)$ in $A_2$ contains the
    adjoint $V(1,1)$ once and the trivial $V(0,0)$ once, but no $V(2,0)$. Thus $\mathbb
    C^3\otimes(\mathbb C^3)^*\cong\mathfrak{sl}_3\oplus\mathbb C$. The three $B_3$
    answers are also $1$, and the final $G_2$ answer records its unique trivial summand
    in the adjoint tensor square.

    As a consistency check, weighting the keys of
    @TO tensorDecomposition@ by @TO weylDimension@ reproduces
    $\dim V(\lambda)\dim V(\mu)$.  Every corresponding
    @TT "tensorCoefficient"@ call returns the corresponding multiplicity.
SeeAlso
  tensorDecomposition
  weylDimension
  weightsOfRep
  weightMultiplicity
  WeightFormat
  fundamentalToPartition
  partitionToFundamental
///

doc ///
Key
  cartanPositiveRootsDegree
  (cartanPositiveRootsDegree,StandardParabolic,ZZ)
  (cartanPositiveRootsDegree,Matrix,ZZ,List)
  (cartanPositiveRootsDegree,Matrix,ZZ,Set)
Headline
  Positive roots of a given parabolic degree
Usage
  R = cartanPositiveRootsDegree(P, d)
  R = cartanPositiveRootsDegree(A, d, parabolic)
Inputs
  P:StandardParabolic
    a standard parabolic, including its removed-node convention
  A:Matrix
    a finite-type Cartan matrix
  d:ZZ
    a nonnegative integer (the parabolic degree)
  parabolic:List
    in the matrix form, the list or set of simple-root positions
    (1-based) declared to have degree 1; all other simple roots
    have degree 0
Outputs
  R:Matrix
    a matrix whose rows are the positive roots of parabolic degree
    exactly $d$, in the simple-root basis
Description
  Text
    A choice of a subset $S \subseteq \{1,\dots,r\}$ of simple roots
    induces a $\mathbb Z$-grading on the root system in which
    $\alpha_i$ has degree $1$ if $i\in S$ and degree $0$ otherwise.
    This function returns all positive roots whose sum of
    coefficients on the nodes in $S$ equals $d$.

    The degree-$0$ positive roots are the positive roots of the
    Levi subalgebra; the positive-degree roots span the nilradical
    of the standard parabolic.  In @TT "cartanPositiveRootsDegree(P,d)"@,
    $S$ is the set of removed nodes defining @TT "P"@.  In
    @TT "cartanPositiveRootsDegree(A,d,parabolic)"@, the final argument
    specifies $S$ directly.
  Example
    D = cartanDatum("A3",
        NodeLabels => {"left","middle","right"});
    P = standardParabolic(D, RemovedNodes => {"middle"});
    cartanPositiveRootsDegree(P, 0)
    cartanPositiveRootsDegree(P, 1)
    cartanPositiveRootsDegree(P, 2)
  Text
    The degree is the coefficient of the removed middle root.
    Degree $0$ consists of $\alpha_1$ and $\alpha_3$, the positive roots
    of the Levi $A_1\times A_1$.  Degree $1$ consists of
    $\alpha_2$, $\alpha_1+\alpha_2$, $\alpha_2+\alpha_3$, and
    $\alpha_1+\alpha_2+\alpha_3$.  The empty degree-$2$ matrix says
    that no positive root has middle coefficient $2$.

SeeAlso
  cartanPositiveRoots
  leviHighestWeights
  nilradicalRoots
  parabolicDegrees
  leviSubalgebra
///

doc ///
Key
  leviHighestWeights
  (leviHighestWeights,StandardParabolic,ZZ)
  (leviHighestWeights,Matrix,ZZ,List)
  (leviHighestWeights,Matrix,ZZ,Set)
  [leviHighestWeights,WeightFormat]
Headline
  Derived-Levi decomposition of a parabolic graded piece
Usage
  T = leviHighestWeights(P, d)
  T = leviHighestWeights(P, d, WeightFormat => fmt)
  T = leviHighestWeights(A, d, parabolic)
Inputs
  P:StandardParabolic
    the standard parabolic that determines the grading and Levi
  A:Matrix
    a symmetrizable generalized Cartan matrix
  d:ZZ
    a nonnegative parabolic grading degree
  parabolic:List
    a list or set of 1-based positions of the removed simple roots, which are
    assigned degree $1$; repetitions are ignored
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@
Outputs
  T:Tally
    the decomposition of $\mathfrak g_d$ into irreducible modules for the
    derived Levi, with @TO Irrep@ keys and integer multiplicities
Description
  Text
    Let $\mathfrak p=\mathfrak l\oplus\mathfrak n$ be the standard
    parabolic represented by @TT "P"@.  In the matrix form it is
    obtained by removing the simple roots in @TT "parabolic"@.
    Giving those roots degree $1$ and the Levi roots degree $0$ produces
    $$\mathfrak n=\bigoplus_{d\geq 1}\mathfrak g_d.$$
    This function decomposes $\mathfrak g_d$ as a module for the derived
    Levi $[\mathfrak l,\mathfrak l]$.
  Text
    The same function computes these decompositions in finite, affine,
    and indefinite type.  When the ambient algebra is affine or indefinite,
    the finite Levi Cartan matrix determines a sufficient root-height bound
    for the chosen degree.  No height bound needs to be supplied: the
    returned decomposition includes every summand in that degree.
    If the surviving Levi has a nonfinite component, the requested graded
    piece cannot be expressed as a finite-dimensional Levi character and the
    function reports an error.
  Text
    Each @TO Irrep@ key represents a highest-weight summand and its Tally
    value is the multiplicity.  Thus @TT "V(mu) => m"@ means
    $V_L(\mu)^{\oplus m}$.  Central characters are omitted, so different
    full-Levi representations can merge after restriction to
    $[\mathfrak l,\mathfrak l]$.  At degree $0$, the output contains one
    adjoint module for every simple derived component and
    $\dim\mathfrak z(\mathfrak l)$ trivial modules.
  Text
    If the derived Levi is disconnected, @TO Irrep@ stores one tensor factor
    per component and prints @TT "V(...) ** V(...)"@.  Factors follow the
    order returned by @TO leviConnectedComponents@.  With
    @TT "WeightFormat => \"Dynkin\""@, component weights are Dynkin labels.
    With @TT "WeightFormat => \"Partition\""@, every component is converted
    through @TO fundamentalToPartition@ and must have classical type
    $A$, $B$, $C$, or $D$.
  Text
    @BOLD "Reading the node labels."@  A @TO CartanDatum@ allows names
    to be assigned to the nodes.  Use those names to specify the removed
    nodes of @TT "P"@; the remaining nodes determine the Levi.  The
    following calls show their positions in the ambient diagram, numbered
    from $1$:
  Example
    D = cartanDatum("D4",
        NodeLabels => {"arm1","hub","arm2","arm3"});
    P = standardParabolic(D, RemovedNodes => {"hub"});
    (removedNodes P, leviNodes P)
    print parabolicDiagram P
    leviHighestWeights(P, 1)
  Text
    Since node $2$ is trivalent, its removal leaves
    $A_1\times A_1\times A_1$, and the output means
    $$\mathfrak g_1\cong V(1)\otimes V(1)\otimes V(1).$$
  Text
    Multiplicities greater than $1$ can occur.  Removing the two end nodes
    of $A_3$ leaves an $A_1$ Levi and two copies of its standard module:
  Example
    leviHighestWeights(cartanA 4, 1, {1,3})
  Text
    The output @TT "Tally{V(1) => 2}"@ means
    $$\mathfrak g_1\big|_{A_1}\cong V_{A_1}(1)^{\oplus2},$$
    a four-dimensional graded piece.
  Text
    Classical Levi weights can be displayed either in Dynkin labels or in
    partition coordinates.  The following examples retain the conventions
    for Grassmannian and isotropic-Grassmannian parabolics:
  Example
    -- Gr(2,5): the derived Levi is A_1 x A_2.
    leviHighestWeights(cartanA 5, 1, {2})
    leviHighestWeights(cartanA 5, 1, {2}, WeightFormat => "Partition")
    -- For so_10 / P_5, g_1 is exterior^2 of the defining GL_5 module.
    leviHighestWeights(cartanD 5, 1, {5}, WeightFormat => "Partition")
    -- For so_7 / P_3, g_1 is the defining GL_3 module.
    leviHighestWeights(cartanB 3, 1, {3}, WeightFormat => "Partition")
    -- For sp_6 / P_3, g_1 is Sym^2 of the defining GL_3 module.
    leviHighestWeights(cartanC 3, 1, {3}, WeightFormat => "Partition")
    -- Removing the short-root node from B_4 leaves an A_3 Levi.
    leviHighestWeights(cartanB 4, 1, {4}, WeightFormat => "Partition")
  Text
    The same function applies in exceptional finite type.  The maximal
    parabolic at node $1$ of $E_7$ has a @EM "Heisenberg"@ nilradical:
    it is $\mathfrak g_1\oplus\mathfrak g_2$, with a one-dimensional
    center $\mathfrak g_2$ and a nondegenerate alternating bracket
    $\mathfrak g_1\times\mathfrak g_1\longrightarrow\mathfrak g_2$:
  Example
    leviHighestWeights(cartanE 7, 1, {1})
    leviHighestWeights(cartanE 7, 2, {1})
  Text
    Affine and indefinite input require no different function or height
    argument.  For affine $A_1^{(1)}$ the output below is
    $\mathfrak g_1\cong V_{A_1}(2)$, the three-dimensional irreducible:
  Example
    Aaff = matrix{{2,-2},{-2,2}};
    leviHighestWeights(Aaff, 1, {1})
  Text
    For the rank-two hyperbolic matrix, the first two pieces are
    $$\mathfrak g_1\cong V_{A_1}(3),\qquad
      \mathfrak g_2\cong V_{A_1}(4)\oplus V_{A_1}(0).$$
  Example
    Ahyp = matrix{{2,-3},{-3,2}};
    leviHighestWeights(Ahyp, 1, {1})
    leviHighestWeights(Ahyp, 2, {1})
SeeAlso
  leviSubalgebra
  leviConnectedComponents
  parabolicDiagram
  cartanPositiveRootsDegree
  nilradicalRoots
  parabolicDegrees
  kostantLieAlgCohomology
  rootSnapshot
  WeightFormat
///

doc ///
Key
  leviConnectedComponents
  (leviConnectedComponents,StandardParabolic)
  (leviConnectedComponents,Matrix)
Headline
  Connected components of a derived-Levi Dynkin diagram
Usage
  C = leviConnectedComponents(P)
  C = leviConnectedComponents(AL)
Inputs
  P:StandardParabolic
    a standard parabolic in an ambient Cartan datum
  AL:Matrix
    a (generalized) Cartan matrix, typically the Levi Cartan
    submatrix
Outputs
  C:List
    a list of lists of 1-based node positions; each inner list
    enumerates one connected component, sorted in ascending order
Description
  Text
    Each connected component corresponds to a simple factor of the derived
    Levi.  In @TO leviHighestWeights@ and @TO kostantLieAlgCohomology@,
    these components determine the order of the tensor factors and their
    Dynkin labels.  With a
    @TO StandardParabolic@, the returned positions refer to the ambient
    diagram and agree with
    @TT "(leviSubalgebra P)#\"components\""@.  With a Levi matrix
    @TT "AL"@, they are local positions in that matrix.  In either form
    positions start at $1$; subtract $1$ explicitly for matrix indices.
  Example
    D = cartanDatum("A4",
        NodeLabels => {"one","two","three","four"});
    P = standardParabolic(D, RemovedNodes => {"three"});
    leviConnectedComponents P
    (leviSubalgebra P)#"components"
  Text
    Both calls return @TT "{{1,2},{4}}"@: the Levi has type
    $A_2\times A_1$ on ambient nodes $\{1,2\}$ and $\{4\}$.
SeeAlso
  leviSubalgebra
  leviHighestWeights
///

doc ///
Key
  WeightFormat
Headline
  Choose Dynkin labels or partition coordinates for weights
Description
  Text
    This option chooses the coordinates for both the weights supplied to
    a function and the weights in its output.  The two coordinate systems
    are related by @TO partitionToFundamental@ and
    @TO fundamentalToPartition@.  The choices are:
  Text
    @UL {
      (TT "\"Dynkin\"", " (default): each weight is given in Dynkin labels (coordinates in the basis of fundamental weights).  Works for any Cartan matrix."),
      (TT "\"Partition\"", ": use classical $\\varepsilon$-coordinates.  A dominant highest-weight input must satisfy the documented weak-decrease and sign conditions, so it is literally a partition (or a half-integral spin partition).  Outputs representing arbitrary, nondominant weights use the same coordinates but need not be partitions.  Input highest weights are converted via ", TO partitionToFundamental, "; outputs via ", TO fundamentalToPartition, ".  Requires connected classical type $A$, $B$, $C$, or $D$.")
    }@
  Text
    The name @TT "Partition"@ refers to this coordinate system; weights
    in an orbit or character need not be weakly decreasing.  The pages
    for the coordinate conversions state the
    precise admissibility conditions for types $A$, $B$, $C$, and $D$.
  Text
    Functions that accept this option include @TO weylDimension@,
    @TO weightsOfRep@, @TO weightMultiplicity@,
    @TO weightMultiplicityTable@,
    @TO dominantWeightsInRep@, @TO weightOrbit@, @TO isDominant@,
    @TO tensorDecomposition@, @TO kostantLieAlgCohomology@,
    @TO leviHighestWeights@,
    @TO irrepData@,
    @TO irrepDimension@, @TO decompositionDimension@, and
    @TO cohomologyDimensions@.
  Text
    The two choices express the same decomposition in different
    coordinates.  For the Grassmannian
    $\mathrm{Gr}(2,5)$ (i.e.\ $A_4$ with parabolic $\{2\}$), the
    tangent space restricts to the single derived-Levi irreducible
    $V(1)\otimes V(0,1)$ (Dynkin), displayed as
    $V(1,0)\otimes V(1,1,0)$ in partition coordinates:
  Example
    leviHighestWeights(cartanA 5, 1, {2})
    leviHighestWeights(cartanA 5, 1, {2}, WeightFormat => "Partition")
  Text
    For example, in type $B_3$, the
    adjoint of $\mathfrak{so}_7$ has Dynkin label $(0,1,0)$ (with
    dimension $21$); in partition form that is $(1,1,0)$.  Both
    calls produce $21$:
  Example
    weylDimension(cartanB 3, {0,1,0})
    weylDimension(cartanB 3, {1,1,0}, WeightFormat => "Partition")
  Text
    Half-integer partitions appear naturally in type $B$ and $D$ spin
    representations.  The spin representation of $\mathfrak{so}_7$ is
    Dynkin $(0,0,1)$, i.e.\ partition $(1/2,1/2,1/2)$:
  Example
    weylDimension(cartanB 3, {0,0,1})
    weylDimension(cartanB 3, {1/2,1/2,1/2}, WeightFormat => "Partition")
  Text
    The same option works in types $B$, $C$, $D$ for Levi highest
    weights.  For example, the isotropic Grassmannian
    $\mathrm{OG}(5,10)$ (i.e.\ $D_5$ with parabolic $\{5\}$) has
    tangent space $\wedge^2\mathbb C^5$, which appears in partition
    form as the single partition $(1,1,0,0,0)$:
  Example
    leviHighestWeights(cartanD 5, 1, {5}, WeightFormat => "Partition")
SeeAlso
  weylDimension
  weightsOfRep
  weightMultiplicity
  dominantWeightsInRep
  weightOrbit
  isDominant
  tensorDecomposition
  leviHighestWeights
  kostantLieAlgCohomology
  fundamentalToPartition
  partitionToFundamental
///

doc ///
Key
  Irrep
Headline
  Highest-weight irreducible representation (or tensor product of such)
Description
  Text
    An @TT "Irrep"@ represents a highest-weight irreducible representation
    of a finite-dimensional semisimple or symmetrizable Kac--Moody Lie
    algebra, or a tensor product of such (one factor per connected
    derived component).  In Levi decompositions it specifies the
    representation of the derived Levi, without its central character.  For an
    affine or indefinite Levi, a dominant Dynkin-label list denotes the
    corresponding integrable highest-weight module, which is generally
    infinite-dimensional.

    To specify $V(a,b,c)$, use @TT "new Irrep from {{a,b,c}}"@;
    to specify $V(a,b)\otimes V(c)$ for two components, use
    @TT "new Irrep from {{a,b},{c}}"@.  Each inner list gives the
    highest weight of one factor, in the coordinates selected by
    @TO WeightFormat@.

    These representations are displayed as @TT "V(a,b,c)"@ and
    @TT "V(a,b) ** V(c)"@, respectively.
  Example
    w = new Irrep from {{1,0}}
    toString w
    wt = new Irrep from {{1,0},{0,1,2}}
    toString wt
    leviHighestWeights(cartanA 5, 1, {2})
  Text
    The first object is one irreducible for a simple factor.  The
    second is also one irreducible: @TT "**"@ denotes the external
    tensor product
    $$V(1,0)\otimes V(0,1,2)$$
    for two simple factors, not a direct sum.  Direct sums appear at
    the Tally level; in @TT "Tally{V => m}"@, @TT "m"@ is the number
    of copies of $V$.
    An @TT "Irrep"@ by itself gives only highest weights.  Its Cartan
    matrix, component types, coordinate convention, and central character
    must be specified separately.  Use @TO irrepData@ to record the Cartan
    matrix and coordinate convention together with the representation, or
    use @TO IncludeCentralCharacter@ in Kostant cohomology to include the
    action of the Levi center.  @TO irrepDimension@ requires a finite-type
    Cartan matrix.
SeeAlso
  LeviIrrep
  irrepData
  IncludeCentralCharacter
  leviHighestWeights
  kostantLieAlgCohomology
  WeightFormat
///

doc ///
Key
  ImaginaryMultiplicity
Headline
  Option for the imaginary-root multiplicity in affine types
Description
  Text
    An option symbol used by @TO affineRootSnapshot@.  With the default
    @TT "null"@,
    exact imaginary-root multiplicities are computed automatically.
    In untwisted affine type every $k\delta$ has multiplicity $r-1$,
    where $r=\mathrm{numrows}\,A$.  A supplied positive integer is
    accepted only as an assertion of that exact value; a conflicting
    value raises an error.  In twisted affine type the multiplicity
    can depend periodically on $k$, is computed by Peterson's
    recursion, and no constant override is accepted.
  Text
    In untwisted $\widehat{\mathfrak{sl}}_2$ (rank $2$ Cartan) the
    imaginary multiplicity equals $1$ by default, matching $\dim
    \mathfrak h = 1$ of $\mathfrak{sl}_2$:
  Example
    A = matrix{{2,-2},{-2,2}};
    (affineRootSnapshot(A, 4))#"mult"
  Text
    The following explicit value is therefore a consistency check,
    not a replacement rule:
  Example
    affineRootSnapshot(A, 6, ImaginaryMultiplicity => 1)
SeeAlso
  affineRootSnapshot
///

doc ///
Key
  leviSubalgebra
  (leviSubalgebra,Matrix,List)
  (leviSubalgebra,Matrix,Set)
Headline
  The Levi factor of a standard parabolic
Usage
  H = leviSubalgebra(A, parabolic)
  H = leviSubalgebra P
Inputs
  A:Matrix
    a Cartan matrix
  parabolic:List
    simple-root indices (1-based) declared of degree 1; a @TO Set@ is also accepted
  P:StandardParabolic
    alternatively, a standard parabolic with its removed nodes already specified
Outputs
  H:LeviData
    the Cartan matrix, nodes, and embedding of the Levi factor
Description
  Text
    First print the ambient diagram: the displayed 1-based labels are
    exactly the labels accepted in @TT "parabolic"@, and they denote
    removed nodes.  The complementary nodes form the Levi diagram.
  Example
    A = cartanD 4;
    print dynkinDiagram A
    L = leviSubalgebra(A, {2});
    print dynkinDiagram (L#"cartanMatrix")
  Text
    The @TO LeviData@ output gives the following information:
  Text
    @UL {
      (TT "\"cartanMatrix\"", " => the Cartan submatrix of the Levi"),
      (TT "\"nodes\"", " => list of 1-based Levi node indices in the ambient algebra"),
      (TT "\"nodes0\"", " => the same list, 0-indexed"),
      (TT "\"removedNodes\"", " and ", TT "\"parabolic\"", " => the removed degree-one nodes"),
      (TT "\"embedding\"", " => the matrix embedding Levi simple-root coordinates into ambient simple-root coordinates"),
      (TT "\"components\"", " => connected components as ambient 1-based labels"),
      (TT "\"semisimpleRank\"", " => the number of Levi simple nodes"),
      (TT "\"centerDimension\"", " => the dimension of the center in the chosen minimal realization"),
      (TT "\"centralCharactersKept\"", " => false; decomposition outputs forget central characters")
    }@
  Example
    L := leviSubalgebra(cartanA 4, {2});
    L#"cartanMatrix"
    L#"nodes"
    L#"rank"
    L#"embedding"
    L#"components"
  Text
    Removing node $2$ from $A_3$ leaves ambient nodes $\{1,3\}$.  The
    diagonal Cartan submatrix has two $A_1$ components, so this output
    says
    $$[\mathfrak l,\mathfrak l]\cong
      \mathfrak{sl}_2\oplus\mathfrak{sl}_2.$$
SeeAlso
  leviHighestWeights
  nilradicalRoots
  parabolicDegrees
///

doc ///
Key
  parabolicDiagram
  (parabolicDiagram,StandardParabolic)
  (parabolicDiagram,Matrix,List)
  (parabolicDiagram,Matrix,Set)
Headline
  Show a Dynkin diagram together with removed and Levi node roles
Usage
  s = parabolicDiagram(P)
  s = parabolicDiagram(A, parabolic)
Inputs
  P:StandardParabolic
    a standard parabolic with removed and Levi nodes already recorded
  A:Matrix
    a generalized Cartan matrix
  parabolic:List
    1-based removed nodes, equivalently the degree-one simple roots;
    a @TO Set@ is accepted as well
Outputs
  s:String
    a text rendering that lists every node's role before the diagram
Description
  Text
    The diagram is drawn as in @TO dynkinDiagram@, with an additional
    list specifying which nodes are removed and which belong to the Levi.
    When @TT "P"@ is supplied, these lists come from the chosen standard
    parabolic.  In the matrix form, specify the removed-node positions,
    numbered from $1$.
  Example
    D = cartanDatum("D4",
        NodeLabels => {"arm1","hub","arm2","arm3"});
    P = standardParabolic(D, RemovedNodes => {"hub"});
    print parabolicDiagram P
  Text
    The text says that node $2$ is removed and nodes $1,3,4$ are Levi
    nodes.  Hence the derived Levi has type $A_1^3$.
SeeAlso
  parabolicData
  dynkinDiagram
  leviSubalgebra
///

doc ///
Key
  parabolicData
  (parabolicData,StandardParabolic)
  (parabolicData,Matrix,List)
  (parabolicData,Matrix,Set)
Headline
  Cartan data and grading for a standard parabolic
Usage
  H = parabolicData(P)
  H = parabolicData(A, parabolic)
Inputs
  P:StandardParabolic
    a standard parabolic, including its ambient and Levi Cartan data
  A:Matrix
    a generalized Cartan matrix
  parabolic:List
    1-based removed nodes; a @TO Set@ is accepted as well
Outputs
  H:HashTable
    ambient, Levi, grading, diagram, and finite nilradical data
Description
  Text
    The record includes @TT "removedNodes"@, @TT "leviNodes"@,
    @TT "gradingVector"@, @TT "diagram"@, and a nested @TO LeviData@
    under @TT "levi"@.  In finite type it also includes the matrix
    @TT "nilradicalRoots"@ and its row count
    @TT "nilradicalDimension"@.  In affine or indefinite type those
    two fields are @TT "null"@; use @TO rootSnapshot@ with a height
    bound to examine roots in these types.  In the
    @TO StandardParabolic@ form, @TT "parabolicObject"@ retains
    @TT "P"@, while @TT "cartanDatum"@ and
    @TT "leviCartanDatum"@ give the ambient and Levi Cartan data.
  Example
    D = cartanDatum("A4",
        NodeLabels => {"one","cut","three","four"});
    P = standardParabolic(D, RemovedNodes => {"cut"});
    H = parabolicData P;
    (H#"removedNodes", H#"leviNodes", H#"gradingVector")
    H#"nilradicalDimension"
  Text
    Removing node $2$ of $A_4$ gives the Grassmannian parabolic.  The
    returned dimension is $2(5-2)=6$, the dimension of its abelian
    nilradical.
SeeAlso
  parabolicDiagram
  leviSubalgebra
  nilradicalRoots
  rootSnapshot
///

doc ///
Key
  nilradicalRoots
  (nilradicalRoots,StandardParabolic)
  (nilradicalRoots,Matrix,List)
  (nilradicalRoots,Matrix,Set)
Headline
  Positive roots lying in the nilradical of a standard parabolic
Usage
  R = nilradicalRoots(P)
  R = nilradicalRoots(A, parabolic)
Inputs
  P:StandardParabolic
    a finite-type standard parabolic
  A:Matrix
    a finite-type Cartan matrix
  parabolic:List
    simple-root indices (1-based) declared of degree 1
Outputs
  R:Matrix
    a matrix whose rows are the positive roots of strictly positive
    parabolic degree, expressed in the simple-root basis
Description
  Text
    These are the positive roots appearing in the nilradical
    $\mathfrak n$ of the standard parabolic $\mathfrak p =
    \mathfrak l \oplus \mathfrak n$.  Equivalently, the positive
    roots whose expansion in simple roots is nonzero on at least
    one removed node.  When @TT "P"@ is supplied, its removed nodes
    determine this condition.  The matrix/list form interprets @TT "parabolic"@
    as 1-based removed-node positions.
  Example
    -- sl_5 parabolic P_2: nilradical = Hom(C^2, C^3), dim 6
    D = cartanDatum("A4",
        NodeLabels => {"one","cut","three","four"});
    P = standardParabolic(D, RemovedNodes => {"cut"});
    nilradicalRoots P
    numrows nilradicalRoots P
  Text
    Each row is a root, not a Dynkin-label weight.  For example,
    @TT "{1,1,1,0}"@ means
    $\alpha_1+\alpha_2+\alpha_3$.  All six rows have positive
    coefficient on the removed root $\alpha_2$; since finite-type root
    spaces are one-dimensional, the six rows give $\dim\mathfrak n=6$.

    The count is easy to verify against the Grassmannian formula:
    $\dim T_{[\mathrm{pt}]}\mathrm{Gr}(k,n) = k(n-k)$.  For
    $\mathrm{Gr}(2,5)$ that is $2\cdot 3 = 6$, matching the row
    count above; for $\mathrm{Gr}(3,7)$ it is $12$:
  Example
    numrows nilradicalRoots(cartanA 7, {3})
  Text
    The total count is also visible as the total number of
    positive-degree pieces reported by @TO parabolicDegrees@:
  Example
    #select(parabolicDegrees(cartanE 6, {1}), p -> p#1 > 0)
    numrows nilradicalRoots(cartanE 6, {1})
  Text
    Multiple removed nodes give larger nilradicals.  The Borel case
    (all nodes removed) recovers every positive root:
  Example
    numrows nilradicalRoots(cartanD 4, toList(1..4))
    numrows cartanPositiveRoots cartanD 4
SeeAlso
  cartanPositiveRoots
  cartanPositiveRootsDegree
  parabolicDegrees
  leviSubalgebra
///

doc ///
Key
  parabolicDegrees
  (parabolicDegrees,StandardParabolic)
  (parabolicDegrees,Matrix,List)
  (parabolicDegrees,Matrix,Set)
Headline
  Parabolic degree of each positive root
Usage
  L = parabolicDegrees(P)
  L = parabolicDegrees(A, parabolic)
Inputs
  P:StandardParabolic
    a finite-type standard parabolic
  A:Matrix
    a finite-type Cartan matrix
  parabolic:List
    simple-root indices (1-based) declared of degree 1
Outputs
  L:List
    a list of pairs @TT "{root, degree}"@, one for each positive
    root, where @TT "root"@ is the root expressed in the
    simple-root basis and @TT "degree"@ is the sum of its
    coefficients on the @TT "parabolic"@ nodes
Description
  Text
    Declaring the removed simple roots to have degree $1$
    (and all others degree $0$) induces a $\mathbb Z$-grading on
    $\mathfrak g = \bigoplus_{d\in\mathbb Z} \mathfrak g_d$; the
    positive-degree part is the nilradical $\mathfrak n$ of the
    associated parabolic, and each $\mathfrak g_d$ for $d \geq 1$ is a
    representation of the Levi.  @TT "parabolicDegrees"@ returns the
    degree of every positive root, so one can read off the graded
    pieces directly.  The @TO StandardParabolic@ form reads this choice
    from @TT "P"@.  In the matrix form, @TT "parabolic"@ is the list
    or set of 1-based removed-node positions.
  Example
    D = cartanDatum("A2", NodeLabels => {"left","right"});
    P = standardParabolic(D, RemovedNodes => {"right"});
    parabolicDegrees P
  Text
    The output is
    $\{\{\alpha_1,0\},\{\alpha_2,1\},
      \{\alpha_1+\alpha_2,1\}\}$ in mathematical notation.  Thus the
    first root belongs to the Levi and the other two root spaces form
    the positive degree-$1$ piece.

    As a larger example, take $E_7$ with parabolic $P_1$ (removing the
    extremal node).  The resulting grading is contact: $\mathfrak g_1$
    is the $32$-dimensional half-spin of the Levi $D_6$, $\mathfrak g_2$
    is the one-dimensional center of the nilradical, and in particular
    only degrees $\{-2, -1, 0, 1, 2\}$ appear.
  Example
    D := parabolicDegrees(cartanE 7, {1});
    tally (D / last)
    -- 32 = dim g_1, 1 = dim g_2, matching the Heisenberg-type grading
  Text
    For the parabolic $P_8$ in $E_8$, the
    coefficient of $\alpha_8$ in the highest root is $2$, so it gives a
    depth-$2$ grading $\mathfrak g_{-2}\oplus\cdots\oplus\mathfrak
    g_0\oplus\cdots\oplus\mathfrak g_2$ on the $248$-dimensional
    adjoint.  Summing the graded dimensions reproduces $|\Phi^+|$:
  Example
    D8 := parabolicDegrees(cartanE 8, {8});
    tally (D8 / last)
    #D8 == numrows cartanPositiveRoots cartanE 8
SeeAlso
  cartanPositiveRootsDegree
  nilradicalRoots
  leviHighestWeights
  leviSubalgebra
///

doc ///
Key
  parabolicCosetReps
  (parabolicCosetReps,StandardParabolic,ZZ)
  (parabolicCosetReps,Matrix,ZZ,List)
  (parabolicCosetReps,Matrix,ZZ,Set)
  [parabolicCosetReps,MaxElements]
Headline
  Minimal length coset representatives of $W/W_L$
Usage
  reps = parabolicCosetReps(P, k)
  reps = parabolicCosetReps(A, k, I)
Inputs
  P:StandardParabolic
    a standard parabolic; its stored Levi nodes generate $W_L$
  A:Matrix
    a generalized Cartan matrix of finite rank
  k:ZZ
    a nonnegative integer (the length)
  I:List
    simple-root indices (1-based) of the @EM "Levi"@ (note the
    convention difference from @TO leviHighestWeights@ and
    @TO nilradicalRoots@, which take the @EM "removed"@ nodes); a @TO Set@ is
    accepted as well
  MaxElements=>ZZ
    maximum number of coset representatives to retain during the calculation;
    default $10000$, or @TT "null"@ for no limit
Outputs
  reps:List
    list of words (each a list of 1-based simple-reflection
    indices) representing the distinct length-$k$
    right-minimal representatives of $W/W_L$, where $W_L$ is the parabolic
    subgroup generated by $\{s_i : i\in I\}$
Description
  Text
    The representatives are found in order of increasing length, without
    listing the full Weyl group.  The calculation stops if the number of
    representatives retained exceeds @TO MaxElements@.  The same function
    applies in finite, affine, and indefinite type: a finitely generated
    Coxeter group has only finitely many elements of any given length.
  Text
    When @TT "P"@ is supplied, its @TO leviNodes@ determine $W_L$.
    Its complementary @TO removedNodes@ determine the grading and
    nilradical.  Thus the same standard parabolic can be used throughout
    a calculation without taking complements by hand.
  Text
    A word $(i_1,\dots,i_k)$ represents the matrix product
    $s_{i_1}\cdots s_{i_k}$, so under the usual left action the
    rightmost reflection acts first.  The elements returned are the
    unique minimal-length representatives $v$ of the cosets
    $v W_L$; equivalently, they have no right descents in $I$, or
    $v(\alpha_i)\in\Phi^+$ for every $i\in I$.

    In the matrix/list form, @TT "I"@ is the list of
    @EM "Levi"@ (= degree-0) simple roots.  In
    @TO leviHighestWeights@, @TO nilradicalRoots@,
    @TO parabolicDegrees@, @TO leviSubalgebra@, and
    @TO kostantLieAlgCohomology@ the analogous argument
    @TT "parabolic"@ denotes the @EM "removed"@ (= degree-1)
    simple roots, i.e.\ the complement.  Supplying a
    @TO StandardParabolic@ determines both sets from the same parabolic.
  Example
    -- W(A_3) / W_{A_2 on {2,3}} has 4 minimal reps, at lengths 0,1,2,3
    D = cartanDatum("A3",
        NodeLabels => {"projective","middle","right"});
    P = standardParabolic(D, RemovedNodes => {"projective"});
    parabolicCosetReps(P, 0)
    parabolicCosetReps(P, 1)
    parabolicCosetReps(P, 2)
    parabolicCosetReps(P, 3)
  Example
    -- In affine A_1^(1), every requested finite slice is still exact.
    Aaff = matrix{{2,-2},{-2,2}};
    parabolicCosetReps(Aaff, 4, {2})
  Text
    There is one minimal representative in each of lengths $0,1,2,3$.
    Thus the four results account for all four cosets for the indicated
    $A_2$ Levi in $W(A_3)$.  In affine $A_1^{(1)}$ the length-$4$
    output is a single representative; it is an exact slice of an
    infinite set of parabolic representatives, not the whole set.

SeeAlso
  dotActionOnWeight
  parabolicDotImages
  kostantLieAlgCohomology
  weylLength
///

doc ///
Key
  dotActionOnWeight
Headline
  Dot action $w\cdot\lambda = w(\lambda+\rho)-\rho$
Usage
  mu = dotActionOnWeight(A, word, lambda)
Inputs
  A:Matrix
    a generalized Cartan matrix of finite rank
  word:List
    a list of 1-based simple-reflection indices representing
    $w = s_{i_1}\cdots s_{i_k}$
  lambda:List
    exactly rank-many integral Dynkin labels (with respect to the
    fundamental weights) of a weight
Outputs
  mu:List
    Dynkin labels of $w\cdot\lambda$
Description
  Text
    Applies the shifted Weyl action $w\cdot\lambda =
    w(\lambda+\rho) - \rho$, where $\rho$ is the half-sum of
    positive roots in finite type and, uniformly in any type, is
    characterized by $\langle\rho,\alpha_i^\vee\rangle=1$.
    The computation works entirely in Dynkin coordinates, so it remains
    valid when the Cartan matrix is singular.
  Example
    -- s_1 . 0 in A_2 is -2 omega_1 + omega_2
    dotActionOnWeight(cartanA 3, {1}, {0,0})
    -- longest element of A_2 sends 0 to -2 omega_1 - 2 omega_2
    dotActionOnWeight(cartanA 3, {1,2,1}, {0,0})
  Text
    The outputs $(-2,1)$ and $(-2,-2)$ are the Dynkin labels
    of $s_1(\rho)-\rho$ and $w_0(\rho)-\rho$.  They illustrate the
    shifted action $w\mathbin{\cdot}\lambda=w(\lambda+\rho)-\rho$,
    not the ordinary Weyl action.

SeeAlso
  parabolicCosetReps
  parabolicDotImages
  kostantLieAlgCohomology
  weylVector
///

doc ///
Key
  parabolicDotImages
  (parabolicDotImages,StandardParabolic,ZZ,List)
  (parabolicDotImages,Matrix,ZZ,List,List)
  (parabolicDotImages,Matrix,ZZ,Set,List)
  [parabolicDotImages,MaxElements]
Headline
  Dot-action images of a weight under minimal coset representatives
Usage
  L = parabolicDotImages(P, k, lambda)
  L = parabolicDotImages(P, k, lambda, MaxElements => N)
  L = parabolicDotImages(A, k, I, lambda)
Inputs
  P:StandardParabolic
    a standard parabolic; its stored Levi nodes determine the quotient
  A:Matrix
    a generalized Cartan matrix of finite rank
  k:ZZ
    a nonnegative integer (the length)
  I:List
    simple-root indices (1-based) of the Levi (same convention as
    @TO parabolicCosetReps@); a @TO Set@ is accepted as well
  lambda:List
    Dynkin labels of the starting weight
  MaxElements=>ZZ
    maximum number of coset representatives to retain during the calculation;
    default $10000$, or @TT "null"@ for no limit
Outputs
  L:List
    a list of pairs @TT "{vInverse, vInverse . lambda}"@, one for each
    right-minimal length-$k$ representative $v$ of $W/W_L$
Description
  Text
    The first entry of each pair is the (reversed) word giving the
    Weyl group element, and the second is its dot-action image
    applied to @TT "lambda"@.  These are the weights that occur in
    @TO kostantLieAlgCohomology@.  As in @TO parabolicCosetReps@, the
    calculation stops if the number of retained coset representatives
    exceeds @TO MaxElements@.  When @TT "P"@ is supplied, its
    @TO leviNodes@ determine the quotient.  In the matrix form, @TT "I"@
    must be the 1-based Levi node set.
  Example
    -- Gr(1,4) = P^3: Levi L = A_2 on {2,3}, lambda = 0
    D = cartanDatum("A3",
        NodeLabels => {"projective","middle","right"});
    P = standardParabolic(D, RemovedNodes => {"projective"});
    parabolicDotImages(P, 0, {0,0,0})
    parabolicDotImages(P, 1, {0,0,0})
    parabolicDotImages(P, 2, {0,0,0})
    parabolicDotImages(P, 3, {0,0,0})
  Text
    If $v$ is the right-minimal representative returned by
    @TO parabolicCosetReps@, this function displays $v^{-1}$ and
    $v^{-1}\!\cdot\lambda$.  These are precisely the weights in the
    $k$-th row of the Kac--Moody Kostant formula.  To see a nonzero
    starting weight, take $\lambda = \omega_1$, the highest weight of
    the standard representation of $\mathfrak{sl}_5$, with
    Levi $L = A_1\times A_2$ on $\{1\}$ and $\{3,4\}$:
  Example
    parabolicDotImages(cartanA 5, 1, {1,3,4}, {1,0,0,0})
    parabolicDotImages(cartanA 5, 2, {1,3,4}, {1,0,0,0})
  Text
    In every length slice the number of pairs matches the number of
    length-$k$ minimal coset representatives returned by @TO parabolicCosetReps@.
    When $W/W_L$ is finite, the union across all lengths has size
    $|W|/|W_L|$; in infinite type the individual requested slice is
    still complete.
SeeAlso
  parabolicCosetReps
  dotActionOnWeight
  kostantLieAlgCohomology
///

doc ///
Key
  kostantLieAlgCohomology
  (kostantLieAlgCohomology,StandardParabolic,List)
  (kostantLieAlgCohomology,Matrix,List,List)
  (kostantLieAlgCohomology,Matrix,Set,List)
  [kostantLieAlgCohomology,WeightFormat]
  [kostantLieAlgCohomology,MaxElements]
  [kostantLieAlgCohomology,MaxDegree]
  [kostantLieAlgCohomology,IncludeCentralCharacter]
Headline
  The Kac--Moody Kostant formula for a parabolic nilradical
Usage
  H = kostantLieAlgCohomology(P, lambda)
  H = kostantLieAlgCohomology(P, lambda, MaxDegree => d)
  H = kostantLieAlgCohomology(A, parabolic, lambda)
Inputs
  P:StandardParabolic
    the standard parabolic whose nilradical cohomology is computed
  A:Matrix
    a symmetrizable generalized Cartan matrix
  parabolic:List
    the @EM "removed"@ simple-root indices (1-based); the Levi is
    the complement; a @TO Set@ is accepted as well
  lambda:List
    a dominant integral weight $\lambda$ for the ambient algebra
    $\mathfrak g$, in the format selected by @TT "WeightFormat"@
  WeightFormat=>String
    @TT "\"Dynkin\""@ (default) or @TT "\"Partition\""@; controls
    @EM "both"@ the input format of @TT "lambda"@ and the output
    format of each Levi highest weight (see @TO WeightFormat@)
  MaxElements=>ZZ
    maximum number of minimal coset representatives to retain;
    default $10000$, or @TT "null"@ for no limit
  MaxDegree=>ZZ
    optional nonnegative cohomological degree bound.  It is required
    exactly when $W/W_L$ is infinite; when supplied, the result is
    complete through that degree
  IncludeCentralCharacter=>Boolean
    false by default; when true, return @TO LeviIrrep@
    keys retaining full Levi central characters; this option requires a
    nonsingular ambient Cartan matrix
Outputs
  H:GradedDecomposition
    a table indexed by the cohomological degree $k$ (only
    nonempty degrees appear).  Each value is a @TO Tally@ whose keys
    are @TO Irrep@ objects by default, or @TO LeviIrrep@ objects when
    @TT "IncludeCentralCharacter => true"@
Description
  Text
    Let $I$ be the Levi node set stored in @TT "P"@ (or the complement
    of @TT "parabolic"@ in the matrix form), and let
    $$W^I_{\rm r}=\{v\in W:v(\alpha_i)\in\Phi^+
      \text{ for all }i\in I\}$$
    be the right-minimal representatives of $W/W_I$.
    For every symmetrizable Kac--Moody algebra, the function computes
    the degreewise formula
    $$H^k_{\rm res}(\mathfrak n_I^+,L(\lambda))
       \;\cong\!
       \bigoplus_{\substack{v\in W^I_{\rm r}\\\ell(v)=k}}
       L_I(v^{-1}\!\cdot\lambda),$$
    where $v\cdot\lambda=v(\lambda+\rho)-\rho$.  The inverse is
    important: @TO parabolicCosetReps@ uses right-minimal
    representatives.  The restriction of $v^{-1}\!\cdot\lambda$ to
    the Levi nodes is automatically dominant, so every theorem summand
    appears in the result.
  Text
    This is equivalently the opposite-nilradical homology decomposition
    $$H_k(\overline{\mathfrak n}_I^-,L(\lambda))
      \cong\bigoplus_{v\in W^I_{\rm r},\,\ell(v)=k}
      L_I(v^{-1}\!\cdot\lambda)$$
    of Kumar, Theorem 3.2.7.  In finite type this agrees, under the
    standard Chevalley/contragredient identification, with ordinary
    Chevalley--Eilenberg cohomology.  In affine or indefinite type,
    @TT "H"@ records restricted, weight-graded cohomology.  It does not
    claim to represent the unrestricted product of all algebraic-dual
    weight spaces.
  Text
    A fixed length stratum of a finite-rank Coxeter group is finite.
    Consequently @TT "MaxDegree => d"@ computes the entire answer in
    degrees $0,\ldots,d$ even when $W/W_I$ is infinite.  Without
    @TO MaxDegree@, the function
    first decides whether the quotient is finite.  It returns every
    degree when it is finite and otherwise stops with an
    instruction to supply a bound.  A quotient inside an infinite
    ambient Weyl group is finite precisely when every infinite-type
    connected component of the Dynkin diagram is contained entirely in
    the Levi.  Equivalently, every removed node belongs to a finite-type
    connected component.
  Text
    The full-Levi theorem is multiplicity-free.  By default,
    the output is organized by cohomological degree and then restricted
    to $[\mathfrak l,\mathfrak l]$; the central character is not
    included, so distinct full-Levi summands may combine into one Tally
    key.  For nonsingular @TT "A"@, set
    @TT "IncludeCentralCharacter => true"@ to retain those characters
    and the ambient dot-action weight in each
    @TO LeviIrrep@ key.  See @TO IncludeCentralCharacter@ for why
    this option requires a nonsingular ambient matrix.
    Each Tally
    value is keyed by @TO Irrep@ objects, which print as
    @TT "V(...)"@ when the Levi is connected and as
    @TT "V(...) ** V(...)"@ when the Levi splits into several
    connected components (one tensor factor per component, in the
    order returned by @TO leviConnectedComponents@).  For an
    infinite-type Levi, each @TO Irrep@ is a symbolic integrable
    highest-weight module and may be infinite-dimensional.  Setting
    @TT "WeightFormat => \"Partition\""@ is supported only when the
    ambient conversion and every Levi component are of finite classical
    type $A$, $B$, $C$, or $D$; use Dynkin labels otherwise.

    When a @TO StandardParabolic@ is supplied, its removed nodes
    determine the nilradical and its complementary Levi nodes determine
    $W_I$.  In the matrix form,
    @TT "parabolic"@ is the list of @EM "removed"@ simple roots
    (those in degree $1$), whereas the @TT "I"@ argument of
    @TO parabolicCosetReps@ and @TO parabolicDotImages@ is the
    @EM "Levi"@ node set.
  Text
    The representatives are found in order of increasing length, so their
    lengths give the cohomological degrees directly.  The calculation
    stops if the number of representatives retained exceeds @TO MaxElements@.
  Example
    -- A_1^(1), with node 1 removed and the finite A_1 Levi at node 2.
    Daff = cartanDatum(matrix{{2,-2},{-2,2}},
        NodeLabels => {"affine","finite"});
    Paff = standardParabolic(Daff, RemovedNodes => {"affine"});
    print parabolicDiagram Paff
    kostantLieAlgCohomology(Paff, {0,0}, MaxDegree => 4)
  Text
    Reading the Dynkin label in each row gives the exact statement
    $$H^k_{\rm res}\cong V_{A_1}(2k),\qquad 0\leq k\leq4.$$
    Omitting @TO MaxDegree@ here raises an error because the quotient
    has one representative in every degree.
    The @TO "Kostant homology and restricted cohomology"@ example
    explains this answer using the current Lie algebra
    $t\mathfrak{sl}_2[t]$, explicit homology classes, and the
    nonperiodic free resolution of its trivial module.
  Example
    -- The Levi on nodes {1,2} is itself affine A_1^(1).
    AinfLevi = matrix{{2,-2,0},{-2,2,-1},{0,-1,2}};
    print dynkinDiagram AinfLevi
    kostantLieAlgCohomology(AinfLevi, {3}, {0,0,0}, MaxDegree => 2)
  Text
    The degree-$0$, $1$, and $2$ modules have affine-Levi Dynkin labels
    $(0,0)$, $(0,1)$, and $(2,0)$.  The degree-$0$ module is trivial
    and one-dimensional; the positive-level modules in degrees $1$ and
    $2$ are infinite-dimensional.  An @TO Irrep@ gives the highest
    weight without its Cartan matrix: use
    @TT "L = leviSubalgebra(AinfLevi,{3})"@ followed by
    @TT "dynkinDiagram(L#\"cartanMatrix\")"@ to recover the diagram
    against which these labels are read.
  Example
    -- sl_4, maximal parabolic at node 1 (Grassmannian Gr(1,4) = P^3),
    -- trivial representation:  |W/W_L| = 24/6 = 4
    kostantLieAlgCohomology(cartanA 4, {1}, {0,0,0})
  Example
    -- Same, but displaying Levi weights as partitions of sl_3.
    -- The input lambda = (0,0,0) is the trivial weight, identical
    -- in both conventions.
    kostantLieAlgCohomology(cartanA 4, {1}, {0,0,0}, WeightFormat => "Partition")
  Text
    Read the result degree by degree:
    $$H^0(\mathfrak n,\mathbb C)\cong\mathbb C,\qquad
      H^1\cong\mathbb C^3,\qquad
      H^2\cong\bigwedge^2\mathbb C^3,\qquad
      H^3\cong\mathbb C.$$
    These are the representations printed as $V(0,0,0)$,
    $V(1,0,0)$, $V(1,1,0)$, and $V(0,0,0)$, respectively.
  Example
    Hfull = kostantLieAlgCohomology(cartanA 4, {1}, {0,0,0},
        IncludeCentralCharacter => true);
    V1 = first keys Hfull#1;
    (V1#"semisimple", V1#"centralCharacter",
     V1#"ambientWeight", V1#"removedNodes")
  Text
    The semisimple factor still identifies the $A_2$ module in $H^1$,
    while @TT "centralCharacter"@ records how the center of the full
    reductive Levi acts.  @TT "ambientWeight"@ is the corresponding
    $w\mathbin{\cdot}0$ in ambient Dynkin coordinates.
  Example
    -- Partition-form input on sl_5: lambda = standard rep = partition
    -- (1,0,0,0,0), Dynkin (1,0,0,0).  Gr(1,5) = P^4.
    kostantLieAlgCohomology(cartanA 5, {1}, {1,0,0,0,0}, WeightFormat => "Partition")
  Example
    -- Gr(2,5):  disconnected Levi A_1 x A_2, nontrivial grading
    kostantLieAlgCohomology(cartanA 5, {2}, {0,0,0,0})
  Text
    For instance, its degree-$2$ entry has two Tally keys and means
    $$H^2\cong
      \bigl(V_{A_1}(0)\otimes V_{A_2}(2,0)\bigr)
      \oplus
      \bigl(V_{A_1}(2)\otimes V_{A_2}(0,1)\bigr).$$
    Within each key @TT "**"@ joins Levi factors; the two separate
    keys are direct summands.
  Example
    -- Borel subalgebra of B_3:  total number of summands is |W(B_3)| = 48
    H := kostantLieAlgCohomology(cartanB 3, {1,2,3}, {0,0,0});
    sum for k in keys H list sum values H#k
SeeAlso
  parabolicCosetReps
  dotActionOnWeight
  parabolicDotImages
  leviHighestWeights
  leviConnectedComponents
  WeightFormat
  IncludeCentralCharacter
  LeviIrrep
  MaxElements
  MaxDegree
  weylDimension
///

doc ///
Key
  rootSnapshot
  (rootSnapshot,Matrix)
  (rootSnapshot,Matrix,ZZ)
Headline
  Positive roots and multiplicities in finite, affine, and indefinite type
Usage
  S = rootSnapshot A
  S = rootSnapshot(A, H)
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
  H:ZZ
    a positive height bound; required in affine and indefinite type and
    optional in finite type
Outputs
  S:RootSnapshot
    the positive real and imaginary roots, their multiplicities, the
    coordinate convention, and the height through which the list is complete
Description
  Text
    The same function finds positive roots and their multiplicities in
    finite, affine, and indefinite type.  The type is determined from
    @TT "A"@ automatically; it can also be inspected with @TO cartanType@.
  Text
    In finite type, @TT "rootSnapshot A"@ returns the complete positive root
    system.  The bounded call @TT "rootSnapshot(A,H)"@ keeps precisely the
    roots of simple-root height at most $H$.  Its
    @TT "globallyComplete"@ field is true exactly when that bound reaches
    every positive root.
  Text
    Affine and indefinite root systems are infinite, so their one-argument
    call raises an error explaining that a height bound is required.  The
    two-argument call returns every positive root of height at most $H$ with
    its exact multiplicity.  For an indecomposable affine matrix, the
    calculation uses the description of affine roots.  For other infinite
    types, multiplicities are computed using Peterson's recurrence.
    In indefinite type, both the number of roots and their multiplicities
    can grow quickly with height.  Begin with a modest height bound and
    inspect the resulting roots before requesting a larger range.
  Example
    Sfinite = rootSnapshot cartanA 4;
    Sfinite
    (Sfinite#"type", Sfinite#"globallyComplete",
     numrows rootSnapshotRoots Sfinite)
  Text
    The last entry is $6$, so this record contains all six positive roots of
    $A_3$.  Since it is globally complete, an absent positive vector has
    multiplicity zero regardless of its height.
  Example
    Saffine = rootSnapshot(cartanAffineA 2, 6);
    (Saffine#"type", Saffine#"delta",
     rootSnapshotRoots(Saffine, Kind => "Imaginary"))
  Example
    A = matrix{{2,-3},{-3,2}}
    Sindefinite = rootSnapshot(A, 5);
    (Sindefinite#"type", rootMultiplicity(Sindefinite,{2,3}))
  Text
    The final output says that $2\alpha_1+3\alpha_2$ is an imaginary
    root of multiplicity $2$.  The snapshot itself displays the first
    $20$ roots, ordered by height and numerical root tuple, with
    multiplicities and real or imaginary type.  Use
    @TO rootSnapshotByHeight@ to show all computed roots, or
    @TO rootMultiplicity@ to find an individual multiplicity.
  Text
    Use @TO affineRootSnapshot@ or @TO indefiniteRootSnapshot@ only when a
    option particular to one of those functions is required.
SeeAlso
  RootSnapshot
  rootSnapshotByHeight
  rootSnapshotRoots
  rootMultiplicity
  cartanType
  affineRootSnapshot
  indefiniteRootSnapshot
///

doc ///
Key
  affineRootSnapshot
  (affineRootSnapshot,Matrix,ZZ)
  [affineRootSnapshot,ImaginaryMultiplicity]
Headline
  Positive roots of an affine Kac-Moody algebra, up to a height bound
Usage
  snap = affineRootSnapshot(A, H)
  snap = affineRootSnapshot(A, H, ImaginaryMultiplicity => m)
Inputs
  A:Matrix
    an indecomposable symmetrizable affine generalized Cartan matrix
    of corank $1$
  H:ZZ
    a positive integer height bound
  ImaginaryMultiplicity=>ZZ
    @TT "null"@ by default, which computes exact values; in
    untwisted type a supplied integer must equal
    @TT "numrows(A)-1"@, while twisted type rejects a constant value
Outputs
  snap:RootSnapshot
    a @TO RootSnapshot@ giving the positive roots of
    $A$ of height at most $H$, their multiplicities, the null root,
    and the height through which the list is complete
Description
  Text
    Use @TO rootSnapshot@ to find roots without specifying the Cartan
    type.  This function also accepts @TO ImaginaryMultiplicity@ as a
    consistency check.
  Text
    Let $\mathfrak g(A)$ be an affine Kac-Moody algebra
    with generalized Cartan matrix $A$ of corank $1$.  Its positive
    roots $\Phi^+$ split as
    $$\Phi^+ \;=\; \Phi^+_{\mathrm{re}} \sqcup \Phi^+_{\mathrm{im}},$$
    where $\Phi^+_{\mathrm{re}}$ is the set of @EM "real"@ roots
    (each of multiplicity $1$) and
    $\Phi^+_{\mathrm{im}} = \{k\delta : k\geq 1\}$ is the set of
    @EM "imaginary"@ roots: positive integer multiples of the
    minimal null root $\delta$ spanning $\ker A$ in the package's
    row-coroot convention.  In untwisted type every $k\delta$ has
    multiplicity @TT "numrows(A)-1"@.  In twisted type it can vary
    periodically with $k$ and is computed exactly by Peterson's
    recursion.  Both $\Phi^+_{\mathrm{re}}$ and
    $\Phi^+_{\mathrm{im}}$ are infinite, so this function lists
    only those roots of simple-root-height $\leq H$.
  Text
    The output @TT "snap"@ is a @TO RootSnapshot@.  Its mathematical
    data fields are:
  Text
    @UL {
      (TT "snap#\"real\"", ": an ", TT "n x r", " matrix whose rows are the positive real roots of height ", TEX "$\\leq H$", ", written in simple-root coordinates (so the row ", TT "{1,2,2,1}", " represents ", TEX "$\\alpha_1+2\\alpha_2+2\\alpha_3+\\alpha_4$", ")."),
      (TT "snap#\"imag\"", ": an ", TT "m x r", " matrix whose rows are the imaginary roots ", TEX "$k\\delta$", " with ", TEX "$k\\,\\mathrm{ht}(\\delta)\\leq H$", ", in simple-root coordinates (so ", TT "{1,1}", ", ", TT "{2,2}", ", ", TT "{3,3}", ", ... for affine ", TEX "$\\widehat{\\mathfrak{sl}}_2$", ")."),
      (TT "snap#\"delta\"", ": a ", TT "1 x r", " matrix giving the primitive positive coprime generator of ", TEX "$\\ker(A)$", " in simple-root coordinates."),
      (TT "snap#\"mult\"", ": a hash table mapping each root (as a list of ", TT "r", " integers) to its exact root multiplicity ", TEX "$m_\\alpha$", ".  Real roots have value ", TEX "$1$", "; imaginary values are constant ", TT "r-1", " only in untwisted type and are computed periodically in twisted type.")
    }@
  Text
    @BOLD "Reading the output."@  @TO rootSnapshotByHeight@ lays out a
    snapshot as one row per positive root, grouped by height and
    annotated with @TT "real"@ or @TT "imaginary"@:
  Example
    A = matrix{{2,-2},{-2,2}}                                     -- affine sl_2
    snap = affineRootSnapshot(A, 8);
    rootSnapshotByHeight snap
  Text
    The table exhibits the classical structure of affine $A_1$.  At
    every odd height $2k+1$, the two real roots
    $k\delta+\alpha_0=(k+1,k)$ and
    $k\delta+\alpha_1=(k,k+1)$ appear.  Equivalently, the second is
    $(k+1)\delta-\alpha_0$.  At every even height $2k$, the imaginary
    root $k\delta=(k,k)$ appears with multiplicity $1$.
  Text
    The real roots, null root, and individual multiplicities can also be
    read directly:
  Example
    snap#"real"
    snap#"delta"
    (snap#"mult")#{2,2}
  Text
    Thus @TT "snap#\"delta\""@ prints the row @TT "| 1 1 |"@,
    meaning $\delta=\alpha_0+\alpha_1$ in conventional affine notation:
    the two columns remain in Cartan-matrix order.  The final coefficient
    calculation returns $1$, the multiplicity of
    $2\delta$.
  Text
    For an affine type of higher rank, say $\widehat{\mathfrak{sl}}_3$,
    the null root is $\delta = \alpha_0+\alpha_1+\alpha_2$ and imaginary
    root multiplicities equal $\mathrm{rank}-1 = 2$:
  Example
    Aaff3 = matrix{{2,-1,-1},{-1,2,-1},{-1,-1,2}}
    snap3 = affineRootSnapshot(Aaff3, 4);
    rootSnapshotByHeight snap3
  Text
    @BOLD "Height bound."@  Both sets of roots are infinite; larger
    $H$ includes more roots.  For a parabolic decomposition,
    @TO leviHighestWeights@ determines a sufficient height from the chosen
    degree and finite Levi Cartan matrix, so no height bound needs to be
    supplied separately.
SeeAlso
  rootSnapshot
  rootSnapshotByHeight
  leviHighestWeights
  indefiniteRootSnapshot
  cartanPositiveRoots
  ImaginaryMultiplicity
///

doc ///
Key
  indefiniteRootSnapshot
  (indefiniteRootSnapshot,Matrix,ZZ)
  [indefiniteRootSnapshot,Algorithm]
Headline
  Positive roots in affine or indefinite type with a choice of multiplicity algorithm
Usage
  snap = indefiniteRootSnapshot(A, H)
  snap = indefiniteRootSnapshot(A, H, Algorithm => alg)
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix of affine or indefinite type
  H:ZZ
    a positive integer height bound
  Algorithm=>String
    @TT "\"Auto\""@ (default), @TT "\"BackusConnickLin\""@
    (aliases @TT "\"BCL\""@ and @TT "\"GradedAscent\""@), or
    @TT "\"SparsePeterson\""@ (aliases @TT "\"Peterson\""@ and
    @TT "\"Sparse\""@)
Outputs
  snap:RootSnapshot
    a @TO RootSnapshot@ giving the positive roots of
    $A$ of height at most $H$, their multiplicities, the chosen algorithm,
    and the height through which the list is complete
Description
  Text
    Use @TO rootSnapshot@ to find roots without choosing a Cartan type or
    multiplicity algorithm.  To compare the two algorithms in affine or
    indefinite type, use this function with the @TT "Algorithm"@ option.
  Text
    Let $\mathfrak g(A)$ be an affine or indefinite symmetrizable Kac-Moody
    algebra with generalized Cartan matrix $A$, and let $(\cdot,\cdot)
    = DA$ be the Weyl-invariant bilinear form.  The positive root
    system $\Phi^+$ partitions into @EM "real"@ roots
    ($(\beta,\beta) > 0$, each of multiplicity exactly $1$) and
    @EM "imaginary"@ roots ($(\beta,\beta) \leq 0$, whose multiplicity
    $\mathrm{mult}(\beta)$ is a positive integer that can grow rapidly
    with the height).
  Text
    Both algorithms use Peterson's recurrence for the formal character
    coefficients
  Text
    @UL {
      (TEX "$$\\big((\\beta,\\beta) - 2(\\beta,\\rho)\\big)\\,c(\\beta) \\;=\\; \\sum_{\\gamma+\\delta=\\beta} (\\gamma,\\delta)\\,c(\\gamma)\\,c(\\delta);$$"),
      ("and recover root multiplicities by divisor inversion:"),
      (TEX "$$\\mathrm{mult}(\\beta) \\;=\\; c(\\beta) - \\sum_{\\substack{n\\mid\\gcd(\\beta)\\\\ n\\geq 2}} \\frac{1}{n}\\,\\mathrm{mult}(\\beta/n).$$")
    }@
  Text
    @TT "Algorithm => \"BackusConnickLin\""@ enumerates lattice points
    in the bounded fundamental imaginary chamber with Normaliz, computes
    one representative of each imaginary Weyl orbit by graded Peterson
    ascent, and finds the remaining orbit elements by simple reflections.
    This is the algorithm of A. Backus, P. Connick, and J. Lin.  The cone
    includes the nonnegative simple-root coordinates, and the recurrence
    uses the symmetrized form $DA$, including when $A$ is nonsymmetric.
  Text
    @TT "Algorithm => \"SparsePeterson\""@ performs Peterson recursion
    globally, but sums only over pairs with nonzero $c$-coefficients.
    It therefore does not list every point in the nonnegative root
    lattice.  @TT "Auto"@ chooses between these two algorithms.  Previous
    calculations for @TT "A"@ are retained for each algorithm and height.
    The number of roots and the number of pairs in Peterson's recurrence
    both affect the running time.  In rank $2$, a root list that grows
    quadratically with $H$ can require roughly quartic growth in the
    number of recurrence terms.  The Backus--Connick--Lin algorithm is
    particularly useful when the
    fundamental imaginary chamber is a small fraction of the full
    imaginary cone.
  Text
    The root-data fields of @TT "snap"@ are:
  Text
    @UL {
      (TT "snap#\"real\"", ": an ", TT "n x r", " matrix whose rows are the positive real roots of height ", TEX "$\\leq H$", ", written in simple-root coordinates."),
      (TT "snap#\"imag\"", ": an ", TT "m x r", " matrix whose rows are the positive imaginary roots of height ", TEX "$\\leq H$", "."),
      (TT "snap#\"mult\"", ": a hash table mapping each positive root ", TEX "$\\beta$", " (as a list of ", TT "r", " integers) to its exact positive multiplicity ", TEX "$\\mathrm{mult}(\\beta)\\in\\mathbb{Z}_{>0}$", ".  Non-roots and zero values are never stored.")
    }@
  Text
    @BOLD "Reading the output."@  The function @TO rootSnapshotByHeight@
    lays out the snapshot as a single table with one row per positive
    root, showing its height, whether it is real or imaginary, and its multiplicity;
    this is usually the easiest way to interpret the result:
  Example
    A = matrix{{2,-3},{-3,2}}                                     -- rank-2 hyperbolic GCM
    snap = indefiniteRootSnapshot(A, 5);
    rootSnapshotByHeight snap
  Text
    The row @TT "(2, 3)"@ with @TT "multiplicity"@ equal to $2$ says
    $$\beta=2\alpha_1+3\alpha_2,\qquad
      \dim\mathfrak g_\beta=\operatorname{mult}(\beta)=2.$$
    This differs from untwisted affine type, where every imaginary
    root multiplicity is the underlying finite rank.  Twisted affine
    multiplicities can instead be periodic.  In indefinite type
    multiplicities can grow rapidly, and their bounds and generating
    series lead to deep open problems (for example Frenkel's
    conjecture in hyperbolic type).
  Text
    No inverse of the symmetrized form is used, so singular indefinite
    Cartan matrices are supported.
  Text
    The root lists and individual multiplicities can also be read directly:
  Example
    snap#"real"
    snap#"imag"
    (snap#"mult")#{1,1}
    (snap#"mult")#{2,3}
SeeAlso
  rootSnapshot
  rootSnapshotByHeight
  leviHighestWeights
  affineRootSnapshot
  symmetrizer
///

doc ///
Key
  rootSnapshotByHeight
  (rootSnapshotByHeight,HashTable)
Headline
  Display roots and multiplicities in a table grouped by height
Usage
  T = rootSnapshotByHeight snap
Inputs
  snap:HashTable
    the output of @TO rootSnapshot@, @TO affineRootSnapshot@, or
    @TO indefiniteRootSnapshot@
Outputs
  T:Net
    a boxed table with one row per positive root, grouped by height
    and listing the columns
    @TT "height"@,
    @TT "root"@ (a numerical tuple in the simple-root basis),
    @TT "multiplicity"@,
    @TT "type"@ (@TT "real"@ or @TT "imaginary"@)
Description
  Text
    A snapshot gives separate lists of real and imaginary roots, together
    with their multiplicities.
    @TT "rootSnapshotByHeight"@ combines them into a single table sorted
    by height and then by root tuple.  It includes every root in the
    snapshot, even when the default display shows only a preview.
  Text
    For affine $\widehat{\mathfrak{sl}}_2$ up to height $6$, the table
    alternates pairs of real roots at odd heights with a single
    imaginary root at even heights:
  Example
    A = matrix{{2,-2},{-2,2}}
    rootSnapshotByHeight rootSnapshot(A, 6)
  Text
    For example, the height-$3$ row @TT "(1, 2)"@ represents
    $\alpha_1+2\alpha_2$; @TT "real"@ and @TT "1"@ say that it is a
    real root with a one-dimensional root space.  The height-$4$ row
    @TT "(2, 2)"@ is $2\delta$; @TT "imaginary"@ and @TT "1"@ identify an
    imaginary root whose root space is also one-dimensional.
  Text
    Affine $\widehat{\mathfrak{sl}}_3$.  The null root is
    $\delta = \alpha_0+\alpha_1+\alpha_2$, so $2\delta$ first appears
    at height $6$ with multiplicity $\mathrm{rank}-1 = 2$:
  Example
    Aaff3 = matrix{{2,-1,-1},{-1,2,-1},{-1,-1,2}}
    rootSnapshotByHeight rootSnapshot(Aaff3, 6)
  Text
    In indefinite type the multiplicities grow rapidly with height.
    For the rank-$2$ hyperbolic $A = \left(\begin{smallmatrix} 2 & -3
    \\ -3 & 2 \end{smallmatrix}\right)$ one already sees
    multiplicity-$2$ imaginary roots at height $5$:
  Example
    B = matrix{{2,-3},{-3,2}}
    rootSnapshotByHeight rootSnapshot(B, 5)
SeeAlso
  rootSnapshot
  affineRootSnapshot
  indefiniteRootSnapshot
///

doc ///
Key
  rootSnapshotRoots
  (rootSnapshotRoots,HashTable)
  [rootSnapshotRoots,Kind]
Headline
  Extract real, imaginary, or all roots from a snapshot
Usage
  R = rootSnapshotRoots snap
  R = rootSnapshotRoots(snap, Kind => kind)
Inputs
  snap:HashTable
    a @TO RootSnapshot@
  Kind=>String
    @TT "\"All\""@ (default), @TT "\"Real\""@, or
    @TT "\"Imaginary\""@, case-insensitive as documented below
Outputs
  R:Matrix
    one selected positive root per row in simple-root coordinates
Description
  Text
    @TT "All"@ concatenates the real and imaginary matrices.  The
    accepted lowercase aliases are @TT "all"@, @TT "real"@,
    @TT "imag"@, and @TT "imaginary"@.
  Example
    S = rootSnapshot(cartanAffineA 2, 6)
    numrows rootSnapshotRoots S
    rootSnapshotRoots(S, Kind => "Imaginary")
  Text
    The imaginary rows $(1,1,1)$ and $(2,2,2)$ mean
    $\delta$ and $2\delta$ in $A_2^{(1)}$.  These are precisely the
    positive imaginary roots of heights at most $6$.  The preceding
    row count counts distinct real and imaginary roots together,
    not their root-space dimensions.

SeeAlso
  Kind
  rootMultiplicity
  isRootInSnapshot
///

doc ///
Key
  rootMultiplicity
  (rootMultiplicity,HashTable,List)
Headline
  Root multiplicity within the known height range
Usage
  m = rootMultiplicity(snap, beta)
Inputs
  snap:HashTable
    a @TO RootSnapshot@
  beta:List
    rank-many integral simple-root coefficients
Outputs
  m:ZZ
    the exact multiplicity, or zero for a nonroot within the known height range
Description
  Text
    If @TT "sum beta"@ exceeds @TT "snap#\"completeThroughHeight\""@,
    the function raises an error for a bounded snapshot, since roots
    beyond that height have not been computed.  A complete
    finite snapshot instead returns zero for every absent positive vector,
    regardless of its height.
  Example
    S = rootSnapshot(matrix{{2,-3},{-3,2}}, 5)
    rootMultiplicity(S, {2,3})
    rootMultiplicity(S, {1,1})
  Text
    The outputs $2$ and $1$ mean
    $\dim\mathfrak g_{2\alpha_1+3\alpha_2}=2$ and
    $\dim\mathfrak g_{\alpha_1+\alpha_2}=1$ for the indicated
    rank-$2$ indefinite algebra.  Both roots lie in the computed
    height range; the first is imaginary.

SeeAlso
  isRootInSnapshot
  rootSnapshotRoots
///

doc ///
Key
  isRootInSnapshot
  (isRootInSnapshot,HashTable,List)
Headline
  Test whether a root is explicitly present in a snapshot
Usage
  b = isRootInSnapshot(snap, beta)
Inputs
  snap:HashTable
    a @TO RootSnapshot@
  beta:List
    rank-many integral simple-root coefficients
Outputs
  b:Boolean
    true exactly when @TT "beta"@ is a key of the snapshot multiplicity table
Description
  Text
    This is a presence test, not a global root predicate.  A false
    result above the snapshot's height bound means only that the vector
    does not appear in the computed list.  Use @TO rootMultiplicity@ to
    obtain a multiplicity, with an error if the vector lies beyond the
    known height range.
  Example
    S = rootSnapshot(cartanAffineA 2, 6)
    isRootInSnapshot(S, {1,1,1})
  Text
    The answer true says that $(1,1,1)$ is one of the computed
    positive roots.  It is the affine null root
    $\delta=\alpha_0+\alpha_1+\alpha_2$, whose height is $3$,
    inside the requested height bound $6$.

SeeAlso
  rootMultiplicity
  rootSnapshotRoots
///

doc ///
Key
  rootSnapshotCacheInfo
  (rootSnapshotCacheInfo,Matrix)
Headline
  Inspect saved root-multiplicity calculations and their height bounds
Usage
  H = rootSnapshotCacheInfo A
Inputs
  A:Matrix
    a generalized Cartan matrix
Outputs
  H:HashTable
    the largest retained affine height, if any, and the retained heights
    for each indefinite multiplicity algorithm
Description
  Text
    Previous root calculations are retained through the largest computed
    height.
    A later request at a smaller height is obtained by truncation.  This
    function reports these bounds.
  Example
    A = matrix{{2,-3},{-3,2}};
    rootSnapshot(A, 5);
    rootSnapshotCacheInfo A
  Text
    The retained height $5$ means that the previously computed
    root multiplicities through height $5$ are available for this
    Cartan matrix.  This table describes retained calculations,
    not a root or a representation; it does not alter the meaning
    of any previously constructed snapshot.

SeeAlso
  clearRootSnapshotCache
  representationCacheInfo
///

doc ///
Key
  clearRootSnapshotCache
  (clearRootSnapshotCache,Matrix)
Headline
  Discard retained affine and indefinite root calculations
Usage
  clearRootSnapshotCache A
Inputs
  A:Matrix
    the Cartan matrix whose saved root calculations are cleared
Description
  Text
    Discards previously retained root calculations for @TT "A"@.  Existing
    @TO RootSnapshot@ values remain valid, and saved finite-representation
    calculations are unaffected.
  Example
    A = matrix{{2,-3},{-3,2}};
    S = rootSnapshot(A,4);
    clearRootSnapshotCache A
    rootMultiplicity(S,{1,1})
  Text
    The multiplicity remains $1$: the previously constructed snapshot
    still represents the same positive-root spaces.  Clearing saved
    calculations does not invalidate an existing snapshot.

SeeAlso
  rootSnapshotCacheInfo
  clearRepresentationCache
///

doc ///
Key
  Kind
Headline
  Option selecting roots from a RootSnapshot
Description
  Text
    Used by @TO rootSnapshotRoots@.  The value is @TT "All"@,
    @TT "Real"@, or @TT "Imaginary"@; documented lowercase aliases
    are also accepted.
  Example
    S = rootSnapshot(cartanAffineA 2,6);
    rootSnapshotRoots(S,Kind=>"Imaginary")
  Text
    The two rows $(1,1,1)$ and $(2,2,2)$ are $\delta$ and
    $2\delta$, the positive imaginary roots through height $6$.
    Selecting imaginary roots changes the displayed subset, not their
    computed multiplicities or the snapshot height bound.

SeeAlso
  rootSnapshotRoots
  RootSnapshot
///

doc ///
Key
  coxeterMatrix
  (coxeterMatrix,CartanDatum,Thing,Thing)
  (coxeterMatrix,KMWeylGroup,Thing,Thing)
  (coxeterMatrix,Realization,Thing,Thing)
  (coxeterMatrix,RootDatum,Thing,Thing)
  (coxeterMatrix,CartanDatum)
  (coxeterMatrix,CartanDatum,ZZ,ZZ)
  (coxeterMatrix,CartanDatum,String,String)
  (coxeterMatrix,Realization)
  (coxeterMatrix,Realization,ZZ,ZZ)
  (coxeterMatrix,Realization,String,String)
  (coxeterMatrix,RootDatum)
  (coxeterMatrix,RootDatum,ZZ,ZZ)
  (coxeterMatrix,RootDatum,String,String)
  (coxeterMatrix,KMWeylGroup)
  (coxeterMatrix,KMWeylGroup,ZZ,ZZ)
  (coxeterMatrix,KMWeylGroup,String,String)
  (coxeterMatrix,Matrix)
  (coxeterMatrix,Matrix,ZZ,ZZ)
Headline
  Coxeter exponents determined by a Cartan matrix
Usage
  C = coxeterMatrix W
  m = coxeterMatrix(W, label1, label2)
  C = coxeterMatrix A
  m = coxeterMatrix(A, i, j)
Inputs
  W:KMWeylGroup
    a Weyl group with its Cartan datum
  D:CartanDatum
    alternatively, a Cartan datum; a @TO RootDatum@ or
    @TO Realization@ is also accepted
  A:Matrix
    a generalized Cartan matrix of rank $r$
  i:ZZ
    an optional 1-based node position
  j:ZZ
    an optional 1-based node position
  label1:String
    an optional node label from the Cartan datum
  label2:String
    a second stored node label
Outputs
  C:List
    the symmetric nested list whose entry in row $i$, column $j$ is
    the Coxeter exponent $m_{ij}$
  m:Number
    the single exponent $m_{ij}$ when two node specifiers are supplied
Description
  Text
    The simple reflections attached to $A=(a_{ij})$ generate the Weyl
    group with relations
    $$s_i^2=1,\qquad (s_is_j)^{m_{ij}}=1
      \quad\text{when }m_{ij}<\infty.$$
    This function sets $m_{ii}=1$ and, for $i\ne j$, computes
    $$m_{ij}=2,3,4,6,\text{ or }\infty
      \quad\text{as }a_{ij}a_{ji}=0,1,2,3,\text{ or }\geq4.$$
    The full result is a nested list rather than a Macaulay2 @TO Matrix@
    because an integer matrix cannot contain @TT "infinity"@.  Thus
    @TT "C#(i-1)#(j-1)"@ is $m_{ij}$, but the three-argument form is
    usually clearer because it follows the package's 1-based Dynkin-node
    convention directly.  The value for an infinite exponent is the
    Macaulay2 number @TT "infinity"@.
  Example
    D = cartanDatum("A3",
        NodeLabels => {"left","middle","right"});
    W = weylGroup D;
    coxeterMatrix W
    coxeterMatrix(W, "left", "middle")
    coxeterMatrix(W, "left", "right")
  Text
    The labeled calls return $3$ and $2$: adjacent simple reflections
    satisfy the length-$3$ braid relation, while the two end
    reflections commute.  When a Cartan datum is supplied, nodes can be
    specified by their names or by positions numbered from $1$.  With a
    matrix, use positions.
  Example
    coxeterMatrix cartanA 4
    coxeterMatrix cartanAffineA 3
    (coxeterMatrix(cartanRank2(1,1), 1, 2),
     coxeterMatrix(cartanRank2(1,2), 1, 2),
     coxeterMatrix(cartanRank2(1,3), 1, 2),
     coxeterMatrix(cartanRank2(2,2), 1, 2))
  Text
    The last line returns @TT "(3, 4, 6, infinity)"@: the first three
    pairs satisfy braid relations of lengths $3$, $4$, and $6$, while
    the final pair has no finite braid relation.  Disconnected nodes
    have exponent $2$, so their simple reflections commute.  Since only
    $a_{ij}a_{ji}$ is used, transposing $A$ leaves the Coxeter matrix
    unchanged even though it exchanges the root and coroot realizations.
SeeAlso
  reflectionMatrix
  weylElementMatrix
  dynkinDiagram
  dualCartanMatrix
///

doc ///
Key
  reflectionMatrix
  (reflectionMatrix,CartanDatum,Thing)
  (reflectionMatrix,KMWeylGroup,Thing)
  (reflectionMatrix,Realization,Thing)
  (reflectionMatrix,RootDatum,Thing)
  (reflectionMatrix,CartanDatum,ZZ)
  (reflectionMatrix,CartanDatum,String)
  (reflectionMatrix,Realization,ZZ)
  (reflectionMatrix,Realization,String)
  (reflectionMatrix,RootDatum,ZZ)
  (reflectionMatrix,RootDatum,String)
  (reflectionMatrix,KMWeylGroup,ZZ)
  (reflectionMatrix,KMWeylGroup,String)
  (reflectionMatrix,Matrix,ZZ)
Headline
  Matrix of a simple reflection
Usage
  S = reflectionMatrix(W, label)
  S = reflectionMatrix(A, i)
Inputs
  W:KMWeylGroup
    a Weyl group with its Cartan datum; a @TO CartanDatum@, @TO RootDatum@,
    or @TO Realization@ is also accepted
  A:Matrix
    a (generalized) Cartan matrix of rank $n$
  i:ZZ
    a 1-based simple-root index, $1\leq i\leq n$
  label:String
    a node label from the Cartan datum
Outputs
  S:Matrix
    the matrix of the simple reflection $s_i$ acting on the simple-root
    basis, i.e.\ $S\cdot e_j = s_i(\alpha_j)$
Description
  Text
    The simple reflection $s_i$ acts on a simple root by
    $s_i(\alpha_j) = \alpha_j - A_{ij}\alpha_i$.
    This function gives the corresponding $n\times n$ integer matrix in
    the simple-root basis.  A Weyl-group word $w = s_{i_1}\cdots s_{i_k}$
    is obtained as the product of these matrices; see
    @TO weylElementMatrix@.  When a Cartan datum is supplied, nodes can be
    specified by name or by positions numbered from $1$.  With a matrix,
    use positions.
  Example
    D = cartanDatum("A3",
        NodeLabels => {"left","middle","right"});
    W = weylGroup D;
    reflectionMatrix(W, "left")
    reflectionMatrix(W, "middle") * reflectionMatrix(W, "left")
  Text
    The columns of the first matrix give $s_1(\alpha_1)=-\alpha_1$,
    $s_1(\alpha_2)=\alpha_1+\alpha_2$, and $s_1(\alpha_3)=\alpha_3$. The second matrix
    represents $s_2s_1$, with $s_1$ acting first. Both matrices act on column vectors of
    simple-root coefficients.

    Reflections are involutions ($s_i^2 = e$), and a pair $(s_i, s_j)$
    of simple reflections satisfies the braid relation
    $(s_i s_j)^{m_{ij}} = e$, where $m_{ij} \in \{2,3,4,6\}$ depending
    on $A_{ij}A_{ji}\in\{0,1,2,3\}$; when the product is at least $4$,
    $m_{ij}=\infty$.  For example in $G_2$ one has
    $m_{12}=6$:
  Example
    G := cartanG2()
    S1 := reflectionMatrix(G, 1)
    S2 := reflectionMatrix(G, 2)
    S1*S1 == id_(ZZ^2)
    (S1*S2)^6 == id_(ZZ^2)
  Text
    Reflections also satisfy the formula $s_i(\alpha_j) = \alpha_j -
    A_{ij}\alpha_i$, which is exactly the $j$-th column of the
    reflection matrix:
  Example
    reflectionMatrix(cartanB 3, 3) * transpose matrix{{0,1,0}}
SeeAlso
  coxeterMatrix
  weylElementMatrix
  weylInversionData
///

doc ///
Key
  weylElementMatrix
  (weylElementMatrix,WeylElement)
  (weylElementMatrix,Matrix,List)
Headline
  Matrix of the Weyl group element of a given word
Usage
  M = weylElementMatrix(A, w)
  M = weylElementMatrix u
Inputs
  A:Matrix
    a (generalized) Cartan matrix of rank $n$
  w:List
    a list of 1-based simple-reflection indices
Outputs
  M:Matrix
    the product $s_{i_1}\cdots s_{i_k}$ expressed as a matrix on the
    simple-root basis
Description
  Text
    The word @TT "w"@ need not be reduced.  The resulting matrix depends
    only on the Weyl-group element represented by the word, so a nonreduced word will
    still give the correct matrix; equivalently, any two reduced words
    for the same element give equal matrices.
    Alternatively, @TT "u"@ can be a @TO WeylElement@.  Its stored
    Cartan datum determines the same matrix on simple-root coordinates.
  Example
    A = cartanA 3
    weylElementMatrix(A, {1,2,1})
    weylElementMatrix(A, {2,1,2})  -- same element by braid relation
  Text
    Both Weyl-element matrices are the same $A_2$ matrix: their columns express
    $\alpha_1\mapsto-\alpha_2$ and $\alpha_2\mapsto-\alpha_1$. The equality reflects the
    braid relation $s_1s_2s_1=s_2s_1s_2$; these two words represent the longest element.

    The longest element $w_0$ of a finite Weyl group acts by $-1$ in
    types $A_1$, $D_{2n}$, $E_7$, $E_8$,
    $B_n$, $C_n$, $F_4$, and $G_2$.  For $A_n$ with $n\geq 2$,
    $w_0$ acts as the negative of the diagram-reversing automorphism.
    For $A_3$, its matrix reverses the simple-root order and changes
    each sign:
  Example
    w0 := longElement cartanA 4          -- S_4 longest, length 6
    weylElementMatrix(cartanA 4, w0)
  Text
    Since $D_4$ is of type $D_{2n}$, its longest element acts as
    $-\mathrm{id}$ on the root lattice:
  Example
    w0D4 := longElement cartanD 4
    weylElementMatrix(cartanD 4, w0D4) == - id_(ZZ^4)
SeeAlso
  reflectionMatrix
  reducedWord
  weylLength
  longElement
///

doc ///
Key
  weylInversionData
  (weylInversionData,Matrix,List)
Headline
  Length and (left) inversion set of a Weyl-group word
Usage
  H = weylInversionData(A, w)
Inputs
  A:Matrix
    a (generalized) Cartan matrix
  w:List
    a list of 1-based simple-reflection indices
Outputs
  H:HashTable
    a hash table with keys @TT "\"word\""@, @TT "\"length\""@, and @TT "\"inversions\""@
Description
  Text
    The word @TT "w"@ need not be reduced.  The function processes
    @TT "w"@ left to right, maintaining a running inversion set: at each
    step it determines whether the next simple reflection adds or
    removes an inversion, updating the set accordingly.  After
    processing, the remaining inversion set is the (left) inversion set
    of the Weyl-group element represented by the word, and @TT "\"length\""@ is its
    Coxeter length.
  Text
    Each inversion is stored as a positive root in simple-root coordinates.
  Example
    A = cartanA 4
    weylInversionData(A, {1,2,1})
    weylInversionData(A, {1,2,1,2,1,2})  -- identity: empty inversions
  Text
    The first element has length $3$ and its inversion roots are $\alpha_1$, $\alpha_2$,
    and $\alpha_1+\alpha_2$, with simple-root tuples $(1,0,0)$, $(0,1,0)$, and $(1,1,0)$.
    The repeated word in the second call is the identity, so its length is $0$ and its
    inversion list is empty.

    The longest element $w_0$ of $A_3 = S_4$ has length
    $\binom{4}{2} = 6$, and its inversion set is all of $\Phi^+$:
  Example
    H := weylInversionData(cartanA 4, longElement cartanA 4);
    H#"length"
    # H#"inversions"
    set H#"inversions" === set entries cartanPositiveRoots cartanA 4
  Text
    In $B_3$ the longest element has length $9$ and inverts all nine
    positive roots (three short, six long):
  Example
    H2 := weylInversionData(cartanB 3, longElement cartanB 3);
    H2#"length"
SeeAlso
  weylLength
  weylElementMatrix
  bruhatLessOrEqual
  reducedWord
///

doc ///
Key
  weylLength
  (weylLength,WeylElement)
  (weylLength,Matrix,List)
Headline
  Coxeter length of a Weyl-group element
Usage
  k = weylLength(A, w)
  k = weylLength u
Inputs
  A:Matrix
    a (generalized) Cartan matrix
  w:List
    a list of 1-based simple-reflection indices
Outputs
  k:ZZ
    the Coxeter length of the Weyl-group element represented by @TT "w"@
Description
  Text
    Equivalently, the number of positive roots sent to negative roots
    by the element; or the length of any reduced word.  The argument
    @TT "w"@ need not itself be reduced.
    The one-argument form takes a @TO WeylElement@ and returns its
    Coxeter length.
  Example
    A = cartanA 3
    weylLength(A, {1,2,1})        -- reduced, length 3
    weylLength(A, {1,2,1,2,1,2})  -- identity, length 0
  Text
    The outputs $3$ and $0$ distinguish a reduced expression for the longest element of
    $A_2$ from the identity. Although the second input has six letters, its Weyl-group
    element has Coxeter length $0$.

    The length of $w_0$ equals the number of positive roots.  For the
    classical types this reproduces the familiar formulas
    $|\Phi^+(A_{n-1})| = \binom{n}{2}$, $|\Phi^+(B_n)| = |\Phi^+(C_n)| = n^2$,
    $|\Phi^+(D_n)| = n(n-1)$:
  Example
    weylLength(cartanA 5, longElement cartanA 5)  -- C(5,2) = 10
    weylLength(cartanB 3, longElement cartanB 3)  -- 3^2 = 9
    weylLength(cartanD 4, longElement cartanD 4)  -- 4*3 = 12
SeeAlso
  weylInversionData
  reducedWord
  longElement
///

doc ///
Key
  bruhatLessOrEqual
  (bruhatLessOrEqual,WeylElement,WeylElement)
  (bruhatLessOrEqual,Matrix,List,List)
Headline
  Bruhat order comparison
Usage
  b = bruhatLessOrEqual(A, u, v)
  b = bruhatLessOrEqual(u, v)
Inputs
  A:Matrix
    a (generalized) Cartan matrix
  u:List
    a word for a Weyl-group element
  v:List
    a word for a Weyl-group element
Outputs
  b:Boolean
    true iff $u\leq v$ in the (strong) Bruhat order
Description
  Text
    The comparison uses the reduced-subword criterion for @EM "strong"@
    Bruhat order: if $v=s_{i_1}\cdots s_{i_k}$ is reduced, then
    $u\leq v$ if and only if some subword represents $u$.
    In the two-argument form, @TT "u"@ and @TT "v"@ are
    @TO WeylElement@ objects belonging to the same Weyl group.
  Example
    A = cartanA 3
    bruhatLessOrEqual(A, {1}, {1,2,1})
    bruhatLessOrEqual(A, {1,2,1}, {1})
  Text
    The answers are true and false: $s_1\leq s_1s_2s_1=w_0$ in $W(A_2)$, but the longest
    element is not below $s_1$. The first comparison follows because the one-letter word
    occurs as a reduced subword of the three-letter word.

    In finite type, the Bruhat order has a unique minimum (the identity, given by the
    empty word @TT "{}"@) and a unique maximum (the longest element
    $w_0$).  In particular, every element of the Weyl group lies
    between these:
  Example
    w0 := longElement cartanB 3
    all(weylGroupElements cartanB 3,
        h -> bruhatLessOrEqual(cartanB 3, {}, h#"word") and
             bruhatLessOrEqual(cartanB 3, h#"word", w0))
  Text
    The Bruhat order is refined by length: $u \leq v$ implies
    $\ell(u) \leq \ell(v)$.  Equal length does not imply equality,
    however -- there can be many incomparable elements of the same
    length.  For instance in $W(A_2) = S_3$, the two reflections $s_1$
    and $s_2$ are both of length $1$ but incomparable:
  Example
    (bruhatLessOrEqual(cartanA 3, {1}, {2}),
     bruhatLessOrEqual(cartanA 3, {2}, {1}))
SeeAlso
  weylInversionData
  longElement
  weylGroupElements
  weylLength
///

doc ///
Key
  longElement
  (longElement,Matrix)
Headline
  Reduced word for the longest Weyl-group element
Usage
  w0 = longElement A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  w0:List
    a reduced word for the longest element $w_0$ of the Weyl group
Description
  Text
    Computed by repeatedly applying a simple right ascent until every
    simple root is sent to a negative root.  This takes exactly
    $|\Phi^+|$ reflection steps and does not enumerate the Weyl group.
    Only finite type is accepted.
  Example
    longElement cartanA 4   -- S_4 longest: C(4,2) = 6 inversions, length 6
    longElement cartanB 2   -- W(B_2) longest: length 4 = |Phi^+(B_2)|
  Text
    The length of $w_0$ is always $|\Phi^+|$, and the @BOLD "matrix"@
    of $w_0$ is the diagram automorphism @BOLD "composed"@ with
    $-\mathrm{id}$ on the root lattice.  For $A_n (n\geq 2)$,
    $D_{2n+1}$, and $E_6$ this gives a nontrivial automorphism; for
    all other finite types $w_0 = -\mathrm{id}$:
  Example
    -- A_3: w_0 acts as -(diagram flip), which is NOT -id
    w0A3 := longElement cartanA 4
    MatA3 := weylElementMatrix(cartanA 4, w0A3)
    MatA3 == - id_(ZZ^3)          -- false
  Example
    -- B_3: w_0 = -id
    weylElementMatrix(cartanB 3, longElement cartanB 3) == - id_(ZZ^3)
SeeAlso
  weylLength
  weylGroupElements
  weylGroupOrder
///

doc ///
Key
  weylGroupOrder
  (weylGroupOrder,Matrix)
Headline
  Order of the Weyl group
Usage
  n = weylGroupOrder A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  n:ZZ
    the cardinality $|W|$ of the Weyl group
Description
  Text
    Computed from the finite Weyl exponents as
    $|W|=\prod_i(m_i+1)$, without listing the elements of the group.
    Affine and indefinite inputs are rejected.
  Example
    weylGroupOrder cartanA 4   -- S_4, order 24
    weylGroupOrder cartanB 3   -- W(B_3), order 48
    weylGroupOrder cartanD 4   -- W(D_4), order 192
  Text
    The classical orders follow the standard formulas $|W(A_{n-1})| = n!$,
    $|W(B_n)| = |W(C_n)| = 2^n n!$, $|W(D_n)| = 2^{n-1} n!$, and the
    exceptional types have the orders
  Text
    @UL {
      {TEX "$|W(G_2)| = 12$"},
      {TEX "$|W(F_4)| = 1152$"},
      {TEX "$|W(E_6)| = 51840$"},
      {TEX "$|W(E_7)| = 2903040$"}
    }@
  Example
    weylGroupOrder cartanG2()
  Text
    These factor as products of @BOLD "degrees"@ of basic $W$-invariants:
    for $A_{n-1}$ the degrees are $(2,3,\ldots,n)$, so $|W(A_{n-1})| =
    n!$, and for $E_6$ the degrees are $(2,5,6,8,9,12)$ (product is
    $51840$); we reproduce the $A_3$ case directly:
  Example
    weylGroupOrder cartanA 4     -- 24 = 4!
    product {2,3,4}              -- = 24
SeeAlso
  weylGroupElements
  weylLengthGenerating
  longElement
///

doc ///
Key
  weylGroupElements
  (weylGroupElements,Matrix)
  (weylGroupElements,Matrix,ZZ)
  [weylGroupElements,MaxElements]
Headline
  Enumerate Weyl-group elements
Usage
  L = weylGroupElements A
  L = weylGroupElements(A, k)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  k:ZZ
    a nonnegative integer; if supplied, restrict to elements of Coxeter
    length exactly @TT "k"@
  MaxElements=>ZZ
    maximum number of elements to retain during the calculation;
    default $10000$, or @TT "null"@ for no limit
Outputs
  L:List
    a list of @TO WeylElement@ records, one per element, with keys @TT "\"word\""@
    (a reduced word, 1-based), @TT "\"matrix\""@ (its matrix on the
    simple-root basis), and @TT "\"length\""@
Description
  Text
    Elements are listed in order of increasing length, so
    @TT "weylGroupElements A"@ begins with the identity (empty word)
    and ends with $w_0$.  Each element is given by one reduced word;
    other reduced words may represent the same element.  Before listing
    all elements, the function checks the group order against
    @TO MaxElements@.  To list elements of a chosen length, the
    calculation also retains elements of smaller length, so those count
    toward the limit as well.
  Example
    L = weylGroupElements cartanA 3;
    length L
    first L
    last L
  Example
    length weylGroupElements(cartanA 4, 3)   -- |{w \in S_4 : l(w) = 3}|
  Text
    The first count $6$ is $|W(A_2)|=|S_3|$.  The first and last
    displayed elements are the identity and the longest element.
    The second count $6$ says that $W(A_3)=S_4$ has exactly six
    elements of Coxeter length $3$, equivalently six permutations
    with three inversions.

SeeAlso
  weylGroupOrder
  weylLengthGenerating
  reducedWord
  weylLength
///

doc ///
Key
  MaxElements
Headline
  Limit on the number of Weyl-group and coset elements
Description
  Text
    An option used by @TO weylGroupElements@ and
    @TO parabolicCosetReps@, including the coset representatives used by
    @TO kostantLieAlgCohomology@.  Its default is $10000$.  If the requested
    calculation would retain more elements, the function stops with an
    error.  Larger groups and quotients require more time and memory.
    Set a larger positive integer to allow more elements, or use
    @TT "MaxElements => null"@ for no limit.
  Text
    This option does not limit closed-form invariants such as
    @TO weylGroupOrder@ or @TO weylLengthGenerating@, neither of which
    enumerates $W$.  For perspective,
    $|W(E_8)|=696729600$, so a full @TT "weylGroupElements(cartanE 8)"@
    would require an extremely large list, even though its order is
    easily computed.
  Example
    weylGroupOrder cartanE 8
    weylGroupElements(cartanA 4, MaxElements => 24)
  Text
    The first output is $696729600=|W(E_8)|$.  The second output
    contains all $24$ elements of $W(A_3)=S_4$, within the specified
    bound.  The bound limits the size of the enumeration, not the
    Weyl group itself.
SeeAlso
  weylGroupElements
  parabolicCosetReps
  kostantLieAlgCohomology
  MaxDegree
///

doc ///
Key
  MaxDegree
Headline
  Exact homological or cohomological degree bound in infinite Weyl type
Description
  Text
    An option used by @TO kostantLieAlgCohomology@ and
    @TO bggResolution@.  Supply a nonnegative integer $d$ to compute
    every requested summand in degrees $0,\ldots,d$.  The bound is
    required when the relevant Weyl group or parabolic quotient is
    infinite.  Every Coxeter-length stratum in the stated degree range
    is computed completely.
  Text
    In @TO kostantLieAlgCohomology@ the degree is cohomological degree in
    the parabolic Kostant formula.  In @TO bggResolution@ it is
    homological degree, equivalently Coxeter length.  For finite Weyl
    type, @TT "MaxDegree => null"@ returns the complete result; a finite
    value returns only the stated initial degree range.
    @TO MaxElements@ separately limits how many elements may be retained
    during the calculation.
  Example
    A = matrix{{2,-2},{-2,2}};
    H = kostantLieAlgCohomology(A, {1}, {0,0}, MaxDegree => 3);
    sort keys H
  Text
    The keys are exactly $0,1,2,3$, and the corresponding modules are
    $V_{A_1}(0),V_{A_1}(2),V_{A_1}(4),V_{A_1}(6)$.
SeeAlso
  kostantLieAlgCohomology
  bggResolution
  MaxElements
  parabolicCosetReps
///

doc ///
Key
  weylLengthGenerating
  (weylLengthGenerating,Matrix)
Headline
  Length generating function of the Weyl group
Usage
  P = weylLengthGenerating A
Inputs
  A:Matrix
    a finite-type Cartan matrix
Outputs
  P:RingElement
    the polynomial $\sum_{w\in W}q^{\ell(w)}$ in $\mathbb Z[q]$
Description
  Text
    The coefficient of $q^k$ counts Weyl-group elements of Coxeter length
    $k$.  This is the Poincaré polynomial of $W$.  If $d_1,\ldots,d_r$
    are the degrees of the basic Weyl-group invariants, then
    $$P(q)=\prod_{i=1}^r [d_i]_q,$$
    where $[k]_q = 1 + q + \cdots + q^{k-1}$.
    Its value at $q=1$ is @TO weylGroupOrder@.  The result is an ordinary
    polynomial, so commands such as @TT "factor"@, @TT "coefficient"@,
    @TT "degree"@, and @TT "sub"@ apply directly.
  Example
    P = weylLengthGenerating cartanA 4;
    P
    factor P
    weylLengthGenerating cartanB 3
  Text
    For $A_3$, the first polynomial is
    $$1+3q+5q^2+6q^3+5q^4+3q^5+q^6,$$
    and its factorization is
    $$(q+1)^2(q^2+q+1)(q^2+1)=[2]_q[3]_q[4]_q.$$
    The factor command gives the irreducible integer-coefficient factors;
    splitting each $q$-integer into cyclotomic factors gives the same answer.
  Text
    The maximum length is always $|\Phi^+|$ (length of the longest
    element), and the polynomial is palindromic:
    $\#\{w : \ell(w) = k\}=\#\{w : \ell(w) = |\Phi^+|-k\}$
    for every $k$.  Its degree and value at $1$ recover the number of
    positive roots and the Weyl-group order:
  Example
    P = weylLengthGenerating cartanD 4;
    q = (ring P)_0;
    first degree P                  -- = |Phi^+(D_4)| = 12
    sub(P,q=>1)                     -- = |W(D_4)| = 192
    weylGroupOrder cartanD 4
  Text
    For $A_{n-1}$ the coefficients count permutations in $S_n$ by their
    number of inversions.  For $A_3$ (i.e.\ $S_4$) they are
    $1,3,5,6,5,3,1$:
  Example
    P = weylLengthGenerating cartanA 4;
    q = (ring P)_0;
    coefficient(q^3,P)
    apply(toList(0..first degree P),k -> coefficient(q^k,P))
  Text
    There are $6$ elements of length $3$.  The final list gives all
    coefficients in ascending powers of $q$.
SeeAlso
  weylGroupOrder
  weylGroupElements
///

doc ///
Key
  reducedWord
  (reducedWord,WeylElement)
  (reducedWord,Matrix,List)
Headline
  Reduced word for the Weyl-group element of a given expression
Usage
  v = reducedWord(A, w)
  v = reducedWord u
Inputs
  A:Matrix
    a generalized Cartan matrix
  w:List
    any (not necessarily reduced) word in 1-based simple-reflection indices
Outputs
  v:List
    a reduced word representing the same Weyl-group element as @TT "w"@
Description
  Text
    Computes the matrix of @TT "w"@ and repeatedly removes a right
    descent.  Each step lowers Coxeter length by one, so the resulting
    word is reduced.  No Weyl-group enumeration is performed.
    If @TT "u"@ is a @TO WeylElement@, the one-argument form returns
    its stored reduced word, in 1-based node positions.
  Example
    A = cartanA 3
    reducedWord(A, {1,2,1,2})       -- length 2
    reducedWord(A, {1,2,1,2,1,2})   -- identity
  Text
    The first reduced word is $(2,1)$, representing $s_2s_1$: the braid relation reduces
    $s_1s_2s_1s_2$ to that product. The second reduced word is the empty word, because
    $(s_1s_2)^3=1$ in type $A_2$.

    Braid-equivalent expressions produce the same element.  The
    fixed choice of right descent returns the same reduced word for the
    same matrix, although a Weyl element generally has many reduced
    expressions:
  Example
    reducedWord(A, {1,2,1})
    reducedWord(A, {2,1,2})         -- braid: s_1 s_2 s_1 = s_2 s_1 s_2
    weylElementMatrix(A, {1,2,1}) == weylElementMatrix(A, {2,1,2})
  Text
    The length of the reduced word matches @TO weylLength@, and for
    the longest element equals the number of positive roots:
  Example
    w0 := longElement cartanB 3
    weylLength(cartanB 3, w0)            -- 9 = |Phi^+(B_3)|
    length reducedWord(cartanB 3, w0 | w0)  -- identity: empty
SeeAlso
  weylLength
  weylInversionData
  weylElementMatrix
///

doc ///
Key
  leftDescentSet
  (leftDescentSet,WeylElement)
  (leftDescentSet,Matrix,List)
Headline
  Left descent set of a Weyl-group element
Usage
  D = leftDescentSet(A, w)
  D = leftDescentSet u
Inputs
  A:Matrix
    a generalized Cartan matrix
  w:List
    a word in 1-based simple-reflection indices
Outputs
  D:List
    the 1-based indices $i$ such that $\ell(s_i w) < \ell(w)$
Description
  Text
    Equivalently, $i\in D$ iff $w^{-1}(\alpha_i)$ is a negative root.
    The one-argument form takes a @TO WeylElement@ and returns the
    same set of 1-based node positions, including when its datum has
    custom node labels.
  Example
    A = cartanA 3
    w0 = longElement A
    leftDescentSet(A, w0)   -- full set {1,2} since w_0 is longest
    leftDescentSet(A, {1})  -- singleton {1}
  Text
    The outputs $\{1,2\}$ and $\{1\}$ say that multiplying the $A_2$ longest element on
    the left by either simple reflection lowers its length, whereas multiplying $s_1$ on
    the left lowers length only when the multiplier is $s_1$.

    A nonidentity element need not have every simple reflection in its
    left descent set.  For example, in $W(A_3)=S_4$
    (i.e.\ @TT "cartanA 4"@), the
    element $w = s_2s_1s_3$ has left descent set $\{2\}$:
  Example
    leftDescentSet(cartanA 4, {2,1,3})
  Text
    For the identity (empty word) the descent set is empty:
  Example
    leftDescentSet(A, {})
SeeAlso
  rightDescentSet
  weylLength
  bruhatLessOrEqual
///

doc ///
Key
  rightDescentSet
  (rightDescentSet,WeylElement)
  (rightDescentSet,Matrix,List)
Headline
  Right descent set of a Weyl-group element
Usage
  D = rightDescentSet(A, w)
  D = rightDescentSet u
Inputs
  A:Matrix
    a generalized Cartan matrix
  w:List
    a word in 1-based simple-reflection indices
Outputs
  D:List
    the 1-based indices $i$ such that $\ell(w s_i) < \ell(w)$
Description
  Text
    Equivalently, $i\in D$ iff $w(\alpha_i)$ is a negative root.
    The one-argument form takes a @TO WeylElement@ and returns the
    same set of 1-based node positions, including when its datum has
    custom node labels.
  Example
    A = cartanA 3
    w0 = longElement A
    rightDescentSet(A, w0)   -- full set {1,2} since w_0 is longest
  Text
    The returned set $\{1,2\}$ says that both $w_0s_1$ and $w_0s_2$ have length $2$, one
    less than the $A_2$ longest element. These integers identify simple-reflection
    positions, not root coefficients.

    The right descent set of the reverse word is the left descent
    set of $w^{-1}$; more directly, reversing a reduced word gives
    the inverse element and swaps the two descent sets.  For
    example in $B_3$:
  Example
    B = cartanB 3
    w = {1,3,2,1}
    rightDescentSet(B, w)
    leftDescentSet(B, reverse w)
  Text
    The identity has empty descent set on both sides:
  Example
    (leftDescentSet(A, {}), rightDescentSet(A, {}))
SeeAlso
  leftDescentSet
  weylLength
  bruhatLessOrEqual
///

doc ///
Key
  fundamentalToPartition
  (fundamentalToPartition, Matrix, List)
Headline
  Convert Dynkin labels to classical partition coordinates
Usage
  lambda = fundamentalToPartition(A, m)
Inputs
  A:Matrix
    a connected Cartan matrix of classical type $A_n$, $B_n$, $C_n$,
    or $D_n$, in any simultaneous node permutation
  m:List
    integral Dynkin labels $(m_1,\dots,m_n)$; they may describe an
    arbitrary weight, and are dominant exactly when all labels are nonnegative
Outputs
  lambda:List
    the corresponding classical $\varepsilon$-coordinates, which form a
    partition (with the stated spin conventions) when the weight is dominant
Description
  Text
    The same function converts Dynkin labels to partition coordinates in
    types $A$, $B$, $C$, and $D$.  The type and node order are determined
    from @TT "A"@, including after a simultaneous permutation of its rows
    and columns.  The function reports an error if @TT "A"@ is not a connected classical Cartan
    matrix, if the label vector has the wrong length, or if a label is not an
    integer.  Negative labels are allowed, so the function can also be used
    to display the nondominant weights inside a representation.
  Example
    fundamentalToPartition(cartanA 4, {1,0,1})     -- sl_4 adjoint
    fundamentalToPartition(cartanB 3, {0,0,1})     -- so_7 spin
    fundamentalToPartition(cartanC 3, {1,0,1})     -- sp_6 mixed
    fundamentalToPartition(cartanD 4, {0,0,1,0})   -- so_8 half-spin
  Text
    The four tuples are $(2,1,1,0)$, $(1/2,1/2,1/2)$, $(2,1,1)$, and
    $(1/2,1/2,1/2,-1/2)$. They express the same four input weights in classical Euclidean
    coordinates. In particular the $B_3$ and $D_4$ half-integer tuples are spin weights;
    the negative last $D_4$ coordinate distinguishes one half-spin representation from
    the other.

    @BOLD "Type A."@  For $A_n$, the normalized representative has
    $\lambda_{n+1}=0$ and
    $\lambda_i=m_i+\lambda_{i+1}$.  Thus partitions differing by a
    global shift determine the same $\mathfrak{sl}_{n+1}$ weight, and
    this function chooses the representative with final part zero.
    The adjoint has labels $(1,0,\ldots,0,1)$ and partition
    $(2,1,\ldots,1,0)$, while $\rho=(1,\ldots,1)$ becomes
    $(n,n-1,\ldots,1,0)$.
  Example
    fundamentalToPartition(cartanA 4, {1,1,0})
    fundamentalToPartition(cartanA 4, {2,1,1})
    fundamentalToPartition(cartanA 5, {1,0,0,1})   -- adjoint of sl_5
    fundamentalToPartition(cartanA 5, {1,1,1,1})   -- rho of A_4
    mA = {2,0,1,0}
    partitionToFundamental(cartanA 5, fundamentalToPartition(cartanA 5, mA))
  Text
    @BOLD "Type B."@  For $B_n$,
    $\lambda_n=m_n/2$ and
    $\lambda_i=m_i+\lambda_{i+1}$.  The entries are integers when
    $m_n$ is even and half-integers when $m_n$ is odd; exact rational
    numbers such as @TT "1/2"@ are returned for spin weights.
  Example
    fundamentalToPartition(cartanB 3, {1,0,0})     -- vector
    fundamentalToPartition(cartanB 3, {0,1,0})     -- adjoint
    fundamentalToPartition(cartanB 3, {0,0,1})     -- spin
    fundamentalToPartition(cartanB 3, {2,1,0})
  Text
    @BOLD "Type C."@  For $C_n$,
    $\lambda_n=m_n$ and $\lambda_i=m_i+\lambda_{i+1}$.  Dominant
    integral weights therefore give ordinary integer partitions; there
    is no half-integer spin case.
  Example
    fundamentalToPartition(cartanC 3, {1,0,0})
    fundamentalToPartition(cartanC 3, {1,1,0})
    fundamentalToPartition(cartanC 3, {2,0,0})
    fundamentalToPartition(cartanC 3, {1,1,1})
  Text
    @BOLD "Type D."@  For $D_n$,
    $$\lambda_{n-1}=(m_{n-1}+m_n)/2,\qquad
      \lambda_n=(m_n-m_{n-1})/2,$$
    and $\lambda_i=m_i+\lambda_{i+1}$ for $i\leq n-2$.  Half-integral
    output records a spin weight, and a negative final coordinate
    distinguishes the second half-spin representation.
  Example
    fundamentalToPartition(cartanD 4, {1,0,0,0})   -- vector
    fundamentalToPartition(cartanD 4, {0,1,0,0})   -- adjoint
    fundamentalToPartition(cartanD 4, {0,0,0,1})   -- first half-spin
    fundamentalToPartition(cartanD 4, {0,0,1,0})   -- second half-spin
    fundamentalToPartition(cartanD 4, {1,0,0,1})
SeeAlso
  partitionToFundamental
  WeightFormat
  weylDimension
///

doc ///
Key
  partitionToFundamental
  (partitionToFundamental, Matrix, List)
Headline
  Convert classical partition coordinates to Dynkin labels
Usage
  m = partitionToFundamental(A, lambda)
Inputs
  A:Matrix
    a connected Cartan matrix of classical type $A_n$, $B_n$, $C_n$,
    or $D_n$, in any simultaneous node permutation
  lambda:List
    a partition-form dominant weight (possibly with half-integer
    entries in types $B$ or $D$; possibly with negative last entry
    in type $D$)
Outputs
  m:List
    the Dynkin labels $(m_1,\dots,m_n)$
Description
  Text
    The same function converts partition coordinates to Dynkin labels in
    types $A$, $B$, $C$, and $D$, and is inverse to
    @TO fundamentalToPartition@ on dominant weights.  The type and node
    order are determined from @TT "A"@.
    Trailing zeros may be omitted.  The dominance and integrality rules
    below are checked before a result is returned.
  Example
    partitionToFundamental(cartanA 4, {2,1,0,0})     -- sl_4 partition
    partitionToFundamental(cartanB 3, {1/2,1/2,1/2}) -- so_7 spin
    partitionToFundamental(cartanC 3, {2,1,0})
    partitionToFundamental(cartanD 4, {1/2,1/2,1/2,-1/2})
  Text
    The outputs are $(1,1,0)$, $(0,0,1)$, $(1,1,0)$, and $(0,0,1,0)$ in their respective
    Dynkin bases. Thus the $B_3$ spin partition becomes the fundamental highest weight at
    node $3$, while the final $D_4$ tuple is the half-spin highest weight at its node
    $3$. The tuples from different Lie types belong to different weight spaces.

    @BOLD "Type A."@  A partition
    $\lambda_1\geq\cdots\geq\lambda_{n+1}\geq0$ maps to
    $$m_i=\lambda_i-\lambda_{i+1}\qquad(1\leq i\leq n).$$
    The standard, adjoint, and Weyl-vector examples for
    $\mathfrak{sl}_4$ are $(1,0,0,0)\mapsto(1,0,0)$,
    $(2,1,1,0)\mapsto(1,0,1)$, and
    $(3,2,1,0)\mapsto(1,1,1)$.
  Example
    partitionToFundamental(cartanA 4, {2,1,0,0})
    partitionToFundamental(cartanA 4, {3,2,1,0})
    partitionToFundamental(cartanA 4, {1,0,0,0})
    partitionToFundamental(cartanA 4, {2,1,1,0})
  Text
    Both compositions are identities after the type-$A$ normalization
    that sets the final partition part to zero:
  Example
    fundamentalToPartition(cartanA 4, partitionToFundamental(cartanA 4, {4,2,1,0}))
    partitionToFundamental(cartanA 4, fundamentalToPartition(cartanA 4, {2,1,1}))
  Text
    The coordinate change is compatible with the Weyl dimension
    formula; these two calls both compute the dimension of the
    $\mathfrak{sl}_4$ adjoint representation:
  Example
    weylDimension(cartanA 4, {1,0,1})
    weylDimension(cartanA 4, {2,1,1,0}, WeightFormat => "Partition")
  Text
    @BOLD "Type B."@  A dominant $B_n$ weight satisfies
    $\lambda_1\geq\cdots\geq\lambda_n\geq0$, with entries either all
    integral or all half-integral, and
    $$m_i=\lambda_i-\lambda_{i+1}\ (i<n),\qquad
      m_n=2\lambda_n.$$
    The factor of two at the short final node turns spin partitions
    into integral Dynkin labels.
  Example
    partitionToFundamental(cartanB 3, {1,0,0})
    partitionToFundamental(cartanB 3, {1,1,0})
    partitionToFundamental(cartanB 3, {1/2,1/2,1/2})
  Text
    @BOLD "Type C."@  A dominant $C_n$ weight is an integer partition,
    and
    $$m_i=\lambda_i-\lambda_{i+1}\ (i<n),\qquad
      m_n=\lambda_n.$$
    Unlike type $B$, the final long node introduces no factor of two.
  Example
    partitionToFundamental(cartanC 3, {1,0,0})
    partitionToFundamental(cartanC 3, {2,0,0})
    partitionToFundamental(cartanC 3, {1,1,0})
    partitionToFundamental(cartanC 3, {2,1,0})
  Text
    @BOLD "Type D."@  A dominant $D_n$ weight satisfies
    $\lambda_1\geq\cdots\geq\lambda_{n-1}\geq|\lambda_n|$, with
    entries either all integral or all half-integral, and
    $$m_i=\lambda_i-\lambda_{i+1}\ (i\leq n-2),\quad
      m_{n-1}=\lambda_{n-1}-\lambda_n,\quad
      m_n=\lambda_{n-1}+\lambda_n.$$
    The sign of $\lambda_n$ distinguishes the two half-spin weights.
  Example
    partitionToFundamental(cartanD 4, {1,0,0,0})
    partitionToFundamental(cartanD 4, {1/2,1/2,1/2,1/2})
    partitionToFundamental(cartanD 4, {1/2,1/2,1/2,-1/2})
    partitionToFundamental(cartanD 4, {1,1,0,0})
SeeAlso
  fundamentalToPartition
  WeightFormat
  weylDimension
///

doc ///
Key
  specialCartanFromPartition
Headline
  Build a distinguished Cartan matrix for a parabolic from a partition
Usage
  A = specialCartanFromPartition(n, k, lambda)
Inputs
  n:ZZ
    the rank of the type-$A$ block
  k:ZZ
    the length of the auxiliary chain attached to node $1$
  lambda:List
    a partition, with Dynkin labels given by the same type-$A$ rule as
    @TO partitionToFundamental@
Outputs
  A:Matrix
    a $(k+n)\times(k+n)$ generalized Cartan matrix combining an
    $A_k$ chain (nodes $1,\dots,k$), an $A_n$ chain (nodes
    $k+1,\dots,k+n$), and couplings from the Dynkin labels of
    @TT "lambda"@
Description
  Text
    @TO specialCartanFromPartition@ assembles the generalized Cartan
    matrix attached to the given parabolic data.  In 1-based terms the
    auxiliary $A_k$ nodes are $1,\dots,k$ and the $A_n$ block is
    $k+1,\dots,k+n$.  For every nonzero Dynkin label $m_i$, node $1$
    is coupled to block node $k+i$ with
    $A_{1,k+i}=-1$ and $A_{k+i,1}=-m_i$.  The resulting matrix is the Cartan
    matrix of an extended Kac-Moody algebra associated to the
    partition.
  Example
    specialCartanFromPartition(3, 2, {2,1,0,0})
  Text
    The partition $(2,1,0,0)$ has Dynkin labels $(1,1,0)$.
    The output therefore has an auxiliary $A_2$ on nodes $1,2$ and
    an $A_3$ on nodes $3,4,5$, with node $1$ joined to nodes $3$ and
    $4$ by simple bonds.  Its entries are the pairings of these five
    simple roots and coroots, not the boxes of the partition.

SeeAlso
  partitionToFundamental
  fundamentalToPartition
  isCartanMatrix
///


doc ///
Key
  (allRoots,Matrix,ZZ)
Headline
  Roots of a finite, affine, or indefinite Cartan matrix up to a height cutoff
Usage
  R = allRoots(A, H)
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
  H:ZZ
    a positive height cutoff
Outputs
  R:Matrix
    a matrix whose rows are roots of absolute height at most $H$
Description
  Text
    The two-argument form extends @TO allRoots@ to general Kac-Moody
    types by truncating the infinite root system to roots of bounded
    height.  The finite-type single-argument form enumerates all
    roots without truncation.
  Example
    Ahyp = matrix{{2,-3},{-3,2}};
    allRoots(Ahyp, 3)
SeeAlso
  allRoots
  cartanPositiveRoots
  negativeRoots
  rootSnapshot
///

doc ///
Key
  (cartanPositiveRoots,Matrix,ZZ)
Headline
  Positive roots of a finite, affine, or indefinite Cartan matrix up to a height cutoff
Usage
  R = cartanPositiveRoots(A, H)
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
  H:ZZ
    a positive height cutoff
Outputs
  R:Matrix
    a matrix whose rows are the positive roots of height $\leq H$,
    each expressed in the basis of simple roots
Description
  Text
    The two-argument form extends @TO cartanPositiveRoots@ to
    infinite (affine or indefinite) root systems by truncating at a
    user-supplied height bound.  Each distinct root occurs once;
    root-space multiplicities are available from @TO rootSnapshot@.
  Example
    Ahyp = matrix{{2,-3},{-3,2}};
    cartanPositiveRoots(Ahyp, 3)
SeeAlso
  cartanPositiveRoots
  cartanPositiveRootsDegree
  allRoots
  rootSnapshot
///

doc ///
Key
  (negativeRoots,Matrix,ZZ)
Headline
  Negative roots of a finite, affine, or indefinite Cartan matrix up to a height cutoff
Usage
  R = negativeRoots(A, H)
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
  H:ZZ
    a positive height cutoff
Outputs
  R:Matrix
    a matrix whose rows are negative roots of height at least $-H$
Description
  Text
    The two-argument form extends @TO negativeRoots@ to affine and
    indefinite types by truncating at a user-supplied height bound
    $H$; it returns the negatives of all positive roots up to height
    $H$.
  Example
    Ahyp = matrix{{2,-3},{-3,2}};
    negativeRoots(Ahyp, 3)
SeeAlso
  negativeRoots
  allRoots
  cartanPositiveRoots
///

doc ///
Key
  (net,Irrep)
Headline
  Display an irreducible representation
Usage
  n = net V
Inputs
  V:Irrep
Outputs
  n:Net
    a short multi-line or single-line rendering of the irreducible
    of the form @TT "V(a1,...,an)"@, or
    @TT "V(a1,...) ** V(b1,...) ** ..."@ in the disconnected case
Description
  Text
    Macaulay2 uses this method whenever an @TO Irrep@ appears at top
    level or inside a Tally or HashTable.  The separator @TT "**"@
    distinguishes the Levi factors.
  Example
    leviHighestWeights(cartanD 4, 1, {2})
SeeAlso
  Irrep
  leviHighestWeights
  (toString,Irrep)
///

doc ///
Key
  (toString,Irrep)
Headline
  Plain-string representation of an Irrep
Usage
  s = toString V
Inputs
  V:Irrep
Outputs
  s:String
    the displayed name of the irreducible, e.g.\
    @TT "V(1,0,1)"@ or @TT "V(1) ** V(2,1)"@
Description
  Text
    Returns the displayed name of an @TO Irrep@ as text.  This name gives
    the highest weights, but does not specify the Cartan matrix, component
    types, or @TO WeightFormat@.  Keep the original representation
    together with these choices, or record them using @TO irrepData@,
    to use it in subsequent calculations.
  Example
    T = leviHighestWeights(cartanA 4, 1, {2});
    toString first keys T
SeeAlso
  Irrep
  leviHighestWeights
  (net,Irrep)
///

doc ///
Key
  toLieAlgebra
  (toLieAlgebra,CartanDatum)
  (toLieAlgebra,Realization)
  (toLieAlgebra,RootDatum)
  (toLieAlgebra,Matrix)
Headline
  Convert a finite-type Cartan matrix to a LieAlgebraRepresentations LieAlgebra
Usage
  L = toLieAlgebra C
Inputs
  C:Matrix
    a generalized Cartan matrix of finite type
Outputs
  L:Thing
    a @TT "LieAlgebra"@ object from the @TO "LieAlgebraRepresentations::LieAlgebraRepresentations"@
    package
Description
  Text
    Constructs the corresponding Lie algebra in Paul Zinn-Justin's
    @TO "LieAlgebraRepresentations::LieAlgebraRepresentations"@
    package.  That package provides representation-theoretic
    computations (characters, tensor decomposition, fusion products, branching
    rules, Gelfand-Tsetlin patterns) specialized to finite-type simple Lie
    algebras.
  Text
    A Cartan matrix constructed with @TO cartanA@, @TO cartanB@,
    @TO cartanE@, or another Cartan-matrix function can therefore be used for
    calculations in @TO "LieAlgebraRepresentations::LieAlgebraRepresentations"@.
    A @TO CartanDatum@, @TO Realization@, or @TO RootDatum@ is also
    accepted; the conversion uses its Cartan matrix, not its chosen
    realization or global character lattice.
  Text
    The companion package is loaded when this function is first called.
    The resulting object can be used in that package:
  Example
    C = cartanA 4;               -- sl_4 = A_3
    L = toLieAlgebra C
    class L
  Text
    The type LieAlgebra identifies the result as the $A_3$ Lie algebra
    $\mathfrak{sl}_4$ in LieAlgebraRepresentations.  It has the same
    abstract root system as the original Cartan matrix, although the
    companion package supplies its own representation objects.
Caveat
  Only finite-type Cartan matrices are accepted; affine and indefinite
  types will raise an error because
  @TO "LieAlgebraRepresentations::LieAlgebraRepresentations"@ does not
  support them.
SeeAlso
  toRootSystem
  cartanType
  isCartanMatrix
///

doc ///
Key
  toRootSystem
  (toRootSystem,CartanDatum)
  (toRootSystem,Realization)
  (toRootSystem,RootDatum)
  (toRootSystem,Matrix)
Headline
  Convert a finite-type Cartan matrix to a WeylGroups root system
Usage
  R = toRootSystem C
Inputs
  C:Matrix
    a generalized Cartan matrix of finite type
Outputs
  R:Thing
    a @TT "RootSystem"@ object from the @TO "WeylGroups::WeylGroups"@ package
Description
  Text
    Constructs the corresponding root system in the
    @TO "WeylGroups::WeylGroups"@ package.  That package
    supports Weyl-group combinatorics (Bruhat order, reduced words, coset
    representatives, and Hasse diagrams) for finite Weyl groups.
    Reducible finite Cartan matrices are accepted, including after a
    simultaneous permutation of their rows and columns.  The resulting
    root system has the same abstract type as the input, with the standard
    Dynkin labeling used by @TT "WeylGroups"@; any custom node order
    of the original matrix is not retained.
    A @TO CartanDatum@, @TO Realization@, or @TO RootDatum@ is also
    accepted, with the same Cartan-matrix conversion and labeling convention.
  Text
    The companion package is loaded when this function is first called.
    The resulting object can be used in that package:
  Example
    C = cartanB 3;
    R = toRootSystem C
    class R
  Text
    The type RootSystem identifies the result as the $B_3$ root system
    in WeylGroups.  Its Weyl group is the group of signed permutations
    of three coordinates, of order $2^3\cdot3!=48$.
Caveat
  Only finite-type generalized Cartan matrices whose components are
  recognized as crystallographic types $A$--$G$ are accepted.  Affine,
  indefinite, and nonsymmetrizable inputs are rejected.
SeeAlso
  toLieAlgebra
  cartanType
  isCartanMatrix
///

-- Root data, characters, geometry, category O, lambda operations, and crystals.

-------------------------------------------------------------------------
-- Infrastructure Documentation
-------------------------------------------------------------------------

-- Official documentation for the parent-aware representation-theoretic
-- infrastructure.  This file is loaded after beginDocumentation().

doc ///
Key
  "Root data and representations"
Headline
  Root data, formal characters, and compatible representation-theoretic constructions
Description
  Text
    A generalized Cartan matrix determines the simple-root pairings and the
    Coxeter relations of the Weyl group.  To work with representations, we
    also need to specify the root datum, a realization of the Cartan
    subalgebra, and the character and cocharacter lattices.  When the Cartan
    matrix is singular, Dynkin labels do not determine every coordinate of
    a weight.  Infinite-dimensional characters also require a completion of
    the group ring.  This section explains these choices and how they enter
    the calculations.
  Text
    Roots and weights.  The same coordinate list can describe weights in
    different root systems.  A @TO KMWeight@ therefore includes its
    @TO RootDatum@.  Addition, Weyl action, restriction, and character
    calculations require the appropriate common datum.  In particular,
    restriction to a Levi factor distinguishes an ambient weight from its
    restriction.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{1,1})
    C = finiteCharacter(D,lambda)
    (characterDimension C,
        characterCoefficient(C,kmWeight(D,{0,0})))
  Text
    The output @TT "(8, 2)"@ says that
    $\operatorname{ch}V(\omega _1+\omega _2)$ has total dimension $8$ and
    that its zero-weight space has dimension $2$.  This is the character of
    the adjoint representation of $\mathfrak{sl}_3$.
  Text
    Realizations and global forms.  The matrix controls the Lie
    algebra, while a root datum also controls integral character and
    cocharacter lattices.  The simply connected and adjoint forms can
    therefore accept different integral highest weights even when their
    Cartan matrices agree.
  Example
    Dsc = rootDatum("A3",GlobalForm=>"SimplyConnected");
    Dad = rootDatum("A3",GlobalForm=>"Adjoint");
    (integratesToGlobalForm kmWeight(Dsc,{1,0,0}),
     integratesToGlobalForm kmWeight(Dad,{1,0,0}),
     pModQInvariants Dsc)
  Text
    The two Boolean values say that the first fundamental weight is a
    character of the simply connected group but not of the adjoint group.
    The final table identifies
    $P/Q\cong\mathbb Z/4\mathbb Z$ for type $A_3$; this is the precise
    obstruction detected in the second calculation.
  Text
    Affine grade directions.  For a singular Cartan matrix, Dynkin
    labels do not determine a full weight.  In affine type the null root
    $\delta$ pairs to zero with every simple coroot, so $\lambda$ and
    $\lambda-\delta$ have identical Dynkin labels but occur in different
    grades.  Full realization coordinates retain this information.
  Example
    Da = rootDatum cartanAffineA 1;
    delta = affineNullRoot Da;
    d = affineDerivation Da;
    Lambda0 = affineWeightFromDynkinLabels(Da,{1,0},0,d);
    Lambda0minusDelta = Lambda0-toWeight delta;
    (datumCoordinates Lambda0,
     datumCoordinates Lambda0minusDelta,
     realizationCoordinates Lambda0,
     realizationCoordinates Lambda0minusDelta)
  Text
    Both Dynkin-label lists are $(1,0)$, whereas the realization coordinates
    are $(1,0,0)$ and $(1,0,-1)$.  Mathematically, the output distinguishes
    $\Lambda _0$ from $\Lambda _0-\delta$ rather than identifying two weight
    spaces that belong to different affine grades.
  Text
    Exact and completed characters.  A finite character is an exact
    element of the integral group ring.  A Verma character or an integrable
    Kac--Moody character is generally an element of a completion and is
    computed through a specified root depth.  Use @TO isExactCharacter@ to
    distinguish a complete character from a truncated expansion.
    @TO characterDimension@ and @TO decomposeCharacter@ require a complete
    character.
  Example
    M = integrableHighestWeightModule(Da,Lambda0);
    C2 = moduleCharacter(M,Depth=>2);
    C2
    (isExactCharacter C2,
     characterCoefficient(C2,Lambda0),
     characterCoefficient(C2,Lambda0-toWeight delta))
  Text
    The exponents are the complete realization coordinates
    $(1,0,0)$, $(-1,2,-1)$, and $(1,0,-1)$, so the terms at
    $\Lambda _0$ and $\Lambda _0-\delta$ remain visibly distinct.
    The root-depth bound printed beside the expansion means
    that every coefficient through depth $2$ is known.  The output
    @TT "(false, 1, 1)"@ confirms that both
    $e^{\Lambda _0}$ and $e^{\Lambda _0-\delta}$ occur with coefficient $1$
    in the computed range.
    In affine fundamental weights, the three terms mean
    $$e^{\Lambda_0}+e^{-\Lambda_0+2\Lambda_1-\delta}
      +e^{\Lambda_0-\delta}.$$
    The middle weight is $\Lambda_0-\alpha_0$, since
    $\alpha_0=2\Lambda_0-2\Lambda_1+\delta$.
    Every omitted term lies at root depth greater than $2$.
  Text
    Independent algorithms.  Freudenthal recursion and Kostant's
    alternating formula compute the same finite-type multiplicities by
    different formulas.  Using both gives an independent check of a
    multiplicity calculation.
  Example
    A = cartanA 3;
    (weightMultiplicity(A,{1,1},{0,0},Strategy=>"Freudenthal"),
     weightMultiplicity(A,{1,1},{0,0},Strategy=>"Kostant"))
  Text
    Both entries are $2$: the zero-weight multiplicity in the adjoint
    representation of $\mathfrak{sl}_3$.
  Text
    Related constructions.  Weyl actions, Demazure operators,
    Borel--Weil--Bott calculations, reductive Levi restriction, and category
    $\mathcal O$ calculations use the same @TO RootDatum@.  Many commands
    also accept a Cartan matrix and coordinate lists directly.
    @TO toLieAlgebra@ and @TO toRootSystem@ give access to the finite-type
    constructions in @TT "LieAlgebraRepresentations"@ and @TT "WeylGroups"@.
Subnodes
  "Cartan data, realizations, and global lattices"
  "Roots, weights, and pairings"
  "Weyl groups and standard parabolics"
  "Finite structural invariants"
  "Affine structural data"
  "Formal and completed characters"
  "Reductive Levi restriction"
  "Finite highest-weight crystals"
  "Independent multiplicity algorithms"
  "Borel-Weil-Bott and Demazure operators"
  "Kazhdan-Lusztig theory and the BGG resolution"
  "Lambda operations and representation indices"
  "Saved results and repeated calculations"
SeeAlso
  CartanDatum
  RootDatum
  KMWeight
  KMWeylGroup
  StandardParabolic
  FormalCharacter
  "Working with formal characters"
  KMRepresentation
  affineNullRoot
  finiteCharacter
  reductiveLeviDatum
  kostantWeightMultiplicity
  borelWeilBott
  kazhdanLusztigPolynomial
///

doc ///
Key
  "Cartan data, realizations, and global lattices"
  (symbol ==,CartanDatum,CartanDatum)
  (symbol ==,Realization,Realization)
  (symbol ==,RootDatum,RootDatum)
Headline
  Construct Cartan data, realizations, and character lattices
Usage
  D = cartanDatum A
  D = cartanDatum("A3",NodeLabels=>labels)
  R = standardRealization D
  R = realization(D,Rmat,Cmat)
  X = rootDatum R
  X = rootDatum(A, GlobalForm=>form)
  X = rootDatum(R,CharacterLattice=>B)
  datumCartanMatrix X
  characterLatticeBasis X
  cocharacterLatticeBasis X
  pModQInvariants X
Inputs
  A:Matrix
    a generalized Cartan matrix
  form:String
    @TT "\"SimplyConnected\""@, @TT "\"Adjoint\""@, or
    @TT "\"Unspecified\""@
  labels:List
    distinct node labels, in Cartan-matrix order
  Rmat:Matrix
    one simple-root row per node, in a rational basis of $\mathfrak h^*$
  Cmat:Matrix
    one simple-coroot row per node, in the dual basis of $\mathfrak h$
  B:Matrix
    an invertible rational matrix whose rows generate the chosen character lattice
Outputs
  D:CartanDatum
    Cartan data, including type, components, rank, corank, and node labels
  R:Realization
    simple roots and coroots in dual rational bases
  X:RootDatum
    a realization together with dual character and cocharacter lattices
Description
  Text
    If $A$ has size $r$ and corank $k$, @TO standardRealization@ constructs
    a minimal realization of dimension $r+k=2r-\operatorname{rank}A$.
    Rows of the simple-root matrix $R$ lie in a basis of $\mathfrak h^*$,
    rows of the simple-coroot matrix $C$ lie in the dual basis of
    $\mathfrak h$, and the checked identity is
    $$C R^{\mathsf T}=A,\qquad
      \langle\alpha_j,\alpha_i^\vee\rangle=a_{ij}.$$
    @TO datumCartanMatrix@ returns the original generalized Cartan matrix
    from any of these three descriptions.
    @TO cartanDatum@ also extracts the Cartan datum from a realization or
    root datum, and returns an existing Cartan datum unchanged.  Node labels
    cannot be changed on an existing datum; choose them when constructing
    the datum from a matrix or type name.  @TO standardRealization@ accepts
    a matrix or Cartan datum.  On a root datum it returns the realization
    already specified, which need not be minimal; @TT "realization X"@
    returns that same realization.
    Equality of Cartan data compares the Cartan matrix, node labels, and
    labeling convention.
    Equality of realizations also compares the specified root and coroot
    coordinates.  Equality of root data additionally compares the selected
    character and cocharacter lattices and their global-form description.
    Constructing the same data again gives equal objects; having the same
    Cartan matrix alone is not enough to identify different realizations
    or lattice choices.
  Example
    D = cartanDatum("A3",NodeLabels=>{"left","middle","right"});
    R = standardRealization D;
    X = rootDatum(R,GlobalForm=>"SimplyConnected");
    (D,R,X,datumCartanMatrix X)
  Text
    This is the root datum of simply connected type $A_3$.  The three node
    names label $\alpha_1,\alpha_2,\alpha_3$ without changing their order,
    and the displayed Cartan matrix recovers their pairings.  Type strings
    @TT "\"A3\""@, @TT "\"B2\""@, and the other finite Cartan--Killing
    names are accepted; arbitrary finite, affine, or indefinite matrices are
    accepted directly.
    A type name uses the Lie-theoretic rank: @TT "\"A3\""@ has three
    nodes, and is the same type as the older matrix constructor
    @TT "cartanA 4"@.  @TO rootDatum@ accepts a type name, matrix, Cartan
    datum, or realization.  A matrix, type name, or Cartan datum uses its
    standard realization; a supplied realization is retained.
  Text
    @TT "GlobalForm=>\"SimplyConnected\""@ uses the character lattice $P$
    and cocharacter lattice $Q^\vee$; @TT "\"Adjoint\""@ uses $Q$ and
    $P^\vee$.  @TT "\"Unspecified\""@ records that no integrality claim is
    being made.  The @TT "CharacterLattice"@ option accepts an invertible
    matrix whose rows are a basis of a custom lattice $X\subset\mathfrak
    h^*$; construction verifies that all simple roots lie in $X$ and all
    simple coroots lie in its perfect dual $Y$.
    The default is @TT "GlobalForm=>\"SimplyConnected\""@.
    Supplying @TO CharacterLattice@ instead specifies the custom lattice,
    independently of the @TO GlobalForm@ option.  If its row-basis matrix
    is $B$, the dual row-basis matrix is $(B^{-1})^{\mathsf T}$.
    @TO characterLatticeBasis@ and @TO cocharacterLatticeBasis@ return
    these matrices in realization coordinates; both return @TT "null"@
    for the unspecified form.  In a realization with additional Cartan
    directions, the simply connected and adjoint choices extend the
    simple-coroot or simple-root rows to a full basis.  These additional
    lattice directions belong to the chosen realization, rather than
    being determined by the Cartan matrix alone.
  Example
    Dsc = rootDatum("A3",GlobalForm=>"SimplyConnected");
    (characterLatticeBasis Dsc,
     cocharacterLatticeBasis Dsc,
     pModQInvariants Dsc)
  Text
    The last table is the Smith-normal-form description of $P/Q$.  In this
    example its torsion invariant is $4$, so $P/Q\cong\mathbb Z/4\mathbb Z$.
    For singular data the quotient may also have a nonzero free rank.
    More precisely, @TO pModQInvariants@ computes the cokernel of
    $A:\mathbb Z^r\longrightarrow\mathbb Z^r$, where the target records
    integral Dynkin labels.  Its entries @TT "\"smithDiagonal\""@,
    @TT "\"torsionInvariants\""@, and @TT "\"freeRank\""@ describe
    $$\mathbb Z^{\text{freeRank}}\oplus
      \bigoplus_{d\in\text{torsionInvariants}}\mathbb Z/d\mathbb Z.$$
    @TT "\"order\""@ is the order when this group is finite, and is
    @TT "null"@ otherwise.  In finite type this is the usual $P/Q$.
    In singular type this calculation concerns Dynkin-label classes:
    it does not include extra realization directions invisible to those
    labels.  The command accepts a matrix, Cartan datum, realization,
    or root datum, and does not depend on the chosen global form.
  Text
    A custom realization is constructed by @TT "realization(D,R,C)"@.  It
    must have independent simple-root and simple-coroot rows and must satisfy
    $CR^{\mathsf T}=A$ exactly over $\mathbb Q$.
    @TT "realization(A,R,C)"@ also accepts the Cartan matrix directly.
  Example
    A1 = cartanDatum "A1";
    R1 = realization(A1,matrix(QQ,{{4}}),matrix(QQ,{{1/2}}));
    datumCartanMatrix R1
  Text
    The output is the $A_1$ matrix $(2)$ because
    $(1/2)(4)=2$.  The realization changes coordinates, not the defining
    Cartan pairing.
SeeAlso
  "Roots, weights, and pairings"
  affineFullWeight
  affineDerivation
  affineWeightFromDynkinLabels
  affineDegree
  symmetrizer
  cartanData
///

doc ///
Key
  "Roots, weights, and pairings"
  (symbol *,KMCoroot,ZZ)
  (symbol *,KMCoweight,ZZ)
  (symbol *,KMRoot,ZZ)
  (symbol *,KMWeight,ZZ)
  (symbol *,ZZ,KMCoroot)
  (symbol *,ZZ,KMCoweight)
  (symbol *,ZZ,KMRoot)
  (symbol *,ZZ,KMWeight)
  (symbol +,KMCoroot,KMCoroot)
  (symbol +,KMCoweight,KMCoweight)
  (symbol +,KMRoot,KMRoot)
  (symbol +,KMWeight,KMWeight)
  (symbol -,KMCoroot)
  (symbol -,KMCoroot,KMCoroot)
  (symbol -,KMCoweight)
  (symbol -,KMCoweight,KMCoweight)
  (symbol -,KMRoot)
  (symbol -,KMRoot,KMRoot)
  (symbol -,KMWeight)
  (symbol -,KMWeight,KMWeight)
  (symbol ==,KMCoroot,KMCoroot)
  (symbol ==,KMCoweight,KMCoweight)
  (symbol ==,KMRoot,KMRoot)
  (symbol ==,KMWeight,KMWeight)
Headline
  Roots, coroots, weights, and coweights of a specified root datum
Usage
  alpha = kmRoot(D,a)
  h = kmCoroot(D,b)
  lambda = kmWeight(D,l)
  eta = kmCoweight(D,m)
  weightBasis D
  coweightBasis D
  pairing(lambda,h)
  lambda = weightFromRealizationCoordinates(D,x)
  eta = coweightFromRealizationCoordinates(D,y)
  parentDatum lambda
  datumCoordinates lambda
  coordinateBasis lambda
  realizationCoordinates lambda
  toWeight alpha
  toCoweight h
  weightToRootCoordinates lambda
  coweightToCorootCoordinates eta
  toRoot lambda
  toCoroot eta
  isRootLatticeWeight lambda
  isCorootLatticeCoweight eta
  integratesToGlobalForm lambda
  integratesCoweightToGlobalForm eta
  toLegacyList lambda
  toLegacyCartanMatrix D
  datumFromLegacyCartan A
  rootFromLegacy(D,a)
  corootFromLegacy(D,b)
  weightFromLegacy(D,l)
  coweightFromLegacy(D,m)
Inputs
  D:RootDatum
    the common root datum
  a:List
    integral simple-root coordinates
  b:List
    integral simple-coroot coordinates
  l:List
    rational Dynkin labels $\langle\lambda,\alpha_i^\vee\rangle$
  m:List
    rational evaluations $\langle\alpha_i,\eta\rangle$
  x:List
    a complete rational weight vector in the realization basis
  y:List
    a complete rational coweight vector in the dual realization basis
  A:Matrix
    a generalized Cartan matrix
Description
  Text
    @TO KMRoot@ and @TO KMCoroot@ use the free simple-root and
    simple-coroot bases.  @TO KMWeight@ uses Dynkin labels and
    @TO KMCoweight@ uses simple-root evaluations.  Addition, subtraction,
    scalar multiplication, conversion, and pairing require the same root
    datum; a calculation cannot add an $A_2$ weight to a $B_2$
    weight merely because both are represented by two numbers.
    The four constructors accept a list with one entry per Cartan node.
    With a @TO RootDatum@ as first argument, they also accept a one-row
    or one-column matrix.  A Cartan datum or Cartan matrix can be supplied
    with a coordinate list to construct an element in a new root datum.
    For several elements intended to interact, first construct one root
    datum $D$ and use it in each constructor.  Compatibility compares the
    specified root data, including the realization, node labels, and
    lattices; it is not merely a check on the length of the tuples.
    Arithmetic is defined within each of the four kinds of elements:
    addition, subtraction, negation, and multiplication by an integer on
    either side.  A nonintegral scalar is not accepted by these arithmetic
    operations, even though weight and coweight constructors allow rational
    coordinates.  @TO parentDatum@ recovers the specified root datum.
    Equality compares the kind of element, its root datum, and its
    coordinates.  Complete weights with equal Dynkin labels but different
    realization vectors are distinct.  An incomplete Dynkin-label class
    is not equal to a complete lift, even when their labels agree; the
    same principle applies to coweights.
  Example
    D = rootDatum "A2";
    alpha1 = kmRoot(D,{1,0});
    alpha2vee = kmCoroot(D,{0,1});
    omega1 = kmWeight(D,{1,0});
    (alpha1,alpha2vee,omega1)
    (pairing(alpha1,alpha2vee),pairing(omega1,alpha2vee),
     datumCoordinates toWeight alpha1)
  Text
    The three coordinate tuples $(1,0)$, $(0,1)$, and $(1,0)$ represent
    $\alpha_1$, $\alpha_2^\vee$, and $\omega_1$, respectively.
    Root and coroot tuples give simple-root and simple-coroot
    coefficients; the weight tuple gives Dynkin labels.
    The second output is $(-1,0,(2,-1))$.  In formulas,
    $\langle\alpha_1,\alpha_2^\vee\rangle=-1$,
    $\langle\omega_1,\alpha_2^\vee\rangle=0$, and
    $\alpha_1=2\omega_1-\omega_2$.
    Pairing accepts a weight and coroot, a root and coweight, a root and
    coroot, or a weight and coweight, in that order.  For root coefficients
    $a$, coroot coefficients $b$, weight labels $l$, and coweight
    evaluations $m$, the first three values are respectively
    $\sum_i l_i b_i$, $\sum_i a_i m_i$, and $b^{\mathsf T}Aa$.
    Pairing a weight with a coweight uses their complete realization
    vectors and therefore requires both vectors to be determined.
  Text
    Coordinate tuples always follow the Cartan-matrix order, including
    in affine type.  The traditional affine-root name $\alpha_0$ refers
    to the affine node; it does not change the order of the tuple.
    @TO affineTypeData@ identifies the affine node, and
    @TO dynkinDiagram@ shows the node arguments used by the commands.
    Explicitly chosen @TO NodeLabels@ remain the labels used in the
    diagram and in node arguments.
  Text
    When the realization has no additional Cartan directions, Dynkin
    labels determine the complete weight.  Otherwise they determine only
    a class modulo the annihilator of the simple coroots.  This occurs in
    singular type and also in nonminimal finite-type realizations.  Use
    @TO weightFromRealizationCoordinates@ and
    @TO coweightFromRealizationCoordinates@ whenever the missing direction
    matters.  @TO realizationCoordinates@ requires a complete weight;
    Dynkin labels alone do not specify a lift in these realizations.
    Complete weights and coweights then print their full realization
    coordinate tuples.  @TO datumCoordinates@ still returns Dynkin labels
    or simple-root evaluations; @TO coordinateBasis@ describes that list,
    while @TO realizationCoordinates@ returns the complete vector.
    A complete weight is printed as its Dynkin tuple when these labels
    determine it, and as its full realization tuple when additional
    directions are needed.  A class of weights is marked modulo the
    annihilator of the simple coroots, so it cannot be mistaken for a
    complete weight.
    The complete-vector constructors require $\dim\mathfrak h$ entries,
    supplied as a list or a one-row or one-column matrix.  Addition of a
    complete weight and an incomplete Dynkin-label class is not defined;
    one must first specify a complete lift of the class.  The same
    distinction applies to coweights.
  Example
    Da = rootDatum cartanAffineA 1;
    labelClass = kmWeight(Da,{1,0});
    full0 = weightFromRealizationCoordinates(Da,{1,0,0});
    full1 = weightFromRealizationCoordinates(Da,{1,0,1});
    (labelClass,full0,full1,full0 == full1)
  Text
    The first object denotes only the Dynkin-label class $(1,0)$, written
    modulo $\operatorname{ann}(\mathfrak h')$, where
    $\mathfrak h'$ is the span of the simple coroots.  The next two
    tuples, $(1,0,0)$ and $(1,0,1)$, represent $\Lambda_0$ and
    $\Lambda_0+\delta$: distinct complete weights with the same labels.
    The final @TT "false"@ confirms that equality
    distinguishes these weights.
  Example
    delta = affineNullRoot Da;
    (datumCoordinates delta,realizationCoordinates delta,
     datumCoordinates toWeight delta,realizationCoordinates toWeight delta)
  Text
    The Dynkin labels of $\delta$ are $(0,0)$, while its realization vector is
    nonzero.  Thus conversion from a root to a weight preserves
    the full affine direction.
  Text
    @TO weightBasis@ returns an ordered basis of complete weights.
    In a minimal affine realization it consists of the
    fundamental weights $\Lambda_i$, normalized by $\Lambda_i(d)=0$ for
    $d=$ @TT "affineDerivation D"@, followed by $\delta$.
    @TO coweightBasis@ gives the corresponding choice for coweights.
    Their realization coordinates specify these bases exactly:
  Example
    weightBasis Da
    apply(weightBasis Da,realizationCoordinates)
    apply(coweightBasis Da,realizationCoordinates)
  Text
    The weight basis is $\Lambda_0,\Lambda_1,\delta$, with realization
    vectors $(1,0,0)$, $(0,1,0)$, and $(0,0,1)$.  The last list gives
    the complete vectors of the coweight basis: mathematically, the
    fundamental coweights $\Lambda_i^\vee$ followed by the central coroot
    $c$.  These vectors are realization coordinates, not Dynkin labels.
  Text
    In more general realizations, the additional weight-basis vectors
    vanish on every simple coroot.  The additional coweight-basis vectors
    vanish on every simple root.  These directions can occur even in finite
    type when the
    realization contains extra central coordinates:
  Example
    Rextra = realization(cartanDatum "A1",
        matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
    Dextra = rootDatum Rextra;
    mu = weightFromRealizationCoordinates(Dextra,{1,7});
    nu = weightFromRealizationCoordinates(Dextra,{1,8});
    (mu,nu,datumCoordinates mu,datumCoordinates nu)
    apply(weightBasis Dextra,realizationCoordinates)
  Text
    The complete weights are $(1,7)$ and $(1,8)$.  Both have Dynkin label
    $(1)$, but their values on the extra Cartan direction differ.
    The basis vectors are $(1,0)$ and $(0,1)$; the second vector vanishes
    on the simple coroot, so its coefficient is invisible to Dynkin labels.
  Text
    @TO weightToRootCoordinates@ and @TO toRoot@ solve the converse problem;
    the latter additionally requires integral coefficients.  The coweight
    functions are dual.  @TO integratesToGlobalForm@ and
    @TO integratesCoweightToGlobalForm@ test membership in the selected
    integral lattices, returning @TT "null"@ when the global form was left
    unspecified.
    More explicitly, @TO weightToRootCoordinates@ returns the rational
    coefficients in the simple-root basis when the complete weight lies
    in the simple-root span.  It reports an error outside that span, or
    when the weight is not completely specified.  Within that span,
    @TO isRootLatticeWeight@ tests whether these coefficients are integers,
    and @TO toRoot@ constructs the corresponding root-lattice element only
    in the integral case.  The coweight commands make the dual tests in
    the simple-coroot span.  These tests concern the root and coroot
    lattices, not the character lattice of the selected global form.
    The global-form tests return @TT "true"@ or @TT "false"@ only for a
    complete weight or coweight and a specified lattice; they also return
    @TT "null"@ when incomplete coordinates prevent a membership test.
  Example
    Dad = rootDatum("A2",GlobalForm=>"Adjoint");
    mu = kmWeight(Dad,{1,1});
    (weightToRootCoordinates mu,isRootLatticeWeight mu,
     toLegacyList(toRoot mu))
  Text
    The output identifies $\omega_1+\omega_2=\alpha_1+\alpha_2$; therefore
    the weight lies in the adjoint character lattice and converts to the
    simple-root coordinate list $(1,1)$.
  Text
    The @TT "*FromLegacy"@ commands construct roots, coroots, weights, and
    coweights from coordinate lists and a specified datum.
    @TO toLegacyList@ returns the coordinate list, and
    @TO toLegacyCartanMatrix@ returns the Cartan matrix.  These last two
    outputs do not include the root datum or realization.
    The four @TT "*FromLegacy"@ commands have the same list conventions
    as @TO kmRoot@, @TO kmCoroot@, @TO kmWeight@, and @TO kmCoweight@;
    their first argument can be a root datum, Cartan datum, or matrix.
    @TO datumFromLegacyCartan@ constructs a Cartan datum from a matrix.
    For a complete weight with extra realization directions,
    @TO toLegacyList@ returns only its Dynkin labels, and for a complete
    coweight it returns only its simple-root evaluations.  It is therefore
    not a way to preserve a complete affine weight: use
    @TO realizationCoordinates@ and the complete-vector constructor when
    those additional coordinates matter.
SeeAlso
  "Cartan data, realizations, and global lattices"
  weylAction
  affineNullRoot
  affineDerivation
  affineWeightFromDynkinLabels
  affineDegree
  formalCharacter
///

doc ///
Key
  "Weyl groups and standard parabolics"
  (symbol *,WeylElement,WeylElement)
  (symbol ==,KMWeylGroup,KMWeylGroup)
  (symbol ==,StandardParabolic,StandardParabolic)
  (leviSubalgebra,StandardParabolic)
Headline
  Weyl elements and standard parabolics with labeled Dynkin diagrams
Usage
  C = cartanDatum("A3",NodeLabels=>{"left","middle","right"})
  X = rootDatum C
  W = weylGroup X
  w = weylElement(W,word)
  e = identityWeylElement W
  s = simpleWeylReflection(W,i)
  winv = inverseWeylElement w
  weylParent w
  weylWord w
  y = weylAction(w,x)
  P = standardParabolic(X,RemovedNodes=>J)
  P = standardParabolic(X,LeviNodes=>I)
  P = standardParabolic(X,J)
  removedNodes P
  leviNodes P
  parabolicLevi P
  parabolicDegree(P,beta)
  isLeviRoot(P,beta)
  R = nilradicalRoots P
  T = leviHighestWeights(P,d)
  Q = parabolicCosetReps(P,k)
Inputs
  C:CartanDatum
    a Cartan datum, optionally with user-supplied node labels
  X:RootDatum
    a root datum with Cartan datum @TT "C"@
  word:List
    a word in node labels or 1-based simple-reflection indices
  J:List
    a list of node labels or 1-based node positions
  I:List
    the retained Levi nodes; either node collection may also be a set
  i:Thing
    a node label, a 1-based node position, or a position-label table
  x:Thing
    a root, coroot, weight, coweight, or formal character
  beta:KMRoot
    simple-root coordinates in the Cartan datum of the parabolic
  d:ZZ
    a parabolic degree
  k:ZZ
    a Coxeter length
Outputs
  W:KMWeylGroup
    the Weyl group of the specified Cartan datum
  w:WeylElement
    a Weyl element represented by a reduced word
  P:StandardParabolic
    complementary removed and Levi node sets with an induced Levi datum
  R:Matrix
    the positive roots in the nilradical, written in simple-root coordinates
  T:Tally
    the irreducible Levi decomposition of parabolic degree @TT "d"@
  Q:List
    the minimal parabolic coset representatives of length @TT "k"@
Description
  Text
    @TO weylGroup@ accepts a @TO CartanDatum@, @TO RootDatum@,
    @TO Realization@, Cartan matrix, or a type string such as @TT "\"A3\""@.
    This construction works in finite, affine, and indefinite type.
    @TO weylElement@ accepts any of these as its first argument, or a
    previously constructed @TO KMWeylGroup@.  The second argument is always
    a list: an empty word gives the identity, and a nonreduced word is
    reduced automatically.  Supplying a datum, matrix, or string constructs
    its Weyl group.  Separately constructed groups with identical Cartan
    data are compatible; retaining @TT "W"@ is a convenient way to keep
    the matrix and node labels consistent across calculations.
  Text
    A @TO CartanDatum@ may assign a meaningful label to each Dynkin node.
    The commands accept those labels in Weyl words, simple
    reflections, Coxeter entries, reflection matrices, and parabolic node
    sets.  An integer in @TT "1..rank"@ means a 1-based node position;
    any other value is looked up among the chosen labels.  Thus integer
    labels such as @TT "{10,20,30}"@ remain usable without making the
    ordinary positional convention ambiguous.  @TO weylWord@,
    @TO removedNodes@, and @TO leviNodes@ return node positions, numbered
    starting at $1$.
    A position-label table returned by
    @TT "minusculeNodes(D,NodeFormat=>\"Records\")"@ can also be used
    as a reflection index or an entry in a Weyl word or parabolic node set.
    Its position and label must agree with the chosen Cartan datum.
  Example
    C = cartanDatum("A3",NodeLabels=>{"left","middle","right"});
    X = rootDatum C;
    W = weylGroup X;
    u = weylElement(W,{"left","middle","left"});
    v = weylElement(W,{"middle","left","middle"})
    u
    (weylWord u,weylLength u,u == v,
        coxeterMatrix(W,"left","middle"))
  Text
    Both Weyl elements have the compact mathematical display
    @TT "s_left s_middle s_left"@.  @TO weylParent@ gives the Weyl group,
    and @TO weylWord@ gives the reduced word.
    The tuple @TT "({1,2,1},3,true,3)"@ says that
    $u=s_{\rm left}s_{\rm middle}s_{\rm left}$ has length $3$,
    $m_{\rm left,middle}=3$, and hence
    $s_{\rm left}s_{\rm middle}s_{\rm left}
    =s_{\rm middle}s_{\rm left}s_{\rm middle}$.
    Two elements with equal @TO KMWeylGroup@ data are equal when
    their root actions agree.  This recognizes words related by braid
    relations.  Equality of Weyl groups includes their Cartan matrices and
    node labels, not just their abstract Dynkin types.  An older
    @TO WeylElement@ record without a parent must first be expressed using
    @TO weylElement@ before it can be compared with @TT "=="@.
  Example
    lambda = kmWeight(X,{1,0,0})
    datumCoordinates weylAction(u,lambda)
  Text
    This computes
    $u(\omega_1)$, whose Dynkin labels are $(0,-1,1)$.  The action uses
    $s_i(\lambda)=\lambda-\langle\lambda,\alpha_i^\vee\rangle\alpha_i$
    and preserves complete realization coordinates in singular type.
    The command @TT "reflectionMatrix(W,\"middle\")"@ gives the
    corresponding simple reflection on simple-root coordinates.  In a
    product @TT "u*v"@ the rightmost factor acts first; elements with
    unequal Weyl-group data cannot be multiplied.
  Text
    @TT "weylAction(w,x)"@ accepts @TO KMRoot@, @TO KMCoroot@,
    @TO KMWeight@, and @TO KMCoweight@ objects.  The result has the same
    kind of coordinates and the same root datum as @TT "x"@.  For a weight
    specified only by Dynkin labels, the result is again a Dynkin-label
    class; for a complete weight, all realization coordinates are retained.
    The same distinction applies to coweights.  Bare coordinate lists are
    not accepted here: construct, for example, @TT "kmWeight(X,l)"@ first.
    On a @TO FormalCharacter@, the action sends each term $e^\mu$ to
    $e^{w\mu}$, retaining its coefficient.  In each case the Cartan datum
    must be the one used to
    construct @TT "W"@.
  Text
    A @TO StandardParabolic@ removes grading nodes and keeps their complement
    as Levi nodes.  The options @TT "RemovedNodes"@ and @TT "LeviNodes"@
    state either convention explicitly and cannot be supplied
    simultaneously.  Positional input remains available and means removed
    nodes.  The result stores both complementary sets, eliminating the need
    to convert between the two conventions by hand.
  Text
    @TO standardParabolic@ accepts a @TO CartanDatum@, @TO RootDatum@,
    @TO Realization@, Cartan matrix, or type string.  Use either
    @TT "standardParabolic(X,J)"@ for removed nodes, or exactly one of
    @TT "RemovedNodes=>J"@ and @TT "LeviNodes=>I"@.  Lists and sets are
    accepted in all three forms; repetitions are ignored and the returned
    node positions are sorted.  A positional node collection cannot be
    combined with either option.  With neither a node collection nor an
    option, the command reports an error rather than choosing a parabolic.
  Example
    P = standardParabolic(X,RemovedNodes=>{"middle"});
    beta = kmRoot(X,{1,1,0})
    (removedNodes P,leviNodes P,parabolicLevi P,
        parabolicDegree(P,beta),isLeviRoot(P,beta))
  Text
    The chosen labels give the node sets
    @TT "{2}"@ and @TT "{1,3}"@.  This is the standard parabolic of $A_3$
    obtained by crossing the middle node.
    Its derived Levi has type $A_1\times A_1$ on nodes $1$ and $3$.
    Since $\beta=\alpha_1+\alpha_2$ has coefficient $1$ on the removed
    node, it has parabolic degree $1$ and is not a Levi root.
  Text
    For simple-root coordinates $\beta=\sum_i b_i\alpha_i$,
    @TT "parabolicDegree(P,beta)"@ returns $\sum_{i\in J}b_i$, where $J$
    consists of the removed nodes.  @TT "isLeviRoot(P,beta)"@ tests whether
    this degree is zero.  For an actual root, this is precisely membership
    in the Levi root system.  These commands do not check whether an
    arbitrary root-lattice vector is a root; their input must have the same
    Cartan datum as @TT "P"@.
  Text
    Functions that depend on a parabolic accept @TT "P"@ directly.
    The grading functions @TO parabolicDiagram@,
    @TO cartanPositiveRootsDegree@, @TO nilradicalRoots@,
    @TO parabolicDegrees@, @TO leviHighestWeights@, and
    @TO kostantLieAlgCohomology@ use the removed nodes.  The quotient
    functions @TO parabolicCosetReps@ and @TO parabolicDotImages@ use the
    Levi nodes.  Crossed nodes define the grading, whereas retained nodes
    define $W_L$ and hence the quotient $W/W_L$.  When @TT "P"@ is supplied,
    each command uses the node set appropriate to its calculation.
  Example
    parabolicDiagram P
    nilradicalRoots P
    leviConnectedComponents P
    leviHighestWeights(P,1)
    parabolicCosetReps(P,2)
  Text
    The four rows of the root matrix are
    $\alpha_2$, $\alpha_1+\alpha_2$, $\alpha_2+\alpha_3$, and
    $\alpha_1+\alpha_2+\alpha_3$.  They all have parabolic degree $1$.
    As a representation of the derived Levi
    $\mathfrak{sl}_2\oplus\mathfrak{sl}_2$, this degree-one piece is
    $$
       V(1)\mathbin{\boxtimes}V(1).
    $$
    The length-two minimal representatives for $W/W_L$ have reduced words
    @TT "{1,2}"@ and @TT "{3,2}"@.  The command
    @TT "leviConnectedComponents P"@ reports ambient node positions, here
    @TT "{{1},{3}}"@, rather than silently renumbering the two Levi nodes.
    The related commands @TT "cartanPositiveRootsDegree(P,d)"@ and
    @TT "parabolicDegrees P"@ respectively select a degree and display the
    degree of every positive root.
  Example
    parabolicDotImages(P,1,{0,0,0})
    H = kostantLieAlgCohomology(P,{0,0,0},MaxDegree=>2);
    cohomologyDimensions(P,H)
  Text
    The first command returns the length-one minimal representative together
    with its dot-action image.  For the trivial highest weight, the second
    calculation gives
    $$
      H^0=V(0)\mathbin{\boxtimes}V(0),\qquad
      H^1=V(1)\mathbin{\boxtimes}V(1),
    $$
    and
    $$
      H^2=
      \bigl(V(2)\mathbin{\boxtimes}V(0)\bigr)
      \mathbin{\oplus}
      \bigl(V(0)\mathbin{\boxtimes}V(2)\bigr).
    $$
    Accordingly, @TO cohomologyDimensions@ returns dimensions $1$, $4$,
    and $6$ in degrees $0$, $1$, and $2$.
  Text
    @TO identityWeylElement@ constructs $1\in W$,
    @TO simpleWeylReflection@ constructs $s_i$, and
    @TO inverseWeylElement@ reverses a reduced word before reducing it.
    @TO parabolicLevi@ returns a labeled @TO CartanDatum@.
    @TO leviSubalgebra@ and @TO parabolicData@ also accept the parabolic
    directly and include it among their results.
  Text
    The empty and full node sets also define standard parabolics.  In particular,
    @TT "LeviNodes=>{}"@ constructs a Borel.  Its derived Levi has rank zero,
    while its full reductive Levi is the Cartan torus.
    Conversely, @TT "RemovedNodes=>{}"@ keeps every node, so the parabolic
    and its Levi are the whole algebra and the nilradical is zero.
  Example
    B = standardParabolic(X,LeviNodes=>{});
    LB = leviSubalgebra B;
    (parabolicLevi B,LB#"semisimpleRank",LB#"centerDimension",
        leviHighestWeights(B,1))
  Text
    For $A_3$ the output records a trivial rank-zero Cartan datum,
    semisimple rank $0$, and center dimension $3$.  The final tally is
    @TT "Tally{V() => 3}"@: the three simple-root spaces in parabolic degree
    one are three copies of the trivial representation of the derived Levi.
SeeAlso
  coxeterMatrix
  reflectionMatrix
  reducedWord
  bruhatLessOrEqual
  nilradicalRoots
  leviHighestWeights
  parabolicCosetReps
  kostantLieAlgCohomology
  leviSubalgebra
  reductiveLeviDatum
///

doc ///
Key
  "Finite structural invariants"
Headline
  Explore finite Weyl orbits, duality, special weights, and Casimir data
Usage
  H = dominantizeWeight lambda
  S = weightStabilizer lambda
  lambdaDual = dualWeight lambda
  c = quadraticCasimir lambda
  isOppositionFixed lambda
  isSelfDual lambda
  F = fundamentalCoweights D
  G = normalizedInvariantForm D
  marks D
  highestCoroot D
  comarks D
  lacingNumber D
  weylExponents D
  invariantDegrees D
  nodes = minusculeNodes D
  cominusculeNodes D
  isMinusculeNode(D,i)
  isCominusculeNode(D,i)
  isMinusculeWeight lambda
  isCominusculeWeight lambda
Inputs
  D:RootDatum
    a finite root datum
  lambda:KMWeight
    an integral weight; isSelfDual, the special-weight predicates, and
    quadraticCasimir require a dominant highest weight
  A:Matrix
    a finite-type Cartan matrix, when supplying coordinate lists directly
  i:Thing
    a node label, a 1-based node position, or a position-label table;
    matrix input requires a 1-based position
Description
  Text
    The weight calculations in this node accept a @TO KMWeight@ alone, or
    the form @TT "dualWeight(D,l)"@ with a datum and integral Dynkin labels.
    The same two-argument form is available for @TO dominantizeWeight@,
    @TO weightStabilizer@, @TO isOppositionFixed@, @TO isSelfDual@,
    @TO isMinusculeWeight@, @TO isCominusculeWeight@, and
    @TO quadraticCasimir@.  Here @TT "D"@ may be a @TO RootDatum@,
    @TO CartanDatum@, @TO Realization@, or type string such as
    @TT "\"A3\""@; @TT "l"@ may be a list or a one-row or one-column
    matrix.  Alternatively, a Cartan matrix @TT "A"@ may be supplied as
    the first argument; in this form a matrix of Dynkin labels must have
    size $1$ by the rank, rather than one column.
  Text
    With a @TO RootDatum@, a complete weight can also be supplied explicitly,
    as in @TT "dualWeight(D,lambda)"@.  The weight must belong to that very
    root datum, not merely to a datum with the same Cartan matrix.
    List and matrix arguments specify Dynkin labels only: in a nonminimal
    realization they do not choose the additional central coordinates.
    To include those coordinates, first construct @TT "lambda"@ with
    @TO weightFromRealizationCoordinates@.
  Text
    The structural commands @TO fundamentalCoweights@,
    @TO normalizedInvariantForm@, @TO marks@, @TO highestCoroot@,
    @TO comarks@, @TO lacingNumber@, @TO weylExponents@,
    @TO invariantDegrees@, @TO minusculeNodes@, and @TO cominusculeNodes@
    accept a Cartan matrix, @TO CartanDatum@, @TO Realization@,
    @TO RootDatum@, @TO KMWeylGroup@, or type string.  The node tests
    @TO isMinusculeNode@ and @TO isCominusculeNode@ accept the same first
    argument followed by a node.  All calculations in this node require
    finite type.
  Text
    These commands accept weights in a specified root datum and use its
    Dynkin node labels in their answers.  They also accept a Cartan matrix
    with coordinate lists.  To act by a Weyl element, construct a
    @TO WeylElement@ and use @TO weylAction@; the element and weight must
    have the same Cartan datum.
    When coordinates are supplied directly, @TO dualWeight@ returns a list
    for a list input, and a $1$-by-rank matrix over the same ring for a
    matrix input.
    With a datum, realization, or type string, @TO dualWeight@ returns a
    @TO KMWeight@ instead.  Likewise, @TO dominantizeWeight@ and
    @TO weightStabilizer@ retain weights as @TO KMWeight@ objects for those
    inputs, whereas their Cartan-matrix forms return coordinate lists.
    Their integer words are available in either form; the datum forms also
    include Weyl elements and the chosen node labels.
  Example
    X = rootDatum cartanDatum(
        "A3",NodeLabels=>{"left","middle","right"});
    print dynkinDiagram X
    lambda = kmWeight(X,{-1,0,0})
    H = dominantizeWeight lambda;
    (H#"dominantWeight",H#"toDominantElement",
        H#"wallNodeLabels",
        weylAction(H#"toDominantElement",lambda))
  Text
    The diagram fixes the positional convention, while the datum records the
    more memorable labels.  Here the displayed Weyl element is an explicit
    witness
    \[
       w(-\omega_1)=\omega_3.
    \]
    The first and last entries of the displayed tuple agree.  The wall labels say that
    $\omega_3$ lies on the left and middle simple walls.
  Example
    S = weightStabilizer kmWeight(X,{1,0,1});
    (S#"dominantWallNodeLabels",S#"generatingElements",S#"order")
  Text
    Thus $\operatorname{Stab}_W(\omega_1+\omega_3)$ is generated by
    $s_{\mathrm{middle}}$ and has order $2$.  For a nondominant input the
    returned generators are the corresponding conjugates.  The command
    constructs these generators and their @TO StandardParabolic@; it does
    not enumerate every stabilizer element.
  Text
    @TO dualWeight@ returns $-w_0\lambda$, the highest weight of the
    contragredient representation when $\lambda$ is dominant.
    It also accepts an arbitrary integral weight, where the output means only
    the linear opposition transform.
    @TO isSelfDual@ deliberately requires a dominant highest weight.
    For an arbitrary integral lattice vector, @TO isOppositionFixed@ tests
    fixedness under the same linear opposition involution without making a
    representation-theoretic claim.  Self-duality does not say whether the
    invariant form is symmetric or alternating; use
    @TO frobeniusSchurIndicator@ for that distinction.
  Example
    D5 = rootDatum "D5";
    spinPlus = kmWeight(D5,{0,0,0,1,0})
    spinMinus = dualWeight spinPlus;
    (spinMinus,isSelfDual spinPlus,isSelfDual(spinPlus+spinMinus))
  Text
    In type $D_5$ the two half-spin highest weights are exchanged, so
    \[
       V_{\omega_4}^*\cong V_{\omega_5},
       \qquad
       (V_{\omega_4+\omega_5})^*
          \cong V_{\omega_4+\omega_5}.
    \]
    In even type $D_6$, by contrast, each half-spin highest weight is fixed.
    The opposition involution acts by permuting Dynkin coordinates.
  Text
    A complete weight in a nonminimal realization may also have a
    Weyl-fixed central character invisible in its Dynkin labels.
    Duality negates that central character as well as transforming the
    semisimple labels.
    Thus two weights with the same semisimple labels need not have the same
    self-duality behavior.  Accordingly, @TO isSelfDual@ asks for a complete
    weight in this situation and explains how to construct one with
    @TO weightFromRealizationCoordinates@; a bare Dynkin-label class does
    not contain enough information.  @TO isOppositionFixed@ may still be
    applied to that class, where it means fixed modulo the annihilator of the
    simple coroots.  This includes a zero-node, semisimple-rank-zero Cartan
    datum with a positive-dimensional toral realization: duality negates its
    complete character, while the Casimir of its zero derived algebra is
    $0$.
  Text
    @TO fundamentalCoweights@ returns the inverse Cartan matrix: its rows are
    $\omega_i^\vee$ in the simple-coroot basis, so multiplication by $A$
    gives the identity pairing matrix.
  Text
    @TO normalizedInvariantForm@ is the Gram matrix on simple roots,
    normalized on each connected component so long roots have squared norm
    $2$.  This is the form used by @TO quadraticCasimir@.
    By contrast, @TO rootInnerProduct@ and @TO rootNormSquared@ use a
    componentwise first-node normalization.  That form is useful for root
    recursions, but its values differ by a component scalar;
    rescale it before comparing absolute Casimir values.
  Text
    @TO lacingNumber@ is the ratio of the squared lengths of long and short
    roots, equal to $1$, $2$, or $3$.  @TO weylExponents@ returns the Weyl
    exponents $m_1,\ldots,m_r$ in increasing order, and
    @TO invariantDegrees@ returns $m_1+1,\ldots,m_r+1$, the degrees of basic
    generators of the polynomial algebra of Weyl-invariant functions on
    the Cartan space.  For a semisimple product the exponents and degrees
    include those of every simple component, with repetitions retained.
  Text
    @TO marks@ are the coefficients of the highest root in the simple-root
    basis;
    @TO highestCoroot@ and @TO comarks@ are the coefficients of the coroot
    attached to that highest long root.  These distinctions matter outside
    simply laced type.
    The commands @TO marks@, @TO highestCoroot@, @TO comarks@, and
    @TO lacingNumber@ require connected finite type.  For a semisimple
    product, apply them separately to its connected components.
  Example
    B3 = cartanB 3;
    (fundamentalCoweights B3,marks B3,comarks B3,lacingNumber B3,
        weylExponents B3,invariantDegrees B3)
  Text
    The first matrix expresses the three fundamental coweights in the
    simple-coroot basis.  The remaining data give highest-root coefficients
    $(1,2,2)$, comarks $(1,2,1)$, and lacing number $2$.  The Weyl exponents
    are $(1,3,5)$, hence the basic invariant degrees are $(2,4,6)$.
  Text
    A dominant integral weight $\lambda$ is @EM "minuscule"@ if all
    weights of $V(\lambda)$ form one Weyl orbit.  Equivalently,
    $\langle\lambda,\beta^\vee\rangle\in\{0,1\}$ for every positive root
    $\beta$.  A node is @EM "cominuscule"@ if its simple root has
    coefficient $1$ in the highest root of its finite-type component;
    removing that node gives an abelian nilradical.
    @TO minusculeNodes@ applies the coroot criterion to fundamental weights;
    @TO cominusculeNodes@ applies the highest-root coefficient criterion.
    For a dominant weight with Dynkin labels $(a_i)$,
    @TO isCominusculeWeight@ tests the corresponding coweight
    $\sum_i a_i\omega_i^\vee$: its pairing with every positive root
    must be $0$ or $1$.
    For a Cartan datum, these commands normally return
    the chosen node labels.  If an integer label could be confused with a
    position, each node is given by a table containing both its position
    and label.  Set @TT "NodeFormat=>\"Positions\""@, @TT "\"Labels\""@, or
    @TT "\"Records\""@ to choose the form of the answer.  The position-label
    tables can be passed directly to the Weyl and parabolic commands.
    Matrix input gives node positions numbered starting at $1$.
    These choices of @TO NodeFormat@ apply to @TO minusculeNodes@ and
    @TO cominusculeNodes@.  To test one node directly, use
    @TT "isMinusculeNode(D,i)"@ or @TT "isCominusculeNode(D,i)"@.
    For a datum, realization, or Weyl group, @TT "i"@ may be a label or
    position-label table as well as a position.  Integers between $1$ and
    the rank are always interpreted as positions, even when a chosen
    integer label has the same value.
  Example
    Bnamed = cartanDatum(
        "B3",NodeLabels=>{"vector","middle","spin"});
    (minusculeNodes Bnamed,cominusculeNodes Bnamed,
        minusculeNodes(Bnamed,NodeFormat=>"Records"))
  Text
    The named output says immediately that the spin node is minuscule and
    the vector node is cominuscule.  The position-label form is useful when
    integer labels could be confused with positions.
  Example
    C3 = cartanC 3;
    (minusculeNodes B3,cominusculeNodes B3,
        minusculeNodes C3,cominusculeNodes C3)
    (isMinusculeWeight(B3,{0,0,1}),
        weylDimension(B3,{0,0,1}),#weightOrbit(B3,{0,0,1}),
        isMinusculeWeight(B3,{1,0,0}),
        weylDimension(B3,{1,0,0}),#weightOrbit(B3,{1,0,0}))
  Text
    In $B_3$, $\omega_3$ is the eight-dimensional minuscule spin
    representation: all eight weights form one Weyl orbit.  The
    seven-dimensional vector representation at $\omega_1$ is cominuscule
    but not minuscule: its six nonzero weights form one orbit and zero forms
    another.  Thus ``cominuscule'', ``minuscule'', and
    ``all weight multiplicities are one'' are different assertions.
    On a product, one may choose a minuscule contribution independently in
    each simple factor; the zero weight is both minuscule and cominuscule.
  Text
    @TO quadraticCasimir@ returns
    $(\lambda,\lambda+2\rho)$ with long roots of squared norm $2$.  Under
    this convention the adjoint eigenvalue is $2h^\vee$.  Its input is in
    Dynkin labels.  In particular, @TO marks@ cannot be passed directly:
    marks are simple-root coordinates and must first be converted with
    @TO toFundamental@.
  Example
    adjointWeight = toFundamental(B3,marks B3);
    (adjointWeight,quadraticCasimir(B3,adjointWeight),
        2*dualCoxeterNumber B3,quadraticCasimir(B3,marks B3))
  Text
    The highest root has Dynkin labels $(0,1,0)$, so it is the adjoint
    highest weight.  Its Casimir and $2h^\vee$ are both $10$.  The final
    value, $56$, is not another normalization of the adjoint: it is the
    Casimir of the different representation whose Dynkin labels happen to
    equal the list of marks.
  Example
    (toFundamental(B3,highestRoot B3),
        toFundamental(C3,highestRoot C3),
        quadraticCasimir(B3,{0,1,0}),
        quadraticCasimir(C3,{2,0,0}),
        2*dualCoxeterNumber B3,2*dualCoxeterNumber C3)
  Text
    This independently verifies the adjoint values $10$ and $8$ in the dual
    nonsimply-laced types.  On a semisimple product, each simple component is
    normalized independently and Casimir eigenvalues add.  For a complete
    weight with extra central coordinates, this command returns the Casimir
    of the derived semisimple algebra: a Cartan matrix does not specify a
    bilinear form on an additional central torus.
SeeAlso
  weylAction
  WeylElement
  highestRoot
  highestShortRoot
  dualCoxeterNumber
  toFundamental
  rootNormSquared
  finiteCharacter
  frobeniusSchurIndicator
  representationDynkinIndex
///

doc ///
Key
  "Affine structural data"
Headline
  Recover affine structure and retain the null, central, level, and degree directions
Usage
  a = affineMarks A
  av = affineComarks A
  affineCentralCoefficients A
  T = affineTypeData D
  delta = affineNullRoot D
  c = affineCentralElement D
  F = affineFinitePart D
  F = affineFinitePart(D,node)
  affineClassicalPart(A,lambda)
  affineClassicalPart(A,lambda,node)
  k = affineLevel(A,lambda)
  d = affineDerivation D
  d = affineDerivation(D,node)
  L = affineWeightFromDynkinLabels(D,lambda,degree)
  L = affineWeightFromDynkinLabels(D,lambda,degree,d)
  L = affineFullWeight(D,x)
  q = affineDegree(L,d)
  affineCartanA2Twisted n
  affineCartanAOddTwisted n
  affineCartanDTwisted n
  affineCartanE6Twisted()
  affineCartanD4Twisted()
  affineStructuralCapabilities()
Inputs
  A:Matrix
    an indecomposable affine generalized Cartan matrix
  D:RootDatum
    an affine root datum, normally with its minimal realization
  node:Thing
    a node position numbered starting at $1$, a chosen label, or a table giving both
  lambda:List
    affine Dynkin labels
  degree:Thing
    a rational value prescribed for $\lambda(d)$
  x:List
    a complete rational weight vector in the realization basis
  n:ZZ
    the family parameter for a twisted affine Cartan matrix, with the bounds stated below
Outputs
  a:List
    primitive positive null-root coefficients
  T:HashTable
    the affine type, ranks, distinguished nodes, null-root coefficients, and finite part
  delta:KMRoot
    the null root in the specified datum
  k:Thing
    the level $\lambda(c)$
  d:KMCoweight
    a complete normalized derivation
  L:KMWeight
    a complete affine weight with the requested labels and degree
  q:Thing
    the eigenvalue $\lambda(d)$
Description
  Text
    Let $A=(a_{ij})$ be an indecomposable affine generalized Cartan matrix.
    The package uses
    $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$.  The primitive positive
    right-kernel vector $a$ and left-kernel vector $a^\vee$ determine
    \[
       Aa=0,\qquad A^{\mathsf T}a^\vee=0,\qquad
       \delta=\sum_i a_i\alpha_i,\qquad
       c=\sum_i a_i^\vee\alpha_i^\vee.
    \]
    @TO affineMarks@ and @TO affineComarks@ return these coefficient lists.
    Both commands, @TO affineCentralCoefficients@, @TO affineTypeData@,
    and @TO affineFinitePart@ accept a matrix, Cartan datum, realization,
    or root datum.  @TO affineCentralCoefficients@ is the coefficient-list
    form of the central coroot.  @TO affineNullRoot@ returns its coefficient
    list for a matrix, Cartan datum, or realization.
    For a @TO RootDatum@, @TO affineNullRoot@ and
    @TO affineCentralElement@ give the null root and central coroot as
    @TO KMRoot@ and @TO KMCoroot@, respectively, in that datum.
  Example
    A = cartanAffineA 2
    X = cartanDatum(
        A,NodeLabels=>{"affine","left","right"});
    D = rootDatum X;
    print dynkinDiagram D
    delta = affineNullRoot D;
    c = affineCentralElement D;
    (datumCoordinates delta,
        datumCoordinates(toWeight delta),
        realizationCoordinates delta,
        datumCoordinates c)
    (A * transpose matrix{datumCoordinates delta},
        transpose(A) * transpose matrix{datumCoordinates c})
  Text
    The diagram displays both positions and root labels.  Package
    positions $1,2,3$ correspond here to the conventional affine indices
    $0,1,2$.  The first tuple is
    \[
       ((1,1,1),(0,0,0),(0,0,0,1),(1,1,1)).
    \]
    Thus
    \[
       \delta=\alpha_0+\alpha_1+\alpha_2,\qquad
       c=\alpha_0^\vee+\alpha_1^\vee+\alpha_2^\vee.
    \]
    The coefficients of the @TO KMRoot@ are not Dynkin labels.  Its
    converted weight has labels $(0,0,0)$ because
    $\langle\delta,\alpha_i^\vee\rangle=0$ for every $i$, while the nonzero
    realization vector proves that $\delta$ itself has not been discarded.
    The two zero matrices verify the right- and left-kernel equations.
  Text
    @TO affineTypeData@ recognizes untwisted and twisted affine families up
    to simultaneous row-and-column permutation.  In its output,
    @TT "\"rank\""@ means the number of affine Cartan nodes.  The more
    explicit entries @TT "\"nodeCount\""@, @TT "\"cartanMatrixRank\""@,
    @TT "\"finiteRank\""@, and @TT "\"corank\""@ distinguish the different
    ranks.  For a specified datum, each distinguished node list also has
    labels and position-label tables.  The @TT "\"finitePart\""@ is a
    table containing a Cartan datum, realization, or root datum according
    to the input; a matrix input gives the finite Cartan matrix and its
    associated information.
  Example
    T = affineTypeData D;
    (T#"type",T#"kind",T#"twistOrder",
        T#"nodeCount",T#"cartanMatrixRank",T#"finiteRank",T#"corank")
    (T#"standardAffineNodeRecord",T#"specialNodeRecords",
        T#"marks",T#"comarks")
    (T#"finitePart"#"nodeLabels",
        T#"finitePart"#"finiteRootDatum")
  Text
    The first tuple is
    @TT "(A_2^(1),untwisted,1,3,2,2,1)"@: there are three simple roots,
    the Cartan matrix and associated finite system both have rank $2$, and
    the affine corank is $1$.  In the cyclic $A_2^{(1)}$ diagram all three
    nodes are special.  A node table contains both
    @TT "\"position\""@ and @TT "\"label\""@, so it remains unambiguous when
    custom integer labels collide with positions.
    A bare integer between $1$ and the number of nodes is interpreted as
    a position.  To refer to a custom integer label that occupies another
    position, use its position-label table, for example an entry of
    @TT "T#\"specialNodeRecords\""@.
  Text
    Two map fields describe recognition after a permutation, in opposite
    directions:
    \[
    \begin{split}
      \texttt{canonicalNodeOrder\#(j-1)}
        &=\text{input position occupied by canonical node }j,\\
      \texttt{inputToCanonicalNodeMap\#(i-1)}
        &=\text{canonical position of input node }i.
    \end{split}
    \]
    The @TT "\"canonicalNodeRecords\""@ field gives the first map together
    with labels.
  Example
    A3tw = affineCartanD4Twisted()
    p = {2,0,1};
    Dp = rootDatum cartanDatum(
        submatrix(A3tw,p,p),
        NodeLabels=>{"longOrbit","affine","shortOrbit"});
    print dynkinDiagram Dp
    Tp = affineTypeData Dp;
    (Tp#"type",Tp#"standardAffineNodeRecord",
        Tp#"canonicalNodeOrder",Tp#"inputToCanonicalNodeMap")
    apply(Tp#"canonicalNodeRecords",R ->
        {R#"canonicalPosition",R#"position",R#"label"})
  Text
    The recognized standard affine vertex is input position $2$, with label
    @TT "\"affine\""@.  Canonical nodes $1,2,3$ occupy input positions
    $2,3,1$, whereas the inverse map is $(3,1,2)$.
  Text
    @TO affineFinitePart@ and @TO affineClassicalPart@ accept a position,
    chosen label, or position-label table when a datum is supplied.  The
    former returns a table describing the finite datum; the latter restricts a weight to its derived
    finite semisimple subalgebra.  In a finite @TO RootDatum@ result, the
    global form is explicitly unspecified: the affine Cartan matrix alone
    does not determine the induced finite character lattice.
    With no selected node, both commands use the recognized standard
    affine node.  A matrix argument takes a node position numbered from
    $1$.  @TO affineClassicalPart@ accepts @TT "(A,lambda)"@ or
    @TT "(A,lambda,node)"@ with a list or one-row or one-column matrix of
    Dynkin labels; for a @TO KMWeight@, use
    @TT "affineClassicalPart lambda"@ or
    @TT "affineClassicalPart(lambda,node)"@.
    The finite-part table retains the ambient positions in its
    @TT "\"nodes\""@ entry and the retained finite Cartan matrix in
    @TT "\"cartanMatrix\""@.  With a supplied root datum, its
    @TT "\"finiteRootDatum\""@ entry is the finite root datum, and a
    classical-part table also includes the restricted
    @TT "\"finiteWeight\""@.  A finite realization constructed this way
    is the standard realization of the retained finite Cartan matrix;
    it is not presented as a subspace of the ambient affine realization.
  Example
    F = affineFinitePart(D,"affine");
    lambdaClass = kmWeight(D,{2,0,1})
    finiteLambda = affineClassicalPart(lambdaClass,"affine");
    (F#"deletedNodeRecord",F#"nodeLabels",
        datumCoordinates(finiteLambda#"finiteWeight"))
  Text
    Deleting the vertex labeled @TT "\"affine\""@ retains the labels
    @TT "{\"left\",\"right\"}"@ and restricts $(2,0,1)$ to the finite
    $A_2$ weight $(0,1)$.
  Text
    The level is already determined by Dynkin labels:
    \[
       \operatorname{level}(\lambda)=\lambda(c)
          =\sum_i a_i^\vee
             \langle\lambda,\alpha_i^\vee\rangle.
    \]
    It does not require dominance or make an integrability assertion.
    Every root has level zero, so level is constant on the weights of a
    highest-weight module.  @TO affineLevel@ accepts matrix/list data,
    @TO KMWeight@, or @TO KMRoot@.
    In @TT "affineLevel(A,lambda)"@ the first argument can also be a Cartan
    datum, realization, or root datum, and the labels can be a one-row or
    one-column matrix.  The label list always has one rational entry per
    affine node.
  Example
    (affineLevel lambdaClass,pairing(lambdaClass,c),
        affineLevel delta)
  Text
    The output $(3,3,0)$ verifies the central-pairing formula directly.
  Text
    Degree requires more information.  @TO affineDerivation@ constructs a
    complete normalized @TO KMCoweight@ in a minimal affine realization.
    For a selected node $i$, it is the row-space representative satisfying
    \[
       \alpha_i(d)=1/a_i,\qquad
       \alpha_j(d)=0\ (j\ne i),\qquad \delta(d)=1.
    \]
    With no node argument, the recognized standard affine node is used.
    The node may be specified by a position, label, or position-label table.
    This choice of representative depends on the supplied realization
    basis.  The Cartan matrix alone does not determine a derivation.
  Example
    d = affineDerivation(D,"affine");
    (datumCoordinates d,realizationCoordinates d,pairing(delta,d))
  Text
    Here the output is @TT "({1,0,0},{0,0,0,1},1)"@, expressing
    $\alpha_0(d)=1$, $\alpha_1(d)=\alpha_2(d)=0$, and $\delta(d)=1$.
    A nonstandard chosen node with mark $a_i>1$ receives evaluation
    $1/a_i$, not $1$.
  Text
    @TO affineWeightFromDynkinLabels@ constructs a weight from this choice
    of derivation.  In a minimal affine realization, Dynkin labels
    together with one prescribed value $\lambda(d)$ determine a unique
    complete weight.  The three-argument form uses @TO affineDerivation@;
    the four-argument form uses an explicitly supplied complete normalized
    derivation.  Construction does not assert dominance, integrality,
    integration to the selected global form, or integrability.
  Example
    Lambda0 = affineWeightFromDynkinLabels(D,{1,0,0},0,d);
    mu = Lambda0-toWeight delta;
    (Lambda0,mu)
    (datumCoordinates mu,realizationCoordinates mu,
        affineLevel mu,affineDegree(mu,d))
    (affineDegree(Lambda0,d),affineDegree(mu,d),
        affineDegree(Lambda0,d)-affineDegree(mu,d))
  Text
    The first output gives the realization tuples $(1,0,0,0)$ and
    $(1,0,0,-1)$, representing $\Lambda_0$ and $\Lambda_0-\delta$.
    Both
    weights have Dynkin labels $(1,0,0)$ and level $1$.  Their
    $d$-eigenvalues are $0$ and $-1$.
    @TO affineDegree@ returns the value $\mu(d)$; the nonnegative grade
    of $\mu$ below the chosen highest weight $\Lambda_0$ is instead
    $\Lambda_0(d)-\mu(d)=1$.  This relative grade is also distinct from the
    root-height @TO Depth@ used to truncate completed characters.
  Text
    Normalization does not make the derivation unique.  If
    $d'=d+c$, then
    \[
       \delta(d')=1,\qquad
       \lambda(d')=\lambda(d)+\lambda(c)
          =\lambda(d)+\operatorname{level}(\lambda).
    \]
    The explicit derivation argument therefore remains mathematically
    meaningful.
  Example
    dPrime = d+toCoweight c
    (pairing(delta,d),pairing(delta,dPrime),
        affineDegree(Lambda0,d),affineDegree(Lambda0,dPrime),
        affineLevel Lambda0)
    Lambda0Prime =
        affineWeightFromDynkinLabels(D,{1,0,0},0,dPrime)
    (Lambda0Prime == (Lambda0-toWeight delta),
        affineDegree(Lambda0Prime,dPrime),
        affineDegree(Lambda0Prime,d))
  Text
    The two grouped tuples are $(1,1,0,1,1)$ and $(\mathrm{true},0,-1)$.
    @TO affineDegree@ rejects incomplete weights, incomplete derivations,
    unnormalized derivations, and weights and derivations belonging to
    different data.  It also accepts a @TO KMRoot@ directly.
  Text
    @TO affineDerivation@ and @TO affineWeightFromDynkinLabels@ require a
    minimal affine realization
    of dimension @TT "nodeCount+1"@.  In a nonminimal realization, labels
    and one degree leave further annihilator directions undetermined.
    Use @TO affineFullWeight@ with a complete realization-coordinate
    vector when those additional coordinates have been specified.
    @TO affineFullWeight@ requires a root datum and accepts a list or a
    one-row or one-column matrix with $\dim\mathfrak h$ entries; it is the
    affine form of @TO weightFromRealizationCoordinates@.
    @TO affineWeightFromDynkinLabels@ also accepts its Dynkin labels as a
    one-row or one-column matrix, with one entry per affine node.
    Neither full-weight construction is available from a Cartan matrix
    alone, because the realization and its coordinates must be specified.
  Text
    Twisted type makes the marks/comarks distinction visible.
  Example
    At = affineCartanA2Twisted 1
    Dt = rootDatum cartanDatum(
        At,NodeLabels=>{"affine","finite"});
    Tt = affineTypeData Dt;
    (Tt#"type",Tt#"twistOrder",Tt#"marks",Tt#"comarks",
        Tt#"specialNodeLabels",Tt#"dualSpecialNodeLabels")
    (affineLevel(Dt,{1,0}),affineLevel(Dt,{0,1}),
        affineLevel(Dt,{3,-2}))
  Text
    The outputs give
    \[
       \delta=\alpha_0+2\alpha_1,\qquad
       c=2\alpha_0^\vee+\alpha_1^\vee,
    \]
    and levels $(2,1,4)$.  Thus level uses comarks rather than marks, and it
    remains defined for a nondominant weight.
  Text
    A mark-one node need not be special.  In type $D_4^{(3)}$ the marks are
    $(1,1,2)$, the mark-one nodes are $(1,2)$, but only node $1$ belongs to
    the automorphism orbit of the recognized standard affine node.
    Every one-node deletion of an indecomposable affine GCM is finite;
    @TT "\"connectedFiniteDeletionNodes\""@ records which remain connected,
    while @TT "\"specialNodes\""@ records the distinguished orbit.  These
    notions are not synonyms.
  Example
    T3 = affineTypeData affineCartanD4Twisted();
    (T3#"marks",T3#"markOneNodes",T3#"specialNodes",
        T3#"connectedFiniteDeletionNodes")
  Text
    Low-rank coincidences can have more than one valid type name.
    @TT "\"type\""@ gives one name,
    @TT "\"typeAliases\""@ gives all the recognized names, and
    @TT "\"typeIsAmbiguous\""@ indicates whether more than one applies.
  Example
    Tc = affineTypeData cartanAffineC 2;
    (Tc#"type",Tc#"typeAliases",Tc#"typeIsAmbiguous",
        Tc#"finiteFamily",Tc#"finiteFamilyAliases",
        Tc#"nodeCount",Tc#"cartanMatrixRank")
  Text
    Here the aliases are $B_2^{(1)}$ and $C_2^{(1)}$; neither name should be
    treated as uniquely intrinsic to this low-rank diagram.
  Text
    Results for marks, comarks, and affine type are saved during the current
    Macaulay2 session.  Equal Cartan matrices use the same saved results,
    but each answer uses the labels and realization of the supplied datum.
    @TO affineStructureCacheInfo@ counts the saved results, and
    @TO clearAffineStructureCache@ clears them.  Existing roots, weights,
    and data remain valid; later calculations compute the results again.
  Example
    clearAffineStructureCache();
    H0 = affineStructureCacheInfo();
    dummy = affineTypeData A;
    H1 = affineStructureCacheInfo();
    B = matrix entries A;
    dummy = affineMarks B;
    H2 = affineStructureCacheInfo();
    (H0#"cartanMatrices",H1#"cartanMatrices",H1#"typeData",
        H2#"cartanMatrices")
  Text
    The output $(0,1,1,1)$ shows that the equal reconstructed matrix uses
    the same saved affine result.
  Text
    The commands @TO affineCartanA2Twisted@,
    @TO affineCartanAOddTwisted@, @TO affineCartanDTwisted@,
    @TO affineCartanE6Twisted@, and @TO affineCartanD4Twisted@ provide
    canonical matrices for the twisted families $A_{2n}^{(2)}$,
    $A_{2n-1}^{(2)}$, $D_{n+1}^{(2)}$, $E_6^{(2)}$, and $D_4^{(3)}$.
    Their syntax is @TT "affineCartanA2Twisted n"@ for $n\geq1$,
    @TT "affineCartanAOddTwisted n"@ and
    @TT "affineCartanDTwisted n"@ for $n\geq2$, and
    @TT "affineCartanE6Twisted()"@ and
    @TT "affineCartanD4Twisted()"@ with no arguments.  Each matrix has
    its standard affine node in the first position.  The first three
    families have $n+1$ nodes; $E_6^{(2)}$ has five, and $D_4^{(3)}$
    has three.
    @TO affineStructuralCapabilities@ lists which calculations require
    only a Cartan matrix and which also require a realization.
SeeAlso
  CartanDatum
  Realization
  RootDatum
  KMRoot
  KMCoroot
  KMWeight
  KMCoweight
  NodeLabels
  NodeFormat
  dynkinDiagram
  dynkinDiagramData
  datumCoordinates
  realizationCoordinates
  pairing
  toWeight
  toCoweight
  weightFromRealizationCoordinates
  coweightFromRealizationCoordinates
  integratesToGlobalForm
  infrastructureCacheInfo
  clearInfrastructureCaches
  affineRootSnapshot
  rootSnapshot
  "Formal and completed characters"
///

doc ///
Key
  "Working with formal characters"
Headline
  Enter characters, take tensor products, and recover irreducible summands
Description
  Text
    A character records the dimensions of the weight spaces of a
    representation.  The term $m e^\mu$ means that the weight $\mu$
    occurs with multiplicity $m$.  The exponents displayed by
    @TO FormalCharacter@ are numerical tuples, and the note beneath the
    sum tells you which coordinates are being used.  We begin with
    $\mathfrak{sl}_3$, where these tuples are Dynkin labels.
  Example
    D = rootDatum "A2";
    Cown = formalCharacter(D,new Tally from {
        kmWeight(D,{1,0})=>1,
        kmWeight(D,{-1,1})=>1,
        kmWeight(D,{0,-1})=>1});
    Cown
    characterTerms Cown == characterTerms finiteCharacter(D,{1,0})
  Text
    This is the character
    $$e^{(1,0)}+e^{(-1,1)}+e^{(0,-1)}$$
    of the standard three-dimensional representation of
    $\mathfrak{sl}_3$.  Each weight space has dimension $1$.
    The final @TT "true"@ says that the character entered by hand agrees
    at every weight with the character computed by @TO finiteCharacter@.
    A tuple $(a,b)$ gives the evaluations on the two simple coroots;
    @TT "dynkinDiagram D"@ shows their node order.
  Text
    In a table supplied to @TO formalCharacter@, each @TO KMWeight@
    is followed by its integer coefficient.  The weights include their
    root datum, so a tuple is first written as @TT "kmWeight(D,tuple)"@.
    Negative coefficients are allowed in formal differences.
    Entering a complete finite sum does not, by itself, assert that the
    sum is the character of an actual representation.
  Example
    (characterDimension(Cown+dualCharacter Cown),
        characterDimension(Cown^2),characterDimension(3*Cown))
    Cown^0
    Cown-Cown
  Text
    The dimensions $(6,9,9)$ belong respectively to
    $V\oplus V^*$, $V\otimes V$, and three copies of $V$, where
    $V$ is the standard representation above.  The next outputs are
    $1$, the character of the trivial representation, and $0$, the
    character of the zero representation.  Addition gives direct sums,
    multiplication gives tensor products, and @TT "C^k"@ gives an
    ordinary tensor power for a nonnegative integer $k$.  Symmetric and
    exterior powers are computed instead by @TO symmetricPowerCharacter@
    and @TO exteriorPowerCharacter@.
  Text
    We now take a tensor product that has several different irreducible
    summands, including one that occurs twice.  Write $V(a,b)$ for the
    irreducible representation with highest-weight Dynkin labels $(a,b)$.
  Example
    C1 = finiteCharacter(D,{2,1})
    C2 = finiteCharacter(D,{1,1})
    (characterDimension C1,characterDimension C2)
  Text
    The two representations have dimensions $15$ and $8$.
    The second is the adjoint representation of $\mathfrak{sl}_3$.
    These characters were computed separately; multiplying them combines
    their weight spaces into the character of the tensor product.
  Example
    P = C1*C2;
    T = decomposeCharacter P;
    T
  Text
    The tuples in this table are now highest weights of irreducible
    summands, not the individual weights of $P$.  Each number after
    @TT "=>"@ gives the number of copies of that summand.  Thus the
    answer is
    $$V(2,1)\otimes V(1,1)\cong
      V(0,2)\oplus V(1,0)\oplus V(1,3)\oplus
      2V(2,1)\oplus V(3,2)\oplus V(4,0).$$
    Their dimensions are respectively $6$, $3$, $24$, $15$, $42$, and
    $15$, giving
    $$6+3+24+2\cdot15+42+15=120=15\cdot8.$$
  Text
    @TO decomposeCharacter@ finds a maximal remaining weight and subtracts
    the irreducible character with that highest weight, with its indicated
    multiplicity.  It continues until nothing remains.  For a complete
    finite-type character of a representation, this gives its irreducible
    decomposition.  A formal sum that is not a nonnegative sum of
    irreducible characters is not accepted as such a decomposition.
  Example
    Pback = characterFromDecomposition(D,T);
    (characterDimension P,
        characterTerms Pback == characterTerms P,
        decomposeCharacter Pback == T)
  Text
    The output @TT "(120, true, true)"@ checks more than the dimension:
    expanding the irreducible summands recovers every weight multiplicity
    of the product, and decomposing that expansion recovers the same
    summands.  @TO characterFromDecomposition@ can also be used to enter
    a representation by giving its irreducible summands rather than all
    of its weight spaces.  For a nonempty table, @TT "characterFromDecomposition T"@
    obtains the root datum from its highest weights.  An empty table
    requires @TT "characterFromDecomposition(D,new Tally)"@ and gives $0$.
  Text
    Character arithmetic requires a common root datum and compatible
    weight coordinates; the highest weights need not agree.  Equal
    independently constructed root data are allowed, but a matching
    Cartan matrix alone does not identify different character lattices
    or different realizations.  In particular, additional central
    coordinates must not be discarded.  Here is a small example with
    an $A_1$ factor and a one-dimensional center.
  Example
    Rcentral = realization(cartanDatum(cartanA 2),
        matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
    Dcentral = rootDatum Rcentral;
    lambda0 = weightFromRealizationCoordinates(Dcentral,{1,0})
    lambda3 = weightFromRealizationCoordinates(Dcentral,{1,3})
    Ccentral = finiteCharacter(Dcentral,lambda0)+finiteCharacter(Dcentral,lambda3)
    apply(pairs decomposeCharacter Ccentral,
        p -> {realizationCoordinates p#0,p#1})
    characterDimension Ccentral
  Text
    The answer lists the full highest weights $(1,0)$ and $(1,3)$,
    each with multiplicity $1$.  In these coordinates the first entry
    is the $A_1$ Dynkin label and the second is the central coordinate.
    Both summands are two-dimensional as $A_1$ representations, but the
    center acts differently on them.  They therefore remain two distinct
    summands, and their direct sum has dimension $4$.
  Text
    Finally, a finite portion of an infinite character is different from
    a complete finite sum.  A Verma module already gives an example in
    type $A_2$.
  Example
    M = vermaModule(D,{1,0});
    Cpartial = moduleCharacter(M,Depth=>2);
    Cpartial
    (isExactCharacter Cpartial,
        characterCoefficient(Cpartial,kmWeight(D,{0,-1})))
  Text
    The ellipsis means that the Verma character continues beyond the
    displayed terms.  @TT "Depth=>2"@ computes all weights
    $\lambda-b_1\alpha_1-b_2\alpha_2$ with $b_1,b_2\geq0$ and
    $b_1+b_2\leq2$, where $\lambda=(1,0)$.  The output
    @TT "(false, 2)"@ says that the expansion is not complete and that
    the weight $(0,-1)=\lambda-\alpha_1-\alpha_2$ has multiplicity $2$.
    The two contributions come from the negative root vector for
    $\alpha_1+\alpha_2$ and the product of the negative simple-root
    vectors in a Poincare--Birkhoff--Witt basis.
  Text
    A coefficient beyond the computed depth is unknown, not zero.
    @TO characterCoefficient@ reports an error if that coefficient has
    not been determined; increase @TT "Depth"@ in the original
    calculation to obtain it.  @TO characterDimension@ and
    @TO decomposeCharacter@ do not treat a partial expansion as a
    complete character.  Arithmetic and Weyl actions retain the
    distinction between known and unknown coefficients.  The precise
    rules for partial expansions, and the full list of character
    commands, are in @TO "Formal and completed characters"@.
SeeAlso
  "Formal and completed characters"
  "Independent multiplicity algorithms"
  "Reductive Levi restriction"
  "Lambda operations and representation indices"
///

doc ///
Key
  "Formal and completed characters"
  (symbol *,FormalCharacter,FormalCharacter)
  (symbol *,FormalCharacter,ZZ)
  (symbol *,ZZ,FormalCharacter)
  (symbol +,FormalCharacter,FormalCharacter)
  (symbol -,FormalCharacter)
  (symbol -,FormalCharacter,FormalCharacter)
  (symbol ^,FormalCharacter,ZZ)
  (symbol ==,FormalCharacter,FormalCharacter)
  (symbol ==,KMRepresentation,KMRepresentation)
Headline
  Characters in the group ring and its root-depth completion
Usage
  C = formalCharacter(D,T)
  C = finiteCharacter(D,lambda)
  M = integrableHighestWeightModule(D,lambda)
  M = vermaModule(D,lambda)
  C = moduleCharacter(M,Depth=>H)
  C = vermaCharacter(M,Depth=>H)
  C = integrableCharacter(M,Depth=>H)
  D = characterDatum C
  T = characterTerms C
  S = characterSupport C
  m = characterCoefficient(C,mu)
  b = isExactCharacter C
  n = characterDimension C
  Cd = dualCharacter C
  T = decomposeCharacter C
  C = characterFromDecomposition(D,T)
  C = characterFromDecomposition T
  Csum = C1+C2
  Cpow = C^k
Inputs
  D:RootDatum
    the root datum
  T:HashTable
    weights with integral coefficients for @TT "formalCharacter"@, or dominant highest weights with nonnegative multiplicities for @TT "characterFromDecomposition"@; a @TO Tally@ is also accepted
  lambda:KMWeight
    a highest weight in @TT "D"@
  mu:KMWeight
    a weight in the same datum as the character
  M:KMRepresentation
    a Verma module or an integrable highest-weight module
  H:ZZ
    a nonnegative root-depth bound
  k:ZZ
    a nonnegative integer, giving the number of tensor factors in an ordinary character power
Outputs
  C:FormalCharacter
    an exact character of finite support or a partial expansion with a specified range of known coefficients
  M:KMRepresentation
    a highest-weight module specified by its datum and highest weight
Description
  Text
    A @TO FormalCharacter@ represents
    $\sum_\mu c_\mu e^\mu$ with @TO KMWeight@ keys belonging to one
    @TO RootDatum@.  Addition is coefficientwise and multiplication is the
    convolution product $e^\lambda e^\mu=e^{\lambda+\mu}$, so multiplying
    actual characters computes a tensor-product character.  Scalar
    multiplication, subtraction, Weyl action, and @TO dualCharacter@ retain
    the root datum and the indication of whether the character is complete
    or truncated.
  Text
    Characters of different representations can be added as long as they
    use the same root datum and weight coordinates.  Their highest weights
    need not agree.  Independently constructed root data with identical
    mathematical contents are accepted; the package remembers each
    comparison for later arithmetic.  A matching Cartan matrix alone does
    not identify different realizations, central directions, node
    conventions, or chosen character lattices.  Complete weights and
    Dynkin-label classes are never mixed implicitly.
  Text
    For a nonnegative integer @TT "k"@, @TT "C^k"@ is the ordinary
    product of @TT "k"@ copies of the character.  For an actual module
    character this gives its tensor power, not its symmetric power.
    @TT "C^0"@ is the exact character $1$ of the trivial representation,
    and @TT "C^1"@ is @TT "C"@.  Negative powers are not defined.
  Example
    Darith = rootDatum "A1";
    Cfund = finiteCharacter(Darith,{1})
    Cadj = finiteCharacter(rootDatum "A1",{2})
    Cfund+Cadj
    Cfund^3
    decomposeCharacter(Cfund^3)
    (Cfund^0,Cfund^2-Cadj)
  Text
    The sum is the character of $V(1)\oplus V(2)$, of dimension $5$.
    The cube has character
    $e^{(3)}+3e^{(1)}+3e^{(-1)}+e^{(-3)}$, and its decomposition says
    $$V(1)^{\otimes3}\cong V(3)\oplus2V(1).$$
    Both entries in the last output are $1$: the zeroth tensor power is
    trivial, and $V(1)\otimes V(1)\cong V(2)\oplus V(0)$.
  Example
    Tcube = decomposeCharacter(Cfund^3);
    Ccube = characterFromDecomposition Tcube;
    Ccube
    (characterTerms Ccube == characterTerms(Cfund^3),
        decomposeCharacter Ccube == Tcube)
    characterFromDecomposition(Darith,new Tally)
  Text
    The reconstructed character is again
    $e^{(3)}+3e^{(1)}+3e^{(-1)}+e^{(-3)}$.
    The two @TT "true"@ entries check both directions: expanding the
    irreducible summands recovers every weight multiplicity, and
    decomposing the expansion recovers the original summands.  The final
    output is $0$, the character of the zero representation.
  Text
    In @TT "formalCharacter(D,T)"@, the keys of @TT "T"@ must be
    @TO KMWeight@ objects in @TT "D"@, not coordinate lists.  Negative
    coefficients are allowed for a virtual character, and terms with
    coefficient zero are omitted.  To enter a weight by its Dynkin labels,
    first use @TT "kmWeight(D,labels)"@.  The constructors
    @TO finiteCharacter@, @TO integrableHighestWeightModule@, and
    @TO vermaModule@ accept a Dynkin-label list directly in place of
    @TT "lambda"@.  @TO finiteCharacter@ also accepts a Cartan matrix or
    @TO CartanDatum@ as its first argument when the highest weight is a list.
    An integrable highest weight must be dominant integral; the highest
    weight of a Verma module need not be dominant.
  Example
    D = rootDatum "A2";
    C1 = finiteCharacter(D,{1,0});
    C2 = finiteCharacter(D,{0,1})
    C1
    (characterDimension C1,characterDimension(C1*C2),
        apply(pairs decomposeCharacter(C1*C2),
            p -> {datumCoordinates p#0,p#1}))
  Text
    The exponents of @TT "C1"@, $(1,0)$, $(-1,1)$, and $(0,-1)$, are
    Dynkin labels.  Thus this character means
    $$\operatorname{ch}V(\omega _1)
      =e^{\omega_1}+e^{-\omega_1+\omega_2}+e^{-\omega_2}.$$
    Here $\omega_i$ is the fundamental weight at node $i$, and $e^\mu$
    records the weight $\mu$.  Each of these three weight spaces has
    dimension $1$.  The final tuple says
    $\dim V(\omega_1)=3$, and
    $$V(\omega_1)\otimes V(\omega_2)
      \cong V(\omega_1+\omega_2)\oplus V(0),$$
    whose dimensions sum to $9$.  The two entries in the final list are
    highest weights $(1,1)$ and $(0,0)$, each with multiplicity $1$.
  Text
    @TO characterDatum@ returns the root datum,
    @TO characterTerms@ gives the coefficients,
    @TO characterSupport@ lists the computed weights with nonzero coefficients,
    and @TO characterCoefficient@ gives a selected coefficient.  For an
    exact character, a weight absent from the terms has coefficient zero.
    For a highest-weight character computed through @TT "Depth=>H"@,
    every coefficient at $\lambda-\sum_i b_i\alpha_i$ with
    $b_i\geq0$ and $\sum_i b_i\leq H$ is known, including coefficients
    that vanish.  Weights outside $\lambda-Q_+$ also have coefficient
    zero.  A query deeper inside this cone reports an error, rather than
    treating an uncomputed coefficient as zero; increase @TT "Depth"@
    in the original calculation to obtain it.
    @TO characterDimension@
    sums coefficients only for an exact character.  @TO decomposeCharacter@
    decomposes a character by successively subtracting the irreducible
    characters of its highest weights.  It requires finite type, integral
    Dynkin labels, and a complete character that is a nonnegative sum of
    irreducible characters.  Complete weights may have arbitrary rational
    central coordinates.  Distinct root-lattice cosets, including different
    central characters, are decomposed separately.
    Its answer is a @TO Tally@ whose keys are the highest weights in the
    original datum and whose values are their direct-summand multiplicities.
    The subtraction works because an irreducible character has coefficient
    $1$ at its highest weight, and all of its other weights are lower.
    The coefficient at a maximal remaining weight is therefore the
    multiplicity of that irreducible summand.
    @TO characterFromDecomposition@ performs the inverse conversion:
    a table $\{\lambda\mapsto m_\lambda\}$ becomes
    $\sum_\lambda m_\lambda\operatorname{ch}V(\lambda)$.
    Its keys must be @TO KMWeight@ objects with dominant integral Dynkin
    labels, all in one root datum, and its multiplicities must be
    nonnegative integers.  Full central coordinates are retained.
    For a nonempty table, the datum can be read from its keys; an empty
    table requires @TT "characterFromDecomposition(D,T)"@.
    These two conversions are inverse on complete finite-type module
    characters and their irreducible decompositions, after zero
    multiplicities have been omitted.  They do not interpret a finite
    portion of an affine or indefinite character as a complete answer.
    @TO dualCharacter@ replaces each exponent $\mu$ by $-\mu$.
    The expression @TT "weylAction(w,C)"@ applies the Weyl element
    @TT "w"@ to every exponent; its Cartan datum must agree with that
    of the character.
    Equality of @TO FormalCharacter@ objects compares their datum,
    terms, and exact or truncated status, including the stated truncation.
    Two printed expressions alone need not describe equal characters.
    Equality of @TO KMRepresentation@ objects also includes the kind of
    module, not just its highest weight.
    On a truncated character, Weyl action and @TO dualCharacter@ carry
    the known coefficients to the transformed weights.  Thus a
    coefficient at $\mu$ in @TT "weylAction(w,C)"@ is known precisely
    when the coefficient at $w^{-1}\mu$ in @TT "C"@ is known; in
    @TT "dualCharacter C"@ the corresponding weight is $-\mu$.
    Their displays say @TT "truncated"@, not that the original ordinary
    root-depth bound holds after the transformation.
  Text
    Arithmetic on partial expansions likewise records only coefficients
    determined by the inputs.  In a sum or difference, the coefficient
    at a weight is known only when it is known in both characters.
    Multiplication by an exact finite character uses a finite sum of
    coefficients of the other factor; all these coefficients must be
    known for the resulting coefficient to be reported.  Two
    highest-weight root-depth expansions can be multiplied through the
    smaller depth, measured from the sum of their highest weights.
    Two arbitrary partial expansions need not have a well-defined
    infinite convolution, so their product is not accepted without
    this highest-weight condition.  A term absent from
    @TO characterTerms@ is therefore not by itself a vanishing assertion.
    For a highest-weight expansion known through depth $H$, a positive
    power remains known through depth $H$, now below the highest weight
    $k\lambda$; the bound does not become $kH$.  Powers above one of an
    arbitrary partial expansion obey the same restrictions as its product.
  Text
    A character is written as a sum of exponentials of weights.  Thus
    @TT "2*e^(1, -1)"@ in finite type means that the weight space of
    $\omega_1-\omega_2$ has dimension $2$, and a constant term $2$
    denotes a two-dimensional zero-weight space.  The exponents remain
    numerical tuples.  In finite type with no additional Cartan directions,
    they give Dynkin labels in node order.  In singular type, or a
    realization with extra directions, a complete weight uses its full
    realization tuple; the note below the character identifies this
    convention.  @TO datumCoordinates@ and @TO realizationCoordinates@
    give these two coordinate lists separately.  Characters of weight
    classes are marked modulo the annihilator of the simple coroots.
  Text
    Small characters show the complete sum.  Larger finite characters
    show its leading terms, the number of omitted terms, and its exact
    dimension; @TO characterTerms@ gives the full sum.  The order respects
    dominance for comparable weights.  The order among incomparable
    weights has no mathematical significance.  A truncated character
    includes an ellipsis.  When its known range is an ordinary
    highest-weight root-depth range, the display gives that depth;
    otherwise it says @TT "truncated"@.  The ellipsis does not assert
    that the omitted coefficients vanish.
  Example
    largeCharacter = finiteCharacter(D,{4,4});
    largeCharacter
  Text
    @TT "CharacterStatus=>\"Exact\""@ is the default for a directly
    constructed finite tally.  A truncated character must instead use
    @TT "CharacterStatus=>\"Truncated\""@ and supply a
    @TT "Truncation"@ table describing the partial expansion.
    For a directly entered table, only the coefficients explicitly
    supplied are taken as known.  Writing a depth in this table does not
    establish that missing coefficients vanish; the highest-weight
    character commands compute their own complete depth ranges.
    A character is exact only when the computed expansion is complete;
    the number of displayed terms does not determine this.
  Example
    lambda = kmWeight(D,{1,0});
    V = formalCharacter(D,new Tally from {lambda=>1},
        CharacterStatus=>"Truncated",
        Truncation=>new HashTable from {"completeThroughDepth"=>0});
    V
    (isExactCharacter C1,isExactCharacter V,
        characterCoefficient(V,lambda))
  Text
    The second character contains the known leading monomial
    $e^{\omega_1}$ with coefficient $1$, but its status says that it must not
    be treated as the complete three-dimensional character.
  Text
    @TO integrableHighestWeightModule@ and @TO vermaModule@ specify modules
    by their root data and highest weights.  In finite type, an integrable
    module with no depth option has the exact character returned by
    @TO finiteCharacter@.  A Verma module,
    or an integrable module in affine or indefinite type, is returned through
    a root-depth completion.  @TT "Depth=>H"@ means that every coefficient
    of $e^{\lambda-\beta}$ with
    $\beta=\sum_i b_i\alpha_i$, $b_i\geq0$, and $\sum_i b_i\le H$ has
    been computed.
    @TO moduleCharacter@ chooses the character appropriate to the module.
    The two commands @TO vermaCharacter@ and @TO integrableCharacter@ take
    that same module as their argument, not a pair consisting of a datum
    and a highest weight.  @TT "Depth"@ is required for every Verma
    character and for every affine or indefinite integrable character.
    Supplying it in finite type asks for a root-depth expansion rather
    than an exact finite character.
  Example
    M = vermaModule(D,{1,0});
    C = moduleCharacter(M,Depth=>2);
    C
    (isExactCharacter C,#characterSupport C,
        characterCoefficient(C,kmWeight(D,{-1,1})))
  Text
    This is the product expansion
    $e^\lambda\prod_{\alpha>0}(1-e^{-\alpha})^{-\operatorname{mult}\alpha}$
    through depth $2$.  The coefficient $1$ at Dynkin labels $(-1,1)$ is the
    weight $\lambda-\alpha_1$.  All coefficients through depth $2$ have
    been computed, but this is only a finite part of the Verma character.
  Text
    Nonfinite calls require a highest weight with complete realization
    coordinates.  In singular type, Dynkin labels alone do not determine a
    weight.  For example, in affine type $\lambda$ and $\lambda+k\delta$
    have the same labels, although their characters have different grades.
  Text
    The same distinction is respected in a nonminimal finite realization.
    If the highest weight is a complete @TO KMWeight@, @TO finiteCharacter@,
    @TO vermaCharacter@, and @TO integrableCharacter@ retain every
    Weyl-fixed central coordinate in each weight.  If the highest weight is
    supplied only as a Dynkin-label list, the output gives weight classes
    modulo those extra directions.  A single formal character cannot mix
    complete weights with label classes,
    since their sum would belong to no well-defined group ring.
  Example
    Rcentral = realization(cartanDatum(cartanA 2),
        matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
    Dcentral = rootDatum Rcentral;
    lambda0 = weightFromRealizationCoordinates(Dcentral,{1,0})
    lambda3 = weightFromRealizationCoordinates(Dcentral,{1,3})
    C = finiteCharacter(Dcentral,lambda0)+finiteCharacter(Dcentral,lambda3)
    apply(pairs decomposeCharacter C,
        p -> {realizationCoordinates p#0,p#1})
    characterDimension C
  Text
    Both highest weights have Dynkin label $(1)$, but their central
    coordinates are $0$ and $3$.  In this realization, $(a,z)$ records
    Dynkin label $a$ and central coordinate $z$.  Each output pair consists
    of the full realization tuple and its direct-summand multiplicity, so
    the answer is
    $$V(1)_0\oplus V(1)_3,$$
    with each summand occurring once.  Here $V(1)$ is the two-dimensional
    irreducible $A_1$ representation, and the subscript is the central
    coordinate.  The total dimension is $4$; the two summands remain
    distinct despite their equal Dynkin labels.
SeeAlso
  "Root data and representations"
  "Working with formal characters"
  crystalCharacter
  demazureOperator
  bggEulerCharacter
  "Reductive Levi restriction"
///

doc ///
Key
  "Reductive Levi restriction"
  (symbol *,LeviFormalCharacter,LeviFormalCharacter)
  (symbol *,LeviFormalCharacter,ZZ)
  (symbol *,ZZ,LeviFormalCharacter)
  (symbol +,LeviFormalCharacter,LeviFormalCharacter)
  (symbol -,LeviFormalCharacter)
  (symbol -,LeviFormalCharacter,LeviFormalCharacter)
  (symbol ^,LeviFormalCharacter,ZZ)
  (symbol ==,LeviFormalCharacter,LeviFormalCharacter)
  (symbol ==,LeviHighestWeight,LeviHighestWeight)
  (symbol ==,LeviWeight,LeviWeight)
  (symbol ==,ReductiveLeviDatum,ReductiveLeviDatum)
Headline
  Restrict characters to the full reductive Levi without discarding its center
Usage
  L = reductiveLeviDatum(P,D)
  Z = leviCenterBasis L
  muL = restrictWeight(L,mu)
  CL = restrictCharacter(L,C)
  muS = semisimplePart muL
  z = centralCharacter muL
  T = decomposeLeviCharacter CL
  S = CL+DL
  T = CL*DL
  E = CL^k
Inputs
  P:StandardParabolic
    a standard parabolic for the Cartan datum of @TT "D"@
  D:RootDatum
    the ambient root datum
  mu:KMWeight
    a weight with complete ambient realization coordinates
  C:FormalCharacter
    an ambient character
  DL:LeviFormalCharacter
    another character for the same full reductive Levi datum as @TT "CL"@
  k:ZZ
    a nonnegative tensor-power exponent
Outputs
  L:ReductiveLeviDatum
    the retained-node root datum together with a basis for its center; the retained Cartan matrix must be nonsingular
  muL:LeviWeight
    a retained-node weight and a central character
  CL:LeviFormalCharacter
    the restricted character
  T:Tally
    irreducible @TO LeviHighestWeight@ keys with multiplicities
Description
  Text
    In finite type, restriction to the semisimple derived Levi remembers
    only Dynkin labels on the Levi nodes.  The full reductive Levi also
    contains a center.  A
    @TO ReductiveLeviDatum@ computes that center as the kernel of the retained
    simple roots in the ambient realization; @TO leviCenterBasis@ displays
    its chosen basis.  A @TO LeviWeight@ then consists of a weight for
    the retained nodes and its values on this center.
    Each row of @TT "leviCenterBasis L"@ is a center generator in the
    ambient realization coordinates; the central-character list gives
    the weight's values on these rows in the same order.
    These coordinates also give a faithful restriction when the retained
    Cartan matrix is nonsingular and indefinite, although decomposition
    by @TO decomposeLeviCharacter@ requires a finite-type Levi.
  Text
    The retained Cartan matrix must be nonsingular.  Precisely in this
    case, the retained simple coroots together with the displayed center
    basis span the ambient Cartan subalgebra, so their evaluations
    distinguish complete ambient weights.  For a singular retained
    matrix, these two subspaces intersect and miss other Cartan directions.
    For example, retaining both nodes of $A_1^{(1)}$ would identify
    weights differing by the null root $\delta$.  Such a restriction is
    rejected rather than assigning the same character coefficient to
    distinct weights.  An ambient affine or indefinite datum is itself
    allowed when the retained matrix is nonsingular.
  Text
    Full realization coordinates are required for @TO restrictWeight@.  This
    requirement prevents two ambient weights with the same Levi Dynkin labels
    but different central characters from being silently merged.
    The shorter expressions @TT "restrictWeight(P,mu)"@ and
    @TT "restrictCharacter(P,C)"@ also work: the ambient root datum is
    taken from the weight or character.  To compare several restricted
    weights or characters in one Levi datum, construct @TT "L"@ once
    and use it in every restriction.
  Example
    D = rootDatum "A2";
    P = standardParabolic(D,RemovedNodes=>{1});
    L = reductiveLeviDatum(P,D);
    C = finiteCharacter(D,{1,0})
    CL = restrictCharacter(L,C);
    CL
    T = decomposeLeviCharacter CL;
    T
    (leviCenterBasis L,
        apply(pairs T,p -> {datumCoordinates semisimplePart p#0,
            centralCharacter p#0,p#1}))
  Text
    The note @TT "coordinates: 1 Levi Dynkin, then 1 central"@
    identifies the two entries of each exponent: the first is the
    semisimple Dynkin label, and the second is its value on the chosen
    center generator.  The numerical weights $(1,-1)$, $(0,2)$, and
    $(-1,-1)$ therefore give the character
    $$e^{\omega_2-\zeta_1}+e^{2\zeta_1}
      +e^{-\omega_2-\zeta_1}.$$
    Here $\omega_2$ is the fundamental weight of the semisimple Levi at
    the retained node $2$.  If $z_1$ is the center generator returned by
    @TO leviCenterBasis@, then $\zeta_1(z_1)=1$; the coefficient of
    $\zeta_1$ records the weight's value on that center generator.
    Thus the two weights $\omega_2$ and $-\omega_2$ both have central
    value $-1$, while the semisimple zero weight has central value $2$.
  Text
    With the displayed center generator, the answer translates to
    $$V_{A_2}(\omega_1)\big|_L
      \cong V_{A_1}(0)_{,2}\oplus V_{A_1}(\omega)_{,-1}.$$
    We write $\omega$ for the Levi fundamental weight at retained node $2$.
    The keys @TT "V(0, 2)"@ and @TT "V(1, -1)"@ combine the
    semisimple highest-weight label with its central value.
    The subscripts in the formula above are the two central characters;
    the final entry in each output triple is the direct-summand
    multiplicity $1$.
  Text
    @TO semisimplePart@ and @TO centralCharacter@ give these two parts for both
    @TO LeviWeight@ and @TO LeviHighestWeight@.  @TO restrictCharacter@
    preserves exact or truncated status.  For a partial character, its
    known coefficients are carried to the corresponding Levi weights.
    A displayed root-depth bound still refers to the original ambient
    simple roots, not to the retained Levi roots; a coefficient not
    determined by the ambient calculation reports an error.  By contrast,
    @TO decomposeLeviCharacter@
    requires a complete character of finite support and decomposes the
    summands with each central character separately.
    The retained Levi must be of finite type for this decomposition,
    and each semisimple character must be a nonnegative integral sum
    of irreducible characters.  Arithmetic allows virtual characters,
    but this decomposition command rejects negative irreducible
    multiplicities.  The commands @TO characterDatum@, @TO characterTerms@,
    @TO characterSupport@, @TO characterCoefficient@, and
    @TO isExactCharacter@ also apply to a @TO LeviFormalCharacter@;
    the selected coefficient is then specified by a @TO LeviWeight@.
    @TO characterDimension@ sums the coefficients of an exact Levi
    character and rejects a truncated Levi character.
    Equality of Levi weights includes the Levi datum and the central
    character.  Equality of Levi characters also includes their terms
    and exact or truncated status.
  Text
    Restricted characters admit the usual character arithmetic:
    @TT "CL+DL"@, @TT "CL-DL"@, @TT "-CL"@, integer multiples,
    @TT "CL*DL"@, and @TT "CL^k"@ for a nonnegative integer $k$.
    Addition describes a direct sum and multiplication describes a
    tensor product.  In each product of terms, both the semisimple
    weights and their central characters are added.  Thus central
    coordinates remain part of the calculation, rather than being
    discarded after restriction.  Negative coefficients are allowed
    for virtual characters.  Both operands must belong to the same
    full @TO ReductiveLeviDatum@; having isomorphic semisimple parts
    alone does not suffice.
  Example
    CD = finiteCharacter(D,{0,1})
    CDL = restrictCharacter(L,CD)
    CT = CL*CDL
    (CT == restrictCharacter(L,C*CD),characterDimension CT)
    decomposeLeviCharacter CT
  Text
    The output @TT "(true, 9)"@ verifies that restriction commutes
    with this tensor product.  Here $C$ is the character of the
    three-dimensional standard $A_2$ representation and $CD$ is the
    character of its dual.  Writing $V(a)_z$ for the $A_1$ irreducible
    of highest Dynkin label $a$ and central value $z$, the displayed
    decomposition is
    $$2V(0)_0\oplus V(2)_0\oplus V(1)_3\oplus V(1)_{-3}.$$
    Its dimensions are $2+3+2+2=9$.  In particular, the two copies of
    the semisimple standard representation have different central
    characters and remain distinct summands.
  Example
    CA = restrictCharacter(L,finiteCharacter(D,{1,1}))
    CT-CA
    (2*CL-CL == CL, -CL+CL == 0*CL)
    CL^0
    (CL^2 == CL*CL,characterDimension(CL^2))
  Text
    Since the standard representation tensored with its dual is the
    direct sum of the trivial and adjoint representations, subtracting
    the restricted adjoint character leaves $e^{(0,0)}$.  The two
    entries record zero semisimple weight and zero central character.
    The next pair of booleans checks cancellation in the group ring.
    The zeroth power is the same trivial character, while the square
    is a tensor square of dimension $9$, not the symmetric square.
  Text
    Arithmetic on partial Levi characters follows the same rules as
    arithmetic on @TO FormalCharacter@.  A sum reports a coefficient
    only when both summands determine it.  An exact finite factor can
    be multiplied by a partial character, with a resulting coefficient
    known only when all of its contributions are known.  Two
    highest-weight root-depth expansions can be multiplied through the
    smaller of their two depths, measured below the sum of their
    highest weights.  These depths still refer to the ambient simple
    roots.  Restriction does not turn an arbitrary pair of partial
    expansions into a well-defined infinite product.
    The zeroth power and multiplication by zero are exact; other
    arithmetic does not treat unknown coefficients as zero.
SeeAlso
  standardParabolic
  leviHighestWeights
  parabolicData
  finiteCharacter
///

doc ///
Key
  "Independent multiplicity algorithms"
  [weightMultiplicity,Strategy]
  [weightMultiplicityTable,Strategy]
Headline
  Compute finite weight multiplicities by Kostant's formula and cross-check Freudenthal recursion
Usage
  p = kostantPartitionFunction(A,beta)
  m = kostantWeightMultiplicity(A,lambda,mu)
  m = weightMultiplicity(A,lambda,mu,Strategy=>strategy)
  p = kostantPartitionFunction(D,betaRoot)
  m = kostantWeightMultiplicity(D,lambdaWeight,muWeight)
Inputs
  A:Matrix
    a finite-type Cartan matrix
  beta:List
    integral simple-root coordinates
  lambda:List
    a dominant integral highest weight in Dynkin labels
  mu:List
    an integral weight in Dynkin labels
  strategy:String
    @TT "\"Freudenthal\""@, @TT "\"Kostant\""@, or @TT "\"Auto\""@
  D:RootDatum
    a finite root datum
  betaRoot:KMRoot
    a root-lattice element in @TT "D"@
  lambdaWeight:KMWeight
    a dominant integral weight in @TT "D"@
  muWeight:KMWeight
    an integral weight in @TT "D"@
Outputs
  p:ZZ
    the Kostant partition number $P(\beta)$
  m:ZZ
    the multiplicity of $\mu$ in $V(\lambda)$
Description
  Text
    @TO kostantPartitionFunction@ counts decompositions of $\beta$ as a sum
    of positive roots, with order ignored and repetition allowed.  It is the
    coefficient of $e^{-\beta}$ in
    $\prod_{\alpha>0}(1-e^{-\alpha})^{-1}$.
    The zero vector has partition number $1$, corresponding to the empty
    sum, and a vector with a negative simple-root coordinate has partition
    number $0$.  The first argument may also be a @TO CartanDatum@ when
    @TT "beta"@ is a list.
  Example
    A = cartanA 3;
    kostantPartitionFunction(A,{1,1})
  Text
    The answer is $2$ because
    $\alpha_1+\alpha_2$ is either the single positive root
    $(\alpha_1+\alpha_2)$ or the sum of the two simple roots.
  Text
    @TO kostantWeightMultiplicity@ evaluates
    $$m_\lambda(\mu)=
      \sum_{w\in W}(-1)^{\ell(w)}
      P\bigl(w(\lambda+\rho)-(\mu+\rho)\bigr).$$
    The partition numbers must be computed for every argument appearing in
    this sum.  Some arguments can have simple-root coordinates larger than
    those of $\lambda-\mu$, even when their contributions eventually cancel.
  Example
    (kostantWeightMultiplicity(A,{1,1},{0,0}),
     weightMultiplicity(A,{1,1},{0,0},Strategy=>"Freudenthal"),
     weightMultiplicity(A,{1,1},{0,0},Strategy=>"Kostant"))
  Text
    All three entries are $2$, the rank of $\mathfrak{sl}_3$ and hence the
    multiplicity of the zero weight in its adjoint representation.  The
    first and third use Kostant's alternating formula; the middle entry uses
    Freudenthal recursion, giving an independent verification.
  Text
    The @TT "Strategy"@ option is accepted by @TO weightMultiplicity@ and
    @TO weightMultiplicityTable@.  @TT "\"Freudenthal\""@ and
    @TT "\"Kostant\""@ select the named algorithms explicitly;
    @TT "\"Auto\""@ uses the package's choice of algorithm.
    @TO kostantPartitionFunction@ and @TO kostantWeightMultiplicity@ also
    accept roots and weights in a specified datum; all must belong to that
    datum.
  Text
    For complete weights, @TO kostantWeightMultiplicity@ compares the
    weights in their full realization.  Before applying Kostant's formula,
    it verifies that $\lambda-\mu$ is a nonnegative integral combination
    of the actual simple roots.  Otherwise the multiplicity is zero.
    In particular, weights with different central characters cannot
    occur in the same irreducible highest-weight module, even when their
    Dynkin labels agree.
    Two Dynkin-label classes are instead compared modulo the annihilator
    of the simple coroots.  In a nonminimal realization, a complete
    weight cannot be compared with a Dynkin-label class: supply both
    complete weights or both label classes.  In a minimal finite
    realization the Dynkin labels already determine the complete weight.
SeeAlso
  weightMultiplicity
  weightMultiplicityTable
  weightsOfRep
  representationCacheInfo
///

doc ///
Key
  "Borel-Weil-Bott and Demazure operators"
  (symbol ==,BorelWeilBottResult,BorelWeilBottResult)
Headline
  Compute finite Borel-Weil-Bott cohomology and Kac-Moody Demazure characters
Usage
  R = borelWeilBott(D,lambda)
  C = borelWeilBottCharacter(D,lambda)
  C = demazureOperator(i,C0)
  C = demazureCharacter(D,lambda,word)
Inputs
  D:RootDatum
    a root datum; finite type is required by @TT "borelWeilBott"@
  lambda:KMWeight
    an integral line-bundle weight or dominant Demazure highest weight
  C0:FormalCharacter
    a finite sum of weight exponentials on which the selected operator acts
  i:ZZ
    a 1-based simple-reflection index
  word:List
    a Weyl word, with its rightmost reflection acting first
Outputs
  R:BorelWeilBottResult
    vanishing status, cohomological degree, dominant dot translate, dual highest weight, and Weyl witness
  C:FormalCharacter
    the resulting exact finite character
Description
  Text
    For semisimple data, let $\widetilde G$ be the simply connected group with the given
    semisimple Lie algebra, and let $\widetilde B$ be a Borel subgroup.
    The line-bundle convention is
    $\mathcal L_\lambda=\widetilde G\mathbin{\times}^{\widetilde B}
      \mathbb C_{-\lambda}$.
    If $\lambda+\rho$ is singular, all cohomology vanishes.  Otherwise there
    is a unique $w$ with $w(\lambda+\rho)$ dominant, and
    $$H^{\ell(w)}(\widetilde G/\widetilde B,\mathcal L_\lambda)
      \cong V(w\mathbin{\cdot}\lambda)^*,$$
    with all other degrees zero.  @TO borelWeilBott@ performs the chamber
    walk and records $w$; @TO borelWeilBottCharacter@ returns the character
    of the displayed dual representation.  The only accepted
    @TT "LineBundleConvention"@ value is @TT "\"FiberMinusWeight\""@, so a
    change of sign cannot be implicit.
    Both Borel--Weil--Bott commands accept a Dynkin-label list in place
    of the weight.  @TT "borelWeilBott(A,labels)"@ also accepts a
    finite-type Cartan matrix @TT "A"@; for its character use
    @TT "borelWeilBottCharacter(rootDatum A,labels)"@.
    The result's entries @TT "degree"@,
    @TT "dominantWeight"@, @TT "cohomologyHighestWeight"@, and
    @TT "weylElement"@ give the degree, dominant dot translate, highest
    weight of its dual, and Weyl element.  In the vanishing case the
    first three entries are @TT "null"@.
    Equality of results compares these entries together with the input
    weight and datum, not just the displayed cohomology.
    The flag variety is unchanged when the simply connected group is
    replaced by a central quotient.  Its line bundles need not all admit
    an action of that quotient, however.  The two commands compute
    cohomology as a Lie-algebra representation, which in the semisimple
    case is also a representation of the simply connected group.  They do not require
    the line-bundle weight to belong to the character lattice selected by
    @TO GlobalForm@.  When @TT "integratesToGlobalForm lambda"@ is
    @TT "true"@, the line bundle and its cohomology also carry the action
    of the chosen group $G$, and one may write
    $\mathcal L_\lambda=G\mathbin{\times}^B\mathbb C_{-\lambda}$.
    A complete weight in a nonminimal finite realization also specifies
    the action of the additional center.  The flag-variety calculation
    uses its semisimple Dynkin labels, while the resulting dual
    representation retains the opposite central character.  For such
    data the group interpretation additionally requires the full weight,
    including these central coordinates, to belong to the chosen
    character lattice.
  Example
    D1 = rootDatum "A1";
    R0 = borelWeilBott(D1,{4});
    R1 = borelWeilBott(D1,{-3});
    Rv = borelWeilBott(D1,{-1});
    R0
    R1
    Rv
    ((R0#"vanishes",R0#"degree",
        datumCoordinates R0#"cohomologyHighestWeight"),
     (R1#"vanishes",R1#"degree",
        datumCoordinates R1#"cohomologyHighestWeight"),
     (Rv#"vanishes",Rv#"degree"))
  Text
    These three outputs mean
    $H^0(\mathcal L_4)\cong V(4)^*$,
    $H^1(\mathcal L_{-3})\cong V(1)^*$, and
    $H^\bullet(\mathcal L_{-1})=0$.  They are the complete $SL_2/B$
    form of Borel--Weil--Bott, including the singular wall.  The displayed
    @TT "0"@ in the third case means that all cohomology vanishes.  Here
    $V(k)$ means $V(k\omega_1)$; these $SL_2$ representations are
    isomorphic to their duals.
  Example
    Dad = rootDatum(cartanA 2,GlobalForm=>"Adjoint");
    lambda = kmWeight(Dad,{1})
    integratesToGlobalForm lambda
    borelWeilBott(Dad,lambda)
    characterDimension borelWeilBottCharacter(Dad,lambda)
  Text
    The flag variety of $PGL_2$ is still $\mathbb P^1$, and the weight
    $(1)$ gives the line bundle $\mathcal O(1)$.  Its two-dimensional
    space of global sections is $V(1)$, so this cohomology calculation is
    valid.  The value @TT "false"@ says that the action is an $SL_2$
    action, not a $PGL_2$ action: the central element $-I$ acts by $-1$.
    Thus $\mathcal O(1)$ does not have the corresponding $PGL_2$ action,
    although it is a perfectly valid line bundle on the same variety.
  Text
    For a monomial $e^\mu$ with $m=\langle\mu,\alpha_i^\vee\rangle$,
    @TO demazureOperator@ is defined by
    $$D_i(e^\mu)=
      \begin{cases}
      \sum_{k=0}^{m}e^{\mu-k\alpha_i},&m\geq0,\\
      0,&m=-1,\\
      -\sum_{k=1}^{-m-1}e^{\mu+k\alpha_i},&m\leq-2.
      \end{cases}$$
    It extends linearly to a @TO FormalCharacter@ and is valid for an
    arbitrary symmetrizable Kac--Moody root datum; complete realization
    coordinates retain affine grade directions.
    @TO demazureCharacter@ applies these operators along a reduced Weyl word;
    the Demazure operators satisfy the braid relations, so the result is
    independent of the chosen reduced expression.
    The expressions @TT "demazureOperator(i,C0)"@ and
    @TT "demazureOperator(C0,i)"@ are equivalent.  The index is a node
    position, numbered from $1$, not a user-supplied diagram label.
    @TO demazureCharacter@ also accepts a Dynkin-label list for the
    highest weight, or @TT "demazureCharacter(A,labels,word)"@ with a
    Cartan matrix.  The empty word returns the single highest-weight
    monomial.  A finite Weyl word gives an exact character of finite
    support even when the ambient root datum is affine or indefinite.
    @TO demazureOperator@ requires an exact character of finite support.
    It rejects a partial expansion: omitted weights can contribute at
    weights already displayed, so applying the operator only to the
    known terms need not give correct coefficients.  This does not
    restrict @TO demazureCharacter@ to finite type.  Starting with its
    highest-weight monomial and applying finitely many operators gives
    an exact finite-support character in any symmetrizable type.
  Example
    D = rootDatum "A2";
    C = demazureCharacter(D,{1,0},{1});
    C
  Text
    This is the Demazure character
    $e^{\omega_1}+e^{-\omega_1+\omega_2}$: its two weight spaces have
    weights $\omega_1$ and $-\omega_1+\omega_2$, each with dimension $1$.
  Example
    fullDemazure = demazureCharacter(D,{1,0},longElement cartanA 3)
    fullCharacter = finiteCharacter(D,{1,0})
    characterTerms fullDemazure == characterTerms fullCharacter
  Text
    The value @TT "true"@ verifies that the longest-element Demazure module
    is the full irreducible module $V(\omega_1)$.
SeeAlso
  dotWeylAction
  finiteCharacter
  weylElement
  "Kazhdan-Lusztig theory and the BGG resolution"
///

doc ///
Key
  "Kazhdan-Lusztig theory and the BGG resolution"
  (symbol ==,BGGResolution,BGGResolution)
Headline
  Kazhdan-Lusztig polynomials, Verma multiplicities, and BGG resolutions
Usage
  P = kazhdanLusztigPolynomial(u,v)
  P = kazhdanLusztigPolynomial(u,v,Variable=>t)
  a = kazhdanLusztigCoefficients(u,v)
  q = kazhdanLusztigVariable()
  mu = dotWeylAction(u,lambda)
  M = simpleHighestWeightModule(D,mu)
  m = vermaCompositionMultiplicity(D,lambda,u,v)
  T = vermaCompositionFactors(D,lambda,u)
  B = bggResolution(D,lambda)
  B = bggResolution(D,lambda,MaxDegree=>k)
  C = bggEulerCharacter(B,Depth=>H)
Inputs
  u:WeylElement
    the lower Weyl element, in the same group as @TT "v"@
  v:WeylElement
    the upper Weyl element, in the same group as @TT "u"@
  D:RootDatum
    a symmetrizable finite, affine, or indefinite root datum
  lambda:KMWeight
    a dominant integral highest weight defining a regular integral block
  mu:KMWeight
    the highest weight of a simple module in @TT "D"@
  k:ZZ
    a nonnegative homological-degree bound
  H:ZZ
    a nonnegative root-depth bound
  t:RingElement
    a variable in a polynomial ring over @TO ZZ@ or @TO QQ@
Outputs
  P:RingElement
    the Kazhdan--Lusztig polynomial in the chosen variable, or in @TO KazhdanLusztigPolynomialRing@ by default
  m:ZZ
    the Verma composition multiplicity $P_{u,v}(1)$
  B:BGGResolution
    all terms in finite type, or all terms through the requested homological degree
  C:FormalCharacter
    its truncated Euler characteristic
Description
  Text
    @TO kazhdanLusztigPolynomial@ computes $P_{u,v}(q)$ recursively for an
    arbitrary symmetrizable Kac--Moody Weyl group.  A reduced word for $v$
    determines the finite Bruhat interval $[e,v]$ by the subword property.
    Only elements in this interval are needed, even when the Weyl group is
    infinite.  @TO kazhdanLusztigCoefficients@ returns the coefficient list in
    ascending powers, and @TO kazhdanLusztigVariable@ returns the generator
    $q$ of @TO KazhdanLusztigPolynomialRing@.  Inputs must have one
    @TO KMWeylGroup@; noncomparable elements give zero.
    Alternatively, @TT "kazhdanLusztigPolynomial(A,uWord,vWord)"@
    and @TT "kazhdanLusztigCoefficients(A,uWord,vWord)"@ take a Cartan
    matrix and two lists of simple-reflection positions.  As with
    @TO weylElement@, the rightmost reflection acts first.
  Example
    A = cartanA 4;
    W = weylGroup A;
    e = identityWeylElement W
    v = weylElement(W,{2,1,3,2})
    kazhdanLusztigPolynomial(e,v)
  Text
    The output is the polynomial $q+1$, meaning $P_{e,v}(q)=1+q$.
    This is the first singular Schubert pattern in type $A_3$; the nonconstant
    term records nontrivial local intersection cohomology.
    To obtain its coefficient list instead, use
    @TT "kazhdanLusztigCoefficients(e,v)"@; this gives @TT "{1,1}"@
    in ascending powers of $q$.
  Text
    The output is an ordinary Macaulay2 polynomial.  To choose its
    variable and ring, use @TT "Variable=>t"@ with a variable in a
    polynomial ring over @TO ZZ@ or @TO QQ@.  The ring may have other
    variables as well.  Without this option, the result belongs to
    @TO KazhdanLusztigPolynomialRing@, which is the ordinary polynomial
    ring $\mathbb Z[q]$; its name does not specify a new kind of
    polynomial.  @TO kazhdanLusztigVariable@ returns this default $q$.
  Example
    R = QQ[t];
    Pt = kazhdanLusztigPolynomial(e,v,Variable=>t)
    (ring Pt === R,coefficient(t,Pt),sub(Pt,{t=>1}))
    factor(Pt^2-1)
  Text
    Now the answer is $t+1$ in the chosen ring $R$.
    The triple @TT "(true, 1, 2)"@ identifies the ring, gives the
    coefficient of $t$, and evaluates the polynomial at $t=1$.
    The final factorization is $(t+1)^2-1=t(t+2)$.
    These are the usual Macaulay2 polynomial operations.
    The same option applies to
    @TT "kazhdanLusztigPolynomial(A,uWord,vWord,Variable=>t)"@.
    It must be an actual ring variable, not a constant, a polynomial
    such as @TT "t+1"@, or a variable in a quotient ring.
  Text
    @TO dotWeylAction@ computes
    $w\mathbin{\cdot}\lambda=w(\lambda+\rho)-\rho$ and preserves complete
    realization directions, including Weyl-fixed coordinates in a
    nonminimal finite realization.  It is defined for any @TO KMWeight@,
    not just a dominant weight.  For dominant integral $\lambda$, the
    regular block multiplicity in arbitrary symmetrizable type is
    $$[M(u\mathbin{\cdot}\lambda):L(v\mathbin{\cdot}\lambda)]
      =P_{u,v}(1).$$
    @TO simpleHighestWeightModule@ specifies the simple module by its datum
    and highest weight.  @TO vermaCompositionMultiplicity@ computes the
    displayed multiplicity for given $u$ and $v$ in finite or infinite
    Weyl type.  @TO vermaCompositionFactors@ lists all composition factors
    in finite type.  In an infinite regular block, the full list is
    infinite, so this last command requires finite type.
    Its @TO Tally@ has simple @TO KMRepresentation@ objects as keys
    and composition multiplicities as values.  The weight in
    @TT "simpleHighestWeightModule(D,mu)"@ may also be given as a list
    of Dynkin labels; no dominance condition is imposed by this constructor.
    Specifying a simple module does not compute its character:
    @TO moduleCharacter@ currently accepts Verma and integrable
    highest-weight modules instead.
  Text
    @TO bggResolution@ records the term in homological degree $k$ as
    $$\bigoplus_{\ell(w)=k}M(w\mathbin{\cdot}\lambda).$$
    It records terms and multiplicities, not differential matrices.  In
    finite Weyl type, the default returns the full exact resolution.  In
    affine or indefinite type, @TT "MaxDegree=>k"@ is required and returns
    every summand through homological degree $k$; the result records
    @TT "exact=>false"@ and the largest homological degree computed.
    The highest weight may also be supplied as a Dynkin-label list.
    In finite type, a smaller @TT "MaxDegree"@ likewise asks for only
    the terms through that degree.  The entries @TT "terms"@,
    @TT "computedThroughDegree"@, and @TT "exact"@ give the degreewise
    Verma tallies, the last computed degree, and whether all terms of the
    resolution are present.
    Equality of these results includes the datum, highest weight, terms,
    and bounds; identical displayed terms do not identify a full
    resolution with a partial one.
  Example
    D = rootDatum "A2";
    B = bggResolution(D,{1,0});
    B
    (B#"length",apply(sort keys B#"terms",
        k -> {k,#keys(B#"terms"#k)}))
  Text
    Each degree is written as a direct sum of Verma modules.  In this
    finite-type example, @TT "M(a,b)"@ denotes the Verma module of
    highest weight $a\omega_1+b\omega_2$.
    The BGG resolution of $L(\omega_1)$ has length $3$ and respectively
    $1,2,2,1$ Verma summands in degrees $0,1,2,3$, matching the Weyl-group
    length distribution of $S_3$.
  Example
    Da = rootDatum cartanAffineA 1;
    lambdaA = affineWeightFromDynkinLabels(Da,{1,0},0)
    Ba = bggResolution(Da,lambdaA,MaxDegree=>3);
    Ba
    (Ba#"exact",Ba#"computedThroughDegree",
        apply(sort keys Ba#"terms",k -> #keys(Ba#"terms"#k)))
  Text
    The output @TT "(false, 3, {1,2,2,2})"@ means that the affine
    $A_1^{(1)}$ BGG resolution is complete in degrees $0,1,2,3$, with one
    Verma summand in degree $0$ and two in each positive degree shown.  It
    does not assert that the infinite resolution ends in degree $3$.
    The chosen highest weight has derivation eigenvalue $0$.  Every tuple in
    @TT "M(a,b,q)"@ gives full realization coordinates: $(a,b)$ are
    Dynkin labels and $q$ is the eigenvalue of the standard derivation.
    Thus modules in different affine grades remain distinct.
  Text
    @TO bggEulerCharacter@ expands the alternating sum of Verma characters
    through an absolute root depth.  Its result is marked truncated even
    when cancellation leaves finite support: all coefficients are known
    through the requested depth, but no assertion is made about larger
    depths.  If the @TO BGGResolution@ is
    homologically truncated through degree $k$, only @TT "Depth=>H"@ with
    $H\leq k$ is accepted; omitted BGG terms can first affect larger root
    depths.
  Text
    For Kazhdan--Lusztig polynomials, @TO MaxElements@ bounds the number
    of elements in the principal Bruhat interval $[e,v]$, including when
    the answer is obtained from commuting factors.  For a BGG resolution,
    it bounds the Weyl elements in the requested homological degrees.
    If this limit is
    exceeded, the command reports an error; it does not return an
    incomplete answer for the requested range.
SeeAlso
  bruhatLessOrEqual
  weylGroupElements
  vermaModule
  vermaCharacter
  "Borel-Weil-Bott and Demazure operators"
///

doc ///
Key
  "Lambda operations and representation indices"
Headline
  Form symmetric and exterior powers and compute bilinear-form invariants
Usage
  A = adamsOperation(C,k)
  S = symmetricPowerCharacter(C,k)
  E = exteriorPowerCharacter(C,k)
  T = symmetricPowerDecomposition(C,k)
  T = exteriorPowerDecomposition(C,k)
  e = frobeniusSchurIndicator(D,lambda)
  j = representationDynkinIndex(D,lambda)
Inputs
  C:FormalCharacter
    an exact character; symmetric and exterior powers require nonnegative weight multiplicities
  k:ZZ
    a nonnegative power, required to be positive for @TT "adamsOperation"@
  D:RootDatum
    a connected finite root datum for @TT "representationDynkinIndex"@
  lambda:KMWeight
    a dominant integral highest weight
Outputs
  A:FormalCharacter
    the character with each exponent multiplied by @TT "k"@
  S:FormalCharacter
    the symmetric-power character
  E:FormalCharacter
    the exterior-power character
  T:Tally
    highest weights and their irreducible multiplicities
  e:ZZ
    the Frobenius--Schur indicator, one of $0$, $1$, and $-1$
  j:QQ
    the Dynkin index in the stated invariant-form normalization
Description
  Text
    @TO adamsOperation@ sends $e^\mu$ to $e^{k\mu}$.
    Both @TT "adamsOperation(C,k)"@ and @TT "adamsOperation(k,C)"@
    are accepted, with $k>0$.  An exact character is required, but its
    coefficients may be negative.  The Adams operation is not in general
    the character of a genuine representation; it preserves the
    coefficients and changes only the weights.
    @TO symmetricPowerCharacter@ and @TO exteriorPowerCharacter@ use the
    weight-space generating products
    $$\prod_\mu(1-t e^\mu)^{-m_\mu},\qquad
      \prod_\mu(1+t e^\mu)^{m_\mu}$$
    and extract the coefficient of $t^k$.  Exactness and nonnegative
    multiplicities are required because a truncated or virtual character
    is not accepted by these commands.
    @TO symmetricPowerDecomposition@ and @TO exteriorPowerDecomposition@
    give the irreducible decompositions of the resulting characters.
    The two character commands also accept the order @TT "(k,C)"@;
    the decomposition commands use @TT "(C,k)"@ and require finite
    type.  The zeroth symmetric and exterior powers are the trivial
    character, and an exterior power above the dimension is zero.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    (apply(pairs symmetricPowerDecomposition(C,2),
        p -> {datumCoordinates p#0,p#1}),
     apply(pairs exteriorPowerDecomposition(C,2),
        p -> {datumCoordinates p#0,p#1}))
  Text
    This translates to
    $\operatorname{Sym}^2 V(\omega_1)\cong V(2\omega_1)$ and
    $\bigwedge^2V(\omega_1)\cong V(\omega_2)$, each irreducible with
    multiplicity $1$.
  Text
    @TO frobeniusSchurIndicator@ returns $0$ for a non-self-dual irreducible,
    $1$ when its invariant bilinear form is symmetric, and $-1$ when it is
    alternating.  For a self-dual highest weight, Tits' formula gives
    $$\operatorname{FS}(V(\lambda))
      =(-1)^{\langle\lambda,2\rho^\vee\rangle},$$
    where $2\rho^\vee$ is the sum of the positive coroots.  Thus the answer
    is determined by self-duality and this integer pairing; it does not
    require constructing either of the second powers.  For a
    complete weight in a nonminimal realization, self-duality includes its
    central character: a nonzero central character that changes sign under
    duality correctly forces the indicator to be $0$.
    Dynkin-label classes describe the representation of the semisimple
    part only.  A complete highest weight instead determines the action
    of the center as well, so these two kinds of input can have different
    self-duality behavior.
    @TT "frobeniusSchurIndicator(D,labels)"@ and
    @TT "frobeniusSchurIndicator(A,labels)"@ accept a Dynkin-label list
    with a finite root datum or Cartan matrix.  For
    @TO representationDynkinIndex@, use a complete or label-class
    @TO KMWeight@ with a @TO RootDatum@, or the pair
    @TT "(A,labels)"@ with a Cartan matrix.
  Example
    (frobeniusSchurIndicator(D,{1,0}),
     frobeniusSchurIndicator(rootDatum "A1",{1}))
  Text
    The $A_2$ standard representation is not self-dual, so its indicator is
    $0$.  The two-dimensional $A_1$ standard representation has an invariant
    alternating form, so its indicator is $-1$.
    The formula is proved as Theorem 3.6 in
    @HREF {"https://arxiv.org/abs/0704.0165",
      "Abu-Hamed and Gelaki, Frobenius-Schur indicators for semisimple Lie algebras"}@.
  Text
    @TO representationDynkinIndex@ uses the long-root-squared-length-$2$
    normalization and returns
    $$\operatorname{ind}(V_\lambda)=
      \frac{\dim V_\lambda}{\dim\mathfrak g}
      (\lambda,\lambda+2\rho).$$
  Example
    representationDynkinIndex(D,kmWeight(D,{1,0}))
  Text
    The answer $1$ is the Dynkin index of the defining representation of
    $\mathfrak{sl}_3$ in this normalization.
  Text
    Connectedness is required: for a reducible semisimple algebra the
    invariant form and Dynkin index have one scalar on each simple factor,
    so a single number would discard essential data.
SeeAlso
  quadraticCasimir
  finiteCharacter
  dualCharacter
  decomposeCharacter
///

doc ///
Key
  "Finite highest-weight crystals"
  (symbol ==,Crystal,Crystal)
  (symbol ==,CrystalVertex,CrystalVertex)
Headline
  Construct finite crystals and recover their exact highest-weight characters
Usage
  B = highestWeightCrystal(D,lambda)
  B = minusculeCrystal(D,lambda)
  B = minusculeCrystal(D,i)
  T = tensorProductCrystal(B,C)
  D = crystalDatum B
  lambda = crystalHighestWeight B
  V = crystalVertices B
  b = crystalVertex(B,data)
  B = crystalParent b
  data = crystalVertexData b
  b0 = crystalE(b,i)
  b1 = crystalF(b,i)
  m = crystalEpsilon(b,i)
  n = crystalPhi(b,i)
  mu = crystalWeight b
  T = crystalTableau b
  E = crystalEdges B
  contained = crystalContains(B,b)
  C = crystalCharacter B
Inputs
  D:RootDatum
    a finite root datum; @TT "minusculeCrystal"@ requires connected type
  lambda:KMWeight
    a dominant integral highest weight, also accepted as a list or row or column matrix of Dynkin labels
  B:Crystal
    a finite crystal
  C:Crystal
    a second finite crystal with the same root datum, for the tensor product
  b:CrystalVertex
    a vertex of @TT "B"@
  i:Thing
    a 1-based node position or a user-supplied node label
  data:List
    tableau rows, path points, a minuscule weight tuple, or a pair of such lists, according to the crystal model
Outputs
  B:Crystal
    a finite crystal with the specified root datum
  T:Crystal
    the tensor product of the two crystals, with one vertex for every pair of factor vertices
  V:List
    all vertices of @TT "B"@
  b1:Thing
    the next vertex on the $i$-string, or @TT "null"@ at its lower end
  C:FormalCharacter
    the exact weight-generating character of the crystal
Description
  Text
    A crystal retains the weight graph left by an integrable representation
    at the crystal limit.  Its colored arrows record the Kashiwara operators
    $e_i$ and $f_i$, while each vertex carries its weight in the specified
    datum.  These weights can be compared with @TO finiteCharacter@.
    The examples below show how to examine individual tableaux and paths,
    their weights, weight multiplicities, and simple-root strings, and
    how to recover irreducible summands from a tensor product.
  Text
    @TO highestWeightCrystal@ constructs the full crystal for any finite
    root datum and dominant integral highest weight.  In connected type
    $A$ it uses the semistandard Young-tableau model.  If the Dynkin labels are
    $(a_1,\ldots,a_r)$, the tableau shape is the partition
    $(a_1+\cdots+a_r,a_2+\cdots+a_r,\ldots,a_r)$, with entries in
    $\{1,\ldots,r+1\}$.  Rows are weakly increasing and columns strictly
    increasing.  The number of tableaux is checked against the Weyl
    dimension.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,1});
    b = crystalVertex(B,{{1,1},{2}});
    b1 = crystalF(b,1);
    B
    (b,b1)
    (B#"model",B#"shape",#crystalVertices B,
        crystalTableau b1,datumCoordinates crystalWeight b1,
        crystalE(b1,1) === b)
  Text
    The output @TT "B(1, 1)"@ denotes the eight-vertex crystal
    $B(\omega_1+\omega_2)$ of the adjoint
    representation of $\mathfrak{sl}_3$, modeled by tableaux of shape
    $(2,1)$.  The highest tableau is
    $\begin{smallmatrix}1&1\\2\end{smallmatrix}$; applying $f_1$ gives
    $\begin{smallmatrix}1&2\\2\end{smallmatrix}$ of weight
    $-\omega_1+2\omega_2$, whose Dynkin labels are $(-1,2)$.
    The final value @TT "true"@ verifies that $e_1$
    returns to the original vertex.
  Text
    The tableau signature uses the Far-Eastern reading word: columns from
    right to left and entries within a column from top to bottom.  For color
    $i$, write $+$ under $i$ and $-$ under $i+1$, cancel $+-$ pairs, let
    $f_i$ change the first surviving $+$, and let $e_i$ change the last
    surviving $-$.  @TO crystalTableau@ gives the tableau of a vertex.
  Text
    In other finite types, including disconnected root systems,
    @TO highestWeightCrystal@ uses Littelmann's path model.  It begins
    with the straight path from $0$ to the highest weight and closes this
    path under the lowering operators.  The weight of a vertex is the
    endpoint of its path.  The operators reflect the appropriate piece
    of the path across a simple-root hyperplane and translate the
    remaining piece, so that $f_i$ changes the endpoint by $-\alpha_i$.
    All path points and their parameters are rational numbers, with no
    numerical approximation.  These are the root operators of
    @HREF {"https://annals.math.princeton.edu/1995/142-3/p03",
      "Littelmann, Paths and root operators in representation theory"}@.
  Example
    DB2 = rootDatum "B2";
    BB2 = highestWeightCrystal(DB2,{1,1},MaxElements=>100)
    (BB2#"model",#crystalVertices BB2,
        characterTerms crystalCharacter BB2 ==
            characterTerms finiteCharacter(DB2,{1,1}))
    p = first crystalVertices BB2;
    p
    crystalVertexData p
    (datumCoordinates crystalWeight p,
        datumCoordinates crystalWeight crystalF(p,1))
  Text
    This is the $16$-vertex crystal of the $B_2$ representation $V(1,1)$.
    Its complete character agrees with @TT "finiteCharacter(DB2,{1,1})"@.
    The two entries of the path list give the points $0$ and $(1,1)$ at
    parameters $0$ and $1$, respectively: this is the straight highest
    path.  Applying $f_1$ changes its weight from $(1,1)$ to $(-1,3)$.
    The subtraction is determined by the first column of
    @TT "datumCartanMatrix DB2"@, so no choice of root coordinates is
    hidden in this step.
  Text
    @TO minusculeCrystal@ works for every connected finite type.  It constructs
    the Weyl orbit of a minuscule highest weight: one for which the weights
    of the irreducible representation form a single Weyl orbit, each with
    multiplicity $1$.  Since every $i$-string
    has length at most one,
    $f_i(\mu)=\mu-\alpha_i$ exactly when
    $\langle\mu,\alpha_i^\vee\rangle=1$.  A fundamental minuscule weight may
    be selected directly by its node number.
  Example
    BD4 = minusculeCrystal(rootDatum "D4",1)
    (BD4#"model",#crystalVertices BD4,#crystalEdges BD4)
  Text
    The output describes the eight-vertex minuscule orbit crystal for the
    selected $D_4$ fundamental weight.  Its eight colored edges are exactly
    the nonzero lowering operations among those orbit weights.
  Text
    @TO crystalE@ and @TO crystalF@ are partial inverses.  The commands
    @TO crystalEpsilon@ and @TO crystalPhi@ return the lengths to the two ends
    of the $i$-string and satisfy, for every vertex $b$,
    $$\varphi_i(b)-\varepsilon_i(b)
      =\langle\operatorname{wt}(b),\alpha_i^\vee\rangle.$$
    Moreover
    $\operatorname{wt}(f_i b)=\operatorname{wt}(b)-\alpha_i$ whenever
    $f_i b$ is defined.  @TO crystalEdges@ returns every nonzero $f_i$ as a
    table giving its starting vertex, ending vertex, node position, and
    chosen node label.
  Example
    (crystalEpsilon(b1,1),crystalPhi(b1,1),
     (datumCoordinates crystalWeight b1)#0,
     #crystalEdges B)
  Text
    The first three values satisfy
    $\varphi_1-\varepsilon_1=\langle\operatorname{wt}(b_1),
    \alpha_1^\vee\rangle=-1$.  The final value counts the eight colored
    arrows in the complete adjoint crystal.
  Text
    @TO tensorProductCrystal@ constructs $B\otimes C$.  Its vertices
    are pairs $b\otimes c$, with weight
    $\operatorname{wt}(b)+\operatorname{wt}(c)$.  The convention is
    that $f_i$ acts on the left factor when
    $\varphi_i(b)>\varepsilon_i(c)$, and on the right factor otherwise;
    $e_i$ acts on the left when
    $\varphi_i(b)\geq\varepsilon_i(c)$, and on the right otherwise.
    The strict inequality for $f_i$ and the weak inequality for $e_i$
    make them partial inverses.  The factors must have the same root
    datum, and their weights must both be complete weights or both
    Dynkin-label classes when the two notions differ.
  Example
    DT = rootDatum "A2";
    BT = highestWeightCrystal(DT,{1,0})
    CT = highestWeightCrystal(DT,{0,1})
    PT = tensorProductCrystal(BT,CT);
    PT
    (#crystalVertices PT,characterDimension crystalCharacter PT)
    tops = select(crystalVertices PT,
        b -> all({1,2},i -> crystalE(b,i) === null));
    sort apply(tops,b -> datumCoordinates crystalWeight b)
  Text
    The two factors are the $3$-dimensional standard representation of
    $\mathfrak{sl}_3$ and its dual.  Their tensor crystal has $9$ vertices.
    A vertex killed by every $e_i$ is the highest vertex of an irreducible
    component.  Here the two highest weights are $(0,0)$ and $(1,1)$,
    giving
    $$V(1,0)\otimes V(0,1)\cong V(0,0)\oplus V(1,1).$$
    This is the familiar decomposition of all endomorphisms into scalars
    and trace-zero endomorphisms, of dimensions $1$ and $8$.
  Example
    (tally apply(tops,crystalWeight) ==
        decomposeCharacter crystalCharacter PT,
     characterTerms crystalCharacter PT ==
        characterTerms(crystalCharacter BT*crystalCharacter CT))
  Text
    The two values @TT "true"@ compare the highest vertices with the
    irreducible-character decomposition, and compare the entire tensor
    crystal character with the product of the factor characters.
    More generally, tallying the weights of all highest vertices gives
    every irreducible highest weight with its multiplicity.  A tensor
    crystal is usually disconnected: @TT "crystalHighestWeight PT"@
    returns the sum of the highest weights of its factors, not the list
    of highest weights of all its components.
  Text
    @TO crystalDatum@ and @TO crystalHighestWeight@ give the root datum and
    highest weight, or the sum of the factor highest weights in a tensor
    product.  @TO crystalParent@ gives the crystal containing a
    vertex, @TO crystalVertexData@ describes that vertex, and
    @TO crystalVertex@ finds a vertex from its model-specific data.
    @TO crystalVertices@ lists the vertices and @TO crystalContains@ tests
    membership.  The Kashiwara operators require vertices of the specified
    crystal.
    Equality of vertices includes their crystal as well as their tableau
    or other vertex data.  Equality of crystals compares the chosen model and
    its complete data; it does not test whether two different crystal
    models are isomorphic.
    In a tableau crystal, @TT "data"@ is the list of tableau rows,
    as in @TT "{{1,1},{2}}"@.  In a minuscule orbit crystal, it is a
    Dynkin-label list such as @TT "{1,0,0,0}"@, not a @TO KMWeight@
    object.  @TO crystalVertexData@ returns precisely this form of the
    vertex; @TO crystalTableau@ is defined only for the tableau model.
    In a path crystal, @TT "data"@ is a list of pairs
    @TT "{parameter,point}"@, with each point given by its Dynkin-label
    coordinates; the list starts at @TT "{0,{0,...,0}}"@ and ends at
    parameter $1$.  A tensor-product vertex has data
    @TT "{leftData,rightData}"@, using the data of its two factor vertices.
    For example, @TT "crystalVertex(PT,{{{1}},{{1},{2}}})"@ gives the
    tensor of the standard highest tableau and the dual highest tableau
    in the example above.
    @TT "crystalWeight(B,b)"@, @TT "crystalE(B,b,i)"@, and
    @TT "crystalF(B,b,i)"@ also allow the crystal to be stated
    explicitly and check that the vertex belongs to it.  A raising
    operator at the upper end of an $i$-string returns @TT "null"@,
    just as a lowering operator does at the lower end.
    Both highest-weight crystal constructors also accept a @TO CartanDatum@ or Cartan
    matrix as their first argument when the highest weight is a list
    or a row or column matrix.
  Text
    Node positions $1,\ldots,r$ take precedence over labels.  Any other
    argument is looked up among the @TT "NodeLabels"@ of the Cartan datum;
    this includes integer labels outside the positional range, such as
    @TT "10"@ or @TT "20"@.  Thus diagrams with names meaningful to the
    user can be used directly.  Node positions are still numbered starting
    at $1$.
  Example
    Dnamed = rootDatum cartanDatum(cartanA 3,NodeLabels=>{10,20});
    Bnamed = highestWeightCrystal(Dnamed,matrix{{1,0}});
    bnamed = crystalVertex(Bnamed,{{1}})
    crystalF(bnamed,10) === crystalF(bnamed,1)
  Text
    The value @TT "true"@ says that the diagram label @TT "10"@ names the
    first simple root.  The row matrix is read as the Dynkin-label vector
    $(1,0)$; a column matrix is accepted in the same way.
  Example
    C = crystalCharacter B
    (isExactCharacter C,characterDimension C,
        characterCoefficient(C,kmWeight(D,{0,0})),
        characterTerms C == characterTerms finiteCharacter(D,{1,1}))
  Text
    The result means that
    $\operatorname{ch}B(\omega_1+\omega_2)$ is an exact character of
    dimension $8$, the zero weight occurs at two vertices, and the full
    weight tally agrees with the independently computed irreducible
    character.  Thus the crystal graph and the Freudenthal character encode
    the same $A_2$ representation by distinct constructions.
  Text
    @TT "MaxElements"@ limits the number of vertices.  When possible, the
    Weyl dimension is checked against this limit before constructing a
    highest-weight crystal.  For a tensor product, the number of pairs of
    factor vertices is checked before they are formed.  A successful
    calculation gives the entire finite crystal; exceeding the limit
    gives an error.  The space required by an individual vertex also
    depends on the number of boxes in a tableau, path segments, or tensor
    factors.
  Text
    Complete highest weights in nonminimal finite realizations keep their
    Weyl-fixed central coordinates at every crystal vertex.  List, row-matrix,
    and column-matrix inputs are Dynkin-label classes and produce label-class
    weights.  To specify the additional central coordinates, supply a
    complete highest weight.
SeeAlso
  finiteCharacter
  minusculeNodes
  weightMultiplicity
  "Formal and completed characters"
  MaxElements
///

doc ///
Key
  "Saved results and repeated calculations"
Headline
  Inspect and clear results saved during a Macaulay2 session
Usage
  H = infrastructureCacheInfo()
  clearInfrastructureCaches()
  Haff = affineStructureCacheInfo()
  clearAffineStructureCache()
Outputs
  H:HashTable
    numbers of saved results for affine structure, Cartan matrices, Kazhdan--Lusztig polynomials,
    weight bases, Verma partition tables, and crystals
  Haff:HashTable
    counts for validated affine matrices, marks, comarks, and type data
Description
  Text
    During a Macaulay2 session, the package saves results for affine
    invariants, Cartan matrices, weight bases, Kazhdan--Lusztig polynomials, Verma
    partitions, and crystal calculations.  Repeated calculations with the
    same matrix or datum can use these results.
    @TO infrastructureCacheInfo@ counts the saved results without changing
    them.  This is useful when many calculations have been performed in one
    session.
  Example
    H = infrastructureCacheInfo();
    sort keys H
  Text
    The entries distinguish saved affine results, Cartan matrices,
    weight bases used in printed expressions, Kazhdan--Lusztig polynomials,
    @TT "vermaPartitions"@, and crystal
    calculations.  The entry @TT "characterParents"@ counts the remembered
    root-datum comparisons for character arithmetic.  The affine entry is the table returned by
    @TO affineStructureCacheInfo@, with separate counts for Cartan matrices,
    marks, comarks, and affine types.  @TO rootSnapshotCacheInfo@ and
    @TO representationCacheInfo@ count saved root-multiplicity and finite
    representation results.
  Text
    @TO clearInfrastructureCaches@ clears the saved results.
    Existing @TO CartanDatum@, @TO RootDatum@, @TO FormalCharacter@, and
    @TO Crystal@ objects remain valid; later calculations compute any needed
    results again.  @TO clearAffineStructureCache@ clears only the affine
    results.
SeeAlso
  affineTypeData
  affineMarks
  affineComarks
  rootSnapshotCacheInfo
  clearRootSnapshotCache
  representationCacheInfo
  clearRepresentationCache
///


-------------------------------------------------------------------------
-- Documentation Exclusions
-------------------------------------------------------------------------

-- These are presentation methods and ordinary operations inherited by ZZ[q],
-- not additional mathematical commands supplied by this package.  The public
-- types and the polynomials themselves remain documented and unchanged.
-- Mathematical arithmetic on roots, weights, characters, and Weyl elements
-- is documented with those objects, not excluded here.
undocumented {
    "KazhdanLusztigPolynomialRing == KazhdanLusztigPolynomialRing",
    (expression,KazhdanLusztigPolynomialRing),
    (lift,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (lift,KazhdanLusztigPolynomialRing,ZZ),
    (lift,List,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (lift,List,KazhdanLusztigPolynomialRing,ZZ),
    (lift,Matrix,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (lift,Matrix,KazhdanLusztigPolynomialRing,ZZ),
    (lift,Module,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (lift,Module,KazhdanLusztigPolynomialRing,ZZ),
    (lift,MutableMatrix,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (lift,MutableMatrix,KazhdanLusztigPolynomialRing,ZZ),
    (net,BGGResolution),
    (net,BorelWeilBottResult),
    (net,CartanDatum),
    (net,Crystal),
    (net,CrystalVertex),
    (net,FormalCharacter),
    (net,KMCoroot),
    (net,KMCoweight),
    (net,KMRepresentation),
    (net,KMRoot),
    (net,KMWeight),
    (net,KMWeylGroup),
    (net,LeviData),
    (net,LeviFormalCharacter),
    (net,LeviHighestWeight),
    (net,LeviIrrep),
    (net,LeviWeight),
    (net,Realization),
    (net,RootDatum),
    (net,RootSnapshot),
    (net,StandardParabolic),
    (net,WeylElement),
    (promote,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (promote,List,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (promote,List,ZZ,KazhdanLusztigPolynomialRing),
    (promote,Matrix,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (promote,Matrix,ZZ,KazhdanLusztigPolynomialRing),
    (promote,Module,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (promote,Module,ZZ,KazhdanLusztigPolynomialRing),
    (promote,MutableMatrix,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (promote,MutableMatrix,ZZ,KazhdanLusztigPolynomialRing),
    (promote,ZZ,KazhdanLusztigPolynomialRing),
    "KazhdanLusztigPolynomialRing * KazhdanLusztigPolynomialRing",
    "KazhdanLusztigPolynomialRing + KazhdanLusztigPolynomialRing",
    "KazhdanLusztigPolynomialRing - KazhdanLusztigPolynomialRing",
    (symbol ?,KazhdanLusztigPolynomialRing,KazhdanLusztigPolynomialRing),
    (toString,BGGResolution),
    (toString,BorelWeilBottResult),
    (toString,CartanDatum),
    (toString,Crystal),
    (toString,CrystalVertex),
    (toString,FormalCharacter),
    (toString,KMCoroot),
    (toString,KMCoweight),
    (toString,KMRepresentation),
    (toString,KMRoot),
    (toString,KMWeight),
    (toString,KMWeylGroup),
    (toString,LeviData),
    (toString,LeviFormalCharacter),
    (toString,LeviHighestWeight),
    (toString,LeviIrrep),
    (toString,LeviWeight),
    (toString,Realization),
    (toString,RootDatum),
    (toString,RootSnapshot),
    (toString,StandardParabolic),
    (toString,WeylElement)
}



-- Individual references for Euclidean root constructors.

doc ///
Key
  simpleRootsB
Headline
  Simple roots of type $B_\ell$ in Euclidean coordinates
Usage
  S = simpleRootsB ell
Inputs
  ell:ZZ
    an integer $ell\geq2$; the Lie-theoretic rank
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses $\alpha_i=\varepsilon_i-\varepsilon_{i+1}$ for $i<\ell$, and $\alpha_\ell=\varepsilon_\ell$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsB 3
  Text
    The first two rows are $\varepsilon_1-\varepsilon_2$ and $\varepsilon_2-\varepsilon_3$; the last row is the short root $\varepsilon_3$.
    Thus there are 3 simple roots in 3 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsB
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsB
Headline
  Positive roots of type $B_\ell$ in Euclidean coordinates
Usage
  R = positiveRootsB ell
Inputs
  ell:ZZ
    an integer $ell\geq2$; the Lie-theoretic rank
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    The positive roots are $\varepsilon_i-\varepsilon_j$, $\varepsilon_i+\varepsilon_j$ for $i<j$, and $\varepsilon_i$.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsB 3;
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(9,3)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 9 rows is one positive root of this rank-3
    system, expressed in 3 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsB@.
SeeAlso
  simpleRootsB
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsC
Headline
  Simple roots of type $C_\ell$ in Euclidean coordinates
Usage
  S = simpleRootsC ell
Inputs
  ell:ZZ
    an integer $ell\geq2$; the Lie-theoretic rank
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses $\alpha_i=\varepsilon_i-\varepsilon_{i+1}$ for $i<\ell$, and $\alpha_\ell=2\varepsilon_\ell$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsC 3
  Text
    The first two rows are $\varepsilon_1-\varepsilon_2$ and $\varepsilon_2-\varepsilon_3$; the last row is the long root $2\varepsilon_3$.
    Thus there are 3 simple roots in 3 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsC
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsC
Headline
  Positive roots of type $C_\ell$ in Euclidean coordinates
Usage
  R = positiveRootsC ell
Inputs
  ell:ZZ
    an integer $ell\geq2$; the Lie-theoretic rank
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    The positive roots are $\varepsilon_i-\varepsilon_j$, $\varepsilon_i+\varepsilon_j$ for $i<j$, and $2\varepsilon_i$.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsC 3;
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(9,3)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 9 rows is one positive root of this rank-3
    system, expressed in 3 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsC@.
SeeAlso
  simpleRootsC
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsD
Headline
  Simple roots of type $D_\ell$ in Euclidean coordinates
Usage
  S = simpleRootsD ell
Inputs
  ell:ZZ
    an integer $ell\geq4$; the Lie-theoretic rank
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses $\alpha_i=\varepsilon_i-\varepsilon_{i+1}$ for $i<\ell$, and $\alpha_\ell=\varepsilon_{\ell-1}+\varepsilon_\ell$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsD 4
  Text
    The four rows are $\varepsilon_1-\varepsilon_2$, $\varepsilon_2-\varepsilon_3$, $\varepsilon_3-\varepsilon_4$, and $\varepsilon_3+\varepsilon_4$.
    Thus there are 4 simple roots in 4 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsD
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsD
Headline
  Positive roots of type $D_\ell$ in Euclidean coordinates
Usage
  R = positiveRootsD ell
Inputs
  ell:ZZ
    an integer $ell\geq4$; the Lie-theoretic rank
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    The positive roots are $\varepsilon_i-\varepsilon_j$ and $\varepsilon_i+\varepsilon_j$ for $i<j$.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsD 4;
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(12,4)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 12 rows is one positive root of this rank-4
    system, expressed in 4 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsD@.
SeeAlso
  simpleRootsD
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsE6
Headline
  Simple roots of type $E_6$ in Euclidean coordinates
Usage
  S = simpleRootsE6()
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses the Bourbaki embedding in $\mathbb Q^8$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsE6()
  Text
    The output has six rows in eight Euclidean coordinates.  Its first row is $(1,-1,-1,-1,-1,-1,-1,1)/2$, the first simple root in this embedding.
    Thus there are 6 simple roots in 8 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsE6
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsE6
Headline
  Positive roots of type $E_6$ in Euclidean coordinates
Usage
  R = positiveRootsE6()
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    Uses the same Euclidean embedding and simple-root numbering as
    @TO simpleRootsE6@.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsE6();
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(36,8)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 36 rows is one positive root of this rank-6
    system, expressed in 8 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsE6@.
SeeAlso
  simpleRootsE6
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsE7
Headline
  Simple roots of type $E_7$ in Euclidean coordinates
Usage
  S = simpleRootsE7()
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses the Bourbaki embedding in $\mathbb Q^8$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsE7()
  Text
    The output has seven rows in eight Euclidean coordinates.  Its first row is $(1,-1,-1,-1,-1,-1,-1,1)/2$; the remaining rows complete the displayed $E_7$ simple system.
    Thus there are 7 simple roots in 8 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsE7
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsE7
Headline
  Positive roots of type $E_7$ in Euclidean coordinates
Usage
  R = positiveRootsE7()
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    Uses the same Euclidean embedding and simple-root numbering as
    @TO simpleRootsE7@.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsE7();
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(63,8)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 63 rows is one positive root of this rank-7
    system, expressed in 8 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsE7@.
SeeAlso
  simpleRootsE7
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsE8
Headline
  Simple roots of type $E_8$ in Euclidean coordinates
Usage
  S = simpleRootsE8()
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses the Bourbaki embedding in $\mathbb Q^8$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsE8()
  Text
    The eight rows form a simple-root basis for $E_8$ in eight Euclidean coordinates.  Its first root is $(1,-1,-1,-1,-1,-1,-1,1)/2$; all eight roots have squared Euclidean length $2$.
    Thus there are 8 simple roots in 8 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsE8
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsE8
Headline
  Positive roots of type $E_8$ in Euclidean coordinates
Usage
  R = positiveRootsE8()
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    Uses the same Euclidean embedding and simple-root numbering as
    @TO simpleRootsE8@.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsE8();
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(120,8)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 120 rows is one positive root of this rank-8
    system, expressed in 8 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsE8@.
SeeAlso
  simpleRootsE8
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsF4
Headline
  Simple roots of type $F_4$ in Euclidean coordinates
Usage
  S = simpleRootsF4()
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses the standard embedding in $\mathbb Q^4$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsF4()
  Text
    The rows mean $\alpha_1=\varepsilon_2-\varepsilon_3$, $\alpha_2=\varepsilon_3-\varepsilon_4$, $\alpha_3=\varepsilon_4$, and $\alpha_4=(\varepsilon_1-\varepsilon_2-\varepsilon_3-\varepsilon_4)/2$.
    Thus there are 4 simple roots in 4 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsF4
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsF4
Headline
  Positive roots of type $F_4$ in Euclidean coordinates
Usage
  R = positiveRootsF4()
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    Uses the same Euclidean embedding and simple-root numbering as
    @TO simpleRootsF4@.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsF4();
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(24,4)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 24 rows is one positive root of this rank-4
    system, expressed in 4 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsF4@.
SeeAlso
  simpleRootsF4
  cartanPositiveRoots
  fromEpsilon
///

doc ///
Key
  simpleRootsG2
Headline
  Simple roots of type $G_2$ in Euclidean coordinates
Usage
  S = simpleRootsG2()
Outputs
  S:Matrix
    one simple root per row, in the ambient $\varepsilon$-basis
Description
  Text
    Uses the embedding in the plane $x_1+x_2+x_3=0$ inside $\mathbb Q^3$.
    Row $i$ gives $\alpha_i$ in the numbering of the corresponding
    Cartan matrix; its entries are Euclidean coordinates, not Dynkin
    labels or simple-root coefficients.
  Example
    simpleRootsG2()
  Text
    The two rows mean $\alpha_1=\varepsilon_1-\varepsilon_2$ and $\alpha_2=-2\varepsilon_1+\varepsilon_2+\varepsilon_3$.  Their squared Euclidean lengths are $2$ and $6$, so the first root is short and the second is long.
    Thus there are 2 simple roots in 3 ambient coordinates.
    Use @TO fromEpsilon@ to express a vector in this simple-root basis.
SeeAlso
  positiveRootsG2
  fromEpsilon
  toEpsilon
///

doc ///
Key
  positiveRootsG2
Headline
  Positive roots of type $G_2$ in Euclidean coordinates
Usage
  R = positiveRootsG2()
Outputs
  R:Matrix
    all positive roots, one per row in the $\varepsilon$-basis
Description
  Text
    Uses the same Euclidean embedding and simple-root numbering as
    @TO simpleRootsG2@.
    The rows describe roots themselves, not their multiplicities;
    every root of this finite root system has multiplicity $1$.
  Example
    R = positiveRootsG2();
    (numrows R,numcols R)
    take(entries R,3)
  Text
    The pair $(6,3)$ gives the number of positive roots and
    the dimension of their Euclidean coordinate space.
    Each of the 6 rows is one positive root of this rank-2
    system, expressed in 3 Euclidean coordinates.  The final output
    displays three of those roots; it is not a list of Dynkin labels.
    Every such row is a nonnegative integral combination of the rows
    of @TO simpleRootsG2@.
SeeAlso
  simpleRootsG2
  cartanPositiveRoots
  fromEpsilon
///


-- Individual command, type, and option reference pages.

doc ///
Key
  cartanDatum
  (cartanDatum,CartanDatum)
  (cartanDatum,Realization)
  (cartanDatum,RootDatum)
  (cartanDatum,Matrix)
  (cartanDatum,String)
  [cartanDatum,NodeLabels]
Headline
  Record a generalized Cartan matrix and its node labels
Usage
  D = cartanDatum A
  D = cartanDatum(name,NodeLabels=>labels)
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
  name:String
    a finite Cartan--Killing type such as "A2"
  labels:List
    distinct node labels in matrix order
Outputs
  D:CartanDatum
    the Cartan matrix together with its type, rank, corank, components, and labels
Description
  Text
    The convention is $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$. A type name uses Lie-theoretic rank: "A2" has two nodes. A realization or root datum is also accepted, in which case its Cartan datum is returned. Node labels must be chosen at the original construction, not changed on an existing datum.
  Example
    D = cartanDatum("A2",NodeLabels=>{"left","right"});
    (D#"rank",D#"corank",D#"nodeLabels",datumCartanMatrix D)
  Text
    The ranks are 2 and 0, the labels are left and right, and the matrix is $\begin{pmatrix}2&-1\\-1&2\end{pmatrix}$. Thus this datum describes type $A_2$ with the indicated order of its simple roots.
SeeAlso
  rootDatum
  dynkinDiagram
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  datumCartanMatrix
  (datumCartanMatrix,CartanDatum)
  (datumCartanMatrix,Realization)
  (datumCartanMatrix,RootDatum)
Headline
  Recover the generalized Cartan matrix
Usage
  A = datumCartanMatrix D
Inputs
  D:Thing
    a Cartan datum, realization, or root datum
Outputs
  A:Matrix
    the generalized Cartan matrix
Description
  Text
    This returns the matrix of simple-root and simple-coroot pairings without changing its row and column order. It accepts a Cartan datum, realization, or root datum.
  Example
    D = rootDatum "B2";
    datumCartanMatrix D
  Text
    The displayed matrix gives $\langle\alpha_j,\alpha_i^\vee\rangle$ in entry $(i,j)$. In particular, the two unequal off-diagonal entries distinguish the long and short simple roots; the command does not replace the matrix by its transpose.
SeeAlso
  cartanDatum
  symmetrizer
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  standardRealization
  (standardRealization,CartanDatum)
  (standardRealization,Matrix)
  (standardRealization,RootDatum)
Headline
  Construct a minimal realization of the Cartan matrix
Usage
  R = standardRealization D
Inputs
  D:Thing
    a Cartan matrix or Cartan datum; a root datum returns its already chosen realization
Outputs
  R:Realization
    independent simple roots and coroots in dual rational bases
Description
  Text
    For a matrix of size $r$ and corank $k$, the constructed Cartan space has dimension $r+k$. Its simple-root rows $R_\alpha$ and simple-coroot rows $R_\vee$ satisfy $R_\vee R_\alpha^{\mathsf T}=A$. On a root datum the existing realization is returned, even if it is nonminimal.
  Example
    C = cartanDatum cartanAffineA 1;
    R = standardRealization C;
    (R#"dimension",R#"isMinimal",R#"simpleRootCoordinates",R#"simpleCorootCoordinates")
  Text
    There are two affine simple roots but the minimal Cartan space has dimension 3. The additional coordinate allows the null root to be nonzero even though all of its Dynkin labels vanish. The two row lists give the exact simple-root and coroot vectors in these dual bases.
SeeAlso
  realization
  rootDatum
  affineNullRoot
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  realization
  (realization,Matrix,Matrix,Matrix)
  (realization,RootDatum)
  (realization,CartanDatum,Matrix,Matrix)
Headline
  Specify simple roots and coroots in dual coordinate bases
Usage
  R = realization(D,B,C)
  R = realization X
Inputs
  D:Thing
    a Cartan datum or generalized Cartan matrix
  B:Matrix
    independent simple-root rows in a basis of the weight space
  C:Matrix
    independent simple-coroot rows in the dual basis
  X:RootDatum
    a root datum whose chosen realization is requested
Outputs
  R:Realization
    the specified realization
Description
  Text
    The matrices must have one row per node and the same number of columns, and must satisfy $CB^{\mathsf T}=A$ exactly over $\mathbb Q$. Extra Cartan directions are allowed. The one-argument form returns the realization of a root datum.
  Example
    R = realization(cartanDatum "A1",matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
    (R#"dimension",R#"isMinimal",datumCartanMatrix R)
  Text
    The Cartan matrix is $(2)$, but the Cartan space has dimension 2 rather than the minimal dimension 1. The second coordinate is a central direction: both the simple root and simple coroot have zero second coordinate.
SeeAlso
  standardRealization
  weightFromRealizationCoordinates
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  rootDatum
  (rootDatum,CartanDatum)
  (rootDatum,Realization)
  (rootDatum,Matrix)
  (rootDatum,String)
  [rootDatum,GlobalForm]
  [rootDatum,CharacterLattice]
Headline
  Choose a realization and character lattice
Usage
  D = rootDatum A
  D = rootDatum(R,GlobalForm=>form)
  D = rootDatum(R,CharacterLattice=>B)
Inputs
  A:Thing
    a Cartan matrix, Cartan datum, or finite type name
  R:Realization
    a specified realization
  form:String
    "SimplyConnected", "Adjoint", or "Unspecified"
  B:Matrix
    a rational row basis for a custom character lattice
Outputs
  D:RootDatum
    a realization with dual character and cocharacter lattices
Description
  Text
    In a minimal finite realization, the default simply connected form uses
    the weight lattice $P$ and coroot lattice $Q^\vee$, while the adjoint
    form uses $Q$ and $P^\vee$. In a larger Cartan space, the simple-coroot
    rows (simply connected form) or simple-root rows (adjoint form) are
    extended to a full rational basis, and the other lattice is its perfect
    dual. These supplementary directions depend on the chosen realization.
    An unspecified form makes no lattice-membership assertion. A custom
    character basis $B$ specifies the perfect dual cocharacter basis
    $(B^{-1})^{\mathsf T}$; all simple roots and coroots must belong to
    these lattices. A supplied realization is retained, while other inputs
    use the standard realization.
  Example
    Dsc = rootDatum "A1";
    Dad = rootDatum("A1",GlobalForm=>"Adjoint");
    (integratesToGlobalForm kmWeight(Dsc,{1}),integratesToGlobalForm kmWeight(Dad,{1}),integratesToGlobalForm kmWeight(Dad,{2}))
  Text
    The output $(\mathrm{true},\mathrm{false},\mathrm{true})$ says that the two-dimensional highest weight 1 integrates to $SL_2$, but not to its adjoint form $PGL_2$. Highest weight 2 does integrate to $PGL_2$. These are statements about the selected character lattices, not about whether the Lie-algebra representations exist.
SeeAlso
  characterLatticeBasis
  integratesToGlobalForm
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  pModQInvariants
  (pModQInvariants,CartanDatum)
  (pModQInvariants,Matrix)
  (pModQInvariants,Realization)
  (pModQInvariants,RootDatum)
Headline
  Describe weight classes modulo the root lattice
Usage
  T = pModQInvariants D
Inputs
  D:Thing
    a generalized Cartan matrix, Cartan datum, realization, or root datum
Outputs
  T:HashTable
    Smith invariants, torsion invariants, free rank, and finite order when defined
Description
  Text
    This computes the cokernel of $A:\mathbb Z^r\longrightarrow\mathbb Z^r$
    in integral Dynkin-label coordinates. The group is
    $\mathbb Z^{f}\oplus\bigoplus_d\mathbb Z/d\mathbb Z$, with $f$ given
    by "freeRank" and $d$ by "torsionInvariants". For the derived finite
    root system it is the usual $P/Q$. Supplementary realization directions
    invisible to Dynkin labels are not included, in any type. The result
    is independent of the chosen global form.
  Example
    T = pModQInvariants rootDatum "A3";
    (T#"torsionInvariants",T#"freeRank",T#"order")
  Text
    The output $((4),0,4)$ means $P/Q\cong\mathbb Z/4\mathbb Z$: there is one cyclic factor of order 4 and no free factor. This is not a list of weights; it describes the abelian group of their congruence classes.
SeeAlso
  characterLatticeBasis
  rootDatum
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  characterLatticeBasis
  (characterLatticeBasis,RootDatum)
Headline
  Return a basis for the chosen character lattice
Usage
  B = characterLatticeBasis D
Inputs
  D:RootDatum
    the root datum in which the element is defined
Outputs
  B:Thing
    a rational row-basis matrix, or null when the global form is unspecified
Description
  Text
    The rows are a basis of the character lattice in the weight realization coordinates. Character and cocharacter lattices are perfect duals, so their basis matrices $B$ and $C$ satisfy $BC^{\mathsf T}=I$. Additional realization directions are included.
  Example
    D = rootDatum("A1",GlobalForm=>"Adjoint");
    B = characterLatticeBasis D;
    B
    characterLatticeBasis D * transpose cocharacterLatticeBasis D
  Text
    The character-basis matrix is $(2)$: the adjoint character lattice is generated by the root, whose Dynkin label is 2. The final identity matrix verifies perfect duality with the cocharacter lattice.
SeeAlso
  cocharacterLatticeBasis
  rootDatum
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  cocharacterLatticeBasis
  (cocharacterLatticeBasis,RootDatum)
Headline
  Return a basis for the chosen cocharacter lattice
Usage
  B = cocharacterLatticeBasis D
Inputs
  D:RootDatum
    the root datum in which the element is defined
Outputs
  B:Thing
    a rational row-basis matrix, or null when the global form is unspecified
Description
  Text
    The rows are a basis of the cocharacter lattice in the dual coweight realization coordinates. Character and cocharacter lattices are perfect duals, so their basis matrices $B$ and $C$ satisfy $BC^{\mathsf T}=I$. Additional realization directions are included.
  Example
    D = rootDatum("A1",GlobalForm=>"Adjoint");
    B = cocharacterLatticeBasis D;
    B
    characterLatticeBasis D * transpose cocharacterLatticeBasis D
  Text
    The cocharacter-basis matrix is $(1/2)$ in the dual realization basis. Its pairing with the adjoint character generator $(2)$ is 1, as verified by the final identity matrix.
SeeAlso
  characterLatticeBasis
  rootDatum
  "Cartan data, realizations, and global lattices"
///

doc ///
Key
  kmRoot
  (kmRoot,CartanDatum,List)
  (kmRoot,Matrix,List)
  (kmRoot,RootDatum,List)
  (kmRoot,RootDatum,Matrix)
Headline
  Construct a root-lattice element
Usage
  alpha = kmRoot(D,a)
Inputs
  D:RootDatum
    the root datum in which the element is defined
  a:List
    simple-root coefficients in matrix order
Outputs
  alpha:KMRoot
    the element with the supplied coordinates in D
Description
  Text
    Integral coefficients define an element of the root lattice; they do not assert that the element is a root of the Lie algebra. A list supplied with a Cartan datum or Cartan matrix also constructs the corresponding root datum. With a root datum, a one-row or one-column matrix may be supplied in place of the list. Coordinates follow the Cartan-matrix order.
  Example
    D = rootDatum "A2";
    alpha = kmRoot(D,{1,1});
    (alpha,datumCoordinates alpha,parentDatum alpha == D)
  Text
    The tuple $(1,1)$ represents $\alpha_1+\alpha_2$ in type $A_2$. The coordinate list is unchanged, and the final true confirms that the element retains this root datum. Here $(1,1)$ happens to be the highest root of $A_2$.
SeeAlso
  datumCoordinates
  parentDatum
  "Roots, weights, and pairings"
///

doc ///
Key
  kmCoroot
  (kmCoroot,CartanDatum,List)
  (kmCoroot,Matrix,List)
  (kmCoroot,RootDatum,List)
  (kmCoroot,RootDatum,Matrix)
Headline
  Construct a coroot-lattice element
Usage
  h = kmCoroot(D,a)
Inputs
  D:RootDatum
    the root datum in which the element is defined
  a:List
    simple-coroot coefficients in matrix order
Outputs
  h:KMCoroot
    the element with the supplied coordinates in D
Description
  Text
    Integral coefficients define an element of the coroot lattice; they do not assert that the element is a coroot of an actual root. A list supplied with a Cartan datum or Cartan matrix also constructs the corresponding root datum. With a root datum, a one-row or one-column matrix may be supplied in place of the list. Coordinates follow the Cartan-matrix order.
  Example
    D = rootDatum "A2";
    h = kmCoroot(D,{1,0});
    (h,datumCoordinates h,parentDatum h == D)
  Text
    The tuple $(1,0)$ represents $\alpha_1^\vee$ in type $A_2$. The coordinate list is unchanged, and the final true confirms that the element retains this root datum. The coefficient 1 belongs to the first simple coroot.
SeeAlso
  datumCoordinates
  parentDatum
  "Roots, weights, and pairings"
///

doc ///
Key
  kmWeight
  (kmWeight,CartanDatum,List)
  (kmWeight,Matrix,List)
  (kmWeight,RootDatum,List)
  (kmWeight,RootDatum,Matrix)
Headline
  Construct a weight from its Dynkin labels
Usage
  lambda = kmWeight(D,a)
Inputs
  D:RootDatum
    the root datum in which the element is defined
  a:List
    rational Dynkin labels in matrix order
Outputs
  lambda:KMWeight
    the element with the supplied coordinates in D
Description
  Text
    The entries are $\langle\lambda,\alpha_i^\vee\rangle$, not simple-root coefficients. In a realization with extra Cartan directions these labels describe only a class of weights; use @TO weightFromRealizationCoordinates@ to specify a complete weight. A list supplied with a Cartan datum or Cartan matrix also constructs the corresponding root datum. With a root datum, a one-row or one-column matrix may be supplied in place of the list. Coordinates follow the Cartan-matrix order.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{1,0});
    (lambda,datumCoordinates lambda,parentDatum lambda == D)
  Text
    The tuple $(1,0)$ represents $\omega_1$ in type $A_2$. The coordinate list is unchanged, and the final true confirms that the element retains this root datum. It is the highest weight of the standard three-dimensional representation.
SeeAlso
  datumCoordinates
  parentDatum
  "Roots, weights, and pairings"
///

doc ///
Key
  kmCoweight
  (kmCoweight,CartanDatum,List)
  (kmCoweight,Matrix,List)
  (kmCoweight,RootDatum,List)
  (kmCoweight,RootDatum,Matrix)
Headline
  Construct a coweight from simple-root evaluations
Usage
  eta = kmCoweight(D,a)
Inputs
  D:RootDatum
    the root datum in which the element is defined
  a:List
    rational evaluations on the simple roots in matrix order
Outputs
  eta:KMCoweight
    the element with the supplied coordinates in D
Description
  Text
    The entries are $\langle\alpha_i,\eta\rangle$, not simple-coroot coefficients. When extra Cartan directions occur they describe only a coweight class; use @TO coweightFromRealizationCoordinates@ for a complete vector. A list supplied with a Cartan datum or Cartan matrix also constructs the corresponding root datum. With a root datum, a one-row or one-column matrix may be supplied in place of the list. Coordinates follow the Cartan-matrix order.
  Example
    D = rootDatum "A2";
    eta = kmCoweight(D,{0,1});
    (eta,datumCoordinates eta,parentDatum eta == D)
  Text
    The tuple $(0,1)$ represents $\omega_2^\vee$ in type $A_2$. The coordinate list is unchanged, and the final true confirms that the element retains this root datum. The evaluations on $\alpha_1$ and $\alpha_2$ are 0 and 1, respectively.
SeeAlso
  datumCoordinates
  parentDatum
  "Roots, weights, and pairings"
///

doc ///
Key
  weightFromRealizationCoordinates
  (weightFromRealizationCoordinates,RootDatum,List)
  (weightFromRealizationCoordinates,RootDatum,Matrix)
Headline
  Construct a complete weight in realization coordinates
Usage
  lambda = weightFromRealizationCoordinates(D,x)
Inputs
  D:RootDatum
    the root datum in which the element is defined
  x:List
    a rational vector of length equal to the Cartan-space dimension
Outputs
  lambda:KMWeight
    a complete weight with the specified realization vector
Description
  Text
    The vector is expressed in the weight realization basis, not in Dynkin labels. All directions are retained, including those invisible to the simple coroots. A one-row or one-column matrix may replace the list.
  Example
    D = rootDatum cartanAffineA 1;
    lambda = weightFromRealizationCoordinates(D,{1,0,-1});
    (datumCoordinates lambda,realizationCoordinates lambda)
  Text
    The output $((1,0),(1,0,-1))$ gives Dynkin labels $(1,0)$ and the complete realization vector $(1,0,-1)$. This is the weight $\Lambda_0-\delta$, not $\Lambda_0$: the last coordinate distinguishes them.
SeeAlso
  realizationCoordinates
  affineDegree
  "Roots, weights, and pairings"
///

doc ///
Key
  coweightFromRealizationCoordinates
  (coweightFromRealizationCoordinates,RootDatum,List)
  (coweightFromRealizationCoordinates,RootDatum,Matrix)
Headline
  Construct a complete coweight in dual realization coordinates
Usage
  eta = coweightFromRealizationCoordinates(D,x)
Inputs
  D:RootDatum
    the root datum in which the element is defined
  x:List
    a rational vector of length equal to the Cartan-space dimension
Outputs
  eta:KMCoweight
    a complete coweight with the specified realization vector
Description
  Text
    The vector is expressed in the dual coweight realization basis, not in Dynkin labels. All directions are retained, including those invisible to the simple roots. A one-row or one-column matrix may replace the list.
  Example
    D = rootDatum cartanAffineA 1;
    eta = coweightFromRealizationCoordinates(D,{0,0,1});
    (datumCoordinates eta,realizationCoordinates eta)
  Text
    The output $((1,0),(0,0,1))$ gives the simple-root evaluations and the complete dual realization vector. In this standard realization the coweight is the normalized derivation $d$, with $\alpha_0(d)=1$ and $\alpha_1(d)=0$.
SeeAlso
  realizationCoordinates
  pairing
  "Roots, weights, and pairings"
///

doc ///
Key
  parentDatum
  (parentDatum,KMCoroot)
  (parentDatum,KMCoweight)
  (parentDatum,KMRoot)
  (parentDatum,KMWeight)
Headline
  Recover the root datum of a typed element
Usage
  D = parentDatum x
Inputs
  x:Thing
    a root, coroot, weight, or coweight
Outputs
  D:RootDatum
    the root datum in which x is defined
Description
  Text
    The returned datum includes the realization and lattice choices, not only the Cartan matrix. Thus weights in different global forms or realizations need not have the same parent.
  Example
    D = rootDatum("A1",GlobalForm=>"Adjoint");
    lambda = kmWeight(D,{2})
    parentDatum lambda == D
  Text
    The true confirms that the weight with label 2 retains the adjoint root datum in which it was constructed. Its integrality is consequently interpreted in the adjoint character lattice.
SeeAlso
  rootDatum
  integratesToGlobalForm
  "Roots, weights, and pairings"
///

doc ///
Key
  datumCoordinates
  (datumCoordinates,KMCoroot)
  (datumCoordinates,KMCoweight)
  (datumCoordinates,KMRoot)
  (datumCoordinates,KMWeight)
Headline
  Read coordinates in the root datum conventions
Usage
  a = datumCoordinates x
Inputs
  x:Thing
    a root, coroot, weight, or coweight
Outputs
  a:List
    simple-root coefficients, simple-coroot coefficients, Dynkin labels, or simple-root evaluations, respectively
Description
  Text
    For roots and coroots these are the coefficients in the corresponding simple bases. For weights they are the simple-coroot pairings; for coweights they are the simple-root evaluations. Extra realization coordinates are not returned by this command.
  Example
    D = rootDatum cartanAffineA 1;
    delta = affineNullRoot D;
    (datumCoordinates delta,datumCoordinates toWeight delta,realizationCoordinates toWeight delta)
  Text
    The root coefficients are $(1,1)$, while the converted weight has Dynkin labels $(0,0)$. Its complete realization vector is $(0,0,1)$, so the null root is nonzero despite having zero Dynkin labels.
SeeAlso
  coordinateBasis
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  coordinateBasis
  (coordinateBasis,KMCoroot)
  (coordinateBasis,KMCoweight)
  (coordinateBasis,KMRoot)
  (coordinateBasis,KMWeight)
Headline
  Identify the convention used by datumCoordinates
Usage
  s = coordinateBasis x
Inputs
  x:Thing
    a root, coroot, weight, or coweight
Outputs
  s:String
    the name of its coordinate convention
Description
  Text
    The result describes the list returned by @TO datumCoordinates@. It does not assert that this list is a complete realization vector in singular type.
  Example
    D = rootDatum "A2";
    (coordinateBasis kmRoot(D,{1,0}),coordinateBasis kmWeight(D,{1,0}))
  Text
    SimpleRoots means simple-root coefficients, while DynkinLabels means evaluations on simple coroots. Thus the same numerical pair $(1,0)$ represents two different elements: $\alpha_1$ and $\omega_1$.
SeeAlso
  datumCoordinates
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  realizationCoordinates
  (realizationCoordinates,KMCoroot)
  (realizationCoordinates,KMCoweight)
  (realizationCoordinates,KMRoot)
  (realizationCoordinates,KMWeight)
Headline
  Read the complete realization vector
Usage
  x = realizationCoordinates a
Inputs
  a:Thing
    a root, coroot, complete weight, or complete coweight
Outputs
  x:List
    rational coordinates in the weight or dual coweight realization basis
Description
  Text
    Simple-root or simple-coroot coefficients are multiplied by their realization vectors. A weight or coweight must be completely specified; in a singular or nonminimal realization a bare list of Dynkin labels is insufficient.
  Example
    D = rootDatum cartanAffineA 1;
    delta = affineNullRoot D;
    (realizationCoordinates delta,realizationCoordinates toWeight delta)
  Text
    Both vectors are $(0,0,1)$. Conversion to a weight has preserved the null-root direction, rather than identifying it with zero because its Dynkin labels vanish.
SeeAlso
  weightFromRealizationCoordinates
  datumCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  weightBasis
  (weightBasis,RootDatum)
Headline
  Return an ordered basis of complete weights
Usage
  B = weightBasis D
Inputs
  D:RootDatum
    the root datum in which the element is defined
Outputs
  B:List
    complete KMWeight basis elements in the supplied datum
Description
  Text
    Fundamental weights are followed by a basis of the directions annihilating all simple coroots. In an indecomposable minimal affine realization these are the degree-zero fundamental weights followed by the null root. The full vectors are available through @TO realizationCoordinates@; the basis is not a list of simple-root or simple-coroot coefficients.
  Example
    D = rootDatum cartanAffineA 1;
    B = weightBasis D;
    apply(B,realizationCoordinates)
  Text
    The rows $(1,0,0)$, $(0,1,0)$, and $(0,0,1)$ form the basis $\Lambda_0,\Lambda_1,\delta$ in this standard affine realization. The final direction is invisible in Dynkin labels.
SeeAlso
  coweightBasis
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  coweightBasis
  (coweightBasis,RootDatum)
Headline
  Return an ordered basis of complete coweights
Usage
  B = coweightBasis D
Inputs
  D:RootDatum
    the root datum in which the element is defined
Outputs
  B:List
    complete KMCoweight basis elements in the supplied datum
Description
  Text
    Fundamental coweights are followed by a basis of the directions annihilating all simple roots. In a minimal affine realization the last direction is central. The full vectors are available through @TO realizationCoordinates@; the basis is not a list of simple-root or simple-coroot coefficients.
  Example
    D = rootDatum cartanAffineA 1;
    B = coweightBasis D;
    apply(B,realizationCoordinates)
  Text
    The vectors $(0,0,1)$ and $(0,1/2,1)$ have simple-root evaluations $(1,0)$ and $(0,1)$, respectively. The last vector $(1,1,0)$ is the central coroot $c$ regarded as a coweight; both of its simple-root evaluations vanish. Together these three vectors form a complete basis of the dual Cartan space.
SeeAlso
  weightBasis
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  pairing
  (pairing,KMRoot,KMCoroot)
  (pairing,KMRoot,KMCoweight)
  (pairing,KMWeight,KMCoroot)
  (pairing,KMWeight,KMCoweight)
Headline
  Pair a weight-space element with a Cartan-space element
Usage
  q = pairing(x,h)
Inputs
  x:Thing
    a root or weight
  h:Thing
    a coroot or coweight in the same root datum
Outputs
  q:QQ
    their rational pairing
Description
  Text
    If $x=\sum_j a_j\alpha_j$ and $h=\sum_i b_i\alpha_i^\vee$, the value is $b^{\mathsf T}Aa$. For a weight and coroot it is the scalar product of Dynkin labels with coroot coefficients; for a root and coweight it uses simple-root evaluations. Pairing a weight with a coweight requires their complete realization vectors.
  Example
    D = rootDatum "A2";
    (pairing(kmRoot(D,{1,0}),kmCoroot(D,{0,1})),pairing(kmWeight(D,{1,0}),kmCoroot(D,{1,0})))
  Text
    The output $(-1,1)$ means $\langle\alpha_1,\alpha_2^\vee\rangle=-1$ and $\langle\omega_1,\alpha_1^\vee\rangle=1$. The two inputs with coordinates $(1,0)$ are a root and a weight, so their pairings differ.
SeeAlso
  kmWeight
  kmCoroot
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  toWeight
  (toWeight,KMRoot)
Headline
  Regard a root-lattice element as a complete weight
Usage
  y = toWeight alpha
Inputs
  alpha:KMRoot
    the element to be converted
Outputs
  y:KMWeight
    the same element in the target coordinate convention
Description
  Text
    Conversion uses the specified realization and retains all coordinates, including null or central directions in singular type. The parent root datum is unchanged.
  Example
    D = rootDatum "A2";
    alpha = kmRoot(D,{1,0})
    y = toWeight alpha;
    (datumCoordinates y,parentDatum y == D)
  Text
    The output coordinates $(2,-1)$ express that $\alpha_1$ has Dynkin labels $(2,-1)$. The final true says that conversion kept the same root datum.
SeeAlso
  datumCoordinates
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  toCoweight
  (toCoweight,KMCoroot)
Headline
  Regard a coroot-lattice element as a complete coweight
Usage
  y = toCoweight h
Inputs
  h:KMCoroot
    the element to be converted
Outputs
  y:KMCoweight
    the same element in the target coordinate convention
Description
  Text
    Conversion uses the specified realization and retains all coordinates, including null or central directions in singular type. The parent root datum is unchanged.
  Example
    D = rootDatum "A2";
    h = kmCoroot(D,{1,0})
    y = toCoweight h;
    (datumCoordinates y,parentDatum y == D)
  Text
    The output coordinates $(2,-1)$ express that $\alpha_1^\vee$ has simple-root evaluations $(2,-1)$. The final true says that conversion kept the same root datum.
SeeAlso
  datumCoordinates
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  toRoot
  (toRoot,KMWeight)
Headline
  Convert an integral root-lattice weight to root coordinates
Usage
  y = toRoot lambda
Inputs
  lambda:KMWeight
    the element to be converted
Outputs
  y:KMRoot
    the same element in the target coordinate convention
Description
  Text
    The input must be completely specified and lie in the integral span of the simple roots. A weight outside this lattice is rejected; rational span coordinates can be requested separately. The parent root datum is unchanged.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{1,1})
    y = toRoot lambda;
    (datumCoordinates y,parentDatum y == D)
  Text
    The output coordinates $(1,1)$ express that $\omega_1+\omega_2=\alpha_1+\alpha_2$. The final true says that conversion kept the same root datum.
SeeAlso
  weightToRootCoordinates
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  toCoroot
  (toCoroot,KMCoweight)
Headline
  Convert an integral coroot-lattice coweight to coroot coordinates
Usage
  y = toCoroot eta
Inputs
  eta:KMCoweight
    the element to be converted
Outputs
  y:KMCoroot
    the same element in the target coordinate convention
Description
  Text
    The input must be completely specified and lie in the integral span of the simple coroots. A weight outside this lattice is rejected; rational span coordinates can be requested separately. The parent root datum is unchanged.
  Example
    D = rootDatum "A2";
    eta = kmCoweight(D,{1,1})
    y = toCoroot eta;
    (datumCoordinates y,parentDatum y == D)
  Text
    The output coordinates $(1,1)$ express that $\omega_1^\vee+\omega_2^\vee=\alpha_1^\vee+\alpha_2^\vee$. The final true says that conversion kept the same root datum.
SeeAlso
  coweightToCorootCoordinates
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  weightToRootCoordinates
  (weightToRootCoordinates,KMWeight)
Headline
  Find rational simple-root coordinates
Usage
  a = weightToRootCoordinates lambda
Inputs
  lambda:KMWeight
    a complete element in the simple-root span
Outputs
  a:List
    rational coefficients in the simple-root basis
Description
  Text
    This solves for the coefficients of the given vector in the simple-root span. It does not require integral coefficients. An incomplete vector or a vector outside that span is rejected rather than projected onto it.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{1,0})
    weightToRootCoordinates lambda
  Text
    The coefficients are $(2/3,1/3)$. Thus $\omega_1=(2/3)\alpha_1+(1/3)\alpha_2$. The vector lies in the rational span but not in the integral root lattice.
SeeAlso
  toRoot
  isRootLatticeWeight
  "Roots, weights, and pairings"
///

doc ///
Key
  coweightToCorootCoordinates
  (coweightToCorootCoordinates,KMCoweight)
Headline
  Find rational simple-coroot coordinates
Usage
  a = coweightToCorootCoordinates eta
Inputs
  eta:KMCoweight
    a complete element in the simple-coroot span
Outputs
  a:List
    rational coefficients in the simple-coroot basis
Description
  Text
    This solves for the coefficients of the given vector in the simple-coroot span. It does not require integral coefficients. An incomplete vector or a vector outside that span is rejected rather than projected onto it.
  Example
    D = rootDatum "A2";
    eta = kmCoweight(D,{1,0})
    coweightToCorootCoordinates eta
  Text
    The coefficients are $(2/3,1/3)$. Thus $\omega_1^\vee=(2/3)\alpha_1^\vee+(1/3)\alpha_2^\vee$. The vector lies in the rational span but not in the integral coroot lattice.
SeeAlso
  toCoroot
  isCorootLatticeCoweight
  "Roots, weights, and pairings"
///

doc ///
Key
  isRootLatticeWeight
  (isRootLatticeWeight,KMWeight)
Headline
  Test membership in the integral root lattice
Usage
  b = isRootLatticeWeight lambda
Inputs
  lambda:KMWeight
    a completely specified element
Outputs
  b:Boolean
    whether its simple-root coordinates are integral
Description
  Text
    The test concerns the lattice generated by simple roots, not the character or cocharacter lattice of a chosen global form. An incomplete vector cannot determine this membership. The command requires the complete vector to lie in the rational simple-root span; outside that span it reports an error instead of returning a Boolean.
  Example
    D = rootDatum "A2";
    (isRootLatticeWeight kmWeight(D,{1,0}),isRootLatticeWeight kmWeight(D,{1,1}))
  Text
    The output $(\mathrm{false},\mathrm{true})$ distinguishes the fundamental weight $(1,0)$ from $(1,1)$. The latter is the sum of the two simple roots, while the former has coefficients $(2/3,1/3)$ in their basis.
SeeAlso
  weightToRootCoordinates
  integratesToGlobalForm
  "Roots, weights, and pairings"
///

doc ///
Key
  isCorootLatticeCoweight
  (isCorootLatticeCoweight,KMCoweight)
Headline
  Test membership in the integral coroot lattice
Usage
  b = isCorootLatticeCoweight eta
Inputs
  eta:KMCoweight
    a completely specified element
Outputs
  b:Boolean
    whether its simple-coroot coordinates are integral
Description
  Text
    The test concerns the lattice generated by simple coroots, not the character or cocharacter lattice of a chosen global form. An incomplete vector cannot determine this membership. The command requires the complete vector to lie in the rational simple-coroot span; outside that span it reports an error instead of returning a Boolean.
  Example
    D = rootDatum "A2";
    (isCorootLatticeCoweight kmCoweight(D,{1,0}),isCorootLatticeCoweight kmCoweight(D,{1,1}))
  Text
    The output $(\mathrm{false},\mathrm{true})$ distinguishes the fundamental coweight $(1,0)$ from $(1,1)$. The latter is the sum of the two simple coroots, while the former has coefficients $(2/3,1/3)$ in their basis.
SeeAlso
  coweightToCorootCoordinates
  integratesCoweightToGlobalForm
  "Roots, weights, and pairings"
///

doc ///
Key
  weylGroup
  (weylGroup,CartanDatum)
  (weylGroup,Matrix)
  (weylGroup,Realization)
  (weylGroup,RootDatum)
  (weylGroup,String)
Headline
  Construct the Weyl group of a specified Cartan datum
Usage
  W = weylGroup D
Inputs
  D:Thing
    a Cartan matrix, Cartan datum, realization, root datum, or finite type name
Outputs
  W:KMWeylGroup
    the Weyl group with its Cartan matrix and node labels
Description
  Text
    The simple reflections satisfy $s_i(\lambda)=\lambda-\langle\lambda,\alpha_i^\vee\rangle\alpha_i$. Construction does not enumerate the group, so infinite Weyl groups are allowed. Its node labels are those of the supplied Cartan datum.
  Example
    D = rootDatum "A2";
    W = weylGroup D;
    (W,weylGroupOrder datumCartanMatrix D)
  Text
    This is the Weyl group of $A_2$, isomorphic to the permutation group $S_3$, and the second entry gives its order 6. The group retains the two simple-root positions used by subsequent Weyl words.
SeeAlso
  weylElement
  coxeterMatrix
  "Weyl groups and standard parabolics"
///

doc ///
Key
  weylElement
  (weylElement,CartanDatum,List)
  (weylElement,KMWeylGroup,List)
  (weylElement,Matrix,List)
  (weylElement,Realization,List)
  (weylElement,RootDatum,List)
  (weylElement,String,List)
Headline
  Construct a Weyl element from a word in simple reflections
Usage
  w = weylElement(W,word)
Inputs
  W:KMWeylGroup
    the Weyl group of a specified Cartan datum
  word:List
    node labels, positions, or position-label tables
Outputs
  w:WeylElement
    the element represented by the supplied word
Description
  Text
    A Cartan matrix, Cartan datum, realization, root datum, or finite type name may be supplied instead of a Weyl group. Finite, affine, and indefinite types are allowed. In $s_{i_1}\cdots s_{i_k}$ the rightmost reflection acts first. The word is reduced automatically, and the empty list represents the identity. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    W = weylGroup "A2";
    w = weylElement(W,{1,2,1,1});
    (w,weylWord w,weylLength w)
  Text
    The two adjacent $s_1$ factors cancel, so the output represents $s_1s_2$ with reduced word $(1,2)$ and length 2. This is a group element, not a formal list that treats different expressions as different elements.
SeeAlso
  weylWord
  weylAction
  "Weyl groups and standard parabolics"
///

doc ///
Key
  weylParent
  (weylParent,WeylElement)
Headline
  Recover the Weyl group of a Weyl element
Usage
  W = weylParent w
Inputs
  w:WeylElement
    an element constructed in a specified Weyl group
Outputs
  W:KMWeylGroup
    its Weyl group
Description
  Text
    The group contains the Cartan matrix and node labels used to interpret the element. This is meaningful even when its stored reduced word is empty.
  Example
    W = weylGroup "A2";
    w = weylElement(W,{1,2})
    weylParent w == W
  Text
    The true says that $s_1s_2$ is still an element of the chosen $A_2$ Weyl group. Its word is interpreted using that group's simple-root order.
SeeAlso
  weylGroup
  weylWord
  "Weyl groups and standard parabolics"
///

doc ///
Key
  weylWord
  (weylWord,WeylElement)
Headline
  Return a reduced word in simple-reflection positions
Usage
  word = weylWord w
Inputs
  w:WeylElement
    a Weyl element with its parent group
Outputs
  word:List
    a reduced word using node positions numbered starting at 1
Description
  Text
    The entries are positions, even when the Weyl group has custom node labels. This gives one reduced expression, not the set of every reduced expression for the element. The rightmost entry acts first.
  Example
    C = cartanDatum("A2",NodeLabels=>{"left","right"});
    w = weylElement(weylGroup C,{"left","right","left"})
    weylWord w
  Text
    The list $(1,2,1)$ means $s_{\mathrm{left}}s_{\mathrm{right}}s_{\mathrm{left}}$. The alternate reduced expression $(2,1,2)$ represents the same element by the $A_2$ braid relation.
SeeAlso
  weylElement
  reducedWord
  "Weyl groups and standard parabolics"
///

doc ///
Key
  identityWeylElement
  (identityWeylElement,KMWeylGroup)
Headline
  Construct the identity Weyl element
Usage
  e = identityWeylElement W
Inputs
  W:KMWeylGroup
    the Weyl group of a specified Cartan datum
Outputs
  e:WeylElement
    the identity in W
Description
  Text
    The identity has empty reduced word, Coxeter length 0, and fixes every root, coroot, weight, coweight, and character on which this Weyl group acts.
  Example
    D = rootDatum "A2";
    e = identityWeylElement weylGroup D;
    lambda = kmWeight(D,{2,1})
    (e,weylWord e,weylAction(e,lambda) == lambda)
  Text
    The element is displayed as 1_W, its word is empty, and true confirms that it fixes the weight with Dynkin labels $(2,1)$. The identity still retains its Weyl group.
SeeAlso
  weylElement
  weylAction
  "Weyl groups and standard parabolics"
///

doc ///
Key
  simpleWeylReflection
  (simpleWeylReflection,KMWeylGroup,Thing)
  (simpleWeylReflection,KMWeylGroup,ZZ)
  (simpleWeylReflection,KMWeylGroup,String)
Headline
  Construct the reflection in a simple root
Usage
  s = simpleWeylReflection(W,i)
Inputs
  W:KMWeylGroup
    the Weyl group of a specified Cartan datum
  i:Thing
    a node position, chosen label, or position-label table
Outputs
  s:WeylElement
    the simple reflection s_i
Description
  Text
    The reflection acts by $s_i(\lambda)=\lambda-\langle\lambda,\alpha_i^\vee\rangle\alpha_i$ and has order 2. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    D = rootDatum "A2";
    s = simpleWeylReflection(weylGroup D,1);
    (s,datumCoordinates weylAction(s,kmWeight(D,{1,0})),s*s == identityWeylElement weylGroup D)
  Text
    The weight $(1,0)$ is reflected to $(-1,1)$, since $s_1\omega_1=\omega_1-\alpha_1$. The final true verifies $s_1^2=1$.
SeeAlso
  reflectionMatrix
  weylAction
  "Weyl groups and standard parabolics"
///

doc ///
Key
  inverseWeylElement
  (inverseWeylElement,WeylElement)
Headline
  Invert a Weyl element
Usage
  v = inverseWeylElement w
Inputs
  w:WeylElement
    an element of a specified Weyl group
Outputs
  v:WeylElement
    its inverse in the same group
Description
  Text
    Each simple reflection is an involution, so $(s_{i_1}\cdots s_{i_k})^{-1}=s_{i_k}\cdots s_{i_1}$. The reversed word is reduced to the package's chosen representative.
  Example
    W = weylGroup "A2";
    w = weylElement(W,{1,2})
    v = inverseWeylElement w;
    (v,weylWord v,w*v == identityWeylElement W)
  Text
    The inverse is $s_2s_1$, represented by $(2,1)$, and true confirms $(s_1s_2)(s_2s_1)=1$. The inverse belongs to the same labeled Weyl group.
SeeAlso
  weylWord
  identityWeylElement
  "Weyl groups and standard parabolics"
///

doc ///
Key
  weylAction
  (weylAction,WeylElement,FormalCharacter)
  (weylAction,WeylElement,KMCoroot)
  (weylAction,WeylElement,KMCoweight)
  (weylAction,WeylElement,KMRoot)
  (weylAction,WeylElement,KMWeight)
Headline
  Act on a root, weight, or character by a Weyl element
Usage
  y = weylAction(w,x)
Inputs
  w:WeylElement
    a Weyl element
  x:Thing
    a root, coroot, weight, coweight, or formal character with the matching Cartan datum
Outputs
  y:Thing
    the transformed object of the same kind
Description
  Text
    The action is the usual reflection action, with the rightmost factor acting first. A character term $m e^\mu$ becomes $m e^{w\mu}$. Complete realization vectors remain complete; a Dynkin-label class remains a class. A transformed truncated character does not acquire an unjustified completeness claim.
  Example
    D = rootDatum "A2";
    w = weylElement(weylGroup D,{1,2})
    (datumCoordinates weylAction(w,kmWeight(D,{1,0})),datumCoordinates weylAction(w,kmRoot(D,{1,0})))
  Text
    The first tuple $(-1,1)$ is $s_1s_2(\omega_1)$ in Dynkin labels. The second tuple $(0,1)$ is $s_1s_2(\alpha_1)=\alpha_2$ in simple-root coefficients. The command retains these distinct coordinate conventions.
SeeAlso
  simpleWeylReflection
  datumCoordinates
  formalCharacter
  "Weyl groups and standard parabolics"
///

doc ///
Key
  standardParabolic
  (standardParabolic,CartanDatum)
  (standardParabolic,CartanDatum,List)
  (standardParabolic,CartanDatum,Set)
  (standardParabolic,Matrix)
  (standardParabolic,Matrix,List)
  (standardParabolic,Matrix,Set)
  (standardParabolic,Realization)
  (standardParabolic,Realization,List)
  (standardParabolic,Realization,Set)
  (standardParabolic,RootDatum)
  (standardParabolic,RootDatum,List)
  (standardParabolic,RootDatum,Set)
  (standardParabolic,String)
  (standardParabolic,String,List)
  (standardParabolic,String,Set)
  [standardParabolic,RemovedNodes]
  [standardParabolic,LeviNodes]
Headline
  Choose a standard parabolic by removed or Levi nodes
Usage
  P = standardParabolic(D,RemovedNodes=>J)
  P = standardParabolic(D,LeviNodes=>I)
  P = standardParabolic(D,J)
Inputs
  D:Thing
    a Cartan matrix, Cartan datum, realization, root datum, or finite type name
  J:Thing
    a list or set of removed nodes
  I:Thing
    a list or set of retained Levi nodes
Outputs
  P:StandardParabolic
    the complementary node sets and their induced Levi datum
Description
  Text
    Removing a node means crossing it in the Dynkin diagram. Its coefficient contributes to the nilradical grading, while retained nodes generate the derived Levi. Exactly one node convention must be specified; positional input means removed nodes. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    D = rootDatum "A3";
    P = standardParabolic(D,RemovedNodes=>{2});
    (removedNodes P,leviNodes P,datumCartanMatrix parabolicLevi P)
  Text
    The removed set is $(2)$ and the retained set is $(1,3)$. The Levi Cartan matrix is diagonal with two entries 2, so its derived algebra is $\mathfrak{sl}_2\oplus\mathfrak{sl}_2$.
SeeAlso
  removedNodes
  leviNodes
  parabolicDegree
  "Weyl groups and standard parabolics"
///

doc ///
Key
  removedNodes
  (removedNodes,StandardParabolic)
Headline
  Read the crossed nodes of a standard parabolic
Usage
  J = removedNodes P
Inputs
  P:StandardParabolic
    a standard parabolic with specified removed and retained nodes
Outputs
  J:List
    sorted removed node positions, numbered starting at 1
Description
  Text
    The result uses ambient node positions rather than chosen labels or a renumbered Levi diagram. Removed and retained node sets are complementary.
  Example
    P = standardParabolic("A3",RemovedNodes=>{2});
    removedNodes P
  Text
    The list $(2)$ identifies the crossed middle node of the $A_3$ diagram. Its coefficient determines the parabolic degree.
SeeAlso
  standardParabolic
  leviNodes
  "Weyl groups and standard parabolics"
///

doc ///
Key
  leviNodes
  (leviNodes,StandardParabolic)
Headline
  Read the retained nodes of a standard parabolic
Usage
  J = leviNodes P
Inputs
  P:StandardParabolic
    a standard parabolic with specified removed and retained nodes
Outputs
  J:List
    sorted retained node positions, numbered starting at 1
Description
  Text
    The result uses ambient node positions rather than chosen labels or a renumbered Levi diagram. Removed and retained node sets are complementary.
  Example
    P = standardParabolic("A3",RemovedNodes=>{2});
    leviNodes P
  Text
    The list $(1,3)$ identifies the two retained nodes. They define the two $A_1$ factors of the derived Levi, in ambient positions 1 and 3.
SeeAlso
  standardParabolic
  removedNodes
  "Weyl groups and standard parabolics"
///

doc ///
Key
  parabolicLevi
  (parabolicLevi,StandardParabolic)
Headline
  Return the Cartan datum of the derived Levi
Usage
  L = parabolicLevi P
Inputs
  P:StandardParabolic
    a standard parabolic with specified removed and retained nodes
Outputs
  L:CartanDatum
    the Cartan datum on the retained nodes
Description
  Text
    The Cartan matrix is the principal submatrix on the Levi nodes. Chosen labels are retained. This gives the derived semisimple or Kac--Moody Levi datum, not its additional central torus.
  Example
    P = standardParabolic("A3",RemovedNodes=>{2});
    L = parabolicLevi P;
    (datumCartanMatrix L,L#"nodeLabels")
  Text
    The matrix is $\operatorname{diag}(2,2)$ and the labels are $(1,3)$. Thus the derived Levi is $\mathfrak{sl}_2\oplus\mathfrak{sl}_2$ with its original ambient labels. Use the reductive Levi commands when its center must also be retained.
SeeAlso
  reductiveLeviDatum
  leviNodes
  "Weyl groups and standard parabolics"
///

doc ///
Key
  parabolicDegree
  (parabolicDegree,StandardParabolic,KMRoot)
Headline
  Compute the grading degree of a root-lattice element
Usage
  d = parabolicDegree(P,beta)
Inputs
  P:StandardParabolic
    a standard parabolic with specified removed and retained nodes
  beta:KMRoot
    a root-lattice element with the same Cartan datum
Outputs
  d:ZZ
    the sum of its coefficients at the removed nodes
Description
  Text
    For $\beta=\sum_i b_i\alpha_i$ and removed nodes $J$, the degree is $\sum_{i\in J}b_i$. The calculation is linear and does not itself test whether the root-lattice vector is an actual root.
  Example
    D = rootDatum "A3";
    P = standardParabolic(D,RemovedNodes=>{2});
    (parabolicDegree(P,kmRoot(D,{1,1,0})),parabolicDegree(P,kmRoot(D,{1,0,0})))
  Text
    The degrees are $(1,0)$. The root $\alpha_1+\alpha_2$ belongs to the degree-one nilradical, whereas $\alpha_1$ lies in the degree-zero Levi root system.
SeeAlso
  isLeviRoot
  nilradicalRoots
  "Weyl groups and standard parabolics"
///

doc ///
Key
  isLeviRoot
  (isLeviRoot,StandardParabolic,KMRoot)
Headline
  Test degree zero for a root-lattice element
Usage
  b = isLeviRoot(P,beta)
Inputs
  P:StandardParabolic
    a standard parabolic with specified removed and retained nodes
  beta:KMRoot
    a root-lattice element with the same Cartan datum
Outputs
  b:Boolean
    whether its parabolic degree is zero
Description
  Text
    For an actual root this is precisely membership in the Levi root system. On an arbitrary root-lattice vector the command only checks that the sum of the removed-node coefficients is zero; it neither tests root membership nor demands that every removed coefficient vanish separately.
  Example
    D = rootDatum "A3";
    P = standardParabolic(D,RemovedNodes=>{2});
    (isLeviRoot(P,kmRoot(D,{1,0,0})),isLeviRoot(P,kmRoot(D,{1,1,0})))
  Text
    The output $(\mathrm{true},\mathrm{false})$ identifies $\alpha_1$ as a Levi root, but not $\alpha_1+\alpha_2$. Both inputs here are actual positive roots, so the degree test has the asserted root-system interpretation.
SeeAlso
  parabolicDegree
  cartanPositiveRoots
  "Weyl groups and standard parabolics"
///

doc ///
Key
  dominantizeWeight
  (dominantizeWeight,CartanDatum,List)
  (dominantizeWeight,CartanDatum,Matrix)
  (dominantizeWeight,KMWeight)
  (dominantizeWeight,Matrix,List)
  (dominantizeWeight,Matrix,Matrix)
  (dominantizeWeight,Realization,List)
  (dominantizeWeight,Realization,Matrix)
  (dominantizeWeight,RootDatum,KMWeight)
  (dominantizeWeight,RootDatum,List)
  (dominantizeWeight,RootDatum,Matrix)
  (dominantizeWeight,String,List)
  (dominantizeWeight,String,Matrix)
Headline
  Find the dominant weight in a finite Weyl orbit
Usage
  T = dominantizeWeight lambda
  T = dominantizeWeight(D,a)
Inputs
  lambda:KMWeight
    an integral weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    integral Dynkin labels
Outputs
  T:HashTable
    the dominant weight, a Weyl element taking lambda to it, and its simple walls
Description
  Text
    Every finite Weyl orbit meets the closed dominant chamber in exactly one weight. The "dominantWeight" entry is that representative; "word" gives a witness with the rightmost reflection acting first. Typed input also gives "toDominantElement" and retains complete realization coordinates. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "A2";
    T = dominantizeWeight kmWeight(D,{-1,0});
    (datumCoordinates T#"dominantWeight",T#"word",T#"wallNodes")
  Text
    The output $((0,1),(2,1),(1))$ means that $s_2s_1$ sends the weight $(-1,0)$ to $(0,1)$. The dominant weight lies on the first simple wall because its first Dynkin label is zero.
SeeAlso
  weylAction
  weightStabilizer
  "Finite structural invariants"
///

doc ///
Key
  weightStabilizer
  (weightStabilizer,CartanDatum,List)
  (weightStabilizer,CartanDatum,Matrix)
  (weightStabilizer,KMWeight)
  (weightStabilizer,Matrix,List)
  (weightStabilizer,Matrix,Matrix)
  (weightStabilizer,Realization,List)
  (weightStabilizer,Realization,Matrix)
  (weightStabilizer,RootDatum,KMWeight)
  (weightStabilizer,RootDatum,List)
  (weightStabilizer,RootDatum,Matrix)
  (weightStabilizer,String,List)
  (weightStabilizer,String,Matrix)
Headline
  Find generators for the Weyl stabilizer of a weight
Usage
  T = weightStabilizer lambda
  T = weightStabilizer(D,a)
Inputs
  lambda:KMWeight
    an integral weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    integral Dynkin labels
Outputs
  T:HashTable
    the dominant walls, stabilizer generators, and its order
Description
  Text
    For a dominant weight, the stabilizer is the standard parabolic Weyl group generated by simple reflections at zero Dynkin labels. For a nondominant weight these generators are conjugated by a witness taking it to the dominant chamber. The table gives generators, not an enumeration of all stabilizer elements. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "A2";
    T = weightStabilizer kmWeight(D,{1,0});
    (T#"dominantWallNodes",T#"generatingElements",T#"order")
  Text
    The zero second label gives the wall set $(2)$, generator $s_2$, and order 2. Thus $\operatorname{Stab}_W(\omega_1)=\{1,s_2\}$ in type $A_2$.
SeeAlso
  dominantizeWeight
  standardParabolic
  "Finite structural invariants"
///

doc ///
Key
  dualWeight
  (dualWeight,CartanDatum,List)
  (dualWeight,CartanDatum,Matrix)
  (dualWeight,KMWeight)
  (dualWeight,Matrix,List)
  (dualWeight,Matrix,Matrix)
  (dualWeight,Realization,List)
  (dualWeight,Realization,Matrix)
  (dualWeight,RootDatum,KMWeight)
  (dualWeight,RootDatum,List)
  (dualWeight,RootDatum,Matrix)
  (dualWeight,String,List)
  (dualWeight,String,Matrix)
Headline
  Compute the highest weight of the contragredient representation
Usage
  mu = dualWeight lambda
  mu = dualWeight(D,a)
Inputs
  lambda:KMWeight
    an integral weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    integral Dynkin labels
Outputs
  mu:Thing
    the weight -w_0(lambda), with the corresponding coordinate form
Description
  Text
    For a dominant highest weight $\lambda$, the dual representation has highest weight $-w_0\lambda$. Arbitrary integral weights are also accepted, where this is the linear opposition involution rather than a highest-weight assertion. Complete weights have their additional Weyl-fixed central character negated. Matrix input preserves a list or row-matrix output; datum input gives a typed weight. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{2,1})
    (datumCoordinates dualWeight lambda,dualWeight dualWeight lambda == lambda)
  Text
    The dual highest weight is $(1,2)$, so $V(2,1)^*\cong V(1,2)$. The final true verifies that taking the dual twice restores the original weight.
SeeAlso
  isSelfDual
  dualCharacter
  "Finite structural invariants"
///

doc ///
Key
  isOppositionFixed
  (isOppositionFixed,CartanDatum,List)
  (isOppositionFixed,CartanDatum,Matrix)
  (isOppositionFixed,KMWeight)
  (isOppositionFixed,Matrix,List)
  (isOppositionFixed,Matrix,Matrix)
  (isOppositionFixed,Realization,List)
  (isOppositionFixed,Realization,Matrix)
  (isOppositionFixed,RootDatum,KMWeight)
  (isOppositionFixed,RootDatum,List)
  (isOppositionFixed,RootDatum,Matrix)
  (isOppositionFixed,String,List)
  (isOppositionFixed,String,Matrix)
Headline
  Test whether a weight is fixed by opposition
Usage
  b = isOppositionFixed lambda
  b = isOppositionFixed(D,a)
Inputs
  lambda:KMWeight
    an integral weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    integral Dynkin labels
Outputs
  b:Boolean
    whether -w_0(lambda) equals lambda
Description
  Text
    This tests $-w_0\lambda=\lambda$ without requiring dominance. For an incomplete Dynkin-label class, equality means fixedness of the class, not of every complete lift. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "A2";
    (isOppositionFixed kmWeight(D,{1,0}),isOppositionFixed kmWeight(D,{1,1}))
  Text
    The output $(\mathrm{false},\mathrm{true})$ says that opposition exchanges $(1,0)$ with $(0,1)$ and fixes $(1,1)$ in type $A_2$.
SeeAlso
  dualWeight
  isSelfDual
  "Finite structural invariants"
///

doc ///
Key
  isSelfDual
  (isSelfDual,CartanDatum,List)
  (isSelfDual,CartanDatum,Matrix)
  (isSelfDual,KMWeight)
  (isSelfDual,Matrix,List)
  (isSelfDual,Matrix,Matrix)
  (isSelfDual,Realization,List)
  (isSelfDual,Realization,Matrix)
  (isSelfDual,RootDatum,KMWeight)
  (isSelfDual,RootDatum,List)
  (isSelfDual,RootDatum,Matrix)
  (isSelfDual,String,List)
  (isSelfDual,String,Matrix)
Headline
  Test self-duality of a finite highest-weight representation
Usage
  b = isSelfDual lambda
  b = isSelfDual(D,a)
Inputs
  lambda:KMWeight
    an integral dominant highest weight; complete coordinates are required when central directions occur
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    integral Dynkin labels
Outputs
  b:Boolean
    whether the corresponding irreducible representation is self-dual
Description
  Text
    This tests $V(\lambda)\cong V(\lambda)^*$. It requires dominance and compares the complete central character as well as the semisimple highest weight. It does not distinguish a symmetric invariant form from an alternating one. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "A2";
    (isSelfDual kmWeight(D,{1,0}),isSelfDual kmWeight(D,{1,1}))
  Text
    The output $(\mathrm{false},\mathrm{true})$ says that the standard $A_2$ representation is not self-dual, whereas the adjoint representation with highest weight $(1,1)$ is.
SeeAlso
  dualWeight
  frobeniusSchurIndicator
  "Finite structural invariants"
///

doc ///
Key
  fundamentalCoweights
  (fundamentalCoweights,CartanDatum)
  (fundamentalCoweights,KMWeylGroup)
  (fundamentalCoweights,Matrix)
  (fundamentalCoweights,Realization)
  (fundamentalCoweights,RootDatum)
  (fundamentalCoweights,String)
Headline
  Express finite fundamental coweights in the coroot basis
Usage
  F = fundamentalCoweights D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  F:Matrix
    the rational matrix whose rows are the fundamental coweights in simple-coroot coordinates
Description
  Text
    If $A$ is the Cartan matrix, then $F=A^{-1}$ and $FA=I$. Thus row $i$ is the coweight $\omega_i^\vee$ satisfying $\langle\alpha_j,\omega_i^\vee\rangle=\delta_{ij}$. These rational coroot coefficients differ from the standard unit-list evaluation coordinates used by @TO kmCoweight@.
  Example
    A = cartanA 3;
    F = fundamentalCoweights A;
    (F,F*(A**QQ))
  Text
    The rows are $(2/3,1/3)$ and $(1/3,2/3)$. Thus $\omega_1^\vee=(2\alpha_1^\vee+\alpha_2^\vee)/3$, and similarly for $\omega_2^\vee$. The identity matrix confirms the required simple-root evaluations.
SeeAlso
  kmCoweight
  coweightToCorootCoordinates
  "Finite structural invariants"
///

doc ///
Key
  normalizedInvariantForm
  (normalizedInvariantForm,CartanDatum)
  (normalizedInvariantForm,KMWeylGroup)
  (normalizedInvariantForm,Matrix)
  (normalizedInvariantForm,Realization)
  (normalizedInvariantForm,RootDatum)
  (normalizedInvariantForm,String)
Headline
  Return the invariant form with long roots of squared norm 2
Usage
  G = normalizedInvariantForm D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  G:Matrix
    the rational Gram matrix in simple-root coordinates
Description
  Text
    Every simple component is normalized separately so that long roots have squared norm 2. If $a$ and $b$ are simple-root coefficient columns, then $(a,b)=a^{\mathsf T}Gb$. Short roots have squared norm $2/r$, where $r$ is the lacing number. No form on additional central directions is determined by this command.
  Example
    G = normalizedInvariantForm cartanB 2;
    (G,G_(0,0),G_(1,1))
  Text
    The Gram matrix is $\begin{pmatrix}2&-1\\-1&1\end{pmatrix}$. Its diagonal says that the first simple root has squared norm 2 and the second squared norm 1. This is the long-to-short ratio 2 in type $B_2$.
SeeAlso
  lacingNumber
  quadraticCasimir
  "Finite structural invariants"
///

doc ///
Key
  marks
  (marks,CartanDatum)
  (marks,KMWeylGroup)
  (marks,Matrix)
  (marks,Realization)
  (marks,RootDatum)
  (marks,String)
Headline
  Return the coefficients of the highest finite root
Usage
  a = marks D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  a:List
    integral coefficients of the highest root in the simple-root basis
Description
  Text
    The Cartan matrix must be connected and finite. If $\theta$ is its highest root, then $\theta=\sum_i a_i\alpha_i$. The entries are root coefficients, not Dynkin labels; use @TO toFundamental@ when the adjoint highest weight is needed in Dynkin coordinates.
  Example
    A = cartanB 2;
    (marks A,toFundamental(A,marks A))
  Text
    The root coefficients $(1,2)$ give $\theta=\alpha_1+2\alpha_2$. Its Dynkin labels are $(0,2)$, so the adjoint representation is $V(0,2)$ in this $B_2$ convention.
SeeAlso
  highestRoot
  comarks
  toFundamental
  "Finite structural invariants"
///

doc ///
Key
  highestCoroot
  (highestCoroot,CartanDatum)
  (highestCoroot,KMWeylGroup)
  (highestCoroot,Matrix)
  (highestCoroot,Realization)
  (highestCoroot,RootDatum)
  (highestCoroot,String)
Headline
  Return the coroot attached to the highest long root
Usage
  a = highestCoroot D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  a:List
    coefficients of theta^vee in the simple-coroot basis
Description
  Text
    For connected finite type, this is the coroot of the highest root $\theta$. Equivalently it is the highest short root of the dual root system. In nonsimply-laced type this is not the highest root of the entire coroot system.
  Example
    A = cartanB 2;
    (highestCoroot A,highestRoot transpose A)
  Text
    The first vector $(1,1)$ means $\theta^\vee=\alpha_1^\vee+\alpha_2^\vee$ for $\theta=\alpha_1+2\alpha_2$ in $B_2$. The second vector $(2,1)$ is the highest root of the dual $C_2$ system; these are different coroot-space elements.
SeeAlso
  comarks
  highestRoot
  dualCartanMatrix
  "Finite structural invariants"
///

doc ///
Key
  comarks
  (comarks,CartanDatum)
  (comarks,KMWeylGroup)
  (comarks,Matrix)
  (comarks,Realization)
  (comarks,RootDatum)
  (comarks,String)
Headline
  Return the simple-coroot coefficients of the highest-root coroot
Usage
  a = comarks D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  a:List
    coefficients of theta^vee in the simple-coroot basis
Description
  Text
    For connected finite type, $\theta^\vee=\sum_i a_i^\vee\alpha_i^\vee$, where $\theta$ is the highest long root. With long-root squared norm 2, $h^\vee=1+\sum_i a_i^\vee$. This agrees with @TO highestCoroot@ and need not agree with the highest root of the dual system.
  Example
    A = cartanB 2;
    (comarks A,1+sum comarks A,dualCoxeterNumber A)
  Text
    The coefficients are $(1,1)$, so $\theta^\vee=\alpha_1^\vee+\alpha_2^\vee$ and the dual Coxeter number is $1+1+1=3$. The last two entries agree.
SeeAlso
  highestCoroot
  marks
  dualCoxeterNumber
  "Finite structural invariants"
///

doc ///
Key
  lacingNumber
  (lacingNumber,CartanDatum)
  (lacingNumber,KMWeylGroup)
  (lacingNumber,Matrix)
  (lacingNumber,Realization)
  (lacingNumber,RootDatum)
  (lacingNumber,String)
Headline
  Compute the long-to-short squared-root-length ratio
Usage
  r = lacingNumber D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  r:ZZ
    the ratio 1, 2, or 3
Description
  Text
    The matrix must be connected and finite. The ratio is 1 in simply-laced type, 2 in types $B,C,F_4$, and 3 in type $G_2$. It is the ratio of squared lengths, not of lengths.
  Example
    (lacingNumber "A2",lacingNumber "B2",lacingNumber "G2")
  Text
    The output $(1,2,3)$ records equal root lengths in $A_2$, ratio 2 in $B_2$, and ratio 3 in $G_2$. With long roots normalized to squared norm 2, the short-root norms are respectively 2, 1, and $2/3$.
SeeAlso
  normalizedInvariantForm
  symmetrizer
  "Finite structural invariants"
///

doc ///
Key
  weylExponents
  (weylExponents,CartanDatum)
  (weylExponents,KMWeylGroup)
  (weylExponents,Matrix)
  (weylExponents,Realization)
  (weylExponents,RootDatum)
  (weylExponents,String)
Headline
  Compute the exponents of a finite Weyl group
Usage
  d = weylExponents D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  d:List
    the exponents in nondecreasing order
Description
  Text
    The exponents $m_i$ are one less than the degrees of basic polynomial invariants of the Weyl group. They are computed from the distribution of heights of positive roots and are combined over simple components.
  Example
    weylExponents "A3"
  Text
    The exponents of $A_3$ are $(1,2,3)$. Equivalently its basic Weyl-invariant polynomial degrees are $(2,3,4)$.
SeeAlso
  invariantDegrees
  weylLengthGenerating
  "Finite structural invariants"
///

doc ///
Key
  invariantDegrees
  (invariantDegrees,CartanDatum)
  (invariantDegrees,KMWeylGroup)
  (invariantDegrees,Matrix)
  (invariantDegrees,Realization)
  (invariantDegrees,RootDatum)
  (invariantDegrees,String)
Headline
  Compute the basic invariant degrees of a finite Weyl group
Usage
  d = invariantDegrees D
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  d:List
    the degrees in nondecreasing order
Description
  Text
    The degrees are $m_i+1$, where $m_i$ are the Weyl exponents. They are the degrees of homogeneous generators of the polynomial ring of Weyl invariants on the reflection representation. On a product, degrees are combined from its components.
  Example
    invariantDegrees "A3"
  Text
    The degrees $(2,3,4)$ describe the polynomial invariants for $A_3$. Their product is $2\cdot3\cdot4=24$, the order of its Weyl group $S_4$.
SeeAlso
  weylExponents
  weylLengthGenerating
  "Finite structural invariants"
///

doc ///
Key
  minusculeNodes
  [minusculeNodes,NodeFormat]
  (minusculeNodes,CartanDatum)
  (minusculeNodes,KMWeylGroup)
  (minusculeNodes,Matrix)
  (minusculeNodes,Realization)
  (minusculeNodes,RootDatum)
  (minusculeNodes,String)
Headline
  Find the nodes of minuscule fundamental representations
Usage
  I = minusculeNodes D
  I = minusculeNodes(D,NodeFormat=>"Positions")
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  I:List
    the selected node labels, positions, or position-label tables
Description
  Text
    The fundamental weight $\omega_i$ is minuscule if its pairing with every positive coroot is 0 or 1; equivalently its coefficient in the highest root of the dual system is 1. Matrix input gives positions numbered starting at 1. Datum input normally gives its node labels; ambiguous integer labels produce position-label tables. The NodeFormat option selects "Positions", "Labels", or "Records".
  Example
    D = cartanDatum("B3",NodeLabels=>{"vector","middle","spin"});
    (minusculeNodes D,minusculeNodes(D,NodeFormat=>"Positions"))
  Text
    The selected label is spin, at position 3. Its fundamental highest weight $(0,0,1)$ gives the eight-dimensional spin representation, whose weights form a single Weyl orbit.
SeeAlso
  cominusculeNodes
  isMinusculeNode
  "Finite structural invariants"
///

doc ///
Key
  cominusculeNodes
  [cominusculeNodes,NodeFormat]
  (cominusculeNodes,CartanDatum)
  (cominusculeNodes,KMWeylGroup)
  (cominusculeNodes,Matrix)
  (cominusculeNodes,Realization)
  (cominusculeNodes,RootDatum)
  (cominusculeNodes,String)
Headline
  Find the nodes defining abelian maximal-parabolic nilradicals
Usage
  I = cominusculeNodes D
  I = cominusculeNodes(D,NodeFormat=>"Positions")
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
Outputs
  I:List
    the selected node labels, positions, or position-label tables
Description
  Text
    A node is cominuscule if its coefficient in the highest root of its simple component is 1. The nilradical of the associated maximal parabolic is then abelian. Matrix input gives positions numbered starting at 1. Datum input normally gives its node labels; ambiguous integer labels produce position-label tables. The NodeFormat option selects "Positions", "Labels", or "Records".
  Example
    D = cartanDatum("B3",NodeLabels=>{"vector","middle","spin"});
    (cominusculeNodes D,cominusculeNodes(D,NodeFormat=>"Positions"))
  Text
    The selected label is vector, at position 1. Crossing this node gives an abelian nilradical. Its fundamental representation has highest weight $(1,0,0)$; it is cominuscule but not minuscule.
SeeAlso
  minusculeNodes
  isCominusculeNode
  "Finite structural invariants"
///

doc ///
Key
  isMinusculeNode
  (isMinusculeNode,CartanDatum,Thing)
  (isMinusculeNode,KMWeylGroup,Thing)
  (isMinusculeNode,Matrix,ZZ)
  (isMinusculeNode,Realization,Thing)
  (isMinusculeNode,RootDatum,Thing)
  (isMinusculeNode,String,Thing)
Headline
  Test whether a node has a minuscule fundamental weight
Usage
  b = isMinusculeNode(D,i)
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
  i:Thing
    a node position, chosen label, or position-label table
Outputs
  b:Boolean
    whether the chosen node satisfies the criterion
Description
  Text
    The test is $\langle\omega_i,\beta^\vee\rangle\in\{0,1\}$ for every positive root $\beta$. Matrix input requires a position numbered starting at 1. For other inputs labels and position-label tables are also accepted. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    (isMinusculeNode("B3",3),isMinusculeNode("B3",1))
  Text
    The output $(\mathrm{true},\mathrm{false})$ says that the spin node 3 is minuscule in $B_3$, whereas the vector node 1 is not.
SeeAlso
  minusculeNodes
  isMinusculeWeight
  "Finite structural invariants"
///

doc ///
Key
  isCominusculeNode
  (isCominusculeNode,CartanDatum,Thing)
  (isCominusculeNode,KMWeylGroup,Thing)
  (isCominusculeNode,Matrix,ZZ)
  (isCominusculeNode,Realization,Thing)
  (isCominusculeNode,RootDatum,Thing)
  (isCominusculeNode,String,Thing)
Headline
  Test whether a node is cominuscule
Usage
  b = isCominusculeNode(D,i)
Inputs
  D:Thing
    a finite-type Cartan matrix, Cartan datum, realization, root datum, Weyl group, or type name
  i:Thing
    a node position, chosen label, or position-label table
Outputs
  b:Boolean
    whether the chosen node satisfies the criterion
Description
  Text
    The coefficient of $\alpha_i$ in the highest root of its finite simple component must be 1; equivalently the associated maximal-parabolic nilradical is abelian. Matrix input requires a position numbered starting at 1. For other inputs labels and position-label tables are also accepted. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    (isCominusculeNode("B3",1),isCominusculeNode("B3",3))
  Text
    The output $(\mathrm{true},\mathrm{false})$ says that the vector node 1 is cominuscule in $B_3$, whereas the spin node 3 is not. This criterion is different from minuscule.
SeeAlso
  cominusculeNodes
  isCominusculeWeight
  "Finite structural invariants"
///

doc ///
Key
  isMinusculeWeight
  (isMinusculeWeight,CartanDatum,List)
  (isMinusculeWeight,CartanDatum,Matrix)
  (isMinusculeWeight,KMWeight)
  (isMinusculeWeight,Matrix,List)
  (isMinusculeWeight,Matrix,Matrix)
  (isMinusculeWeight,Realization,List)
  (isMinusculeWeight,Realization,Matrix)
  (isMinusculeWeight,RootDatum,KMWeight)
  (isMinusculeWeight,RootDatum,List)
  (isMinusculeWeight,RootDatum,Matrix)
  (isMinusculeWeight,String,List)
  (isMinusculeWeight,String,Matrix)
Headline
  Test whether a dominant weight is minuscule
Usage
  b = isMinusculeWeight lambda
  b = isMinusculeWeight(D,a)
Inputs
  lambda:KMWeight
    a dominant integral weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    nonnegative integral Dynkin labels
Outputs
  b:Boolean
    whether the corresponding criterion holds
Description
  Text
    A dominant weight is minuscule if $\langle\lambda,\beta^\vee\rangle$ is 0 or 1 for every positive root $\beta$. Its irreducible representation has a single Weyl orbit of weights, each of multiplicity 1. The zero weight satisfies the criterion; on products the test is performed in every simple component. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "B3";
    (isMinusculeWeight kmWeight(D,{0,0,1}),isMinusculeWeight kmWeight(D,apply({0,0,1},a -> 2*a)))
  Text
    The output $(\mathrm{true},\mathrm{false})$ says that the fundamental spin weight $(0,0,1)$ is minuscule, but its double $(0,0,2)$ is not. Dominance alone is not sufficient.
SeeAlso
  minusculeNodes
  minusculeCrystal
  "Finite structural invariants"
///

doc ///
Key
  isCominusculeWeight
  (isCominusculeWeight,CartanDatum,List)
  (isCominusculeWeight,CartanDatum,Matrix)
  (isCominusculeWeight,KMWeight)
  (isCominusculeWeight,Matrix,List)
  (isCominusculeWeight,Matrix,Matrix)
  (isCominusculeWeight,Realization,List)
  (isCominusculeWeight,Realization,Matrix)
  (isCominusculeWeight,RootDatum,KMWeight)
  (isCominusculeWeight,RootDatum,List)
  (isCominusculeWeight,RootDatum,Matrix)
  (isCominusculeWeight,String,List)
  (isCominusculeWeight,String,Matrix)
Headline
  Test the cominuscule criterion for dominant coweight labels
Usage
  b = isCominusculeWeight lambda
  b = isCominusculeWeight(D,a)
Inputs
  lambda:KMWeight
    a dominant integral weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    nonnegative integral Dynkin labels
Outputs
  b:Boolean
    whether the corresponding criterion holds
Description
  Text
    For dominant Dynkin labels $(a_i)$, the command considers the coweight $\eta=\sum_i a_i\omega_i^\vee$ and tests $\langle\beta,\eta\rangle\in\{0,1\}$ for every positive root $\beta$. It is the coweight criterion, not another name for a minuscule representation. The zero weight satisfies the criterion; on products the test is performed in every simple component. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "B3";
    (isCominusculeWeight kmWeight(D,{1,0,0}),isCominusculeWeight kmWeight(D,apply({1,0,0},a -> 2*a)))
  Text
    The output $(\mathrm{true},\mathrm{false})$ says that the fundamental coweight with labels $(1,0,0)$ is cominuscule, while its double fails the positive-root evaluation criterion.
SeeAlso
  cominusculeNodes
  isMinusculeWeight
  "Finite structural invariants"
///

doc ///
Key
  quadraticCasimir
  (quadraticCasimir,CartanDatum,List)
  (quadraticCasimir,CartanDatum,Matrix)
  (quadraticCasimir,KMWeight)
  (quadraticCasimir,Matrix,List)
  (quadraticCasimir,Matrix,Matrix)
  (quadraticCasimir,Realization,List)
  (quadraticCasimir,Realization,Matrix)
  (quadraticCasimir,RootDatum,KMWeight)
  (quadraticCasimir,RootDatum,List)
  (quadraticCasimir,RootDatum,Matrix)
  (quadraticCasimir,String,List)
  (quadraticCasimir,String,Matrix)
Headline
  Compute the quadratic Casimir eigenvalue
Usage
  c = quadraticCasimir lambda
  c = quadraticCasimir(D,a)
Inputs
  lambda:KMWeight
    a dominant integral highest weight in finite type
  D:Thing
    a finite Cartan matrix, Cartan datum, realization, root datum, or type name
  a:List
    nonnegative integral Dynkin labels
Outputs
  c:QQ
    the rational eigenvalue (lambda,lambda+2 rho)
Description
  Text
    Long roots have squared norm 2 on each simple component, and $\rho$ has all Dynkin labels equal to 1. With this convention the adjoint eigenvalue is $2h^\vee$. On a product the eigenvalues add. Additional central directions are not assigned an invariant form, so the value is the Casimir of the derived semisimple algebra. Inputs are Dynkin labels, not marks. One may instead supply a finite-type matrix and a list of Dynkin labels, or a Cartan datum, realization, root datum, or type name followed by the labels. List or one-row matrix input is accepted with a Cartan matrix; the datum forms also accept one-column matrices. A complete typed weight preserves any additional central coordinates.
  Example
    D = rootDatum "A2";
    (quadraticCasimir kmWeight(D,{1,0}),quadraticCasimir kmWeight(D,{1,1}),2*dualCoxeterNumber datumCartanMatrix D)
  Text
    The values are $(8/3,6,6)$. The standard representation has eigenvalue $8/3$, while the adjoint highest weight $(1,1)$ has eigenvalue $6=2h^\vee$ since $h^\vee=3$ for $A_2$.
SeeAlso
  normalizedInvariantForm
  dualCoxeterNumber
  marks
  "Finite structural invariants"
///

doc ///
Key
  affineTypeData
  (affineTypeData,CartanDatum)
  (affineTypeData,Matrix)
  (affineTypeData,Realization)
  (affineTypeData,RootDatum)
Headline
  Identify affine type and its distinguished nodes
Usage
  T = affineTypeData D
Inputs
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
Outputs
  T:HashTable
    affine type, twist order, node information, marks, comarks, and finite part
Description
  Text
    The affine type is recognized up to simultaneous row and column permutation. The table distinguishes the number of nodes, Cartan-matrix rank, finite rank, and corank. Named data also retain labels and position-label tables. Low-rank coincidences are listed in "typeAliases" rather than forced into a uniquely intrinsic name. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    D = rootDatum cartanAffineA 1;
    T = affineTypeData D;
    (T#"type",T#"twistOrder",T#"nodeCount",T#"cartanMatrixRank",T#"finiteRank",T#"corank",T#"standardAffineNode")
  Text
    The affine type is $A_1^{(1)}$ with twist order 1, two nodes, Cartan-matrix rank 1, finite rank 1, and corank 1. The standard affine node occupies position 1. Its conventional mathematical label is 0; the two conventions refer to the same node.
SeeAlso
  affineFinitePart
  affineMarks
  dynkinDiagram
  "Affine structural data"
///

doc ///
Key
  affineMarks
  (affineMarks,CartanDatum)
  (affineMarks,Matrix)
  (affineMarks,Realization)
  (affineMarks,RootDatum)
Headline
  Compute the primitive null-root coefficients
Usage
  a = affineMarks D
Inputs
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
Outputs
  a:List
    primitive positive integral null-root coefficients
Description
  Text
    For an indecomposable affine matrix, the right kernel has a unique primitive positive integral generator. It satisfies $Aa=0$ and $\delta=\sum_i a_i\alpha_i$. The entries follow the input matrix order, including after a permutation of nodes.
  Example
    A = affineCartanA2Twisted 1
    a = affineMarks A;
    (a,A * transpose matrix{a})
  Text
    The list $(1,2)$ gives $\delta=\alpha_0+2\alpha_1$ in type $A_2^{(2)}$. The zero column verifies $Aa=0$. It consists of simple-root coefficients, not Dynkin labels.
SeeAlso
  affineComarks
  affineTypeData
  "Affine structural data"
///

doc ///
Key
  affineComarks
  (affineComarks,CartanDatum)
  (affineComarks,Matrix)
  (affineComarks,Realization)
  (affineComarks,RootDatum)
Headline
  Compute the primitive central-coroot coefficients
Usage
  a = affineComarks D
Inputs
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
Outputs
  a:List
    primitive positive integral central-coroot coefficients
Description
  Text
    For an indecomposable affine matrix, the left kernel has a unique primitive positive integral generator. It satisfies $A^{\mathsf T}a^\vee=0$ and $c=\sum_i a_i^\vee\alpha_i^\vee$. The entries follow the input matrix order, including after a permutation of nodes.
  Example
    A = affineCartanA2Twisted 1
    a = affineComarks A;
    (a,transpose A * transpose matrix{a})
  Text
    The list $(2,1)$ gives $c=2\alpha_0^\vee+\alpha_1^\vee$ in type $A_2^{(2)}$. The zero column verifies $A^{\mathsf T}a^\vee=0$. These comarks differ from the null-root coefficients $(1,2)$.
SeeAlso
  affineMarks
  affineTypeData
  "Affine structural data"
///

doc ///
Key
  affineCentralCoefficients
  (affineCentralCoefficients,CartanDatum)
  (affineCentralCoefficients,Matrix)
  (affineCentralCoefficients,Realization)
  (affineCentralCoefficients,RootDatum)
Headline
  Return the central coroot as a coefficient list
Usage
  a = affineCentralCoefficients D
Inputs
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
Outputs
  a:List
    primitive positive integral central-coroot coefficients
Description
  Text
    For an indecomposable affine matrix, the left kernel has a unique primitive positive integral generator. It satisfies $A^{\mathsf T}a^\vee=0$ and $c=\sum_i a_i^\vee\alpha_i^\vee$. The entries follow the input matrix order, including after a permutation of nodes. This is the same coefficient list as @TO affineComarks@.
  Example
    A = affineCartanA2Twisted 1
    a = affineCentralCoefficients A;
    (a,transpose A * transpose matrix{a})
  Text
    The list $(2,1)$ gives $c=2\alpha_0^\vee+\alpha_1^\vee$ in type $A_2^{(2)}$. The zero column verifies $A^{\mathsf T}a^\vee=0$. These comarks differ from the null-root coefficients $(1,2)$.
SeeAlso
  affineCentralElement
  affineTypeData
  "Affine structural data"
///

doc ///
Key
  affineNullRoot
  (affineNullRoot,CartanDatum)
  (affineNullRoot,Matrix)
  (affineNullRoot,Realization)
  (affineNullRoot,RootDatum)
Headline
  Construct the affine null root
Usage
  delta = affineNullRoot D
Inputs
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
Outputs
  delta:Thing
    a KMRoot for root-datum input, otherwise its simple-root coefficient list
Description
  Text
    The null root is $\delta=\sum_i a_i\alpha_i$, with primitive positive marks satisfying $Aa=0$. In a root datum it is a nonzero element of the realization even though every Dynkin label is zero. A matrix, Cartan datum, or realization input returns its coefficient list.
  Example
    D = rootDatum cartanAffineA 1;
    delta = affineNullRoot D;
    (datumCoordinates delta,datumCoordinates toWeight delta,realizationCoordinates delta)
  Text
    The output $((1,1),(0,0),(0,0,1))$ means $\delta=\alpha_0+\alpha_1$. Its labels vanish, but its complete realization vector is nonzero. Thus the command retains the essential affine direction.
SeeAlso
  affineMarks
  toWeight
  realizationCoordinates
  "Affine structural data"
///

doc ///
Key
  affineCentralElement
  (affineCentralElement,RootDatum)
Headline
  Construct the canonical affine central coroot
Usage
  c = affineCentralElement D
Inputs
  D:RootDatum
    an indecomposable affine root datum
Outputs
  c:KMCoroot
    the primitive positive central coroot
Description
  Text
    The element is $c=\sum_i a_i^\vee\alpha_i^\vee$, with primitive positive comarks satisfying $A^{\mathsf T}a^\vee=0$. A specified root datum is required so that the central element retains its realization. For a coefficient list alone, use @TO affineCentralCoefficients@.
  Example
    D = rootDatum affineCartanA2Twisted 1;
    c = affineCentralElement D;
    (datumCoordinates c,pairing(kmWeight(D,{1,0}),c),pairing(kmWeight(D,{0,1}),c))
  Text
    The coefficients $(2,1)$ give $c=2\alpha_0^\vee+\alpha_1^\vee$. The next entries 2 and 1 are the levels of the two fundamental Dynkin-label classes, obtained by pairing them with this central coroot.
SeeAlso
  affineComarks
  affineLevel
  pairing
  "Affine structural data"
///

doc ///
Key
  affineFinitePart
  (affineFinitePart,CartanDatum)
  (affineFinitePart,CartanDatum,Thing)
  (affineFinitePart,CartanDatum,ZZ)
  (affineFinitePart,Matrix)
  (affineFinitePart,Matrix,ZZ)
  (affineFinitePart,Realization)
  (affineFinitePart,Realization,Thing)
  (affineFinitePart,Realization,ZZ)
  (affineFinitePart,RootDatum)
  (affineFinitePart,RootDatum,Thing)
  (affineFinitePart,RootDatum,ZZ)
Headline
  Delete an affine node and describe the resulting finite datum
Usage
  F = affineFinitePart D
  F = affineFinitePart(D,i)
Inputs
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
  i:Thing
    an optional node to delete
Outputs
  F:HashTable
    retained ambient positions, finite Cartan matrix, and corresponding finite datum
Description
  Text
    With no node argument the recognized standard affine node is deleted. An explicit node position, label, or position-label table selects a different deletion; a matrix takes only a position. The finite datum has the standard finite realization, not an asserted embedded subspace of the affine realization. For root-datum input its global form is unspecified, because a Cartan matrix alone does not determine the induced finite character lattice. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    D = rootDatum cartanAffineA 1;
    F = affineFinitePart D;
    (F#"nodes",F#"cartanMatrix",F#"finiteRootDatum")
  Text
    The retained ambient node is position 2, and its finite Cartan matrix is $(2)$. The resulting finite root datum therefore has type $A_1$. This is the finite algebra $\mathfrak{sl}_2$ underlying the untwisted affine algebra.
SeeAlso
  affineClassicalPart
  affineTypeData
  "Affine structural data"
///

doc ///
Key
  affineClassicalPart
  (affineClassicalPart,KMWeight)
  (affineClassicalPart,KMWeight,Thing)
  (affineClassicalPart,KMWeight,ZZ)
  (affineClassicalPart,Matrix,List)
  (affineClassicalPart,Matrix,List,ZZ)
  (affineClassicalPart,Matrix,Matrix)
  (affineClassicalPart,Matrix,Matrix,ZZ)
Headline
  Restrict an affine weight to a finite node deletion
Usage
  F = affineClassicalPart lambda
  F = affineClassicalPart(lambda,i)
  F = affineClassicalPart(A,a,i)
Inputs
  lambda:KMWeight
    an affine weight or Dynkin-label class
  A:Matrix
    an indecomposable affine Cartan matrix
  a:List
    affine Dynkin labels, also accepted as a row or column matrix
  i:Thing
    an optional deleted node; matrix input uses a position
Outputs
  F:HashTable
    the finite-part data and restricted Dynkin labels, including a finiteWeight for typed input
Description
  Text
    The retained labels are the evaluations on the finite simple coroots. This forgets the affine central and degree directions rather than identifying them with finite weights. If no node is specified, the recognized standard affine node is used. For typed input a label or position-label table may choose the deletion. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    D = rootDatum cartanAffineA 1;
    lambda = kmWeight(D,{2,3})
    F = affineClassicalPart lambda;
    (F#"nodes",datumCoordinates F#"finiteWeight",affineLevel lambda)
  Text
    The retained node is position 2 and the finite $A_1$ highest weight has label 3. The affine weight has level $2+3=5$, which is separate information and is not part of the finite label 3.
SeeAlso
  affineFinitePart
  affineLevel
  "Affine structural data"
///

doc ///
Key
  affineLevel
  (affineLevel,CartanDatum,List)
  (affineLevel,CartanDatum,Matrix)
  (affineLevel,KMRoot)
  (affineLevel,KMWeight)
  (affineLevel,Matrix,List)
  (affineLevel,Matrix,Matrix)
  (affineLevel,Realization,List)
  (affineLevel,Realization,Matrix)
  (affineLevel,RootDatum,List)
  (affineLevel,RootDatum,Matrix)
Headline
  Compute the eigenvalue of the affine central element
Usage
  k = affineLevel lambda
  k = affineLevel(D,a)
Inputs
  lambda:Thing
    an affine KMWeight or KMRoot
  D:Thing
    an indecomposable affine Cartan matrix, Cartan datum, realization, or root datum
  a:List
    rational Dynkin labels, also accepted as a row or column matrix
Outputs
  k:QQ
    the level lambda(c)
Description
  Text
    If $a_i^\vee$ are the affine comarks, then $\operatorname{level}(\lambda)=\sum_i a_i^\vee\langle\lambda,\alpha_i^\vee\rangle$. Consequently labels alone determine level; dominance and a degree are not required. Every root has level zero. This calculation does not assert integrability.
  Example
    A = affineCartanA2Twisted 1
    (affineLevel(A,{1,0}),affineLevel(A,{0,1}),affineLevel(A,{3,-2}))
  Text
    The levels are $(2,1,4)$ because the comarks are $(2,1)$: the last value is $2\cdot3-2=4$. Using the marks $(1,2)$ instead would compute the wrong central pairing.
SeeAlso
  affineCentralElement
  affineComarks
  "Affine structural data"
///

doc ///
Key
  affineDerivation
  (affineDerivation,RootDatum)
  (affineDerivation,RootDatum,Thing)
  (affineDerivation,Thing)
Headline
  Choose a complete normalized affine derivation
Usage
  d = affineDerivation D
  d = affineDerivation(D,i)
Inputs
  D:RootDatum
    an indecomposable affine root datum in a minimal realization
  i:Thing
    an optional selected node position, label, or position-label table
Outputs
  d:KMCoweight
    a complete normalized derivation
Description
  Text
    For a selected node $i$ with mark $a_i$, this chooses the realization-dependent row-space representative with $\alpha_i(d)=1/a_i$ and all other simple-root evaluations zero. Hence $\delta(d)=1$. With no node argument it uses the recognized standard affine node. A Cartan matrix without a realization does not specify this vector. Nonminimal realizations require additional choices and are not accepted by this constructor. Node arguments are positions numbered starting at 1, chosen node labels, or position-label tables. An integer between 1 and the number of nodes always means a position. Coordinate tuples follow the matrix order, so the conventional affine node 0 occupies position 1 in the canonical affine matrices.
  Example
    D = rootDatum cartanAffineA 1;
    d = affineDerivation D;
    (datumCoordinates d,realizationCoordinates d,pairing(affineNullRoot D,d))
  Text
    The output $((1,0),(0,0,1),1)$ says $\alpha_0(d)=1$, $\alpha_1(d)=0$, and $\delta(d)=1$. The complete derivation lies in the dual realization space; its evaluations $(1,0)$ are not its realization coordinates.
SeeAlso
  affineDegree
  affineWeightFromDynkinLabels
  "Affine structural data"
///

doc ///
Key
  affineDegree
  (affineDegree,KMRoot,KMCoweight)
  (affineDegree,KMWeight,KMCoweight)
  (affineDegree,Matrix,List)
  (affineDegree,Matrix,Matrix)
  (affineDegree,Thing,Thing)
Headline
  Evaluate a complete affine weight on a normalized derivation
Usage
  q = affineDegree(lambda,d)
Inputs
  lambda:Thing
    a complete affine weight or root
  d:KMCoweight
    a complete derivation in the same datum with delta(d)=1
Outputs
  q:QQ
    the value lambda(d)
Description
  Text
    The degree is the pairing with the supplied normalized derivation. It
    cannot be recovered from Dynkin labels alone. This value may be negative.
    For the standard derivation, or a selected-node derivation, the
    nonnegative grade below a highest weight $\Lambda$ is instead
    $\Lambda(d)-\lambda(d)$. For an arbitrary normalized derivation this is
    a degree difference, not necessarily nonnegative: $\delta(d)=1$ alone
    does not force every $\alpha_i(d)$ to be nonnegative. Changing $d$ to
    $d+c$ shifts a weight's degree by its level. Matrix-and-label input is
    not sufficient.
  Example
    D = rootDatum cartanAffineA 1;
    d = affineDerivation D
    lambda = affineWeightFromDynkinLabels(D,{1,0},-2)
    (affineDegree(lambda,d),affineLevel lambda,affineDegree(affineNullRoot D,d))
  Text
    The output $(-2,1,1)$ means that this weight has derivation eigenvalue $-2$ and level 1, while $\delta(d)=1$. It is $\Lambda_0-2\delta$ in the standard realization, so its grade below degree-zero $\Lambda_0$ is 2.
SeeAlso
  affineDerivation
  affineLevel
  affineWeightFromDynkinLabels
  "Affine structural data"
///

doc ///
Key
  affineFullWeight
  (affineFullWeight,Matrix,List)
  (affineFullWeight,Matrix,Matrix)
  (affineFullWeight,RootDatum,List)
  (affineFullWeight,RootDatum,Matrix)
Headline
  Construct an affine weight from every realization coordinate
Usage
  lambda = affineFullWeight(D,x)
Inputs
  D:RootDatum
    an indecomposable affine root datum with a specified realization
  x:List
    a rational complete realization vector, also accepted as a row or column matrix
Outputs
  lambda:KMWeight
    the complete affine weight
Description
  Text
    The input has one entry per Cartan-space dimension, not one per affine node. This is the affine form of @TO weightFromRealizationCoordinates@ and allows nonminimal realizations. The constructor does not assert dominance, lattice integrality, or integrability. A Cartan matrix alone is insufficient to interpret these coordinates.
  Example
    D = rootDatum cartanAffineA 1;
    lambda = affineFullWeight(D,{1,0,-2});
    (datumCoordinates lambda,realizationCoordinates lambda)
  Text
    The output gives labels $(1,0)$ and complete vector $(1,0,-2)$. Thus $\lambda=\Lambda_0-2\delta$: the last coordinate records the affine direction that the two labels alone would omit.
SeeAlso
  weightFromRealizationCoordinates
  affineWeightFromDynkinLabels
  "Affine structural data"
///

doc ///
Key
  affineWeightFromDynkinLabels
  (affineWeightFromDynkinLabels,RootDatum,List,Thing)
  (affineWeightFromDynkinLabels,RootDatum,List,Thing,KMCoweight)
  (affineWeightFromDynkinLabels,RootDatum,Matrix,Thing)
  (affineWeightFromDynkinLabels,RootDatum,Matrix,Thing,KMCoweight)
  (affineWeightFromDynkinLabels,Thing,Thing,Thing)
  (affineWeightFromDynkinLabels,Thing,Thing,Thing,Thing)
Headline
  Complete an affine weight by prescribing its degree
Usage
  lambda = affineWeightFromDynkinLabels(D,a,q)
  lambda = affineWeightFromDynkinLabels(D,a,q,d)
Inputs
  D:RootDatum
    an indecomposable affine root datum in a minimal realization
  a:List
    rational Dynkin labels, also accepted as a row or column matrix
  q:QQ
    the desired value lambda(d)
  d:KMCoweight
    an optional complete normalized derivation in the same datum
Outputs
  lambda:KMWeight
    the unique complete weight with these labels and degree
Description
  Text
    In a minimal affine realization, Dynkin labels leave exactly one weight direction undetermined. Prescribing its value on a normalized derivation with $\delta(d)=1$ determines that direction. Without an explicit derivation the command uses @TO affineDerivation@. Nonminimal realizations need further coordinates and must instead use @TO affineFullWeight@. No dominance or integrability is asserted.
  Example
    D = rootDatum cartanAffineA 1;
    d = affineDerivation D;
    lambda = affineWeightFromDynkinLabels(D,{1,0},-2,d);
    (realizationCoordinates lambda,affineDegree(lambda,d),affineLevel lambda)
  Text
    The complete vector $(1,0,-2)$ has the prescribed degree $-2$ and level 1. It represents $\Lambda_0-2\delta$, which has the same Dynkin labels as $\Lambda_0$ but is a different weight.
SeeAlso
  affineFullWeight
  affineDegree
  affineDerivation
  "Affine structural data"
///

doc ///
Key
  affineCartanA2Twisted
Headline
  Construct a Cartan matrix of type A_(2n)^(2)
Usage
  A = affineCartanA2Twisted n
Inputs
  n:ZZ
    a positive integer
Outputs
  A:Matrix
    the canonical twisted affine generalized Cartan matrix over ZZ
Description
  Text
    The standard affine node is in position 1, conventionally called node 0. Entries use $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$. The parameter $n$ gives $n+1$ affine nodes. The matrix has corank 1 and independent simple roots become available through a minimal realization.
  Example
    A = affineCartanA2Twisted 1;
    (A,numrows A,rank A,(affineTypeData A)#"twistOrder")
  Text
    The matrix is of type $A_2^{(2)}$. It has 2 simple-root nodes, matrix rank 1, and twist order 2. The difference between node count and matrix rank is the one-dimensional kernel containing the null-root coefficients.
SeeAlso
  affineTypeData
  affineMarks
  rootDatum
  "Affine structural data"
///

doc ///
Key
  affineCartanAOddTwisted
Headline
  Construct a Cartan matrix of type A_(2n-1)^(2)
Usage
  A = affineCartanAOddTwisted n
Inputs
  n:ZZ
    an integer at least 2
Outputs
  A:Matrix
    the canonical twisted affine generalized Cartan matrix over ZZ
Description
  Text
    The standard affine node is in position 1, conventionally called node 0. Entries use $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$. The parameter $n$ gives $n+1$ affine nodes. The matrix has corank 1 and independent simple roots become available through a minimal realization.
  Example
    A = affineCartanAOddTwisted 3;
    (A,numrows A,rank A,(affineTypeData A)#"twistOrder")
  Text
    The matrix is of type $A_5^{(2)}$. It has 4 simple-root nodes, matrix rank 3, and twist order 2. The difference between node count and matrix rank is the one-dimensional kernel containing the null-root coefficients.
SeeAlso
  affineTypeData
  affineMarks
  rootDatum
  "Affine structural data"
///

doc ///
Key
  affineCartanDTwisted
Headline
  Construct a Cartan matrix of type D_(n+1)^(2)
Usage
  A = affineCartanDTwisted n
Inputs
  n:ZZ
    an integer at least 2
Outputs
  A:Matrix
    the canonical twisted affine generalized Cartan matrix over ZZ
Description
  Text
    The standard affine node is in position 1, conventionally called node 0. Entries use $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$. The parameter $n$ gives $n+1$ affine nodes. The matrix has corank 1 and independent simple roots become available through a minimal realization.
  Example
    A = affineCartanDTwisted 3;
    (A,numrows A,rank A,(affineTypeData A)#"twistOrder")
  Text
    The matrix is of type $D_4^{(2)}$. It has 4 simple-root nodes, matrix rank 3, and twist order 2. The difference between node count and matrix rank is the one-dimensional kernel containing the null-root coefficients.
SeeAlso
  affineTypeData
  affineMarks
  rootDatum
  "Affine structural data"
///

doc ///
Key
  affineCartanE6Twisted
Headline
  Construct the twisted affine Cartan matrix of type E6^(2)
Usage
  A = affineCartanE6Twisted()
Outputs
  A:Matrix
    the canonical twisted affine generalized Cartan matrix over ZZ
Description
  Text
    This command takes no arguments. The standard affine node is in position 1, conventionally called node 0. Entries use $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$. The matrix has corank 1 and independent simple roots become available through a minimal realization.
  Example
    A = affineCartanE6Twisted();
    (A,numrows A,rank A,(affineTypeData A)#"twistOrder")
  Text
    The matrix is of type $E_6^{(2)}$. It has 5 simple-root nodes, matrix rank 4, and twist order 2. The difference between node count and matrix rank is the one-dimensional kernel containing the null-root coefficients.
SeeAlso
  affineTypeData
  affineMarks
  rootDatum
  "Affine structural data"
///

doc ///
Key
  affineCartanD4Twisted
Headline
  Construct the twisted affine Cartan matrix of type D4^(3)
Usage
  A = affineCartanD4Twisted()
Outputs
  A:Matrix
    the canonical twisted affine generalized Cartan matrix over ZZ
Description
  Text
    This command takes no arguments. The standard affine node is in position 1, conventionally called node 0. Entries use $a_{ij}=\langle\alpha_j,\alpha_i^\vee\rangle$. The matrix has corank 1 and independent simple roots become available through a minimal realization.
  Example
    A = affineCartanD4Twisted();
    (A,numrows A,rank A,(affineTypeData A)#"twistOrder")
  Text
    The matrix is of type $D_4^{(3)}$. It has 3 simple-root nodes, matrix rank 2, and twist order 3. The difference between node count and matrix rank is the one-dimensional kernel containing the null-root coefficients.
SeeAlso
  affineTypeData
  affineMarks
  rootDatum
  "Affine structural data"
///

doc ///
Key
  affineStructuralCapabilities
Headline
  Describe the data needed for affine structural calculations
Usage
  T = affineStructuralCapabilities()
Outputs
  T:HashTable
    lists of calculations determined by a Cartan matrix or by additional realization data
Description
  Text
    This command takes no arguments. It separates facts determined by the affine Cartan matrix, such as marks and level, from quantities requiring a realization and complete vectors, such as a derivation and its degree pairing. It performs no root enumeration or representation calculation.
  Example
    T = affineStructuralCapabilities();
    (T#"requiresExplicitCompleteWeightAndDerivation",T#"notDeterminedByDynkinLabelsAlone")
  Text
    The first list contains affineDegree. The second also includes translation by the null root: two weights with the same Dynkin labels can differ by a multiple of $\delta$, and hence have different degrees. These lists state what mathematical data must be supplied, rather than return weights or multiplicities.
SeeAlso
  affineDegree
  affineFullWeight
  affineTypeData
  "Affine structural data"
///

doc ///
Key
  integratesToGlobalForm
  (integratesToGlobalForm,KMWeight)
Headline
  Test membership in the chosen character lattice
Usage
  b = integratesToGlobalForm lambda
Inputs
  lambda:KMWeight
    the element whose integrality is to be tested
Outputs
  b:Thing
    true or false when membership is determined, and null otherwise
Description
  Text
    The selected root datum specifies the character lattice. The test is membership in that lattice, including all realization directions. It returns null for an unspecified global form or an incomplete vector. It does not test dominance or the existence of a Lie-algebra representation.
  Example
    D = rootDatum("A1",GlobalForm=>"Adjoint");
    (integratesToGlobalForm kmWeight(D,{1}),integratesToGlobalForm kmWeight(D,{2}))
  Text
    The output $(\mathrm{false},\mathrm{true})$ says that the adjoint $A_1$ character lattice consists of even Dynkin labels. Highest weight 1 does not descend to $PGL_2$, whereas highest weight 2 does.
SeeAlso
  rootDatum
  characterLatticeBasis
  "Roots, weights, and pairings"
///

doc ///
Key
  integratesCoweightToGlobalForm
  (integratesCoweightToGlobalForm,KMCoweight)
Headline
  Test membership in the chosen cocharacter lattice
Usage
  b = integratesCoweightToGlobalForm eta
Inputs
  eta:KMCoweight
    the element whose integrality is to be tested
Outputs
  b:Thing
    true or false when membership is determined, and null otherwise
Description
  Text
    The selected root datum specifies the cocharacter lattice. The test is membership in that lattice, including all realization directions. It returns null for an unspecified global form or an incomplete vector. It does not test dominance or the existence of a Lie-algebra representation.
  Example
    D = rootDatum("A1",GlobalForm=>"SimplyConnected");
    (integratesCoweightToGlobalForm kmCoweight(D,{1}),integratesCoweightToGlobalForm kmCoweight(D,{2}))
  Text
    The output $(\mathrm{false},\mathrm{true})$ says that for simply connected $A_1$ the cocharacter lattice is the coroot lattice. The fundamental coweight with evaluation 1 is not in it, whereas the simple coroot with evaluation 2 is.
SeeAlso
  rootDatum
  cocharacterLatticeBasis
  "Roots, weights, and pairings"
///

doc ///
Key
  toLegacyList
  (toLegacyList,KMCoroot)
  (toLegacyList,KMCoweight)
  (toLegacyList,KMRoot)
  (toLegacyList,KMWeight)
Headline
  Return the coordinate list without its root datum
Usage
  a = toLegacyList x
Inputs
  x:Thing
    a root, coroot, weight, or coweight
Outputs
  a:List
    the coordinates given by datumCoordinates
Description
  Text
    This returns simple-root coefficients for a root, simple-coroot
    coefficients for a coroot, Dynkin labels for a weight, or simple-root
    evaluations for a coweight. The list has no parent datum. For complete
    weights or coweights it omits any supplementary realization directions
    invisible to these evaluations, including in nonminimal finite
    realizations as well as singular type. Use @TO realizationCoordinates@
    when those directions matter.
  Example
    D = rootDatum cartanAffineA 1;
    lambda = weightFromRealizationCoordinates(D,{1,0,-1})
    (toLegacyList lambda,realizationCoordinates lambda)
  Text
    The legacy list is $(1,0)$, while the complete vector is $(1,0,-1)$. The shorter list does not distinguish $\Lambda_0-\delta$ from $\Lambda_0$ and therefore cannot preserve this affine weight by itself.
SeeAlso
  datumCoordinates
  realizationCoordinates
  "Roots, weights, and pairings"
///

doc ///
Key
  toLegacyCartanMatrix
  (toLegacyCartanMatrix,CartanDatum)
  (toLegacyCartanMatrix,Realization)
  (toLegacyCartanMatrix,RootDatum)
Headline
  Return a datum as an ordinary Cartan matrix
Usage
  A = toLegacyCartanMatrix D
Inputs
  D:Thing
    a Cartan datum, realization, or root datum
Outputs
  A:Matrix
    the generalized Cartan matrix in its original node order
Description
  Text
    This agrees with @TO datumCartanMatrix@. The output contains the Cartan pairing but does not include chosen node labels, a realization, or character lattices.
  Example
    D = rootDatum "A2";
    toLegacyCartanMatrix D
  Text
    The matrix is $\begin{pmatrix}2&-1\\-1&2\end{pmatrix}$, the simple-root and coroot pairing matrix of $A_2$. The root datum is no longer part of this ordinary matrix output.
SeeAlso
  datumCartanMatrix
  datumFromLegacyCartan
  "Roots, weights, and pairings"
///

doc ///
Key
  datumFromLegacyCartan
  (datumFromLegacyCartan,Matrix)
Headline
  Construct a Cartan datum from an ordinary matrix
Usage
  D = datumFromLegacyCartan A
Inputs
  A:Matrix
    a symmetrizable generalized Cartan matrix
Outputs
  D:CartanDatum
    its Cartan datum with the default node labels
Description
  Text
    This is the matrix form of @TO cartanDatum@. The matrix order is retained, and the default node positions are used as labels.
  Example
    D = datumFromLegacyCartan cartanA 3;
    (D#"nodeLabels",datumCartanMatrix D == cartanA 3)
  Text
    The labels $(1,2)$ name the two simple roots of $A_2$, and true confirms that the original Cartan pairing matrix was retained.
SeeAlso
  cartanDatum
  rootDatum
  "Roots, weights, and pairings"
///

doc ///
Key
  rootFromLegacy
  (rootFromLegacy,CartanDatum,List)
  (rootFromLegacy,Matrix,List)
  (rootFromLegacy,RootDatum,List)
Headline
  Attach a datum to integral simple-root coefficients
Usage
  alpha = rootFromLegacy(D,a)
Inputs
  D:Thing
    a root datum, Cartan datum, or Cartan matrix
  a:List
    integral simple-root coefficients in matrix order
Outputs
  alpha:KMRoot
    the corresponding element with its root datum
Description
  Text
    This has the same coordinate convention as @TO kmRoot@. Supplying an existing root datum retains its realization and lattice choices. Dynkin labels or simple-root evaluations alone do not specify extra Cartan directions.
  Example
    D = rootDatum "A2";
    alpha = rootFromLegacy(D,{1,1});
    (alpha,parentDatum alpha == D)
  Text
    The tuple $(1,1)$ represents $\alpha_1+\alpha_2$ in type $A_2$. The final true confirms that the coordinate list is now attached to the specified root datum.
SeeAlso
  kmRoot
  toLegacyList
  "Roots, weights, and pairings"
///

doc ///
Key
  corootFromLegacy
  (corootFromLegacy,CartanDatum,List)
  (corootFromLegacy,Matrix,List)
  (corootFromLegacy,RootDatum,List)
Headline
  Attach a datum to integral simple-coroot coefficients
Usage
  h = corootFromLegacy(D,a)
Inputs
  D:Thing
    a root datum, Cartan datum, or Cartan matrix
  a:List
    integral simple-coroot coefficients in matrix order
Outputs
  h:KMCoroot
    the corresponding element with its root datum
Description
  Text
    This has the same coordinate convention as @TO kmCoroot@. Supplying an existing root datum retains its realization and lattice choices. Dynkin labels or simple-root evaluations alone do not specify extra Cartan directions.
  Example
    D = rootDatum "A2";
    h = corootFromLegacy(D,{1,0});
    (h,parentDatum h == D)
  Text
    The tuple $(1,0)$ represents $\alpha_1^\vee$ in type $A_2$. The final true confirms that the coordinate list is now attached to the specified root datum.
SeeAlso
  kmCoroot
  toLegacyList
  "Roots, weights, and pairings"
///

doc ///
Key
  weightFromLegacy
  (weightFromLegacy,CartanDatum,List)
  (weightFromLegacy,Matrix,List)
  (weightFromLegacy,RootDatum,List)
Headline
  Attach a datum to rational Dynkin labels
Usage
  lambda = weightFromLegacy(D,a)
Inputs
  D:Thing
    a root datum, Cartan datum, or Cartan matrix
  a:List
    rational Dynkin labels in matrix order
Outputs
  lambda:KMWeight
    the corresponding element with its root datum
Description
  Text
    This has the same coordinate convention as @TO kmWeight@. Supplying an existing root datum retains its realization and lattice choices. Dynkin labels or simple-root evaluations alone do not specify extra Cartan directions.
  Example
    D = rootDatum "A2";
    lambda = weightFromLegacy(D,{1,0});
    (lambda,parentDatum lambda == D)
  Text
    The tuple $(1,0)$ represents $\omega_1$ in type $A_2$. The final true confirms that the coordinate list is now attached to the specified root datum.
SeeAlso
  kmWeight
  toLegacyList
  "Roots, weights, and pairings"
///

doc ///
Key
  coweightFromLegacy
  (coweightFromLegacy,CartanDatum,List)
  (coweightFromLegacy,Matrix,List)
  (coweightFromLegacy,RootDatum,List)
Headline
  Attach a datum to rational simple-root evaluations
Usage
  eta = coweightFromLegacy(D,a)
Inputs
  D:Thing
    a root datum, Cartan datum, or Cartan matrix
  a:List
    rational simple-root evaluations in matrix order
Outputs
  eta:KMCoweight
    the corresponding element with its root datum
Description
  Text
    This has the same coordinate convention as @TO kmCoweight@. Supplying an existing root datum retains its realization and lattice choices. Dynkin labels or simple-root evaluations alone do not specify extra Cartan directions.
  Example
    D = rootDatum "A2";
    eta = coweightFromLegacy(D,{0,1});
    (eta,parentDatum eta == D)
  Text
    The tuple $(0,1)$ represents $\omega_2^\vee$ in type $A_2$. The final true confirms that the coordinate list is now attached to the specified root datum.
SeeAlso
  kmCoweight
  toLegacyList
  "Roots, weights, and pairings"
///

doc ///
Key
  formalCharacter
  (formalCharacter,RootDatum,HashTable)
  (formalCharacter,RootDatum,Tally)
  [formalCharacter,CharacterStatus]
  [formalCharacter,Truncation]
Headline
  Construct a finite sum of weight exponentials
Usage
  C = formalCharacter(D,T)
Inputs
  D:RootDatum
    the root datum of all weights
  T:HashTable
    weights in D with integral coefficients; a Tally is also accepted
Outputs
  C:FormalCharacter
    the specified formal sum, exact unless explicitly marked partial
Description
  Text
    The table specifies the coefficient of each $e^\mu$. All keys must be
    weights in the same datum and use compatible coordinates. Coefficients may
    be negative, so virtual characters are allowed. A formal sum need not be
    Weyl invariant or be the character of a representation. The default is an
    exact finite sum; CharacterStatus and Truncation are for explicitly
    specified partial expansions, not estimates of uncomputed coefficients.
  Example
    D = rootDatum "A1";
    C = formalCharacter(D,new Tally from {kmWeight(D,{2})=>1,kmWeight(D,{0})=>1,kmWeight(D,{-2})=>1});
    C
    decomposeCharacter C
  Text
    The sum is $e^{(2)}+1+e^{(-2)}$. In type $A_1$, its three one-dimensional
    weight spaces form the irreducible representation $V(2)$, so the
    decomposition has the single highest weight $(2)$ with multiplicity $1$.
SeeAlso
  "Formal and completed characters"
  finiteCharacter
  characterTerms
  decomposeCharacter
///

doc ///
Key
  characterDatum
  (characterDatum,FormalCharacter)
  (characterDatum,LeviFormalCharacter)
Headline
  Recover the root datum of a character
Usage
  D = characterDatum C
Inputs
  C:Thing
    a FormalCharacter or LeviFormalCharacter
Outputs
  D:Thing
    its RootDatum or ReductiveLeviDatum
Description
  Text
    A character includes its ambient root datum, not just a table of tuples.
    For a restricted character this is the full reductive Levi datum,
    including the chosen coordinates on its center. This information
    determines which weight additions and pairings are meaningful.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    characterDatum C == D
  Text
    The value true confirms that the weights are weights for the original
    $A_2$ datum. Its character is the defining three-dimensional
    representation of the corresponding semisimple Lie algebra.
SeeAlso
  "Formal and completed characters"
  parentDatum
  restrictCharacter
///

doc ///
Key
  characterTerms
  (characterTerms,FormalCharacter)
  (characterTerms,LeviFormalCharacter)
Headline
  Recover the coefficients of weight exponentials
Usage
  T = characterTerms C
Inputs
  C:Thing
    a FormalCharacter or LeviFormalCharacter
Outputs
  T:HashTable
    weight keys with their integral coefficients
Description
  Text
    This table records the displayed terms of the character. An exact
    character has no other nonzero coefficients. For a partial character, the
    table is only its stored support; an absent key is not automatically a
    known zero. Use characterCoefficient to ask for a mathematically certified
    coefficient.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    characterTerms C
  Text
    The three keys $(1,0)$, $(-1,1)$, and $(0,-1)$ each have coefficient $1$.
    Thus the character is $e^{(1,0)}+e^{(-1,1)}+e^{(0,-1)}$, where the tuples
    are Dynkin labels.
SeeAlso
  "Formal and completed characters"
  characterCoefficient
  characterSupport
///

doc ///
Key
  characterSupport
  (characterSupport,FormalCharacter)
  (characterSupport,LeviFormalCharacter)
Headline
  List the weights displayed in a character
Usage
  S = characterSupport C
Inputs
  C:Thing
    a FormalCharacter or LeviFormalCharacter
Outputs
  S:List
    its stored weights with nonzero coefficients
Description
  Text
    The support consists of the weights whose exponentials have a nonzero
    displayed coefficient. In a partial expansion it is not the support of the
    entire, possibly infinite, character. Each weight retains its datum.
    For a @TO KMWeight@, @TO datumCoordinates@ gives its Dynkin labels.
    For a @TO LeviWeight@, use @TT "datumCoordinates semisimplePart mu"@
    and @TT "centralCharacter mu"@ to read the derived-Levi labels and
    the center evaluations separately.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    sort apply(characterSupport C,datumCoordinates)
  Text
    The result lists $(-1,1)$, $(0,-1)$, and $(1,0)$, exactly the three
    weights of $V(1,0)$. The sorting is only for a predictable display; it is
    not a choice of highest-weight ordering.
SeeAlso
  "Formal and completed characters"
  characterTerms
  characterCoefficient
///

doc ///
Key
  characterCoefficient
  (characterCoefficient,FormalCharacter,KMWeight)
  (characterCoefficient,LeviFormalCharacter,LeviWeight)
Headline
  Find a certified weight coefficient
Usage
  m = characterCoefficient(C,mu)
Inputs
  C:Thing
    a FormalCharacter or LeviFormalCharacter
  mu:Thing
    a KMWeight or LeviWeight in the character datum
Outputs
  m:ZZ
    the coefficient of the selected weight exponential
Description
  Text
    For an actual character the coefficient is the dimension of its weight
    space. For an exact character, an absent weight has coefficient zero. For
    a partial character, a zero or nonzero coefficient is returned only when
    the character certifies it; a weight outside the known region gives an
    error. The weight must have the same datum and coordinate convention as
    the character.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,1})
    (characterCoefficient(C,kmWeight(D,{0,0})),characterCoefficient(C,kmWeight(D,{3,0})))
  Text
    The output $(2,0)$ says that the zero-weight space in the adjoint
    representation of $A_2$ has dimension $2$, while $(3,0)$ is not a weight
    of that representation.
SeeAlso
  "Formal and completed characters"
  characterTerms
  isExactCharacter
///

doc ///
Key
  isExactCharacter
  (isExactCharacter,FormalCharacter)
  (isExactCharacter,LeviFormalCharacter)
Headline
  Determine whether a character is a complete finite sum
Usage
  b = isExactCharacter C
Inputs
  C:Thing
    a FormalCharacter or LeviFormalCharacter
Outputs
  b:Boolean
    whether the character is certified to be exact
Description
  Text
    Exact means that all coefficients outside the finite displayed support are
    zero. It does not mean that the sum is an irreducible character, or even
    an actual representation character. A root-depth expansion is not exact
    merely because its displayed terms happen to match a finite
    representation.
  Example
    D = rootDatum "A1";
    C = finiteCharacter(D,{2})
    M = integrableHighestWeightModule(D,{2});
    Cp = moduleCharacter(M,Depth=>2)
    (isExactCharacter C,isExactCharacter Cp)
  Text
    The answer is $(true,false)$. Both expansions display
    $e^{(2)}+1+e^{(-2)}$, but the first is the entire character whereas the
    second is certified only through root depth $2$.
SeeAlso
  "Formal and completed characters"
  moduleCharacter
  characterCoefficient
///

doc ///
Key
  characterDimension
  (characterDimension,FormalCharacter)
  (characterDimension,LeviFormalCharacter)
Headline
  Sum the coefficients of an exact character
Usage
  n = characterDimension C
Inputs
  C:Thing
    an exact FormalCharacter or LeviFormalCharacter
Outputs
  n:ZZ
    the sum of its integral coefficients
Description
  Text
    Evaluation at $e^\mu=1$ gives the dimension of a representation whose
    character is C. For a virtual character it gives a signed integer. A
    partial character is rejected: the sum of its known coefficients need not
    be a finite dimension.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,1})
    characterDimension C
  Text
    The answer is $8$, the dimension of the adjoint representation of $A_2$:
    its six nonzero root weights have multiplicity $1$ and its zero weight has
    multiplicity $2$.
SeeAlso
  "Formal and completed characters"
  weylDimension
  isExactCharacter
///

doc ///
Key
  finiteCharacter
  (finiteCharacter,CartanDatum,List)
  (finiteCharacter,Matrix,List)
  (finiteCharacter,RootDatum,KMWeight)
  (finiteCharacter,RootDatum,List)
Headline
  Compute the complete character of a finite highest-weight representation
Usage
  C = finiteCharacter(D,lambda)
Inputs
  D:RootDatum
    a finite-type root datum; a Cartan matrix or CartanDatum is accepted with a list
  lambda:Thing
    a dominant integral KMWeight in D, or its Dynkin-label list
Outputs
  C:FormalCharacter
    the exact irreducible highest-weight character
Description
  Text
    This computes every weight multiplicity in $V(\lambda)$ using the finite
    highest-weight calculation. A list specifies Dynkin labels. A complete
    weight in a nonminimal realization also specifies a central character,
    which is retained at every weight. Affine and indefinite highest-weight
    expansions instead use integrableCharacter with a root-depth bound.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,1});
    C
    characterDimension C
  Text
    This is the adjoint character: each of the six roots contributes one
    exponential and the zero weight contributes $2$. The total dimension $8$
    agrees with the six root spaces and two-dimensional Cartan subalgebra.
SeeAlso
  "Formal and completed characters"
  moduleCharacter
  decomposeCharacter
  weightMultiplicity
///

doc ///
Key
  dualCharacter
  (dualCharacter,FormalCharacter)
Headline
  Negate the exponents of a character
Usage
  Cd = dualCharacter C
Inputs
  C:FormalCharacter
    a character or certified partial expansion
Outputs
  Cd:FormalCharacter
    the character with the opposite weights and unchanged coefficients
Description
  Text
    The dual operation replaces each $e^\mu$ by $e^{-\mu}$. For an exact
    representation character this is the contragredient character. For a
    partial expansion, the coefficient information is transferred to the
    opposite region; no unsupported highest-weight depth bound is asserted.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    Cd = dualCharacter C
    (Cd == finiteCharacter(D,{0,1}),dualCharacter Cd == C)
  Text
    The input character has exponents $(1,0)$, $(-1,1)$, and $(0,-1)$.
    Its dual has the opposite exponents $(-1,0)$, $(1,-1)$, and $(0,1)$,
    with the same coefficients. Both final values are true:
    $V(1,0)^*$ is $V(0,1)$, and taking the dual twice
    recovers every coefficient of the original character.
SeeAlso
  "Formal and completed characters"
  dualWeight
  characterCoefficient
///

doc ///
Key
  decomposeCharacter
  (decomposeCharacter,FormalCharacter)
Headline
  Rewrite an exact character as a sum of irreducible characters
Usage
  T = decomposeCharacter C
Inputs
  C:FormalCharacter
    an exact character in finite type
Outputs
  T:Tally
    dominant KMWeight keys with their nonnegative irreducible multiplicities
Description
  Text
    The largest remaining highest weight determines an irreducible character
    to subtract. Repeating this process expresses C as a finite sum of
    irreducible characters. The input must be an actual finite representation
    character: partial expansions, a nonzero remainder, or a negative
    irreducible multiplicity give an error. Complete weights keep their full
    central coordinates.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    Cd = finiteCharacter(D,{0,1})
    P = C*Cd
    T = decomposeCharacter P;
    apply(pairs T,p -> {datumCoordinates p#0,p#1})
    characterFromDecomposition(D,T) == C*Cd
  Text
    The highest weights $(0,0)$ and $(1,1)$ each occur once, giving
    $V(1,0)\otimes V(0,1)\cong V(0,0)\oplus V(1,1)$. These are the trivial
    representation and the eight-dimensional adjoint representation. The final
    true verifies the character identity coefficient by coefficient.
SeeAlso
  "Formal and completed characters"
  characterFromDecomposition
  decomposeLeviCharacter
///

doc ///
Key
  characterFromDecomposition
  (characterFromDecomposition,RootDatum,HashTable)
  (characterFromDecomposition,RootDatum,Tally)
  (characterFromDecomposition,HashTable)
  (characterFromDecomposition,Tally)
Headline
  Expand a sum of irreducible highest-weight characters
Usage
  C = characterFromDecomposition(D,T)
Inputs
  D:RootDatum
    a finite root datum; it may be omitted when nonempty KMWeight keys determine it
  T:HashTable
    dominant KMWeight keys with nonnegative integral multiplicities; a Tally is also accepted
Outputs
  C:FormalCharacter
    the exact sum of the specified irreducible characters
Description
  Text
    A coefficient in T counts copies of the irreducible module with that
    highest weight, rather than a weight-space multiplicity. Providing D
    explicitly also defines the zero character when T is empty. Typed keys
    must belong to the same datum; negative highest-weight multiplicities are
    not accepted.
    Construct each key from its Dynkin-label tuple with
    @TT "kmWeight(D,labels)"@; bare coordinate lists are not table keys
    for this command, even when the datum is supplied explicitly.
  Example
    D = rootDatum "A1";
    T = new Tally from {kmWeight(D,{3})=>1,kmWeight(D,{1})=>2}
    C = characterFromDecomposition(D,T);
    C
    decomposeCharacter C == T
  Text
    The sum is the character of $V(3)\oplus2V(1)$:
    $e^{(3)}+3e^{(1)}+3e^{(-1)}+e^{(-3)}$. The final true confirms that
    expansion and irreducible decomposition are inverse operations in this
    example.
SeeAlso
  "Formal and completed characters"
  decomposeCharacter
  finiteCharacter
///

doc ///
Key
  integrableHighestWeightModule
  (integrableHighestWeightModule,RootDatum,KMWeight)
  (integrableHighestWeightModule,RootDatum,List)
Headline
  Specify an integrable highest-weight module
Usage
  M = integrableHighestWeightModule(D,lambda)
Inputs
  D:RootDatum
    a symmetrizable root datum
  lambda:Thing
    a dominant integral KMWeight in D, or a Dynkin-label list
Outputs
  M:KMRepresentation
    the integrable module with the given highest weight
Description
  Text
    Dominant integral means that every simple-coroot evaluation is a
    nonnegative integer. This constructor specifies the module but does not
    expand its character. In nonfinite type, subsequent character calculations
    require a complete highest weight so that weights differing in realization
    directions are not identified.
  Example
    D = rootDatum "A1";
    M = integrableHighestWeightModule(D,{2});
    M
    characterDimension moduleCharacter M
  Text
    The display $L(2)$ denotes the irreducible integrable highest-weight
    module of highest weight $(2)$. In type $A_1$ it is three-dimensional,
    with weights $(2)$, $(0)$, and $(-2)$.
SeeAlso
  "Formal and completed characters"
  moduleCharacter
  integrableCharacter
///

doc ///
Key
  vermaModule
  (vermaModule,RootDatum,KMWeight)
  (vermaModule,RootDatum,List)
Headline
  Specify a Verma module
Usage
  M = vermaModule(D,lambda)
Inputs
  D:RootDatum
    a symmetrizable root datum
  lambda:Thing
    a KMWeight in D, or a Dynkin-label list; dominance is not required
Outputs
  M:KMRepresentation
    the Verma module of highest weight lambda
Description
  Text
    A Verma module is generated by a highest-weight vector with the prescribed
    Cartan action, with no integrability relations imposed on the lowering
    operators. Its character is $e^{\lambda}$ times the product of
    $(1-e^{-\alpha})^{-mult(\alpha)}$ over positive roots. The constructor
    records the module; a character expansion requires Depth.
  Example
    D = rootDatum "A1";
    M = vermaModule(D,{2});
    M
    vermaCharacter(M,Depth=>2)
  Text
    The display $M(2)$ is the Verma module of highest weight $(2)$. Its
    expansion starts $e^{(2)}+1+e^{(-2)}$ and continues at weights
    $(-4),(-6),...$, each with multiplicity $1$; the dots indicate that only
    the first three depths were requested.
SeeAlso
  "Formal and completed characters"
  vermaCharacter
  bggResolution
///

doc ///
Key
  moduleCharacter
  (moduleCharacter,KMRepresentation)
  [moduleCharacter,Depth]
Headline
  Compute the character of a specified highest-weight module
Usage
  C = moduleCharacter(M,Depth=>H)
Inputs
  M:KMRepresentation
    a Verma module or integrable highest-weight module
  H:ZZ
    a nonnegative root-depth bound; optional for an integrable module in finite type
Outputs
  C:FormalCharacter
    the complete finite character or a certified root-depth expansion
Description
  Text
    For finite integrable modules, omitting Depth computes the complete
    character. Verma modules and nonfinite integrable modules require a depth
    bound. Depth is the sum of simple-root coefficients in the difference
    between the highest weight and a weight. Nonfinite characters require full
    realization coordinates, not just Dynkin labels.
  Example
    D = rootDatum "A2";
    M = integrableHighestWeightModule(D,{1,0})
    C = moduleCharacter M
    (isExactCharacter C,characterDimension C,C == finiteCharacter(D,{1,0}))
  Text
    The triple $(true,3,true)$ means that the entire three-dimensional
    defining character has been computed and agrees with the character
    computed directly from its highest weight.
SeeAlso
  "Formal and completed characters"
  integrableCharacter
  vermaCharacter
///

doc ///
Key
  vermaCharacter
  (vermaCharacter,KMRepresentation)
  [vermaCharacter,Depth]
Headline
  Expand a Verma character through a root depth
Usage
  C = vermaCharacter(M,Depth=>H)
Inputs
  M:KMRepresentation
    a Verma module
  H:ZZ
    the required nonnegative root-depth bound
Outputs
  C:FormalCharacter
    a character certified through the specified root depth
Description
  Text
    The positive-root partition product gives the weight multiplicities of a
    Verma module. This works in finite, affine, and indefinite symmetrizable
    type, using root multiplicities where needed. The result contains all
    coefficients at offsets of height at most H; it is not an exact finite
    character. In nonfinite type the highest weight must have full realization
    coordinates.
  Example
    D = rootDatum "A1";
    M = vermaModule(D,{2})
    C = vermaCharacter(M,Depth=>2);
    C
    characterCoefficient(C,kmWeight(D,{-2}))
  Text
    The displayed weights are $(2),(0),(-2)$, one copy of each. The final
    coefficient $1$ is the multiplicity at offset $2$ simple roots. An
    attempted coefficient query at $(-4)$ is outside this expansion and is not
    a known zero.
SeeAlso
  "Formal and completed characters"
  vermaModule
  characterCoefficient
///

doc ///
Key
  integrableCharacter
  (integrableCharacter,KMRepresentation)
  [integrableCharacter,Depth]
Headline
  Compute an integrable highest-weight character
Usage
  C = integrableCharacter(M,Depth=>H)
Inputs
  M:KMRepresentation
    an integrable highest-weight module
  H:ZZ
    a nonnegative root-depth bound; optional in finite type
Outputs
  C:FormalCharacter
    an exact finite character or a certified depth expansion
Description
  Text
    With no Depth in finite type, the entire character is computed. With
    Depth, weight multiplicities are computed by the highest-weight recurrence
    through that bound in any symmetrizable type. Positive-root multiplicities
    enter the recurrence. Full realization coordinates are required in
    nonfinite type to keep null-root shifts distinct.
  Example
    D = rootDatum cartanAffineA 1;
    lambda = affineWeightFromDynkinLabels(D,{1,0},0)
    M = integrableHighestWeightModule(D,lambda)
    C = integrableCharacter(M,Depth=>2);
    C
  Text
    There are three displayed weight spaces, each of dimension $1$, with
    realization coordinates $(1,0,0)$, $(-1,2,-1)$, and $(1,0,-1)$. The first
    two coordinates are Dynkin labels and the last is the derivation
    eigenvalue. In particular, the two equal Dynkin-label pairs occur in
    different affine grades; the expansion is complete only through root depth
    $2$.
SeeAlso
  "Formal and completed characters"
  affineWeightFromDynkinLabels
  moduleCharacter
///

doc ///
Key
  reductiveLeviDatum
  (reductiveLeviDatum,StandardParabolic,RootDatum)
Headline
  Describe a Levi factor including its center
Usage
  L = reductiveLeviDatum(P,D)
Inputs
  P:StandardParabolic
    a parabolic for the Cartan datum of D
  D:RootDatum
    the ambient root datum
Outputs
  L:ReductiveLeviDatum
    the retained-node root datum together with its central coordinates
Description
  Text
    The derived-Levi coordinates are the Dynkin labels on retained nodes.
    The full Levi also contains a center, given by the common kernel of the
    retained simple roots in the ambient Cartan subalgebra. The retained
    Cartan matrix must be nonsingular so that these two pieces faithfully
    distinguish ambient weights. The ambient datum itself need not be finite.
    The full Levi is reductive when the retained matrix has finite type.
    A nonsingular indefinite retained matrix is also accepted; in that case
    the derived Levi is a Kac--Moody algebra, not a finite-dimensional
    semisimple algebra.
  Example
    D = rootDatum "A2";
    P = standardParabolic(D,RemovedNodes=>{1});
    L = reductiveLeviDatum(P,D);
    leviCenterBasis L
  Text
    The retained node $2$ gives an $A_1$ derived algebra. The row $(2,1)$ is a
    basis of the one-dimensional center in simple-coroot coordinates: the
    retained simple root evaluates to zero because $2(-1)+1(2)=0$.
SeeAlso
  "Reductive Levi restriction"
  restrictWeight
  leviCenterBasis
///

doc ///
Key
  leviCenterBasis
  (leviCenterBasis,ReductiveLeviDatum)
Headline
  Display the center of a full Levi factor
Usage
  Z = leviCenterBasis L
Inputs
  L:ReductiveLeviDatum
    a full reductive Levi datum
Outputs
  Z:Matrix
    rows giving a basis of its center in ambient realization coordinates
Description
  Text
    Each row is annihilated by every retained simple root. Evaluating a
    restricted weight on these rows gives its centralCharacter, in the same
    row order. The basis is chosen by the package; changing its normalization
    changes the numerical central coordinates, not the underlying weight.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,RemovedNodes=>{1}),D);
    leviCenterBasis L
  Text
    The row $(2,1)$ means the center is spanned by $2h_1+h_2$. A weight of
    Dynkin labels $(a,b)$ therefore has central coordinate $2a+b$ for this
    Levi.
SeeAlso
  "Reductive Levi restriction"
  centralCharacter
  restrictWeight
///

doc ///
Key
  restrictWeight
  (restrictWeight,ReductiveLeviDatum,KMWeight)
  (restrictWeight,StandardParabolic,KMWeight)
Headline
  Restrict a weight to the full Levi
Usage
  muL = restrictWeight(L,mu)
Inputs
  L:Thing
    a ReductiveLeviDatum or StandardParabolic
  mu:KMWeight
    a complete ambient weight
Outputs
  muL:LeviWeight
    the retained-node weight and the evaluations on the center
Description
  Text
    Restriction preserves both the semisimple weight and the central
    character. If the first argument is a parabolic, its full Levi datum is
    constructed from the weight datum. Complete realization coordinates are
    required; Dynkin labels alone may omit central information.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,RemovedNodes=>{1}),D);
    muL = restrictWeight(L,kmWeight(D,{1,0}));
    (datumCoordinates semisimplePart muL,centralCharacter muL)
  Text
    The output $({0},{2})$ says that the retained $A_1$ highest-weight
    coordinate is $0$, and the center generator $2h_1+h_2$ acts with
    eigenvalue $2$. Thus this weight is trivial on the derived Levi but not on
    the full Levi.
SeeAlso
  "Reductive Levi restriction"
  semisimplePart
  centralCharacter
///

doc ///
Key
  semisimplePart
  (semisimplePart,LeviHighestWeight)
  (semisimplePart,LeviWeight)
Headline
  Read the derived-Levi part of a restricted weight
Usage
  muS = semisimplePart muL
Inputs
  muL:Thing
    a LeviWeight or LeviHighestWeight
Outputs
  muS:KMWeight
    its weight for the retained-node root datum
Description
  Text
    This forgets the central character and retains the evaluations on the Levi
    simple coroots. Two different full Levi weights can have the same
    semisimplePart; use centralCharacter when distinguishing the corresponding
    full Levi representations.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,RemovedNodes=>{1}),D);
    muL = restrictWeight(L,kmWeight(D,{-1,1}));
    datumCoordinates semisimplePart muL
  Text
    The answer $(1)$ is the evaluation on the retained coroot $h_2$. It is the
    highest weight of the two-dimensional $A_1$ representation; the additional
    central coordinate is not part of this output.
SeeAlso
  "Reductive Levi restriction"
  centralCharacter
  restrictWeight
///

doc ///
Key
  centralCharacter
  (centralCharacter,LeviHighestWeight)
  (centralCharacter,LeviWeight)
Headline
  Read the central coordinates of a restricted weight
Usage
  z = centralCharacter muL
Inputs
  muL:Thing
    a LeviWeight or LeviHighestWeight
Outputs
  z:List
    evaluations on the rows of leviCenterBasis
Description
  Text
    These are the scalar eigenvalues of the chosen center generators. They are
    part of the full Levi highest weight, not representation multiplicities.
    The row order and normalization are specified by leviCenterBasis.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,RemovedNodes=>{1}),D);
    muL = restrictWeight(L,kmWeight(D,{-1,1}));
    centralCharacter muL
  Text
    The output $(-1)$ means that $2h_1+h_2$ acts with eigenvalue $-1$, since
    $2(-1)+1=-1$. Together with semisimple weight $(1)$, this specifies the
    full Levi weight.
SeeAlso
  "Reductive Levi restriction"
  leviCenterBasis
  semisimplePart
///

doc ///
Key
  restrictCharacter
  (restrictCharacter,ReductiveLeviDatum,FormalCharacter)
  (restrictCharacter,StandardParabolic,FormalCharacter)
Headline
  Restrict a character to the full Levi
Usage
  CL = restrictCharacter(L,C)
Inputs
  L:Thing
    a ReductiveLeviDatum or StandardParabolic
  C:FormalCharacter
    a character for the ambient root datum
Outputs
  CL:LeviFormalCharacter
    the character with retained-node and central weights
Description
  Text
    Every ambient weight is restricted with its multiplicity. Central
    coordinates remain part of the weight keys, so weights with equal retained
    Dynkin labels need not be merged. Exactness or the known-coefficient
    region is retained. The resulting characters support addition,
    subtraction, integral scalar multiplication, products, and nonnegative
    powers for compatible full Levi data.
  Example
    D = rootDatum "A2";
    P = standardParabolic(D,RemovedNodes=>{1});
    C = finiteCharacter(D,{1,0})
    CL = restrictCharacter(P,C);
    CL
    characterDimension CL
  Text
    The dimension remains $3$. The three weights form a one-dimensional
    derived-Levi trivial summand with central eigenvalue $2$, and a
    two-dimensional $A_1$ summand with central eigenvalue $-1$. Restriction
    changes the acting algebra, not the underlying vector space.
SeeAlso
  "Reductive Levi restriction"
  decomposeLeviCharacter
  restrictWeight
///

doc ///
Key
  decomposeLeviCharacter
  (decomposeLeviCharacter,LeviFormalCharacter)
Headline
  Decompose an exact full Levi character
Usage
  T = decomposeLeviCharacter CL
Inputs
  CL:LeviFormalCharacter
    an exact character whose derived Levi is finite type
Outputs
  T:Tally
    LeviHighestWeight keys with nonnegative irreducible multiplicities
Description
  Text
    The decomposition is performed separately for each central character, then
    by irreducible highest weights for the derived Levi. Keeping the central
    coordinates avoids identifying distinct representations with the same
    semisimple highest weight. Partial characters and virtual sums requiring
    negative irreducible multiplicities are rejected.
  Example
    D = rootDatum "A2";
    P = standardParabolic(D,RemovedNodes=>{1});
    CL = restrictCharacter(P,finiteCharacter(D,{1,0}))
    T = decomposeLeviCharacter CL;
    apply(pairs T,p -> {datumCoordinates semisimplePart p#0,centralCharacter p#0,p#1})
  Text
    The entries give $V_{A_1}(0)_{2}\oplus V_{A_1}(1)_{-1}$, where the
    subscripts denote the eigenvalues of the chosen center generator. The
    summands have dimensions $1$ and $2$ and occur once each. Thus the
    dimensions add to $3$, and the central values distinguish the full Levi
    summands.
SeeAlso
  "Reductive Levi restriction"
  restrictCharacter
  decomposeCharacter
///

doc ///
Key
  kostantPartitionFunction
  (kostantPartitionFunction,CartanDatum,List)
  (kostantPartitionFunction,Matrix,List)
  (kostantPartitionFunction,RootDatum,KMRoot)
Headline
  Count positive-root partitions
Usage
  p = kostantPartitionFunction(A,beta)
Inputs
  A:Thing
    a finite Cartan matrix or CartanDatum; a RootDatum is accepted with a KMRoot
  beta:Thing
    integral simple-root coordinates, or a KMRoot in the given datum
Outputs
  p:ZZ
    the number of unordered sums of positive roots equal to beta
Description
  Text
    Repetitions are allowed, and root order is ignored. This is the
    coefficient of the specified root monomial in the product of
    $(1-e^{-\alpha})^{-1}$ over positive roots. The zero vector has one
    partition, the empty sum; a negative simple-root coordinate gives zero.
  Example
    A = cartanA 3;
    kostantPartitionFunction(A,{1,1})
  Text
    The answer $2$ counts the single root with coordinates $(1,1)$ and the sum
    of the two simple roots $(1,0)+(0,1)$. These are different positive-root
    partitions of the same root-lattice element.
SeeAlso
  "Independent multiplicity algorithms"
  kostantWeightMultiplicity
///

doc ///
Key
  kostantWeightMultiplicity
  (kostantWeightMultiplicity,Matrix,List,List)
  (kostantWeightMultiplicity,RootDatum,KMWeight,KMWeight)
Headline
  Compute a weight multiplicity by Kostant's formula
Usage
  m = kostantWeightMultiplicity(D,lambda,mu)
Inputs
  D:Thing
    a finite RootDatum with KMWeight inputs, or a Cartan matrix with Dynkin-label lists
  lambda:Thing
    a dominant integral highest weight
  mu:Thing
    an integral weight in the same datum and coordinate convention
Outputs
  m:ZZ
    the multiplicity of mu in the irreducible module of highest weight lambda
Description
  Text
    Kostant's formula alternates positive-root partition numbers over the Weyl
    group, evaluated at $w(\lambda+\rho)-(\mu+\rho)$. Complete weights are
    compared in the full realization: a different central character gives
    multiplicity zero even if Dynkin labels agree. In a nonminimal
    realization, complete weights and Dynkin-label classes cannot be mixed.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{1,1})
    mu = kmWeight(D,{0,0})
    kostantWeightMultiplicity(D,lambda,mu)
  Text
    The answer $2$ is the dimension of the zero-weight space in the adjoint
    representation of $A_2$, namely its rank-two Cartan subalgebra.
SeeAlso
  "Independent multiplicity algorithms"
  kostantPartitionFunction
  weightMultiplicity
///

doc ///
Key
  borelWeilBott
  (borelWeilBott,Matrix,List)
  (borelWeilBott,RootDatum,KMWeight)
  (borelWeilBott,RootDatum,List)
  [borelWeilBott,LineBundleConvention]
Headline
  Find the nonzero cohomology of a line bundle on a finite flag variety
Usage
  R = borelWeilBott(D,lambda)
Inputs
  D:Thing
    a finite RootDatum or Cartan matrix
  lambda:Thing
    an integral KMWeight with RootDatum input, or a Dynkin-label list with either input
Outputs
  R:BorelWeilBottResult
    the nonzero degree and representation, or complete vanishing
Description
  Text
    The convention is that the fiber of the line bundle has weight $-\lambda$.
    If $\lambda+\rho$ is singular, all cohomology vanishes. Otherwise, if w
    carries $\lambda+\rho$ to the dominant chamber, the nonzero group is in
    degree equal to the length of w and is the dual of
    $V(w(\lambda+\rho)-\rho)$. The calculation is as a Lie-algebra
    representation; a chosen global form does not restrict which line bundles
    may be considered. Extra central coordinates of complete weights are
    negated by duality.
  Example
    D = rootDatum "A1";
    borelWeilBott(D,{-3})
    borelWeilBott(D,{-1})
  Text
    The first output is $H^1=V(1)$, the two-dimensional cohomology of the line
    bundle of degree $-3$ on the projective line. The second output is $0$:
    the degree $-1$ line bundle has no cohomology. In type $A_1$ these
    irreducibles are self-dual.
SeeAlso
  "Borel-Weil-Bott and Demazure operators"
  borelWeilBottCharacter
  dotWeylAction
///

doc ///
Key
  borelWeilBottCharacter
  (borelWeilBottCharacter,RootDatum,KMWeight)
  (borelWeilBottCharacter,RootDatum,List)
  [borelWeilBottCharacter,LineBundleConvention]
Headline
  Compute the character of line-bundle cohomology
Usage
  C = borelWeilBottCharacter(D,lambda)
Inputs
  D:RootDatum
    a finite root datum
  lambda:Thing
    an integral KMWeight in D, or its Dynkin-label list
Outputs
  C:FormalCharacter
    the exact character of the unique nonzero cohomology group, or the zero character
Description
  Text
    This computes the representation selected by Borel--Weil--Bott, with
    line-bundle fiber weight $-\lambda$, then expands its character. It does
    not multiply the result by a cohomological sign. Use borelWeilBott to
    recover the cohomological degree; vanishing gives the exact zero
    character.
  Example
    D = rootDatum "A1";
    C = borelWeilBottCharacter(D,{-3});
    C
    characterDimension C
  Text
    The character is $e^{(1)}+e^{(-1)}$ and has dimension $2$. This is the
    character of $H^1$ for the degree $-3$ line bundle on the projective line,
    not its negative Euler characteristic.
SeeAlso
  "Borel-Weil-Bott and Demazure operators"
  borelWeilBott
  characterDimension
///

doc ///
Key
  demazureOperator
  (demazureOperator,FormalCharacter,ZZ)
  (demazureOperator,ZZ,FormalCharacter)
Headline
  Apply one simple Demazure operator to a character
Usage
  C1 = demazureOperator(i,C)
Inputs
  i:ZZ
    a simple-root position numbered from 1
  C:FormalCharacter
    an exact finite sum whose weights have integral evaluation on the selected simple coroot
Outputs
  C1:FormalCharacter
    the exact image under the simple Demazure operator
Description
  Text
    On $e^\mu$ with simple-coroot evaluation m, the operator sums $e^{\mu-k
    \alpha_i}$ for $0\leq k\leq m$ when $m\geq 0$, gives zero when $m=-1$, and
    gives the negative of the intervening upward string when $m\leq -2$. It
    extends linearly and accepts either argument order. Partial expansions are
    rejected because omitted terms can contribute to displayed weights.
    The evaluation $m$ must be an integer for every term, even though
    @TO formalCharacter@ also permits rational weight coordinates.
  Example
    D = rootDatum "A2";
    C = formalCharacter(D,new Tally from {kmWeight(D,{1,0})=>1})
    demazureOperator(1,C)
  Text
    The first output is the starting monomial $e^{(1,0)}$. Applying the
    operator gives the second character $e^{(1,0)}+e^{(-1,1)}$. The highest
    weight pairs to $1$ with the first simple coroot, so the operator adds
    the weight obtained by subtracting the first simple root.
SeeAlso
  "Borel-Weil-Bott and Demazure operators"
  demazureCharacter
  formalCharacter
///

doc ///
Key
  demazureCharacter
  (demazureCharacter,Matrix,List,List)
  (demazureCharacter,RootDatum,KMWeight,List)
  (demazureCharacter,RootDatum,List,List)
Headline
  Compute a Demazure module character along a Weyl word
Usage
  C = demazureCharacter(D,lambda,word)
Inputs
  D:Thing
    a RootDatum in any symmetrizable type, or a Cartan matrix with list input
  lambda:Thing
    a dominant integral KMWeight in D, or its Dynkin-label list
  word:List
    simple-reflection positions or chosen node labels, with the rightmost acting first
Outputs
  C:FormalCharacter
    the exact finite Demazure character
Description
  Text
    Starting from the highest-weight exponential, the simple Demazure
    operators are applied in the given Weyl order. Their braid relations
    identify different reduced words for the same Weyl element. A finite word
    gives an exact finite-support character even if the ambient highest-weight
    module is infinite-dimensional. The empty word gives only the
    highest-weight monomial.
  Example
    D = rootDatum "A2";
    C = demazureCharacter(D,{1,0},{1});
    C
    demazureCharacter(D,{1,0},longElement cartanA 3) == finiteCharacter(D,{1,0})
  Text
    The first character has the two weights $(1,0)$ and $(-1,1)$, each once.
    The final true says that the longest-element Demazure module gives the
    whole three-dimensional module $V(1,0)$.
SeeAlso
  "Borel-Weil-Bott and Demazure operators"
  demazureOperator
  finiteCharacter
///

doc ///
Key
  kazhdanLusztigVariable
Headline
  Obtain the default polynomial variable
Usage
  q = kazhdanLusztigVariable()
Outputs
  q:RingElement
    the generator of the ordinary polynomial ring ZZ[q]
Description
  Text
    This returns the generator used by Kazhdan--Lusztig polynomials when no
    Variable is supplied. Its ring is an ordinary Macaulay2 polynomial ring,
    so the usual polynomial operations apply. The function takes no arguments.
  Example
    q = kazhdanLusztigVariable()
    factor(q^2-1)
  Text
    The factors are $q-1$ and $q+1$: the default Kazhdan--Lusztig variable is
    usable in ordinary polynomial arithmetic.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  kazhdanLusztigPolynomial
///

doc ///
Key
  kazhdanLusztigCoefficients
  (kazhdanLusztigCoefficients,Matrix,List,List)
  (kazhdanLusztigCoefficients,WeylElement,WeylElement)
  [kazhdanLusztigCoefficients,MaxElements]
Headline
  Compute the coefficients of a Kazhdan--Lusztig polynomial
Usage
  a = kazhdanLusztigCoefficients(u,v)
Inputs
  u:WeylElement
    a Weyl element in the same group as v
  v:WeylElement
    the upper element of the Bruhat interval
Outputs
  a:List
    integral coefficients in ascending powers of the polynomial variable
Description
  Text
    The result gives the coefficients of $P_{u,v}(q)$, starting with the
    constant term. The calculation uses the finite principal Bruhat interval
    below v, so finite, affine, and indefinite Weyl groups are allowed. A
    Cartan matrix and two word lists can be supplied instead. MaxElements
    bounds the size of the principal interval; exceeding it gives an error,
    not a partial polynomial.
  Example
    W = weylGroup cartanA 4;
    u = identityWeylElement W
    v = weylElement(W,{2,1,3,2})
    kazhdanLusztigCoefficients(u,v)
  Text
    The list @TT "{1,1}"@ means that the constant and linear coefficients are
    both $1$, so $P_{u,v}(q)=1+q$. The first list entry is not the coefficient
    of $q$.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  kazhdanLusztigPolynomial
  bruhatLessOrEqual
///

doc ///
Key
  kazhdanLusztigPolynomial
  (kazhdanLusztigPolynomial,Matrix,List,List)
  (kazhdanLusztigPolynomial,WeylElement,WeylElement)
  [kazhdanLusztigPolynomial,MaxElements]
  [kazhdanLusztigPolynomial,Variable]
Headline
  Compute a Kazhdan--Lusztig polynomial
Usage
  P = kazhdanLusztigPolynomial(u,v,Variable=>t)
Inputs
  u:WeylElement
    a Weyl element in the same group as v
  v:WeylElement
    the upper element of the Bruhat interval
  t:RingElement
    an optional variable in a polynomial ring over ZZ or QQ
Outputs
  P:RingElement
    the ordinary polynomial in the chosen ring, or ZZ[q] by default
Description
  Text
    This computes $P_{u,v}$ from the finite Bruhat interval below v in any
    symmetrizable Weyl type. Noncomparable elements give zero. A Cartan matrix
    and two word lists are also accepted. Variable must be an actual
    polynomial generator, not a constant, a general polynomial, or an element
    of a quotient ring. MaxElements bounds the principal interval.
  Example
    W = weylGroup cartanA 4;
    u = identityWeylElement W
    v = weylElement(W,{2,1,3,2})
    R = QQ[t];
    P = kazhdanLusztigPolynomial(u,v,Variable=>t);
    P
    (ring P === R,sub(P,{t=>1}))
  Text
    The output $t+1$ is the Kazhdan--Lusztig polynomial for this type $A_3$
    interval. The pair $(true,2)$ confirms that it lies in the user's ring and
    has value $2$ at $t=1$. Its nonconstant term distinguishes this interval
    from one with polynomial $1$.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  kazhdanLusztigCoefficients
  vermaCompositionMultiplicity
///

doc ///
Key
  dotWeylAction
  (dotWeylAction,WeylElement,KMWeight)
Headline
  Apply the rho-shifted Weyl action to a weight
Usage
  mu = dotWeylAction(w,lambda)
Inputs
  w:WeylElement
    a Weyl element for the Cartan datum of lambda
  lambda:KMWeight
    a weight; dominance is not required
Outputs
  mu:KMWeight
    the shifted weight in the original root datum
Description
  Text
    The dot action is $w(\lambda+\rho)-\rho$, where rho evaluates to $1$ on
    every simple coroot. The weight datum and complete realization directions
    are retained, including additional Weyl-fixed central coordinates. This is
    the action that indexes highest weights in BGG resolutions and regular
    integral blocks.
  Example
    D = rootDatum "A1";
    s = simpleWeylReflection(weylGroup D,1)
    datumCoordinates dotWeylAction(s,kmWeight(D,{2}))
  Text
    The result $(-4)$ is the shifted reflection of $(2)$: $-(2+1)-1=-4$. In
    contrast, the ordinary reflection gives $(-2)$.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  weylAction
  bggResolution
///

doc ///
Key
  simpleHighestWeightModule
  (simpleHighestWeightModule,RootDatum,KMWeight)
  (simpleHighestWeightModule,RootDatum,List)
Headline
  Specify a simple highest-weight module
Usage
  M = simpleHighestWeightModule(D,mu)
Inputs
  D:RootDatum
    a symmetrizable root datum
  mu:Thing
    a KMWeight in D, or a Dynkin-label list; dominance is not required
Outputs
  M:KMRepresentation
    the simple highest-weight module with highest weight mu
Description
  Text
    This constructor records the simple module $L(\mu)$. A dominant integral
    weight gives an integrable simple module, but a general highest weight
    need not be integrable. The constructor does not compute its character;
    moduleCharacter currently computes characters of Verma modules and modules
    specified by integrableHighestWeightModule.
  Example
    D = rootDatum "A1";
    simpleHighestWeightModule(D,{-4})
  Text
    The output $L(-4)$ denotes the simple highest-weight module of highest
    weight $(-4)$. Its highest weight is not dominant, so this does not
    describe a finite-dimensional $A_1$ representation.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  vermaModule
  vermaCompositionFactors
  integrableHighestWeightModule
///

doc ///
Key
  vermaCompositionMultiplicity
  (vermaCompositionMultiplicity,RootDatum,KMWeight,WeylElement,WeylElement)
  [vermaCompositionMultiplicity,MaxElements]
Headline
  Compute a simple composition multiplicity in a regular Verma module
Usage
  m = vermaCompositionMultiplicity(D,lambda,u,v)
Inputs
  D:RootDatum
    a symmetrizable root datum
  lambda:KMWeight
    a dominant integral highest weight in D
  u:WeylElement
    the Weyl index of the Verma highest weight
  v:WeylElement
    the Weyl index of the simple highest weight, in the same group
Outputs
  m:ZZ
    the composition multiplicity P_(u,v)(1)
Description
  Text
    The returned number is the multiplicity of $L(v(\lambda+\rho)-\rho)$ in
    $M(u(\lambda+\rho)-\rho)$. This is the regular integral-block
    Kazhdan--Lusztig multiplicity formula. It is valid in finite, affine, and
    indefinite symmetrizable type, even when a full Verma composition list is
    infinite. MaxElements bounds the principal Bruhat interval used.
  Example
    D = rootDatum "A1";
    W = weylGroup D;
    u = identityWeylElement W
    v = simpleWeylReflection(W,1)
    vermaCompositionMultiplicity(D,kmWeight(D,{2}),u,v)
  Text
    The answer $1$ says that $L(-4)$ occurs once in the Verma module $M(2)$,
    because the dot reflection takes $(2)$ to $(-4)$ and the corresponding
    Kazhdan--Lusztig polynomial is $1$.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  dotWeylAction
  vermaCompositionFactors
///

doc ///
Key
  vermaCompositionFactors
  (vermaCompositionFactors,RootDatum,KMWeight,WeylElement)
  [vermaCompositionFactors,MaxElements]
Headline
  List the simple factors of a finite-type Verma module
Usage
  T = vermaCompositionFactors(D,lambda,u)
Inputs
  D:RootDatum
    a finite root datum
  lambda:KMWeight
    a dominant integral highest weight in D
  u:WeylElement
    the index of the Verma highest weight in its regular integral block
Outputs
  T:Tally
    simple KMRepresentation keys with composition multiplicities
Description
  Text
    The Verma highest weight is $u(\lambda+\rho)-\rho$. Each key is the simple
    module at a Weyl dot translate of lambda, and its coefficient is the
    corresponding Kazhdan--Lusztig polynomial evaluated at $1$. Finite type is
    required to return the entire list; use vermaCompositionMultiplicity for
    individual factors in nonfinite type.
  Example
    D = rootDatum "A1";
    u = identityWeylElement weylGroup D
    vermaCompositionFactors(D,kmWeight(D,{2}),u)
  Text
    The two keys are $L(2)$ and $L(-4)$, each with coefficient $1$. These are
    the two simple composition factors of $M(2)$; a composition series is not
    a direct-sum decomposition.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  vermaCompositionMultiplicity
  simpleHighestWeightModule
///

doc ///
Key
  bggResolution
  (bggResolution,RootDatum,KMWeight)
  (bggResolution,RootDatum,List)
  [bggResolution,MaxElements]
  [bggResolution,MaxDegree]
Headline
  Compute the Verma terms of a BGG resolution
Usage
  B = bggResolution(D,lambda,MaxDegree=>k)
Inputs
  D:RootDatum
    a symmetrizable root datum
  lambda:Thing
    a dominant integral KMWeight in D, or a Dynkin-label list
  k:ZZ
    an optional nonnegative homological bound, required in nonfinite type
Outputs
  B:BGGResolution
    all requested degreewise Verma summands and their bounds
Description
  Text
    The term in homological degree k is the sum of $M(w(\lambda+\rho)-\rho)$
    over Weyl elements of length k. Differential matrices are not computed.
    Finite type gives all terms by default. In nonfinite type, MaxDegree
    computes every summand through the requested degree without asserting that
    the infinite resolution ends there. MaxElements bounds the number of Weyl
    elements considered.
  Example
    D = rootDatum "A1";
    B = bggResolution(D,{2});
    B
  Text
    The terms $C_0=M(2)$ and $C_1=M(-4)$ mean the exact resolution
    $0\longrightarrow M(-4)\longrightarrow M(2)\longrightarrow
    L(2)\longrightarrow0$. These are its module terms; the maps themselves are
    not part of the output.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  bggEulerCharacter
  dotWeylAction
///

doc ///
Key
  bggEulerCharacter
  (bggEulerCharacter,BGGResolution)
  [bggEulerCharacter,Depth]
Headline
  Expand the alternating sum of BGG Verma terms
Usage
  C = bggEulerCharacter(B,Depth=>H)
Inputs
  B:BGGResolution
    a full or homologically bounded BGG resolution
  H:ZZ
    the required nonnegative absolute root-depth bound
Outputs
  C:FormalCharacter
    the alternating Verma character certified through root depth H
Description
  Text
    The signs alternate with homological degree. When B is incomplete and
    contains terms only through degree k, the requested depth must satisfy
    $H\leq k$, because omitted terms can contribute beyond that range.
    A complete finite resolution has no such homological restriction,
    even if constructed with an explicit MaxDegree bound. The answer remains
    marked partial even if cancellation leaves finite support; the stated
    depth is a guarantee about coefficients, not a claim about all higher
    depths.
  Example
    D = rootDatum "A1";
    B = bggResolution(D,{2})
    C = bggEulerCharacter(B,Depth=>2);
    C
    isExactCharacter C
  Text
    The displayed sum is $e^{(2)}+1+e^{(-2)}$, matching the weights of $L(2)$
    through depth $2$. The value false is intentional: this calculation
    certifies only the requested part of the alternating character, rather
    than declaring it a complete finite character.
SeeAlso
  "Kazhdan-Lusztig theory and the BGG resolution"
  bggResolution
  characterCoefficient
///

doc ///
Key
  adamsOperation
  (adamsOperation,FormalCharacter,ZZ)
  (adamsOperation,ZZ,FormalCharacter)
Headline
  Multiply every weight exponent by a positive integer
Usage
  A = adamsOperation(C,k)
Inputs
  C:FormalCharacter
    an exact finite formal sum; negative coefficients are allowed
  k:ZZ
    a positive integer
Outputs
  A:FormalCharacter
    the exact sum with exponents multiplied by k
Description
  Text
    The kth Adams operation sends $e^\mu$ to $e^{k \mu}$ without changing its
    coefficient. It is not a tensor power and need not be the character of an
    actual representation. Either argument order is accepted; partial
    expansions are not accepted.
  Example
    D = rootDatum "A1";
    C = finiteCharacter(D,{1})
    adamsOperation(C,2)
  Text
    The Adams image is $e^{(2)}+e^{(-2)}$. It omits the zero-weight term of $V(2)$:
    the Adams operation changes exponents rather than forming the symmetric
    square or tensor square.
SeeAlso
  "Lambda operations and representation indices"
  symmetricPowerCharacter
  exteriorPowerCharacter
///

doc ///
Key
  symmetricPowerCharacter
  (symmetricPowerCharacter,FormalCharacter,ZZ)
  (symmetricPowerCharacter,ZZ,FormalCharacter)
Headline
  Compute a symmetric-power character
Usage
  S = symmetricPowerCharacter(C,k)
Inputs
  C:FormalCharacter
    an exact character with nonnegative weight multiplicities
  k:ZZ
    a nonnegative integer
Outputs
  S:FormalCharacter
    the exact character of the kth symmetric power
Description
  Text
    The answer is the coefficient of the kth power of t in the product of
    $(1-t e^\mu)^{-m_{\mu}}$ over weight spaces. Either argument order is
    accepted. The zeroth power is the trivial character. Partial expansions
    and virtual characters with negative coefficients are rejected.
  Example
    D = rootDatum "A1";
    C = finiteCharacter(D,{1})
    S = symmetricPowerCharacter(C,2);
    S
    S == finiteCharacter(D,{2})
  Text
    The symmetric-square character is $e^{(2)}+1+e^{(-2)}$. The final true identifies the
    symmetric square of the two-dimensional standard $A_1$ representation with
    the three-dimensional irreducible $V(2)$.
SeeAlso
  "Lambda operations and representation indices"
  symmetricPowerDecomposition
  exteriorPowerCharacter
///

doc ///
Key
  exteriorPowerCharacter
  (exteriorPowerCharacter,FormalCharacter,ZZ)
  (exteriorPowerCharacter,ZZ,FormalCharacter)
Headline
  Compute an exterior-power character
Usage
  E = exteriorPowerCharacter(C,k)
Inputs
  C:FormalCharacter
    an exact character with nonnegative weight multiplicities
  k:ZZ
    a nonnegative integer
Outputs
  E:FormalCharacter
    the exact character of the kth exterior power
Description
  Text
    The answer is the coefficient of the kth power of t in the product of
    $(1+t e^\mu)^{m_{\mu}}$ over weight spaces. Either argument order is
    accepted. The zeroth power is the trivial character, and powers above the
    dimension are zero. Partial expansions and negative weight multiplicities
    are rejected.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0})
    E = exteriorPowerCharacter(C,2);
    E
    E == finiteCharacter(D,{0,1})
  Text
    The exterior-square character has the three weights of $V(0,1)$, the dual defining representation
    of $A_2$. Thus the second exterior power of $V(1,0)$ is $V(0,1)$, and the
    final true verifies their character equality.
SeeAlso
  "Lambda operations and representation indices"
  exteriorPowerDecomposition
  symmetricPowerCharacter
///

doc ///
Key
  symmetricPowerDecomposition
  (symmetricPowerDecomposition,FormalCharacter,ZZ)
Headline
  Decompose a symmetric power into irreducibles
Usage
  T = symmetricPowerDecomposition(C,k)
Inputs
  C:FormalCharacter
    an exact finite-type representation character
  k:ZZ
    a nonnegative integer
Outputs
  T:Tally
    dominant KMWeight keys with irreducible multiplicities
Description
  Text
    This computes the symmetric-power character and rewrites it as a sum of
    finite-type irreducible characters. Weight coefficients must be
    nonnegative, and the resulting sum must have nonnegative irreducible
    multiplicities. The input character may already be reducible.
  Example
    D = rootDatum "A1";
    C = finiteCharacter(D,{2})
    T = symmetricPowerDecomposition(C,2);
    apply(pairs T,p -> {datumCoordinates p#0,p#1})
  Text
    The two highest weights $(4)$ and $(0)$ each occur once. Thus
    $\operatorname{Sym}^2 V(2)\cong V(4)\oplus V(0)$, of total dimension
    $5+1=6$.
SeeAlso
  "Lambda operations and representation indices"
  symmetricPowerCharacter
  exteriorPowerDecomposition
  decomposeCharacter
///

doc ///
Key
  exteriorPowerDecomposition
  (exteriorPowerDecomposition,FormalCharacter,ZZ)
Headline
  Decompose an exterior power into irreducibles
Usage
  T = exteriorPowerDecomposition(C,k)
Inputs
  C:FormalCharacter
    an exact finite-type representation character
  k:ZZ
    a nonnegative integer
Outputs
  T:Tally
    dominant KMWeight keys with irreducible multiplicities
Description
  Text
    This forms the exterior-power character and decomposes it into finite-type
    irreducibles. The zeroth power is the trivial representation, and a power
    above the dimension has an empty decomposition. The character must have
    nonnegative weight coefficients and yield an actual representation
    decomposition.
  Example
    D = rootDatum "A1";
    C = finiteCharacter(D,{2})
    T = exteriorPowerDecomposition(C,2);
    apply(pairs T,p -> {datumCoordinates p#0,p#1})
  Text
    The only highest weight is $(2)$ with multiplicity $1$, giving
    $\bigwedge^2V(2)\cong V(2)$. Thus the second exterior power of this
    three-dimensional representation is again the same irreducible
    representation.
SeeAlso
  "Lambda operations and representation indices"
  exteriorPowerCharacter
  decomposeCharacter
///

doc ///
Key
  frobeniusSchurIndicator
  (frobeniusSchurIndicator,Matrix,List)
  (frobeniusSchurIndicator,RootDatum,KMWeight)
  (frobeniusSchurIndicator,RootDatum,List)
Headline
  Determine the invariant bilinear-form type of an irreducible
Usage
  e = frobeniusSchurIndicator(D,lambda)
Inputs
  D:Thing
    a finite RootDatum or Cartan matrix
  lambda:Thing
    a dominant integral KMWeight in D, or a Dynkin-label list
Outputs
  e:ZZ
    0 for non-self-dual, 1 for symmetric, or -1 for alternating
Description
  Text
    A self-dual irreducible has, up to scale, a nondegenerate invariant
    bilinear form. Its symmetry is given by the parity of the pairing of the
    highest weight with twice the coroot Weyl vector. For complete weights,
    self-duality includes the full central character; a Dynkin-label class
    concerns the semisimple representation only.
  Example
    D = rootDatum "A1";
    (frobeniusSchurIndicator(D,{1}),frobeniusSchurIndicator(D,{2}))
  Text
    The pair $(-1,1)$ says that the two-dimensional representation $V(1)$ has
    an invariant alternating form, while the three-dimensional representation
    $V(2)$ has an invariant symmetric form.
SeeAlso
  "Lambda operations and representation indices"
  isSelfDual
  dualWeight
///

doc ///
Key
  representationDynkinIndex
  (representationDynkinIndex,Matrix,List)
  (representationDynkinIndex,RootDatum,KMWeight)
Headline
  Compute the Dynkin index of an irreducible representation
Usage
  j = representationDynkinIndex(D,lambda)
Inputs
  D:Thing
    a connected finite RootDatum, or a Cartan matrix with a Dynkin-label list
  lambda:Thing
    a dominant integral KMWeight in D, or a list with matrix input
Outputs
  j:QQ
    the index using long-root squared length 2
Description
  Text
    The index is the dimension of the representation divided by the dimension
    of the simple Lie algebra, times its quadratic Casimir eigenvalue.
    Equivalently it compares the representation trace form with the invariant
    form in the stated normalization. Connectedness is required: for several
    simple factors there is a separate index for each factor.
  Example
    D = rootDatum "A2";
    representationDynkinIndex(D,kmWeight(D,{1,0}))
  Text
    The answer $1$ is the index of the three-dimensional defining
    representation of $A_2$. In this normalization its Casimir is $8/3$, and
    $(3/8)(8/3)=1$.
SeeAlso
  "Lambda operations and representation indices"
  quadraticCasimir
  normalizedInvariantForm
///

doc ///
Key
  highestWeightCrystal
  (highestWeightCrystal,CartanDatum,List)
  (highestWeightCrystal,CartanDatum,Matrix)
  (highestWeightCrystal,Matrix,List)
  (highestWeightCrystal,Matrix,Matrix)
  (highestWeightCrystal,RootDatum,KMWeight)
  (highestWeightCrystal,RootDatum,List)
  (highestWeightCrystal,RootDatum,Matrix)
  [highestWeightCrystal,MaxElements]
Headline
  Construct a complete finite highest-weight crystal
Usage
  B = highestWeightCrystal(D,lambda)
Inputs
  D:Thing
    a finite RootDatum, CartanDatum, or Cartan matrix
  lambda:Thing
    a dominant integral KMWeight with RootDatum input, or Dynkin labels as a list or row or column matrix with any input
Outputs
  B:Crystal
    the full crystal of the irreducible highest-weight representation
Description
  Text
    The vertices are a combinatorial basis of the irreducible module, with
    weights and colored Kashiwara arrows. Connected type A uses semistandard
    tableaux; other finite types and disconnected systems use exact rational
    Littelmann paths. MaxElements bounds the number of vertices; exceeding it
    gives an error rather than a partial crystal. Complete highest weights
    retain their additional central coordinates.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,1})
    (#crystalVertices B,characterCoefficient(crystalCharacter B,kmWeight(D,{0,0})))
  Text
    The pair $(8,2)$ means the adjoint crystal has eight vertices, two of
    which have zero weight. Thus the crystal counts both the total dimension
    and the rank-two zero-weight multiplicity.
SeeAlso
  "Finite highest-weight crystals"
  crystalVertices
  crystalCharacter
///

doc ///
Key
  minusculeCrystal
  (minusculeCrystal,CartanDatum,List)
  (minusculeCrystal,CartanDatum,Matrix)
  (minusculeCrystal,CartanDatum,Thing)
  (minusculeCrystal,CartanDatum,ZZ)
  (minusculeCrystal,Matrix,List)
  (minusculeCrystal,Matrix,Matrix)
  (minusculeCrystal,Matrix,Thing)
  (minusculeCrystal,Matrix,ZZ)
  (minusculeCrystal,RootDatum,KMWeight)
  (minusculeCrystal,RootDatum,List)
  (minusculeCrystal,RootDatum,Matrix)
  (minusculeCrystal,RootDatum,Thing)
  (minusculeCrystal,RootDatum,ZZ)
  [minusculeCrystal,MaxElements]
Headline
  Construct the orbit crystal of a minuscule representation
Usage
  B = minusculeCrystal(D,i)
Inputs
  D:Thing
    a connected finite RootDatum, CartanDatum, or Cartan matrix
  i:Thing
    a minuscule node position or label; a minuscule highest weight may also be supplied
Outputs
  B:Crystal
    the Weyl-orbit crystal, with one vertex per weight
Description
  Text
    A minuscule representation has all its weights in one Weyl orbit, each
    with multiplicity one. Its simple-root strings have length at most one.
    This command constructs that orbit directly, with a lowering arrow when
    the appropriate simple-coroot evaluation is $1$. Node positions are
    numbered from $1$ and take precedence over labels. MaxElements bounds the
    full vertex count.
    A typed @TO KMWeight@ requires @TO RootDatum@ input. A list or row
    or column matrix of Dynkin labels is accepted with any of the three
    datum forms.
  Example
    D = rootDatum "D4";
    B = minusculeCrystal(D,1)
    (#crystalVertices B,#crystalEdges B,characterDimension crystalCharacter B)
  Text
    The triple $(8,8,8)$ gives eight orbit weights, eight colored lowering
    arrows, and an eight-dimensional representation. Every weight occurs once,
    as required for this minuscule $D_4$ fundamental representation.
SeeAlso
  "Finite highest-weight crystals"
  minusculeNodes
  highestWeightCrystal
///

doc ///
Key
  tensorProductCrystal
  (tensorProductCrystal,Crystal,Crystal)
  [tensorProductCrystal,MaxElements]
Headline
  Construct the crystal of a tensor product
Usage
  T = tensorProductCrystal(B,C)
Inputs
  B:Crystal
    the left finite crystal
  C:Crystal
    the right finite crystal with the same root datum and weight-coordinate convention
Outputs
  T:Crystal
    the full tensor crystal on pairs of factor vertices
Description
  Text
    A vertex has weight equal to the sum of the factor weights. For a color i,
    the lowering operator acts on the left when its phi exceeds the right
    epsilon, and on the right otherwise; the raising operator uses the
    corresponding weak inequality. The tensor crystal may have several
    connected components. MaxElements bounds the number of vertex pairs.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{1})
    T = tensorProductCrystal(B,B)
    (#crystalVertices T,characterDimension crystalCharacter T)
    sort apply(select(crystalVertices T,b -> crystalE(b,1) === null),b -> datumCoordinates crystalWeight b)
  Text
    The first pair is $(4,4)$. The highest vertices have weights $(0)$ and
    $(2)$, giving $V(1)\otimes V(1)\cong V(2)\oplus V(0)$, of dimensions
    $3+1=4$.
SeeAlso
  "Finite highest-weight crystals"
  crystalHighestWeight
  crystalCharacter
///

doc ///
Key
  crystalDatum
  (crystalDatum,Crystal)
Headline
  Recover the root datum of a crystal
Usage
  D = crystalDatum B
Inputs
  B:Crystal
    a finite crystal
Outputs
  D:RootDatum
    the datum of its weights and simple-root colors
Description
  Text
    The root datum determines the weight lattice, node labels, and Cartan
    matrix used by the crystal operators. It is retained by highest-weight,
    minuscule, and tensor-product crystals.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,0})
    crystalDatum B == D
  Text
    The value true confirms that the crystal weights belong to the original
    $A_2$ datum, rather than an unlabeled collection of integer tuples.
SeeAlso
  "Finite highest-weight crystals"
  crystalWeight
  crystalParent
///

doc ///
Key
  crystalHighestWeight
  (crystalHighestWeight,Crystal)
Headline
  Read a crystal highest weight
Usage
  lambda = crystalHighestWeight B
Inputs
  B:Crystal
    a highest-weight, minuscule, or tensor-product crystal
Outputs
  lambda:KMWeight
    its highest weight, or the sum of factor highest weights in a tensor product
Description
  Text
    For an irreducible highest-weight crystal this is the weight of its
    highest vertex. A tensor crystal can have several components; this command
    returns the sum of the factor highest weights, not the list of all
    component highest weights. To find all components, select vertices killed
    by every raising operator.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,0})
    C = highestWeightCrystal(D,{0,1})
    datumCoordinates crystalHighestWeight tensorProductCrystal(B,C)
  Text
    The answer $(1,1)$ is the sum $(1,0)+(0,1)$. This tensor product also has
    a component of highest weight $(0,0)$, which is not included in this
    single highest-weight output.
SeeAlso
  "Finite highest-weight crystals"
  tensorProductCrystal
  crystalE
///

doc ///
Key
  crystalParent
  (crystalParent,CrystalVertex)
Headline
  Find the crystal containing a vertex
Usage
  B = crystalParent b
Inputs
  b:CrystalVertex
    a crystal vertex
Outputs
  B:Crystal
    the crystal that owns the vertex
Description
  Text
    Vertices retain their crystal as well as their tableau, path, or orbit
    data. This prevents applying operators from a different crystal merely
    because two vertices have equal weights.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,2}})
    crystalParent b === B
  Text
    The value true says that the middle vertex of weight $(0)$ belongs to this
    three-vertex $B(2)$ crystal. A weight-zero vertex of another crystal is a
    different object.
SeeAlso
  "Finite highest-weight crystals"
  crystalContains
  crystalVertex
///

doc ///
Key
  crystalVertexData
  (crystalVertexData,CrystalVertex)
Headline
  Read the model-specific description of a vertex
Usage
  data = crystalVertexData b
Inputs
  b:CrystalVertex
    a vertex in any supported crystal model
Outputs
  data:List
    tableau rows, path points, an orbit weight tuple, or a pair of factor descriptions
Description
  Text
    The description is precisely the form accepted by crystalVertex. Tableau
    data are lists of rows. Path data are parameter-point pairs.
    Minuscule-orbit data are Dynkin-label lists; tensor data contain the left
    and right descriptions. These data describe a basis vertex, not only its
    weight, since distinct vertices may have the same weight.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,2}})
    crystalVertexData b
  Text
    The list {{1,2}} is a one-row tableau with entries 1 and 2. It describes
    the middle vertex in $B(2)$, whose Dynkin-label weight is $(0)$.
SeeAlso
  "Finite highest-weight crystals"
  crystalVertex
  crystalWeight
///

doc ///
Key
  crystalVertex
  (crystalVertex,Crystal,List)
Headline
  Find a vertex from its combinatorial data
Usage
  b = crystalVertex(B,data)
Inputs
  B:Crystal
    a finite crystal
  data:List
    a valid vertex description for the crystal model
Outputs
  b:CrystalVertex
    the existing vertex with those data
Description
  Text
    The command looks up a vertex in B; it does not add a new vertex. Tableau
    data are row lists, path data are parameter-point pairs, minuscule data
    are weight-coordinate lists, and tensor data are pairs of factor
    descriptions. Invalid data give an error.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,2}})
    datumCoordinates crystalWeight b
  Text
    The tableau with row $(1,2)$ has one entry of each kind, so its $A_1$
    weight is $(0)$. This is the middle vertex of the three-dimensional
    representation, not the highest vertex.
SeeAlso
  "Finite highest-weight crystals"
  crystalVertexData
  crystalVertices
///

doc ///
Key
  crystalVertices
  (crystalVertices,Crystal)
Headline
  List every vertex of a finite crystal
Usage
  V = crystalVertices B
Inputs
  B:Crystal
    a finite crystal
Outputs
  V:List
    all of its CrystalVertex objects
Description
  Text
    Each vertex counts once, even if several vertices have the same weight.
    Consequently the length of this list equals the representation dimension
    for a highest-weight crystal, and equals the product of the vertex counts
    for a tensor crystal.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2})
    sort apply(crystalVertices B,b -> datumCoordinates crystalWeight b)
  Text
    The weights are $(-2),(0),(2)$, one vertex each. Thus $B(2)$ has three
    vertices and its character is $e^{(2)}+1+e^{(-2)}$.
SeeAlso
  "Finite highest-weight crystals"
  crystalCharacter
  crystalVertex
///

doc ///
Key
  crystalContains
  (crystalContains,Crystal,CrystalVertex)
Headline
  Test whether a vertex belongs to a crystal
Usage
  b0 = crystalContains(B,b)
Inputs
  B:Crystal
    the proposed parent crystal
  b:CrystalVertex
    a vertex
Outputs
  b0:Boolean
    whether b belongs to B
Description
  Text
    Membership includes the parent crystal, not merely a matching weight or
    tableau. This is useful when working simultaneously with irreducible and
    tensor crystals.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2})
    b = crystalVertex(B,{{1,2}})
    C = highestWeightCrystal(D,{1})
    (crystalContains(B,b),crystalContains(C,b))
  Text
    The pair $(true,false)$ says that the weight-zero tableau belongs to
    $B(2)$ and not to $B(1)$. In particular it cannot be passed as a vertex of
    the two-dimensional crystal.
SeeAlso
  "Finite highest-weight crystals"
  crystalParent
  crystalVertex
///

doc ///
Key
  crystalTableau
  (crystalTableau,CrystalVertex)
Headline
  Display a tableau vertex as rows
Usage
  T = crystalTableau b
Inputs
  b:CrystalVertex
    a vertex in the semistandard-tableau model
Outputs
  T:List
    the tableau rows
Description
  Text
    This accessor is specific to the tableau model used in connected finite
    type A. Other crystal models use crystalVertexData. Entries weakly
    increase along rows and strictly increase down columns; the crystal
    operators change entries according to the tableau signature rule.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,1});
    b = crystalVertex(B,{{1,1},{2}})
    crystalTableau crystalF(b,1)
  Text
    The rows {{1,2},{2}} give the tableau with top row $(1,2)$ and lower entry
    $2$. It is obtained from the highest tableau by one lowering operation of
    color $1$, so its weight is $(-1,2)$.
SeeAlso
  "Finite highest-weight crystals"
  crystalVertexData
  crystalF
///

doc ///
Key
  crystalWeight
  (crystalWeight,Crystal,CrystalVertex)
  (crystalWeight,CrystalVertex)
Headline
  Read the weight of a crystal vertex
Usage
  mu = crystalWeight b
Inputs
  b:CrystalVertex
    a vertex; crystalWeight(B,b) also checks the specified parent
Outputs
  mu:KMWeight
    its weight in the crystal root datum
Description
  Text
    For tableaux the weight is determined by entry counts; for paths it is the
    endpoint; for minuscule orbits it is the orbit weight. For a tensor vertex
    it is the sum of factor weights. Complete central coordinates from the
    highest weight are retained at every vertex.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,1});
    b = crystalVertex(B,{{1,2},{2}})
    datumCoordinates crystalWeight b
  Text
    The tuple $(-1,2)$ gives its evaluations on the two simple coroots. It
    equals the highest weight $(1,1)$ minus the first Cartan column $(2,-1)$,
    because this vertex is obtained by applying the first lowering operator.
SeeAlso
  "Finite highest-weight crystals"
  datumCoordinates
  crystalVertex
///

doc ///
Key
  crystalF
  (crystalF,Crystal,CrystalVertex,Thing)
  (crystalF,CrystalVertex,Thing)
Headline
  Apply a Kashiwara lowering operator
Usage
  b1 = crystalF(b,i)
Inputs
  b:CrystalVertex
    the starting vertex; crystalF(B,b,i) also checks its parent
  i:Thing
    a 1-based simple-root position or a node label
Outputs
  b1:Thing
    the next CrystalVertex, or null at the lower end of the string
Description
  Text
    A nonzero lowering operation of color i subtracts the corresponding simple
    root from the weight. It is a partial inverse of crystalE. Node positions
    take precedence over labels; a nonpositional argument is looked up among
    the datum labels.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,1}})
    b1 = crystalF(b,1);
    (crystalTableau b1,datumCoordinates crystalWeight b1)
  Text
    The row $(1,2)$ and weight $(0)$ show one step down the unique simple-root
    string. The highest weight $(2)$ decreases by the simple-root weight $(2)$
    to $(0)$.
SeeAlso
  "Finite highest-weight crystals"
  crystalE
  crystalPhi
///

doc ///
Key
  crystalE
  (crystalE,Crystal,CrystalVertex,Thing)
  (crystalE,CrystalVertex,Thing)
Headline
  Apply a Kashiwara raising operator
Usage
  b0 = crystalE(b,i)
Inputs
  b:CrystalVertex
    the starting vertex; crystalE(B,b,i) also checks its parent
  i:Thing
    a 1-based simple-root position or a node label
Outputs
  b0:Thing
    the preceding CrystalVertex, or null at the upper end of the string
Description
  Text
    A nonzero raising operation adds the corresponding simple root to the
    weight and reverses a lowering arrow of the same color. At the upper end
    of a simple-root string it returns null. Positions are numbered from $1$
    and take precedence over node labels.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,2}})
    b0 = crystalE(b,1);
    (crystalTableau b0,datumCoordinates crystalWeight b0,crystalE(b0,1) === null)
  Text
    The raised tableau row $(1,1)$ has weight $(2)$, the highest vertex. The final
    true verifies that there is no further raising step of color $1$.
SeeAlso
  "Finite highest-weight crystals"
  crystalF
  crystalEpsilon
///

doc ///
Key
  crystalEpsilon
  (crystalEpsilon,CrystalVertex,Thing)
Headline
  Count available raising steps on a crystal string
Usage
  m = crystalEpsilon(b,i)
Inputs
  b:CrystalVertex
    a vertex
  i:Thing
    a simple-root position or node label
Outputs
  m:ZZ
    the number of successive raising operations before reaching null
Description
  Text
    This is the simple-root string parameter epsilon. It counts the distance
    from b to the upper end of the i-string, not the multiplicity of its
    weight space. The difference phi minus epsilon equals the simple-coroot
    evaluation of the weight.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,2}})
    (crystalEpsilon(b,1),crystalPhi(b,1))
  Text
    The pair $(1,1)$ says that this weight-zero vertex is one raising step
    below the top and one lowering step above the bottom. Their difference is
    zero, its Dynkin label.
SeeAlso
  "Finite highest-weight crystals"
  crystalPhi
  crystalE
///

doc ///
Key
  crystalPhi
  (crystalPhi,CrystalVertex,Thing)
Headline
  Count available lowering steps on a crystal string
Usage
  m = crystalPhi(b,i)
Inputs
  b:CrystalVertex
    a vertex
  i:Thing
    a simple-root position or node label
Outputs
  m:ZZ
    the number of successive lowering operations before reaching null
Description
  Text
    This is the simple-root string parameter phi. Together with epsilon it
    determines which factor a Kashiwara operator acts on in a tensor product.
    The relation phi minus epsilon equals the simple-coroot evaluation of the
    vertex weight.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2});
    b = crystalVertex(B,{{1,1}})
    (crystalPhi(b,1),crystalEpsilon(b,1))
  Text
    The pair $(2,0)$ says that the highest vertex admits two lowering steps
    and no raising step. Its Dynkin label is $2-0=2$, agreeing with its
    highest weight $(2)$.
SeeAlso
  "Finite highest-weight crystals"
  crystalEpsilon
  tensorProductCrystal
///

doc ///
Key
  crystalEdges
  (crystalEdges,Crystal)
Headline
  List all colored lowering arrows
Usage
  E = crystalEdges B
Inputs
  B:Crystal
    a finite crystal
Outputs
  E:List
    tables with source, target, node, and nodeLabel for each nonzero lowering arrow
Description
  Text
    Every nonzero simple lowering operation appears once. The node entry is
    its 1-based color position, while nodeLabel gives the selected datum
    label. Distinct arrows may have the same color; weights alone need not
    identify their endpoints when weight multiplicities exceed one.
  Example
    D = rootDatum "A1";
    B = highestWeightCrystal(D,{2})
    E = crystalEdges B;
    apply(E,e -> {e#"node",datumCoordinates crystalWeight e#"source",datumCoordinates crystalWeight e#"target"})
  Text
    There are two arrows of color $1$, from $(2)$ to $(0)$ and from $(0)$ to
    $(-2)$. Each subtracts the simple-root weight $(2)$, giving the full
    three-vertex $A_1$ string.
SeeAlso
  "Finite highest-weight crystals"
  crystalF
  crystalVertices
///

doc ///
Key
  crystalCharacter
  (crystalCharacter,Crystal)
Headline
  Count crystal vertices by weight
Usage
  C = crystalCharacter B
Inputs
  B:Crystal
    a finite crystal
Outputs
  C:FormalCharacter
    the exact sum of one weight exponential per vertex
Description
  Text
    The coefficient of a weight is the number of vertices with that weight.
    For a highest-weight crystal this is its representation character; for a
    tensor crystal it is the product of the factor characters. The result is
    an exact character, not a root-depth approximation.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,1})
    C = crystalCharacter B
    (characterDimension C,characterCoefficient(C,kmWeight(D,{0,0})),C == finiteCharacter(D,{1,1}))
  Text
    The triple $(8,2,true)$ recovers the dimension and zero-weight
    multiplicity of the $A_2$ adjoint representation and verifies agreement of
    all weight coefficients with its highest-weight character.
SeeAlso
  "Finite highest-weight crystals"
  finiteCharacter
  crystalWeight
///

doc ///
Key
  infrastructureCacheInfo
Headline
  Count results retained for repeated calculations
Usage
  H = infrastructureCacheInfo()
Outputs
  H:HashTable
    counts of saved Cartan, affine, character, polynomial, and crystal results
Description
  Text
    This function takes no arguments and does not change any mathematical
    object. Repeated calculations can reuse results retained during the
    current Macaulay2 session. The entries count those results, not roots or
    representations. Finite-representation and root-snapshot tables are
    counted separately by representationCacheInfo and rootSnapshotCacheInfo.
  Example
    clearInfrastructureCaches();
    H = infrastructureCacheInfo();
    (H#"crystalData",H#"vermaPartitions",H#"characterParents")
  Text
    The triple $(0,0,0)$ records that no crystal results, Verma partition
    tables, or character-datum comparisons remain immediately after clearing
    these saved results. It is a statement about retained calculations, not a
    zero-dimensional representation.
SeeAlso
  "Saved results and repeated calculations"
  clearInfrastructureCaches
///

doc ///
Key
  clearInfrastructureCaches
Headline
  Discard retained intermediate results without changing mathematical objects
Usage
  clearInfrastructureCaches()
Description
  Text
    This function takes no arguments and returns null. It clears the retained
    affine, Cartan-datum, weight-display, Kazhdan--Lusztig, crystal, Verma
    partition, and character-parent results. Existing root data, characters,
    modules, and crystals remain valid; later calculations compute any
    required results again. The separate finite-representation and
    root-snapshot tables have their own clearing commands.
  Example
    D = rootDatum "A1";
    C = finiteCharacter(D,{2});
    clearInfrastructureCaches();
    (characterDimension C,C == finiteCharacter(D,{2}))
  Text
    The pair $(3,true)$ confirms that clearing retained computations does not
    alter the three-dimensional character or its weight coefficients.
SeeAlso
  "Saved results and repeated calculations"
  infrastructureCacheInfo
  clearRepresentationCache
  clearRootSnapshotCache
///

doc ///
Key
  affineStructureCacheInfo
Headline
  Count saved affine structural results
Usage
  H = affineStructureCacheInfo()
Outputs
  H:HashTable
    counts of saved matrices, affine validations, marks, comarks, and type descriptions
Description
  Text
    This function takes no arguments. The counts describe affine structural
    calculations retained in the current session. They do not count roots or
    weight multiplicities; use rootSnapshotCacheInfo for retained root
    calculations.
  Example
    clearAffineStructureCache();
    affineMarks cartanAffineA 1;
    H = affineStructureCacheInfo();
    (H#"cartanMatrices",H#"validatedAffineMatrices",H#"marks")
  Text
    The triple $(1,1,1)$ says that one Cartan matrix was retained, verified to
    be affine, and used to compute its marks. It does not say that the affine
    algebra has only one root.
SeeAlso
  "Saved results and repeated calculations"
  clearAffineStructureCache
  affineMarks
///

doc ///
Key
  clearAffineStructureCache
Headline
  Discard saved affine invariants
Usage
  clearAffineStructureCache()
Description
  Text
    This function takes no arguments and returns null. Only saved affine
    structural results are cleared. Matrices, root data, weights, and
    characters already constructed remain unchanged; subsequent affine
    invariant calculations recompute the information.
  Example
    A = cartanAffineA 1;
    a = affineMarks A;
    clearAffineStructureCache();
    H = affineStructureCacheInfo();
    (H#"marks",affineMarks A == a)
  Text
    The pair $(0,true)$ says that the saved marks were removed, and that
    recomputing them gives the same mathematical answer. Here the marks are
    $(1,1)$, the coefficients of the null root in the simple-root basis.
SeeAlso
  "Saved results and repeated calculations"
  affineStructureCacheInfo
  affineMarks
///

doc ///
Key
  CartanDatum
Headline
  A generalized Cartan matrix with its node labels and type
Description
  Text
    A @TO CartanDatum@ records a symmetrizable generalized Cartan matrix,
    its node labels, and its finite, affine, or indefinite type.  Construct
    one with @TT "cartanDatum A"@ for a matrix, or with a finite type name
    such as @TT "\"A2\""@.  Node order is the order of the matrix rows and
    columns; labels do not change that order.
  Example
    D = cartanDatum("A2",NodeLabels=>{"left","right"});
    D
    datumCartanMatrix D
  Text
    This is type $A_2$.  Its matrix has diagonal entries $2$ and
    off-diagonal entries $-1$, meaning
    $\langle\alpha_2,\alpha_1^\vee\rangle=
      \langle\alpha_1,\alpha_2^\vee\rangle=-1$.
    The names ``left'' and ``right'' label these two simple roots.
SeeAlso
  cartanDatum
  datumCartanMatrix
  RootDatum
///

doc ///
Key
  Realization
Headline
  Simple roots and coroots in a specified Cartan space
Description
  Text
    A @TO Realization@ specifies rational coordinates for the simple roots
    in $\mathfrak h^*$ and simple coroots in the dual space $\mathfrak h$.
    Their pairing is the Cartan matrix.  Use @TO standardRealization@ for
    a minimal realization, or @TO realization@ to supply the root and
    coroot matrices in a chosen pair of dual bases.
  Example
    R = standardRealization cartanAffineA 1;
    (R,R#"dimension")
  Text
    There are two simple roots, but $\dim\mathfrak h=3$.  The affine
    Cartan matrix has corank $1$, so its minimal realization has dimension
    $2+1=3$.  The extra direction is essential: Dynkin labels alone do
    not determine a complete affine weight.
SeeAlso
  standardRealization
  realization
  realizationCoordinates
  RootDatum
///

doc ///
Key
  RootDatum
Headline
  A realization with character and cocharacter lattices
Description
  Text
    A @TO RootDatum@ records a realization and a chosen pair of dual
    integral lattices.  Roots, weights, and characters retain this datum,
    so calculations do not silently identify different realizations or
    different global forms.  Construct it with @TO rootDatum@ from a
    matrix, finite type name, Cartan datum, or realization.
  Example
    D = rootDatum("A2",GlobalForm=>"Adjoint");
    (integratesToGlobalForm kmWeight(D,{1,0}),
        integratesToGlobalForm kmWeight(D,{1,1}))
  Text
    The output is @TT "(false,true)"@.  The fundamental weight $(1,0)$
    is not in the adjoint character lattice, whereas $(1,1)$ is:
    in type $A_2$ it equals $\alpha_1+\alpha_2$.
    Both are valid Lie-algebra weights, but only the latter integrates
    to the chosen adjoint group.
SeeAlso
  rootDatum
  GlobalForm
  CharacterLattice
  integratesToGlobalForm
///

doc ///
Key
  KMRoot
Headline
  An element of a specified root lattice
Description
  Text
    A @TO KMRoot@ is an integral element of the root lattice, recorded
    together with its @TO RootDatum@.  In @TT "kmRoot(D,b)"@ the entries
    of @TT "b"@ are coefficients of the simple roots, not Dynkin labels.
    The constructor does not assert that this lattice element is a root
    of the Lie algebra; @TO rootMultiplicity@ can test that question
    within a computed root range.
  Example
    D = rootDatum "A2";
    beta = kmRoot(D,{1,1});
    (beta,datumCoordinates toWeight beta)
  Text
    The first tuple denotes $\beta=\alpha_1+\alpha_2$.
    Converting to a weight gives Dynkin labels $(1,1)$, since
    $\langle\beta,\alpha_1^\vee\rangle=
      \langle\beta,\alpha_2^\vee\rangle=1$.
    The coordinate lists happen to agree here; they describe different
    bases and need not agree in general.
SeeAlso
  kmRoot
  toWeight
  KMWeight
  rootMultiplicity
///

doc ///
Key
  KMCoroot
Headline
  An element of a specified coroot lattice
Description
  Text
    A @TO KMCoroot@ is an integral combination of simple coroots in one
    @TO RootDatum@.  Construct it with @TT "kmCoroot(D,b)"@, where
    @TT "b"@ gives simple-coroot coefficients.  These coefficients are
    distinct from the evaluations of a coweight on the simple roots.
  Example
    D = rootDatum "A2";
    beta = kmCoroot(D,{1,1});
    (beta,datumCoordinates toCoweight beta)
  Text
    The coroot is $\alpha_1^\vee+\alpha_2^\vee$.
    Its associated coweight evaluates to $1$ on each simple root, so the
    second list is $(1,1)$.  @TO toCoweight@ preserves the element while
    expressing it by these evaluations.
SeeAlso
  kmCoroot
  toCoweight
  KMCoweight
///

doc ///
Key
  KMWeight
Headline
  A weight, or an explicitly marked Dynkin-label class
Description
  Text
    A @TO KMWeight@ belongs to a specified @TO RootDatum@.
    @TT "kmWeight(D,a)"@ supplies its Dynkin labels
    $a_i=\langle\lambda,\alpha_i^\vee\rangle$.
    When these labels do not determine a complete weight, the result is
    marked as a class modulo the annihilator of the simple coroots.
    Use @TO weightFromRealizationCoordinates@ to specify the missing
    coordinates as well.
  Example
    D = rootDatum "A2";
    lambda = kmWeight(D,{1,0});
    (lambda,pairing(lambda,kmCoroot(D,{1,0})))
  Text
    This is the first fundamental weight: its Dynkin tuple is $(1,0)$.
    The final entry $1$ is its value on the first simple coroot.
    In this minimal finite realization the labels determine the complete
    weight, so no additional coordinates are required.
SeeAlso
  kmWeight
  weightFromRealizationCoordinates
  datumCoordinates
  realizationCoordinates
  pairing
///

doc ///
Key
  KMCoweight
Headline
  A coweight, or an explicitly marked simple-root evaluation class
Description
  Text
    A @TO KMCoweight@ belongs to a specified @TO RootDatum@.
    @TT "kmCoweight(D,a)"@ supplies the evaluations
    $a_i=\langle\alpha_i,\nu\rangle$.
    If these do not determine a complete coweight, the result is a class
    modulo the common kernel of the simple roots.  A complete vector can
    instead be supplied to @TO coweightFromRealizationCoordinates@.
  Example
    D = rootDatum "A2";
    nu = kmCoweight(D,{1,0});
    (nu,pairing(kmRoot(D,{1,0}),nu))
  Text
    The tuple $(1,0)$ is the first fundamental coweight.
    Its value on $\alpha_1$ is $1$, and its value on $\alpha_2$ is $0$.
    The displayed pairing verifies the first of these evaluations.
SeeAlso
  kmCoweight
  coweightFromRealizationCoordinates
  pairing
  KMCoroot
///

doc ///
Key
  KMWeylGroup
Headline
  The Weyl group of specified Cartan data
Description
  Text
    A @TO KMWeylGroup@ records the Cartan datum of a Weyl group.
    Construct it with @TO weylGroup@ from a matrix, type name, Cartan
    datum, realization, or root datum.  Its simple generators follow
    the node order of that datum.  Elements are constructed separately
    by @TO weylElement@, without requiring enumeration of the group.
  Example
    D = rootDatum "A2";
    W = weylGroup D;
    W
    weylGroupOrder datumCartanMatrix D
  Text
    This is the Weyl group of type $A_2$, isomorphic to $S_3$.
    Its order is $6$.  Its two simple reflections correspond to the two
    simple roots, and their product has order $3$.
SeeAlso
  weylGroup
  WeylElement
  weylElement
  coxeterMatrix
///

doc ///
Key
  StandardParabolic
Headline
  A choice of retained Levi nodes in a Cartan datum
Description
  Text
    A @TO StandardParabolic@ records a standard parabolic by specifying
    the nodes retained in its Levi factor and their complementary removed
    nodes.  Construct it with @TO standardParabolic@.
    Its positional list denotes removed nodes; the options
    @TO RemovedNodes@ and @TO LeviNodes@ make either choice explicit.
  Example
    D = rootDatum "A2";
    P = standardParabolic(D,LeviNodes=>{1});
    (removedNodes P,leviNodes P)
  Text
    The output @TT "({2},{1})"@ says that node $2$ is removed and node
    $1$ retained.  The derived Levi algebra is $\mathfrak{sl}_2$.
    The nilradical consists of the root spaces for $\alpha_2$ and
    $\alpha_1+\alpha_2$.
SeeAlso
  standardParabolic
  removedNodes
  leviNodes
  ReductiveLeviDatum
///

doc ///
Key
  FormalCharacter
Headline
  A finite character sum or a specified portion of an infinite character
Description
  Text
    A @TO FormalCharacter@ records a sum $\sum_\mu c_\mu e^\mu$,
    with integer coefficients and weights in one @TO RootDatum@.
    The term $c_\mu e^\mu$ records a weight-space multiplicity when the
    sum is an actual representation character.  Negative coefficients
    are allowed in formal differences.  The character also records
    whether the sum is complete or only partially known.
  Example
    D = rootDatum "A2";
    C = finiteCharacter(D,{1,0});
    C
    characterDimension(C*C)
  Text
    The three displayed weights are $(1,0)$, $(-1,1)$, and $(0,-1)$,
    each with multiplicity $1$.  Thus $C$ is the character of the
    standard three-dimensional representation $V$ of
    $\mathfrak{sl}_3$.  Multiplication gives tensor products, so the
    final output is $\dim(V\otimes V)=9$.
  Text
    Addition, subtraction, integer scaling, multiplication, and
    nonnegative powers preserve the underlying datum.  Characters must
    have compatible full-weight or Dynkin-label-class coordinates.
    An omitted coefficient of a partial character is not presumed zero.
    See @TO "Working with formal characters"@ for arithmetic and
    irreducible decompositions, and @TO Truncation@ for partial sums.
SeeAlso
  formalCharacter
  finiteCharacter
  characterTerms
  isExactCharacter
  LeviFormalCharacter
///

doc ///
Key
  KMRepresentation
Headline
  A highest-weight module in a specified root datum
Description
  Text
    A @TO KMRepresentation@ specifies a highest-weight module by its
    datum, highest weight, and kind.  Use @TO vermaModule@ for a Verma
    module or @TO integrableHighestWeightModule@ for an integrable module
    with dominant integral highest weight.  @TO moduleCharacter@ computes
    these characters, with a root-depth bound when required.
    @TO simpleHighestWeightModule@ can also specify a simple highest-weight
    module; this does not by itself compute its character, and
    @TT "moduleCharacter"@ is not defined for that module kind.
  Example
    D = rootDatum "A1";
    M = vermaModule(D,{2});
    M
    moduleCharacter(M,Depth=>2)
  Text
    The module is $M(2)$, the $\mathfrak{sl}_2$ Verma module whose
    highest weight has Dynkin label $2$.  Since the simple root has
    Dynkin label $2$, its character begins
    $e^{(2)}+e^{(0)}+e^{(-2)}+\cdots$.
    All these weight spaces have dimension $1$; the ellipsis records
    the terms below root depth $2$, not a finite-dimensional module.
SeeAlso
  vermaModule
  integrableHighestWeightModule
  simpleHighestWeightModule
  moduleCharacter
///

doc ///
Key
  ReductiveLeviDatum
Headline
  The derived Levi algebra together with its full center
Description
  Text
    A @TO ReductiveLeviDatum@ records a standard Levi subalgebra of an
    ambient @TO RootDatum@.  Construct it with
    @TT "reductiveLeviDatum(P,D)"@, using a standard parabolic @TT "P"@
    in @TT "D"@.  It retains both the derived Levi root datum and a
    rational basis of the Levi center.  The retained Cartan matrix must
    be nonsingular so that these coordinates determine the full weight.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,{2}),D);
    leviCenterBasis L
  Text
    The row $(1/2,1)$ spans the Levi center in the ambient Cartan-space
    coordinates.  Indeed, the retained simple root has coordinates
    $(2,-1)$, and $2(1/2)-1=0$.
    This Levi has derived algebra $\mathfrak{sl}_2$ and a
    one-dimensional center.  The numerical central character of a weight
    is its evaluation on this displayed basis vector.
SeeAlso
  reductiveLeviDatum
  leviCenterBasis
  LeviWeight
  restrictCharacter
///

doc ///
Key
  LeviWeight
Headline
  A Levi weight with its central character retained
Description
  Text
    A @TO LeviWeight@ records a weight of the derived Levi algebra
    together with its evaluations on the chosen Levi-center basis.
    It belongs to one @TO ReductiveLeviDatum@.  Obtain it by applying
    @TO restrictWeight@ to a complete ambient weight.  The printed tuple
    lists the Levi Dynkin labels first, then the central evaluations.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,{2}),D);
    mu = restrictWeight(L,kmWeight(D,{1,0}));
    (mu,datumCoordinates semisimplePart mu,centralCharacter mu)
  Text
    The Levi weight is $(1,1/2)$.  Its derived $A_1$ Dynkin label is $1$,
    and the center acts with value $1/2$ on the chosen center basis.
    These are different kinds of coordinates, not a two-node Dynkin
    tuple.  Together they determine the restriction of the complete
    ambient weight $(1,0)$.
SeeAlso
  restrictWeight
  semisimplePart
  centralCharacter
  LeviHighestWeight
///

doc ///
Key
  LeviHighestWeight
Headline
  The highest weight and central character of an irreducible Levi summand
Description
  Text
    A @TO LeviHighestWeight@ is a key in the output of
    @TO decomposeLeviCharacter@.  It specifies a dominant highest weight
    for the derived Levi algebra and a character of its center.
    The printed expression @TT "V(a,z)"@ denotes that irreducible
    summand, with the Levi Dynkin coordinates followed by central
    evaluations.  The corresponding tally value is its multiplicity.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,{2}),D);
    decomposeLeviCharacter restrictCharacter(L,finiteCharacter(D,{1,0}))
  Text
    The two entries mean
    $$V_{A_2}(1,0)\big|_{\mathfrak l}
      \cong V_{A_1}(1)_{1/2}\oplus V_{A_1}(0)_{-1}.$$
    The first summand has dimension $2$ and central value $1/2$;
    the second has dimension $1$ and central value $-1$.
    Each occurs once.  The subscripts refer to the basis returned by
    @TT "leviCenterBasis L"@.
SeeAlso
  decomposeLeviCharacter
  semisimplePart
  centralCharacter
  LeviWeight
///

doc ///
Key
  LeviFormalCharacter
Headline
  A character expressed in full reductive-Levi coordinates
Description
  Text
    A @TO LeviFormalCharacter@ records a sum of weight exponentials
    with @TO LeviWeight@ exponents in one @TO ReductiveLeviDatum@.
    These retain the center as well as the derived Levi coordinates.
    Obtain one by restricting a @TO FormalCharacter@ with
    @TO restrictCharacter@.  Exactness or a specified partial range is
    retained from the ambient character.
  Example
    D = rootDatum "A2";
    L = reductiveLeviDatum(standardParabolic(D,{2}),D);
    C = restrictCharacter(L,finiteCharacter(D,{1,0}));
    C
    characterDimension(C*C)
  Text
    The character is
    $e^{(1,1/2)}+e^{(-1,1/2)}+e^{(0,-1)}$.
    The first coordinate is the $A_1$ Dynkin label and the second the
    central evaluation.  It has dimension $3$, and its square has
    dimension $9$: this is the character of the tensor square of the
    restricted representation.
  Text
    Addition, subtraction, negation, integer scaling, multiplication,
    and nonnegative powers are defined for compatible Levi data.
    Multiplication adds both the Levi weights and their central
    characters.  Partial expansions retain their known coefficient
    range; they are not treated as complete characters.
SeeAlso
  restrictCharacter
  characterTerms
  characterCoefficient
  decomposeLeviCharacter
  FormalCharacter
///

doc ///
Key
  Crystal
Headline
  A finite highest-weight crystal or tensor-product crystal
Description
  Text
    A @TO Crystal@ is a finite colored directed graph encoding weight
    spaces and the Kashiwara raising and lowering operators of a
    representation.  Each vertex contributes one weight exponential to
    its character.  Use @TO highestWeightCrystal@ for any finite-type
    dominant integral highest weight, @TO minusculeCrystal@ for a
    minuscule orbit model, or @TO tensorProductCrystal@ for a tensor
    product.  The datum and vertex model are retained.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,0});
    (B,#crystalVertices B)
  Text
    This is $B(1,0)$, the crystal of the standard representation of
    $\mathfrak{sl}_3$.  Its three vertices have the weights
    $(1,0)$, $(-1,1)$, and $(0,-1)$, each once.
    Thus its crystal character is the ordinary character of a
    three-dimensional irreducible representation.
SeeAlso
  highestWeightCrystal
  minusculeCrystal
  tensorProductCrystal
  crystalCharacter
  CrystalVertex
///

doc ///
Key
  CrystalVertex
Headline
  A vertex in a specified crystal
Description
  Text
    A @TO CrystalVertex@ records both its crystal and its tableau, path,
    or tensor-product data.  Obtain vertices from @TO crystalVertices@
    or use @TT "crystalVertex(B,data)"@ to select a vertex of @TT "B"@.
    A weight alone need not determine a vertex, because several vertices
    can have the same weight.  @TO crystalE@ and @TO crystalF@ act on
    vertices, returning @TT "null"@ when the indicated operator vanishes.
  Example
    D = rootDatum "A2";
    B = highestWeightCrystal(D,{1,0});
    b = crystalVertex(B,{{1}})
    (crystalTableau b,datumCoordinates crystalWeight b,
        datumCoordinates crystalWeight crystalF(b,1))
  Text
    The one-box tableau contains $1$, and its weight is $(1,0)$.
    Applying $f_1$ changes the entry to $2$ and lowers the weight to
    $(-1,1)=(1,0)-\alpha_1$, where the first simple root has Dynkin
    labels $(2,-1)$.
SeeAlso
  crystalVertex
  crystalVertices
  crystalVertexData
  crystalWeight
  crystalF
///

doc ///
Key
  BorelWeilBottResult
Headline
  The nonzero flag-variety cohomology, or its vanishing
Description
  Text
    A @TO BorelWeilBottResult@ is returned by @TO borelWeilBott@.
    It records whether all cohomology vanishes and, otherwise, the one
    nonzero cohomological degree and irreducible highest weight.  The
    entries @TT "degree"@, @TT "cohomologyHighestWeight"@, and
    @TT "weylElement"@ retain the degree, weight, and dot-action witness.
  Example
    D = rootDatum "A1";
    R = borelWeilBott(D,{-3});
    R
    (R#"degree",datumCoordinates R#"cohomologyHighestWeight")
  Text
    The output is $H^1=V(1)$ and @TT "(1,{1})"@.
    In type $A_1$ the flag variety is $\mathbb P^1$, and the input
    describes $\mathcal O(-3)$.  Its $H^1$ is the two-dimensional
    irreducible $\mathfrak{sl}_2$ representation of highest weight $1$.
    All other cohomological degrees vanish.
SeeAlso
  borelWeilBott
  borelWeilBottCharacter
  LineBundleConvention
///

doc ///
Key
  BGGResolution
Headline
  The Verma-module terms of a BGG resolution
Description
  Text
    A @TO BGGResolution@ records the terms of the BGG resolution of a
    dominant integral highest-weight module.  Use @TO bggResolution@
    with a root datum and highest weight.  The degree-$k$ term is the
    sum of the Verma modules $M(w\mathbin{\cdot}\lambda)$ with
    $\ell(w)=k$.  Differential maps are not computed.  In nonfinite type,
    the terms are computed only through a specified @TO MaxDegree@.
  Example
    D = rootDatum "A1";
    B = bggResolution(D,{2});
    B
  Text
    The output lists $C_0=M(2)$ and $C_1=M(-4)$.
    This is the resolution
    $$0\longrightarrow M(-4)\longrightarrow M(2)
      \longrightarrow L(2)\longrightarrow0.$$
    The reflected dot-action weight is $s\mathbin{\cdot}2=-4$,
    and $L(2)$ is the three-dimensional irreducible representation of
    $\mathfrak{sl}_2$.  The displayed object specifies these terms,
    not the arrow maps.
SeeAlso
  bggResolution
  bggEulerCharacter
  dotWeylAction
///

doc ///
Key
  KazhdanLusztigPolynomialRing
Headline
  The ordinary integer polynomial ring used for default Kazhdan--Lusztig answers
Description
  Text
    @TO KazhdanLusztigPolynomialRing@ is the polynomial ring $\mathbb Z[q]$,
    not a separate kind of polynomial.  The default outputs of
    @TO kazhdanLusztigPolynomial@ belong to this ring.  M2 displays the
    name of a polynomial's ring as its class, and ordinary polynomial
    operations apply.  Use @TT "Variable=>t"@ to obtain an answer in
    a polynomial ring of your own instead.
  Example
    q = kazhdanLusztigVariable();
    P = q^2+2*q+1;
    (P,factor P,class P)
  Text
    The polynomial is $q^2+2q+1=(q+1)^2$.
    Its class is @TT "KazhdanLusztigPolynomialRing"@ because that is the
    name of its coefficient ring $\mathbb Z[q]$.  The factorization is
    M2's usual polynomial factorization, with no conversion required.
SeeAlso
  kazhdanLusztigPolynomial
  kazhdanLusztigVariable
  kazhdanLusztigCoefficients
///

doc ///
Key
  GlobalForm
Headline
  Choose the integral lattices of a global form
Description
  Text
    The @TO GlobalForm@ option of @TO rootDatum@ chooses
    @TT "\"SimplyConnected\""@, @TT "\"Adjoint\""@, or
    @TT "\"Unspecified\""@.  The default is
    @TT "\"SimplyConnected\""@.  In semisimple finite type these choose,
    respectively, the weight lattice $P$, the root lattice $Q$, or no
    character lattice.  The cocharacter lattice is the dual chosen
    lattice.  @TO integratesToGlobalForm@ tests membership; with an
    unspecified lattice it returns @TT "null"@.
  Example
    Dsc = rootDatum("A2",GlobalForm=>"SimplyConnected");
    Dad = rootDatum("A2",GlobalForm=>"Adjoint");
    (integratesToGlobalForm kmWeight(Dsc,{1,0}),
        integratesToGlobalForm kmWeight(Dad,{1,0}))
  Text
    The output @TT "(true,false)"@ says that the standard
    three-dimensional $\mathfrak{sl}_3$ representation integrates to
    $SL_3$ but not to its adjoint quotient $PGL_3$.
    The Lie-algebra highest weight $(1,0)$ is valid in either datum;
    the chosen group changes its integrality requirement.
  Text
    In general realizations the option specifies the package's integral
    lattices.  It does not classify all possible global Kac--Moody groups
    or all actions of additional central tori.  Use @TO CharacterLattice@
    to specify an integral lattice directly in realization coordinates.
SeeAlso
  rootDatum
  CharacterLattice
  integratesToGlobalForm
  characterLatticeBasis
///

doc ///
Key
  CharacterLattice
Headline
  Supply a character lattice in the realization basis
Description
  Text
    The @TO CharacterLattice@ option of @TO rootDatum@ accepts an
    invertible rational matrix whose rows form a basis of the character
    lattice in full realization coordinates.  The dual cocharacter
    lattice is determined from this choice.  The character lattice must
    contain the simple roots, and its dual must contain the simple
    coroots; these requirements are checked.  This is a lattice basis,
    not a list of highest weights or a Cartan matrix to be reinterpreted
    as a new Lie algebra.
  Example
    D = rootDatum("A2",CharacterLattice=>matrix(QQ,{{2,-1},{-1,2}}));
    (integratesToGlobalForm kmWeight(D,{1,0}),
        integratesToGlobalForm kmWeight(D,{1,1}))
  Text
    The rows are the simple-root vectors in the fundamental-weight
    basis of $A_2$, so the chosen lattice is $Q$.
    The answer @TT "(false,true)"@ says that $(1,0)$ is outside this
    lattice and $(1,1)=\alpha_1+\alpha_2$ belongs to it.
SeeAlso
  rootDatum
  characterLatticeBasis
  cocharacterLatticeBasis
  integratesToGlobalForm
///

doc ///
Key
  NodeLabels
Headline
  Name nodes without changing Cartan-matrix order
Description
  Text
    The @TO NodeLabels@ option of @TO cartanDatum@ supplies a list of
    distinct labels, one per Cartan-matrix row and column.  Later Weyl,
    parabolic, and diagram commands retain these names.
    Positions $1$ through the rank are always interpreted as positions;
    use a position-label record when integer labels would otherwise be
    ambiguous.  Labels are chosen when the datum is constructed.
  Example
    D = cartanDatum("A2",NodeLabels=>{"left","right"});
    P = standardParabolic(D,RemovedNodes=>{"right"});
    (removedNodes P,leviNodes P)
  Text
    The result @TT "({2},{1})"@ identifies ``right'' with simple root
    $\alpha_2$.  Removing it retains the $A_1$ Levi at $\alpha_1$.
    The labels make the choice readable without changing the mathematical
    ordering of the two simple roots.
SeeAlso
  cartanDatum
  dynkinDiagram
  NodeFormat
  RemovedNodes
///

doc ///
Key
  NodeFormat
Headline
  Choose node positions, labels, or position-label records
Description
  Text
    The @TO NodeFormat@ option controls how selected nodes are reported
    by @TO minusculeNodes@, @TO cominusculeNodes@, and the Dynkin-diagram
    commands.  The choices are @TT "\"Positions\""@,
    @TT "\"Labels\""@, and @TT "\"Records\""@.
    Records retain both pieces of information and avoid ambiguity from
    custom integer labels.  A matrix without named Cartan data supports
    positions only.
  Example
    D = cartanDatum("B3",NodeLabels=>{"vector","middle","spin"});
    (minusculeNodes(D,NodeFormat=>"Positions"),
        minusculeNodes(D,NodeFormat=>"Labels"),
        minusculeNodes(D,NodeFormat=>"Records"))
  Text
    All three outputs identify the same node: position $3$, named
    ``spin''.  The final table pairs @TT "position => 3"@ with
    @TT "label => spin"@.  Its fundamental weight is the highest weight
    of the eight-dimensional minuscule spin representation of type $B_3$.
SeeAlso
  NodeLabels
  minusculeNodes
  cominusculeNodes
  dynkinDiagramData
///

doc ///
Key
  RemovedNodes
Headline
  Specify the nodes deleted from the Levi diagram
Description
  Text
    The @TO RemovedNodes@ option of @TO standardParabolic@ gives the
    simple-root nodes deleted to obtain the derived Levi diagram.
    Supply a list or set of positions, labels, or position-label records.
    This cannot be combined with @TO LeviNodes@ or with a positional
    second argument.  A node choice is required: use an empty removed-node
    list to retain the whole diagram.
  Example
    D = cartanDatum("A2",NodeLabels=>{"left","right"});
    P = standardParabolic(D,RemovedNodes=>{"right"});
    (removedNodes P,leviNodes P)
  Text
    The answer @TT "({2},{1})"@ removes $\alpha_2$ and retains
    $\alpha_1$.  The derived Levi is $A_1$; its complementary positive
    root spaces, for $\alpha_2$ and $\alpha_1+\alpha_2$, form the
    nilradical of this standard parabolic.
SeeAlso
  standardParabolic
  LeviNodes
  removedNodes
  leviNodes
///

doc ///
Key
  LeviNodes
Headline
  Specify the nodes retained in the Levi diagram
Description
  Text
    The @TO LeviNodes@ option of @TO standardParabolic@ gives the
    nodes retained in the derived Levi algebra.  Supply a list or set
    of positions, labels, or position-label records.  Its complement is
    the set of removed nodes.  It cannot be combined with
    @TO RemovedNodes@ or with a positional second argument.
  Example
    D = rootDatum "A2";
    P = standardParabolic(D,LeviNodes=>{1});
    (leviNodes P,removedNodes P)
  Text
    The answer @TT "({1},{2})"@ retains $\alpha_1$ and removes
    $\alpha_2$, so the derived Levi has type $A_1$.
    This is the same parabolic as
    @TT "standardParabolic(D,RemovedNodes=>{2})"@.
SeeAlso
  standardParabolic
  RemovedNodes
  leviNodes
  removedNodes
///

doc ///
Key
  CharacterStatus
Headline
  Declare a supplied character sum complete or partial
Description
  Text
    The @TO CharacterStatus@ option of @TO formalCharacter@ is
    @TT "\"Exact\""@ by default: the supplied table is a complete finite
    sum.  Choose @TT "\"Truncated\""@ for a partial expansion and supply
    @TO Truncation@ information as well.  Exactness means the sum is
    complete, not that it has been proved to be the character of a genuine
    representation.
  Example
    D = rootDatum "A1";
    C = formalCharacter(D,new Tally from {kmWeight(D,{0})=>1},
        CharacterStatus=>"Exact");
    (C,isExactCharacter C)
  Text
    The output is the character $1=e^{(0)}$, followed by
    @TT "true"@.  It is the complete character of the trivial
    representation: the zero weight has multiplicity $1$ and every
    other weight has multiplicity $0$.
SeeAlso
  formalCharacter
  Truncation
  Depth
  isExactCharacter
///

doc ///
Key
  Truncation
Headline
  Describe which part of a character is known
Description
  Text
    The @TO Truncation@ option of @TO formalCharacter@ accompanies
    @TT "CharacterStatus=>\"Truncated\""@.  It records the meaning of a
    partial table.  A mere table of displayed terms does not prove that
    absent coefficients vanish.  The module-character commands supply
    a checked root-depth description, including a highest weight and the
    depth through which all coefficients have been computed.
  Example
    D = rootDatum "A1";
    C0 = vermaCharacter(vermaModule(D,{2}),Depth=>2)
    C = formalCharacter(D,characterTerms C0,
        CharacterStatus=>"Truncated",Truncation=>C0#"truncation");
    (C,isExactCharacter C)
    characterCoefficient(C,kmWeight(D,{0}))
  Text
    The reconstructed expansion is
    $e^{(2)}+e^{(0)}+e^{(-2)}+\cdots$, complete through root depth $2$.
    It is not an exact finite character, and the coefficient at $(0)$
    is $1$.  Reusing the description is appropriate here because the
    supplied terms are exactly the same checked table; it must not be
    used to assert completeness for an unrelated or incomplete table.
  Text
    When entering a partial character by hand, a general description can
    be supplied without asserting a coefficient certificate.  Such a
    description does not make absent weights known.  Usually it is simpler
    to compute a larger portion with @TO moduleCharacter@ and @TO Depth@.
SeeAlso
  formalCharacter
  CharacterStatus
  Depth
  characterCoefficient
///

doc ///
Key
  Depth
Headline
  Bound the sum of simple-root coefficients below a highest weight
Description
  Text
    The @TO Depth@ option of @TO moduleCharacter@, @TO vermaCharacter@,
    and @TO integrableCharacter@ gives a nonnegative integer $H$.
    It requests every coefficient at
    $\lambda-\sum_i b_i\alpha_i$ with $b_i\geq0$ and
    $\sum_i b_i\leq H$.  This is root depth, not homological degree,
    and in affine type it is not the derivation degree alone.
  Example
    D = rootDatum "A1";
    M = vermaModule(D,{2});
    C = moduleCharacter(M,Depth=>2);
    C
  Text
    The three displayed weights are
    $2$, $2-\alpha_1=0$, and $2-2\alpha_1=-2$ in Dynkin coordinates.
    Thus the answer begins $e^{(2)}+e^{(0)}+e^{(-2)}$ and is complete
    at depths $0$, $1$, and $2$.  Further terms exist; increasing
    @TT "Depth"@ computes them rather than interpreting them as zero.
SeeAlso
  moduleCharacter
  vermaCharacter
  integrableCharacter
  MaxDegree
  affineDegree
///

doc ///
Key
  LineBundleConvention
Headline
  Fix the sign of the Borel--Weil--Bott line-bundle weight
Description
  Text
    The @TO LineBundleConvention@ option of @TO borelWeilBott@ and
    @TO borelWeilBottCharacter@ accepts only
    @TT "\"FiberMinusWeight\""@, which is also the default.
    The input weight $\lambda$ defines
    $\mathcal L_\lambda=\widetilde G\times^{\widetilde B}
      \mathbb C_{-\lambda}$ on the flag variety of the simply connected
    semisimple group.  Accordingly a dominant input gives the dual
    highest-weight representation in degree $0$.
  Example
    D = rootDatum "A2";
    R = borelWeilBott(D,{1,0},LineBundleConvention=>"FiberMinusWeight");
    R
    datumCoordinates R#"cohomologyHighestWeight"
  Text
    The result is $H^0=V(0,1)$, whose highest-weight tuple is $(0,1)$.
    It is the dual of the standard representation $V(1,0)$.
    This output makes the sign convention visible in a type where the
    standard representation is not self-dual.
  Text
    The calculation is a Lie-algebra cohomology representation and does
    not require the input weight to integrate to the chosen global form.
    Equivariance under that group additionally requires membership in
    its character lattice; see @TO borelWeilBott@.
SeeAlso
  borelWeilBott
  borelWeilBottCharacter
  GlobalForm
///

doc ///
Key
  "Command reference"
Headline
  Individual references for the mathematical commands
Description
  Text
    Each command has a separate reference page with its input conventions,
    a computed example, and an explanation of the mathematical answer.
    The topic guides give longer examples connecting several commands.
    Types and options are described on their own pages and linked from
    the relevant commands.
Subnodes
  :Cartan matrices, realizations, and lattices
    cartanA
    cartanAffineA
    cartanAffineB
    cartanAffineC
    cartanAffineD
    cartanAffineE
    cartanAffineF4
    cartanAffineG2
    cartanB
    cartanC
    cartanD
    cartanData
    cartanDatum
    cartanE
    cartanExtendUntwisted
    cartanF4
    cartanG2
    cartanOverExtended
    cartanPositiveRoots
    cartanPositiveRootsDegree
    cartanRank
    cartanRank2
    cartanStar
    cartanT
    cartanType
    cartanVeryExtended
    characterLatticeBasis
    cocharacterLatticeBasis
    datumCartanMatrix
    isCartanMatrix
    pModQInvariants
    realization
    rootDatum
    standardRealization
    symmetrizer
  :Root coordinates and finite root systems
    allRoots
    coordinateBasis
    corootFromLegacy
    coroots
    coweightBasis
    coweightFromLegacy
    coweightFromRealizationCoordinates
    coweightToCorootCoordinates
    datumCoordinates
    datumFromLegacyCartan
    dynkinDiagram
    dynkinDiagramData
    fromEpsilon
    fundamentalWeights
    integratesCoweightToGlobalForm
    integratesToGlobalForm
    isCorootLatticeCoweight
    isRootLatticeWeight
    kmCoroot
    kmCoweight
    kmRoot
    kmWeight
    negativeRoots
    pairing
    parentDatum
    positiveRootsA
    positiveRootsB
    positiveRootsC
    positiveRootsD
    positiveRootsE6
    positiveRootsE7
    positiveRootsE8
    positiveRootsF4
    positiveRootsG2
    realizationCoordinates
    rootFromLegacy
    rootHeight
    rootInnerProduct
    rootNormSquared
    simpleRoots
    simpleRootsA
    simpleRootsB
    simpleRootsC
    simpleRootsD
    simpleRootsE6
    simpleRootsE7
    simpleRootsE8
    simpleRootsF4
    simpleRootsG2
    toCoroot
    toCoweight
    toEpsilon
    toFundamental
    toLegacyCartanMatrix
    toLegacyList
    toRoot
    toSimple
    toWeight
    weightBasis
    weightFromLegacy
    weightFromRealizationCoordinates
    weightToRootCoordinates
  :Weyl groups
    bruhatLessOrEqual
    coxeterMatrix
    identityWeylElement
    inverseWeylElement
    leftDescentSet
    longElement
    reducedWord
    reflectionMatrix
    rightDescentSet
    simpleWeylReflection
    weylAction
    weylElement
    weylElementMatrix
    weylGroup
    weylGroupElements
    weylGroupOrder
    weylInversionData
    weylLength
    weylLengthGenerating
    weylParent
    weylWord
  :Parabolics and Kostant cohomology
    cohomologyDimensions
    decompositionDimension
    dotActionOnWeight
    isLeviRoot
    kostantLieAlgCohomology
    leviConnectedComponents
    leviHighestWeights
    leviNodes
    leviSubalgebra
    nilradicalRoots
    parabolicCosetReps
    parabolicData
    parabolicDegree
    parabolicDegrees
    parabolicDiagram
    parabolicDotImages
    parabolicLevi
    removedNodes
    standardParabolic
  :Reductive Levi restriction
    centralCharacter
    decomposeLeviCharacter
    leviCenterBasis
    reductiveLeviDatum
    restrictCharacter
    restrictWeight
    semisimplePart
  :Finite structural invariants
    comarks
    cominusculeNodes
    coxeterNumber
    dominantizeWeight
    dualCartanMatrix
    dualCoxeterNumber
    dualWeight
    fundamentalCoweights
    highestCoroot
    highestRoot
    highestShortRoot
    invariantDegrees
    isCominusculeNode
    isCominusculeWeight
    isDominant
    isMinusculeNode
    isMinusculeWeight
    isOppositionFixed
    isSelfDual
    lacingNumber
    marks
    minusculeNodes
    normalizedInvariantForm
    quadraticCasimir
    weightStabilizer
    weylExponents
    weylVector
  :Characters, dimensions, and tensor operations
    adamsOperation
    characterCoefficient
    characterDatum
    characterDimension
    characterFromDecomposition
    characterSupport
    characterTerms
    decomposeCharacter
    dominantWeightsInRep
    dualCharacter
    exteriorPowerCharacter
    exteriorPowerDecomposition
    finiteCharacter
    formalCharacter
    frobeniusSchurIndicator
    integrableCharacter
    integrableHighestWeightModule
    irrepData
    irrepDimension
    isExactCharacter
    kostantPartitionFunction
    kostantWeightMultiplicity
    moduleCharacter
    representationDynkinIndex
    symmetricPowerCharacter
    symmetricPowerDecomposition
    tensorCoefficient
    tensorDecomposition
    vermaCharacter
    vermaModule
    weightMultiplicity
    weightMultiplicityTable
    weightOrbit
    weightsOfRep
    weylDimension
  :Affine structure and root multiplicities
    affineCartanA2Twisted
    affineCartanAOddTwisted
    affineCartanD4Twisted
    affineCartanDTwisted
    affineCartanE6Twisted
    affineCentralCoefficients
    affineCentralElement
    affineClassicalPart
    affineComarks
    affineDegree
    affineDerivation
    affineFinitePart
    affineFullWeight
    affineLevel
    affineMarks
    affineNullRoot
    affineRootSnapshot
    affineStructuralCapabilities
    affineTypeData
    affineWeightFromDynkinLabels
    indefiniteRootSnapshot
    isRootInSnapshot
    rootMultiplicity
    rootSnapshot
    rootSnapshotByHeight
    rootSnapshotRoots
  :Borel-Weil-Bott and Demazure theory
    borelWeilBott
    borelWeilBottCharacter
    demazureCharacter
    demazureOperator
  :Kazhdan-Lusztig theory and BGG resolutions
    bggEulerCharacter
    bggResolution
    dotWeylAction
    kazhdanLusztigCoefficients
    kazhdanLusztigPolynomial
    kazhdanLusztigVariable
    simpleHighestWeightModule
    vermaCompositionFactors
    vermaCompositionMultiplicity
  :Highest-weight crystals
    crystalCharacter
    crystalContains
    crystalDatum
    crystalE
    crystalEdges
    crystalEpsilon
    crystalF
    crystalHighestWeight
    crystalParent
    crystalPhi
    crystalTableau
    crystalVertex
    crystalVertexData
    crystalVertices
    crystalWeight
    highestWeightCrystal
    minusculeCrystal
    tensorProductCrystal
  :Partitions and weight coordinates
    fundamentalToPartition
    partitionToFundamental
    specialCartanFromPartition
  :Related packages and retained calculations
    affineStructureCacheInfo
    clearAffineStructureCache
    clearInfrastructureCaches
    clearRepresentationCache
    clearRootSnapshotCache
    infrastructureCacheInfo
    representationCacheInfo
    rootSnapshotCacheInfo
    toLieAlgebra
    toRootSystem
SeeAlso
  "Mathematical background"
  "Root data and representations"
  "Working with formal characters"
///
