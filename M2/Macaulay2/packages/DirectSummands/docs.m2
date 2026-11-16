-- TODO:
-- [x] example that is indecomposable
-- [ ] example that decomposes after field extension
-- [ ] example over ZZ, ZZ[i]/(i^2+1), QQ, QQ[i]/(i^2+1), GF, RR, CC?
-- [ ] graded example
-- [ ] local example
-- [ ] example of passing hints of summands
-- [ ] example of isIndecomposable

doc ///
Node
  Key
    DirectSummands
  Headline
    decompositions of graded modules and coherent sheaves
  Description
    Text
      This package implements algorithms for computing indecomposable summands of finitely generated
      $R$-module $M$ over a $k$-algebra when $M$ is either a homogeneous module over a (multi)graded ring
      with $k$ a field of arbitrary characteristic, or when $R$ is local and $k$ a field of positive
      characteristic contained in $\overline{\mathbb F_p}$.

      Further, in the graded case, this package enables the computation of indecomposable summands of
      coherent sheaves on subvarieties of the projective space $\PP^n$ as well as other complete toric
      varieties, while in the local case it enables the study of germs of inhomogeneous singularities
      in positive characteristic.
    Tree
      : Primary methods:
        * directSummands
	* isIndecomposable
      : Supporting methods:
	* changeBaseField
	* generalEndomorphism
        * findProjectors
	* findIdempotents
	* findSplitInclusion
	* isomorphismTally
	* tallySummands
	* potentialExtension
      : Featured applications:
        * "Example: summands of the Frobenius pushforward"
        * "Example: syzygies over Artinian rings"
        * "Example: symbolic diagonalization"
    Text
      The decomposition algorithms are randomized, so repeated runs may sample different endomorphisms.
      Over a large enough field, this implementation is often capable of completely decomposing a module
      in a single non-recursive iteration. When a decomposition appears only after extending scalars,
      the package can suggest or carry out a field extension via @TO "potentialExtension"@ and @TO "changeBaseField"@.
    Text
      The package also (temporarily) includes utilities for working with the Frobenius map in positive characteristic:
      @TO "frobeniusMap"@, @TO "frobeniusRing"@, @TO "frobeniusPullback"@, @TO "frobeniusPushforward"@,
      and @TO "frobeniusTwist"@.  These make it possible to study decompositions of Frobenius pushforwards
      of modules and sheaves, which is one of the motivating applications.
    Text
      As a sample computation, consider the splitting of the Frobenius pushforward of the structure sheaf
      of an elliptic curve over $\mathbb F_7$ and after base change to $\mathbb F_{49}$.
    Example
      R = ZZ/7[x,y,z]/(x^3+y^3+z^3);
      X = Proj R;
      F = frobeniusPushforward(1, OO_X);
      elapsedTime tallySummands summands F
      elapsedTime tallySummands summands changeBaseField(GF(7, 2), F)
  Acknowledgement
    The authors thank the organizers of the @HREF{"https://aimath.org/pastworkshops/macaulay2efie.html",
	"Macaulay2 workshop at AIM"}@, where significant progress on this package was made.
  References
    @HREF{"https://doi.org/10.1016/j.jsc.2025.102486", "Computing direct sum decompositions"}@,
    Mallory and Sayrafi, Journal of Symbolic Computation, Volume 133, March–April 2026
    @HREF{"https://arxiv.org/abs/2412.19799v2", "[arXiv:2412.19799]"}@.
  Citation
    @article{MS26,
      author       = {Mallory, Devlin and Sayrafi, Mahrud},
      title	       = {Computing direct sum decompositions},
      journal      = {J. Symbolic Comput.},
      fjournal     = {Journal of Symbolic Computation},
      volume       = {133},
      year	       = {2026},
      doi	       = {10.1016/j.jsc.2025.102486},
    }
  Subnodes
    "Example: summands of the Frobenius pushforward"
    "Example: syzygies over Artinian rings"
    "Example: symbolic diagonalization"
    directSummands
    isDirectSummand
    isIndecomposable
    findProjectors
    findIdempotents
    findSplitInclusion
    generalEndomorphism
    isomorphismTally
    tallySummands
    potentialExtension
    changeBaseField


Node
  Key
     "Example: summands of the Frobenius pushforward"
  Description
    Text
      Here we present examples of using this package to study summands of the Frobenius pushforward
      of the structure sheaf on different varieties. Depending on the geometry, the result may be a
      complete splitting into line bundles, a decomposition involving higher-rank indecomposable summands,
      a decomposition that becomes finer only after extending the ground field, or an indecomposable bundle.

      For further details, see Section 5 of @HREF{"https://doi.org/10.1016/j.jsc.2025.102486", "[MS26]"}@,
      Further references for the geometric situations are collected in that section.
    Text
      The following computations illustrate several common patterns: standard and multigraded examples,
      Grassmannian examples with higher-rank summands, and computations on an elliptic curve.
    Example
      S = ZZ/3[x_0..x_2];
      X = Proj S;
      tally summands frobeniusPushforward(1, OO_X)
    -- FIXME: frobeniusPushforward doesn't like multigraded rings yet
    -- Example
    --   needsPackage "NormalToricVarieties";
    --   X = hirzebruchSurface 3;
    --   rank \ summands frobeniusPushforward(1, OO_X)
    Text
      Base change is often essential when the bundle splits only over a larger finite field.
      In that situation, @TO "changeBaseField"@ and @TO "potentialExtension"@ are useful.
    Example
      R = ZZ/7[x,y,z]/(x^3+y^3+z^3);
      X = Proj R;
      F = frobeniusPushforward(1, OO_X);
      rank \ summands F
      potentialExtension F
      rank \ summands changeBaseField(GF(7, 2), F)
    Text
      Outside of toric varieties, Frobenius pushforwards of the structure sheaf contain
      higher-rank indecomposable bundles in addition to line bundles.
    Example
      R = quotient Grassmannian(1, 3, CoefficientRing => ZZ/3);
      X = Proj R
      tallySummands summands frobeniusPushforward(1, OO_X)
      rank \ keys oo
    Text
      In non-homogeneous situations, decompositions indicate the local singularities of the ring.
      For example, the following ring is an example of a D51 rational double point singularity in characteristic 2,
      for which we can compute the Frobenius pushforward of the ring and observe that it is $F$-split;
      further pushforwards would reveal that it is not F-regular. (Note that this ring is not quasihomogeneous.)
    Example
      R = ZZ/2[x,y,z]/(x^2*y + x*y^2 + x*y*z + z^2);
      F = frobeniusPushforward(1, R)
      summands F
  SeeAlso
    directSummands
    changeBaseField
    potentialExtension
  Subnodes
    frobeniusMap
    frobeniusRing
    frobeniusPullback
    frobeniusPushforward
    frobeniusTwist

Node
  Key
     "Example: syzygies over Artinian rings"
  Description
    Text
      The decomposition methods in this package can be used to study the structure of infinite resolutions.
      For instance, we can look for recurring indecomposable summands in successive syzygies of the residue field.
    Example
      R = ZZ/101[x,y]/ideal(x^3, x^2*y^3, y^5);
      F = res(coker vars R, LengthLimit => 6)
      netList for i from 1 to 5 list tallySummands summands coker F.dd_i
    Text
      This computation records the indecomposable summands appearing in the early syzygies of
      the residue field. This family of examples is studied in @HREF{"https://arxiv.org/abs/2408.13425",
      "Syzygies of the Residue Field over Golod Rings"}@ by Cuong, Dao, Eisenbud, Kobayashi, Polini, and Ulrich.
  SeeAlso
    directSummands
    tallySummands
    isIndecomposable

Node
  Key
     "Example: symbolic diagonalization"
  Description
    Text
      Another application of the package is symbolic diagonalization: by decomposing the cokernel
      of a parameterized matrix, one can recover block-diagonal or diagonal normal forms after an
      appropriate field extension.
    Example
      S = QQ[a,b,c,d];
      m = matrix "a,b,c,d;d,a,b,c;c,d,a,b;b,c,d,a"
      presentation directSum summands coker m
      factor det m
    Example
      K = toField(QQ[i]/(i^2+1));
      T = K[a,b,c,d];
      m = matrix "a,b,c,d;d,a,b,c;c,d,a,b;b,c,d,a"
      prune directSum summands coker m
      factor det m
    Text
      Over $\QQ$, this matrix decomposes into fewer summands than it does over $\QQ(i)$.
      The refinement after base change reflects the further factorization of the symbolic matrix
      over the larger field.
  SeeAlso
    directSummands
    changeBaseField
    isDirectSummand

Node
  Key
     directSummands
    (directSummands, Module)
    (directSummands, CoherentSheaf)
    (directSummands, Module, Module)
    (directSummands, CoherentSheaf, CoherentSheaf)
    (directSummands, List, Module)
    (directSummands, List, CoherentSheaf)
  Headline
    compute the direct summands of a module or coherent sheaf
  Synopsis
    Heading
       split a module or coherent sheaf
    Usage
      summands M
    Inputs
      M:{Module,CoherentSheaf}
    Outputs
      :List
        containing modules or coherent sheaves which are direct summands of $M$
    Description
      Text
        This function attempts to find the indecomposable summands of a module or coherent sheaf $M$.
	The output is a list of modules or sheaves that are direct summands of $M$. The algorithm is probabilistic,
	and so the output may not necessarily consist of indecomposable summands, particularly in small characteristics.
	The user can query whether a summand is certifiably indecomposable using @TO isIndecomposable@.
      Example
        S = QQ[x,y]
        M = coker matrix{{x,y},{x,x}}
        L = summands M
        apply(L, isIndecomposable)
        assert isIsomorphic(M, directSum L)
      Text
        Note that when the input module is not homogeneous, the calculation is treated as occurring over
	the local ring at the origin. That is, the resulting modules will have direct summand isomorphic
	to M after localizing at the origin.

  Synopsis
    Heading
      split prescribed summands from a module or coherent sheaf with
    Usage
      summands(L, M)
      summands(N, M)
    Inputs
      L:{List}
      N:{Module,CoherentSheaf}
      M:{Module,CoherentSheaf}
    Outputs
      :List
        containing modules or coherent sheaves which are direct summands of $M$
    Description
      Text
        If a candidates summand, or list of candidates, is provided as the first argument, the algorithm
	will split off only summands isomorphic to these modules. This can often be much faster than a
	full decomposition when the user has some guess for the summands, and the remaining summands
	can then be found by applying summands again to the final leftover piece.
      Example
        R = ZZ/3[x,y,z]/(y^2-x*z)
        M = frobeniusPushforward(1, R)
        R1 = ring M
        L = summands(R1^1, M) -- check for free summands only
        summands \ select(L, not isFreeModule) -- decompose remaining pieces
  SeeAlso
    isIndecomposable
    findProjectors
    findIdempotents

Node
  Key
     isDirectSummand
    (isDirectSummand, Module, Module)
     isSummand
  Headline
    test whether one module is a direct summand of another
  Usage
    isDirectSummand(L, M)
  Inputs
    L:Module
      the candidate summand
    M:Module
      the ambient module
  Outputs
    :Boolean
      whether the algorithm finds $L$ as a direct summand of $M$
  Description
    Text
      This function tries to detect whether $L$ is a direct summand of $M$ by searching for
      maps $L \to M$ and $M \to L$ whose composition is an isomorphism of $L$.
    Text
      The computation is randomized, so a return value of @TT "true"@ certifies that a summand
      was found, while a return value of @TT "false"@ only means that no splitting was found
      within the allotted number of attempts.  The alias @TT "isSummand"@ is provided for convenience.
    Text
      When $L$ is free, the method instead searches for a surjection $M \to L$, which is enough
      to split off a free summand.
  SeeAlso
    directSummands
    findSplitInclusion
    isIndecomposable

Node
  Key
     isIndecomposable
    (isIndecomposable, Module)
    (isIndecomposable, CoherentSheaf)
  Headline
    tests whether a module or coherent sheaf is indecomposable
  Usage
    isIndecomposable M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :Boolean
      when the algorithm certifies decomposability or indecomposability
    :Nothing
      when the available tests are inconclusive
  Description
    Text
      This function runs inexpensive indecomposability checks without invoking the full decomposition algorithm.
      It returns @TT "false"@ if it finds a splitting, @TT "true"@ if indecomposability is certified, and @TT "null"@ otherwise.
    Text
      As an example, we prove the indecomposability of the @wikipedia "Horrocks–Mumford bundle"@ on $\PP^4$.
    Example
      needsPackage "BGG"
      S = ZZ/32003[x_0..x_4];
      E = ZZ/32003[e_0..e_4, SkewCommutative => true];
      alphad = map(E^5, E^{-2,-2}, transpose matrix{
	      { e_1*e_4, -e_0*e_2, -e_1*e_3, -e_2*e_4,  e_0*e_3},
	      {-e_2*e_3, -e_3*e_4,  e_0*e_4, -e_0*e_1, -e_1*e_2}});
      alpha = syz alphad;
      alphad = beilinson(alphad, S);
      alpha = beilinson(alpha, S);
      FHM = prune homology(alphad, alpha)
      assert(2 == rank FHM)
      -- initially ~30s for End(FHM), ~110s for basis; ~35s in ZZ/2; now down to ~1s total!
      assert elapsedTime isIndecomposable FHM
      assert({FHM} == summands FHM)
      assert FHM.cache.isIndecomposable
  SeeAlso
    directSummands
    findIdempotents

Node
  Key
     findProjectors
    (findProjectors, Module)
    (findProjectors, CoherentSheaf)
  Headline
    construct projectors on a homogeneous module
  Usage
    findProjectors M
  Inputs
    M:{Module,CoherentSheaf}
      expected to be homogeneous
  Outputs
    :List
      of endomorphisms that can be used to split $M$
  Description
    Text
      This routine constructs projectors from a general degree-zero endomorphism and its eigenvalues.
      It is the main splitting strategy for homogeneous modules.
  SeeAlso
    directSummands
    generalEndomorphism
    findIdempotents

Node
  Key
     findIdempotents
    (findIdempotents, Module)
    (findIdempotents, CoherentSheaf)
  Headline
    construct idempotent endomorphisms
  Usage
    findIdempotents M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :List
      of idempotent endomorphisms of $M$ suitable for splitting
  Description
    Text
      This routine constructs nontrivial idempotents in the endomorphism algebra of a module.
      When successful, those idempotents can be used to recover direct sum decompositions.
      This is the main splitting strategy for modules over local rings.
  SeeAlso
    directSummands
    isIndecomposable
    generalEndomorphism

Node
  Key
     findSplitInclusion
    (findSplitInclusion, Module, Module)
  Headline
    construct a split inclusion of one module into another
  Usage
    findSplitInclusion(M, N)
  Inputs
    M:Module
    N:Module
  Outputs
    :Matrix
      a split injection $M \to N$ when one is found
    :Nothing
      when no such map is found
  Description
    Text
      This function performs a randomized search for maps $M \to N$ and $N \to M$
      whose composition is an isomorphism of $M$.
  SeeAlso
    directSummands
    generalEndomorphism

Node
  Key
     generalEndomorphism
    (generalEndomorphism, Module)
    (generalEndomorphism, CoherentSheaf)
    -- TODO: should these be documented separately?
    (generalEndomorphism, Matrix, Nothing, Matrix)
    (generalEndomorphism, Module, Matrix)
    (generalEndomorphism, Module, Matrix, Nothing)
    (generalEndomorphism, Module, Matrix, Matrix)
  Headline
    construct a generic degree-zero endomorphism
  Usage
    generalEndomorphism M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :{Matrix,SheafMap}
  Description
    Text
      This function forms a random linear combination of generators of the degree-zero endomorphism space of $M$.
      It is used internally by the splitting algorithms and can also be useful for experimentation.
  SeeAlso
    findProjectors
    findIdempotents

Node
  Key
     isomorphismTally
    (isomorphismTally, List)
  Headline
    group isomorphic modules or sheaves in a list
  Usage
    isomorphismTally L
  Inputs
    L:List
      of modules or coherent sheaves
  Outputs
    :List
      of pairs consisting of a representative and its multiplicity
  Description
    Text
      The list is traversed up to isomorphism, returning one representative
      from each isomorphism class together with its count.
    Example
      R = ZZ/101[x,y]/ideal(x^3, x^2*y^3, y^5);
      F = res(coker vars R, LengthLimit => 5)
      netList isomorphismTally summands coker F.dd_5
  SeeAlso
    tallySummands

Node
  Key
     tallySummands
    (tallySummands, List)
  Headline
    tally a list of modules up to isomorphism
  Usage
    tallySummands L
  Inputs
    L:List
      of modules or coherent sheaves
  Outputs
    :Tally
      whose keys are representatives of isomorphism classes
  Description
    Text
      This function converts a list of summands into a tally modulo isomorphism.
    Example
      R = ZZ/101[x,y]/ideal(x^3, x^2*y^3, y^5);
      F = res(coker vars R, LengthLimit => 5)
      tallySummands summands coker F.dd_5
  SeeAlso
    isomorphismTally

Node
  Key
     frobeniusMap
    (frobeniusMap, ZZ, Ring)
    (frobeniusMap, Ring, ZZ)
  Headline
    the iterated Frobenius map of a ring in positive characteristic
  Usage
    frobeniusMap(e, R)
  Inputs
    e:ZZ
    R:Ring
  Outputs
    :RingMap
      from the Frobenius source ring to the twisted target ring
  Description
    Text
      This function constructs the $e$-th Frobenius ring map used by the pushforward and pullback routines.
  SeeAlso
    frobeniusRing
    frobeniusTwist
    frobeniusPushforward
    frobeniusPullback

Node
  Key
     frobeniusRing
    (frobeniusRing, ZZ, Ring)
  Headline
    construct the source ring for an iterated Frobenius map
  Usage
    frobeniusRing(e, R)
  Inputs
    e:ZZ
    R:Ring
  Outputs
    :Ring
      with the grading scaled by $p^e$
  Description
    Text
      This function returns the ring that serves as the source of the $e$-th Frobenius map on $R$.
  SeeAlso
    frobeniusMap
    frobeniusTwist

Node
  Key
     frobeniusTwist
    (frobeniusTwist, ZZ, Ring)
    (frobeniusTwist, ZZ, Ideal)
    (frobeniusTwist, ZZ, Module)
    (frobeniusTwist, ZZ, Matrix)
  Headline
    apply the Frobenius twist to rings, modules, and matrices
  Usage
    frobeniusTwist(e, M)
  Inputs
    e:ZZ
    M:{Ring,Ideal,Module,Matrix}
  Outputs
    :Ring
    :Ideal
    :Module
    :Matrix
  Description
    Text
      This function applies the $e$-fold Frobenius twist to the input,
      adjusting coefficients and gradings accordingly.
  SeeAlso
    frobeniusMap
    frobeniusPushforward

Node
  Key
     frobeniusPushforward
    (frobeniusPushforward, ZZ, Ring)
    (frobeniusPushforward, ZZ, Ideal)
    (frobeniusPushforward, ZZ, Module)
    (frobeniusPushforward, ZZ, Matrix)
    (frobeniusPushforward, ZZ, SheafOfRings)
    (frobeniusPushforward, ZZ, CoherentSheaf)
    (frobeniusPushforward, ZZ, SheafMap)
  Headline
    compute Frobenius pushforwards
  Usage
    frobeniusPushforward(e, M)
  Inputs
    e:ZZ
    M:{Ring,Ideal,Module,Matrix,SheafOfRings,CoherentSheaf,SheafMap}
  Outputs
    :Ring
    :Module
    :Matrix
    :CoherentSheaf
    :SheafMap
  Description
    Text
      This function computes the pushforward along the $e$-fold Frobenius morphism.
      In graded situations it attempts to preserve decomposed presentations.
  SeeAlso
    frobeniusMap
    frobeniusTwist
    frobeniusPullback

Node
  Key
     frobeniusPullback
    (frobeniusPullback, ZZ, Ring)
    (frobeniusPullback, ZZ, Ideal)
    (frobeniusPullback, ZZ, Module)
    (frobeniusPullback, ZZ, Matrix)
    (frobeniusPullback, ZZ, CoherentSheaf)
    (frobeniusPullback, ZZ, SheafMap)
  Headline
    compute Frobenius pullbacks
  Usage
    frobeniusPullback(e, M)
  Inputs
    e:ZZ
    M:{Ring,Ideal,Module,Matrix,CoherentSheaf,SheafMap}
  Outputs
    :Ring
    :Module
    :Matrix
    :CoherentSheaf
    :SheafMap
  Description
    Text
      This function computes pullback along the $e$-fold Frobenius morphism.
  SeeAlso
    frobeniusMap
    frobeniusPushforward

Node
  Key
     potentialExtension
    (potentialExtension, Module)
    (potentialExtension, CoherentSheaf)
  Headline
    suggest a field extension where splitting may become visible
  Usage
    potentialExtension M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :Ring
      typically a splitting field of a sampled endomorphism
  Description
    Text
      This function inspects a general endomorphism of $M$ and returns a candidate field extension that may expose further splitting.
  SeeAlso
    changeBaseField
    generalEndomorphism

Node
  Key
     changeBaseField
     extendGroundField
    (changeBaseField, ZZ, Module)
    (changeBaseField, ZZ, CoherentSheaf)
    (changeBaseField, Ring, Matrix)
    (changeBaseField, Ring, Module)
    (changeBaseField, GaloisField, Module)
    (changeBaseField, GaloisField, CoherentSheaf)
  Headline
    extend scalars for modules, matrices, and coherent sheaves
  Usage
    changeBaseField(L, M)
  Inputs
    L:{Ring,GaloisField,ZZ}
      the new coefficient ring or field data
    M:{Module,Matrix,CoherentSheaf}
  Outputs
    :Module
    :Matrix
    :CoherentSheaf
  Description
    Text
      This function changes the coefficient field or base ring of the input while reusing cached decompositions when possible.
  SeeAlso
    potentialExtension
    directSummands
///

-- Template:
///
Node
  Key
  Headline
  Usage
  Inputs
  Outputs
  Consequences
    Item
  Description
    Text
    Example
    CannedExample
    Code
    Pre
  ExampleFiles
  Contributors
  References
  Caveat
  SeeAlso
--    Tree
--    CannedExample
--  Contributors
--  References
--  Caveat
--  SeeAlso
///

end--

restart
installPackage "DirectSummands"
viewHelp DirectSummands

uninstallPackage "DirectSummands"
