-- TODO: cohomology(...,Degree=>...)

-----------------------------------------------------------------------------
-- sheaf cohomology
-----------------------------------------------------------------------------

doc ///
Node
  Key
    (cohomology, ZZ, SheafOfRings)
  Headline
    cohomology of a sheaf of rings on a projective variety
  Usage
    HH^d(OX)
  Inputs
    i:ZZ
    OX:SheafOfRings
      on a @syn ProjectiveVariety@ $X$
  Outputs
    :Module
      representing a vector space over the coefficient field of $X$
  Description
    Text
      The command computes the $i$-th cohomology group of $OX$ as a vector space over the coefficient field of $X$.
    Example
      Cubic = Proj QQ[x_0..x_2]/ideal(x_0^3+x_1^3+x_2^3)
      HH^1(OO_Cubic)
  SeeAlso
    SheafOfRings
    (cohomology, ZZ, SumOfTwists)
    (cohomology, ZZ, CoherentSheaf)

-- Note: TorsionFree and GlobalSectionLimit are set by twistedGlobalSectionsModule,
-- which is called by HH^0(SumOfTwists) and HH^0(CoherentSheaf) are used by prune(SheafMap)
-- Similarly, SaturationMap is set here
Node
  Key
    (cohomology, ZZ,                    SumOfTwists)
    (cohomology, ZZ, ProjectiveVariety, SumOfTwists)
    TorsionFree
    GlobalSectionLimit
    SaturationMap
  Headline
    coherent sheaf cohomology module
  Usage
    HH^i(F(>=d))
    HH^i(F(*))
  Inputs
    i:ZZ
    F(>=d):SumOfTwists -- see @TO2 {(symbol SPACE, CoherentSheaf, LowerBound), TT "F(>=d)"}@
  Outputs
    :Module
      over the homogeneous coordinate ring of the variety $X$
  Description
    Text
      Given a @syn CoherentSheaf@ @TT "F"@ on a @syn ProjectiveVariety@ $X$,
      the notation @TT "F(>=d)"@ creates an object representing
      the @syn SumOfTwists@ $\bigoplus_{a=d}^\infty\mathcal F(a)$.
    Text
      This command computes a graded module $M$ over the homogeneous coordinate ring of
      the variety $X$ such that the graded component $M_a$ for $a\geq d$
      is isomorphic to the cohomology group @TT "HH^i(F(a))"@.
    Text
      To discard the part of the module $M$ of degree less than $d$,
      truncate the module with @TO2 {"Truncations::truncate(ZZ,Module)", TT "truncate(d, M)"}@.
    Text
      Use @TT "HH^i(F(>d))"@ to compute the cohomology of the twists strictly greater than $d$
      and @TT "HH^i(F(*))"@ to try to compute the whole graded module.
      When $i=0$, this is known as the module of twisted global sections
      $$ \Gamma_*(\mathcal F) = \bigoplus_{a\in\mathbf Z}(X, \mathcal F(a)).$$
      Note that this computation will fail if the module is not finitely generated.
    Text
      As a first example we look at the cohomology of line bundles on the projective plane.
    Example
      X = Proj QQ[x_0..x_2]
      HH^0(OO_X^1(>=0))
      HH^1(OO_X^1(>=0))
      HH^2(OO_X^1(>=-3)) -- this should change to * once implemented
      TruncDual = HH^2(OO_X^1(>=-4))
      hilbertFunction(-4, TruncDual)
      hilbertFunction(-3, TruncDual)
    Text
      As a second example we compute the $H^1$ cohomology module $T$ of
      the Horrocks-Mumford bundle on the projective fourspace, which is
      an artinian module with Hilbert function (5,10,10,2):
    Example
      R = QQ[x_0..x_4];
      a = {1,0,0,0,0}
      b = {0,1,0,0,1}
      c = {0,0,1,1,0}
      M1 = matrix table(5,5, (i,j)-> x_((i+j)%5)*a_((i-j)%5))
      M2 = matrix table(5,5, (i,j)-> x_((i+j)%5)*b_((i-j)%5))
      M3 = matrix table(5,5, (i,j)-> x_((i+j)%5)*c_((i-j)%5))
      M = M1 | M2 | M3;
      betti (C=res coker M)
      N = transpose submatrix(C.dd_3,{10..28},{2..36});
      betti (D=res coker N)
      Pfour = Proj(R)
      HorrocksMumford = sheaf(coker D.dd_3);
      T = HH^1(HorrocksMumford(>=-1))
      apply(-1..2, i-> hilbertFunction(i,T))
  Caveat
    The computation of @TT "HH^0(F(*))"@ will fail if the module is not finitely generated.
    Also the version @TT "HH^i(F(*))"@ for $i>0$ is not yet implemented.
  SeeAlso
    HH
    (cohomology, ZZ, CoherentSheaf)
    (cohomology, ZZ, Module)
    (prune, CoherentSheaf)
    (prune, SheafMap)

Node
  Key
    (cohomology, ZZ, AffineVariety, CoherentSheaf)

Node
  Key
    (cohomology, ZZ, ProjectiveVariety, CoherentSheaf)
    (cohomology, ZZ,                    CoherentSheaf)
  Headline
    cohomology of a coherent sheaf on a projective variety
  Usage
    HH^i(X,F)
    HH^i F
    cohomology(i,X,F)
    cohomology(i,F)
  Inputs
    i:ZZ
    X:ProjectiveVariety
    F:CoherentSheaf
      on the projective variety @TT "X"@
  Outputs
    :Module
      the $i$-th cohomology group of @TT "F"@ as a vector space
      over the coefficient field of @TT "X"@
  Description
    Text
      The command computes the $i$-th cohomology group of @TT "F"@
      as a vector space over the coefficient field of @TT "X"@.
      For $i>0$ this is currently done via local duality. More precisely, the code takes advantage of
      the isomorphism
      $$H^i (X, \mathcal{F}) = ( H_{\mathfrak{m}}^i ( M) )_0,$$
      where the notation $(-)_0$ denotes the degree $0$ part of a module, and $M$ is any module representing $\mathcal F$.
      Combining this with the local duality isomorphism for the graded polynomial ring $S$ yields
      $$H_{\mathfrak{m}}^i (M) \cong \operatorname{Ext}^{d-i}_S (M, S(-n-1)),$$
      where we are viewing $X$ as being embedded into $\mathbb{P}^n$ via some affine morphism.
      For $i=0$, things are not as simple since there is not a direct isomorphism between the global
      sections of a sheaf and the degree 0 part of the $0$th local cohomology. Thus it is instead computed
      as a limit of @TO2 {(Hom, Module, Module), "Hom"}@s.
    Text
      See @TO "BGG::cohomologyTable"@ and @TO "TateOnProducts::cohomologyHashTable"@ for
      alternative sheaf cohomology algorithms via the Bernstein-Gelfand-Gelfand correspondence.
    Text
      --I feel like giving simpler examples that most users will already
      --know the answer to should be done first, then do more complicated examples
      As examples we compute the Picard numbers, Hodge numbers and
      dimension of the infinitesimal deformation spaces of various
      quintic hypersurfaces in projective fourspace (or their Calabi-Yau small resolutions).
    Text
      We will make computations for quintics @TT "V"@ in the family given by
      $$ x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-5\lambda x_0x_1x_2x_3x_4=0 $$
      for various values of $\lambda$. If $\lambda$ is general
      (that is, $\lambda$ not a 5-th root of unity, 0 or $\infty$),
      then the quintic @TT "V"@ is smooth, so is a Calabi-Yau threefold,
      and in that case the Hodge numbers are as follows.
      $$ h^{1,1}(V)=1,   h^{2,1}(V) = h^{1,2}(V) = 101, $$
      so the Picard group of @TT "V"@ has rank 1 (generated by the hyperplane section)
      and the moduli space of @TT "V"@ (which is unobstructed) has dimension 101:
    Example
      Quintic = Proj QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-101*x_0*x_1*x_2*x_3*x_4)
      singularLocus Quintic
      omegaQuintic = cotangentSheaf Quintic;
      h11 = rank HH^1(omegaQuintic)
      h12 = rank HH^2(omegaQuintic)
    Text
      By Hodge duality this is $h^{2,1}$.  Directly $h^{2,1}$ could be computed as:
    Example
      h21 = rank HH^1(cotangentSheaf(2, Quintic))
    Text
      The Hodge numbers of a (smooth) projective variety can also be computed
      directly using the @TO hh@ command:
    Example
      hh^(2,1)(Quintic)
      hh^(1,1)(Quintic)
    Text
      Using the Hodge number we compute the topological Euler characteristic of @TT "V"@:
    Example
      euler Quintic
    Text
      When $\lambda$ is a 5th root of unity the quintic @TT "V"@ is singular.
      It has 125 ordinary double points (nodes), namely the orbit of the point
      $(1:\lambda:\lambda:\lambda:\lambda)$ under a natural action of $\ZZ/5^3$.
      Then @TT "V"@ has a projective small resolution $W$ which is a Calabi-Yau threefold
      (since the action of $\ZZ/5^3$ is transitive on the sets of nodes of @TT "V"@,
      or for instance, just by blowing up one of the $(1,5)$ polarized abelian surfaces
      @TT "V"@ contains). Perhaps the most interesting such 3-fold is the one
      for the value $\lambda=1$, which is defined over $\QQ$ and is modular
      (see Schoen's work). To compute the Hodge numbers of the small resolution
      $W$ of @TT "V"@ we proceed as follows:
    Example
      SchoensQuintic = Proj QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-5*x_0*x_1*x_2*x_3*x_4)
      Z = singularLocus SchoensQuintic
      degree Z
      II'Z = sheaf module ideal Z
    Text
      The defect of $W$ (that is, $h^{1,1}(W)-1$) can be computed from the cohomology
      of the ideal sheaf of the singular locus $Z$ of @TT "V"@ twisted by 5 (see Werner's thesis):
    Example
      defect = rank HH^1(II'Z(5))
      h11 = defect + 1
    Text
      The number $h^{2,1}(W)$ (the dimension of the moduli space of $W$) can be computed (Clemens-Griffiths, Werner)
      as $\dim H^0({\mathbf I}_Z(5))/\mathrm{JacobianIdeal}(V)_5$.
    Example
      quinticsJac = numgens source basis(5, ideal Z)
      h21 = rank HH^0(II'Z(5)) - quinticsJac
    Text
      In other words W is rigid. It has the following topological Euler characteristic.
    Example
      chiW = euler Quintic + 2 * degree Z
  SeeAlso
    "coherent sheaves"
    (cohomology, ZZ, SumOfTwists)
    (cohomology, ZZ, Module)
    hh
    CoherentSheaf

Node
  Key
    (cohomology, ZZ, ProjectiveVariety, SheafMap)
    (cohomology, ZZ,                    SheafMap)

-----------------------------------------------------------------------------
-- hh
-----------------------------------------------------------------------------

Node
  Key
    hh
   (hh, Sequence, ProjectiveVariety)
  Headline
    Hodge numbers of a smooth projective variety
  Usage
    hh^(p,q)(X)
  Inputs
    (p,q):
      a pair of integers between 0 and $\dim X$
    X:ProjectiveVariety
  Outputs
    :ZZ
      the $(p,q)$-Hodge numbers of $X$
  Description
    Text
      The command computes the Hodge numbers
      $$ h^{p,q}(X) = \dim H^q(\Omega_X^p) $$
      of the smooth projective variety $X$, calculated as @TT "HH^q(cotangentSheaf(p, X))"@.
    Text
      As an example we compute the Hodge diamond of a smooth K3 surface (Fermat quartic in projective threespace):
    Example
      X = Proj QQ[x_0..x_3]/ideal(x_0^4+x_1^4+x_2^4+x_3^4)
      assert isSmooth X
      matrix table(toList(0..2), toList(0..2), (p,q) -> hh^(p,q)(X))
      euler X
  SeeAlso
    (cotangentSheaf, ZZ, ProjectiveVariety)
    (cohomology, ZZ, CoherentSheaf)
    (euler, ProjectiveVariety)

-----------------------------------------------------------------------------
-- Hom
-----------------------------------------------------------------------------


--TODO: add the fact that we can access the actual morphism of sheaves corresponding
--to a global section?
Node
  Key
    (Hom, CoherentSheaf, CoherentSheaf)
    (Hom, CoherentSheaf, SheafOfRings)
    (Hom, SheafOfRings, CoherentSheaf)
    (Hom, SheafOfRings, SheafOfRings)
  Headline
    global Hom
  Usage
    Hom(F, G)
  Inputs
    F:{SheafOfRings,CoherentSheaf}
    G:{SheafOfRings,CoherentSheaf}
  Outputs
    :Module
      representing a vector space over the coefficient field of $X$
  Description
    Text
      If @TT "F"@ or @TT "G"@ is a @syn SheafOfRings@, it is regarded as a sheaf of modules in the evident way.
    Text
      Both @TT "F"@ and @TT "G"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      The result is the sheaf associated to the graded module @TT "Hom(module F, module G)"@.
    Example
      R = QQ[a..d];
      P3 = Proj R
      I = monomialCurveIdeal(R, {1,3,4})
      G = sheaf module I
      Hom(OO_P3, G(3))
      HH^0(G(3))
  SeeAlso
    sheafHom
    sheafExt
    (Ext, ZZ, CoherentSheaf, CoherentSheaf)

-----------------------------------------------------------------------------
-- sheafHom
-----------------------------------------------------------------------------

Node
  Key
    sheafHom
   (sheafHom, CoherentSheaf, CoherentSheaf)
   (sheafHom, CoherentSheaf, SheafOfRings)
   (sheafHom, SheafOfRings,  CoherentSheaf)
   (sheafHom, SheafOfRings,  SheafOfRings)
   [sheafHom, DegreeLimit]
   [sheafHom, MinimalGenerators]
   [sheafHom, Strategy]
  Headline
    sheaf Hom
  Usage
    sheafHom(F, G)
  Inputs
    F:{CoherentSheaf,SheafOfRings}
    G:{CoherentSheaf,SheafOfRings}
  Outputs
    :CoherentSheaf
      corresponding to homomorphisms from $M \to N$, where $F = \tilde M$ and $G = \tilde N$
  Description
    Text
      If @TT "F"@ or @TT "G"@ is a @syn SheafOfRings@, it is regarded as a sheaf of modules in the evident way.
    Text
      Both @TT "F"@ and @TT "G"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      The result is the sheaf associated to the graded module @TT "Hom(module F, module G)"@.
    Example
      S = QQ[x,y]
      X = Proj S
      sheafHom(OO_X^1(2), OO_X^1(11))
      Hom(S^{2}, S^{11})
  SourceCode
    (sheafHom, CoherentSheaf, CoherentSheaf)
  SeeAlso
    (Hom, Module, Module)
    (Hom, CoherentSheaf, CoherentSheaf)
    (sheafHom, CoherentSheaf, SheafMap)
    (sheafExt, ZZ, CoherentSheaf, CoherentSheaf)

Node
  Key
    (sheafHom, SheafMap, SheafMap)
    (sheafHom, SheafMap, SheafOfRings)
    (sheafHom, SheafMap, CoherentSheaf)
    (sheafHom, CoherentSheaf, SheafMap)
    (sheafHom, SheafOfRings,  SheafMap)
  Headline
    functor of sheaf Hom

-----------------------------------------------------------------------------
-- Ext
-----------------------------------------------------------------------------

-- Note: TruncateDegree is set by Ext^ZZ(CoherentSheaf, SumOfTwists)
-- and used by yonedaSheafExtension to construct the extensions
Node
  Key
    (Ext, ZZ, CoherentSheaf, SumOfTwists)
    (Ext, ZZ, SheafOfRings,  SumOfTwists)
    -- TODO: document the option MinimalGenerators
  Headline
    global Ext
  Usage
    Ext^i(F, G(>=d))
  Inputs
    i:ZZ
    F:{CoherentSheaf,SheafOfRings}
    G(>=d):SumOfTwists -- see @TO2 {(symbol SPACE, CoherentSheaf, LowerBound), TT "G(>=d)"}@
  Outputs
    :Module
      $\bigoplus_{j \geq d} \mathrm{Ext}_X^i(F, G(j))$
  Description
    Text
      If @TT "F"@ or @TT "G"@ is a @syn SheafOfRings@, it is regarded as a sheaf of modules in the evident way.
    Text
      Both @TT "F"@ and @TT "G"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      As an example, we consider the rational quartic curve in $\mathbf P^3$.
    Example
      S = QQ[a..d]
      I = monomialCurveIdeal(S, {1,3,4})
      R = S/I
      X = Proj R
      IX = sheaf (module I ** R)
      Ext^1(IX, OO_X(>=-3))
      Ext^0(IX, OO_X(>=-10))
    Text
      The algorithm used may be found in:
    Code
      UL { LI { "Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746." } }
    Text
      If the vector space $\mathrm{Ext}^i(M, N)$ is desired, see @TO (Ext,ZZ,CoherentSheaf,CoherentSheaf)@.
  SeeAlso
    resolution
    Tor
    Hom
    HH
    sheafExt
    yonedaSheafExtension

Node
  Key
    (Ext, ZZ, CoherentSheaf, CoherentSheaf)
    (Ext, ZZ, CoherentSheaf, SheafOfRings)
    (Ext, ZZ, SheafOfRings, CoherentSheaf)
    (Ext, ZZ, SheafOfRings, SheafOfRings)
  Usage
    Ext^i(F, G)
  Headline
    global Ext
  Inputs
    i:ZZ
    F:{SheafOfRings,CoherentSheaf}
    G:{SheafOfRings,CoherentSheaf}
  Outputs
    :Module
      the global Ext module $\mathrm{Ext}^i_X(\mathcal F, \mathcal G)$
  Description
    Text
      The global Ext module $\operatorname{Ext}^i_X (\mathcal F, \mathcal G)$ is a vector space, defined
      as the $i$th right derived functor of the global Hom functor $\operatorname{Hom}_X (\mathcal{F}, -)$. The
      elements of the $i$th global Ext functor represent extensions of the sheaf $\mathcal{F}$ by $\mathcal{G}$; that
      is, exact sequences of sheaves of the form
      $$0 \to \mathcal{G} \to \mathcal{C}_i \to \mathcal{C}_{i-1} \to \cdots \to \mathcal{C}_1 \to \mathcal{F} \to 0.$$
      These representatives can be accessed using the --put in yonedaSheafExt command
    Text
      Of course, in Macaulay2 these vector spaces are not computed using injective resolutions of sheaves.
      Instead, a result of Greg Smith is used that shows that if $\mathca{F}$ and $\mathcal{G}$ are sheaves
      represented by modules $M$ and $N$, respectively, then there exists an integer $d$ (depending on $M$, $N$, and $i$)
      such that
      $$\operatorname{Ext}^i_X (\mathcal F, \mathcal G) = \operatorname{Ext}^i_S (M_{\geq d} , N)_0,$$
      where in the above $S$ is some polynomial ring over a field, $M_{\geq d}$ denotes truncation,
      and $(-)_0$ denotes the degree $0$ part of a graded module. Moreover, the modules $M$ and $N$ are being
      viewed as modules over the polynomial ring $S$ via restriction of scalars along the canonical
      surjection $S \to R$, where $X = \operatorname{Proj} (R)$. 
    Text
      If @TT "F"@ or @TT "G"@ is a @syn SheafOfRings@, it is regarded as a sheaf of modules in the evident way.
    Text
      Both @TT "F"@ and @TT "S"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      As an example, we compute $\mathrm{Hom}_X(\mathcal I_X,\mathcalO_X)$ and $\mathrm{Ext}^1_X(\mathcal I_X,\mathcal O_X)$,
      for the rational quartic curve in $\PP^3$.
    Example
      S = QQ[a..d];
      I = monomialCurveIdeal(S,{1,3,4})
      R = S/I
      X = Proj R
      IX = sheaf (module I ** R)
      Hom(IX, OO_X)
      Ext^1(IX, OO_X)
    Text
      The $\mathrm{Ext}^1$ being zero says that the point corresponding to $I$
      on the Hilbert scheme is smooth (unobstructed), and vector space dimension
      of $\mathrm{Hom}$ tells us that the dimension of the component at the point $I$ is 16.
    Text
      The algorithm used may be found in:
    Code
      UL { LI { "Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746." } }
    Text
      If the module $\bigoplus_{a\geq 0} \mathrm{Ext}^i(M, N(a))$ is desired, see @TO (Ext,ZZ,CoherentSheaf,SumOfTwists)@.
  SeeAlso
    resolution
    Tor
    Hom
    HH
    sheafExt

Node
  Key
    (Ext, ZZ, CoherentSheaf, SheafMap)
  Headline
    maps on global Ext induced by a sheaf map
  Usage
    Ext^i(F, g)
  Inputs
    i:ZZ
    F:{CoherentSheaf}
    g:{SheafMap}
  Outputs
    :Matrix
      the induced map on global Ext $\mathrm{Ext}^i_X(\mathcal{F} , g)$, where $g$ is some morphism of sheaves
  Description
    Text
      As explained in @TO (Ext, ZZ, CoherentSheaf, CoherentSheaf)@, the global Ext module $\operatorname{Ext}^i_X (\mathcal F, \mathcal G)$ is a
      vector space, defined as the $i$th right derived functor of the global Hom functor $\operatorname{Hom}_X (\mathcal{F}, -)$.
      In particular, this implies that for any morphism of sheaves $g : \mathcal{G} \to \mathcal{G}'$ there is an induced map
      $$\operatorname{Ext}^i_X (\mathcal F , g) : \operatorname{Ext}^i_X (\mathcal F , \mathcal G) \to \operatorname{Ext}^i_X (\mathcal F , \mathcal{G}').$$
    Text
      In Macaulay2, these vector spaces are not computed using injective resolutions of sheaves.
      Instead, a result of Greg Smith is used that shows that if $\mathca{F}$ and $\mathcal{G}$ are sheaves
      represented by modules $M$ and $N$, respectively, then there exists an integer $d$ (depending on $M$, $N$, and $i$)
      such that
      $$\operatorname{Ext}^i_X (\mathcal F, \mathcal G) = \operatorname{Ext}^i_S (M_{\geq d} , N)_0,$$
      where in the above $S$ is some polynomial ring over a field, $M_{\geq d}$ denotes truncation,
      and $(-)_0$ denotes the degree $0$ part of a graded module. Moreover, the modules $M$ and $N$ are being
      viewed as modules over the polynomial ring $S$ via restriction of scalars along the canonical
      surjection $S \to R$, where $X = \operatorname{Proj} (R)$. In particular, if $\mathcal{G}'$ is represented by some module $N'$,
      then after taking $d$ to be the maximum of the integers required to satisfy the assumptions of G. Smith's
      result, the induced map on global Ext may be computed as
      $$\operatorname{Ext}^i_X (\mathcal F , g) = \operatorname{Ext}_S^i (M_{\geq d} , \widehat{g} )_0,$$
      where $\widetilde{g}$ is some map of modules whose associated sheaf is equal to $g$. 
    Text
      If @TT "F"@ or @TT "G"@ is a @syn SheafOfRings@, it is regarded as a free $\mathcal{O}_X$-module of rank $1$.
      In particular, if $\mathcal{F} = \mathcal{O}_X$ then the map being computed is just the induced map
      on cohomology
      $$H^i (g) : H^i (\mathcal{G}) \to H^i (\mathcal{G}').$$
    Text
      Both @TT "F"@ and @TT "G"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      As an example, we compute $\mathrm{Hom}_X(\mathcal I_X,\mathcal O_X)$ and $\mathrm{Ext}^1_X(\mathcal I_X,\mathcal O_X)$,
      for the rational quartic curve in $\PP^3$.
    Example
      S = QQ[a..d]; --KELLER: need to add examples here
      I = monomialCurveIdeal(S,{1,3,4})
      R = S/I
      X = Proj R
      IX = sheaf (module I ** R)
      Hom(IX, OO_X)
      Ext^1(IX, OO_X)
    Text
      The $\mathrm{Ext}^1$ being zero says that the point corresponding to $I$
      on the Hilbert scheme is smooth (unobstructed), and vector space dimension
      of $\mathrm{Hom}$ tells us that the dimension of the component at the point $I$ is 16.
    Text
      The algorithm used may be found in:
    Code
      UL { LI { "Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746." } }
    Text
      If the module $\bigoplus_{a\geq 0} \mathrm{Ext}^i(M, N(a))$ is desired, see @TO (Ext,ZZ,CoherentSheaf,SumOfTwists)@.
  SeeAlso
    resolution
    Tor
    Hom
    HH
    sheafExt

Node
  Key
    ExtLongExactSequence
--   (ExtLongExactSequence, CoherentSheaf, SheafMap)
--   (ExtLongExactSequence, CoherentSheaf, SheafMap, SheafMap)
--   [ExtLongExactSequence, Concentration]
  Headline
    the long exact sequence of the Ext functor

-----------------------------------------------------------------------------
-- sheafExt
-----------------------------------------------------------------------------

Node
  Key
    sheafExt
   (sheafExt, ZZ, CoherentSheaf, CoherentSheaf)
   (sheafExt, ZZ, SheafOfRings, CoherentSheaf)
   (sheafExt, ZZ, CoherentSheaf, SheafOfRings)
   (sheafExt, ZZ, SheafOfRings, SheafOfRings)
  Headline
    sheaf extension of coherent sheaves
  Usage
    sheafExt^n(F,G)
  Inputs
    n:ZZ
    F:{SheafOfRings,CoherentSheaf}
    G:{SheafOfRings,CoherentSheaf}
  Outputs
    :CoherentSheaf
      the n-th sheaf extension of @TT "F"@ and @TT "G"@
  Description
    Text
      If @TT "F"@ or @TT "G"@ is a @syn SheafOfRings@, it is regarded as a sheaf of modules in the evident way.
    Text
      Both @TT "F"@ and @TT "S"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      The result is the sheaf associated to the graded module @TT "Ext^n(module F, module G)"@.
    Example
      X = Proj QQ[x,y]
      sheafExt^1(OO_X^1(2), OO_X^1(-11))
  SeeAlso
    sheafHom
    Hom
    Ext
    HH
    (Ext, ZZ, CoherentSheaf, CoherentSheaf)
///


///
Node
  Key
   (determinant, CoherentSheaf)
  Headline
    sheaf extension of coherent sheaves
  Usage
    determinant(F)
  Inputs
    F:CoherentSheaf
      preferably a vector bundle in order for determinant to be well-defined
  Outputs
    :CoherentSheaf
      the top exterior power of the sheaf F
  Description
    Text
      The determinant of a vector bundle is defined to be the top exterior power of that bundle. More precisely,
      if $\mathcal E$ has rank $n$, then
      $$\operatorname{det} (\mathcal E) := \bigwedge^n \mathcal{E}.$$
    Text
      Both @TT "F"@ and @TT "G"@ must be coherent sheaves on the same projective variety or scheme $X$.
    Text
      The result is the sheaf associated to the graded module $\bigwedge^n M$.
    Example
      X = Proj QQ[x,y] ----KELLER: DO
      sheafExt^1(OO_X^1(2), OO_X^1(-11))
  SeeAlso
    sheafHom
    Hom
    Ext
    HH
    (Ext, ZZ, CoherentSheaf, CoherentSheaf)
///
