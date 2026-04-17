doc ///
Node
  Key
    eulerSequence
   (eulerSequence, ProjectiveVariety)

-- Note: TruncateDegree is set by Ext^ZZ(CoherentSheaf, SumOfTwists)
Node
  Key
    yonedaSheafExtension
   (yonedaSheafExtension, Matrix)
  SeeAlso
    (Ext, ZZ, CoherentSheaf, CoherentSheaf)

Node
  Key
    idealSheafSequence
   (idealSheafSequence, ProjectiveVariety)
  Headline
    ideal sheaf sequence of a projective variety
  Usage
    idealSheafSequence X
  Inputs
    X:ProjectiveVariety
      The projective variety for which to construct the ideal sheaf sequence
  Outputs
    :Complex
      The ideal sheaf sequence of the projective variety
  Description
    Text
      This method computes the ideal sheaf sequence of the projective variety $X$. The sequence is given by $$0 \to \mathcal{I}_X \to \mathcal{O}_{\mathbb{P}^n} \to \mathcal{O}_{\mathbb{P}^n}/\mathcal{I}_X \to 0,$$ where $\mathcal{I}_X$ is the ideal sheaf of $X$, $\mathcal{O}_{\mathbb{P}^n}$ is the structure sheaf of the ambient projective space, and $\mathcal{O}_{\mathbb{P}^n}/\mathcal{I}_X$ is the quotient sheaf.
    Text
      As an example, consider the projective variety defined by the equation $x^4 + y^4 + z^4 = 0$. The ideal sheaf sequence of this variety is computed.
    Example
      X = Proj QQ[x,y,z]/(x^4+y^4+z^4)
      seq = idealSheafSequence X
  SeeAlso
    idealSheaf
    ProjectiveVariety
    
Node
  Key
    naiveCotangentComplex
   (naiveCotangentComplex, ProjectiveVariety)
   [naiveCotangentComplex, Strategy]
  Headline
    the naive cotangent complex of a subspace of weighted projective space
  Usage
    naiveCotangentComplex X
  Inputs
    X:ProjectiveVariety
  Outputs
    :Complex
  Description
    Text
      This is only relevant for $X$ a closed subspace of a weighted projective space,
      rather than the usual projective space.
    Text
      Let $X$ be a closed subspace of a weighted projective space over a ring $k$, with $X$
      viewed as a stack. (Namely, if $Y$ denotes the affine cone over $X$ in affine space $A^{n+1}$,
      $X$ is the quotient stack $[(Y-0)/G_m]$, for the action of the multiplicative group $G_m$
      on $A^{n+1}$ with given weights.)
      The cotangent complex of $X$ lives in cohomological degrees $\leq 1$.
      This function computes its truncation to degrees $\geq 0$, which lives in degrees 0 and 1.
    Text
      The output is a complex of coherent sheaves in cohomological degrees 0 and 1. It can be viewed as
      the complex of $G_m$-equivariant sheaves
      $$0 \to \Omega^1_Y \to \mathfrak{g}^*\otimes O_Y \to 0,$$
      where $\mathfrak{g}$ is the Lie algebra of $G_m$. (The boundary map is given
      by plugging in the vector field associated to the G_m-action on Y.)
    Text
      The cohomology sheaf of this complex in degree 0 is @TO{"cotangentSheaf"," X"}@. If $X$ is smooth as a stack
      (that is, if $X$ is "quasi-smooth" in the coarse moduli space),
      then @TT "naiveCotangentComplex X"@ is equivalent (in the derived category of $X$)
      to the whole cotangent complex of X.
    Text
      If the characteristic is 0 or the characteristic $p > 0$ does not divide any of the weights,
      then $X$ is a Deligne-Mumford stack, and so the degree-1 cohomology sheaf
      of the cotangent complex of $X$ is zero.
      In that case, @TT "naiveCotangentComplex X"@ is equivalent (in the derived category of $X$)
      to cotangentSheaf(X). But without those assumptions,
      @TT "naiveCotangentComplex X"@ should be considered as more natural
      than its cohomology sheaf in degree 0. Eventually,
      one might want to consider the full cotangent complex of $X$,
      or at least its truncation to degrees $\geq -1$ rather than $\geq 0$.
    Text
      In the following example, the cohomology sheaf of the naive cotangent complex in degree 1
      is nonzero at the point $[0,0,1]$ (and only there), because the characteristic 5 divides the last weight.
    Example
      X1 = ProjectiveStack_(ZZ/5)(1,3,5);
      C1 = naiveCotangentComplex X1;
      S0 = HH^0 C1;
      S1 = HH^1 C1;
      dim S0
      dim S1
  SeeAlso
    cotangentSheaf
    cotangentSurjection
    (cotangentSheaf, ZZ, Variety)
    ProjectiveVariety

Node
  Key
   (naiveCotangentComplex, ZZ, ProjectiveVariety)
  Headline
    exterior powers of the naive cotangent complex of a subspace of weighted projective space
  Usage
    naiveCotangentComplex(m,X)
  Inputs
    m:ZZ
    X:ProjectiveVariety
  Outputs
    :Complex
  Description
    Text
      This is only relevant for $X$ a closed subspace of a weighted projective space,
      rather than the usual projective space.
    Text
      Let $X$ be a closed subspace of a weighted projective space over a ring $k$, with $X$
      viewed as a stack. (Namely, if $Y$ denotes the affine cone over $X$ in affine space $A^{n+1}$,
      $X$ is the quotient stack $[(Y-0)/G_m]$, for the action of the multiplicative group $G_m$
      on $A^{n+1}$ with given weights.)
      The cotangent complex of $X$ lives in cohomological degrees $\leq 1$.
      Here naiveCotangentComplex(m, X) computes its $m$th exterior power truncated to degrees $\geq 0$,
      which lives in degrees from 0 to $m$.
    Text
      The output is a complex of coherent sheaves in cohomological degrees from 0 to $m$. It can be viewed as
      the complex of $G_m$-equivariant sheaves
      $$0 \to \Omega^m_Y \to \Omega^{m-1}_Y \otimes\mathfrak{g}^* \to \cdots
      \to O_Y\otimes S^m(\mathfrak{g}^*)\to 0,$$
      where $\mathfrak{g}$ is the Lie algebra of $G_m$. (The boundary maps are given
      by plugging in the vector field associated to the $G_m$-action on $Y$.)
    Text
      The cohomology sheaf of this complex in degree 0 is @{"cotangentSheaf","(m, X)"}@. If $X$ is smooth as a stack
      (that is, if $X$ is "quasi-smooth" in the coarse moduli space),
      then @TT "naiveCotangentComplex(m, X)"@ is equivalent (in the derived category of $X$)
      to the $m$th exterior power of the whole cotangent complex of $X$.
    Text
      If the characteristic is 0 or the characteristic $p > 0$ does not divide any of the weights,
      then $X$ is a Deligne-Mumford stack, and so the degree-1 cohomology sheaf
      of the cotangent complex of $X$ is zero.
      In that case, @TT "naiveCotangentComplex(m, X)"@ is equivalent (in the derived category of $X$)
      to @TT "cotangentSheaf(m, X)"@. But without those assumptions,
      @TT "naiveCotangentComplex (m, X)"@ should be considered as more natural
      than its cohomology sheaf in degree 0.
    Text
      In the following example, the cohomology sheaf of the naive cotangent complex in degree 1
      is nonzero at the point $[0,0,1]$ (and only there), because the characteristic 5 divides the last weight.
    Example
      X1 = ProjectiveStack_(ZZ/5)(1,3,5);
      C1 = naiveCotangentComplex(1, X1);
      S0 = HH^0 C1;
      S1 = HH^1 C1;
      dim S0
      dim S1
  SeeAlso
    cotangentSheaf
    cotangentSurjection
    (cotangentSheaf, ZZ, Variety)
    ProjectiveVariety

Node
  Key
   (hh, ZZ, Complex)
  Headline
    dimension of the cohomology of a projective variety with coefficients in a complex of coherent sheaves
  Usage
    hh^i(C)
  Inputs
    i:ZZ
    C:Complex
      on a ProjectiveVariety @TT "X"@
  Outputs
    :ZZ
      dimension of the $i$-th cohomology group of @TT "X"@ with coefficients in @TT "C"@
  Description
    Text
      The command computes the dimension of the $i$-th cohomology group of @TT "X"@ with coefficients in @TT "C"@
      (sometimes called "hypercohomology"),
      as a vector space over the coefficient field of @TT "X"@.
      If you want to compute cohomology with many twists, the functions @TO2{(hh,ZZ,Complex,ZZ,ZZ),"hh^i(C,b1,b2)"}@
      or @TO2{(hh,ZZ,SumOfTwistsComplex),"hh^i(C(*))"}@ should be more convenient than running this program repeatedly.
    Text
      The base space of @TT "C"@ may be a closed subspace @TT "X"@ of a weighted projective space.
      The distinction between @TT "X"@ as a stack and its associated coarse moduli space,
      $e\colon X \to W$, does not matter for computing cohomology.
      Twists such as @TT "C(a)"@
      are interpreted by tensoring with the line bundles $O(a)$ on the stack.
    Text
      The base ring should be a field. Note that it is usually faster
      to work over $\mathbb{Z}/p$ for a prime number $p \leq 32767$, say @TT "ZZ/31991"@, rather than over @TT "QQ"@.
    Text
      For example, the following complex @TT "C1"@ of sheaves on $\mathbb{P}^2$ is isomorphic in the derived category
      to the direct image of the sheaf of regular functions on $\mathbb{P}^1$; so they have the same cohomology.
    Example
      R = ZZ/31991[x,y,z];
      PP2 = Proj R;
      M1 = R(-1); M0 = R(0);
      C1 = sheaf complex map(M0, M1, matrix(R, {{x}}))
      hh^0(C1(5))
      PP1 = Proj(R/(x));
      hh^0(OO_PP1(5))
  SeeAlso
    (hh, ZZ, CoherentSheaf)
    (hh, ZZ, SumOfTwists)
    (cohomology, ZZ, SumOfTwists)
    (euler, CoherentSheaf)

Node
  Key
   (hh, ZZ, Complex, ZZ, ZZ)
  Headline
    dimension of a projective variety with coefficients in a complex of sheaves with a range of twists
  Usage
    hh^i(C,b,c)
  Inputs
    i:ZZ
    C:Complex
      on a ProjectiveVariety $X$
    b:ZZ
    c:ZZ
  Outputs
    :RingElement
      the Laurent polynomial $\sum_{j=b}^c h^i(X,C(j))T^j$, giving the dimensions of the $i$-th cohomology groups
      of @TT "X"@ with coefficients in @TT "C(j)"@ for $j$ from $b$ to $c$
  Description
    Text
      This function computes the dimension of the $i$-th cohomology group of @TT "X"@
      (sometimes called "hypercohomology") with coefficients
      in the twist $C(j)=C\otimes O_X(j)$
      for each $j$ from $b$ to $c$,
      as a vector space over the coefficient field of $X$.
      In some cases, you may prefer the function @TO2{(hh,ZZ,SumOfTwistsComplex),"hh^i(C(*))"}@,
      which computes the cohomology with all twists at once.
    Text
      The base space of @TT "C"@ may be a closed subspace @TT "X"@ of a weighted projective space.
      The distinction between @TT "X"@ as a stack and its associated coarse moduli space,
      $e\colon X \to W$, does not matter for computing cohomology.
      Twists such as @TT "C(a)"@
      are interpreted by tensoring with the line bundles $O(a)$ on the stack.
    Text
      The base ring should be a field. Note that it is usually faster
      to work over $\mathbb{Z}/p$ for a prime number $p \leq 32767$, say @TT "ZZ/31991"@, rather than over @TT "QQ"@.
    Text
      The Euler characteristic is faster to compute, for example by the function
      @TO2{(euler,CoherentSheaf),"euler(F)"}@.
    Text
      For example, the following complex @TT "C1"@ of sheaves on $\mathbb{P}^2$ is isomorphic in the derived category
      to the direct image of the sheaf of regular functions on $\mathbb{P}^1$; so they have the same cohomology.
    Example
      R = ZZ/31991[x,y,z];
      PP2 = Proj R;
      M1 = R(-1); M0 = R(0);
      C1 = sheaf complex map(M0, M1, matrix(R, {{x}}))
      hh^0(C1,-5,5)
      PP1 = Proj(R/(x));
      hh^0(OO_PP1,-5,5)
  SeeAlso
    (hh, ZZ, Complex)
    (hh, ZZ, SumOfTwistsComplex)
    (cohomology, ZZ, SumOfTwists)
    (euler, CoherentSheaf)

Node
  Key
   (hh, ZZ, SumOfTwistsComplex)
  Headline
    dimension of cohomology of a projective variety with coefficients in a complex of sheaves with all twists
  Usage
    hh^i(C(*))
  Inputs
    i:ZZ
    C(*):SumOfTwistsComplex
  Outputs
    :Sequence
      The output describes the formal series $\sum_{j\in\mathbb{Z}} h^i(X,C(j))T^j$ by a sequence consisting of
      some text explanation, and also:
      the infimum of the weights j such that $H^i(X,C(j))$ is not zero (possibly $-\infty$ or,
      if the cohomology is zero in all weights, $\infty$) as entry 1, the supremum of such weights (entry 3),
      a rational function in $T$ (entry 5), and a rational function in $U = T^{-1}$ (entry 7),
      such that the sum of the latter two functions
      as Laurent series is $\sum_{j\in\mathbb{Z}} h^i(X, C(j)) T^j$. (The two functions do not overlap,
      as formal series in T.)
  Description
    Text
      The command computes the dimension of the $i$th cohomology group of @TT "X"@ (sometimes called "hypercohomology")
      with coefficients in the twist $C(j)=C\otimes O_X(j)$ 
      for every integer $j$,
      as a vector space over the coefficient field of @TT "X"@. The algorithm is based on local duality.
      In some cases, you may prefer the function @TO2{(hh,ZZ,Complex,ZZ,ZZ),"hh^i(C,b,c)"}@,
      which computes the cohomology with all twists from $b$ to $c$.
    Text
      The base space of @TT "C"@ may be a closed subspace @TT "X"@ of a weighted projective space.
      The distinction between @TT "X"@ as a stack and its associated coarse moduli space,
      $e\colon X \to W$, does not matter for computing cohomology.
      Twists such as @TT "C(a)"@
      are interpreted by tensoring with the line bundles $O(a)$ on the stack.
    Text
      The base ring should be a field. Note that it is usually faster
      to work over $\mathbb{Z}/p$ for a prime number $p \leq 32767$, say @TT "ZZ/31991"@, rather than over @TT "QQ"@.
    Text
      Euler characteristics are faster to compute, for example by the function
      @TO2{(euler,CoherentSheaf,ZZ,ZZ),"euler(F,b,c)"}@.
    Text
      For example, the following complex @TT "C1"@ of sheaves on $\mathbb{P}^2$ is isomorphic in the derived category
      to the direct image of the sheaf of regular functions on $\mathbb{P}^1$; so they have the same cohomology.
    Example
      R = ZZ/31991[x,y,z];
      PP2 = Proj R;
      M1 = R(-1); M0 = R(0);
      C1 = sheaf complex map(M0, M1, matrix(R, {{x}}))
      hh^0(C1(*))
      PP1 = Proj(R/(x));
      hh^0(OO_PP1(*))
  SeeAlso
    (hh, ZZ, CoherentSheaf)
    (hh, ZZ, CoherentSheaf, ZZ, ZZ)
    (cohomology, ZZ, SumOfTwists)
    (euler, CoherentSheaf)

Node
  Key
    ext
    (ext, ZZ, Complex, Complex)
  Headline
    dimension of an Ext group between complexes of sheaves on a projective variety
  Usage
    ext^i(C,D)
  Inputs
    i:ZZ
    C:Complex
    D:Complex
      on a ProjectiveVariety @TT "X"@
  Outputs
    :ZZ
      dimension of the $i$-th Ext group from @TT "C"@ to @TT "D"@ over @TT "X"@
  Description
    Text
      The command computes the dimension of the $i$-th global Ext group over @TT "X"@ from @TT "C"@ to @TT "D"@,
      as a vector space over the coefficient field of @TT "X"@.
      If you want to compute Ext with many twists, the function
      @TO2{(ext,ZZ,Complex,SumOfTwistsComplex),"ext^i(C,D(*))"}@
      should be more convenient than running this program repeatedly.
    Text
      The complexes @TT "C"@ and @TT "D"@ must be defined over the same space @TT "X"@.
      Here @TT "X"@ may be a closed subspace of a weighted projective space.
      It is viewed as an algebraic stack when that makes a difference.
      In particular, twists are interpreted by tensoring with the line bundles O(a) on the stack.
    Text
      The base ring should be a field. Note that it is usually faster
      to work over $\mathbb{Z}/p$ for a prime number $p \leq 32767$, say @TT "ZZ/31991"@, rather than over @TT "QQ"@.
    Text
      For example, the following complex @TT "D"@ of sheaves on $\mathbb{P}^2$ is isomorphic in the derived category
      to the direct image of the sheaf of regular functions on $\mathbb{P}^1$; so they have the same cohomology.
      We can also view cohomology as Ext from the sheaf of regular functions.
    Example
      R = ZZ/31991[x,y,z];
      PP2 = Proj R;
      M1 = R(-1); M0 = R(0);
      C = sheaf complex M0
      D = sheaf complex map(M0, M1, matrix(R, {{x}}))
      ext^0(C,D(5))
      hh^0(D(5))
      PP1 = Proj(R/(x));
      hh^0(OO_PP1(5))
  SeeAlso
    (hh, ZZ, Complex)
    (hh, ZZ, SumOfTwistsComplex)
    (cohomology, ZZ, SumOfTwists)
    (euler, CoherentSheaf)

Node
  Key
   (ext, ZZ, Complex, SumOfTwistsComplex)
  Headline
    dimension of Ext between complexes of sheaves over a projective variety, with all twists
  Usage
    ext^i(C,D(*))
  Inputs
    i:ZZ
    C:Complex
    D(*):SumOfTwistsComplex
  Outputs
    :Sequence
      The output describes the formal series $\sum_{j\in\mathbb{Z}} \text{dim}_k Ext^i_X(C,D(j))T^j$ by a sequence consisting of
      some text explanation, and also:
      the infimum of the weights j such that $Ext^i_X(C,D(j))$ is not zero (possibly $-\infty$ or,
      if the cohomology is zero in all weights, $\infty$) as entry 1, the supremum of such weights (entry 3),
      a rational function in $T$ (entry 5), and a rational function in $U = T^{-1}$ (entry 7),
      such that the sum of the latter two functions
      as Laurent series is $\sum_{j\in\mathbb{Z}} \text{dim}_k Ext^i_X(C, D(j)) T^j$. (The two functions do not overlap,
      as formal series in T.)
  Description
    Text
      The command computes the dimension of the $i$th global Ext group of @TT "X"@ from @TT "C"@
      to the twist $D(j)=D\otimes O_X(j)$ 
      for every integer $j$,
      as a vector space over the coefficient field of @TT "X"@. The algorithm is based on local duality.
    Text
      The complexes @TT "C"@ and @TT "D"@ must be defined over the same space @TT "X"@.
      Here @TT "X"@ may be a closed subspace of a weighted projective space.
      It is viewed as an algebraic stack when that makes a difference.
      In particular, twists are interpreted by tensoring with the line bundles O(a) on the stack.
    Text
      The base ring should be a field. Note that it is usually faster
      to work over $\mathbb{Z}/p$ for a prime number $p \leq 32767$, say @TT "ZZ/31991"@, rather than over @TT "QQ"@.
    Text
      For example, the following complex @TT "D"@ of sheaves on $\mathbb{P}^2$ is isomorphic in the derived category
      to the direct image of the sheaf of regular functions on $\mathbb{P}^1$; so they have the same cohomology.
      We can also view cohomology as Ext from the sheaf of regular functions.
    Example
      R = ZZ/31991[x,y,z];
      PP2 = Proj R;
      M1 = R(-1); M0 = R(0);
      C = sheaf complex M0
      D = sheaf complex map(M0, M1, matrix(R, {{x}}))
      ext^0(C,D(*))
      hh^0(D(*))
      PP1 = Proj(R/(x));
      hh^0(OO_PP1(*))
  SeeAlso
    (hh, ZZ, CoherentSheaf)
    (hh, ZZ, CoherentSheaf, ZZ, ZZ)
    (cohomology, ZZ, SumOfTwists)
    (euler, CoherentSheaf)
///

document {
    Key => SumOfTwistsComplex,
    Headline => "the class of all sums of twists of complexes",
    "This class is used internally to represent an infinite direct sum of twists of a complex of coherent sheaves.",
    EXAMPLE lines ///
	  R = QQ[x,y,z];
	  X = Proj R;
	  M1 = R(-1); M0 = R(0);
	  C1 = sheaf complex map(M0, M1, matrix(R, {{x}}))
	  hh^0(C1(*))
    ///
    }
