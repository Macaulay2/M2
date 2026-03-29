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
    Example
      X1 = ProjectiveStack_(ZZ/5)(1,3,5);
      C1 = naiveCotangentComplex X1;
      S0 = HH^0 C1;
      S1 = HH^1 C1;
      dim S0
      dim S1 -- The sheaf S1 is supported at the point [0,0,1], because the characteristic 5 divides the last weight.
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
    Example
      X1 = ProjectiveStack_(ZZ/5)(1,3,5);
      C1 = naiveCotangentComplex(1, X1);
      S0 = HH^0 C1;
      S1 = HH^1 C1;
      dim S0
      dim S1 -- The sheaf S1 is supported at the point [0,0,1], because the characteristic 5 divides the last weight.
  SeeAlso
    cotangentSheaf
    cotangentSurjection
    (cotangentSheaf, ZZ, Variety)
    ProjectiveVariety
  
///
