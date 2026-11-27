--- status: DRAFT
--- author(s): MES
--- notes: 

doc ///
  Key
    isInjective
    (isInjective, Matrix)
    (isInjective, RingMap)
  Headline
    whether a map is injective
  Usage
    isInjective f
  Inputs
    f:Nothing
      @ofClass Matrix@, or @ofClass RingMap@
  Outputs
    :Boolean
      whether the kernel is zero
  Description
    Text
      This function computes the kernel, and checks whether it is zero.
    Example
      R = QQ[a..d];
      F = matrix{{a,b},{c,d}}
      isInjective F
      G = substitute(F, R/(det F))
      isInjective G
    Text
      Similarly for ring maps:
    Example
      S = QQ[r,s,t];
      phi = map(S,R,{r^3, r^2*s, r*s*t, s^3})
      isInjective phi
      S' = coimage phi
      phi' = phi * map(R,S')
      isInjective phi'
  Caveat
    One could imagine a faster routine for this. If you write one, please send
    it to us!
  SeeAlso
    kernel
    isSurjective
    (coimage,RingMap)
    det
    "substitution and maps between rings"
///
