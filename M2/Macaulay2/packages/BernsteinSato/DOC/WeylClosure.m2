doc ///
  Key
    WeylClosure
    (WeylClosure, Ideal)
    (WeylClosure, Ideal, RingElement)
  Headline
    Weyl closure of an ideal
  Usage
    WeylClosure I
    WeylClosure(I,f)
  Inputs
    I:Ideal
      a left ideal of the Weyl Algebra
    f:RingElement
      a polynomial
  Outputs
    :Ideal
      the Weyl closure (w.r.t. $f$) of $I$
  Description
    Text
     Let $D$ be the Weyl algebra 
     with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$ over a field $K$
     of characteristic zero, and denote
     $R = K(x_1..x_n)<\partial_1..\partial_n>$, 
     the ring of differential
     operators with rational function coefficients. The {\em Weyl closure}
     of an ideal $I$ in $D$ is the intersection of the extended ideal
     $R I$ with $D$.  It consists of all operators which vanish on the common
     holomorphic solutions of $D$ and is thus analogous to the radical
     operation on a commutative ideal.
     
     The 
     {\em partial Weyl closure} of $I$ with respect to a polynomial $f$
     is the intersection of the extended ideal $D[f^{-1}] I$ with $D$.
     
     The Weyl closure is computed by localizing $D/I$ with respect to
     a polynomial $f$ vanishing on the singular locus, and computing
     the kernel of the map $D \to D/I \to (D/I)[f^{-1}]$.
    Example
      makeWA(QQ[x])
      I = ideal(x*dx-2)
      holonomicRank I
      WeylClosure I
  Caveat
    The ideal I should be of finite holonomic rank, which can be tested
    manually by using the function holonomicRank. The Weyl closure of non-finite rank
    ideals or arbitrary submodules has not been implemented.
  SeeAlso
    Dlocalize
    singLocus
    holonomicRank
///
