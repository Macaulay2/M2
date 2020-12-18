doc ///
   Key
     antipode
     (antipode, RingElement)
   Headline
     antipode for skew commuting polynomial rings
   Usage
     antipode f
   Inputs
     f:RingElement
       An element in any ring
   Outputs
     :RingElement
       An element in the same ring, the antipode of f
   Description
    Text
      If the ring does not have skew commuting variables, antipode is the identify function.
      
      If the ring does have skew commuting variables, then the antipode returns a new polynomial
      with the same monomials, and the coefficients have been modified to provide an isomorphism
      with the ring, and its opposite ring, i.e. this switches left and right modules of the ring.
      
      This is implemented in the following simple manner: each monomial which has exactly $d$
      skew commuting variables occuring (thus, with exponent 1 each), is multiplied by 
      $(-1)^{(d-1)(d-2)/2}$.
    Example
      R = ZZ/32003[a..f, SkewCommutative => true]
      F = 1 + a + b + a*b + b*c*d + a*b*c*d*e
      antipode F
    Text
      If the ring has some skew commuting variables, and some commutative ones, then
      only the skew ones are considered.
    Example
      S = ZZ/32003[a..f, SkewCommutative => {0,1,2}]
      F = 1 + a + b + a*b + b*c*d + a*b*c*d*e
      antipode F
    Text
      The transpose of a matrix applies this function to each entry.  This is basically
      because the transpose of a left module gives a right module, and we need to make that a
      left module again.  The key reason for this is so that the transpose of a complex
      remains a complex.
    Example
      use R
      M = matrix{{a*b-1, a*c-d, e-a*b*c}}
      N = syz M
      M*N
      (transpose N) * (transpose M)
    Text
      If we had not used the antipode, then this would be false!
    Example
      Mt = matrix transpose entries M -- this does not use antipode
      Nt = matrix transpose entries N -- this does not use antipode
      Nt*Mt -- is not zero!
   Caveat
     If the ring is a Weyl algebra, the antipode is the identity, which is probably not what
     is wanted.  If the ring is a quotient of a skew commuting polynomial ring $R/I$, then the antipode
     of an element should really be in the quotient ring $R/(antipode(I))$, but this
     function returns an element in the same ring.
   SeeAlso
     transpose
///
