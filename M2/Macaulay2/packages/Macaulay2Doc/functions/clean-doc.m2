--- status: TODO
--- author(s): 
--- notes: 

doc ///
  Key
    clean
    (clean,RR,Matrix)
    (clean,RR,MutableMatrix)
    (clean,RR,Number)
    (clean,RR,RingElement)
  Headline
    set to zero elements that are approximately zero
  Usage
    clean(epsilon,M)
    clean_epsilon M
  Inputs
    epsilon:RR
    M:Matrix
      or a @ofClass RingElement@
  Outputs
    :
      @ofClass Matrix@, or @ofClass RingElement@
  Description
    Text
      If the input is @ofClass Matrix@ or @ofClass RingElement@, then the
      result has the same type, where each real or complex number coefficient
      that is less than @TT "epsilon"@ in absolute value is replaced with zero.
    Example
      e = 1e-11;
      M = random(RR^4,RR^4)
      M * (M + 1) + 1 - M^2 - M
      clean_e oo
    Text
      Cleaning a polynomial is a way to get rid of small terms.
    Example
      CC[x];
      f = product(5,j -> x - exp(2*pi*j*ii/5))
      clean_e f
  SeeAlso
    norm
    RR
    CC
    fillMatrix
///
