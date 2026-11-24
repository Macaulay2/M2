doc ///
  Key
    support
    (support,RingElement)
    (support,Matrix)
  Headline
    list of variables occurring in a polynomial or matrix
  Usage
    support f
  Inputs
    f:RingElement
      or @ofClass Matrix@ over a polynomial ring
  Outputs
    :List
      of @TO2 (RingElement, "ring elements")@, the variables occurring in
      @TT "f"@
  Description
    Example
      R = QQ[a..g]
      f = a^3+b^2*c+3*f^10*d-1+e-e
      support f
    Example
      M = matrix"a+b2,c+g2;c,a-1"
      support M
    Text
      If the ring is a polynomial ring over another polynomial ring, then the
      support contains all of the variables, even the ones in the coefficient
      ring. The ring of each of these is the ring of f.
    Example
      A = ZZ[a,b]; B = A[r,s,t]; C = B[x,y,z,w];
      f = (a+r+z+1)^2+y
      S = support f
      ring S_2 === ring f
    Text
      Here is one way to select only the top level variables.
    Example
      select(S, x -> index x < numgens C)
    Text
      To obtain a list of the integer indices of the variables one can use
      either @TO (indices, RingElement)@ or apply @TO index@ to each variable.
    Example
      indices f
      apply(support f, index)
  SeeAlso
    index
    (indices,RingElement)
    (indices,Matrix)
    (symbol_,Ring,ZZ)
///

doc ///
  Key
    (support,Ideal)
  Headline
    list of variables occurring in the generators of an ideal
  Usage
    support I
  Inputs
    I:
      an ideal in a polynomial ring
  Outputs
    :List
      of @TO2 (RingElement, "ring elements")@, the variables occurring in the
      generators of the ideal
  Description
    Example
      R = QQ[a..g]
      I = ideal(b,c,e+f)
      support I
///
