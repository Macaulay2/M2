-- -*- coding: utf-8 -*-
--- status: DRAFT.
--- author(s):  kummini
--- notes: 

doc ///
  Key
    target
  Headline
    target of a map
  Description
    Text
      This command returns the target of a morphism or Gröbner basis.
  SeeAlso
    source
///
doc ///
  Key
    (target,GroebnerBasis)
  Headline
    find target of a Gröbner basis
  Usage
    target g
  Inputs
    g:GroebnerBasis
  Outputs
    :
      the target of the matrix given by the generators of the Gröbner basis
  Description
    Example
      R = ZZ/10007[x,y,z];
      g = gb ideal(x^2, x*y-z^2, z^5);
      target g
///
doc ///
  Key
    (target,Matrix)
    (target,MutableMatrix)
  Headline
    find the target module of matrix
  Usage
    target f
  Inputs
    f:Matrix
  Outputs
    :
      the target module of @TT "f"@
  Description
    Example
      R = ZZ[x,y,z];
      M = R^1/(x,y,z);
      N = R^1/(x^2,y^2,x*y*z,z^2);
      g = map(N,M,x*y);
      target g
///
doc ///
  Key
    (target,RingMap)
  Headline
    find the target ring for a map of rings
  Usage
    target f
  Inputs
    f:
  Outputs
    :
      the target ring of the map
  Description
    Example
      S = ZZ/10007[x, y, z];
      R = ZZ/10007[t];
      f = map(R,S,{t^3,t^4,t^5})
      target f
///
