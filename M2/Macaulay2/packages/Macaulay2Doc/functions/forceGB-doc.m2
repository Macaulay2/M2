-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

doc ///
  Key
    forceGB
    (forceGB,Matrix)
  Headline
    declare that the columns of a matrix are a Gröbner basis
  Usage
    forceGB f
  Inputs
    f:Matrix
  Outputs
    :GroebnerBasis
  Description
    Text
      Declares that the columns of the matrix @TT "f"@ constitute a Gröbner
      basis, autoreduces it, minimizes it, sorts it, and returns a Gröbner
      basis object declaring itself complete, without computing any S-pairs.

      Sometimes one knows that a set of polynomials (or columns of such) form a
      Gröbner basis, but @EM "Macaulay2"@ doesn't. This is the way to inform
      the system of this fact.
    Example
      gbTrace = 3;
      R = ZZ[x,y,z];
      f = matrix{{x^2-3, y^3-1, z^4-2}};
      g = forceGB f
    Text
      This Gröbner basis object is stored with the matrix and can be obtained
      as usual:
    Example
      g === gb(f, StopBeforeComputation=>true)
    Text
      Requesting a Gröbner basis for @TT "f"@ requires no computation.
    Example
      gens gb f
    Text
      If an autoreduced Gröbner basis (not just the Groebner basis object) is
      desired, replace @TT "forceGB f"@ by @TT "gens forceGB f"@.
  Caveat
    If the columns do not form a Gröbner basis, nonsensical answers may result
  SeeAlso
    "Gröbner bases"
  Subnodes
    [forceGB, ChangeMatrix]
    [forceGB, MinimalMatrix]
    [forceGB, SyzygyMatrix]
///
doc ///
  Key
    [forceGB, ChangeMatrix]
  Headline
    specify the change of basis matrix from a Gröbner basis to the generators of a module
  Usage
    forceGB(...,ChangeMatrix=>m)
  Inputs
    m:Matrix
  Consequences
    Item
      Set the change of basis matrix from the Gröbner basis to the original
      generators
  Description
    Text
      The matrix @TT "m"@ should have size a by b, where a is the number of
      columns of the original matrix, and b is the number of columns of f.
    Example
      gbTrace = 3
      R = ZZ[x,y,z];
      f = matrix{{x^2-3, y^3-1, z^4-2}};
      g = forceGB(f, ChangeMatrix=>id_(source f));
      x^2*y^3 // g
///
doc ///
  Key
    [forceGB, MinimalMatrix]
  Headline
    specify the minimal generator matrix
///
doc ///
  Key
    [forceGB, SyzygyMatrix]
  Headline
    specify the syzygy matrix
  Usage
    forceGB(f,SyzygyMatrix=>z,...)
  Inputs
    z:Matrix
  Consequences
    Item
      A request for the syzygy matrix of @TT "f"@ will return @TT "z"@
  Description
    Text
      In the following example, the only computation being performed when asked
      to compute the @TO kernel@ or @TO syz@ of @TT "f"@ is the minimal
      generator matrix of @TT "z"@.
    Example
      gbTrace = 3
      R = ZZ[x,y,z];
      f = matrix{{x^2-3, y^3-1, z^4-2}};
      z = koszul(2,f)
      g = forceGB(f, SyzygyMatrix=>z);
      syz g -- no extra computation
      syz f
      kernel f
    Text
      If you know that the columns of z already form a set of minimal
      generators, then one may use @TO forceGB@ once again.
  Caveat
    If the columns of @TT "z"@ do not generate the syzygy module of @TT "f"@,
    nonsensical answers may result
  SeeAlso
    "Gröbner bases"
///
