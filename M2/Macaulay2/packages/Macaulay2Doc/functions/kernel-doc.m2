doc ///
Node
  Key
    kernel
  Headline
    kernel of a map of modules or rings
  Description
   Text
    See the separate documentation nodes for the two cases. To compute
    the kernel of a map of free modules the command @TO syz@ is usually faster,
    since it computes only the generators of the kernel, not the relations on them,
    as is necessary to return the kernel as a module.
  SeeAlso
    source
    syz
///

document {
  Key => {(kernel,Matrix),
  (kernel, RingElement)},
  Headline => "kernel of a map of modules",
  Usage => "kernel f, kernel a",
  Inputs => {
    "f" => {"a map of modules ", TT "M --> N"}
  },
  Outputs => {
    Module => {"the kernel of f, a submodule of M"}
  },
  PARA{},
  "The kernel is the submodule of M of all elements mapping to zero under ", TT "f", ".",
  "If f is a RingElement it is interpreted as a 1 by 1 matrix",".",
  EXAMPLE lines ///
    R = ZZ/32003[a,b]/(ideal(a,b))^3
    M = R^1/(ideal a^2)
    mat = matrix{{a^2,b^2},{b,a}}
    ker mat
    presentation ker mat
    syz mat
    f = map(M++M, M++M, mat)
    ker f
  ///,
  SeeAlso => {
    syz,
    cokernel,
    image,
    map,
    matrix
  }
}

doc ///
Key
  [kernel, SubringLimit]
Headline
    stop after finding enough elements of a subring
Description
  Text
    @TT "SubringLimit => n"@ -- an option for @TO kernel@ which
    causes the computation of the kernel of a ring map to stop after @TT "n"@
    elements have been discovered.
Caveat
  Used only for computing the kernel of a @TO RingMap@.
///


doc ///
Key
  [kernel, DegreeLimit]
Headline
    stop after finding enough elements of a subring
Description
  Text
    @TT "DegreeLimit => d"@ -- an option for @TO kernel@ which
    causes the computation of the kernel of a ring map to stop after generators for the degree @TT "d"@
    part of the kernel have been discovered.
Caveat
  Used only for computing the kernel of a @TO RingMap@.
///


doc ///
Key
  [kernel, Strategy]
Headline
  strategy for Groebner Basis computations used in kernel computations
Description
  Text
    This option is passed through to @TO gb@ when computing the kernel of a @TT "RingMap"@.
Caveat
  Used only for computing the kernel of a @TO RingMap@.
SeeAlso
  gb
///


doc ///
  Key
    (kernel, RingMap)
  Headline
    kernel of a ringmap
  Usage
    kernel f
  Inputs
    f:RingMap
      $f: R \rightarrow S$
    SubringLimit => ZZ
      stop the computation after this many elements of the kernel have been found.
    DegreeLimit => ZZ
      stop the computation after generators for the kernel in this degree have been found
    Strategy => String
      Groebner basis computation strategy. See @TO gb@.
  Outputs
    :Ideal
      an ideal of $R$
  Description
    Text
      The twisted cubic.
    Example
      R = QQ[a..d];
      S = QQ[s,t];
      F = map(S,R,{s^3, s^2*t, s*t^2, t^3})
      ker F
    Text
      Passing @TT "SubringLimit"@ stops the computation early.
    Example
      R = QQ[a..d];
      S = QQ[s,t];
      F = map(S,R,{s^3, s^2*t, s*t^2, t^3})
      K = ker(F, SubringLimit => 1)
      assert(numgens K == 1)
    Text
      In the case when everything is homogeneous, Hilbert functions are used to speed up the computations.
  Caveat
    It should be possible to interrupt the computation and restart it, but this has not yet been implemented.
  SeeAlso
    "substitution and maps between rings"
    "elimination of variables"
    monomialCurveIdeal
  Subnodes
    kernel
    "kernel(...,SubringLimit=>...)"
    "kernel(...,DegreeLimit=>...)"
    "kernel(...,Strategy=>...)"
///
