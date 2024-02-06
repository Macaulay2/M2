undocumented { presentationComplex }

doc ///
Key
  LocalRings
Headline
  Localizations of polynomial rings at prime ideals
Description
  Text
    This package defines the @TO LocalRing@ class for localizations of polynomial rings
    and extends most basic commutative algebra computations to such local rings.
    Moreover, the functions @TO hilbertSamuelFunction@ and @TO (length, Module)@ for
    Artinian modules over a local ring are implemented in this package.

    For information about the classical way of working with local rings at maximal ideals
    see @TO "replacements for functions from version 1.0"@.

    The following is an example of defining the rational quartic curve in $\PP^3$ localized
    at a maximal ideal and a prime ideal using two different methods.
  Example
    R = ZZ/32003[a..d];
    I = monomialCurveIdeal(R,{1,3,4})
    M = ideal"a,b,c,d"; -- maximal ideal at the origin
    P = ideal"a,b,c"; -- prime ideal
    RM = R_M
    RP = localRing(R, P)
  Text
    An ideal, module, or chain complex may either be localized using @TO promote@ or using
    the @TO tensor@ product.
  Example
    C = res I
    D = C ** RM;
    E = pruneComplex D
  Text
    The computation above shows that the rational quartic curve is not locally Cohen-Macaulay
    at the origin. Therefore the curve is not Cohen-Macaulay

    However, the curve is Cohen-Macaulay at the prime ideal $(a, b, c)$ (and in fact any other prime ideal).
  Example
    D' = C ** RP;
    E' = pruneComplex D'
  Text
    The elementary definitions and operations are declared in @TT "localring.m2"@.
    Engine routines for core computations are implemented in @TT "e/localring.hpp"@.

    The following commutative algebra computations are implemented in this package:
    @TO syz@, @TO resolution@, @TO mingens@, @TO minimalPresentation@, @TO trim@, @TO (length, Module)@,
    @TO isSubset@, @TO inducedMap@, @TO (quotient, Matrix, Matrix)@, @TO (remainder, Matrix, Matrix)@,
    @TO "Saturation :: quotient(Module,Module)"@, @TO saturate@, @TO annihilator@.
    Most of these routines rely on the functions @TO liftUp@ and @TO "PruneComplex :: pruneComplex"@ and
    take advantage of Nakayama's lemma and flatness of local rings.

    In addition, methods such as @TO map@, @TO modulo@, @TO subquotient@, @TO kernel@, @TO cokernel@,
    @TO image@, @TO homology@, @TO Hom@, @TO Ext@, @TO Tor@, etc. work over local rings automatically.
Caveat
  Currently limited to localization at prime ideals rather than arbitrary multiplicatively closed sets.
  Quotients of local rings are not implemented yet. Moreover, certain functions (such as symbol%,
  radical, minimalPrimes, leadingCoefficient) are ambiguous or not yet defined.
SeeAlso
  "PruneComplex :: PruneComplex"
Subnodes
  LocalRing
///

doc ///
Node
  Key
    "replacements for functions from version 1.0"
  Description
    Text
      This page describes the replacements for functions implemented in version 1.0 of this package by
      Mike Stillman and David Eisenbud. That version implemented functionality for finding minimal
      generators, syzygies and resolutions for polynomial rings localized at a maximal ideal.

      Defining a local ring using @TO setMaxIdeal@ and @TO localRing@:
    Example
      S = ZZ/32003[x,y,z,w]
      P = ideal(x,y,z,w)
      setMaxIdeal P -- version 1.0
      R = localRing(S, P) -- version 2.0 and above
-- TODO
--    Text
--      @TO localComplement@
--    Example
    Text
      Computing syzygies using @TO localsyz@ and @TO syz@:
    Example
      use S
      m = matrix{{x,y*z},{z*w,x}}
      m * localsyz m
      use R
      m = matrix{{x,y*z},{z*w,x}}
      m * syz m
    Text
      Computing syzygies using @TO localMingens@ and @TO mingens@:
    Example
      use S
      localMingens matrix{{x-1,x,y},{x-1,x,y}}
      use R
      mingens image matrix{{x-1,x,y},{x-1,x,y}}
    Text
      Computing syzygies using @TO localModulo@ and @TO modulo@:
    Example
      use S
      localModulo(matrix {{x-1,y}}, matrix {{y,z}})
      use R
      modulo(matrix {{x-1,y}}, matrix {{y,z}})
    Text
      Computing syzygies using @TO localPrune@ and @TO prune@:
    Example
      use S
      localPrune image matrix{{x-1,x,y},{x-1,x,y}}
      use R
      prune image matrix{{x-1,x,y},{x-1,x,y}}
    Text
      Computing syzygies using @TO localResolution@ and @TO resolution@:
    Example
      use S
      localResolution coker matrix{{x,y*z},{z*w,x}}
      oo.dd
      use R
      res coker matrix{{x,y*z},{z*w,x}}
      oo.dd
  Subnodes
    setMaxIdeal
    localComplement
    localsyz
    localMingens
    localModulo
    localPrune
    localResolution
///

doc ///
Node
  Key
    LocalRing
    residueMap
    maxIdeal
    (max, LocalRing)
  Headline
    The class of all local rings
  Description
    Text
      Currently only localizations at prime ideals of a polynomial ring are supported.
    Example
      S = QQ[x,y,z,w];
      I = ideal"xz-y2,yw-z2,xw-yz"; -- The twisted cubic curve
      R = S_I
      K = frac(S/I)
    Text
      The maximal ideal and a residue map to the residue field are stored in the ring.
    Example
      max R
      R.maxIdeal
      R.residueMap
    Text
      Objects over the base ring can be localized easily.
    Example
      I ** R
  SeeAlso
    PolynomialRing
    hilbertSamuelFunction
   (length, Module)
  Subnodes
    localRing
    liftUp
   (baseRing, LocalRing)
   (char, LocalRing)
   (coefficientRing, LocalRing)
   (degreeLength, LocalRing)
   (degrees, LocalRing)
   (dim, LocalRing)
   (frac, LocalRing)
   (generators, LocalRing)
   (isCommutative, LocalRing)
   (isWellDefined, LocalRing)
   (numgens, LocalRing)

Node
  Key
    localRing
   (localRing, Ring,       Ideal)
   (localRing, EngineRing, Ideal)
   (symbol_, PolynomialRing, Ideal)
   (symbol_, PolynomialRing, RingElement)
  Headline
    Constructor for local rings
  Usage
    R_P
    R_f
    localRing(R, P)
  Inputs
    R:PolynomialRing
      the base ring for the localization
    P:Ideal
      a prime ideal for the localization
    f:RingElement
      a ring element to localize (not yet implemented)
  Outputs
    :LocalRing
      the local ring $R_{\mathfrac p}$
  Description
    Text
      This is the constructor for the type @TO LocalRing@.
    Example
      R = QQ[x,y,z,w];
      P = ideal"xz-y2,yw-z2,xw-yz"; -- The twisted cubic curve
      I = ideal"xz-y2,z(yw-z2)-w(xw-yz)";
      RP = R_P
      M = RP^1/promote(I, RP)
      length M
    Text
      Note that the ideal $P$ is assumed to be prime. Use @TO (isWellDefined, LocalRing)@
      to confirm that a local ring is well defined.

Node
  Key
   (isWellDefined, LocalRing)
  Headline
    whether a local ring is well defined
  Description
    Text
      This method checks whether the local ring was produced by localization at a prime ideal.
    Example
      R = QQ[x,y,z,w]
      P = ideal"xz-y2,yw-z2,xw-yz"; -- The twisted cubic curve
      isWellDefined R_P
      Q = ideal"xz-y2,z(yw-z2)-w(xw-yz)";
      isWellDefined R_Q
  SeeAlso
    isPrime
///

doc ///
Key
   liftUp
  (liftUp, Thing)
  (liftUp, Ideal,  Ring)
  (liftUp, Module, Ring)
  (liftUp, Matrix, Ring)
  (liftUp, RingElement,   Ring)
  (liftUp, MutableMatrix, Ring)
Headline
  Lifts various objects over R_P to R.
Usage
  I = liftUp IP
  M = liftUp(MP, R)
Inputs
  MP: Matrix
    a matrix, module, or ideal over a local ring RP
  R: Ring
    a ring, RP is a localization of R
Outputs
  M: Matrix
    a matrix, module, or ideal over the ring R
Description
  Text
    Given an object, for instance an ideal IP, over a local ring (RP, P), this method returns the
    preimage of that object under the canonical map R -> RP after clearing denominators of IP.

    For matrices (hence most other objects as well), clearing denominators is performed columnwise.

    In conjunction with pruneComplex, liftUp is used to implement many of the elementary operations
    over local rings such as syz.

    Here is an example of computing the syzygy over a local ring using liftUp and pruneComplex:
  Example
    R = ZZ/32003[vars(0..5)];
    I = ideal"abc-def,ab2-cd2-c,-b3+acd";
    C = res I;
    M = ideal gens R;
    RM = localRing(R, M);
    F = C.dd_2;
    FM = F ** RM
  Text
    This is the process for finding the syzygy of FM:
  Example
    f = liftUp FM;
    g = syz f;
    h = syz g;
    C = {g ** RM, h ** RM};
  Text
    Now we prune the map h, which is the first map from the right:
  Example
    C = first pruneComplex(C, 1, Direction => "right");
    g' = C#0;
  Text
    Scale each row with the common denominator of the corresponding column in FM:
  Example
    N = transpose entries FM;
    for i from 0 to numcols FM - 1 do
      rowMult(g', i, N_i/denominator//lcm);
  Text
    The syzygy of FM is:
  Example
    GM = map(source FM, , matrix g')
    kernel FM == image GM
Caveat
  This is NOT the same as lift.
  Not tested with quotients properly.
SeeAlso
  pruneComplex
///

doc ///
Key
   hilbertSamuelFunction
  (hilbertSamuelFunction, Module, ZZ)
  (hilbertSamuelFunction, Module, ZZ, ZZ)
  (hilbertSamuelFunction, Ideal, Module, ZZ)
  (hilbertSamuelFunction, Ideal, Module, ZZ, ZZ)
Headline
  Computes the Hilbert-Samuel Function of Modules over Local Rings
Usage
  h = hilbertSamuelFunction(M, n)
  L = hilbertSamuelFunction(M, n0, n1)
  h = hilbertSamuelFunction(q, M, n)
  L = hilbertSamuelFunction(q, M, n0, n1)
Inputs
  M: Module
    a module over a local ring
  n:  ZZ
    single input to the function
  n0: ZZ
    first input to the function
  n1: ZZ
    last input to the function
  q: Ideal
    a parameter ideal
Outputs
  h: ZZ
    the integer output of the Hilbert-Samuel function
  L: Sequence
    an sequence of integer outputs of the Hilbert-Samuel function
Description
  Text
    Given a module over a local ring, a parameter ideal, and an integer, this method computes the
    Hilbert-Samuel function for the module. If parameter ideal is not given, the maximal ideal is
    assumed.

    Note:
     If computing at index n is fast but slows down at n+1, try computing at range (n, n+1).
     On the other hand, if computing at range (n, n+m) is slow, try breaking up the range.

    To learn more read Eisenbud, Commutative Algebra, Chapter 12.

    Here is an example from Computations in Algebraic Geometry with Macaulay2, pp. 61:
  Example
    R = QQ[x,y,z];
    RP = localRing(R, ideal gens R);
    I = ideal"x5+y3+z3,x3+y5+z3,x3+y3+z5"
    M = RP^1/I
    elapsedTime hilbertSamuelFunction(M, 0, 6)
    oo//sum

  Text
    An example of using a parameter ideal:
  Example
    R = ZZ/32003[x,y];
    RP = localRing(R, ideal gens R);
    N = RP^1
    q = ideal"x2,y3"
    elapsedTime hilbertSamuelFunction(N, 0, 5) -- n+1 -- 0.02 seconds
    elapsedTime hilbertSamuelFunction(q, N, 0, 5) -- 6(n+1) -- 0.32 seconds
Caveat
  Hilbert-Samuel function with respect to a parameter ideal other than the maximal ideal can be slower.
SeeAlso
  "Macaulay2Doc :: length(Module)"
///

importFrom_Core { "headline" }
scan({ baseRing, char, coefficientRing, degreeLength, degrees, dim, frac, generators, isCommutative, numgens },
    m -> document { Key => (m, LocalRing), Headline => headline makeDocumentTag (m, Ring), PARA {"See ", TO (m, Ring)} })

undocumented apply({describe, expression, precision, presentation, toExternalString}, m -> (m, LocalRing))

end--

doc ///
Key
Headline
Usage
Inputs
Outputs
Description
  Text
  Example
Caveat
SeeAlso
///
