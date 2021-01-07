doc ///
Key
  LocalRings
   localRing
  (localRing, Ring, Ideal)
Headline
  Localizing polynomial rings at a prime ideal
Description
  Text
    The basic definition of localization of polynomial rings at prime ideals along with various
    elementary operations are defined in m2/localring.m2, which in turn depends on a raw local ring
    type defined in e/localrings.hpp. This package extends the following methods to such local rings:
    syz, resolution, length, trim, mingens, minimalPresentation, symbol//, inducedMap, symbol:,
    saturate, annihilate.

    Note: Methods isSubset and symbol== are fixed in m2/modules2.m2 and reduce is fixed in m2/matrix.m2.
    Many other methods that only rely on the methods above, such as map, modulo, subquotient, kernel,
    cokernel, image, homology, Hom, Ext, Tor, etc. work for local rings automatically.

    If you need specific methods that do not work, please inform Mahrud Sayrafi.
  Example
    R = ZZ/32003[a..d];
  Text
    Rational quartic curve in P^3:
  Example
    I = monomialCurveIdeal(R,{1,3,4})
    C = res I
    M = ideal"a,b,c,d"; "maximal ideal at the origin";
    P = ideal"a,b,c"; "prime ideal";
    RM = localRing(R, M);
    D = C ** RM;
    E = pruneComplex D
  Text
    That is to say, the rational quartic curve is not locally Cohen-Macaulay at the origin.
    Therefore the curve is not Cohen-Macaulay
  Example
    RP = localRing(R, P);
    D' = C ** RP;
    E' = pruneComplex D'
  Text
    However, the curve is Cohen-Macaulay at the prime ideal P (and in fact any other prime ideal)
Caveat
  Currently limited to localization at prime ideals rather than any multiplicatively closed set.
  Quotients of local rings are not implemented yet. Moreover, certain functions (such as symbol%,
  radical, minimalPrimes, leadingCoefficient) are ambiguous or not yet defined.
SeeAlso
  PruneComplex
///

doc ///
Key
   liftUp
  (liftUp, Thing)
  (liftUp, Ideal, Ring)
  (liftUp, Module, Ring)
  (liftUp, Matrix, Ring)
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
    preimage of that object under the cannonical map R -> RP after clearing denominators of IP.

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

-- See (length, Module) in packages/Macaulay2Doc/functions/degree-doc.m2.

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
