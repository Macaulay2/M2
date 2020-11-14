doc ///
Node
  Key
    MinimalPrimes
  Headline
    minimal associated primes of an ideal
  Description
    Text
      Find the @TO "minimal primes of an ideal"@ in a polynomial ring over a prime field,
      or a quotient ring of that. These are the geometric components of the corresponding algebraic set.

      Multiple strategies are implemented via @TO2 {"Macaulay2Doc :: using hooks", "hooks"}@.
      In many cases the default @TT "Birational"@ strategy is {\it much} faster,
      although there are cases when the @TT "Legacy"@ strategy does better.
      For @ofClass MonomialIdeal@, a more efficient algorithm is used instead.

      @TT "minprimes"@ and @TT "decompose"@ are synonyms for @TO "minimalPrimes"@.
  Caveat
    Only works for ideals in (commutative) polynomial rings or quotients of polynomial rings over a prime field,
    might have bugs in small characteristic and larger degree (although, many of these cases are caught correctly).
  Subnodes
    minimalPrimes
    (isPrime, Ideal)
    "minimal primes of an ideal"

Node
  Key
    "minimal primes of an ideal"
  Description
    Text
      @SUBSECTION "using {\tt minimalPrimes}"@

      To obtain a list of the minimal associated primes for an ideal {\tt I}
      (i.e. the smallest primes containing {\tt I}), use the function @TO minimalPrimes@.
    Example
      R = QQ[w,x,y,z];
      I = ideal(w*x^2-42*y*z, x^6+12*w*y+x^3*z, w^2-47*x^4*z-47*x*z^2)
      minimalPrimes I
    Text
      If the ideal given is a prime ideal then @TO minimalPrimes@ will return the ideal given.
    Example
      R = ZZ/101[w..z];
      I = ideal(w*x^2-42*y*z, x^6+12*w*y+x^3*z, w^2-47*x^4*z-47*x*z^2);
      minimalPrimes I
    Text
      See @TO "Macaulay2Doc :: associated primes of an ideal"@ for information on finding associated prime ideals
      and @TO "Macaulay2Doc :: primary decomposition"@ for more information about finding the full primary decomposition
      of an ideal.

Node
  Key
   (minimalPrimes, Ideal)
    minimalPrimes
   (decompose,     Ideal)
    decompose
-- FIXME: this is due to using Options => true
   [(minimalPrimes, Ideal), Verbosity]
   [(minimalPrimes, Ideal), Strategy]
   [(minimalPrimes, Ideal), CodimensionLimit]
   [(minimalPrimes, Ideal), MinimalGenerators]
  Headline
    minimal associated primes of an ideal
  Usage
    minimalPrimes I
    minprimes I
    decompose I
  Inputs
    I:Ideal
    Verbosity => ZZ
      a larger number will print more information during the computation
    Strategy => String
      specifies the computation strategy to use. If it is slow, try @TT "Legacy"@, @TT "Birational"@, or @TT "NoBirational"@.
      The strategies might change in the future, and there are other undocumented ways to fine-tune the algorithm.
    CodimensionLimit => ZZ
      stop after finding components of codimension less than or equal to this value
    MinimalGenerators=>Boolean
      if false, the minimal primes will not be minimalized
  Outputs
    :List
      with entries the minimal associated primes of @TT "I"@
  Description
    Text
      Given an ideal in a polynomial ring, or a quotient of a polynomial ring whose base ring
      is either {\tt QQ} or {\tt ZZ/p}, this function computes the minimal associated primes of
      the ideal @TT "I"@. Geometrically, it decomposes the algebraic set defined by @TT "I"@.
    Example
      R = ZZ/32003[a..e]
      I = ideal"a2b-c3,abd-c2e,ade-ce2"
      C = minprimes I;
      netList C
    Example
      C2 = minprimes(I, Strategy=>"NoBirational", Verbosity=>2)
      C1 = minprimes(I, Strategy=>"Birational", Verbosity=>2)

    Text
      Example. The homogenized equations of the affine twisted cubic curve define the union of the
      projective twisted cubic curve and a line at infinity:
    Example
      R = QQ[w,x,y,z];
      I=ideal(x^2-y*w, x^3-z*w^2)
      minimalPrimes I
    Text
      Note that the ideal is decomposed over the given field of coefficients and not over the extension
      field where the decomposition into absolutely irreducible factors occurs:
    Example
      I = ideal(x^2 + y^2)
      minimalPrimes I
    Text
      For monomial ideals, the method used is essentially what is shown in the example.
    Example
      I = monomialIdeal ideal"wxy,xz,yz"
      minimalPrimes I
      P = intersect(monomialIdeal(w,x,y), monomialIdeal(x,z), monomialIdeal(y,z))
      minI = apply(P_*, monomialIdeal @@ support)
    Text
      It is sometimes useful to compute @TT "P"@ instead, where each generator encodes a single
      minimal prime. This can be obtained directly, as in the following code.
    Example
      dual radical I
      P == oo
  SeeAlso
    (isPrime, Ideal)
    topComponents
    removeLowestDimension
    radical
    irreducibleCharacteristicSeries
    "Macaulay2Doc :: dual(MonomialIdeal)"

Node
  Key
    (isPrime, Ideal)
    [(isPrime, Ideal), Strategy]
    [(isPrime, Ideal), Verbosity]
  Headline
    whether an ideal is prime
  Usage
    isPrime I
  Inputs
    I:Ideal
      in a polynomial ring or quotient of one
    Strategy => String
      See @TO [(minimalPrimes, Ideal), Strategy]@
    Verbosity => ZZ
      See @TO [(minimalPrimes, Ideal), Verbosity]@
  Outputs
    :Boolean
      @TO "true"@ if {\tt I} is a prime ideal, or @TO "false"@ otherwise
  Description
    Text
      This function determines whether an ideal in a polynomial ring is prime.
    Example
      R = QQ[a..d];
      I = monomialCurveIdeal(R, {1, 5, 8})
      isPrime I
  SeeAlso
    minimalPrimes
///
