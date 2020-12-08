undocumented { installMinprimes }

doc ///
Node
  Key
    MinimalPrimes
  Headline
    minimal primes and radical routines for ideals
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
    (radical, Ideal)
    "minimal primes of an ideal"
    "radical of an ideal"

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
      See @TO "PrimaryDecomposition :: associated primes"@ for information on finding associated prime ideals
      and @TO "PrimaryDecomposition :: primary decomposition"@ for more information about finding the full primary decomposition
      of an ideal.

Node
  Key
    "radical of an ideal"
  Description
    Text
      There are two main ways to find the radical of an ideal.
      On some large examples the second method is faster.

      @SUBSECTION {"using ", TO radical}@
    Example
      S = ZZ/101[x,y,z]
      I = ideal(x^3-y^2,y^2*z^2)
      radical I
    Text
      @SUBSECTION {"using the", TO "minimal primes of an ideal"}@

      An alternate way to find the radical of an ideal @TT "I"@ is to take the intersection of its
      minimal prime ideals. To find the @TO "minimal primes of an ideal"@ @TT "I"@ use the
      function @TO minimalPrimes@. Then use @TO "intersect"@.
    Example
      intersect minimalPrimes I

Node
  Key
    minimalPrimes
   (minimalPrimes, Ideal)
   [minimalPrimes, Verbosity]
   [minimalPrimes, Strategy]
   [minimalPrimes, CodimensionLimit]
   [minimalPrimes, MinimalGenerators]
    (decompose,     Ideal)
   [(decompose,    Ideal), Verbosity]
   [(decompose,    Ideal), Strategy]
   [(decompose,    Ideal), CodimensionLimit]
   [(decompose,    Ideal), MinimalGenerators]
  Headline
    minimal primes of an ideal
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
      stop after finding primes of codimension less than or equal to this value
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
    "PrimaryDecomposition :: topComponents"
    "PrimaryDecomposition :: removeLowestDimension"
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
      See @TO [minimalPrimes, Strategy]@
    Verbosity => ZZ
      See @TO [minimalPrimes, Verbosity]@
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

--- author(s): Decker
Node
  Key
    radical
   (radical, Ideal)
   [radical, Strategy]
   [radical, Unmixed]
   [radical, CompleteIntersection]
  Headline
    the radical of an ideal
  Usage
    radical I
  Inputs
    I:Ideal
    Unmixed=>Boolean
      whether it is known that the ideal {\tt I} is unmixed.
      The ideal $I$ is said to be unmixed if all associated primes of $R/I$ have the same dimension.
      In this case the algorithm tends to be much faster.
    Strategy=>Symbol
      the strategy to use, either @TT "Decompose"@ or @TT "Unmixed"@
    CompleteIntersection=>Ideal
      an ideal @TT "J"@ of the same height as @TT "I"@ whose generators form a maximal regular sequence contained
      in @TT "I"@. Providing this option as a hint allows a separate, often faster, algorithm to be used to compute
      the radical. This option should only be used if @TT "J"@ is nice in some way. For example, if @TT "J"@ is
      randomly generated, but @TT "I"@ is relatively sparse, then this will most likely run slower than just giving
      the @TO "Unmixed"@ option.
  Outputs
    :Ideal
      the radical of @TT "I"@
  Description
    Text
      If I is an ideal in an affine ring (i.e. a quotient of a polynomial ring over a field), and
      if the characteristic of this field is large enough (see below), then this routine yields the
      radical of the ideal I. The method used is the Eisenbud-Huneke-Vasconcelos algorithm.

      The algorithms used generally require that the characteristic of the ground field is larger than
      the degree of each primary component. In practice, this means that if the characteristic is something
      like 32003, rather than, for example, 5, the methods used will produce the radical of @TT "I"@.
      Of course, you may do the computation over @TO "QQ"@, but it will often run much slower.
      In general, this routine still needs to be tuned for speed.
    Example
      R = QQ[x, y]
      I = ideal((x^2+1)^2*y, y+1)
      radical I
    Text
      If @TT "I"@ is @ofClass MonomialIdeal@, a faster, combinatorial algorithm is used.
    Example
      R = ZZ/101[a..d]
      I = intersect(ideal(a^2,b^2,c), ideal(a,b^3,c^2))
      elapsedTime radical(ideal I_*, Strategy => Monomial)
      elapsedTime radical(ideal I_*, Unmixed => true)
    Text
      For another example, see @TO "PrimaryDecomposition :: PrimaryDecomposition"@.
  References
    Eisenbud, Huneke, Vasconcelos, Invent. Math. 110 207-235 (1992).
  Caveat
    The current implementation requires that the characteristic of the ground field
    is either zero or a large prime (unless @TT "I"@ is @ofClass MonomialIdeal@).
  SeeAlso
    minimalPrimes
    "PrimaryDecomposition :: topComponents"
    "PrimaryDecomposition :: removeLowestDimension"
    "Saturation :: saturate"
    "Saturation :: Ideal : Ideal"
  Subnodes
    radicalContainment

Node
  Key
    radicalContainment
   (radicalContainment, Ideal,       Ideal)
   (radicalContainment, RingElement, Ideal)
   [radicalContainment, Strategy]
  Headline
    whether an element is contained in the radical of an ideal
  Usage
    radicalContainment(g, I)
  Inputs
    g:RingElement
    I:Ideal
  Outputs
    :Boolean
      true if @TT "g"@ is in the radical of @TT "I"@, and false otherwise
  Description
    Text
      This method determines if a given element @TT "g"@ is contained in the radical of a given
      ideal @TT "I"@. There are 2 algorithms implemented for doing so: the first (default) uses the
      Rabinowitsch trick in the proof of the Nullstellensatz, and is called with @TT "Strategy => \"Rabinowitsch\""@.
      The second algorithm, for homogeneous ideals, uses a theorem of Kollar to obtain an effective
      upper bound on the required power to check containment, together with repeated squaring, and
      is called with @TT "Strategy => \"Kollar\""@. The latter algorithm is generally quite fast if
      a Grobner basis of @TT "I"@ has already been computed. A recommended way to do so is to check
      ordinary containment, i.e. @TT "g % I == 0"@, before calling this function.
    Example
      d = (4,5,6,7)
      n = #d
      R = QQ[x_0..x_n]
      I = ideal homogenize(matrix{{x_1^(d#0)} | apply(toList(1..n-2), i -> x_i - x_(i+1)^(d#i)) | {x_(n-1) - x_0^(d#-1)}}, x_n)
      D = product(I_*/degree/sum)
      x_0^(D-1) % I != 0 and x_0^D % I == 0
      elapsedTime radicalContainment(x_0, I)
      elapsedTime radicalContainment(x_0, I, Strategy => "Kollar")
      elapsedTime radicalContainment(x_n, I, Strategy => "Kollar")
  SeeAlso
    radical

Node
  Key
    Hybrid
  Headline
    the class of lists encapsulating hybrid strategies
  Description
    Text
      This class is used to encapsulate hybrid strategies for certain method functions.

      This example demonstrates an example of what is meant by a hybrid strategy.
    Example
      debug MinimalPrimes
      R = ZZ/101[w..z];
      I = ideal(w*x^2-42*y*z, x^6+12*w*y+x^3*z, w^2-47*x^4*z-47*x*z^2);
      elapsedTime minimalPrimes(ideal I_*, Strategy => Hybrid{Linear,Birational,Factorization,DecomposeMonomials}, Verbosity => 2);
  SeeAlso
    "PrimaryDecomposition :: primaryDecomposition(...,Strategy=>...)"
///
