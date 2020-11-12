doc ///
Key
  MinimalPrimes
Headline
  experimental package: minimal primes of an ideal
Description
  Text
    Find the minimal primes of an ideal in a polynomial ring over a prime field,
    or a quotient ring of that.  These are the geometric components
    of the corresponding algebraic set.

    The main routine is @TO "minprimes"@, although in a future
    release this will be renamed to {\tt minimalPrimes}.

    In many cases the new function is {\it much} faster,
    although there are cases when the older, current, version is faster.
Caveat
  Only works for ideals in (commutative)polynomial rings or quotients of
    polynomial rings over a prime field, might have bugs in small characteristic and larger degree
    (although, many of these cases are caught correctly).
SeeAlso
  decompose
  minimalPrimes
  isPrime
///

doc ///
   Key
     minprimes
   Headline
     minimal primes in a polynomial ring over a field
   Usage
     C = minprimes I
   Inputs
     I:Ideal
     Verbosity => ZZ
       A larger number will cause more output during the computation
     Strategy => String
       The default is fine for most things.  If it is slow, try "NoBirational".
         The strategies might change, so is it best to stick with these two
         for now (there are other undocumented strategies)
     CodimensionLimit => ZZ
       Only find components of codimension less than or equal to this value
   Outputs
     C:List
       a list of the minimal primes of I
   Description
    Text
      Given an ideal in a polynomial ring, or a quotient of a polynomial ring
      whose base ring is either {\tt QQ} or {\tt ZZ/p}, return a list
      of minimal primes of the ideal.
    Example
      R = ZZ/32003[a..e]
      I = ideal"a2b-c3,abd-c2e,ade-ce2"
      C = minprimes I;
      netList C
    Example
      C2 = minprimes(I, Strategy=>"NoBirational", Verbosity=>2)
      C1 = minprimes(I, Strategy=>"Birational", Verbosity=>2)
   Caveat
     This will eventually be made to work over GF(q), and over other fields too.
   SeeAlso
///

document {
     Key => {
	  decompose,
	 (decompose, Ideal),
	 (decompose, MonomialIdeal),
	  minimalPrimes,
	 (minimalPrimes, Ideal),
	 (minimalPrimes, MonomialIdeal)},
     Headline => "minimal associated primes of an ideal",
     Usage => "minimalPrimes I\ndecompose I",
     Inputs => {"I" => Ideal},
     Outputs => {List => {"whose entries are the minimal associated primes of ", TT "I", " ."}},
     TT "decompose", " is a synonym for ", TT "minimalPrimes", ".",
     PARA{},
     "This function computes the minimal associated primes
     of the ideal ", TT "I"," using characteristic sets. Geometrically,
     it decomposes the algebraic set defined by ", TT "I.",
     PARA{},
     "If I is ", ofClass MonomialIdeal, ", then a more efficient algorithm
     is used.",
     PARA{},
     "Example. The homogenized equations
      of the affine twisted cubic curve define the union of the
      projective twisted cubic curve and a line at infinity:",
     EXAMPLE lines ///
	  R=QQ[w,x,y,z];
	  I=ideal(x^2-y*w,x^3-z*w^2)
	  minimalPrimes I
	  ///,
     "Note that the ideal is decomposed over the given field
     of coefficients and not over the extension field where
     the decomposition into absolutely irreducible factors
     occurs:",
      EXAMPLE lines ///
	  I = ideal(x^2+y^2)
	  minimalPrimes I
	  ///,
     PARA{},
     "For monomial ideals, the method used is essentially what is shown in the example.",
     EXAMPLE lines ///
	  I = monomialIdeal ideal"wxy,xz,yz"
	  minimalPrimes I
	  P = intersect(monomialIdeal(w,x,y),monomialIdeal(x,z),monomialIdeal(y,z))
	  minI = apply(flatten entries gens P, monomialIdeal @@ support)
	  ///,
     "It is sometimes useful to have the result P instead (where each generator
     encodes a single minimal prime.  This can be obtained directly, as in the
     following code.",
     EXAMPLE lines ///
	  dual radical I
	  P == oo
          ///,
     SeeAlso => {
	 topComponents,
	 removeLowestDimension,
	 radical,
	 irreducibleCharacteristicSeries,
	 (dual, MonomialIdeal)}
     }

end--

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///
