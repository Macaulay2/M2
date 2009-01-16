--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isPrime, (isPrime, ZZ), (isPrime,Ideal)},
     Headline => "whether a integer, polynomial, or ideal is prime",
     Usage => "isPrime f",
     Inputs => {
	  "f" => {ofClass ZZ, ", or an element in a ", TO2("PolynomialRing", "polynomial ring"), ", or an ", ofClass Ideal, " in a polynomial ring"}
	  },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "f", " is either a prime integer or an irreducible polynomial and ",
	       TO "false", " otherwise"}
	  },
     EXAMPLE lines ///
     ZZ/2[t];
     isPrime(t^2+t+1)
     isPrime(t^2+1)
     isPrime 101
     isPrime 158174196546819165468118574681196546811856748118567481185669501856749
     isPrime 158174196546819165468118574681196546811856748118567481185669501856749^2
     ///,
     PARA {
	  "Since ", TO "factor", " returns factors guaranteed only to be pseudoprimes, it
	  may be useful to check their primality, as follows."
	  },
     EXAMPLE lines ///
     f = factor 28752093487520394720397634653456
     peek'_2 f
     first \ toList f
     isPrime \ oo
     ///,
     "This function can be used also to determine whether an ideal in a polynomial ring is prime.",
     EXAMPLE lines ///
     R = QQ[a..d];
     I = monomialCurveIdeal(R,{1,5,8})
     isPrime I
     ///,
     PARA {
	  "Primality testing for integers is handled by ", TO "pari", "."
	  },
--      Caveat => {
-- 	  {"At the moment, for integers larger than ", TT "2^31-1", " it checks for
--      	       divisibility by small primes, and then applies a strong pseudoprimality
--      	       test (Rabin-Miller) to the base 2."},},
     SeeAlso => {factor}
     }

TEST "
assert (not isPrime 1333333)
assert (not isPrime 3133333)
assert (not isPrime 3313333)
assert ( isPrime 3331333)
assert ( isPrime 3333133)
assert ( isPrime 3333313)
assert ( isPrime 3333331)
"

TEST "
R=ZZ/2[t]
assert isPrime (t^2+t+1)
assert (not isPrime (t^2+1))
"

