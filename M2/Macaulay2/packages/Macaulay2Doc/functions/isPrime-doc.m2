--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {(isPseudoprime, ZZ), isPseudoprime},
     Headline => "whether an integer is a pseudoprime",
     Usage => "isPseudoprime x",
     Inputs => { "x" },
     Outputs => {
	  Boolean => {TO "true", " if ", TT "x", " is a strong pseudoprime in the sense of Baillie-Pomerance-Selfridge-Wagstaff"}
	  },
     PARA {
	  "The algorithm is provided by ", TO "pari", ".  The pseudoprimality test means that
	  it has no small factors, that it is a Rabin-Miller pseudoprime for the 
	  base $2$, and that it passes the strong Lucas test for the sequence $(P, -1)$, where $P$ is the 
	  smallest positive integer such that $P^2 - 4$ is not a square modulo $x$.
	  Such pseudoprimes may not be prime; to check primality, use ", TO "isPrime", ".
	  According to the documentation of ", TO "pari", TEX ", such pseudoprimes are known
	  to be prime up to $10^{13}$, and no nonprime pseudoprime is known."
	  }
     }

TEST ///
assert not isPseudoprime(101*1617839547365369353)
assert not isPseudoprime(18158848484363*1617839547365369353)
assert isPseudoprime 1617839547365369353
///

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
     SeeAlso => {factor, isPseudoprime}
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

