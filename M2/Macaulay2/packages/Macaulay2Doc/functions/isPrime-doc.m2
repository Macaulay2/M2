--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

doc /// 
    Key
	(isProbablePrime,ZZ)
    	isProbablePrime	
    Headline
    	whether an integer is probably prime
    Usage 
    	isProbablePrime x
    Inputs
    	x:ZZ
    Outputs
    	:Boolean
    Description
    	Text 
	    Performs some trial division and then some probabilistic
	    primality tests. If $x$ is definitely composite, the function
	    returns false, otherwise it is declared probably prime, i.e. prime
	    for most practical purposes, and the function returns true. The
	    chance of declaring a composite number prime is very small.
	    Subsequent calls to the same function do not increase the
	    probability of the number being prime.  In fact, there are no known numbers
            which are composite, and for which this function returns true, although
            it is expected that there are an infinite number of such primes.
    	    
            This function calls {\tt fmpz_is_probabprime} in the {\tt flint} library.
	Example
            n = 1166513229502037
   	    isProbablePrime n
            isPrime n
            n1 = nextPrime(n+1)
            factor(n1^2*n)
        Text
            These functions handle numbers larger than this.  For example,
        Example
            m = 158174196546819165468118574681196546811856748118567481185669501856749
            isProbablePrime m
            isPrime m
            isPrime m^2
            factor m^2
        Example
            ndigits = 30
            m = nextPrime(10^30)
            m1 = nextPrime (m+10^10)
            m2 = nextPrime (m1 + 10^20)
            isPrime m
            isPrime m1
            isPrime (m*m1)
            isPrime(m*m*m1*m1*m2^6)
            elapsedTime facs = factor(m*m1)
            facs = facs//toList/toList
            assert(set facs === set {{m,1}, {m1,1}})
            m3 = nextPrime (m^3)
            elapsedTime isPrime m3
            elapsedTime isProbablePrime m3
    SeeAlso
        (isPrime,ZZ)
        (factor,ZZ)
        (nextPrime,Number)
///

document { 
     Key => {(isPseudoprime, ZZ), isPseudoprime},
     Headline => "deprecated, use isProbablePrime",
     SeeAlso => {"isProbablePrime"}
     -*
     PARA {
	  "The algorithm is provided by ", TO "pari", ".  The pseudoprimality test means that
	  it has no small factors, that it is a Rabin-Miller pseudoprime for the 
	  base $2$, and that it passes the strong Lucas test for the sequence $(P, -1)$, where $P$ is the 
	  smallest positive integer such that $P^2 - 4$ is not a square modulo $x$.
	  Such pseudoprimes may not be prime; to check primality, use ", TO "isPrime", ".
	  According to the documentation of ", TO "pari", TEX ", such pseudoprimes are known
	  to be prime up to $10^{13}$, and no nonprime pseudoprime is known."
	  }
     *-
     }

TEST ///
assert not isProbablePrime(101*1617839547365369353)
assert not isProbablePrime(18158848484363*1617839547365369353)
assert isProbablePrime 1617839547365369353
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
     SeeAlso => {factor, isProbablePrime}
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

