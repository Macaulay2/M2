--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

doc /// 
    Key
	(isPseudoprime,ZZ)
    	isPseudoprime
    Headline
    	whether an integer is probably prime
    Usage 
    	isPseudoprime x
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
    	    
            This function calls a function in the @ TO "FLINT" @ library.
	Example
            n = 1166513229502037
   	    isPseudoprime n
            isPrime n
            n1 = nextPrime(n+1)
            factor(n1^2*n)
        Text
            These functions handle numbers larger than this.  For example,
        Example
            m = 158174196546819165468118574681196546811856748118567481185669501856749
            isPseudoprime m
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
            elapsedTime isPseudoprime m3
    SeeAlso
        (isPrime,ZZ)
        (factor,ZZ)
        (nextPrime,Number)
///

TEST ///
assert not isPseudoprime(101*1617839547365369353)
assert not isPseudoprime(18158848484363*1617839547365369353)
assert isPseudoprime 1617839547365369353
///

document { 
     Key => {isPrime, (isPrime, ZZ)},
     Headline => "whether a integer or polynomial is prime",
     Usage => "isPrime f",
     Inputs => {
	  "f" => {ofClass ZZ, ", or an element in a ", TO2("PolynomialRing", "polynomial ring")}
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
     PARA {
	  "Primality testing for integers is handled by ", TO "FLINT", "."
	  },
     SeeAlso => {factor, isPseudoprime, "MinimalPrimes :: isPrime(Ideal)"}
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

