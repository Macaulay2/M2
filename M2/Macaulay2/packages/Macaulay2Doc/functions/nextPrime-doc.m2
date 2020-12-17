doc ///
  Key
    nextPrime
    (nextPrime,Number)
  Headline
    compute the smallest prime greater than or equal to a given number
  Usage
    nextPrime n
  Inputs
    n: Number
  Outputs
    : ZZ
        the smallest prime $\ge n$
  Description
     Example
       nextPrime 10000
       nextPrime 3.5678
       nextPrime (3/7)
  SeeAlso
     isPrime
     getPrimeWithRootOfUnity
///

doc ///
  Key  
   getPrimeWithRootOfUnity
   (getPrimeWithRootOfUnity, ZZ,ZZ) 
   [getPrimeWithRootOfUnity,Range]
  Headline 
   find a prime p with a primitive n-th root of unity r in ZZ/p
  Usage
    (p,r)=getPrimeWithRootOfUnity(n,r1)
  Inputs
    n: ZZ
       the exponent of the root of unity
    r1: ZZ
       tentative root of unity
    Range => Sequence
      of integers (a,b)
  Outputs
     p: ZZ
          a prime in the specified range. The default range is $(10^4,3*10^4)$
     r: ZZ
          a primitive n-th root of unity mod p
  Description
     Text
       We compute the prime p as a larger prime factor of $r1^n-1$.
       If the largest p in the desired range does not work, we pass to $r1+1$ and repeat.
     Example
       n = 12
       (p,r) = getPrimeWithRootOfUnity(n,5)
       factor(r^n-1)
       r^12%p==1, r^6%p==1, r^4%p==1
       (p,r) = getPrimeWithRootOfUnity(12,11,Range=>(100,200))
       factor(r^n-1)
       r^12%p==1, r^6%p==1, r^4%p==1
  SeeAlso
    nextPrime
    Range
///

doc ///
  Key
    Range 
  Headline
     can be assigned a integral interval
  Usage
    getPrimeWithRootOfUnity(p,r,Range=>(a,b))
  Inputs
    a: ZZ
    b: ZZ
  Description
    Text
       Specifies an integral interval in which we search for a prime number with desired properties.
       If $b< a+2*log a$ an error message will be returned. The default value is $(10^4,3*10^4)$
  SeeAlso
    nextPrime
    getPrimeWithRootOfUnity
///

doc ///
  Key
    randomKRationalPoint
    (randomKRationalPoint,Ideal)
  Headline
    Pick a random K rational point on the scheme X defined by I
  Usage
    randomKRationalPoint I
  Inputs
    I: Ideal
       in a polynomial ring over a finite ground field K
  Outputs
     : Ideal
       of a K-rational point on V(I)
  Description
     Text
       If X has codimension 1, then we intersect X with a randomly choosen
       line, and hope that the decomposition of the the intersection contains a 
       K-rational point. If n=degree X then the probability P that this happens, is the 
       proportion  of permutations in $S_n$ with a fix point on $\{1,\ldots,n \}$,
       i.e. $$P=\sum_{j=1}^n (-1)^{j-1} binomial(n,j)(n-j)!/n! = 1-1/2+1/3! + \ldots $$
       which approachs $1-exp(-1) = 0.63\ldots$. Thus a probabilistic approach works.
	
       For higher codimension we first project X birationally onto a 
       hypersurface Y, and find a point on Y. Then we take the preimage of this point.
     Example
       p=nextPrime(random(2*10^4))
       kk=ZZ/p;R=kk[x_0..x_3];
       I=minors(4,random(R^5,R^{4:-1}));
       codim I, degree I
       time randomKRationalPoint(I)
       R=kk[x_0..x_5]; 
       I=minors(3,random(R^5,R^{3:-1})); 
       codim I, degree I
       time randomKRationalPoint(I)
     Text
       The claim that $63 \%$ of the intersections contain a K-rational point can be experimentally tested:
     Example
       p=10007,kk=ZZ/p,R=kk[x_0..x_2]
       n=5; sum(1..n,j->(-1)^(j-1)*binomial(n,j)*(n-j)!/n!)+0.0
       I=ideal random(n,R); 
       time (#select(apply(100,i->(degs=apply(decompose(I+ideal random(1,R)),c->degree c); 
		      #select(degs,d->d==1))),f->f>0))
///
	 

-- tests for nextprime
TEST ///
  assert( nextPrime(-10) == 2)
  assert( nextPrime 100 == 101)
  assert( nextPrime 1000 == 1009)
///

TEST ///
     setRandomSeed("getPrimeOfUnity")
     (p,r)=getPrimeWithRootOfUnity(2,3,Range=>(10^3,10^4))
     assert( (p,r)==(3511,-1)) -- works if the random number generator is not unchanged 
     (p,r)=getPrimeWithRootOfUnity(15,20)
     assert((p,r)==(18181,21))
     (p,r)=getPrimeWithRootOfUnity(12,2,Range=>(100,200))     	  
     assert(r^12%p==1 and r^6%p !=1 and r^4%p != 1)
/// 

TEST ///
     setRandomSeed 0
     p=10007,kk=ZZ/p 
     R=kk[x_0..x_2]
     I=ideal(random(4,R)); 
     time randomKRationalPoint(I)
     R=kk[x_0..x_4]
     I=minors(3,random(R^5,R^{3:-1}));
     codim I
     time pt = randomKRationalPoint(I)
     -- The following is the answer given the above random seed.
     ans = ideal(x_3+74*x_4,x_2+2336*x_4,x_1-4536*x_4,x_0-4976*x_4)
     assert(pt == ans)
///
