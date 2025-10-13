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
    Range => "(a,b)"
      specifying an integral interval in which we search for a prime number with desired properties.
      If $b < a + 2*\log a$ an error message will be returned.

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
///

doc ///
  Key
    randomKRationalPoint
    (randomKRationalPoint,Ideal)
  Headline
    pick a random K rational point on the scheme X defined by I
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
       If X has codimension 1, then we intersect X with a randomly chosen
       line, and hope that the decomposition of the intersection contains a 
       K-rational point. If n=degree X then the probability P that this happens, is the 
       proportion  of permutations in $S_n$ with a fix point on $\{1,\ldots,n \}$,
       i.e. $$P=\sum_{j=1}^n (-1)^{j-1} binomial(n,j)(n-j)!/n! = 1-1/2+1/3! + \ldots $$
       which approaches $1-exp(-1) = 0.63\ldots$. Thus a probabilistic approach works.
	
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
