-- probabilistic irreducibility test based on 
-- http://arxiv.org/pdf/math/0404342v1

newPackage(
     "QuickAndDirtyIrreducibility",
     Version => "0.1", 
     Date => "14.01.2010",
     Authors => {{Name => "Hans-Christian Graf v. Bothmer", 
	       Email => "bothmer@math.uni-hannover.de", 
	       HomePage => "http://www.crcg.de/wiki/Hans-Christian_Graf_v._Bothmer"}},
     Headline => "A Quick and Dirty Irreducibility Test",
     DebuggingMode => true
     )

export {
     isProbablyIrreducible
     }
     
-- Code here


-- Error rate 0.5%
alpha = 2.58;

-- number of points that should be found for an
-- irreducible hypersurface
trialFactor = (n,p) -> (
     rho = (2*p-1.0)/p;
     (alpha*(1+sqrt(rho))/(rho-1))^2
     )

-- number of trials needed for this error rate
trials = (n,p) -> (
     ceiling(p*trialFactor(n,p))
     )

-- number of points in P^n
pointsPn = (n,p) -> (p^(n+1)-1)/(p-1)

-- deciding number of points found
middle = (n,p) -> (
       --if (n>10) or (p>17) then 55 else Hmiddle#(n,p)
       t := trialFactor(n,p);
       t+alpha*sqrt(t)
     )


-- F a homogeneous polynomial over a finite field
isProbablyIrreducible = (F) -> (
     R := ring F;
     K := coefficientRing R;
     n := #(gens R) - 1;
     p := char K;
     t  := trials(n,p);
     -- want at least 10 times more points than trials
     if trials(n,p)*10 < pointsPn(n,p) then error "not enough rational points over this prime";
     zeros := 0;
     for i from 1 to t do (
	  if 0 == sub(F,random(K^1,K^(n+1))) then zeros = zeros+1
	  );
     zeros < middle(n,p)
     )


beginDocumentation()
     
     doc ///
     Key
       QuickAndDirtyIrreducibility
     Headline
       A quick and dirty irreducibility test
     Description
       Text
         Testing irreducibility of a homogeneous polynomial
	 by evaluating it at random points. 
       Example
     Caveat
       Gives the correct answer only in 99,5% of all cases.
     SeeAlso
     ///

     doc ///
     Key
     	  isProbablyIrreducible
     Headline
     	  a quick and dirty irreducibility test for homogeneous polynomials
     Usage
     	  b = isProbablyIrreducible(F)
     Inputs
     	  F: RingElement
	     a homogeneous Polynomial
     Outputs
     	  b: Boolean
	     true if the algorithm thinks that F is irreducible
     Consequences
     Description
       Text
       	  This function perfoms a quick and dirty irreducibility
	  test of a homogeneous Polynomial by substituting random
	  points and counting the number of zeros. 
	  
	  For details see
	  "A quick and Dirty Irreducibility Test" at
	  http://arxiv.org/pdf/math/0404342v1
	  
       Example
       	  R = ZZ/7[x_0..x_4]
       	  isProbablyIrreducible(random(4,R))
	  isProbablyIrreducible(random(3,R)*random(3,R))
     Caveat
        Gives the correct answer only in 99,5% of all cases. This
	percentage is NOT increased by running the test severeal times
	for the same polynomial.
	
	For the algorithm to work it is necessary that the
	projective space associated to the ring of F has enough
	rational points. Currently the algorithm works for the following
	cases:
	$$\begin{pmatrix}
  	    & 2 & 3 & 5 & 7 & 11& 13 & 17 & 19 & 23\\
	  1 & . & . & . & . & . & . & . & . & .\\
	  2 & . & . & . & . & . & . & . & . & .\\
	  3 & . & . & . & . & . & . & . & . & *\\
	  4 & . & . & . & . & * & * & * & * & *\\
	  5 & . & . & * & * & * & * & * & * & *\\
	  6 & . & . & * & * & * & * & * & * & *\\
	  7 & . & * & * & * & * & * & * & * & *\\
	  8 & . & * & * & * & * & * & * & * & *\\
	  9 & . & * & * & * & * & * & * & * & *\\
	  10 & . & * & * & * & * & * & * & * & *\\
	  11 & * & * & * & * & * & * & * & * & *\\
	\end{pmatrix}$$
        For hypersurfaces in P^n with n\ge 11 the Algorithm
	works for all primes. For primes from 19 to 383 the algorithm
	works for n \ge 3 and for primes 389 and larger ist works
	for n \ge 2.
     SeeAlso
     ///

     TEST ///
        assert(isProbablyIrreducible(random(4,R))==true);
	assert(isProbablyIrreducible(random(3,R)*random(3,R))==false);
     ///



end
----

uninstallPackage"QuickAndDirtyIrreducibility"
restart
installPackage"QuickAndDirtyIrreducibility"
viewHelp
--load"quickAndDirtyIrreducibility.m2"
prime = 11
n = 3
K = ZZ/prime
R = K[x_0..x_n]

time tally apply(10,i->isProbablyIrreducible(random(4,R)*random(4,R)))
time tally apply(10,i->isProbablyIrreducible(random(4,R)))

primes = {2,3,5,7,11,13,17,19,23} 



--    | 2 3 5 7 11 13 17 19 23 |
-- -----------------------------
--  0 | 0 0 0 0  0  0  0  0  0 |
--  1 | 0 0 0 0  0  0  0  0  0 |
--  2 | 0 0 0 0  0  0  0  0  0 |
--  3 | 0 0 0 0  0  0  0  0  1 |
--  4 | 0 0 0 0  1  1  1  1  1 |
--  5 | 0 0 1 1  1  1  1  1  1 |
--  6 | 0 0 1 1  1  1  1  1  1 |
--  7 | 0 1 1 1  1  1  1  1  1 |
--  8 | 0 1 1 1  1  1  1  1  1 |
--  9 | 0 1 1 1  1  1  1  1  1 |
-- 10 | 0 1 1 1  1  1  1  1  1 |
-- 11 | 1 1 1 1  1  1  1  1  1 |

apply(400..420,p->trialFactor(2,p))
apply(388..400,i->if isPrime i then i)

-- plane curves start from p=389

-- the statistik from the paper ist not used at the moment

Mtrials = {
{null,null,null,null,null,null,null},
{null,null,null,null,null,null,null},
{null,null,null,28373,2355,1908,1669},
{null,null,1103,647,634,682,3803},
{null,1705,367,369,482,551,695},
{null,384,259,308,447,521,673},
{4457,224,225,289,437,513,667},
{619,173,212,283,434,511,666},
{295,151,206,280,433,511,666},
{197,140,204,279,433,511,665}
}

Htrials = new HashTable from 
          flatten apply(9,i->apply(7,j->(i+1,primes_j) => Mtrials#i#j))

Mmiddle = {
{null,null,null,null,null,null,null},
{null,null,null,null,null,null,null},
{null,null,null,5607,301,207,139},
{null,null,303,128,81,74,66},
{null,754,101,73,61,59,57},
{null,170,71,61,57,56,55},
{2821,99,61,57,55,55,55},
{391,76,58,56,55,55,55},
{186,67,56,55,55,55,55},
{125,62,56,55,55,55,55}
}

Hmiddle = new HashTable from
         flatten apply(9,i->apply(7,j->(i+1,primes_j) => Mmiddle#i#j))



 apply(primes,p->middle(n,p))   
