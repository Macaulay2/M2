------------------------
-- Arithmetic operations
------------------------

-- Input: An integer or rational number n
-- Output: The smallest magnitude integer in the same integer square class as n

getSquarefreePart = method()
getSquarefreePart ZZ := ZZ => n -> (
    if n == 0 then return 0;
    tableOfPrimeFactors := hashTable factor abs n;
    return sub(n/abs(n),ZZ)*product(apply(keys(tableOfPrimeFactors),p -> p^(tableOfPrimeFactors#p%2)));
    )

getSquarefreePart QQ := ZZ => n -> (
    getSquarefreePart(numerator(n)*denominator(n))
    )

-- Input: An integer or rational number n
-- Output: A list of prime factors of n

getPrimeFactors = method()
getPrimeFactors ZZ := List => n -> (
    sort keys hashTable(factor abs n)
    )

getPrimeFactors QQ := List => n -> (
    if not liftable(n, ZZ) then error "gatPrimeFactors expected an integer";
    getPrimeFactors sub(n, ZZ)  
    )

-- Input: An integer or rational number n and a prime number p
-- Output: The p-adic valuation of n

getPadicValuation = method()
getPadicValuation (ZZ, ZZ) := ZZ => (n, p) -> (
    if n == 0 then error "Trying to find prime factorization of 0";
    H := hashTable factor abs n;
    if H#?p then return H#p else return 0;
    )

getPadicValuation (QQ, ZZ) := ZZ => (q, p) -> (
    getPadicValuation(numerator q, p) - getPadicValuation(denominator q, p)
    )

-- Input: An element of a finite field
-- Output: Boolean that gives whether the element is a square

isGFSquare = method()
isGFSquare RingElement := Boolean => a -> (
    if not instance(ring a, GaloisField) then 
        error "isGFSquare only works for Galois fields";
    q := (ring a).order;
    -- Detects if a is a square in F_q
    a^((q-1)//2) == 1 
    )

-- Input: An integer a and a prime number p
-- Output: 1 if a is a unit square, -1 if a = p^(even power) x (non-square unit), 0 otherwise
-- Note: The terminology "Square Symbol" comes from John Voight's Quaternion Algebra book

getSquareSymbol = method()
getSquareSymbol (ZZ,ZZ) := ZZ => (a,p) -> (
    R := GF(p);
    e1 := getPadicValuation(a,p);
    if even e1 then (
    	a1 := sub(a/(p^e1), ZZ);
	a2 := sub(a1, R);
	if isGFSquare a2 then return 1 else return -1;
        );
    return 0;
    )

------------------------------
-- P-adic methods
------------------------------

-- Input: Two integers a and b, and a prime number p
-- Output: Boolean that gives whether a and b differ by a square in QQ_p

isEqualUpToPadicSquare = method()
isEqualUpToPadicSquare (ZZ,ZZ,ZZ) := Boolean => (a,b,p) -> (
    
-- One has to separately handle the cases when p is odd and when p = 2

    if odd p then (
        -- if p is odd, we need to check that the p-adic valuations of a and b have the same parity and the units
        -- differ by a square in GF(p)
        a1 := getSquarefreePart a;
        b1 := getSquarefreePart b;
        if (getPadicValuation(a1,p) != getPadicValuation(b1,p)) then (
            return false;
            )
        else (
    	    -- c1 will be an integer coprime to p
	    c1 := getSquarefreePart(a1*b1);
	    return isGFSquare sub(c1, GF(p)); 
	    );
        )
    else (
        -- Case when p = 2. Here we have to check that the p-adic valuations of a and b have the same parity and 
        -- that the units agree mod 8.
        a1 = getSquarefreePart a;
        b1 = getSquarefreePart b;
        if getPadicValuation(a1,p) != getPadicValuation(b1,p) then (
	    return false;
            )
        else (
    	    -- c1 will be an integer coprime to p
	    c1 = getSquarefreePart(a1*b1);
	    c1 = c1 % 8;
	    -- If c1 = 1, then the two odd units are congruent mod 8 and are squares in QQ_2
	    return c1 == 1; 
	    );
        );
    )

-- Input: An integer a and a prime number p
-- Output: Boolean that gives whether a is a square in QQ_p

isPadicSquare = method()
isPadicSquare (ZZ,ZZ) := Boolean => (a,p) -> (
    isEqualUpToPadicSquare(a,1,p)
    )

------------------------------
-- Commutative algebra methods
------------------------------

-- Input: A list L of functions f1,...,fn over the same ring R and p a prime ideal of an isolated zero
-- Output: A list of basis elements of the local k-algebra Q_p(f) = R[x]_p/(f)

getLocalAlgebraBasis = method()
getLocalAlgebraBasis (List, Ideal) := List => (L, p) -> (
    
    -- Verify that the ideal p is prime
    if not isPrime p then error "ideal is not prime";
    
    -- Ambient ring
    R := ring L#0;
    I := ideal L;
    
    -- Check whether the ideal I is zero-dimensional
    if dim I > 0 then error "morphism does not have isolated zeros";
    if not isSubset(I, p) then error "prime is not a zero of function";
    J := I:saturate(I, p);
    A := R/J;
    B := basis A;
    flatten entries B
    )

-- Input: A zero-dimensional ideal (f_1,...,f_n) < k[x_1,...,x_n]
-- Output: The rank of the global algebra as a k-vector space

getGlobalAlgebraRank = method()
getGlobalAlgebraRank List := ZZ => Endo -> (
    
    -- Get the underlying field    
    kk := coefficientRing ring(Endo#0);    
    if not isField kk then kk = toField kk;
    
    -- Let S = kk[x_1,...,x_n] be the ambient polynomial ring
    S := ring(Endo#0);
    
    -- First check verify that the morphism has isolated zeroes
    if dim ideal(Endo) > 0  then error "ideal is not zero-dimensional";
    
    -- Get the rank of S/ideal(Endo) as a kk-vector space
    numColumns basis(S/ideal(Endo))
    )

-- Input: A pair of integers a and b
-- Output: A pair of integers x and y such that a*x + b*y = gcd(a,b)
-- This is adapted from the igcdx method from the Parametrization package

computeExtendedEuclidean = method()
computeExtendedEuclidean(ZZ,ZZ) := (a,b) -> (
    if a % b == 0 then (
        return {0,1};
        )
    else (
        L := computeExtendedEuclidean(b, a % b);
        return {L#1, L#0 - L#1*(a // b)};
        );
    )

-- Input: A list of four integers a,b,p,q
-- Output: An integer solution x to the congruences x = a (mod p) and x = b (mod q)
-- This is adapted from the chineseRemainder0 method from the Parametrization package

solveCongruencePair = method()
solveCongruencePair(ZZ,ZZ,ZZ,ZZ) := (a,b,n,m) -> (
    k := a - b;
    L := computeExtendedEuclidean(n,m);
    u := L#0;
    v := L#1;
    a - k*u*n % n*m
    )

-- This is adapted from the chineseRemainder method from the Parametrization package

solveCongruenceList = method()
solveCongruenceList(List,List) := (L1,L2) -> (
    q := 1;
    a := L1#0;
    n := L2#0;
    L := {};
    while q<#L1 do (
        a = solveCongruencePair(a, L1#q, n, L2#q);
        n = n*L2#q;
        q = q + 1
        );
    a
    )
