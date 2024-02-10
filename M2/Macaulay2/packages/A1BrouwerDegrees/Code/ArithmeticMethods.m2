------------------------
-- Arithmetic operations
------------------------

-- Input: A rational number or an integer
-- Output: Smallest magnitude integer in its square class

squarefreePart = method()
squarefreePart (QQ) := (ZZ) => (n) -> (
    if n == 0 then (
        return 0
        );
    if n > 0 then (
        tableOfPrimeFactorsQQ := hashTable(factor(numerator(n)*denominator(n)));
        return product(apply(keys(tableOfPrimeFactorsQQ),p -> p^(tableOfPrimeFactorsQQ#p%2)))
        );
    if n < 0 then (
        tableOfPrimeFactorsQQNeg := hashTable(factor(numerator(-n)*denominator(-n)));
        return -product(apply(keys(tableOfPrimeFactorsQQNeg),p -> p^(tableOfPrimeFactorsQQNeg#p%2)))
        );
    )

squarefreePart (ZZ) := (ZZ) => (n) -> (
    if n == 0 then (
        return 0
        );
    if n > 0 then (
        tableOfPrimeFactors := hashTable(factor(n));
        return product(apply(keys(tableOfPrimeFactors),p -> p^(tableOfPrimeFactors#p%2)))
        );
    if n < 0 then (
        tableOfPrimeFactorsNeg := hashTable(factor(-n));
        return -product(apply(keys(tableOfPrimeFactorsNeg),p -> p^(tableOfPrimeFactorsNeg#p%2)))
        );
    )

-- Input: An integer or rational number n
-- Output: A list of prime factors of n

primeFactors = method()
primeFactors (ZZ) := List => (n) -> (
    if abs(n) == 1 then(
	return {};
	);
    
    return sort keys(hashTable(factor(abs(n))))
    );

primeFactors (QQ) := List => (n) -> (
    if (not liftable(n,ZZ) == true) then(
	error "tried to take prime factors of a rational";
	);
    
    return primeFactors(sub(n,ZZ));   
    )

-- Input: An integer n and a prime number p
-- Output: The p-adic valuation of n

padicValuation = method()
padicValuation (ZZ, ZZ) := (ZZ) => (n, p) -> (
    if (n < 0) then (n = -n);
    if (n == 0) then error "Error: Trying to find prime factorization of 0";
    H := hashTable (factor n);
    a := 0;
    if H#?p then (
    	a = H#p;)
    else (
	a = 0;
	);
    return a;
    );

-- Input: A rational number q and a prime number p
-- Output: the p-adic valuation of q
padicValuation (QQ, ZZ) := (ZZ) => (q, p) -> (
    num := numerator(q);
    denom := denominator(q);
    return (padicValuation(num,p) - padicValuation(denom,p));
);

-- Input: An element a of a finite field
-- Output: True if a is a square, false otherwise

legendreBoolean = method()
legendreBoolean (RingElement) := (Boolean) => a -> (
    if not instance(ring(a),GaloisField) then error "Error: this works only for Galois fields";
    q := (ring(a)).order;
    -- Detects if a is a square in F_q
    a^((q-1)//2) == 1 
    )

-- Input: An integer a and a prime p
-- Output: 1, if a is a unit square,  -1, if a = p^(even power)x  (non-square unit), 0 otherwise
-- Note:  The terminology "Square Symbol" comes from John Voight's Quaternion Algebra book

squareSymbol = method()
squareSymbol(ZZ, ZZ) := (ZZ) => (a, p) -> (
    x := getSymbol "x";
    R := GF(p, Variable => x);
    e1 := padicValuation(a,p);
    if (even e1) then (
    	a1 := sub(a/(p^e1), ZZ);
	a2 := sub(a1, R);
	if legendreBoolean(a2) then (
	    ans := 1;
	    ) 
	else (
	    ans = -1;
	    );
	)
    else (
	ans = 0;
	);
    return ans;
    );

------------------------------
-- P-adic methods
------------------------------

-- Boolean checking if two integers a and b differ by a square in Qp

equalUptoPadicSquare = method()
equalUptoPadicSquare (ZZ, ZZ, ZZ) := (Boolean) => (a, b, p) -> (
    
-- One has to handle the cases when p is odd, and p = 2 differently

if (odd p) then (
    -- p is odd and we need to check that the powers of p have the same parity, and the units
    -- differ by a square in GF(p)
    a1 := squarefreePart(a);
    b1 := squarefreePart(b);
    if (padicValuation(a1, p ) != padicValuation(b1, p)) then (
	return false;
        )
    else (
    	-- c1 will be an integer prime to p
	c1 := squarefreePart(a1*b1);
	x := getSymbol "x";
	return (legendreBoolean( sub(c1, GF(p, Variable => x)))); 
	);
    )
else (
    -- Case when p=2.  Then we have to check that the powers of p have the same parity, and 
    -- that the units agree mod 8.
    a1 = squarefreePart(a);
    b1 = squarefreePart(b);
    if (padicValuation(a1, p ) != padicValuation(b1, p)) then (
	return false;
        )
    else (
    	-- c1 will be an integer prime to p
	c1 = squarefreePart(a1*b1);
	c1 = c1 % 8;
	-- if c1 =1, then the two odd units are congruent mod 8, and are squares in Q2
	return (c1 == 1); 
	);
    );
  );

-- Boolean to check if an integer a is a p-adic square

isPadicSquare = method()
isPadicSquare (ZZ, ZZ) := (Boolean) => (a, p) -> (
    return equalUptoPadicSquare(a,1,p)
    );

------------------------------
-- Commutative algebra methods
------------------------------

-- Input: A list L of functions f1,...,fn over the same ring R and p is a prime ideal of an isolated zero
-- Output: A list of basis elements of the local k-algebra Q_p(f) = R[x]_p/(f)

localAlgebraBasis = method()
localAlgebraBasis (List, Ideal) := (List) => (L,p) -> (
    
    -- Determine whether or not an ideal is prime
    if isPrime(p) == false then (
        error "Error: ideal is not prime"
        );
    
    -- Ambient ring
    R := ring L#0;
    I := ideal(L);
    
    -- Check whether or not an ideal is zero-dimensional
    if dim I > 0  then (
        error "Error: morphism does not have isolated zeroes"
        );
    if (not isSubset(I,p)) then (
        error "Error: prime is not a zero of function"
        );
    J := I:saturate(I,p);
    A := R/J;
    B := basis(A);
    return flatten(entries(B))
    )

-- Input: A zero-dimensional ideal (f_1,..f_n) < k[x_1..x_n].
-- Output: The rank of the global algebra  k[x_1..x_n].

rankGlobalAlgebra = method()
rankGlobalAlgebra (List) := (ZZ) => (Endo) -> (
    
    -- Get the underlying field    
    kk := coefficientRing(ring(Endo#0));    
    if isField(kk) == false then(
    	kk = toField(kk);
    	);
    
    -- Let S = k[x_1..x_n] be the ambient polynomial ring
    S := ring(Endo#0);
    
    -- First check if the morphism does not have isolated zeroes
    if dim ideal(Endo) > 0  then (
	error "Error: ideal is not zero-dimensional";
	);
    
    -- Get the rank of S/ideal(Endo) as a kk-vector space
    return numColumns(basis(S/ideal(Endo)));   
    )

