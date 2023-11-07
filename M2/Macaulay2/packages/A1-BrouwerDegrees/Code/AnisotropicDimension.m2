---------------------------------------
-- Anisotropic dimension
---------------------------------------

-- Input: A Grothendieck-Witt class beta (over QQ) and an integer
-- Output: True if matrix representation of beta is hyperbolic
-- Notes: Koprowski/Czogala Algorithm 6

isHyperbolicQp = method()
isHyperbolicQp (GrothendieckWittClass, ZZ) := Boolean => (beta, p) ->(
    B := beta.matrix;
    rankForm := numRows(B);
    kk := ring B;
    
    if (not (kk === QQ)) then (error "GrothendieckWittClass is not over QQ");
    if not isPrime(p) then error "second argument must be a prime number";
    if (isDegenerate(B)) then (error "form is degenerate");
    
    -- Odd rank forms are not hyperbolic
    if odd rankForm then return false; 
    
    -- Hyperbolic forms always have square discriminants
    -- Note that Koprowski and Czogala are using a different, signed definition of the discriminant
    d := (-1)^(rankForm*(rankForm-1)/2) *integralDiscriminant(beta);
    
    -- If this discriminant is not a square in Q_p then return false
    if (isPadicSquare(d,p)==false) then return false;
    
    -- At this stage, the rank and discriminant of our beta agrees with that of a hyperbolic form,
    -- so by e.g. Lam V.3.25 it suffices to check if their Hasse-Witt invariants agree
    if even rankForm then(
	
	m := sub(rankForm/2,ZZ);
	
	-- The Hasse-Witt invariant of mH:
	HasseWittHyperbolicForm := (HilbertSymbol(-1,-1,p))^(m*(m - 1)/2);
	HasseWittBeta := HasseWittInvariant(beta,p);
	return (HasseWittHyperbolicForm == HasseWittBeta)
	);
    );

-- Input: A Grothendieck-Witt class beta and an integer
-- Output: An integer, the rank of the anisotropic part of beta over Q_p

-- Note: Given a symmetric bilinear form over QQ, and a prime p, outputs the anisotropic
-- dimension of the form over the p-adic numbers Q_p. Note that as all quadratic forms
-- over Q_p of dimension >= 5 are isotropic, this method will always output 0, 1, 2, 3, or 4.
-- This is an implementation of Koprowski/Czogala Algorithm 8

anisotropicDimensionQp = method()
anisotropicDimensionQp (GrothendieckWittClass, ZZ) := ZZ => (beta, p) ->(
    B := beta.matrix;
    rankForm := numRows(B);
    kk := ring B;
    
    if (not (kk === QQ)) then (error "GrothendieckWittClass is not over QQ");
    if not isPrime(p) then error "second argument must be a prime number";
    if (isDegenerate(B)) then (error "form is degenerate");
    
    if even rankForm then(
	-- If the form is hyperbolic it has no anisotropic part
	if isHyperbolicQp(beta,p) then return 0;
       	
	-- Note Koprowski and Czogala use a signed version of the discriminant
	d := (-1)^(rankForm*(rankForm-1)/2) * integralDiscriminant(beta);
	if isPadicSquare(d,p) then return 4;
	
	return 2;
       
	);
    
    if odd rankForm then(
	
	c := (-1)^(rankForm*(rankForm+1)/2) * integralDiscriminant(beta);
	
	gamma := gwAdd(beta, diagonalForm(QQ,(c)));
	
	if isHyperbolicQp(gamma,p) then return 1;
	
	return 3
	
	);
    );

-- Input: A Grothendieck-Witt class beta in GW(QQ)
-- Output: An integer, the rank of the anisotropic part of beta
-- Notes: Computes the anisotropic dimension of a form over QQ
-- following Algorithm 9 of Koprowski/Czogala

anisotropicDimensionQQ = method()
anisotropicDimensionQQ (GrothendieckWittClass) := ZZ => (beta) -> (
    B := beta.matrix;
    rankForm := numRows(B);
    kk := ring B;
    
    if (not (kk === QQ)) then (error "GrothendieckWittClass is not over QQ");
    if (isDegenerate(B)) then (error "form is degenerate");
    
    -- The anisotropic dimension of a form over Q is the maximum of its anisotropic dimensions at any of its completions
    
    ListOfLocalAnistropicDimensions := {};
    
    -- The anisotropic dimension at RR is the absolute value of the signature of the form
    ListOfLocalAnistropicDimensions = append(ListOfLocalAnistropicDimensions, abs(signature(beta)));
    
    -- For math reasons(?) we always have to add the anisotropic dimension at the prime 2
    ListOfLocalAnistropicDimensions = append(ListOfLocalAnistropicDimensions, anisotropicDimensionQp(beta,2));
       
    -- For the remaining local fields, we can just look at relevant primes
    for p in relevantPrimes(beta) do(
	ListOfLocalAnistropicDimensions = append(ListOfLocalAnistropicDimensions, anisotropicDimensionQp(beta,p))
	
	);
    
    return max ListOfLocalAnistropicDimensions;
    );

-- Input: A symmetric matrix representing a quadratic form or a GrothendieckWittClass; over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: An integer, the rank of the anisotropic part of beta

anisotropicDimension = method()
anisotropicDimension (Matrix) := (ZZ) => (A) -> (
    k := ring A;
    -- Ensure base field is supported
    if not (k === CC or instance(k,ComplexField) or k === RR or instance(k,RealField) or k === QQ or (instance(k, GaloisField) and k.char != 2)) then (
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
        );
    -- Ensure matrix is symmetric
    if (transpose(A) != A) then (
        error "Matrix is not symmetric";
	);
    diagA := congruenceDiagonalize(A);
    -- Over CC, the anisotropic dimension is 0 or 1 depending on the parity of number of nonzero diagonal entries
    if (k === CC or instance(k,ComplexField)) then (
        return (numNonzeroDiagEntries(diagA)%2);
        )
    --Over RR, the anisotropic dimension is the difference between the number of positive diagonal entries and the number of negative diagonal entries
    else if (k === RR or instance(k,RealField)) then (
        return (abs(numPosDiagEntries(diagA) - numNegDiagEntries(diagA)));
        )
    -- Over QQ, call anisotropicDimensionQQ
    else if (k === QQ) then (
        return anisotropicDimensionQQ(gwClass(A));
        )
    -- Over a finite field, if the number of nonzero diagonal entries is odd, then the anisotropic dimension is 1; if the number of nonzero diagonal entries is even, then the anisotropic dimension is either 0 or 2 depending on whether the nondegenerate part of the form is totally hyperbolic
    else if (instance(k, GaloisField) and k.char != 2) then (
        if (numNonzeroDiagEntries(A)%2 == 1) then (
            return 1;
            )
        else if (legendreBoolean(det(nondegeneratePartDiagonal(diagA))) == legendreBoolean(sub((-1)^(numRows(nondegeneratePartDiagonal(diagA))/2),k))) then (
            return 0;
            )
        else (
            return 2;
            );
        )
    -- We should never get here
    else error "Problem with base field"
    );

anisotropicDimension (GrothendieckWittClass) := (ZZ) => (alpha) -> (
    return(anisotropicDimension(alpha.matrix));
    );


-- Input: A Grothendieck-Witt class alpha in GW(k), where k is the complex numbers, the real, the rationals or a finite field of characteristic not 2
-- Output: An integer, the rank of the totally isotropic part of alpha

WittIndex = method()
WittIndex (GrothendieckWittClass) := (ZZ) => (alpha) -> (
    n := numRows(alpha.matrix);
    return sub((n - anisotropicDimension(alpha))/2,ZZ);
    );
