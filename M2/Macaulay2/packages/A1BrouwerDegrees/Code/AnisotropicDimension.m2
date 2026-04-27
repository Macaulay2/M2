---------------------------------------
-- Anisotropic dimension
---------------------------------------

-- Input: A Grothendieck-Witt class beta over QQ and a prime number p
-- Output: Boolean that indicates whether beta is totally hyperbolic over the p-adic rationals QQ_p
-- Notes: Koprowski/Czogala Algorithm 6

isHyperbolicQQp = method()
isHyperbolicQQp (GrothendieckWittClass, ZZ) := Boolean => (beta, p) -> (
    kk := getBaseField beta;
    rankBeta := getRank beta;
    
    if not kk === QQ then error "GrothendieckWittClass is not over QQ";
    if not isPrime p then error "second argument must be a prime number";
    
    -- Odd rank forms are not hyperbolic
    if odd rankBeta then return false; 
    
    -- Hyperbolic forms always have square discriminants
    -- Note that Koprowski and Czogala are using a different, signed, version of the discriminant
    d := (-1)^(rankBeta*(rankBeta-1)/2) * getIntegralDiscriminant(beta);
    
    -- If this discriminant is not a square in QQ_p then return false
    if not isPadicSquare(d,p) then return false;
    
    -- At this stage, the rank and discriminant of our beta agrees with that of a totally hyperbolic form,
    -- so by e.g. Lam V.3.25 it suffices to check whether their Hasse-Witt invariants agree
    m := sub(rankBeta/2,ZZ);
    HasseWittHyperbolicForm := (getHilbertSymbol(-1,-1,p))^(m*(m - 1)/2);
    HasseWittBeta := getHasseWittInvariant(beta, p);
    HasseWittHyperbolicForm == HasseWittBeta
    )

-- Input: A Grothendieck-Witt class beta over QQ and a prime number p
-- Output: An integer, the rank of the anisotropic part of beta over the p-adic rationals QQ_p
-- Note that as all symmetric bilinear forms over QQ_p of dimension >= 5 are isotropic, 
-- this method will always output 0, 1, 2, 3, or 4.
-- This is an implementation of Koprowski/Czogala Algorithm 8

getAnisotropicDimensionQQp = method()
getAnisotropicDimensionQQp (GrothendieckWittClass, ZZ) := ZZ => (beta, p) -> (
    kk := getBaseField beta;
    rankBeta := getRank beta;
    
    if not kk === QQ then error "GrothendieckWittClass is not over QQ";
    if not isPrime p then error "second argument must be a prime number";
    
    if even rankBeta then (
	-- If the form is hyperbolic it has no anisotropic part
	if isHyperbolicQQp(beta, p) then return 0;
       	
	-- Note Koprowski and Czogala use a different, signed, version of the discriminant
	d := (-1)^(rankBeta*(rankBeta-1)/2) * getIntegralDiscriminant(beta);
	if isPadicSquare(d,p) then return 4 else return 2;
	);
    
    if odd rankBeta then (
	c := (-1)^(rankBeta*(rankBeta+1)/2) * getIntegralDiscriminant(beta);
	gamma := addGW(beta, makeDiagonalForm(QQ, (c)));
	if isHyperbolicQQp(gamma, p) then return 1 else return 3;
	);
    )

-- Input: A Grothendieck-Witt class beta over QQ
-- Output: An integer, the rank of the anisotropic part of beta over QQ
-- Notes: Follows Algorithm 9 of Koprowski/Czogala

getAnisotropicDimensionQQ = method()
getAnisotropicDimensionQQ GrothendieckWittClass := ZZ => beta -> (
    kk := getBaseField beta;
    
    if not kk === QQ then error "GrothendieckWittClass is not over QQ";
    
    -- The anisotropic dimension of a form over QQ is the maximum of its anisotropic dimensions at any of its completions or over QQ_2
    ListOfLocalAnistropicDimensions := {};
    
    -- The anisotropic dimension at RR is the absolute value of the signature of the form
    ListOfLocalAnistropicDimensions = append(ListOfLocalAnistropicDimensions, abs(getSignature beta));
    
    -- We always have to include the anisotropic dimension at the prime 2
    ListOfLocalAnistropicDimensions = append(ListOfLocalAnistropicDimensions, getAnisotropicDimensionQQp(beta, 2));
       
    -- For the remaining local fields, we can just look at relevant primes
    for p in getRelevantPrimes(beta) do
	ListOfLocalAnistropicDimensions = append(ListOfLocalAnistropicDimensions, getAnisotropicDimensionQQp(beta, p));
    max ListOfLocalAnistropicDimensions
    )

-- Input: A symmetric matrix representing a symmetric bilinear form or a GrothendieckWittClass over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: An integer, the rank of the anisotropic part of the symmetric bilinear form or GrothendieckWittClass

getAnisotropicDimension = method()
getAnisotropicDimension Matrix := ZZ => A -> (
    k := ring A;
    -- Ensure base field is supported
    if not (instance(k, ComplexField) or instance(k, RealField) or k === QQ or (instance(k, GaloisField) and k.char != 2)) then
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
    -- Ensure matrix is symmetric
    if not isSquareAndSymmetric A then error "Matrix is not symmetric";
    -- Over CC, the anisotropic dimension is 0 or 1 depending on the parity of the rank
    if instance(k, ComplexField) then (
        return getRank(A)%2;
        )
    -- Over RR, the anisotropic dimension is the absolute value of the signature
    else if instance(k, RealField) then (
        diagonalA := diagonalizeViaCongruence A;
        return abs(countPosDiagEntries(diagonalA) - countNegDiagEntries(diagonalA));
        )
    -- Over QQ, call getAnisotropicDimensionQQ
    else if k === QQ then (
        return getAnisotropicDimensionQQ makeGWClass(getNondegeneratePartDiagonal A);
        )
    -- Over a finite field, if the number of nonzero diagonal entries is odd, then the anisotropic dimension is 1
    -- if the number of nonzero diagonal entries is even, then the anisotropic dimension is either 0 or 2,
    -- depending on whether the nondegenerate part of the form is totally hyperbolic
    else if (instance(k, GaloisField) and k.char != 2) then (
        diagA := diagonalizeViaCongruence A;
        if (getRank(diagA)%2 == 1) then (
            return 1;
            )
        else if isGFSquare(det getNondegeneratePartDiagonal diagA) == isGFSquare(sub((-1)^(getRank(diagA)/2), k)) then (
            return 0;
            )
        else
            return 2;
        );
    )

getAnisotropicDimension GrothendieckWittClass := ZZ => alpha -> (
    getAnisotropicDimension getMatrix alpha
    )

-- Input: A Grothendieck-Witt class over the complex numbers, the real numbers, the rational numbers, or a finite field of characteristic not 2
-- Output: An integer, the Witt index of the class, i.e. the rank of the maximal totally isotropic subspace

getWittIndex = method()
getWittIndex GrothendieckWittClass := ZZ => alpha -> (
    sub((getRank(alpha) - getAnisotropicDimension(alpha))/2,ZZ)
    )
