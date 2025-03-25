---------------------------------------
-- Invariants
---------------------------------------

-- Input: A Grothendieck-Witt class alpha
-- Output: The rank of a symmetric bilinear form representing alpha

getRank = method()
getRank GrothendieckWittClass := ZZ => alpha -> (
    numRows getMatrix alpha
    )

getRank Matrix := ZZ => M -> (
    if numRows(M) == 0 then return 0;
    rank M
    )

-- Input: A symmetric matrix or Grothendieck-Witt class defined over QQ or RR
-- Output: The number of positive entries on the diagonal of a diagonal matrix to which the underlying matrix is congruent
-- Note: countPosDiagEntries is *not* included as a method in the A1BrowerDegrees package

countPosDiagEntries = method()
countPosDiagEntries Matrix := Matrix => A -> (
    -- Ensure matrix is symmetric
    if not isSquareAndSymmetric A then error "Matrix is not symmetric";
    -- Ensure base field is QQ or RR
    k := ring A;
    if not (instance(k, RealField) or k === QQ) then
        error "Only implemented over QQ and RR";
    if not isDiagonal A then A = diagonalizeViaCongruence A;
    posDiagEntries := 0;
    for i from 0 to numRows(A) - 1 do (
        if A_(i,i) > 0 then posDiagEntries = posDiagEntries + 1;
        );
    posDiagEntries
    )

countPosDiagEntries GrothendieckWittClass := ZZ => beta -> (
    countPosDiagEntries getMatrix beta
    )

-- Input: A symmetric matrix or Grothendieck-Witt class defined over QQ or RR
-- Output: The number of positive entries on the diagonal of a diagonal matrix to which the underlying matrix is congruent
-- Note: countPosDiagEntries is *not* included as a method in the A1BrowerDegrees package

countNegDiagEntries = method()
countNegDiagEntries Matrix := Matrix => A -> (
    -- Ensure matrix is symmetric
    if not isSquareAndSymmetric A then error "Matrix is not symmetric";
    -- Ensure base field is QQ or RR
    k := ring A;
    if not (instance(k, RealField) or k === QQ) then
        error "Only implemented over QQ and RR";
    if not isDiagonal A then A = diagonalizeViaCongruence A;
    negDiagEntries := 0;
    for i from 0 to numRows(A) - 1 do (
        if A_(i,i) < 0 then negDiagEntries = negDiagEntries + 1;
        );
    negDiagEntries
    )

countNegDiagEntries GrothendieckWittClass := ZZ => beta -> (
    countNegDiagEntries getMatrix beta
    )

-- Input: A Grothendieck-Witt class beta defined over QQ or RR
-- Output: The signature of beta

getSignature = method()
getSignature GrothendieckWittClass := ZZ => beta -> (
    countPosDiagEntries(beta) - countNegDiagEntries(beta)
    )

---------------------------
-- Comparing forms over QQ
---------------------------

-- Input: A Grothendieck-Witt class defined over QQ
-- Output: A squarefree integral representative of its discriminant

getIntegralDiscriminant = method()
getIntegralDiscriminant GrothendieckWittClass := ZZ => beta -> (
    kk := getBaseField beta;
    if not kk === QQ then error "GrothendieckWittClass is not over QQ";

    -- Return a squarefree integral representative of the product of diagonal entries of a diagonal representative of the form 
    getSquarefreePart product getDiagonalEntries(beta)
    )

-- Input: A Grothendieck-Witt class defined over QQ
-- Output: The list of primes that divide entries of its diagonal representative

getRelevantPrimes = method()
getRelevantPrimes GrothendieckWittClass := List => beta -> (
    kk := getBaseField beta;
    if not kk === QQ then error "GrothendieckWittClass is not over QQ";
    
    -- Find the diagonal entries of a diagonal integral representative of the form
    D := getDiagonalEntries getDiagonalClass beta;
    
    -- Make a list of all prime factors of diagonal entries
    L := {};
    for x in D do L = unique(L | getPrimeFactors(sub(x, ZZ)));
    L
    )

-- Input:  A list of the diagonal elements of a symmetric bilinear form 
-- or a Grothendieck-Witt class over QQ, and a prime number p
-- Output: The Hasse-Witt invariant of the symmetric bilinear form or Grothendieck-Witt class over QQ_p

getHasseWittInvariant = method()
getHasseWittInvariant (List, ZZ) := ZZ => (L, p) -> (
    if not isPrime p then error "second argument must be a prime number";

    a := 1;
    len := #L;
       
    -- Replace every entry of L by its squarefree part so we can work with integers
    f := {};
    for x in L do f = append(f, getSquarefreePart x);
    for i from 0 to len - 2 do (
       	for j from i + 1 to len - 1 do
	    a = a * getHilbertSymbol(f_i,f_j,p);
	);
    a
    )

getHasseWittInvariant(GrothendieckWittClass, ZZ) := ZZ => (beta, p) -> (
    kk := getBaseField beta;
    if not kk === QQ then
	error "method is only implemented over the rational numbers";
    getHasseWittInvariant(getDiagonalEntries beta, p)
    )

