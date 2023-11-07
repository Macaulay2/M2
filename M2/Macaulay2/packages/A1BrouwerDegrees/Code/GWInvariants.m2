---------------------------------------
-- Invariants
---------------------------------------

-- Input: A diagonal matrix
-- Output: The number of nonzero entries on the diagonal of a diagonal matrix to which it is congruent
-- Note: numNonzeroDiagEntries is *not* included as a method in the A1BrowerDegrees package

numNonzeroDiagEntries = method()
numNonzeroDiagEntries (Matrix) := (Matrix) => (A) -> (
    if not isDiagonal(A) then(
        A = congruenceDiagonalize(A);
        );
    nonzeroDiagEntries := 0;
    for i from 0 to (numRows(A)-1) do (
        if A_(i,i) != 0 then (
            nonzeroDiagEntries = nonzeroDiagEntries + 1;
            );
        );
    return(nonzeroDiagEntries);
    )

-- Input: A diagonal matrix over QQ or RR
-- Output: The number of positive entries on the diagonal of a diagonal matrix to which it is congruent
-- Note: numPosDiagEntries is *not* included as a method in the A1BrowerDegrees package

numPosDiagEntries = method()
numPosDiagEntries (Matrix) := (Matrix) => (A) -> (
    if not isDiagonal(A) then(
        A = congruenceDiagonalize(A);
        );
    k := ring A;
    if not (k === RR or instance(k,RealField) or k === QQ) then(
        error "Only implemented over QQ and RR";
        );
    posDiagEntries := 0;
    for i from 0 to (numRows(A)-1) do (
        if A_(i,i) > 0 then (
            posDiagEntries = posDiagEntries + 1;
            );
        );
    return(posDiagEntries);
    )

-- Input: A diagonal matrix over QQ or RR
-- Output: The number of positive entries on the diagonal of a diagonal matrix to which it is congruent
-- Note: numPosDiagEntries is *not* included as a method in the A1BrowerDegrees package

numNegDiagEntries = method()
numNegDiagEntries (Matrix) := (Matrix) => (A) -> (
    if not isDiagonal(A) then(
        A = congruenceDiagonalize(A);
        );
    k := ring A;
    if not (k === RR or instance(k,RealField) or k === QQ) then(
        error "Only implemented over QQ and RR";
        );
    negDiagEntries := 0;
    for i from 0 to (numRows(A)-1) do (
        if A_(i,i) < 0 then (
            negDiagEntries = negDiagEntries + 1;
            );
        );
    return(negDiagEntries);
    )

-- Input: A Grothendieck-Witt class beta defined over QQ or RR
-- Output: The number of positive entries on the diagonal of a diagonal matrix representing the Grothendieck-Witt class
-- Note:  numPosEntries is *not* included as a method in the A1BrowerDegrees package

numPosEntries = method()
numPosEntries (GrothendieckWittClass) := ZZ => beta ->(
    return(numPosDiagEntries(beta.matrix));
    );

-- Input: A Grothendieck-Witt class beta defined over QQ or RR
-- Output: The number of negative entries on the diagonal of a diagonal matrix representing the Grothendieck-Witt class
-- Note:  numNegEntries is *not* included as a method in the A1BrowerDegrees package

numNegEntries = method()
numNegEntries (GrothendieckWittClass) := ZZ => beta ->(
    return(numNegDiagEntries(beta.matrix));
    );

-- Input: A Grothendieck-Witt class beta defined over QQ or RR
-- Output: The signature of beta

signature = method()
signature (GrothendieckWittClass) := ZZ => (beta) ->(
    sig := numPosEntries(beta) - numNegEntries(beta);
    return sig
    );

---------------------------
-- Comparing forms over QQ
---------------------------

-- Input: A Grothendieck-Witt class beta defined over QQ
-- Output: A squarefree integral representative of its discriminant

integralDiscriminant = method()
integralDiscriminant (GrothendieckWittClass) := (ZZ) => (beta) -> (
    B:= beta.matrix;
    rankForm:= numRows(B);
    kk:= ring B;
    
    if (not (kk === QQ)) then (error "GrothendieckWittClass is not over QQ");
    
    -- Take an integral diagonal representative for beta
    gamma := diagonalClass(beta);
    G := gamma.matrix;
    
    discrimForm:= 1;
    for i from 0 to (rankForm-1) do(
	discrimForm = discrimForm * (G_(i,i));
	);
    
    return sub(squarefreePart(discrimForm),ZZ);
    );

-- Input: A Grothendieck-Witt class beta defined over QQ
-- Output: The smallest list of primes that divide its discriminant

relevantPrimes = method()
relevantPrimes (GrothendieckWittClass) := List => (beta) -> (
    B := beta.matrix;
    rankForm := numRows(B);
    kk:= ring B;
    
    -- Take a diagonal integral representative of the form
    gamma := diagonalClass(beta);
    D := diagonalEntries(gamma);
    
    L := {};
    
    -- Append all the prime factors of each of the entries appearing on a diagonal
    for x in D do(
	L = unique(L | primeFactors(sub(x,ZZ)));
	);
    
    return L
    );

-- Two Q forms over Q_p are isomorphic if they have same rank, same discriminant, and same Hasse-Witt invariant   

HasseWittInvariant = method()

-- epsilonHilbert computes the epsilon function for a diagonal quadratic form over Q_p
-- Function requires the list of the diagonal elements of the quadratic form, to be integers

-- Input:  A list of the diagonal elements (f_i) for the quadratic form, assumed to be integers, and a prime p
-- Output: The HasseWittInvariant function for the quadratic form (f_i) for Q_p

HasseWittInvariant (List, ZZ) := ZZ => (L,p) -> (
       a := 1;
       len := #L;
       
       -- Replace every entry of L with its squarefree part so we can be sure we're evaluating at integers
       f := {};
       for x in L do(
	   f = append(f,squarefreePart(x));
	   );
       
       for i from 0 to len - 1 do (
	   if not liftable(f_i,ZZ) then (error "Error:  Hilbert symbol evaluated at a non-integer");
	   );
       for i from 0 to len - 2 do (
       	   for j from i + 1 to len - 1 do (
	       a = a * HilbertSymbol(f_i, f_j, p);
	       );
	   );
       
       return a;
    );

HasseWittInvariant(GrothendieckWittClass, ZZ) := ZZ => (beta,p) -> (
    kk := baseField beta;
    if not (kk === QQ) then error "method is only implemented over the rationals";
    if not isPrime(p) then error "second argument must be a prime number";
    return HasseWittInvariant(diagonalEntries(diagonalClass(beta)),p)
    
    )
