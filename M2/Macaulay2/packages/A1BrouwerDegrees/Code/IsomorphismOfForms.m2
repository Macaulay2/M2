-- Input: A pair of Grothendieck-Witt classes or a pair of matrices representing symmetric bilinear forms over QQ
-- Output: Boolean that gives whether the Grothendieck-Witt classes/symmetric bilinear forms are isomorphic

isIsomorphicFormQQ = method()
isIsomorphicFormQQ (GrothendieckWittClass,GrothendieckWittClass) := Boolean => (alpha,beta) -> (
    if not getBaseField(alpha) === QQ then
	error "first input must have base field QQ";
    if not getBaseField(beta) === QQ then
	error "second input must have base field QQ";
    
    -- If the ranks differ, then the forms are not isomorphic
    if getRank(alpha) != getRank(beta) then return false;
    
    -- If the signatures (Hasse-Witt invariants at RR) differ, then the forms are not isomorphic
    if getSignature(alpha) != getSignature(beta) then return false;
    
    -- If the discriminants differ, then the forms are not isomorphic
    if getIntegralDiscriminant(alpha) != getIntegralDiscriminant(beta) then
	return false;
    
    -- Check the Hasse-Witt invariants
    PrimesToCheck := unique(getRelevantPrimes(alpha) | getRelevantPrimes(beta));
    for p in PrimesToCheck do (
        -- If any Hasse-Witt invariants differ, then the forms are not isomorphic
	if (getHasseWittInvariant(alpha, p) != getHasseWittInvariant(beta, p)) then
	    return false;
	);
    -- If we get here, then the rank, signature, discriminant, and all Hasse-Witt invariants agree,
    -- so thus the forms are isomorphic
    true
    )

isIsomorphicFormQQ (Matrix,Matrix) := Boolean => (M,N) -> (
    isIsomorphicFormQQ(makeGWClass M, makeGWClass N)
    )

-- Input: Two matrices representing symmetric bilinear forms over CC, RR, QQ, or a finite field of characteristic not 2
-- Output: Boolean that gives whether the bilinear forms are isomorphic

isIsomorphicForm = method()
isIsomorphicForm (Matrix,Matrix) := Boolean => (A,B) -> (
    k1 := ring A;
    k2 := ring B;
    -- Ensure both base fields are supported
    if not (instance(k1, ComplexField) or instance(k1, RealField) or k1 === QQ or (instance(k1, GaloisField) and k1.char != 2)) then
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
    if not (instance(k2, ComplexField) or instance(k2, RealField) or k2 === QQ or (instance(k1, GaloisField) and k1.char != 2)) then
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
    
    -- Ensure both matrices are symmetric
    if not isSquareAndSymmetric A then
	error "Underlying matrix is not symmetric";
    if not isSquareAndSymmetric B then
	error "Underlying matrix is not symmetric";
    
    -----------------------------------
    -- Complex numbers
    -----------------------------------
    
    -- Over CC, forms over spaces of the same dimension are isomorphic if and only if they have the same rank
    if (instance(k1, ComplexField) and instance(k2, ComplexField)) then (
        return (numRows(A) == numRows(B) and getRank(A) == getRank(B));
        )
    
    -----------------------------------
    -- Real numbers
    -----------------------------------
    
    -- Over RR, diagonal forms of the same dimension are isomorphic if and only if they have the same number of positive and negative entries
    else if (instance(k1, RealField) and instance(k2, RealField)) then (
        diagA := diagonalizeViaCongruence A;
        diagB := diagonalizeViaCongruence B;
        return (numRows(A) == numRows(B) and countPosDiagEntries(diagA) == countPosDiagEntries(diagB) and countNegDiagEntries(diagA) == countNegDiagEntries(diagB));
        )
    
    -----------------------------------
    -- Rational numbers
    -----------------------------------
    
    -- Over QQ, if the spaces have same dimension, then call isIsomorphicFormQQ on the nondegenerate parts of the forms
    else if (k1 === QQ and k2 === QQ) then (
        return (numRows(A) == numRows(B) and isIsomorphicFormQQ(getNondegeneratePartDiagonal A, getNondegeneratePartDiagonal B));
        )
    
    -----------------------------------
    -- Finite fields
    -----------------------------------
    
    -- Over a finite field, diagonal forms over spaces of the same dimension are equivalent if and only if they have the same number of nonzero entries and the product of these nonzero entries is in the same square class
    else if (instance(k1, GaloisField) and instance(k2, GaloisField) and k1.char !=2 and k2.char != 2 and k1.order == k2.order) then (
        return (numRows(A) == numRows(B) and getRank(A) == getRank(B) and isGFSquare(det(getNondegeneratePartDiagonal A)) == isGFSquare(sub(det(getNondegeneratePartDiagonal B), k1)));
        )
    -- If we get here, then the base fields are not the same
    else
	error "Base fields are not the same";
    )

-- Input: Two Grothendieck-Witt classes alpha and beta, defined over CC, RR, QQ, or a finite field of characteristic not 2
-- Output: Boolean that gives whether alpha and beta are the same Grothendieck-Witt class

isIsomorphicForm (GrothendieckWittClass,GrothendieckWittClass) := Boolean => (alpha,beta) -> (
    isIsomorphicForm(getMatrix alpha, getMatrix beta)
    )
