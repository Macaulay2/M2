---------------------
-- Simplifying forms
---------------------

-- Input: A GrothendieckWittClass over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A diagonalized form of the GrothendieckWittClass, with squares stripped out

diagonalClass = method()
diagonalClass (GrothendieckWittClass) := (GrothendieckWittClass) => (beta) -> (

    -- Check if the diagonalClass has already been computed, if so recall it from the cache
    if beta.cache.?diagonalClass then return beta.cache.diagonalClass;

    diagonalClassOfBetaMatrix := congruenceDiagonalizeSimplify(beta.matrix);

    -- The diagonal form gets cached in the GWclass type
    beta.cache.diagonalClass = gwClass(diagonalClassOfBetaMatrix);
    return gwClass(diagonalClassOfBetaMatrix);
    );

-- Input: A Grothendieck-Witt class beta
-- Output: The diagonal entries of a diagonal matrix representing beta as a list

diagonalEntries = method()
diagonalEntries (GrothendieckWittClass) := (List) => (beta) -> (
    
    M := congruenceDiagonalize(beta.matrix);
    L := {};
    n := numRows M;
    
    for i from 0 to (n-1) do(
	L = append(L, M_(i,i));
	);
    return L
    );
    
    
