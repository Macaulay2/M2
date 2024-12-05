---------------------
-- Simplifying forms
---------------------

-- Input: A Grothendieck-Witt class beta over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A diagonalized form of beta, with squarefree entries on the diagonal

getDiagonalClass = method()
getDiagonalClass GrothendieckWittClass := GrothendieckWittClass => beta -> (

    -- Check if the diagonal class has already been computed; if so, recall it from the cache
    if beta.cache.?getDiagonalClass then return beta.cache.getDiagonalClass;

    getDiagonalClassOfBetaMatrix := diagonalizeAndSimplifyViaCongruence getMatrix beta;

    -- The computed diagonal class gets stored in the cache
    beta.cache.getDiagonalClass = makeGWClass getDiagonalClassOfBetaMatrix;
    makeGWClass getDiagonalClassOfBetaMatrix
    )

-- Input: A Grothendieck-Witt class beta over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A list of the diagonal entries of a diagonal matrix representing beta

getDiagonalEntries = method()
getDiagonalEntries GrothendieckWittClass := List => beta -> (
    
    M := diagonalizeViaCongruence getMatrix beta;
    n := numRows M;
    L := {};
    
    for i from 0 to n - 1 do
	L = append(L, M_(i,i));
    L
    )
    
    
