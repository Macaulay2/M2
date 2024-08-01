-- Input: A form q over QQ of anisotropic dimension d >= 4
-- Output: A form < a > so that q + < -a > has anisotropic dimension d - 1
-- Note: This is Koprowski/Rothkegel's Algorithm 5 in the case of QQ

reduceAnisotropicPartQQDimension4 = method()
reduceAnisotropicPartQQDimension4 GrothendieckWittClass := GrothendieckWittClass => beta -> (
    if getAnisotropicDimensionQQ(beta) < 4 then
	error "anisotropic dimension of form is not >= 4";
    
    -- If the signature is non-negative then return < 1 >
    if getSignature(beta) >= 0 then return makeDiagonalForm(QQ, 1);
    
    -- Otherwise return < -1 >
    if getSignature(beta) < 0 then return makeDiagonalForm(QQ, -1);	
    )

-- Input: A form q over QQ of anisotropic dimension 3
-- Output: A form < a > so that q + < -a > has anisotropic dimension 2
-- Note: This is Koprowski/Rothkegel's Algorithm 7 in the case of QQ

reduceAnisotropicPartQQDimension3 = method()
reduceAnisotropicPartQQDimension3 GrothendieckWittClass := GrothendieckWittClass => beta -> (
    if getAnisotropicDimensionQQ(beta) != 3 then
	error "anisotropic dimension of form is not 3";

    d := getIntegralDiscriminant beta;
    
    -- Build lists of relevant primes where the p-adic valuation of the discriminant is even or is odd
    L1 := {1};
    L2 := {};
    S1 := {1};
    S2 := {};
    for p in getRelevantPrimes(beta) do (
	if odd getPadicValuation(d,p) then (
	    L1 = append(L1, p);
	    S1 = append(S1, d - 1);
	    );
	if even getPadicValuation(d,p) then (
	    L2 = append(L2, p^2);
	    S2 = append(S2, p)
	    );
	);
    
    -- We are looking for an element which is equivalent to (d - 1) mod p for each p in L1 and equivalent to p mod p^2 for each p in L2
    -- We use the solveCongruenceList method to find such an element
    alpha := solveCongruenceList(S1 | S2, L1 | L2);
    a := getSquarefreePart alpha;
    makeDiagonalForm(QQ, a)
    )

-- Input: A form q over QQ of anisotropic dimension 2
-- Output: The anisotropic part of q
-- Note: This is Koprowski/Rothkegel's Algorithm 8 in the case of QQ

getAnisotropicPartQQDimension2 = method()
getAnisotropicPartQQDimension2 GrothendieckWittClass := GrothendieckWittClass => beta -> (
    if getAnisotropicDimensionQQ(beta) != 2 then
	error "anisotropic dimension of form is not 2";

    n := getRank beta;

    -- Shortcut: if the form has anisotropic dimension 2 and the form is rank 2, return the form itself
    if n == 2 then return beta; 
    
    -- Step 1: We want the Witt index to be 0 mod 4 
    w := getWittIndex beta;
    q := beta;
    if (w % 4) != 0 then (
	w = w % 4;
	q = addGW(q, makeHyperbolicForm(QQ, 2*(4-w)));
	n = n + 2*(4-w);
	);

    -- Step 2: Compute discriminant (note that Koprowski/Rothkegel use a signed version of the discriminant in their algorithm)
    d := ((-1)^(n*(n-1)/2))*getIntegralDiscriminant(q);
    
    -- Step 3: Take relevant primes plus 2
    S := getRelevantPrimes beta;
    if not member(2, S) then S = append(S, 2);
    
    -- Start the loop at p = 2
    p := 2;
    solnFound := false;
    
    while not solnFound do (
	s := #S;

	-- Step 5a: Make a basis for the group of S-singular elements
	basisES:= append(S, -1);
	m := #basisES;

    	-- Step 5c: Make a vector of exponents of Hasse invariants
	W := mutableMatrix(QQ, s,1);
	for i from 0 to s - 1 do
	    W_(i,0) = (1 - (getHasseWittInvariant(q, S_i)))/2;
       	
	-- Step 5b / 5f: 
	W = matrix W;
    	if d < 0 then (
	    if abs(getSignature(q)) != 2 then
		error "getSignature isn't pm 2";
	    if getSignature(q) == 2 then W = matrix(QQ, {{0}}) || W;
	    if getSignature(q) == -2 then W = matrix(QQ, {{1}}) || W;
	    );
        
    	-- Step 5e: Make a matrix of Hilbert symbols
    	B := mutableMatrix(QQ, s,m);	
    	for i from 0 to s - 1 do (
	    for j from 0 to m - 1 do
	    	B_(i,j) = (1 - getHilbertSymbol(basisES_j, d,S_i))/2;
	    );
	B = matrix B;
    	
	-- Step 5d: Append a zero column on the left if the discriminant is negative
    	if (d < 0) then (
	    A := mutableMatrix(QQ, 1,m);
	    for i from 0 to m - 1 do (
	    	if basisES_i > 0 then (
		    A_(0,i) = 0;
		    )
		else 
                    A_(0,i) = 1;
	    	);
	    B = matrix(A) || B;
	    );

	-- Step 5f: Try to solve system of equations over GF(2)
        kk := GF(2);
    	W = matrix(kk, entries W);
    	B = matrix(kk, entries B);

	if class(solve(B, W)) === Matrix then (
	    X := solve(B, W);
	    solnFound = true;
	    break;
	    )
	else (
	    p = nextPrime(p + 1);
	    while member(p, S) do p = nextPrime(p + 1);
	    S = append(S, p);
	    );
	);
    alpha := sub(1, ZZ);
    for j from 0 to m - 1 do
	alpha = alpha * (basisES_j)^(sub(X_(j,0), ZZ));
    makeDiagonalForm(QQ, (alpha, -getSquarefreePart(alpha*d)))
    )

-- Input: A Grothendieck-Witt class representing a symmetric bilinear form over QQ
-- Output: The Grothendieck-Witt class representing the anisotropic part of this form

getAnisotropicPartQQ = method()
getAnisotropicPartQQ GrothendieckWittClass := GrothendieckWittClass => beta -> (
    beta = getDiagonalClass beta;
    n := getRank beta;
    
    -- If the form is anisotropic 
    if getAnisotropicDimension(beta) == n then return beta;
    
    -- Initialize an empty symmetric bilinear form
    outputForm := makeDiagonalForm(QQ, ());    
    alpha := 1;

    while getAnisotropicDimension(beta) >= 4 do (
	outputForm = addGW(outputForm, reduceAnisotropicPartQQDimension4(beta));
	alpha = (getMatrix reduceAnisotropicPartQQDimension4 beta)_(0,0);	
	beta = addGW(beta, makeDiagonalForm(QQ, ((-1)*alpha)));
	);
    
    if getAnisotropicDimension(beta) == 3 then (
	outputForm = addGW(outputForm, reduceAnisotropicPartQQDimension3(beta));
	alpha = (getMatrix reduceAnisotropicPartQQDimension3 beta)_(0,0);	
	beta = addGW(beta, makeDiagonalForm(QQ, ((-1)*alpha)));
	);
    
    if getAnisotropicDimension(beta) == 2 then
        outputForm = addGW(outputForm, getAnisotropicPartQQDimension2 beta);
    
    if getAnisotropicDimension(beta) == 1 then
	outputForm = addGW(outputForm, makeDiagonalForm(QQ, ((-1)^((n-1)/2))*getIntegralDiscriminant(beta)));
    
    outputForm
    )

-- Input: A symmetric matrix representing a symmetric bilinear form or a GrothendieckWittClass; over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A symmetric matrix or GrothendieckWittClass that is the anisotropic part of the input

getAnisotropicPart = method()
getAnisotropicPart Matrix := Matrix => A -> (
    k := ring A;
    -- Ensure base field is supported
    if not (instance(k, ComplexField) or instance(k, RealField) or k === QQ or (instance(k, GaloisField) and k.char != 2)) then
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
    -- Ensure underlying matrix is symmetric
    if not isSquareAndSymmetric A then
	error "Underlying matrix is not symmetric";
    -- Over CC, the anisotropic part is either the rank 0 form or the rank 1 form, depending on the anisotropic dimension
    if instance(k, ComplexField) then (
        if getAnisotropicDimension(A) == 0 then (
            return diagonalMatrix(CC, {});
            )
        else
            return matrix(CC, {{1}});
        )
    --Over RR, the anisotropic part consists of the positive entries in excess of the number of negative entries, or vice versa
    else if instance(k, RealField) then (
        diagonalA := diagonalizeViaCongruence A;
        posEntries := countPosDiagEntries diagonalA;
        negEntries := countNegDiagEntries diagonalA;
        if posEntries > negEntries then (
            return id_(RR^(posEntries - negEntries));
            )
        else if posEntries < negEntries then (
            return -id_(RR^(negEntries - posEntries));
            )
        else
            return diagonalMatrix(RR, {});
        )
    -- Over QQ, call getAnisotropicPartQQ
    else if k === QQ then (
        return getMatrix getAnisotropicPartQQ(makeGWClass getNondegeneratePartDiagonal A);
        )
    -- Over a finite field, if the anisotropic dimension is 1, then the form is either < 1 > or < e >, where e is any nonsquare representative,
    -- and if the anisotropic dimension is 2 then the form is <1,-e>
    else if (instance(k, GaloisField) and k.char != 2) then (
        diagA := diagonalizeViaCongruence(A);
        if getAnisotropicDimension(A) == 1 then (
            return matrix(k, {{sub((-1)^((getRank(diagA)-1)/2), k)*det(getNondegeneratePartDiagonal diagA)}});
            )
        else if getAnisotropicDimension(A) == 0 then (
            return diagonalMatrix(k, {});
            )
        else
            return matrix(k, {{1,0},{0, sub((-1)^((getRank(diagA)-2)/2), k)*det(getNondegeneratePartDiagonal diagA)}});
        );
    )

getAnisotropicPart GrothendieckWittClass := GrothendieckWittClass => alpha -> (
    makeGWClass getAnisotropicPart getMatrix(alpha)
    )

---------------------------------------
-- Simplifying a form
---------------------------------------

-- Input: A Grothendieck-Witt class beta over over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A simplified diagonal representative of beta
getSumDecompositionVerbose = method()
getSumDecompositionVerbose GrothendieckWittClass := (GrothendieckWittClass, String) => beta -> (
    -- Get base field of beta
    kk := getBaseField beta;

    if getRank(beta) == 0 then
	return (makeGWClass(diagonalMatrix(kk, {})), "empty form");
    
    outputString := "";
    
    -- Get isotropic dimension of beta and construct its isotropic and anistropic parts
    w := getWittIndex beta;
    
    if w > 0 then outputString = outputString | toString(w) | "H";
    
    hyperbolicPart := makeHyperbolicForm(kk,2*w);
    alpha := getAnisotropicPart beta;
    
    if getRank(alpha) > 0 then (
        D := getDiagonalEntries alpha;
        for i from 0 to length(D) - 1 do
	    outputString = outputString | " + <" | toString(D_i) | ">";
	);
    
    -- Return a simplified form of beta
    return (addGW(alpha, hyperbolicPart), outputString);
    )    

-- Input: A Grothendieck-Witt class beta over over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A simplified diagonal representative of beta

getSumDecomposition = method()
getSumDecomposition GrothendieckWittClass := GrothendieckWittClass => beta -> (
    beta.cache.getDiagonalClass = (getSumDecompositionVerbose beta)_0;
    (getSumDecompositionVerbose beta)_0
    )

-- Input: A Grothendieck-Witt class beta over over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: The decomposition of beta as a sum of hyperbolic and rank one forms

getSumDecompositionString = method()
getSumDecompositionString GrothendieckWittClass := String => beta -> (
    (getSumDecompositionVerbose beta)_1
    )
