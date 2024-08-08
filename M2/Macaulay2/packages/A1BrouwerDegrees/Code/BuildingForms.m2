-----------------------
-- Producing new forms
-----------------------

-- Input: A field kk of characteristic not 2, and a list of elements a_1,...,a_n of kk
-- Output: The Grothendieck-Witt class represented by the diagonal form <a_1,...,a_n> 

makeDiagonalForm = method()
makeDiagonalForm (Ring, RingElement) := GrothendieckWittClass => (kk, a) -> (
    makeGWClass matrix(kk, {{sub(a, kk)}})
    )

makeDiagonalForm (Ring, ZZ) := GrothendieckWittClass => (kk, a) -> (
    makeGWClass matrix(kk, {{sub(a, kk)}})
    )

makeDiagonalForm (Ring, QQ) := GrothendieckWittClass => (kk, a) -> (
    makeGWClass matrix(kk, {{sub(a, kk)}})
    )

makeDiagonalForm (Ring, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Build an n x n mutable identity matrix
    A := mutableIdentity(kk, n);
    
    for i from 0 to n - 1 do A_(i,i) = sub(L_i, kk);
    
    -- A is mutable so we take matrix A and form a Grothendieck-Witt class
    makeGWClass matrix A
    )

makeDiagonalForm (InexactFieldFamily,RingElement) := GrothendieckWittClass => (kk, a) -> (
    makeGWClass matrix(kk, {{sub(a, kk)}})
    )

makeDiagonalForm (InexactFieldFamily,ZZ) := GrothendieckWittClass => (kk, a) -> (
    makeGWClass matrix(kk, {{sub(a, kk)}})
    )

makeDiagonalForm (InexactFieldFamily,QQ) := GrothendieckWittClass => (kk, a) -> (
    makeGWClass matrix(kk, {{sub(a, kk)}})
    )

makeDiagonalForm (InexactFieldFamily, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Build an n x n mutable identity matrix
    A := mutableIdentity(kk, n);
    
    for i from 0 to n - 1 do A_(i,i) = sub(L_i, kk);
    
    -- A is mutable so we take matrix A and form a Grothendieck-Witt class
    makeGWClass matrix A
    )

-- Input: A field kk of characteristic not 2, and an optional even rank n (default is n = 2)
-- Output: A Grothendieck-Witt class over kk represented by a totally hyperbolic form of rank n

makeHyperbolicForm = method()
makeHyperbolicForm Ring := GrothendieckWittClass => kk -> (
    makeGWClass matrix(kk, {{1,0},{0,-1}})
    )

makeHyperbolicForm (Ring, ZZ) := GrothendieckWittClass => (kk, n) -> (
    if odd n then error "entered rank is odd";
    H := matrix(kk, {{1,0},{0,-1}});
    m := sub(n/2, ZZ);
    outputMatrix := diagonalMatrix(kk, {});
    for i from 0 to m - 1 do outputMatrix = outputMatrix ++ H;
    makeGWClass outputMatrix
    )

makeHyperbolicForm InexactFieldFamily := GrothendieckWittClass => kk -> (
    makeGWClass matrix(kk, {{1,0},{0,-1}})
    )

makeHyperbolicForm (InexactFieldFamily, ZZ) := GrothendieckWittClass => (kk, n) -> (
    if odd n then error "entered rank is odd";
    H := matrix(kk, {{1,0},{0,-1}});
    m := sub(n/2, ZZ);
    outputMatrix := diagonalMatrix(kk, {});
    for i from 0 to m - 1 do outputMatrix = outputMatrix ++ H;
    makeGWClass outputMatrix
    )

-- Input: A field kk of characteristic not 2, and a list of elements a_1,...,a_n of kk
-- Output: The Pfister form <<a_1,...,a_n>>

makePfisterForm = method()
makePfisterForm (Ring, RingElement) := GrothendieckWittClass => (kk, a) -> (
    makeDiagonalForm(kk, (1, sub((-1)*a, kk)))
    )

makePfisterForm (Ring, ZZ) := GrothendieckWittClass => (kk, a) -> (
    makeDiagonalForm(kk, (1, sub((-1)*a, kk)))
    )

makePfisterForm (Ring, QQ) := GrothendieckWittClass => (kk, a) -> (
    makeDiagonalForm(kk, (1, sub((-1)*a, kk)))
    )

makePfisterForm (Ring, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Iteratively multiply <1,-L_0>*<1,-L_1>*...
    outputForm := makeDiagonalForm(kk, 1);
    for i from 0 to n - 1 do (
	ithPfister := makePfisterForm(kk, L_i);
	outputForm = multiplyGW(outputForm, ithPfister);
	);
    outputForm
    )

makePfisterForm (InexactFieldFamily, RingElement) := GrothendieckWittClass => (kk, a) -> (
    makeDiagonalForm(kk, (1, sub((-1)*a, kk)))
    )

makePfisterForm (InexactFieldFamily,ZZ) := GrothendieckWittClass => (kk, a) -> (
    makeDiagonalForm(kk,(1, sub((-1)*a, kk)))
    )

makePfisterForm (InexactFieldFamily,QQ) := GrothendieckWittClass => (kk, a) -> (
    makeDiagonalForm(kk,(1, sub((-1)*a, kk)))
    )

makePfisterForm (InexactFieldFamily,Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Iteratively multiply <1,-L_0>*<1,-L_1>*...
    outputForm := makeDiagonalForm(kk, 1);
    for i from 0 to n - 1 do (
	ithPfister := makePfisterForm(kk, L_i);
	outputForm = multiplyGW(outputForm, ithPfister);
	);
    outputForm
    )
