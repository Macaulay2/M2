-----------------------
-- Producing new forms
-----------------------

-- Input: A ring and a scalar
-- Output: Boolean that gives whether the scalar is compatible with the ring

isCompatibleElement = method()
-- The first version of this function treats the case when R is a Ring and b is a Number (e.g. an element of RR_53, CC_53, QQ, or ZZ)
isCompatibleElement (Ring, Number) := Boolean => (R, b) -> (
    -- If the ring is the complex numbers, require the scalar to be a complex number, a real number, a rational number, or an integer
    if instance(R, ComplexField) then (
        if not (instance(ring b, ComplexField) or instance(ring b, RealField) or ring b === QQ or ring b === ZZ) then return false;
    )
    -- If the ring is the real numbers, require the scalar to be a real number, a rational number, or an integer
    else if instance(R, RealField) then (
        if not (instance(ring b, RealField) or ring b === QQ or ring b === ZZ) then return false;
    )
    -- If the ring is the rational numbers, require the scalar to be a rational number or an integer
    else if R === QQ then (
        if not (ring b === QQ or ring b === ZZ) then return false;
    )
    -- If the ring is a finite field, require the scalar to be a rational number or an integer. (The case when the scalar is an element of a finite field is treated in the next variant.)
    else if instance(R, GaloisField) then (
        if not (ring b === QQ or ring b === ZZ) then return false;
    )
    -- For all other rings, require the scalar to be an element of the ring
    else (
        if ring b =!= R then return false;
    );
    -- If we get through the above, then the scalar is compatible with the ring
    true
)

-- The second version of this function treats the case when R is a Ring and b is a RingElement (e.g. an element of a finite field)
isCompatibleElement (Ring, RingElement) := Boolean => (R, b) -> (
    -- If the ring is the complex numbers, the real numbers, or the rational numbers, the scalar is not compatible. (The case when the scalar is a complex number, real number, rational number, or integer is treated in the previous variant.)
    if instance(R, ComplexField) or instance(R, RealField) or R === QQ then return false;
    -- If the ring is a finite field, require the scalar to be an element of that finite field. (The case when the scalar is a rational number or integer is treated in the previous variant.)
    if instance(R, GaloisField) then (
        if not (instance(ring b, GaloisField) and R.order == (ring b).order) then return false;
    )
    -- For other rings, require the scalar to be an element of the ring
    else (
        if ring b =!= R then return false;
    );
    -- If we get through the above, then the scalar is compatible with the ring
    true
)

-- The third version of this function treats the case when R is an InexactFieldFamily and b is a Number (e.g. an element of RR_53, CC_53, QQ, or ZZ)
isCompatibleElement (InexactFieldFamily, Number) := Boolean => (R, b) -> (
    -- If the inexact field family is the complex numbers, require the scalar to be a complex number, a real number, a rational number, or an integer
    if R === CC then (
        if not (instance(ring b, ComplexField) or instance(ring b, RealField) or ring b === QQ or ring b === ZZ) then return false;
    )
    -- If the inexact field family is the real numbers, require the scalar to be a real number, a rational number, or an integer
    else if R === RR then (
        if not (instance(ring b, RealField) or ring b === QQ or ring b === ZZ) then return false;
    )
    -- For any other inexact field families, require the scalar to be an element of the inexact field family
    else (
        if ring b =!= R then return false;
    );
    -- If we get through the above, then the scalar is compatible with the ring
    true
)

-- The fourth version of this function treats the case when R is an InexactFieldFamily and b is a RingElement (e.g. an element of a finite field)
isCompatibleElement (InexactFieldFamily, RingElement) := Boolean => (R, b) -> (
    -- Here we require the scalar to be an element of the inexact field family
    ring b === R
)

-- Input: A field kk of characteristic not 2, and a list of elements b_1,...,b_n of kk
-- Output: The Grothendieck-Witt class represented by the diagonal form <b_1,...,b_n> 

makeDiagonalForm = method()
makeDiagonalForm (Ring, RingElement) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeGWClass matrix(kk, {{substitute(b, kk)}})
    )

makeDiagonalForm (Ring, Number) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeGWClass matrix(kk, {{substitute(b, kk)}})
    )

makeDiagonalForm (Ring, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Iteratively add <L_0> + <L_1> + ...
    outputForm := makeGWClass matrix(kk,{});
    for i from 0 to n - 1 do (
	    ithDiagonal := makeDiagonalForm(kk, L_i);
	    outputForm = addGW(outputForm, ithDiagonal);
	);
    outputForm
    )

makeDiagonalForm (InexactFieldFamily,RingElement) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeGWClass matrix(kk, {{substitute(b, kk)}})
    )

makeDiagonalForm (InexactFieldFamily, Number) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeGWClass matrix(kk, {{substitute(b, kk)}})
    )


makeDiagonalForm (InexactFieldFamily, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Iteratively add <L_0> + <L_1> + ...
    outputForm := makeGWClass matrix(kk,{});
    for i from 0 to n - 1 do (
	    ithDiagonal := makeDiagonalForm(kk, L_i);
	    outputForm = addGW(outputForm, ithDiagonal);
	);
    outputForm
    )

makeAntidiagonalUnstableForm = method()
makeAntidiagonalUnstableForm (Ring, RingElement, ZZ) := UnstableGrothendieckWittClass => (kk, b, n) -> (
    
    -- Build an n x n mutable identity matrix
    A := mutableMatrix(kk, n,n);
    for i from 0 to n - 1 do (
        if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
        A_(i,n-1-i) = substitute(b, kk);
    );
    
    -- A is mutable so we take matrix A and form a Grothendieck-Witt class
    makeGWuClass matrix A
    )

makeAntidiagonalUnstableForm (Ring, Number, ZZ) := UnstableGrothendieckWittClass => (kk, b, n) -> (
    
    -- Build an n x n mutable identity matrix
    A := mutableMatrix(kk, n,n);
    for i from 0 to n - 1 do (
        if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
        A_(i,n-1-i) = substitute(b, kk);
    );
    
    -- A is mutable so we take matrix A and form a Grothendieck-Witt class
    makeGWuClass matrix A
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
    m := substitute(n/2, ZZ);
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
    m := substitute(n/2, ZZ);
    outputMatrix := diagonalMatrix(kk, {});
    for i from 0 to m - 1 do outputMatrix = outputMatrix ++ H;
    makeGWClass outputMatrix
    )

-- Input: A field kk of characteristic not 2, and a list of elements b_1,...,b_n of kk
-- Output: The Pfister form <<b_1,...,b_n>>

makePfisterForm = method()
makePfisterForm (Ring, RingElement) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeDiagonalForm(kk, (1, substitute((-1)*b, kk)))
    )

makePfisterForm (Ring, Number) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeDiagonalForm(kk, (1, substitute((-1)*b, kk)))
    )

makePfisterForm (Ring, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Iteratively multiply <1,-L_0> * <1,-L_1> * ...
    outputForm := makeDiagonalForm(kk, 1);
    for i from 0 to n - 1 do (
	    ithPfister := makePfisterForm(kk, L_i);
	    outputForm = multiplyGW(outputForm, ithPfister);
	);
    outputForm
    )

makePfisterForm (InexactFieldFamily, RingElement) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeDiagonalForm(kk, (1, substitute((-1)*b, kk)))
    )

makePfisterForm (InexactFieldFamily, Number) := GrothendieckWittClass => (kk, b) -> (
    if not isCompatibleElement(kk, b) then error "scalar not compatible with field";
    makeDiagonalForm(kk, (1, substitute((-1)*b, kk)))
    )

makePfisterForm (InexactFieldFamily, Sequence) := GrothendieckWittClass => (kk, L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Iteratively multiply <1,-L_0> * <1,-L_1> * ...
    outputForm := makeDiagonalForm(kk, 1);
    for i from 0 to n - 1 do (
	ithPfister := makePfisterForm(kk, L_i);
	outputForm = multiplyGW(outputForm, ithPfister);
	);
    outputForm
    )

-- Input: A field kk of characteristic not 2, and a list of elements b_1,...,b_n of kk
-- Output: The unstable Grothendieck-Witt class represented by the diagonal form <b_1,...,b_n> 

makeDiagonalUnstableForm = method()
makeDiagonalUnstableForm (Ring, RingElement) := UnstableGrothendieckWittClass => (kk, b) -> (
    makeGWuClass makeDiagonalForm(kk, b)
    )

makeDiagonalUnstableForm (Ring, Number) := UnstableGrothendieckWittClass => (kk, b) -> (
    makeGWuClass makeDiagonalForm(kk, b)
    )

makeDiagonalUnstableForm (Ring, Sequence) := UnstableGrothendieckWittClass => (kk, L) -> (
    makeGWuClass makeDiagonalForm(kk, L)
    )

makeDiagonalUnstableForm (InexactFieldFamily, RingElement) := UnstableGrothendieckWittClass => (kk, b) -> (
    makeGWuClass makeDiagonalForm(kk, b)
    )

makeDiagonalUnstableForm (InexactFieldFamily, Number) := UnstableGrothendieckWittClass => (kk, b) -> (
    makeGWuClass makeDiagonalForm(kk, b)
    )

makeDiagonalUnstableForm (InexactFieldFamily, Sequence) := UnstableGrothendieckWittClass => (kk, L) -> (
    makeGWuClass makeDiagonalForm(kk, L)
    )

-- Input: A field kk of characteristic not 2, and an optional even rank n (default is n = 2)
-- Output: An unstable Grothendieck-Witt class over kk represented by a totally hyperbolic form of rank n

makeHyperbolicUnstableForm = method()
makeHyperbolicUnstableForm Ring := UnstableGrothendieckWittClass => kk -> (
    makeGWuClass makeHyperbolicForm kk
    )

makeHyperbolicUnstableForm (Ring, ZZ) := UnstableGrothendieckWittClass => (kk, n) -> (
    makeGWuClass makeHyperbolicForm(kk, n)
    )

makeHyperbolicUnstableForm InexactFieldFamily := UnstableGrothendieckWittClass => kk -> (
    makeGWuClass makeHyperbolicForm kk
    )

makeHyperbolicUnstableForm (InexactFieldFamily, ZZ) := UnstableGrothendieckWittClass => (kk, n) -> (
    makeGWuClass makeHyperbolicForm(kk, n)
    )
