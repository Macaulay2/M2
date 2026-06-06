-- Input: A ring
-- Output: Boolean that gives whether the ring is a finite etale algebra over a field (and in particular if it is a field or a field extension)
isFiniteEtaleAlgebra = method()
isFiniteEtaleAlgebra QuotientRing := Boolean => (Alg) -> (
    -- Verifies that the input is a finitely generated algebra over a field of dimension 0
    if not (isField Alg or (instance(Alg, QuotientRing) and isField coefficientRing Alg and dim Alg == 0)) then return false;

    -- Being a finite etale algebra over a field can be checked by verifying that the trace defines a nondegenerate symmetric bilinear form from Alg x Alg to the base field
    baseField := coefficientRing Alg;
    B := flatten entries basis Alg; 
    n := #B;
    
    -- Define the matrix of the trace form
    M := mutableMatrix id_(baseField^n);
    
    for i from 0 to n-1 do (
        for j from 0 to n-1 do (
            M_(i,j) = getTrace(Alg, B_i * B_j);
        );
    );

    discAlg := determinant matrix M;

    if (discAlg == 0_baseField) then return false;

    true
)


-- Input: A matrix
-- Output: Boolean that gives whether the matrix defines a nondegenerate symmetric bilinear form over an algebra of characteristic not 2

isWellDefinedGW = method()
isWellDefinedGW Matrix := Boolean => M -> (
    
    -- Return false if the matrix isn't square and symmetric
    if not isSquareAndSymmetric M then return false;

    -- Return false if the matrix represents a degenerate form
    if isDegenerate M then return false;

    -- Return false if the matrix isn't defined over a field
    if not (isField ring M or isFiniteEtaleAlgebra ring M) then return false;
    
    -- Returns false if the matrix is defined over a ring of characteristic 2
    if char(ring M) == 2 then return false;

    -- Otherwise, return true
    true
    )

-- We define GrothendieckWittClass to be a new type, meant to represent the isomorphism class 
-- of a nondegenerate symmetric bilinear form over a field of characteristic not 2

GrothendieckWittClass = new Type of HashTable
GrothendieckWittClass.synonym = "Grothendieck-Witt Class"

-- Input: A matrix M representing a nondegenerate symmetric bilinear form over a field of characteristic not 2
-- Output: The GrothendieckWittClass representing the symmetric bilinear form determined by M

makeGWClass = method()
makeGWClass Matrix := GrothendieckWittClass => M -> (
   if isWellDefinedGW M then (
        new GrothendieckWittClass from {
            symbol matrix => M,
            symbol cache => new CacheTable,
            }
        )
    else (
        error "makeGWClass called on a matrix that does not represent a nondegenerate symmetric bilinear form over a field of characteristic not 2";
	)
    )

-- Input: A GrothendieckWittClass
-- Output: A net for printing the underlying matrix

net GrothendieckWittClass := Net => alpha -> (
    net getMatrix alpha
    )

-- Input: A GrothendieckWittClass
-- Output: A string for printing the underlying matrix

texMath GrothendieckWittClass := String => alpha -> (
    texMath getMatrix alpha
    )

getAlgebra = method()
getAlgebra GrothendieckWittClass := Ring => beta -> (
    ring getMatrix beta
    )

-- Input: A Grothendieck-Witt class beta, the isomorphism class of a symmetric bilinear form
-- Output: The base field of beta

getBaseField = method()
getBaseField GrothendieckWittClass := Ring => beta -> (
    if (instance(getAlgebra beta, ComplexField)) or (instance(getAlgebra beta, RealField)) or (getAlgebra beta === QQ) or (instance(getAlgebra beta, GaloisField)) then return getAlgebra beta;

    if not isPrime ideal(0_(getAlgebra beta)) then error "the Grothendieck-Witt class is not defined over a field";

    if (not isField getAlgebra beta) then return toField getAlgebra beta;

    ring getMatrix beta
    )

-- Input: A GrothendieckWittClass representing a symmetric bilinear form determined by a matrix M
-- Output: The matrix M

getMatrix = method()
getMatrix GrothendieckWittClass := Matrix => alpha -> (
    alpha.matrix
    )

-- Input: Two Grothendieck-Witt classes beta and gamma over the same field
-- Output: The direct sum of beta and gamma

addGW = method()
addGW (GrothendieckWittClass,GrothendieckWittClass) := GrothendieckWittClass => (beta,gamma) -> (
    Kb := getAlgebra beta;
    Kg := getAlgebra gamma;
    
    -- Galois field case
    if instance(Kb, GaloisField) and instance(Kg, GaloisField) then (
	-- Return an error if the underlying fields of the two classes are different
	if not Kb.order == Kg.order then
	    error "these classes have different underlying fields";
	return makeGWClass(getMatrix beta ++ sub(getMatrix gamma, Kb));
	);
    
    -- Remaining cases
    if not Kb === Kg then
	error "these classes have different underlying fields";
    makeGWClass(getMatrix beta ++ getMatrix gamma)
    )

-- Input: Two Grothendieck-Witt classes beta and gamma over the same field
-- Output: The tensor product of beta and gamma

multiplyGW = method()
multiplyGW (GrothendieckWittClass,GrothendieckWittClass) := GrothendieckWittClass => (beta,gamma) -> (
    Kb := getAlgebra beta;
    Kg := getAlgebra gamma;
    
    -- Galois field case
    if instance(Kb, GaloisField) and instance(Kg, GaloisField) then (
	-- Return an error if the underlying fields of the two classes are different
	if not Kb.order == Kg.order then
	    error "these classes have different underlying fields";
	return makeGWClass(getMatrix beta ** substitute(getMatrix gamma,Kb));
	);
    
    -- Remaining cases
    if not Kb === Kg then
	error "these classes have different underlying fields";
    makeGWClass(getMatrix beta ** getMatrix gamma)
    )
