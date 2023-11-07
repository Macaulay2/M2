-- We define GrothendieckWittClass to be a new type,
--    meant to represent the isomorphism class of a symmetric bilinear form
--    over a base field.
GrothendieckWittClass = new Type of HashTable
GrothendieckWittClass.synonym = "Grothendieck Witt Class"

-- Input: A symmetric matrix M defined over an arbitrary field
-- Output: The isomorphism class of a symmetric bilinear form represented by M

-- Note: A class in GW can be constructed from a representing matrix

gwClass = method()
gwClass (Matrix) := GrothendieckWittClass => M -> (
   if (isWellDefined(M)) then (
        new GrothendieckWittClass from {
            symbol matrix => M,
            symbol cache => new CacheTable
            }
        )
    else (
        error "gwClass called on a matrix that does not represent a nondegenerate symmetric bilinear form over a field of characteristic not 2";
        )
    )

-- This allows us to extract the matrix from a class
matrix GrothendieckWittClass := Matrix => beta -> beta.matrix



-- Check whether a matrix defines a nondegenerate symmeteric bilinear form
isWellDefined (Matrix) := Boolean => M -> (
    
    -- Returns false if a matrix isn't symmetric
    --	  Note that this will also return false if a matrix isn't square,
    --	      so we don't need another check for that.
    if not isSquareAndSymmetric(M) then(
	<< "-- Defining matrix is not symmetric" << endl;
	return false;
	);

    -- Returns false if the matrix represents a degenerate form
    if isDegenerate(M) then (
	<< "-- Defining matrix is degenerate" << endl;
	return false;
        );

    -- Returns false if the matrix isn't defined over a field
    if not isField ring M then (
	<< "-- Matrix is not defined over a field" << endl;
	return false;
	);
    
    -- Returns false if a matrix is defined over a field of characteristic two
    if char ring M == 2 then(
	<< "-- Package does not support base fields of characteristic two" <<endl;
	return false;
	);
    
    true);

-- Input: A Grothendieck-Witt class beta, the isomorphism class of a symmetric bilinear form
-- Output: The base ring of beta

baseField = method()
baseField GrothendieckWittClass := Ring => beta -> (
    ring beta.matrix
    )

-- Input: Two Grothendieck-Witt classes beta and gamma
-- Output: The direct sum of beta and gamma

gwAdd = method()
gwAdd(GrothendieckWittClass, GrothendieckWittClass) := GrothendieckWittClass => (beta, gamma) -> (
    Kb := baseField(beta);
    Kg := baseField(gamma);
    
    -- Galois field case
    if instance(Kb, GaloisField) and instance(Kg, GaloisField) then (
	-- Returns an error if the underlying fields of the two classes beta and gamma are different
	if not Kb.order == Kg.order  then error "Error: these classes have different underlying fields";
	return gwClass(beta.matrix ++ substitute(gamma.matrix,Kb))
	);
    
    -- remaining cases
    if not Kb === Kg then error "Error: these classes have different underlying fields";
    	return gwClass(beta.matrix ++ gamma.matrix)
    )

-- Input: Two Grothendieck-Witt classes beta and gamma
-- Output: The tensor product of beta and gamma

gwMultiply = method()

gwMultiply(GrothendieckWittClass, GrothendieckWittClass) := GrothendieckWittClass => (beta, gamma) -> (
    Kb := baseField(beta);
    Kg := baseField(gamma);
    
    -- Galois field case
    if instance(Kb, GaloisField) and instance(Kg, GaloisField) then (
	-- Returns an error if the underlying fields of the two classes beta and gamma are different
	if not Kb.order == Kg.order  then error "Error: these classes have different underlying fields";
	return gwClass(beta.matrix ** substitute(gamma.matrix,Kb))
	);
    
    -- remaining cases
    if not Kb === Kg then error "Error: these classes have different underlying fields";
    	return gwClass(beta.matrix ** gamma.matrix)
    )
