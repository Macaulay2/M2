protect symbol scalar

-- Input: A matrix and a scalar
-- Output: Boolean that gives whether the matrix defines a well-defined class of the unstable Grothendieck-Witt group. 

isWellDefinedGWu = method()
-- First version of this function treats the case where b is a Number (eg. an element of CC_53, RR_53, QQ, or ZZ)
isWellDefinedGWu (Matrix, Number) := Boolean => (M, b) -> (

    -- Return false if b is not compatible with the ring of M
    if not isCompatibleElement(ring M, b) then return false;
    c := substitute(b, ring M);

    -- Return false if c is not a unit of the finite etale algebra
    if not isUnit c then return false;

    -- If matrix is defined over the real numbers, require that the signs of c and of det M agree
    if instance(ring M, RealField) and sign c != sign det M then return false;
    
    -- If matrix is defined over the rational numbers, require that det M and c are in the same square class
    if ring M === QQ and getSquarefreePart det M != getSquarefreePart c then return false;
    
    -- If matrix is defined over a finite field, require that det M and c are in the same square class.
    if instance(ring M, GaloisField) and isGFSquare det M != isGFSquare c then return false;
    
    -- If matrix is defined over an arbitrary algebra, scalars being equal to the determinant of the matrix are allowed automatically, but we are unable to check representatives up to squares.
    if not (instance(ring M, ComplexField) or instance(ring M, RealField) or ring M === QQ or instance(ring M, GaloisField)) then (
        -- warn if we can’t test square class of det
        if det M =!= c then
            print "Warning, unable to verify whether the determinant of M and b agree up to squares.";
    );
    -- Then check whether M gives a well-defined element of GW(k)
    isWellDefinedGW M
)

-- Second version of this function treats the case where b is a RingElement (e.g. an element of a Galois field)
isWellDefinedGWu (Matrix, RingElement) := Boolean => (M, b) -> (

    -- return false if b is not compatible with the ring of M
    if not isCompatibleElement(ring M, b) then return false;
    c := substitute(b, ring M);

    -- Return false if a is not a unit in the finite etale algebra
    if not isUnit c then return false;

    -- If matrix is defined over a finite field, require that det M and c are in the same square class.
    if instance(ring M, GaloisField) and isGFSquare det M != isGFSquare c then return false;

    -- If matrix is defined over an arbitrary algebra, scalars being equal to the determinant of the matrix are allowed automatically, but we are unable to check representatives up to squares.
    if not instance(ring M, GaloisField) then (
        -- warn if we can’t test square‐class of det
        if det M =!= c then
            print "Warning, unable to verify whether the determinant of M and b agree up to squares.";
    );
    -- Then check whether M gives a well-defined element of GW(k)
    isWellDefinedGW M
    )

-- We define UnstableGrothendieckWittClass to be a new type, meant to represent the isomorphism class 
-- of a nondegenerate symmetric bilinear form over a field of characteristic not 2 together with the data of a scalar.

UnstableGrothendieckWittClass = new Type of HashTable
UnstableGrothendieckWittClass.synonym = "Unstable Grothendieck-Witt Class"

-- Input: An UnstableGrothendieckWittClass
-- Output: A net for printing the underlying data

net UnstableGrothendieckWittClass := Net => alpha -> (
    net (getMatrix alpha, getScalar alpha)
    )

-- Input: An UnstableGrothendieckWittClass
-- Output: A string for printing the underlying matrix

texMath UnstableGrothendieckWittClass := String => alpha -> (
    texMath (getMatrix alpha, getScalar alpha)
    )

-- Input: Either a matrix M or a matrix-scalar pair (M,b) representing a well-defined element of the unstable Grothendieck-Witt group. 
-- Output: The GrothendieckWittClass representing the symmetric bilinear form determined by M

makeGWuClass = method()

-- First version of this function treats the case of an input (M,b) where b is a Number (eg. an element of CC_53, RR_53, QQ, or ZZ)
makeGWuClass (Matrix, Number) := UnstableGrothendieckWittClass => (M, b) -> (
   if isWellDefinedGWu (M, b) then (
        new UnstableGrothendieckWittClass from {
            symbol matrix => M,
            symbol cache => new CacheTable,
            symbol scalar => substitute(b, ring M)
            }
        )
    else (
        error "makeGWuClass called on a pair that does not produce a well-defined element of the unstable Grothendieck-Witt group.";
	)
    )

-- Second version of this function treats the case of an input (M,b) where b is a Number (eg. an element of a Galois field)
makeGWuClass (Matrix, RingElement) := UnstableGrothendieckWittClass => (M, b) -> (
   if isWellDefinedGWu (M, b) then (
        new UnstableGrothendieckWittClass from {
            symbol matrix => M,
            symbol cache => new CacheTable,
            symbol scalar => substitute(b, ring M)
            }
        )
    else (
        error "makeGWuClass called on a pair that does not produce a well-defined element of the unstable Grothendieck-Witt group.";
	)
    )

-- Third version of this function treats the case of an input M, where the scalar is assumed to be the determinant of M. 
makeGWuClass Matrix := UnstableGrothendieckWittClass => M -> (
   if isWellDefinedGWu (M, det M) then (
        new UnstableGrothendieckWittClass from {
            symbol matrix => M,
            symbol cache => new CacheTable,
            symbol scalar => substitute(det M, ring M)
            }
        )
    else (
        error "makeGWuClass called on a matrix that does not produce a well-defined element of the unstable Grothendieck-Witt group.";
	)
    )

-- Fourth version of this function treats the case of an input a GrothendieckWittClass alpha and a Number (eg. an element of CC_53, RR_53, QQ, or ZZ)
makeGWuClass (GrothendieckWittClass, Number) := UnstableGrothendieckWittClass => (alpha, b) -> (
   if isWellDefinedGWu (getMatrix alpha, b) then (
        new UnstableGrothendieckWittClass from {
            symbol matrix => getMatrix alpha,
            symbol cache => new CacheTable,
            symbol scalar => substitute(b, getBaseField alpha)
            }
        )
    else (
        error "makeGWuClass called on a pair that does not produce a well-defined element of the unstable Grothendieck-Witt group.";
	)
    )

-- Fifth version of this function treats the case of an input a GrothendieckWittClass alpha and a RingElement (eg. an element of a Galois field)
makeGWuClass (GrothendieckWittClass, RingElement) := UnstableGrothendieckWittClass => (alpha, b) -> (
   if isWellDefinedGWu (getMatrix alpha, b) then (
        new UnstableGrothendieckWittClass from {
            symbol matrix => getMatrix alpha,
            symbol cache => new CacheTable,
            symbol scalar => substitute(b, getBaseField alpha)
            }
        )
    else (
        error "makeGWuClass called on a pair that does not produce a well-defined element of the unstable Grothendieck-Witt group.";
	)
    )

-- Sixth version of this function treats the case of an input a GrothendieckWittClass alpha, where the scalar is assumed to be the determinant of the Gram matrix of alpha.  
makeGWuClass GrothendieckWittClass := UnstableGrothendieckWittClass => alpha -> (
   if isWellDefinedGWu (getMatrix alpha, det getMatrix alpha) then (
        new UnstableGrothendieckWittClass from {
            symbol matrix => getMatrix alpha,
            symbol cache => new CacheTable,
            symbol scalar => substitute(det getMatrix alpha, getBaseField alpha)
            }
        )
    else (
        error "makeGWuClass called on a form that does not produce a well-defined element of the unstable Grothendieck-Witt group.";
	)
    )

-- Input: An unstable Grothendieck-Witt class
-- Output: Its stable part

getGWClass = method()
getGWClass UnstableGrothendieckWittClass := GrothendieckWittClass => alpha -> (
    makeGWClass getMatrix alpha
)

-- Input: An UnstableGrothendieckWittClass
-- Output: A string for printing the underlying scalar

getScalar = method()
getScalar UnstableGrothendieckWittClass := alpha -> (
    alpha.scalar
)


-- Input: A GrothendieckWittClass representing a symmetric bilinear form determined by a matrix M
-- Output: The matrix M

getMatrix UnstableGrothendieckWittClass := Matrix => alpha -> (
    alpha.matrix
    )

-- Input: An UnstableGrothendieckWittClass
-- Output: A ring, the algebra it is defined over
getAlgebra UnstableGrothendieckWittClass := Ring => beta -> (
    ring getMatrix beta
    )

-- Input: An unstable Grothendieck-Witt class beta
-- Output: The base field of beta
getBaseField UnstableGrothendieckWittClass := Ring => beta -> (
    if instance(getAlgebra beta, ComplexField) or instance(getAlgebra beta, RealField) or getAlgebra beta === QQ or instance(getAlgebra beta, GaloisField) then return getAlgebra beta;

    if not isPrime ideal(0_(ring getMatrix beta)) then error "the Grothendieck-Witt class is not defined over a field";

    if not isField ring getMatrix beta then return toField ring getMatrix beta;

    ring getMatrix beta
    )

-- Input: Two Grothendieck-Witt classes beta and gamma over the same field
-- Output: The direct sum of beta and gamma

addGWu = method()
addGWu (UnstableGrothendieckWittClass, UnstableGrothendieckWittClass) := UnstableGrothendieckWittClass => (beta, gamma) -> (
    Kb := getBaseField beta;
    Kg := getBaseField gamma;
    
    -- Galois field case
    if instance(Kb, GaloisField) and instance(Kg, GaloisField) then (
	-- Return an error if the underlying fields of the two classes are different
	if not Kb.order == Kg.order then
	    error "these classes have different underlying fields";
	return makeGWuClass(getMatrix beta ++ substitute(getMatrix gamma, Kb), getScalar beta * substitute(getScalar gamma, Kb));
	);
    
    -- Remaining cases
    if not Kb === Kg then
	error "these classes have different underlying fields";
    makeGWuClass(getMatrix beta ++ getMatrix gamma, getScalar beta * getScalar gamma)
    )

-- Input: List of GWu(k) classes, list of elements of k
-- Output: The divisorial sum of the GWu(k) classes as a GWu(k) class

addGWuDivisorial = method()
addGWuDivisorial (List, List) := UnstableGrothendieckWittClass => (classList, rootList) -> (
    n := #classList;
    baseFieldList := apply(classList, getBaseField);
    matrixList := apply(classList, getMatrix);
    scalarList := apply(classList, getScalar);
    multiplicityList := apply(classList, i -> rank getMatrix i);
    isGaloisField := apply(baseFieldList, i -> instance(i, GaloisField));

    -- Return an error if list of roots is of different size than list of classes
    if n != #rootList then
        error "need same number of classes and roots";

    -- Return an error if lists are empty
    if n == 0 then
        error "the empty sum is the additive identity of the unstable Grothendieck-Witt group over the field of interest; please construct this as makeGWuClass(matrix(k,{}),1)";

    -- Return an error if the base fields are different for the list of GWu classes
    if (not instance(baseFieldList#0, GaloisField) and not same baseFieldList) or (isGaloisField#0 and (not same isGaloisField or not same apply(baseFieldList, i -> i.order))) then 
        error "the list of GWu classes should have the same base field";
    
    -- Return an error if the roots are not in the correct field
    for i from 0 to n-1 do (
        if not isCompatibleElement(baseFieldList#i, rootList#i) then error "the roots must be in the base field of the classes";
    );

    -- Create the sum matrix and scalar    
    newForm := directSum matrixList;
    newScalar := product scalarList;
    for i from 0 to n-1 do (
        for j from i+1 to n-1 do ( -- We require j > i 
            newScalar = newScalar * (rootList#i - rootList#j)^(2 * multiplicityList#i * multiplicityList#j);
        );
    );
    makeGWuClass(newForm,newScalar)
    )

-- Input: An unstable Grothendieck-Witt class beta over QQ, RR, CC, or a finite field of characteristic not 2
-- Output: A diagonalized form of beta, with squarefree entries on the diagonal
getDiagonalClass UnstableGrothendieckWittClass := UnstableGrothendieckWittClass => beta -> (

    -- Check if the diagonal class has already been computed; if so, recall it from the cache
    if beta.cache.?getDiagonalClass then return beta.cache.getDiagonalClass;

    getDiagonalClassOfBetaMatrix := diagonalizeAndSimplifyViaCongruence getMatrix beta;

    -- The computed diagonal class gets stored in the cache
    beta.cache.getDiagonalClass = makeGWuClass(getDiagonalClassOfBetaMatrix, getScalar beta);
    makeGWuClass(getDiagonalClassOfBetaMatrix, getScalar beta)
    )

-- Input: Two unstable Grothendieck-Witt classes over CC, RR, QQ, or a finite field of characteristic not 2
-- Output: Boolean that gives whether the classes are isomorphic

isIsomorphicForm (UnstableGrothendieckWittClass,UnstableGrothendieckWittClass) := Boolean => opts -> (alpha,beta) -> (
                                                                                                                                                          
    linTol := opts.linearTolerance;

    if linTol <= 0 then error "linearTolerance must be a positive number";
    
    r1 := getScalar alpha;
    r2 := getScalar beta;

    k1 := ring r1;
    k2 := ring r2;

    -- Ensure both base fields are supported
    if not (instance(k1, ComplexField) or instance(k1, RealField) or k1 === QQ or (instance(k1, GaloisField) and k1.char != 2)) then
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
    if not (instance(k2, ComplexField) or instance(k2, RealField) or k2 === QQ or (instance(k2, GaloisField) and k2.char != 2)) then
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
    
    -- In most cases, we can check equality directly
    if (instance(k1, ComplexField) and instance(k2, ComplexField)) or (instance(k1, RealField) and instance(k2, RealField)) then (
                                                                                                                                                                      return(abs(r1 - r2) < linTol and isIsomorphicForm(getMatrix alpha, getMatrix beta)
	    );
	)
	
    -- Over QQ
    else if  (k1 === QQ and k2 === QQ) then (
        return (isIsomorphicForm(getMatrix alpha, getMatrix beta) and r1 == r2);
	)
    
    -- Over a finite field, the scalars are in the same square class if and only if they are either both squares or both not squares 
    else if instance(k1, GaloisField) and instance(k2, GaloisField) and k1.char !=2 and k2.char != 2 and k1.order == k2.order then (
        return (isIsomorphicForm(getMatrix alpha, getMatrix beta) and getScalar alpha == substitute(getScalar beta, k1));
        )
    
    -- If we get here, then the base fields are not the same
    else error "Base fields are not the same";
)
