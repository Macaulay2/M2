-- return the monomial order stashed inside of a ring
getMonomialOrder = S -> (options S).MonomialOrder

-- Sorts and adds the elements of the matrix "candidates" to the pending list of R
    -- R is a subalgebra
    -- candidates is a matrix of elements of the subalgebra.
    -- Algorithm makes a pass through the elements in the first row of "candidates" and places them in the correct sublist of subalgComp#"Pending".

insertPending = (compTable, candidates) -> (
    for candidate in first entries candidates do(
	
        -- get the entry of the column and its degree
        level := (degree candidate)_0;
	if compTable#"pending"#?level then(
            compTable#"pending"#level = append(compTable#"pending"#level, candidate);
    	) else (
	    compTable#"pending"#level = new MutableList from{candidate};
	);
    );
)

-- Finds the lowest nonempty list in Pending
-- Makes a pass through the lists of Pending until it finds something nonempty
    -- R is a subalgebra
    -- maxDegree is an integer
lowestDegree = (compTable) -> (
    min keys compTable#"pending"
    )

-- Adds newGens to R.cache.SagbiGens. Updates the appropriate rings/maps in R.cache.SubalgComputations.
    -- R is of Type Subring
    -- newGens is a 1-row matrix of generators to be added
appendToBasis = (compTable, newGens) -> (
    compTable#"sagbiDegrees" = compTable#"sagbiDegrees" | matrix{flatten degrees source newGens};
    compTable#"sagbiGenerators" = compTable#"sagbiGenerators" | newGens;
    )

--Accepts a 1-row matrix inputMatrix and returns a matrix of columns of inputMatrix whose entries all have total degree less than maxDegree
submatBelowDegree = (inputMatrix,maxDegree) -> (
    selectedCols := positions(0..numcols inputMatrix - 1,
        i -> (degrees source inputMatrix)_i < {maxDegree});
    inputMatrix_selectedCols
    )

--Accepts a 1-row matrix inputMatrix and returns a matrix of columns of inputMatrix where the highest degree entry has total degree equal to currDegree
submatByDegree = (inputMatrix, currDegree) -> (
    selectedCols := positions(0..numcols inputMatrix - 1,
        i -> (degrees source inputMatrix)_i === {currDegree});
    inputMatrix_selectedCols
    )


-- Reduces the lowest degree in subalgComp#"Pending", updating subalgComp#"Pending" and subalgComp#"sagbiGB".
-- The various maps, tensor ring, and syzygy ideal are updated to reflect this change.
-- !!!Assumes that the pending list has been subducted!!!
   -- R is the subalgebra.
   -- maxDegree is the degree limit.
processPending = (compTable) -> (

    currentLowest := lowestDegree(compTable);
    
    if currentLowest != infinity then (
	
	-- remove redundant elements of the lowest degree in subalgComp#"Pending".
	reducedGenerators := gens gb(matrix{toList compTable#"pending"#currentLowest}, DegreeLimit=>currentLowest);
    	remove(compTable#"pending", currentLowest);
    	insertPending(compTable, reducedGenerators);
	
    	-- Find the lowest degree elements after reduction.
    	currentLowest = lowestDegree(compTable);
	if currentLowest != infinity then (
	    
    	    -- Add new generators to the basis
            appendToBasis(compTable, matrix{toList compTable#"pending"#currentLowest});
            remove(compTable#"pending", currentLowest);
	    );
    	);
    compTable#"presentation" = makePresRing(compTable#"ambientRing", compTable#"sagbiGenerators");
    currentLowest
    )

