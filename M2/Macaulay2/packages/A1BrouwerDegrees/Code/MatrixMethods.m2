-----------------------
-- Matrix manipulations
-----------------------

-- Input: A matrix
-- Output: True if the matrix is a square; false otherwise

isSquare = method()
isSquare (Matrix) := Boolean => M -> (
    numRows(M) == numColumns(M)
    )

-- Input: A matrix
-- Output: True if the matrix is a square and symmetric; false otherwise

isSquareAndSymmetric = method()
isSquareAndSymmetric (Matrix) := Boolean => M -> (
    transpose(M) == M
    )

-- Input: A matrix
-- Output: True if the matrix represents a degenerate bilinear form; false otherwise

isDegenerate = method()
isDegenerate (Matrix) := Boolean => M ->(
    det(M) == 0
    )

isNondegenerate = method()
isNondegenerate (Matrix) := Boolean => M ->(
    not isDegenerate(M)
    )

-- Input: A matrix
-- Output: True if the matrix is upper-left triangular, meaning that all the entries below the main antidiagonal are zero; false otherwise

isUpperLeftTriangular = method()
isUpperLeftTriangular (Matrix) := Boolean => M -> (
    if not isSquare(M) then error "Error: matrix isn't square";
    n := numRows(M);
    for i from 1 to n - 1 do(
	for j from 0 to i - 1 do(
    	-- If any entry in this range is nonzero then the matrix isn't upper left triangular
		if not M_(i,j) == 0 then(
		    return false
		    );
		
        );
    );
    true
    )
 
-- Input: A square matrix
-- Output: True if the matrix is diagonal; false otherwise

isDiagonal = method()
isDiagonal (Matrix) := Boolean => M -> (

    if not isSquare(M) then error "Error: matrix is not a square";

    n := numRows(M);
    
    for i from 0 to n-2 do(
	for j from  i+1 to n-1 do(
	    
	    -- Search in the matrix entries that aren't on diagonal
	    if i != j  then(
		
		-- If any entry off diagonal  is nonzero then the matrix isn't diagonal
		if  M_(i,j) != 0 or M_(j,i) != 0  then(
		    return false
		    );
		);
        );
    );
    true
    )

-- Input: A symmetric matrix
-- Output: A diagonal matrix congruent to the original matrix

congruenceDiagonalize = method()
congruenceDiagonalize (Matrix) := (Matrix) => (AnonMut) -> (
    k := ring AnonMut;
    if isField k == false then error "Error: expected matrix entries from a field";
    if not isSquareAndSymmetric(AnonMut) then error "matrix is not symmetric";
    
    -- If the matrix is already diagonal then return it
    if isDiagonal(AnonMut) == true then(
	return AnonMut
	);
    
    -- Otherwise we iterate through columns and rows under the diagonal, and perform row operations followed by the corresponding
    -- transpose operation on columns in order to reduce to a diagonal matrix congruent to the original
    A := mutableMatrix AnonMut;
    n := numRows(A);
    for col from 0 to (n - 1) do (
	-- If diagonal entry in column "col" is zero
        if A_(col,col) == 0 then (
            for row from col + 1 to n - 1 do ( 
		-- Scan for nonzero entries below the diagonal entry
                if A_(row,col) != 0 then (
                    if A_(row,row) == 0 then (
		        -- Row reduction to make A_(col,col) nonzero
                        rowAdd(A,col,1,row);
		        -- Column reduction to keep reduced matrix congruent to original matrix
                        columnAdd(A,col,1,row);
                        )
                    else (
		        -- Row and column swaps to make A_(col,col) nonzero
                -- Better alternative for keeping entries smaller in A in case A_(row,row)!=0
                        rowSwap(A,col,row);
                        columnSwap(A,col,row);
                        );
                    break;
                    );
                );
            );
        -- Now A_(col,col) != 0 unless there was a zero row/column and we use it to clear the column below
        if A_(col,col) != 0 then (
            for row from (col+1) to (n-1) do (
                temp:=A_(row,col);
                -- More row reduction make every entry below A_(col,col) is zero
                rowAdd(A,row,-temp/A_(col,col),col);
	        -- Column reduction to keep reduced matrix congruent
                columnAdd(A,row,-temp/A_(col,col),col);
                );
            );
        );
    return matrix A 
    )

-- Input: A symmetric matrix 
-- Output: A diagonal matrix congruent to the original matrix, with squares stripped out

congruenceDiagonalizeSimplify = method()
congruenceDiagonalizeSimplify (Matrix) := (Matrix) => (AnonMut) -> (
    k := ring AnonMut;
    if not (k === CC or instance(k,ComplexField) or k === RR or instance(k,RealField) or k === QQ or (instance(k, GaloisField) and k.char != 2)) then (
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
        );
    if not isSquareAndSymmetric(AnonMut) then error "matrix is not symmetric";

    diagForm := congruenceDiagonalize(AnonMut);
    A := mutableMatrix diagForm;
    n := numRows(A);

    -- If the field is the complex numbers, we can replace each nonzero entry of the diagonalization by 1
    if (k === CC or instance(k,ComplexField)) then (
	for i from 0 to (n-1) do (
	    if A_(i,i) != 0 then (
		A_(i,i) = 1;
		);
            );
	return (matrix A);
	)
    
    -- If the field is the real numbers, we can replace each positive entry of the diagonalization by 1 and each negative entry by -1
    else if (k === RR or instance(k,RealField)) then (
	for i from 0 to (n-1) do (
	    if A_(i,i) > 0 then (
		A_(i,i) = 1;
		);
	    if A_(i,i) < 0 then (
		A_(i,i) = -1;
		);
	    );
	return (matrix A);
	)

    -- If the field is the rational numbers, we can diagonalize and take squarefree parts
    else if (k === QQ) then (
	for i from 0 to (n-1) do (
            A_(i,i) = squarefreePart(A_(i,i));
	    );
	return (matrix A);
	)

    -- Over a finite field, we can diagonalize and replace every entry by 1 or a nonsquare representative
    else if (instance(k, GaloisField) and k.char != 2) then (
        nonSquareRep := sub(-1,k);
        if (legendreBoolean(sub(-1,k))) then (
	    for i from 0 to (n-1) do (
	        if (diagForm_(i,i) != 0 and (not legendreBoolean(diagForm_(i,i)))) then (
	     	    nonSquareRep = diagForm_(i,i);
                    break;
		    );
                );
	    );
	for i from 0 to (n-1) do (
	    if (A_(i,i) != 0 and legendreBoolean(A_(i,i))) then (
		A_(i,i) = 1;
		);
	    if (A_(i,i) != 0 and not legendreBoolean(A_(i,i))) then (
		A_(i,i) = nonSquareRep;
		);
	    );
	return (matrix A);
	)

    -- We should never get here
    else error "Problem with base field"
    )

-- Input: A symmetric matrix representing a quadratic form
-- Output: A diagonal matrix representing the nondegenerate part of the quadratic form

nondegeneratePartDiagonal = method()
nondegeneratePartDiagonal (Matrix) := (Matrix) => (A) -> (
    diagA := congruenceDiagonalize(A);
    i := 0;
    while (i < numRows(diagA)) do (
        if (diagA_(i,i) == 0) then (
            diagA = submatrix'(diagA,{i},{i});
            )
        else (
            i = i+1;
            );
        );
    return (diagA);
    )

-- Input: A symmetric matrix representing a quadratic form
-- Output: The dimension of the nondegenerate part of the quadratic form

nondegenerateDimension = method()
nondegenerateDimension (Matrix) := (ZZ) => (A) -> (
    return (numRows(nondegeneratePartDiagonal(A)));
    )
