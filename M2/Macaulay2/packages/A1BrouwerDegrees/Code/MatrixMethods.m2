-----------------------
-- Matrix manipulations
-----------------------

-- Input: A matrix
-- Output: Boolean that gives whether the matrix is square

isSquare = method()
isSquare Matrix := Boolean => M -> (
    numRows(M) == numColumns(M)
    )

-- Input: A matrix
-- Output: Boolean that gives whether the matrix is square and symmetric

isSquareAndSymmetric = method()
isSquareAndSymmetric Matrix := Boolean => M -> (
    transpose(M) == M
    )

-- Input: A symmetric matrix
-- Output: Boolean that gives whether the matrix represents a degenerate bilinear form

isDegenerate = method()
isDegenerate Matrix := Boolean => M -> (
    if not isSquareAndSymmetric M then error "matrix is not symmetric";
    if numRows(M) == 0 then (
	return false;
	)
    else
	return det(M) == 0;
    )

-- Input: A symmetric matrix
-- Output: Boolean that gives whether the matrix represents a nondegenerate bilinear form

isNondegenerate = method()
isNondegenerate Matrix := Boolean => M -> (
    not isDegenerate M
    )
 
-- Input: A square matrix
-- Output: Boolean that gives whether the matrix is diagonal

isDiagonal = method()
isDiagonal Matrix := Boolean => M -> (

    if not isSquare M then error "matrix is not a square";

    n := numRows M;
    -- Check the matrix entries that aren't on the diagonal
    for i from 0 to n - 2 do (
	for j from  i + 1 to n - 1 do
	    -- If any off-diagonal entry is nonzero, then the matrix isn't diagonal
	    if  M_(i,j) != 0 or M_(j,i) != 0 then return false;
        );
    -- Otherwise, the matrix is diagonal
    true
    )

-- Input: A symmetric matrix over a field
-- Output: A diagonal matrix congruent to the original matrix

diagonalizeViaCongruence = method()
diagonalizeViaCongruence Matrix := Matrix => AnonMut -> (
    k := ring AnonMut;
    if not isField k then error "expected matrix over a field";
    if not isSquareAndSymmetric AnonMut then
	error "matrix is not symmetric";
    
    -- If the matrix is already diagonal, then return it
    if isDiagonal AnonMut then return AnonMut;
    
    -- Otherwise, we iterate through positions below the diagonal, performing row operations followed by the corresponding
    -- column operations in order to obtain a diagonal matrix congruent to the original
    A := mutableMatrix AnonMut;
    n := numRows A;
    for col from 0 to n - 1 do (
	-- If diagonal entry in column "col" is zero
        if A_(col,col) == 0 then (
            for row from col + 1 to n - 1 do ( 
		-- Scan for nonzero entries in column "col" below the diagonal entry
                if A_(row,col) != 0 then (
                    if A_(row,row) == 0 then (
		        -- Row operation to make A_(col,col) nonzero
                        rowAdd(A, col,1,row);
		        -- Column operation to keep reduced matrix congruent to original matrix
                        columnAdd(A, col,1,row);
                        )
                    else (
		        -- Row and column swaps to make A_(col,col) nonzero
                        rowSwap(A, col,row);
                        columnSwap(A, col,row);
                        );
                    break;
                    );
                );
            );
        -- Now A_(col,col) != 0 unless there was a zero row/column; we use it to clear column "col" below this entry
        if A_(col,col) != 0 then (
            for row from col + 1 to n - 1 do (
                temp := A_(row,col);
                -- Row operation to make A_(row,col) zero
                rowAdd(A, row, -temp/A_(col,col), col);
	        -- Column operation to keep reduced matrix congruent
                columnAdd(A, row, -temp/A_(col,col), col);
                );
            );
        );
    matrix A 
    )

-- Input: A symmetric matrix over QQ, RR, CC, and finite fields of characteristic not 2
-- Output: A diagonal matrix congruent to the original matrix, with squarefree entries on the diagonal

diagonalizeAndSimplifyViaCongruence = method()
diagonalizeAndSimplifyViaCongruence Matrix := Matrix => AnonMut -> (
    k := ring AnonMut;
    if not (instance(k, ComplexField) or instance(k, RealField) or k === QQ or (instance(k, GaloisField) and k.char != 2)) then (
        error "Base field not supported; only implemented over QQ, RR, CC, and finite fields of characteristic not 2";
        );
    if not isSquareAndSymmetric AnonMut then error "matrix is not symmetric";

    A := mutableMatrix diagonalizeViaCongruence AnonMut;
    n := numRows A;

    -- If the field is the complex numbers, replace each nonzero entry of the diagonalization by 1
    if instance(k, ComplexField) then (
	for i from 0 to n - 1 do (
	    if A_(i,i) != 0 then A_(i,i) = 1;
            );
	)
    
    -- If the field is the real numbers, replace each positive entry of the diagonalization by 1 and each negative entry by -1
    else if instance(k, RealField) then (
	for i from 0 to n - 1 do (
	    if A_(i,i) > 0 then A_(i,i) = 1;
	    if A_(i,i) < 0 then A_(i,i) = -1;
	    );
	)

    -- If the field is the rational numbers, replace each diagonal entry by its squarefree part
    else if k === QQ then (
	for i from 0 to n - 1 do
            A_(i,i) = getSquarefreePart A_(i,i);
	)

    -- Over a finite field, replace each diagonal entry by 1 or a nonsquare representative
    else if (instance(k, GaloisField) and k.char != 2) then (
        -- Initially let the nonsquare representative be -1
        nonSquareRep := sub(-1, k);
        -- If -1 is a square, then find another nonsquare representative
        if isGFSquare sub(-1, k) then (
	    for i from 0 to n - 1 do (
	        if (A_(i,i) != 0 and not isGFSquare(A_(i,i))) then (
                    -- If there is a nonsquare on the diagonal, choose it as the nonsquare representative
	     	    nonSquareRep = A_(i,i);
                    break;
		    );
                );
	    );
	for i from 0 to n - 1 do (
	    if (A_(i,i) != 0 and isGFSquare(A_(i,i))) then
		A_(i,i) = 1;
	    if (A_(i,i) != 0 and not isGFSquare(A_(i,i))) then
		A_(i,i) = nonSquareRep;
	    );
	);
    matrix A
    )

-- Input: A matrix representing a symmetric bilinear form
-- Output: A diagonal matrix representing the nondegenerate part of the symmetric bilinear form

getNondegeneratePartDiagonal = method()
getNondegeneratePartDiagonal Matrix := Matrix => A -> (
    diagA := diagonalizeViaCongruence A;
    i := 0;
    while i < numRows(diagA) do (
        if diagA_(i,i) == 0 then (
            diagA = submatrix'(diagA, {i},{i});
            )
        else
            i = i+1;
        );
    diagA
    )
