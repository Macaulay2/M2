doc ///
    Key
        reducedRowEchelonForm
        (reducedRowEchelonForm, Matrix)
        (reducedRowEchelonForm, MutableMatrix)
    Headline
        compute the reduced row echelon form of a matrix or mutable matrix over a field
    Usage
        R = reducedRowEchelonForm A
    Inputs
        A:Matrix
            or @ofClass MutableMatrix@, a matrix over eiither a finite field or the rationals
    Outputs
        R:Matrix
            or @ofClass MutableMatrix@, the same type as {\tt A}, the 
            reduced row echelon form of {\tt A}
    Description
        Text
            A matrix over a field is in reduced row echelon form
            if (1) all rows consisting only of zeros are at the bottom,
            (2) the leading coefficient  (called the pivot) of a nonzero row
            is always strictly to the right of the leading coefficient 
            of the row above it, and (3) the leading coefficient of each nonzero row is
            1.  See @wikipedia "Row echelon form"@ for more discussion.

            Given a matrix {\tt A} (over a field), there is a unique matrix {\tt R}
            such that (1) the row spans of {\tt A} and {\tt R} are the same, and 
            (2) {\tt R} is in reduced row echelon form.

            If the matrix {\tt A} is over either a finite field, or
            the rationals, then this returns the unique reduced row
            echelon form for the matrix, which is unique.
        Example
            R0 = matrix(QQ, {{1,0,3,0,5},{0,1,-2,0,2},{0,0,0,1,7}})
            B = matrix(QQ, {{1,2,3},{0,3,-1},{1,2,0}})
            A = B * R0
            R = reducedRowEchelonForm A
            assert(R == R0)
            LUdecomposition A
        Example
            A5 = sub(A, ZZ/5)
            reducedRowEchelonForm A5
            rank A5
        Text
            This function is useful while learning the concepts and
            algorithms of linear algebra.  In practice though, one tends to
            use LU decompositions rather than reduced row echelon forms.
            
            In fact, this function doesn't work over the reals or
            complexes (due to often poor numerical stability), as an LU
            decomposition is better for solving systems over the real
            numbers.  If numerical stability is an issue, using a
            singular value decomposition (SVD) is also a good idea.
        Example
            AR = sub(A, RR)
            LUdecomposition AR
            SVD AR
        Example
            AC = sub(A, CC)
            LUdecomposition AC
            SVD AC
        Text
            This function also works over finite fields.
        Example
            A9 = sub(A, GF 9)
            R9 = reducedRowEchelonForm A9
        Text
            It is not yet
            implemented over fraction fields and extension fields.
            Use @TO "LUdecomposition"@ instead.
        Example
            S = frac(QQ[x])
            R = matrix(S, {{1,0,x,0,x^2+1},{0,1,-1/x,0,2*x},{0,0,0,1,7*x}})
            B = matrix(S, {{1,2*x,3},{0,3,-x},{1,2*x^2,0}})
            A = B * R
            LUdecomposition A
        Text
            Even though it is not implemented currently as a canned
            routine, we can put {\tt A} into reduced row echelon form
            using elementary row operations.  Recall that rows and
            columns in Macaulay2 are indexed starting with 0.
        Example
            M = mutableMatrix A
            rowAdd(M, 2, -1, 0)
            rowMult(M, 1, 1/M_(1,1))
            rowAdd(M, 2, -M_(2,1), 1)
            rowMult(M, 2, 1/M_(2,3))
        Text
            At this point the matrix is in row echelon form.
            We clear out the entries above the pivots to
            place it into reduced row echelon form.
        Example
            rowAdd(M, 0, -M_(0,1), 1)
            rowAdd(M, 0, -M_(0,3), 2)
            rowAdd(M, 1, -M_(1,3), 2)
            assert(R == matrix M)
    SeeAlso
        rowAdd
        rowMult
        rowPermute
        rowSwap
        LUdecomposition
        SVD
        (rank, Matrix)
///
