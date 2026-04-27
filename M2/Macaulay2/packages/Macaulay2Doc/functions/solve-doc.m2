doc ///
   Key
     solve
     (solve,Matrix,Matrix)
     (solve,MutableMatrix,MutableMatrix)
     [solve,MaximalRank]
     [solve,ClosestFit]
     [solve,Precision]
     [solve, Invertible] -- TODO: document this
   Headline
     solve linear equation(s)
   Usage
     X = solve(A,B)
   Inputs
     A:Matrix
       or @ofClass MutableMatrix@, of size m by n over a field R, which can be
       one of: @demark_", " \\ TT \ {"ZZ/p", "GF(p^n)", "QQ", "RR", "CC"}@
     B:
       of the same type of matrix as {\tt A}, over the same ring, of size m by r
     ClosestFit => Boolean 
       whether to use the least squares method, in the case when the ring is {\tt RR} or {\tt CC}
     MaximalRank => Boolean 
       declares to the system that the matrix is full rank.  In some cases, this can dramatically
       speed up the computation.  If the matrix is not full rank, then the results are potentially 
       meaningless.
   Outputs
     X:
       of the same type of matrix as {\tt A}, over the same ring, such that $AX=B$
   Description
     Example
       kk = ZZ/101;
       A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
       b = matrix"1;1;1" ** kk
       x = solve(A,b)
       A*x-b
     Example
       kk = GF(25)
       a = kk_0
       A = matrix"a,a+1,a+2,3a,4;a-1,1,2a,6,10;19,7,a,11,13" ** kk
       b = matrix"1;-a+1;1" ** kk
       x = solve(A,b)
       A*x-b
     Example
       kk = QQ
       A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
       b = matrix"1;1;1" ** kk
       x = solve(A,b)
       A*x-b
     Text

       Over {\tt RR_{53}} or {\tt CC_{53}}, if the matrix A is non-singular and square, 
       then highly optimized LAPACK routines will be called.

     Example
       printingPrecision = 4;
       A = matrix "1,2,3;1,3,6;19,7,11" ** RR
       b = matrix "1;1;1" ** RR
       x = solve(A,b)
       A*x-b
       norm oo
       clean(1e-15, A*x-b)
     Text

       If you know that your matrix is square, and invertible, then providing the
       hint: MaximalRank=>true allows Macaulay2 to choose the fastest routines.  For
       small matrix sizes, it should not be too noticeable, but for large matrices, the
       difference in time taken can be dramatic.

     Example
       printingPrecision = 4;
       N = 40
       A = mutableMatrix(CC_53, N, N); fillMatrix A;
       B = mutableMatrix(CC_53, N, 2); fillMatrix B;
       time X = solve(A,B);
       time X = solve(A,B, MaximalRank=>true);
       norm(A*X-B)
     Text

       Over higher precision RR or CC, these routines will be much slower than
       the lower precision LAPACK routines.

     Example
       N = 100
       A = mutableMatrix(CC_100, N, N); fillMatrix A;
       B = mutableMatrix(CC_100, N, 2); fillMatrix B;
       time X = solve(A,B);
       time X = solve(A,B, MaximalRank=>true);
       norm(A*X-B)
     Text

       Giving the option ClosestFit=>true, in the case when the field is RR or CC, 
       uses a least squares algorithm to find a best fit solution.

     Example
       kk = RR_53;
       A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
       b = matrix"1;1;1" ** kk
       x1 = solve(A,b, ClosestFit=>true)
       A*x1-b
     Text

       Giving both options ClosestFit and MaximalRank allows Macaulay2 to call
       a faster algorithm.

     Example
       x2 = solve(A,b, ClosestFit=>true, MaximalRank=>true)
       A*x2-b
   Caveat
     (1) This function is limited in scope, but has been designed to be much faster than
     generic algorithms.
     (2) If the matrix is a square invertible matrix, giving the option MaximalRank=>true can 
     strongly speed up the computation. 
     (3) For mutable matrices, this function is only currently implemented for densely encoded matrices.
   SeeAlso
     LUdecomposition
     inverse
     determinant
     SVD
     norm
     clean
///

-*
document { 
     Key => {
         solve,(solve,Matrix,Matrix),
       (solve,MutableMatrix,MutableMatrix),
         [solve,MaximalRank],
         [solve,ClosestFit]
    },
     Headline => "solve a linear equation",
     Usage => "x = solve(A,b)",
     Inputs => {
    "A" => {ofClass Matrix, ", or ", ofClass MutableMatrix, " of size m by n over either
    a finite field ZZ/p, RR or CC"},
    "b" => {"the same type of matrix, over the same ring, of size m by r"},
    ClosestFit => Boolean => {"whether to use the least squares method"},
    MaximalRank => Boolean => {"whether to assume the matrix has maximal rank, in case the least squares method is used"}
    },
     Outputs => {
    "x" => {"the same type of matrix, over the same ring, of size n by r,
         such that ", TT "Ax=b"}
    },
     PARA {
    "(Disambiguation: for division of matrices, which can also be thought of as solving a
    system of linear equations, see instead ", TO (symbol //,Matrix, Matrix), ".  For lifting a map between modules to a map 
    between their free resolutions, see ", TO extend, ".)"
    },
     "There are several restrictions.  The first is that there are only a limited number of rings
     for which this function is implemented.  Second, over ", TO "RR", " or ", TO "CC", 
     ", the matrix ", TT "A", " must be a square
     non-singular matrix.  Third, if ", TT "A", " and ", TT "b", 
     " are mutable matrices over ", TO "RR", " or ", TO "CC", ", they must be dense matrices.",
     EXAMPLE lines ///
        kk = ZZ/101;
        A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
    b = matrix"1;1;1" ** kk
    x = solve(A,b)
    A*x-b
     ///,
     "Over ", TO "RR", " or ", TO "CC", ", the matrix ", TT "A", " must be a non-singular square matrix.",
     EXAMPLE lines ///
        printingPrecision = 2;
        A = matrix "1,2,3;1,3,6;19,7,11" ** RR
    b = matrix "1;1;1" ** RR
    x = solve(A,b)
    A*x-b
    norm oo
     ///,
     "For large dense matrices over ", TO "RR", " or ", TO "CC", ", this function calls 
     the LAPACK routines.",
     EXAMPLE lines ///
        n = 10;
    A = random(CC^n,CC^n)
    b = random(CC^n,CC^2)
    x = solve(A,b)
    norm ( matrix A * matrix x - matrix b )
     ///,
     "This may be used to invert a matrix over ", TT "ZZ/p", ", ", TO "RR", " or ", TT "QQ", ".",
     EXAMPLE lines ///
          A = random(RR^5, RR^5)
    I = id_(target A)
    A' = solve(A,I)
    norm(A*A' - I)
    norm(A'*A - I)
        ///,
     "Another method, which isn't generally as fast, and isn't as stable over ", TO "RR", " or ", TO "CC", ", 
     is to lift the matrix ", TT "b", "
     along the matrix ", TT "A", " (see ", TO (symbol//,Matrix,Matrix), ").",
     EXAMPLE lines ///
          A'' = I // A
    norm(A' - A'')
          ///,
     Caveat => {"This function is limited in scope, but is sometimes useful for very large 
    matrices"},
     SeeAlso => {LUdecomposition, SVD, MutableMatrix, norm, random}
     }
*-
