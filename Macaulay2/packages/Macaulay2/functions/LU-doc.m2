document { 
     Key => {LU,(LU,MutableMatrix),(LU,Matrix)},
     Headline => "LU decomposition",
     Usage => "(P,L,U) = LU A",
     Inputs => {
	  "A" => MutableMatrix => {"or ", ofClass Matrix, ",of size ", TT "m", " by ", TT "n", ", over a 
	  a finite field ", TT "ZZ/p", ", ", TO "RR", " or ", TO "CC", ""}
	  },
     Outputs => {
	  "P" => List => {"of integers in [0, ..., m-1] forming a permutation"},
	  "L" => MutableMatrix => {"or ", ofClass Matrix, ", an m by min(m,n) lower 
	            triangular matrix, with 1's on the diagonal"},
	  "U" => MutableMatrix => {"or ", ofClass Matrix, ", a min(m,n) by n upper triangular matrix"}
	  },
     "The output matrices are mutable exactly when the input matrix is.",
     PARA{},
     "If ", TT "Q", " is the ", TT "m", " by ", TT "m", " permutation matrix 
     such that ", TT "Q_(P_i,i) = 1", ", and all other entries are zero,
     then ", TT "A = QLU", ".  The matrix ", TT "A", " is not modified",
     PARA{},
     "There are several restrictions.  The first is that there are only a limited number of rings
     for which this function is implemented. Second, if ", TT "A", " is a mutable matrix ",
     " defined over ", TT "RR", " or ", TT "CC", ", then ", TT "A", " must be densely encoded.  This
     restriction is not present if ", TT "A", " is ", ofClass Matrix, ".",
     PARA{},
     EXAMPLE lines ///
     	  kk = ZZ/101;
     	  A = matrix"1,2,3,4;1,3,6,10;19,7,11,13" ** kk
	  A = mutableMatrix A
	  (P,L,U) = LU A
	  matrix L * matrix U == matrix A
     ///,
     "Over ", TT "RR", " or ", TT "CC", ", the matrix ", TT "A", " must be densely encoded (which is the default).",
     EXAMPLE lines ///
     	  kk = RR
     	  A = matrix"1,2,3,4,5,6;1,3,6,12,13,16;19,7,11,47,48,21" ** kk
	  A = mutableMatrix(A, Dense=>true)
	  (P,L,U) = LU A
	  Q = mutableMatrix(kk, numrows A, numrows A)
	  for i from 0 to numrows A - 1 do Q_(P_i,i) = 1.0
	  matrix Q * matrix L * matrix U == matrix A
     ///,
     "For matrices over ", TT "RR", " or ", TT "CC", ", this function calls 
     the lapack routines, which are restricted to 53 bits of precision.",
     EXAMPLE lines ///
	  A = mutableMatrix(CC,5,10, Dense=>true)
	  printingPrecision = 2
	  setRandomSeed 0
	  fillMatrix A
	  (P,L,U) = LU A;
	  Q = mutableMatrix(kk, numrows A, numrows A)
	  for i from 0 to numrows A - 1 do Q_(P_i,i) = 1.0
	  clean(1e-15,matrix Q * matrix L * matrix U - matrix A)
     ///,
     Caveat => {"This function is limited in scope, but is sometimes useful for very large 
	  matrices"},
     SeeAlso => {solve, SVD, MutableMatrix, fillMatrix, clean, norm}
     }

TEST ///
kk = RR
A = matrix"1,2,3;4,7,8" ** kk
(P,L,U) = LU A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0
Q = matrix Q
norm(Q*L*U - A)

kk = RR
A = matrix"1,2;3,4;7,8" ** kk
(P,L,U) = LU A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0
Q = matrix Q
norm(Q*L*U - A)

kk = CC
A = matrix"1,2,3;4,7,8" ** kk
(P,L,U) = LU A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0+0.0*ii
Q = matrix Q
norm(Q*L*U - A)

kk = CC
A = matrix"1,2;3,4;7,8" ** kk
(P,L,U) = LU A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0+0.0*ii
Q = matrix Q
norm(Q*L*U - A)

-- For version 1.0
kk = RR
A = matrix"1,2,3;4,7,8" ** kk
A = mutableMatrix A
(P,L,U) = LU A
Q = mutableZero(kk, #P, #P)
for i from 0 to #P-1 do Q_(i,P_i) = 1.0
Q = matrix Q
Q*(matrix L)*(matrix U) - matrix A  == 0
-- For version 1.0
kk = CC
A = matrix"1,2,3;4,7,8" ** RR
A = matrix{{ii,2.0,3.0},{4.0,7.0*ii,8.0}}
A = mutableMatrix A
(P,L,U) = LU A
Q = mutableZero(kk, #P, #P)
for i from 0 to #P-1 do Q_(i,P_i) = 1.0*ii
Q = matrix Q
Q*(matrix L)*(matrix U)
matrix A


///