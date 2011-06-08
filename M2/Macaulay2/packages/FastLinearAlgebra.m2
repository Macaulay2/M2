-- -*- coding: utf-8 -*-
newPackage(
	"FastLinearAlgebra",
	AuxiliaryFiles => false,
    	Version => "0.1",
    	Date => "May 12, 2011",
	Authors => {
	     {Name => "Michael E. Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	     {Name => "Jakob Kroeker", 
		  Email => "Jakob Kröker <kroeker@uni-math.gwdg.de>", 
		  HomePage => "" }
	     },
    	Headline => "Fast linear algebra over finite fields: interface to ffpack",
        DebuggingMode => true
    	)


-- also: rank, det, solve
export {
     RightSide,
     nullSpace,
     invert,
     rowRankProfile,
     columnRankProfile,
     addMultipleTo,
     solveLinear,
     TransposeA,
     TransposeB,
     Alpha,
     Beta
--     ,
--     columnRankProfile,
--     rowRankProfile
     }

debug Core

isPrimeField = method()
isPrimeField Ring := (R) -> (
     true
     )

determinant MutableMatrix := opts -> (M) -> (
     R := ring M;
     if isPrimeField R and char R > 0 then (
	  new R from rawFFPackDeterminant raw M
	  )
     )

rank MutableMatrix := (M) -> (
     R := ring M;
     if isPrimeField R and char R > 0 then (
	  new ZZ from rawFFPackRank raw M
	  )
     )

nullSpace = method(Options => {RightSide=>true})
nullSpace(MutableMatrix) := opts -> (M) -> (
     R := ring M;
     if isPrimeField R and char R > 0 then (
     	  map(R,rawFFPackNullSpace(raw M,opts.RightSide))
     ))

solveLinear = method(Options => options nullSpace)
solveLinear(MutableMatrix, MutableMatrix) := opts -> (A,B) -> (
     -- solve A*X = B, or solve X*A = B
     R := ring A;
     if ring A =!= ring B then error "expected same base rings";
     if isPrimeField R and char R > 0 then (
     	  map(R,rawFFPackSolve(raw A,raw B,opts.RightSide))
     ))

invert = method()
invert MutableMatrix := (A) -> (
     R := ring A;
     if isPrimeField R and char R > 0 then (
     	  map(R,rawFFPackInvert(raw A)))
     else 
     	  error "invert only implemented for mutable matrices over prime finite fields"
     )

addMultipleTo = method(Options => {TransposeA => false, TransposeB => false, Alpha => null, Beta => null})
addMultipleTo(MutableMatrix,MutableMatrix,MutableMatrix) := opts -> (C,A,B) -> (
     R := ring C;
     if ring A =!= ring C or ring B =!= ring C then error "expected matrices over the same ring";
     if isPrimeField R and char R > 0 then (
	  a := if opts.Alpha === null then 1_R else opts.Alpha;
	  b := if opts.Beta === null then 1_R else opts.Beta;
     	  rawFFPackAddMultipleTo(raw C, raw A, raw B,opts.TransposeA, opts.TransposeB, raw a, raw b);
	  C)
     )

MutableMatrix * MutableMatrix := (A,B) -> (
     C := mutableMatrix(ring A, numRows A, numColumns B, Dense=>true);
     addMultipleTo(C,A,B)
     )

rowRankProfile = method()
rowRankProfile MutableMatrix := (A) -> (
     R := ring A;
     if isPrimeField R and char R > 0 then (
     	  rawFFPackRowRankProfile A
     ))

columnRankProfile = method()
columnRankProfile MutableMatrix := (A) -> (
     R := ring A;
     if isPrimeField R and char R > 0 then (
     	  rawFFPackColumnRankProfile A
     ))

beginDocumentation()

TEST ///
kk = ZZ/101
A = random(kk^2, kk^4)
B = random(kk^5, kk^2)
M = mutableMatrix(B*A)
N = nullSpace M
assert((matrix M) * (matrix N) == 0)
N = nullSpace(M, RightSide=>false)
assert((matrix N) * (matrix M) == 0)
///

TEST ///
kk = ZZ/101
A = random(kk^23, kk^400)
B = random(kk^500, kk^23)
M = mutableMatrix(B*A);
N = nullSpace M;
assert(numRows N == numColumns M)
assert(numColumns N == numColumns M - 23)
assert((matrix M) * (matrix N) == 0)

time N = nullSpace(M, RightSide=>false);
assert(numRows N == numRows M - 23)
assert(numColumns N == numRows M)
assert((matrix N) * (matrix M) == 0)
///

TEST ///
kk = ZZ/101
A = random(kk^23, kk^1000);
B = random(kk^1000, kk^23);
M = mutableMatrix(B*A);
N = nullSpace M;
assert(numRows N == numColumns M)
assert(numColumns N == numColumns M - 23)
assert((matrix M) * (matrix N) == 0)

time N = nullSpace(M, RightSide=>false);
assert(numRows N == numRows M - 23)
assert(numColumns N == numRows M)
assert((matrix N) * (matrix M) == 0)
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^4, kk^4)
B = mutableMatrix random(kk^4, kk^1)
X = solveLinear(A,B)
(matrix A) * (matrix X) - matrix B
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^100, kk^100);
B = mutableMatrix random(kk^100, kk^5);
X = solveLinear(A,B) 
((matrix A) * (matrix X)) - matrix B
///

TEST ///
kk = ZZ/101
N = 10

time A = mutableMatrix(kk, N, N, Dense=>true);
time fillMatrix A;
time B = invert A;
time C = A*B;
C == mutableIdentity(kk, N)

idN = mutableIdentity(kk, N, Dense=>true);
time X = solveLinear(A, idN);
assert(B == X)
///


TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^3, kk^4);
B = mutableMatrix random(kk^4, kk^7);
A * B
(matrix A) * (matrix B) == matrix(A*B)
///

TEST ///
kk = ZZ/101
C = mutableMatrix random(kk^3, kk^7);
A = mutableMatrix random(kk^3, kk^4);
B = mutableMatrix random(kk^4, kk^7);

C0 = matrix C
addMultipleTo(C,A,B,Alpha=>3_kk, Beta=>-1_kk)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

C = mutableMatrix C0
addMultipleTo(C,mutableMatrix transpose matrix A, B,Alpha=>3_kk, Beta=>-1_kk, TransposeA=>true)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

C = mutableMatrix C0
addMultipleTo(C,A,mutableMatrix transpose matrix B,Alpha=>3_kk, Beta=>-1_kk, TransposeB=>true)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

C = mutableMatrix C0
addMultipleTo(C,mutableMatrix transpose matrix A,
     mutableMatrix transpose matrix B,
     Alpha=>3_kk, 
     Beta=>-1_kk, 
     TransposeB=>true, 
     TransposeA=>true)
assert(-C0 + 3*(matrix A)*(matrix B) == matrix C)

A * B
assert((matrix A) * (matrix B) == matrix(A*B))
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^300, kk^400);
B = mutableMatrix random(kk^400, kk^700);
time C1 = A * B;
time C2 = (matrix A) * (matrix B);
assert(matrix C1 == C2)
///

TEST ///
kk = ZZ/101
A = mutableMatrix random(kk^3, kk^4);
B = mutableMatrix random(kk^4, kk^7);
assert (try (B * A;false) else true)
time C2 = (matrix A) * (matrix B);
assert(matrix C1 == C2)
///


end

--load "ffpack-test.m2"
restart
loadPackage "FastLinearAlgebra"
kk = ZZ/101
N = 3

N = 4000
N = 100
M = mutableMatrix(kk, N, N, Dense=>true);
time fillMatrix M;
--viewHelp fillMatrix
M
time det M
time det matrix M
rank M
methods rank

A = random(kk^10, kk^31)
B = random(kk^45, kk^10)
C = B*A
D = mutableMatrix C
rank D
rank C

A = random(kk^10, kk^31)
B = random(kk^31, kk^10)
C = B*A
D = mutableMatrix C
det C
det D

restart
loadPackage "FastLinearAlgebra"



-- TODO:
--   top level M2 package (this file): calls the rawFFPack routines
--   interface.dd:  glue functions to call engine functions
--   engine.h: put in headers for new functions
--   x-mutablemat.cpp: 2 options:  (1) as we do now, just copy to/from ffpack matrices to do computation
--     (2) make a new MutableMatrix type in M2 engine
--   for new MutableMatrix type:
--      (a) subclass MutableMat<coeffRing, ...>
--      (b) include in MutableMatrix: the functions we want.
--      (c) write the matrix class.
-- Also, want GF(q).  Current problem: givaro didn't even compile.

-- invert2 DONE
-- test that with solve DONE
-- multiplication DONE
-- rank profiles (column and row) MOSTLY DONE: need to fix ffpack, test it
-- LU decomposition
-- submatrix + stride
-- row and col echelon forms
-- incorporate givaro types for all of these operations

-- after that:
--  make a new mutable matrix data type
--  

-- HW:
--  Mike:
--    think about the interface for ffpack matrices, so we do not need to copy matrices each operation
--  Jakob:
--    check with ffpack people about: row rank profile.  DONE

-- HW after 24 May 2001:
--   FIXED givaro seems to crash M2 when linked in (Jakob and Mike will both look at this).  Still does!
--   linbox: try to compile it in (maybe after givaro is working)
--   need: how to use givaro in the same way as for fflas-ffpack. (Where is doc and test code for this?  Jakob will send to Mike?)
--     converting elements?
--     what exactly is the field?  ZZ/p[a]/(g(a)).  What is g?
--   need: new snapshots of givaro and fflas-ffpack.  Dan does this? DONE
--   need: ask them about license?  Check with Dan.  ffpack: LGPL license.  givaro: LGPL? DONE: seems OK
--   ask Dan: look at the givaro license. DONE
--   Mike: in my GF ring code, allow givaro to decide the polynomial, and/or tell givaro what the poly is.
--    learn givaro myself before our next meeting, on Tuesday at 10am, July 7.
--    REALLY start thinking about design for placing these in as M2 types.
--   Jakob: play with givaro
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=FastLinearAlgebra all check-FastLinearAlgebra RemakeAllDocumentation=true RerunExamples=true RemakePackages=true"
-- End:
