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
     solveLinear
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
     	  rawFFPackAddMultipleTo(C,A,B,opts.TransposeA, opts.TransposeB, a, b);
	  map(R, C))
     )

MutableMatrix * MutableMatrix := (A,B) -> (
     C := mutableMatrix(ring A, numRows A, numColumns B, Dense=>true);
     addMultipleTo(C,A,B)
     )

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
N = 5

time A = mutableMatrix(kk, N, N, Dense=>true);
time fillMatrix A;
time B = invert A;
--((matrix A) * (matrix B)) - 1 == 0

idN = mutableIdentity(kk, N, Dense=>true);
time X = solveLinear(A, idN);
assert(B == X)
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
-- submatrix + stride
-- rank profiles (column and row)
-- LU decomposition
-- multiplication

-- after that:
--  make a new mutable matrix data type
--  

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=FastLinearAlgebra all check-FastLinearAlgebra RemakeAllDocumentation=true RerunExamples=true RemakePackages=true"
-- End:
