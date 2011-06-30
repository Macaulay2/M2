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
restart
needsPackage "FastLinearAlgebra"
kk = ZZ/101
A = mutableMatrix random(kk^2, kk^2)
rank A

kk = GF(2^4)
A = mutableMatrix random(kk^2, kk^2)
rank A
///

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

-- HW after 24 May 2011:
--   FIXED givaro seems to crash M2 when linked in (Jakob and Mike will both look at this).  Still does! FIXED
--   linbox: try to compile it in (maybe after givaro is working) FIXED: but need to patch M2
--   need: how to use givaro in the same way as for fflas-ffpack. (Where is doc and test code for this?  Jakob will send to Mike?)
--     converting elements?
--     what exactly is the field?  ZZ/p[a]/(g(a)).  What is g?
--   need: new snapshots of givaro and fflas-ffpack.  Dan does this? DONE
--   need: ask them about license?  Check with Dan.  ffpack: LGPL license.  givaro: LGPL? DONE: seems OK
--   ask Dan: look at the givaro license. DONE
--   Mike: in my GF ring code, allow givaro to decide the polynomial, and/or tell givaro what the poly is.
--    learn givaro myself before our next meeting.
--    REALLY start thinking about design for placing these in as M2 types.
--   Jakob: play with givaro
--
-- HW after 15 June 2011:
--   Jakob needs to check valgrind on his givaro-mpir mix
--   Jakob: will check to see if linbox has a "better" solution for small vs large GF fields
--   Mike: will setup GF so that in the presence of givaro, the polynomial used
--     is chosen by givaro.
--     Questions/issues: 1. front end chooses the polynomial currently
--        2. We need to consider two cases: when the poly rep is via exponents vs via polynomials.
--         (see the wiki for how to check this)
--        3. Change the GF class to setup a givaro field once and for all.
--   
-- Plan made 30 June 2011, before Mike goes to Budapest:
--   A. class structure (rings and matrix types)
--   B. make sure top level interface to lin algebra routines is what we want
--   C. Connect ffpack, givaro, linbox to this interface (in B).
--   D. Want possibly other functions from linbox, or elsewhere
--   E. think about connecting Fgb of Faugere (but it is not open source)
--   Milestones:
--    1. Do (B): Top level interface and documentation for these functions.  For fast linear algebra functions
--       what is needed: FastLinearAlgebra package: have to document the functions.
--       thid package should also document: rank, det, ...  but it can't quite do that.  So we will need to change the
--       doc in M2 itself.  Need examples and tests.  Do this in July (in particular: the week of July 18).
--    2. Do (A). The current implementation of rings: each ring type will have a type from (A) as a member field
--      5 days of work?  Jakob: look at e/ring-test.hpp before August. 
--      Do this in August.  Finish this in August.  
--      Also do (or start)  (3), (4) in August.
--    3. Connect linear algebra over ZZ/p  (fflas-ffpack) (adding each ring type: should be easy code, but tests should be written, in FastLinearAlgebra)
--    4. Connect linear algebra over GF(q)  (givaro, or maybe linbox)
--    5. Connect linear algebra over ZZ or QQ (linbox).  This includes LLL, Smith normal form, and perhaps other normal forms.
--      finish (5) in September?
--    6. Want these linear algebra routines (det, solving, LU, ...) for approximate fields: RRR, CCC (i.e. arbitrary precision real and complex).
--       Other possible approx fields: interval, ball arithmetic. (use mathemagix?  Date for this one is unspecified).
--
--   Field  <-- what we have been using
--   FieldEnvelope <-- wrapper for Field for doing virtual function calls, also a template (takes a Field)
--   FieldAbstract <-- base for all of the FieldEnvelope's
--   FieldExample <-- example Field
--   Mike's plan: create a similar class structure
--    also: EngineTests package
--   matrices:
--     we are interested in the following routines:
--       rank
--       det
--       LU decomposition
--       solve AX=B, or XA = B
--       A*B
--         template<typename Ring, template Mat1, template Mat2> multiplyMatrices(const Mat1<Ring> &, const Mat2<Ring> &);
--         (Mat1, Mat2 could be DenseMatrix or SparseMatrix).
--       A += B*C
--       Smith normal form
--       maybe others
--     dmat.hpp: implement dense matrices over coeff rings.  These are templates.
--     matrix.hpp: use mutable matrices as the base. (these are immutable, graded matrices)
--   overall:
--    C++ organization of classes (like above)
--    top level interface to these functions (the user writing m2 code)
--      rank, det, A*B, solving interfaces should not need to change.
--      a few functions need to be added.
--
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=FastLinearAlgebra all check-FastLinearAlgebra RemakeAllDocumentation=true RerunExamples=true RemakePackages=true"
-- End:
