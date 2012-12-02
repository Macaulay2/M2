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
   testDivide,
    testAdd,
    testNegate,
    testSubtract,
    testMultiply,
    testAxioms,    
    constructGivaroField,
    constructMacaulayGF,
    constructMacaulayZZp,
    testField,
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

------------tests

testEqual = method();
testEqual (MutableHashTable, ZZ) := (fieldHashTable, nrtests)->
(
    --todo
    -- require random function.
)
testNegate = method();
testNegate (MutableHashTable, ZZ) := (fieldHashTable,nrtests)->
(
    galoisField := fieldHashTable#"field";
    fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    apply(nrtests,i->(  rnd:=random cardinality; 
                        assert( fieldGen^rnd + (- fieldGen^rnd) == 0_galoisField );
                    ) 
        );
)


--depends on correct testNegate.
testAdd = method();
testAdd (MutableHashTable, ZZ) := (fieldHashTable, nrtests )->
(
    galoisField := fieldHashTable#"field";
    fieldGen := galoisField_0;
    cardinality := fieldHashTable#"cardinality";
    apply( nrtests, i->( rnd1:=random cardinality; 
                         rnd2:=random cardinality; 
                         assert( fieldGen^rnd1 + fieldGen^rnd2 + (-(fieldGen)^rnd1) == fieldGen^rnd2);
                    ) 
        );
)


testSubtract = method();
testSubtract (MutableHashTable, ZZ) := (fieldHashTable, nrtests)->
(
   galoisField := fieldHashTable#"field";
  fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    apply( nrtests,i->( rnd1 := random cardinality;   
                        rnd2 := random cardinality; 
                        assert( fieldGen^rnd1 + fieldGen^rnd2 - fieldGen^rnd1 == fieldGen^rnd2);) 
        );
)

-- relies on correct pow and comparison
testMultiply = method();
testMultiply (MutableHashTable, ZZ) :=(fieldHashTable,nrtests)->
(
    galoisField := fieldHashTable#"field";
      fieldGen := fieldHashTable#"generator";
    Zero := 0_galoisField;
     cardinality := fieldHashTable#"cardinality";
    characteristic := fieldHashTable#"characteristic";
    apply( nrtests, i->( rnd1:=random cardinality;  assert( fieldGen^rnd1*Zero == Zero );) );
    --
    apply(nrtests, i->( rnd1:=random characteristic;
                        rnd2:=random characteristic; 
                        addresult := Zero; 
                        apply(rnd1,j->addresult = addresult + fieldGen^rnd2); 
                        assert(fieldGen^rnd2* rnd1_galoisField == addresult);) 
        );
)

-- relies on correct pow and comparison
testAxioms = method();
testAxioms (MutableHashTable, ZZ) := (fieldHashTable, nrtests)->
(
    galoisField := fieldHashTable#"field";
   fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    Zero := 0_galoisField;
    apply( nrtests, i->( rnd1:=random cardinality;  assert( fieldGen^rnd1*Zero == Zero );) );
    -- test commutative 
    apply( nrtests, i->(      rnd1:=random cardinality;
                            rnd2 := random cardinality; 
                            assert( fieldGen^rnd1 * fieldGen^rnd2 == fieldGen^rnd2 * fieldGen^rnd1);
                        assert( fieldGen^rnd1 + fieldGen^rnd2 == fieldGen^rnd2 + fieldGen^rnd1);
                    ) 
    );
    --
    -- test associative and distributive
    apply( nrtests, i->(      rnd1:=random cardinality;
                            rnd2 := random cardinality; 
                            rnd3 := random cardinality;
                            -- test associative
                            assert( (fieldGen^rnd1 * fieldGen^rnd2) * fieldGen^rnd3 == fieldGen^rnd2 * (fieldGen^rnd1 * fieldGen^rnd3) );
                            assert( (fieldGen^rnd1 + fieldGen^rnd2) + fieldGen^rnd3 == fieldGen^rnd2 + (fieldGen^rnd1 + fieldGen^rnd3) );
                            -- test distributive
                            assert( (fieldGen^rnd1 + fieldGen^rnd2) * fieldGen^rnd3 == fieldGen^rnd1 * fieldGen^rnd3  + fieldGen^rnd2 * fieldGen^rnd3 );
                    ) 
    );
)

--relies on correct multiplication and comparison
testDivide = method();
testDivide (MutableHashTable, ZZ) := ( fieldHashTable, nrtests )->
(
    galoisField := fieldHashTable#"field";
     fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    One := 1_galoisField;
    apply( nrtests, i->(    rnd1 := random cardinality;
                            assert(( One // fieldGen^rnd1) * fieldGen^rnd1 == One );
                       )
        );
    apply( cardinality, i->(  assert(( One // fieldGen^i )*fieldGen^i == One);) );
    --
    apply( nrtests, i->(    rnd1 := random cardinality; 
                            rnd2 := random cardinality; 
                            assert( (fieldGen^rnd2 // fieldGen^rnd1)*fieldGen^rnd1 == fieldGen^rnd2);
                    ) 
        );
)


testPower = method();
testPower (MutableHashTable) := (fieldHashTable)->
(
    galoisField := fieldHashTable#"field";
  fieldGen := fieldHashTable#"generator";
    cardinality := fieldHashTable#"cardinality";
    apply( cardinality-2 , i-> (print (fieldGen^(i+2)); assert( fieldGen^(i+2) != fieldGen)));
    
    assert( fieldGen^cardinality == fieldGen);
    One := 1_galoisField; 
    assert( fieldGen^(cardinality-1)   == One);
    assert( fieldGen^(-2) * (fieldGen^2) == One );
    assert( fieldGen^(-1) * fieldGen   == One );
    -- overflow test is not possible here because of big memory consumption...
    -- idea for overflow test: suppose elem= generator^k.  Then elem^m  is implemented as 
    --                         generator^(k*m mod cardinality). If k*m implementation overflows this would result in an error.
    --
    --B := rawARingGaloisField(30013,2);
    --c:= (B_0)^100040;
    --c^100040;
    --ZZP=ZZ/997;
)

constructGivaroField = method();
constructGivaroField (ZZ,ZZ) := (characteristic,dimension)->
(
    result := new MutableHashTable;
    --
    result#"field" = rawARingGaloisField(characteristic, dimension );
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;
    result#"generator" = (result#"field")_0;
    assert( result#"generator"!=0_(result#"field"));
    return result;
)
constructMacaulayGF = method();
constructMacaulayGF (ZZ,ZZ) := (characteristic,dimension)->
(
    result := new MutableHashTable;
    --
    result#"field" = GF(characteristic, dimension );
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;
    result#"generator" = (result#"field")_0;
    assert( result#"generator"!=0_(result#"field"));
    return result;
)
constructMacaulayZZp = method();
constructMacaulayZZp (ZZ,ZZ) := (characteristic,dimension)->
(
    result := new MutableHashTable;
    --
    result#"field" = ZZ/characteristic;
    result#"char" =  characteristic;
    result#"characteristic" =  characteristic;
    result#"dimension" =  dimension;
    result#"cardinality" =  characteristic^dimension;
    --todo:
    tmp:=GF(characteristic,1);
    result#"generator"= (gens tmp)_0;
    return result;
)

testField = method();
testField (MutableHashTable,ZZ) := (fieldHashTable,nrTests )->
(
    assert( fieldHashTable#"generator" != 0_(fieldHashTable#"field"));
    assert( fieldHashTable#"generator" != 1_(fieldHashTable#"field"));
    --testEqual(fieldHashTable, nrTests);
    testPower(fieldHashTable);
    testDivide(fieldHashTable, nrTests);
    testAdd(fieldHashTable,    nrTests);
    testNegate(fieldHashTable, nrTests);
    testSubtract(fieldHashTable, nrTests);
    testMultiply(fieldHashTable, nrTests);
    testAxioms(fieldHashTable, nrTests);
);



beginDocumentation()


TEST ///
needsPackage "FastLinearAlgebra"
debug Core


 

nrTests = 100; -- may depend on cardinality, e.g. 1 % or so
fieldHashTable = constructGivaroField( 37,1 );
testField(fieldHashTable, nrTests);

nrTests = 100; -- may depend on cardinality, e.g. 1 % or so
fieldHashTable = constructGivaroField( 5,3 );
testField(fieldHashTable, nrTests);

nrTests = 100; -- may depend on cardinality, e.g. 1 % or so
fieldHashTable = constructGivaroField( 11,2 );
testField(fieldHashTable, nrTests);

fieldHashTable = constructMacaulayGF( 5,3 );
testField(fieldHashTable, nrTests);

fieldHashTable = constructMacaulayZZp(23,1);
testField(fieldHashTable, nrTests);


///



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
time C1 = A * B;
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
-- TODO notes made 14 Oct 2011.
--   - Jakob: doxygen: to do tags, make help, place documentation directory "elsewhere", handle options for generating class info DONE
--   - Mike: recompile all stuff, and retest everything -- we recall some test not working. CONTINUAL
--   - goal: have fast linear algebra over finite fields, and possibly other fields too.
--      problem: connect this seamlessly into M2
--   - we will add in calls that operate on mutables matrices.
--   -- design#1: all these interface files will be in Mutable matrices, and in DMat<CR>, SMat<CR>, ...
--   -- ring work:
--      (a) ZZ/p: M2 version, and the ffpack version
--      (b) GF: M2 version, givaro version
--   -- after this: incorporate more rings for these operations: ZZ, QQ, ...
--   -- Mike: add an include file to each cpp file: intent: ASSERT macro. TODO
--   -- SO:
--     (1) x-mutablemat.cpp: need the interface routines to front end (rank,det, ...)
--     (2) also: add in the corresponding interface routines in the d directory
--     (3) MutableMatrix, DMat, SMat: define class methods for these linear algebra routines
--          note: implementation type of DMat, SMat will use ARing data structures, e.g. ARingGF::ElementType
--          then: if entries are required, or other M2 matrices are required, these values will be translated to 
--           "ringelem" types.  (Later: maybe the "ringelem" type will be exactly these ElementType's.)
--          note: this will mean that we do not need to translate matrices before calling the appropriate functions
--            in ffpack/lapack/givaro/etc.
--     (4) write tests for these functions, as well as arithmetic, in the FastLinearAlgebra, EngineTests packages.
--     (5) document exactly what is required for ring types (e.g. what classes/types/methods are required), in order
--         to instantiate them with DMat, SMat.
--
-- TODO notes 19 Jan 2012
--    a. get latest changes of Jakob working (Givaro problem Jakob TODO)
--    b. merge in trunk changes (Mike TODO)
--    c. possibly: merge out changes back to the trunk, delete the branch, and create a new branch. (ask Dan for read/write privs for Jakob on new branch)
--    d. put x-mutablemat routines (fast linear algebra) into dmat.  Organize dispatch in x-mutablemat.
--    e. tests: for matrices/elements for all of our new ring types (ffpack ZZ/p, givaro GF, ...)
--    f. have toplevel GF have an option to call new GF code, same with ZZ/p.
--       issues: at least have a toplevel M2 function which makes new GF code "first class".
--    g. make sure that new GF and ZZ/p code allow to change rings (ring map code, promote, lift)  (e.g. GF(p^2) --> GF(p^4))
--    h. TODO: Mike: what is the global picture in the e directory, especially rings.
--       Mike: document this.  Is it @brief?  Put this into ring.hpp (seems like a good spot).
--         Ring
--         CoefficientRing
--         ARing
--    i. we have ffpack/givaro/linbox field types.
--       it would be nice:  ConcreteFromLinbox<FieldType> this will be usable in M2 as an ARing type.
--                          ConcreteRing<ARingType> This is a "Ring" in M2.
--                          ConcretePolynomialRing<APolyRingType>
--                          ConcreteField<AFieldType>
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=FastLinearAlgebra all check-FastLinearAlgebra RemakeAllDocumentation=true RerunExamples=true RemakePackages=true"
-- End:
