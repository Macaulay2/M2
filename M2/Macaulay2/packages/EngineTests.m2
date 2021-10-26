newPackage(
        "EngineTests",
    	AuxiliaryFiles => true,
        Version => "0.1", 
        Date => "29 Aug 2011",
	    Authors => {
            {Name => "Michael E. Stillman", 
		        Email => "mike@math.cornell.edu", 
		        HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	        {Name => "Jakob Kröker", 
		        Email => "kroeker@math.uni-hannover.de", 
		        HomePage => "" }
            },
        Headline => "a test suite for the Macaulay2 engine",
	Keywords => {"Miscellaneous"},
        AuxiliaryFiles=> true
        )

export { 
   "runTests",
   "testNorm",
   "testClean",
   "hasFFPACK",
   "fields",
   "finitefields",
   "fieldsFFPACK",
   "fieldsFLINT",
   "fieldsGivaro",
   "fieldsGF",
   "ringsPID",
   "ringsRR",
   "ringsCC"
   }

debug Core

hasFFPACK = try (ZZp(101, Strategy=>"FFPACK"); true) else false;

fieldsFLINT = {
    "ZZpFlint 2",
    "ZZpFlint 5",
    "ZZpFlint 101",
    "ZZpFlint 4294967291"		  -- less than 2^32
--    "ZZpFlint 8589934669"
--    "ZZpFlint 4611686018427387847",
--    "ZZpFlint 9223372036854775783"
    }

fieldsFFPACK = if hasFFPACK and false then{
    "ZZpFFPACK 2",
    "ZZpFFPACK 3",
    "ZZpFFPACK 5",
    "ZZpFFPACK 101",
    "ZZpFFPACK 30000001",
    "ZZpFFPACK maxFFPACKPrime"
    } else {}
fieldsGivaro = if hasFFPACK then {
    ///GF(3,2, Strategy=>"Givaro")///,
    ///GF(2,7, Strategy=>"Givaro")///,
    ///GF(3,2, Strategy=>"CompleteGivaro")///,
    ///GF(2,7, Strategy=>"CompleteGivaro")///
    } else {}
fieldsGF = {
    "GF(3,2)",
    "GF(5,12)",
    ///GF(3,2, Strategy=>"New")///,
    ///GF(2,7, Strategy=>"New")///
    }

fieldsGFFlintBig = {
--    ///GF(2,1, Strategy=>"FlintBig")///,
    ///GF(2,2, Strategy=>"FlintBig")///,
    ///GF(2,3, Strategy=>"FlintBig")///,
    ///GF(2,4, Strategy=>"FlintBig")///,
    ///GF(2,5, Strategy=>"FlintBig")///,
    ///GF(2,6, Strategy=>"FlintBig")///,
    ///GF(2,7, Strategy=>"FlintBig")///,
    ///GF(2,8, Strategy=>"FlintBig")///,
    ///GF(2,9, Strategy=>"FlintBig")///,
    ///GF(2,10, Strategy=>"FlintBig")///,
    ///GF(2,11, Strategy=>"FlintBig")///,
    ///GF(2,12, Strategy=>"FlintBig")///,
    ///GF(2,13, Strategy=>"FlintBig")///,
    ///GF(2,20, Strategy=>"FlintBig")///,
    ///GF(2,30, Strategy=>"FlintBig")///,
    
    ///GF(3,1, Strategy=>"FlintBig")///,
    ///GF(3,2, Strategy=>"FlintBig")///,
    ///GF(3,3, Strategy=>"FlintBig")///,
    ///GF(3,4, Strategy=>"FlintBig")///,
    ///GF(3,5, Strategy=>"FlintBig")///,
    ///GF(3,6, Strategy=>"FlintBig")///,
    ///GF(3,7, Strategy=>"FlintBig")///,
    ///GF(3,8, Strategy=>"FlintBig")///,
    ///GF(3,13, Strategy=>"FlintBig")///,
    ///GF(3,20, Strategy=>"FlintBig")///,

    ///GF(5,1, Strategy=>"FlintBig")///,
    ///GF(5,2, Strategy=>"FlintBig")///,
    ///GF(5,3, Strategy=>"FlintBig")///,
    ///GF(5,4, Strategy=>"FlintBig")///,
    ///GF(5,5, Strategy=>"FlintBig")///,

    ///GF(7,1, Strategy=>"FlintBig")///,
    ///GF(7,2, Strategy=>"FlintBig")///,
    ///GF(7,3, Strategy=>"FlintBig")///,
    ///GF(7,4, Strategy=>"FlintBig")///,

    ///GF(11,1, Strategy=>"FlintBig")///,
    ///GF(11,2, Strategy=>"FlintBig")///,
    ///GF(11,3, Strategy=>"FlintBig")///,

    ///GF(13,1, Strategy=>"FlintBig")///,
    ///GF(13,2, Strategy=>"FlintBig")///,
    ///GF(13,30, Strategy=>"FlintBig")///,

    ///GF(17,1, Strategy=>"FlintBig")///,
    ///GF(17,2, Strategy=>"FlintBig")///,
    ///GF(17,30, Strategy=>"FlintBig")///,

    ///GF(19,1, Strategy=>"FlintBig")///,
    ///GF(19,2, Strategy=>"FlintBig")///,
    ///GF(19,10, Strategy=>"FlintBig")///,

    ///GF(23,7, Strategy=>"FlintBig")///
    }

fieldsGFFlint = {
    ///GF(2,1, Strategy=>"Flint")///,
    ///GF(2,2, Strategy=>"Flint")///,
    ///GF(2,3, Strategy=>"Flint")///,
    ///GF(2,4, Strategy=>"Flint")///,
    ///GF(2,5, Strategy=>"Flint")///,
    ///GF(2,6, Strategy=>"Flint")///,
    ///GF(2,7, Strategy=>"Flint")///,
    ///GF(2,8, Strategy=>"Flint")///,
    ///GF(2,9, Strategy=>"Flint")///,
    ///GF(2,10, Strategy=>"Flint")///,
    ///GF(2,11, Strategy=>"Flint")///,
    ///GF(2,12, Strategy=>"Flint")///,
    ///GF(2,13, Strategy=>"Flint")///,
    
    ///GF(3,1, Strategy=>"Flint")///,
    ///GF(3,2, Strategy=>"Flint")///,
    ///GF(3,3, Strategy=>"Flint")///,
    ///GF(3,4, Strategy=>"Flint")///,
    ///GF(3,5, Strategy=>"Flint")///,
    ///GF(3,6, Strategy=>"Flint")///,
    ///GF(3,7, Strategy=>"Flint")///,
    ///GF(3,8, Strategy=>"Flint")///,

    ///GF(5,1, Strategy=>"Flint")///,
    ///GF(5,2, Strategy=>"Flint")///,
    ///GF(5,3, Strategy=>"Flint")///,
    ///GF(5,4, Strategy=>"Flint")///,
    ///GF(5,5, Strategy=>"Flint")///,

    ///GF(7,1, Strategy=>"Flint")///,
    ///GF(7,2, Strategy=>"Flint")///,
    ///GF(7,3, Strategy=>"Flint")///,
    ///GF(7,4, Strategy=>"Flint")///,

    ///GF(11,1, Strategy=>"Flint")///,
    ///GF(11,2, Strategy=>"Flint")///,
    ///GF(11,3, Strategy=>"Flint")///,

    ///GF(13,1, Strategy=>"Flint")///,
    ///GF(13,2, Strategy=>"Flint")///,
    ///GF(13,3, Strategy=>"Flint")///,

    ///GF(17,1, Strategy=>"Flint")///,
    ///GF(17,2, Strategy=>"Flint")///,
    ///GF(17,3, Strategy=>"Flint")///,

    ///GF(19,1, Strategy=>"Flint")///,
    ///GF(19,2, Strategy=>"Flint")///,
    ///GF(19,3, Strategy=>"Flint")///,

    ///GF(23,2, Strategy=>"Flint")///
    }

finitefields = join({
    "ZZp 2", 
    "ZZp 3",
    "ZZp 5", 
    "ZZp 101", 
    "ZZp 32719"},
    fieldsFLINT,
    fieldsFFPACK,
    fieldsGivaro,
    fieldsGF,
    fieldsGFFlint,
    fieldsGFFlintBig
    )

fields = join({
    "ZZp 2", 
    "ZZp 3",
    "ZZp 5", 
    "ZZp 101", 
    "ZZp 32719"},
    fieldsFLINT,
    fieldsFFPACK,
    fieldsGivaro,
    fieldsGF,
    fieldsGFFlint,
    fieldsGFFlintBig,
    {"QQ"}
    -- QQFlint not working yet
    )

ringsPID = {
    "ZZ"
    --"ZZFlint"
    }

ringsRR = {
    "RR_53",
    "RR_54",
    "RR_100",
    "RR_134",
    "RR_200",
    "RR_256",
    "RR_1000",
    "RR_10000"
    }
ringsCC = {
    "CC_53",
    "CC_54",
    "CC_100",
    "CC_134",
    "CC_200",
    "CC_256",
    "CC_1000",
    "CC_10000"
    }

runTests = (rings, tests, exceptions) -> (
    R := rings/(r -> (r,value r));
    T := tests/(t -> (t,value t));
    for t in T do for r in R do  (
        if member((r#0,t#0), exceptions) then (
            << "xcep " << t#0 << " ring " << r#0 << endl;
            )
        else (
            << "test " << t#0 << " ring " << r#0 << " time ";
            tim := timing (t#1(r#1));
            << tim#0 << endl;
            )
        )
    )

--load (EngineTests#"source directory"|"EngineTests/test-gbZZ.m2")
--load (EngineTests#"source directory"|"EngineTests/test-linalg.m2")

load "EngineTests/LinearAlgebra.Test.Driver.m2"
load "EngineTests/MutableMatrix.Test.Driver.m2"
load "EngineTests/Ring.Test.Driver.m2"
load "EngineTests/GB.Test.Mathic.m2"

load "EngineTests/Res.f4.m2"
--check  EngineTests

///
testFracRaw = () -> (
     debug Core;
     K = rawFractionOverNoetherNormalization(raw R, 1);
     f = (raw 1_K) / (raw x);
     g = 1_K / y;
     assert(f + g == (raw (x+y)) / raw (x*y));
     )
testFrac = () -> (
     R = ZZ/101[y,x,MonomialOrder=>{1,1}]/(y^3-x^2*y^2-x-3);
     K = frac R;
     assert(1/x + 1/y == (x+y)/(x*y));
     assert(1/x - 1/y == (-x+y)/(x*y));
     1/y;
     1(y-x);
     x/y^3;
     )
///

--load (EngineTests#"source directory"|"EngineTests/test-tower-rings.m2")

-- Tests of mutable matrix operations over ZZ/p

-- Test row and column operations on dense and sparse mutable matrices,
-- over the rings: ZZ, ZZ/p, QQ, QQ[x,y,z], RRR, CCC, frac QQ[x,y]

--TODO ringOpsZZp
ringOpsZZp = (p) -> (
     -- test basic arithmetic over ZZ/p
     -- we do this by doing operations on 1x1 mutable matrices.
     -- 
     kk := ZZ/p;
     -- test: from_int
     -- test: addition, subtraction, negation
     -- test: subtract_multiple
     -- test: multiplication
     -- test: powers (can't do yet?)
     -- test: set_from_mpz, set_from_mpq
     -- test: invert, divide (not possible yet?)
     -- test: discreteLog
     -- test? syzygy, elem_text_out, text_out, compare_elems
     -- what is the primitive root? characteristic?
     for i from 0 to p-1 do
       for j from 0 to p-1 do (
	    -- check that i+j mod p == i_kk + j_kk
	    a := mutableMatrix(kk,1,1); a_(0,0) = i_kk;
	    b := mutableMatrix(kk,1,1); b_(0,0) = i_kk;
	    )
     )



-----------------------------------
-- tests over approximate fields --
-----------------------------------
testClean = (R) -> (
    -- R should be an RR or CC
    -- this tests "clean" for: Matrix, MutableMatrix, poly ring.
    M := mutableMatrix(R, 10, 10);
    fillMatrix M;
    N := M^4 - M*M*M^2;
    for i from 0 to numRows N - 1 do N_(i,i) = 1.0 + N_(i,i);
    N2 := matrix N;
    assert(norm clean(.00001, N - mutableIdentity(R, numRows N)) == 0);
    assert(norm clean(.00001, N2 - matrix mutableIdentity(R, numRows N2)) == 0);
    -- this next test is actually not correct: 
    --   clean does NOT truncate values, only ones that are close to 0
    --assert(norm(clean(.00001, N) - mutableIdentity(R, numRows N)) == 0)
    P := R[getSymbol "x", getSymbol "y"];
    x := P_0;
    y := P_1;
    f := 1.01*x^2+3.2*y^2-y;
    assert(clean(.0001, f^4 - (f^2)^2) == 0)
    )
testNorm = (R) -> (
    -- R should be an RR or CC
    M := mutableMatrix(R, 10, 10);
    fillMatrix M;
    a := norm matrix M;
    matrixM := matrix M;
    assert(mutableMatrix matrixM == M);
    
    b := norm M;
    assert(a == b);
    ans := (flatten entries M)/abs//max;
    error := abs(b-ans);
    if error > 2^(-precision R + 5) then (
        print(" a==b" | toExternalString (a==b) ); 
        print(" a:=" | toExternalString a | "; ans:=" | toExternalString ans ); 
        peek M ;
        print(" M := " | toExternalString matrixM );
    );
    )

TEST ///
  testClean(RR_53)
  testClean(RR_100)
  testClean(RR_200)
  testClean(RR_54)

  testNorm(RR_53)
  testNorm(RR_100)
  testNorm(RR_200)
  testNorm(RR_54)

  testClean(CC_53)
  testClean(CC_100)
  testClean(CC_200)
  testClean(CC_54)

  apply(15, i-> testNorm(CC_53) ) -- fails from time to time
  apply(15, i->testNorm(CC_100) ) -- fails from time to time
  apply(15, i->testNorm(CC_200) ) -- fails from time to time
  apply(15, i-> testNorm(CC_54) ) -- fails from time to time
///






testLinearAlgebraOverField = (R) -> (
    -- fields to test this over:
    --  ZZ/p (internal)
    --  ZZ/p (FLINT)
    --  ZZ/p (FFPACK)
    --  GF (internal)
    --  GF (GIVARO?)
    --  GF (FLINT?)
    --  QQ (internal, GMP)
    --  QQ (FLINT)
    --  rational function fields
    --
    -- test the following functions:
    -- determinant
    -- rank
    -- inverse
    -- mult
    -- nullspace
    -- solveLinear
    -- rankProfile (row and column)
    -- addmultipleto, subtractmultipleto
    -- LU decomposition
    << "testing matrix linear algebra for " << describe R << endl;
    << "tests passed for " << raw R << endl;
    )




TEST ///
  R = RR_100
  M = mutableMatrix(R,5,5)
  fillMatrix M
  N = matrix M
  det N
  det M
  M*M - mutableMatrix(N*N)
  inverse M
  

  R = CC_100
  M = mutableMatrix(R,5,5)
  fillMatrix M
  N = matrix M
  det N
  det M
  det(N*N) - (det N)^2
  det(M*M) - (det M)^2
  M*M - mutableMatrix(N*N)
  
  R = RR_53
  M = mutableMatrix(R,5,5)
  fillMatrix M
  N = matrix M
  det N
  det M
  M*M - mutableMatrix(N*N)
///



TEST /// 
  -- of clean
  R = CC_100 
  M = matrix{{0.0001_R+ii_R}}
  M = mutableMatrix{{0.0001_R+ii_R}}
  clean_0.001 M
///

TEST ///
  -- eigenvalues
  M = mutableMatrix(CC_100,3,3)
  M = matrix fillMatrix M
  eigenvalues M
  LUdecomposition M
///

TEST ///
-- of rawDiscreteLog
debug Core
kk = ZZ/32003
L = for i from 1 to 32002 list rawDiscreteLog raw (i_kk);
a = 2_kk;
assert(1 == rawDiscreteLog (raw a))
L2 = for i from 0 to 32001 list a^(L#i);
L3 = toList (1..32002);
assert(L2 == L3)

///

beginDocumentation()

doc ///
Key
  EngineTests
Headline
  a test suite for the Macaulay2 engine
Description
  Text
Caveat
SeeAlso
///


