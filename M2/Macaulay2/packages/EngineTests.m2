newPackage(
        "EngineTests",
    	AuxiliaryFiles => true,
        Version => "0.1", 
        Date => "29 Aug 2011",
	    Authors => {
            {Name => "Michael E. Stillman", 
		        Email => "mike@math.cornell.edu", 
		        HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	        {Name => "Jakob Kroeker", 
		        Email => "Jakob Kröker <kroeker@math.uni-hannover.de>", 
		        HomePage => "" }
            },
        Headline => "a test suite for the Macaulay2 engine",
	    PackageExports => {"FastLinearAlgebra"},
        DebuggingMode => true
        )

export { 
   testNorm,
   testClean

    }



debug Core


--load (EngineTests#"source directory"|"EngineTests/test-gbZZ.m2")
--load (EngineTests#"source directory"|"EngineTests/test-linalg.m2")

load "EngineTests/LinearAlgebra.Test.Driver.m2"
load "EngineTests/MutableMatrix.Test.Driver.m2"
load "EngineTests/Ring.Test.Driver.m2"

--check  EngineTests



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
    ans := (flatten entries M)/abs//max;
    if not (a == ans) then (
      print(" a==b" | toExternalString (a==b) ); 
      print(" a:=" | toExternalString a | "; ans:=" | toExternalString ans ); 
     peek M ;
      print(" M := " | toExternalString matrixM );
    );
    assert(a == ans);
    if not (b == ans) then ( print ("a="|toExternalString a|"; ans="|toExternalString ans););
    assert(b == ans);
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



end


