-- -*- coding: utf-8 -*-



--------------------------------
-- ffpack linear algebra: ZZ/p -
--------------------------------

debug Core;



export {
    "ZZpFFPACK"
}

ZZpFFPACK  = ( modPrime )->
(
  assert(isPrime modPrime);
  ZZp( modPrime, Strategy=>"Ffpack")
)

testSolve$FFPACK = (R) -> (
    testSolveSimple R;
    return;
    -- now for more complicated examples
    -- FAILING TEST: crashes
    debug Core;
    R = ZZp(101, Strategy=>"Ffpack");
    N := 90;
    M := mutableMatrix(R, N, N);
    fillMatrix M;
    B := mutableMatrix(R, N, 5);
    fillMatrix B;
    XRaw := time rawLinAlgSolve(raw M, raw B);
    X := solve(M, B);
    assert( raw X == XRaw);
    assert(M*X-B == 0);
    )


if hasFFPACK then 
TEST ///
  debug Core
  R = ZZpFFPACK(2 );
  testDeterminant R;
  testMult R;
  -*
  testInverse R; -- FAILS
  testRank R;  -- FAILS
  *-
  R = ZZpFFPACK ( 2 );
  testLinearAlgebraSet R;

///

if hasFFPACK then 
TEST ///
  R = ZZpFFPACK ( 3 );
  testLinearAlgebraSet R;
///

if hasFFPACK then 
TEST ///
  R = ZZpFFPACK( 5 );
  testLinearAlgebraSet R;
///

if hasFFPACK then 
TEST ///
  R = ZZpFFPACK( 101 );
  testLinearAlgebraSet  R;
///

if hasFFPACK then 
TEST ///
  R = ZZpFFPACK( 30000001 );
  testLinearAlgebraSet R;
///

if hasFFPACK then 
TEST ///
  R = ZZpFFPACK( maxFFPACKPrime )
  testLinearAlgebraSet R;

  --R = ZZp(33554467, Strategy => "Ffpack") -- this should not work
  assert try (R = ZZp(33554467, Strategy => "Ffpack"); false) else true
///

if hasFFPACK then
///
  debug Core
  R =  ZZp(101,Strategy=>"Ffpack")
  m = mutableMatrix matrix{{0_R,6_R},{0_R,0_R},{0_R,4_R}}
   LUdecomposition m
   (P,Q) = rawLQUPFactorization(raw m)
///