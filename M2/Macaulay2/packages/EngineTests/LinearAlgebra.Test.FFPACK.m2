-- -*- coding: utf-8 -*-



--------------------------------
-- ffpack linear algebra: ZZ/p -
--------------------------------

debug Core;



export {
ZZpFFPACK
}

ZZpFFPACK  = ( modPrime )->
(
  assert(isPrime modPrime);
  ZZp( modPrime, Strategy=>"FFPACK")
)

testSolve$FFPACK = (R) -> (
    testSolveSimple R;
    return;
    -- now for more complicated examples
    -- FAILING TEST: crashes
    debug Core;
    R = ZZp(101, Strategy=>"FFPACK");
    N := 90;
    M := mutableMatrix(R, N, N);
    fillMatrix M;
    B := mutableMatrix(R, N, 5);
    fillMatrix B;
    XRaw := time rawLinAlgSolve(raw M, raw B, true);
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
  {*
  testInverse R; -- FAILS
  testRank R;  -- FAILS
  *}
///

if hasFFPACK then 
TEST ///
  debug Core --required, why? otherwiese testLinearAlgebraSet is not defined.
  R = ZZpFFPACK ( 3 );
  testLinearAlgebraSet R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZpFFPACK( 5 );
  testLinearAlgebraSet R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZpFFPACK( 101 );
  testLinearAlgebraSet  R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZpFFPACK( 30000001 );
  testLinearAlgebraSet R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZpFFPACK( maxFFPACKPrime )
  testLinearAlgebraSet R;
  --R = ZZp(33554467, Strategy => "FFPACK") -- this should not work
///

