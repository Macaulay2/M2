-- -*- coding: utf-8 -*-

--------------------------------
-- flint linear algebra: ZZ/p --
--------------------------------

export {
    ZZpFlint,
    testLinearAlgebra$FLINT
}

ZZpFlint = (char) ->
(
   ZZp(char, Strategy => "FLINT")
)



testLinearAlgebra$FLINT = (rng)->
(
  debug Core;

  testDeterminant rng;
  testMult        rng;
  testInverse     rng;
  testRank        rng;
  testNullspace   rng;
  --testSolve       rng;
)

  -- Not written yet: nullSpace for left side
  -- solveLinear: somehow the wrong one is being called
  -- rankProfile
  -- LUdecomposition
TEST ///
  debug Core
  R = ZZp(2, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;   -- the fillMatrix is not working here...
  testRank R;
  
 -- testSolve R  -- calling wrong rawLinAlgSolve: not implemented!

///

TEST ///
  R = ZZpFlint( 3 )
  testLinearAlgebra$FLINT R
///

TEST ///
  R = ZZpFlint( 5 )
  testLinearAlgebra$FLINT R
  
  M = mutableMatrix(R,10,20); fillMatrix M
  LUdecomposition M
  
  debug EngineTests
  checkLU matrix M
///

TEST ///
  debug Core
  for i from 32500 to 32767 do (
      if isPrime i then (
          R = ZZp(i, Strategy=>"FLINT");
          time testLinearAlgebra$FLINT R
          );
      )
///

TEST ///
  for i from 2 to 10 do (
      R = GF(2,i,Strategy=>"FlintBig");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 10 do (
      R = GF(3,i,Strategy=>"FlintBig");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 10 do (
      R = GF(5,i,Strategy=>"FlintBig");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 10 do (
      R = GF(7,i,Strategy=>"FlintBig");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 10 do (
      R = GF(11,i,Strategy=>"FlintBig");
      time testLinearAlgebra R
      )
///

TEST ///
  R = ZZpFlint( 101 )
  testLinearAlgebra$FLINT R
///

TEST ///
  -- largest prime < 2^62
  R = ZZpFlint( 4611686018427387847 ) 
  testLinearAlgebra$FLINT R
///

TEST ///
  -- largest prime < 2^63
  R = ZZpFlint(9223372036854775783 )
  testLinearAlgebra$FLINT R
///


TEST ///
  R = ZZpFlint(maxFLINTPrime)
  testLinearAlgebra$FLINT R
///

TEST ///
  debug Core
  -- Most of this code is designed for fields...
  R = ZZFlint
  testDeterminant R
  testMult R
///

TEST ///
  -- this ring is still experimental
  debug Core
  -- Flint QQ
  R = QQFlint
  testDeterminant R
  testMult R
  -- testRank R       --FAILS
  -- testNullspace R; --FAILS
///

TEST ///
  debug Core
///

