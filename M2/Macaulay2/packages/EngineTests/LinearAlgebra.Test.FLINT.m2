-- -*- coding: utf-8 -*-

--------------------------------
-- flint linear algebra: ZZ/p --
--------------------------------

export {
    "ZZpFlint",
    "testLinearAlgebra$FLINT"
}

ZZpFlint = (char) ->
(
   assert(isPrime char and char < 2^64);
   ZZp(char, Strategy => "Flint")
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
  R = ZZp(2, Strategy => "Flint")
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
  
  M = mutableMatrix matrix(R,  {
      {0, 0, -1, 2, 1, 2, 2, -2, -1, -1, 1, 1, 0, -1, 0, 1, 0, -1, -1, -2}, 
      {-2, 0, 1, 2, 2, 1, -2, 2, -1, 2, 0, -1, -2, -1, -1, 1, 0, -2, 0, 1}, 
      {0, 0, 0, 2, 0, 0, -1, 2, 1, 0, -2, -1, 2, 1, 0, 1, 2, 1, 0, -1}, 
      {0, -2, -2, 2, 0, -1, 0, -2, 0, 0, 2, 2, 2, -1, 0, 0, 2, 0, 1, 2}, 
      {2, -2, 1, 2, -1, 1, -1, 2, 2, 2, 1, 0, 2, 1, 1, -2, 2, -2, 1, -1}, 
      {-1, -1, -2, 2, -2, 1, 2, -2, 1, -2, -1, -2, 2, 0, -2, -1, 1, 2, -1, 0}, 
      {0, 1, -2, 0, 2, 0, 2, 2, 2, -1, 0, -1, 1, 1, -1, 1, 0, 1, 2, -1}, 
      {-1, 1, 1, 2, 2, -1, -2, 2, -1, 1, 0, 1, 2, 2, 2, 0, -1, 0, 0, 0}, 
      {1, 1, 2, 2, 2, 0, 2, 1, -1, -2, -2, -2, 1, -2, 1, 2, 2, -2, 0, 2}, 
      {-1, 1, 1, 1, -2, 1, -1, -2, 2, -1, 0, 0, 1, 2, -1, 2, -2, 2, -1, -1}}  )
  LUdecomposition M
  debug EngineTests
  checkLU matrix M
///

TEST ///
  R = GF(2^7, Strategy=>"FlintBig")
  testLinearAlgebra$FLINT R
  
  M = mutableMatrix matrix(R,  {
      {0, 0, -1, 2, 1, 2, 2, -2, -1, -1, 1, 1, 0, -1, 0, 1, 0, -1, -1, -2}, 
      {-2, 0, 1, 2, 2, 1, -2, 2, -1, 2, 0, -1, -2, -1, -1, 1, 0, -2, 0, 1}, 
      {0, 0, 0, 2, 0, 0, -1, 2, 1, 0, -2, -1, 2, 1, 0, 1, 2, 1, 0, -1}, 
      {0, -2, -2, 2, 0, -1, 0, -2, 0, 0, 2, 2, 2, -1, 0, 0, 2, 0, 1, 2}, 
      {2, -2, 1, 2, -1, 1, -1, 2, 2, 2, 1, 0, 2, 1, 1, -2, 2, -2, 1, -1}, 
      {-1, -1, -2, 2, -2, 1, 2, -2, 1, -2, -1, -2, 2, 0, -2, -1, 1, 2, -1, 0}, 
      {0, 1, -2, 0, 2, 0, 2, 2, 2, -1, 0, -1, 1, 1, -1, 1, 0, 1, 2, -1}, 
      {-1, 1, 1, 2, 2, -1, -2, 2, -1, 1, 0, 1, 2, 2, 2, 0, -1, 0, 0, 0}, 
      {1, 1, 2, 2, 2, 0, 2, 1, -1, -2, -2, -2, 1, -2, 1, 2, 2, -2, 0, 2}, 
      {-1, 1, 1, 1, -2, 1, -1, -2, 2, -1, 0, 0, 1, 2, -1, 2, -2, 2, -1, -1}}  )
  LUdecomposition M
  debug EngineTests
  checkLU matrix M
///

TEST ///
  debug Core
  for i from 32500 to 32767 do (
      if isPrime i then (
          R = ZZp(i, Strategy=>"Flint");
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
  for i from 2 to 13 do (
      R = GF(2,i,Strategy=>"Flint");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 13 do (
      R = GF(3,i,Strategy=>"Flint");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 13 do (
      R = GF(5,i,Strategy=>"Flint");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 13 do (
      R = GF(7,i,Strategy=>"Flint");
      time testLinearAlgebra R
      )
///

TEST ///
  for i from 2 to 13 do (
      R = GF(11,i,Strategy=>"Flint");
      time testLinearAlgebra R
      )
///




TEST ///
  R = ZZpFlint( 101 )
  testLinearAlgebra$FLINT R
///

TEST ///
  -- largest prime < 2^31
  R = ZZpFlint( 2147483647 ) 
  testLinearAlgebra$FLINT R
///

TEST ///
  -- largest prime < 2^30
  R = ZZpFlint(1073741789 )
  testLinearAlgebra$FLINT R
///


TEST ///
  R = ZZpFlint(maxFLINTPrime)
  testLinearAlgebra$FLINT R
///

///
  debug Core
  -- Most of this code is designed for fields...
  R = ZZFlint
  testDeterminant R
  testMult R
///

///
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
  N = 100
  R = ZZp(101, Strategy=>"Flint")
  R = GF(2^20, Strategy=>"FlintBig")
  R = GF(3^5, Strategy=>"Flint")
  m = mutableMatrix(R,N,N); fillMatrix m;
  b = mutableMatrix(R,N,1); fillMatrix b;
  time X = solve(m,b,Invertible=>true);
  time X = solve(m,b,Invertible=>false);
  assert(m*X - b == 0)
///
