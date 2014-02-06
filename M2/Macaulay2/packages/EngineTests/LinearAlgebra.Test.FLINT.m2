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
if hasFlint then 
TEST ///
  debug Core
  R = ZZp(2, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;   -- the fillMatrix is not working here...
  testRank R;
  
 -- testSolve R  -- calling wrong rawLinAlgSolve: not implemented!

///

if hasFlint then 
TEST ///
  R = ZZpFlint( 3 )
  testLinearAlgebra$FLINT R
///

if hasFlint then 
TEST ///
  R = ZZpFlint( 5 )
  testLinearAlgebra$FLINT R
///

if hasFlint then 
TEST ///
  R = ZZpFlint( 101 )
  testLinearAlgebra$FLINT R
///

if hasFlint then 
TEST ///
  -- largest prime < 2^62
  R = ZZpFlint( 4611686018427387847 ) 
  testLinearAlgebra$FLINT R
///

if hasFlint then 
TEST ///
  -- largest prime < 2^63
  R = ZZpFlint(9223372036854775783 )
  testLinearAlgebra$FLINT R
///


if hasFlint then 
TEST ///
  R = ZZpFlint(maxFLINTPrime)
  testLinearAlgebra$FLINT R
///

if hasFlint then 
TEST ///
  debug Core
  -- Most of this code is designed for fields...
  R = ZZFlint
  testDeterminant R
  testMult R
///

if hasFlint then 
TEST ///
  debug Core
  -- Flint QQ
  R = QQFlint
  testDeterminant R
  testMult R
  testRank R       --FAILS
  testNullspace R; --FAILS
///


