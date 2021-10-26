-- -*- coding: utf-8 -*-

--------------------------------
-- Givaro GF -------------------
--------------------------------
if hasFFPACK then
TEST ///
  debug Core
  R = GF(3,2, Strategy=>"Givaro")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

if hasFFPACK then
TEST ///
  debug Core
  R = GF(5,12)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///



