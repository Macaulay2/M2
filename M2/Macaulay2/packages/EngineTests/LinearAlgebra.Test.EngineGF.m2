-- -*- coding: utf-8 -*-

--------------------------------
-- Engine GF -------------------
--------------------------------
TEST ///
  debug Core
  R = GF(3,2)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

TEST ///
  debug Core
  R = GF(5,12)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///
