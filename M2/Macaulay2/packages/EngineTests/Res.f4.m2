-- Tests for free resolutions
-- test 1
TEST ///
  restart
  R = ZZ/101[a..d]
  M = coker vars R
  C = res(M, Strategy=>4)
///