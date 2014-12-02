-- Tests for free resolutions
-- test 1
TEST ///
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[a..d, Degrees=>{1,2,3,4}, MonomialOrder=>{Weights=>{1,2,3,4}}]
  R = kk[a..d, MonomialOrder=>{Weights=>{1,1,1,1}}]
  M = coker vars R
  gbTrace = 1
  C = res(M, Strategy=>4)
  F = R^{0,-1,-2}
  M = coker map(F,,{{a,0,0},{0,b,0},{0,0,c}})
///
