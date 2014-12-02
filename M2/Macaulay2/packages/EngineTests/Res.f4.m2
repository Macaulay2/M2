-- Tests for free resolutions

-- Current caveats:
--   a. need to give Weights: first wt vector is the degree vector
--   b. need to pass in the coker (or ideal) of a groebner basis
--   c. the elements at level 1 must be inserted in order of monotone increasing component
--   d. if the input free module is a Schreyer order: need to handle that

-- test 1
TEST ///
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  --R = kk[a..d, Degrees=>{1,2,3,4}, MonomialOrder=>{Weights=>{1,2,3,4}}]
  R = kk[a..d, MonomialOrder=>{Weights=>{1,1,1,1}}]
  M = coker vars R
  gbTrace = 1
  C = res(M, Strategy=>4)
  F = R^{0,-1,-2}
  M = coker map(F,,{{a,0,0},{0,b,0},{0,0,c}})
  
  I = monomialCurveIdeal(R,{1,3,7})
  gens gb I
  C = res(I, Strategy=>4)
  
  R = kk[vars(0..17), MonomialOrder=>{Weights=>splice{18:1}}]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  C = res(ideal gens gb I, Strategy=>4)
///
