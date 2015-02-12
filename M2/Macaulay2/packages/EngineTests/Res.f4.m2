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
  rawBetti(raw C.Resolution, 1)
///

TEST ///  
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[vars(0..17), MonomialOrder=>{Weights=>splice{18:1}}]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)

  C = res(I, Strategy=>0)
  rawBetti(raw C.Resolution, 1)
  rawBetti(raw C.Resolution, 0)
  rawBetti(raw C.Resolution, 2)
  rawBetti(raw C.Resolution, 3)
  raw C.Resolution
  
  C = res(I, Strategy=>1)
  raw C.Resolution
  
  C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)
///

TEST ///  
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[vars(0..4), MonomialOrder=>{Weights=>splice{5:1}}]
  I = ideal"b2c,abc,a2c,a2de,b3d,bc2de,ac2de,ab2d2e,c3d2e2"
  J = gens gb I  
  elapsedTime C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)
///

TEST ///  
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[a..f, MonomialOrder=>{Weights=>{1,1,1,1,1,1}}]
  I = ideal(a*b*c-d*e*f, a*b^2-d*c^2, a*e*f-d^2*b)
  J = gens gb I

  C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)
  
  I = ideal I_*
  C = res(ideal gens gb I, Strategy=>1)
  rawBetti(raw C.Resolution, 1)

  I = ideal I_*
  C = res(ideal gens gb I, Strategy=>0)
  rawBetti(raw C.Resolution, 1)
///

TEST ///
  -- this is a small-ish example used to get the logic of matrix building right
  restart
  setRandomSeed 0
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[vars(0..3), MonomialOrder=>{Weights=>splice{4:1}}]
  I = ideal fromDual random(R^1, R^{-3});
  J = ideal gens gb I;
  see J  
  elapsedTime C = res(ideal gens gb I, Strategy=>4)
  f1 = map(R, rawResolutionGetMatrix(raw C,1))
  f2 = map(R, rawResolutionGetMatrix(raw C,2))
  f3 = map(R, rawResolutionGetMatrix(raw C,3));
  map(kk,rawResolutionGetMatrix2(raw C,2,3))
  map(kk,rawResolutionGetMatrix2(raw C,2,4))
  C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)

///

TEST ///  
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[vars(0..10), MonomialOrder=>{Weights=>splice{11:1}}]
  I = ideal fromDual random(R^1, R^{-3});
  J = gens gb I;
  
  elapsedTime C = res(ideal gens gb I, Strategy=>4)
  C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)

  I = ideal I_*
  C = res(ideal gens gb I, Strategy=>1, DegreeLimit=>0)
  rawBetti(raw C.Resolution, 1)

  I = ideal I_*;
  elapsedTime C = res(ideal gens gb I, Strategy=>0, DegreeLimit=>0)
  rawBetti(raw C.Resolution, 1)

///

TEST ///  
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[vars(0..15), MonomialOrder=>{Weights=>splice{16:1}}]
  I = ideal fromDual random(R^1, R^{-3});
  J = gens gb I;
  elapsedTime C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)
///

TEST ///  
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[vars(0..18), MonomialOrder=>{Weights=>splice{19:1}}]
  I = ideal fromDual random(R^1, R^{-3});
  J = gens gb I;
  elapsedTime C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)  
///

///
  restart
  debug Core
  load "g16n2.m2"
  kk = ZZp(101, Strategy=>"Old")
  R = kk[t_0..t_14, MonomialOrder=>{Weights=>splice{15:1}}]
  I = sub(I,R);
  J = ideal groebnerBasis(I, Strategy=>"F4");
  J = ideal sort(gens J, MonomialOrder=>Descending, DegreeOrder=>Ascending);
  elapsedTime C = res(J, Strategy=>4)  
  rawBetti(raw C.Resolution, 1)

  I = ideal J_*;
  elapsedTime C = res(I, Strategy=>0, DegreeLimit=>-1)
  rawBetti(raw C.Resolution, 1)

///