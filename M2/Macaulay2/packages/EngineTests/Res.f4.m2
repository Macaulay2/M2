-- Tests for free resolutions

-- Current caveats:
--   a. REMOVED need to give Weights: first wt vector is the degree vector
--   b. ?? need to pass in the coker (or ideal) of a groebner basis
--   c. REMOVED the elements at level 1 must be inserted in order of monotone increasing component
--   d. TODO!! if the input free module is a Schreyer order: need to handle that

TEST ///
  R = ZZ/101[a..d]
  M = coker vars R
  gbTrace = 1
  C = nonminimalResolution M
  C = res(M, Strategy=>4)
  betti'ans = new BettiTally from {(0,{0},0) => 1, (3,{3},3) => 4, (1,{1},1) => 4, (4,{4},4) => 1, (2,{2},2) => 6}
  assert(betti'ans == betti C)
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)    
///

TEST ///
  R = ZZ/101[a..d]
  F = R^{0,-1,-2}
  M = coker map(F,,{{a,0,0},{0,b,0},{0,0,c}})
  isHomogeneous M  
  C = nonminimalResolution M
  betti res M == betti C
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)  

  nonminimalBetti C
///

TEST ///
  R = ZZ/101[a..d]
  F = R^{0,-2,-1}
  M = coker map(F,,{{a,0,0},{0,0,b},{0,c,0}})
  M1 = coker map(F,,{{a,0,0},{0,0,b},{0,c,0}})
  isHomogeneous M  
  C = nonminimalResolution M
  betti res M1 == betti C -- BUG?? YES!!
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)  

  nonminimalBetti C
///

TEST ///
  R = ZZ/101[a..e]
  I = monomialCurveIdeal(R,{1,3,7,9})
  C = nonminimalResolution I
  assert(betti C == betti res (ideal I_*))
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
///

TEST ///  
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  elapsedTime C = nonminimalResolution I
  elapsedTime  assert(betti C == betti res (ideal I_*))
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
  betti C

  elapsedTime  res(ideal(I_*), Strategy=>0)
  elapsedTime  res(ideal(I_*), Strategy=>1)
  elapsedTime  res(ideal(I_*), Strategy=>2)
  elapsedTime  res(ideal(I_*), Strategy=>3)

  nonminimalBetti C
  debug Core
  rawBetti(raw C.Resolution, 1)
  rawBetti(raw C.Resolution, 0)
  rawBetti(raw C.Resolution, 2) -- not implemented yet
  rawBetti(raw C.Resolution, 3) -- not implemented yet

  kk = coefficientRing R

  assert(rank map(kk,rawResolutionGetMatrix2(raw C,2,3)) == 12)
  assert(rank map(kk,rawResolutionGetMatrix2(raw C,2,4)) == 5)
  assert(rank map(kk,rawResolutionGetMatrix2(raw C,2,5)) == 1)

  assert(rank map(kk,rawResolutionGetMatrix2(raw C,3,4)) == 9)
  assert(rank map(kk,rawResolutionGetMatrix2(raw C,3,5)) == 41)
  assert(rank map(kk,rawResolutionGetMatrix2(raw C,3,6)) == 7)

  assert(rank map(kk,rawResolutionGetMatrix2(raw C,4,5)) == 2)
  assert(rank map(kk,rawResolutionGetMatrix2(raw C,4,6)) == 69)
  assert(rank map(kk,rawResolutionGetMatrix2(raw C,4,7)) == 20)
///

TEST ///  
  restart
  kk = ZZ/101
  R = kk[vars(0..4)]
  I = ideal"b2c,abc,a2c,a2de,b3d,bc2de,ac2de,ab2d2e,c3d2e2"
  elapsedTime C = nonminimalResolution I
  C1 = res (ideal I_*)
  elapsedTime  assert(betti C == betti C1)
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
  nonminimalBetti C
///

TEST ///  
  restart
  kk = ZZ/101
  R = kk[a..f]
  I = ideal(a*b*c-d*e*f, a*b^2-d*c^2, a*e*f-d^2*b)
  gbTrace=3
  elapsedTime C = res(I, Strategy=>4)
  betti C
  debug Core
  rawBetti(raw C.Resolution, 1)
  rawBetti(raw C.Resolution, 0)
  
  I = ideal I_*
  C = res(ideal gens gb I, Strategy=>1)
  rawBetti(raw C.Resolution, 1)
  rawBetti(raw C.Resolution, 0)

  I = ideal I_*
  C = res(ideal gens gb I, Strategy=>0)
  rawBetti(raw C.Resolution, 1)
  rawBetti(raw C.Resolution, 0)  
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
  J = ideal gens gb I
    elapsedTime C = res(J, Strategy=>4, DegreeLimit=>1)
    elapsedTime C = res(J, Strategy=>4, DegreeLimit=>2)
    elapsedTime C = res(J, Strategy=>4, DegreeLimit=>3)
  f1 = map(R, rawResolutionGetMatrix(raw C,1))
  f2 = map(R, rawResolutionGetMatrix(raw C,2))
  f3 = map(R, rawResolutionGetMatrix(raw C,3));
  assert(f1*f2 == 0)
  assert(f2*f3 == 0)
  map(kk,rawResolutionGetMatrix2(raw C,2,3))
  map(kk,rawResolutionGetMatrix2(raw C,2,4))

  map(kk,rawResolutionGetMatrix2(raw C,3,4))
  map(kk,rawResolutionGetMatrix2(raw C,2,4))
  C = res(ideal gens gb I, Strategy=>4)
  rawBetti(raw C.Resolution, 1)

///

TEST ///  
  restart
  debug Core
  kk1 = ZZ/101
  kk = ZZp(101, Strategy=>"Old")
  nvars = 13
  nvars = 14
  R = kk[vars(0..nvars-1), MonomialOrder=>{Weights=>splice{nvars:1}}]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  J = ideal gens gb I;
  gbTrace=2
  elapsedTime C = res(ideal gens gb I, Strategy=>4, StopBeforeComputation=>true)
  elapsedTime C = res(ideal gens gb I, Strategy=>4)
  elapsedTime C = res(J, Strategy=>4, LengthLimit=>16)
  elapsedTime C = res(J, Strategy=>4);
  elapsedTime C = res(J, Strategy=>4, DegreeLimit=>0)
  rawBetti(raw C.Resolution, 1)

  rank matrix map(kk,rawResolutionGetMatrix2(raw C,2,3))
  rank matrix map(kk,rawResolutionGetMatrix2(raw C,3,4))
  time matrix map(kk,rawResolutionGetMatrix2(raw C,4,5));
  time rank oo
  time matrix map(kk,rawResolutionGetMatrix2(raw C,5,6));
  time lift(oo,ZZ);
  time promote(oo,kk1);
  time mutableMatrix oo;
  time rank oo  
  time matrix map(kk,rawResolutionGetMatrix2(raw C,6,7));  
  time rank oo
  time promote(lift(matrix map(kk,rawResolutionGetMatrix2(raw C,6,7)), ZZ), kk1);
  time rank oo    
  time promote(lift(matrix map(kk,rawResolutionGetMatrix2(raw C,7,8)), ZZ), kk1);
  time promote(lift(matrix map(kk,rawResolutionGetMatrix2(raw C,8,9)), ZZ), kk1);  
  time rank promote(lift(matrix map(kk,rawResolutionGetMatrix2(raw C,9,10)), ZZ), kk1)
  time rank promote(lift(matrix map(kk,rawResolutionGetMatrix2(raw C,7,9)), ZZ), kk1)

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
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  J = ideal gens gb I;
  elapsedTime C = res(J, Strategy=>4)
  elapsedTime C = res(J, Strategy=>4, StopBeforeComputation=>true)
  rawBetti(raw C.Resolution, 1)
  
  kk = ZZ/101
  R = kk[vars(0..10)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  elapsedTime C = nonminimalResolution I
  betti C
  nonminimalBetti C
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
  load "free-resolutions/g16n2.m2"
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

///
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[x_1..x_20, MonomialOrder=>{Weights=>splice{20:1}}]
  I = Grassmannian(2,5,R)
  gbTrace=2
  elapsedTime C = res(ideal gens gb I, Strategy=>4)  
///

///
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[x_1..x_21, MonomialOrder=>{Weights=>splice{21:1}}]
  I = Grassmannian(1,6,R)
  gbTrace=2
  elapsedTime C = res(ideal gens gb I, Strategy=>4)  
///

///
-- this one takes too much memory on my laptop
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[x_1..x_28, MonomialOrder=>{Weights=>splice{28:1}}]
  I = Grassmannian(1,7,R)
  gbTrace=2
  elapsedTime C = res(ideal gens gb I, Strategy=>4)  
///

///
-- this one takes too much memory on my laptop too
  restart
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[x_1..x_35, MonomialOrder=>{Weights=>splice{35:1}}]
  I = Grassmannian(2,6,R)
  gbTrace=2
  elapsedTime C = res(ideal gens gb I, Strategy=>4)  
///

///
  restart 
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[x_1..x_16, MonomialOrder=>{Weights=>splice{16:1}}]
  M = genericMatrix(R,x_1,4,4)
  I = permanents(3,M)
  gbTrace=1
  J = ideal gens gb(I, Algorithm=>LinearAlgebra);
  gbTrace=2
  elapsedTime C = res(J, Strategy=>4)  

  restart 
  debug Core
  kk = ZZp(101, Strategy=>"Old")
  R = kk[x_1..x_16, MonomialOrder=>{Weights=>splice{16:1}}]
  M = genericMatrix(R,x_1,4,4)
  I = permanents(3,M)
  res I
  codim I
  phi = map(R,R,random(R^1, R^{16:-1}));
  J = phi I;
  gbTrace=2
  
  gens gb(J, Algorithm=>LinearAlgebra);
///
