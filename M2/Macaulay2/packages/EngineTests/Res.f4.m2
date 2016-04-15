-- Tests for free resolutions

-- Current caveats:
--   a. REMOVED need to give Weights: first wt vector is the degree vector
--   b. DONE need to pass in the coker (or ideal) of a groebner basis (in res.m2)
--   c. REMOVED the elements at level 1 must be inserted in order of monotone increasing component
--   d. TODO!! if the input free module is a Schreyer order: need to handle that
--   e. some module gradings don't seem to work.  Either fix, or check for this.

TEST ///
  -- nonminimal resolution.
  -- test the zero ideal
  R = ZZ/101[a..d]
  I = ideal(0_R)
  C = nonminimalResolution I
  betti'ans = new BettiTally from {(0,{0},0) => 1}
  assert(betti C == betti'ans)
  assert(C.dd_1 == 0)
  assert(length C == 0)

  I = trim ideal(0_R)
  C = nonminimalResolution I
  betti'ans = new BettiTally from {(0,{0},0) => 1}
  assert(betti C == betti'ans)
  assert(C.dd_1 == 0)
  assert(length C == 0)
  assert(C.dd^2 == 0)
///

TEST ///
  -- nonminimal resolution.
  -- test the unit ideal
  R = ZZ/101[a..d]
  I = ideal(1_R)
  C = nonminimalResolution I
  betti'ans = new BettiTally from {}
  assert(betti C == betti'ans)
  assert(C.dd_1 == 1)
  assert(length C == 1)
  assert(C.dd^2 == 0)
///

TEST ///
  -- nonminimal resolution.
  -- test linear ideals.
  R = ZZ/101[a..d]
  I = ideal(a-d, b+13*a)
  C = nonminimalResolution I
  betti'ans = new BettiTally from {(1,{1},1) => 2, (0,{0},0) => 1, (2,{2},2) => 1}
  assert(betti C == betti'ans)
  assert(numRows C.dd_1 == 1) 
  assert(numColumns C.dd_1 == 2)
  assert(length C == 2)
  assert(C.dd^2 == 0)
///

TEST ///
  -- nonminimal resolution.
  -- test linear ideal in exterior algebra
  R = ZZ/101[a..d, SkewCommutative=>true]
  I = ideal(a-d, b+13*a)
  B1 = betti res I
  C = nonminimalResolution(ideal(I_*))
  assert(B1 == betti C)
  B1 = betti res (ideal(I_*), LengthLimit=>7)
  B2 = betti nonminimalResolution(ideal(I_*), LengthLimit=>7)
  assert(B1 == B2)
  assert(C.dd^2 == 0)
///

TEST ///
  -- nonminimal resolution.
  -- test ideal in non-standard grading
  R = ZZ/101[a..d, Degrees=>{1,2,3,4}]
  I = ideal(a^4-a*c-d, b^3-c^2, a^8-d^2)
  isHomogeneous I
  B1 = betti res I
  C = nonminimalResolution(ideal(I_*))
  B2 = betti C
  assert(B1 == B2)
  nonminimalBetti C
  C.dd_-1
  C.dd_0
  C.dd_1
  C.dd_2
  C.dd_3
  C.dd_4
  C.dd_5
  assert(C.dd^2 == 0)
  degrees C_1
  degrees C_2
  degrees C_-1
  C_0 
  C_5 
  C_4 
  assert isHomogeneous C
///

TEST ///
  -- test that inhomogeneous input either works, or doesn't crash the system
  -- inhomogeneous input.  For now, disallow...
  -- We never get to our code if the module is inhomogeneous.
  -- Instead, the front end homogenizes the inout, calls this, and then dehomogenizes later.
  S = ZZ/101[a..d]
  I = ideal(a*b-a-1, b^3-c*d-3)
  time res I
  C = nonminimalResolution(ideal(I_*)) -- Want: to do this, just don't try to minimize??
  betti C -- ??
  C.dd 
  
  -- don't allow quotient rings
  S = ZZ/101[a..d]
  I = ideal(a^2-b*c, b^2-c*d)
  R = S/I
  M = coker vars R
  assert (try (nonminimalResolution M; false) else true) 
  
  -- don't allow Weyl algebras

  -- don't allow zero heft vectors
  -- this code never gets to non minimal res code in the engine.
  S = ZZ/101[a,b,c,d,Degrees=>{-1,1,2,-5}]  
  C = nonminimalResolution (ideal gens S)^3
  C.dd^2 == 0
  
  -- don't allow multi-gradings  
  S = ZZ/101[a,b,c,d,DegreeRank=>4]
  assert try (nonminimalResolution ideal gens S; false) else true
///

TEST ///
  R = ZZ/101[a..d]
  M = coker vars R
  gbTrace = 1
  C = nonminimalResolution M
  C = res(M, Strategy=>4)
  betti'ans = new BettiTally from {(0,{0},0) => 1, (3,{3},3) => 4, (1,{1},1) => 4, (4,{4},4) => 1, (2,{2},2) => 6}
  assert(betti'ans == betti C)
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
  isHomogeneous C
  debug Core
  raw source C.dd_3
///

TEST ///
  R = ZZ/101[a..d]
  F = R^{0,-1,-2}
  M = coker map(F,,{{a,0,0},{0,b,0},{0,0,c}})
  assert isHomogeneous M  
  C = nonminimalResolution M
  betti res M == betti C
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)  

  nonminimalBetti C
  assert isHomogeneous C
  C.dd
///

TEST ///
  R = ZZ/101[a..d]
  F = R^{0,-2,-1}
  M = coker map(F,,{{a,0,0},{0,0,b},{0,c,0}})
  M1 = coker map(F,,{{a,0,0},{0,0,b},{0,c,0}})
  isHomogeneous M  
  C = nonminimalResolution M
  betti res M1 == betti C
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)  
  assert isHomogeneous C
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
  rawBetti(raw C.Resolution, 1) == nonminimalBetti C
  rawBetti(raw C.Resolution, 0) == betti C
  --rawBetti(raw C.Resolution, 2) -- not implemented yet
  --rawBetti(raw C.Resolution, 3) -- not implemented yet

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
  
  assert(schreyerOrder target C.dd_2 != 0)
  assert(schreyerOrder source C.dd_2 != 0)
///

TEST ///  
  kk = ZZ/101
  R = kk[vars(0..4)]
  I = ideal"b2c,abc,a2c,a2de,b3d,bc2de,ac2de,ab2d2e,c3d2e2"
  elapsedTime C = nonminimalResolution I
  C1 = res (ideal I_*)
  assert(betti C == betti C1)
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
  nonminimalBetti C
///

TEST ///  
  kk = ZZ/101
  R = kk[a..f]
  I = ideal(a*b*c-d*e*f, a*b^2-d*c^2, a*e*f-d^2*b)
  gbTrace=3
  elapsedTime C = nonminimalResolution I
  betti C == betti res ideal(I_*)
  nonminimalBetti C
  I = ideal I_*
  C1 = res(ideal gens gb I, Strategy=>1)
  rawBetti(raw C1.Resolution, 1)
  rawBetti(raw C1.Resolution, 0)

  I = ideal I_*
  C2 = res(ideal gens gb I, Strategy=>0)
  rawBetti(raw C2.Resolution, 1)
  rawBetti(raw C2.Resolution, 0)  
///

TEST ///
  -- this is a small-ish example used to get the logic of matrix building right
  setRandomSeed 0
  kk = ZZ/101
  R = kk[vars(0..3)]
  I = ideal fromDual random(R^1, R^{-3});
  C = nonminimalResolution I
  
  I = ideal(I_*)
  elapsedTime C1 = nonminimalResolution(I, DegreeLimit=>1) -- DOES NOTHING (i.e. does the whole thing)
  betti C == 
  betti C1 -- totally non-minimal, so maybe it did do something. ACTUALLY: returns without doing ranks
  nonminimalBetti C1
  elapsedTime C2 = nonminimalResolution(I)
  betti C2 == betti C
  assert(C.dd^2 == 0)
  assert(isHomogeneous C)
  C1 = betti res ideal(I_*)
  assert(betti C == betti C1)
///

TEST ///  
  kk = ZZ/101
  nvars = 13
  R = kk[vars(0..nvars-1)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  gbTrace=2
  elapsedTime C = nonminimalResolution I -- 49.39 seconds on MBP
///

TEST ///  
  restart
  kk = ZZ/101
  R = kk[vars(0..10)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  elapsedTime C = nonminimalResolution I
  betti'ans = new BettiTally from {
      (0,{0},0) => 1, 
      (1,{2},2) => 55, 
      (2,{3},3) => 320, 
      (3,{4},4) => 891, 
      (4,{5},5) => 1408,
      (5,{6},6) => 1155, 
      (6,{8},8) => 1155, 
      (7,{9},9) => 1408, 
      (8,{10},10) => 891, 
      (9,{11},11) => 320, 
      (10,{12},12) => 55, 
      (11,{14},14) => 1
      }
  assert(betti C == betti'ans)
  nonminimalBetti C
///

///
  kk = ZZ/101
  R = kk[x_1..x_20]
  I = Grassmannian(2,5,R)
  gbTrace=2
  elapsedTime C = nonminimalResolution I
  betti C
///

///
  kk = ZZ/101
  R = kk[x_1..x_21]
  I = Grassmannian(1,6,R)
  elapsedTime C = nonminimalResolution I
  betti C
///

-------------------------------------
-- Test resolutions of modules ------
-------------------------------------
TEST ///
  R = ZZ/101[vars(0..7)]
  M = genericMatrix(R,a,2,4)
  I = minors(2,M)
  N = syz gens I
  C = nonminimalResolution coker N
  assert(betti C == betti res coker syz gens I)
  nonminimalBetti C  
///

TEST ///
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  C = nonminimalResolution I
  betti C
  m3 = C.dd_2;
  gbTrace=0
  
  nonminimalResolution coker C.dd_2
  nonminimalResolution coker C.dd_3
  nonminimalResolution coker C.dd_4
  nonminimalResolution coker C.dd_5
  nonminimalResolution coker C.dd_6
  nonminimalResolution coker C.dd_7  
  nonminimalResolution coker C.dd_8
  betti C
  
  gensI = schreyerOrder gens gb I
  P = gens gb syz gensI;
  nonminimalResolution coker P  
  
  degrees C.dd_2
///


TEST ///
  R = ZZ/101[a..f]
  I = ideal(a*b-e*f, a*c*e-f^3, a^2*c*d^2-b^2*e*f^2)
  time betti res I
  elapsedTime C0 = nonminimalResolution (ideal I_*)
  P = gens gb syz gens I
  time C1 = nonminimalResolution coker P
  betti C1
  betti C0
///

TEST ///
  setRandomSeed "10"
  R = ZZ/101[a..d]
  I = ideal random(R^1, R^{-2,-3,-4})
  P = gens gb syz gens I
  betti nonminimalResolution coker P  -- This one gives an error: array is out of order.
  betti P
  
  F = source schreyerOrder gens I
  raw F
  P1 = map(F,,P)
  isHomogeneous P1
  betti nonminimalResolution coker P1
///

TEST ///
  needsPackage "BGG"
  S = ZZ/101[x_0..x_5] -- P^5
  E = ZZ/101[e_0..e_5, SkewCommutative => true]
  F = random(S^2, S^{-3,-3})
  I = ideal F
  M = S^1/I
  m = bgg(2,M,E);
  time gens gb m;
  gbTrace=0
  time C1 = nonminimalResolution(coker m, LengthLimit=>10)
  m = bgg(2,M,E);  
  time C2 = res(coker m, LengthLimit=>8)
  betti C2
  betti C1  

  m = bgg(3,M,E);
  time gens gb m;
  time C1 = nonminimalResolution(coker m, LengthLimit=>10)
  -- the following takes too long for a test
  --  m = bgg(3,M,E);
  -- time C2 = res(coker m, LengthLimit=>8)
  betti C2
  betti C1  
///