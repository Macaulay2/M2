-- FFPACK:
--  copy tarfile to uiuc.
--  d/version.dd: version.
--  d/startup.m2.in: copyright.
--  fix Makefile to point to that file.
--  doc node: linked from "Copyright and license"
--
--  configure script
--  FFPACK enable define.

-- Tests for free resolutions

-- Current caveats:
--   a. REMOVED need to give Weights: first wt vector is the degree vector
--   b. DONE need to pass in the coker (or ideal) of a groebner basis (in res.m2)
--   c. REMOVED the elements at level 1 must be inserted in order of monotone increasing component
--   d. TODO!! if the input free module is a Schreyer order: need to handle that
--   e. DONE, but could improve support.  some module gradings don't seem to work.  Either fix, or check for this.

TEST ///
  -- nonminimal resolution.
  -- test the zero ideal
  R = ZZ/101[a..d]
  I = ideal(0_R)
  C = res(I, FastNonminimal => true)
  betti'ans = new BettiTally from {(0,{0},0) => 1}
  assert(betti C == betti'ans)
  assert(C.dd_1 == 0)
  assert(length C == 0)

  assert(betti'ans == minimalBetti (ideal I_*))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>5))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>0))
  assert((new BettiTally from {}) === minimalBetti (ideal I_*, LengthLimit=>-1))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>1000))  
  assert(betti'ans == minimalBetti (ideal I_*, DegreeLimit=>1000))  
  assert(betti'ans == minimalBetti (ideal I_*, DegreeLimit=>-1000))

  I = trim ideal(0_R)
  C = res(I, FastNonminimal => true)
  betti'ans = new BettiTally from {(0,{0},0) => 1}
  assert(betti C == betti'ans)
  assert(C.dd_1 == 0)
  assert(length C == 0)
  assert(C.dd^2 == 0)

  I = trim ideal(0_R)
  assert(betti'ans == minimalBetti I)

  I = trim ideal(0_R)
  assert(betti'ans == minimalBetti (I, LengthLimit=>5))

  I = trim ideal(0_R)
  assert(betti'ans == minimalBetti (I, LengthLimit=>0))

  I = trim ideal(0_R)
  assert((new BettiTally from {}) == minimalBetti (I, LengthLimit=>-1))

  I = trim ideal(0_R)
  assert(betti'ans == minimalBetti (I, LengthLimit=>1000))  

  I = trim ideal(0_R)
  assert(betti'ans == minimalBetti (I, DegreeLimit=>1000))  

  I = trim ideal(0_R)
  assert(betti'ans == minimalBetti (I, DegreeLimit=>-1000)) 

///

TEST ///
  -- nonminimal resolution.
  -- test the unit ideal
  R = ZZ/101[a..d]
  I = ideal(1_R)
  C = res(I, FastNonminimal => true)
  betti'ans = new BettiTally from {}
  assert(betti(C,Minimize=>true) == betti'ans)
  assert(C.dd_1 == 1)
  assert(length C == 1)
  assert(C.dd^2 == 0)
  
  assert(betti'ans == minimalBetti (ideal I_*))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>5))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>0))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>-1))
  assert(betti'ans == minimalBetti (ideal I_*, LengthLimit=>1000))  
  assert(betti'ans == minimalBetti (ideal I_*, DegreeLimit=>1000))  
  assert(betti'ans == minimalBetti (ideal I_*, DegreeLimit=>-1000))
///

TEST ///
  -- nonminimal resolution.
  -- test linear ideals.
  R = ZZ/101[a..d]
  I = ideal(a-d, b+13*a)
  C = res(I, FastNonminimal => true)
  betti'ans = new BettiTally from {(1,{1},1) => 2, (0,{0},0) => 1, (2,{2},2) => 1}
  assert(betti(C,Minimize=>true) == betti'ans)
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
  C = res(ideal(I_*), FastNonminimal => true)
  assert(B1 == betti C)
  B1 = betti res (ideal(I_*), LengthLimit=>7)
  B2 = betti res(ideal(I_*), FastNonminimal => true, LengthLimit=>7)
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
  C = res(ideal(I_*), FastNonminimal=>true)
  B2 = betti(C, Minimize=>true)
  B3 = minimalBetti ideal(I_*)
  assert(B1 == B2)
  assert(B1 == B3)
  betti C
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
  assert try(res(ideal(I_*), FastNonminimal => true); false) else true
  
  -- don't allow quotient rings
  S = ZZ/101[a..d]
  I = ideal(a^2-b*c, b^2-c*d)
  R = S/I
  M = coker vars R
  assert (try (res(M, FastNonminimal => true); false) else true) 
  
  -- don't allow Weyl algebras

  -- don't allow zero heft vectors
  -- this code never gets to non minimal res code in the engine.
  S = ZZ/101[a,b,c,d,Degrees=>{-1,1,2,-5}]  
  assert try (res((ideal gens S)^3, FastNonminimal => true); false) else true;
  
  -- don't allow multi-gradings  
  S = ZZ/101[a,b,c,d,DegreeRank=>4]
  assert try (res(ideal gens S, FastNonminimal => true); false) else true
///

TEST ///
  R = ZZ/101[a..d]
  M = coker vars R
  gbTrace = 1
  C = res(M, FastNonminimal => true)
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
  C = res(M, FastNonminimal => true)
  assert(betti res M == betti C)
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)  

  M2 = coker map(F,,{{a,0,0},{0,b,0},{0,0,c}})
  assert(minimalBetti M2 == betti C) -- doesn't always hold, but does here.
  assert(minimalBetti M2 == betti(C, Minimize=>true))
  assert isHomogeneous C
///

TEST ///
  R = ZZ/101[a..d]
  F = R^{0,-2,-1}
  M = coker map(F,,{{a,0,0},{0,0,b},{0,c,0}})
  M1 = coker map(F,,{{a,0,0},{0,0,b},{0,c,0}})
  isHomogeneous M  
  C = res(M, FastNonminimal => true)
  betti res M1 == betti C
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)  
  assert isHomogeneous C
  assert(betti(C, Minimize=>true) == betti C)
///

TEST ///
  R = ZZ/101[a..e]
  I = monomialCurveIdeal(R,{1,3,7,9})
  C = res(I, FastNonminimal => true)
  assert(betti(C, Minimize=>true) == betti res (ideal I_*))
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
///

///
  -- memory leak test, not run in general.
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  elapsedTime for i from 1 to 1000 do (
      J = ideal I_*;
      res J;
      )
  debug Core
  engineMemory()

  elapsedTime for i from 1 to 1000 do (
      J = ideal I_*;
      res(J, Strategy=>0);
      )
  debug Core
  engineMemory()

  elapsedTime for i from 1 to 1000 do (
      J = ideal I_*;
      res(J, Strategy=>2);
      )
  debug Core
  engineMemory()

  elapsedTime for i from 1 to 1000 do (
      J = ideal I_*;
      res(J, Strategy=>3);
      )
  debug Core
  engineMemory()

  elapsedTime for i from 1 to 1000 do (
      J = ideal I_*;
      res(J, Strategy=>4);
      )
  debug Core
  engineMemory()
  J = null
  collectGarbage()
  engineMemory()  

  elapsedTime for i from 1 to 10 list (
      J = ideal I_*;
      res(J, Strategy=>4)
      );
  debug Core
  engineMemory()
///

TEST ///  
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  elapsedTime C = res(I, FastNonminimal => true)
  elapsedTime  assert(betti(C,Minimize=>true) == betti res (ideal I_*))
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
  betti C

  elapsedTime  res(ideal(I_*), Strategy=>0)
  elapsedTime  res(ideal(I_*), Strategy=>1)
  elapsedTime  res(ideal(I_*), Strategy=>2)
  elapsedTime  res(ideal(I_*), Strategy=>3)

  betti(C, Minimize=>true)
  debug Core
  assert(rawBetti(raw C.Resolution, 1) == betti(C))
  assert(rawBetti(raw C.Resolution, 0) == betti C)
  assert(rawBetti(raw C.Resolution, 4) == betti(C, Minimize=>true))
  rawBetti(raw C.Resolution, 5)
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
  elapsedTime C = res(I, FastNonminimal => true)
  C1 = res (ideal I_*)
  assert(betti C == betti C1)
  for i from 2 to length C do assert(C.dd_(i-1) * C.dd_i == 0)
  assert(betti(C, Minimize=>true) == betti C)
///

TEST ///  
  kk = ZZ/101
  R = kk[a..f]
  I = ideal(a*b*c-d*e*f, a*b^2-d*c^2, a*e*f-d^2*b)
  gbTrace=3
  elapsedTime C = res(I, FastNonminimal => true)
  betti(C, Minimize=>true) == betti res ideal(I_*)
  betti(C)
  I = ideal I_*
  C1 = res(ideal gens gb I, Strategy=>1)
  debug Core
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
  C = res(I, FastNonminimal => true)
  
  I = ideal(I_*)
  elapsedTime C1 = res(I, FastNonminimal => true, DegreeLimit=>1) -- DOES NOTHING (i.e. does the whole thing)
----  assert(betti(C,Minimize=>true) != betti(C1,Minimize=>true)) -- totally non-minimal, so maybe it did do something. ACTUALLY: returns without doing ranks
  betti C1
  elapsedTime C2 = res(I, FastNonminimal => true)
  betti C2 == betti C
  assert(C.dd^2 == 0)
  assert(isHomogeneous C)
  C1 = betti res ideal(I_*)
  assert(betti(C,Minimize=>true) == betti(C1,Minimize=>true))
///

TEST ///  
  kk = ZZ/101
  nvars = 9
  R = kk[vars(0..nvars-1)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  gbTrace=2
  elapsedTime C = res(I, FastNonminimal => true)
  betti(C, Minimize=>true)
  betti C
///

///
  -- disabled since it takes too long
  kk = ZZ/101
  nvars = 13
  R = kk[vars(0..nvars-1)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  gbTrace=2
  elapsedTime C = res(I, FastNonminimal => true) -- 49.39 seconds on MBP
///

///  
  -- disables since it is right on the edge of limits, so sometimes fails sometimes succeeds.
  kk = ZZ/101
  R = kk[vars(0..10)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  elapsedTime C = res(I, FastNonminimal => true)
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
///

///
  kk = ZZ/101
  R = kk[x_1..x_20]
  I = Grassmannian(2,5,R)
  gbTrace=2
  elapsedTime C = res(I, FastNonminimal => true)
  betti(C, Minimize=>true)
///

///
  kk = ZZ/101
  R = kk[x_1..x_21]
  I = Grassmannian(1,6,R)
  elapsedTime C = res(I, FastNonminimal => true)
  betti(C, Minimize=>true)
///

-------------------------------------
-- Test resolutions of modules ------
-------------------------------------
TEST ///
  R = ZZ/101[vars(0..7)]
  M = genericMatrix(R,a,2,4)
  I = minors(2,M)
  N = syz gens I
  C = res(coker N, FastNonminimal => true)
  assert(betti(C,Minimize=>true) == betti res coker syz gens I)
  betti C
///

TEST ///
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  C = res(I, FastNonminimal => true)
  betti C
  m3 = C.dd_2;
  gbTrace=0
  
  res(coker C.dd_2, FastNonminimal => true)
  res(coker C.dd_3, FastNonminimal => true)
  res(coker C.dd_4, FastNonminimal => true)
  res(coker C.dd_5, FastNonminimal => true)
  res(coker C.dd_6, FastNonminimal => true)
  res(coker C.dd_7, FastNonminimal => true)
  res(coker C.dd_8, FastNonminimal => true)
  assert(res(coker C.dd_9, FastNonminimal => true) == 0)
  assert(res(coker C.dd_10, FastNonminimal => true) == 0)

  betti C
  
  gensI = schreyerOrder gens gb I
  P = gens gb syz gensI;
  res(coker P, FastNonminimal => true)
///

TEST ///
  R = ZZ/101[a..f]
  m = map(R^0,R^1,0)
  gbTrace = 3
  res(coker m, FastNonminimal => true)
///

TEST ///
  R = ZZ/101[a..f]
  I = ideal(a*b-e*f, a*c*e-f^3, a^2*c*d^2-b^2*e*f^2)
  time betti res I
  elapsedTime C0 = res(ideal I_*, FastNonminimal => true)
  P = gens gb syz gens I

  -- would be nice to get this one to work!
  time try(C1 = res(coker P, FastNonminimal => true); false) else true -- gives error
///

TEST ///
--- XXXX
  setRandomSeed "10"
  R = ZZ/101[a..d]
  F = R^{-2,-3,-4}
  M = map(F,,{{a^2, b^2,c^3},{b, a, a^2},{0, 0, d}})
  --M = map(F,,{{a^2, b^2},{b, a},{0, 0}})
  leadTerm M
  gens gb M
  --P = gens gb syz gens I
  try(betti res(coker M, FastNonminimal => true); false) else true  -- This one gives an error: array is out of order.
///

TEST ///
  setRandomSeed "10"
  R = ZZ/101[a..d]
  I = ideal random(R^1, R^{-2,-3,-4})
  P = gens gb syz gens I
  try (betti res(coker P,  FastNonminimal => true); false) else true  -- This one gives an error: array is out of order.
  
  F = source schreyerOrder gens I
  debug Core
  raw F
  P1 = map(F,,P)
  isHomogeneous P1
  betti(res(coker P1, FastNonminimal => true), Minimize=>true)
  res coker P1 -- this one looks wrong if one diesn't do the line before this?
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
  time C1 = res(coker m, FastNonminimal => true, LengthLimit=>7)
  betti(C1, Minimize=>true)
  m = bgg(2,M,E);  
  time C2 = res(coker m, LengthLimit=>6)
  betti C2
  betti(C1, Minimize => true)

  m = bgg(3,M,E);
  time gens gb m;
  time C1 = res(coker m, FastNonminimal => true, LengthLimit=>7)
  betti(C1, Minimize=>true)
///

TEST ///
  n = 6
  d = 3
  S = ZZ/101[x_1..x_n]
  soc = random(S^1, S^{ -d});
  I = ideal fromDual soc;
  time B = betti res I
  C = res I;
  time F = res(I, FastNonminimal=>true)
  time B == betti(F, Minimize =>true)
  assert(betti F != betti C)
  assert(F =!= C)

  I = ideal I_*;
  F = res(I, FastNonminimal=>true)
  assert(B == betti(F, Minimize =>true))
  C = res I;
  assert(B == betti C)
  
  C1 = res(I, Strategy=>1);
  C2 = res(I, Strategy=>2);
  C0 = res(I, Strategy=>0);
  C4 = res(I, Strategy=>4);
  assert(C2 === C1)
  assert(C0 === C1)
  assert(C4 =!= C1)
  C1 = res(I, Strategy=>1);
  C2 = res(I, Strategy=>2);
  C0 = res(I, Strategy=>0);
  assert(C2 === C1)
  assert(C0 === C1)
  assert(C4 =!= C1)
  assert(C0 == res I)
///