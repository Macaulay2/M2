-- Tests for free resolutions

-- Notes:
--  Tests are here
--  Unit tests for f4 free resolution code are in e/unit-tests/ResTest.cpp
--  packages/NonminimalComplexes: has further routines, doc, and maybe some tests.

-- March 2018 todo list:
-- 1. Allow multi-gradings
--   DONE: get the correct free modules.
--   QUESTION: should we make these Schreyer order free modules?
--   TODO: 
--     (a) given a multidegree, level, find the degree zero map at that degree.
--     (b) find all such multi-degrees
--     (c) given a frame element, find its multi-degree.
--     (d) make a table of all of the possible multi-degrees?  How do we turn that into a betti table?
-- 
--   how: need to make sure we can return the correct free modules in these cases.
--   allow: get degree zero matrices for a specific multi-grading
--          todo list of degree,level includes multi-degrees: maybe?  (heftdeg, multidegree, level).
-- 2. Allow inhomogeneous input
-- 3. Allow modules to be input (several related tests below failing)
--   how: 2,3 are closeely related: need better monomial order support.
--   TODO:
--      (a) try using the monomial order in input matrix.
--      (b) sorting a set of monomials at a specific level:
--         try the following: for each, write down a monomial in the input free module.
--                            sort these (positions of these).
--         given a (lev,index): get a mTotalMonom.  Translate this to a 'monomial' from Monoid.
--         call M->compare()
--         given a whole bunch of monomials:
--           translate to total monoms, make array, sort positions.
--
-- 4. Allow the input free module to be a Schreyer order too.
--   probably very close to (3) too.

-- 5. Allow QQ as coefficients
-- 6. Allow a GB to be provided (DONE), i.e. without recomputing one, or changing it.
-- 7. improve 'rank' computations for sparser matrices.
--
--  Would like monomial types:
--   ExponentVector: range?
--   PackedMonomial (packed from Monoid)
--   SparseMonomial
--   DenseMonomial === NTupleMonomial === ExponentVector
-- none of these have components, or do they?
-- each monomial could have: component, hash value, weight vector values, actual exponents.

-- need a function:
--   PackedMonomial(const Monoid * M, monomial m): lightweight, but let's us know what it is.
--   PackedMonomial(const Monoid * M, Iter begin, Iter end): lightweight, but let's us know what it is.
--
--   template<typename Iter>
--   packedMonomialToNTupleMonomial(const Monoid* M, PackedMonomial m, Iter begin, Iter end)
--   packedMonomialToNTupleMonomial(const Monoid* M, PackedMonomial m, Iter outputIter)
--
-- In any case: we need
--   packedMonomialToNTupleMonomial(const Monoid* M, monomial m, res_ntuple_monomial outputAlreadyAllocated);
--   ntupleMonomialToPackedMonomial(const Monoid* M, res_ntuple_monomial m, monomial outputAlreadyAllocated);



-- Older caveats:
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
  -- Instead, the front end homogenizes the input, calls this, and then dehomogenizes later.
  S = ZZ/101[a..d]
  I = ideal(a*b-a-1, b^3-c*d-3)
  time res I
  res(ideal(I_*), FastNonminimal => true)
  
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
  
  -- allow multi-gradings  
  S = ZZ/101[a,b,c,d,DegreeRank=>4]
  C = res(ideal gens S, FastNonminimal=>true)
  assert isHomogeneous C
  assert(degrees C_0 === {{0, 0, 0, 0}})
  assert(degrees C_1 === {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}})
  assert(degrees C_2 === {{1, 1, 0, 0}, {1, 0, 1, 0}, {0, 1, 1, 0}, {1, 0, 0, 1}, {0, 1, 0, 1}, {0, 0, 1, 1}})
///

TEST ///
   -- another multi-graded ideal
   R = ZZ/32003[a..d, Degrees=>transpose {{1,2,3,4},{1,0,-1,0}}]
   basis(degree(a*b*c*d)^2,R)
   I = ideal for i from 1 to 3 list random(degree((a*b*c*d)^2*c), R)
   C = res(I, FastNonminimal=>true)

   -- need to implement: find all the degrees where C might be not minimal, 
   -- for each: compute rank, fill in a betti table.  Note that this Betti table has multigrading.
   degs = unique join(unique degrees C_1,   unique degrees C_2,   unique degrees C_3)
   
   H = hashTable for deg in degs list deg => {
       positions(degrees C_1, d -> d === deg),
       positions(degrees C_2, d -> d === deg),
       positions(degrees C_3, d -> d === deg)
       }
   select(keys H, d -> #(H#d)#0 > 0 and #(H#d)#1 > 0)
   for d in oo list submatrix(C.dd_2, (H#d)#0, (H#d)#1)
   select(keys H, d -> #(H#d)#1 > 0 and #(H#d)#2 > 0)
   for d in oo list submatrix(C.dd_3, (H#d)#1, (H#d)#2)

   assert isHomogeneous C
   minimalBetti I -- BUG! WRONG: needs to have multi-degrees, not single degrees (even though the main betti numbers are correct)
   pairs betti res I
   -- TODO: get minimalBetti working with multi-gradings.
   -- assert(minimalBetti I === new BettiTally from {((2, {43, -1}, 43), 3), ((3, {63, -1}, 63), 1), ((0, {0, 0}, 0), 1), ((1, {23, -1}, 23), 3)})
   
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

TEST ///
  R = ZZ/101[a..e]
  M = coker random(R^2, R^{-1,-1,-1,-1,-1})
  minimalBetti M
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

///
  kk = ZZ/101
  R = kk[x_1..x_20]
  I = Grassmannian(2,5,R)
  gbTrace=0
  elapsedTime minimalBetti I
  elapsedTime C = res(I, FastNonminimal => true)
  betti(C, Minimize=>true)
///

///
  kk = ZZ/101
  R = kk[x_1..x_21]
  I = Grassmannian(1,6,R)
  elapsedTime minimalBetti I
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
  minimalBetti coker N
  C = res(coker N, FastNonminimal => true)
  
  assert(betti(C,Minimize=>true) == betti res coker syz gens I)
  betti C
///

TEST ///
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  elapsedTime minimalBetti I
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

  C1 = res(coker P, FastNonminimal => true)
  B1 = minimalBetti coker P
  B2 = betti res coker P
  assert(B1 == B2)
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
  P = gens gb image M
  C = res(coker P, FastNonminimal => true)
  assert(minimalBetti coker P == betti res coker P)
///

TEST ///
  setRandomSeed "10"
  R = ZZ/101[a..d]
  I = ideal random(R^1, R^{-2,-3,-4})
  P = gens gb syz gens I
  betti res(coker P,  FastNonminimal => true)
  minimalBetti coker P
  
  F = source schreyerOrder gens I
  debug Core
  raw F
  P1 = map(F,,P)
  isHomogeneous P1
  betti(res(coker P1, FastNonminimal => true), Minimize=>true)
  res coker P1 -- this one looks wrong if one does not do the line before this? (MES: I don't see any issue here 3/2018)
  -- ?? is this a bug??
///

TEST ///
  setRandomSeed "10"
  needsPackage "BGG"
  S = ZZ/101[x_0..x_5] -- P^5
  E = ZZ/101[e_0..e_5, SkewCommutative => true]
  F = random(S^2, S^{-3,-3})
  I = ideal F
  M = S^1/I
  m = bgg(2,M,E);
  elapsedTime gens gb m;
  gbTrace=0
  elapsedTime C1 = res(coker m, FastNonminimal => true, LengthLimit=>7)
  B1 = minimalBetti(coker m, LengthLimit=>7)
  B2 = betti res(coker m)
  assert(B1 == B2)

  m = bgg(2,M,E);  
  elapsedTime C2 = res(coker m, LengthLimit=>6)
  elapsedTime minimalBetti(coker m, LengthLimit=>6)
  assert(betti C2 == oo)
  betti(C1, Minimize => true)
  elapsedTime res(coker m, FastNonminimal=>true, LengthLimit=>7)
  betti oo
  
  m = bgg(3,M,E);
  elapsedTime gens gb m;
  elapsedTime B1 = minimalBetti(coker m, LengthLimit=>7) -- 1.4 sec
  elapsedTime B2 = betti res(coker m) -- 27 seconds
  time C1 = res(coker m, FastNonminimal => true, LengthLimit=>7) -- .65 sec if done without minimalBetti line.
  assert(B1 == B2)
///


TEST ///
  -- this is a small-ish example used to get the logic of matrix building right
  setRandomSeed 0
  kk = ZZ/101
  R = kk[vars(0..3)]
  I = ideal fromDual random(R^1, R^{-3});
  C = res(I, FastNonminimal => true)
  
  I = ideal(I_*)
  elapsedTime C1 = res(I, FastNonminimal => true, DegreeLimit=>1) -- DOES NOTHING (i.e. does the whole thing) BUG
----  assert(betti(C,Minimize=>true) != betti(C1,Minimize=>true)) -- totally non-minimal, so maybe it did do something. ACTUALLY: returns without doing ranks
  betti C1
  elapsedTime C2 = res(I, FastNonminimal => true)
  betti C2 == betti C
  assert(C.dd^2 == 0)
  assert(isHomogeneous C)
  C1 = betti res ideal(I_*)
  assert(betti(C,Minimize=>true) == betti(C1,Minimize=>true))
  assert(minimalBetti I == betti C1)
///

TEST ///  
  kk = ZZ/101
  nvars = 9
  R = kk[vars(0..nvars-1)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  gbTrace=2
  elapsedTime C = res(I, FastNonminimal => true)
  minimalBetti I
  betti(C, Minimize=>true)
  betti C
///

TEST ///
  -- Construction of AGR examples.
  -- See also AGRExamples.m2 in git repo with Frank.
  randomForm = method()
  randomForm(ZZ, Ring) := (d,R) -> (
    B := basis(d,R);
    n := numColumns B;
    kk := coefficientRing R;
    rands := if kk === QQ then (
      for i from 0 to n-1 list (round(100 * (-1 + random(2.0))))/100
      )
    else for i from 0 to n-1 list random kk;
    (B * transpose matrix{rands})_(0,0)
    )
  AGR = method()
  AGR(ZZ,ZZ,ZZ,ZZ) := (d,n,s,p) -> (
    kk := if p == 0 then QQ else ZZ/p;
    R := kk[vars(0..n)];
    F := sum(gens R, x -> x^d) + sum(s-(n+1), i -> (randomForm(1,R))^d);
    --F := sum for i from 1 to s list (randomForm(1, R1))^d;
    ideal fromDual matrix{{F}}
    )
  AGR(ZZ,ZZ,ZZ) := (d,n,p) -> (
    kk := if p == 0 then QQ else ZZ/p;
    R := kk[vars(0..n)];
    F := randomForm(d, R);
    ideal fromDual matrix{{F}}
    )

  setRandomSeed 0
  I = AGR(4,9,8,101);
  I = AGR(4,9,12,101);
  I = AGR(4,9,7,101);
  I = AGR(3,12,15,101);
  I = AGR(3,11,15,101);
  R = ring I  

  -- this test takes too much memory.
  stderr << "--EngineTests/Res.f4.m2: *** Warning: bypassing a test that takes too much memory." << endl;
  exit 0

  elapsedTime C = res(I, FastNonminimal=>true)
  gbTrace=2
  elapsedTime minimalBetti I

///
-* TEST *- ///
  -- takes too much memory
  -- might take too long ...
  kk = ZZ/101
  nvars = 13
  R = kk[vars(0..nvars-1)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  gbTrace=2
  elapsedTime C = res(I, FastNonminimal => true) -- 49.39 seconds on MBP, 11.3 seconds in 2018
  elapsedTime minimalBetti I  -- 75 sec in 2018

  debug needsPackage "NonminimalComplexes"  
  elapsedTime Cd = constantStrand(C, kk, 8) -- 1.7 sec
  Mk = Cd.dd_7;
  M = Mk ** R;
  elapsedTime M' = transpose M;
  elapsedTime Mk' = transpose Mk;
  -- over R:
  elapsedTime gens gb M; -- >= 140 sec
  elapsedTime gens gb M'; -- >= 108 sec
  -- over kk:
  elapsedTime gens gb Mk; -- >= 104  sec
  elapsedTime gens gb Mk'; --.01  sec

  debug Core
  kkp = ZZp(101, Strategy=>"Ffpack") -- use this ring for M1
  debug Core
  comp = fastNonminimalComputation C
  elapsedTime M1 = rawResolutionGetMutableMatrix2B(comp, raw kk, 8, 7);
  M1 = map(kk,M1);
  elapsedTime M2 = sub(M1,kkp); -- uugh, very slow. crashed because of memory usage.
  
  elapsedTime rank M1
  (numRows M1, numColumns M1)
  class M1
  ring M1
  
///

-* TEST *- ///  
  -- disabled since it is right on the edge of limits, so sometimes fails sometimes succeeds.
  kk = ZZ/101
  R = kk[vars(0..10)]
  setRandomSeed 0
  I = ideal fromDual random(R^1, R^{-3});
  elapsedTime C = res(I, FastNonminimal => true)
  elapsedTime B1 = minimalBetti I
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
  assert(B1 == betti'ans)
///

TEST ///
  n = 6
  d = 3
  S = ZZ/101[x_1..x_n]
  soc = random(S^1, S^{ -d});
  I = ideal fromDual soc;
  time C = res I
  time B = betti res I
  time F = res(ideal I_*, FastNonminimal=>true)
  time B == betti(F, Minimize =>true)
  assert(betti F != betti C)
  assert(F =!= C)

  F = res(ideal I_*, FastNonminimal=>true)
  assert(B == betti(F, Minimize =>true))
  C = res I;
  assert(B == betti C)

  I = ideal I_*;
  C1 = res(I, Strategy=>1);
  C2 = res(I, Strategy=>2);
  C0 = res(I, Strategy=>0);
  C4 = res(I, Strategy=>4);
  assert(C2 === C1)
  assert(C0 === C1)
  assert(C4 === C1)

  I = ideal I_*;
  C4 = res(I, Strategy=>4);
  C1 = res(I, Strategy=>1);
  C2 = res(I, Strategy=>2);
  C0 = res(I, Strategy=>0);
  assert(C2 === C1)
  assert(C0 === C1)
  assert(C4 =!= C1)
  assert(C0 == res I)
///

///
  restart
  debug needsPackage "NonminimalComplexes"
  kk = ZZ/101
  R = kk[vars(0..14)]
  M = genericMatrix(R,a,3,5)
  I = minors(2,M)
  minimalBetti I
  gbTrace=2
  --minimalBetti I^2
  J = I^2;
  elapsedTime C = res(J, FastNonminimal=>true)
  elapsedTime minimalBetti J
  gbTrace=0
  elapsedTime M = submatrixByDegrees(C.dd_3, {6}, {6});
  elapsedTime Mk = sub(M,kk);
  elapsedTime C6 = constantStrand(C, kk, 6)
  assert(Mk == C6.dd_3)
  -- over R:
  elapsedTime gens gb M;
  elapsedTime M' = transpose M;
  elapsedTime gens gb M';
  elapsedTime gens gb (transpose M');
  time rank M -- very slow...
  elapsedTime rank Mk -- slow
  elapsedTime gens gb Mk;
  elapsedTime Mk' = transpose Mk;
  elapsedTime rank Mk' -- slow
  elapsedTime gens gb Mk';

  elapsedTime M = submatrixByDegrees(C.dd_4, {7}, {7});
  elapsedTime C7 = constantStrand(C, kk, 7)
  Mk = C7.dd_4;
  M = Mk ** R;
  elapsedTime M' = transpose M;
  elapsedTime Mk' = transpose Mk;
  -- over R:
  time rank M -- very slow... (over 200 times slower than 'gens gb M'
  time rank M' -- very fast.
  elapsedTime gens gb M; -- faster than "gens gb M'"
  elapsedTime gens gb M';
  -- over kk:
  elapsedTime gens gb Mk;
  elapsedTime gens gb Mk';
  time rank Mk -- very slow... 
  time rank Mk'

  elapsedTime C7 = constantStrand(C, kk, 8) -- 4.02 sec IMPROVED THIS!
  Mk = C7.dd_5;
  M = Mk ** R;
  elapsedTime M' = transpose M;
  elapsedTime Mk' = transpose Mk;
  -- over R:
  time rank M -- very slow... 52 sec
  time rank M' -- very fast... .295 sec
  elapsedTime gens gb M; -- faster than "gens gb M'"
  elapsedTime gens gb M';
  -- over kk:
  elapsedTime gens gb Mk;
  elapsedTime gens gb Mk';
  time rank Mk -- very slow... 41 sec
  time rank Mk' -- also slow, since it is being done by dense methods: 53 sec

  elapsedTime C9 = constantStrand(C, kk, 9) -- 4.86 sec
  Mk = C9.dd_6;
  M = Mk ** R;
  elapsedTime M' = transpose M;
  elapsedTime Mk' = transpose Mk;
  -- over R:
  time rank M -- very slow...
  time rank M' -- very fast... .36 sec
  elapsedTime gens gb M; -- .32 sec
  elapsedTime gens gb M'; -- .41 sec
  -- over kk:
  elapsedTime gens gb Mk; -- .01 sec
  elapsedTime gens gb Mk'; -- .01 sec


  elapsedTime C10 = constantStrand(C, kk, 10) -- 7.8 sec
  Mk = C10.dd_7;
  M = Mk ** R;
  elapsedTime M' = transpose M;
  elapsedTime Mk' = transpose Mk;
  -- over R:
  elapsedTime gens gb M; -- .23 sec
  elapsedTime gens gb M'; -- .29 sec
  -- over kk:
  elapsedTime gens gb Mk; -- .01  sec
  elapsedTime gens gb Mk'; --.01  sec
///

///
  kk = ZZ/101
  R = kk[vars(0..15)]
  M = genericMatrix(R,a,3,4)
  I = permanents(2,M)
  elapsedTime C = res(I, FastNonminimal=>true)
  gbTrace=2
  minimalBetti I

  J = I^2;
  gens gb J;  
  elapsedTime C = res(J, FastNonminimal=>true)
  gbTrace=2
  minimalBetti J
///

///
  -- getting ready to test minimization in multi-gradings.
  -- 
  needsPackage "RandomIdeals"
  kk = ZZ/32003
  R = kk[vars(0..10)]
  I = randomMonomialIdeal(splice{20:4}, R)
  S = kk[vars(0..10), DegreeRank=>11]
  I = sub(I,S)
  C = res(I, FastNonminimal=>true)

  Ds = hashTable for i from 1 to length C list i => set keys tally degrees C_i;
  Es = hashTable for i from 2 to length C list i => toList(Ds#(i-1) * Ds#(i));
  (pairs Ds)/(x -> #x#1)//sum
  (pairs Es)/(x -> #x#1)//sum

  elapsedTime hashTable flatten for i from 2 to length C list (
      for deg in Es#i list (i,deg) => elapsedTime submatrixByDegrees(C.dd_i, deg, deg)
      );

  elapsedTime hashTable flatten for i from 2 to length C list (
      for deg in Es#i list (i,deg) => (
          << "doing (i,deg)=" << (i,deg) << " (" << numRows(C.dd_i) << "," << numColumns(C.dd_i) << ")" << endl;
          elapsedTime submatrixByDegrees(C.dd_i, deg, deg)
          )
      );

  
  hashTable for deg in toList(Ds#2 * Ds#3) list deg => submatrixByDegrees(C.dd_4, deg, deg)

  toList(Ds#3 * Ds#4)
  hashTable for deg in toList(Ds#3 * Ds#4) list deg => submatrixByDegrees(C.dd_5, deg, deg)

  for i from 2 to length C list (
      hashTable for deg in toList(Ds#(i-2) * Ds#(i-1)) list (i,deg) => submatrixByDegrees(C.dd_i, deg, deg)
      )


///
