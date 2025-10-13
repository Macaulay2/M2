--============================ Tests Sections ===================================--

-- Returns a non-minimal free resolution, for testing purposes.
-- Homogenizes the ideal w.r.t. a new variable, calculates a free resolution
-- using res with option Strategy => Nonminimal, then dehomogenizes
-- Warning: only works for finite fields
freeRes = I -> (
    -- Warning: only works for finite fields
    R := ring I;
    homogvar := local homogvar;
    S := (coefficientRing R)(monoid [gens R, homogvar]);
    Ih := ideal homogenize(sub(gens gb I, S), S_(numgens R));
    C1 := freeResolution(Ih, Strategy => Nonminimal);
    phi := map(R,S,gens R | {1});
    phi C1
    )

-- Runs various sanity checks
testSuit = (I, P, strategy) -> (
    R := ring I;
    if not instance(R, LocalRing) then (
        C := freeRes I;
        assert(C.dd^2 == 0);
        time D := pruneComplex(C, Strategy => strategy, Direction => "left", PruningMap => true);
        assert(D.dd^2 == 0);
        if isHomogeneous I then assert(betti D == betti freeResolution I);
        assert(isCommutative D.cache.pruningMap);
        assert(isQuasiIsomorphism(D, I));
        E := D ** R_P;
        ) else E = freeResolution I;
    RP := ring E;
    assert(E.dd^2 == 0);
    time F := pruneComplex(E, Strategy => strategy, Direction => "left", PruningMap => true);
    return (
        F.dd^2 == 0 and isMinimal F and
        isCommutative F.cache.pruningMap and
        isQuasiIsomorphism(F, ideal(gens I ** RP)))
    )

-- Given a homogeneous module, with a non-minimal resolution, the pruned complex should be homogeneous.
TEST ///
  R = ZZ/32003[x,y,z]
  M = coker random(R^{1, 2, -1}, R^{-1, -2, -3, -4})
  assert isHomogeneous M
  C = freeResolution M
  assert isHomogeneous C
  C' = pruneComplex C
  assert(C'.dd^2 == 0)
  assert isHomogeneous C'
///

TEST ///
  debug needsPackage "PruneComplex"
  R = ZZ/32003[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  C = freeResolution I;
  C' = freeRes I;
  C' = C'[-10]
  C'' = C'[20]
  MC' = toMutableComplex C';
  MC'' = toMutableComplex C'';
  elapsedTime C1 = pruneComplex(C', Strategy => Engine, PruningMap => true); -- TODO: consider using sparse matrices
  assert(isCommutative C1.cache.pruningMap);
  assert(betti C[-10] == betti C1)
  elapsedTime (MC, M) = pruneComplex(MC'', Strategy => null, UnitTest => isScalar, PruningMap => true); -- FIXME very slow
  C2 = toChainComplex MC;
  assert(isCommutative map(C'', C2, i -> M#(i+ min C'' +1)//matrix));
  assert(betti C[10] == betti C2[10])
///

TEST ///
  debug needsPackage "PruneComplex"
  R = ZZ/32003[vars(0..8)]
  M = genericMatrix(R,3,3)
  I = minors(2, M)
  C = freeRes I
  elapsedTime C0 = pruneComplex(C); -- fast
  elapsedTime C1 = pruneComplex(C, UnitTest => isScalar); -- fastest
  elapsedTime C2 = pruneComplex(C, Strategy=>null); -- FIXME far too slow
  elapsedTime C3 = pruneComplex(C, Strategy=>null, UnitTest => isScalar); -- okay
  elapsedTime C4 = minimize C; -- slow
  assert({freeResolution I, C0, C1, C2, C3, C4}/betti//same)

  needsPackage "LocalRings"
  P = ideal gens R
  RP = localRing(R,P)
  IP = promote(I, RP)
  C' = C ** RP
  elapsedTime B0 = betti pruneComplex(C'); -- fastest
  elapsedTime B1 = betti pruneComplex(C', UnitTest => isScalar); -- fast
  elapsedTime B2 = betti pruneComplex(C', Strategy=>null); -- slow
  elapsedTime B3 = betti pruneComplex(C', Strategy=>null, UnitTest => isScalar); -- slow
  assert(same {betti freeResolution IP, B0, B1, B2, B3})
///

TEST ///
  debug needsPackage "PruneComplex"
  R = ZZ/32003[a..f]

  --
  I = ideal"abc-def,ab2-cd2-c,acd-b3-1"
  C = freeRes I -- 1 9 15 8 1

  D = pruneComplex(C, UnitTest => isScalar, Strategy => null, Direction => "whole") -- 1 4 4 1
  assert(D.dd^2 == 0)
  assert(isQuasiIsomorphism(D,I))

  D' = pruneComplex(C, UnitTest => isScalar, Strategy => null, Direction => "both") -- 1 4 4 1
  assert(D'.dd^2 == 0)
  assert(isQuasiIsomorphism(D',I))

  --
  I = ideal"abc-def,ab2-cd2,acd-b3"
  C = freeRes I -- 1 8 13 7 1

  D = pruneComplex(C, UnitTest => isScalar) -- 1 3 4 2
  assert(D.dd^2 == 0)
  assert(isQuasiIsomorphism(D,I))
///

TEST ///
  debug needsPackage "PruneComplex"
  needsPackage "LocalRings"
  R = ZZ/32003[a..f]
  P = ideal gens R

  I = ideal"abc-def,ab2-cd2-c,acd-b3-1"; -- Now: 0.0005 vs. 0.01 -- 1 3 3 1
  assert testSuit(I, P, null)

  I = ideal"abc-def,ab2-cd2,acd-b3"; -- Now: 0.0003 vs 0.008 -- 1 3 4 2
  assert testSuit(I, P, null)

  I = ideal"abc-def,ab2-cd2,acd-d3-a2c"; -- Now: 0.0018 vs. 0.0133 -- 1 3 4 2
  assert testSuit(I, P, null)
///

TEST ///
  debug needsPackage "PruneComplex"
  R = ZZ/32003[x,y,z]

  I = ideal"xyz+z5,2x2+y3+z7,3z5+y5"
  C = freeRes I -- 1 15 38 34 10

  checker = (D, I) -> (
      assert(D.dd^2 == 0);
      assert(isMinimal(D, UnitTest => isScalar));
      assert(isCommutative D.cache.pruningMap);
      assert(isQuasiIsomorphism(D, I));)

  D = pruneComplex(C, PruningMap => true, UnitTest => isScalar, Strategy => Engine) -- 1 3 4 2
  checker(D, I)

  D = pruneComplex(C, PruningMap => true, UnitTest => isScalar, Strategy => Engine, Direction => "right") -- 1 4 5 2
  checker(D, I)

  D = pruneComplex(C, PruningMap => true, Strategy => null, UnitTest => isScalar, Direction => "right") -- 1 7 9 3
  checker(D, I)

  -- FIXME these three strategies return terribly ugly stuff
  D = pruneComplex(C, PruningMap => true, Strategy => null, UnitTest => isScalar, Direction => "left") -- 1 6 9 5 1
  checker(D, I)

  D = pruneComplex(C, PruningMap => true, Strategy => null, UnitTest => isScalar, Direction => "both") -- 1 6 11 6
  checker(D, I)

  D = pruneComplex(C, PruningMap => true, Strategy => null, UnitTest => isScalar, Direction => "best") -- 1 6 9 5 1
  checker(D, I)
///

TEST ///
  debug needsPackage "PruneComplex"
  needsPackage "LocalRings"

  R = ZZ/32003[a..f]
  P = ideal"a,b,c,d,e,f"
  I = ideal"abc-def,ab2-cd2-c,-b3+acd"
  assert testSuit(I, P, null)
  assert testSuit(I, P, Engine)

--  use R
--  setMaxIdeal ideal gens R
--  L = localResolution I
--  L**RP == F
///

TEST ///
  debug needsPackage "PruneComplex"
  needsPackage "LocalRings"
  R = ZZ/32003[a..d]

  I = monomialCurveIdeal(R,{1,3,4})
  P = ideal"a,b,c"
  assert testSuit(I, P, null)
  assert testSuit(I, P, Engine)
  M = ideal"a,b,c,d"
  assert testSuit(I, M, null)
  assert testSuit(I, M, Engine)

--  use R
--  setMaxIdeal ideal gens R
--  L = localResolution I
--  assert(isQuasiIsomorphism(L**RM, E'))
///

TEST /// -- An example for comparison with Singular
  needsPackage "LocalRings"
  R = ZZ/32003[a..d]
  I = ideal"d3+3bd2+3b2d+b3-7994b3c3-15997b3c3d-8000b3c3d2+15997a3b2c2+16000a3b2c2d-8000a6bc, a2,
  c4-12785b3c2-12785b3c2d+6406b3c2d2-6400b3c2d3-12812a3b2c+12794a3b2cd+12800a3b2cd2+12803a6b-6400a6bd,
  b-15961b2c3-15961b2c3d-15988b2c3d2-16000b2c3d3+15988b3c3-9b3c3d+16000b3c3d2-27a3bc2-18a3bc2d-3a3bc2d2+9a3b2c2+3a3b2c2d-15997a6c-16000a6cd+16000a6bc"
  RP = localRing(R, ideal gens R)
  IP = promote(I, RP)
  C = pruneComplex freeResolution I
  elapsedTime CP = pruneComplex freeResolution IP -- 0.17
///

/// -- Singular code for the above example, this gives a resolution with no real effort.
  -- This is at least partly because the initial ideal in the local ring is (b, a^2, d^3, c^4),
  -- so the Schreyer frame gives a minimal resolution
  ring R1 = 32003,(a,b,c,d),ls;
  ideal I1 = 9*a^8-18*a^5*b*c*d+9*a^2*b^2*c^2*d^2-54*a^5*b*c+54*a^2*b^2*c^2*d+81*a^2*b^2*c^2-3*a^2,
         -3*a^6*c*d+6*a^3*b*c^2*d^2-3*b^2*c^3*d^3-9*a^6*c+36*a^3*b*c^2*d-27*b^2*c^3*d^2+54*a^3*b*c^2-81*b^2*c^3*d-81*b^2*c^3-4*b^3-12*b^2*d-12*b*d^2-4*d^3-2*b,
         -3*a^6*b*d+6*a^3*b^2*c*d^2-3*b^3*c^2*d^3-9*a^6*b+36*a^3*b^2*c*d-27*b^3*c^2*d^2+54*a^3*b^2*c-81*b^3*c^2*d-81*b^3*c^2-5*c^4,
         -3*a^6*b*c+6*a^3*b^2*c^2*d-3*b^3*c^3*d^2+18*a^3*b^2*c^2-18*b^3*c^3*d-27*b^3*c^3-4*b^3-12*b^2*d-12*b*d^2-4*d^3;
  ideal J = std(I1);
  resolution rI = sres(J,0);
  resolution minI = minres(rI);
  print(list(minI));
///

--============================ Engine Tests Sections ===================================--

TEST ///
  debug needsPackage "PruneComplex"
  debug needsPackage "LocalRings"
  R = ZZ/32003[a..f];
  P = ideal gens R;

  I = ideal"abc-def,ab2-cd2,acd-d3-a2c"; -- Now: 0.0018 vs. 0.0133 -- 1 3 4 2
  assert testSuit(I, P, Engine)

  I = ideal"abc-def,ab2-cd2-c,acd-b3-1"; -- Now: 0.0005 vs. 0.01 -- 1 3 3 1
  assert testSuit(I, P, Engine)

  I = ideal"abc-def,ab2-cd2,acd-b3"; -- Now: 0.0003 vs 0.008 -- 1 3 4 2
  assert testSuit(I, P, Engine)

  I = ideal"abc+c5,2a2+b3+c7,3c5+b5" -- Now: 0.0161 vs 0.022 -- 1 3 4 2
  assert testSuit(I, P, Engine)
///

TEST ///
  debug needsPackage "PruneComplex"
  needsPackage "LocalRings"
  R = ZZ/32003[a..d]
  P = ideal"a,b,c"
  RP = localRing(R, P)
  I = ideal(a/d, b/d^2, c/d^2)
  assert testSuit(I, P, null)
  assert testSuit(I, P, Engine)
///

TEST ///
  debug needsPackage "PruneComplex"
  needsPackage "LocalRings"
  R = ZZ/32003[x,y,z]
  I = ideal"xyz+z5,2x2+y3+z7,3z5+y5"
  C = freeRes I -- 1 15 38 34 10
  D = pruneComplex(C, PruningMap => true) -- 1 3 4 2
  f = D.cache.pruningMap;

  assert(D.dd^2 == 0)
  assert(isCommutative f)
  assert(isQuasiIsomorphism f)
  assert(isMinimal(D, UnitTest => isScalar));

  h = nullHomotopy(f, FreeToExact => true)
  f' = h * D.dd + C.dd * h;
  assert(isCommutative f')
  assert(nullHomotopy(f - f', FreeToExact => true) == 0)
  -- TODO what does this tell me? hahaha
--  prune HH f -- what to do with this?

  RP = localRing(R, ideal gens R)
  C' = C ** RP
  D' = pruneComplex(C', PruningMap => true) -- 1 3 3 1
  g = D'.cache.pruningMap;

  assert(D'.dd^2 == 0)
  assert(isCommutative g)
  assert(isQuasiIsomorphism g)
  assert(isMinimal D');

--  h' = nullhomotopy(g) -- FIXME what does it mean that h' is zero?
--  g' = h' * D'.dd + C'.dd * h';
--  assert(isCommutative g')
--  assert(nullhomotopy(g - g') == 0)
--  prune HH f -- To slow, probably why isQuasiIsomorphism is slow
///

end--

-------------------------------------------------------------------------------------------------------
-- Stress Test for Pruning Complexes in the Engine
restart
debug needsPackage "PruneComplex"
needsPackage "RandomIdeals"
R = ZZ/32003[a..f];

randomCombo = (n, d) -> (
--    ideal for i from 1 to n list (random(2, R) - random(3, R))
    L := (randomMonomialIdeal(apply(2 + random 3, i-> 2 + random d), R))_*;
    ideal apply(n, i -> sum(L, j -> random(coefficientRing ring L#0) * j + random 3))
    )

(nstep, flag) = (-1, 1025);
(i, m) = (0, 50); time tally while i < m list (
    I = randomCombo(3, 10);
    try ( alarm (1); testEngine I; )
    then (i = i + 1) else ( print "Lookit!"; break; );
    );

-------------------------------------------------------------------------------------------------------
--  Development stuff
--  path = prepend("~/src/m2/M2-local-rings/M2/Macaulay2/packages/", path) -- Mike
restart
needsPackage "PruneComplex"
elapsedTime check PruneComplex -- 18 sec

restart
uninstallPackage "PruneComplex"
restart
installPackage "PruneComplex"
viewHelp "PruneComplex"
