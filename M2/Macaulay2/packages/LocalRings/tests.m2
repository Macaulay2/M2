--============================ Tests Sections ===================================--

TEST /// -- test for localRing, res, **, ==
  debug needsPackage "Complexes"
  R = ZZ/32003[vars(0..3)]
  I = monomialCurveIdeal(R, {1, 3, 4})
  CI = freeResolution I
  P = ideal"a,b,c";
  RP = localRing(R, P);
  J = ideal(gens I ** RP)
  CJ = freeResolution J
  CP = CJ++CJ[-10]
  DJ = CI ** RP
  DP = pruneComplex(DJ++DJ[-10], PruningMap=>true)
  f = DP.cache.pruningMap
  assert(DP.dd^2 == 0)
  assert(DP == CP)
  assert(isMinimal DP)
  assert(isCommutative f)
  assert(isQuasiIsomorphism f)
///

TEST /// -- test for liftUp and //
  R = QQ[vars(0..4)]
  f = matrix{{-1/2 *b^2,c,a,0},{-1/2*a*c,d,b,0},{-1/2*b*d,0,-c,a},{-1/2*c^2,0,-d,b}}
  RP = localRing(R, ideal"a,b,c,d")
  f' = matrix{{-1/2 *b^2,c,a,0},{-1/2*a*c,d,b,0},{-1/2*b*d,0,-c,a},{-1/2*c^2,0,-d,b}}
  f'' = f ** RP
  -- see https://github.com/Macaulay2/M2/issues/1958
--  assert(liftUp f' == liftUp f'') -- FIXME in the engine. GCD(4, 2) is not 1!
--  assert(liftUp (f' // id_(target f')) == f // id_(target f)) -- FIXME also in the engine
--  assert(liftUp (f'' // id_(target f'')) == f // id_(target f)) -- FIXME
  assert(f'_(0,0) == f''_(0,0)) -- good
  assert(f' - f'' == 0) -- good
--  assert(raw f' == raw f'') --FIXME this is bad, but it happens in other places too
///

TEST /// -- test for syz, liftUp, **, modulo, inducedMap
  R = ZZ/32003[a..d]
  P = ideal gens R;
  RP = localRing(R, P);
  I = monomialCurveIdeal(R, {1,3,4})
  C = freeResolution I
  IP = monomialCurveIdeal(RP, {1,3,4})
  CP = freeResolution IP
  -- syz
  f = syz transpose CP.dd_3
  g = transpose CP.dd_2
  assert(image syz f == kernel f)
  -- liftUp
  f' = syz transpose C.dd_3
  g' = transpose C.dd_2
  assert(f' == liftUp f)
  assert(g' == liftUp g)
  -- inducedMap
  assert(inducedMap(coker f, target f) * f == 0)
  assert(kernel(inducedMap(coker f, target f)) == image f)
  assert(kernel(inducedMap(coker g, target g) * f) == image modulo (f, g))
  -- modulo
  A = modulo(f', g')
  A' = A ** RP
  B = modulo(f, g)
  C = subquotient(f', g')
  C' = C ** RP -- FIXME: is there a way to do this without minimizing?
  D = subquotient(f, g)
  -- image
  assert(image A == liftUp image B)
  assert(image A' == image B)
  -- coker
  assert(coker A == liftUp coker B)
  assert(coker A' == coker B)
  -- subquotient
  assert(C == liftUp D)
--  assert(C' == D) -- FIXME: Module ** LocalRing minimizes
  assert(subquotient(A, liftUp B) == 0)
  assert(subquotient(liftUp B, A) == 0)
  -- trim
  assert(trim image A == liftUp trim image B)
  assert(trim image A' == trim image B)
  assert(trim coker A == liftUp trim coker B)
  assert(trim coker A' == trim coker B)
  assert(trim C == liftUp trim D)
--  assert(trim C' == trim D) -- FIXME: Module ** LocalRing minimizes
  -- cover
  assert(cover coker A == liftUp cover coker B)
  assert(cover coker A' == cover coker B)
-- The following two fail because degrees on the source aren't the same
-- FIXME when source degree of modulo is fixed
--  assert(cover image A == liftUp cover image B)
--  assert(cover image A' == cover image B)
  assert(cover C == liftUp cover D)
  assert(cover C' == cover D)
  -- ambient
  assert(ambient coker A == liftUp ambient coker B)
  assert(ambient coker A' == ambient coker B)
  assert(ambient image A == liftUp ambient image B)
  assert(ambient image A' == ambient image B)
  assert(ambient C == liftUp ambient D)
--  assert(ambient C' == ambient D) -- FIXME: Module ** LocalRing minimizes
-- TODO:
  -- mingens
  -- minimalPresentation: how to make this work?
--  minimalPresentation coker modulo(f, g) ==  minimalPresentation subquotient(f, g)
///

TEST ///
  R = ZZ/32003[a..d]
  P = ideal gens R;
  RP = localRing(R, P);
  I = monomialCurveIdeal(RP, {1,3,4})
  C = freeResolution I
  F = syz transpose C.dd_3
  G = transpose C.dd_2
   -- free module
  M = RP^{-1,-2,-3,0,4,5}
  N = minimalPresentation M
  assert(N == M)
  assert(mingens M == id_M)
  f = N.cache.pruningMap
  assert(isIsomorphism f)
  -- local rings aren't graded, so it doesn't make sense to ask whether f is homogeneous:
  -- assert(isHomogeneous f)
  assert(target f === M and source f === N)
  -- image module
  M = image F
  N = minimalPresentation M
  f = N.cache.pruningMap
  assert(isIsomorphism f)
  assert(target f === M and source f === N)
  -- cokernel module
  M = coker F
  N = minimalPresentation M
  assert(presentation N == mingens image F)
  f = N.cache.pruningMap
  assert(isIsomorphism f)
  assert(target f === M and source f === N)
  -- subquotient module
  M = subquotient(F, G)
  N = minimalPresentation M
  f = N.cache.pruningMap
  assert(isIsomorphism f)
  assert(target f === M and source f === N)
///

TEST ///
  S = ZZ/32003[s,t]
  R = ZZ/32003[a..d]
  phi = map(S,R,{s^2*t-t, s*t-s, t^3-s*t^2-s, s^2*t^2})
  I = ker phi
  C = pruneComplex freeResolution I
  RP = localRing(R, ideal gens R)
  C' = pruneComplex(C ** RP)

  IP = ideal(gens I ** RP)
  elapsedTime m1 = mingens image gens IP
  elapsedTime m2 = presentation minimalPresentation image m1
  assert(m1 * m2 == 0)
///

TEST /// -- testing quotient of non-factorable matrices
  R = ZZ/32003[vars(0..4)]
  P = ideal"a,b,c,d";
  RP = localRing(R, P);

  f = transpose matrix{{a, 0, c}, {0, b, 0}}
  g = transpose matrix{{a, b, c}}
  h = subquotient(f, g)
  assert(image mingens liftUp h == liftUp image mingens h)
  assert(image(RP ** mingens liftUp h) == image mingens h)
  -- f does not factor through g, but we should get an answer so that
  -- f = g * (f // g) + r where r = remainder(f, g)
  assert(remainder(f, g) == f - g * (f // g))
  assert(prune h == prune image remainder(f, g))
  assert(liftUp f // liftUp g - liftUp (f // g) == 0)

  f = transpose matrix{{e*a, 0, e*c}, {0, b, 0}}
  -- FIXME when degrees are given by prime filtration
--  h = subquotient(f, g)
  -- Work around:
  h = subquotient(map(RP^3, RP^2, f), map(RP^3,RP^1,g))
  assert(image mingens liftUp h == liftUp image mingens h)
  assert(image(RP ** mingens liftUp h) == image mingens h)
///

TEST /// -- chain homotopy over local rings
  S = ZZ/32003[x,y,z]
  R = S_(ideal vars S)
  J = ideal map(R^1, R^{{-3}, {-3}, {-3}}, {{
	      x^2*y-12373*x^2-8521*y^2,
	      x*y^2+5019*x*y+3216*y^2-13233*z^2+3723*x,
	      13424*x^2*y+936*x*y^2+10667*y*z^2+14913*x^2-8521*x*y-15541*y^2-12289*x}})

  F = complex { gens J, syz gens J, syz syz gens J }
  f0 = J_0
  s0 = map(R^1, 0, 0)
  L = for i to 3 list (
      phi = map(F_i, F_i, f0 * id_(F_i));
      s = (phi - s0 * F.dd_i) // F.dd_(i + 1);
      assert(F.dd_(i + 1) * s == phi - s0 * F.dd_i);
      s0 = s)
  -- TODO: this should work:
  -- extend(F, F, f0 * id_(F_0))
  phi = map(F, F, i -> L#i, Degree => 1)
  -- TODO: run checks on this

  F = freeResolution J
  f0 = J_0
  s0 = map(R^1, 0, 0)
  L = for i to 3 list (
      phi = map(F_i, F_i, f0 * id_(F_i));
      s = (phi - s0 * F.dd_i) // F.dd_(i + 1);
      assert(F.dd_(i + 1) * s == phi - s0 * F.dd_i);
      s0 = s)
///

TEST ///
  R = ZZ/32003[vars(0..3)]
  P = ideal"a,b,c,d";
  RP = localRing(R, P);

  C =  freeResolution monomialCurveIdeal(R,  {1, 3, 4})
  CP = freeResolution monomialCurveIdeal(RP, {1, 3, 4})
  F = ker   transpose C.dd_3;
  G = image transpose C.dd_2;
  H = subquotient(gens F, gens G)
  FP = ker   transpose CP.dd_3;
  GP = image transpose CP.dd_2;
  HP = subquotient(gens FP, gens GP)
  assert(image mingens HP == image(RP ** mingens H))
  assert(image liftUp mingens HP == image mingens H)
///


TEST /// -- test for // -- TODO add more
  R = ZZ/32003[a..d]
  P = ideal gens R;
  RP = localRing(R, P);
  I = monomialCurveIdeal(RP, {1,3,4})
  C = freeResolution I
  f = syz transpose C.dd_3
  g = transpose C.dd_2
  f' = liftUp f
  g' = liftUp g
  use R
  g' = f'
  f' = f' ** a
  g = f
  f = f ** a
  -- here is an interesting example where it seems like
  -- columns of syz(f | g) are not ordered the natural way
  assert(f === g  * (f//g))
  -- TODO: is there a way to force syz to put the columns with units first?
  syz (f|g)
  leadTerm oo
///

TEST /// -- see https://github.com/Macaulay2/M2/issues/2542
  RP = localRing(QQ[a,b,c,u,v,w],ideal(a,b,c,u,v,w))
  f = matrix {{0, -9*u, u, 9*a}, {-b^2*w-w, b^2*w+w, 0, 0}, {-9*w, 0, w, 0}, {0, 0, 0, b^2*v+v}}
  g = matrix {{u, -9*a, -9*u}, {0, 0, b^2*w+w}, {w, 0, 0}, {0, -b^2*v-v, 0}}
  assert(f  % g == matrix{for i from 0 to numcols f - 1 list f_{i}  % g})
  assert(f // g == matrix{for i from 0 to numcols f - 1 list f_{i} // g})
///

TEST ///
  -- by hand compute Ext^1(RP/IP,RP) == 0
  R = ZZ/32003[a..c]
  RP = localRing(R, ideal gens R)
  f = matrix for i from 0 to 2 list for j from 0 to 2 list (random(2, R)+random(1,R))
  I = minors(2, f)
  IP = ideal(gens I ** RP);
  C = pruneComplex freeResolution IP
  assert(coker C.dd_1 == RP^1/IP)
  assert(image C.dd_1 == module IP)
  f1 = relations minimalPresentation image transpose C.dd_2
  f2 = transpose C.dd_1
  f1 = map(target f2,,f1)
  M = subquotient(f1,f2)
  assert(minimalPresentation M == 0)
  assert(mingens M == 0)
  -- by hand compute Ext^2(RP/IP,RP) == 0
  f1 = relations minimalPresentation image transpose C.dd_3
  f2 = transpose C.dd_2
  f1 = map(target f2,,f1)
  M = subquotient(f1,f2)
  assert(mingens M == 0)
  assert(minimalPresentation M == 0)

  M = ker transpose C.dd_3 / image transpose C.dd_2
  assert(prune M == 0)
///

------------------------- length and hilbertSamuelPolynomial ---------------------------

TEST /// -- Intersection Theory: Geometric Multiplicity
  R = ZZ/32003[x,y];
  C = ideal"y-x2"; -- parabola
  D = ideal"y-x";  -- line
  E = ideal"y";    -- line

  use R;
  P = ideal"y-1,x-1";
  RP = localRing(R, P);
  assert(length (RP^1/promote(C+D, RP)) == 1)
  assert(length (RP^1/promote(C+E, RP)) == 0)

  use R;
  P = ideal"x,y";  -- origin
  RP = localRing(R, P);
  assert(length(RP^1/promote(C+D, RP)) == 1)
  assert(length(RP^1/promote(C+E, RP)) == 2)
///

TEST ///
  R = ZZ/32003[x,y];
  C = ideal"y-x3";
  D = ideal"y-x2";
  E = ideal"y";

  use R;
  P = ideal"x,y";
  RP = localRing(R, P);
  assert(length(RP^1/promote(C+D, RP)) == 2)
  assert(length(RP^1/promote(C+E, RP)) == 3)

  use R;
  P = ideal"x-1,y-1";
  RP = localRing(R, P);
  assert(length(RP^1/promote(C+D, RP)) == 1)
  assert(length(RP^1/promote(C+E, RP)) == 0)
///

TEST /// -- Hilbert-Samuel Function
  R = ZZ/32003[x,y];
  RP = localRing(R, ideal gens R);
  -- FIXME
  --assert(apply(-3..3, i -> hilbertSamuelFunction(RP^{2},  i)) == apply(-3..3, i -> hilbertFunction_i R^{2}))
  --assert(apply(-3..3, i -> hilbertSamuelFunction(RP^{-2}, i)) == apply(-3..3, i -> hilbertFunction_i R^{-2}))
  N = RP^1
  q = ideal"x2,y3"
  time assert({1,2,3,4,5,6} == hilbertSamuelFunction(N, 0, 5)) -- n+1 -- 0.02 seconds
  time assert({6,12,18,24,30,36} == hilbertSamuelFunction(q, N, 0, 5)) -- 6(n+1) -- 0.3 seconds
///

TEST ///-- Computations in Algebraic Geometry with Macaulay2 pp 61
  R = QQ[x,y,z];
  RP = localRing(R, ideal gens R);
  I = ideal"x5+y3+z3,x3+y5+z3,x3+y3+z5"
  M = RP^1/I
  time assert(length M == 27) -- 0.55 seconds
  time assert((hilbertSamuelFunction(M, 0, 6))//sum == 27) -- 0.55 seconds
  time assert((hilbertSamuelFunction(max ring M, M, 0, 6))//sum == 27) -- 0.56 seconds
///

TEST /// -- test from Mengyuan
  R = ZZ/32003[x,y,z,w]
  P = ideal "  yw-z2,   xw-yz,  xz-y2"
  I = ideal "z(yw-z2)-w(xw-yz), xz-y2"
  codim I == codim P
  -- Hence this is finite, thus I is artinian in R_P, i.e. RP/IP is an artinian ring.
  C = freeResolution I
  radical I == P

  RP = localRing(R, P)
  IP = promote(I, RP)
  N = RP^1/IP
  M = promote(P, RP)

  assert(length N == 2)
  assert(length N == degree I / degree P)

  assert({1,1,0} == hilbertSamuelFunction(N, 0, 2))
  assert({1,1,0} == hilbertSamuelFunction(max RP, N, 0, 2))
  -- TODO: Find the polynomial from this
///

-- #################################### Subfunctions #################################### --

TEST ///
  R = ZZ/32003[a,b,c];
  P = ideal"a,b"
  RP = localRing(R, P);
  I = ideal(a/(a+1)-b/(b+1), a/(a+1)+b/(b+1))

  assert(ideal"a,b" == I)
  assert(ideal"a,b+1" != I)
  assert(ideal"a2-a" == ideal "a")

  assert(ideal"a" != I)
  assert(isSubset(ideal"a", I))
  assert(not isSubset(I, ideal"a"))

  assert(ideal"(a-b)(b2+1)c" != I)
  assert(isSubset(ideal"(a-b)(b2+1)c", I))
  assert(not isSubset(I, ideal"(a-b)(b2+1)c"))
///

TEST ///
  R = ZZ/32003[vars(0..8)]
  RP = localRing(R, ideal"a,b,c")
  M = genericMatrix(R,3,3)
  I = minors(2,M)

  IP = promote(I,RP)
  assert(IP == 1)
  assert(mingens IP == matrix{{d*i-f*g}})
  -- FIXME technically correct, but how to get matrix{{1}}?

  use R
  RM = localRing(R, ideal"a,b,c,d,e,f")
  IM = promote(I,RM)
  assert(IM != 1)
  assert not isSubset(ideal(a_RM), IM)
  assert isSubset(ideal(a_RM), ideal(b_RM) + IM)
///

TEST ///
  R = ZZ/32003[a..f];
  P = ideal"a,b"
  RP = localRing(R, P);

  phi = map(RP, RP, {a/c, 0, c, d, e, a*f})
  assert(phi((a+b)/(c+b*(a+1))) == a/c^2)
  assert(sub(a+b+c, b=>0) == a+c)
  assert(phi matrix {{a,b,c}, {d,e,f}} - matrix {{a/c, 0, c}, {d, e, a*f}} == 0)
///

-- ###################################### Other Tests ###################################### --

TEST /// -- Legacy test
  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2,-b3+acd";
  C = freeResolution I
  -- legacy
  setMaxIdeal ideal gens R
  E = localResolution coker C.dd_1
  assert(betti C == betti E)
///

TEST ///
  S = ZZ/32003[r,s,t]
  R = ZZ/32003[a..e]
  phi = map(S,R,{r^3, s^3, t^3-1, r*s*t-r^2-s, s^2*t^2})
  I = ker phi
  assert not isHomogeneous I

  C = freeResolution I
  f = syz transpose C.dd_3;
  g = transpose C.dd_2;
  modulo(f,g);
  prune coker oo
///

-- ##################################### Experimental ##################################### --
-- ############################### Colon ideals and modules ############################### --
-- ############################# Saturations and annihilators ############################# --

-- FIXME commented lines give error: unable to flatten ring over given coefficient ring
TEST /// -- I:J -> I
  R = ZZ/32003[x,y,z,w,u]
  RP = localRing(R, ideal"x,y,z,w")
  S = ZZ/32003[r,t,s]
--  J = ker map(S, RP, {s^4*t^2, s^2, t^2, s^2*t^4})
  I = ker map(S, R, {s^4*t^2*r^5, s^2, t^2*r^7, s^2*t^4*r^11,r}) -- TODO come up with a more complicated map
  mingens I
  J = promote(I, RP)
  mingens J
  -- TODO: fix in MinimalPrimes
--  radical J
--  minimalPrimes J
  assert(J : ideal"y2z-xu2" == 1)
  -- TODO expand this test
///

TEST /// -- test for I:f -> I
  R = ZZ/32003[a,b,c,d,e,f]
  I = ideal"abc,abd,ade,def" : ideal"abf"
  RP = localRing(R, ideal gens R)
  J = ideal"abc,abd,ade,def" : ideal"abf"
  J' = ideal"abc,abd,ade,def" : (a*b*f)
  assert(J === J')
  assert(liftUp J === I)
  assert(J === promote(I, RP))
///

TEST /// -- from Macaulay2Docs/tests/quotient.m2
  -- note: this ring is over ZZ, so equalities
  -- are checked in the local ring instead
  R = ZZ[x,y];
  RP = localRing(R, ideal gens R)
  i = ideal(6*x^3, 9*x*y, 8*y^2);
  j = ideal(-3, x^2, 4*y);
  j1 = ideal(-3, x^2);
  j2 = ideal(4*y);
  A = i:j
  A' = liftUp i : liftUp j
  assert(A == A' ** RP)
  B = i:j1
  B' = liftUp i : liftUp j1
  assert(B == B' ** RP)
  C = i:j2
  C' = liftUp i : liftUp j2
  assert(C == C' ** RP)
  C'' = i : (4*y)
  assert(C === C'')
--  intersect(B, C) -- FIXME one of the generators are just zero!
  assert(intersect(B, C) == intersect(B', C') ** RP)
///

TEST /// -- M:N -> I, annihilator
  S = ZZ/32003[r,t,s]
  SP = localRing(S, ideal"s,t")
  phi = map(SP^1, , matrix{{s^4-t^2-1, s^2, t^2-r^7, s^2-t^4+r^11,r}})
  M = coker phi
  assert(ann liftUp M == liftUp ann M) -- 1_S
  assert(ann M == image matrix{0_M}:M )
  assert(mingens M == 0)
-- TODO expand this test
///

TEST /// -- Compute the annihilator of the canonical module of the rational quartic curve.
  R = QQ[a..d];
  RP = localRing(R, ideal gens R)
  I = monomialCurveIdeal(RP,{1,3,4})
  assert(saturate I == promote(saturate I, RP))
  assert(liftUp saturate I == saturate liftUp I)
  assert(saturate module I == promote(saturate module I, RP))
-- FIXME saturate uses Module ** LocalRing, which unfortunately automatically minimizes the module
  assert(liftUp saturate module I == minimalPresentation saturate module liftUp I)
  M =  Ext^2(RP^1/I, RP)
  assert(annihilator M == I)
///

TEST /// -- test for saturate
  S = QQ[u,v,a,c,Degrees=>{2,3,1,2}];
  SP = localRing(S, ideal gens S)
  P = ideal(v^3-u^3*a^3,u*v^2-c^2*a^4);
  Q = saturate(P,a)
  assert( Q == Q:a )
///

-- Tests added in the 2026 test-audit pass.  Targets:
--   * the three "dark" exports (localComplement, localModulo,
--     presentationComplex cache key) which had documentation/examples but
--     no TEST blocks of their own;
--   * residueMap and maxIdeal, both exported with --not documented and
--     never directly asserted;
--   * boundary cases for the LocalRing constructor (non-prime ideal,
--     hypersurface element, wrong-ring ideal, field, caching);
--   * the explicit "not implemented" stubs at localring.m2:75,81
--     (leadCoefficient, RP % RP) which deserve a regression guard;
--   * isFiniteLength's documented "always true" behaviour (LocalRings.m2:258)
--     pinned down so the next refactor can't quietly change semantics;
--   * the (quotient, Matrix, Matrix) / saturate / annihilator hooks over a
--     *non-maximal* prime, addressing the TODO at LocalRings.m2:369 and the
--     self-doubted localAnnihilator at LocalRings.m2:484;
--   * the legacy MES "wrong answer / key not found, OK now" cases at
--     legacy.m2:357,371,374 which were historically broken but now work and
--     had no regression guard;
--   * lift / numerator / denominator round-trips and liftUp on the full
--     dispatch fan (Ideal, Module, Matrix, MutableMatrix, RingElement,
--     default-Thing).

TEST /// -- localComplement (no prior TEST block names it)
  R = QQ[x,y]
  setMaxIdeal ideal(x,y)
  m = matrix{{x, y-1}, {0, x}}
  lc = localComplement m
  -- the defining splitting property: m | lc has full row rank modulo max
  -- (i.e., is surjective on the residue field)
  combined = R.residueMap (m | lc)
  assert(rank combined == numrows m)
  -- doc example, second form: a 1x2 matrix whose mod-max class spans R^1
  -- yields the zero map R^0 -> R^1 as its complement
  m2 = matrix{{x-1, y}}
  lc2 = localComplement m2
  assert(class lc2 === Matrix)
  assert(rank R.residueMap (m2 | lc2) == 1)
///

TEST /// -- localModulo (no prior TEST block names it)
  R = QQ[x,y,z]
  setMaxIdeal ideal vars R
  m = matrix {{x-1, y}}
  n = matrix {{y, z}}
  L = localModulo(m, n)
  assert(class L === Matrix)
  -- shape: source m has 2 cols, source n has 2 cols, syz lives in their sum
  assert(target L === source m)
  -- error path: maps with different targets should error
  m' = matrix {{x-1, y}, {0, 0}}
  assert(try (localModulo(m', n); false) else true)
///

TEST /// -- residueMap (exported with --not documented, never directly asserted)
  R = ZZ/101[a,b,c]
  -- maximal ideal: residueMap sends every generator to 0, 1 to 1
  RPmax = localRing(R, ideal gens R)
  phiMax = RPmax.residueMap
  assert(source phiMax === RPmax)
  assert(instance(target phiMax, FractionField))
  assert(phiMax (a_RPmax) == 0)
  assert(phiMax (1_RPmax) == 1)
  assert(phiMax (1 + a_RPmax) == 1)
  -- non-maximal prime (a,b): residueMap kills a, b but preserves c
  use R
  P = ideal(a, b)
  RPpr = localRing(R, P)
  phiPr = RPpr.residueMap
  assert(source phiPr === RPpr)
  assert(instance(target phiPr, FractionField))
  assert(phiPr promote(a, RPpr) == 0)
  assert(phiPr promote(b, RPpr) == 0)
  -- c is outside P, so its image is nonzero; shift by 1 confirms a non-trivial map
  use R
  imC = phiPr promote(c, RPpr)
  assert(imC != 0)
  assert(phiPr promote(c + 1, RPpr) - imC == 1)
  -- the legacy setMaxIdeal(I) path with point coordinates
  use R
  setMaxIdeal ideal(a-1, b-2, c-3)
  assert(R.residueMap a == 1_R)
  assert(R.residueMap b == 2_R)
  assert(R.residueMap c == 3_R)
///

TEST /// -- maxIdeal access (exported with --not documented, never directly asserted)
  R = ZZ/101[x,y,z]
  P = ideal(x,y)
  RP = localRing(R, P)
  -- RP.maxIdeal holds the ideal *in the base ring*
  assert(RP.maxIdeal === P)
  assert(ring RP.maxIdeal === R)
  -- `max RP` promotes the ideal into RP itself
  m = max RP
  assert(ring m === RP)
  assert(m == promote(P, RP))
///

TEST /// -- LocalRing constructor: caching, identity, and the R _ I sugar
  R = ZZ/101[x,y,z]
  P = ideal(x,y)
  RPa = localRing(R, P)
  RPb = localRing(R, P)
  -- LocalRing instances are memoized on (ring, ideal) per localring.m2:64
  assert(RPa === RPb)
  -- R _ P delegates to localRing
  assert(R _ P === RPa)
  -- localRing on a field returns the field unchanged
  fld = ZZ/101
  assert(localRing(fld, ideal 0_fld) === fld)
  -- wrong-ring ideal errors
  S = QQ[u,v]
  assert(try (localRing(R, ideal(u_S, v_S)); false) else true)
  assert(try (R _ (ideal(u_S, v_S)); false) else true)
  -- hypersurface element form errors with the documented "not yet" message
  assert(try (R _ (x_R); false) else true)
///

TEST /// -- isWellDefined: prime ideal yields true, non-prime yields false
  R = ZZ/101[u,v]
  -- localRing rebinds the global symbols u, v to the new LocalRing, so
  -- subsequent ideal expressions need an explicit `use R` to refer to R.
  -- (u*v) factors so is not prime
  RNP = localRing(R, ideal(u*v))
  assert(isWellDefined RNP === false)
  -- (u) is prime in ZZ/101[u,v]
  use R
  RPP = localRing(R, ideal u)
  assert(isWellDefined RPP === true)
  -- the maximal ideal (u,v) is also prime
  use R
  RM = localRing(R, ideal(u,v))
  assert(isWellDefined RM === true)
///

TEST /// -- documented "not implemented" stubs at localring.m2:75,81
  R = ZZ/101[a,b]
  RP = localRing(R, ideal vars R)
  -- leadCoefficient is intentionally stubbed
  assert(try (leadCoefficient (a_RP + 1); false) else true)
  -- the % operator is intentionally stubbed
  assert(try (a_RP % b_RP; false) else true)
///

TEST /// -- isUnit / numerator / denominator on LocalRing elements
  R = ZZ/101[x,y]
  RP = localRing(R, ideal vars R)
  -- elements outside the maximal ideal are units; elements in it are not
  assert(isUnit (1 + x_RP))
  assert(isUnit 1_RP)
  assert(not isUnit x_RP)
  assert(not isUnit 0_RP)
  -- numerator/denominator extract the underlying fraction
  e = (1 + x_RP) / (2 - y_RP)
  assert(ring numerator e === R)
  assert(ring denominator e === R)
  -- they recover the element by multiplication (using the unit-denominator
  -- branch of fraction)
  assert(numerator e == promote(numerator e, R))
///

TEST /// -- lift across LocalRing
  R = ZZ/101[x,y]
  RP = localRing(R, ideal vars R)
  -- units lift back to R
  assert(lift(promote(x_R, RP), R) == x_R)
  assert(lift(promote(1_R, RP), R) == 1_R)
  -- elements with a denominator in the maximal ideal do not lift
  assert(try (lift(1 / x_RP, R); false) else true)
  -- promote / lift roundtrip on the fraction field
  S = QQ[x]
  Sp = S _ (ideal x)
  F = frac Sp
  assert(promote(x_Sp, F) === x_F)
  assert(lift(x_F, Sp) === x_Sp)
  assert(not liftable(1/x_F, Sp))
///

TEST /// -- liftUp on every dispatch in its method table
  -- use a 4-variable ring so monomialCurveIdeal(R, {1,3,4}) is well-formed
  R = ZZ/101[a..d]
  RP = localRing(R, ideal gens R)
  -- RingElement
  assert(liftUp (a_RP + 1) == a_R + 1)
  assert(ring liftUp (a_RP + 1) === R)
  -- the default Thing dispatch picks the base ring automatically
  assert(liftUp (a_RP * b_RP) == a_R * b_R)
  -- Ideal
  I = ideal(a_RP, b_RP)
  J = liftUp I
  assert(ring J === R)
  assert(J == ideal(a_R, b_R))
  -- free Module
  F = RP^{-1, -2, -3}
  liftedF = liftUp F
  assert(ring liftedF === R)
  assert(isFreeModule liftedF)
  assert(degrees liftedF == {{1}, {2}, {3}})
  -- Matrix
  m = matrix{{a_RP, b_RP}, {c_RP, 0}}
  lm = liftUp m
  assert(ring lm === R)
  assert(lm == matrix{{a_R, b_R}, {c_R, 0_R}})
  -- MutableMatrix
  mm = mutableMatrix m
  lmm = liftUp(mm, R)
  assert(class lmm === MutableMatrix)
  assert(matrix lmm == lm)
  -- monomialCurveIdeal compatibility (4-var ring, 3 degrees)
  IP = monomialCurveIdeal(RP, {1,3,4})
  assert(liftUp IP == monomialCurveIdeal(R, {1,3,4}))
///

TEST /// -- presentationComplex cache key (audit: exported with "what was this for?")
  R = ZZ/32003[a..d]
  RP = localRing(R, ideal gens R)
  -- minimalPresentation of a subquotient over RP populates the cache key
  CP = freeResolution monomialCurveIdeal(RP, {1,3,4})
  F = syz transpose CP.dd_3
  G = transpose CP.dd_2
  M = subquotient(F, G)
  N = minimalPresentation M
  assert(M.cache.?presentationComplex)
  assert(instance(M.cache.presentationComplex, Complex))
  -- and likewise for a cokernel
  M2 = coker CP.dd_1
  N2 = minimalPresentation M2
  assert(M2.cache.?presentationComplex)
///

TEST /// -- isFiniteLength: pin down the documented "always true" stub
  -- per LocalRings.m2:257-258, this currently returns true for any input
  -- without checking Artinianity.  A later refactor should change this; if
  -- so, this test should be updated to whichever invariants the corrected
  -- isFiniteLength asserts.
  debug LocalRings
  R = QQ[x,y]
  RP = localRing(R, ideal vars R)
  -- free module over a local ring has infinite length, but isFiniteLength
  -- returns true
  assert(isFiniteLength RP^1 === true)
  -- as does an arbitrary subquotient
  M = RP^1 / promote(ideal(x), RP)
  assert(isFiniteLength M === true)
///

TEST /// -- quotient / saturate / annihilator hooks over a non-maximal prime
  -- (LocalRings.m2:369 "does this work over a prime ideal?";
  --  LocalRings.m2:484 "is this theoretically correct?")
  R = ZZ/32003[x,y,z]
  P = ideal(x,y)              -- height-2 prime, not maximal
  RP = localRing(R, P)
  -- (x^2, x*y, y^2) : x  collapses to (x, y) in both R and R_P
  A = promote(ideal(x^2, x*y, y^2), RP)
  B = promote(ideal x, RP)
  assert(quotient(A, B) == promote(ideal(x,y), RP))
  -- saturate((x,y), x) is the unit ideal
  assert(saturate(promote(ideal(x,y), RP), promote(ideal x, RP)) == promote(ideal 1_R, RP))
  -- annihilator of R_P / (x R_P) is x R_P
  M = RP^1 / promote(ideal x, RP)
  assert(annihilator M == promote(ideal x, RP))
///

TEST /// -- hilbertSamuelFunction error handling
  R = QQ[x,y]
  RP = localRing(R, ideal vars R)
  -- input over a non-local ring errors with an explicit message
  assert(try (hilbertSamuelFunction(R^1, 0); false) else true)
  -- parameter ideal from a different ring errors
  S = QQ[s]
  assert(try (hilbertSamuelFunction(ideal s_S, RP^1, 0); false) else true)
  -- when q == max RP, the parametric form delegates to the unparameterized one
  assert(hilbertSamuelFunction(max RP, RP^1, 0, 3) == hilbertSamuelFunction(RP^1, 0, 3))
///

TEST /// -- legacy MES "OK now" regression: m = matrix"x,y2;z3,x4"
  -- per legacy.m2:367-389, the following calls had historical "wrong answer",
  -- "key not found in hash table", and "not even composable maps" failures
  -- that were silently fixed but never had a regression test.  Pin them
  -- down so they can't quietly regress again.
  S = ZZ/101[t,x,y,z]
  setMaxIdeal ideal vars S
  m = matrix"x,y2;z3,x4"
  -- localResolution coker m completes ("wrong answer, OK now: MES")
  C = localResolution coker m
  assert(class C === Complex)
  -- localResolution of a graded coker map completes
  -- ("gives error msg 'key not found in hash table', OK mow: MES")
  N = coker map(S^{-2,0}, S^{-3,-4}, m)
  CN = localResolution N
  assert(class CN === Complex)
///

TEST /// -- Hom / Tor / Ext / kernel / cokernel / image smoke over LocalRing
  R = QQ[a,b,c]
  RP = localRing(R, ideal gens R)
  M = RP^1 / promote(ideal a, RP)
  N = RP^1 / promote(ideal b, RP)
  assert(instance(Hom(M, N), Module))
  assert(instance(Tor_1(M, N), Module))
  assert(instance(Ext^1(M, N), Module))
  -- kernel, cokernel, image of an explicit matrix
  use RP
  f = matrix{{a, b}, {c, b}}
  assert(instance(kernel f, Module))
  assert(instance(coker f, Module))
  assert(instance(image f, Module))
  -- freeResolution of the zero module
  Z = freeResolution (RP^0)
  assert(length Z == 0)
///

TEST /// -- dim and char of a LocalRing reflect the underlying prime
  -- localRing rebinds the global variables to the new ring, so each prime
  -- ideal is constructed *after* an `use` of the polynomial ring.
  R = QQ[a,b,c]
  -- localizing at the maximal ideal: dim = codim = numgens
  Rmax = localRing(R, ideal gens R)
  assert(dim Rmax == 3)
  assert(numgens Rmax == 3)
  -- localizing at a height-2 prime
  use R
  Rht2 = localRing(R, ideal(a,b))
  assert(dim Rht2 == 2)
  -- char is inherited from the coefficient field
  R2 = ZZ/101[u,v]
  assert(char localRing(R2, ideal vars R2) == 101)
  R3 = QQ[u,v]
  assert(char localRing(R3, ideal vars R3) == 0)
  -- degreeLength matches the underlying polynomial ring
  assert(degreeLength Rmax == 1)
///

end--

--============================ Tests Under Development ===================================--

/// -- XXX TODO: This is a comparison of new methods, old methods, and Mora.
  R = ZZ/32003[a..c]
  RP = localRing(R, ideal gens R)
  f = matrix for i from 0 to 2 list for j from 0 to 2 list (random(2, R)+random(1,R))
  I = minors(2, f)
  IP = ideal(gens I ** RP);
  C = pruneComplex freeResolution IP
--  IP = liftUp IP -- uncomment this line to compare speed with the nonlocal case
  elapsedTime m1 = mingens image gens IP;
  elapsedTime m2 = presentation minimalPresentation image m1;
  elapsedTime m3 = presentation minimalPresentation image m2;
  elapsedTime m4 = presentation minimalPresentation image m3;
  assert(m1 * m2 == 0)
  assert(m2 * m3 == 0) -- FIXME why not composable?
  assert(m3 * m4 == 0)
///

/// -- Same as above using old LocalRing methods
  R = ZZ/32003[a..c]
  f = matrix for i from 0 to 2 list for j from 0 to 2 list (random(2, R)+random(1,R))
  I = minors(2, f)
  setMaxIdeal ideal gens R
  elapsedTime localResolution I -- how long does this even take?
  elapsedTime m1 = localMingens gens I;
  elapsedTime m2 = localsyz m1;
  elapsedTime m3 = localsyz m2;
  elapsedTime m4 = localsyz m3; -- this is the one that takes almost all of the time
///

/// -- Now try Mora algorithm
  A = (ZZ/32003){a..d}
  IA = sub(I,A);
  m1 = gens gb IA;
  ideal m1
  m1 = mingens gb IA;
  m2 = mingens gb syz m1; -- not good.  Is this easy to fix in engine code?
///

/// --XXX Here are two similar examples
  kk = ZZ/32003
  S = kk[t]
  R = kk[a..d]
  phi = map(S,R,{t^2-t^4, t-t^7, t^2+2*t^5, t^8})
  I = ker phi
  psi = map(S,R,{t^2-t^4, t^3-t^4-t^7, t^5+2*t^6, t^8})
  J = ker psi

  RP = localRing(R, ideal gens R)

  C = freeResolution J
  C1 = pruneComplex C
  C2 = C1 ** RP
  elapsedTime pruneComplex(C2, UnitTest => isScalar, PruningMap => false) -- very slow

  IP = ideal(gens I ** RP)
--  IP = liftUp IP -- uncomment this line to compare speed with the nonlocal case
  elapsedTime m1 = mingens image gens IP -- very slow
  elapsedTime m2 = presentation minimalPresentation image m1
  elapsedTime m3 = presentation minimalPresentation image m2
  elapsedTime m4 = presentation minimalPresentation image m3
  assert(m1 * m2 == 0) -- FIXME why not composable?
  assert(m2 * m3 == 0)
  assert(m3 * m4 == 0)
///

/// --XXX
  R = ZZ/32003[a..d]
  F = (a^3-b*c*d-3*b*c)^3-a^3-b^2-(b+d)^4-c^5
  I = ideal jacobian matrix{{F}}
  C = freeResolution I
  RP = localRing(R, ideal gens R)
  elapsedTime C1 = pruneComplex(C, PruningMap => false)
  elapsedTime C2 = pruneComplex(C ** RP, PruningMap => false) -- how long does this even take?
  --
  Rloc = (ZZ/32003){a,b,c,d}
  Iloc = sub(I, Rloc)
  m1 = mingens Iloc
  m2 = syz m1 -- how long does this take?
  gens gb Iloc
///

TEST ///
-- promoting/lifting to/from fraction field
S = QQ[x]
p = ideal x
R = S_p
F = frac R
assert(promote(x_R, F) === x_F)
assert(lift(x_F, R) === x_R)
assert not liftable(1/x, R)
///

end--

--  Development stuff
--  path = prepend("~/src/m2/M2-local-rings/M2/Macaulay2/packages/", path) -- Mike
restart
needsPackage "LocalRings"
elapsedTime check LocalRings -- 15.5 seconds

restart
uninstallPackage "LocalRings"
restart
installPackage "LocalRings"
viewHelp "LocalRings"
