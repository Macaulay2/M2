///
restart
debug needsPackage "DirectSummands"
errorDepth = 2
///

TEST /// -- basic test
  -- ~0.2s
  S = QQ[x,y]
  M = coker matrix{{1,0},{1,y}}
  A = summands M
  B = summands prune M
  C = summands trim M
  assert same(prune \ A, {prune M}, B, prune \ C)
///

TEST /// -- vector space and ZZ-module tests
  M = QQ^3
  assert(summands M == toList(3:QQ^1))
  -- FIXME: what do we want here?
  summands coker matrix(QQ, {{0,0},{2,0},{0,5}})
  summands coker matrix(QQ, {{0,0},{2,0},{0,5},{0,0}})
  summands coker matrix(QQ, {{2,1},{3,5},{0,0}})
  -- TODO: also test projection/injection maps
  --
  M = coker matrix {{5,0},{0,3},{0,0},{0,0}}
  N = coker matrix {{0,0},{0,0},{2,0},{0,6}}
  P = coker matrix {{0,0},{1,0},{0,0},{0,6}}
  assert(summands M == {coker matrix{{5}}, coker matrix{{3}}, ZZ^1, ZZ^1})
  assert(summands N == {ZZ^1, ZZ^1, coker matrix{{2}}, coker matrix{{6}}})
  assert(summands P == {ZZ^1, ZZ^0, ZZ^1, coker matrix{{6}}}) -- TODO: keep zeros?
  -- TODO: also test projection/injection maps
  -- M^[0]
  -- M_[0]
///

TEST /// -- direct summands of a free module
  -- ~1.1s
  R = ZZ/2[x_0..x_5]
  M = R^{100:-2,100:0,100:2}
  A = summands M;
  B = summands changeBaseField(2, M);
  C = summands changeBaseField(4, M);
  D = summands changeBaseField(GF 101, M);
  E = summands changeBaseField(GF(2,2), M);
  assert same(M, directSum A)
  assert same apply({A, B, C, D, E}, length)
  --
  M = R^{1,2,3}
  summands M
  -- TODO: M^[0]
///

TEST /// -- direct summands of a multigraded free module
  debug needsPackage "DirectSummands"
  -- ~0.05s
  R = QQ[x,y,z] ** QQ[a,b,c]
  M = R^{{1, 0}, {1, -1}, {0, 0}, {-1, 0}}
  assert same(M, directSum summands M)
  assert same(M, directSum sort summandsFromProjectors M)
  assert same(M, directSum sort summandsFromIdempotents M)
  -- TODO: M^[0]
///

TEST /// -- direct summands of a ring
  -- ~0.06s
  S = ZZ/3[x,y,z]
  R = ZZ/3[x,y,z,w]/(x^3+y^3+z^3+w^3)
  f = map(R, S)
  -- TODO: find a non-F-split example
  M = pushForward(f, module R)
  assert(summands M == {S^{0}, S^{-1}, S^{-2}})
///

TEST /// -- direct summands of a finite dimensional algebra
  R = ZZ/101[x]/x^3
  T = R/x
  f = map(R, T)
  -- FIXME: pushforward is wrong in this case
  --assert(3 == #summands pushForward_f R^1)
  --needsPackage "PushForward"
  --assert(3 == #summands pushFwd_f R^1)
  f = map(R, prune T)
  assert(3 == #summands pushForward_f R^1)
  f = map(R, ZZ/101)
  assert(3 == #summands pushForward_f R^1)
///

TEST /// -- direct summands over field extensions
  -- ~9s
  R = (ZZ/7)[x,y,z]/(x^3+y^3+z^3);
  X = Proj R;
  M = module frobeniusPushforward(1, OO_X);
  -* is smartBasis useful? yes!
  elapsedTime A = End M; -- ~0.65s
  elapsedTime B = basis({0}, A); -- ~0.23s
  elapsedTime B = smartBasis({0}, A); -- ~0.03s
  *-
  -- if this test fails, check if "try findIdempotents M" if hiding any unexpected errors
  -- FIXME: this is slow because random homomorphisms shouldn't be over the extended field
  elapsedTime assert({1, 2, 2, 2} == rank \ summands M) -- 2.28s
  elapsedTime assert({1, 2, 2, 2} == rank \ summands changeBaseField(GF 7, M)) -- 2.87s -> 2.05
  elapsedTime assert({1, 2, 2, 2} == rank \ summands changeBaseField(GF(7, 3), M)) -- 3.77s -> 2.6
  elapsedTime assert(toList(7:1)  == rank \ summands changeBaseField(GF(7, 2), M)) -- 2.18s -> 0.47
///

TEST ///
  debug needsPackage "DirectSummands"
  K = GF(7, 2)
  R = K[x..z]/(x^3+y^3+z^3)
  M = coker map(R^{6:{-1}}, R^{6:{-2}}, {
	  {(-a-2)*z, -2*y, (-a+1)*y, 0, x, (-2*a+1)*z},
	  {(2*a+3)*y, 0, x, (a+2)*z, (3*a-3)*z, y},
	  {x, (3*a-2)*z, (-a+2)*z, (-2*a+1)*y, (2*a-3)*y, 0},
	  {(2*a-1)*z, (a+1)*y, (-3*a+1)*y, x, 0, (-2*a-1)*z},
	  {(-a+3)*y, x, 0, (-2*a-2)*z, z, -3*a*y},
	  {0, a*z, -2*z, (a-3)*y, (2*a-1)*y, x}})
  assert({1,1} == rank \ summandsFromProjectors M)
  assert({1,1} == rank \ summandsFromIdempotents M)
///

TEST /// -- testing the local case
  -- the structure is significantly altered by homogenizing modules
  -- simpler example: nodal cubic in affine vs projective plane
  debug needsPackage "DirectSummands"
  k = ZZ/2
  -- D_4^1 singularity
  R = k[x,y,z]/(x^2*y + x*y^2 + x*y*z + z^2)
  M = frobeniusPushforward(1, R)
  -- uses a basic idem
  elapsedTime assert(toList(4:1) == rank \ summands M) -- ~2s
  elapsedTime assert(toList(4:1) == rank \ summandsFromIdempotents M) -- ~0s
  --
  k = ZZ/2
  R = k[x,y,z,h]/(x^2*y + x*y^2 + x*y*z + z^2*h)
  M = frobeniusPushforward(1, R)
  elapsedTime assert(toList(8:1) == rank \ summands M) -- <2s
  --elapsedTime assert(toList(8:1) == rank \ summandsFromProjectors M)  -- 6s
  --elapsedTime assert(toList(8:1) == rank \ summandsFromIdempotents M) -- 10s
  --
  k = ZZ/2
  R = k[x,y]/(x^2-y^3-y^2)
  M = frobeniusPushforward(1, R)
  elapsedTime assert({1,1} == rank \ summands M) -- 3s
  --
  R = k[x,y,z]/(x^2*z-y^3-y^2*z)
  M = frobeniusPushforward(1, R)
  elapsedTime assert(toList(4:1) == rank \ summands M) -- <2s
///

TEST /// -- Grassmannian example
  X = Proj quotient Grassmannian(1, 3, CoefficientRing => ZZ/3);
  elapsedTime F = frobeniusPushforward(1, OO_X); -- <1s in char 2 & 3
  elapsedTime assert(splice{65:1, 8:2} == rank \ summands F) -- ~8s
///

TEST ///
  -- ~1.1s
  R = ZZ/32003[x,y,z]/(x^3, x^2*y, x*y^2, y^4, y^3*z)
  C = res(coker vars R, LengthLimit => 3)
  D = res(coker transpose C.dd_3, LengthLimit => 3)
  M = coker D.dd_3
  elapsedTime L = summands M
  assert(8 == #L)
  assert all(L, isHomogeneous)
  assert isIsomorphic(M, directSum L)
  assert all(8, i -> same { M, target M_[i], source M^[i] }
      and same { L#i, target M^[i], source M_[i] })
  --elapsedTime profile summands M;
  --profileSummary "DirectSum"
///

TEST ///
  R = ZZ/32003[x,y,z]/(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3)
  F = res(coker vars R, LengthLimit => 4)
  M = coker F.dd_4
  elapsedTime L = sort summands(M, Verbose => true)
  assert({1, 1, 1, 1, 8, 8, 8, 34} == 9 * (rank \ L))
  -- summand of 4th syzygy of residue field of ring defined by
  -- ideal(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3) is indecomposable,
  -- but it has many nilpotent endomorphisms
  -- TODO: find a certificate for indecomposability of the last summand
  --assert all(L, isIndecomposable)
///

TEST ///
  -- ~1.7s
  n = 4
  S = ZZ/32003[x_0..x_(n-1)]
  I = trim minors_2 matrix { S_*_{0..n-2}, S_*_{1..n-1}}
  R = quotient I
  C = res(coker vars R, LengthLimit => 3)
  M = prune image C.dd_3
  elapsedTime L = summands M
  assert(6 == #L)
  all(6, i -> isWellDefined M^[i] and isWellDefined M_[i]
      and M^[i] * M_[i] == id_(L#i))
///

TEST /// -- testing in char 0
  -- FIXME:
  --S = ZZ[x,y];
  --assert(2 == #summands coker matrix "x,y;y,x")
  S = QQ[x,y];
  assert(2 == #summands coker matrix "x,y; y,x")
  assert(1 == #summands coker matrix "x,y;-y,x")
  debug needsPackage "DirectSummands"
  S = QQ[a,b,c,d];
  assert(3 == #summands coker matrix "a,b,c,d;d,a,b,c;c,d,a,b;b,c,d,a")
  K = toField(QQ[i]/(i^2+1));
  S = K[x,y];
  assert(2 == #summands coker matrix "x,y; y,x")
  assert(2 == #summands coker matrix "x,y;-y,x")
  S = K[a,b,c,d];
  assert(4 == #summands coker matrix "a,b,c,d;d,a,b,c;c,d,a,b;b,c,d,a")
  S = CC[x,y];
  -- FIXME scan(20, i -> assert(set summands coker matrix {{x,y},{-y,x}} == set {cokernel matrix {{x-ii*y}}, cokernel matrix {{x+ii*y}}}))
///

TEST /// -- testing inhomogeneous examples
  debug needsPackage "DirectSummands"
  S = GF(2,2)[x,y,z];
  -- homogeneous baseline, used as control
  M = coker matrix matrix"x,y,z;y,z,x;z,x,y"
  assert(3 == #summands M)
  assert(3 == #summandsFromIdempotents M)
  assert(3 == #summandsFromProjectors M)
  assert isIsomorphic(directSum summands M, M, Tries => 10)
  --
  M = coker matrix matrix"1,y,z;y,1,x;z,x,1" -- locally zero
  assert(summands M == {M})
  assert(summandsFromIdempotents M == {M})
  --
  needsPackage "LocalRings"
  R = S_(ideal vars S)
  M = coker matrix matrix"1,y,z;y,1,x;z,x,1"
  assert(summands M == {M})
  assert(summandsFromIdempotents M == {M})
  --
  S = QQ[x,y,z];
  M = coker matrix matrix"x,y,z;y,z,x;z,x,y"
  assert(2 == #summands M)
  assert(2 == #summandsFromProjectors M)
  assert isIsomorphic(directSum summands M, M)
  --
  M = coker matrix matrix"1,y,z;y,1,x;z,x,1" -- locally zero
  assert(summands M == {M})
  assert(summandsFromIdempotents M == {M})
///

TEST ///
  kk = ZZ/101
  S = kk[x,y,z]
  P = Proj S
  T = tangentSheaf P
  R = S/(x*y-z^2)
  M = module T ** R
  -- the module doesn't split, but the sheaf does
  assert(1 == length summands M)
  assert(2 == length summands sheaf M)
///

TEST ///
  debug needsPackage "DirectSummands"
  K = ZZ/7
  R = K[x,y,z]/(x^3+y^3+z^3)
  X = Proj R
  --
  F1 = frobeniusPushforward(1, OO_X);
  elapsedTime assert({1, 2, 2, 2} == rank \ summands F1) -- 2s
  elapsedTime L1 = summands changeBaseField(2, F1); -- 5s
  assert(toList(7:1) == rank \ L1)
  --
  F2 = frobeniusPushforward(1, L1#1);
  elapsedTime assert({7} == rank \ summands F2) -- 2s
  L = potentialExtension F2
  elapsedTime L2 = summands changeBaseField(L, F2); -- projectors 14s, idempotents 85s->45s
  assert(toList(7:1) == rank \ L2)
  -- tests largepowers, but is very slow:
  -- findIdempotents changeBaseField(L, F2)
///

TEST ///
  debug needsPackage "DirectSummands"
  kk = ZZ/13
  S = kk[x,y,z]
  R = S/(x*z-y^2)
  M = module frobeniusPushforward(1, OO_(Proj R));
  elapsedTime L = summands(M, Verbose => true);
  elapsedTime assert({1,12} == last \ isomorphismTally L);
  elapsedTime L = summands(S^3000, Verbose => true);
  elapsedTime assert({3000} == last \ isomorphismTally L);
  -- nonstandard graded case
  kk = ZZ/11
  S = kk[x,y,z, Degrees => {5,1,5}]
  R = S/(x*z-y^10)
  M = module frobeniusPushforward(1, OO_(Proj R));
  elapsedTime L = summands(M, Verbose => true);
  elapsedTime assert({1,2,2,2,2,2} == last \ isomorphismTally L);
///

TEST ///
  -- testing handling of eigenvalues over extensions
  debug needsPackage "DirectSummands"
  randomChangeOfBasis = m -> randomIsomorphism target m * m * randomIsomorphism source m
  -- FIXME: doesn't work over RR yet
  kk = QQ
  kk' = toField(kk[i]/(i^2+1))
  R = kk[x,y]
  n = matrix {{x, y}, {-y, x}}; m = n ++ n;
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField(kk', coker n))
  assert(4 == # summands changeBaseField(kk', coker m))
  --
  n = randomChangeOfBasis n
  m = randomChangeOfBasis m
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField(kk', coker n))
  assert(4 == # summands changeBaseField(kk', coker m))
  --
  kk = ZZ/13
  R = kk[x,y]
  n = matrix {{x, 2*y}, {-y, x}}; m = n ++ n;
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  n = randomChangeOfBasis n
  m = randomChangeOfBasis m
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  kk = ZZ/17
  R = kk[x,y]
  n = matrix {{x, y}, {-y, x}}; m = n ++ n;
  assert(2 == # summands coker n)
  assert(4 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  n = randomChangeOfBasis n
  m = randomChangeOfBasis m
  assert(2 == # summands coker n)
  assert(4 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  kk = ZZ/19
  R = kk[x,y]
  n = matrix {{x, y}, {-y, x}}; m = n ++ n;
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  n = randomChangeOfBasis n
  m = randomChangeOfBasis m
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  kk = ZZ/32003
  R = kk[x,y]
  n = matrix {{x, y}, {-y, x}}; m = n ++ n;
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
  --
  n = randomChangeOfBasis n
  m = randomChangeOfBasis m
  assert(1 == # summands coker n)
  assert(2 == # summands coker m)
  assert(2 == # summands changeBaseField_2 coker n)
  assert(4 == # summands changeBaseField_2 coker m)
///

///
  restart
  errorDepth=2
  debug needsPackage "DirectSummands"
  -- TODO: ARRGGAGGGHHHH GF is fucking up 'a'
  -- see https://github.com/Macaulay2/M2/issues/3834
  R = ZZ/101[a,b, Degrees => {6,2}]/(a^2+b^6)
  assert(2 == #summands coker matrix {{a, b^3}, {-b^3, a}})
  R = ZZ/32003[a,b, Degrees => {6,2}]/(a^2+b^6)
  assert(1 == #summands coker matrix {{a, b^3}, {-b^3, a}})
  assert(2 == #summands changeBaseField(2, coker matrix {{a, b^3}, {-b^3, a}}))
  R = ZZ/32003[a,b]/(a^2+b^6)
  assert(1 == #summands coker matrix {{a, b^3}, {-b^3, a}})
  assert(2 == #summands changeBaseField(2, coker matrix {{a, b^3}, {-b^3, a}}))
  R = GF(32003, 2)[a,b, Degrees => {6,2}]/(a^2+b^6)
  assert(2 == #summands coker matrix {{a, b^3}, {-b^3, a}})

  R = GF(32003, 2)[a,b]/(a^2+b^6)
  assert(2 == #summands coker matrix {{a, b^3}, {-b^3, a}})

  M = coker matrix {{a, b^3}, {-b^3, a}}
  findIdempotents M
  summands changeBaseField(2, M)
///

TEST ///
  debug needsPackage "DirectSummands"
  m1 = matrix {{1,1},{0,1}}
  m2 = matrix {{2,1,1},{0,2,1},{0,0,2}}
  f = m1 ++ m2
  assert("t^5-8*t^4+25*t^3-38*t^2+28*t-8" == toString minimalPolynomial f)
  assert((f - 1)^2 == minimalProjectorFromEigenvalue(f-1, f-1))
  assert((f - 2)^4 == minimalProjectorFromEigenvalue(f-2, f-2))
///

TEST ///
  -- testing splitByDegrees
  -- let G be the grading group of R and M a G-graded R-module
  -- with monomials in degrees L, then L is a G-module.
  -- we split L first, then split M based on that
  debug needsPackage "DirectSummands"
  kk = ZZ/32003
  R = kk[s,t,u, Degrees => {1,2,3}]
  f = sum flatten entries basis(6, R)
  E1 = R/ideal f
  n = 2
  PPn = kk[X_0..X_n]
  I = ker (map(E1, PPn, basis(n+1, E1)))
  E = PPn/I
  B = basis(n+1, E1)
  phi = map(E1, E, B, DegreeMap => i -> {n+1})
  assert isHomogeneous phi
  M = pushForward'(phi, module E1, options pushForward)
  assert({1,1,1} == rank \ summands M)
  assert({1,1,1} == rank \ M.cache#"DegreeSummands")
  assert isIsomorphic(M, directSum M.cache#"DegreeSummands")
  -- TODO: M^[0] doesn't work
///

TEST ///
  S = ZZ/3[x,y,z,w]
  I = ideal(x^3-y*z*w)
  R = S/I
  X = Proj R

  E2 = frobeniusPushforward(1, OO_X(2))
  assert isIsomorphic(directSum summands module E2, module E2) -- should be true, no matter what E2 is!
  assert(isIsomorphic (prune module E2, prune dual dual module E2)) -- true, and should be true
  assert(length summands prune module E2 == length summands prune dual dual module E2)

  -- E = frobeniusPushforward(1, OO_X^1 ++ OO_X(1));
  -- E'= frobeniusPushforward(1, OO_X) ++ frobeniusPushforward(1, OO_X(1));
  -- assert isIsomorphic(prune E, prune E', Tries => 100)
///

TEST ///
  -- no-check-flag
  S = ZZ/2[x,y,z]
  R = quotient ideal(x^2*y+x*y*z+y^2*z+z^2)
  M1 = last sort(summands frobeniusPushforward(1, R), numgens)
  M2 = prune frobeniusPushforward(1, M1) -- slower without prune
  elapsedTime assert({2, 2, 2, 2} == rank \ summands M2) -- ~12s
///

TEST /// -- isDirectSummand / isSummand: detecting direct summands
  S = ZZ/3[x,y,z]
  M = S^{0,-1,-2}
  -- free L: twists occurring among the generators of M are summands
  assert isDirectSummand(S^{-1}, M)
  assert isDirectSummand(S^{0,-2}, M)
  -- isSummand is an exported synonym
  assert(isSummand === isDirectSummand)
  assert isSummand(S^{-1}, M)
  -- free L: a twist not occurring in M is not a summand
  assert not isDirectSummand(S^{1}, M)
  -- boundary: rank L > rank M is immediately false
  assert not isDirectSummand(S^4, M)
  -- boundary: equal ranks reduce to an isomorphism test
  assert isDirectSummand(M, M)
  assert not isDirectSummand(S^3, M)
  -- non-free L: a genuine summand of N ++ N is found
  N = module ideal(x,y,z)
  assert not isFreeModule N
  assert isDirectSummand(N, N ++ N)
  -- non-free L: an unrelated module is not a summand
  assert not isDirectSummand(module ideal(x,y), N ++ N)
  -- error: the two modules must be defined over the same ring.  Pin the
  -- exact error message (rather than accept any failure from isDirectSummand)
  -- to distinguish the cross-ring guard from unrelated runtime errors.
  S2 = ZZ/5[a,b,c]
  ret = capture "isDirectSummand(S2^1, M)"
  assert(ret#0 and match("expected objects over the same ring", ret#1) =!= null)
///

TEST /// -- findProjectors: graded projectors that split a module
  debug needsPackage "DirectSummands"
  R = ZZ/7[x,y,z]
  M = coker matrix"x,y,z;y,z,x;z,x,y"
  P = findProjectors M
  -- type: a nonempty list of endomorphisms of M
  assert instance(P, List)
  assert(0 < #P)
  assert all(P, g -> instance(g, Matrix))
  assert all(P, g -> source g === M and target g === M)
  -- property: each projector is a nonzero, non-injective endomorphism
  assert all(P, g -> g != 0 and not isInjective g)
  -- run: the projectors drive the decomposition into three summands
  L = summandsFromProjectors(M, P)
  assert(3 == #L)
  assert isIsomorphic(directSum L, M)
  -- error: findProjectors rejects a non-homogeneous module.  Pin the
  -- exact "expected a homogeneous module" error rather than any failure,
  -- so the test distinguishes the homogeneity guard from a numerical or
  -- algorithmic failure that happens to throw on the same input.
  Minhom = coker matrix{{x, y + 1}, {y, x}}
  assert not isHomogeneous Minhom
  ret = capture "findProjectors Minhom"
  assert(ret#0 and match("expected a homogeneous module", ret#1) =!= null)
  -- error: an indecomposable module (here R^1) yields no projectors.
  -- findProjectors here raises an error rather than returning null after
  -- exhausting its retry budget; pin the "no projectors found" message
  -- so a future change to make it return null (or to surface a different
  -- error) is caught.
  ret = capture "findProjectors R^1"
  assert(ret#0 and match("no projectors found", ret#1) =!= null)
///

TEST /// -- findIdempotents / findIdem: idempotents that split a module
  debug needsPackage "DirectSummands"
  R = ZZ/7[x,y,z]
  M = coker matrix"x,y,z;y,z,x;z,x,y"
  I = findIdempotents M
  -- type: a nonempty list of endomorphisms of M
  assert instance(I, List)
  assert(0 < #I)
  assert all(I, h -> instance(h, Matrix))
  assert all(I, h -> source h === M and target h === M)
  -- property: each returned endomorphism is (weakly) idempotent
  assert all(I, isWeakIdempotent)
  -- run: the idempotents drive the decomposition into three summands
  L = summandsFromIdempotents(M, I)
  assert(3 == #L)
  assert isIsomorphic(directSum L, M)
  -- findIdem is an exported synonym for findIdempotents
  assert(findIdem === findIdempotents)
  -- error: an indecomposable module (here R^1) yields no idempotents.
  -- Pin the "no idempotent found" error message so the test catches a
  -- future change that returns null or surfaces a different error.
  ret = capture "findIdempotents R^1"
  assert(ret#0 and match("no idempotent found", ret#1) =!= null)
///

TEST /// -- frobeniusPullback: pullback along the Frobenius
  -- over a prime field the Frobenius twist of the ring is the ring itself
  R = ZZ/3[x]
  -- a free module pulls back to a free module, scaling degrees by p^e
  assert(frobeniusPullback(1, R^1)    == R^1)
  assert(frobeniusPullback(1, R^{-1}) == R^{-3})
  -- the 0-th Frobenius pullback is the identity
  assert(frobeniusPullback(0, R^{-1}) == R^{-1})
  -- on a cokernel, the presentation entries are raised to the p^e power
  assert(frobeniusPullback(1, coker matrix{{x}}) == coker matrix{{x^3}})
  assert(frobeniusPullback(2, coker matrix{{x}}) == coker matrix{{x^9}})
  -- the matrix version raises every entry to the p^e power
  assert(frobeniusPullback(1, vars R) == map(R^1, R^{-3}, {{x^3}}))
  -- the Ring and Ideal inputs reduce to the module case
  assert(frobeniusPullback(1, R) == R^1)
  assert instance(frobeniusPullback(1, ideal x), Module)
///

TEST /// -- frobeniusPullback over GF(p^e), e > 1: Frobenius twists the underlying ring
  -- When the ground field is GF(p^e) with e > 1, Frobenius is non-trivial
  -- on the field itself (a -> a^p), so the pullback of a module over a
  -- quotient ring twists the constants in the defining ideal accordingly.
  -- Here R = (GF 4)[x]/(x + a), and pulling back R^1 once should produce
  -- the quotient by (x + a^p) = (x + a^2) = (x + a + 1) (since GF 4's
  -- generator a satisfies a^2 + a + 1 = 0, so a^2 == a + 1).
  F4 = GF 4
  S = F4[x]
  a = F4_0
  R = S/(x + a)
  fp = frobeniusPullback(1, R^1)
  -- the pullback created a new ring, distinct from R
  assert(ring fp =!= R)
  -- ...with the same underlying polynomial ring
  assert(ambient ring fp === S)
  -- but a Frobenius-twisted defining ideal: (x + a) became (x + a^2).
  -- We lift back to S to compare polynomials in the same ring (in R
  -- itself, x + a = 0, so any comparison there would collapse).
  gen = lift((ideal ring fp)_0, S)
  use S  -- evaluate the RHS in the polynomial ring S, not in the quotient R
  assert(gen == x + a^2)
  -- equivalently x + a + 1, since a^2 == a + 1 in GF 4
  assert(gen == x + a + 1)
///

TEST /// -- frobeniusPullback(ZZ, CoherentSheaf) on an elliptic curve
  -- Exercises the (ZZ, CoherentSheaf) overload of frobeniusPullback by
  -- composing it with frobeniusPushforward on a smooth elliptic curve in
  -- P^2 over a prime field.  The Frobenius morphism F: X -> X has degree
  -- p^{dim X} = p, so F_* OO_X has rank p as an OO_X-module, and F^* F_* OO_X
  -- again has rank p over the same ring (Frobenius pullback over a prime
  -- field doesn't twist the ring).
  S = ZZ/5[x, y, z]
  X = Proj(S/(y^2*z - x^3 - x*z^2 - z^3))
  pp = frobeniusPullback(1, frobeniusPushforward(1, OO_X))
  assert(instance(pp, CoherentSheaf))
  assert(rank module pp == 5)
  -- the underlying ring is unchanged: ZZ/5 is its own prime field
  assert(ring module pp === ring module OO_X)
///

load "./large-tests.m2"

end--

restart
debug needsPackage "DirectSummands"
elapsedTime check DirectSummands -- ~48s
