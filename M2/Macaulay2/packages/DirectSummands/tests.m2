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
  -- FIXME: this keeps annoyingly breaking
  assert same(prune \ A, {prune M}, B, prune \ C)
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
///

TEST /// -- direct summands of a multigraded free module
  debug needsPackage "DirectSummands"
  -- ~0.05s
  R = QQ[x,y,z] ** QQ[a,b,c]
  M = R^{{1, 0}, {1, -1}, {0, 0}, {-1, 0}}
  assert same(M, directSum summands M)
  assert same(M, directSum sort summandsFromProjectors M)
  assert same(M, directSum sort summandsFromIdempotents M)
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
  debug needsPackage "DirectSummands"
  k = ZZ/2
  -- D_4^1 singularity
  R = k[x,y,z]/(x^2*y + x*y^2 + x*y*z + z^2)
  M = frobeniusPushforward(1, R)
  errorDepth=2
  --elapsedTime summands(M, Verbose => true);
  --elapsedTime summandsFromIdempotents(M, Verbose => true);
  -- TODO
  -- the structure is significantly altered by homogenizing it
  -- simpler example: nodal cubic in affine vs projective plane
  k = ZZ/2
  R = k[x,y,z,h]/(x^2*y + x*y^2 + x*y*z + z^2*h)
  M = frobeniusPushforward(1, R)
  elapsedTime assert(toList(8:1) == rank \ summandsFromProjectors M)  -- 6s
  elapsedTime assert(toList(8:1) == rank \ summandsFromIdempotents M) -- 10s
  --
  end
  -- FIXME: these don't finish???
  R = k[x,y]/(x^2-y^3-y^2)
  M = frobeniusPushforward(1, R)
  elapsedTime assert(toList(4:1) == rank \ summandsFromProjectors(M, Verbose => true))
  assert(toList(4:1) == rank \ summandsFromIdempotents M)
  R = k[x,y,z]/(x^2*z-y^3-y^2*z)
  assert(toList(4:1) == rank \ summandsFromProjectors M)
  assert(toList(4:1) == rank \ summandsFromIdempotents M)
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
  assert first isIsomorphic(M, directSum L)
  assert all(8, i -> same { M, target M_[i], source M^[i] }
      and same { L#i, target M^[i], source M_[i] })
  --elapsedTime profile summands M;
  --profileSummary "DirectSum"
///

TEST ///
  -- ~1.7s
  n = 4
  S = ZZ/32003[x_0..x_(n-1)]
  I = trim minors_2 matrix { S_*_{0..n-2}, S_*_{1..n-1}}
  R = quotient I
  C = res coker vars R
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

TEST /// -- testing inhomogeneous
  debug needsPackage "DirectSummands"
  S = GF(2,2)[x,y,z];
  -- homogeneous baseline, used as control
  M = coker matrix matrix"x,y,z;y,z,x;z,x,y"
  assert(3 == #summands M)
  assert(3 == #summandsFromIdempotents M)
  assert(3 == #summandsFromProjectors M)
  --
  -- FIXME: broken when using multiple idempotents at once
  M = coker matrix matrix"1,y,z;y,1,x;z,x,1"
  assert(2 == #summands M)
  assert(2 == #summandsFromIdempotents M)
  R = S_(ideal vars S)
  M = coker matrix matrix"1,y,z;y,1,x;z,x,1"
  assert(1 == #summands M)
  --
  -- FIXME:
  S = QQ[x,y,z];
  M = coker matrix matrix"x,y,z;y,z,x;z,x,y"
  M = coker matrix matrix"1,y,z;y,1,x;z,x,1"
  -- findBasicIdempotent M
  -- findIdempotents M
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

///
  restart
  debug needsPackage "DirectSummands"
  kk = ZZ/13
  S = kk[x,y,z]
  R = S/(x*z-y^2)
  L = summands frobeniusPushforward(1, R);
  L = summands S^30000;
  elapsedTime isomorphismTally L
  elapsedTime tallySummands L
  set(last \ isomorphismTally summands frobeniusPushforward(1,R)) == set{12,13}

  kk = ZZ/11
  S = kk[x,y,z, Degrees => {5,1,5}]
  R = S/(x*z-y^10)
  L = summands frobeniusPushforward(1, R);
  elapsedTime isomorphismTally L;
  elapsedTime tallySummands L;
  set(last \ isomorphismTally summands frobeniusPushforward(1,R)) == set{12,13}
///

///
  restart
  errorDepth=2
  debug needsPackage "DirectSummands"
  -- TODO: ARRGGAGGGHHHH GF is fucking up 'a'
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

load "./large-tests.m2"

end--

restart
elapsedTime check "DirectSummands" -- ~48s
