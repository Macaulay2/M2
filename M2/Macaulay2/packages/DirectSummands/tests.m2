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
  B = summands(M, ExtendGroundField => 2);
  C = summands(M, ExtendGroundField => 4);
  D = summands(M, ExtendGroundField => ZZ/101);
  E = summands(M, ExtendGroundField => GF(2,2));
  assert same(M, directSum A)
  assert same(A, B, C, D, E)
///

TEST /// -- direct summands of a multigraded free module
  -- ~0.05s
  R = QQ[x,y,z] ** QQ[a,b,c]
  M = R^{{0,0},{0,1},{1,0},{-1,0},{0,-1},{1,-1},{-1,1}}
  assert same(M, directSum summands M)
  assert first isIsomorphic(directSum summands M, M)
///

TEST /// -- direct summands of a ring
  -- ~0.06s
  S = ZZ/3[x,y,z]
  R = ZZ/3[x,y,z,w]/(x^3+y^3+z^3+w^3)
  f = map(R, S)
  M = pushForward(f, module R)
  assert(M == S^{0,-1,-2})
///

TEST /// -- direct summands over field extensions
  -- ~9s
  debug needsPackage "DirectSummands"
  R = (ZZ/7)[x,y,z]/(x^3+y^3+z^3);
  X = Proj R;
  M = module frobeniusPushforward(1, OO_X);
  -* is smartBasis useful? yes!
  elapsedTime A = End M; -- ~0.65s
  elapsedTime B = basis({0}, A); -- ~0.23s
  elapsedTime B = smartBasis({0}, A); -- ~0.03s
  *-
  -- if this test fails, check if "try findIdempotent M" if hiding any unexpected errors
  -- FIXME: this is slow because random homomorphisms shouldn't be over the extended field
  elapsedTime assert({1, 2, 2, 2} == rank \ summands M) -- 2.28s
  elapsedTime assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF 7)) -- 2.87s -> 2.05
  elapsedTime assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF(7, 3))) -- 3.77s -> 2.6
  elapsedTime assert(toList(7:1)  == rank \ summands(M, ExtendGroundField => GF(7, 2))) -- 2.18s -> 0.47
///

TEST ///
  -- ~2.2s
  R = ZZ/101[x,y,z]/(x^3, x^2*y, x*y^2, y^4, y^3*z)
  C = res(coker vars R, LengthLimit => 3)
  D = res(coker transpose C.dd_3, LengthLimit => 3)
  M = coker D.dd_3
  elapsedTime assert(8 == #summands M)
///

TEST ///
  -- ~1.7s
  debug needsPackage "DirectSummands"
  n = 4
  S = ZZ/11[x_0..x_(n-1)]
  I = trim minors_2 matrix { S_*_{0..n-2}, S_*_{1..n-1}}
  R = quotient I
  C = res coker vars R
  M = image C.dd_3
  assert(6 == #summands M)
///

TEST ///
  -- FIXME: why is this test so slow?
  end
  n = 3
  S = (ZZ/2)[x_0..x_(n-1)]
  R = quotient (ideal vars S)^3
  F = res coker vars R
  M = image F.dd_3
  summands M
  summands(image F.dd_1, M)
  -- TODO: have a flag to test for twists of input summands as well
  summands({image F.dd_1, coker vars R}, M)
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

TEST ///
  K = ZZ/7
  R = K[x,y,z]/(x^3+y^3+z^3)
  X = Proj R
  F1 = frobeniusPushforward(1, OO_X)
  L1 = summands(F1, ExtendGroundField => 2)
  assert(7 == #L1)
  F2 = frobeniusPushforward(1, L1#1)
  L = potentialExtension F2
  -- tests largepowers, but is very slow
  -- findIdem changeBaseField(L, F2)
  -- TODO: is 7 correct here?
  assert(7 == #summands changeBaseField(L, F2))
///

///
  -- from David's email: reaches recursion limit overnight
  needsPackage "DirectSummands"
  kk = ZZ/101
  S = kk[x,y,z]
  I = monomialIdeal(x^4,x*y,y^4,x*z,y^2*z,z^4)
  R = S/I
  F = res(coker vars R, LengthLimit => 5)
  M = coker F.dd_5;
  debugLevel = 1
  elapsedTime L5 = summands M;
///

///
needsPackage "DirectSummands"
  kk = ZZ/101
  S = kk[x,y,z]
  P = Proj S
  TP = tangentSheaf P
  R = S/(x*y-z^2)
  assert(length summands prune sheaf(module TP ** R) == rank TP)
  assert(length summands sheaf(module TP ** R) == length summands prune sheaf(module TP ** R))
///

///
  debug needsPackage "DirectSummands"
  kk = ZZ/13
  S = kk[x,y,z]
  R = S/(x*z-y^2)
  L = summands frobeniusPushforward(1, R);
  L = summands S^30000;
  elapsedTime isomorphismTally L
  elapsedTime tallySummands L
  set(last \ isomorphismTally summands frobeniusPushforward(1,R)) == set{12,13}
///

///
  debug needsPackage "DirectSummands"
  kk = ZZ/11
  S = kk[x,y,z, Degrees => {5,1,5}]
  R = S/(x*z-y^10)
  L = summands frobeniusPushforward(1, R);
  elapsedTime isomorphismTally L;
  elapsedTime tallySummands L;
  set(last \ isomorphismTally summands frobeniusPushforward(1,R)) == set{12,13}
///

TEST ///
  restart
  errorDepth=2
  debug needsPackage "DirectSummands"
  -- TODO: ARRGGAGGGHHHH GF is fucking up 'a'
  R = ZZ/101[a,b, Degrees => {6,2}]/(a^2+b^6)
  assert(2 == #summands coker matrix {{a, b^3}, {-b^3, a}})
  R = ZZ/32003[a,b, Degrees => {6,2}]/(a^2+b^6)
  assert(1 == #summands coker matrix {{a, b^3}, {-b^3, a}})
  assert(2 == #summands(coker matrix {{a, b^3}, {-b^3, a}}, ExtendGroundField => 2))
  R = ZZ/32003[a,b]/(a^2+b^6)
  assert(1 == #summands coker matrix {{a, b^3}, {-b^3, a}})
  assert(2 == #summands(coker matrix {{a, b^3}, {-b^3, a}}, ExtendGroundField => 2))
  R = GF(32003, 2)[a,b, Degrees => {6,2}]/(a^2+b^6)
  assert(2 == #summands coker matrix {{a, b^3}, {-b^3, a}})

  R = GF(32003, 2)[a,b]/(a^2+b^6)
  assert(2 == #summands coker matrix {{a, b^3}, {-b^3, a}})

  M = coker matrix {{a, b^3}, {-b^3, a}}
  findIdempotent M
  summands(M, ExtendGroundField => 2)
///

load "./large-tests.m2"

end--

restart
elapsedTime check "DirectSummands"
