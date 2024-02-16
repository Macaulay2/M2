TEST /// -- basic test
  S = QQ[x,y]
  M = coker matrix{{1,0},{1,y}}
  A = summands M
  B = summands prune M
  C = summands trim M
  assert same(A, {prune M}, B, prune \ C)
///

TEST /// -- direct summands of a free module
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
  R = QQ[x,y,z] ** QQ[a,b,c]
  M = R^{{0,0},{0,1},{1,0},{-1,0},{0,-1},{1,-1},{-1,1}}
  assert same(M, directSum summands M)
  --assert first isIsomorphic(directSum elapsedTime summands M, M)
///

TEST /// -- direct summands of a ring
  S = ZZ/3[x,y,z]
  R = ZZ/3[x,y,z,w]/(x^3+y^3+z^3+w^3)
  f = map(R, S)
  M = pushForward(f, module R)
  assert(M == S^{0,-1,-2})
///

TEST /// -- direct summands over field extensions
  debug needsPackage "DirectSummands"
  R = (ZZ/7)[x,y,z]/(x^3+y^3+z^3);
  X = Proj R;
  M = module frobeniusPushforward(OO_X, 1);
  -* is smartBasis useful? yes!
  elapsedTime A = End M; -- ~0.65s
  elapsedTime B = basis({0}, A); -- ~0.23s
  elapsedTime B = smartBasis({0}, A); -- ~0.03s
  *-
  -- if this test fails, check if "try findIdempotent M" if hiding any unexpected errors
  elapsedTime assert({1, 2, 2, 2} == rank \ summands M) -- 2.28s
  elapsedTime assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF 7)) -- 2.87s -> 2.05
  elapsedTime assert({1, 2, 2, 2} == rank \ summands(M, ExtendGroundField => GF(7, 3))) -- 3.77s -> 2.6
  elapsedTime assert(toList(7:1)  == rank \ summands(M, ExtendGroundField => GF(7, 2))) -- 2.18s -> 0.47
///

TEST ///
  R = ZZ/101[x,y,z]/(x^3, x^2*y, x*y^2, y^4, y^3*z)
  C = res(coker vars R, LengthLimit => 3)
  D = res(coker transpose C.dd_3, LengthLimit => 3)
  M = coker D.dd_3
  summands M
///

TEST ///
  --tests largepowers
  K = ZZ/7
  R = K[x,y,z]/(x^3+y^3+z^3)
  X = Proj R
  M1 = summands(frobeniusPushforward(1, OO_X), ExtendGroundField => 2)
  M2 = frobeniusPushforward(1, M1#1)
  L = potentialExtension M2
  findIdem changeBaseField(L, M2)
///
