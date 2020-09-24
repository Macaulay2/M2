-- used to be in tests/normal/saturate.m2
TEST ///
  S = QQ[x, y, z]
  d = 3
  f = ideal x^d
  R = S/f
  -- a general effective cartier divisor
  -- of degree 3 supported at x=y=0
  P = homogenize(x * x^d - y, z)
  assert(saturate(ideal P, z) == ideal y)
  -- FIXME: GRevLex is failing currently, but it should work
  for strategy in {GRevLex, "Unused"} do
  assert(saturate(ideal P, z, Strategy => strategy) == ideal y)
///

-- example by Leslie Roberts <robertsl@mast.queensu.ca>
-- used to be in tests/normal/saturate2.m2
TEST ///
  K = QQ;
  S = K[u, v, a, c, Degrees => {2, 3, 1, 2}];
  P = ideal(v^3-u^3*a^3, u*v^2-c^2*a^4);
  Q = saturate(P, a)
  assert(Q == quotient(Q, a))
///

TEST get(currentFileDirectory | "saturate3.m2")
TEST get(currentFileDirectory | "saturate4.m2")
TEST get(currentFileDirectory | "saturate5.m2")

-- Tests for saturationZero
TEST ///
  S = ZZ/11[x_0..x_4];
  irr = intersect(ideal(x_0, x_1), ideal(x_2, x_3, x_4));
  I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
  I' = saturate(I, irr);
  R = S^1/I';
  t = (saturate(R, irr) == 0);
  assert(saturationZero(R, irr) == t)
///

TEST ///
  S = ZZ/11[x_0..x_4];
  irr = intersect(ideal(x_0, x_1), ideal(x_2, x_3, x_4));
  I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
  I' = saturate(I, irr);
  R = S^1/I';
  t = (saturate(R, irr) == 0);
  assert(saturationZero(I', irr) == t)
///
