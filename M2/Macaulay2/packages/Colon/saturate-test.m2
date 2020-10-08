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
  for strategy in {-*GRevLex,*- "Unused"} do
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

-- previously in packages/Macaulay2Doc/doc9.m2

TEST ///
  -- The ideal case
  R = ZZ/101[a..d]
  I = monomialCurveIdeal(R,{1,3,4})
  F = I_0
  J = ideal(F*I_1, I_2, F^2*I_3)
  saturate(J,F)

  J = truncate(4,I)
  time saturate(J,a)
  time saturate(J,a,Strategy=>Bayer)
  time saturate(J,a,Strategy=>Linear)
  time saturate(J,a,Strategy=>Iterate)
  time saturate(J,a,Strategy=>Eliminate)

  time saturate(J)
  time saturate(J,Strategy=>Iterate)
  assert(try saturate(J,Strategy=>Bayer) else true)
  assert(try saturate(J,Strategy=>Linear) else true)
  -- FIXME: why was this supposed to fail?
  -- assert(try saturate(J,Strategy=>Eliminate) else true)
///

TEST ///
  -- The module case
  R = ZZ/101[a..d]
  M = subquotient(matrix{{a^2,b^2},{a*d,c^2}}, matrix{{c},{d}})

  time saturate(M,a)
  -- FIXME: how did these work? Seems to be missing now
  -- time saturate(M,a,Strategy=>Bayer)
  -- time saturate(M,a,Strategy=>Linear)
  -- time saturate(M,a,Strategy=>Eliminate)
  time saturate(M,a,Strategy=>Iterate)
///

TEST ///
  R = ZZ/101[x,y,z,a,b,c,d]
  S = ZZ/101[x,y,z]
  row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
  row1 = matrix{{a,b,c,d}}
  J = minors(2,row1 || row2)
  -- gbTrace = 3
  -- best time so far for the following: 30.41 seconds
  -- but this doesn't yet include finding a minimal set
  -- of generators for the image
  time saturate(J, ideal row2);
  time saturate(J, ideal row2, Strategy=>Iterate);
  time saturate(J, ideal row2, MinimalGenerators=>false);

  -- the time for the following is 40.58 seconds...
  -- but I think too many GB's are being done...
  time (
      J1 = quotient(J, ideal row2);
      J2 = quotient(J1, ideal row2);
      J3 = quotient(J2, ideal row2);
      J4 = quotient(J3, ideal row2);
      )
///

TEST ///
  R = ZZ/101[x,y,z,a,b,c,d]
  --R = ZZ/101[a,b,c,d,x,y,z]  This order is VERY BAD!!
  --R = ZZ/101[x,y,z,a,b,c,d,MonomialOrder=>ProductOrder{3,4}]
  S = ZZ/101[x,y,z]
  row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
  row1 = matrix{{a,b,c,d}}
  J = minors(2,row1 || row2)
  -- gbTrace = 3
  F = row2_(0,0)
  -- For this example, just saturate w.r.t. F.
  -- best time: 21.76 seconds
  time saturate(J, F)
  time saturate(J, F, Strategy=>Bayer)  -- 21.76
  time saturate(J, F, Strategy=>Eliminate) -- 26.08
///

TEST ///
  R = ZZ/101[a..f]
  m = monomialCurveIdeal(R,{1,3,4})
  I = ideal(d-c) + m
  saturate(I,a+b)
  I
///

TEST ///
  R = ZZ/101[a..f]
  I = ideal (d^2, d*f, f^2)
  J = ideal (d,f)
  assert( saturate(I,J) == R )
///
