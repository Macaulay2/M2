TEST ///
  -- used to be in tests/normal/saturate.m2
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

TEST ///
  -- example by Leslie Roberts <robertsl@mast.queensu.ca>
  -- used to be in tests/normal/saturate2.m2
  K = QQ;
  S = K[u, v, a, c, Degrees => {2, 3, 1, 2}];
  P = ideal(v^3-u^3*a^3, u*v^2-c^2*a^4);
  Q = saturate(P, a)
  assert(Q == quotient(Q, a))
  -- added in 2020
  R = newRing(S, Degrees => entries id_(ZZ^4))
  Q = saturate(sub(P, R), a)
  assert(Q == quotient(Q, a))
///

TEST get(currentFileDirectory | "saturate3.m2")
TEST get(currentFileDirectory | "saturate4.m2")
TEST get(currentFileDirectory | "saturate5.m2")

TEST ///
  -- used to be in tests/quarantine/saturate.m2
  -- TODO: add assertions
  -- a problem with matrices formed from lists of vectors in a subquotient module
  R = QQ[x..z]
  m = ideal vars R
  M = m/m^2
  N = saturate 0_M
///

TEST /// -- Tests for isSupportedInZeroLocus
  S = ZZ/11[x_0..x_4];
  B = intersect(ideal(x_0, x_1), ideal(x_2, x_3, x_4));
  I = saturate(ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3)), B);
  M = comodule I;
  -- t = (saturate(ann M, B) == ideal 1_S); -- this is commented out to make sure cache isn't used
  time assert(isSupportedInZeroLocus_B M == false)
  time assert(isSupportedInZeroLocus_B I == false)
///

TEST ///
  needsPackage "VirtualResolutions"
  K = ZZ/32003;
  X = fold(apply({1, 1, 2}, r -> toricProjectiveSpace(r, CoefficientRing => K)), (a, b) -> a ** b)
  (S, B) = (ring X, ideal X);
  -- 2 points in P1xP1xP2
  I = saturate(intersect apply(2, i -> ideal(random({1,0,0},S), random({0,1,0},S), random({0,0,1},S), random({0,0,1},S))), B);
  -- TODO: find a more complicated example, perhaps a false one
  M = prune HH_1 virtualOfPair(I, {{1, 2, 2}});
  -- FIXME: without prune we get an engine error:
  -- terminate called after throwing an instance of 'std::logic_error'
  -- what():  ERROR: Inserted duplicate entry into a KD tree.
  debugLevel = 1
  time assert(isSupportedInZeroLocus_B M == (saturate(ann M, B) == 1)) -- 0.126s
  time assert(isSupportedInZeroLocus_B M == (saturate(ann M, B) == 1)) -- 0.001s woohoo caching!
///

TEST ///
  -- previously in packages/Macaulay2Doc/doc9.m2
  -- The ideal case
  R = ZZ/101[a..d]
  I = monomialCurveIdeal(R,{1,3,4})
  J = truncate(4, ideal(I_0 * I_1, I_2, I_0^2 * I_3))

  time saturate(ideal J_*,a)
  time saturate(ideal J_*,a,Strategy=>Bayer)
  time saturate(ideal J_*,a,Strategy=>Linear)
  time saturate(ideal J_*,a,Strategy=>Iterate)
  time saturate(ideal J_*,a,Strategy=>Eliminate)
  time assert(saturate(module J, a, Strategy => Iterate) == module saturate(J, a))

  time saturate(ideal J_*)
  time saturate(ideal J_*,Strategy=>Iterate)
  assert(try saturate(ideal J_*,Strategy=>Bayer) else true)
  assert(try saturate(ideal J_*,Strategy=>Linear) else true)
  -- FIXME: why was this supposed to fail?
  -- assert(try saturate(ideal J_*,Strategy=>Eliminate) else true)
///

TEST ///
  -- The module case
  R = ZZ/101[a..d]
  M = subquotient(matrix{{a^2,b^2},{a*d,c^2}}, matrix{{c},{d}})
  clear = M -> subquotient(generators M, relations M)

  -- TODO: how to clear cache for modules?
  elapsedTime saturate(clear M,a)
  -- FIXME: how did these work? Seems to be missing now
  -- time saturate(M,a,Strategy=>Bayer)
  -- time saturate(M,a,Strategy=>Linear)
  -- time saturate(M,a,Strategy=>Eliminate)
  elapsedTime saturate(clear M, a, Strategy => Iterate)
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
  time saturate(ideal J_*, ideal row2);
  time saturate(ideal J_*, ideal row2, Strategy=>Iterate);
  time saturate(ideal J_*, ideal row2, MinimalGenerators=>false);

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
  time saturate(ideal J_*, F)
  time saturate(ideal J_*, F, Strategy=>Bayer)  -- 21.76
  time saturate(ideal J_*, F, Strategy=>Eliminate) -- 26.08
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
