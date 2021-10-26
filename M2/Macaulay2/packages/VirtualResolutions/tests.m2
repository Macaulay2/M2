------ Tests for randomRationalCurve
TEST ///
    elapsedTime assert (dim randomRationalCurve(2,3,ZZ/32003) == 3) -- 0.19
    elapsedTime assert (dim randomRationalCurve(2,3,ZZ/32003) == 3) -- 0.15
    elapsedTime assert (dim randomRationalCurve(2,3) == 3) -- 0.17
///

------ Tests for randomMonomialCurve
TEST ///
    elapsedTime assert (dim randomMonomialCurve(2,3,ZZ/32003) == 3) -- 0.07
    elapsedTime assert (dim randomMonomialCurve(2,3,ZZ/32003) == 3) -- 0.06
    elapsedTime assert (dim randomMonomialCurve(2,3) == 3) -- 0.1
///

------ Tests for curveFromP3toP1P2
TEST ///
    R = ZZ/101[z_0,z_1,z_2,z_3];
    C = ideal(z_0*z_2-z_1^2, z_1*z_3-z_2^2, z_0*z_3-z_1*z_2);
    elapsedTime assert (dim curveFromP3toP1P2(C) == 3) -- 0.3
///

TEST ///
    R = ZZ/101[w_0,w_1,w_2,w_3];
    C = ideal(w_0*w_2-w_1^2, w_1*w_3-w_2^2, w_0*w_3-w_1*w_2);
    elapsedTime assert (dim curveFromP3toP1P2(C) == 3) -- 0.27
///

TEST ///
    R = ZZ/101[x_0,x_1,x_2,x_3];
    C = ideal(x_0*x_2-x_1^2, x_1*x_3-x_2^2, x_0*x_3-x_1*x_2);
    elapsedTime assert (dim curveFromP3toP1P2(C) == 3) -- 0.25
///

TEST ///
    R = ZZ/101[z_0,z_1,z_2,z_3];
    C = ideal(z_0*z_2-z_1^2, z_1*z_3-z_2^2, z_0*z_3-z_1*z_2);
    elapsedTime assert (dim curveFromP3toP1P2(C,PreserveDegree=>false) == 3) -- 0.22
///

------ Tests for randomCurveP1P2
TEST ///
    setRandomSeed("syzygies");
    elapsedTime assert (dim randomCurveP1P2(3,0,ZZ/32003) == 3) -- 0.42
    elapsedTime assert (dim randomCurveP1P2(5,2,ZZ/32003,Attempt=>50) == 3) -- 1.16
    elapsedTime assert (dim randomCurveP1P2(3,0) == 3) -- 0.45
    elapsedTime assert (degree randomCurveP1P2(3,0) == 6) -- 0.60
    elapsedTime assert (dim randomCurveP1P2(3,0,Attempt=>50) == 3) -- 0.24
    elapsedTime assert (degree randomCurveP1P2(3,0,Attempt=>50) == 6) -- 0.25
    elapsedTime assert (dim randomCurveP1P2(5,2,Attempt=>50) == 3) -- 0.41
///

------ Tests for isVirtual
TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_1^3*x_2+x_1^3*x_3+x_0^3*x_4,
            x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
            x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
            x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    d2 = map(source d1, ,{{x_3^2, x_4^2, -x_2^2},
        {-x_1*x_2-x_1*x_3, 0, x_0*x_4},
        {x_0, -x_1, 0},
        {0, x_0, x_1}});
    C = chainComplex({d1,d2});
    elapsedTime assert(isVirtual(irr,C) == true) -- 0.028
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
        x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
        x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    C = chainComplex({d1});
    elapsedTime assert(isVirtual(irr,C) == false) -- 0.022
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
        x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
        x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    C = chainComplex({d1});
    elapsedTime assert(isVirtual(irr,C) == false) -- 0.027
///

TEST ///
    S = ZZ/101[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(random({1,2},S),random({3,1},S),random({2,2},S));
    r = res I;
    elapsedTime assert(isVirtual(irr,r) == true) -- 0.34
///

----- Tests for idealSheafGens
TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    J = ourSaturation(I,irr);
    elapsedTime assert(idealSheafGens(2,J,irr) == {I}) -- 4.2
///

TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,y_0,y_1, Degrees=>{2:{1,0},2:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(y_0,y_1));
    I = intersect(ideal(x_0,y_0),ideal(x_1,y_1));
    J = ourSaturation(I, irr);
    elapsedTime output = idealSheafGens(2,I,irr,GeneralElements=>true); -- 0.08
    elapsedTime assert(J == ourSaturation(output_0, irr) and J == ourSaturation(output_1, irr)) -- 0.05
///

TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    J = ourSaturation(I,irr);
    elapsedTime output = idealSheafGens(2,J,irr,GeneralElements=>true); -- 6.8
    elapsedTime assert(J == ourSaturation(output_0, irr)) -- 0.06
///

-- Test for resolveViaFatPoint
TEST ///
    N = {1,1,2} -- Example 5.7 of [BES] uses 1x1x2 and 6 points
    pts = 6
    (S, E) = productOfProjectiveSpaces N
    irr = intersect for n to #N-1 list (
        ideal select(gens S, i -> (degree i)#n == 1)
        )
    -- Generate ideal of 6 random points in P1xP1xP2
    I = saturate intersect for i to pts - 1 list (
        P := sum for n to N#0 - 1 list ideal random({1,0,0}, S);
        Q := sum for n to N#1 - 1 list ideal random({0,1,0}, S);
        R := sum for n to N#2 - 1 list ideal random({0,0,1}, S);
        P + Q + R
        );
    elapsedTime assert isVirtual(irr, resolveViaFatPoint (I, irr, {2,1,0})) -- 0.15
    elapsedTime assert isVirtual(irr, resolveViaFatPoint (I, irr, {3,3,0})) -- 0.26
///

-- Test for virtualOfPair
TEST ///
    X = toricProjectiveSpace(1)**toricProjectiveSpace(1);
    --X = normalToricVarietyWithTateData X;
    S = ring X; B = ideal X;
    J = saturate(intersect(
            ideal(x_1 - 1*x_0, x_3 - 4*x_2),
            ideal(x_1 - 2*x_0, x_3 - 5*x_2),
            ideal(x_1 - 3*x_0, x_3 - 6*x_2)),
            B);
    elapsedTime vres = virtualOfPair(res(J, LengthLimit => 2), {{3,1}}); -- 0.004
    elapsedTime vres' = virtualOfPair(J, {{3,1}}, LengthLimit => 2); -- 0.001
    elapsedTime assert isVirtual(B,vres); -- 0.02
    elapsedTime assert isVirtual(B,vres',Strategy=>"Determinantal"); -- 0.10
///

-- Tests for multigradedRegularity
TEST ///
    X = toricProjectiveSpace(1)**toricProjectiveSpace(2);
    --X = normalToricVarietyWithTateData X;
    S = ring X; B = ideal X;
    I = saturate(ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3)), B);
    -- taking NormalToricVariety as input
    elapsedTime assert(multigradedRegularity(X, I) == {{2,2},{4,1},{1,5}}) -- 18 -> 8 -> 4 -> 1.3
    -- taking the ring of NormalToricVariety as input
    elapsedTime assert(multigradedRegularity(S, I) == {{2,2},{4,1},{1,5}}) -- woohoo cache hit!!
    -- test for weird ring problems
    assert(ring x_0 === value getSymbol "S")
///

TEST ///
    (S, E) = productOfProjectiveSpaces {1, 2};
    B = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1), x_(1,2)))
    I = saturate(ideal(x_(0,0)^2*x_(1,0)^2+x_(0,1)^2*x_(1,1)^2+x_(0,0)*x_(0,1)*x_(1,2)^2,
            x_(0,0)^3*x_(1,2)+x_(0,1)^3*(x_(1,0)+x_(1,1))), B);
    -- taking the ring of a productOfProjectiveSpaces as input
    elapsedTime assert(multigradedRegularity(S, I) == {{2,2},{4,1},{1,5}}) -- 9 -> 2.43 -> 1.3
    -- test for weird ring problems
    assert(ring x_(0,0) === value getSymbol "S")
///

TEST ///
  debug needsPackage "VirtualResolutions"
  S = QQ[x_(0,0),x_(0,1),x_(1,0),x_(1,1), Degrees => {{1,0},{1,0},{0,1},{0,1}}]
  --S = imbueRingWithTateData S;
  M = cokernel matrix{{x_(1,0)^2*x_(0,1), x_(0,0)^2*x_(1,0)*x_(0,1), x_(0,1)^3*x_(1,1)^2, x_(1,0)*x_(0,1)^3*x_(1,1), x_(0,0)^4*x_(1,0), x_(1,0)*x_(0,1)^5}}
  -- This is an unsaturated input test, where res truncate({3,2}, M) is quasi-linear, but not short
  -- elapsedTime assert(multigradedRegularity(S, M) == {{4,2}}) -- 1
  I = saturate(ideal relations M, irrelevantIdeal S) -- intersect last ringData S
  -- taking an arbitrary multigraded ring as input
  elapsedTime assert(multigradedRegularity(S, I) == {{2,0}}) -- < 0.2
  -- test for weird ring problems
  assert(ring x_(0,0) === value getSymbol "S")
///

TEST /// -- testing picard rank 3
    (S, E) = productOfProjectiveSpaces {1, 1, 2};
    irr = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1)), ideal(x_(2,0), x_(2,1), x_(2,2)))
    I = saturate(intersect apply(6,i-> ideal(random({1,0,0},S),random({0,1,0},S), random({0,0,1},S),random({0,0,1},S))), irr);
    elapsedTime assert(multigradedRegularity(S, I) == {{0,0,2},{0,1,1},{1,0,1},{1,2,0},{2,1,0},{0,5,0},{5,0,0}}) -- 5.4 -> 8.5 -> 2.7
    -- test for weird ring problems
    assert(ring x_(0,0) === value getSymbol "S")
    end
    -- good benchmark test for isZeroSheaf and isIsomorphismOfSheaves
    elapsedTime assert(multigradedRegularity(S, module I) == {{1, 1, 2}, {1, 2, 1}, {2, 1, 1}}) -- ~60
    -- used to get stuck in cohomologyHashTable with 117 spots to check, now takes 62s
///

--TODO
-- warning: clearing value of symbol i to allow access to subscripted variables based on it
--       : debug with expression   debug 8267   or with command line option   --debug 8267
TEST /// -- testing twisted modules
  (S, E) = productOfProjectiveSpaces {1, 1}
  for i from -1 to 1 list(
      M = S^{{-i,-i}};
      r = regularity M;
      degs = apply(2, i -> min(degrees M / (deg -> deg_i)));
      low  = degs-toList(2:2);
      high = apply(2, i -> max({r} | degrees M / (deg -> deg_i)));
      assert(multigradedRegularity(S, M) == {{i, i}});
--      (cohomologyMatrix(M,low,high), multigradedRegularity(S, M))
      )
///

TEST /// -- example 5.8 of BES
  debugLevel = 1
  debug needsPackage "VirtualResolutions"
  importFrom_LinearTruncations {"isQuasiLinear"}
  (S, E) = productOfProjectiveSpaces {1, 3}
  X = normalToricVarietyFromTateData S
  -- already saturated
  I = ideal(
      x_(0,1) * x_(1,1)^2 + x_(0,0) * x_(1,0) * x_(1,2) + x_(0,1) * x_(1,1) * x_(1,3),
      x_(0,0)^2 * x_(1,1)^2 + x_(0,0) * x_(0,1) * x_(1,2)^2 + x_(0,1)^2 * x_(1,0) * x_(1,3),
      x_(0,0) * x_(1,1)^4 - x_(0,0) * x_(1,0) * x_(1,2)^3 + x_(0,0) * x_(1,1)^3 * x_(1,3)
      - x_(0,1) * x_(1,0)^2 * x_(1,2) * x_(1,3),
      x_(1,1)^6 - x_(1,0) * x_(1,1)^2 * x_(1,2)^3 + 2 * x_(1,1)^5 * x_(1,3) + x_(1,0)^3
      * x_(1,2)^2 * x_(1,3) - x_(1,0) * x_(1,1) * x_(1,2)^3 * x_(1,3) + x_(1,1)^4 * x_(1,3)^2)
  M = truncate({1, 3}, comodule I)
  C = res M
  assert isQuasiLinear({1, 3}, M)
  assert(ann HH_0 C == I)

  D = virtualOfPair(comodule I, {{2, 6}})
  assert isVirtual(variety S, D)
  assert(ann HH_0 D == I)
  elapsedTime assert(multigradedRegularity(S, I) === {{1, 3}, {2, 2}}) -- 6.1s -> 1s
///

TEST ///
  debug needsPackage "VirtualResolutions"
  X = toricProjectiveSpace(1)**toricProjectiveSpace(2);
  --X = normalToricVarietyWithTateData X
  S = ring X;
  B = ideal X;
  M = S^{{0,0},-{3,3}}
  -- testing both strategies of isVirtualOfPair
  -- TODO: benchmark
  plotRegion((i,j) -> isVirtualOfPair({i,j}, M, IrrelevantIdeal => B), {0,0},{5,5})
  plotRegion((i,j) ->         isChiH0({i,j}, M, IrrelevantIdeal => B), {0,0},{5,5})
  assert not isVirtualOfPair({1,1}, M, IrrelevantIdeal => B)
  assert     isVirtualOfPair({1,1}, M, IrrelevantIdeal => B, Strategy => "ann")
///

TEST /// -- Example 1.4 of BES, also in documentation of multigradedRegularity
--restart
debugLevel=1
  assert(# hooks multigradedRegularity == 2)
  debug needsPackage "VirtualResolutions"
  X = toricProjectiveSpace(1)**toricProjectiveSpace(2);
  --X = normalToricVarietyWithTateData X
  S = ring X;
  B = ideal X;
  I = saturate(ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3)), B)
  elapsedTime assert(multigradedRegularity(S, module I) == {{3, 3}, {4, 2}, {2, 5}}) -- used to be too slow, now 3s
  elapsedTime assert(multigradedRegularity(X,        I) == {{2, 2}, {4, 1}, {1, 5}})
  -- demonstration of internal helper tools
  M = comodule ideal I_*; -- maybe in regularity {{3, 1}, {2, 1}}
  plotRegion(regularityBound M, {0,0}, {5,5})
  plotRegion((i,j) -> isQuasiLinear({i,j}, M), {0,0},{5,5})
  plotRegion((i,j) -> isChiH0({i,j}, M, IrrelevantIdeal => B), {0,0},{5,5})
  plotRegion((i,j) -> isVirtualOfPair({i,j}, M, IrrelevantIdeal => B), {0,0},{5,5})
  plotRegion({{2, 2}, {4, 1}, {1, 5}}, {0,0}, {5,5})
  -- testing the winnowing map and isIsomorphismOfSheaves
  -- in two spots, {2,1} and {3,1}, we get a virtual resolution, but the resolution of
  -- the truncation at those degrees, which are not in regularity, is not quasi-linear
  V = virtualOfPair(res M, {{2,1} + {1,2}})
  phi = V.cache.winnowingMap
  -- TODO: test this more
  assert isIsomorphismOfSheaves(B, phi, Strategy => "Support")
  assert isIsomorphismOfSheaves(B, phi)
  K = prune ker phi
  assert(saturate(ann K, B) == 1)
  -- a sheaf is zero if the Hilbert function vanishes
  -- in some translation of the positive quadrant:
  N = 2 * regularity K; matrix table(N,N, (y, x) -> hilbertFunction_{N - y - 1, x} K)
///

TEST ///
  (S,E) = productOfProjectiveSpaces {1,2};
  I = ideal(x_(0,0)+13748*x_(0,1),
      x_(1,0)^2*x_(1,1)+890*x_(1,0)*x_(1,1)^2-1192*x_(1,1)^3+
      7949*x_(1,0)^2*x_(1,2)-609*x_(1,0)*x_(1,1)*x_(1,2)-
      12541*x_(1,1)^2*x_(1,2)+205*x_(1,0)*x_(1,2)^2+3078*x_(1,1)*x_(1,2)^2+11091*x_(1,2)^3,
      x_(1,0)^3+9088*x_(1,0)*x_(1,1)^2+10840*x_(1,1)^3+4661*x_(1,0)^2*x_(1,2)+
      3899*x_(1,0)*x_(1,1)*x_(1,2)-5917*x_(1,1)^2*x_(1,2)-7815*x_(1,0)*x_(1,2)^2-
      2114*x_(1,1)*x_(1,2)^2-3089*x_(1,2)^3);
  assert(multigradedRegularity(S,   module I) == {{1,5}})
  assert(multigradedRegularity(S, comodule I) == {{0,4}})
///

TEST /// -- test of returning -infinity for irrelevant ideals
  debug needsPackage "VirtualResolutions"
  X = toricProjectiveSpace(1)**toricProjectiveSpace(2);
  --X = normalToricVarietyWithTateData X
  S = ring X;
  B = ideal X;
  assert(multigradedRegularity(S, S^0) == {{-infinity, -infinity}})
  --
  -- The following tests involve intentionally unsaturated ideals, therefore it is not
  -- guaranteed that multigradedRegularity will give the right answer, but the correct
  -- answers are provided for future improvement.
  --
  -- Test of an irrelevant module where regularity is -infinity only in one component
  -- here's a simplified version of K from the end of 2 tests ago
  K = cokernel(map(S^{{-1, -1}}, S^{{-2, -1}, {-2, -1}}, {{x_1, x_0}}))
  assert(multigradedRegularity(S, K) == {{2, 1}}) -- FIXME: should give {{2, -infinity}}?
  plotRegion((i,j) -> isQuasiLinear({i,j}, K), {0,0},{3,3})
  plotRegion((i,j) -> isChiH0({i,j}, K, IrrelevantIdeal => B), {0,0},{3,3})
  plotRegion((i,j) -> isVirtualOfPair({i,j}, K, IrrelevantIdeal => B), {0,0},{3,3})
  plotRegion({{2, -infinity}}, {0,0}, {3,3}) -- TODO: why is {1, 1} not in regularity?
  n = 2 * regularity K; matrix table(n, n, (y, x) -> hilbertFunction_{x, n - y - 1} K)
  --
  -- Test of a non-zero module where regularity is -infinity in both components
  N = K / truncate({0, 2}, K)
  assert(multigradedRegularity(S, N) == {{1, 2}, {2, 1}}) -- FIXME: should give {{-infinity, 2}, {2, -infinity}}
  plotRegion((i,j) -> isQuasiLinear({i,j}, N), {0,0},{3,3})
  plotRegion((i,j) -> isChiH0({i,j}, N, IrrelevantIdeal => B), {0,0},{3,3})
  plotRegion((i,j) -> isVirtualOfPair({i,j}, N, IrrelevantIdeal => B), {0,0},{3,3})
  plotRegion({{-infinity, 2}, {2, -infinity}}, {0,0}, {3,3})
  n = 2 * regularity N; matrix table(n, n, (y, x) -> hilbertFunction_{n - y - 1, x} N)
///
