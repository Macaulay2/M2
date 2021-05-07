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
    needsPackage "TateOnProducts"
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
    S = ring X; B = ideal X;
    I = saturate(ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3)), B);
    -- taking NormalToricVariety as input
    elapsedTime assert(multigradedRegularity(X, I) == {{1,5},{2,2},{4,1}}) -- 18
    -- taking the ring of NormalToricVariety as input
    elapsedTime assert(multigradedRegularity(S, I) == {{1,5},{2,2},{4,1}}) -- woohoo cache hit!!
///

TEST ///
    (S, E) = productOfProjectiveSpaces {1, 2};
    B = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1), x_(1,2)))
    I = saturate(ideal(x_(0,0)^2*x_(1,0)^2+x_(0,1)^2*x_(1,1)^2+x_(0,0)*x_(0,1)*x_(1,2)^2,
            x_(0,0)^3*x_(1,2)+x_(0,1)^3*(x_(1,0)+x_(1,1))), B);
    -- taking the ring of a productOfProjectiveSpaces as input
    elapsedTime assert(multigradedRegularity(S, I) == {{1,5},{2,2},{4,1}}) -- 9
///

TEST ///
  S = QQ[x_(0,0),x_(1,0),x_(0,1),x_(1,1), Degrees => {{1,0},{1,0},{0,1},{0,1}}]
  M = cokernel matrix{{x_(0,1)^2*x_(1,0), x_(0,0)^2*x_(0,1)*x_(1,0), x_(1,0)^3*x_(1,1)^2, x_(0,1)*x_(1,0)^3*x_(1,1), x_(0,0)^4*x_(0,1), x_(0,1)*x_(1,0)^5}}
  -- taking an arbitrary multigraded ring as input
  elapsedTime assert(multigradedRegularity(S, M) == {{0,2},{2,0}}) -- 1.13
///

TEST /// -- testing picard rank 3
    (S, E) = productOfProjectiveSpaces {1, 1, 2};
    irr = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1)), ideal(x_(2,0), x_(2,1), x_(2,2)))
    I = saturate(intersect apply(6,i-> ideal(random({1,0,0},S),random({0,1,0},S), random({0,0,1},S),random({0,0,1},S))), irr);
    elapsedTime assert(multigradedRegularity(S, I) == {{0,0,2},{0,1,1},{0,5,0},{1,0,1},{1,2,0},{2,1,0},{5,0,0}}) -- 9.7
///

TEST /// -- testing twisted modules
  (S,E)=productOfProjectiveSpaces {1,1}
  for i from -1 to 1 list(
      M=S^{{-i,-i}};
      r=regularity M;
      degs := apply(2, i -> min(degrees M / (deg -> deg_i)));
      low := degs-toList(2:2);
      high := apply(2, i -> max({r} | degrees M / (deg -> deg_i)));
      assert(multigradedRegularity(S, M) == {{i, i}});
--      (cohomologyMatrix(M,low,high), multigradedRegularity(S, M))
      )
///
