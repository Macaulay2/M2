------ Tests for randomRationalCurve
TEST ///
    assert (dim randomRationalCurve(2,3,ZZ/32003) == 3)
///

TEST ///
    assert (dim randomRationalCurve(2,3,ZZ/32003) == 3)
///

TEST ///
    assert (dim randomRationalCurve(2,3) == 3)
///

------ Tests for randomMonomialCurve
TEST ///
    assert (dim randomMonomialCurve(2,3,ZZ/32003) == 3)
///

TEST ///
    assert (dim randomMonomialCurve(2,3,ZZ/32003) == 3)
///

TEST ///
    assert (dim randomMonomialCurve(2,3) == 3)
///

------ Tests for curveFromP3toP1P2
TEST ///
    R = ZZ/101[z_0,z_1,z_2,z_3];
    C = ideal(z_0*z_2-z_1^2, z_1*z_3-z_2^2, z_0*z_3-z_1*z_2);
    assert (dim curveFromP3toP1P2(C) == 3)
///

TEST ///
    R = ZZ/101[w_0,w_1,w_2,w_3];
    C = ideal(w_0*w_2-w_1^2, w_1*w_3-w_2^2, w_0*w_3-w_1*w_2);
    assert (dim curveFromP3toP1P2(C) == 3)
///

TEST ///
    R = ZZ/101[x_0,x_1,x_2,x_3];
    C = ideal(x_0*x_2-x_1^2, x_1*x_3-x_2^2, x_0*x_3-x_1*x_2);
    assert (dim curveFromP3toP1P2(C) == 3)
///

TEST ///
    R = ZZ/101[z_0,z_1,z_2,z_3];
    C = ideal(z_0*z_2-z_1^2, z_1*z_3-z_2^2, z_0*z_3-z_1*z_2);
    assert (dim curveFromP3toP1P2(C,PreserveDegree=>false) == 3)
///

------ Tests for randomCurveP1P2
TEST ///
    assert (dim randomCurveP1P2(3,0,ZZ/32003) == 3)
///

TEST ///
    assert (dim randomCurveP1P2(5,2,ZZ/32003,Attempt=>50) == 3)
///

TEST ///
    assert (dim randomCurveP1P2(3,0) == 3)
///

TEST ///
    assert (degree randomCurveP1P2(3,0) == 6)
///

TEST ///
    assert (dim randomCurveP1P2(3,0,Attempt=>50) == 3)
///

TEST ///
    assert (degree randomCurveP1P2(3,0,Attempt=>50) == 6)
///

TEST ///
    assert (dim randomCurveP1P2(5,2,Attempt=>50) == 3)
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
    assert(isVirtual(irr,C) == true)
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
        x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
        x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    C = chainComplex({d1});
    assert(isVirtual(irr,C) == false)
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
        x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
        x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    C = chainComplex({d1});
    assert(isVirtual(irr,C) == false)
///

TEST ///
    S = ZZ/101[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(random({1,2},S),random({3,1},S),random({2,2},S));
    r = res I;
    assert(isVirtual(irr,r) == true)
///

----- Tests for idealSheafGens
TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    J = ourSaturation(I,irr);
    assert(idealSheafGens(2,J,irr) == {I})
///

TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,y_0,y_1, Degrees=>{2:{1,0},2:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(y_0,y_1));
    I = intersect(ideal(x_0,y_0),ideal(x_1,y_1));
    J = ourSaturation(I, irr);
    output = idealSheafGens(2,I,irr,GeneralElements=>true);
    assert(J == ourSaturation(output_0, irr) and J == ourSaturation(output_1, irr))
///

TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    J = ourSaturation(I,irr);
    output = idealSheafGens(2,J,irr,GeneralElements=>true);
    assert(J == ourSaturation(output_0, irr))
///

-- Test for resolveViaFatPoint
TEST ///
    debug needsPackage "TateOnProducts"
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
        )
    assert isVirtual(irr, resolveViaFatPoint (I, irr, {2,1,0}))
    assert isVirtual(irr, resolveViaFatPoint (I, irr, {3,3,0}))
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
    elapsedTime vres = virtualOfPair(res(J, LengthLimit => 2), {{3,1}});
    elapsedTime vres' = virtualOfPair(J, {{3,1}}, LengthLimit => 2);
    assert isVirtual(B,vres);
    assert isVirtual(B,vres',Strategy=>"Determinantal");
///

-- Tests for multigradedRegularity
-- TODO: takes too long
TEST ///
    X = toricProjectiveSpace(1)**toricProjectiveSpace(2);
    S = ring X; B = ideal X;
    I = saturate(ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3)), B);
    -- taking NormalToricVariety as input
    assert(multigradedRegularity(S, I) == {{1,5},{2,2},{4,1}})
    -- taking the ring of NormalToricVariety as input
    assert(multigradedRegularity(X, I) == {{1,5},{2,2},{4,1}})
///

TEST ///
    (S, E) = productOfProjectiveSpaces {1, 2};
    B = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1), x_(1,2)))
    I = saturate(ideal(x_(0,0)^2*x_(1,0)^2+x_(0,1)^2*x_(1,1)^2+x_(0,0)*x_(0,1)*x_(1,2)^2,
            x_(0,0)^3*x_(1,2)+x_(0,1)^3*(x_(1,0)+x_(1,1))), B);
    -- taking the ring of a productOfProjectiveSpaces as input
    assert(multigradedRegularity(S, I) == {{1,5},{2,2},{4,1}})
///

TEST ///
    (S, E) = productOfProjectiveSpaces {1, 1, 2};
    irr = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1)), ideal(x_(2,0), x_(2,1), x_(2,2)))
    I = saturate(intersect apply(6,i-> ideal(random({1,0,0},S),random({0,1,0},S), random({0,0,1},S),random({0,0,1},S))), irr);
    assert(multigradedRegularity(S, I) == {{0,0,2},{0,1,1},{0,5,0},{1,0,1},{1,2,0},{2,1,0},{5,0,0}})
///
