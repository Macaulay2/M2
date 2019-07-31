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
    assert(isVirtual(I,irr,C) == true)
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
        x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
        x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    C = chainComplex({d1});
    assert(isVirtual(I,irr,C) == false)
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2,
        x_0*x_1*x_2^3+x_0*x_1*x_2^2*x_3-x_0^2*x_3^2*x_4+x_1^2*x_2*x_4^2+x_1^2*x_3*x_4^2,
        x_1^2*x_2^3+x_1^2*x_2^2*x_3-x_0*x_1*x_3^2*x_4-x_0^2*x_4^3}};
    C = chainComplex({d1});
    assert(isVirtual(I,irr,C) == false)
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2}};
    C = chainComplex({d1});
    assert(isVirtual(I,irr,C) == false)
///

TEST ///
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    d1 = matrix{{x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2}};
    C = chainComplex({d1});
    assert(isVirtual(I,irr,C) == false)
///

TEST ///
    S = ZZ/101[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(random({1,2},S),random({3,1},S),random({2,2},S));
    r = res I;
    assert(isVirtual(I,irr,r) == true)
///

TEST ///
    X = toricProjectiveSpace(1)**toricProjectiveSpace(1);
    S = ring X; B = ideal X;
    J = saturate(intersect(
            ideal(x_1 - 1*x_0, x_3 - 4*x_2),
            ideal(x_1 - 2*x_0, x_3 - 5*x_2),
            ideal(x_1 - 3*x_0, x_3 - 6*x_2)),
            B);
    minres = res J;
    vres = virtualOfPair(J,{{3,1}});
    assert isVirtual(J,B,vres,Strategy=>"Determinantal")
///

----- Tests for findGensUpToIrrelevance
TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    J = ourSaturation(I,irr);
    lst = {{0,1}};
    assert(findGensUpToIrrelevance(2,J,irr) == lst)
///

TEST ///
    S = ZZ/32003[x_0,x_1,y_0,y_1, Degrees=>{2:{1,0},2:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(y_0,y_1));
    I = intersect(ideal(x_0,y_0),ideal(x_1,y_1));
    output = findGensUpToIrrelevance(2,I,irr,GeneralElements=>true);
    assert(length(output) == 3 and output_1 == {0,1} and output_2 == {1,2})
///

TEST ///
    debug needsPackage "VirtualResolutions"
    S = ZZ/32003[x_0,x_1,x_2,x_3,x_4, Degrees=>{2:{1,0},3:{0,1}}];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    J = ourSaturation(I,irr);
    output = findGensUpToIrrelevance(2,J,irr,GeneralElements=>true);
    assert(length(output) == 2 and output_1 == {0,1})
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
    assert isVirtual(I, irr, resolveViaFatPoint (I, irr, {2,1,0}))
    assert isVirtual(I, irr, resolveViaFatPoint (I, irr, {3,3,0}))
///

-- Test for multiWinnow
TEST ///
    X = toricProjectiveSpace(1)**toricProjectiveSpace(1);
    S = ring X; B = ideal X;
    J = saturate(intersect(
            ideal(x_1 - 1*x_0, x_3 - 4*x_2),
            ideal(x_1 - 2*x_0, x_3 - 5*x_2),
            ideal(x_1 - 3*x_0, x_3 - 6*x_2)),
            B);
    minres = res J;
    vres = virtualOfPair(J,{{3,1}});
    assert isVirtual(J,B,vres)
///

-- Tests for multigradedRegularity
TEST ///
    X = toricProjectiveSpace(1)**toricProjectiveSpace(2);
    S = ring X; B = ideal X;
    I = saturate(ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3)), B);
    -- taking NormalToricVariety as input
    assert(multigradedRegularity(S, I) == {{2,2},{4,1},{1,5}})
    -- taking the ring of NormalToricVariety as input
    assert(multigradedRegularity(X, I) == {{2,2},{4,1},{1,5}})
///

TEST ///
    (S, E) = productOfProjectiveSpaces {1, 2};
    B = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1), x_(1,2)))
    I = saturate(ideal(x_(0,0)^2*x_(1,0)^2+x_(0,1)^2*x_(1,1)^2+x_(0,0)*x_(0,1)*x_(1,2)^2,
            x_(0,0)^3*x_(1,2)+x_(0,1)^3*(x_(1,0)+x_(1,1))), B);
    -- taking the ring of a productOfProjectiveSpaces as input
    assert(multigradedRegularity(S, I) == {{2,2},{4,1},{1,5}})
///

-- FIXME: this test currently fails, but the issue doesn't appear to be in multigradedRegularity
///
    (S, E) = productOfProjectiveSpaces {1, 1, 2};
    irr = intersect(ideal(x_(0,0), x_(0,1)), ideal(x_(1,0), x_(1,1)), ideal(x_(2,0), x_(2,1), x_(2,2)))
    I = saturate(intersect apply(6,i-> ideal(random({1,0,0},S),random({0,1,0},S), random({0,0,1},S),random({0,0,1},S))), irr);
    assert(length(multigradedRegularity(S, I)) > 0)
///
