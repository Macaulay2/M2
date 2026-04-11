
------------------------------------------------------------------------
------------------------------- Tests ----------------------------------
------------------------------------------------------------------------

TEST /// -- Test 0 -- cubic fourfolds from strings: describe, discriminant, parameterCount
strIn := {"quintic del Pezzo surface", "quartic scroll", "3-nodal septic scroll", "one-nodal septic del Pezzo surface", "general cubic 4-fold of discriminant 38", "general cubic 4-fold of discriminant 42", "cubic 4-fold of discriminant 48"};
strOut := "Special cubic fourfold of discriminant 14
containing a rational surface of degree 5 and sectional genus 1
cut out by 5 hypersurfaces of degree 2
Special cubic fourfold of discriminant 14
containing a rational surface of degree 4 and sectional genus 0
cut out by 6 hypersurfaces of degree 2
Special cubic fourfold of discriminant 26
containing a 3-nodal surface of degree 7 and sectional genus 0
cut out by 13 hypersurfaces of degree 3
Special cubic fourfold of discriminant 26
containing a 1-nodal surface of degree 7 and sectional genus 1
cut out by 14 hypersurfaces of degree 3
Special cubic fourfold of discriminant 38
containing a rational surface of degree 10 and sectional genus 6
cut out by 10 hypersurfaces of degree 3
Special cubic fourfold of discriminant 42
containing a 5-nodal surface of degree 9 and sectional genus 2
cut out by 9 hypersurfaces of degree 3
Special cubic fourfold of discriminant 48
containing a 6-nodal surface of degree 9 and sectional genus 2
cut out by 5 hypersurfaces of degrees 2^1 3^4 
";
X = apply(strIn,cubicFourfold);
-- X = apply(strIn,x -> cubicFourfold(x,InputCheck=>10,Verbose=>true));
assert all(X,x -> x.cache#?(surface x,"label"));
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> cubicFourfold surface x);
assert all(Y,y -> not y.cache#?(surface y,"label"));
assert(apply(Y,discriminant) == {14,14,26,26,38,42,48});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_0,Verbose=>true) == (1, (25, 35, 5)) and parameterCount(Y_1,Verbose=>true) == (1, (28, 29, 2)));
///

TEST /// -- Test 1 (1/2) -- GM fourfolds from strings: describe, discriminant, parameterCount, toGrass
strIn := {"sigma-plane", "rho-plane", "tau-quadric"};
strOut := "Special Gushel-Mukai fourfold of discriminant 10('')
containing a plane
and with class in G(1,4) given by s_(3,1)
Type: ordinary
(case 6 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 12
containing a plane
and with class in G(1,4) given by s_(2,2)
Type: ordinary
(case 9 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 10(')
containing a surface of degree 2 and sectional genus 0
cut out by 6 hypersurfaces of degrees 1^5 2^1 
and with class in G(1,4) given by s_(3,1)+s_(2,2)
Type: ordinary
(case 1 of Table 1 in arXiv:2002.07026)
";
X = apply(strIn,gushelMukaiFourfold);
assert(apply(X,x -> x.cache#(surface x,"label")) == {6, 9, 1});
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> gushelMukaiFourfold(sub(ideal (toGrass x) surface x,ring target toGrass x),InputCheck=>0))
assert all(Y,y -> not y.cache#?(surface y,"label"));
assert(apply(Y,discriminant) == {10, 12, 10});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_0,Verbose=>true) == (2, (34, 4, 0)) and parameterCount(Y_1,Verbose=>true) == (3, (34, 3, 0)));
///

TEST /// -- Test 2 (2/2) -- GM fourfolds from strings: describe, discriminant, parameterCount, toGrass
strIn := {"cubic scroll", "quintic del Pezzo surface", "general GM 4-fold of discriminant 20"};
strOut := "Special Gushel-Mukai fourfold of discriminant 12
containing a surface of degree 3 and sectional genus 0
cut out by 7 hypersurfaces of degrees 1^4 2^3 
and with class in G(1,4) given by 2*s_(3,1)+s_(2,2)
Type: ordinary
(case 7 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 10('')
containing a surface of degree 5 and sectional genus 1
cut out by 8 hypersurfaces of degrees 1^3 2^5 
and with class in G(1,4) given by 3*s_(3,1)+2*s_(2,2)
Type: ordinary
(case 4 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 20
containing a surface of degree 9 and sectional genus 2
cut out by 19 hypersurfaces of degree 2
and with class in G(1,4) given by 6*s_(3,1)+3*s_(2,2)
Type: ordinary
(case 17 of Table 1 in arXiv:2002.07026)
";
X = apply(strIn,x -> clean gushelMukaiFourfold x);
debug SpecialFanoFourfolds;
assert(apply(X,recognize) == {7, 4, 17});
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> gushelMukaiFourfold(sub(ideal (toGrass x) surface x,ring target toGrass x),InputCheck=>0))
assert all(Y,y -> not y.cache#?(surface y,"label"));
assert(apply(Y,discriminant) == {12, 10, 20});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_1,Verbose=>true) == (1, (24, 18, 3)));
///

TEST /// -- Test 3 -- 21 examples from GMtables
X = for i from 1 to 21 list (
   A = GMtables(i,ZZ/65521);
   time gushelMukaiFourfold((rationalMap(ideal A_0,Dominant=>2)) ideal A_1,InputCheck=>0)
);
S = apply(X,x -> surface x);
assert(apply(X,x -> degree surface x) === {2, 4, 14, 5, 9, 1, 3, 7, 1, 10, 10, 14, 12, 8, 9, 11, 9, 7, 10, 4, 12});
assert(apply(X,x-> sectionalGenus surface x) == {0, 0, 8, 1, 3, 0, 0, 2, 0, 4, 3, 8, 5, 2, 3, 5, 2, 0, 3, 0, 5});
assert(last cycleClass X_18 == (6,4) and discriminant X_18 == 24);
assert(last cycleClass X_7 == (4,3) and discriminant X_7 == 12);
///

TEST /// -- Test 4 -- parametrizations of Fano fourfolds
setRandomSeed 0;
for dg in {(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)} do (
    <<"(d,g) = "<<dg<<endl;
    X = fanoFourfold dg;
    assert(dim X == 4 and degree X == dg_0 and (genera ideal X)_3 == dg_1);
    if member(dg,{(2,0),(4,1),(5,1),(16,9)}) then (
        time f = parametrizeFanoFourfold X;
        assert(source f == ambient source f and dim source f == 4);
        assert(target f === X);
        g = f#"inverse";
        assert(g =!= null);
        p = point source f;
        assert(g f p == p);
    );
);
///

TEST /// -- Test 5 -- rational and unirational parametrizations
X = cubicFourfold surface({3,4},ZZ/333331);
time h = parametrize X;
assert(degree(h,Strategy=>"random point") == 1 and target h === X and ambient source h == source h and h#"inverse" =!= null);
time f = unirationalParametrization X;
assert(# factor f == 1 and target f === X and unique degrees ideal matrix first factor f == {{10}});
assert isSubset(f point source f,X);
S = schubertCycle({3,1},GG(ZZ/33331,1,4),Standard=>true);
Y = gushelMukaiFourfold S;
time g = parametrize Y;
assert(degree(g,Strategy=>"random point") == 1 and target g === Y and dim ambient source g == 5 and dim source g == 4 and g#"inverse" =!= null);
-- time g = unirationalParametrization Y;
-- assert(# factor g == 1 and target g === Y and unique degrees ideal matrix first factor g == {{26}})
-- assert isSubset(g point source g,Y)
///

TEST /// -- Test 6 (1/3) -- associated K3 surfaces
f = last building associatedK3surface(cubicFourfold "quartic scroll",Verbose=>false);
assert(f#"image" =!= null and dim image f == 2 and degree image f == 14 and dim target f == 8)
///

TEST /// -- Test 7 (2/3) -- associated K3 surfaces
g = last building associatedK3surface(cubicFourfold "quintic del Pezzo surface",Verbose=>true,Singular=>false);
assert(g#"image" =!= null and dim image g == 2 and degree image g == 14 and dim target g == 8)
///

TEST /// -- Test 8 (3/3) -- associated K3 surfaces
building associatedK3surface(gushelMukaiFourfold "tau-quadric",Verbose=>false);
///

TEST /// -- Test 9 -- simple tests on schubertCycle
debug MultiprojectiveVarieties;
S = schubertCycle({2,2},GG(ZZ/33331,1,4),Standard=>true)
assert(idealOfSubvariety S == idealOfSubvariety tangentialChowForm(projectiveVariety ideal((Grass(0,4,ZZ/33331,Variable=>"x"))_0,(Grass(0,4,ZZ/33331,Variable=>"x"))_1),1,1))
S = schubertCycle({3,2,1},GG(ZZ/33331,2,5),Standard=>true)
use ring ambientVariety S;
assert(idealOfSubvariety S == ideal(x_(0,4,5),x_(0,3,5),x_(1,2,5),x_(0,2,5),x_(0,1,5),x_(2,3,4),x_(1,3,4),x_(0,3,4),x_(1,2,4),x_(0,2,4),x_(0,1,4),x_(1,2,3),x_(0,2,3),x_(0,1,3),x_(0,1,2)))
///

TEST /// -- Test 10 (1/2) -- detectCongruence
X = cubicFourfold("quintic del Pezzo surface",ZZ/33331);
detectCongruence(X,Verbose=>true);
///

TEST /// -- Test 11 (2/2) -- detectCongruence
use Grass(1,4,ZZ/33331);
S31 = ideal(p_(3,4),p_(2,4),p_(1,4),p_(0,4),p_(2,3),p_(1,3),p_(1,2));
Y = gushelMukaiFourfold(S31,InputCheck=>0);
assert(not Y.cache#?(surface Y,"label")); Y.cache#(surface Y,"label") = 6;
detectCongruence(Y,Verbose=>true);
-- Y = gushelMukaiFourfold("18",ZZ/3331);
-- detectCongruence Y;
///

TEST /// -- Test 12 (1/2) -- GM fourfolds containing nodal surfaces
debug SpecialFanoFourfolds;
K = ZZ/65521;
X = makeGMfromCurveOnSurfaceInP6((surface({2,0,0,0},K,ambient=>6)).cache#"takeCurve" (1,(0,0,0)),InputCheck=>0);
assert(discriminant X == 36);
assert(numberNodes surface X == 1);
X' = random X;
assert(surface X === surface X' and ambientFivefold X === ambientFivefold X' and isSubset(surface X',X') and dim(X*X') == 3);
assert(discriminant X' == 44 and discriminant X == 44);
///

TEST /// -- Test 13 (2/2) -- GM fourfolds containing nodal surfaces
X = gushelMukaiFourfold("nodal surface of degree 11 and genus 3 with class (7,4)",ZZ/33331,InputCheck=>0);
assert(discriminant X == 26 and last cycleClass X == (7,4) and degree surface X == 11 and sectionalGenus surface X == 3);
Y = gushelMukaiFourfold("nodal D44",ZZ/33331,InputCheck=>0);
assert(discriminant Y == 44 and last cycleClass Y == (6,3) and degree surface Y == 9 and sectionalGenus surface Y == 1);
///

TEST /// -- Test 14 -- gluing scrolls along curves
debug SpecialFanoFourfolds
S = surface({3,4,0,0},ambient=>6);
for a in {(1,0),(2,0),(3,0),(4,0),(5,0),(5,1)} do (
    (d,g) := a;
    E := curvesOnSurface(S,d,g);
    assert(#E>0);
    for C in E do (
        <<"(d,g) = "<<(d,g)<<", curve: "<<? ideal C<<endl;
        assert(degree C == d and sectionalGenus C == g);
        B := glueScroll C;
        assert(dim B == 3 and degree B == 3 and degrees B == {({1},1),({2},3)} and isSubset(C,S*B));
        if g == 0 then (
            B' := glueScroll' C;
            assert(dim B' == 4 and degree B' == 4 and degrees B' == {({2},1),({3},3)} and isSubset(C,B'*S));
        );
    );
);
C = first curvesOnSurface(surface({3,3,0,0},ZZ/333331),6,0);
assert(dim C == 1 and degree C == 6 and sectionalGenus C == 0)
B = glueScroll' C;
assert(dim B == 4 and degree B == 4 and degrees B == {({2},1), ({3},3)} and isSubset(C,B) and isSubset(C,(ambientVariety C)*B))
///

TEST /// -- Test 15
debug SpecialFanoFourfolds
L = takeGMsfromSurfaceInP6(surface({3,1,1,0},ambient=>6),InputCheck=>0,"Gluing"=>"cubic scroll",Degrees=>hashTable{1=>(1,1),2=>(19,infinity),3=>(0,0)});
X = first L;
assert(#L == 1 and discriminant X == 18 and last cycleClass X == (5,3))
-- L = takeGMsfromSurfaceInP6(surface({3,1,1,0},ambient=>6),InputCheck=>0,"Gluing"=>"quartic scroll",Degrees=>hashTable{1=>(1,1),2=>(19,infinity),3=>(0,0)});
-- X = first L;
-- assert(#L == 1 and discriminant X == 20 and last cycleClass X == (4,3))
///

TEST /// -- Test 16 -- new description for fourfolds in v.2.8
X = cubicFourfold "quartic scroll";
mirrorFourfold X;
s1 = "Special cubic fourfold of discriminant 14
containing a rational surface of degree 4 and sectional genus 0
cut out by 6 hypersurfaces of degree 2
K3 status: [▓▓▓░░ / ▓▓▓▓▓]
Mirror fourfold: hypersurface in PP^5 of degree 2
Surface U of degree 10, sectional genus 7, χ(O_U) = 2, cut out by 7 hypersurfaces of degrees 2^1 3^6 ";
assert(toString describe X === s1)
associatedK3surface X
s2 = "Special cubic fourfold of discriminant 14
containing a rational surface of degree 4 and sectional genus 0
cut out by 6 hypersurfaces of degree 2
K3 status: [▓▓▓▓▓ / ▓▓▓▓▓]
Mirror fourfold: hypersurface in PP^5 of degree 2
Surface U of degree 10, sectional genus 7, χ(O_U) = 2, cut out by 7 hypersurfaces of degrees 2^1 3^6 
Exceptional curves: an irreducible conic curve
Minimal K3 surface Ũ: degree 14 and sectional genus 8 in PP^8 cut out by 15 hypersurfaces of degree 2";
assert(toString describe X === s2)
///

TEST /// -- Test 17
X = specialFourfold "plane in PP^7";
assert(discriminant X == 31);
S = associatedCastelnuovoSurface X;
assert((dim S,dim ambient S,degree S,sectionalGenus S,degrees S) == (2, 4, 9, 9, {({3}, 1), ({4}, 3)}))
///

TEST /// -- Test 18
debug MultiprojectiveVarieties;
K := ZZ/65521;
P := PP_K(1,1,1,4);
C = random({{2},{3}},0_P)
assert(discriminant(surface(C,random(8,C)),Algorithm=>2) == -7)
D = internalProjection_2 (surface({3, 1, 1, 0},K)).cache#"takeCurve"(3,{0, 0, 0});
x := gens ring P;
f := (rationalMap {{x_0^4,x_0^3*x_1,x_0^3*x_2,x_3}}) << (ambient D);
E = f^* D;
assert(discriminant(surface(E,random(8,E)),Algorithm=>1) == -43)
E' = f^* random D;
assert(discriminant(surface(E',random(8,E')),Algorithm=>2) == -43)
///

TEST /// -- test 19 DSCF
-- generateInputsForRunExampleTest 6
debug SpecialFanoFourfolds;
runExampleTest(6,64997,4,0,20,6,4,2,4,1,0,0,0,4,matrix {{6, 5}, {5, -2}})
///

TEST /// -- test 20 DSCF
debug SpecialFanoFourfolds;
(B,V,C) = GMtables(1,ZZ/65521);
assert(B * V == C and dim C == 1)
X = specialFourfold(B & V);
assert(surfaceIntersectionNumber X == 1)
assert(latticeIntersectionMatrix3x3 X == matrix {{3, 3, 2}, {3, 7, 1}, {2, 1, 4}})
///

TEST /// -- test 21 DSCF
debug SpecialFanoFourfolds;
debug MultiprojectiveVarieties;
S = nodalProjection surface {3,3};
n = inverseNormalizationMapRaw(S,"S",1,true);
assert(dim target n == 6 and dim image n == 2 and degree image n == 6 and euler hilbertPolynomial image n == 1)
S' = nodalProjection surface {3,1};
n' = inverseNormalizationMapExperimentalRaw(S',"S'",1,true)
assert(dim target n' == 8 and dim image n' == 2 and degree image n' == 8 and euler hilbertPolynomial image n' == 1)
///

TEST /// -- test 22 DSCF
debug SpecialFanoFourfolds;
L = discoverCubicFourfoldsInC8({4,4,3,2,1},1,4,5,7,"test_file",true,300,{});
assert(apply(L,discriminant) == {14, 20, 24, 32, 38})
assert(fileExists "test_file.txt" and fileExists "test_file_commands.m2")
removeFile "test_file.txt"
removeFile "test_file_commands.m2"
///

TEST /// -- test 23 DSCF -- switch between Fano map types
debug SpecialFanoFourfolds;
X = specialFourfold("DSCF-"|(toString 6))
E = polarizedK3surface(X,Verbose=>true,Strategy=>("Approximate","MapFromU-Virtual"),FanoMapType=>"P2xP2")
assert(computationStatus X == 2);
(mu,U,LC,f) = building E;
assert(U === surface mirrorFourfold X);
assert(mirrorFourfold mirrorFourfold X === X);
assert(degree U == 19 and sectionalGenus U == 15 and euler hilbertPolynomial U == 4);
W = target mu;
assert(W == mirrorFourfold X);
assert isDeformationP2P2 mirrorFourfold X
assert(isFanoMapToP2xP2 X and fanoMapDSCF X === mu);
assert(E#"LatticePolarization" === null);
polarizedK3surface(X,Verbose=>true);
assert(E#"LatticePolarization" =!= null);
assert(computationStatus X == 2);
assert(isVirtualLatticeK3 E);
assert instance(latticePolarization E, LatticePolarizationOnK3Surface);
assert(latticeMatrix latticePolarization E == matrix {{3, 7}, {7, 2}});
assert(recoverFourfold E === X and recoverFourfold projectiveVariety E === X);
assert("Approximate" === U.cache#"strategy for surface U");
assert(not U.cache#?"birational maps from X to W and from W to X");
E' = polarizedK3surface(X,Verbose=>true,Strategy=>("Inverse","MapFromW-Virtual"),FanoMapType=>"Standard")
W' = mirrorFourfold X;
U' = (building E')_1;
assert(not isDeformationP2P2 W');
assert(isFanoMapStandard X and fanoMapDSCF X === first building E');
assert(E'#"LatticePolarization" === null);
polarizedK3surface(X,Verbose=>true);
assert(E'#"LatticePolarization" =!= null);
assert(computationStatus X == 3);
assert(isVirtualLatticeK3 E');
assert instance(latticePolarization E', LatticePolarizationOnK3Surface);
assert(latticeMatrix latticePolarization E' ==  matrix {{6, 12}, {12, 2}});
assert(recoverFourfold E' === X and recoverFourfold projectiveVariety E' === X);
assert("Inverse" === U'.cache#"strategy for surface U");
assert(U'.cache#?"birational maps from X to W and from W to X");
E'' = polarizedK3surface polarizedK3surface(X,Verbose=>true,Strategy=>"Approximate",FanoMapType=>"P2xP2")
assert(isDeformationP2P2 mirrorFourfold X);
assert(latticeMatrix latticePolarization E'' == matrix {{3, 7}, {7, 2}});
///

TEST /// -- test 24 DSCF
-- reduced complexity to avoid memory allocation failures
debug SpecialFanoFourfolds;
for i in {1,5,7,20,40} do assert(recognizeDSCF specialFourfold("DSCF-"|(toString i)) === "DSCF-V1-"|toString(i))
///

TEST /// -- test 25 DSCF -- Tregub
debug SpecialFanoFourfolds;
K = ZZ/65521;
S = PP_K^(2,2);
P = linearSpan {point S,point S,point S};
X = specialFourfold(S & P);
describe X
assert(latticeIntersectionMatrix3x3 X == matrix {{3, 4, 1}, {4, 12, 3}, {1, 3, 3}})
Utilde = polarizedK3surface(X,Strategy=>"Approximate",Verbose=>true)
assert(X.cache#(first surfaces X,last surfaces X,"labelDSCF") === "Tregub1")
assert(recognizeDSCF X === "Tregub1")
(mu,U,C,f) = building Utilde;
assert(degree U == 18 and sectionalGenus U == 10 and euler hilbertPolynomial U == -1)
///

TEST /// -- test 26 DSCF
-- generateInputsForRunExampleTest 33
debug SpecialFanoFourfolds;
runExampleTest(33,42169,8,1,56,13,8,2,7,7,3,1,0,8,matrix {{14, 9}, {9, 2}})
///

TEST /// -- test 27 DSCF
-- generateInputsForRunExampleTest 21
debug SpecialFanoFourfolds;
runExampleTest(21,38113,7,1,38,16,10,2,8,8,3,2,0,10,matrix {{18, 9}, {9, 2}})
///

TEST /// -- test 28 DSCF
debug SpecialFanoFourfolds;
X = specialFourfold surface((2,0,0),(1,0,0),ZZ/61001);
assert(char coefficientRing X === 61001);
T = polarizedK3surface polarizedK3surface X;
assert(computationStatus T == 4)
f = map(T,1,1);
assert(source f === projectiveVariety T and dim ambient source f == 4 and dim ambient target f == 8)
E = T(1,1);
assert(E === image f and degree E == 14 and sectionalGenus E == 8)
assert((latticePolarization T)(1,1) === E)
T' = polarizedK3surface(T,Strategy=>"MapFromU-Virtual")
assert(computationStatus T == 3)
assert instance(T'(1,1),LatticePolarizationOnK3Surface)
assert((polarizedK3surface(T,Strategy=>"SpecialCurve"))(1,1) === E)
///
