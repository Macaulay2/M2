-------------------------
-------------------------
--**TESTS SECTIONS**--
-------------------------
-------------------------

TEST ///
--isPartialASM
L = {
    matrix{{1}},
    matrix{{1,0},{0,1}},
    matrix{{0,1},{1,0}},
    matrix{{0,1,0},{0,0,1},{1,0,0}},
    matrix{{0,1,0},{1,-1,1},{0,1,0}},
    matrix{{0,1,0,0},{0,0,1,0},{1,0,0,0},{0,0,0,1}},
    matrix{{1,0,0,0},{0,0,1,0},{0,1,-1,1},{0,0,1,0}},
    matrix{{0,0,1,0},{0,1,-1,1},{1,-1,1,0},{0,1,0,0}},
    matrix{{0,0,1,0},{1,0,-1,1},{0,1,0,0},{0,0,1,0}},
    matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}},
    matrix{{0,0,1,0,0,0,0,0},{1,0,-1,0,1,0,0,0},{0,0,0,1,-1,0,0,1},{0,0,1,-1,1,0,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,1,0,0},{0,1,-1,1,0,0,0,0},{0,0,1,0,0,0,0,0}},
    matrix{{0,0,0,0,1,0,0,0},{0,0,1,0,-1,1,0,0},{0,0,0,1,0,0,0,0},{1,0,0,-1,1,-1,1,0},{0,1,-1,1,-1,1,0,0},{0,0,0,0,1,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,0,0,0,0,1}},
    matrix{{0,0,0},{0,1,0},{1,-1,0}},
    matrix{{1,0,0},{0,0,0}},
    matrix{{1,0,0},{0,0,1}},
    matrix{{0,1,0},{1,-1,0}},
    matrix{{0,1,0},{1,-1,0}},
    matrix{{0,0,1},{1,0,-1}},
    matrix{{0,0,1,0,0},{0,0,0,0,1},{0,0,0,0,0},{0,1,0,0,0}}
    };
assert(apply(L,isPartialASM) == toList (#L:true))



T = {
    matrix{{-1}},
    matrix{{0,1,0},{1,0,1},{0,1,0}},
    matrix{{0,0,1,0,0,0,0,0},{1,0,1,0,1,0,0,0},{0,0,0,1,-1,0,0,1},{0,0,1,-1,1,0,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,1,0,0},{0,1,-1,1,0,0,0,0},{0,0,1,0,0,0,0,0}},
    matrix{{1,0,0,0},{0,0,1,0},{-1,1,0,0},{1,0,-1,1}}
    };
assert( apply(T, isPartialASM) == toList (#T:false))
///

TEST ///
---partialASMToASM
assert(partialASMToASM matrix{{0,0,1,0},{1,0,-1,0},{0,0,0,0}} == matrix{{0,0,1,0,0,0},{1,0,-1,0,1,0},{0,0,0,0,0,1},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0}})
///

TEST ///
--antiDiagInit
I = antiDiagInit({1,3,2});
R = ring I;
assert(I == ideal ( R_1 * R_3));

I = antiDiagInit(matrix{{0,0,0,1},{0,1,0,0},{1,-1,1,0},{0,1,0,0}});
R = ring I;
assert(I == ideal (R_0,R_1,R_2,R_4,R_5*R_8));
///

TEST ///
--rankTable
assert(rankTable({1,3,2}) == matrix{{1, 1, 1}, {1, 1, 2}, {1, 2, 3}});
assert(rankTable(matrix{{0,0,0,1},{0,1,0,0},{1,-1,1,0},{0,1,0,0}}) == matrix{{0, 0, 0, 1}, {0, 1, 1, 2}, {1, 1, 2, 3}, {1, 2, 3, 4}});
///

TEST ///
-- rotheDiagram

assert(sort rotheDiagram matrix{{0,0,1,0,0},{1,0,-1,0,0},{0,1,0,0,0},{0,0,1,0,0}} == sort {(1,1),(1,2),(2,3),(2,4),(2,5)})
assert(sort rotheDiagram {2,6,5,1,4,3} == sort {(1,1),(2,1),(2,3),(2,4),(2,5),(3,1),(3,3),(3,4),(5,3)})
///

TEST ///
-- augmentedRotheDiagram 

assert(sort augmentedRotheDiagram {2,1,5,4,3} == sort {((1,1),0), ((3,3),2),((3,4),2), ((4,3),2)})
assert(sort augmentedRotheDiagram matrix{{0,1,0},{1,-1,1},{0,1,0}} == sort{((1,1),0), ((2,2),1)})
assert (sort augmentedRotheDiagram matrix {{0,0,1,0,0},{1,0,0,0,0},{0,1,-1,1,0},{0,0,0,0,1},{0,0,1,0,0}} == sort {((1,1),0),((1,2),0),((4,3),2),((3,3),2)})
///

TEST ///
--essentialSet
--Example 2.1 in Weigandt "Prism Tableaux for ASMs"
A = matrix{{0,0,0,1},{0,1,0,0},{1,-1,1,0},{0,1,0,0}};
assert(isPartialASM A)
assert(sort essentialSet(A) == {(1,3),(2,1),(3,2)})
///

TEST ///
-- essentialSet

assert(essentialSet({2,1,6,3,5,4 })== {(1, 1), (3, 5), (5, 4)})
assert(essentialSet matrix {{0,1,0,0,0,0},{1,0,0,0,0,0},{0,0,0,0,0,1},{0,0,1,0,0,0},{0,0,0,0,1,0},{0,0,0,1,0,0}} == {(1, 1), (3, 5), (5, 4)})

assert(essentialSet({1}) == {})
assert(essentialSet(matrix {{0,0,1,0,0},{1,0,0,0,0},{0,1,-1,1,0},{0,0,0,0,1},{0,0,1,0,0}}) == {(1,2),(4,3)}) -- previously broken example
///


TEST ///
-- augmentedEssentialSet

assert(augmentedEssentialSet({2,1,6,3,5,4 })== {((1, 1), 0), ((3, 5), 2), ((5, 4), 3)})
assert(augmentedEssentialSet matrix {{0,1,0,0,0,0},{1,0,0,0,0,0},{0,0,0,0,0,1},{0,0,1,0,0,0},{0,0,0,0,1,0},{0,0,0,1,0,0}} == {((1, 1), 0), ((3, 5), 2), ((5, 4), 3)})

assert(augmentedEssentialSet({1}) == {})
assert(augmentedEssentialSet(matrix {{0,0,1,0,0},{1,0,0,0,0},{0,1,-1,1,0},{0,0,0,0,1},{0,0,1,0,0}}) == {((1, 2), 0), ((4, 3), 2)}) -- previously broken example
///


--TODO: make more complicated tests
TEST ///
--schubertDeterminantalIdeal
--Example 15.4 from Miller-Sturmfels
I = schubertDeterminantalIdeal({1,2,3});
assert(I == ideal(0_(ring I)));

I = schubertDeterminantalIdeal({2,1,3});
assert(I == ideal((ring I)_0));

I = schubertDeterminantalIdeal({2,3,1});
assert(I == ideal((ring I)_0, (ring I)_3));

I = schubertDeterminantalIdeal({3,2,1});
assert(I == ideal((ring I)_0, (ring I)_1, (ring I)_3));

I = schubertDeterminantalIdeal({1,3,2});
assert(I == ideal((ring I)_0 * (ring I)_4 - (ring I)_1 * (ring I)_3));
///

TEST ///
--fultonGens
L = fultonGens matrix{{0,1,0},{1,-1,1},{0,1,0}};
assert(toExternalString L_0 == "z_(1,1)")
assert(toExternalString L_1 == "-z_(1,2)*z_(2,1)+z_(1,1)*z_(2,2)")


L = fultonGens {2,5,4,1,3};
assert(toExternalString L_0 == "-z_(1,2)*z_(2,1)+z_(1,1)*z_(2,2)");
assert(toExternalString L_1 == "-z_(1,3)*z_(2,1)+z_(1,1)*z_(2,3)");
assert(toExternalString L_2 == "-z_(1,3)*z_(2,2)+z_(1,2)*z_(2,3)");
assert(toExternalString L_3 == "-z_(1,4)*z_(2,1)+z_(1,1)*z_(2,4)");
assert(toExternalString L_4 == "-z_(1,4)*z_(2,2)+z_(1,2)*z_(2,4)");
assert(toExternalString L_5 == "-z_(1,4)*z_(2,3)+z_(1,3)*z_(2,4)");
assert(toExternalString L_6 == "z_(1,1)");
assert(toExternalString L_7 == "z_(2,1)");
assert(toExternalString L_8 == "z_(3,1)");
assert(toExternalString L_9 == "-z_(1,2)*z_(3,1)+z_(1,1)*z_(3,2)");
assert(toExternalString L_10 == "-z_(2,2)*z_(3,1)+z_(2,1)*z_(3,2)");
assert(toExternalString L_11 == "-z_(1,3)*z_(3,1)+z_(1,1)*z_(3,3)");
assert(toExternalString L_12 == "-z_(2,3)*z_(3,1)+z_(2,1)*z_(3,3)");
assert(toExternalString L_13 == "-z_(1,3)*z_(3,2)+z_(1,2)*z_(3,3)");
assert(toExternalString L_14 == "-z_(2,3)*z_(3,2)+z_(2,2)*z_(3,3)");

///

TEST ///
-- entrywiseMinRankTable
assert(entrywiseMinRankTable(({{4,3,1,2},{2,4,3,1}} / permToMatrix)) == matrix{{0, 0, 0, 1}, {0, 0, 1, 2}, {0, 1, 2, 3}, {1, 2, 3, 4}});
///


TEST ///
-- entrywiseMaxRankTable
assert(entrywiseMaxRankTable(({{4,3,1,2},{2,4,3,1}} / permToMatrix)) == matrix{{0, 1, 1, 1}, {0, 1, 1, 2}, {1, 1, 2, 3}, {1, 2, 3, 4}});
///

TEST ///
-- isASMUnion
assert isASMUnion {{1}}
assert isASMUnion {{4,3,2,1}}
assert not isASMUnion {{2,1,3},{1,3,2}}
assert isASMUnion {{3,1,2},{2,3,1}}
assert isASMUnion {{4,1,3,2},{3,4,1,2},{2,4,3,1}}
assert isASMUnion {{2,1,3}} --catches old bug from using cycle Decompositions
assert isASMUnion {{2,1,3},{3,1,2}} --catches old bug that misses containment
///

TEST ///
-- isMinRankTable
T1 = matrix {{0,1,1},{1,1,2},{1,2,3}}
T2 = matrix {{1,1,1,1,1},{1,2,2,2,2},{1,2,2,2,3},{1,2,2,3,3},{1,2,3,3,3}}
F1 = matrix {{1,0,1,0},{0,1,0,-1},{2,2,0,0},{3,5,8,0}}
F2 = matrix {{1,1,1,1,1},{1,2,2,2,0},{1,2,2,2,3},{1,2,2,3,3},{1,2,3,3,3}}

assert(isMinRankTable(T1))
assert(isMinRankTable(T2))
assert(not isMinRankTable(F1))
assert(not isMinRankTable(F2))
///

TEST///
-- rankTableToASM
Ar = matrix {{0,0,1,1},{0,1,1,2},{1,2,2,3},{1,2,3,4}}
A = matrix {{0,0,1,0},{0,1,-1,1},{1,0,0,0},{0,0,1,0}}
assert(rankTableToASM(Ar) == A)

Br = matrix {{0,0,1,1,1},{1,1,1,2,2},{1,2,2,3,3},{1,2,3,4,4},{1,2,3,4,5}}
B = matrix {{0,0,1,0,0},{1,0,-1,1,0},{0,1,0,0,0},{0,0,1,0,0},{0,0,0,0,1}}
assert(rankTableToASM(Br) == B)
///

TEST///
-- rankTableFromMatrix
Am = matrix {{1,0,0},{0,23,24},{23,24,25}}
A = matrix {{0,0,0},{0,1,1},{1,2,2}}
assert(rankTableFromMatrix(Am) == A)
///

TEST ///
--schubertIntersect
I=schubertIntersect {matrix {{0,1,0},{1,-1,1},{0,1,0}}, {3,2,1}};
R=ring I;
assert(I==ideal(R_0,R_1*R_3));
///


TEST ///
--schubertAdd
I=schubertAdd {matrix {{0,1,0},{1,-1,1},{0,1,0}}, {3,2,1}};
R=ring I;
assert(I==ideal(R_0,R_1,R_3));
///

--Testing Permutation Functions--

TEST ///
--composePerms
assert(composePerms({2,3,4,1}, {4,3,2,1}) == {1,4,3,2})
assert(composePerms({4,3,2,1}, {4,3,2,1}) == {1,2,3,4})
assert(composePerms({1,2,3,4,5}, {3,5,2,1,4}) == {3,5,2,1,4})
assert(composePerms({3,5,2,1,4}, {1,2,3,4,5}) == {3,5,2,1,4})
///

TEST ///
--isPatternAvoiding
assert(not isPatternAvoiding({2,3,7,1,5,8,4,6}, {1,4,3,2}));
assert(isPatternAvoiding({1,4,6,2,3,7,5}, {1,4,3,2}));

assert(not isPatternAvoiding({7,2,5,8,1,3,6,4}, {2,1,4,3}));
assert(isPatternAvoiding({1,6,9,2,4,7,3,5,8}, {2,1,4,3}));

assert(not isPatternAvoiding({3,1,2},{3,1,2}));
assert(not isPatternAvoiding({1,2,3,6,4,5}, {3,1,2}));
assert(isPatternAvoiding({3,1,2},{2,3,1}));

--isVexillary
assert(not isVexillary({7,2,5,8,1,3,6,4}));
assert(isVexillary({1,6,9,2,4,7,3,5,8}));
///

TEST ///
--isCDG
assert(isCDG({5,4,3,2,1}));

--isCartwrightSturmfels
assert(isCartwrightSturmfels({5,4,3,2,1}));
///

TEST /// 
-- permLength 

assert(permLength {1} == 0)
assert(permLength {1,2} == 0)
assert(permLength {3,2,1} == 3)
assert(permLength {2,1,3} == 1)
assert(permLength {8,7,6,5,4,3,2,1} == 28)
///


TEST ///
-- schubertCodim
L = {
    {1},
    {2,1},
    matrix{{0,0,0,1},{0,1,0,0},{1,-1,1,0},{0,1,0,0}},
    matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}}
}
assert all (L, i -> schubertCodim i == codim schubertDeterminantalIdeal i)
///

TEST ///
--KPolynomialASM
A = matrix{{0, 0, 0}, {0, 1, 0}, {1, -1, 0}};
assert(toExternalString KPolynomialASM A == "1-3*T_0-T_1+3*T_0^2+3*T_0*T_1-3*T_1*T_2-T_0^3-3*T_0^2*T_1+9*T_0*T_1*T_2+4*T_1^2*T_2+T_1*T_2^2+T_0^3*T_1-9*T_0^2*T_1*T_2-12*T_0*T_1^2*T_2-3*T_0*T_1*T_2^2-T_1^3*T_2-T_1^2*T_2^2+3*T_0^3*T_1*T_2+12*T_0^2*T_1^2*T_2+3*T_0^2*T_1*T_2^2+3*T_0*T_1^3*T_2+3*T_0*T_1^2*T_2^2-4*T_0^3*T_1^2*T_2-T_0^3*T_1*T_2^2-3*T_0^2*T_1^3*T_2-3*T_0^2*T_1^2*T_2^2+T_0^3*T_1^3*T_2+T_0^3*T_1^2*T_2^2");
assert(toExternalString KPolynomialASM matrix {{1,0,0},{0,1,0},{0,0,1}} == "1");
assert(toExternalString KPolynomialASM matrix {{0, 0, 1, 0, 0}, {0, 1, -1, 1, 0}, {1, -1, 1, 0, 0}, {0, 1, 0, -1, 0}, {0, 0,0, 1, 0}}  == "1-2*T_0-T_1+T_0^2+T_0*T_1-T_1*T_2+T_0^2*T_1+T_0*T_1^2+3*T_0*T_1*T_2+T_1^2*T_2-T_0^3*T_1-2*T_0^2*T_1^2-3*T_0^2*T_1*T_2-3*T_0*T_1^2*T_2-5*T_0*T_1*T_2*T_3+T_0^3*T_1^2+T_0^3*T_1*T_2+3*T_0^2*T_1^2*T_2+11*T_0^2*T_1*T_2*T_3+6*T_0*T_1^2*T_2*T_3+T_0*T_1*T_2^2*T_3+T_0*T_1*T_2*T_3^2-T_0^3*T_1^2*T_2-7*T_0^3*T_1*T_2*T_3-8*T_0^2*T_1^2*T_2*T_3-2*T_0^2*T_1*T_2^2*T_3-2*T_0^2*T_1*T_2*T_3^2-T_0*T_1^3*T_2*T_3+4*T_0*T_1^2*T_2^2*T_3-T_0*T_1^2*T_2*T_3^2+T_0^4*T_1*T_2*T_3-3*T_0^3*T_1^2*T_2*T_3+T_0^3*T_1*T_2^2*T_3+T_0^3*T_1*T_2*T_3^2-4*T_0^2*T_1^3*T_2*T_3-15*T_0^2*T_1^2*T_2^2*T_3+T_0^2*T_1^2*T_2*T_3^2-6*T_0*T_1^3*T_2^2*T_3-T_0*T_1^2*T_2^3*T_3-T_0*T_1^2*T_2^2*T_3^2+6*T_0^4*T_1^2*T_2*T_3+12*T_0^3*T_1^3*T_2*T_3+19*T_0^3*T_1^2*T_2^2*T_3+T_0^3*T_1^2*T_2*T_3^2+T_0^2*T_1^4*T_2*T_3+20*T_0^2*T_1^3*T_2^2*T_3+T_0^2*T_1^3*T_2*T_3^2+3*T_0^2*T_1^2*T_2^3*T_3+3*T_0^2*T_1^2*T_2^2*T_3^2+T_0*T_1^4*T_2^2*T_3+T_0*T_1^3*T_2^3*T_3+T_0*T_1^3*T_2^2*T_3^2-T_0^5*T_1^2*T_2*T_3-8*T_0^4*T_1^3*T_2*T_3-9*T_0^4*T_1^2*T_2^2*T_3-T_0^4*T_1^2*T_2*T_3^2-2*T_0^3*T_1^4*T_2*T_3-23*T_0^3*T_1^3*T_2^2*T_3-2*T_0^3*T_1^3*T_2*T_3^2-3*T_0^3*T_1^2*T_2^3*T_3-3*T_0^3*T_1^2*T_2^2*T_3^2-3*T_0^2*T_1^4*T_2^2*T_3-3*T_0^2*T_1^3*T_2^3*T_3-3*T_0^2*T_1^3*T_2^2*T_3^2+T_0^5*T_1^3*T_2*T_3+T_0^5*T_1^2*T_2^2*T_3+T_0^4*T_1^4*T_2*T_3+10*T_0^4*T_1^3*T_2^2*T_3+T_0^4*T_1^3*T_2*T_3^2+T_0^4*T_1^2*T_2^3*T_3+T_0^4*T_1^2*T_2^2*T_3^2+3*T_0^3*T_1^4*T_2^2*T_3+3*T_0^3*T_1^3*T_2^3*T_3+3*T_0^3*T_1^3*T_2^2*T_3^2-T_0^5*T_1^3*T_2^2*T_3-T_0^4*T_1^4*T_2^2*T_3-T_0^4*T_1^3*T_2^3*T_3-T_0^4*T_1^3*T_2^2*T_3^2")
///

TEST ///
--grothendieckPolynomial
assert(toExternalString grothendieckPolynomial({2,1,4,3}) == "x_1^2*x_2*x_3-x_1^2*x_2-x_1^2*x_3-x_1*x_2*x_3+x_1^2+x_1*x_2+x_1*x_3")
assert(toExternalString grothendieckPolynomial({1,2,3,4}) == "1")
///

TEST ///
--schubertPolynomial
assert(toExternalString schubertPolynomial({2,1,5,4,3}) == "x_1^3*x_2+x_1^2*x_2^2+x_1^3*x_3+2*x_1^2*x_2*x_3+x_1*x_2^2*x_3+x_1^2*x_3^2+x_1*x_2*x_3^2+x_1^3*x_4+x_1^2*x_2*x_4+x_1*x_2^2*x_4+x_1^2*x_3*x_4+x_1*x_2*x_3*x_4+x_1*x_3^2*x_4")
assert(toExternalString schubertPolynomial({1,2,3,4}) == "1")
///

TEST ///
--doubleSchubertPolynomial
assert(toExternalString doubleSchubertPolynomial({2,1,5,4,3}) == "x_1^3*x_2+x_1^2*x_2^2+x_1^3*x_3+2*x_1^2*x_2*x_3+x_1*x_2^2*x_3+x_1^2*x_3^2+x_1*x_2*x_3^2+x_1^3*x_4+x_1^2*x_2*x_4+x_1*x_2^2*x_4+x_1^2*x_3*x_4+x_1*x_2*x_3*x_4+x_1*x_3^2*x_4-x_1^3*y_1-3*x_1^2*x_2*y_1-2*x_1*x_2^2*y_1-3*x_1^2*x_3*y_1-4*x_1*x_2*x_3*y_1-x_2^2*x_3*y_1-2*x_1*x_3^2*y_1-x_2*x_3^2*y_1-2*x_1^2*x_4*y_1-2*x_1*x_2*x_4*y_1-x_2^2*x_4*y_1-2*x_1*x_3*x_4*y_1-x_2*x_3*x_4*y_1-x_3^2*x_4*y_1+2*x_1^2*y_1^2+3*x_1*x_2*y_1^2+x_2^2*y_1^2+3*x_1*x_3*y_1^2+2*x_2*x_3*y_1^2+x_3^2*y_1^2+x_1*x_4*y_1^2+x_2*x_4*y_1^2+x_3*x_4*y_1^2-x_1*y_1^3-x_2*y_1^3-x_3*y_1^3-x_1^3*y_2-2*x_1^2*x_2*y_2-x_1*x_2^2*y_2-2*x_1^2*x_3*y_2-2*x_1*x_2*x_3*y_2-x_1*x_3^2*y_2-x_1^2*x_4*y_2-x_1*x_2*x_4*y_2-x_1*x_3*x_4*y_2+3*x_1^2*y_1*y_2+4*x_1*x_2*y_1*y_2+x_2^2*y_1*y_2+4*x_1*x_3*y_1*y_2+2*x_2*x_3*y_1*y_2+x_3^2*y_1*y_2+2*x_1*x_4*y_1*y_2+x_2*x_4*y_1*y_2+x_3*x_4*y_1*y_2-3*x_1*y_1^2*y_2-2*x_2*y_1^2*y_2-2*x_3*y_1^2*y_2-x_4*y_1^2*y_2+y_1^3*y_2+x_1^2*y_2^2+x_1*x_2*y_2^2+x_1*x_3*y_2^2-2*x_1*y_1*y_2^2-x_2*y_1*y_2^2-x_3*y_1*y_2^2+y_1^2*y_2^2-x_1^3*y_3-2*x_1^2*x_2*y_3-x_1*x_2^2*y_3-2*x_1^2*x_3*y_3-2*x_1*x_2*x_3*y_3-x_1*x_3^2*y_3-x_1^2*x_4*y_3-x_1*x_2*x_4*y_3-x_1*x_3*x_4*y_3+3*x_1^2*y_1*y_3+4*x_1*x_2*y_1*y_3+x_2^2*y_1*y_3+4*x_1*x_3*y_1*y_3+2*x_2*x_3*y_1*y_3+x_3^2*y_1*y_3+2*x_1*x_4*y_1*y_3+x_2*x_4*y_1*y_3+x_3*x_4*y_1*y_3-3*x_1*y_1^2*y_3-2*x_2*y_1^2*y_3-2*x_3*y_1^2*y_3-x_4*y_1^2*y_3+y_1^3*y_3+2*x_1^2*y_2*y_3+2*x_1*x_2*y_2*y_3+2*x_1*x_3*y_2*y_3+x_1*x_4*y_2*y_3-4*x_1*y_1*y_2*y_3-2*x_2*y_1*y_2*y_3-2*x_3*y_1*y_2*y_3-x_4*y_1*y_2*y_3+2*y_1^2*y_2*y_3-x_1*y_2^2*y_3+y_1*y_2^2*y_3+x_1^2*y_3^2+x_1*x_2*y_3^2+x_1*x_3*y_3^2-2*x_1*y_1*y_3^2-x_2*y_1*y_3^2-x_3*y_1*y_3^2+y_1^2*y_3^2-x_1*y_2*y_3^2+y_1*y_2*y_3^2-x_1^2*x_2*y_4-x_1^2*x_3*y_4-x_1*x_2*x_3*y_4-x_1^2*x_4*y_4-x_1*x_2*x_4*y_4-x_1*x_3*x_4*y_4+x_1^2*y_1*y_4+2*x_1*x_2*y_1*y_4+2*x_1*x_3*y_1*y_4+x_2*x_3*y_1*y_4+2*x_1*x_4*y_1*y_4+x_2*x_4*y_1*y_4+x_3*x_4*y_1*y_4-2*x_1*y_1^2*y_4-x_2*y_1^2*y_4-x_3*y_1^2*y_4-x_4*y_1^2*y_4+y_1^3*y_4+x_1^2*y_2*y_4+x_1*x_2*y_2*y_4+x_1*x_3*y_2*y_4+x_1*x_4*y_2*y_4-2*x_1*y_1*y_2*y_4-x_2*y_1*y_2*y_4-x_3*y_1*y_2*y_4-x_4*y_1*y_2*y_4+y_1^2*y_2*y_4-x_1*y_2^2*y_4+y_1*y_2^2*y_4+x_1^2*y_3*y_4+x_1*x_2*y_3*y_4+x_1*x_3*y_3*y_4+x_1*x_4*y_3*y_4-2*x_1*y_1*y_3*y_4-x_2*y_1*y_3*y_4-x_3*y_1*y_3*y_4-x_4*y_1*y_3*y_4+y_1^2*y_3*y_4-x_1*y_2*y_3*y_4+y_1*y_2*y_3*y_4-x_1*y_3^2*y_4+y_1*y_3^2*y_4")
assert(toExternalString doubleSchubertPolynomial({1,2,3,4}) == "1")
///

TEST ///
--rajCode
assert(rajcode({7,2,5,8,1,3,6,4}) == {6, 4, 4, 4, 1, 1, 1, 0});
assert(rajcode({1,6,9,2,4,7,3,5,8}) =={4, 5, 6, 2, 2, 2, 0, 0, 0});
assert(rajcode({1, 2, 3, 4, 5, 6, 7, 8}) == {0, 0, 0, 0, 0, 0, 0, 0});
///

TEST ///
--rajIndex
assert(rajIndex({7,2,5,8,1,3,6,4}) == 21);
assert(rajIndex({1,6,9,2,4,7,3,5,8}) == 21);
assert(rajIndex({1, 2, 3, 4, 5, 6, 7, 8}) == 0);
///

TEST ///
--schubertRegularity
L = {
    {2,9,3,4,1,7,5,6,8}, -- example 1.2 in PSW
    {2,1},
    matrix{{0,0,0,1},{0,1,0,0},{1,-1,1,0},{0,1,0,0}},
    matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}},
    matrix{{0,1,0,0},{1,0,0,0},{0,0,0,1},{0,0,1,0}}
}
expected = {5,0,1,4,2}

assert all (#L, i -> schubertRegularity L#i == expected#i)
///

TEST ///
-- bijections between ASMs and monotone triangles
-- example from introduction of Hamaker-Reiner
A = matrix{{0,1,0,0,0,0},{0,0,0,1,0,0},{1,-1,1,-1,0,1},{0,0,0,1,0,0},{0,1,0,-1,1,0},{0,0,0,1,0,0}}
M = {{}, {2}, {2, 4}, {1, 3, 6}, {1, 3, 4, 6}, {1, 2, 3, 5, 6}, {1, 2, 3, 4, 5, 6}}

assert(ASMToMonotoneTriangle A == M)
assert(monotoneTriangleToASM M == A)
assert(ASMToMonotoneTriangle monotoneTriangleToASM M == M)    -- inverse operations
assert(monotoneTriangleToASM ASMToMonotoneTriangle A == A)    -- inverse operations
///


--Testing MatrixSchubertConstructions with Identity permutation / matrix--
TEST ///

w = {1,2,3,4};
I = matrix{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}};

--isPartialASM--
assert(isPartialASM(I) == true);
assert(partialASMToASM(I) == I);

--antiDiagInit--
testIdealPerm = antiDiagInit(w);
testIdealMat = antiDiagInit(I);
assert(testIdealPerm == ideal(0_(ring testIdealPerm)));
assert(testIdealMat == ideal(0_(ring testIdealMat)));

--rankMatrix--
assert(rankTable(w) == matrix{{1,1,1,1},{1,2,2,2},{1,2,3,3},{1,2,3,4}} );
assert(rankTable(I) == matrix{{1,1,1,1},{1,2,2,2},{1,2,3,3},{1,2,3,4}} );

--rotheDiagram--
assert(rotheDiagram(w) == {} );
assert(rotheDiagram(I) == {} );

--augmentedRotheDiagram--
assert(augmentedRotheDiagram(w) == {} );
assert(augmentedRotheDiagram(I) == {} );

--essentialSet--
assert(essentialSet(w) == {} );
assert(essentialSet(I) == {} );

assert(augmentedEssentialSet(w) == {} );
assert(augmentedEssentialSet(I) == {} );

--schubertDeterminantalIdeal--
testIdealPerm = schubertDeterminantalIdeal(w);
testIdealMat = schubertDeterminantalIdeal(I);
assert(testIdealPerm == ideal(0_(ring testIdealPerm)));
assert(testIdealMat == ideal(0_(ring testIdealMat)));

--fultonGens--
assert(fultonGens(w) == {0} );
assert(fultonGens(I) == {0} );

--diagLexInitSE--
testIdealPerm = diagLexInitSE(w);
testIdealMat = diagLexInitSE(I);
assert(testIdealPerm == monomialIdeal(0_(ring testIdealPerm)));
assert(testIdealMat == monomialIdeal(0_(ring testIdealMat)));

--diagLexInitNW--
testIdealPerm = diagLexInitNW(w);
testIdealMat = diagLexInitNW(I);
assert(testIdealPerm == monomialIdeal(0_(ring testIdealPerm)));
assert(testIdealMat == monomialIdeal(0_(ring testIdealMat)));

--diagRevLexInit--
testIdealPerm = diagRevLexInit(w);
testIdealMat = diagRevLexInit(I);
assert(testIdealPerm == monomialIdeal(0_(ring testIdealPerm)));
assert(testIdealMat == monomialIdeal(0_(ring testIdealMat)));

--subwordComplex--
assert(toExternalString facets subwordComplex w == "{z_(1,1)*z_(1,2)*z_(1,3)*z_(1,4)*z_(2,1)*z_(2,2)*z_(2,3)*z_(2,4)*z_(3,1)*z_(3,2)*z_(3,3)*z_(3,4)*z_(4,1)*z_(4,2)*z_(4,3)*z_(4,4)}")

--entrywiseMinRankTable--
assert(entrywiseMinRankTable {I} == matrix{{1, 1, 1, 1}, {1, 2, 2, 2}, {1, 2, 3, 3}, {1, 2, 3, 4}})

--entrywiseMaxRankTable--
assert(entrywiseMaxRankTable {I} == matrix{{1, 1, 1, 1}, {1, 2, 2, 2}, {1, 2, 3, 3}, {1, 2, 3, 4}})

--schubertDecompose--
testIdealPerm = schubertDeterminantalIdeal(w);
testIdealMat = schubertDeterminantalIdeal(I);
assert(schubertDecompose schubertDeterminantalIdeal w == {{1, 2, 3, 4}})
assert(schubertDecompose schubertDeterminantalIdeal I == {{1, 2, 3, 4}})

--permSetOfASM--
assert(permSetOfASM I == {{1, 2, 3, 4}})

--isIntersectionOfSchubertDeterminantalIdeals--
assert(isIntersectionOfSchubertDeterminantalIdeals schubertDeterminantalIdeal w == true );
assert(isIntersectionOfSchubertDeterminantalIdeals schubertDeterminantalIdeal I == true );

--isASMIdeal--
assert(isASMIdeal schubertDeterminantalIdeal w == true );
assert(isASMIdeal schubertDeterminantalIdeal I == true );

--isASM--
assert(isASM permToMatrix w == true );
assert(isASM I == true );

--isASMUnion--
--Examples in other file

--getASM--
assert(getASM schubertDeterminantalIdeal w == I );
assert(getASM schubertDeterminantalIdeal I == I );

--isMinRankTable--
assert(isMinRankTable rankTable w == true );
assert(isMinRankTable rankTable I == true );


--getPermFromASM
assert(toOneLineNotation getASM schubertDeterminantalIdeal I == w );


--ASMToMonotoneTriangle--
assert(ASMToMonotoneTriangle(I) == {{},{1},{1,2},{1,2,3},{1,2,3,4}})

--MonotoneTriangleToASM--
assert(monotoneTriangleToASM({{},{1},{1,2},{1,2,3},{1,2,3,4}}) == I)

--pipeDreams--
assert(pipeDreams w == {{{"/", "/", "/", "/"}, {"/", "/", "/", "/"}, {"/", "/", "/", "/"}, {"/", "/", "/", "/"}}})

--pipeDreamsNonReduced--
assert(pipeDreamsNonReduced w == {{{"/", "/", "/", "/"}, {"/", "/", "/", "/"}, {"/", "/", "/", "/"}, {"/", "/", "/", "/"}}})
///

------------------------
-------------------------
--**TESTS SECTIONS**--
-------------------------
-------------------------

--Testing MatrixSchubertConstructions with semi-interesting permutation / ASM / partial ASM--


TEST ///
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

--isPartialASM--
assert(isPartialASM(I) == true);
assert(isPartialASM(PI) == true);

--partialASMtToASM
outPI = matrix{{0, 0, 1, 0, 0, 0}, {0, 1, -1, 1, 0, 0}, {1, -1, 1, 0, 0, 0}, {0, 1, 0, -1, 0, 1}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}};
assert(partialASMToASM(I) == I);
assert(partialASMToASM(PI) == outPI);
///

TEST ///
--antiDiagInit--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

L = antiDiagInit w;
assert(numgens L == 14);
assert(toExternalString L_0 == "z_(1,1)");
assert(toExternalString L_1 == "z_(1,3)*z_(2,2)*z_(3,1)");
assert(toExternalString L_2 == "z_(1,4)*z_(2,2)*z_(3,1)");
assert(toExternalString L_3 == "z_(1,5)*z_(2,2)*z_(3,1)");
assert(toExternalString L_4 == "z_(1,4)*z_(2,3)*z_(3,1)");
assert(toExternalString L_5 == "z_(1,5)*z_(2,3)*z_(3,1)");
assert(toExternalString L_6 == "z_(1,5)*z_(2,4)*z_(3,1)");
assert(toExternalString L_7 == "z_(1,4)*z_(2,3)*z_(3,2)");
assert(toExternalString L_8 == "z_(1,5)*z_(2,3)*z_(3,2)");
assert(toExternalString L_9 == "z_(1,5)*z_(2,4)*z_(3,2)");
assert(toExternalString L_10 == "z_(1,5)*z_(2,4)*z_(3,3)");
assert(toExternalString L_11 == "z_(1,4)*z_(2,3)*z_(4,2)*z_(5,1)");
assert(toExternalString L_12 == "z_(1,4)*z_(3,3)*z_(4,2)*z_(5,1)");
assert(toExternalString L_13 == "z_(2,4)*z_(3,3)*z_(4,2)*z_(5,1)");

L = antiDiagInit I;
assert(numgens L == 6);
assert(toExternalString L_0 == "z_(1,1)");
assert(toExternalString L_1 == "z_(1,2)");
assert(toExternalString L_2 == "z_(2,1)");
assert(toExternalString L_3 == "z_(1,3)*z_(2,2)");
assert(toExternalString L_4 == "z_(2,2)*z_(3,1)");
assert(toExternalString L_5 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)");

L= antiDiagInit PI;
assert(numgens L == 10);
assert(toExternalString L_0 == "z_(1,1)");
assert(toExternalString L_1 == "z_(1,2)");
assert(toExternalString L_2 == "z_(2,1)");
assert(toExternalString L_3 == "z_(1,3)*z_(2,2)");
assert(toExternalString L_4 == "z_(2,2)*z_(3,1)");
assert(toExternalString L_5 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)");
assert(toExternalString L_6 == "z_(1,5)*z_(2,3)*z_(3,2)*z_(4,1)");
assert(toExternalString L_7 == "z_(1,5)*z_(2,4)*z_(3,2)*z_(4,1)");
assert(toExternalString L_8 == "z_(1,5)*z_(2,4)*z_(3,3)*z_(4,1)");
assert(toExternalString L_9 == "z_(1,5)*z_(2,4)*z_(3,3)*z_(4,2)");
///

TEST ///
--rankTable--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(rankTable(w) == matrix{{0, 1, 1, 1, 1, 1}, {1, 2, 2, 2, 2, 2}, {1, 2, 2, 2, 2, 3}, {1, 2, 3, 3, 3, 4}, {1, 2, 3, 3, 4, 5}, {1, 2, 3, 4, 5, 6}} );
assert(rankTable(I) == matrix{{0, 0, 1, 1, 1}, {0, 1, 1, 2, 2}, {1, 1, 2, 3, 3}, {1, 2, 3, 3, 4}, {1, 2, 3, 4, 5}} );
assert(rankTable(PI) == matrix{{0, 0, 1, 1, 1}, {0, 1, 1, 2, 2}, {1, 1, 2, 3, 3}, {1, 2, 3, 3, 3}, {1, 2, 3, 4, 4}} );
///

TEST ///
--rotheDiagram--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(rotheDiagram(w) == {(1, 1), (3, 3), (3, 4), (3, 5), (5, 4)} );
assert(rotheDiagram(I) == {(1, 1), (1, 2), (2, 1), (2, 3), (3, 2), (4, 4)} );
assert(rotheDiagram(PI) == {(1, 1), (1, 2), (2, 1), (2, 3), (3, 2), (4, 4), (4, 5)} );
///

TEST ///
--augmentedRotheDiagram--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(augmentedRotheDiagram(w) == {((1, 1), 0), ((3, 3), 2), ((3, 4), 2), ((3, 5), 2), ((5, 4), 3)} );
assert(augmentedRotheDiagram(I) ==  {((1, 1), 0), ((1, 2), 0), ((2, 1), 0), ((2, 3), 1), ((3, 2), 1), ((4, 4), 3)} );
assert(augmentedRotheDiagram(PI) ==  {((1, 1), 0), ((1, 2), 0), ((2, 1), 0), ((2, 3), 1), ((3, 2), 1), ((4, 4), 3), ((4, 5),3)} );
///

TEST ///
--essentialSet--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(essentialSet(w) ==  {(1, 1), (3, 5), (5, 4)} );
assert(essentialSet(I) ==  {(1, 2), (2, 1), (2, 3), (3, 2), (4, 4)} );
assert(essentialSet(PI) == {(1, 2), (2, 1), (2, 3), (3, 2), (4, 5)} );
///

TEST ///
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(augmentedEssentialSet(w) == {((1, 1), 0), ((3, 5), 2), ((5, 4), 3)} );
assert(augmentedEssentialSet(I) == {((1, 2), 0), ((2, 1), 0), ((2, 3), 1), ((3, 2), 1), ((4, 4), 3)} );
assert(augmentedEssentialSet(PI) ==  {((1, 2), 0), ((2, 1), 0), ((2, 3), 1), ((3, 2), 1), ((4, 5), 3)} );
///

TEST ///
--schubertDeterminantalIdeal--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

L = schubertDeterminantalIdeal w;
assert(toExternalString L_0 == "z_(1,1)");
assert(toExternalString L_1 == "-z_(1,3)*z_(2,2)*z_(3,1)+z_(1,2)*z_(2,3)*z_(3,1)+z_(1,3)*z_(2,1)*z_(3,2)-z_(1,1)*z_(2,3)*z_(3,2)-z_(1,2)*z_(2,1)*z_(3,3)+z_(1,1)*z_(2,2)*z_(3,3)");
assert(toExternalString L_2 == "-z_(1,4)*z_(2,2)*z_(3,1)+z_(1,2)*z_(2,4)*z_(3,1)+z_(1,4)*z_(2,1)*z_(3,2)-z_(1,1)*z_(2,4)*z_(3,2)-z_(1,2)*z_(2,1)*z_(3,4)+z_(1,1)*z_(2,2)*z_(3,4)");
assert(toExternalString L_3 == "-z_(1,4)*z_(2,3)*z_(3,1)+z_(1,3)*z_(2,4)*z_(3,1)+z_(1,4)*z_(2,1)*z_(3,3)-z_(1,1)*z_(2,4)*z_(3,3)-z_(1,3)*z_(2,1)*z_(3,4)+z_(1,1)*z_(2,3)*z_(3,4)");
assert(toExternalString L_4 == "-z_(1,4)*z_(2,3)*z_(3,2)+z_(1,3)*z_(2,4)*z_(3,2)+z_(1,4)*z_(2,2)*z_(3,3)-z_(1,2)*z_(2,4)*z_(3,3)-z_(1,3)*z_(2,2)*z_(3,4)+z_(1,2)*z_(2,3)*z_(3,4)");
assert(toExternalString L_5 == "-z_(1,5)*z_(2,2)*z_(3,1)+z_(1,2)*z_(2,5)*z_(3,1)+z_(1,5)*z_(2,1)*z_(3,2)-z_(1,1)*z_(2,5)*z_(3,2)-z_(1,2)*z_(2,1)*z_(3,5)+z_(1,1)*z_(2,2)*z_(3,5)");
assert(toExternalString L_6 == "-z_(1,5)*z_(2,3)*z_(3,1)+z_(1,3)*z_(2,5)*z_(3,1)+z_(1,5)*z_(2,1)*z_(3,3)-z_(1,1)*z_(2,5)*z_(3,3)-z_(1,3)*z_(2,1)*z_(3,5)+z_(1,1)*z_(2,3)*z_(3,5)");
assert(toExternalString L_7 == "-z_(1,5)*z_(2,3)*z_(3,2)+z_(1,3)*z_(2,5)*z_(3,2)+z_(1,5)*z_(2,2)*z_(3,3)-z_(1,2)*z_(2,5)*z_(3,3)-z_(1,3)*z_(2,2)*z_(3,5)+z_(1,2)*z_(2,3)*z_(3,5)");
assert(toExternalString L_8 == "-z_(1,5)*z_(2,4)*z_(3,1)+z_(1,4)*z_(2,5)*z_(3,1)+z_(1,5)*z_(2,1)*z_(3,4)-z_(1,1)*z_(2,5)*z_(3,4)-z_(1,4)*z_(2,1)*z_(3,5)+z_(1,1)*z_(2,4)*z_(3,5)");
assert(toExternalString L_9 == "-z_(1,5)*z_(2,4)*z_(3,2)+z_(1,4)*z_(2,5)*z_(3,2)+z_(1,5)*z_(2,2)*z_(3,4)-z_(1,2)*z_(2,5)*z_(3,4)-z_(1,4)*z_(2,2)*z_(3,5)+z_(1,2)*z_(2,4)*z_(3,5)");
assert(toExternalString L_10 == "-z_(1,5)*z_(2,4)*z_(3,3)+z_(1,4)*z_(2,5)*z_(3,3)+z_(1,5)*z_(2,3)*z_(3,4)-z_(1,3)*z_(2,5)*z_(3,4)-z_(1,4)*z_(2,3)*z_(3,5)+z_(1,3)*z_(2,4)*z_(3,5)");
assert(toExternalString L_11 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(4,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,4)");
assert(toExternalString L_12 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(5,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(5,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(5,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(5,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(5,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(5,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(5,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(5,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(5,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(5,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(5,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(5,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(5,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(5,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(5,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(5,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(5,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(5,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(5,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(5,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(5,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(5,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(5,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(5,4)");
assert(toExternalString L_13 == "z_(1,4)*z_(2,3)*z_(4,2)*z_(5,1)-z_(1,3)*z_(2,4)*z_(4,2)*z_(5,1)-z_(1,4)*z_(2,2)*z_(4,3)*z_(5,1)+z_(1,2)*z_(2,4)*z_(4,3)*z_(5,1)+z_(1,3)*z_(2,2)*z_(4,4)*z_(5,1)-z_(1,2)*z_(2,3)*z_(4,4)*z_(5,1)-z_(1,4)*z_(2,3)*z_(4,1)*z_(5,2)+z_(1,3)*z_(2,4)*z_(4,1)*z_(5,2)+z_(1,4)*z_(2,1)*z_(4,3)*z_(5,2)-z_(1,1)*z_(2,4)*z_(4,3)*z_(5,2)-z_(1,3)*z_(2,1)*z_(4,4)*z_(5,2)+z_(1,1)*z_(2,3)*z_(4,4)*z_(5,2)+z_(1,4)*z_(2,2)*z_(4,1)*z_(5,3)-z_(1,2)*z_(2,4)*z_(4,1)*z_(5,3)-z_(1,4)*z_(2,1)*z_(4,2)*z_(5,3)+z_(1,1)*z_(2,4)*z_(4,2)*z_(5,3)+z_(1,2)*z_(2,1)*z_(4,4)*z_(5,3)-z_(1,1)*z_(2,2)*z_(4,4)*z_(5,3)-z_(1,3)*z_(2,2)*z_(4,1)*z_(5,4)+z_(1,2)*z_(2,3)*z_(4,1)*z_(5,4)+z_(1,3)*z_(2,1)*z_(4,2)*z_(5,4)-z_(1,1)*z_(2,3)*z_(4,2)*z_(5,4)-z_(1,2)*z_(2,1)*z_(4,3)*z_(5,4)+z_(1,1)*z_(2,2)*z_(4,3)*z_(5,4)");
assert(toExternalString L_14 == "z_(1,4)*z_(3,3)*z_(4,2)*z_(5,1)-z_(1,3)*z_(3,4)*z_(4,2)*z_(5,1)-z_(1,4)*z_(3,2)*z_(4,3)*z_(5,1)+z_(1,2)*z_(3,4)*z_(4,3)*z_(5,1)+z_(1,3)*z_(3,2)*z_(4,4)*z_(5,1)-z_(1,2)*z_(3,3)*z_(4,4)*z_(5,1)-z_(1,4)*z_(3,3)*z_(4,1)*z_(5,2)+z_(1,3)*z_(3,4)*z_(4,1)*z_(5,2)+z_(1,4)*z_(3,1)*z_(4,3)*z_(5,2)-z_(1,1)*z_(3,4)*z_(4,3)*z_(5,2)-z_(1,3)*z_(3,1)*z_(4,4)*z_(5,2)+z_(1,1)*z_(3,3)*z_(4,4)*z_(5,2)+z_(1,4)*z_(3,2)*z_(4,1)*z_(5,3)-z_(1,2)*z_(3,4)*z_(4,1)*z_(5,3)-z_(1,4)*z_(3,1)*z_(4,2)*z_(5,3)+z_(1,1)*z_(3,4)*z_(4,2)*z_(5,3)+z_(1,2)*z_(3,1)*z_(4,4)*z_(5,3)-z_(1,1)*z_(3,2)*z_(4,4)*z_(5,3)-z_(1,3)*z_(3,2)*z_(4,1)*z_(5,4)+z_(1,2)*z_(3,3)*z_(4,1)*z_(5,4)+z_(1,3)*z_(3,1)*z_(4,2)*z_(5,4)-z_(1,1)*z_(3,3)*z_(4,2)*z_(5,4)-z_(1,2)*z_(3,1)*z_(4,3)*z_(5,4)+z_(1,1)*z_(3,2)*z_(4,3)*z_(5,4)");
assert(toExternalString L_15 == "z_(2,4)*z_(3,3)*z_(4,2)*z_(5,1)-z_(2,3)*z_(3,4)*z_(4,2)*z_(5,1)-z_(2,4)*z_(3,2)*z_(4,3)*z_(5,1)+z_(2,2)*z_(3,4)*z_(4,3)*z_(5,1)+z_(2,3)*z_(3,2)*z_(4,4)*z_(5,1)-z_(2,2)*z_(3,3)*z_(4,4)*z_(5,1)-z_(2,4)*z_(3,3)*z_(4,1)*z_(5,2)+z_(2,3)*z_(3,4)*z_(4,1)*z_(5,2)+z_(2,4)*z_(3,1)*z_(4,3)*z_(5,2)-z_(2,1)*z_(3,4)*z_(4,3)*z_(5,2)-z_(2,3)*z_(3,1)*z_(4,4)*z_(5,2)+z_(2,1)*z_(3,3)*z_(4,4)*z_(5,2)+z_(2,4)*z_(3,2)*z_(4,1)*z_(5,3)-z_(2,2)*z_(3,4)*z_(4,1)*z_(5,3)-z_(2,4)*z_(3,1)*z_(4,2)*z_(5,3)+z_(2,1)*z_(3,4)*z_(4,2)*z_(5,3)+z_(2,2)*z_(3,1)*z_(4,4)*z_(5,3)-z_(2,1)*z_(3,2)*z_(4,4)*z_(5,3)-z_(2,3)*z_(3,2)*z_(4,1)*z_(5,4)+z_(2,2)*z_(3,3)*z_(4,1)*z_(5,4)+z_(2,3)*z_(3,1)*z_(4,2)*z_(5,4)-z_(2,1)*z_(3,3)*z_(4,2)*z_(5,4)-z_(2,2)*z_(3,1)*z_(4,3)*z_(5,4)+z_(2,1)*z_(3,2)*z_(4,3)*z_(5,4)");


L = schubertDeterminantalIdeal I;
assert(toExternalString L_0 == "z_(1,1)");
assert(toExternalString L_1 == "z_(1,2)");
assert(toExternalString L_2 == "z_(2,1)");
assert(toExternalString L_3 == "-z_(1,2)*z_(2,1)+z_(1,1)*z_(2,2)");
assert(toExternalString L_4 == "-z_(1,3)*z_(2,1)+z_(1,1)*z_(2,3)");
assert(toExternalString L_5 == "-z_(1,3)*z_(2,2)+z_(1,2)*z_(2,3)");
assert(toExternalString L_6 == "-z_(1,2)*z_(3,1)+z_(1,1)*z_(3,2)");
assert(toExternalString L_7 == "-z_(2,2)*z_(3,1)+z_(2,1)*z_(3,2)");
assert(toExternalString L_8 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(4,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,4)");


L = schubertDeterminantalIdeal PI;
assert(numgens L == 13);
assert(toExternalString L_0 == "z_(1,1)");
assert(toExternalString L_1 == "z_(1,2)");
assert(toExternalString L_2 == "z_(2,1)");
assert(toExternalString L_3 == "-z_(1,2)*z_(2,1)+z_(1,1)*z_(2,2)");
assert(toExternalString L_4 == "-z_(1,3)*z_(2,1)+z_(1,1)*z_(2,3)");
assert(toExternalString L_5 == "-z_(1,3)*z_(2,2)+z_(1,2)*z_(2,3)");
assert(toExternalString L_6 == "-z_(1,2)*z_(3,1)+z_(1,1)*z_(3,2)");
assert(toExternalString L_7 == "-z_(2,2)*z_(3,1)+z_(2,1)*z_(3,2)");
assert(toExternalString L_8 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(4,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,4)");
assert(toExternalString L_9 == "z_(1,5)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,5)*z_(3,2)*z_(4,1)-z_(1,5)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,5)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,5)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,5)*z_(4,1)-z_(1,5)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,5)*z_(3,1)*z_(4,2)+z_(1,5)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,5)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,5)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,5)*z_(4,2)+z_(1,5)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,5)*z_(3,1)*z_(4,3)-z_(1,5)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,5)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,5)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,5)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,5)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,5)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,5)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,5)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,5)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,5)");
assert(toExternalString L_10 == "z_(1,5)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,5)*z_(3,2)*z_(4,1)-z_(1,5)*z_(2,2)*z_(3,4)*z_(4,1)+z_(1,2)*z_(2,5)*z_(3,4)*z_(4,1)+z_(1,4)*z_(2,2)*z_(3,5)*z_(4,1)-z_(1,2)*z_(2,4)*z_(3,5)*z_(4,1)-z_(1,5)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,5)*z_(3,1)*z_(4,2)+z_(1,5)*z_(2,1)*z_(3,4)*z_(4,2)-z_(1,1)*z_(2,5)*z_(3,4)*z_(4,2)-z_(1,4)*z_(2,1)*z_(3,5)*z_(4,2)+z_(1,1)*z_(2,4)*z_(3,5)*z_(4,2)+z_(1,5)*z_(2,2)*z_(3,1)*z_(4,4)-z_(1,2)*z_(2,5)*z_(3,1)*z_(4,4)-z_(1,5)*z_(2,1)*z_(3,2)*z_(4,4)+z_(1,1)*z_(2,5)*z_(3,2)*z_(4,4)+z_(1,2)*z_(2,1)*z_(3,5)*z_(4,4)-z_(1,1)*z_(2,2)*z_(3,5)*z_(4,4)-z_(1,4)*z_(2,2)*z_(3,1)*z_(4,5)+z_(1,2)*z_(2,4)*z_(3,1)*z_(4,5)+z_(1,4)*z_(2,1)*z_(3,2)*z_(4,5)-z_(1,1)*z_(2,4)*z_(3,2)*z_(4,5)-z_(1,2)*z_(2,1)*z_(3,4)*z_(4,5)+z_(1,1)*z_(2,2)*z_(3,4)*z_(4,5)");
assert(toExternalString L_11 == "z_(1,5)*z_(2,4)*z_(3,3)*z_(4,1)-z_(1,4)*z_(2,5)*z_(3,3)*z_(4,1)-z_(1,5)*z_(2,3)*z_(3,4)*z_(4,1)+z_(1,3)*z_(2,5)*z_(3,4)*z_(4,1)+z_(1,4)*z_(2,3)*z_(3,5)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,5)*z_(4,1)-z_(1,5)*z_(2,4)*z_(3,1)*z_(4,3)+z_(1,4)*z_(2,5)*z_(3,1)*z_(4,3)+z_(1,5)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,5)*z_(3,4)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,5)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,5)*z_(4,3)+z_(1,5)*z_(2,3)*z_(3,1)*z_(4,4)-z_(1,3)*z_(2,5)*z_(3,1)*z_(4,4)-z_(1,5)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,5)*z_(3,3)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,5)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,5)*z_(4,4)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,5)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,5)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,5)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,5)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,5)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,5)");
assert(toExternalString L_12 == "z_(1,5)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,4)*z_(2,5)*z_(3,3)*z_(4,2)-z_(1,5)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,3)*z_(2,5)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,3)*z_(3,5)*z_(4,2)-z_(1,3)*z_(2,4)*z_(3,5)*z_(4,2)-z_(1,5)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,4)*z_(2,5)*z_(3,2)*z_(4,3)+z_(1,5)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,2)*z_(2,5)*z_(3,4)*z_(4,3)-z_(1,4)*z_(2,2)*z_(3,5)*z_(4,3)+z_(1,2)*z_(2,4)*z_(3,5)*z_(4,3)+z_(1,5)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,3)*z_(2,5)*z_(3,2)*z_(4,4)-z_(1,5)*z_(2,2)*z_(3,3)*z_(4,4)+z_(1,2)*z_(2,5)*z_(3,3)*z_(4,4)+z_(1,3)*z_(2,2)*z_(3,5)*z_(4,4)-z_(1,2)*z_(2,3)*z_(3,5)*z_(4,4)-z_(1,4)*z_(2,3)*z_(3,2)*z_(4,5)+z_(1,3)*z_(2,4)*z_(3,2)*z_(4,5)+z_(1,4)*z_(2,2)*z_(3,3)*z_(4,5)-z_(1,2)*z_(2,4)*z_(3,3)*z_(4,5)-z_(1,3)*z_(2,2)*z_(3,4)*z_(4,5)+z_(1,2)*z_(2,3)*z_(3,4)*z_(4,5)");
///

TEST ///
--fultonGens--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

L = fultonGens w; 
assert(# L == 16);
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "-z_(1,3)*z_(2,2)*z_(3,1)+z_(1,2)*z_(2,3)*z_(3,1)+z_(1,3)*z_(2,1)*z_(3,2)-z_(1,1)*z_(2,3)*z_(3,2)-z_(1,2)*z_(2,1)*z_(3,3)+z_(1,1)*z_(2,2)*z_(3,3)" );
assert(toExternalString L_2 == "-z_(1,4)*z_(2,2)*z_(3,1)+z_(1,2)*z_(2,4)*z_(3,1)+z_(1,4)*z_(2,1)*z_(3,2)-z_(1,1)*z_(2,4)*z_(3,2)-z_(1,2)*z_(2,1)*z_(3,4)+z_(1,1)*z_(2,2)*z_(3,4)" );
assert(toExternalString L_3 == "-z_(1,4)*z_(2,3)*z_(3,1)+z_(1,3)*z_(2,4)*z_(3,1)+z_(1,4)*z_(2,1)*z_(3,3)-z_(1,1)*z_(2,4)*z_(3,3)-z_(1,3)*z_(2,1)*z_(3,4)+z_(1,1)*z_(2,3)*z_(3,4)" );
assert(toExternalString L_4 == "-z_(1,4)*z_(2,3)*z_(3,2)+z_(1,3)*z_(2,4)*z_(3,2)+z_(1,4)*z_(2,2)*z_(3,3)-z_(1,2)*z_(2,4)*z_(3,3)-z_(1,3)*z_(2,2)*z_(3,4)+z_(1,2)*z_(2,3)*z_(3,4)" );
assert(toExternalString L_5 == "-z_(1,5)*z_(2,2)*z_(3,1)+z_(1,2)*z_(2,5)*z_(3,1)+z_(1,5)*z_(2,1)*z_(3,2)-z_(1,1)*z_(2,5)*z_(3,2)-z_(1,2)*z_(2,1)*z_(3,5)+z_(1,1)*z_(2,2)*z_(3,5)" );
assert(toExternalString L_6 == "-z_(1,5)*z_(2,3)*z_(3,1)+z_(1,3)*z_(2,5)*z_(3,1)+z_(1,5)*z_(2,1)*z_(3,3)-z_(1,1)*z_(2,5)*z_(3,3)-z_(1,3)*z_(2,1)*z_(3,5)+z_(1,1)*z_(2,3)*z_(3,5)" );
assert(toExternalString L_7 == "-z_(1,5)*z_(2,3)*z_(3,2)+z_(1,3)*z_(2,5)*z_(3,2)+z_(1,5)*z_(2,2)*z_(3,3)-z_(1,2)*z_(2,5)*z_(3,3)-z_(1,3)*z_(2,2)*z_(3,5)+z_(1,2)*z_(2,3)*z_(3,5)" );
assert(toExternalString L_8 == "-z_(1,5)*z_(2,4)*z_(3,1)+z_(1,4)*z_(2,5)*z_(3,1)+z_(1,5)*z_(2,1)*z_(3,4)-z_(1,1)*z_(2,5)*z_(3,4)-z_(1,4)*z_(2,1)*z_(3,5)+z_(1,1)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_9 == "-z_(1,5)*z_(2,4)*z_(3,2)+z_(1,4)*z_(2,5)*z_(3,2)+z_(1,5)*z_(2,2)*z_(3,4)-z_(1,2)*z_(2,5)*z_(3,4)-z_(1,4)*z_(2,2)*z_(3,5)+z_(1,2)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_10 == "-z_(1,5)*z_(2,4)*z_(3,3)+z_(1,4)*z_(2,5)*z_(3,3)+z_(1,5)*z_(2,3)*z_(3,4)-z_(1,3)*z_(2,5)*z_(3,4)-z_(1,4)*z_(2,3)*z_(3,5)+z_(1,3)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_11 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(4,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,4)" );
assert(toExternalString L_12 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(5,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(5,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(5,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(5,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(5,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(5,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(5,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(5,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(5,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(5,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(5,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(5,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(5,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(5,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(5,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(5,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(5,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(5,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(5,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(5,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(5,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(5,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(5,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(5,4)" );
assert(toExternalString L_13 == "z_(1,4)*z_(2,3)*z_(4,2)*z_(5,1)-z_(1,3)*z_(2,4)*z_(4,2)*z_(5,1)-z_(1,4)*z_(2,2)*z_(4,3)*z_(5,1)+z_(1,2)*z_(2,4)*z_(4,3)*z_(5,1)+z_(1,3)*z_(2,2)*z_(4,4)*z_(5,1)-z_(1,2)*z_(2,3)*z_(4,4)*z_(5,1)-z_(1,4)*z_(2,3)*z_(4,1)*z_(5,2)+z_(1,3)*z_(2,4)*z_(4,1)*z_(5,2)+z_(1,4)*z_(2,1)*z_(4,3)*z_(5,2)-z_(1,1)*z_(2,4)*z_(4,3)*z_(5,2)-z_(1,3)*z_(2,1)*z_(4,4)*z_(5,2)+z_(1,1)*z_(2,3)*z_(4,4)*z_(5,2)+z_(1,4)*z_(2,2)*z_(4,1)*z_(5,3)-z_(1,2)*z_(2,4)*z_(4,1)*z_(5,3)-z_(1,4)*z_(2,1)*z_(4,2)*z_(5,3)+z_(1,1)*z_(2,4)*z_(4,2)*z_(5,3)+z_(1,2)*z_(2,1)*z_(4,4)*z_(5,3)-z_(1,1)*z_(2,2)*z_(4,4)*z_(5,3)-z_(1,3)*z_(2,2)*z_(4,1)*z_(5,4)+z_(1,2)*z_(2,3)*z_(4,1)*z_(5,4)+z_(1,3)*z_(2,1)*z_(4,2)*z_(5,4)-z_(1,1)*z_(2,3)*z_(4,2)*z_(5,4)-z_(1,2)*z_(2,1)*z_(4,3)*z_(5,4)+z_(1,1)*z_(2,2)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_14 == "z_(1,4)*z_(3,3)*z_(4,2)*z_(5,1)-z_(1,3)*z_(3,4)*z_(4,2)*z_(5,1)-z_(1,4)*z_(3,2)*z_(4,3)*z_(5,1)+z_(1,2)*z_(3,4)*z_(4,3)*z_(5,1)+z_(1,3)*z_(3,2)*z_(4,4)*z_(5,1)-z_(1,2)*z_(3,3)*z_(4,4)*z_(5,1)-z_(1,4)*z_(3,3)*z_(4,1)*z_(5,2)+z_(1,3)*z_(3,4)*z_(4,1)*z_(5,2)+z_(1,4)*z_(3,1)*z_(4,3)*z_(5,2)-z_(1,1)*z_(3,4)*z_(4,3)*z_(5,2)-z_(1,3)*z_(3,1)*z_(4,4)*z_(5,2)+z_(1,1)*z_(3,3)*z_(4,4)*z_(5,2)+z_(1,4)*z_(3,2)*z_(4,1)*z_(5,3)-z_(1,2)*z_(3,4)*z_(4,1)*z_(5,3)-z_(1,4)*z_(3,1)*z_(4,2)*z_(5,3)+z_(1,1)*z_(3,4)*z_(4,2)*z_(5,3)+z_(1,2)*z_(3,1)*z_(4,4)*z_(5,3)-z_(1,1)*z_(3,2)*z_(4,4)*z_(5,3)-z_(1,3)*z_(3,2)*z_(4,1)*z_(5,4)+z_(1,2)*z_(3,3)*z_(4,1)*z_(5,4)+z_(1,3)*z_(3,1)*z_(4,2)*z_(5,4)-z_(1,1)*z_(3,3)*z_(4,2)*z_(5,4)-z_(1,2)*z_(3,1)*z_(4,3)*z_(5,4)+z_(1,1)*z_(3,2)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_15 == "z_(2,4)*z_(3,3)*z_(4,2)*z_(5,1)-z_(2,3)*z_(3,4)*z_(4,2)*z_(5,1)-z_(2,4)*z_(3,2)*z_(4,3)*z_(5,1)+z_(2,2)*z_(3,4)*z_(4,3)*z_(5,1)+z_(2,3)*z_(3,2)*z_(4,4)*z_(5,1)-z_(2,2)*z_(3,3)*z_(4,4)*z_(5,1)-z_(2,4)*z_(3,3)*z_(4,1)*z_(5,2)+z_(2,3)*z_(3,4)*z_(4,1)*z_(5,2)+z_(2,4)*z_(3,1)*z_(4,3)*z_(5,2)-z_(2,1)*z_(3,4)*z_(4,3)*z_(5,2)-z_(2,3)*z_(3,1)*z_(4,4)*z_(5,2)+z_(2,1)*z_(3,3)*z_(4,4)*z_(5,2)+z_(2,4)*z_(3,2)*z_(4,1)*z_(5,3)-z_(2,2)*z_(3,4)*z_(4,1)*z_(5,3)-z_(2,4)*z_(3,1)*z_(4,2)*z_(5,3)+z_(2,1)*z_(3,4)*z_(4,2)*z_(5,3)+z_(2,2)*z_(3,1)*z_(4,4)*z_(5,3)-z_(2,1)*z_(3,2)*z_(4,4)*z_(5,3)-z_(2,3)*z_(3,2)*z_(4,1)*z_(5,4)+z_(2,2)*z_(3,3)*z_(4,1)*z_(5,4)+z_(2,3)*z_(3,1)*z_(4,2)*z_(5,4)-z_(2,1)*z_(3,3)*z_(4,2)*z_(5,4)-z_(2,2)*z_(3,1)*z_(4,3)*z_(5,4)+z_(2,1)*z_(3,2)*z_(4,3)*z_(5,4)" );

L = fultonGens I;
assert(# L == 9); 
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "z_(1,2)" );
assert(toExternalString L_2 == "z_(2,1)" );
assert(toExternalString L_3 == "-z_(1,2)*z_(2,1)+z_(1,1)*z_(2,2)" );
assert(toExternalString L_4 == "-z_(1,3)*z_(2,1)+z_(1,1)*z_(2,3)" );
assert(toExternalString L_5 == "-z_(1,3)*z_(2,2)+z_(1,2)*z_(2,3)" );
assert(toExternalString L_6 == "-z_(1,2)*z_(3,1)+z_(1,1)*z_(3,2)" );
assert(toExternalString L_7 == "-z_(2,2)*z_(3,1)+z_(2,1)*z_(3,2)" );
assert(toExternalString L_8 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(4,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,4)" );

L = fultonGens PI; 
assert(# L == 13);
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "z_(1,2)" );
assert(toExternalString L_2 == "z_(2,1)" );
assert(toExternalString L_3 == "-z_(1,2)*z_(2,1)+z_(1,1)*z_(2,2)" );
assert(toExternalString L_4 == "-z_(1,3)*z_(2,1)+z_(1,1)*z_(2,3)" );
assert(toExternalString L_5 == "-z_(1,3)*z_(2,2)+z_(1,2)*z_(2,3)" );
assert(toExternalString L_6 == "-z_(1,2)*z_(3,1)+z_(1,1)*z_(3,2)" );
assert(toExternalString L_7 == "-z_(2,2)*z_(3,1)+z_(2,1)*z_(3,2)" );
assert(toExternalString L_8 == "z_(1,4)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,4)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,4)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,4)*z_(4,1)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,4)*z_(3,1)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,4)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,4)" );
assert(toExternalString L_9 == "z_(1,5)*z_(2,3)*z_(3,2)*z_(4,1)-z_(1,3)*z_(2,5)*z_(3,2)*z_(4,1)-z_(1,5)*z_(2,2)*z_(3,3)*z_(4,1)+z_(1,2)*z_(2,5)*z_(3,3)*z_(4,1)+z_(1,3)*z_(2,2)*z_(3,5)*z_(4,1)-z_(1,2)*z_(2,3)*z_(3,5)*z_(4,1)-z_(1,5)*z_(2,3)*z_(3,1)*z_(4,2)+z_(1,3)*z_(2,5)*z_(3,1)*z_(4,2)+z_(1,5)*z_(2,1)*z_(3,3)*z_(4,2)-z_(1,1)*z_(2,5)*z_(3,3)*z_(4,2)-z_(1,3)*z_(2,1)*z_(3,5)*z_(4,2)+z_(1,1)*z_(2,3)*z_(3,5)*z_(4,2)+z_(1,5)*z_(2,2)*z_(3,1)*z_(4,3)-z_(1,2)*z_(2,5)*z_(3,1)*z_(4,3)-z_(1,5)*z_(2,1)*z_(3,2)*z_(4,3)+z_(1,1)*z_(2,5)*z_(3,2)*z_(4,3)+z_(1,2)*z_(2,1)*z_(3,5)*z_(4,3)-z_(1,1)*z_(2,2)*z_(3,5)*z_(4,3)-z_(1,3)*z_(2,2)*z_(3,1)*z_(4,5)+z_(1,2)*z_(2,3)*z_(3,1)*z_(4,5)+z_(1,3)*z_(2,1)*z_(3,2)*z_(4,5)-z_(1,1)*z_(2,3)*z_(3,2)*z_(4,5)-z_(1,2)*z_(2,1)*z_(3,3)*z_(4,5)+z_(1,1)*z_(2,2)*z_(3,3)*z_(4,5)" );
assert(toExternalString L_10 == "z_(1,5)*z_(2,4)*z_(3,2)*z_(4,1)-z_(1,4)*z_(2,5)*z_(3,2)*z_(4,1)-z_(1,5)*z_(2,2)*z_(3,4)*z_(4,1)+z_(1,2)*z_(2,5)*z_(3,4)*z_(4,1)+z_(1,4)*z_(2,2)*z_(3,5)*z_(4,1)-z_(1,2)*z_(2,4)*z_(3,5)*z_(4,1)-z_(1,5)*z_(2,4)*z_(3,1)*z_(4,2)+z_(1,4)*z_(2,5)*z_(3,1)*z_(4,2)+z_(1,5)*z_(2,1)*z_(3,4)*z_(4,2)-z_(1,1)*z_(2,5)*z_(3,4)*z_(4,2)-z_(1,4)*z_(2,1)*z_(3,5)*z_(4,2)+z_(1,1)*z_(2,4)*z_(3,5)*z_(4,2)+z_(1,5)*z_(2,2)*z_(3,1)*z_(4,4)-z_(1,2)*z_(2,5)*z_(3,1)*z_(4,4)-z_(1,5)*z_(2,1)*z_(3,2)*z_(4,4)+z_(1,1)*z_(2,5)*z_(3,2)*z_(4,4)+z_(1,2)*z_(2,1)*z_(3,5)*z_(4,4)-z_(1,1)*z_(2,2)*z_(3,5)*z_(4,4)-z_(1,4)*z_(2,2)*z_(3,1)*z_(4,5)+z_(1,2)*z_(2,4)*z_(3,1)*z_(4,5)+z_(1,4)*z_(2,1)*z_(3,2)*z_(4,5)-z_(1,1)*z_(2,4)*z_(3,2)*z_(4,5)-z_(1,2)*z_(2,1)*z_(3,4)*z_(4,5)+z_(1,1)*z_(2,2)*z_(3,4)*z_(4,5)" );
assert(toExternalString L_11 == "z_(1,5)*z_(2,4)*z_(3,3)*z_(4,1)-z_(1,4)*z_(2,5)*z_(3,3)*z_(4,1)-z_(1,5)*z_(2,3)*z_(3,4)*z_(4,1)+z_(1,3)*z_(2,5)*z_(3,4)*z_(4,1)+z_(1,4)*z_(2,3)*z_(3,5)*z_(4,1)-z_(1,3)*z_(2,4)*z_(3,5)*z_(4,1)-z_(1,5)*z_(2,4)*z_(3,1)*z_(4,3)+z_(1,4)*z_(2,5)*z_(3,1)*z_(4,3)+z_(1,5)*z_(2,1)*z_(3,4)*z_(4,3)-z_(1,1)*z_(2,5)*z_(3,4)*z_(4,3)-z_(1,4)*z_(2,1)*z_(3,5)*z_(4,3)+z_(1,1)*z_(2,4)*z_(3,5)*z_(4,3)+z_(1,5)*z_(2,3)*z_(3,1)*z_(4,4)-z_(1,3)*z_(2,5)*z_(3,1)*z_(4,4)-z_(1,5)*z_(2,1)*z_(3,3)*z_(4,4)+z_(1,1)*z_(2,5)*z_(3,3)*z_(4,4)+z_(1,3)*z_(2,1)*z_(3,5)*z_(4,4)-z_(1,1)*z_(2,3)*z_(3,5)*z_(4,4)-z_(1,4)*z_(2,3)*z_(3,1)*z_(4,5)+z_(1,3)*z_(2,4)*z_(3,1)*z_(4,5)+z_(1,4)*z_(2,1)*z_(3,3)*z_(4,5)-z_(1,1)*z_(2,4)*z_(3,3)*z_(4,5)-z_(1,3)*z_(2,1)*z_(3,4)*z_(4,5)+z_(1,1)*z_(2,3)*z_(3,4)*z_(4,5)" );
assert(toExternalString L_12 == "z_(1,5)*z_(2,4)*z_(3,3)*z_(4,2)-z_(1,4)*z_(2,5)*z_(3,3)*z_(4,2)-z_(1,5)*z_(2,3)*z_(3,4)*z_(4,2)+z_(1,3)*z_(2,5)*z_(3,4)*z_(4,2)+z_(1,4)*z_(2,3)*z_(3,5)*z_(4,2)-z_(1,3)*z_(2,4)*z_(3,5)*z_(4,2)-z_(1,5)*z_(2,4)*z_(3,2)*z_(4,3)+z_(1,4)*z_(2,5)*z_(3,2)*z_(4,3)+z_(1,5)*z_(2,2)*z_(3,4)*z_(4,3)-z_(1,2)*z_(2,5)*z_(3,4)*z_(4,3)-z_(1,4)*z_(2,2)*z_(3,5)*z_(4,3)+z_(1,2)*z_(2,4)*z_(3,5)*z_(4,3)+z_(1,5)*z_(2,3)*z_(3,2)*z_(4,4)-z_(1,3)*z_(2,5)*z_(3,2)*z_(4,4)-z_(1,5)*z_(2,2)*z_(3,3)*z_(4,4)+z_(1,2)*z_(2,5)*z_(3,3)*z_(4,4)+z_(1,3)*z_(2,2)*z_(3,5)*z_(4,4)-z_(1,2)*z_(2,3)*z_(3,5)*z_(4,4)-z_(1,4)*z_(2,3)*z_(3,2)*z_(4,5)+z_(1,3)*z_(2,4)*z_(3,2)*z_(4,5)+z_(1,4)*z_(2,2)*z_(3,3)*z_(4,5)-z_(1,2)*z_(2,4)*z_(3,3)*z_(4,5)-z_(1,3)*z_(2,2)*z_(3,4)*z_(4,5)+z_(1,2)*z_(2,3)*z_(3,4)*z_(4,5)" );
///

TEST ///
--diagLexInitSE--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

L = diagLexInitSE w; 
assert(numgens L == 17);
assert(toExternalString L_0 == "z_(5,4)*z_(4,3)*z_(3,2)*z_(2,1)" );
assert(toExternalString L_1 == "z_(5,4)*z_(4,3)*z_(3,5)*z_(3,1)*z_(2,2)*z_(1,4)" );
assert(toExternalString L_2 == "z_(3,5)*z_(2,1)*z_(1,4)" );
assert(toExternalString L_3 == "z_(3,5)*z_(2,4)*z_(1,3)" );
assert(toExternalString L_4 == "z_(5,4)*z_(4,3)*z_(3,5)*z_(3,1)*z_(2,2)*z_(1,3)" );
assert(toExternalString L_5 == "z_(5,4)*z_(4,3)*z_(3,4)*z_(3,1)*z_(2,2)*z_(1,3)" );
assert(toExternalString L_6 == "z_(3,5)*z_(2,1)*z_(1,3)" );
assert(toExternalString L_7 == "z_(3,4)*z_(2,1)*z_(1,3)" );
assert(toExternalString L_8 == "z_(5,4)*z_(4,3)*z_(3,1)*z_(1,2)" );
assert(toExternalString L_9 == "z_(3,5)*z_(2,4)*z_(1,2)" );
assert(toExternalString L_10 == "z_(3,5)*z_(2,3)*z_(1,2)" );
assert(toExternalString L_11 == "z_(3,4)*z_(2,3)*z_(1,2)" );
assert(toExternalString L_12 == "z_(5,4)*z_(4,3)*z_(2,1)*z_(1,2)" );
assert(toExternalString L_13 == "z_(3,5)*z_(2,1)*z_(1,2)" );
assert(toExternalString L_14 == "z_(3,4)*z_(2,1)*z_(1,2)" );
assert(toExternalString L_15 == "z_(3,3)*z_(2,1)*z_(1,2)" );
assert(toExternalString L_16 == "z_(1,1)" );

L = diagLexInitSE I; 
assert(numgens L == 7);
assert(toExternalString L_0 == "z_(3,1)*z_(2,2)" );
assert(toExternalString L_1 == "z_(2,1)" );
assert(toExternalString L_2 == "z_(4,1)*z_(3,3)*z_(2,2)^2*z_(1,4)" );
assert(toExternalString L_3 == "z_(4,2)*z_(3,1)*z_(2,4)*z_(1,3)" );
assert(toExternalString L_4 == "z_(2,2)*z_(1,3)" );
assert(toExternalString L_5 == "z_(1,2)" );
assert(toExternalString L_6 == "z_(1,1)" );

L = diagLexInitSE PI; 
assert(numgens L == 16);
assert(toExternalString L_0 == "z_(3,1)*z_(2,2)" );
assert(toExternalString L_1 == "z_(2,1)" );
assert(toExternalString L_2 == "z_(4,1)*z_(3,3)*z_(2,2)^2*z_(1,5)" );
assert(toExternalString L_3 == "z_(4,2)*z_(3,1)*z_(2,5)*z_(1,4)" );
assert(toExternalString L_4 == "z_(4,5)*z_(3,3)*z_(2,2)*z_(1,4)" );
assert(toExternalString L_5 == "z_(4,1)*z_(3,5)*z_(2,3)*z_(2,2)*z_(1,4)" );
assert(toExternalString L_6 == "z_(4,1)*z_(3,5)*z_(2,2)^2*z_(1,4)" );
assert(toExternalString L_7 == "z_(4,1)*z_(3,3)*z_(2,2)^2*z_(1,4)" );
assert(toExternalString L_8 == "z_(4,2)*z_(3,1)*z_(2,5)*z_(1,3)" );
assert(toExternalString L_9 == "z_(4,5)*z_(3,1)*z_(2,4)*z_(1,3)" );
assert(toExternalString L_10 == "z_(4,2)*z_(3,1)*z_(2,4)*z_(1,3)" );
assert(toExternalString L_11 == "z_(4,5)*z_(4,1)*z_(3,5)*z_(3,2)*z_(2,4)*z_(2,3)*z_(1,3)" );
assert(toExternalString L_12 == "z_(2,2)*z_(1,3)" );
assert(toExternalString L_13 == "z_(4,5)*z_(3,2)*z_(2,4)*z_(1,3)^2" );
assert(toExternalString L_14 == "z_(1,2)" );
assert(toExternalString L_15 == "z_(1,1)" );
///

TEST ///
--diagLexInitNW--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

L = diagLexInitNW w; 
assert(numgens L == 17);
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "z_(1,2)*z_(2,1)*z_(3,3)" );
assert(toExternalString L_2 == "z_(1,2)*z_(2,1)*z_(3,4)" );
assert(toExternalString L_3 == "z_(1,3)*z_(2,1)*z_(3,4)" );
assert(toExternalString L_4 == "z_(1,2)*z_(2,3)*z_(3,4)" );
assert(toExternalString L_5 == "z_(1,2)*z_(2,1)*z_(3,5)" );
assert(toExternalString L_6 == "z_(1,3)*z_(2,1)*z_(3,5)" );
assert(toExternalString L_7 == "z_(1,4)*z_(2,1)*z_(3,5)" );
assert(toExternalString L_8 == "z_(1,2)*z_(2,3)*z_(3,5)" );
assert(toExternalString L_9 == "z_(1,2)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_10 == "z_(1,3)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_11 == "z_(1,2)*z_(2,1)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_12 == "z_(1,2)*z_(3,1)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_13 == "z_(2,1)*z_(3,2)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_14 == "z_(1,3)*z_(2,2)*z_(3,1)*z_(3,4)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_15 == "z_(1,3)*z_(2,2)*z_(3,1)*z_(3,5)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_16 == "z_(1,4)*z_(2,2)*z_(3,1)*z_(3,5)*z_(4,3)*z_(5,4)" );

L = diagLexInitNW I; 
assert(numgens L == 7);
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "z_(1,2)" );
assert(toExternalString L_2 == "z_(2,1)" );
assert(toExternalString L_3 == "z_(1,3)*z_(2,2)" );
assert(toExternalString L_4 == "z_(2,2)*z_(3,1)" );
assert(toExternalString L_5 == "z_(1,4)*z_(2,2)^2*z_(3,3)*z_(4,1)" );
assert(toExternalString L_6 == "z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)" );

L = diagLexInitNW PI; 
assert(numgens L == 16);
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "z_(1,2)" );
assert(toExternalString L_2 == "z_(2,1)" );
assert(toExternalString L_3 == "z_(1,3)*z_(2,2)" );
assert(toExternalString L_4 == "z_(2,2)*z_(3,1)" );
assert(toExternalString L_5 == "z_(1,4)*z_(2,2)^2*z_(3,3)*z_(4,1)" );
assert(toExternalString L_6 == "z_(1,5)*z_(2,2)^2*z_(3,3)*z_(4,1)" );
assert(toExternalString L_7 == "z_(1,4)*z_(2,2)*z_(2,5)*z_(3,3)*z_(4,1)" );
assert(toExternalString L_8 == "z_(1,4)*z_(2,2)*z_(3,5)*z_(4,1)" );
assert(toExternalString L_9 == "z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)" );
assert(toExternalString L_10 == "z_(1,3)*z_(2,5)*z_(3,1)*z_(4,2)" );
assert(toExternalString L_11 == "z_(1,4)*z_(2,5)*z_(3,1)^2*z_(4,2)" );
assert(toExternalString L_12 == "z_(1,4)*z_(2,3)*z_(2,5)*z_(3,1)*z_(3,5)*z_(4,2)" );
assert(toExternalString L_13 == "z_(1,3)*z_(2,4)*z_(3,1)*z_(4,5)" );
assert(toExternalString L_14 == "z_(1,3)*z_(2,4)*z_(3,2)*z_(4,5)" );
assert(toExternalString L_15 == "z_(1,4)*z_(2,2)^2*z_(3,3)*z_(4,5)" );
///

TEST ///
--diagRevLexInit--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

L = diagRevLexInit w; 
assert(numgens L == 17);
assert(toExternalString L_0 == "z_(1,1)" );
assert(toExternalString L_1 == "z_(1,3)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_2 == "z_(1,2)*z_(2,4)*z_(3,5)" );
assert(toExternalString L_3 == "z_(1,2)*z_(2,3)*z_(3,5)" );
assert(toExternalString L_4 == "z_(1,4)*z_(2,1)*z_(3,5)" );
assert(toExternalString L_5 == "z_(1,3)*z_(2,1)*z_(3,5)" );
assert(toExternalString L_6 == "z_(1,2)*z_(2,1)*z_(3,5)" );
assert(toExternalString L_7 == "z_(1,2)*z_(2,3)*z_(3,4)" );
assert(toExternalString L_8 == "z_(1,3)*z_(2,1)*z_(3,4)" );
assert(toExternalString L_9 == "z_(1,2)*z_(2,1)*z_(3,4)" );
assert(toExternalString L_10 == "z_(1,2)*z_(2,1)*z_(3,3)" );
assert(toExternalString L_11 == "z_(1,2)*z_(2,1)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_12 == "z_(2,1)*z_(3,2)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_13 == "z_(1,2)*z_(3,1)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_14 == "z_(1,4)*z_(2,2)*z_(3,5)*z_(3,1)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_15 == "z_(1,3)*z_(2,2)*z_(3,5)*z_(3,1)*z_(4,3)*z_(5,4)" );
assert(toExternalString L_16 == "z_(1,3)*z_(2,2)*z_(3,4)*z_(3,1)*z_(4,3)*z_(5,4)" );

L = diagRevLexInit I; 
assert(numgens L == 7);
assert(toExternalString L_0 == "z_(1,2)" );
assert(toExternalString L_1 == "z_(1,1)" );
assert(toExternalString L_2 == "z_(1,3)*z_(2,2)" );
assert(toExternalString L_3 == "z_(2,1)" );
assert(toExternalString L_4 == "z_(2,2)*z_(3,1)" );
assert(toExternalString L_5 == "z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)" );
assert(toExternalString L_6 == "z_(1,4)*z_(2,2)^2*z_(3,3)*z_(4,1)" );


L = diagRevLexInit PI; 
assert(numgens L == 17);
assert(toExternalString L_0 == "z_(1,2)");
assert(toExternalString L_1 == "z_(1,1)");
assert(toExternalString L_2 == "z_(1,3)*z_(2,2)");
assert(toExternalString L_3 == "z_(2,1)");
assert(toExternalString L_4 == "z_(2,2)*z_(3,1)");
assert(toExternalString L_5 == "z_(1,4)*z_(2,2)*z_(3,3)*z_(4,5)");
assert(toExternalString L_6 == "z_(1,3)^2*z_(2,4)*z_(3,2)*z_(4,5)");
assert(toExternalString L_7 == "z_(1,3)*z_(2,4)*z_(3,1)*z_(4,5)");
assert(toExternalString L_8 == "z_(1,4)*z_(2,5)*z_(3,1)*z_(4,2)");
assert(toExternalString L_9 == "z_(1,3)*z_(2,5)*z_(3,1)*z_(4,2)");
assert(toExternalString L_10 == "z_(1,3)*z_(2,4)*z_(3,1)*z_(4,2)");
assert(toExternalString L_11 == "z_(1,4)*z_(2,3)*z_(2,2)*z_(3,5)*z_(4,1)");
assert(toExternalString L_12 == "z_(1,4)*z_(2,2)^2*z_(3,5)*z_(4,1)");
assert(toExternalString L_13 == "z_(1,5)*z_(2,2)^2*z_(3,3)*z_(4,1)");
assert(toExternalString L_14 == "z_(1,4)*z_(2,2)^2*z_(3,3)*z_(4,1)");
assert(toExternalString L_15 == "z_(1,5)*z_(2,3)*z_(2,2)*z_(3,4)*z_(3,3)*z_(4,5)*z_(4,1)");
assert(toExternalString L_16 == "z_(1,4)*z_(1,3)*z_(2,4)*z_(2,3)*z_(3,5)*z_(3,2)*z_(4,5)*z_(4,1)");
///

--subwordComplex--
TEST ///
F = facets subwordComplex({4,3,2,1});
assert(toExternalString(F) == "{z_(1,4)*z_(2,3)*z_(2,4)*z_(3,2)*z_(3,3)*z_(3,4)*z_(4,1)*z_(4,2)*z_(4,3)*z_(4,4)}")
///


TEST ///
--schubertDecompose--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(flatten schubertDecompose schubertDeterminantalIdeal w == w)
assert(schubertDecompose schubertDeterminantalIdeal I == {{3, 5, 1, 2, 4}, {5, 2, 3, 1, 4}, {4, 2, 5, 1, 3}, {3, 4, 1, 5, 2}, {4, 2, 3, 5, 1}})
assert(schubertDecompose schubertDeterminantalIdeal PI == {{3, 6, 1, 2, 4, 5}, {6, 2, 3, 1, 4, 5}, {4, 2, 6, 1, 3, 5}, {3, 4, 1, 6, 2, 5}, {4, 2, 3, 6, 1, 5}})
///

TEST ///
--permSetOfASM--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(permSetOfASM I == {{3, 5, 1, 2, 4}, {5, 2, 3, 1, 4}, {4, 2, 5, 1, 3}, {3, 4, 1, 5, 2}, {4, 2, 3, 5, 1}})
///

TEST ///
--isIntersectionOfSchubertDeterminantalIdeals--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(isIntersectionOfSchubertDeterminantalIdeals schubertDeterminantalIdeal w == true );
assert(isIntersectionOfSchubertDeterminantalIdeals schubertDeterminantalIdeal I == true );
assert(isIntersectionOfSchubertDeterminantalIdeals schubertDeterminantalIdeal PI == true);
///

TEST ///
--isASMIdeal--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(isASMIdeal schubertDeterminantalIdeal w == true );
assert(isASMIdeal schubertDeterminantalIdeal I == true );
assert(isASMIdeal schubertDeterminantalIdeal PI == true);
///

TEST ///
--isASM--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(isASM permToMatrix w == true );
assert(isASM I == true );
assert(isASM PI == false);
///

TEST ///
--getASM--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(getASM schubertDeterminantalIdeal w == matrix{{0, 1, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 1}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 1, 0, 0}} );
assert(getASM schubertDeterminantalIdeal I == I );
assert(getASM schubertDeterminantalIdeal PI == PI);
///

TEST ///
--isMinRankTable--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(isMinRankTable rankTable w == true );
assert(isMinRankTable rankTable I == true );
assert(isMinRankTable rankTable PI == true );
///

TEST ///
--rankTableToASM--
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(rankTableToASM rankTable w == matrix{{0, 1, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 1}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 1, 0, 0}});
assert(rankTableToASM rankTable I == I);
assert(rankTableToASM rankTable PI == PI);
///

TEST ///
w = {2,1,6,3,5,4};
I = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,1},{0,0,0,1,0}};
PI = matrix{{0,0,1,0,0},{0,1,-1,1,0},{1,-1,1,0,0},{0,1,0,-1,0},{0,0,0,1,0}};

assert(toOneLineNotation getASM schubertDeterminantalIdeal w == w );
assert(toOneLineNotation getASM schubertDeterminantalIdeal I == {} );
assert(toOneLineNotation getASM schubertDeterminantalIdeal PI == {});
///


TEST ///
---isSchubertCM
--assert(isSchubCM({1,3,2}) == true)
assert(isSchubertCM(matrix{{0,0,0,1},{0,1,0,0},{1,-1,1,0},{0,1,0,0}}) == true)
assert(isSchubertCM(matrix{{0, 0, 1, 0}, {1, 0, -1, 1}, {0, 1, 0, 0}, {0, 0, 1, 0}}) == false)
///

TEST ///
assert(#ASMFullList(1) == 1);
///

TEST ///
--for schubertAdd
A = {3,2,4,1};
B = {2,3,1};
I = schubertDeterminantalIdeal A;
J = schubertDeterminantalIdeal B;
K = schubertAdd {A,B};
assert(K == sub(I + sub(J, ring I), ring K))

---

A = matrix{{0,0,0},{0,1,0},{1,-1,0}}
B = matrix{{0,1,0,0,0},{0,0,0,1,0},{1,-1,1,0,0},{0,0,0,0,1},{0,1,0,0,0}}
C = {3,1,4,2}
L = schubertAdd {A,B,C}

I = schubertDeterminantalIdeal A;
J = schubertDeterminantalIdeal B;
K = schubertDeterminantalIdeal C;
I' = sub(I, ring J)
K' = sub(K, ring J)
assert(L == sub(I'+J+K', ring L))

///

TEST ///
--for padASM
B = matrix{{0,1,0,0,0},{0,0,0,1,0},{1,-1,1,0,0},{0,0,0,0,1},{0,1,0,0,0}}
assert(padASM(B,0) == B)
assert(padASM(permToMatrix {3,1,2}, 2) == permToMatrix {3,1,2,4,5})

///

TEST ///
assert(descentSet {3,1,2} == {1});
assert(descentSet {3,1,2,5,4} == {1,4});
assert(descentSet {1,2,3} == {});
///

TEST ///
--rankTableFromMatrix nonsquare example
M = matrix{{0,3,4},{1,1,1}}
A = matrix{{0,1,1},{1,1,1}}
assert(rankTableFromMatrix M == A)
assert(rankTableFromMatrix A == A)
///

TEST ///
--toOneLineNotation edge case
assert(toOneLineNotation({1},1) == {1})

///
