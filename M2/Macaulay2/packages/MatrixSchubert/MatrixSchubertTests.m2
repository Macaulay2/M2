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
---isSchubertCM (Matrix input checks pdim == codim; List input uses Fulton's theorem)
assert(isSchubCM({1,3,2}) == true)
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

-------------------------
-------------------------
--**ADDED RIGOROUS COVERAGE**--
-- Direct tests for permutation utilities, ASM_Lists generators, option keys,
-- algorithm branches, the PipeDream type, and subwordComplex.
-------------------------
-------------------------

TEST ///
-- isPerm: recognizes 1-line-notation permutations; rejects repeated values,
-- gaps, and out-of-range entries. Boundary inputs {} and {1} included.
assert(isPerm {1})
assert(isPerm {2,1})
assert(isPerm {1,2,3})
assert(isPerm {3,1,2})
assert(isPerm {})
assert(not isPerm {1,1})
assert(not isPerm {1,3})
assert(not isPerm {0,1})
assert(not isPerm {2,3,4})
///

TEST ///
-- firstDescent / lastDescent: index of the first/last descent of a permutation.
-- error test: both reject the identity (no descent) and non-permutations.
assert(firstDescent {1,3,2} == 2 and lastDescent {1,3,2} == 2)
assert(firstDescent {3,2,1} == 1 and lastDescent {3,2,1} == 2)
assert(firstDescent {2,1,4,3} == 1 and lastDescent {2,1,4,3} == 3)
assert(firstDescent {1,4,3,2} == 2 and lastDescent {1,4,3,2} == 3)
assert(try (firstDescent {1,2,3}; false) else true)
assert(try (lastDescent {1,2,3}; false) else true)
assert(try (firstDescent {1,1}; false) else true)
assert(try (lastDescent {2,2}; false) else true)
///

TEST ///
-- inverseOf: the group inverse. Property tests: it is an involution, and a
-- permutation composed with its inverse (either order) is the identity.
assert(inverseOf {2,3,1} == {3,1,2})
assert(inverseOf {3,1,2} == {2,3,1})
assert(inverseOf {1,2,3,4} == {1,2,3,4})
w = {4,1,5,2,3};
assert(inverseOf inverseOf w == w)
assert(composePerms(w, inverseOf w) == toList(1..#w))
assert(composePerms(inverseOf w, w) == toList(1..#w))
assert(try (inverseOf {1,1}; false) else true)
///

TEST ///
-- longestPerm: the longest element {n,n-1,...,1} of S_n. Property test: its
-- Coxeter length is n(n-1)/2. Boundary: a non-positive argument must error.
assert(longestPerm 1 == {1})
assert(longestPerm 4 == {4,3,2,1})
assert(all(1..6, n -> permLength longestPerm n == n*(n-1)//2))
assert(try (longestPerm 0; false) else true)
assert(try (longestPerm(-3); false) else true)
///

TEST ///
-- avoidsAllPatterns: whether a permutation avoids every pattern in a list.
-- Property tests: it agrees with the conjunction of isPatternAvoiding, and
-- reduces to isVexillary for the single pattern {2,1,4,3}.
assert(avoidsAllPatterns({1,2,3,4}, {{2,1}}))
assert(not avoidsAllPatterns({1,2,3,4}, {{2,1},{1,2}}))
assert(avoidsAllPatterns({2,1,4,3}, {}))
w = {7,2,5,8,1,3,6,4};
pats = {{2,1},{1,3,2},{2,1,4,3}};
assert(avoidsAllPatterns(w, pats) == all(pats, p -> isPatternAvoiding(w, p)))
assert(avoidsAllPatterns(w, {{2,1,4,3}}) == isVexillary w)
///

TEST ///
-- permToMatrix / toOneLineNotation: the permutation <-> permutation-matrix
-- bijection. Property tests: round-trip recovery, and every permutation matrix
-- is an ASM. error test: a non-permutation must be rejected.
assert(permToMatrix {2,1} == matrix{{0,1},{1,0}})
assert(permToMatrix {1,2,3} == id_(ZZ^3))
assert(permToMatrix {3,1,2} == matrix{{0,0,1},{1,0,0},{0,1,0}})
for w in {{1},{2,1},{3,1,2},{2,1,4,3},{4,1,5,2,3}} do (
    assert(toOneLineNotation permToMatrix w == w);
    assert(isASM permToMatrix w)
    )
assert(try (permToMatrix {2,2}; false) else true)
///

TEST ///
-- grothendieckPolynomial Algorithm option: the default "DividedDifference" and
-- the "PipeDream" algorithm must produce the same polynomial.
-- error test: an unrecognized Algorithm value must be rejected.
for w in {{1,3,2},{2,1,4,3}} do
    assert(toExternalString grothendieckPolynomial w
        == toExternalString grothendieckPolynomial(w, Algorithm => "PipeDream"))
assert(try (grothendieckPolynomial({2,1,3}, Algorithm => "Nonsense"); false) else true)
///

TEST ///
-- schubertPolynomial Algorithm option: the default "DividedDifference" and the
-- "Transition" algorithm must produce the same polynomial.
-- error test: an unrecognized Algorithm value must be rejected.
for w in {{2,1,5,4,3},{1,4,3,2}} do
    assert(toExternalString schubertPolynomial w
        == toExternalString schubertPolynomial(w, Algorithm => "Transition"))
assert(try (schubertPolynomial({2,1,3}, Algorithm => "Nonsense"); false) else true)
///

TEST ///
-- CoefficientRing / Variable options on the ideal constructors must take
-- effect: they change the coefficient field and the variable name.
assert(char ring antiDiagInit({2,1,3}, CoefficientRing => ZZ/101) == 101)
assert(char ring first fultonGens({3,2,1}, CoefficientRing => ZZ/2) == 2)
I = schubertDeterminantalIdeal({2,1,3}, Variable => getSymbol "y");
assert(toExternalString (ring I)_0 == "y_(1,1)")
J = antiDiagInit({2,1,3}, Variable => getSymbol "w");
assert(toExternalString (ring J)_0 == "w_(1,1)")
///

TEST ///
-- property test: an initial ideal preserves the Hilbert function, so the
-- antidiagonal initial ideal and the three diagonal initial ideals (SE, NW,
-- revlex) all share the codimension and degree of the Schubert ideal.
for w in {{1,3,2},{2,1,4,3},{3,1,4,2}} do (
    I = schubertDeterminantalIdeal w;
    inits = {antiDiagInit w, diagLexInitSE w, diagLexInitNW w, diagRevLexInit w};
    assert(all(inits, K -> codim K == codim I));
    assert(all(inits, K -> degree K == degree I))
    )
A = matrix{{0,1,0},{1,-1,1},{0,1,0}};
IA = schubertDeterminantalIdeal A;
initsA = {antiDiagInit A, diagLexInitSE A, diagLexInitNW A, diagRevLexInit A};
assert(all(initsA, K -> codim K == codim IA and degree K == degree IA))
///

TEST ///
-- cohenMacaulayASMsList / nonCohenMacaulayASMsList: every listed matrix is an
-- ASM, and the CM / non-CM labelling is correct (cross-checked against
-- isSchubertCM). Boundary: nonCohenMacaulayASMsList is {} for n <= 3, and an
-- out-of-range size must error.
cm4 = cohenMacaulayASMsList 4;
assert(#cm4 == 15)
assert(all(cm4, isASM))
assert(all(cm4, isSchubertCM))
assert(nonCohenMacaulayASMsList 3 == {})
ncm4 = nonCohenMacaulayASMsList 4;
assert(#ncm4 == 3)
assert(all(ncm4, isASM))
assert(all(ncm4, A -> not isSchubertCM A))
assert(try (cohenMacaulayASMsList 0; false) else true)
assert(try (cohenMacaulayASMsList 7; false) else true)
assert(try (nonCohenMacaulayASMsList 7; false) else true)
///

TEST ///
-- ASMRandomList(n,m): returns m ASMs of size n, each drawn from ASMFullList n.
-- Boundary: a size outside 1..7 must error.
full4 = ASMFullList 4;
sample = ASMRandomList(4, 8);
assert(#sample == 8)
assert(all(sample, isASM))
assert(all(sample, A -> member(A, full4)))
assert(try (ASMRandomList(8, 2); false) else true)
assert(try (ASMRandomList(0, 2); false) else true)
///

TEST ///
-- initialIdealsList(n): the precomputed antidiagonal initial ideals of size n.
-- Returns a nonempty list of ideals; the size must lie in 3..6.
L = initialIdealsList 3;
assert(#L == 7)
assert(all(L, K -> instance(K, Ideal)))
assert(try (initialIdealsList 2; false) else true)
assert(try (initialIdealsList 7; false) else true)
///

TEST ///
-- PipeDream: the exported type returned by pipeDreams / pipeDreamsNonReduced.
-- Covers type membership, the PipeDream == List comparison, the net / toString
-- display methods, reduced-subset-of-nonreduced containment, and the property
-- that every reduced pipe dream of w has exactly permLength(w) crossing tiles.
assert(instance(PipeDream, Type))
D = first pipeDreams {1};
assert(class D === PipeDream)
assert(D == {{"/"}})
assert(instance(net D, Net))
assert(instance(toString D, String))
crosses = E -> sum apply(toList E, row -> #select(row, t -> t == "+"));
for w in {{3,1,2},{2,1,4,3},{1,4,3,2}} do (
    pds = pipeDreams w;
    assert(all(pds, E -> instance(E, PipeDream)));
    assert(all(pipeDreamsNonReduced w, E -> instance(E, PipeDream)));
    assert(isSubset(set pds, set pipeDreamsNonReduced w));
    assert(all(pds, E -> crosses E == permLength w))
    )
///

TEST ///
-- subwordComplex(w): the simplicial complex whose Stanley-Reisner ideal is the
-- antidiagonal initial ideal of w (Knutson-Miller). The sub accounts for the
-- fresh ring built by simplicialComplex.
assert(instance(subwordComplex {2,1,3}, SimplicialComplex))
for w in {{2,1,3},{1,4,3,2},{3,1,4,2}} do (
    adi = antiDiagInit w;
    assert(adi == sub(monomialIdeal subwordComplex w, ring adi))
    )
///
