TEST ///
P = convexHull transpose matrix {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
M = matrix {{1, 0}, {0, -1}};
assert isSymmetric(P, M)
///

TEST /// -* isSymmetric hypersimplex(4,2) at 2134 *-
needsPackage "Permutations";
P = convexHull transpose matrix{
    {1,1,0,0},
    {1,0,1,0},
    {1,0,0,1},
    {0,1,1,0},
    {0,1,0,1},
    {0,0,1,1}
    };
assert( isSymmetric(P, matrix permutation {2, 1, 3, 4}) );
///

TEST /// -* representationRing for S_4 *-
(R,T) = representationRing(4, ReturnTable => true);
assert(numgens representationRing 4 == numColumns T);
assert(numgens representationRing 4 == 5)
///

TEST /// -* orbitPolytope of the  point {1/2,0,1} *-
P  = orbitPolytope(transpose matrix{{1/2,0,1}});
expected = transpose matrix{{1,1/2,0},{1/2,1,0},{1,0,1/2}, {0,1,1/2},{1/2,0,1},{0,1/2,1}};
assert(sort vertices P == sort expected)
///

TEST /// -* isSymmetric on a polytope symmetric to matrix{{0,1,0},{1,0,0},{0,0,1}} *-
P = orbitPolytope(transpose matrix{{1/2,0,1}});
assert(isSymmetric(P, matrix{{0,1,0},{1,0,0},{0,0,1}}))
///

TEST /// -* isSymmetric on a polytope with matrice that its not symmetric against*-
P = convexHull transpose matrix "1,0,0;0,2,0;0,0,3";
M = matrix{{1,1,0},{1,0,0},{0,1,1}}
assert(isSymmetric(P, M) == false)
///

TEST /// -* generateGroup on an element of order 4, checks that it returns in order *-
g = matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0}}
Gs = generateGroup {g}
assert(length(Gs) == 4)
assert(Gs#0 == g^4)
assert(Gs#1 == g)
assert(Gs#2 == g^2)
///

TEST /// -* generateGroup generates S4 when given generators for S4 *-
gList = {matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0}}, matrix{{0,0,1,0},{1,0,0,0},{0,0,0,1},{0,1,0,0}}}
Gs = generateGroup gList
assert(#Gs == 24)
///

TEST /// -* fixedPolytope of the triangle formed by e_1,e_2,e_3 along e_1 <---> e_2 *-
P = orbitPolytope(transpose matrix{{1,0,0}});
fP = fixedPolytope(P, matrix{{0,1,0},{1,0,0},{0,0,1}});
assert(sort vertices fP == sort transpose matrix{{0,0,1},{1/2,1/2,0}})
///

TEST /// -* cycleTypeRepresentatives on 2 *-
assert(cycleTypeRepresentatives 2 == { matrix{{0,1},{1,0}},matrix{{1,0},{0,1}}})
///


TEST /// -* simple isEffective tests *-
    -- Triangle in R4
    P = convexHull transpose matrix "1,0,0,0;0,1,0,0;0,0,1,0" 
    g = matrix "0,1,0,0;1,0,0,0;0,0,1,0;0,0,0,1"
    assert(isEffective (equivariantEhrhartSeries(P, {g}))_0);

    -- Square in R4
    P = convexHull transpose matrix "1,1,0,0;0,1,1,0;0,0,1,1;1,0,0,1"
    g = matrix "0,0,0,1;1,0,0,0;0,1,0,0;0,0,1,0"
    assert(isEffective (equivariantEhrhartSeries(P, {g^2}))_0);

    -- hypersimplex
    hypersimplex = method()
    hypersimplex(ZZ, ZZ) := (n, k) -> (
        convexHull transpose matrix for s in subsets(n, k) list (
        for i from 0 to n-1 list if member(i, s) then 1 else 0
        )
        )
    P = hypersimplex(5,3);
    g = matrix randomPermutation ambDim P;
    assert(isEffective (equivariantEhrhartSeries(P,{g}))_0);
///

TEST /// -*check that a cyclic group is generated in order*-
    g = (matrix "0,1;1,0") ++ (matrix "0,0,1;1,0,0;0,1,0");
    expectedGroup = {g^0, g^1, g^2, g^3, g^4, g^5};
    assert(generateGroup {g} == expectedGroup);
///

TEST /// -*check that symmetric group character table is in the right order*-
    R = QQ[x_1 .. x_5];
    T = symmetricGroupTable(R);
    result = matrix(R, {
        {1,  1,  1,  1,  1,  1, 1},
        {-1, 0, -1,  1,  0,  2, 4},
        {0, -1,  1, -1,  1,  1, 5},
        {1,  0,  0,  0, -2,  0, 6},
        {0,  1, -1, -1,  1, -1, 5},
        {-1, 0,  1,  1,  0, -2, 4},
        {1, -1, -1,  1,  1, -1, 1}
        });
    assert(T.table == result);
///

TEST /// -*check that generateGroup errors correctly*-
    g = matrix "1,1;0,1"; -- generates an infinite group
    E = trap generateGroup({g}, MaxComputeTime => 1)
    assert(E_0 === null)
    assert(toString(E_1) == "time exceeded MaxComputeTime")
///

TEST /// -*check options for equivariantEhrhartSeries*-
    P = convexHull transpose matrix "1,0,0;0,1,0;0,0,1";
    ES = equivariantEhrhartSeries P;
    assert(class class ES_0 === PolynomialRing);
    assert(class ES_1 === Matrix);
    assert(class ES_2 === List);
    ES' = equivariantEhrhartSeries(P,
        ReturnTable     => false,
        ReturnClassReps => false,
        ReturnHStarList => false);
    assert(#ES' == 1);
///
