-- 0
TEST ///
X = toricProjectiveSpace 2;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1+y_2+1);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == {1,1,1})
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == {1,1,1})
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == X_{0} )
assert( classFromTropical(X,I) == X_{0} )

S = ring X;
f = S_0+S_1+S_2;
assert ( listForm torusIntersection(X,f) == listForm(y_1+y_2+1) )
assert ( classFromTropical(X,I) == classFromTropicalCox(X,ideal(f)) )
f = S_0*S_1+S_0*S_2+S_1*S_2;
I = ideal(y_1*y_2+y_1+y_2);
assert ( listForm(torusIntersection(X,f)) == listForm(y_1*y_2+y_1+y_2) )
assert ( classFromTropical(X,I) == classFromTropicalCox(X,ideal(f)) )
f = S_0^2 + S_1^2 + S_2^2;
assert ( listForm torusIntersection(X,f) == listForm(y_1^2+y_2^2+1) )
///

-- 1
TEST ///
X = toricProjectiveSpace 2;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1*y_2+y_1+y_2);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == {1,1,1})
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == {2,2,2})
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == 2*X_{0} )
assert( classFromTropical(X,I) == 2*X_{0} )
///

-- 2
TEST ///
X = toricProjectiveSpace 2;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1^2 + y_2 + 1);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == {1,1,2})
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == {2,2,2})
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == 2*X_{0} )
assert( classFromTropical(X,I) == 2*X_{0} )
///

-- 3
TEST ///
X = toricProjectiveSpace 3;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1^2 + y_2 + y_3 + 1);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == {1,1,1,1,2,1})
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == {2,2,2,2,2,2})
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == 2*X_{0} )
assert( classFromTropical(X,I) == 2*X_{0} )
S = ring X;
f = S_0*S_1*S_2+S_0*S_1*S_3+S_0*S_2*S_3+S_1*S_2*S_3;
assert ( listForm(torusIntersection(X,f)) == listForm(y_1*y_2*y_3+y_1*y_2+y_1*y_3+y_2*y_3) )
///

-- 4
TEST ///
X = toricProjectiveSpace 3;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1 + y_2 + y_3 + 1, y_1 + 2*y_2 + 3*y_3 - 1); -- U_2,4
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == {1,1,1,1})
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == {1,1,1,1})
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == X_{0,1} )
assert( classFromTropical(X,I) == X_{0,1} )
///

-- 5
TEST ///
X = toricProjectiveSpace 4;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1^2 + y_2 + y_3 + y_4 + 1);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == {1,1,1,1,1,1,1,2,1,1})
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == toList(10:2))
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == 2*X_{0,1,2} )
assert( classFromTropical(X,I) == 2*X_{0,1,2} )
S = ring X;
f = S_0^4 + S_1^4 + S_2^4 + S_4^4 + S_0*S_3^3 + S_1*S_3^3 + S_2*S_3^3 + S_4*S_3^3;
assert ( listForm(torusIntersection(X,f)) == listForm(y_1^4+y_2^4+y_1*y_3^3+y_2*y_3^3+y_3^3*y_4+y_4^4+y_3^3+1) )
///

-- 6
TEST ///
X = toricProjectiveSpace 4;
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1 + y_2 + y_3 + y_4 + 1, y_1 + 2*y_2 + 3*y_3 + 4*y_4 -1); -- U_3,5
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
assert(mult == toList(10:1))
prod = pushforwardMultiplicity(X,X',mult,k);
assert(prod == toList(10:1))
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( D == X_{0,1} )
assert( classFromTropical(X,I) == X_{0,1} )
///

-- 7
TEST ///
-- Blow up of P^2 at one point
X = normalToricVariety({{1,0},{1,1},{0,1},{-1,-1}},{{0,1},{1,2},{2,3},{3,0}});
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(y_1^2 + y_1*y_2 + y_2^2);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
prod = pushforwardMultiplicity(X,X',mult,dim T);
Y = poincareDuality(prod,X,k);
O = orbits(X,k);
D = sum apply(#O, s -> Y_s * X_(O_s));
assert( mult == {2,2} )
assert( prod == {0,2,0,2} )
assert( D == 2*X_{0} )
assert( classFromTropical(X,I) == 2*X_{0} )
-- The class is 2(H-E)
///

-- 8
TEST ///
--chromatic polynomial of K_3 with Huh-Katz Theorem
X = NormalToricVarieties$cartesianProduct apply((3,3), i-> toricProjectiveSpace i);
n = dim X;
TR = QQ[y_1..y_n];
I = ideal(-y_1+y_2+y_3, y_1*y_4-1, y_2*y_5-1, y_3*y_6-1);
D = classFromTropical(X,I);
assert( D == 2* X_{0,1,2,4} + 3 * X_{0,1,4,5} + X_{0,4,5,6} )
///

-- 9
TEST ///
-- Keel-Vermeire divisor of M_0,6
R = QQ[x_0..x_8];
I = ideal {-x_0+x_3+x_4, -x_1+x_3+x_5,-x_2+x_3+x_6, -x_0+x_2+x_7, -x_1+x_2+x_8, -x_0+x_1+1};
raysList = {{1,0,0,0,0,0,0,0,0},{0,1,0,0,0,0,0,0,0},
{0,0,1,0,0,0,0,0,0},{-1,-1,-1,-1,0,0,0,0,0},
{0,0,0,1,0,0,0,0,0},{0,0,0,0,1,0,0,0,0},
{1,0,0,1,1,0,0,0,0},{0,0,0,0,0,1,0,0,0},
{0,1,0,1,0,1,0,0,0},{0,0,0,-1,-1,-1,-1,0,0},
{-1,-1,-1,-1,-1,-1,-1,0,0},{0,0,0,0,0,0,1,0,0},
{0,0,1,1,0,0,1,0,0},{0,0,0,0,0,0,0,1,0},
{1,0,1,0,0,0,0,1,0},{0,0,0,0,1,0,1,1,0},
{1,0,1,1,1,0,1,1,0},{0,0,-1,0,0,0,-1,-1,-1},
{-1,-1,-1,-1,0,0,-1,-1,-1},{0,0,-1,-1,-1,-1,-1,-1,-1},
{-1,-1,-1,-1,-1,-1,-1,-1,-1},{0,0,0,0,0,0,0,0,1},
{0,1,1,0,0,0,0,0,1},{0,0,0,0,0,1,1,0,1},{0,1,1,1,0,1,1,0,1}};
coneList = {{0,6,16},{0,6,17},{0,6,21},{0,7,14},{0,7,17},{0,7,23},
{0,9,14},{0,9,19},{0,9,21},{0,11,16},{0,11,19},{0,11,23},
{0,14,16},{0,17,19},{0,21,23},{1,5,15},{1,5,17},{1,5,22},
{1,8,13},{1,8,17},{1,8,24},{1,9,13},{1,9,19},{1,9,22},
{1,11,15},{1,11,19},{1,11,24},{1,13,15},{1,17,19},{1,22,24},
{2,5,16},{2,5,18},{2,5,22},{2,7,14},{2,7,18},{2,7,24},
{2,9,14},{2,9,20},{2,9,22},{2,12,16},{2,12,20},{2,12,24},
{2,14,16},{2,18,20},{2,22,24},{3,5,15},{3,5,18},{3,5,21},
{3,7,13},{3,7,18},{3,7,23},{3,10,13},{3,10,20},{3,10,21},
{3,11,15},{3,11,20},{3,11,23},{3,13,15},{3,18,20},{3,21,23},
{4,6,16},{4,6,17},{4,6,21},{4,8,13},{4,8,17},{4,8,24},
{4,10,13},{4,10,20},{4,10,21},{4,12,16},{4,12,20},{4,12,24},
{4,13,16},{4,17,20},{4,21,24},{5,6,16},{5,6,17},{5,6,21},
{5,15,16},{5,17,18},{5,21,22},{7,8,13},{7,8,17},{7,8,24},
{7,13,14},{7,17,18},{7,23,24},{9,10,13},{9,10,20},{9,10,21},
{9,13,14},{9,19,20},{9,21,22},{11,12,16},{11,12,20},{11,12,24},
{11,15,16},{11,19,20},{11,23,24},{13,14,16},{13,15,16},{17,18,20},
{17,19,20},{21,22,24},{21,23,24}};
X = normalToricVariety(raysList,coneList);
l = {0,1,2,4,5,7,11,13,21};
f = x_0*x_1-x_2*x_3;
D = classWonderfulCompactification(X,I,f);
D = toricDivisorFromCycle D;
Q = X_1 + X_5 + X_13 + 2*X_15 + X_16 - X_20;
assert( makeTransverse(D,l) == makeTransverse(Q,l) )
///

-- 10
TEST ///
X = normalToricVariety({{1,0},{1,1},{0,1},{-1,0},{-1,-1},{0,-1}},{{0,1},{1,2},{2,3},{3,4},{4,5},{0,5}});
n = dim X;
TR = QQ[y_1..y_n];
S = ring X;
f = S_1*S_2*S_3*S_5 + S_0*S_3*S_4*S_5 + S_1*S_4*S_5^2;
g = torusIntersection(X,f);
assert ( listForm(g) == listForm(y_1+y_2+y_1*y_2) )
I = ideal(g);
T = tropicalVariety(I);
k = dim T;
F = gfanFanCommonRefinement(fan X, fan T);
X' = makeSimplicial (normalToricVariety F);
mult = refineMultiplicity(T,X');
prod = pushforwardMultiplicity(X,X',mult,dim T);
assert ( mult == toList( #(max X') : 1 ) )
assert ( prod == {0,1,0,1,0,1} )
D = classFromTropical(X,I);
assert ( D == X_{0}+X_{1}+X_{2} )
assert ( D == classFromTropicalCox(X,ideal(f)) )
///


-------------------------------------------------

-- TEST ///
-- Code
-- assert(statement)
-- ///
