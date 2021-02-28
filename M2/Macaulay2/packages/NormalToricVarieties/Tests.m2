
-- test 0
TEST ///
X = affineSpace 1;
assert isWellDefined X 
assert (rays X == {{1}})
assert (max X == {{0}})
assert (dim X == 1)
assert (orbits (X,0) === max X)
assert (orbits (X,1) === {{}})
assert not isDegenerate X
assert isSmooth X
assert not isComplete X
assert not isFano X
assert (weilDivisorGroup X == ZZ^1) 
assert (fromWDivToCl X == 0)
assert (classGroup X == ZZ^0)
assert (cartierDivisorGroup X == ZZ^1)
assert (fromCDivToWDiv X == id_(ZZ^1))
assert (fromCDivToPic X == 0)
assert (picardGroup X == ZZ^0)
assert (fromPicToCl X == 0)
assert (nefGenerators X == 0)
assert isEffective X_0
assert try (monomials X_0; false) else true
assert not isEffective (-X_0) 
assert isCartier X_0
assert not isNef X_0
assert (OO X_0 === OO_X^1)
assert (degrees ring X === {{}})
assert (ideal X == 1)
assert (cotangentSheaf X === OO_X^1)
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
assert (null == try affineSpace 0)
///

-- test 1
TEST ///
X = toricProjectiveSpace 1;
assert isWellDefined X
assert (set rays X === set ({-1},{1}))
assert (max X == sort subsets (2,1))
assert (dim X === 1)
assert (orbits (X,0) === max X)
assert (orbits (X,1) === {{}})
assert not isDegenerate X
assert isSmooth X
assert isProjective X
assert isFano X
assert (weilDivisorGroup X == ZZ^2) 
assert (fromWDivToCl X == map (ZZ^1,ZZ^2, i -> 1_ZZ))
assert (classGroup X == ZZ^1)
assert (cartierDivisorGroup X == ZZ^2)
assert (fromCDivToWDiv X == id_(ZZ^2))
assert (fromCDivToPic X == map (ZZ^1,ZZ^2, i -> 1_ZZ))
assert (picardGroup X == ZZ^1)
assert (fromPicToCl X == id_(ZZ^1))
assert (nefGenerators X == 1)
assert (isNef toricDivisor (flatten entries  ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)), X))
assert isEffective X_0
assert (monomials (4*X_0)  == sort first entries basis (degree (4*X_0), ring variety X_0))
assert (X_0 + X_1 === toricDivisor ({1,1},X))
assert (2*X_0 === X_0 + X_0)
assert isVeryAmple X_0
assert (vertices (2*X_0) == matrix {{0,2}})
assert (latticePoints (2*X_0) == matrix {{0,1,2}})
assert (degrees ring X === {{1},{1}})
assert (ideal X == ideal gens ring X)
assert (cotangentSheaf X === OO_X(-2))
assert (all (5, i -> rank HH^0(X,OO_X(i)) == binomial(1+i,i)))
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
assert (null == try toricProjectiveSpace 0)
///

-- test 2
TEST ///
n = 4;
X = toricProjectiveSpace n;
assert isWellDefined X
assert (rays X === {toList (n:-1)} | entries id_(ZZ^n))
assert (max X === subsets (n+1,n))
assert (dim X === n)
assert (orbits (X,1) === sort subsets (n+1,n-1))
assert (orbits (X,2) === sort subsets (n+1,n-2))
assert (orbits (X,4) === {{}})
assert not isDegenerate X
assert isSmooth X
assert isProjective X
assert isFano X
assert (weilDivisorGroup X == ZZ^(n+1)) 
assert (fromWDivToCl X == map (ZZ^1,ZZ^(n+1), i -> 1_ZZ))
assert (classGroup X == ZZ^1)
assert (cartierDivisorGroup X == ZZ^(n+1))
assert (fromCDivToWDiv X == id_(ZZ^(n+1)))
assert (fromCDivToPic X == map (ZZ^1,ZZ^(n+1), i -> 1_ZZ))
assert (picardGroup X == ZZ^1)
assert (fromPicToCl X == id_(ZZ^1))
assert (nefGenerators X == 1)
assert (isNef toricDivisor (flatten entries ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)), X))
assert (isEffective X_0 === true)
assert (monomials (2*X_0)  == sort first entries basis(degree (2*X_0), ring variety X_0))
assert (X_0 + X_1 === toricDivisor ({1,1} | toList (n-1:0),X))
assert (2*X_0 === X_0 + X_0)
assert isVeryAmple X_0
assert (degree X_0 === {sum entries X_0})
assert (degree (2*X_0+X_3) === {sum entries (2*X_0+X_3)})
assert (vertices (2*X_0) == map (ZZ^n,ZZ^1,i -> 0) | 2*id_(ZZ^n))
assert (degrees ring X === toList (n+1 : {1}))
assert (ideal X == ideal gens ring X)
assert (cotangentSheaf (X, Minimize => true) === prune sheaf (X, 
	homology (vars ring X,jacobian ring X)))
assert ({degree (-X_0+3*X_2)} === - degrees OO (-X_0+3*X_2))
assert (all (5, i -> rank HH^0(X,OO_X(i)) == binomial(n+i,i)))
assert (all (5, i -> HH^1(X,OO_X(i)) == 0))
assert (all (5, i -> rank HH^n(X,OO_X(-i-n-1)) == binomial(n+i,i)))
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
///

-- test 3
TEST ///
X = hirzebruchSurface 2;
assert isWellDefined X
assert (rays X == {{1,0},{0,1},{-1,2},{0,-1}})
assert (max X == {{0,1},{0,3},{1,2},{2,3}})	  
assert (dim X == 2)	  
assert (orbits (X,0) === max X)
assert (orbits (X,1) === apply (4, i -> {i}))
assert (orbits (X,2) === {{}})
assert not isDegenerate X
assert isSmooth X
assert isProjective X
assert not isFano X
assert (weilDivisorGroup X == ZZ^4) 
assert (fromWDivToCl X == map (ZZ^2,ZZ^4, matrix{{1,-2,1,0},{0,1,0,1}}))
assert (classGroup X == ZZ^2)
assert (cartierDivisorGroup X == ZZ^4)
assert (fromCDivToWDiv X == id_(ZZ^4))
assert (fromCDivToPic X == map (ZZ^2,ZZ^4, matrix{{1,-2,1,0},{0,1,0,1}}))
assert (picardGroup X == ZZ^2)
assert (fromPicToCl X == id_(ZZ^2))
assert (nefGenerators X == 1)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isEffective X_0
assert (monomials (2*X_0 + X_1) == sort first entries basis (degree (2*X_0 + X_1), ring variety X_0))
assert isWellDefined X_0
assert (X_0 + X_1 === toricDivisor ({1,1,0,0},X))
assert (2*X_0 === X_0 + X_0)
assert (degree (-3*X_0 + 7*X_1) === - first degrees OO (-3*X_0 + 7*X_1))
assert isNef X_0
assert not isNef X_1
assert isVeryAmple (X_2+X_3)
assert (vertices (X_2+X_3) === matrix{{0,1,0,3},{0,0,1,1}})
assert (latticePoints (X_2+X_3) === matrix {{0, 1, 0, 1, 2, 3}, {0, 0, 1, 1, 1, 1}})
assert (degrees ring X === {{1,0},{-2,1},{1,0},{0,1}})
S = ring X;
assert (ideal X == intersect (ideal (S_0,S_2),ideal (S_1,S_3)))
assert (makeSimplicial X === X)
assert (makeSmooth X === X)
///

-- test 4
TEST ///
X = weightedProjectiveSpace {1,2,3};
assert isWellDefined X
assert (rays X == {{-2,-3},{1,0},{0,1}})
assert (max X == {{0,1},{0,2},{1,2}})
assert (dim X == 2)	  
assert (orbits (X,0) === max X)
assert (orbits (X,1) === apply (3, i -> {i}))
assert (orbits (X,2) === {{}})
assert not isDegenerate X
assert isSimplicial X
assert not isSmooth X
assert isProjective X
assert isFano X
assert (weilDivisorGroup X == ZZ^3) 
assert (fromWDivToCl X == map (ZZ^1,ZZ^3, matrix{{1,2,3}}))
assert (classGroup X == ZZ^1)
assert (cartierDivisorGroup X == ZZ^3)
assert (rank fromCDivToPic X === 1)
assert (picardGroup X == ZZ^1)
assert (fromPicToCl X == map (ZZ^1,ZZ^1, {{6}}))
assert (nefGenerators X == 1)
assert all (entries transpose  ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isEffective X_0
assert (X_0 + X_1 === toricDivisor ({1,1,0},X))
assert (2*X_0 === X_0 + X_0)
assert isNef X_0
assert not isCartier X_0
assert isQQCartier X_0
assert not isAmple X_1
assert isAmple  (3*X_1)
assert isVeryAmple (3*X_1)
assert (vertices (6*X_0) === matrix{{0,3,0},{0,0,2}})
assert (OO (6*X_0) === OO (3*X_1))
assert (degrees ring X === apply (3, i -> {i+1}))
assert (ideal X == ideal gens ring X)
Y = makeSmooth X;
assert isWellDefined Y
assert isSmooth Y
assert (set rays Y === set {{-2,-3},{1,0},{0,1},{-1,-2},{-1,-1},{0,-1}})
assert (sort max Y === sort {{0,5},{0,4},{1,2},{1,3},{2,4},{3,5}})
///

-- test 5
TEST ///
X = kleinschmidt (9,{1,2,3});
assert (orbits (X, dim X) === {{}})
assert isWellDefined X
assert isFano X
assert isSmooth X
assert (picardGroup X === ZZ^2)
assert (nefGenerators X == 1)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
///

-- test 6
TEST ///
assert (all (5, i -> (
    X := smoothFanoToricVariety (2,i);
    isWellDefined X and isSmooth X and isFano X)))
assert (all (18, i -> (
    X := smoothFanoToricVariety (3,i);
    isWellDefined X and isSmooth X and isFano X)))
X = smoothFanoToricVariety (2,4);
assert (HH^1 (X,OO_X (-2,1,1,-2)) == QQ^2)
///

-- test 7
TEST ///
rayList = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
coneList = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
X = normalToricVariety (rayList,coneList);
assert isWellDefined X
assert (dim X === 3)
assert (orbits (X,0) === max X)
assert (orbits (X,2) === apply (6, i -> {i}))
assert not isDegenerate X
assert isSimplicial X
assert not isSmooth X
assert isComplete X
assert not isProjective X
assert not isFano X
assert (weilDivisorGroup X == ZZ^6)
assert (classGroup X === ZZ^3)
assert (picardGroup X === ZZ^3)
assert (nefGenerators X == 0)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isNef (0*X_0)
assert all (6, i -> not isAmple (X_i))
Y = makeSmooth X;
assert isWellDefined Y
assert isSmooth Y
///

-- test 8
TEST ///
X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
assert isWellDefined X
assert (set rays X === set entries matrix{{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},
    {1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}})
assert (dim X === 3)
assert (orbits (X,0) === max X)
assert (orbits (X,2) === apply (8, i -> {i}))
assert not isDegenerate X
assert not isSimplicial X
assert not isSmooth X
assert isProjective X   
assert isFano X
assert (weilDivisorGroup X == ZZ^8) 
assert (classGroup X ==  (cokernel matrix{{2}})^2 ++ ZZ^5)
assert (picardGroup X == ZZ^1)
assert (fromWDivToCl X * fromCDivToWDiv X == fromPicToCl X * fromCDivToPic X)
assert (nefGenerators X == 1)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
assert isEffective X_0
assert not isCartier X_0
K = toricDivisor X;
assert isCartier K
assert not isNef K
Y = makeSimplicial X;
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Y = makeSimplicial (X, Strategy => 1);
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Z = makeSmooth X;
assert isWellDefined Z
assert isSmooth Z 
///

-- test 9
TEST ///
X = normalToricVariety ({{1,0,0,0},{0,1,0,0},{0,0,1,0},{1,-1,1,0},{1,0,-2,0}},
  {{0,1,2,3},{0,4}});
assert isWellDefined X
assert (dim X === 4)
assert (orbits (X,0) === {})
assert (orbits (X,1) === {{0,1,2,3}})
assert (orbits (X,2) === {{0,1},{0,3},{0,4},{1,2},{2,3}})
assert (orbits (X,3) === apply (5, i -> {i}))
assert isDegenerate X
assert not isSimplicial X
assert not isSmooth X
assert not isComplete X
assert (weilDivisorGroup X == ZZ^5)
assert (classGroup X == ZZ^2)
assert (picardGroup X == ZZ^1)
assert isEffective X_0
Y = makeSimplicial X;
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Y = makeSimplicial (X, Strategy => 0);
assert isWellDefined Y
assert isSimplicial Y
assert not isSmooth Y
Z = makeSmooth X;
assert isWellDefined Z
assert isSmooth Z
///

-- test 10
TEST ///
C = normalToricVariety ({{1,0,0},{1,1,0},{1,0,1},{1,1,1}},{{0,1,2,3}});
Bl1 = toricBlowup ({0,1,2,3},C);
assert (rays Bl1 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{2,1,1}})
assert (max Bl1 === {{0,1,4},{0,2,4},{1,3,4},{2,3,4}})
Bl2 = toricBlowup ({0,1},C);
assert (rays Bl2 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{2,1,0}})
assert (max Bl2 === {{0,2,4},{1,3,4},{2,3,4}})
Bl3 = toricBlowup ({0,1,2,3},C,{5,3,4});
assert (rays Bl3 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{5,3,4}})
assert (max Bl3 === {{0,1,4},{0,2,4},{1,3,4},{2,3,4}})
Bl4 = toricBlowup ({0},C);
assert isSimplicial Bl4
assert (rays Bl4 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1}})
assert (max Bl4 === {{0,1,3},{0,2,3}})
X = normalToricVariety (id_(ZZ^3) | (-id_(ZZ^3)));
Bl5 = toricBlowup ({0,2},X);
assert (rays Bl5 === rays X | {{1,0,1}})
assert not isSimplicial Bl5
assert isProjective Bl5 
assert isWellDefined Bl5
Bl6 = toricBlowup ({0},X);
assert (rays Bl6 === rays X)
assert not isSimplicial Bl6
assert isProjective Bl6 
Bl7 = toricBlowup ({7},Bl6);
assert (rays Bl7 === rays X)
assert isSimplicial Bl7
assert isProjective Bl7 
assert isWellDefined Bl7
///

-- test 11
TEST ///
rayList = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{0,1,1,1},{1,0,1,-1},
  {1,-1,0,1},{1,1,-1,0},{0,0,0,-1},{-1,0,-1,1},{0,-1,0,0},{-1,1,0,-1},
  {0,0,-1,0},{-1,-1,1,0}};
coneList = {{0,5},{0,6},{0,7},{1,4},{1,7},{1,11},{2,4},{2,5},{2,13},{3,4},{3,6},
  {3,9},{5,8},{6,10},{7,12},{8,9},{10,11},{12,13}};
X = normalToricVariety (rayList,coneList);
assert not isComplete X
///

-- test 12
TEST ///
-- examples provided by Claudiu Raicu, Mike Stillman, and me to illustrate an
-- earlier bug in 'isProjective'
X = normalToricVariety ({{1,2,3},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},
	{1,-1,-1},{-1,-1,-1}},{{0,1,2,3},{0,1,4,5},{0,2,4,6},{1,3,5,7},
	{2,3,6,7},{4,5,6,7}});
assert isWellDefined X
assert not isSmooth X
assert not isSimplicial X
assert not isProjective X
assert (nefGenerators X == 0)
Y = normalToricVariety ({{-1,-1,-1,-1},{-1,-1,-1,0},{-1,-1,0,2},{-1,0,-1,-1},
	{0,-1,-1,-1},{1,-1,0,-1},{1,2,2,2}},{{0,1,2,5},{0,1,2,6},{0,1,3,5},
	{0,1,3,6},{0,2,5,6},{0,3,5,6},{1,2,4,5},{1,2,4,6},{1,3,4,5},
	{1,3,4,6},{2,4,5,6},{3,4,5,6}});
assert isWellDefined Y
assert not isSmooth Y
assert isSimplicial Y
assert not isProjective Y
Z = normalToricVariety ({{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},
	{0,0,-1}},{{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},
	{1,5,6},{2,3,4},{2,4,5}});
assert isWellDefined Z
assert isComplete Z
assert isSimplicial Z
assert not isProjective Z
///

-- test 13
TEST ///
-- examples provided by Mike Stillman to illustrate an earlier bug in
-- 'cartierDivisorGroup'
X = normalToricVariety ({{-1,7,-5,-4},{-1,-1,1,1},{-1,-1,3,1},{-1,-1,1,2},
	{1,-1,0,0},{-1,3,-2,-1},{-1,-1,2,1},{-1,1,0,0}},{{0,1,4,5},{0,1,4,6},
	{0,1,5,7},{0,1,6,7},{0,2,4,5},{0,2,4,6},{0,2,5,7},{0,2,6,7},{1,3,4,5},
	{1,3,4,6},{1,3,5,7},{1,3,6,7},{2,3,4,5},{2,3,4,6},{2,3,5,7},{2,3,6,7}});
assert isWellDefined X 
assert isSimplicial X
assert (cartierDivisorGroup X == ZZ^8)
assert (weilDivisorGroup X == ZZ^8)
assert isCartier toricDivisor X
assert all (# rays X, i -> isCartier  (2*X_i))
assert all (# rays X, i -> not isCartier X_i)
assert all (entries transpose ( (fromCDivToWDiv X) * (nefGenerators X // fromCDivToPic X)),
    coeffs -> isNef toricDivisor (coeffs, X))
///


-- test 14
TEST ///
-- first example provided by Chris Eur to illustrate an earlier bug in
-- 'normalToricVariety'
P = convexHull transpose matrix unique permutations {1,1,0,0};
X = normalToricVariety P;
assert isWellDefined X
assert (8 === #rays X)
assert (dim P === dim X)
assert (1 === rank picardGroup X)
Q = convexHull ((transpose matrix unique permutations {1,1,0,0}) || matrix {toList{6:1}});
Y = normalToricVariety Q;
assert isWellDefined Y
assert (dim Q === dim Y)
assert (1 === rank picardGroup Y)
assert (rays X == rays Y)
assert (max X == max X)
///

-- test 15
-- testing toric maps
TEST ///
-- not all lattice maps yield toric maps
X = normalToricVariety({{1,0,0},{0,0,1},{0,0,-1}}, {{0,1},{0,2}});
assert isWellDefined X
Y = normalToricVariety({{0,-1}}, {{0}});
assert isWellDefined Y
f = map(Y, X, matrix{{1,0,0}, {0,1,0}})
assert not isWellDefined f
-- the source not proper but the target is so the map is not proper
FF2 = hirzebruchSurface 2;
PP1 = toricProjectiveSpace 1;
Y = normalToricVariety(rays FF2, drop(max FF2, -1));
assert isWellDefined Y
assert not isComplete Y
f = map(PP1, Y, matrix{{1,0}})
assert isWellDefined f
assert not isProper f
g = map(Y, PP1, matrix{{0},{1}})
assert isWellDefined g
assert isProper g
-- the source and target are both not proper but the map is proper
P1A1 = (affineSpace 1) ** (toricProjectiveSpace 1);
assert isWellDefined P1A1
A1 = affineSpace 1;
h = map(A1, P1A1, matrix{{1,0}})
assert isWellDefined h
assert isProper h
--
X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{-1,0,0},{0,0,-1}},{{0,1},{1,2,3},{1,3,4}});
assert isWellDefined X
Y = (toricProjectiveSpace 1) ** (affineSpace 1);
f = map(Y,X,matrix{{1,0,0},{0,1,0}})
assert isWellDefined f
--
X = normalToricVariety({{0,1},{1,0},{0,-1}},{{0,1},{1,2}})
assert isWellDefined X
Y = normalToricVariety({{-1,-1},{1,0},{0,1}},{{0,1},{1,2}})
assert isWellDefined Y
A = id_(ZZ^2)
f = map(Y,X,A)
assert isWellDefined f
assert not isProper f
assert (f == map(Y,X,1))
--
X = normalToricVariety({{0,1},{1,0},{0,-1},{-1,-1}},{{0,1},{1,2},{2,3}})
assert isWellDefined X
Y = normalToricVariety({{-1,-1},{1,0},{0,1}},{{0,1},{1,2}})
assert isWellDefined Y
A = id_(ZZ^2)
f = map(Y,X,A)
assert isWellDefined f
assert isProper f
assert (f == map(Y,X,1))
-- 
X = normalToricVariety({{1,0,0},{0,1,0},{-1,0,0},{0,0,1},{0,0,-1}},{{0,3},{1,2,3},{1,2,4}})
assert isWellDefined X
Y = normalToricVariety({{1,0},{0,1},{-1,0}},{{0},{1,2}})
assert isWellDefined Y
A = matrix{{1,0,0},{0,1,0}}
f = map(Y,X,A)
assert isWellDefined f
assert not isProper f
--
X = normalToricVariety({{0,-1,0},{1,0,0},{0,1,0},{-1,0,0},{0,0,1},{0,0,-1}},{{0},{1,4},{1,5},{2,3,4},{2,3,5}})
assert isWellDefined X
Y = normalToricVariety({{0,-1},{1,0},{0,1},{-1,0}},{{0},{1},{2,3}})
assert isWellDefined Y
A = matrix{{1,0,0},{0,1,0}}
f = map(Y,X,A)
assert isWellDefined f
assert not isProper f
--
X'' = normalToricVariety({{1,0,0},{0,1,0},{-1,0,0},{0,0,1},{0,0,-1}},{{0,3},{0,4},{1,2,3},{1,2,4}})
assert isWellDefined X
Y' = normalToricVariety({{1,0},{0,1},{-1,0}},{{0},{1,2}})
assert isWellDefined Y
A = matrix{{1,0,0},{0,1,0}}
f = map(Y',X'',A)
assert isWellDefined f
assert isProper f
--
X = normalToricVariety({{-1,1,0},{0,0,1},{0,0,-1}},{{0,1},{0,2}})
Y = normalToricVariety({{0,1},{1,0}},{{0,1}})
A = matrix{{1,1,0},{1,1,0}}
f = map(Y,X,A)
assert isWellDefined f
assert not isProper f
--
X = normalToricVariety({{1,-1,0},{1,1,0},{-1,1,0},{0,0,1}},{{0,1,3},{1,2,3}})
Y = normalToricVariety({{0,1},{1,0}},{{0,1}})
A = matrix{{1,1,0},{1,1,0}}
f = map(Y,X,A)
assert isWellDefined f
assert not isProper f
--
X = normalToricVariety({{1,-1,0},{1,1,0},{-1,1,0},{0,0,1},{0,0,-1}},{{0,1,3},{1,2,3},{0,1,4},{1,2,4}})
Y = normalToricVariety({{0,1},{1,0}},{{0,1}})
A = matrix{{1,1,0},{1,1,0}}
f = map(Y,X,A)
assert isWellDefined f
assert isProper f
///

-- test 16
-- more testing of toric maps
TEST ///
X = normalToricVariety({{1,0}, {0,1}, {-1,-1}}, {{0,1}})
Y = toricProjectiveSpace 1
f = map(Y,X, matrix{{1,0}})
D = toricDivisor({-2,3}, Y)
assert (pullback(f,D) == toricDivisor({3,0,-2},X))
assert (pullback(f,OO D) === OO toricDivisor({3,0,-2},X))
--
PP1 = toricProjectiveSpace 1
X = PP1 ** PP1
f = map(PP1, X, matrix{{1, 0}})
DPP1=toricDivisor({1, 1}, PP1)
assert (pullback(f, DPP1) == toricDivisor({1,1,0,0}, X))
--
AA2 = affineSpace 2;
BlO = toricBlowup({0,1}, AA2)
f  = map(AA2, BlO, 1)
DAA2=toricDivisor({1,0},AA2)
assert (pullback(f, DAA2) == toricDivisor({1,0,1},BlO))
assert (pullback(f,OO DAA2) === OO toricDivisor({1,0,1},BlO))
--
Y = weightedProjectiveSpace({1,1,2})
X = normalToricVariety({{1,0},{0,-1},{-1,-2},{-1,0},{0,1}}, {{0,1},{1,2},{2,3},{3,4},{4,0}})
f = map(Y,X, 1)
assert (null == try pullback(f,Y_1))
--
Y = toricProjectiveSpace 2;
X = toricProjectiveSpace 1;
f = map(Y, X, matrix{{1},{1}})
assert (pullback(f,Y_0) == toricDivisor({1,0},X))
g = map(Y, X, matrix{{2},{1}})
assert isWellDefined g
assert (pullback(g, toricDivisor({1,0,1},Y)) == toricDivisor({3,1},X))
--
X = hirzebruchSurface 1;
R = ring X;
PP2 = toricProjectiveSpace 2;
f = map(PP2, X, matrix{{1,0},{0,-1}})
assert isWellDefined f
assert (matrix inducedMap f == matrix{{R_1*R_2, R_0*R_1, R_3}})
D = toricDivisor({1,2,3}, PP2)
assert(pullback(f, OO D) === OO pullback(f, D))
--
AA2 = affineSpace 2;
R = ring AA2;
PP2 = toricProjectiveSpace 2;
f = map(PP2, AA2, 1)
assert isWellDefined f
assert (matrix inducedMap f == matrix{{1,R_0,R_1}})
D = toricDivisor({1,2,3}, PP2);
-- there is only one line bundle on AA2, so there's only one place to go
assert(pullback(f, OO D) === OO pullback(f, D))
///


-- test 17
TEST ///
Y = toricProjectiveSpace 2;
X = hirzebruchSurface 1;
f = map(Y, X, matrix{{1,0},{0,-1}})
assert isWellDefined f
assert isDominant f
assert isSurjective f
--
Y = toricProjectiveSpace 3;
X = affineSpace 3;
f = map(Y, X, matrix{{2,0,0},{1,1,0},{3,1,0}})
assert isWellDefined f
assert not isDominant f
assert not isSurjective f
assert isWellDefined id_Y
assert isDominant id_Y
assert isSurjective id_Y
--
X = affineSpace 2;
Y = normalToricVariety({{1,0,0},{0,1,0}},{{0,1}});
assert isWellDefined Y
f1 = map(X,Y,matrix{{1,0,0},{0,1,0}})
f2 = map(Y,X,matrix{{1,0},{0,1},{0,0}})
assert isWellDefined f1
assert isWellDefined f2
assert isSurjective f1
assert not isSurjective f2
Y = toricProjectiveSpace 2;
f = map(Y,X,matrix{{1,0},{0,1}})
assert isWellDefined f
assert not isSurjective f
--
X = toricProjectiveSpace 1;
Y = toricProjectiveSpace 2;
f = map(Y,X,matrix{{1},{1}});
g = map(Y,X,matrix{{2},{1}});
assert isWellDefined f
assert isWellDefined g
R = ring Y;
assert(ideal f == ideal(R_1-R_2))
assert(ideal g == ideal(R_0*R_1-R_2^2))
--
X = toricProjectiveSpace 1;
Y = hirzebruchSurface 2;
f = map(X,Y,matrix{{1,0}});
fCD = cartierDivisorGroup f;
fPic = picardGroup f;
assert(source fPic == picardGroup target f);
assert(target fPic == picardGroup source f);
assert(source fCD == cartierDivisorGroup target f);
assert(target fCD == cartierDivisorGroup source f);
assert(fPic * fromCDivToPic(X) == fromCDivToPic(Y) * fCD)
--
X = toricProjectiveSpace 2;
Y = hirzebruchSurface (-1);
code (hirzebruchSurface, ZZ)
-- Y is a blowup of X
E = Y_3;
f = map(X,Y,1);
fCD = cartierDivisorGroup f;
assert(fCD*(vector X_0) == vector (Y_2+E))
assert(fCD*(vector X_1) == vector (Y_0+E))
--
n = 3
P = toricProjectiveSpace 3;
f = diagonalToricMap (P,2)
assert isWellDefined f
assert (codim ideal f == n)
///