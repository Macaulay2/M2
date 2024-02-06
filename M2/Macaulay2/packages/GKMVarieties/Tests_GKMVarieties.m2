-*-----------------------------------------------------------------------

Tests for GKMVarieties.m2

-----------------------------------------------------------------------*-

--------------------------------
-- flagGeomTuttePolynomial test
--------------------------------
TEST ///
ML = (random drop(drop(allMatroids 4,1),-1))_{0,1}
TML = apply(ML, m -> {tuttePolynomial m, flagGeomTuttePolynomial flagMatroid({m})}) -- 2.5 seconds
assert all(TML, l -> (map(ring first l, ring last l, gens ring first l))(last l) == first l)
N = flagMatroid(matrix{{1,1,1},{1,0,0}},{1,2}) 
f = flagGeomTuttePolynomial N --see Example 8.24 of [CDMS18]
x = (ring f)_0
y = (ring f)_1
assert (f == x^2*y^2 + x^2*y + x*y^2 + x^2 + x*y)
A = matrix{{1,1,1,1},{0,1,2,3}}
FMr = flagMatroid(A,{1,2})
FM = flagMatroid {uniformMatroid(1,4),uniformMatroid(2,4)}
assert (FMr === FM)
--g = flagGeomTuttePolynomial FM
--assert (16 == sub(g, {(ring g)_0 => 1, (ring g)_1 => 1}))
///

--------------------------------
-- Type A tests (ordinary flag variety)
--------------------------------
TEST ///
X = generalizedFlagVariety("A",3,{1,2})
A = matrix{{1,1,1,1},{0,1,2,3}}
U = flagMatroid {uniformMatroid(1,4),uniformMatroid(2,4)}
C = makeKClass(X,U)
D = orbitClosure(X,A)
O1 = ampleKClass X
assert (C === D)
assert (isWellDefined C)
assert (isWellDefined (C * O1))
Eu1 = euler O1
Eu2 = euler (C * O1)
assert (set exponents Eu1 === set exponents Eu2)
R = X.characterRing
assert (sum(latticePoints U, i -> R_i) == Eu2)
Y = generalizedFlagVariety("A",3,{2},R)
assert (set Y.points === set apply(subsets(4,2), i -> {set i}))
Z = generalizedFlagVariety("A",3,{1},R)
f = flagMap(X,Y)
g = flagMap(X,Z)
assert (O1 === (pullback f)(ampleKClass Y) * (pullback g)(ampleKClass Z))
dualTautSub = makeKClass(Y,apply(Y.points, p -> sum(elements first p, i -> R_i)))
assert ((pushforward f)((pullback g)(ampleKClass Z)) === dualTautSub)
assert (euler ampleKClass X == euler (pushforward f)(ampleKClass X))
///

----------------------------------------------------------------
-- Type C tests: Lagrangian Grassmannian LGr(2,4), and complete flag SpFl(1,2;4)
----------------------------------------------------------------
TEST ///
R = makeCharacterRing 2
X = generalizedFlagVariety("C",2,{2},R)
assert (set X.points === set {{set{0,1}},{set{"0*",1}},{set{0,"1*"}},{set{"0*","1*"}}})
assert (6 == #(momentGraph X).edges)
O1 = ampleKClass X
assert (euler O1 == sum({{1,1},{-1,1},{1,-1},{-1,-1},{0,0}}, i -> R_i))
A1 = matrix{{1,0,1,0},{0,1,0,1}}
A2 = matrix{{1,0,1,2},{0,1,2,1}}
C1 = orbitClosure(X,A1)
C2 = orbitClosure(X,A2)
assert (euler (C1*O1) == sum({{1,1},{-1,1},{1,-1},{-1,-1}}, i -> R_i))
assert (euler (C2*O1) == sum({{1,1},{-1,1},{1,-1},{-1,-1},{0,0}}, i -> R_i))
Y = generalizedFlagVariety("C",2,{1},R)
Z = generalizedFlagVariety("C",2,{1,2},R)
f = flagMap(Z,Y)
g = flagMap(Z,X)
assert (euler ampleKClass Y == R_0 + R_1 + R_0^(-1) + R_1^(-1))
dualTautSub = makeKClass(X,apply(X.points, p -> (
	    l := setIndicator(first p,2);
	    R_0^(l_0) + R_1^(l_1)
	    )
	))
assert (dualTautSub === (pushforward g)(pullback f)(ampleKClass Y))
///


----------------------------------------------------------------
-- Type B tests: OG(3,7) and OGFl(2,3;7) and OGFl(1,2,3;7)
----------------------------------------------------------------
TEST ///
-- Checking points
R = makeCharacterRing 3
X = generalizedFlagVariety("B",3,{3,3},R)
Y = generalizedFlagVariety("B",3,{2,3,3},R)
Z = generalizedFlagVariety("B",3,{1,2,3,3},R)
assert(set X.points === set {{set {0, 1, 2}}, {set {0, 1, "2*"}}, {set {0, "1*", 2}}, {set {0, "1*", "2*"}}, 
	{set {"0*", 1, 2}}, {set {"0*", 1, "2*"}}, {set {"0*", "1*", 2}}, {set {"0*", "1*", "2*"}}})
X' = generalizedFlagVariety("C",3,{3,3},R)
Y' = generalizedFlagVariety("C",3,{2,3,3},R)
Z' = generalizedFlagVariety("C",3,{1,2,3,3},R)
assert(set X'.points === set X.points and set Y'.points === set Y.points and set Z'.points === set Z.points)

-- Checking charts
peek X;
HX = X.charts;
HX' = X'.charts;
missingCharX = apply(X.points, v -> toList(set HX'#v - set HX#v));
assert all(missingCharX, v -> #v == 3 and all(v, w -> max w ==2 or min w == -2))

HY = Y.charts;
HY' = Y'.charts;
missingCharY = apply(Y.points, v -> toList(set HY'#v - set HY#v));
missingCharY#0
assert all(missingCharY, v -> #v == 3 and all(v, w -> max w ==2 or min w == -2))

HZ = Z.charts;
HZ' = Z'.charts;
missingCharZ = apply(Z.points, v -> toList(set HZ'#v - set HZ#v));
assert all(missingCharZ, v -> #v == 3 and all(v, w -> max w ==2 or min w == -2))

-- Checking maps
f = flagMap(Z,Y)
g = flagMap(Y,X)
h = flagMap(Z,X)
assert(h === compose(g,f))

-- Checking TOrbClosure
M = matrix{{-2,0,0,1,0,0,2},{0,-2,0,2,1,0,2},{0,0,-2,2,2,1,2}}
-- Verifying M is isotropic
A = matrix{{-2,0,0},{0,-2,0},{0,0,-2}};
B = matrix{{1,0,0},{2,1,0},{2,2,1}};
D = matrix{{2},{2},{2}};
assert(A* transpose(B)  + B *transpose(A) + D*transpose(D) == 0)


time C1 = orbitClosure(X,M);
time C1'= orbitClosure(X,M, RREFMethod => true);
assert isWellDefined C1
assert(C1 === C1')

time C2 = orbitClosure(Y,M)
time C2'= orbitClosure(Y,M, RREFMethod => true)
assert isWellDefined C2
assert(C2 === C2')

time C3 = orbitClosure(Z,M)
time C3'= orbitClosure(Z,M, RREFMethod => true)
assert isWellDefined C3
assert(C3 === C3')
///



----------------------------------------
-- Type D tests: OG(4,8) 
----------------------------------------
TEST ///
-- Checking points
R = makeCharacterRing 4
X1 = generalizedFlagVariety("D",4,{4,4},R)
X2 = generalizedFlagVariety("D",4,{3,3},R)
Z = generalizedFlagVariety("C",4,{4},R)

PointSet = set{
    {set{0,1,2,3}}, {set{0,1,2,"3*"}}, {set{0,1,"2*",3}}, {set{0,1,"2*","3*"}},
    {set{"0*",1,2,3}}, {set{"0*",1,2,"3*"}}, {set{"0*",1,"2*",3}}, {set{"0*",1,"2*","3*"}},
    {set{0,"1*",2,3}}, {set{0,"1*",2,"3*"}}, {set{0,"1*","2*",3}}, {set{0,"1*","2*","3*"}},
    {set{"0*","1*",2,3}}, {set{"0*","1*",2,"3*"}}, {set{"0*","1*","2*",3}}, {set{"0*","1*","2*","3*"}}}
assert(set X1.points + set X2.points === PointSet)

assert(set X1.points + set X2.points === set Z.points)


-- Checking charts
HX1 = X1.charts;
HX2 = X2.charts;
HZ = Z.charts;
missingCharX = (apply(X1.points, v -> toList(set HZ#v - set HX1#v)) |apply(X2.points, v -> toList(set HZ#v - set HX2#v)))
assert all(missingCharX, v -> #v == 4 and all(v, w -> max w ==2 or min w == -2))


-- Checking TOrbClosure
-- Example that is 0 on X2
A = matrix{{1,3,-2,-1/4},{-1,-1,19,-61/4},{0,1,19,-73/4},{2,0,22,-89/4}}
B = matrix(QQ,{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16}})
M = A | B
-- Verifying M is isotropic
assert(A* transpose(B)  + B *transpose(A) == 0)

time C1 = orbitClosure(X1,M);
time C2 = orbitClosure(X2,M);
assert(isWellDefined C1 and isWellDefined C2)
assert all(X2.points, v -> C2.KPolynomials#v == 0)

-- Example that is 0 on X1
A = matrix{{1,0,0,1},{0,1,0,1},{0,0,1,1},{0,0,0,0}}
B = matrix{{0,-1/9,-6/5,0},{1/9,0,-6/5,0},{6/5,6/5,0,0},{-1,-1,-1,1}}
N = A|B
-- Verifying M is isotropic
assert(A* transpose(B)  + B *transpose(A) == 0)

time D1 = orbitClosure(X1,N);
time D2 = orbitClosure(X2,N);
assert(isWellDefined D1 and isWellDefined D2)
assert all(X1.points, v -> D1.KPolynomials#v == 0)
///


----------------------------------
-- Type D Test: OGFl(1,4;8)
----------------------------------
TEST ///
A = matrix{{1,3,-2,-1/4},{-1,-1,19,-61/4},{0,1,19,-73/4},{2,0,22,-89/4}}
B = matrix(QQ,{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16}})
M = A | B
R = makeCharacterRing 4
Y1 = generalizedFlagVariety("D",4,{3,4,3,3},R)
Y2 = generalizedFlagVariety("D",4,{3,4,4,4},R)
Z = generalizedFlagVariety("D",4,{3,4},R)
assert(set ((Y1.points)/first) === set ((Z.points)/first))
assert(set ((Y2.points)/first) === set ((Z.points)/first))

time C1 = orbitClosure(Y1,M);
time C2 = orbitClosure(Y2,M);
assert all(Y1.points, v -> C1.KPolynomials#v == 0)
assert isWellDefined C2

f = flagMap(Y1,Z)
assert (trivialKClass Z === (pushforward f)(trivialKClass Y1) )
///



-------------------------
-- Low degree isogenies
-------------------------
TEST ///
-- OG(1,3), SGr(1,2) and P1^* = Gr(1,2)
X1 = generalizedFlagVariety("B",1,{1,1})
X2 = generalizedFlagVariety("C",1,{1})
X3 = generalizedFlagVariety("A",1,{1})
assert(#X1.points === #X2.points)
assert(apply(X1.points, v-> #(X1.charts)#v) === apply(X2.points, v-> #(X2.charts)#v))
assert(apply(X1.points, v-> #(X1.charts)#v) === apply(X3.points, v-> #(X3.charts)#v))


-- SOGr(3,6), SOGr(2,6), and P3^* = Gr(3,4)
X1 = generalizedFlagVariety("D",3,{3,3})
X2 = generalizedFlagVariety("D",3,{2,2})
X3 = generalizedFlagVariety("A",3,{3})
assert(#X1.points === #X2.points and #X2.points === #X3.points)
assert(apply(X1.points, v-> #(X1.charts)#v) == apply(X2.points, v-> #(X2.charts)#v) and
    apply(X1.points, v-> #(X1.charts)#v) === apply(X3.points, v-> #(X3.charts)#v))

--SOGr(1,6) and Gr(2,4)
X1 = generalizedFlagVariety("D",3,{1})
X2 = generalizedFlagVariety("A",3,{2})
assert(#X1.points === #X2.points)
assert(apply(X1.points, v-> #(X1.charts)#v) === apply(X2.points, v-> #(X2.charts)#v))
///

-------------------------------------
-- Boundary cases for TOrbitClosure
-------------------------------------
TEST ///
M = matrix(QQ,{{1,0,0,0},{0,1,0,0}})
R = makeCharacterRing 4
X = generalizedFlagVariety("A",3,{2},R)
C = orbitClosure(X,M)
peek C
assert all(keys C.KPolynomials, v -> (
	if not v === {set{0,1}} then C.KPolynomials#v == 0 else 
	sub(C.KPolynomials#v,R)  === product apply({0,1} ** {2,3}, v -> 1- R_(v_0)^(-1)*R_(v_1)) 
	)
    )
///

-----------------------
-- A toric variety test
-----------------------
TEST ///
X = kleinschmidt(2,{2})
Y = makeGKMVariety X
assert (normalToricVariety Y === X)
R = Y.characterRing
assert (2 == numgens R)
G = momentGraph Y
assert all(keys G.edges, e -> 0 == (matrix{(rays X)_(first elements (set e_0 * set e_1))} * transpose matrix{G.edges#e}))
antiK = - toricDivisor X
TantiK = makeKClass(Y,antiK)
ltsPts = {{1,1},{0,-1},{0,0},{0,1},{-1,-3},{-1,-2},{-1,-1},{-1,0},{-1,1}}
assert (sum(ltsPts, i -> R_i) == euler TantiK)
///


