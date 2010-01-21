restart
debug loadPackage "Dmodules"

--R = QQ[x,y];
--a = ideal {x^2-y^3}; 
--a = ideal {x*y*(x+y)*(x+2*y)}; 
--a = ideal {x*y*(x+y)*(x+2*y)*(x+3*y)};

R = QQ[x_1..x_3];
a = ideal {x_2^2-x_1*x_3, x_1^3-x_3^2}; 
a = ideal {x_1^3-x_2^2, x_2^3-x_3^2};
a = ideal {x_1^4-x_2^3, x_3^2-x_1*x_2^2};
a = ideal {x_1,x_2};

aS = analyticSpread a
hasRationalSing a_*

-- candidates for jumping numbers below the analytic spread
lowJumps = select(sort(bFunctionRoots generalB(a_*, 1_R) / minus), c->c<aS)
mI = multiplierIdeal(a,lowJumps)
all(#mI, i->all((mI#i)_*,g->isInMultiplierIdeal(g,a,lowJumps#i)))


------
------
R = QQ[x,y,z];
W = makeWeylAlgebra R;
F = {-y^3*z^3+y^5+x^2*z^3-x^2*y^2}; --intersect( ideal(x^2-y^3),ideal(y^2-z^3) )
b = {1_W,x} / (g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
b = {1_W,x} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,i->sub(i,R)); --lct(ideal xF) --is 19/30
time multiplierIdeal(ideal xF,19/30)


-----------------------------------------------
-------------Examples from [ELSV]:-------------
-----------------------------------------------
R = QQ[x,y];
W = makeWeylAlgebra R;
F = {x^5+y^4+x^3*y^2}; --Example 2.5 (not all roots are JCs)
b = {1_W,x,5*x^4+3*x^2*y^2,4*y^3+2*x^3*y,x*y,x-y} / (g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
b = {1_W,x,5*x^4+3*x^2*y^2,4*y^3+2*x^3*y,x*y,x-y} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,i->sub(i,R)); --lct(ideal xF) -- is ?
--time multiplierIdeal(ideal xF,)
--***also compute the q_i's for this one!***

-----------------------------------------------
------Example from one of Saito's papers:------
-----------------------------------------------
R = QQ[x,y];
W = makeWeylAlgebra R;
F = {(x^2-y^2)*(x^2-1)*(y^2-1)};
b = {1_W,x,x*y,x-y} / (g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
b = {1_W,x,x*y,x-y} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,i->sub(i,R)); --lct(ideal xF) -- is ?
--time multiplierIdeal(ideal xF,)

-----------------------------------------------
-------Examples from Zach's suggestions:-------
-----------------------------------------------

---- Line arrangements in \CC^3 (from Section 7 of arXiv:0508308v1):

--The first two are 3 noncollinear/collinear points in \PP^2:
restart
debug loadPackage "Dmodules"
R = QQ[x,y,z];
---I = intersect(ideal(x-1,y-1,z-1),ideal(x-3/2,y-5/2,z-1),ideal(x-11,y-13/5,z)); --3 noncollinear points
--F = flatten entries mingens I;
--I = intersect(ideal(x-z,y-z),ideal(3*x-2*z,5*y-2*z),ideal(55*y-13*x,z));
I = intersect(ideal(x-z,y-z),ideal(3*x-z,y-2*z),ideal(5*y-x,z)); --better
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
time print factorBFunction generalB (F,1_R,Strategy=>StarIdeal)
lct ideal F --Expect: lct = 3/2
multiplierIdeal(ideal F, 3/2)

R = QQ[x,y,z];
--I = intersect(ideal(x-1,y,z),ideal(x-2,y-1,z-1),ideal(x-3,y-2,z-2)); --3 collinear points
--F = flatten entries mingens I;
I = intersect(ideal(y,z),ideal(x-2*z,y-z),ideal(2*x-3*z,y-z));
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
time print factorBFunction generalB (F,1_R,Strategy=>StarIdeal)
lct ideal F --Expect: lct = 5/3
multiplierIdeal(ideal F, 5/3)


--The next two are 6 points in general position vs. 6 general points on a conic in \PP^2:
--6 points in general position in \PP^2: 
R = QQ[a,b,c];
small = 3;
setRandomSeed 1;
while true do (
     x = apply(6, i -> random small - small//2);
     y = apply(6, i -> random small - small//2);
     z = apply(6, i -> random 2 + 1);
     --z = toList (6:1);
     xym = matrix {x, y, z};
     I = intersect (entries transpose xym /(l -> ideal {l#2*a-l#0*c, l#2*b-l#1*c}));
     if multiplicity I == 15 and all(z,zz->zz!=0) and all(flatten entries gens minors(3, xym), i -> (i!=0)) then break;
     );
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
time print factorBFunction generalB (F,1_R,Strategy=>StarIdeal)
lct ideal F --Expect: lct = 1

--6 general points on a^2-b*c in \PP^2: 
R = QQ[a,b,c];
--x = take(unique apply(101, i -> random (coefficientRing R)), 6);
x = { -2,-1,0,1,2,3 };
y = x/(i -> i^2);
xym = matrix {x, y, toList (6:1)};
all(flatten entries gens minors(3, xym), i -> (i!=0))
I = intersect (entries transpose xym /(l -> ideal {a-l#0*c, b-l#1*c}));
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
///
       2     3      4      5      7
(s + 2) (s + -)(s + -)(s + -)(s + -)
             2      3      3      3
     -- used 18.717 seconds
///
time print factorBFunction generalB (F,1_W,Strategy=>StarIdeal)
///
       2     3      4      5      7
(s + 2) (s + -)(s + -)(s + -)(s + -)
             2      3      3      3
     -- used 71.3508 seconds
///
--Note: lct = 4/3

--Zach does not know the multiplier ideals for this one.
--11 general points on a^3-b*c^2 in \PP^2: --(This code is courtesy of Manoj Kummini.)
R = QQ[a,b,c];
--x = take(unique apply(101, i -> random (coefficientRing R)), 11);
x = toList(-5..5);
y = x/(i -> i^3);
xym = matrix {x, y, toList (11:1)};
all(flatten entries gens minors(3, xym), i -> (i!=0))
I = intersect (entries transpose xym /(l -> ideal {a-l#0*c, b-l#1*c}));
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)

---- Monomial curves:
--A monomial curve in \PP^3: 
n = 4;
R = QQ[x_1..x_4];
A = matrix{{1,1,1,1},{0,1,3,4}}; 
H = gkz(A,{0,0})
phi = map(R,ring H, {0,0,0,0,x_1..x_4});
xIA = ideal mingens phi(H);
F = toList apply(numColumns(gens xIA),i-> (gens xIA)_i_0);
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
--b = {1_W,x_1,F#0,x_1*x_4,x_1*x_2} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
--use roots to decide which multiplier ideals to compute
analyticSpread (ideal dF)
--multiplierIdeal(ideal F,1_QQ)

--A monomial curve in \PP^4: 
A = matrix{{1,1,1,1,1},{0,1,0,1,0},{0,0,1,1,-2}}; 
H = gkz(A,{0,0,0})
apply(1..5,i-> D_i = (gens(ring H))#(i+4) )
IA = ideal {D_2*D_3-D_1*D_4, D_1*D_2^2-D_4^2*D_5, D_1^2*D_2-D_3*D_4*D_5, D_1^3-D_3^2*D_5};
R = QQ[x_1..x_5];
phi = map(R,ring H, {0,0,0,0,0,x_1..x_5});
xIA = phi(IA); --toString gens xIA
use R;
F = {x_2*x_3-x_1*x_4, x_1*x_2^2-x_4^2*x_5, x_1^2*x_2-x_3*x_4*x_5, x_1^3-x_3^2*x_5};
multiplierIdeal(ideal F,1_QQ)


---- The "big diagonal" of (\CC^d)^n: 
--Note: It would be great if we had a way to mod out by the "small diagonal" ideal(x_1-x_3,x_1-x_5,x_2-x_4,x_2-x_6).

--n=3, d=2 --This is known (and easily computable by hand).
R = QQ[x_1..x_6];
W = makeWeylAlgebra R; 
bigDiag = mingens intersect( ideal(x_1-x_3,x_2-x_4), ideal(x_1-x_5,x_2-x_6), ideal(x_3-x_5,x_4-x_6) );
F = entries bigDiag_0
--time print factorBFunction generalB (F,1_W)
-----(Use JC to decide which multiplier ideals to compute.)
--time print factorBFunction generalB (F,1_W,Exponent=>2)
--b = {1_W,x_1,x_1^2,x_1*x_2,x_1*x_2*x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,h->sub(h,R))
analyticSpread(ideal xF)
multiplierIdeal( ideal xF, 1_QQ )

--n=4, d=2
R = QQ[x_1..x_4,y_1..y_4];
W = makeWeylAlgebra R; 
bigDiag = mingens intersect( ideal(x_1-y_1,x_2-y_2), ideal(x_1-x_3,y_1-y_3), ideal(x_1-x_4,y_1-y_4), ideal(x_2-x_3,y_2-y_3), ideal(x_2-x_4,y_2-y_4), ideal(x_3-x_4,y_3-y_4 ));
F = entries bigDiag_0;
time print factorBFunction generalB (F,1_W) 
--time print factorBFunction generalB (F,1_W,Exponent=>2)
--b = {1_W,x_1,x_1^2,x_1*x_2,x_1*x_2*x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,h->sub(h,R)) --analyticSpread(ideal xF) --1
k = binomial(4,2); multiplierIdeal( ideal xF, 1/k ) --This tests a conjecture of Kyungyong.
--multiplierIdeal( ideal xF, 1_QQ )

--Is there any hope of computing n=5?
n=5; d=2;
R = QQ[x_1..x_5,y_1..y_5];
W = makeWeylAlgebra R; 
bigDiag = mingens intersect (flatten toList(apply(1..n,i-> toList(apply(1..(i-1),j-> ideal(x_i-x_j, y_i-y_j))))))
F = entries bigDiag_0;
time print factorBFunction generalB (F,1_W) --****Note****: If we can compute this, many people would be very happy!
--time print factorBFunction generalB (F,1_W,Exponent=>2)
--b = {1_W,x_1,x_1^2,x_1*x_2,x_1*x_2*x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,h->sub(h,R)) --analyticSpread(ideal xF) --1
k = binomial(4,2); multiplierIdeal( ideal xF, 1/k ) --This tests a conjecture of Kyungyong.
--multiplierIdeal( ideal xF, 1_QQ )

