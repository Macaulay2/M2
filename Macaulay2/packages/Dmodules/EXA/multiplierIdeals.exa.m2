restart
debug loadPackage "Dmodules"

--R = QQ[x,y];
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


-----------------------------------------------
----Examples related to Zach's suggestions:----
-----------------------------------------------
---- A monomial curve in \PP^3: 
n = 4;
R = QQ[x_1..x_4];
A = matrix{{1,1,1,1},{0,1,3,4}}; 
H = gkz(A,{0,0})
phi = map(R,ring H, {0,0,0,0,x_1..x_4});
xIA = ideal mingens phi(H);
F = toList apply(numColumns(gens xIA),i-> (gens xIA)_i_0);
W = makeWeylAlgebra R;
dF = toList apply(length F,i->sub(F#i,W))
time print factorBFunction generalB (dF,1_W,Strategy=>StarIdeal)
--b = {1_W,x_1,F#0,x_1*x_4,x_1*x_2} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
--use roots to decide which multiplier ideals to compute
analyticSpread (ideal dF)
--multiplierIdeal(ideal F,1_QQ)

-----------
---- A monomial curve in \PP^4: 
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


------------
---- The "big diagonal" of (\CC^d)^n: 
---- Note: If'd be great if we had a way to mod out by the "small diagonal" ideal(x_1-x_3,x_1-x_5,x_2-x_4,x_2-x_6).
R = QQ[x_1..x_6];
W = makeWeylAlgebra R; 
--bigDiag = toString mingens intersect( ideal(x_1-x_3,x_2-x_4), ideal(x_1-x_5,x_2-x_6), ideal(x_3-x_5,x_4-x_6) );
F = {x_2*x_3-x_1*x_4-x_2*x_5+x_4*x_5+x_1*x_6-x_3*x_6, x_2^2*x_4-x_2*x_4^2-x_2^2*x_6+x_4^2*x_6+x_2*x_6^2-x_4*x_6^2, x_1*x_2*x_4-x_1*x_4^2-x_2*x_4*x_5+x_4^2*x_5-x_1*x_2*x_6+x_1*x_4*x_6+x_2*x_5*x_6-x_4*x_5*x_6, x_1^2*x_4-x_1*x_3*x_4-x_1*x_4*x_5+x_3*x_4*x_5-x_1^2*x_6+x_1*x_3*x_6+x_1*x_5*x_6-x_3*x_5*x_6, x_1^2*x_3-x_1*x_3^2-x_1^2*x_5+x_3^2*x_5+x_1*x_5^2-x_3*x_5^2}
--time print factorBFunction generalB (F,1_W)
-----(Use JC to decide which multiplier ideals to compute.)
--time print factorBFunction generalB (F,1_W,Exponent=>2)
--b = {1_W,x_1,x_1^2,x_1*x_2,x_1*x_2*x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,h->sub(h,R))
analyticSpread(ideal xF)
multiplierIdeal( ideal xF, 1_QQ )

------------
