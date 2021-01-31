-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules";

----------------- TESTS for Drestriction & Dintegration--------------------

--Example 1: Derived restriction of a GKZ hypergeometric system
I = gkz(matrix{{1,1,1},{0,1,3}}, {1/2,1/3});
W = ring I;

-- No integer roots in b-function
assert(DrestrictionIdeal(I, {1,3,5}) == ideal 1_QQ);
assert(Drestriction(0, I, {1,3,5}) == QQ^0);
Dr = Drestriction(I, {1,3,5});
assert(all(toList(0..3), i -> (Dr#i == QQ^0)));
Dc = DrestrictionComplex(I, {1,3,5});
assert(all(toList(0..3), i -> (Dc.dd#i == map QQ^0)) );

assert(DintegrationIdeal(I, {1,3,5}) == ideal 1_QQ);
assert(Dintegration(0, I, {1,3,5}) == QQ^0);
Dr = Dintegration(I, {1,3,5});
assert(all(toList(0..3), i -> (Dr#i == QQ^0)));
Dc = DintegrationComplex(I, {1,3,5});
assert(all(toList(0..3), i -> (Dc.dd#i == map QQ^0)) );


--Example 2: Derived restriction of a GKZ hypergeometric system
I = gkz(matrix{{1,2,4}},{1});
J = gkz(matrix{{1,3,4}},{-3});

F1 = DrestrictionIdeal(I, {1,5,0});
F2 = DrestrictionIdeal(I, {25,6,0});
assert(F1 == substitute(F2, ring F1));

F1 = Drestriction(I, {1,5,0});
F2 = Drestriction(I, {1,6,0});
assert(all (toList(0..2), i -> (
	       relations F1#i == substitute(relations F2#i, ring relations F1#i)) ) );


-- Example 3: Restriction and Integration of a rational function
y = symbol y; t = symbol t;
Dy = symbol Dy; Dt = symbol Dt;
W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}];
I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2); -- annihilator of 1/(t^2-y)
resI = DrestrictionIdeal(I, {1,0});
assert(resI == substitute(ideal(t*Dt + 2), ring resI));
assert(DintegrationIdeal(I, {1,4}) == DintegrationIdeal(I, {3,1}));

--Example 4: Derived integration of a GKZ hypergeometric system
I = gkz(matrix{{1,1,1},{0,1,3}}, {-3,-2});
W = ring I;
F = DintegrationAll(I, {1,3,5});
FV = F#VResolution;
FC = F#GenCycles;
assert all(toList(0..3), i -> (
	  apps = Dtransposition ((FV.dd#i)*(FC#i));
	  if (apps == 0) then true
	  else (apps % directSum apply(toList(1..rank target apps), 
		    j -> matrix{W.dpairVars#1}) == 0) ));

--2015-06: Uli's bug (rewritten by Avi in 2019-07 to remove dependence on gkz)
W = QQ[x_1..x_3, D_1..D_3, WeylAlgebra => {x_1=>D_1, x_2=>D_2, x_3=>D_3}];
M = cokernel(map(W^{{3}, {3}},W^6,{{x_2^3-x_1^2*x_3, -x_1*D_1-x_2*D_2-x_3*D_3-3, -x_2*D_2-3*x_3*D_3-4, 0, 0, 0}, {0, 0, 0, x_2^3-x_1^2*x_3, -x_1*D_1-x_2*D_2-x_3*D_3-4, -x_2*D_2-3*x_3*D_3-6}}));
RM = Drestriction(M,{0,1,0});
assert( toString RM#0 == "cokernel matrix {{x_3*D_3+3, x_1*D_1+4, x_1^4}}" );
assert( toString RM#1 == "cokernel matrix {{x_1, x_3*D_3+1}}" );
