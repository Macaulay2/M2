-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules";
--Things needing tests: Dprune (waiting for documentation)

methods createCommAlgebra

-- Boundary cases
x = symbol x; Dx = symbol Dx;
W = QQ[x,Dx,WeylAlgebra => {x=>Dx}];
I0 = ideal (0_W);
I1 = ideal (1_W);
assert (Ddim I0 == 2);
assert (Ddim I1 == -1);
assert (holonomicRank I0 == infinity);
assert (holonomicRank I1 ==  0);
assert (singLocus I0 == 0);
assert (singLocus I1 == ideal(1_W));
assert (charIdeal I0 == 0);
assert (chI = charIdeal I1; chI == ideal(1_(ring chI)) );

-- Dbasics basics
R = QQ[r,s];
A = makeWeylAlgebra R;
B = QQ[r,s,dr,ds,WeylAlgebra => {r=>dr,s=>ds}];
assert (describe A===describe B);

--all things Fourier
D = QQ[u,v,Du,Dv, WeylAlgebra => {u => Du, v => Dv}];
L = u^3 + u*v*Dv + 4*u*Du^3*Dv;
assert (Fourier L ==  -Du^3 + Du*Dv*v - 4*Du*u^3*v);
I = ideal (u*v^2, u*Du, v*Du+Dv^2);
assert (Fourier I == ideal (-Du*Dv^2, -Du*u, -Dv*u+v^2));
M = matrix{{Du, v},{Dv, u^2}};
assert (Fourier M == matrix{{u, -Dv}, {v, Du^2}});
assert (entries Fourier M == entries matrix{{u, -Dv}, {v, Du^2}});
assert (Fourier coker M == coker matrix{{u, -Dv}, {v, Du^2}});
C = res Fourier coker M;
assert (rank C_0==2);
J = ideal (u*Du+Dv);
assert (FourierInverse J == ideal(-Du*u-v));
assert (FourierInverse Dv == -v);
assert (FourierInverse coker M == coker FourierInverse M);

-- Boundary cases for module scripts
M = directSum(cokernel gens I0, cokernel gens I1);
N = directSum(cokernel gens I1, cokernel gens I1);
assert (Ddim M == 2);
assert (Ddim N == -1);
assert (holonomicRank M == infinity);
assert (holonomicRank N == 0);
assert (singLocus M == 0);
assert (singLocus N == ideal 1_W);
assert (charIdeal M == 0);
assert (chN = charIdeal N; chN == ideal(1_(ring chN)) );

-- Properties of AppellF1
I = AppellF1 ({2,4,-1,3/2});
J = substitute (AppellF1 ({3,-1,7/3,-5}), vars ring I);
K = directSum(cokernel gens I, cokernel gens J);
assert (Ddim I == Ddim J);
assert (holonomicRank I == holonomicRank J);
assert (singLocus I == singLocus J);
assert (charIdeal I == charIdeal J);
assert (isHolonomic K);
assert (holonomicRank K == holonomicRank I + holonomicRank J);
assert (singLocus K == singLocus I);

w' = {0,0,1,1}
assert (inw(I,w') == inw(J,w'));

-- Ranks of gkz systems
A = matrix{{1,1,1,1},{0,1,3,4}};
assert (holonomicRank(gkz(A, {1,3})) == 4);
assert (holonomicRank(gkz(A, {1,2})) == 5);
assert (isHolonomic gkz(A,{-1/2, 5/3}));

-- Polynomial and Rational annihilators
W = QQ[u,v,Du,Dv, WeylAlgebra => {u => Du, v => Dv}];
f = u^5 - v^2;
I = PolyAnn f;
J = RatAnn f;
K = RatAnn (u-v^2, f);
L = directSum (W^1/I, W^1/J);
assert ( isHolonomic I );
assert ( isHolonomic J );
assert ( isHolonomic K );
assert ( isHolonomic L );
assert ( holonomicRank I == 1 );
assert ( holonomicRank J == 1 );
assert ( holonomicRank K == 1 );
assert ( holonomicRank L == 2 );
assert ( singLocus I == ideal(1_W) );
assert ( singLocus J == ideal(f) );
assert ( singLocus K == ideal(f) );
assert ( singLocus L == ideal(f) );

-- DTransposition performs the standard involution of the Weyl algebra which sends x^aDx^b to (-Dx)^bx^a
D = QQ[u,v,Du,Dv, WeylAlgebra => {u => Du, v => Dv}];
assert (Dtransposition (u*Du) ==-Du*u);
assert (Dtransposition ideal (u*Dv^2+Du^2*Dv) == ideal (u*Dv^2-Du^2*Dv));
assert (entries Dtransposition matrix {{u*Du, v}, {v*Dv^2, u^2}} == entries matrix {{-Du*u, v}, {Dv^2*v, u^2}});
C1 = Dtransposition res ideal(u*Du);
C2 = res ideal(-Du*u);
assert (C1_1==C2_1);

-- extract polynomial ring of ordinary variables and, separately, of differentials from Weyl algebras
D = QQ[u,v,Du,Dv, WeylAlgebra => {u => Du, v => Dv}, Degrees => {2,4,-3,9}];
assert(describe extractVarsAlgebra D === describe(QQ[u,v, Degrees => {2,4}]));
assert(describe extractDiffsAlgebra D === describe(QQ[Du,Dv, Degrees => {-3,9}]));

