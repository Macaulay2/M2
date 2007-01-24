-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules";

----------------------- TESTS for PolySols -------------------------

-- Test 1: Simple example
x = symbol x; Dx = symbol Dx;
W = QQ[x,Dx,WeylAlgebra => {x=>Dx}];
I = ideal(x*Dx^4);
ans = ideal(1, x, x^2, x^3);
assert( ans == substitute(ideal PolySols I, W) );
assert( ans == substitute(ideal PolySols (I, Alg => Duality), W) );

-- Test 2: Polynomial solutions of an Appell F1
I = AppellF1({-1,5,4,-2}, Vars => Local);
I1 = ideal PolySols I;
I2 = ideal PolySols (I, Alg => Duality);
assert(I1 == substitute(I2, ring I1));

--------------------- TESTS for RatSols -----------------------

-- Test 3: 
x = symbol x; Dx = symbol Dx;
y = symbol y; Dy = symbol Dy;
W = QQ[x,y,Dx,Dy,WeylAlgebra =>{x=>Dx, y=>Dy}];
tx = x*Dx;
ty = y*Dy;
I = ideal(tx*(tx+ty)-x*(tx+ty+3)*(tx-1),
     ty*(tx+ty)-y*(tx+ty+3)*(ty+1));
assert(#RatSols(I, y, {10,1}) == 1);
assert(#RatSols(I, y-1, {10,1}) == 1);


---------------------- TESTS forDHom and DExt ------------------------
-- Example 1: Simple ODE examples
x = symbol x; dx = symbol dx;
W = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = cokernel matrix{{x*(dx-1)^2}}
N = cokernel matrix{{x*dx*(dx-1)}}
B = DHom(N,M);
assert( (matrix{{B#0_(0,0)}})*(relations N)%(relations M) == 0 )
BE = DExt(M,N);
BE' = DExt(N,M);
assert( all (keys BE, i -> BE#i == BE'#i) );

--Example 2: small GKZ -- takes a while
A = matrix{{1,2}};
I = gkz(A, {2});
J = substitute(gkz(A, {1}), vars ring I);
B = DHom(I,J);
assert( (matrix{{B#0_(0,0)}})*(gens I)%(gens J) == 0 )
