-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

-- TESTS TO WRITE (exported symbols);
--    PolySols (Module, List)
--    PolySols (Module, List) .... with Alg => Duality

--    PolyExt Ideal
--    PolyExt Module
--    PolyExt (ZZ, Ideal)
--    PolyExt (ZZ, Module)

--    RatSols Ideal
--    RatSols (Ideal, List)
--    RatSols (Ideal, RingElement)
--    RatSols (Ideal, List, List)

--    RatExt Ideal
--    RatExt Module
--    RatExt (Ideal, RingElement)
--    RatExt (Module, RatExt)
--    RatExt (ZZ, Module)
--    RatExt (ZZ, Ideal)
--    RatExt (ZZ, Ideal, RingElement)
--    RatExt (ZZ, Module, RingElement)

--    DHom (Module, Module, List)

--    DExt (Module, Module, List)

--    ExternalProduct (Module, Module)
--    ExternalProduct (ChainComplex, ChainComplex)
--    ExternalProduct (Module, Module) .... with TwistMap => true
--    ExternalProduct (ChainComplex, ChainComplex) .... with TwistMap => true



-- TESTS TO WRITE (unexported symbols);
--    divideOutGCD RingElement
--    divideOutGCD Matrix

--    TwistOperator (Ideal, RingElement, ZZ)
--    TwistOperator (Ideal, List, List)
--    TwistOperator (RingElement, RingElement, ZZ)
--    TwistOperator (RingElement, List, List)


needsPackage "Dmodules";


----------------------- TESTS for ^ -----------------------

x = symbol x; y = symbol y; z = symbol z;
assert({x,y,z}^{2,3,4} == x^2*y^3*z^4);



----------------------- TESTS for PolySols -------------------------

-- Test 1: Simple example 
x = symbol x; Dx = symbol Dx;
W = QQ[x,Dx,WeylAlgebra => {x=>Dx}];
I = ideal(x*Dx^4);
ansGD = {1, x, x^2, x^3};
ansDuality = {-1, x, -x^2, x^3};
assert( ansGD == PolySols I / (f -> substitute(f, W)) );
assert( ansGD == PolySols comodule I / (f -> substitute(f, W)) );
assert( ansDuality == PolySols(I, Alg => Duality) / (f -> substitute(f, W)) );
assert( ansDuality == PolySols(comodule I, Alg => Duality) / (f -> substitute(f, W)) );

-- Test 2: Polynomial solutions of an Appell F1
I = AppellF1({-1,5,4,-2}, Vars => Local);
sols1 = PolySols I;
R = ring sols1#0;
sols2 = PolySols (I, Alg => Duality) / (f -> substitute(f, R));
solsMat = lift(last coefficients matrix{sols1 | sols2}, coefficientRing R)
sols1' = solsMat_{0..<#sols1};
sols2' = solsMat_{#sols1..< numColumns solsMat};
assert(image sols1' == image sols2');



--------------------- TESTS for PolyExt -----------------------


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
