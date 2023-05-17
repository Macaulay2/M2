path = join(path, {"../"})
load "Dloadfile.m2"
Dtrace 4

----------------------- EXAMPLES for PolySols, PolyExt -------------------------

-- Example 0: Simple example
W = QQ[x,Dx,WeylAlgebra => {x=>Dx}]
I = ideal(x*Dx^4)
PolySols I
PolyExt I

-- Example 1: Polynomial solutions of a GKZ
I = gkz(matrix{{1,3}}, {9}, Vars => Local)
PolySols I
PolySols (I, Alg => Duality)
PolyExt I

-- Example 2: Polynomial solutions of an Appell F1
I = AppellF1({-1,5,4,-2})
PolySols I
PolySols (I, Alg => Duality)
PolyExt I

--------------------- EXAMPLES for RatSols and RatExt -----------------------

-- Example 1: Rational solutions of an Appell F1
I = AppellF1({2,-3,-2,5})
RatSols (I, x, {1,8})
RatlExt (I, x)
RatlExt (I, y-1)

-- Example 2: 
W = QQ[x,y,Dx,Dy,WeylAlgebra =>{x=>Dx, y=>Dy}]
tx = x*Dx
ty = y*Dy
I = ideal(tx*(tx+ty)-x*(tx+ty+3)*(tx-1),
     ty*(tx+ty)-y*(tx+ty+3)*(ty+1))
RatSols(I, y, {10,1})
RatSols(I, y-1, {10,1})

holonomicRank I
(mingens singLocus I)_(0,0) == x*y*(x-1)*(y-1)*(x-y)
RatSols(I, {x,y,x-1,y-1,x-y}, {10,1})

------------------------- EXAMPLES for DHom ---------------------------
-- Example 1: Simple ODE examples
W = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = cokernel matrix{{x*(dx-1)^2}}
N = cokernel matrix{{x*dx*(dx-1)}}
DHom(M,N)
DHom(N,M)
DExt(M,N)
DExt(N,M)

-- Example 2: small GKZ
A = matrix{{1,2}}
I = gkz(A, {2})
J = substitute(gkz(A, {1}), ring I)
B = DHom(I,J)
(matrix{{B#0_(0,0)}})*(gens I)%(gens J)

-- Fool-proof test: nonholomonic!
W = QQ[x, dx, y, dy, WeylAlgebra => {x=>dx, y=>dy}]
M = cokernel matrix{{dx}}
N = cokernel matrix{{dx,dy}}
DHom(M,N)
DHom(N,M)



