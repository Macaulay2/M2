-- 0. Service routines 
R = QQ[x,y,z]
W = makeWA R

-- fill an m-by-n  matrix with elements of W 
-- that have monomials up to degree d  
randomMatrix = (m,n,d,W) -> (
    RW := (coefficientRing W) (monoid [gens W]); 
    toW := map(W,RW,gens W);
    matrix apply(m, i->apply(n,j->(
		sum(d+1, e->toW random(e,RW))
		)))
    )
M = randomMatrix(2,3,1,W)
M = randomMatrix(3,2,1,W)
Dprune M == prune M

-- 1. Basic invariants
-- GKZ of the twisted quartic
A = matrix{{1,1,1,1},{0,1,3,4}}
b = {1,2}
I = gkz(A,b,Vars=>Local)

Ddim I -- check it's holoomic
holonomicRank I  -- holonomic rank
singLocus I -- singular locus
charIdeal I -- characteristic ideal

-- Appell F1
I = AppellF1 ({2,4,-1,3/2}, Vars=>Local)

holonomicRank I
singLocus I
charIdeal I

-- Polynomial annihilator
W = QQ[x,y,z,Dx,Dy,Dz, WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz}]
f = x^3-y^2*z^2
I = PolyAnn f

holonomicRank I
singLocus I
charIdeal I

-- Rational annihilator
W = QQ[x,y,z,Dx,Dy,Dz, WeylAlgebra => {x=>Dx, y=>Dy, z=>Dz}]
f = x^3-y^2*z^2
I = RatAnn f

holonomicRank I
singLocus I
charIdeal I

f = x^2-y^3
g = y-2*z
J = RatAnn (g,f)

holonomicRank J
singLocus J
charIdeal J

-- Module routines
A = matrix{{1,1,1},{0,1,3}}
A' = matrix{{1,1,1},{0,1,4}}
b = {1,2}
b' = {2,3}
I = gkz(A,b,Vars=>Local)
I' = substitute(gkz(A',b',Vars=>Local), ring I)
M = directSum(cokernel gens I, cokernel gens I')

Ddim M
holonomicRank M
singLocus M
charIdeal M

-- 2. initial ideals and gb wrt weight vectors
A = matrix{{1,1,1},{0,2,3}}
b = {1,5}
I = gkz(A,b,Vars=>Local)

-- weight vector of the form (-u,u)
w1 = {-1,-2,-3,1,2,3}
inw(I, w1)
gbw(I, w1)

-- weight vector (u,v) with u+v > 0
w2 = {0,1,2,3,4,5}
inw(I, w2)
gbw(I, w2)

-- weight vector (u,v) with some comp's of u+v > 0, others equal to 0.
w3 = {1,-3,7,-1,4,-5}
inw(I, w3)
gbw(I, w3)

-- matrix versions
m = directSum(gens I, gens I)
w1 = {-1,-2,-3,1,2,3}
w2 = {0,1,2,3,4,5}
inw(m, w1)
gbw(m, w1)
inw(m, w2)
gbw(m, w2)

W = QQ[x, y, Dx, Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
m = matrix{{x*Dx+y*Dy+x^3},{3+y^2+y*Dx+x^2*Dy}}
inw(m, {-1,-2,1,2})
gbw(m, {-1,-2,1,2})
inw(m, {0,0,1,1})
gbw(m, {0,0,1,1})
