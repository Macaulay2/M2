-------------------------EXAMPLES for bFunction(Module)
restart
load "Dloadfile.m2"

-- Example 1
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = cokernel matrix {{x^2, 0, 0}, {0, dx^3, 0}, {0, 0, x^3}}
factorBFunction bFunction(M, {1}, {0,0,0})
factorBFunction bFunction(M, {1}, {1,2,3})

-- Example 2
R = QQ[x, y, dx, dy, WeylAlgebra => {x=>dx, y=>dy}]
M = cokernel matrix {{x^2, 0, y^3, 0}, {0, x^2, 0, y^3}}
factorBFunction bFunction(M, {0,1}, {0,0})
factorBFunction bFunction(M, {1,0}, {0,0})

--Example 
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = cokernel matrix {{x^2, 0, 0}, {0, dx^3, 0}, {0, 0, x^3}}
bFunction(M, {1}, {2,0,1})
bFunction(M, {1}, {0,0,0})
factorBFunction oo

--Bug #1
restart
load "Dloadfile.m2"
W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
w = {1,1}
m = zeroize transpose matrix{{-Dy^2-Dy, -x*Dx-x*Dy-x+1, y*Dy^2+y*Dy-Dy-7},
     {-Dx+Dy, x-(5/4)*y, -1}, {0, -y, -4}, {-1, 0, -x}}
M = cokernel m

wt = {1,1}
shift = {0,0,0}

isHolonomic M
bFunction2(M, wt, shift) -- bug here
bFunction(M, wt, shift)


