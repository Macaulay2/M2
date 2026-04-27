-------------------------
-- Example R^3
-------------------------
restart
load "Dloadfile.m2"
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = matrix {{dx, 0, 0}, {0, dx, 0}, {0, 0, dx}}
setInfoLevel 1
h = makeCyclic M
factorBFunction bFunction(h.AnnG,{1})
factorBFunction bFunction(cokernel M, {1}, {0,1,2})

-----------------------------------
-- Example (D/I_1)oplus(D/I_2) 
--------------------------------
restart
load "Dloadfile.m2"
R = QQ[t, dt, x, dx, WeylAlgebra => {t=>dt, x=>dx}]
-- both I_1 and I_2 come from the computation of globalBFunction
-- the b-function of I_1 is supposed to have 0, 1/3, 2/3 as roots 
--   "     "     "   I_2   "     "     "     0, 1/2
-- thus the roots of the globalBFunction of the module should be 0,1/2,1/3,2/3 
-- (mod Z)  
M = matrix {
     {-x^3+t, 3*dt*x^2+dx, 0     , 0        },
     {0     , 0          , -x^2+t, 2*dt*x+dx}
     }
setInfoLevel 1
h = makeCyclic M
factorBFunction bFunction(h.AnnG,{1,0})
factorBFunction bFunction(cokernel M, {1,0}, {0,0})

-------------------------
-- Example  
-------------------------
restart
load "Dloadfile.m2"
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = presentation prune image map( R^1/ideal dx^3, R^3, matrix{{1, x, x^2}} )

setInfoLevel 666
h = makeCyclic M
factorBFunction bFunction(ideal dx^3, {1})
factorBFunction bFunction(h.AnnG,{1})
factorBFunction bFunction(cokernel M, {1}, {2,1,0})

-------------------------------------
-- Harry's example (does not compute)
-------------------------------------
restart
load "Dloadfile.m2"
W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
w = {1,1}
m = zeroize transpose matrix{{-Dy^2-Dy, -x*Dx-x*Dy-x+1, y*Dy^2+y*Dy-Dy-7},
     {-Dx+Dy, x-(5/4)*y, -1}, {0, -y, -4}, {-1, 0, -x}}

gbTrace 3
h = makeCyclic m

M = cokernel m

wt = {1,1}
shift = {0,0,0}

isHolonomic M
bFunction2(M, wt, shift) -- bug here
bFunction(M, wt, shift)


----------------------------
----------------------------
-------------------------------------------------------------------------------
restart
load "Dloadfile.m2"
load "makeCyclic.m2"
R = QQ[x, y, dx, dy, WeylAlgebra => {x=>dx, y=>dy}]
p = cancelOutX(dx^5*x^7, 0, 2)
h = valuePres(p, dx^5*x^7)
q = cancelOutDX(h, 0, 2)
h = valuePres(q, h)
valuePres(composePres(p,q), dx^5*x^7)

p = getOne(dx^5*x^7)
h = valuePres(p, dx^5*x^7)

----------------------------------------------------
R = QQ[x, y, dx, dy, WeylAlgebra => {x=>dx, y=>dy}]
I = ideal {dx, dy}
f = x^5
I : ideal f
----
M = R^1/I
semicolon(M, f*M_0)
F = ambient M

-----------------------------------------------------

