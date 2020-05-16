-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules"
Dtrace 1
pInfo(1, "testing makeCyclic...")

-------------------------
-- R^3
-------------------------

x = symbol x; dx = symbol dx; 
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = matrix {{dx, 0, 0}, {0, dx, 0}, {0, 0, dx}}
h = makeCyclic M
assert (listForm bFunction(h.AnnG,{1}) == listForm bFunction(cokernel M, {1}, {0,1,2}))
assert (holonomicRank h.AnnG == holonomicRank cokernel M)
assert (singLocus h.AnnG == singLocus cokernel M)


-------------------------
-- R^1 / dx^3  
-------------------------
use R
M = presentation image map( R^1/ideal dx^3, R^3, matrix{{1, x, x^2}} )
h = makeCyclic M 
b1 = bFunction(ideal dx^3, {1})
b2 = bFunction(cokernel M, {1}, {0,-1,-2})
assert (listForm b1 == listForm b2)
b2 = bFunction(cokernel M, {1}, {2,1,0})
b3 = bFunction(h.AnnG, {1})
assert (listForm b3 == listForm b2)






