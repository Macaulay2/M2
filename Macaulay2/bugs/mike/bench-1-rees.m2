-- Add these examples to the ReesAlgebra benchmark code
kk=ZZ/101
n=6
S=kk[vars(0..n)]
i=monomialCurveIdeal(S,{4,8,11,13,15})
time reesIdeal(i) -- 1.7 sec
--time reesIdeal(i,i_0) -- doesn't finish before I get impatient (10 min?)!


restart
loadPackage "ReesAlgebra2"
kk=ZZ/101
n=3
S=kk[vars(0..n)]
i=ideal random(S^1, S^{-3,-4,-5,-6})
time reesIdeal(i) --8.4 sec
time reesIdeal(i,i_0) --1.4sec
