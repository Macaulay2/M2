W = QQ[x, t_0, dt_0, s, WeylAlgebra=>{t_0=>dt_0}]
I = ideal(x*t_0*dt_0,x^2,2*t_0^2*dt_0^2+3*t_0*dt_0,t_0*dt_0+s+1)
J = eliminate(I,{t_0, dt_0})
assert isSubset(J,I)

end
-- MES seeing how to do this:
elimvars = {t_0,dt_0}
keepvars = sort toList(set gens W - set elimvars)
W1 = QQ[elimvars,keepvars,WeylAlgebra=>(options W).WeylAlgebra,MonomialOrder=>Eliminate(#elimvars)]
I1 = sub(I,W1)
J = ideal selectInSubring(1,gens gb I1)
isSubset(J,I1)

restart
loadPackage "Elimination"
debug Elimination

W = QQ[x, t_0, dt_0, s, WeylAlgebra=>{t_0=>dt_0}]
(F,G) = eliminationRing(W,{1,2},z)
target F
options oo
