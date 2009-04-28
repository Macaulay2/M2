QQ[x, t_0, dt_0, s, WeylAlgebra=>{t_0=>dt_0}]
I = ideal(x*t_0*dt_0,x^2,2*t_0^2*dt_0^2+3*t_0*dt_0,t_0*dt_0+s+1)
J = eliminate(I,{t_0, dt_0})
assert isSubset(J,I)
