-- by Taylor Brysiewicz (2015-05)

loadPackage "NumericalAlgebraicGeometry"--load the package



R = CC[s,t]--define the ring

f=s+t;
g=s*t+1;
solveSystem({f,g})--here are a couple of simple polynomials to solve to make sure solveSystem is syntactically working


F=0.535487*s^3*t^(16)-3.21292*s^2*t^(16)+5.89036*s*t^(16)-3.21292*t^(16)+0.4269*s^4*t^(10)-3.842*s^3*t^(10)+12.809*s^2*t^(10)-18.78*s*t^(10)+10.2476*t^(10)
T=8.056

FT = {F,t-T}
solveSystem FT
sols = solveSystem(FT, PostProcess=>false)

apply(sols, s->norm evaluate(matrix{FT}, s))
rsols = refine(FT,sols)
apply(rsols, s->norm evaluate(matrix{FT}, s))

G=map(CC[x],CC[s,t],{x,T})

F=0.535487*s^3*t^(16)-3.21292*s^2*t^(16)+5.89036*s*t^(16)-3.21292*t^(16)+0.4269*s^4*t^(10)-3.842*s^3*t^(10)+12.809*s^2*t^(10)-18.78*s*t^(10)+10.2476*t^(10)
T=8.056

Fsub = G(F)

peek solveSystem ({Fsub}, PostProcess=>false)




