restart
loadPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
T = {z-x*y, x^2-y, y^2-z*x}
d = 2;
RM = random(CC^d,CC^#T)
RT = flatten entries (RM*transpose matrix{T})
L = sum(gens R, v->v*random CC) + 1 
point = first solveSystem(RT|{L}) 
apply(10, i->(
	  L' = sum(gens R, v->v*random CC) + 1;
	  track(RT|{L},RT|{L'},point)
	  ))
