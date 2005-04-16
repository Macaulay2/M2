R = ZZ/2[x,y];
f = random(R^3,R^{3:-1})
exteriorPower_2 f
g = map(coker matrix {{x^2},{x*y},{y^2}}, R^3, id_(R^3))
g2 = exteriorPower(2,g)
target g2
