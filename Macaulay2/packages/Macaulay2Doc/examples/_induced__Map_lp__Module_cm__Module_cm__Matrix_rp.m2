R = ZZ/32003[x,y,z]
g1 = matrix{{x,y,z}}
g2 = matrix{{x^2,y^2,z^2}}
K1 = ker g1
K2 = ker g2
f = map(ambient K1, ambient K2, {{x,0,0},{0,y,0},{0,0,z}})
h = inducedMap(K1,K2,f)
h1 = inducedMap(target f,K2,f)
h2 = inducedMap(,K2,f)
h1 == h2
