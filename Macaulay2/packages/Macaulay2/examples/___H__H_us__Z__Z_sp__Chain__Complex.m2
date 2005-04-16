R = ZZ/101[x,y]
C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})
M = HH_1 C
prune M
