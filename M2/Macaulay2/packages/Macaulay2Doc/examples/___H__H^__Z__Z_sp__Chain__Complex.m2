R = ZZ/101[x,y]
C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})
M = HH^1 C
prune M
needsPackage "SimplicialComplexes"
R = QQ[a..d]
D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}
C = chainComplex D
HH_2 C
prune oo
