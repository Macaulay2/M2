R = QQ[x,y,z];
I = ideal(x*y,x*z,y*z)
M = I/I^2
f = matrix{{x,y}}
g = matrix{{x^2,x*y,y^2,z^4}}
M = subquotient(f,g)
N = (image f)/(image g)
N1 = (image f + image g)/(image g)
M === N
generators M
relations M
N2 = R*M_0 + I*M
M/N2
prune(M/N2)
ambient M
ambient M === target relations M
ambient M === target generators M
super M
super M === cokernel relations M
M + M
trim (M+M)
minimalPresentation M
prune M
