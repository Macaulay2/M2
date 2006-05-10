R = QQ[x,y,z];
M = R^3
I = ideal(x^2,y^2-x*z)
I*M
R*M_0
I*M_1
J = I*M_1 + R*y^5*M_1 + R*M_2
isSubset(I*M,M)
isSubset((x^3-x)*M,x*M)
F = matrix{{x,y,z}}
image F
kernel F
F = R^3
F/(x*F+y*F+R*F_2)
