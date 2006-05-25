A = QQ[x,y,z];
f = matrix{{x*y-1,y^4},{z^2+3,x^3},{x*y*z,z^2}}
M = image f
numgens M
ambient M
Q = A/(x^2+y^2+z^2);
substitute(M,Q)
