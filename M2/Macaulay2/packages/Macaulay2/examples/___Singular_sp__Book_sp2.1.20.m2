A = QQ[x,y,z];
M = image matrix{{x*y,x},{x*z,x}}
N = image matrix{{y^2,x},{z^2,x}}
M + N
intersect(M,N)
M : N
N : M
Q = A/x^5;
M = substitute(M,Q)
ann M
M : x
