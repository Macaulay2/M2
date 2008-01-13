R = ZZ[a..d];
M = cokernel matrix{{2*a-b,3*c-5*d,a^2-b-3}}
f = map(a*M, M, a^3+a^2*b)
(target f,source f)
N = cokernel f
minimalPresentation N
