R = ZZ/5[s..z];
M = matrix {{ x^2+y, z^3}, {y^3-z,3*z-6*x-5*y}}
G = map(R^3,3,(i,j)->R_i^j)
f = 3*s^2*v-t*u*v+s*t^2
H = map(R^4,R^4,(i,j)->diff(R_j*R_i,f))
id_(R^3)
id_(source M)
