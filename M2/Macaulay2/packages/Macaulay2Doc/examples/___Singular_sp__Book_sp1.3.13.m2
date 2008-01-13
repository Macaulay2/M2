R = ZZ/32003[x,y,z];
Q = R/(x^2+y^2-z^5, z-x-y^2)
f = z^2+y^2
g = z^2+2*x-2*z-3*z^5+3*x^2+6*y^2
f == g
ann f
