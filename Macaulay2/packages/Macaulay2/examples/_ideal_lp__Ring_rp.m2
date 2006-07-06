S = ZZ/2[x,y,z];
ideal S
R = S/(y^2-x*z,x^2*y-z^2)
ideal R
T = R/(x^3-y*z)
ideal T
ambient T
sing = singularLocus T
ideal sing
ambient sing
