R = ZZ/32003[x,y,z];
I = ideal(x^2,y*z-x);
module I
A = matrix{{x*y-z,z^3}};
M = image A
ideal M
coker generators I
R^1/I
resolution I
