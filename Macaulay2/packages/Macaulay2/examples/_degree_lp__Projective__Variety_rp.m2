S = ZZ/32003[x,y,z];
I = ideal(x^4-4*x*y*z^2-z^4-y^4);
R = S/I;
X = variety I
degree X
degree X == degree I
degree X == degree R
