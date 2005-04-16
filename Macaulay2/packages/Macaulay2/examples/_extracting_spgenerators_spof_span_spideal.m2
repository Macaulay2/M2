R = ZZ[w,x,y,z];
I = ideal(z*w-2*x*y, 3*w^3-z^3,w*x^2-4*y*z^2,x);
I_0
I_3
gens I
first entries gens I
numgens I
mingens I
trim I
R = QQ[a..d];
I = monomialCurveIdeal(R,{1,2,3});
toString I
