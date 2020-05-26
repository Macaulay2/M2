-- An example of Uli Walther and Anurag Singh
R=ZZ[b,c,s,t,u,v,w,x,y,z,MonomialSize=>16];

f1=b*t*v^2+c*s*u^2;
f2=-b*s*u*x+b*t*v*y+c*s*u*x-c*t*v*y;
f3=-b*s*x^2-c*t*y^2;

ff=b*c*s^4*u^4*x^4+b*c*s^3*t*u^2*v^2*x^4+b*c*s^3*t*u^4*x^2*y^2 -
b*c*s*t^3*v^4*x^2*y^2+2*b^2*s*t^3*u*v^3*x*y^3+2*c^2*s*t^3*u*v^3*x*y^3 -
b*c*s*t^3*u^2*v^2*y^4+2*b^2*t^4*v^4*y^4-b*c*t^4*v^4*y^4+2*c^2*t^4*v^4*y^4;

f0=ff*f1*f2*f3;

I = ideal(4, f1^3, f2^3, f3^3)
gens gb I;
assert( f0 % I == 0 )


