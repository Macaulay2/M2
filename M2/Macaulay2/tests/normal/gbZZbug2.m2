-- An example of Uli Walther and Anurag Singh
-- Email dated 4/17/2006

R=ZZ[b,c,s,t,u,v,w,x,y,z,MonomialSize=>16];

f1=b*t*v^2+c*s*u^2;
f2=-b*s*u*x+b*t*v*y+c*s*u*x-c*t*v*y;
f3=-b*s*x^2-c*t*y^2;

ff=b*c*s^4*u^4*x^4+b*c*s^3*t*u^2*v^2*x^4+b*c*s^3*t*u^4*x^2*y^2 -
b*c*s*t^3*v^4*x^2*y^2+2*b^2*s*t^3*u*v^3*x*y^3+2*c^2*s*t^3*u*v^3*x*y^3 -
b*c*s*t^3*u^2*v^2*y^4+2*b^2*t^4*v^4*y^4-b*c*t^4*v^4*y^4+2*c^2*t^4*v^4*y^4;

f0=ff*f1*f2*f3;

I = ideal(4, f1^3, f2^3, f3^3)
gbTrace=3
gens gb I;
f0 % I

end
R=ZZ[b,c,s,t,u,v,x,y];

M = matrix{{b^2,b*c,c^2}} ** matrix{{s^4,s^3*t,s^2*t^2,s*t^3,t^4}} ** matrix{{f1^3,f2^3,f3^3}}
(mm,cf) = coefficients(M,Variables=>{b,c,s,t})
(mm2,cf2) = coefficients(matrix{{f0}},Variables=>{b,c,s,t},Monomials=>mm)
mm - mm2
N = cf2 | cf
L =  4*id_(target cf) | cf
gens gb L;
cf2 // L
b^2*f1^3, b*c*f1^3, c^2*f1^3
f0

QQ[a,b,x,y]
f = a^2*x^2-b^2*y^2
coefficients(matrix{{f}},Variables=>{a,b},Monomials=>matrix{{a^2,a*b,b^2}})
