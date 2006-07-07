restart
R = ZZ/32003[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z, MonomialSize=>8]
I = ideal(
    -a*b-a*d+2*a*h,
    a*d-b*d-c*f-2*a*h+2*b*h+2*c*k,
    a*b-a*d-2*b*h+2*d*h-2*c*k+2*f*k+2*g*l,
    a*c-2*c*s-a*t+2*b*t,
    a*c-c*s-2*a*t+b*t,
    -d-3*s+4*u,
    -f-3*t+4*v,
    -g+4*w,
    -a+2*x,
    -b^2-c^2+2*b*x+2*c*y,
    -d^2-f^2-g^2+2*d*x+2*f*y+2*g*z)
hf = poincare coker gens I
R = ZZ/32003[a,b,c,d,f,g,s,t,x,h,k,l,u,v,w,y,z, MonomialSize=>8,MonomialOrder=>{9,8}]
J = substitute(I,R)
options gb
gbTrace = 3
time gens gb(J, Hilbert=>hf);


time leadTerm(1,gens gb I);

restart
needsPackage "PrimaryDecomposition"
debug PrimaryDecomposition
R = ZZ/32003[b,c,h,k,l,s,t,u,v,w,y,z, MonomialSize=>8]
I = ideal(s^2+t^2+10665*s*u-7110*u^2+10665*t*v-7110*v^2-7110*w^2-10667*t*y+3555*v*y+3555*w*z,c*s-b*t,b*s-2*h*s+c*t-2*k*t-10669*b*u-10665*h*u-10669*c*v-10665*k*v-10665*l*w,b*h+c*k+3*h*s+3*k*t-4*h*u-4*k*v-4*l*w,b^2+c^2-2*c*y)
J = saturation(I,c)
L1 = J_0
L2 = trim(I + ideal(c))

-- Compute the primary decomposition of L1
codim L1 -- 5
degree L1 -- 16
genera L1
res L1 -- length 5, so CM, so equidimensional

independentSets L1
transpose gens L1
-- how best to check that L1 is prime?

load "Elimination.m2"
M1 = trim eliminate(L1,{h,s,y})
codim M1
degree M1
res M1
(L1,M1) = GTZ0 I
(codim oo, degree oo)

transpose gens trim L1
codim L1
codim L2
transpose gens trim L2
