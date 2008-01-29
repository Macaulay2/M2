restart
load "Elimination.m2"
load "GTZ.m2"
R = ZZ/32003[a,b,c,d,e,f]
I = ideal(
    a^2*c*d*f^2,
    b^2*c*d*f^2,
    a^2*b*d*f^2,
    b^3*d*f^2,
    a^3*d*f^2,
    a*b^2*d*f^2,
    a^2*c*d*e,
    b^2*c*d*e,
    a^2*b*d*e,
    b^3*d*e,
    a^3*d*e,
    a*b^2*d*e,
    a^2*c*d^2,
    b^2*c*d^2,
    a^2*b*d^2,
    b^3*d^2,
    a^3*d^2,
    a*b^2*d^2,
    a^2*c^2*f^2,
    b^2*c^2*f^2,
    a^2*b*c*f^2,
    b^3*c*f^2,
    a^3*c*f^2,
    a*b^2*c*f^2,
    a^2*b^2*f^2,
    b^4*f^2,
    a^3*b*f^2,
    a*b^3*f^2,
    a^4*f^2)
GTZ1 I


R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
GTZ1 I

R = ZZ/32003[x,y,z]
I = ideal(
    x*y^2*z^2-x*y^2*z+x*y*z^2-x*y*z,
    x*y^3*z+x*y^2*z,
    x*y^4-x*y^2,
    x^2*y*z^2-x^2*y*z,
    x^2*y^3-x^2*y^2,
    x^4*z^3-x^4*z^2+2*x^3*z^3-2*x^3*z^2+x^2*z^3-x^2*z^2,
    x^2*y^2*z,
    x^4*y*z+x^3*y*z,
    2*x^4*y^2+6*x^3*y^2+6*x^2*y^2+x*y^3+x*y^2,
    x^5*z+x^4*z^2+x^4*z+2*x^3*z^2-x^3*z+x^2*z^2-x^2*z,
    x^6*y+3*x^5*y+3*x^4*y+x^3*y)
GTZ1 I

R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    s*u-b*v,
    t*v-s*w,
    v*x-u*y,
    w*y-v*z)
GTZ1 I -- becomes non-binomial...

R = ZZ/32003[a,b,c,d,e,f,g,h]
I = ideal(
    h+f+e-d-a,
    2*f*b+2*e*c+2*d*a-2*a^2-a-1,
    3*f*b^2+3*e*c^2-3*d*a^2-d+3*a^3+3*a^2+4*a,
    6*g*e*b-6*d*a^2-3*d*a-d+6*a^3+6*a^2+4*a,
    4*f*b^3+4*e*c^3+4*d*a^3+4*d*a-4*a^4-6*a^3-10*a^2-a-1,
    8*g*e*c*b+8*d*a^3+4*d*a^2+4*d*a-8*a^4-12*a^3-14*a^2-3*a-1,
    12*g*e*b^2+12*d*a^3+12*d*a^2+8*d*a-12*a^4-18*a^3-14*a^2-a-1,
    -24*d*a^3-24*d*a^2-8*d*a+24*a^4+36*a^3+26*a^2+7*a+1)
codim I
I1 = eliminate(I, {d,e,f,h});
degree I
GTZ0 I
independentSets I
flatt(I,b*c*g)

-----------------------------------------------------
needsPackage "Markov"
G = makeGraph {{},{1},{1},{1},{2,3,4}}
R = markovRing(2,2,2,2,2)
F = marginMap(1,R)
I = F markovIdeal(R, localMarkovStmts G)
transpose gens I

time codim I -- takes a while to get 14
degree I -- 336

debug PrimaryDecomposition
time GTZ0 I;
J0 = oo_0;
J1 = ooo_1;
codim J1
degree J0
degree J1

time GTZ0 J1;
J2 = oo_1;
time GTZ0 J2;
J3 = oo_1;
codim J3
degree J3
time GTZ0 J3;
-----------------------------------------------------
restart
errorDepth = 0
debug PrimaryDecomposition

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

time minimalPrimes I -- 7.06 sec, or 5.53 sec
time primaryDecomposition(I, Strategy=>ShimoyamaYokoyama)
debug PrimaryDecomposition
time GTZ1 I;

J = minPres I
(J1,g) = saturation(J,x)
J1
J2 = trim(J + ideal g)
J2 = trim substitute(J, x=>0)
intersect(J1,J2) == J
C1 = time primaryDecomposition(J1, Strategy=>ShimoyamaYokoyama);
C2 = time primaryDecomposition(J2, Strategy=>ShimoyamaYokoyama);


time minimalPrimes J -- this is much worse than 'minimalPrimes I'
transpose gens J
time I = trim I 
L = ideal apply(sort apply(flatten entries gens I, f -> (size f, first degree f, f)), g -> g#2)
time minimalPrimes L

gbTrace = 3
time (L1, M1) = GTZ0 I;
time gens gb I;
independentSets I
leadTerm(1,gens gb I)
