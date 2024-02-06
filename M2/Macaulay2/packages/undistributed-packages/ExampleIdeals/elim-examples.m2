---------------------------------
-- bernd1
-- This is from an email of Bernd's dated 12/15/2009.
restart
kk = QQ
kk = ZZ/101
R = kk[a,b,c];

pol = -120*a^2*c*b^3+3*c^6+66*a^5*b+60*a*c*b^4-84*c^2*a*b^3-4*a*c^5+21*a^6+8*c^3*a*b^2-6*a*c^4*b-162*a^2*c^2*b^2-8*a^2*c^3*b+3*a^2*c^4+21*b^6-12*c*b^5-27*c^2*b^4+40*c^3*b^3+66*a*b^5-84*a^3*c^2*b-69*a^4*b^2-36*a^3*b^3+3*c^4*b^2+4*c^5*b-69*a^2*b^4+12*a^5*c-27*a^4*c^2-40*a^3*c^3-60*a^4*c*b+120*a^3*c*b^2;


S = R/ideal(pol);
T = kk[p12,p13,p14,p23,p24,p34];

f = map(S,T,{-a^4*c+3*a^5-10*a^3*c^2-2*a^2*c^3+3*a*c^4-c^5+a^4*b-18*a^3*b^2+4*a^3*c*b+18*a^2*b^3-6*a^2*c*b^2-18*a^2*c^2*b-a*b^4+4*a*c*b^3+18*c^2*a*b^2+4*c^3*a*b-3*b^5-c*b^4+10*c^2*b^3-2*c^3*b^2-3*c^4*b, 2*a^5-2*a*c^4-6*a^4*b+4*a^3*b^2+4*a^2*b^3-6*a*b^4+2*b^5-2*c^4*b, -9*a^4*c-3*a^5-6*a^3*c^2+6*a^2*c^3-3*a*c^4-c^5+15*a^4*b-30*a^3*b^2-12*a^3*c*b+30*a^2*b^3+42*a^2*c*b^2+18*a^2*c^2*b-15*a*b^4-12*a*c*b^3-18*c^2*a*b^2+4*c^3*a*b+3*b^5-9*c*b^4+6*c^2*b^3+6*c^3*b^2+3*c^4*b, 2*a^5-2*a*c^4-2*a^4*b-4*a^3*b^2+40*a^3*c*b+4*a^2*b^3+48*a^2*c*b^2+8*a^2*c^2*b+2*a*b^4+40*a*c*b^3-8*c^2*a*b^2-24*c^3*a*b-2*b^5+2*c^4*b, -8*a^4*c-6*a^5+4*a^3*c^2+8*a^2*c^3-6*a*c^4-46*a^4*b-204*a^3*b^2-112*a^3*c*b-204*a^2*b^3+60*a^2*c^2*b-46*a*b^4+112*a*c*b^3+60*c^2*a*b^2-6*b^5+8*c*b^4+4*c^2*b^3-8*c^3*b^2-6*c^4*b, -2*a^5+2*a*c^4-30*a^4*b-28*a^3*b^2-24*a^3*c*b+28*a^2*b^3+48*a^2*c*b^2+24*a^2*c^2*b+30*a*b^4-24*a*c*b^3-24*c^2*a*b^2-24*c^3*a*b+2*b^5-2*c^4*b});

time P = kernel f;
dim P, degree P
toString mingens P
betti mingens P

U = kk[p12,p13,p14,p23,p24,p34,u1,u2,u3,u4, MonomialOrder=>Eliminate 6];
g = map(U,T,{p12,p13,p14,p23,p24,p34});
PP = g(P) + 
ideal(u1*p23-u2*p13+u3*p12,
u1*p24-u2*p14+u4*p12,
u1*p34-u3*p14+u4*p13,
u2*p34-u3*p24+u4*p23);
time PP = (PP : ideal(p12));
time PP = (PP : ideal(p13));
time PP = (PP : ideal(p14));
time PP = (PP : ideal(p23));
time PP = (PP : ideal(p24));
time PP = (PP : ideal(p34));
codim PP, degree PP

surface = ideal selectInSubring(1,gens gb PP)

V = QQ[x,y,z];
h = map(V,U,{0,0,0,0,0,0,1,x,y,z});
toString gens h(surface)


U1 = kk[p13,p14,p23,p24,p34,u1,u2,u3,u4,p12];
PP = trim sub(PP,U1);
PP1 = sub(PP,U1);

L = flatten entries gens gb(PP1, DegreeLimit => 7);
PP1_*/factor

U1 = kk[p13,p14,p23,p24,p34,u1,u2,u3,u4,p12,h];
PP = sub(PP, U1) + ideal(p12*p13*p14*p23*p24*p34-h^6);
gbTrace=3
time gens gb(PP, Algorithm=>LinearAlgebra);
time gens gb(PP);
time gens gb(PP, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);


time gens gb(PP1, Algorithm=>LinearAlgebra);
time gens gb(PP1);
time gens gb(PP1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);

