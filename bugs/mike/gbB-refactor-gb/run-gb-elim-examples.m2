end

restart
load "~/src/M2-branch-mike/BRP/test-results/gbByHomog.m2"
elimvars = rsort toList(set gens ring J1 - set{x10})
elimvars = rsort toList(set gens ring J1 - set{s})
time eliminate1(elimvars,J1)

R = ZZ/2[vars(0..17), z, MonomialOrder=>Lex]
R = ZZ/2[vars(0..17), z]
L1 = ideal(a^2+a,b^2+b,c^2+c,d^2+d,e^2+e,f^2+f,g^2+g,h^2+h,i^2+i,j^2+j,k^2+k,l^2+l,m^2+m,n^2+n,o^2+o,p^2+p,q^2+q,r^2+r)
L2 = ideal(a*e*g*h*i*l*m*n*o*q*r+b*d*e*g*j*k*l*m*p*r+b*c*e*g*j*k*m*o*p*r+a*b*d*e*f*g*h*m*o+a*b*c*d*g*j*o,a*b*d*e*f*i*j*k*m*n*q*r+a*c*d*h*i*k*m*o*p*q*r+d*g*i*j*m*n*o*p*r+a*b*c*e*f*h*k*n+b*c*e*h*i*k*l*r,b*d*e*g*i*k*l*o*p*r+b*c*d*h*i*j*l*n*p+a*g*i*k*l*m*o*p*q+a*c*g*j*k*l*n*q*r+c*d*e*m*o*p,c*e*f*g*i*j*l*n*o*p*r+b*e*f*i*j*k*n*o*p+h*j*l*m*n*p*q*r+b*c*h*l*n+a*c*f*i*o,a*b*c*f*h*i*k*l*m*p*r+a*c*f*h*k*m*o*q*r+a*e*j*l*m*n*o*r+d*e*i*l*m*n*p*r+a*b*g*j*o*p*r,b*d*e*f*k*l*n*o*q*r+a*f*i*j*k*n*p*q+a*c*d*i*n*o*p*r+b*f*g*i*k*n*o+c*e*h*i*k*o,b*c*f*g*h*i*k*l*m*o*p*r+a*d*e*g*h*i*k*l*n*o*r+b*e*f*i*k*l*m*n*o*q*r+b*d*e*g*h*i*k*m*n*r+g*j*n*o*p*q,a*d*e*g*h*l*n*o*p*q*r+c*d*f*h*i*k*n*o*p*q+a*d*g*k*l*m*n*o*p+b*f*g*k*m*n*o*q+b*c*d*f*i*n*p)
L = L1 + L2
L = homogenize(L,z)
time gens gb L;
L = ideal(L_*);
gbTrace=3
time gens gb(L, Algorithm=>LinearAlgebra);
L0 = sub(gens gb L, {z=>1});
G = forceGB L0;
leadTerm G

restart
load "~/src/M2-branch-mike/BRP/test-results/gbByHomog.m2"
R = ZZ/2[vars(0..17), MonomialOrder=>Lex]
L1 = ideal(a^2+a,b^2+b,c^2+c,d^2+d,e^2+e,f^2+f,g^2+g,h^2+h,i^2+i,j^2+j,k^2+k,l^2+l,m^2+m,n^2+n,o^2+o,p^2+p,q^2+q,r^2+r)
A = R/L1
L2 = ideal(a*e*g*h*i*l*m*n*o*q*r+b*d*e*g*j*k*l*m*p*r+b*c*e*g*j*k*m*o*p*r+a*b*d*e*f*g*h*m*o+a*b*c*d*g*j*o,a*b*d*e*f*i*j*k*m*n*q*r+a*c*d*h*i*k*m*o*p*q*r+d*g*i*j*m*n*o*p*r+a*b*c*e*f*h*k*n+b*c*e*h*i*k*l*r,b*d*e*g*i*k*l*o*p*r+b*c*d*h*i*j*l*n*p+a*g*i*k*l*m*o*p*q+a*c*g*j*k*l*n*q*r+c*d*e*m*o*p,c*e*f*g*i*j*l*n*o*p*r+b*e*f*i*j*k*n*o*p+h*j*l*m*n*p*q*r+b*c*h*l*n+a*c*f*i*o,a*b*c*f*h*i*k*l*m*p*r+a*c*f*h*k*m*o*q*r+a*e*j*l*m*n*o*r+d*e*i*l*m*n*p*r+a*b*g*j*o*p*r,b*d*e*f*k*l*n*o*q*r+a*f*i*j*k*n*p*q+a*c*d*i*n*o*p*r+b*f*g*i*k*n*o+c*e*h*i*k*o,b*c*f*g*h*i*k*l*m*o*p*r+a*d*e*g*h*i*k*l*n*o*r+b*e*f*i*k*l*m*n*o*q*r+b*d*e*g*h*i*k*m*n*r+g*j*n*o*p*q,a*d*e*g*h*l*n*o*p*q*r+c*d*f*h*i*k*n*o*p*q+a*d*g*k*l*m*n*o*p+b*f*g*k*m*n*o*q+b*c*d*f*i*n*p)

time gbByHomog1(L2);
L2 = ideal L2_*;
time gbByHomog1(L2, Algorithm=>LinearAlgebra);
L2 = ideal L2_*;
time doMyExample L2;

J = first flattenRing L2
R1 = (coefficientRing R)[gens R, zhomog]
J1 = homogenize(sub(J,R1),zhomog)
time G1 = gens gb(J1, Algorithm=>LinearAlgebra);
G2 = forceGB sub(G1, {zhomog => 1});
hf1 = poincare monomialIdeal leadTerm G2

G1 = first divideByVariable(G1, zhomog);
hf = poincare ideal leadTerm G1

R2 = (coefficientRing R)[gens R1, MonomialOrder => Lex]
J2 = sub(ideal gens G2, R2);
J2 = homogenize(J2, zhomog);
G3 = time gb(J2, Algorithm=>LinearAlgebra, Hilbert=>hf);
ANS = gens forceGB sub(gens G3, {zhomog=>1});
ANS

