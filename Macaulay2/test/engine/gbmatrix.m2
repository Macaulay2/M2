error "rawRemoveScalarMultiples was being used to test Eschreyer, fix this test file, Mike!"

debug Macaulay2Core
R = ZZ/101[a..d]
m = matrix{{a^2, a*b, a*c}}
rawRemoveScalarMultiples raw m
rawRemoveScalarMultiples oo

R = ZZ/101[a..f]
m = matrix{{a^2, a*b, a*c, a*d, a*e, a*f}}
gbTrace = 4
m1 = rawRemoveScalarMultiples raw m
m2 = rawRemoveScalarMultiples m1
m3 = rawRemoveScalarMultiples m2
m4 = rawRemoveScalarMultiples m3
m5 = rawRemoveScalarMultiples m4
m6 = rawRemoveScalarMultiples m5
assert(m1 * m2 == 0)
assert(m2 * m3 == 0)
assert(m3 * m4 == 0)
assert(m4 * m5 == 0)
assert(m6 == 0)

R = ZZ/101[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = ideal flatten entries(m1*m2-m2*m1)
time s1 = gens gb J
time (
s2 = rawRemoveScalarMultiples raw s1;
s3 = rawRemoveScalarMultiples s2;
s4 = rawRemoveScalarMultiples s3;
s5 = rawRemoveScalarMultiples s4;
s6 = rawRemoveScalarMultiples s5;
s7 = rawRemoveScalarMultiples s6;
s8 = rawRemoveScalarMultiples s7;
)

debug Macaulay2Core
J = Grassmannian(2,5)
R = ZZ/31991[vars(0..19)]
J = ideal(p*r-o*s+m*t,j*r-i*s+g*t,p*q-n*s+l*t,o*q-n*r+k*t,m*q-l*r+k*s,j*q-h*s+f*t,i*q-h*
     r+e*t,g*q-f*r+e*s,d*q-c*r+b*s-a*t,j*o-i*p+d*t,m*n-l*o+k*p,j*n-h*p+c*t,i*n-h*o+b*t,g*
     n-f*o+e*p+a*t,d*n-c*o+b*p,j*m-g*p+d*s,i*m-g*o+d*r,h*m-f*o+e*p+c*r-b*s+a*t,j*l-f*p+c*
     s,i*l-f*o+c*r+a*t,h*l-f*n+c*q,g*l-f*m+a*s,d*l-c*m+a*p,j*k-e*p+b*s-a*t,i*k-e*o+b*r,h*
     k-e*n+b*q,g*k-e*m+a*r,f*k-e*l+a*q,d*k-b*m+a*o,c*k-b*l+a*n,g*h-f*i+e*j,d*h-c*i+b*j,d*
     f-c*g+a*j,d*e-b*g+a*i,c*e-b*f+a*h)
s1 = raw gens gb J;
gbTrace = 4
s2 = rawRemoveScalarMultiples s1;
time s3 = rawRemoveScalarMultiples s2;
time s4 = rawRemoveScalarMultiples s3;
time s5 = rawRemoveScalarMultiples s4;
time s6 = rawRemoveScalarMultiples s5;
time s7 = rawRemoveScalarMultiples s6;
time s8 = rawRemoveScalarMultiples s7;
exit

s9 = rawRemoveScalarMultiples s8;
s10 = rawRemoveScalarMultiples s9;
s11 = rawRemoveScalarMultiples s10;
s1 * s2 == 0
s2 * s3 == 0
s3 * s4 == 0
s4 * s5 == 0
s5 * s6 == 0
s6 * s7 == 0
s7 * s8 == 0

nterms = (m) -> (
     d := numgens ring J;
     apply(rawNumberOfColumns m, c -> (
	       sum apply(rawNumberOfRows m, r -> (
		    rawTermCount(d, m_(r,c)))))))
J = vars R
nterms s2

time res ideal J
raw s1 * s2 == 0
s2 * s3 == 0
s3 * s4 == 0
s4 * s5 == 0
s5 * s6 == 0
s6 * s7 == 0
s7 * s8 == 0
s7
s8
m = matrix{{a,b},{c,d}}
m^2
raw (m^5) - rawRemoveScalarMultiples raw (m^5)

