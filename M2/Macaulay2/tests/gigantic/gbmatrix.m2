debug Core
R = ZZ/101[a..d]
m = matrix{{a^2, a*b, a*c}}
rawKernelOfGB raw m
rawKernelOfGB oo

R = ZZ/101[a..f]
m = matrix{{a^2, a*b, a*c, a*d, a*e, a*f}}
m1 = rawKernelOfGB raw m
m2 = rawKernelOfGB m1
m3 = rawKernelOfGB m2
m4 = rawKernelOfGB m3
m5 = rawKernelOfGB m4
m6 = rawKernelOfGB m5
assert(m1 * m2 == 0)
assert(m2 * m3 == 0)
assert(m3 * m4 == 0)
assert(m4 * m5 == 0)
assert(m6 == 0)

R = ZZ/101[a..g]
m = gens gb matrix{{a^2, a*b-b^2, c^2, d^2}}
m1 = map(R,rawKernelOfGB raw m)
m2 = map(R,rawKernelOfGB raw m1)
m3 = map(R,rawKernelOfGB raw m2)
assert(m1 * m2 == 0)
assert(m2 * m3 == 0)
-- column 5 of m1 has a -1:
n1 = mutableMatrix m1
columnAdd(n1,6,d^2,5); n1
columnAdd(n1,7,c^2,5); n1
columnAdd(n1,8,a-b,5); n1
m1 = matrix n1
m1 * m2
m2
m1
n2 = mutableMatrix m2
n2_(5,2) = 0_R
n2_(5,3) = 0_R
n2
m2 = matrix n2
m1 * m2
R = ZZ/101[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = ideal flatten entries(m1*m2-m2*m1)
time s1 = gens gb J
time (
s2 = rawKernelOfGB raw s1;
s3 = rawKernelOfGB s2;
s4 = rawKernelOfGB s3;
s5 = rawKernelOfGB s4;
s6 = rawKernelOfGB s5;
s7 = rawKernelOfGB s6;
s8 = rawKernelOfGB s7;
)
assert((raw s1) * s2 == 0)
assert(s2*s3 == 0)
assert(s3*s4 == 0)
assert(s4*s5 == 0)
assert(s5*s6 == 0)
assert(s6*s7 == 0)
assert(s7*s8 == 0)

debug Core
--J = Grassmannian(2,5)
R = ZZ/31991[vars(0..19)]
J = ideal(p*r-o*s+m*t,j*r-i*s+g*t,p*q-n*s+l*t,o*q-n*r+k*t,m*q-l*r+k*s,j*q-h*s+f*t,i*q-h*
     r+e*t,g*q-f*r+e*s,d*q-c*r+b*s-a*t,j*o-i*p+d*t,m*n-l*o+k*p,j*n-h*p+c*t,i*n-h*o+b*t,g*
     n-f*o+e*p+a*t,d*n-c*o+b*p,j*m-g*p+d*s,i*m-g*o+d*r,h*m-f*o+e*p+c*r-b*s+a*t,j*l-f*p+c*
     s,i*l-f*o+c*r+a*t,h*l-f*n+c*q,g*l-f*m+a*s,d*l-c*m+a*p,j*k-e*p+b*s-a*t,i*k-e*o+b*r,h*
     k-e*n+b*q,g*k-e*m+a*r,f*k-e*l+a*q,d*k-b*m+a*o,c*k-b*l+a*n,g*h-f*i+e*j,d*h-c*i+b*j,d*
     f-c*g+a*j,d*e-b*g+a*i,c*e-b*f+a*h)
s1 = raw gens gb J;
s2 = rawKernelOfGB s1;
time s3 = rawKernelOfGB s2;
time s4 = rawKernelOfGB s3;
time s5 = rawKernelOfGB s4;
time s6 = rawKernelOfGB s5;
time s7 = rawKernelOfGB s6;
time s8 = rawKernelOfGB s7;

s9 = rawKernelOfGB s8;
s10 = rawKernelOfGB s9;
s11 = rawKernelOfGB s10;
assert(s1 * s2 == 0)
assert(s2 * s3 == 0)
assert(s3 * s4 == 0)
assert(s4 * s5 == 0)
assert(s5 * s6 == 0)
assert(s6 * s7 == 0)
assert(s7 * s8 == 0)
assert(s8 * s9 == 0)
assert(s9 * s10 == 0)
assert(s10 * s11 == 0)

C = time res J
