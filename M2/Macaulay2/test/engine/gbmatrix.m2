debug Macaulay2
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

J = Grassmannian(2,5)
s1 = raw gens gb J;
s2 = rawRemoveScalarMultiples s1;
s3 = rawRemoveScalarMultiples s2;
s4 = rawRemoveScalarMultiples s3;
s5 = rawRemoveScalarMultiples s4;
s6 = rawRemoveScalarMultiples s5;
s7 = rawRemoveScalarMultiples s6;
s8 = rawRemoveScalarMultiples s7;
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

