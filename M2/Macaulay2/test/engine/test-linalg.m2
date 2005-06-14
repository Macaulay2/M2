R = ZZ/32003[a..e]
I = ideal(b*c-d^2-2, b^2-c*d-1)
G = gb(I, Algorithm=>LinearAlgebra)
gens G

J = ideal flatten entries gens gb I
G = gb(J, Algorithm=>LinearAlgebra)
gens G

m = basis(3,R) * random(R^35,R^3)
time gb(m, Algorithm=>LinearAlgebra);
time gb(ideal flatten entries m);

load "markov.m2"
R = markovRing(2,2,2,2)
G = makeGraph {{},{1},{2},{1,2}}
I = markovIdeal(R, localMarkovStmts G);
R = (coefficientRing R)[vars(0..numgens R-1)]
I = substitute(I,vars R)
gb(I, Algorithm=>Faugere)
transpose gens oo
I2 = markovIdeal(R, localMarkovStmts G);
gbTrace=3
time gens gb I2
I = ideal flatten entries gens I
transpose gens gb I
f0 = I_4
f1 = I_0
f2 = I_1
f3 = I_2
f4 = I_3
f5 = i*f0-b*f1
f6 = a*d*f1+c*f5+(d*e-c*f-b*g-f*g+a*h+e*h+d*i+h*i-c*j-g*j-f*k-j*k+a*l+e*l+i*l+d*m+h*m+l*m-c*n-g*n-k*n-b*o-f*o-j*o-n*o+a*p+e*p+i*p+m*p)*f1+(-a*k-i*k)*f0+k*f5
f0
f1




load "markov.m2"
R = markovRing(3,3,3,2,2)
G = makeGraph {{},{1},{2},{1,2},{1,2}}
I = markovIdeal(R, localMarkovStmts G);
gb(I, Algorithm=>Faugere)


load "raw-util.m2"
R = ZZ/32003[a..e]
m = basis(3,R) * random(R^35,R^3)
rawMatrixCompress raw m_{0}

m2 = basis(5,R) * random(R^126,R^3)
m = (m | m2)
raw m === rawMatrixCompress raw m


m2 = basis(25,R);
raw m2 === rawMatrixCompress raw m2

m2 = basis(35,R);
time (raw m2 === rawMatrixCompress raw m2)

time m2 = basis(40,R);
raw m2 === time rawMatrixCompress raw m2

restart
load "raw-util.m2"
rawMatrixCompress raw gens I;

-- simpler example
restart
load "raw-util.m2"
R = ZZ/32003[a..e]
m = matrix{{a*e^2}}
m = matrix{{a*e^2, b*d^3, a*d*e}}
time rawMatrixCompress raw m
rawMatrixCompress raw matrix{{3*a-7*b, a^100-5*b^100*c*d}}

-- test of spair handling
restart
load "raw-util.m2"
R = ZZ/32003[a..e]

m = matrix{{a^3, a^2*b, c^2*d, d^4, a*b*c*d}}
m = matrix{{a^2, a*b, a*c, a*d, b^2}}
time rawMatrixCompress raw m



