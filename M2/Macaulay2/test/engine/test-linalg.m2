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
load "markov.m2"
R = markovRing(3,3,3,2,2)
G = makeGraph {{},{1},{2},{1,2},{1,2}}
I = markovIdeal(R, localMarkovStmts G);
gens I;
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



