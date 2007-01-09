lift(4/2,ZZ)
R = ZZ[x];
f = ((x+1)^3*(x+4))/((x+4)*(x+1))
lift(f,R)
A = QQ[a..d];
B = A/(a^2-b,c^2-d-a-3);
f = c^5
lift(f,A)
jf = jacobian ideal f
lift(jf,A)
use B;
g = (a^2+2*a-3)-(a+1)^2
lift(g,A)
lift(g,QQ)
lift(lift(g,QQ),ZZ)
substitute(3,RR)
lift(3.0,ZZ)
lift(3.0,QQ)
12/127.
lift(oo,QQ)
setPrecision 15
z = lift(.2341124,RRR)
