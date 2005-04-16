R1 = ZZ/101;
R2 = ZZ/101[s,t];
describe R2
I = ideal (s^4+t^2+1);
R3 = R2/I;
describe R3
f = s^4+1
g = s^4+t^2+1
use R2;
substitute(g,R2)
f = s^4+1
g = s^4+t^2+1
substitute(f,R3)
describe R3
R4 = frac R3;
describe R4
use R2;
f = s^4+1;
substitute(f,R4)
use R3;
g = substitute(f,R3);
substitute(g,R4)
F = map(R4,R3)
F(f)
R5 = R4[u,v,w];
describe R5
J = ideal(u^3-v^2*w+w^3,v^2+w^2,u*v-v*w+u*w)
R6 = R5/J;
describe R6
map(R6,R2)
substitute(f,R6)
use R2;
f = s^4+1;
F = map(R4,R2);
G = map(R5,R4);
H = map(R6,R5);
H(G(F(f)))
f1 = substitute(f,R4)
f2 = substitute(f1,R5)
substitute(f2,R6)
substitute(f,vars R3)
try substitute(f,vars R5) else "found error"
