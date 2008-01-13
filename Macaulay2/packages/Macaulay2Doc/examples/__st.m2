set{hi,you,there} * set{hi,us,here,you}
R = QQ[a..d];
a * {b,c,d}
f = map(R,R,{b,c,a,d})
g = map(R,R,{(a+b)^2,b^2,c^2,d^2})
f*g
(f*g)(a) == f(g(a))
M = R^2; I = ideal(a+b,c);
N = I*M + a*R^2
isHomogeneous N
