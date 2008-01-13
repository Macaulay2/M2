S = QQ[x,y,z]/ideal(x^3+y^3+z^3);
T = QQ[u,v,w]/ideal(u^3+v^3+w^3);
G = map(T,S,matrix{{u,v,w^2}})
G(x^3+y^3+z)
R = QQ[x,y,w];
F = map(S,R)
F(x^3)
U = QQ[s,t,u, Degrees => {{1,2},{1,1},{1,3}}];
H = map(U,R,matrix{{s^2,t^3,u^4}})
use R; H(x^2+y^2+w^2)
source H
target H
H.matrix
source H.matrix
target H.matrix
