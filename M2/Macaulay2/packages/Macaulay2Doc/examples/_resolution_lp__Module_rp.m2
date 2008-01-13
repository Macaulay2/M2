k = ZZ/101; T = k[v..z];
m = matrix {{x,y,z,x^2*v,x*y*v,y^2*v,z*v,x*w,y^3*w,z*w}}
n = rank source m
R = k[u_1 .. u_n]
S = k[u_1 .. u_n,Degrees => degrees source m]
f = map(T,R,m)
g = map(T,S,m)
res ker f
res ker g
isHomogeneous f
isHomogeneous g
R = ZZ/32003[a..d]/(a^2+b^2+c^2+d^2);
M = coker vars R
C = resolution(M, LengthLimit=>6)
