R = ZZ[x,y,z];
S = ZZ/101[x,y,z,Degrees => {{1,2},{1,3},{1,3}}]/ideal(x+z^3);
F = map(S,R,{x,y^2,z^3})
use R; F(107*x+y+z)
T = ZZ/5[x,y];
G = map(T,S);
G*F
use R; G(F(107*x+y+z))
