S = QQ[a,b,c];
R = QQ[x,y,z];
phi = map(R,S,{x,y,x^2-y^3})
isInjective phi
ker phi
psi = map(R,S,{x,x+y,z-x^2+y^3})
isInjective psi
ker psi
