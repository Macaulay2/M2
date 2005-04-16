R = ZZ[x,y];
S = ZZ[a,b,c];
f = map(R,S,{x^2,x*y,y^2})
f(a+b+c^2)
