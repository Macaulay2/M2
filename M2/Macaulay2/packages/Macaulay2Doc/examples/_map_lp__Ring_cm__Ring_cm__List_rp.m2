R = ZZ[x,y];
S = ZZ[a,b,c];
f = map(R,S,{x^2,x*y,y^2})
f(a+b+c^2)
g = map(R,S,{a=>x^2,b=>x*y,c=>y^2})
g(a+b+c^2)
