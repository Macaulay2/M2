R = ZZ[x,y];
S = ZZ[a,b,c];
f = map(R,S,matrix {{x^2,x*y,y^2}})
f(a+b+c^2)
g = map(R,S,matrix {{1,2,3},{4,5,6}})
g(a+b)
S = ZZ[a][b,c];
h = map(S,S,matrix {{b,c,2*a}})
h(a^7 + b^3 + c)
k = map(S,S,matrix {{c,b}})
k(a^7 + b^3 + c)
