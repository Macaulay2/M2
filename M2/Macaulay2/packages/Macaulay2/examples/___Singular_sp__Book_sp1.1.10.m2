A = QQ[a,b,c];
f = a+b+a*b+c^3;
B = QQ[x,y,z];
F = map(B,A,{x+y, x-y, z})
g = F f
A1 = QQ[x,y,c,b,a,z];
substitute(f,A1)
v = take(gens A1, numgens A)
G = map(A1,A,v)
G f
