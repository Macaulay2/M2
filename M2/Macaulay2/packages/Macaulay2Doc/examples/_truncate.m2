R = ZZ/101[a..c];
truncate(2,R^1)
truncate(2, ideal(a,b,c^3)/ideal(a^2,b^2,c^4))
truncate(2,ideal(a,b*c,c^7))
A = ZZ[x,y,z];
truncate(2,ideal(3*x,5*y,15))
truncate(2,comodule ideal(3*x,5*y,15))
L = ZZ/691[x,y,z];
B = L[s,t];
truncate(2,ideal(3*x*s,5*y*t^2,s*t))
truncate(2,comodule ideal(3*x,5*y,15))
S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];
truncate({7,24}, S^1 ++ S^{{-8,-20}})
