R = ZZ/101[a..c];
truncate(2,R^1)
truncate(2, ideal(a,b,c^3)/ideal(a^2,b^2,c^4))
S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];
truncate({7,24}, S^1 ++ S^{{-8,-20}})
