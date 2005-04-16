R = ZZ/101[a..c];
basis(2, R)
M = ideal(a,b,c)/ideal(a^2,b^2,c^2);
f = basis(2,M)
target f
super f
S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];
basis({7,24}, S)
