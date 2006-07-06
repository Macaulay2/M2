R = ZZ/101[a..c];
basis(2, R)
M = ideal(a,b,c)/ideal(a^2,b^2,c^2)
f = basis(2,M)
target f
super f
S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];
basis({7,24}, S)
R = QQ[x,y,z]/(x^2,y^3,z^5)
basis R
R = QQ[x,y,z]/(x^3,y^2,z^5);
basis R
basis(-infinity,4,R)
basis(5,infinity,R)
basis(2,4,R)
