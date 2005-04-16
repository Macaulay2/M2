R = ZZ/101[a,b,c];
M = R^3;
N = ideal(a,b) * M
isSubmodule N
N1 = ideal(a,b) * (R^1 / ideal(a^2,b^2,c^2))
isSubmodule N1
