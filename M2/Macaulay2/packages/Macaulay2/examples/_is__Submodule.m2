R = ZZ/5[a,b,c];
M = R^3;
isSubmodule M
N = ideal(a,b) * M
isSubmodule N
N' = ideal(a,b) * (R^1 / ideal(a^2,b^2,c^2))
isSubmodule N'
