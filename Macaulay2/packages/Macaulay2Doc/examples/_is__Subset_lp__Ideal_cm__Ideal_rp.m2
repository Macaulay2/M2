R = QQ[a..d];
I = ideal(a^2-b*c-1,a*c-1,b^3-1);
isSubset(I^2,I)
isSubset(I,I^2)
