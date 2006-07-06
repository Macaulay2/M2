R = QQ[a..d];
I = ideal(a*b-c*d, a*c-b*d)
leadTerm I
R = ZZ[a..d][x,y,z];
I = ideal(a*x-b*y, x^3, y^3, z^3)
leadTerm I
