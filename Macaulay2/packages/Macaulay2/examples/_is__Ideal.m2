S = QQ[x,y,z];
I = ideal(x^2, y^2)
isIdeal I
J = monomialIdeal I
isIdeal J
R = QQ[a..d]/(a*b*c*d);
I = ideal(a^2,b^2) * R^1
isIdeal I
J = a^2 * R^2 + a*b * R^2
isIdeal J
