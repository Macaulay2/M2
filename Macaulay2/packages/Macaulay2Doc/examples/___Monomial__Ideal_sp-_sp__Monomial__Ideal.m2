R = QQ[a..d];
I = monomialIdeal(a^3,b^2,a*b*c)
J = monomialIdeal(a^2,b^3,a*b*c)
I - J
J - I
I - (I-J)
