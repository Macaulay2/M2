R = ZZ[a..d];
F = R^3
f = b*F_0 + a*F_1
leadTerm f
R = ZZ[a..d, MonomialOrder => {GRevLex => 4, Position => Up}];
F = R^3
leadTerm(a*F_0 + a*F_1)
R = ZZ[a..d, MonomialOrder => {GRevLex => 4, Position => Down}];
F = R^3
leadTerm(a*F_0 + a*F_1)
R = ZZ[a..d, MonomialOrder => {GRevLex => 2, Position => Down, GRevLex => 2}];
F = R^3
leadTerm(a*F_0 + a*F_1)
leadTerm(b*F_0 + c^4*F_1)
leadTerm(c*F_0 + d^2*F_1)
R = ZZ[a..d, MonomialOrder => {Position => Down}];
F = R^3
leadTerm(a*F_0 + a*F_1)
leadTerm(b*F_0 + c^4*F_1)
leadTerm(c*F_0 + d^2*F_1)
