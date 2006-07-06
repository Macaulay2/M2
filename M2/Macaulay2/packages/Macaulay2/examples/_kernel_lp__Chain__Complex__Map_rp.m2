R = QQ[a..d]
I = ideal(a^3,b^3,c^3)
C = res coker gens I
D = res coker gens (I + ideal(a*b*c))
F = extend(D,C,map(D_0,C_0,1))
