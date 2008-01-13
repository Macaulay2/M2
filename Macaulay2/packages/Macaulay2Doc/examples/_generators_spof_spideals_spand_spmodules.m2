R = QQ[a..d];
I = ideal(a^3, b^3-c^3, a^4, a*c);
numgens I
I_0, I_2
J = trim I
J_0
M = cokernel matrix{{a,b},{c,d}}
M_0
M/M_0
N = M/(a*M + R*M_0)
N_0 == 0_N
