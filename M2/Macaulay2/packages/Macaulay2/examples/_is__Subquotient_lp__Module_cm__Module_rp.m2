R = QQ[a..d];
N = coker matrix{{a,b},{c,d}}
N1 = N/(a^4*N)
M = a*N/(R*a*N_0+a*b*N)
isSubquotient(M,N)
isSubquotient(M,N1)     
