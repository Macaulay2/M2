A = ZZ/5[a]/(a^3-a-2)
B = GF(A,Variable=>a)
C = ZZ/5[b]/(b^3+1+3*b^2+b)
D = GF C
map(B,D,{a^2})
