-- 'res' used to fail here:
A = QQ[a,b,c]/c
C = res coker matrix{{a+1,b}}
g = map(A^1, A^{{-1}, {-1}}, {{b, a+1}})
assert(C.dd_1 == g or C.dd_1 == -g)
d = map(A^{-1,-1},A^{-2}, {{a+1}, {-b}})	       
assert(C.dd_2 == d or C.dd_2 == -d)
