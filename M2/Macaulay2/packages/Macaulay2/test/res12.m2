-- 'res' used to fail here:
A = QQ[a,b,c]/c
C = res coker matrix{{a+1,b}}
assert(C.dd_1 == map(A^1, A^{{-1}, {-1}}, {{b, a+1}}))
assert(C.dd_2 == map(A^{-1,-1},A^{-2}, {{a+1}, {-b}}))
