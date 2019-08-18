i = id_(ZZ^1)
f = 2 * i ++ 6 * i ++ 30 * i
sortColumns f
f = f _ oo
assert( sortColumns f === toList ( 0 .. 2) )

f = matrix {{0, 0, 1}, {0, 3, 2}, {390, 103, 251}}
sortColumns f
f = f _ oo
sortColumns f
assert( sortColumns f === toList ( 0 .. 2) )

R = ZZ[a..d]
m = matrix{{a,0,0,d},{0,0,c,0},{b^2,b^2,2*b^2,0}}
sortColumns m
m = m_oo
sortColumns m
assert( sortColumns m === toList ( 0 .. 3) )
