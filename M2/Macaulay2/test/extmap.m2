R = ZZ/101[a..f,MonomialOrder => Lex]

I = ideal(a,b,c)
J = ideal(b,c,d,e)
J' = ideal(b,c^2,d^2+e*f,e)
K = cokernel generators intersect(I,J)
K' = cokernel generators intersect(I,J')
f = inducedMap(K,K')
g = Ext^3(f,R)
assert(ann source g == I)
assert(ann target g == I)
assert(g == 1)
h = Ext^3(K,matrix {{d}})
assert(h == d)

