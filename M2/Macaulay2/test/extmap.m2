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

R = ZZ/101[a..d]
I = ideal (a*b,c*d)
M = R^1/I
J = ideal (a^2*b^2,c^2*d^2)
N = R^1/J
f = map(M,N)
g = Ext^2(f,R)
assert( g - a*b*c*d == 0 )

I = intersect(ideal(a,b), ideal(c,d))
J = ideal apply(first entries gens I, r -> r^2)
M = R^1/I
N = R^1/J
f = map(M,N)
g = Ext^2(f,R)
assert( presentation target g - matrix{{a^2,b^2,0,0},{0,0,c^2,d^2}} == 0 )
assert( presentation source g - matrix {{a, b, 0, 0}, {0, 0, c, d}} == 0 )
assert( matrix g - matrix {{a*b, 0}, {0, c*d}} == 0 )
