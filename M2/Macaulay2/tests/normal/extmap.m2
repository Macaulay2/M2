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
f = inducedMap(M,N)
g = Ext^2(f,R)
assert( g - a*b*c*d == 0 )

I = intersect(ideal(a,b), ideal(c,d))
J = ideal apply(first entries gens I, r -> r^2)
M = R^1/I
N = R^1/J
f = inducedMap(M,N)
g = Ext^2(f,R)
so = m -> m_(sortColumns m)
print presentation target g
h = matrix{{a^2,b^2,0,0},{0,0,c^2,d^2}}
assert(
     so presentation target g - so h == 0 
     or
     so presentation target g - so h^{1,0} == 0 
     )
print presentation source g
h2 = matrix {{a, b, 0, 0}, {0, 0, c, d}}
assert(
     so presentation source g - so h2 == 0
     or
     so presentation source g - so h2^{1,0} == 0
     )
h3 = matrix {{a*b, 0}, {0, c*d}}
assert(
     so matrix g - so h3 == 0 
     or
     so matrix g - so h3^{1,0} == 0 
     )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test extmap.out"
-- End:
