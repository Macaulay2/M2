-- here we try to see whether resolutions of inohomogeneous modules really work

R = ZZ/101[a..f,MonomialOrder => Lex]

I = ideal(a,b,c)
J = ideal(b,c,d,e)
K = cokernel generators intersect(I,J)
time assert( I == ann Ext^3(K,R) )

I = ideal(a-1,b^2-3,c-d^2)
J = ideal(b,c,d-f,e+4)
K = cokernel generators intersect(I,J)
time assert( I == ann Ext^3(K,R) )

I = ideal(a^2-1,b^2-3+c*e,c-d^2)
J = ideal(b,c^2,d-f,e+4)
K = cokernel generators intersect(I,J)
time assert( I == ann Ext^3(K,R) )

S = R/(f-a^3+b*c+11)

assert( S === class a )

I = ideal(a,b,c)
J = ideal(b,c,d,e)
K = cokernel generators intersect(I,J)
time assert( I == ann Ext^3(K,S) )

I = ideal(a-1,b-11)
J = ideal(b-3,c-4,e)
K = cokernel generators intersect(I,J)
time assert( I == ann Ext^2(K,S) )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test inhom.out"
-- End:
