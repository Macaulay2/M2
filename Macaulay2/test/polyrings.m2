comm := (f,g) -> ( f*g - g*f )
com := (f,g) -> assert( f*g == g*f )
del := (f,g,t) -> assert( f*g == g*f + t )

A = ZZ[a,b]
B = A[x,y]
assert( a - B_2 == 0 )
assert( b - B_3 == 0 )
com(a,b)
com(a,x)
com(b,y)

C = ZZ[x,dx,WeylAlgebra => x => dx]
del(dx,x,1)

D = C[t]
com(x,t)
com(dx,t)
del(D_2,D_1,1)

E = A[s,ds,WeylAlgebra => s => ds]
com(a*1_E,b*1_E)
com(a,s)
com(b,s)
com(a,ds)
com(b,ds)
del(ds,s,1)

F = C[r,dr,WeylAlgebra => r => dr]
del(dx*1_F,x*1_F,1)
del(dr,r,1)
com(dx,dr)
com(dx,r)
com(x,dr)
com(x,r)

G = ZZ[p,SkewCommutative => true]
assert( p^2 == 0 )
H = G[x]
assert( (p*1_H)^2 == 0 )
assert( x^2 != 0 )

G = ZZ[p]
assert( p^2 != 0 )
H = G[x,SkewCommutative => true]
assert( (p*1_H)^2 != 0 )
assert( x^2 == 0 )


G = ZZ[p,SkewCommutative => true]
assert( p^2 == 0 )
H = G[x,SkewCommutative => true]
assert( (p*1_H)^2 == 0 )
assert( x^2 == 0 )

