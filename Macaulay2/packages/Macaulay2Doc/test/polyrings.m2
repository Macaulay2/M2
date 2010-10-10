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

A = ZZ[a,b]/(a^3-b^3)
B = A[x,y]
assert(B_2^3 == B_3^3)

C = ZZ/3[x,dx,k,WeylAlgebra => x => dx]/(k^3-x^3)	    -- I made this ZZ/3 so x^3 would be in the center [dan]
del(dx,x,1)

D = C[t]
assert(D_1^3==D_3^3)
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

G = ZZ[p]/p^3
assert( p^2 != 0 )
H = G[x,SkewCommutative => true]
assert( (p*1_H)^2 != 0 )
assert( (p*1_H)^3 == 0 )
assert( x^2 == 0 )


G = ZZ[p,SkewCommutative => true]
assert( p^2 == 0 )
H = G[x,SkewCommutative => true]
assert( (p*1_H)^2 == 0 )
assert( x^2 == 0 )


R = QQ[x][y]
assert( degree x == {1} )
assert( degree x_R == {0,1} )
assert( degree y_R == {1,0} )
(options R).Heft
assert( (options R).Heft =!= null )

R = QQ[x][y, Join => false]
assert( degree x == {1} )
assert( degree x_R == {1} )
assert( degree y_R == {1} )
(options R).Heft
assert( (options R).Heft =!= null )

R = QQ[x][y, Join => false, DegreeMap => x -> 0]
assert( degree x == {1} )
assert( degree x_R == {0} )
assert( degree y_R == {1} )
(options R).Heft
assert( (options R).Heft === {1} )

R = QQ[x,Degrees => {{0,1}}]
(options R).Heft
assert( (options R).Heft === {0,1} )			    -- inessential, but it makes the tests below interesting
R = QQ[x,Degrees => {{1,0}}]
(options R).Heft
assert( (options R).Heft === {1,0} )

R = QQ[x][y,z,Degrees => {1,-1}]
(options R).Heft
assert( (options R).Heft === null )

M = tensor(monoid [x],monoid [y,z,Degrees => {1,-1}])
degrees M
(options M).Heft
assert( (options M).Heft === null )

M = tensor(monoid [y,z,Degrees => {1,-1}],monoid [x])
degrees M
(options M).Heft
assert( (options M).Heft === null )

R = QQ[y,z,Degrees => {1,-1}][x]
degree x, degree y_R, degree z_R
assert( (options R.FlatMonoid).Heft === null )

R = QQ[x,Degrees => {{1,0}}][y,Degrees => {{0,1}}]
(options R.FlatMonoid).Heft
assert( (options R.FlatMonoid).Heft === {0, 1, 1, 0} )

R = QQ[x][y]
(options R.FlatMonoid).Heft
assert( (options R.FlatMonoid).Heft === {1, 1} )
