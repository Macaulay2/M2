R1 = ZZ/101[x_0 .. x_10]
R3 = ZZ/101[x_0 .. x_8][x_9,x_10]

F3 = map(R3,R1)
F3inv = map(R1,R3) -- fails
F3 * F3inv
assert((F3inv * F3) (vars R1) ==  vars R1)

-- ringmap for towers

R = QQ[x][y]
F = frac R
f = map(F,F)
g = map(F,F,{y+1})
h = map(F,F,{y+1,x-1})
x = promote(x,F)
y = promote(y,F)
assert( f x == x )
assert( f y == y )
assert( g x == x )
assert( g y == y + 1 )
assert( h x == x - 1)
assert( h y == y + 1 )

-- common ground rings should give identity

A = ZZ[a]

R = A[x]
S = A[y]
f = map(R,S,{x^2})
use A
assert( f promote(a,S) == promote(a,R) )

S = A[y]
R = A[a]
f = map(R,S,{a^2})
use A
assert( f promote(a,S) == promote(a,R) )

S = A[a]
R = A[x]
f = map(R,S,{x^2})
use A
assert( f promote(a,S) == promote(a,R) )

S = A[x]
R = A[a]
f = map(R,S,{a^2})
use A
assert( f promote(a,S) == promote(a,R) )

S = A[a]
R = A[a]
f = map(R,S,{a^2})
use A
assert( f promote(a,S) == promote(a,R) )

-- a few more

A = ZZ[x]
B = A[x]
f = map(B,B,{x^2})
x' = promote(A_0,B)
assert( x != x' )
assert( f x == x^2 )
assert( f x' == x' )

C = ZZ[x][y]
g = map(ZZ,C,{2})
assert ( g y == 2 )
assert ( g promote(x,C) == 0 )

-- maps between fraction rings of quotient rings

R = frac(QQ[t])
S = frac(QQ[x,y]/(x^3-y^2))
p = map(R, S,{t^2,t^3})
assert( p S_"x" == t^2 )
assert( p S_"y" == t^3 )


R = frac(QQ[x,y])
S = frac(QQ[x])
p = map(R, S)
assert( p S_"x" == R_"x" )


R = frac(QQ[x,y]/(y^2+1))
S = frac(QQ[x])
p = map(R, S)
assert( p S_"x" == R_"x" )


R = frac(QQ[x,y]/(x^2+1))
S = frac(QQ[x]/(x^2+1))
p = map(R, S)
assert( p S_"x" == R_"x" )

