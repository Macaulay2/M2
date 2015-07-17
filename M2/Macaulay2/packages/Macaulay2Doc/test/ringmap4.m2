R1 = ZZ/101[x_0 .. x_10]
R3 = ZZ/101[x_0 .. x_8][x_9,x_10,Join=>false]

F3 = map(R3,R1)
F3inv = map(R1,R3)
F3 * F3inv
assert((F3inv * F3) vars R1 ==  vars R1)
assert(F3inv F3 vars R1 ==  vars R1)

-- ringmap for towers

R = QQ[x][y]
f = map(R,R)
g = map(R,R,{y+1})
h = map(R,R,{y+1,x-1})
x = promote(x,R)
y = promote(y,R)
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

X = symbol x
X' = local x
assert( X =!= X' )
assert( toString X == toString X' )
R = QQ[X,X']
S = QQ[X]
T = QQ[X']
f = map(R,S)
g = map(R,T)
h = map(S,T)
k = map(T,S)
assert( f S_X == R_X )
assert( g T_X' == R_X' )

-- assert( h T_X' == S_X ) -- only if we match by name, too
assert( h T_X' == 0 )

-- assert( k S_X == T_X' ) -- only if we match by name, too
assert( k S_X == 0 )

map(S,R)						    -- no collision, no error
-- assert try ( map(S,R) ; false ) else true		    -- collision detection should give error
map(T,R)						    -- no collision, no error
-- assert try ( map(T,R) ; false ) else true		    -- collision detection should give error

-- use different symbols
R = QQ[X][X']
S = QQ[X]
T = QQ[X']
f = map(R,S)
g = map(R,T)
h = map(S,T)
k = map(T,S)
assert( f S_X == R_X )
assert( g T_X' == R_X' )
assert( h T_X' == 0 )
assert( k S_X == 0 )
map(S,R)
map(T,R)

-- now use the same symbols
R = QQ[X][X]
S = QQ[X]
T = QQ[X]
f = map(R,S)
g = map(R,T)
h = map(S,T)
k = map(T,S)
assert( f S_X == R_X )
assert( g T_X == R_X )
assert( h T_X == S_X )
assert( k S_X == T_X)
assert try ( map(T,R) ; false ) else true		    -- collision detection should give error
assert try ( map(S,R) ; false ) else true		    -- collision detection should give error

-- isWellDefined RingMap

k = GF (P = ZZ/3[a]/(a^2-2), PrimitiveElement => a+1)	    -- we specify the primitive element, because otherwise it's chosen randomly
r = map(k,k,{a+1})					    -- this map is not well defined

{*
  -- This functionality has changed
  -- and this not-well-defined behavior is no longer valid.
  -- In particular, 
pe = promote(k.PrimitiveElement,k)
assert ( pe == a+1 )
r pe == (a+1)+1				  -- it works for the primitive element
a == (a+1)^6						    -- and for others, when expressed as powers of the primitive one
r a == ((a+1)+1)^6

U = ZZ[x]; netList apply(13,i -> (hold pe)^(toString i) == (hold k.PrimitiveElement^i) => factor (map(U,k,{x})) k.PrimitiveElement^i)

a^2+1 == 0
(r a)^2+1 == 0						    -- the map looks well defined on "a", which is not a primitive element

pe+pe^2-1 == 0
(r pe)+(r pe)^2-1 == 0					    -- the map looks ill defined on "pe", the primitive element

pe^2 + a == 0
(r pe)^2 + a == 0
*}

-- deferred
-- assert( not isWellDefined r )

R = k[x,y]/x^2
S = k[t,u]/t^3
assert( x === R_0 )
assert( y === R_1 )
assert( t === S_0 )
assert( u === S_1 )

assert isWellDefined map(R,S,{x,y})
assert not isWellDefined map(S,R,{t,u})
assert isWellDefined map(R,S,{x,y,a})
assert isWellDefined map(R,S,{x,y,-a})
q = map(R,S,{x,y,a+1})
-- deferred:
-- assert not isWellDefined q


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ringmap4.out"
-- End:
