-- test galois substitution

A = ZZ/5[c]/(c^2+c+1)
assert( baseName c === symbol c )
k = GF(A,PrimitiveElement => - c - 2)
assert( class c === k )
assert( baseName c === symbol c )
e = 16
assert( k.PrimitiveElement^e == c )

-- three cases:

-- 1.  both rings are k-algebras, and we don't specify where c goes

p = map(k,k)
assert(p c == c)

R = k[x..z]

p = map(R,R)
assert( p (c*x-3) == (c*x-3) )

p = map(R,R,{z,y,x})
assert( p (c*x-3) == (c*z-3) )

-- 2.  both rings are k-algebras, but we specify where c goes

p = map(k,k,{c})
assert(p c == c)

p = map(R,R,{x,y,z,c})
assert( p (c*x-3) == (c*x-3) )

p = map(R,R,{z,y,x,c})
assert( p (c*x-3) == (c*z-3) )

-- 3.  target is not c k-algebra, but it has a generator of the same name

S = ZZ[c]
p = map(S,k)

-- 4.  target is not a k-algebra, and it has no generator of the same name

S = ZZ[a]
p = map(S,k)
use k
p c
--assert(p c == (-2_S)^e ) -- this behavior has changed (15 June 2014):
assert(p c == 0)

-- 5.  target is not a k-algebra, but we specify where c goes

S = ZZ[t]
p = map(S,k,{t})
--assert( p c == (-t-2)^e )  -- this behavior has changed (15 June 2014):
assert(p c == t)

S = ZZ[t,q,r]
p = map(S,R,{q*r,q^2,r^2,t})
--assert( p (c*x) == (-t-2)^e * q*r ) -- this behavior has changed (15 June 2014):
assert( p (c*x) == t*q*r)

-- # Local Variables:
-- # compile-command: "make -k -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test galois1.out "
-- # End:
