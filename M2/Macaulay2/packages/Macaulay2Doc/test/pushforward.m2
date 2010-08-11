R = QQ[x]
S = R[y]
T = S[z]
f = map(R,R,{x^2})
g = map(S,R)
h = map(T,S)
m = map(R^1,R^1,f,{{1}})
assert isHomogeneous m
p = map(S^1,R^1,g,{{y}},Degree => degree y)
assert isHomogeneous p
q = map(T^1,S^1,h,{{z^2}},Degree => degree z^2)
assert isHomogeneous q
assert( m.RingMap === f )
n = m*m
assert(n.RingMap x == x^4)
assert isHomogeneous n
r = q*p
assert( r.RingMap x == x )
assert( degree r == degree q + q.RingMap.cache.DegreeMap degree p )
assert( isHomogeneous r )

m' = map(R^1,f)
assert( m === m' )

errorDepth = 0
isHomogeneous (p|p)
isHomogeneous (q|q)
isHomogeneous (p||p)
isHomogeneous (q||q)

end

isHomogeneous matrix {{p},{p}}
isHomogeneous matrix {{p,p}}

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test pushforward.out"
-- End:
