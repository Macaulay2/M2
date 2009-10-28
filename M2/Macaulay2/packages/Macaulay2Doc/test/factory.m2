-- I was working on the interface to factory and intended to get it done before the 1.2 release
-- plan B : try ntl or pari instead
A = QQ[a]
f = a^2+1
k = A/f
k = toField k
R = k[x]
d = x-a
p = d*(x^5-a)
q = d*(x^7-a)
-- deferred:
-- gcd(p,q)						    -- to do
-- assert( gcd(p,q) == d )

--
k = GF 9
R = k[x]
d = x-a
p = d*(x^5-a)
q = d*(x^7-a)
gcd(p,q)
assert( gcd(p,q) == d )
-- test deferred, trying to get it to work
-- factor p						    -- not implemented yet
