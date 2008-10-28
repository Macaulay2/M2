--status: I'm working on the interface to factory and intend to get it done before the 1.2 release
A = QQ[a]
f = a^2+1
k = A/f
toField k
R = k[x]
d = x-a
p = d*(x^5-a)
q = d*(x^7-a)
debug Core
rawGCD(raw p,raw q,raw f)
gcd(p,q)
assert( gcd(p,q) == d )

S = QQ[a,x]

F = ambient first flattenRing(R,CoefficientRing=>QQ)
p = lift(p,F)
q = lift(q,F)

