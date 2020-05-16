--status: this old test depends on internal things and probably should be deleted


needs "raw-util.m2"
-- This used to cause problems
R = polyring(rawZZ(), (vars 0 .. vars 15))
m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),0)

m2 = m*m
m3 = m2*m
m4 = m2*m2
m4b = m3*m
m4b
m4

assert(m4 == m4b)

m2 = m*m
m3 = m2*m
m3b = m*m2
assert(m3 == m3b)

m2 = m*m
m3 = m2*m
m3b = m2*m
assert(m3 == m3b)

m2 = m*m
m2b = m*m
assert(m2 == m2b)

m4c = m*m3
assert(m4 == m4b)
assert(m4b == m4c)


