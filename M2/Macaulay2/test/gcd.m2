R = QQ[x,y]
d = 5*x^2*y+7*x^3-1/11*y^4
f = (3*x^3-x*y+y^2) * d
g = (3*x^3+x*y+y^2) * d
assert( degree ( gcd(f,g) // d ) == {0} )
-- Local Variables:
-- compile-command: "make gcd.okay"
-- End:
