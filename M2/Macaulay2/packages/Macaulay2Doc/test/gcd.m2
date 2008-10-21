R = ZZ/32003[x,y]
f = (x+y)^3*(x-y^2)
g = (x+y)^2*(x^3-x*y+y^3)^4
assert ( gcd(f,g) == (x+y)^2 )

R = (GF 25)[x]
-- one of the book chapters depends on this feature
assert(gcd((x^3+x-a)*(x^5+x-1),(x^3+x-a)*(x^6-a*x-1)) == x^3+x-a)

R = (GF 25)[x,y]
-- one of the book chapters depends on this feature
assert(gcd((x^3+x-a)*(x^5+x-1),(x^3+x-a)*(x^6-a*x-1)) == x^3+x-a)

R = ZZ[x]
assert( gcd(6*x, 4*x) == 2*x )

R = QQ[x]
assert( gcd(6*x, 4*x) == x )

R = ZZ[x,y]
d = 5*x^2*y+7*x^3-y^4
f = (3*x^3-x*y+y^2) * d
g = (3*x^3+x*y+y^2) * d
h = gcd(f,g)
assert(h == -d)

R = QQ[x,y]
d = 5*x^2*y+7*x^3-1/11*y^4
f = (3*x^3-x*y+y^2) * d
g = (3*x^3+x*y+y^2) * d
h = gcd(f,g)
assert(h == -11* d)

debug Core
R = QQ[x,y]
f = 1+x^2
g = 1+x^3
rawGCD( raw ( f ), raw ( g ))
rawGCD( raw ( 1/2*f ), raw ( 1/3*g ))
rawExtendedGCD( raw ( f ), raw ( g ))
rawExtendedGCD( raw ( 1/2*f ), raw ( 1/3*g ))

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test gcd.out"
-- End:
