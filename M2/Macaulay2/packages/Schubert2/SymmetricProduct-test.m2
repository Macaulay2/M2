needs "Schubert2/SymmetricProduct.m2"

g = 5
X = CxCd(g,g-1)
Cd = target X.projections_1
euler Cd
assert( oo === 70 )			-- just recording the result we got ...
ch Cd.TangentBundle
use ring oo
assert( ch Cd.TangentBundle === 4-theta-x*theta-(1/2)*x^2*theta-(1/6)*x^3*theta )
todd OO(X.UniversalDivisor)

X.UniversalDivisor
assert(X.UniversalDivisor == x' + gamma + 4*eta)

X = CxCd(g,2)
Cd = target X.projections_1
euler Cd
assert( oo === 28 )
