k = GF 4
a = k_0
R = k[x]

S = R/x^4
p = map(k,S)
assert( p(a+x) == a )
assert( p(1+x) == 1 )
assert( p(x) == 0 )

use R
x = promote(x,R/x^3)
p = map(k,ring x)
assert( p(a+x) == a )
assert( p(1+x) == 1 )
assert( p(x) == 0 )

R = QQ[x..z]/x^3
p = map(QQ,R)
assert( p(1/2 + x) == 1/2)

QQ[s]
substitute(s^2,{s => 4})

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ringmap3.out"
-- End:
