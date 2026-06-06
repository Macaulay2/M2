R = ZZ/101[a,r..z]
f = a  * id_(R^1)
g = a^2* id_(R^1)
assert( degree f == {1} )
assert isHomogeneous f
assert( degree g == {2} )
assert isHomogeneous g
h = g // f
assert( degree h == {1} )
assert isHomogeneous h

chk = (f,g) -> assert( target f == target g and (h := f//g; target h == source g and source h == source f and f % g + g * h == f ))
M = R^2
N = R^3
chk(map(M,N,0),map(M,M,1))
chk(map(M,M,1),map(M,N,0))
chk(map(M,M,{{r,s},{t,u}}),map(M,N,{{r,s,t},{u,v,w}}))


f = map(R^2,,{{x},{y}})
g = map(R^2,,{{x},{z}})
(q,r) = quotientRemainder(f,g)
assert( g*q+r == f )
assert( degree g == {0} )
assert( degree q == {0} )
assert( degree f == {0} )

R = ZZ[x,d,WeylAlgebra=>{x=>d}]
f = matrix{{x*d,d*x,x,d}}
g = matrix{{x,d}}
(q,r) = quotientRemainder(f,g)
assert(g*q+r == f)
g = matrix{{x}}
(q,r) = quotientRemainder(f,g)
assert(g*q+r == f)
g = matrix{{d}}
(q,r) = quotientRemainder(f,g)
assert(g*q+r == f)

a =-1.428571429e-01
lift(a,QQ)
assert( instance(oo,QQ) and a === promote(oo,RR) )
a = 4.537815126e-01
lift(a,QQ)
assert( instance(oo,QQ) and a === promote(oo,RR) )
a = 5.923344948e-02
lift(a,QQ)
assert( instance(oo,QQ) and a === promote(oo,RR) )
a = -7.481504637e-02
lift(a,QQ) 
assert( instance(oo,QQ) and a === promote(oo,RR) )
a = -1.984139325e-01
lift(a,QQ)
assert( instance(oo,QQ) and a === promote(oo,RR) )

-- from github issue #194
R=QQ[x,Inverses=>true,MonomialOrder=>RevLex]
assert not liftable(x-1,QQ)


assert(lift(2, ZZ) === 2)
assert(lift(2/1, ZZ) === 2)
assert(lift(2.0, ZZ) === 2)
assert(lift(toRRi 2, ZZ) === 2)
assert(lift(toCC 2, ZZ) === 2)
assert(lift(toCCi 2, ZZ) === 2)
scan({5/2, 2.5, toRRi 2.5, toCC 2.5, toRRi 2.5},
    x -> (
	assert not liftable(x, ZZ);
	lift(x, ZZ, Verify => false)))

assert(lift(5/2, QQ) === 5/2)
assert(lift(2.5, QQ) === 5/2)
assert(lift(toRRi 2.5, QQ) === 5/2)
assert(lift(toCC 2.5, QQ) === 5/2)
assert(lift(toCCi 2.5, QQ) === 5/2)
scan({toRRi(3, 4), ii, toCCi(toRRi(2, 3), toRRi(4, 5))},
    x -> (
	assert not liftable(x, QQ);
	lift(x, QQ, Verify => false)))

assert(lift(2.0, RR) === 2.0)
assert(lift(2.0, RR_100) === 2.0p100)
assert(lift(toRRi 2.0, RR) === 2.0)
assert(lift(toRRi 2.0, RR_100) === 2.0p100)
assert(lift(toCC 2.0, RR) === 2.0)
assert(lift(toCC 2.0, RR_100) === 2.0p100)
assert(lift(toCCi 2.0, RR) === 2.0)
assert(lift(toCCi 2.0, RR_100) === 2.0p100)
scan({toRRi(3, 4), ii, toCCi(toRRi(2, 3), toRRi(4, 5))},
    x -> (
	assert not liftable(x, RR);
	lift(x, RR, Verify => false)))

assert(lift(interval(2, 3), RRi) === toRRi(53, 2, 3))
assert(lift(interval(2, 3), RRi_100) === toRRi(100, 2, 3))
assert(lift(toCC(53, 2, 0), RRi) === toRRi(53, 2, 2))
assert(lift(toCC(53, 2, 0), RRi_100) === toRRi(100, 2, 2))
assert(lift(toCCi(53, interval(2, 3), 0), RRi) === toRRi(53, 2, 3))
assert(lift(toCCi(53, interval(2, 3), 0), RRi_100) === toRRi(100, 2, 3))
scan({ii, interval ii}, x -> (
	assert not liftable(x, RRi);
	lift(x, RRi, Verify => false)))

assert(lift(toCC(53, 2, 3), CC) == toCC(53, 2, 3))
assert(lift(toCC(53, 2, 3), CC_100) == toCC(100, 2, 3))
assert(lift(toCCi(53, 2, 3), CC) == toCC(53, 2, 3))
assert(lift(toCCi(53, 2, 3), CC_100) == toCC(100, 2, 3))
scan({toCCi(53, toRRi(2, 3), toRRi(3, 4))}, x -> (
	assert not liftable(x, CC);
	lift(x, CC, Verify => false)))

x = toCCi(53, toRRi(2, 3), toRRi(3, 4))
assert(lift(x, CCi) === x)
assert(lift(x, CCi_100) === toCCi(toRRi(100, 2, 3), toRRi(100, 3, 4)))

x = symbol x
assert(lift(1_(ZZ[x]), ZZ) === 1)
assert(lift(1_(QQ[x]), QQ) === 1/1)
assert(lift(1_(RR[x]), RR) === 1.0)
assert(lift(1_(RRi[x]), RRi) === toRRi 1)
assert(lift(1_(CC[x]), CC) === toCC 1)
assert(lift(1_(CCi[x]), CCi) === toCCi 1)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test lift.out"
-- End:
