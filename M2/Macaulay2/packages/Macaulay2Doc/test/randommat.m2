-- with 1.1 we no longer try to keep the random number generator from changing

end

-- let's make sure the random number generator doesn't change

assert( random (ZZ^3, ZZ^3) == matrix {{0, 2, 0}, {1, -9, 7}, {5, -9, 8}} )

-- and let's make sure that random matrices work over other rings

R = ZZ/101[x,y]/(x^2,y^2)
f = random(R^{2:0},R^{2:-1})
toString f
assert( f == matrix {{-39*x+30*y, 2*x-4*y}, {19*x-38*y, -36*x-16*y}})

S = QQ[x,y]/(x^2,y^2)
g = random(S^{2:0},S^{2:-1})
toString g
assert( g == matrix {{5/6*x-y, 9/4*x+7/3*y}, {-3/7*x-4/3*y, -9/10*x+1/4*y}} )

-- check random isomorphisms are isomorphisms

assert isIsomorphism random(ZZ^6,ZZ^6,MaximalRank=>true)
assert isInjective random(ZZ^6,ZZ^3,MaximalRank=>true)
assert isSurjective random(ZZ^3,ZZ^6,MaximalRank=>true)

R = ZZ/101[x,y]
assert isIsomorphism random(R^4,R^4,MaximalRank=>true)
assert isInjective random(R^6,R^3,MaximalRank=>true)
assert isSurjective random(R^3,R^6,MaximalRank=>true)

