-- let's make sure the random number generator doesn't change
-- but for finite fields, it changed right before 1.7
-- and this changes it for others too, if the one for finite fields is called.
setRandomSeed()

assert( random (ZZ^3, ZZ^3) == matrix {{4, 1, 6}, {3, 3, 5}, {8, 9, 6}} )
-- assert( random (ZZ^3, ZZ^3) == matrix {{0, 2, 0}, {1, -9, 7}, {5, -9, 8}} )

-- and let's make sure that random matrices work over other rings

R = ZZ/101[x,y]/(x^2,y^2)
f = random(R^{2:0},R^{2:-1})
toString f
assert( f == matrix {{47*x-16*y, 21*x-42*y}, {-17*x-23*y, 16*x-34*y}})
--old version: assert( f == matrix {{42*x-50*y, -15*x-22*y}, {39*x+9*y, 50*x+45*y}})

S = QQ[x,y]/(x^2,y^2)
g = random(S^{2:0},S^{2:-1})
toString g
assert( g == matrix {{10/3*x+7/4*y, 6/7*x+9/8*y}, {5/6*x+8*y, 3/4*x+8/7*y}})
-- old version: assert( g == matrix {{x+5/6*y, 3/7*x+4/5*y}, {8*x+2/5*y, 3/4*x+1/6*y}} )

-- check random isomorphisms are isomorphisms

assert isIsomorphism random(ZZ^6,ZZ^6,MaximalRank=>true)
assert isInjective random(ZZ^6,ZZ^3,MaximalRank=>true)
assert isSurjective random(ZZ^3,ZZ^6,MaximalRank=>true)

R = ZZ/101[x,y]
assert isIsomorphism random(R^4,R^4,MaximalRank=>true)
assert isInjective random(R^6,R^3,MaximalRank=>true)
assert isSurjective random(R^3,R^6,MaximalRank=>true)

