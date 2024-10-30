needsPackage "WeylAlgebras"
needsPackage "AssociativeAlgebras"

D = ZZ/101[x,dx, WeylAlgebra => {x => dx}]
R = ZZ/101<|dy,y|>/(dy*y - 1 - y*dy)

f = map(R, D, {y,dy})

assert(f(dx*x) == y*dy+1)
assert(f(x*dx) == y*dy)

assert(f(dx*x^2) == y^2*dy+2*y)
assert(f(x^2*dx) == y^2*dy)
