needsPackage "WeylAlgebras"
needsPackage "AssociativeAlgebras"

D = ZZ/101[x,dx, WeylAlgebra => {x => dx}]
R = ZZ/101<|dy,y|>/(dy*y - 1 - y*dy)
S = ZZ/101<|z,dz|>/(dz*z - 1 - z*dz)

f = map(R, D, {y,dy})
g = map(D, R, {dx,x})
h = map(S, R, {dz,z})

assert(f(dx*x) == y*dy+1)
assert(f(x*dx) == y*dy)

assert(f(dx*x^2) == y^2*dy+2*y)
assert(f(x^2*dx) == y^2*dy)

assert(g(dy*y) == x*dx+1)
assert(g(y*dy) == x*dx)

assert(g(dy*y^2) == x^2*dx+2*x)
assert(g(y^2*dy) == x^2*dx)

assert(h(dy*y) == z*dz+1)
assert(h(y*dy) == z*dz)

assert(h(dy*y^2) == z^2*dz+2*z)
assert(h(y^2*dy) == z^2*dz)

