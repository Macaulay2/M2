-- check functoriality of symmetricAlgebra
R = QQ[x..z]
f = vars R
M = target f
M.cache.foo = bar
g = map(dual source f, M, transpose vars R)
(source g).cache.foo
assert( source g === target f )
assert( source symmetricAlgebra g === target symmetricAlgebra f )
assert( symmetricAlgebra g * symmetricAlgebra f === symmetricAlgebra (g*f) )
