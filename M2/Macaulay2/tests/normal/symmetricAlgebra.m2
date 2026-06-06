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

--
R = ZZ/101[s,t]
J = image matrix {{s^4, s^3*t, s*t^3, t^4}}
S = symmetricAlgebra J
assert( (ideal S)_* == {-t*p_0+s*p_1, -t*p_2+s*p_3, -t^2*p_1+s^2*p_2} )
S = symmetricAlgebra(J, Variables => vars(0..3))
assert( (ideal S)_* == {-t*a+s*b, -t*c+s*d, -t^2*b+s^2*c} )
