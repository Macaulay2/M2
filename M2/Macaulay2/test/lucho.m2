Q = ZZ/101[a,b]
h = random (Q^3, Q^{-2,-2})
f1 = a^3
f2 = b^4
R = cokernel matrix {{f1, f2}}
N = prune (cokernel h ** R)
E = resolution N
d = E.dd
i = id_E
s1 = nullhomotopy (f1*i)
s2 = nullhomotopy (f2*i)

assert( d^2 == 0 )
assert( d*s1 + s1*d == f1 )
assert( d*s2 + s2*d == f2 )
assert( s1^2 == 0 )
assert( s2^2 == 0 )
assert( s2*s1 + s1*s2 == 0 )

S = ZZ/101[a,b,X1,X2,Degrees=>{1,1,2,2}]/(a^3,b^4)

d = substitute(sum E.dd, S)
S1 = substitute(sum s1, S)
S2 = substitute(sum s2, S)

D = transpose( d - X1 * S1 - X2 * S2 )

D = substitute(D, {a=>0, b=>0})

n = rank E

D = map(S^n, S^n, entries D, Degree => 2)

-- res homology(D,D)
