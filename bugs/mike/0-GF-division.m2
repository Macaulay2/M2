-- After changing the choice of gb for 'toField' rings, the
-- following fails:

R = ZZ/101[a]
F = a^2+a+1
factor F
A = R/F
k = toField A
assert( isField k )
assert( class a === k )
1/a
assert( oo * a == 1 )
(a-1) * (1/(a-1))
assert( oo == 1 )
1//a
assert( oo * a == 1 )
1//(1-a)
assert( oo * (1-a) == 1 )

R = ZZ/101[a]/(a^2-1)
k = toField R
assert( isField k )
assert( class a === k )
1/a
assert( oo * a == 1 )
1//a
assert( oo == a )
1//(1-a)
assert( oo != 0 )					    -- still failing
1/(a-1)
assert( oo * (a-1) == 1 )				    -- still failing

-- bug: one of these operations should say something about finding a zero divisor!!
