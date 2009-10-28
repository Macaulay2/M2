-- After changing the choice of gb for 'toField' rings, the
-- folllowing fails:

R = ZZ/101[a]
F = a^2+a+1
factor F
A = R/F
toField A
1/a
(a-1) * (1/(a-1))
1//a
1//(1-a)

R = ZZ/101[a]/(a^2-1)
toField R
1/a
1//a
1//(1-a)
1/(a-1)

-- bug: one of these operations should say something about finding a zero divisor!!
