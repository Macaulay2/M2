-- see test/mut-mult-ops
-- fixed, 10-14-09, by adding in 'not implemented' checks.

m = mutableIdentity(QQ[x],6)
x*m
m = mutableIdentity(RR,6)
-- 3.2*m
m + m
m - m
-- m * m
-- 3 * m
