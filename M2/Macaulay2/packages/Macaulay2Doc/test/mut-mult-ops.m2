-- see test/mut-mult-ops
-- fixed, 10-14-09, by adding in 'not implemented' checks.

m = mutableIdentity(QQ[x],6)
assert try (x*m) else true  -- used to crash

m = mutableIdentity(RR,6)
assert try (3.2*m) else true 

assert try m + m else true
assert try m - m else true
assert try m * m else true
assert try 3 * m else true
