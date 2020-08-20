R = ZZ/32003[a..j]
I = ideal random(R^1, R^{-2,-2,-2,-2,-2,-2,-2});
gbTrace = 3
L = mingens I; -- this should stop after degree 2!!
assert(numgens source L <= 7)
