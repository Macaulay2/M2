-- compute center of skew ring
kk = ZZ/11
n = 4
R = kk[x_1..x_n, SkewCommutative => true]
z = center R
Z = source z
assert(numcols basis Z == 2^(n-1))

I = ideal Z
for i from 0 to numgens Z - 1 do assert(Z_i^2 == 0)

-- computes center of some commutative rings
assert(map(kk, kk) === center kk)

R = kk[a..c] / ideal {a^2 + b, b^2 + c^3, c^2 - a*b^2}
assert(map(R, R) === center R)