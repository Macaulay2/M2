-- checking that random(GaloisField) truly produces a random element of the field
-- and not just powers of the generator.
kk = ZZ/32003
R = kk[q]
f = q^4 + q^3 + q^2 + q + 1
ll = R/(f)
LL = GF(ll)
-- not sure how to test this exactly, but there
-- should not be *any* duplicates very often
assert(#(unique apply(1000, i -> random LL)) > 995)
