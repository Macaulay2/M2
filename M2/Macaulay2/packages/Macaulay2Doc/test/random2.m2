-- these are the random numbers that came out in version 0.9.2
-- and they still come out in 1.1
-- and now, before 1.7, they change for finite fields, as we have changed
-- finite field implementation.
setRandomSeed()
kk = ZZ/101
f = random(kk^3,kk^3)
assert( f == map(kk^3, kk^3,{{-26, -29, -2}, {47, -4, 9}, {-34, -42, 11}}) )
--old version: assert( f == map(kk^3, kk^3, {{42, 9, 50}, {-50, -15, 45}, {39, -22, -29}}) )
