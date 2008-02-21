-- these are the random numbers that came out in version 0.9.2
-- and they still come out in 1.1
kk = ZZ/101
f = random(kk^3,kk^3)
assert( f == map(kk^3, kk^3, {{42, 9, 50}, {-50, -15, 45}, {39, -22, -29}}) )
