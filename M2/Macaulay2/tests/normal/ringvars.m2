S = ZZ/32003[a..i]
assert(class a === S)
debug Core
-- now a has been clobbered, for some reason
assert(class a === S)
-- even a 'use' doesn't help:
use S
assert(class a === S)

load "Markov.m2"
R = markovRing(2,2,2,2)
p_(1,1,1,1)
gens R
use R
R_0
assert(p_(1,1,1,1) == R_0)

