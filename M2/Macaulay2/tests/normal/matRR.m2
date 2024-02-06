matrix{{1.0,3.2}}
matrix(RR,{{1.2}})
matrix(CC,{{1.2}})
matrix{{1.0+ii,3.2+.3*ii}}


-- issue #2405
f = matrix{{1.3, 1.4}, {1.1, 1.5}, {.3, .6}}
M = image f
assert(M == M)
assert(M == image f)
