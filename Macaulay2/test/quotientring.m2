if getenv "USER" == "dan" then exit 0

A = ZZ[x]
B = A/8
f = 6*x
g = 2*x+4*x
assert( f-g == 0 )
assert( f == g )
