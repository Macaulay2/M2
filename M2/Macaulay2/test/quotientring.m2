-- if getenv "USER" == "dan" then exit 0

A = ZZ[x]
B = A/8
f = 6*x
g = 2*x+4*x
f-g
assert( f-g == 0 )
f==g
assert( f == g )
-- Local Variables:
-- compile-command: "make quotientring.okay"
-- End:
