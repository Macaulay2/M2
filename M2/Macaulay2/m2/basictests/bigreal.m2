assert = x -> if not x then error "assertion failed "

x = toRR(2/3)
y = toRR(5)

assert( -4*x < x )
assert( -3*x < x )
assert( -3*x < 2*x )
assert( x < 2*x )
assert( x < y )
assert( -x < y )
assert( x > -y )
assert( -x > -y )

assert( 1e2  === 100. )
assert( 1E2  === 100. )
assert( 1e+2 === 100. )
assert( 1E+2 === 100. )
