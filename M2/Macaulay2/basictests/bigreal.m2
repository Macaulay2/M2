assert = x -> if not x then error "assertion failed "

x = toRRR(2/3)
y = toRRR(5)

assert( -4*x < x )
assert( -3*x < x )
assert( -3*x < 2*x )
assert( x < 2*x )
assert( x < y )
assert( -x < y )
assert( x > -y )
assert( -x > -y )
