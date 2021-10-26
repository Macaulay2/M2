assert = x -> if not x then error "assertion failed "


x = 28343/734534
y = 33234345
z = -2345/1137434

assert( x*y*z == z*y*x )
assert( (x+y)*z == x*z + y*z )
assert( (x+z)*y == x*y + z*y )
assert( 1/8 + 36 == (36 * 8 + 1)/8 )
assert( 36+1/8 == (36 * 8 + 1)/8 )

assert( floor0 10000.1 == 10000 )
assert( floor0 (-10000.1) == -10001 )
assert( floor0 ( 2.^33 + .4 ) == 2^33 )
assert( floor0 ( -2.^33 + .4 ) == -2^33 )

assert( #environment > 0 )
assert( #commandLine > 0 )
