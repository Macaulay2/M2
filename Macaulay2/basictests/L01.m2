assert = x -> if not x then error "assertion failed "
f = x -> true
assert( true === f y )
assert( true === f (3,3) )
assert( true === f (y,3) )
