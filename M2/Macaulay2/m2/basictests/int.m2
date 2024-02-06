assert = x -> if not x then error "assertion failed "

-- test integer literals in different bases
assert( 0b101010 == 42 )
assert( 0B101010 == 42 )
assert( 0o52 == 42 )
assert( 0O52 == 42 )
assert( 0x2a == 42 )
assert( 0x2A == 42 )
assert( 0X2a == 42 )
assert( 0X2A == 42 )
