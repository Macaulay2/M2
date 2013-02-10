assert = x -> if not x then error "assertion failed "
assert( "a" === ascii 97 )
assert( "a" === ascii {97} )
assert( "ab" === ascii {97,98} )
assert( "ascii" === ascii ascii "ascii")
assert( ascii "ascii" === ascii ascii ascii "ascii")
