assert = x -> if not x then error "assertion failed "
f = x -> 33
assert( 33 === f() )
assert( 33 === f(3,3) )
assert( 33 === f(y,3) )

-- The statement above failed under linux alpha with gcc 2.95.3.
-- Fix: turn off optimization for Macaulay2/d files.
