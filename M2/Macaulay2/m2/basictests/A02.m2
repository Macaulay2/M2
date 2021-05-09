assert = x -> if not x then error "assertion failed"

-- test scopes

a = 1
g = () -> a := 2
g()
assert( a === 1 )

h = () -> a = 3
h()
assert( a === 3 )

k = () -> a
a := 4
assert( k() === 3 )
assert( a === 4 )
