assert = x -> if not x then error "assertion failed"

g = x -> (3;4+5*6^return;8;error "oops")
h = x -> (3;for i from 1 to 3 do 4+5*6^return {i,x};8;error "oops")
p = x -> (h x;okay)

assert( {null, null, null, null} === for i to 3 list g i )
assert( {{1, 0}, {1, 1}, {1, 2}, {1, 3}} === for i to 3 list h i )
assert( {okay, okay, okay, okay} === for i to 3 list p i )
assert( p 4 === okay )

scan(3, x -> (return; error "oops"))
scan(3, (x) -> (return; error "oops"))
scan({(1,2),(1,2)}, (x,y) -> (return; error "oops"))

apply(3, x -> (return; error "oops"))
apply(3, (x) -> (return; error "oops"))
apply({(1,2),(1,2)}, (x,y) -> (return; error "oops"))
