a = {1,2,3}
b = {3,4,5}
f = x -> x+1
g = x -> x+2
h = (x,y) -> x+y
assert( sum(a,f) == 9 )
assert( product(a,f) == 24 )
assert( sum(3,g) == 9 )
assert( product(3,g) == 24 )
assert( sum(a,b,h) == 18 )
assert( product(a,b,h) == 192 )

