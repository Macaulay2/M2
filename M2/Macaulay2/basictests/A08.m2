assert = x -> if not x then error "assertion failed "

-- test lookupCount
assert ( lookupCount something === 1 )
assert ( lookupCount something === 2 )
assert ( lookupCount something === 3 )
assert ( lookupCount something === 4 )

-- test erase
erase something
assert ( lookupCount something === 1 )
