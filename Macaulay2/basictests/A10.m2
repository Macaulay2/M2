assert = x -> if not x then error "assertion failed "

-- test #

x = hashTable {}
y = hashTable {(a,x)}

assert( #y.a === 0 )
