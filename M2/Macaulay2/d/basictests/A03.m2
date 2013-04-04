
assert = x -> if not x then error "assertion failed "

-- test hash tables

x = new MutableHashTable
x#0 = a
x#1 = b

y = new HashTable from x

x#1 = c

assert ( y#1 == b )			  -- it should have copied x to create y

