assert = x -> if not x then error "assertion failed "

-- test #

x = hashTable {}
y = hashTable {(a,x)}

assert( #y.a === 0 )

-- cache tables

c1 = new CacheTable from { }
c2 = new CacheTable from { }
assert( c1 === c2 )

c1 = new CacheTable from { a => 2 }
c2 = new CacheTable from { b => 3 }
assert( c1 === c2 )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/basictests A10.out"
-- End:
