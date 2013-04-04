-- we want some hash codes to stay the same forever, so cached example output gets the same hashcodes
-- if the assertions here fail, it means some new types were created too early in the file Macaulay2/d/tokens.d
assert = x -> if not x then error "assertion failed "
assert ( hash {("a","bcd"),("bcd","a")} == -2128549206 )

-- it would also be nice to keep the following hash codes the same
assert( (hash 123) === 123 )
assert( (hash "asdf") === 3003444 )
assert( (hash {1,2,3}) === 1669655998 )
assert( (hash (1,2,3)) === 1305590783 )
assert( (hash [1,2,3]) === 2125831184 )
assert( (hash (3/5)) === 6499208 )
assert( (hash new HashTable from { 1 => 2 , 3 => 4 }) === 839493619 )
assert( (hash true) === 444777 )
assert( (hash false) === 777333 )
assert( (hash null) === 333889 )
assert( (hash Nothing) === 1000069 )
assert( (hash (1 => 2)) === 716026890 )
assert( hash Nothing == 1000069 )
assert( (hash Boolean) === 1000035 )
assert( (hash true) === 444777 )
assert( (hash false) === 777333 )

-- these might change if our floating point implementation changes, but let's check anyway:
assert( hash 1.23p200 === -1502350228 {* big endian *} or hash 1.23p200 == -640232547 {* little endian *} )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/basictests hashcodes.okay"
-- End:
