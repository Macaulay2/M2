importFrom_Core "BinaryPowerMethod"
plus' = (x,y) -> ( z := x+y ; if z == 0 then continue ; z )
Poly = new Type of HashTable
assert( (one = new Poly from {1 => 1}) === new Poly from {1 => 1} )
Poly#1 = x -> one
assert( (T = new Poly from {2 => 1}) === new Poly from {2 => 1} )
- Poly := (f) -> applyValues(f,minus);
Poly + Poly := (f,g) -> merge(f,g,plus');
Poly - Poly := (f,g) -> f + -g;
Poly * Poly := (f,g) -> combine(f,g,times,times,plus');
Poly ^ ZZ := BinaryPowerMethod
assert( (f = T^0+T^1+T^2+T^3+T^4+T^5) === new Poly from {32 => 1, 16 => 1, 8 => 1, 1 => 1, 2 => 1, 4 => 1} )
assert( ((T-one)*f) === new Poly from {64 => 1, 1 => -1} )
assert( (f - (T^3 + T^5)) === new Poly from {16 => 1, 1 => 1, 2 => 1, 4 => 1} )
assert( (tally {1,1,1,2,4} - tally {1,2}) === new Tally from {4 => 1, 1 => 2} )
assert( (tally {1,1,1,2,4} ? tally {1,2}) === symbol > )
assert( (tally {1,2} ? tally {1,1,1,2,4}) === symbol < )
assert( (tally {1,1,1,2,4,10} ? tally {1,2,11}) === incomparable )
assert( (tally {1,2,10} ? tally {1,1,1,2,4,11}) === incomparable )
assert( (tally {1,1,1,2,4} ? tally {1,1,1,2,4}) === symbol == )
assert( ( - (new VirtualTally from tally {1,2,3,4})) === new VirtualTally from {1 => -1, 2 => -1, 3 => -1, 4 => -1} )
assert( (r = (new VirtualTally from tally {1,1,1,2,4}) - (new VirtualTally from tally {1,2,3,4})) === new VirtualTally from {1 => 2, 3 => -1} )
assert( (r + new VirtualTally from tally {1,1,4}) === new VirtualTally from {4 => 1, 1 => 4, 3 => -1} )

assert( class(tally{1,2,3}+set{3,4,5}) === Tally )
assert( mutable( merge(new Type of List, new Type of Ring, first) ) )
assert( class((new VirtualTally from {1 => -1, 2 => -2, 3 => -3}) + tally {1,2,3}) === VirtualTally )

t = hashTable{(1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8)}
assert(selectKeys(t, odd) === hashTable {(1, 1), (3, 2), (5, 5)})
assert(selectKeys(2, t, odd) === hashTable {(1, 1), (3, 2)})
assert(selectValues(t, odd) === hashTable {(1, 1), (2, 1), (4, 3), (5, 5)})
assert(selectValues(2, t, odd) === hashTable {(1, 1), (2, 1)})
assert(select(t, odd) === hashTable {(1, 1), (2, 1), (4, 3), (5, 5)})
assert(select(2, t, odd) === hashTable {(1, 1), (2, 1)})
assert(selectPairs(t, (k,v) -> odd(k+v)) === hashTable {(2, 1), (3, 2), (4, 3)})
assert(selectPairs(2, t, (k,v) -> odd(k+v)) === hashTable {(2, 1), (3, 2)})

x = set(1..10)
assert(select(x, odd) === set {1, 3, 5, 7, 9})
assert(select(2, x, odd) == set {1, 3})

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test hashtables.out"
-- End:
