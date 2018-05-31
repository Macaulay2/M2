plus' = (x,y) -> ( z := x+y ; if z == 0 then continue ; z )
Poly = new Type of HashTable
assert( (one = new Poly from {1 => 1}) === new Poly from {1 => 1} )
assert( (T = new Poly from {2 => 1}) === new Poly from {2 => 1} )
- Poly := (f) -> applyValues(f,minus);
Poly + Poly := (f,g) -> merge(f,g,plus');
Poly - Poly := (f,g) -> f + -g;
Poly * Poly := (f,g) -> combine(f,g,times,times,plus');
Poly ^ ZZ := lookup(symbol ^,CC,ZZ)
assert( (f = one+T+T^2+T^3+T^4+T^5) === new Poly from {32 => 1, 16 => 1, 8 => 1, 1 => 1, 2 => 1, 4 => 1} )
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

end

print generateAssertions ///
plus' = (x,y) -> ( z := x+y ; if z == 0 then continue ; z )
Poly = new Type of HashTable
one = new Poly from {1 => 1}
T = new Poly from {2 => 1}
- Poly := (f) -> applyValues(f,minus);
Poly + Poly := (f,g) -> merge(f,g,plus');
Poly - Poly := (f,g) -> f + -g;
Poly * Poly := (f,g) -> combine(f,g,times,times,plus');
Poly ^ ZZ := lookup(symbol ^,CC,ZZ)
f = one+T+T^2+T^3+T^4+T^5
(T-one)*f
f - (T^3 + T^5)
tally {1,1,1,2,4} - tally {1,2}
tally {1,1,1,2,4} ? tally {1,2}
tally {1,2} ? tally {1,1,1,2,4}
tally {1,1,1,2,4,10} ? tally {1,2,11}
tally {1,2,10} ? tally {1,1,1,2,4,11}
tally {1,1,1,2,4} ? tally {1,1,1,2,4}
 - (new VirtualTally from tally {1,2,3,4})
r = (new VirtualTally from tally {1,1,1,2,4}) - (new VirtualTally from tally {1,2,3,4})
r + new VirtualTally from tally {1,1,4}
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test hashtables.out"
-- End:
