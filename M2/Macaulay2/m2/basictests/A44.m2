assert = x -> if not x then error "assertion failed "

-- test drop

assert( drop( {a,b}, 3 ) === {} )
assert( drop( {a,b}, 2 ) === {} )
assert( drop( {a,b}, 1 ) === {b} )
assert( drop( {a,b}, 0 ) === {a,b} )
assert( drop( {a,b}, -1 ) === {a} )
assert( drop( {a,b}, -2 ) === {} )
assert( drop( {a,b}, -3 ) === {} )

assert( drop( (a,b), 3 ) === () )
assert( drop( (a,b), 2 ) === () )
assert( drop( (a,b), 1 ) === toSequence{b} )
assert( drop( (a,b), 0 ) === (a,b) )
assert( drop( (a,b), -1 ) === toSequence{a} )
assert( drop( (a,b), -2 ) === () )
assert( drop( (a,b), -3 ) === () )

assert( drop( {0,1,2,3,4,5,6}, {2,2} ) === {0,1,3,4,5,6} )
assert( drop( {0,1,2,3,4,5,6}, {2,1} ) === {0,1,2,3,4,5,6} )

assert( drop( {0,1,2,3,4,5,6}, {2,4} ) === {0,1,5,6} )
assert( drop( {0,1,2,3,4,5,6}, {0,4} ) === {5,6} )
assert( drop( {0,1,2,3,4,5,6}, {-1,4} ) === {5,6} )
assert( drop( {0,1,2,3,4,5,6}, {2,6} ) === {0,1} )
assert( drop( {0,1,2,3,4,5,6}, {2,7} ) === {0,1} )
assert( drop( {0,1,2,3,4,5,6}, {-1,7} ) === {} )

assert( drop( (0,1,2,3,4,5,6), {2,2} ) === (0,1,3,4,5,6) )
assert( drop( (0,1,2,3,4,5,6), {2,1} ) === (0,1,2,3,4,5,6) )

assert( drop( (0,1,2,3,4,5,6), {2,4} ) === (0,1,5,6) )
assert( drop( (0,1,2,3,4,5,6), {0,4} ) === (5,6) )
assert( drop( (0,1,2,3,4,5,6), {-1,4} ) === (5,6) )
assert( drop( (0,1,2,3,4,5,6), {2,6} ) === (0,1) )
assert( drop( (0,1,2,3,4,5,6), {2,7} ) === (0,1) )
assert( drop( (0,1,2,3,4,5,6), {-1,7} ) === () )

-- test take

assert( take( {a,b}, 3 ) === {a,b} )
assert( take( {a,b}, 2 ) === {a,b} )
assert( take( {a,b}, 1 ) === {a} )
assert( take( {a,b}, 0 ) === {} )
assert( take( {a,b}, -1 ) === {b} )
assert( take( {a,b}, -2 ) === {a,b} )
assert( take( {a,b}, -3 ) === {a,b} )

assert( take( (a,b), 3 ) === (a,b) )
assert( take( (a,b), 2 ) === (a,b) )
assert( take( (a,b), 1 ) === toSequence{a} )
assert( take( (a,b), 0 ) === () )
assert( take( (a,b), -1 ) === toSequence{b} )
assert( take( (a,b), -2 ) === (a,b) )
assert( take( (a,b), -3 ) === (a,b) )

assert( take( {0,1,2,3,4,5,6}, {2,2} ) === {2} )
assert( take( {0,1,2,3,4,5,6}, {2,1} ) === {} )

assert( take( {0,1,2,3,4,5,6}, {2,4} ) === {2,3,4} )
assert( take( {0,1,2,3,4,5,6}, {0,4} ) === {0,1,2,3,4} )
assert( take( {0,1,2,3,4,5,6}, {-1,4} ) === {0,1,2,3,4} )
assert( take( {0,1,2,3,4,5,6}, {2,6} ) === {2,3,4,5,6} )
assert( take( {0,1,2,3,4,5,6}, {2,7} ) === {2,3,4,5,6} )
assert( take( {0,1,2,3,4,5,6}, {-1,7} ) === {0,1,2,3,4,5,6} )

assert( take( (0,1,2,3,4,5,6), {2,2} ) === toSequence{2} )
assert( take( (0,1,2,3,4,5,6), {2,1} ) === () )

assert( take( (0,1,2,3,4,5,6), {2,4} ) === (2,3,4) )
assert( take( (0,1,2,3,4,5,6), {0,4} ) === (0,1,2,3,4) )
assert( take( (0,1,2,3,4,5,6), {-1,4} ) === (0,1,2,3,4) )
assert( take( (0,1,2,3,4,5,6), {2,6} ) === (2,3,4,5,6) )
assert( take( (0,1,2,3,4,5,6), {2,7} ) === (2,3,4,5,6) )
assert( take( (0,1,2,3,4,5,6), {-1,7} ) === (0,1,2,3,4,5,6) )
