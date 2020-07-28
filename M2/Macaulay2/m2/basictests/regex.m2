assert = x -> if not x then error "assertion failed "

-- test regexp
assert( regex ( "^(.*[^ ])? *$", " abcdef " ) === {(0, 8), (0, 7)} )
assert( regex ( "^ *(.*[^ ])? *$", " abcdef " ) === {(0, 8), (1, 6)} )
assert( regex ( "^ *(.*)$", " abcdef " ) === {(0, 8), (1, 7)} )
assert( regex ( ".?","" ) === {(0, 0)} )
