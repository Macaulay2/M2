-- test flatten
assert := x -> if not x then error "assertion failed "


assert({1,2,3,4,5} === flatten{1,{},{2},{3,4},5})
f = { new MutableList from {0,1} }
assert(f === flatten f)

-- test regexp
assert( regex ( "^(.*[^ ])? *$", " abcdef " ) === {(0, 8), (0, 7)} )
assert( regex ( "^ *(.*[^ ])? *$", " abcdef " ) === {(0, 8), (1, 6)} )
assert( regex ( "^ *(.*)$", " abcdef " ) === {(0, 8), (1, 7)} )
assert( regex ( ".?","" ) === {(0, 0)} )
