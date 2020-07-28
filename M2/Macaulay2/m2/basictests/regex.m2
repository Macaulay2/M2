assert = x -> if not x then error "assertion failed "

-- test regexp
assert( regex ( "^(.*[^ ])? *$", " abcdef " ) === {(0, 8), (0, 7)} )
assert( regex ( "^ *(.*[^ ])? *$", " abcdef " ) === {(0, 8), (1, 6)} )
assert( regex ( "^ *(.*)$", " abcdef " ) === {(0, 8), (1, 7)} )
assert( regex ( ".?","" ) === {(0, 0)} )

match = X -> null =!= regex X
assert not match(".a", "  \na  ")
assert     match("^a", "  \na  ")
assert     match(".a", "  a  ")
assert not match("^a", "  a  ")
assert match("a|b","a")
assert match("a+","a")
assert match("(a)","a")
assert match("a{2}","aa")
assert match("a[2]","a2")
assert match("a[^a]","a2")
assert match("a\\b","a b")
assert not match("a\\b","ab")
assert match("a\\>","a b")
assert match("a\\>","a")
assert not match("a\\>"," ab")
