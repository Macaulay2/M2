assert = x -> if not x then error "assertion failed "
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

assert( replaceStrings("^a","x","a \na \naaa a") === "x \nx \nxaa a" )
