assert = x -> if not x then error "assertion failed "

v = version#"VERSION"
assert ( #v > 2 )
assert ( "0" <= v#0 and v#0 <= "9" )
assert ( v#1 == "." )
