-- test flatten
File << Thing := (x,y) -> printString(x,string y)
File << String := (x,y) -> printString(x,y)
assert := x -> if not x then error "assertion failed "


assert({1,2,3,4,5} === flatten{1,{},{2},{3,4},5})
f = { new MutableList from {0,1} }
assert(f === flatten f)

-- make sure alarms are turned off after an error
try ( alarm 1 ; error "" ) else sleep 3

-- test regexp
assert( matches ( "^(.*[^ ])? *$", " abcdef " ) === {(0, 8), (0, 7)} )
assert( matches ( "^ *(.*[^ ])? *$", " abcdef " ) === {(0, 8), (1, 6)} )
assert( matches ( "^ *(.*)$", " abcdef " ) === {(0, 8), (1, 7)} )
assert( matches ( ".?","" ) === {(0, 0)} )
