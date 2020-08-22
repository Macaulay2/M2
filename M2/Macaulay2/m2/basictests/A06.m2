-- test flatten
assert := x -> if not x then error "assertion failed "

assert({1,2,3,4,5} === flatten{1,{},{2},{3,4},5})
f = { new MutableList from {0,1} }
assert(f === flatten f)
