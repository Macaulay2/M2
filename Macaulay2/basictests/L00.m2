assert = x -> if not x then error "assertion failed "
simpleLoad concatenate(srcdir, "L00.input")
assert( y === 55 )
