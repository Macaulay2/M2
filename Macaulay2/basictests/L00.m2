assert = x -> if not x then error "assertion failed "
load concatenate(srcdir, "L00.input")
assert( y === 55 )
