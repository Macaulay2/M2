-- test closure

assert = x -> if not x then error "assertion failed "

f = i -> () -> i = i + 1
g = f 1
h = f 2
assert(g() === 2)
assert(h() === 3)
assert(g() === 3)
assert(h() === 4)

assert( class aa === Symbol )
assert( class a0 === Symbol )
assert( class a0b === Symbol )
assert( class a0b7 === Symbol )
