
assert = x -> if not x then error "assertion failed "

-- test comparison

 assert((4<5) === true)
 assert((4<4) === false)
 assert((4<3) === false)
 assert((4<=5) === true)
 assert((4<=4) === true)
 assert((4<=3) === false)
 assert((4>5) === false)
 assert((4>4) === false)
 assert((4>3) === true)
 assert((4>=5) === false)
 assert((4>=4) === true)
 assert((4>=3) === true)
 assert((3?4) === quote <)
 assert((4?3) === quote >)
 assert((3?3) === quote ==)
 assert(("a" ? "b") === quote <)
 assert(("b" ? "a") === quote >)
 assert(("a" ? "a") === quote ==)
