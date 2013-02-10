
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
 assert((3?4) === symbol <)
 assert((4?3) === symbol >)
 assert((3?3) === symbol ==)
 assert(("a" ? "b") === symbol <)
 assert(("b" ? "a") === symbol >)
 assert(("a" ? "a") === symbol ==)
