
assert = x -> if not x then error "assertion failed "

-- test parallel assignment

global a1
local a2
f = a3 -> (
     local a4;
     v = (a1,a2,a3,a4) = (1,2,3,4);
     assert( a1 === 1 );
     assert( a2 === 2 );
     assert( a3 === 3 );
     assert( a4 === 4 );
     assert( v === (1,2,3,4));
     a1 = a2 = a3 = a4 = 0;
     v = (a1,a2,a3,a4) = (1,2,3,(4,5));
     assert( a1 === 1 );
     assert( a2 === 2 );
     assert( a3 === 3 );
     assert( a4 === (4,5) );
     assert( v === (1,2,3,(4,5)));
     a1 = a2 = a3 = a4 = 0;
     v = (a1,a2,a3,a4) = (1,2,3,);
     assert( a1 === 1 );
     assert( a2 === 2 );
     assert( a3 === 3 );
     assert( a4 === null );
     assert( v === (1,2,3,));
     )
f ()

-- test closure


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

