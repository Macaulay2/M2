assert = x -> if not x then error "assertion failed"
g = x -> (3;4+5*6^return;8;error "oops")
h = x -> (3;for i from 1 to 3 do 4+5*6^return {i,x};8;error "oops")
p = x -> (h x;okay)

assert( {0, 2, 4, 6, 8, 10} === for i to 10 list if i%2 === 0 then i else continue )

r = assert( {null,null,null,null} === for i to 3 list g i )
assert(null === r)
r = assert( {{1, 0}, {1, 1}, {1, 2}, {1, 3}} === for i to 3 list h i )
assert(null === r)
r = assert( {okay, okay, okay, okay} === for i to 3 list p i )
assert(null === r)
r = assert( p 4 === okay )
assert(null === r)
scan(3, x -> (3*return; error "oops"))
scan(3, (x) -> (3*return; error "oops"))
scan({(1,2),(1,2)}, (x,y) -> (3*return; error "oops"))
apply(3, x -> (3*return; error "oops"))
apply(3, (x) -> (3*return; error "oops"))
apply({(1,2),(1,2)}, (x,y) -> (3*return; error "oops"))
r = assert( 44 === (() -> (apply(3, x -> (3*return; error "oops")); 44))())
assert(null === r)
r = assert( 44 === (() -> (apply(3, (x) -> (3*return; error "oops")); 44))())
assert(null === r)
r = assert( 44 === (() -> (apply({(1,2),(1,2)}, (x,y) -> (3*return; error "oops")); 44))())
assert(null === r)
r = assert( {44,44,44} === (() -> apply(3, x -> (3*return 44; error "oops")))())
assert(null === r)
r = assert( {44,44,44} === (() -> apply(3, (x) -> (3*return 44; error "oops")))())
assert(null === r)
r = assert( {44,44,44} === (() -> apply({(1,2),(1,2),(1,2)}, (x,y) -> (3*return 44; error "oops")))())
assert(null === r)
r = assert( null === (() -> scan(3, x -> (3*return 44; error "oops")))())
assert(null === r)
r = assert( null === (() -> scan(3, (x) -> (3*return 44; error "oops")))())
assert(null === r)
r = assert( null === (() -> scan({(1,2),(1,2)}, (x,y) -> (3*return 44; error "oops")))())
assert(null === r)
r = assert( 44 === (() -> (apply(3, x -> (7*break; error "oops")); 44))())
assert(null === r)
r = assert( 44 === (() -> (apply(3, (x) -> (7*break; error "oops")); 44))())
assert(null === r)
r = assert( 44 === (() -> (apply({(1,2),(1,2)}, (x,y) -> (7*break; error "oops")); 44))())
assert(null === r)
r = assert( 44 === (() -> apply(3, x -> (7*break 44; error "oops")))())
assert(null === r)
r = assert( 44 === (() -> apply(3, (x) -> (7*break 44; error "oops")))())
assert(null === r)
r = assert( 44 === (() -> apply({(1,2),(1,2),(1,2)}, (x,y) -> (7*break 44; error "oops")))())
assert(null === r)
r = assert( 44 === (() -> scan(3, x -> (7*break 44; error "oops")))())
assert(null === r)
r = assert( 44 === (() -> scan(3, (x) -> (7*break 44; error "oops")))())
assert(null === r)
r = assert( 44 === (() -> scan({(1,2),(1,2)}, (x,y) -> (7*break 44; error "oops")))())
assert(null === r)

i = 1
r = assert( 44 === while i < 3 do (break 44; i = i+1) )
assert( i == 1 )
assert(null === r)

i = 1
r = assert( 44 === for i from 1 to 3 do (break 44; i = i+1) )
assert( i == 1 )
assert(null === r)

i = 1
r = assert( 44 === for i from 1 to 3 list (break 44; i = i+1) )
assert( i == 1 )
assert(null === r)

r = assert( 44 === scan({3,5,7,44,11,13,15}, j -> if j === 44 then 4 * break j))
assert(null === r)


any({1,2,3}, i -> break 4)
select({1,2,3,4}, i -> break 4 )

assert( relativizeFilename ( "/a/b/c" , "/a/b/c/d/e" ) === "d/e" )
assert( relativizeFilename ( "/a/b/c" , "/a/b/c/d/e/" ) === "d/e/" )
assert( relativizeFilename ( "/a/b/c/" , "/a/b/c/d/e/" ) === "d/e/" )

assert( relativizeFilename ( "/a/b/c/d/e" , "/a/b/c" ) === "../../" )
assert( relativizeFilename ( "/a/b/c/d/e/" , "/a/b/c" ) === "../../" )
assert( relativizeFilename ( "/a/b/c/d/e" , "/a/b/c/" ) === "../../" )
assert( relativizeFilename ( "/a/b/c/d/e/" , "/a/b/c/" ) === "../../" )

assert( relativizeFilename ( "/a/b/c/d/e" , "/a/b/c/f/g" ) === "../../f/g" )
assert( relativizeFilename ( "/a/b/c/d/e/" , "/a/b/c/f/g" ) === "../../f/g" )
assert( relativizeFilename ( "/a/b/c/d/e" , "/a/b/c/f/g" ) === "../../f/g" )
assert( relativizeFilename ( "/a/b/c/d/e/" , "/a/b/c/f/g" ) === "../../f/g" )

assert( relativizeFilename ( "/a/b/c/d/e" , "/a/b/c/f/g/" ) === "../../f/g/" )
assert( relativizeFilename ( "/a/b/c/d/e/" , "/a/b/c/f/g/" ) === "../../f/g/" )
assert( relativizeFilename ( "/a/b/c/d/e" , "/a/b/c/f/g/" ) === "../../f/g/" )
assert( relativizeFilename ( "/a/b/c/d/e/" , "/a/b/c/f/g/" ) === "../../f/g/" )

assert( realpath "." == currentDirectory () )
assert( realpath ".." == toAbsolutePath "../" )
assert( realpath "/" == "/" )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/basictests A77.okay"
-- End:
