-- test min
assert( infinity === min {} )
assert( 2 === min { 3,2,4 } )
assert( 2 === min ( 3,2,4 ) )

-- test max
assert( -infinity === max {} )
assert( 4 === max (3,4,2) )
assert( 4 === max {3,4,2} )
