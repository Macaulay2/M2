-- test min
assert( infinity === min {} )
assert( 2 === min { 3,2,4 } )
assert( 2 === min ( 3,2,4 ) )

-- test max
assert( -infinity === max {} )
assert( 4 === max (3,4,2) )
assert( 4 === max {3,4,2} )

-- test unique
assert( unique {4,4,5,5,3,3} === {4, 5, 3} )
assert( unique {3,4,4,5,5,3,3} === {3, 4, 5} )

-- test for duplicate entries in the dictionary

assert ( {} === select ( pairs tally keys Core.Dictionary , (s,n) -> n>1 ) )

-- version and printing

assert ( toString version =!= "version" )
assert ( version#"VERSION" =!= "" )

-- test override
--status: fixing this will be a minor improvement to the way functions with optional arguments work
assert ( override (new OptionTable from {a=>1},(1:(3,4,5))) === (new OptionTable from {a => 1}, 1: (3, 4, 5)) )
assert ( override (new OptionTable from {a=>1},((3,4,5),a=>2)) === (new OptionTable from {a => 2}, (3, 4, 5)) )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test C05.out"
-- End:
