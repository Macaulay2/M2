-- test methods

-- test ==
assert( { 1. } == { 1 } )
assert( (1,2) == (1.,2.) )


-- test singleton
assert( # singleton (1,2,3) === 1 )
assert( class singleton 3 === Sequence )
