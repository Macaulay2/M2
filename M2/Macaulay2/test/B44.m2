-- test methods

-- test ==
assert( { 1. } == { 1 } )
assert( (1,2) == (1.,2.) )


-- test singleton
assert( # singleton (1,2,3) === 1 )
assert( class singleton 3 === Sequence )

-- test unSingleton
assert( unSingleton (1,2,3) === (1,2,3) )
assert( unSingleton 1 === 1 )
assert( unSingleton singleton 1 === 1 )
assert( unSingleton () === () )
