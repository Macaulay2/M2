assert = x -> if not x then error "assertion failed"

assert( getGlobalSymbol "stopIfError" === symbol stopIfError )
threadVariable foo
assert( getGlobalSymbol "foo" === symbol foo )
