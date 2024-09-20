document { Key => "foo", "hi there", EXAMPLE "a", "ho there", EXAMPLE "b" }
ex = examples "foo"
assert( ex#-2 == "a" )
assert( ex#-1 == "b" )
