document { Key => "foo", "hi there", EXAMPLE "a", "ho there", EXAMPLE "b" }
examples "foo"
assert( oo == "a\nb"^-1)
