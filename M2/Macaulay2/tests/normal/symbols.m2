assert (i -> symbol i === symbol i)()
assert( (i -> symbol i)() =!= (i -> symbol i)() )
assert( symbolBody (i -> symbol i)() =!= symbolBody (i -> symbol i)() )
assert( f = i -> symbol i ; symbolBody f() === symbolBody f() )

