
assert = x -> if not x then error "assertion failed "

-- test join

assert( join ( {a,b,c}, {d,{e,f}}, {(g,h,i)} ) === {a,b,c,d,{e,f},(g,h,i)} )
assert( join () === () )
assert( join {} === {} )
assert( join ( [a,b,c], {d,e,f} ) === [a,b,c,d,e,f] )

x = new MutableList from splice {(0..10)}
y = join x
assert isMutable y
x#2 = 33
assert (y#2 === 2)

-- test splice
assert ( splice () === () )
assert ( splice (a,b,c) === (a,b,c) )
assert ( splice (a,toSequence{b},c) === (a,b,c) )
assert ( splice (a,(b,c),d) === (a,b,c,d) )
assert ( splice {a,(b,c),d} === {a,b,c,d} )
assert ( splice [a,(b,c),d] === [a,b,c,d] )
assert ( splice (a,(b,c,(d,(e,(f,{g}))))) === (a,b,c,(d,(e,(f,{g})))) )

-- test deepSplice
assert ( deepSplice () === () )
assert ( deepSplice (a,b,c) === (a,b,c) )
assert ( deepSplice (a,toSequence{b},c) === (a,b,c) )
assert ( deepSplice (a,(b,c),d) === (a,b,c,d) )
assert ( deepSplice {a,(b,c),d} === {a,b,c,d} )
assert ( deepSplice [a,(b,c),d] === [a,b,c,d] )
assert ( deepSplice (a,(b,c,(d,(e,(f,{g}))))) === (a,b,c,d,e,f,{g}) )

-- test instance
assert instance(3,ZZ)
assert instance(3,Thing)
assert (not instance(3,Ring))
assert (not instance(Ring,Ring))

-- test version

assert( version#"VERSION" =!= "" )
