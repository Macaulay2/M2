x = new MutableHashTable from { val => 1000 }
f = (t -> (print "hi there"; t.val^4))
h = (stashValue VALUE) f
h x
h x
peek x
