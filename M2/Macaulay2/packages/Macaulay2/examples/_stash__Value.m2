x = new MutableHashTable from { val => 1000 }
g = (t -> (print "hi there"; t.val^4))
f = (stashValue VALUE) g
f x
f x
peek x
