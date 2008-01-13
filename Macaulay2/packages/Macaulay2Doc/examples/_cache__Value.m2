x = new HashTable from { val => 1000, cache => new CacheTable }
f = (t -> (print "hi there"; t.val^4))
h = (cacheValue VALUE) f
h x
h x
peek'_2 x
