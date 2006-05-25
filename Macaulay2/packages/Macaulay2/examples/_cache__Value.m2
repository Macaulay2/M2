x = new HashTable from { val => 1000, cache => new CacheTable }
g = (t -> (print "hi there"; t.val^4))
f = (cacheValue VALUE) g
f x
f x
peek'_2 x
