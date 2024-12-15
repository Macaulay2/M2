undocumented {
    ((symbol SPACE, symbol =), Function, Thing),
}

document { Key => cache,
    Headline => "a key under which to store cache tables",
    SeeAlso => {CacheTable},
    EXAMPLE lines ///
     	  F = ZZ^3
     	  peek F
	  F.cache#Foo = Bar
	  peek F
	  peek F.cache
	  F === ZZ^3
    ///}

document {
     Key => CacheTable,
     Headline => "hash tables for caching",
     "A type of mutable hash table designed for caching computed values that
     could always be recomputed.  Cache tables are designed so their contents
     will not participate in any comparisons by the strict comparison
     operator ", TT "===", ".  To that end, any two cache tables with the same
     class and parent are considered equal to each other and have hash code equal to 0."
     }
