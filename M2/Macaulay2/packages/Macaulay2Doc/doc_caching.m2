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

document {
     Key => cacheValue,
     Headline => "cache values of functions in their arguments",
     Usage => "((cacheValue KEY) f) x",
     Inputs => {
	  "KEY",
	  "f" => Function,
	  "x" => {"an argument for ", TT "f", " that has ", ofClass CacheTable, " stored in it under ", TT "x.cache"}
	  },
     Outputs => {
	  { TT "f x", " is returned, but the value is saved in ", TT "x.cache#KEY", " and not recomputed later.
	       However, if the value found in ", TT "x.cache#KEY", " is ", ofClass CacheFunction, ", such as is
	       returned by ", TT "(stashValue KEY) f", ", then the value of ", TT "x.cache#KEY x", " is returned instead, after
	       first removing ", TT "x.cache#KEY", " from ", TT "x.cache", "." }
	  },
     EXAMPLE {
	  "x = new HashTable from { val => 1000, cache => new CacheTable }",
	  ///f = (t -> (print "hi there"; t.val^4))///,
	  ///h = (cacheValue VALUE) f///,
	  "h x",
	  "h x",
	  "peek'_2 x"
	  },
     SourceCode => { cacheValue },
     SeeAlso => { stashValue }
     }

document {
     Key => stashValue,
     Headline => "stash values of functions in their arguments",
     Usage => "((stashValue KEY) f) x",
     Inputs => {
	  "KEY",
	  "f" => Function,
	  "x" => MutableHashTable => { "an argument for ", TT "f" }
	  },
     Outputs => {
	  { "The value of ", TT "f x", " is returned, but the value is saved in ", TT "x#KEY", " and not recomputed later.
	       However, if the value found in ", TT "x#KEY", " is ", ofClass CacheFunction, ", such as is
	       returned by ", TT "(stashValue KEY) f", ", then the value of ", TT "x#KEY x", " is returned instead, after
	       first removing ", TT "x#KEY", " from ", TT "x", "."
	       }
	  },
     EXAMPLE {
	  "x = new MutableHashTable from { val => 1000 }",
	  ///f = (t -> (print "hi there"; t.val^4))///,
	  ///h = (stashValue VALUE) f///,
	  "h x",
	  "h x",
	  "peek x"
	  },
     SourceCode => { stashValue },
     SeeAlso => { cacheValue }
     }

document {
     Key => CacheFunction,
     Headline => "the class of cache functions",
     "Functions of class ", TO "CacheFunction", " are created and used by ", TO "cacheValue", " and by ", TO "stashValue", "."
     }
