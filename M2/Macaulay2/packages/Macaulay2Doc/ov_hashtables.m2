--authors: Dan Grayson, Lily Silverstein

document {
    Key => "hash tables",
    "A hash table is a data structure that can implement a function
    whose domain is a finite set.  An element of the domain is called
    a key.  The hash table stores the key-value pairs in such a way
    that when presented with a key, the corresponding value can be
    quickly recovered.",
    PARA{},
    "A dictionary could be implemented as a hash table: the keys would
    be the words in the language, and the values could be the definitions
    of the words.",
    PARA{},
    "A phone book could also be implemented as a hash table: the keys would
    be the names of the subscribers, and the values could be the corresponding
    phone numbers.  (We exclude the possibility of two subscribers with
	the same name.)",
    PARA{},
    "As an example we implement a phone book.",
    EXAMPLE {
	  ///book = new HashTable from {
     "Joe" => "344-5567",
     "Sarah" => "567-4223",
     "John" => "322-1456"}///,
     },
    "We use the operator ", TO "#", " to obtain values from the phone book.",
    EXAMPLE ///book#"Sarah"///,
    "The operator ", TO "#?", " can be used to tell us whether a given key
    has an entry in the hash table.",
    EXAMPLE ///book#?"Mary"///,
    "We have implemented the notion of set via hash tables in which every value
    is the number 1.",
    EXAMPLE {
	  "x = set {a,b,c,r,t}",
	  "peek x",
	  "x#?a",
	  "x#?4",
	  },
    "There is a type of hash table that is mutable, i.e., a hash table
    whose entries can be changed.  They are changed with assignment
    statements of the form ", TT "x#key=value", ".",
    EXAMPLE {
	  ///x = new MutableHashTable;///,
	  ///x#"Joe" = "344-5567";///,
	  ///x#3 = {a,b,c};///,
	  ///x#{1,2} = 44;///,
	  ///x#3///,
	  ///x#?4///,
	  },
    "When a mutable hash table is printed, its contents are not displayed.
    This prevents infinite loops in printing routines.",
    EXAMPLE "x",
    "Use ", TO "peek", " to see the contents of a mutable hash table.",
    EXAMPLE "peek x",
    "A variant of ", TO "#", " is ", TO ".", ".  It takes only global symbols
    as keys, and ignores their values.",
    EXAMPLE {
	  "p=4;",
	  "x.p = 444;",
	  "x.p",
	  "x#?4"
	  },
    Subnodes => {
	TO "HashTable",
	TO "MutableHashTable",
	TO "hashing",
	TO "hash",
	TO youngest,
	"constructing hash tables",
	TO "hashTable",
	TO "new HashTable from List",
	"functions for manipulating hash tables",
	TO "isMutable",
	TO "keys",
	TO "values",
	TO "pairs",
	TO "copy",
	TO "remove",
	TO "merge",
	TO "combine",
	TO "override",
	TO "mapping over hash tables",
        },
    }

doc///
 Key
  HashTable
 Headline
  the class of all hash tables
 Description
  Text
   A hash table consists of: a class type, a parent type, and a
   set of key-value pairs.  The keys and values can be anything.
   The access functions below accept a key and return the
   corresponding value.  For details of the mechanism
   underlying this, see @TO "hashing"@.
   
   One important feature of hash tables that when the keys
   are consecutive integers starting at 0, the keys are scanned
   in the natural order.

   There is a subclass of HashTable called @TO MutableHashTable@
   which consists of those hash tables whose entries can be changed.
   
   This node is currently in progress!!!
 SeeAlso
  "#"
  "."
  "#?"
  ".?"
 Subnodes
   VirtualTally
   OptionTable
///

document {
    Key => MutableHashTable,
    Headline => "the class of all mutable hash tables",
    PARA{},
    "A mutable hash table is a type of hash table whose entries can be changed.",
    PARA{},
    "Normally the entries in a mutable hash table are not printed, to prevent
    infinite loops in the printing routines.  To print them out, use
    ", TO "peek", ".",
    EXAMPLE {
	"x = new MutableHashTable",
	"scan(0 .. 30, i -> x#i = i^2)",
	"x # 20",
	"x #? 40",
    },
    SeeAlso => "HashTable",
    Subnodes => {
	TO CacheTable,
	TO IndexedVariableTable,
	TO Descent,
    },
}

document {
    Key => {
	hashTable,
	(hashTable, BasicList),
	(hashTable, Function, BasicList)},
    Headline => "make a hash table",
    TT "hashTable(h,v)", " -- produce a hash table from a list ", TT "v", " of key-value pairs, with an optional collision handler function ", TT "h", ".",
    PARA{},
    "The pairs may be of the form ", TT "a=>b", ", ", TT "{a,b}", ",
    or ", TT "(a,b)", ".",
    PARA{},
    "Missing entries in the list, represented by ", TO "null", ", will be silently
    ignored.",
    PARA{},
    EXAMPLE {
	"x = hashTable {a=>b, c=>d, }",
	"x#a",
	"hashTable(plus, {(a,3),(b,4),(a,10)})"
    },
}

document {
     Key => "hashing",
     "A hash table contains a set of key-value pairs.  The access
     functions for hash tables accept a key and retrieve the
     corresponding value.  Here are the details, together with a
     discussion of how we designed the hash table system seen in
     Macaulay2.",
     PARA{},
     "The keys and values are stored in the hash table.  The hash table consists
     of a certain number of ", ITALIC "buckets", ", each of which can hold an
     arbitrary number of key-value pairs.  The number of buckets is chosen
     to be large enough that typically one may expect each bucket to hold fewer than
     three key-value pairs.  The key is used  as follows to determine in which
     bucket the key-value pair is to be stored.  The function ", TO "hash", " 
     is applied to the key to produce, in a deterministic way, an integer called
     the hash code, and the remainder of the hash code upon division by the
     number of buckets tells which bucket will be used.",
     PARA{},
     "It is essential that the
     hash code of a key never change, for otherwise the next 
     attempt to find the key in the hash table will have an unpredictable 
     result - the new hash code of the key may or may not lead to 
     the same bucket, and the value may or may not be located.",
     PARA{},
     "Some hash tables and lists are ", TO "mutable", ", i.e., their 
     contents can be altered by assignment statements.  As explained
     above, the hash code of a mutable thing is not permitted to
     change when the contents of the thing change.  Hence, the 
     algorithm for computing the hash code may not refer to the
     contents of a mutable thing.",
     PARA{},
     "The strong comparison operator ", TO "===", " is provided to 
     parrot the equality testing that occurs implicitly when 
     accessing a key in a hash table.  The fundamental requirement for this
     strong comparison operator is that things with different hash codes must also
     turn out to be different when tested with this comparison operator.",
     PARA{},
     "Here we come to a question of design.  As discussed above, we must assign
     hash codes to mutable things in such a way that the hash codes don't depend
     on their contents.  We can do this in various ways.",
     UL {
	  {
     	       "One way to assign hash codes to mutable things is to give 
     	       the same hash code, say 1000000, to every mutable thing.  We
	       could then implement a strong comparison operator for mutable
	       things that would proceed by examining the contents of the
	       things, so that two mutable things would be equal if and only
	       if their contents were equal.  A
	       disadvantage of this approach would be that a hash table in
	       which many mutable things appear as keys would have all of those
	       key-value pairs appearing in the same bucket, so that access
	       to this hash table would be slow.  (Each bucket is implemented
	       as a linear list, and searching a long linear list is slow.)"
	       },
	  {
     	       "Another way to assign hash codes to mutable things is to
     	       give different hash codes to each mutable thing; for example, the 
	       first mutable thing could receive hash code 1000000, the second
	       could receive the hash code 1000001, and so on.  (Another
     	       choice for such a hash code is the 
     	       address in memory of the thing.  But this address can change
     	       depending on environmental factors not under the control of the
     	       interpreter, and thus its use as a hash code would lead 
	       to unpredictable behavior.)  A disadvantage
	       of this approach is that the strong comparison operator could not
	       examine the contents of mutable objects!  (Remember that
	       if the hash codes are different, the strong comparison must declare
	       the things to be different, too.)  The offsetting advantage is
	       that a hash table in which many mutable things appear as keys would
	       typically have the key-value pairs distributed among the buckets,
	       so that access to this hash table would be fast."
	       }
	  },
     PARA{},
     "In Macaulay2, we chose the second approach listed above; we expect to
     have many mutable things appearing as keys in hash tables, and we need
     the speed.  A counter with initial value 1000000 is incremented each time 
     a mutable thing is created, and its value is taken as the hash code of the
     thing and stored within it.  The strong comparison test cannot depend on 
     the contents of mutable things, and thus such things appear to be 
     containers with opaque walls.  For mutable things, the test for equality 
     must be the same as equality of the hash codes.",
     PARA{},
     "It is essential to have some hash tables for which equality amounts
     to equality of the contents.  This cannot be achieved for mutable
     hash tables, but we do achieve it for non-mutable hash tables -- the
     hash code is computed directly from the contents
     of the thing in a deterministic way.  This allows us to
     implement the notion of polynomial, say, as a hash table -- the 
     keys can be the monomials (necessarily non-mutable) and the 
     values can be the coefficients.  The notion of monomial can be
     implemented as a hash table where the keys are the variables and the
     values are the corresponding exponents.",
     PARA{},
     "One further comforting remark: the routines that compute hash 
     codes or strong equality do not get into infinite loops, despite 
     the existence of circular structures: any circular structure 
     must come into being by means of changing something, and
     so the circular loop in the structure necessarily involves a 
     mutable thing, whose contents are not examined by the routines.
     This provides another argument in favor of taking the second approach listed
     above.",
     SeeAlso => "HashTable"
     }

document {
    Key => "mapping over hash tables",
    Headline => "apply a function to each element of a hash table",
    "Each entry in a hash table consists of a key and a value.  We provide
    three ways to map functions over a hash table, depending on whether the
    function is to receive a value and return a new value, to receive a key
    and return a new key, or to receive a key-value pair and return a new
    key-value pair.  The corresponding functions, ", TO "applyValues", ",
    ", TO "applyKeys", ", and ", TO "applyPairs", " are illustrated in the
    next example.",
    EXAMPLE {
	"x = new HashTable from {a=>1, b=>2}",
	"applyValues(x, value -> 1000*value)",
	"applyKeys(x, key -> {key})",
	"applyPairs(x, (key,value) -> (value,key))",
    },
    "The functions, ", TO "scanValues", ", ", TO "scanKeys", ", and 
    ", TO "scanPairs", " are similar, but the values returned are discarded
    instead of being assembled into a new hash table.",
    EXAMPLE {
	"x = new HashTable from {a=>1, b=>2}",
	"scanValues(x, print)",
	"scanKeys(x, print)",
	"scanPairs(x, print)",
    },
    "The function ", TO "merge", " can be used to merge two hash tables.  The
    result is a hash table whose keys are those occurring in one of the two
    incoming hash tables.  We must provide a function of two arguments
    that is used to combine the values when a key occurs in both hash tables.",
    EXAMPLE {
	"y = new HashTable from {b=>200, c=>300}",
	"merge(x, y, plus)",
    },
    "The function ", TO "combine", " can be used to combine two hash tables ", TT "x", "
    and ", TT "y", " into a new hash table.  Three functions must be provided.  The first 
    one produces new keys from a key of ", TT "x", " and a key of ", TT "y", ".  The
    second one produces a new values from a value of ", TT "x", " and a value
    of ", TT "y", ".  The third one is used to combine values when two new keys
    turn out to be the same.",
    EXAMPLE {
	"combine(x,y,identity,times,plus)",
    },
    Subnodes => {
	TO "applyKeys",
	TO "applyValues",
	TO "applyPairs",
	TO "scanKeys",
	TO "scanValues",
	TO "scanPairs",
	TO "selectKeys",
	TO "selectValues",
	TO "selectPairs",
	TO (any, HashTable, Function),
	TO (all, HashTable, Function),
    }
}

document {
    Key => copy,
    Headline => "copy an object",
    TT "copy x", " yields a copy of x.",
    PARA{},
    "If x is an hash table, array, list or sequence, then the elements are
    placed into a new copy. If x is a hash table, the copy is mutable if
    and only if x is.",
    PARA{},
    "It is not advisable to copy such things as modules and rings,
    for: (1) the operations that have already been installed for them will return
    values in the original object, rather than in the copy; and (2) the copy
    operation is shallow, not copying keys and values that happen to be hash tables.",
    PARA{},
    SeeAlso => { "newClass" }
}

doc///
 Key
   keys
  (keys, Database)
  (keys, Dictionary)
  (keys, HashTable)
 Headline
  keys used in a hash table, dictionary, or database
 Usage
  keys t
 Inputs
  t:{HashTable,Dictionary,Database}
 Outputs
  :List
   the keys occurring in the hash table {\tt t}
 Description
  Example
   x = new HashTable from {a => 1, b => 2}
   keys x
 SeeAlso
  "hash tables"
  applyKeys
  pairs
  scanKeys
  values
  "#"
  "#?"
///

doc ///
 Key
  isMutable
  (isMutable, Thing)
 Headline
  whether something may be modified
 Usage
  isMutable x
 Inputs
  x:Thing
 Outputs
  :Boolean
   whether {\tt x} is mutable
 Description
  Text
   If {\tt x} is a hash table, list, dictionary, or database, then 
   it is mutable if its contents can be destructively altered.
   
   If {\tt x} is a symbol, then it's mutable if a value can be 
   assigned to it; i.e., if it is not @TO protect@ed.
   
   If {\tt x} is anything else, {\tt isMutable x} will return {\bf false}.
  Example
   T = new MutableList from (a, b, c)
   isMutable T
   V = new List from (a, b, c)
   isMutable V
   isMutable join(T, V)
   isMutable a
   isMutable "a"  
  Text
   This function may also be called using the synonym @TT "mutable"@.
 Caveat
  The (changeable) contents of a mutable hash table or list 
  do not participate in strong comparison with @TO "==="@
  or in @TO "hashing"@.
 SeeAlso
  MutableHashTable
  MutableList
  "hash tables"
  "lists and sequences"
///

doc ///
 Key
  pairs
  (pairs, HashTable)
  (pairs, Dictionary)
  (pairs, BasicList)
  (pairs, Thing)
 Headline
  list the pairs in a hash table, dictionary, or basic list
 Synopsis
  Usage
   pairs x
  Inputs
   x:{HashTable, Dictionary, BasicList}
  Outputs
   L:List -- of all pairs @CODE "(k, x#k)"@
 Synopsis
  Usage
   pairs x
  Inputs
   x:Thing -- an instance of a class with an @TO iterator@ method installed
  Outputs
   :Iterator
 Description
  Text
   If {\tt x} is a hash table or dictionary, the pairs consist of
   each key along with its associated value.
  Example
   x = new HashTable from {a => 1, b => 2, c => 3}
   pairs x
  Text
   A dictionary is a special hash table whose keys are strings, 
   and whose values are the corresponding symbols.
  Example
   d = new Dictionary
   getGlobalSymbol (d, "foo")
   getGlobalSymbol (d, "bar")
   pairs d
   first oo
   class \ oo
  Text
   If {\tt x} is a basic list, the pairs consist of each index 
   along with the element at that index in the list.
  Example
   L = {3, 5, 7};
   pairs L
   pairs {apple, banana, carrot}
  Text
   If @CODE "x"@ belongs to any other class, then @TO iterator@ is called
   on it, and if successful, an @TO Iterator@ object is returned.
  Example
   pairs "foo"
   toList oo
   i = pairs Iterator(() -> 5)
   next i
   next i
   next i
 Caveat
  As the first example illustrates, pairs are not necessarily listed in
  any particular order.
 SeeAlso
  "hash tables"
  applyPairs
  keys
  scanPairs
  values
  "#"
  "#?"
///

doc ///
 Key
  remove
  (remove, HashTable, Thing)
  (remove, MutableList, ZZ)
  (remove, Database, String)
 Headline
  remove an entry from a mutable hash table, list, or database
 Usage
  remove(T, k)
 Inputs
  T:{HashTable, MutableList, Database}
  k:
    the key to remove (must be @ofClass ZZ@ if @TT "T"@ is a mutable list or
    @ofClass String@ if it is a database)
 Outputs
  :Thing -- the removed value
 Description
  Text
   {\tt remove(T, k)} removes and returns the entry of {\tt T} stored under
   the key {\tt k}.
  Example
   T = new MutableHashTable from {a => 1, b => 2, c => 3}; peek T
   remove(T, a)
   peek T
  Text
   If {\tt T} is not a mutable hash table, an error is thrown.
   One way to remove an entry from an immutable hash table is
   with the function @TO applyPairs@:
  Example
   T = new HashTable from {a => 1, b => 2, c => 3}
   T = applyPairs(T, (k, v) -> if k =!= a then (k, v))
  Text
   @TT "remove"@ works similarly when @TT "T"@ is a database.  See @TO Database@
   for more information.
  Text
    If @TT "T"@ is a mutable list, then @TT "k"@ gives the index of the element
    to be removed.
  Example
    T = new MutableList from {1, 2, 3, 4}; peek T
    remove(T, 0)
    peek T
  Text
    If @TT "k"@ is negative, then the index is determined by counting backwards
    from the end of @TT "T"@.
  Example
    remove(T, -1)
    peek T
 SeeAlso
  applyKeys
  applyPairs
  applyValues
  delete
  drop
  keys
  mutable
  scanKeys
  "hash tables"
///

doc ///
 Key
  scanKeys
  (scanKeys, Database, Function)
  (scanKeys, HashTable, Function)
 Headline
  apply a function to each key in a hash table or database
 Usage
  scanKeys(t, f)
 Inputs
  t:HashTable
   or Database
  f:Function
 Description
  Text
   {\tt scanKeys(t, f)} applies the function {\tt f} to each key
   in the hash table {\tt t}. 
  Example
   t = hashTable {{1,8},{2,20},{3,4},{4,20}}
   scanKeys(t, print)
   scanKeys(t, k -> if k>2 then print t#k)
 Caveat
  This function requires an immutable hash table.  To scan the keys in
  a mutable hash table, use {\tt scan(keys t, f)}.
 SeeAlso
  "hash tables"
  applyKeys
  keys
  scan
  scanPairs
  scanValues
///

doc ///
 Key
  scanPairs
  (scanPairs, HashTable, Function)
  (scanPairs, Thing, Function)
 Headline
  apply a function to the pairs in a hash table
 Usage
  scanPairs(t, f)
 Inputs
  t:{HashTable, BasicList, Dictionary}
    or any instance of a class with an @TO iterator@ method installed
  f:Function
 Description
  Text
   {\tt scanPairs(t, f)} applies the function {\tt f} to each pair
   {\tt (k, t#k)} in the hash table {\tt t}. In other words, {\tt f}
   is applied to each key {\tt k} paired with its corresponding value
   {\tt t#k}.
  Example
   t = hashTable {{1,8},{2,20},{3,4},{4,20}}
   scanPairs(t, (k,v) -> print (k+v))
   scanPairs(t, (k,v) -> if v==20 then print k)
  Text
   If @CODE "t"@ is not a hash table, then @M2CODE "scan(pairs t, f)"@ is
   called.
  Example
   scanPairs({4, 5, 6}, print)
 Caveat
  This function requires an immutable hash table.  To scan the pairs in
  a mutable hash table, use {\tt scan(pairs t, f)}.
 SeeAlso
  "hash tables"
  applyPairs
  pairs
  scan
  scanKeys
  scanValues
///

doc ///
 Key
  scanValues
  (scanValues, HashTable, Function)
 Headline
  apply a function to each value in a hash table or database
 Usage
  scanValues(t, f)
 Inputs
  t:HashTable
  f:Function
 Description
  Text
   {\tt scanValues(t, f)} applies the function {\tt f} to each value
   in the hash table {\tt t}. 
  Example
   t = hashTable {{1,8},{2,20},{3,4},{4,20}}
   scanValues(t, print)
   scanValues(t, v -> if v>10 then print v)
 Caveat
  This function requires an immutable hash table.  To scan the values in
  a mutable hash table, use {\tt scan(values t, f)}.
 SeeAlso
  "hash tables"
  applyValues
  scan
  scanKeys
  scanPairs
  values
///

doc ///
 Key
  values
  (values, Dictionary)
  (values, HashTable)
 Headline
  values in a hash table
 Usage
  values t
 Inputs
  t:HashTable
 Outputs
  :List
   the values occurring in the hash table {\tt t}
 Description
  Example
   x = new HashTable from {a => 1, b => 2}
   values x
 SeeAlso
  "hash tables"
  applyValues
  keys
  pairs
  scanValues
  "#"
  "#?"
///

doc ///
 Key
  symbol #
 Headline
  length or access to elements
 Description
  Text
   {\tt #} is used as both a unary and a binary operator.
   
   As a unary operator: 
   {\tt #x} returns the length or cardinality of a list, 
   set, hash table, or string {\tt x}. 

   As a binary operator: 
   {\tt x#i} returns the {\tt i}th element of a list, hash table, 
   database, or string {\tt x}.
  Example
   L = {23, 42, 107, 2, 50};
   #L
   L#2
 Caveat
  The precedence of {\tt #} when used as a binary operator is high,
  as high as @TO "."@, but when used as a unary operator the
  precedence is much lower.
 SeeAlso
  "#?"
  (symbol #, BasicList)
  (symbol #, BasicList, ZZ)
  "hash tables"
  "lists and sequences"
///

doc ///
 Key
  (symbol #, List)
  (symbol #, Sequence)
  (symbol #, BasicList)
  (symbol #, String)
  (symbol #, Net)
  (symbol #, HashTable)
  (symbol #, Dictionary)
  (symbol #, Set)
 Headline
  length or cardinality of a list, hash table, dictionary, set, or string
 Usage
  #x
 Inputs
  x:{List, HashTable, Set, String}
 Outputs
  :ZZ
   the length of {\tt x}
 Description
  Text
   If {\tt x} is a list, {\tt #x} is the number of elements in {\tt x}.
  Example
   L = {1, 2, 3, 2, 1};
   #L
  Text
   If {\tt x} is a set, {\tt #x} is the cardinality of {\tt x}.
  Example
   S = new Set from L 
   #S
  Text
   If {\tt x} is a hash table, {\tt #x} is the number of 
   key-value pairs stored in {\tt x}.
  Example
   T = new HashTable from {a => 1, b => 2}
   #T
  Text
   If {\tt x} is a string, {\tt #x} is the number of characters in {\tt x}.
  Example
   s = "a perfectly adequate example of a string";
   #s
 SeeAlso
  symbol #
  symbol #?
  (symbol #, BasicList, ZZ)
  keys
  pairs
  values
  "hash tables"
  "lists and sequences"
///

doc///
 Key
  (symbol #, List, ZZ)
  (symbol #, Sequence, ZZ)
  (symbol #, BasicList, ZZ)
  (symbol #, Database, String)
  (symbol #, Dictionary, String)
  (symbol #, HashTable, Thing)
  (symbol #, String, ZZ)
  (symbol #, Net, ZZ)
 Headline
  get value from list, hash table, database, dictionary, or string
 Usage
  x#i
 Inputs
  x:{List, HashTable, Database, String}
  i:
   an index or key
 Description
  Text
   If {\tt x} is a list, {\tt x#i} returns the {\tt i}th element of {\tt x}.
   The entries of the list are numbered starting with 0. If {\tt i}
   is negative, then the entries are numbered ending with -1. If {\tt i}
   is out of range, an error is signaled.
  Example
   L = {a, b, c, b, a};
   L#2
   L#-2
  Text
   If {\tt x} is a hash table or database, {\tt x#i} provides the
   value associated with the key {\tt i}.
  Example
   T = new HashTable from {a => 103, b => 89.4, c => 92};
   T#a
   T#b
  Text
   If {\tt x} is a string, {\tt x#i} provides the {\tt i}th character of {\tt x},
   if there is one. Negative indices are counted backward from the end, as with
   lists. If {\tt i} is out of range, an error is thrown.
  Example
   s = "a perfectly adequate example of a string";
   s#2
   s#-2
  Text
   Assignment to {\tt x#i} can change {\tt x} if {\tt x} is mutable.
  Example
   V = new MutableHashTable from T;
   V#a = 5;
   V#d = 22.3;
   peek V
 SeeAlso
  symbol #
  symbol #?
  (symbol #, BasicList)
  (symbol _, VisibleList, ZZ)
  "hash tables"
  "lists and sequences"
///

doc///
 Key
  symbol #?
  (symbol #?, List, ZZ)
  (symbol #?, Sequence, ZZ)
  (symbol #?, BasicList, ZZ)
  (symbol #?, Database, String)
  (symbol #?, Dictionary, String)
  (symbol #?, HashTable, Thing)
  (symbol #?, String, ZZ)
  (symbol #?, Net, ZZ)
  (symbol #?, Nothing, Thing)
 Headline
  check existence of value in a list, hash table, database, or string
 Usage
  x#?i
 Inputs
  x:{List, HashTable, Database, String}
  i:
   an index or key
 Outputs
  :Boolean
   whether or not {\tt x} contains an element with index or key {\tt i}
 Description
  Text
   If {\tt x} is a list, {\tt x#?i} tells whether there is
   an {\tt i}th element of {\tt x}.
   The entries of the list are numbered starting with 0. If {\tt i}
   is negative, then the entries are numbered ending with -1. 
  Example
   L = {a, b, c, b, a};
   L#?2
   L#?12
  Text
   If {\tt x} is a hash table or database, {\tt x#?i} tells
   whether there is a value associated with the key {\tt i}.
  Example
   T = new HashTable from {a => 103, b => 89.4, c => 92};
   T#?a
   T#?A
  Text
   If {\tt x} is a string, {\tt x#?i} tells if {\tt x} has an
   {\tt i}th character.
  Example
   s = "a perfectly adequate example of a string";
   s#?2
   s#?52
  Text
   {\tt #?} can be very useful in avoiding errors from attempting
   to access nonexistent elements of lists or hash tables. 
 SeeAlso
  symbol #
  (symbol #, BasicList)
  (symbol _, VisibleList, ZZ)
  "hash tables"
  "lists and sequences"
///

document {
     Key => ".",
     Headline => "access to elements whose key is a symbol",
     TT "x.k", " -- the same as ", TT "x#(global k)", ", i.e., treat ", TT "k", " as
     a global symbol and provide the value stored in the hash table ", TT "x", " 
     under the key ", TT "k", ".",
     PARA{},
     "May also be used in an assignment.",
     PARA{},
     EXAMPLE {
	  "x = new MutableHashTable;",
      	  "x.k = 444",
      	  "x.k",
      	  "peek x",
	  },
     SeeAlso => {"#", ".?", "global"}
     }
document {
     Key => ".?",
     Headline => "check for presence of elements whose key is a symbol",
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA{},
     SeeAlso =>{ ".", "#?" }
     }

document {
     Key => hash,
     Headline => "hash code of an object",
     Usage => "hash x",
     Inputs => { "x" => Thing },
     Outputs => { ZZ => { "the hash code of ", TT "x" } },
     PARA{
	  "The hash code of ", TT "x", " is an integer produced in a deterministic way
	  from ", TT "x", ", and perhaps from the hash codes of the contents of ", TT "x", ".
	  See ", TO "hashing", " for a discussion of the requirements that
	  the hash codes used here are designed to satisfy."
	  },
     PARA {
	  "Hash codes may change from one version of Macaulay2 to the next, but changes to the hash codes
	  of the basic types will be avoided, if possible.  That includes lists, sequences, strings, hash 
	  tables, options, Boolean values, and numbers."
	  }
     }
