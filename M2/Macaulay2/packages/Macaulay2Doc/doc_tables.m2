doc///
 Key
  table
 Headline
  make a table from a binary function
 Usage
  table(a, b, f)
  table(m, n, f)
 Inputs
  a:List
  b:List
  m:ZZ
  n:ZZ
  f:
   a function {\tt f(i,j)} of two variables
 Outputs
  T:
   a table, or list of lists, where $T_{ij}$ is the value
   of $f(a_i, b_j)$, OR, if using integer arguments $m$ and $n$, 
   $T_{ij}=f(i,j)$ for $0\le i < m, 0\le j < n$
 Description
  Text
   The command {\tt table(m, n, f)} is equivalent to 
   {\tt table(0..(m-1), 0..(n-1), f)}.
  Example
   t1 = table({1,3,5,7}, {0,1,2,4}, (i,j) -> i^j)
   t2 = table(5, 5, (i,j) -> i+j)
  Text
   Tables can be displayed nicely using @TO netList@.
  Example
   netList t1
 SeeAlso
  applyTable
  isTable
  subtable
  "lists and sequences"
///

doc///
 Key
  applyTable
 Headline
  apply a function to each element of a table
 Usage
  applyTable(T, f)
 Inputs
  T:List
   a table (list of lists of the same length)
  f:Function
 Outputs
  A:List
   a table of the same shape as $T$, where the function
   $f$ has been applied elementwise
 Description
  Example
   t = table({1,3,5,7}, {0,1,2,4}, (i,j) -> i^j);
   netList t
   netList applyTable(t, i -> 2*i)
   netList applyTable(t, isPrime)
 SeeAlso
  isTable
  subtable
  table
  "lists and sequences"
///

doc ///
 Key
  subtable
 Headline
  extract a subtable from a table
 Usage
  subtable(a, b, T)
 Inputs
  a:List
   of rows to extract
  b:List
   of columns to extract
 Outputs
  S:
   the subtable of $T$ defined by restricting to rows in the list $a$
   and columns in the list $b$
 Description
  Example
   t = table({1,3,5,7}, {0,1,2,4}, (i,j) -> i^j);
   netList t
   s1 = subtable({0,2}, {1,3}, t);
   netList s1
   s2 = subtable(toList(0..3), {1}, t);
   netList s2
 SeeAlso
  applyTable
  isTable
  positions
  table
  select
  "lists and sequences"
///

doc///
 Key
  isTable
 Headline
  whether something is a list of lists of equal length
 Usage
  isTable t
 Inputs
  t:Thing
 Outputs
  b:Boolean
   whether or not $t$ is a table
 Description
  Example
   isTable {{1,2,3},{4,5,6}}
   isTable {{1,2,3},{4,5}}
 Caveat
  It is intrinsically impossible to represent a $0\times k$ matrix
  as a list of lists.
 SeeAlso
  applyTable
  table
  subtable
  "lists and sequences"
///
 

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
   underlying this, see @TO hashing@.
   
   One important feature of hash tables that when the keys
   are consecutive integers starting at 0, the keys are scanned
   in the natural order.

   There is a subclass of HashTable called @TO MutableHashTable@
   which consists of those hash tables whose entries can be changed.
   
   Access functions:
     @UL {
 	  TO "#",
 	  TO "."
 	  }@
    
    Query functions:
     @UL {
 	  TO "#?",
 	  TO ".?"
 	  }@
///


doc///
 Key
  keys
  (keys, HashTable)
 Headline
  keys used in a hash table
 Usage
  keys t
 Inputs
  t:HashTable
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
  pairs
  (pairs, HashTable)
  (pairs, Dictionary)
  (pairs, BasicList)
 Headline
  list the pairs in a hash table, dictionary, or basic list
 Usage
  pairs x
 Inputs
  x:HashTable
   or @TO Dictionary@ or @TO BasicList@
 Outputs
  L:List
   of all pairs {\tt (k, x#k)}
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
 Headline
  apply a function to the pairs in a hash table
 Usage
  scanPairs(t, f)
 Inputs
  t:HashTable
  f:Function
 Description
  Text
   {\tt scanPairs(t, f)} applies the function {\tt f} to each pair
   {\tt (k, t#k)} in the hash table {\tt t}. In other words, {\tt f}
   is applied to each key {\tt k} paired with its corresponding value
   {\tt t#k}.
  Example
   t = hashTable {{1,8},{2,20},{3,4},{4,20}}
   applyPairs(t, (k,v) -> k+v)
   scanPairs(t, (k,v) -> print (k+v))
   scanPairs(t, (k,v) -> if v==20 then print k)   
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


document {
     Key => symbol #,
     Headline => "length, or access to elements",
     "The precedence of ", TT "#", " when used as a binary operator is high,
     as high as ", TT ".", ", but the precedence when used as a unary operator
     is lower, as low as adjacency or function application.",
     SeeAlso =>{ "#?" }
     }
document {
     Key => (symbol #, BasicList),
     Headline => "length",
     TT "#x", " -- provides the length of a list.",
     }
document {
     Key => (symbol #, HashTable),
     Headline => "length",
     TT "#x", " -- provides the number of key-value pairs recorded
     in a hash table.",
     }
document {
     Key => (symbol #, Set),
     Headline => "cardinality",
     TT "#x", " -- provides the number of elements in the set ", TT "x", "."
     }
document {
     Key => (symbol #, String),
     Headline => "length",
     TT "#x", " -- provides the length of a string.",
     }
document {
     Key => (symbol #, HashTable, Thing),
     Headline => "get value from hash table",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the hash table
     ", TT "x", ".",
     PARA{},
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable.",
     EXAMPLE {
	  "x = new MutableHashTable",
	  "x#i = p",
	  "x#i",
	  },
     SeeAlso => {(symbol #?, HashTable, Thing), "hashing"}
     }
document {
     Key => (symbol #, Database, String),
     Headline => "get value from database",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the database
     ", TT "x", ".",
     SeeAlso => {(symbol #?, Database, String)}
     }
document {
     Key => (symbol #, String, ZZ),
     Headline => "get character from string",
     TT "x#i", " -- provides the ", TT "i", "-th character of the string ", TT "x", ",
     as a string of length 1, if there is one.",
     PARA{},
     "If ", TT "i", " is out of range, a string of length 0 is returned.
     If  ", TT "i", " is negative, then the ", TT "i", "-th character
     from the end is provided.",
     SeeAlso => {(symbol #?, String, ZZ)}
     }
document {
     Key => (symbol #, BasicList, ZZ),
     Headline => "get element from list",
     Usage => "x#i",
     Inputs => { "x", "i" },
     Outputs => { { "the ", TT "i", "-th element of the list ", TT "x" }},
     SeeAlso => {(symbol _, VisibleList, ZZ)},
     PARA{
     	  "The entries of the list are numbered starting with 0.  If  ", TT "i", " 
          is negative, then the ", TT "i", "-th entry counting from the end is provided.
          If ", TT "i", " is out of range, an error is signaled." },
     PARA{
	  "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is
          mutable, i.e., an instance of the class ", TO "MutableList", "." },
     EXAMPLE lines ///
          x = a .. z
	  x#12
	  y = new MutableList from x
	  y#12 = foo
	  toSequence y
     ///
     }
document {
     Key => (symbol #?, HashTable, Thing),
     Headline => "check for value in hash table",
     TT "x#?i", " -- tells whether there is a value associated to the
     key ", TT "i", " stored in the hash table ", TT "x", ".",
     SeeAlso => {(symbol #, HashTable, Thing), "hashing"}
     }
document {
     Key => (symbol #?, Database, String),
     Headline => "check for value in database",
     TT "x#?i", " -- tells whether there is a value associated to the string
     ", TT "i", " in the database ", TT "x", ".",
     SeeAlso => {(symbol #, Database, String)}
     }
document {
     Key => (symbol #?, String, ZZ),
     Headline => "check for character in string",
     TT "x#?i", " -- tells whether there is an ", TT "i", "-th character in
     the string ", TT "x", ".",
     EXAMPLE {
	  ///"asdf" #? 2///,
	  ///"asdf" #? 22///
	  },
     SeeAlso => {(symbol #, String, ZZ)}
     }
document {
     Key => (symbol #?, BasicList, ZZ),
     Headline => "check for element in list",
     TT "x#?i", " --  tells whether there is an ", TT "i", "-th element in
     the list ", TT "x", ".",
     EXAMPLE {
	  ///{a,b,c} #? 2///,
	  ///{a,b,c} #? 22///
	  },
     SeeAlso => {(symbol #, BasicList, ZZ)}
     }
document {
     Key => symbol #?,
     Headline => "check for presence of elements",
     SeeAlso =>{ "#" }
     }

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

document {
     Key => {remove,(remove, HashTable, Thing)},
     Headline => "remove an entry from a hash table",
     TT "remove(x,k)", " -- removes the entry stored in the hash table ", TT "x", "
     under the key ", TT "k", ".",
     PARA{},
     EXAMPLE {
	  "x = new MutableHashTable from {a => 1, b => 2}",
	  "remove(x,a)",
	  "x"
	  }
     }

document {
     Key => {mutable,(mutable, Thing)},
     Headline => "whether something may be modified",
     TT "mutable x", " -- returns true or false, depending on whether x is mutable.",
     PARA{},
     "If ", TT "x", " is a hash table, list, dictionary, or database, then it is mutable if its contents
     can be destructively altered.",
     PARA{},
     "If ", TT "x", " is a symbol, then it's mutable if a value can be assigned to
     it. (See ", TO "protect", ".)",
     PARA{},
     "If ", TT "x", " is anything else, then it isn't mutable.",
     PARA{},
     "The (changeable) contents of a mutable hash table or list do not participate in strong comparison
     with ", TO "===", " or in ", TO "hashing", ".",
     SeeAlso => {"MutableList", "MutableHashTable"}
     }

