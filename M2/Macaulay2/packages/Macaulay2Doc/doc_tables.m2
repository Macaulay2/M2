--authors: Dan Grayson, Lily Silverstein

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
///


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
  mutable
  (mutable, Thing)
 Headline
  whether something may be modified
 Usage
  mutable x
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
   
   If {\tt x} is anything else, {\tt mutable x} will return {\bf false}.
  Example
   T = new MutableList from (a, b, c)
   mutable T
   V = new List from (a, b, c)
   mutable V
   mutable join(T, V)
   mutable a
   mutable "a"  
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
  remove
  (remove, HashTable, Thing)
 Headline
  remove an entry from a mutable hash table
 Usage
  remove(T, k)
 Inputs
  T:HashTable
  k:
   key
 Outputs
  :Nothing
 Description
  Text
   {\tt remove(T, k)} removes the entry of {\tt T} stored under
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
   The {\tt remove} command does not return any output.
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
  (symbol #, BasicList)
  (symbol #, HashTable)
  (symbol #, Set)
  (symbol #, String)
 Headline
  length or cardinality
 Usage
  #x
 Inputs
  x:BasicList
   HashTable, Set, or String
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
  (symbol #, BasicList, ZZ)
  (symbol #, Database, String)
  (symbol #, HashTable, Thing)
  (symbol #, String, ZZ)
 Headline
  get value from list, hash table, database, or string
 Usage
  x#i
 Inputs
  x:
   a list, hash table, or string
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
  (symbol #?, BasicList, ZZ)
  (symbol #?, Database, String)
  (symbol #?, HashTable, Thing)
  (symbol #?, String, ZZ)
 Headline
  check existence of value in a list, hash table, database, or string
 Usage
  x#?i
 Inputs
  x:
   a list, hash table, or string
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

