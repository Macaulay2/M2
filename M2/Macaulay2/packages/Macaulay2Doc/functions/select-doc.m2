--- status: Rewritten July 2020
--- author(s): Mahrud
--- notes:

-- TODO: bring all uses as synopses under this node
doc ///
  Key
    select
  Headline
    select from a list, hash table, or string
  SeeAlso
    positions
    partition
    selectInSubring
    selectVariables
    selectKeys
    selectValues
    selectPairs
    sublists
  Subnodes
    (select, BasicList, Type)
    (select, BasicList, Function)
    (select, Thing, Function)
    (select, ZZ, BasicList, Function)
    (select, ZZ, Function)
///

doc ///
  Key
    (select, String, String, String)
    (select, String, String)
    [(select, String, String), POSIX]
    [(select, String, String, String), POSIX]
  Headline
    select and reformat substrings matching a regular expression
  Usage
    select(re, replacement, str)
    select(re, str)
  Inputs
    re:String
      a @TO2 {"regular expressions", "regular expression"}@ describing a pattern
    replacement:String
      following the @TO2 {"regular expressions", "formatting syntax"}@
    str:String
      a subject string to be searched
    POSIX=>Boolean
      if true, interpret the @TT "re"@ using the POSIX Extended flavor, otherwise the Perl flavor
  Outputs
    :List
      of mutually exclusive substrings of @TT "str"@ matching the pattern @TT "re"@;
      if @TT "replacement"@ is given, the matching substrings are formatted based on it.
  Description
    Text
      For an introduction to regular expressions, see @TO "regular expressions"@.
    Example
      select("[[:alpha:]]+", "Dog, cat, and deer.")
      select("^.*$", "ABC\nDEF\r\nGHI")
    Text
      The @TT "replacement"@ string may contain @TO2 {"regular expressions", "formatting syntax"}@
      such as backreferences @TT "$1"@ or @TT "\\\\1"@, which will be replaced by the string matching
      the corresponding parenthesized subexpression of @TT "re"@.
    Example
      select("([a-zA-Z]+);", "$1", "Dog; cat, deer;")
    Text
      Special operators such as the lowercase operator @TT "\\\\L"@ may also be used to manipulate the
      replacement substring.
    Example
      select("([a-zA-Z]+);", "\\L$1", "Dog; cat, deer;")
    Text
      Lookaheads and lookbehinds can be used to precisely control the regular expression matches.
    Example
      s = "catfish cats dogs";
      select("cat(?!fish)s?", s)
      select("\\w+(?=s\\b)", s)
      s = "goldfish swordfish catfish catdog";
      select("\\w+(?=fish)", s)
      select("(?<=cat)\\w+", s)
    Text
      The @TT "POSIX => true"@ option can be used to specify the POSIX Extended flavor for the regular
      expression used to match. Note that only the Perl flavor allows the use of lookaheads and lookbehinds.
  SeeAlso
    "regular expressions"
    "strings and nets"
    regex
    separate
    (replace, String, String, String)
///

doc ///
Node
  Key
    (select, BasicList, Type)
  Headline
    select elements of a given type in a list
  Usage
    select(L, T)
  Inputs
    L:BasicList
    T:Type
  Outputs
    :BasicList
      containing elements of @TT "L"@ whose class inherits from type @TT "T"@
  Description
    Text
      The order of the elements in the result will be the same as in the original list @TT "L"@,
      and the class of the result will be the same as the class of @TT "L"@.
    Example
      select({1,"2",3.14,4+5*ii}, ZZ)
      select([1,"2",3.14,4+5*ii], RR)
  SeeAlso
    (select, BasicList, Function)
///

document { 
     Key => (select,ZZ,BasicList,Function),
     Headline => "select a limited number of elements from a list",
     Usage => "select(n,v,f)",
     Inputs => { "n", "v", "f" => {"returning either ", TO "true", " or ", TO "false"}},
     Outputs => {
	  {"a list containing at most ", TT "n", " elements of the list ", TT "v", " 
	       that yield ", TT "true", " when the function ", TT "f", " is applied."}
	  },
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ".",
     EXAMPLE {
	  ///select(4,0..10,even)///
	  },
     SeeAlso => {(select,BasicList,Function),partition}
     }

document { 
    Key => {
	 selectValues,
	(selectValues, HashTable, Function),
	(selectValues, ZZ, HashTable, Function),
	(select, HashTable, Function),
	(select, ZZ, HashTable, Function),
    },
     Headline => "select part of a hash table by values",
     Usage => "selectValues(v,f)\nselectValues(n,v,f)",
     Inputs => { "n" => ZZ, "v" => HashTable, "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"whose pairs are those key-value pairs ", TT "(k,w)", " of the hash table ", TT "v", " that
	       yield ", TT "true", " when the function ", TT "f", " is applied to the value ", TT "w", ".",
	   "If ", TT "n", " is provided, at most ", TT "n", " pairs will be selected."}
	  },
     "The hash table ", TT "v", " should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", ".",
     EXAMPLE {
	  "x = new HashTable from { x => 1, y => 2, z => 3 }",
	  "select(x,odd)",
	  "select(1,x,odd)"
	  },
     SeeAlso => {
	 partition,
	 selectKeys,
	 selectPairs}
     }

document { 
     Key => (select,BasicList,Function),
     Headline => "select elements from a list",
     Usage => "select(v,f)",
     Inputs => { "v", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"a list of those elements of the list ", TT "v", " that yield ", TT "true", " when the function ", TT "f", " is applied"}
	  },
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ", and the class of the result 
     will be the same as the class of ", TT "v", ".",
     EXAMPLE {
	  "select({1,2,3,4,5}, odd)",
	  "select([1,2,3,4,5], odd)",
	  },
     SeeAlso => {(select,ZZ,BasicList,Function), partition, positions}
     }


document { 
     Key => (select,ZZ,Function),
     Headline => "select integers",
     Usage => "select(n,f)",
     Inputs => { "n", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"a list of those natural numbers ", TT "i", " less than ", TT "n", " that yield
	       ", TT "true", " when the function ", TT "f", " is applied"}
	  },
     EXAMPLE {
	  "select(20, odd)",
	  "select(20, even)",
	  },
     SeeAlso => {(select,ZZ,BasicList,Function), partition, positions}
     }

doc ///
  Key
    (select, Thing, Function)
  Headline
    select elements from an object with an iterator
  Usage
    select(x, f)
  Inputs
    x:Thing -- an instance of a class with the @TO iterator@ method installed
    f:Function -- returning either @TO true@ or @TO false@
  Outputs
    :Iterator
  Description
    Text
      Suppose @TT "x"@ is an instance of a class with the @TO iterator@ method
      installed, e.g., a string, and suppose @TT "iter"@ is the output of
      @TT "iterator x"@.  Then a new @TO Iterator@ object is returned
      whose @TO next@ method returns the next output of @TT "next iter"@
      for which @TT "f next iter" @ is true, unless @TT "next iter"@ returns
      @TO StopIteration@, in which case this new iterator does the same.
    Example
      selectiter = select("foo", i -> i == "o")
      next selectiter
      next selectiter
      next selectiter
  SeeAlso
    iterator
    Iterator
    next
    StopIteration
    (apply, Thing, Function)
///

doc ///
  Key
    selectKeys
    (selectKeys, HashTable, Function)
    (selectKeys, ZZ, HashTable, Function)
  Headline
    select a part of a hash table by keys
  Usage
    selectKeys(x, f)
    selectKeys(n, x, f)
  Inputs
    n:ZZ
    x:HashTable -- must be immutable
    f:Function
  Outputs
    :HashTable
      containing all (or @VAR "n"@, if it is given) key-value pairs
      (@VAR "k"@,@VAR "v"@) from @VAR "x"@ for which @CODE "f k"@ evaluates to
      true.
  Description
    Example
      x = hashTable{(1, a), (2, b), (3, c), (4, d), (5, e)}
      selectKeys(x, odd)
      selectKeys(2, x, odd)
  SeeAlso
    selectValues
    selectPairs
///

doc ///
  Key
    selectPairs
    (selectPairs, BasicList, Function)
    (selectPairs, HashTable, Function)
    (selectPairs, ZZ, BasicList, Function)
    (selectPairs, ZZ, HashTable, Function)
  Headline
    select a part of a hash table by pairs
  Usage
    selectPairs(x, f)
    selectPairs(n, x, f)
  Inputs
    n:ZZ
    x:{HashTable, BasicList}
    f:Function
  Outputs
    :{HashTable, BasicList}
      containing all (or @VAR "n"@, if it is given) key-value pairs
      (@VAR "k"@,@VAR "v"@) from @VAR "x"@ for which @CODE "f(k,v)"@ evaluates
      to true.
  Description
    Example
      x = hashTable{(1, 2), (2, 4), (3, 6), (4, 8), (5, 10)}
      selectPairs(x, (k,v) -> odd(k + v))
      selectPairs(2, x, (k, v) -> odd(k + v))
    Text
      If @CODE "x"@ is not a hash table, then @M2CODE "select(pairs x, f)"@
      (or @M2CODE "select(n, pairs x, f)"@) is called.
    Example
      selectPairs(toList(1..10), (i, x) -> even x)
      selectPairs(3, toList(1..10), (i, x) -> even x)
  Caveat
    If @CODE "x"@ is a hash table, then it must be immutable.
  SeeAlso
    selectValues
    selectKeys
///

doc ///
  Key
    (select, Set, Function)
    (select, ZZ, Set, Function)
  Headline
    select a part of a set
  Usage
    select(x, f)
    select(n, x, f)
  Inputs
    x:Set
    f:Function
  Outputs
    :Set
      containing all (or @VAR "n"@, if it is given) elements of @VAR "x"@
      that evaluate to true when passed to @VAR "f"@
  Description
    Example
      x = set(1..10)
      select(x, odd)
      select(2, x, odd)
///
