--- status: Rewritten July 2020
--- author(s): Mahrud
--- notes:

doc ///
  Key
    select
  Headline
    select from a list, hash table, or string
  SeeAlso
    partition
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
      a replacement or regex formatting string, may include backreferences
    str:String
      a subject string to be searched
    POSIX=>Boolean
      if true, interpret the @TT "re"@ using the POSIX Extended flavor, otherwise the Perl flavor
  Outputs
    :List
      a list of mutually exclusive substrings of @TT "str"@ matching the pattern @TT "re"@;
      if @TT "replacement"@ is given, the matching substrings are formatted based on it.
  Description
    Text
      For an introduction to regular expressions, see @TO "regular expressions"@.
    Example
      select("[[:alpha:]]+", "Dog, cat, and deer.")
      select("^.*$", "ABC\nDEF\r\nGHI")
    Text
      The @TT "replacement"@ string can contain backreferences such as @TT "$1"@ or @TT "\\\\1"@, which
      will be replaced by the string matching the corresponding parenthesized subexpression of @TT "re"@.
    Example
      select("([a-zA-Z]+);", "$1", "Dog; cat, deer;")
    Text
      Special operators such as the lowercase operator @TT "\\\\L"@ may also be used to manipulate the
      replacement substring.
    Example
      select("([a-zA-Z]+);", "\\L$1", "Dog; cat, deer;")
    Text
      The @TT "POSIX => true"@ option can be used to specify the POSIX Extended flavor for the regular
      expression used to match. Note that only the Perl flavor allows the use of lookahead and lookbehinds.
    Example
      s = "catfish cats dogs"
      select("cat(?!fish)", s)
      select("\\w+(?=s\\b)", s)
      s = "goldfish swordfish catfish catdog"
      select("\\w+(?=fish)", s)
      select("(?<=cat)\\w+", s)
  SeeAlso
    "regular expressions"
    "strings and nets"
    regex
    replace
    separate
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
     Key => (select,HashTable,Function),
     Headline => "select part of a hash table",
     Usage => "select(v,f)",
     Inputs => { "v", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"whose pairs are those key-value pairs ", TT "(k,w)", " of the hash table ", TT "v", " that
	       yield ", TT "true", " when the function ", TT "f", " is applied to the value ", TT "w", "."}
	  },
     "The hash table ", TT "v", " should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", ".",
     EXAMPLE {
	  "x = new HashTable from { x => 1, y => 2, z => 3 }",
	  "select(x,odd)"
	  },
     SeeAlso => {partition}
     }

document { 
     Key => (select,ZZ,HashTable,Function),
     Headline => "select a limited number of pairs from a hash table",
     Usage => "select(n,v,f)",
     Inputs => { "n", "v", "f" => {"returning either ", TO "true", " or ", TO "false"} },
     Outputs => {
	  {"whose pairs are those key-value pairs of the hash table ", TT "v", " that
	       yield ", TT "true", " when the function ", TT "f", " is applied to the value,
	       except that at most ", TT "n", " pairs will be selected"}
	  },
     "The hash table ", TT "v", " should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", ".",
     EXAMPLE {
	  "x = new HashTable from { x => 1, y => 2, z => 3 }",
	  "select(1,x,odd)"
	  },
     SeeAlso => {(select,HashTable,Function), partition}
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
