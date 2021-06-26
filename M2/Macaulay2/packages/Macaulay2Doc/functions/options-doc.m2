--- status:
--- author(s):
--- notes:

doc ///
Node
  Key
    options
  Headline
    get options
  SeeAlso
    Option

Node
  Key
    Option
  Headline
    the class of all key-value pairs K => V
  Description
    Text
      Such pairs are used as optional arguments for functions.
    Example
      o = Limit => 5
      peek o
    Text
      There is also a way to make new @TO2{HashTable, "hash tables"}@ from lists of key-value pairs.
    Example
      ht = new HashTable from {a => 5, b => 7}
      keys ht
      values ht
      pairs ht
    Text
      These pairs are implemented as lists, so that if {\tt z} is {\tt x => y},
      then {\tt x} is {\tt z#0} and {\tt y} is {\tt z#1}.
    Example
      o#0
      o#1
  SeeAlso
    (NewFromMethod, HashTable, List)
    (symbol=>, Thing, Thing)
    options

Node
  Key
    (options, Command)
    (options, Function)
    (options, Sequence)
    (options, ZZ)
  Headline
    get optional arguments and default values of functions and methods
  Usage
    options f
  Inputs
    f:{Command,Function,Sequence,ZZ}
  Outputs
    :{OptionTable,Boolean}
  Description
    Text
      The keys of the output are the names of the optional arguments accepted by
      the function {\tt f} and the values are the corresponding default values;
      or @TO "true"@, if the function accepts arbitrary options and provides no default values.
    Example
      options res
      options codim
      options(codim, Ideal)
      methods intersect
      options 0
  SeeAlso
    methods
///
