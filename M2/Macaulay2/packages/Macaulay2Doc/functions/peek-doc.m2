--- status: TODO
--- author(s): 
--- notes: 

undocumented toList(set methods peek' - set{(peek', ZZ, Thing)})

doc ///
Node
  Key
    peek
  Headline
    examine contents of an object
  Usage
    peek o
  Inputs
    o:Thing
      any Macaulay2 object
  Outputs
    :Net
      laying out the internal contents of @TT "o"@ to depth 1, bypassing installed methods for displaying the object
  Description
    Text
      This function is used during debugging Macaulay2 programs to examine the internal structure of objects.
    Example
      set {1, 2, 3}
      peek oo
      new MutableHashTable from { a => 3, b => 44}
      peek oo
  SeeAlso
    peek'

Node
  Key
     peek'
    (peek', ZZ, Thing)
  Headline
    examine contents of an object
  Usage
    peek'(n, o)
  Inputs
    n:ZZ
    o:Thing
      any Macaulay2 object
  Outputs
    :Net
      laying out the contents of @TT "o"@, bypassing installed formatting and printing methods to depth @TT "n"@
  Description
    Example
      s = factor 112
      peek s
      peek'_2 s
  SeeAlso
    peek
///
