doc ///
  Key
    tally
    (tally, VisibleList)
    (tally, String)
  Headline
    tally the elements of a list, sequence, array, or string
  Usage
    y = tally x
  Inputs
    x:{VisibleList,String}
  Outputs
    y:Tally
  Description
    Text
      It produces a hash table (multiset) @TT "y"@ which tallies the frequencies
      of the occurrences of items in the list or string @TT "x"@, i.e.,
      @TT "y_i"@ is the number of times @TT "i"@ appears in @TT "x"@, or is
      @TT "0"@ if @TT "i"@ doesn't appear in the list or string.
    Example
      y = tally {1,2,3,a,b,1,2,a,1,2,{a,b},{a,b},a}
      y_2
      y_5
      y_{a,b}
      tally "Hello, world!"
  SeeAlso
    Tally
///

doc ///
  Key
    Tally
  Headline
    the class of all tally results
  Description
    Text
      @TT "Tally"@ -- a class designed to hold tally results, i.e., multisets.
  SeeAlso
    Set
  Subnodes
    (symbol +, Tally, Tally)
    (symbol -, Tally, Tally)
///

doc ///
  Key
    VirtualTally
  Description
    Text
      The only difference between this class and @TO "Tally"@ is that this
      class allows negative numbers.
    Example
      x = tally {a,b,b,c,c,c}
      y = tally {a,a,a,b,b,c}
      x' = new VirtualTally from x
      y' = new VirtualTally from y
      x-y
      x'-y'
  Subnodes
    Tally
    BettiTally
    (symbol **, VirtualTally, VirtualTally)
    (symbol ^**, VirtualTally, ZZ)
    (symbol +, VirtualTally, VirtualTally)
    (symbol -, VirtualTally, VirtualTally)
    (symbol -, VirtualTally)
    (symbol _, VirtualTally, Thing)
    (product, VirtualTally)
    (sum, VirtualTally)
///

doc ///
  Key
    (symbol **, VirtualTally, VirtualTally)
  Headline
    Cartesian product of tallies
  Description
    Text
      @TT "x ** y"@ -- produces the Cartesian product of two tallies.

      One of the arguments may be a @TO "Set"@.
    Example
      x = tally {a,a,b}
      y = tally {1,2,2,2}
      x ** y
  SeeAlso
    "Tally"
    "tally"
///

-- TODO
doc ///
  Key
    (symbol -, VirtualTally, VirtualTally)
///

doc ///
  Key
    (symbol +, VirtualTally, VirtualTally)
  Headline
    union of tallies
  Description
    Text
      @TT "x + y"@ -- produces the union of two tallies.

      One of the arguments may be a @TO "Set"@.
    Example
      x = tally {a,b,b,c,c,c,d,d,d}
      y = tally {a,a,a,b,b,c,d}
      x' = new VirtualTally from x
      y' = new VirtualTally from y
      z' = y' - x'
      z' + x'
      z' + y'
///

doc ///
  Key
    (symbol -, VirtualTally)
  Headline
    negation of a virtual tally
  Description
    Text
      @TT "-x"@ -- the negation of @TT "x"@
    Example
      x = tally {a,b,b,c,c,d,d,d}
      x' = new VirtualTally from x
      - x'
///

doc ///
  Key
    (symbol +, Tally, Tally)
  Headline
    union of tallies
  Description
    Text
      @TT "x + y"@ -- produces the union of two tallies.

      One of the arguments may be a @TO "Set"@.
    Example
      x = tally {a,a,a,b,b,c}
      y = tally {b,c,c,d,d,d}
      x + y
  SeeAlso
    "Tally"
    "tally"
///

doc ///
  Key
    (symbol -, Tally, Tally)
  Headline
    difference of tallies
  Usage
    x - y
  Inputs
    x:
    y:
  Outputs
    :
      the difference of the two tallies
  Description
    Text
      The count associated to an item @TT "i"@ in the result is the difference
      of the counts in @TT "x"@ and in @TT "y"@ if it's positive, otherwise,
      zero.
    Example
      tally {a,a,b,c} - tally {c,d,d}
  SeeAlso
    "Tally"
///

doc ///
  Key
    (symbol ^**, VirtualTally, ZZ)
  Headline
    Cartesian power of sets and tallies
  Usage
    B = A^**n
  Inputs
    A:
    n:
  Outputs
    B:
      the tally of @TT "n"@-tuples of elements from @TT "A"@
  Description
    Text
      If @TT "A"@ is @ofClass Set@, then so is @TT "B"@.
    Example
      A = set {1,2}
      A^**3
      A = tally {1,1,2}
      A^**3
  SeeAlso
    Set
    (symbol**,Set,Set)
///

doc ///
  Key
    (symbol _, VirtualTally, Thing)
  Headline
    get a count from a tally
  Usage
    t_x
  Inputs
    t:
    x:
  Outputs
    :ZZ
      the number of times @TT "x"@ is counted by @TT "t"@
  Description
    Example
      t = tally apply(1..10000, i -> # factor i)
      t_5
      t_6
  SeeAlso
    Tally
///
