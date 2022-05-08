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
