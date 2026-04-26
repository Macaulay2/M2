doc ///
  Key
    shuffle
    (shuffle, List)
    (shuffle, MutableList)
    (shuffle, List, ZZ)
  Headline
    shuffle a list randomly
  Usage
    shuffle L
    shuffle(L, n)
  Inputs
    L:List
    n:ZZ
  Outputs
    :List
      a new list containing the elements of @TT "L"@ in a shuffled random order
  Description
    Example
      shuffle toList (0 .. 12)
    Text
      Mutable lists are shuffled in place.
    Example
      x = new MutableList from 0..12; peek x
      shuffle x; peek x
    Text
      Is @VAR "n"@ is given, then a shuffled random sublist of length
      @VAR "n"@ is returned.  Contrast this with @TO randomSubset@,
      which preserves the order.
    Example
      shuffle(toList(0..12), 4)
  SeeAlso
    setRandomSeed
    (random, List)
    randomSubset
///
