doc ///
  Key
    shuffle
    (shuffle, List)
    (shuffle, MutableList)
    (shuffle, List, ZZ)
    (random, List)
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
    Text
      Currently, @TO random@ has this behavior when given a list.
      However, in the near future, this will change to the behavior of
      @TO randomElement@.
    Example
      random toList(0..12)
  SeeAlso
    setRandomSeed
    randomSubset
///
