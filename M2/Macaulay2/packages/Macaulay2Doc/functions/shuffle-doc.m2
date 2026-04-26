doc ///
  Key
    shuffle
    (shuffle, List)
    (shuffle, MutableList)
  Headline
    shuffle a list randomly
  Usage
    shuffle L
  Inputs
    L:List
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
  SeeAlso
    setRandomSeed
    (random, List)
///
