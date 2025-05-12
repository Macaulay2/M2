doc ///
  Key
     randomSubset
    (randomSubset, VisibleList, ZZ)
    (randomSubset, Set, ZZ)
    (randomSubset, ZZ, ZZ)
    (randomSubset, VisibleList)
    (randomSubset, Set)
    (randomSubset, ZZ)
  Usage
    randomSubset(x, n)
    randomSubset x
  Inputs
    x:{VisibleList, Set, ZZ}
    n:ZZ
  Description
    Text
      When @VAR "n"@ is given, then a random subset @VAR "x"@ of cardinality
      @VAR "n"@ is returned.
    Example
      randomSubset({2, 3, 5, 7, 11}, 2)
    Text
      Otherwise, a random subset of arbitrary cardinality is returned.
    Example
      randomSubset {2, 3, 5, 7, 11}
      randomSubset {2, 3, 5, 7, 11}
      randomSubset {2, 3, 5, 7, 11}
    Text
      If @VAR "x"@ is an integer, then a subset of $\{0,\ldots,x - 1\}$ is
      returned.
    Example
      randomSubset(4, 2)
      randomSubset 5
  References
    Knuth, Donald E.
    @EM "The Art of Computer Programming: Seminumerical Algorithms, Volume 2"@.
    Addison-Wesley Professional, 2014. (Algorithm S, Section 3.4.2)
  SeeAlso
    random
///
