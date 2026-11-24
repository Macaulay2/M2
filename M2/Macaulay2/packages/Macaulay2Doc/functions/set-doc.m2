undocumented {
    (NewFromMethod, Set, Set),
    (NewFromMethod, Set, HashTable),
    (NewFromMethod, Set, VisibleList),
}

doc ///
  Key
    set
    (set,VisibleList)
    (set,HashTable)
    (set, Set)
  Headline
    make a set
  Usage
    set v
    set(v1,v2,...)
  Inputs
    v:
      @ofClass {VisibleList, HashTable}@
  Outputs
    :Set
      the set whose elements are the members of the list v or keys of the hash
      table v
  Description
    Example
      v = {1,2,3,2,1}
      S = set v
      T = set(a,b,c,a,b,d)
  SeeAlso
    Set
///

doc ///
  Key
    Set
  Headline
    the class of all sets
  Description
    Text
      Elements of sets may be any immutable object, such as integers, ring
      elements and lists of such. Ideals may also be elements of sets.
    Example
      A = set {1,2};
      R = QQ[a..d];
      B = set{a^2-b*c,b*d}
    Text
      Set operations, such as @TO2((isMember, Thing, Set), "membership")@,
      @TO2((symbol+, Set, Set), "union")@,
      @TO2((symbol*, Set, Set), "intersection")@,
      @TO2((symbol-, Set, Set), "difference")@,
      @TO2((symbol**, Set, Set), "Cartesian product")@,
      @TO2((symbol^**, VirtualTally, ZZ), "Cartesian power")@, and
      @TO2((isSubset, Set, Set), "subset")@ are available. For example,
    Example
      toList B
      member(1,A)
      member(-b*c+a^2,B)
      A ** A
      A^**2
      set{1,3,2} - set{1}
      set{4,5} + set{5,6}
      set{4,5} * set{5,6}
      set{1,3,2} === set{1,2,3}
    Text
      @TO2(Ideal, "Ideals")@ in Macaulay2 come equipped with a specific
      sequence of generators, so the following two ideals are not considered
      strictly equal, and thus the set containing them will appear to have two
      elements.
    Example
      I = ideal(a,b); J = ideal(b,a);
      I == J
      I === J
      C = set(ideal(a,b),ideal(b,a))
    Text
      However, if you @TO trim@ the ideals, then the generating sets will be
      the same, and so the set containing them will have one element.
    Example
      C1 = set(trim ideal(a,b),trim ideal(b,a))
    Text
      A set is implemented as a @TO HashTable@, whose keys are the elements of
      the set, and whose values are all 1. In particular, this means that two
      objects are considered the same exactly when they are strictly equal,
      according to @TO symbol===@.
  Subnodes
    set
    elements
    union
    intersect
    (symbol #?, Set, Thing)
    (symbol -, Set, Set)
    (union, Set, Set)
    (intersect, Set, Set)
    (symbol **, Set, Set)
    (sum, Set)
    (product, Set)
    (select, Set, Function)
    (isSubset, Set, Set)
///

doc ///
  Key
    (symbol #?, Set, Thing)
  Headline
    test set membership
  Usage
    x#?e
  Inputs
    x:
    e:
  Outputs
    :Boolean
      whether e is in the set x
  Description
    Text
      This is identical to @TT "member(e,x)"@.
    Example
      x = set{1,2,3}
      x#?2
      member(2,x)
  SeeAlso
    Set
///

doc ///
  Key
    (symbol -, Set, Set)
    (symbol -, Set, VisibleList)
    (symbol -, VisibleList, Set)
  Headline
    set difference
  Usage
    x - y
  Inputs
    x:
      or @ofClass List@
    y:
      or @ofClass List@
  Outputs
    :Set
      or @ofClass List@, consisting of those elements of x not in y
  Description
    Text
      At least one of @TT "x"@, @TT "y"@ must be a set, and the other may be a
      list. If @TT "x"@ is a list, then so is the result.
    Example
      set{a,b,c} - set{a}
      set{a,b,c} - {a}
      {a,b,c} - set{a}
  SeeAlso
    Set
///

doc ///
  Key
    (union, Set, Set)
    (symbol +, Set, Set)
  Headline
    set union
  Usage
    x + y
  Inputs
    x:
    y:
  Outputs
    :Set
      the union of @TT "x"@ and @TT "y"@
  Description
    Example
      set{a,b,c} + set{a,d,f}
      union(set{a,b,c}, set{a,d,f}, set{a,c,e})
    Text
      The function @TT "sum"@ can be used to form the union of a list of sets,
      but this can be slow for long lists.
    Example
      x = apply(3, i -> set apply(3, j -> 10*i+j))
      sum x
  SeeAlso
    Set
///

doc ///
  Key
    (intersect, Set, Set)
    (symbol *, Set, Set)
  Headline
    set intersection
  Usage
    x * y
  Inputs
    x:
    y:
  Outputs
    :
      the intersection of @TT "x"@ and @TT "y"@
  Description
    Example
      set {1,2,3} * set {2,3,4}
    Example
      intersect(set{1,2,3}, set{2,3,4}, set{2,4,6})
  SeeAlso
    Set
///

doc ///
  Key
    (symbol **, Set, Set)
  Headline
    Cartesian product
  Usage
    x ** y
  Inputs
    x:
    y:
  Outputs
    whose elements are the sequences (a,b), where a is an element of x, and b is an element of y.:Set
  Description
    Example
      set {1,2} ** set {a,b,c}
    Text
      Suppose we wish to form the set of all triples with entries either in the
      set A below.
    Example
      A = set{1,2}
      A ** A ** A
    Text
      To make this last a set of triples, @TO splice@ each element together.
      Or, use @TO (symbol^**, VirtualTally, ZZ)@.
    Example
      (A ** A ** A)/splice
      A^**3
  SeeAlso
    Set
///

doc ///
  Key
    (sum, Set)
  Headline
    sum of elements
  Description
    Text
      @TT "sum v"@ yields the sum of the elements in the set @TT "v"@.
    Example
      a = set{1,100,10000}
      sum a
  SeeAlso
    "sum"
///

doc ///
  Key
    (product, Set)
  Headline
    product of elements
  Description
    Text
      @TT "product v"@ yields the product of the elements in the set @TT "v"@.
    Example
      a = set select(1..50, isPrime)
      product a
  SeeAlso
    "product"
///
