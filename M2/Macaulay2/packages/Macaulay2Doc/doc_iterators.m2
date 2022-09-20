doc ///
  Key
    "iterators"
  Headline
    an overview of iterators in Macaulay2
  Description
    Text
      A class in Macaulay2 is @EM "iterable"@ if there is a method
      function installed for @TO iterator@ that takes as input instances
      of that class and returns instances of an @EM "iterator"@ class, which
      has method functions installed for @TO iterator@ (typically @TO identity@)
      and @TO next@.  Note that iterators are themselves iterable.
    Example
      PrimeIterator = new SelfInitializingType of MutableHashTable;
      iterator PrimeIterator := identity;
      next PrimeIterator := p -> p#"val" = nextPrime(p#"val" + 1);
      i = PrimeIterator {"val" => 1};
      next i
      next i
      next i
    Text
      If we wish to iterate over an object more than once, then it should have
      a separate corresponding iterator class.
    Example
      PrimeIterable = new Type of HashTable;
      iterator PrimeIterable := x -> PrimeIterator {"val" => 1};
      P = new PrimeIterable;
      i = iterator P;
      next i
      next i
      j = iterator P;
      next j
      next j
    Text
      If we would like iteration to stop at a certain point, then the @TO next@
      method should return the symbol @TO StopIteration@.
    Example
      BoundedPrimeIterator = new SelfInitializingType of MutableHashTable;
      iterator BoundedPrimeIterator := identity;
      next BoundedPrimeIterator := i -> (
	  q := nextPrime(i#"val" + 1);
	  if q > i#"max" then StopIteration else i#"val" = q);
      BoundedPrimeIterable = new SelfInitializingType of HashTable;
      iterator BoundedPrimeIterable := x -> BoundedPrimeIterator {
	  "val" => 1, "max" => x#"max"};
      P = BoundedPrimeIterable {"max" => 6};
      i = iterator P;
      next i
      next i
      next i
      next i
    Text
      When an iterator eventually returns @TO StopIteration@, then it (or the
      iterable to which it corresponds) may be used with @TO toList@, @TO scan@,
      and @TO "for"@.
    Example
      toList P
      scan(P, print)
      for x in P list x + 2
///

doc ///
  Key
    iterator
  Headline
    get an iterator for an iterable
  Usage
    iterator x
  Inputs
    x:Thing -- an instance of an iterable class
  Outputs
    :Thing -- an iterator for @TT "x"@
  SeeAlso
    "iterators"
///

doc ///
  Key
    next
  Headline
    get the next object from an iterator
  Usage
    next x
  Inputs
    x:Thing -- an iterator
  Outputs
    :Thing -- the next object in an iterable
  SeeAlso
    "iterators"
///

doc ///
  Key
    StopIteration
  Headline
    stop iteration
  Description
    Text
      This symbol is returned by @TO next@ to signal that iteration is complete.
  SeeAlso
    "iterators"
///
