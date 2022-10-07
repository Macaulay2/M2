doc ///
  Key
    Iterator
  Headline
    class for iterators
  Description
    Text
      This is a class designed to simplify writing @TO iterator@ methods.
      Each instance is a nullary @TO FunctionClosure@ that serves as the
      @TO next@ method for the iterator.
    Example
      iter = iterator {1, 2, 3}
      code iter
    Text
      Each call of @TT "next iter"@ is equivalent to @TT "iter()"@.
    Example
      next iter
      iter()
      next iter
      iter()
    Text
      For example, let us create a class that we may use to iterate over the
      Fibonacci numbers.
    Example
      FibonacciNumbers = new Type of HashTable;
      iterator FibonacciNumbers := fib -> Iterator(
	  a := 0;
	  b := 1;
	  () -> (
	      r := a;
	      (a, b) = (b, a + b);
	      r));
      fibonacci = new FibonacciNumbers;
      for i in fibonacci list if i > 100 then break else i
  SeeAlso
    iterator
    next
    StopIteration
///

doc ///
  Key
    iterator
    (iterator, Iterator)
    (iterator, Sequence)
    (iterator, String)
    (iterator, VisibleList)
  Headline
    get an iterator
  Usage
    iterator x
  Inputs
    x:Thing
  Outputs
    :Thing -- likely an @TO Iterator@
  Description
    Text
      An iterator is an object that is used to traverse through @TT "x"@.
      Usually, but not necessarily, this will be an instance of the
      @TO Iterator@ class.
    Example
      iter = iterator {1, 2, 3}
    Text
      The class of an iterator should have a @TO next@ method installed that
      gets the next element of @TT "x"@.
    Example
      next iter
      next iter
      next iter
    Text
      If @TT "x"@ contains only a finite number of elements, then @TO next@
      should return the symbol @TO StopIteration@ after exhausting them all.
    Example
      next iter
    Text
      Instances of classes with this method installed can be used like lists
      in @TO "for"@ loops and @TO scan@.
    Example
      lookup(iterator, String)
      for i in "foo" list i
      scan("foo", print)
    Text
      They can also be passed to @TO apply@ and @TO select@.  In each case, an
      @TO Iterator@ object will be returned that is an iterator for itself.
    Example
      apply("foo", toUpper)
      for i in oo list i
      select("foo", i -> i == "o")
      for i in oo list i
  SeeAlso
    Iterator
    next
    StopIteration
///

doc ///
  Key
    next
    (next, Iterator)
  Headline
    get the next object from an iterator
  Usage
    next x
  Inputs
    x:Thing -- an iterator
  Outputs
    :Thing -- the next object from @TT "x"@
  Description
    Text
      This gets the next object from an iterator, i.e., something returned by
      @TO iterator@.
    Example
      iter = iterator {1, 2, 3}
      next iter
      next iter
      next iter
    Text
      If the iterator is exhausted, then the symbol @TO StopIteration@ is
      returned.
    Example
      next iter
  SeeAlso
    Iterator
    iterator
    StopIteration
///

doc ///
  Key
    StopIteration
  Headline
    stop iteration
  Description
    Text
      This symbol is returned by @TO next@ to signal that iteration is complete.
    Example
      iter = iterator {1, 2, 3}
      next iter
      next iter
      next iter
      next iter
  SeeAlso
    Iterator
    iterator
    next
///
