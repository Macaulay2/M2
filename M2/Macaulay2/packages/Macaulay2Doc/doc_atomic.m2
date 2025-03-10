doc ///
  Key
    AtomicInt
  Headline
    the class of atomic integers
  Description
    Text
      Atomic integers are a special type of integer for which various
      @EM "atomic"@ operations, which cannot be interrupted, are
      available.  This makes them useful for parallel programming.
  Caveat
    On most systems, atomic integers use 32 bits, i.e., they must lie
    between $-2^{31}$ and $2^{31} - 1$.
  SeeAlso
    "parallel programming with threads and tasks"
  Subnodes
    (NewFromMethod, AtomicInt, ZZ)
    (NewFromMethod, ZZ, AtomicInt)
    store
    exchange
    compareExchange
    (symbol +=, AtomicInt)
    (symbol -=, AtomicInt)
    (symbol &=, AtomicInt)
    (symbol |=, AtomicInt)
    (symbol ^^=, AtomicInt)
///

doc ///
  Key
    (NewFromMethod, AtomicInt, ZZ)
    (NewMethod, AtomicInt)
  Headline
    initialize an atomic integer
  Usage
    new AtomicInt from x
    new AtomicInt
  Inputs
    x:ZZ
  Outputs
    :AtomicInt
  Description
    Text
      Use this to initialize a new atomic integer.
    Example
      new AtomicInt from 5
    Text
      If @VAR "x"@ is not given, then the initial value is 0.
    Example
      new AtomicInt
///

doc ///
  Key
    (NewFromMethod, ZZ, AtomicInt)
  Headline
    load an atomic integer
  Usage
    new ZZ from x
  Inputs
    x:AtomicInt
  Outputs
    :ZZ
  Description
    Text
      This loads the value of the atomic integer @VAR "x"@.  This
      operation occurs atomically and is thread safe.
    Example
      x = new AtomicInt
      x += 5
      new ZZ from x
///

doc ///
  Key
    (symbol +=, AtomicInt)
  Headline
    atomic fetch and add
  Usage
    x += y
  Inputs
    x:AtomicInt
    y:ZZ
  Outputs
    :ZZ
  Description
    Text
      This performs a @wikipedia "fetch-and-add"@ operation.  The atomic integer
      @VAR "x"@ is increased by @VAR "y"@ and its original value is returned.
      This operation occurs atomically and is thread safe.
    Example
      x = new AtomicInt from 12
      x += 10
      x
///

doc ///
  Key
    (symbol -=, AtomicInt)
  Headline
    atomic fetch and subtract
  Usage
    x -= y
  Inputs
    x:AtomicInt
    y:ZZ
  Outputs
    :ZZ
  Description
    Text
      The atomic integer @VAR "x"@ is decreased by @VAR "y"@ and its
      original value is returned.  This operation occurs atomically
      and is thread safe.
    Example
      x = new AtomicInt from 12
      x -= 10
      x
///

doc ///
  Key
    (symbol &=, AtomicInt)
  Headline
    atomic fetch and bitwise and
  Usage
    x &= y
  Inputs
    x:AtomicInt
    y:ZZ
  Outputs
    :ZZ
  Description
    Text
      Bitwise @EM "and"@ is performed between @VAR "x"@ and @VAR "y"@
      and the original value of @VAR "x"@ is returned.  This
      operation occurs atomically and is thread safe.
    Example
      x = new AtomicInt from 12
      x &= 10
      x
///

doc ///
  Key
    (symbol |=, AtomicInt)
  Headline
    atomic fetch and bitwise or
  Usage
    x |= y
  Inputs
    x:AtomicInt
    y:ZZ
  Outputs
    :ZZ
  Description
    Text
      Bitwise @EM "or"@ is performed between @VAR "x"@ and @VAR "y"@
      and the original value of @VAR "x"@ is returned.  This
      operation occurs atomically and is thread safe.
    Example
      x = new AtomicInt from 12
      x |= 10
      x
///

doc ///
  Key
    (symbol ^^=, AtomicInt)
  Headline
    atomic fetch and bitwise xor
  Usage
    x ^^= y
  Inputs
    x:AtomicInt
    y:ZZ
  Outputs
    :ZZ
  Description
    Text
      Bitwise @EM "xor"@ is performed between @VAR "x"@ and @VAR "y"@
      and the original value of @VAR "x"@ is returned.  This
      operation occurs atomically and is thread safe.
    Example
      x = new AtomicInt from 12
      x ^^= 10
      x
///

doc ///
  Key
    store
    (store, AtomicInt, ZZ)
  Headline
    store the value of an atomic integer
  Usage
    store(x, y)
  Inputs
    x:AtomicInt
    y:ZZ
  Description
    Text
      The value of @VAR "y"@ is stored in @VAR "x"@ and @TO null@ is
      returned.  This operation occurs atomically and is thread safe.
    Example
      x = new AtomicInt
      store(x, 5)
      x
///

doc ///
  Key
    exchange
    (exchange, AtomicInt, ZZ)
  Headline
    exchange the value of an atomic integer
  Usage
    exchange(x, y)
  Inputs
    x:AtomicInt
    y:ZZ
  Outputs
    :ZZ
  Description
    Text
      The value of @VAR "y"@ is stored in @VAR "x"@ and the original
      value of @VAR "x"@ is returned.  This operation occurs
      atomically and is thread safe.
    Example
      x = new AtomicInt
      exchange(x, 5)
      x
///

doc ///
  Key
    compareExchange
    (compareExchange, AtomicInt, ZZ, ZZ)
  Headline
    compare and possibly exchange the value of an atomic integer
  Usage
    compareExchange(x, y, z)
  Inputs
    x:AtomicInt
    y:ZZ
    z:ZZ
  Outputs
    :Boolean
  Description
    Text
      The values of @VAR "x"@ and @VAR "y"@ are compared.  If they agree,
      then @TO true@ is returned and the value of @VAR "z"@ is stored in
      @VAR "x"@.  Otherwise, @TO false@ is returned.  This operation occurs
      atomically and is thread safe.
    Example
      x = new AtomicInt
      compareExchange(x, 2, 3)
      x
      compareExchange(x, 0, 3)
      x
///
