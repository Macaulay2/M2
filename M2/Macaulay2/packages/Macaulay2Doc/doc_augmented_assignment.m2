doc ///
  Key
    "augmented assignment"
  Description
    Text
      Most binary @TO "operators"@ have an associated
      @wikipedia "augmented assignment"@ operator that modifies the given object
      using the corresponding binary operator.  Essentially, @SAMP "x OP= y"@
      is equivalent to @SAMP "x = x OP y"@.
    Example
      x = 2
      x += 3
      x -= 4
      x *= 5
      x /= 6
    Text
      The following augmented assignment operators are supported.
    Example
      importFrom(Core, "augmentedAssignmentOperators");
      augmentedAssignmentOperators
  SeeAlso
    "installing augmented assignment methods"
///

scan(core "augmentedAssignmentOperators", op -> (
	binop := concatenate drop(toSequence toString op, -1);
	document {
	    Key => op,
	    Headline => "augmented assignment for " | binop,
	    Usage => "x " | toString op | "y",
	    "In most cases, ", SAMP ("x " | toString op | " y"),
	    " is equivalent to ", SAMP("x = x " | binop | " y"),
	    ", unless a method is installed for the class of ",
	    SAMP "x", ".",
	    SeeAlso => {
		binop,
		"augmented assignment",
		"installing augmented assignment methods"}}))

doc ///
  Key
    "installing augmented assignment methods"
    (symbol <<=, Type)
    (symbol >>=, Type)
    (symbol ||=, Type)
    (symbol <==>=, Type)
    (symbol |-=, Type)
    (symbol ==>=, Type)
    (symbol ===>=, Type)
    (symbol |=, Type)
    (symbol ^^=, Type)
    (symbol &=, Type)
    (symbol ..=, Type)
    (symbol ..<=, Type)
    (symbol -=, Type)
    (symbol +=, Type)
    (symbol ++=, Type)
    (symbol **=, Type)
    (symbol *=, Type)
    (symbol \\=, Type)
    (symbol /=, Type)
    (symbol \=, Type)
    (symbol %=, Type)
    (symbol //=, Type)
    (symbol @=, Type)
    (symbol @@=, Type)
    (symbol ^=, Type)
    (symbol _=, Type)
    (symbol ^**=, Type)
  Description
    Text
      In most cases, the default behavior of @TO "augmented assignment"@ gives
      the desired result.  But in some situations, it may be useful to override
      this behavior and install a custom method for a given type.

      Consider the following example.
    Example
      Foo = new SelfInitializingType of MutableList;
      net Foo := x -> net x#0;
      Foo + Foo := (x, y) -> Foo {x#0 + y#0};
      x = Foo {1}
      y = Foo {2}
      x += y
    Text
      Note that an intermediate @SAMP "Foo"@ object was created and then
      assigned to @SAMP "x"@.  Instead, it would be more efficient if @SAMP "x"@
      was modified directly.

      The first two lines below do exactly the same thing; the second
      line is syntactic sugar for the first.
    Example
      installMethod(symbol +=, Foo, (x, y) -> (x#0 += y#0; x));
      Foo += (x, y) -> (x#0 += y#0; x);
      x += y
    Text
      In some cases, it may be useful to fall back on the default behavior
      of the given operator.  When this is desired, the installed method should
      return the @TO Default@ symbol.
    Example
      Bar = new SelfInitializingType of List;
      net Bar := x -> net x#0#0;
      Bar * Bar := (x, y) -> Bar {{x#0#0 * y#0#0}};
      Bar *= (x, y) -> if isMutable x#0 then (
	  print "using custom method";
	  x#0#0 *= y#0#0; x) else Default;
      x = Bar {new MutableList from {3}}
      y = Bar {{4}}
      x *= y
      y *= x
  SeeAlso
    "augmented assignment"
    "installing methods"
///
