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
	    Usage => "x " | toString op | " y",
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
    Example
      installMethod(symbol +=, Foo, (x, y) -> (x#0 += y#0; x));
      x += y
    Text
      In some cases, it may be useful to fall back on the default behavior
      of the given operator.  When this is desired, the installed method should
      return the @TO Default@ symbol.
    Example
      Bar = new SelfInitializingType of List;
      net Bar := x -> net x#0#0;
      Bar * Bar := (x, y) -> Bar {{x#0#0 * y#0#0}};
      installMethod(symbol *=, Bar, (x, y) -> if isMutable x#0 then (
	      print "using custom method";
	      x#0#0 *= y#0#0; x) else Default)
      x = Bar {new MutableList from {3}}
      y = Bar {{4}}
      x *= y
      y *= x
  SeeAlso
    "augmented assignment"
    "installing methods"
///

doc ///
  Key
    symbol ??
  Headline
    null coalescing operator
  Usage
    x ?? y
  Inputs
    x:Thing
    y:Thing
  Outputs
    :Thing -- either @VAR "x"@ or @VAR "y"@
  Description
    Text
      If @VAR "x"@ is null or a non-interrupting error, then @VAR "y"@
      is returned.  When it is an error, the error message is suppressed like
      it is with @TO symbol try@.
    Example
      null ?? 2
      1/0 ?? 3
    Text
      In all other cases, @VAR "x"@ is returned.
    Example
      5 ?? 6
    Text
      Note that @VAR "y"@ is lazily evaluated, that is, it is only
      evaluated if it is necessary.

      The null coalescing operator can be combined with
      @TO "augmented assignment"@ as a shortcut for
      @M2CODE "if x === null then x = y"@ and
      @M2CODE "if not x#?i then x#i = y"@.  Note that it behaves
      slightly differently than other augmented assignment operators,
      as @CODE "x ??= y"@ is treated like @CODE "x ?? (x = y)"@ rather
      than @CODE "x = x ?? y"@.
    Example
      x = null
      x ??= 2
      x ??= 3
      x = new MutableList
      x#0 ??= 4
      peek x
      x#0 ??= 5
      peek x
    Text
      It is also possible to install a method for a particular type to
      determine whether an instance @VAR "x"@ of that type should be
      considered "null" or not.  Such a method should return either
      @VAR "x"@ or @TO null@
    Example
      X = new Type of BasicList;
      ?? X := x -> if #x > 0 then x;
      x = new X from {};
      y = new X from {5};
      x ?? y
      y ?? x
    Text
      It is also possible to use @VAR "??"@ as a prefix operator to call
      this method.
    Example
      ?? 2
      ?? x
      ??(1/0)
  Caveat
    Although this operator is "flexible" in the sense that it possible to
    install a method to determine the behavior on the left-hand side, it
    is not truly flexible like most other binary operators.  In particular,
    since it is only lazily evaluated, it is not possible to install a method
    that uses the value of the right-hand side.
///
