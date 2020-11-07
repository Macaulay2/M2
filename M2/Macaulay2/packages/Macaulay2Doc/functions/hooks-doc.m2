-- TODO: replace the old API with this one
undocumented {
    (addHook,    Sequence, Function),
    (removeHook, Sequence, Function),
    (runHooks,   Sequence, Thing),
    }

doc ///
Node
  Key
    "using hooks"
  Description
    Text
      Hooks are a way to provide different implementations of functions and methods.
      The user can attach multiple hooks to a function via @TO "addHook"@.

      Hooks can be functions or methods, and they can accept optional arguments.
    Example
      f = {a=>3, c=>12} >> opts -> val -> if val == 1 then opts.a + opts.c
      g = method(Options => {b => 5})
      g ZZ := opts -> val -> if val == 2 then opts.b + 1
      h = val -> if val == 3 then 24
      addHook(ZZ, symbol foo, f)
      addHook(ZZ, symbol foo, g)
      addHook(ZZ, symbol foo, h)
    Text
      The command @TO "hooks"@ lists all hooks attached to a symbol.
    Example
      hooks(ZZ, symbol foo)
    Text
      The method @TO "runHooks"@ runs all the hooks until one of them returns a non-null value.
      Hooks are run in order starting from the most recently added hook. Because of this,
      hooks should be able to decide quickly if they should be used or not.

      Any optional argument passed to @TT "runHooks"@ that matches a key in the @TO "OptionTable"@
      of a hook will be passed on to it. Otherwise it will be ignored.
    Example
      foo = true >> opts -> args -> runHooks(ZZ, symbol foo, args, opts)
      assert( foo 1 == 15 )
      assert( foo(2, b => 9) == 10 )
      assert( foo 3 == 24 )
  Subnodes
    addHook
    removeHook
    runHooks
    hooks

Node
  Key
     addHook
    (addHook, HashTable,        Thing, Function)
    (addHook, MutableHashTable, Thing, Function)
    (addHook, Symbol,                  Function)
  Headline
    add a hook function to an object for later processing
  Synopsis
    Usage
      addHook(obj, key, hook)
    Inputs
      obj:HashTable
      key:Thing
      hook:Function
    Consequences
      Item
        the function @TT "hook"@ is added to the beginning of the list (possibly absent) of hooks
        stored in @TT "obj#key"@ if @TT "obj"@ is mutable, or in @TT "obj.cache#key"@ if not
  Synopsis
    Usage
     addHook(sym, hook)
    Inputs
      sym:Symbol
      hook:Function
    Consequences
      Item
        the function @TT "hook"@ is added to the beginning of the list (possibly absent) of hooks
        stored as the value of @TT "sym"@
  SourceCode
    (addHook, HashTable,        Thing, Function)
    (addHook, MutableHashTable, Thing, Function)
    (addHook, Symbol,                  Function)
  SeeAlso
    hooks
    runHooks
    removeHook

Node
  Key
     removeHook
    (removeHook, HashTable,        Thing, Function)
    (removeHook, MutableHashTable, Thing, Function)
    (removeHook, Symbol,                  Function)
  Headline
    remove a hook function from an object
  Synopsis
    Usage
      removeHook(obj, key, hook)
    Inputs
      obj:HashTable
      key:Thing
      hook:Function
    Consequences
      Item
        the function @TT "hook"@ is removed from the list of hooks stored in @TT "obj#key"@ or @TT "obj.cache#key"@
  Synopsis
    Usage
      removeHook(sym, hook)
    Inputs
      sym:Symbol
      hook:Function
    Consequences
      Item
        the function @TT "hook"@ is removed from the list of hooks stored in the value of @TT "sym"@
  SourceCode
    (removeHook, HashTable,        Thing, Function)
    (removeHook, MutableHashTable, Thing, Function)
    (removeHook, Symbol,                  Function)
  SeeAlso
    hooks
    addHook
    runHooks

Node
  Key
     runHooks
    (runHooks, HashTable,        Thing, Thing)
    (runHooks, MutableHashTable, Thing, Thing)
    (runHooks, Symbol,                  Thing)
  Headline
    run the hook functions stored in an object
  Synopsis
    Usage
      runHooks(obj, key, arg)
    Inputs
      obj:HashTable
      key:Thing
      arg:Thing
    Outputs
      :
        if one of the hook functions returns a non-@TO "null"@ value,
        that value will be returned. Otherwise @TO "null"@ will be returned.
    Description
      Text
        Each function @TT "hook"@ in the list of hooks stored in @TT "obj#key"@ or @TT "obj.cache#key"@ is
        called with @TT "arg"@ as its argument or sequence of arguments. Any optional argument for @TT "runHooks"@
        that matches any key in @TT "options hook"@ will be passed on to @TT "hook"@. Otherwise it will be ignored.
  Synopsis
    Usage
      runHooks(sym, arg)
    Inputs
      sym:Symbol
      arg:Thing
    Outputs
      :
        if one of the hook functions returns a non-@TO "null"@ value, that value will be returned.
        Otherwise @TO "null"@ will be returned.
    Description
      Text
        Each function @TT "hook"@ in the list of hooks stored in the value of @TT "sym"@ is
        called with @TT "arg"@ as its argument or sequence of arguments. Any optional argument for
        @TT "runHooks"@ that matches any key in @TT "options hook"@ will be passed on to @TT "hook"@.
        Otherwise it will be ignored.
  SeeAlso
    hooks
    addHook
    removeHook

Node
  Key
     hooks
    (hooks, Sequence)
    (hooks, Symbol)
  Headline
    list hooks
  Synopsis
    Usage
      methods(X, f)
    Inputs
      x:MutableHashTable
    Outputs
      :VerticalList
        of those hooks associated with @TT "X#f"@
    Description
      Example
        instance(Ideal, MutableHashTable)
	addHook(Ideal, symbol foo, I -> gens I);
	addHook(Ideal, symbol foo, I -> if dim I == 0 then vars ring I);
	Ideal#?(symbol foo)
	hooks(Ideal, symbol foo)
  Synopsis
    Usage
      methods(X, f)
    Inputs
      x:HashTable
    Outputs
      :VerticalList
        of those hooks associated with @TT "X.cache#f"@
    Description
      Text
        ht = new HashTable from {cache => new MutableHashTable};
	addHook(ht, symbol foo, i -> i + 1);
	hooks(ht, symbol foo)
  Synopsis
    Usage
      hooks(sym)
    Inputs
      sym:Symbol
    Outputs
      :VerticalList
        of those hooks stored in the value of @TT "sym"@
    Description
      Example
        addHook(symbol bar, i-> i + 1)
	hooks(symbol bar)
  SeeAlso
     addHook
     runHooks
     removeHook
///
