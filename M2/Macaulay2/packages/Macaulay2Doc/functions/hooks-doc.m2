undocumented { -- old syntax
    (addHook,    HashTable, Thing, Function),
    (removeHook, HashTable, Thing, Thing),
    (runHooks,   HashTable, Thing, Thing)
    }

doc ///
Node
  Key
    "using hooks"
  Description
    Text
      Hooks are a way to provide different implementations of methods and events.
      The @TO addHook@ method allows the user to attach multiple hooks or strategies
      to a method key such as @TT "(intersect, Ideal, Ideal)"@ or a symbol.

      Hooks can be functions or methods, and they can accept optional arguments.
    Example
      f = {a=>3, c=>12} >> opts -> val -> if val == 1 then opts.a + opts.c;
      g = method(Options => {b => 5});
      g ZZ := opts -> val -> if val == 2 then opts.b + 1;
      h = val -> if val == 3 then 24;
      foo = method(Options => true);
      addHook((foo, ZZ), f)
      addHook((foo, ZZ), g, Strategy => "G")
      addHook((foo, ZZ), h)
    Text
      The method @TO runHooks@ runs all the hooks until one of them returns a non-null value.
      Hooks are run in order starting from the most recently added hook. Because of this,
      hooks should be able to decide quickly if they should be used or not.

      Any optional argument passed to @TT "runHooks"@ that matches a key in the @TO OptionTable@
      of a hook will be passed on to it. Otherwise it will be ignored.
    Example
      foo ZZ := true >> opts -> args -> runHooks((foo, ZZ), args, opts);
      debugLevel = 1
      assert( foo 1 == 15 )
      assert( foo(2, b => 9) == 10 )
      assert( foo 3 == 24 )
    Text
      The function @TO hooks@ lists all hooks attached to a method key or symbol,
      in the {\it opposite} order in which they are run.
    Example
      L = hooks(foo, ZZ)
    Text
      Hooks are automatically assigned an integer which can be used as the value
      of the @TT "Strategy"@ option to specify only one strategy to run.
    Example
      toList strategies(foo, ZZ)
      assert( foo(2, Strategy => "G") == 6 )
      assert( foo(3, Strategy => 2) == 24 )
    Text
      Lastly, hooks can be removed with the @TO removeHook@ function.
    Example
      removeHook((foo, ZZ), "G")
      toList strategies(foo, ZZ)
  Subnodes
    addHook
    runHooks
    strategies
    hooks
    removeHook

Node
  Key
     addHook
    (addHook, Sequence, Function)
    (addHook, Symbol,   Function)
    [addHook, Strategy]
  Headline
    add a hook function to an object for later processing
  Usage
    addHook(key, hook)
  Inputs
    key:{Sequence,Symbol}
    hook:Function
    Strategy=>Thing
      specifies the strategy name for the hook
  Consequences
    Item
      the function @TT "hook"@ is added to the (possibly absent) hash table of hooks, which is
      either stored as the value of @TT "key"@ if it is a symbol, or under the @TO youngest@ type
      listed in @TT "key"@ if it is a method key. In this case, the hash table is either stored in
      @TT "obj.Hooks#key"@ if @TT "obj"@ is mutable, or in @TT "obj.cache.Hooks#key"@ otherwise
  SourceCode
    (addHook, Sequence, Function)
  SeeAlso
    hooks
    runHooks
    removeHook
    strategies

Node
  Key
     removeHook
    (removeHook, Sequence, Thing)
    (removeHook, Symbol,   Thing)
  Headline
    remove a hook function from an object
  Usage
    removeHook(key, name)
  Inputs
    key:{Sequence,Symbol}
    name:Thing
  Consequences
    Item
      the hook whose key in the hash table of hooks is @TT "name"@ is removed
  SourceCode
    (removeHook, Sequence, Thing)
  SeeAlso
    hooks
    addHook
    runHooks
    strategies

Node
  Key
     runHooks
    (runHooks, Sequence, Thing)
  Headline
    run the hook functions stored in an object
  Usage
    runHooks(key, args)
  Inputs
    key:{Sequence,Symbol}
    args:Thing
  Outputs
    :
      if one of the hook functions returns a non-@TO "null"@ value,
      that value will be returned. Otherwise @TO "null"@ will be returned.
  Description
    Text
      Each function @TT "hook"@ in the hash table of hooks associated to @TT "key"@ is called with
      @TT "args"@ as its argument or sequence of arguments. The optional argument @TT "Strategy"@
      can be used to specify which hook should be run. Any other optional argument for @TT "runHooks"@
      that matches any key in @TT "options hook"@ will be passed on to @TT "hook"@. All other options are ignored.
  SourceCode
    (runHooks, Sequence, Function)
  SeeAlso
    hooks
    addHook
    removeHook
    strategies

Node
  Key
     hooks
    (hooks, Sequence)
    (hooks, Symbol)
    [hooks, Strategy]
  Headline
    list hooks attached to a key
  Usage
    hooks key
  Inputs
    key:{Sequence,Symbol}
  Outputs
    :NumberedVerticalList
      of those hooks associated with @TT "key"@
  Description
    Example
      hooks(intersect, Ideal, Ideal)
  SeeAlso
     addHook
     runHooks
     removeHook
     strategies

Node
  Key
     strategies
    (strategies, Sequence)
    (strategies, Symbol)
  Headline
    list strategies attached to a method as hooks
  Usage
    strategies key
  Inputs
    key:{Sequence,Symbol}
  Outputs
    :HashTable
      of hooks associated to @TT "key"@, with strategies as keys and hooks as values
  Description
    Example
      strategies(intersect, Ideal, Ideal)
  SeeAlso
     addHook
     runHooks
     removeHook
     hooks
///
