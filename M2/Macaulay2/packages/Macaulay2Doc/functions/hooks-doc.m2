doc ///
Node
  Key
    "using hooks"
  Description
    Text
      Hooks in Macaulay2 are a type of @wikipedia "dynamic dispatch"@, that is, a way to
      provide different implementations of methods and events and select which implementation
      to use depending on characteristics of the input arguments.

      The @TO addHook@ method allows the user to attach multiple hooks, or strategies,
      for computing a method key such as @TT "(intersect, Ideal, Ideal)"@, or a symbol.

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
      The method @TO runHooks@ runs all the hooks until one of them returns a non-@TO "null"@ value.
      Hooks are run in order starting from the most recently added hook. Because of this,
      each hook should be able to decide quickly whether it is the right implementation for
      the input, and if not should return @TO "null"@.

      Any optional argument passed to @TT "runHooks"@ that matches a key in the @TO OptionTable@
      of a hook will be passed on to it. Otherwise it will be ignored.
    Example
      foo ZZ := true >> opts -> args -> runHooks((foo, ZZ), args, opts);
      importFrom_Core "debugHooksLevel"
      debugHooksLevel = 1
      assert( foo 1 == 15 )
      assert( foo(2, b => 9) == 10 )
      assert( foo 3 == 24 )
    Text
      The function @TO hooks@ lists all hooks attached to a method key or symbol, in the
      order in which they were added, that is, the {\it opposite} order in which they are run.
    Example
      hooks(foo, ZZ)
    Text
      Hooks are automatically assigned an integer which can be used as the value
      of the @TT "Strategy"@ option to specify only one strategy to run.
    Example
      assert( foo(3, Strategy => 2) == 24 )
      assert( foo(2, Strategy => "G") == 6 )
    Text
      If the code for a hook was read from a file, then it can be retrieved with the @TO code@ function.
    Example
      hooks(quotient, Ideal, Ideal)
      code 1
    Text
      Internally, the information about hooks are stored either in types or under @TT "GlobalHookStore"@.
    Example
      importFrom_Core { "getHookStore", "Hooks", "HookPriority", "HookAlgorithms" }
      Ideal.Hooks === getHookStore((quotient, Ideal, Ideal), false)
      peek Ideal.Hooks
      peek Ideal.Hooks#(quotient, Ideal, Ideal)
      peek Ideal.Hooks#(quotient, Ideal, Ideal).HookPriority
      peek Ideal.Hooks#(quotient, Ideal, Ideal).HookAlgorithms
      peek GlobalHookStore
  Subnodes
    hooks
    addHook
    runHooks
  SeeAlso
    code
    method
    methods
    "making a new method function"

Node
  Key
     addHook
    (addHook, Symbol,                  Function)
    (addHook, Sequence,                Function)
    (addHook, MutableHashTable, Thing, Function)
    [addHook, Strategy]
    "GlobalHookStore"
  Headline
    add a hook function to an object for later processing
  Usage
    addHook(key, hook)
    addHook(store, key, hook)
  Inputs
    key:{Sequence,Symbol}
    hook:Function
    store:MutableHashTable
    Strategy=>Thing
      specifies the name for the hook
  Consequences
    Item
      the function @TT "hook"@ is added to the (possibly absent) hash table of hooks, which is
      either stored in the mutable hash table @TT "store"@ or under the @TO youngest@ @TO Type@
      @TT "T"@ listed in the @TO method@ key @TT "key"@. In the latter case, the hash table is
      either stored in @TT "T.Hooks#key"@ if @TT "T"@ is mutable, or in @TT "T.cache.Hooks#key"@
      otherwise. If no appropriate object is found, or if @TT "key"@ is a @TO Symbol@, then the
      hook is stored under the hash table @TT "GlobalHookStore"@
  Description
    Text
      For an explanation and examples of hooks see @TO "using hooks"@.
  SourceCode
    (addHook, MutableHashTable, Thing, Function)
  SeeAlso
    hooks
    runHooks

Node
  Key
     runHooks
    (runHooks, Symbol,                  Thing)
    (runHooks, Sequence,                Thing)
    (runHooks, MutableHashTable, Thing, Thing)
  Headline
    run the hook functions stored in an object
  Usage
    runHooks(key, args)
    runHooks(store, key, args)
  Inputs
    key:{Sequence,Symbol}
    args:Thing
    store:MutableHashTable
  Outputs
    :
      if one of the hook functions returns a non-@TO "null"@ value,
      that value will be returned. Otherwise @TO "null"@ will be returned.
  Description
    Text
      Each function @TT "hook"@ in the hash table of hooks associated to @TT "key"@ is called with
      @TT "args"@ as its argument or sequence of arguments. The optional argument @TT "Strategy"@
      can be used to specify which hook should be run. See @TO addHook@ for where the hooks are
      stored.

      Any other optional argument for @TT "runHooks"@ that matches any key in @TT "options hook"@
      will be passed on to @TT "hook"@. All other options are ignored.

      For further explanation and examples of hooks see @TO "using hooks"@.
  SourceCode
    (runHooks, MutableHashTable, Thing, Function)
  SeeAlso
    hooks
    addHook

Node
  Key
     hooks
    (hooks, ZZ)
    (hooks, List)
    (hooks, Thing)
    (hooks, Symbol)
    (hooks, Sequence)
    (hooks, HashTable)
    [hooks, Strategy]
  Headline
    list hooks attached to a key
  Usage
    hooks key
    hooks store
    hooks(store, key)
  Inputs
    key:{Sequence,Symbol}
    store:HashTable
      the hash table where the hooks are stored
    Strategy=>Thing
      only list hooks with the given strategy
  Outputs
    :NumberedVerticalList
      of those hooks associated with @TT "key"@ or stored in @TT "store"@
  Description
    Example
      hooks(intersect, Ideal, Ideal)
      code 0
      hooks(quotient, Strategy => Iterate)
  SeeAlso
     addHook
     runHooks
///
