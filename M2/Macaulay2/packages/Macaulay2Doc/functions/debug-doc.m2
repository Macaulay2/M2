doc ///
Node
  Key
    debug
  Headline
    assistant for debugging packages, dictionaries, etc
  SeeAlso
    (debug, Package)
  Subnodes
    (debug, LocalDictionary)
    (debug, String)

Node
  Key
    (debug, String)
  Headline
    open the local dictionary of a loaded file
  Usage
    debug filename
  Inputs
    filename:String
  Consequences
    Item
      symbols from the local dictionary @TT "fileDictionaries#filename"@ are added to
      the private dictionary of @TT "User"@, and are therefore made visible to the user
  Description
    Text
      This function allows access to the local symbols of a any loaded file.
    Example
      load "Macaulay2Doc/demos/demo1.m2" -* no-capture-flag *-
      listUserSymbols
      debug "Macaulay2Doc/demos/demo1.m2"
      listUserSymbols
      code f
  SeeAlso
    "fileDictionaries"
    (debug, Package)
    (debug, LocalDictionary)
    (debug, GlobalDictionary)

Node
  Key
    (debug, LocalDictionary)
    (debug, GlobalDictionary)
  Headline
    open a local or global dictionary to the user
  Usage
    debug dict
  Inputs
    dict:{LocalDictionary,GlobalDictionary}
  Consequences
    Item
      if @TT "dict"@ is a local dictionary, its symbols are added to
      the private dictionary of @TT "User"@, otherwise @TT "dict"@ is
      prepended to @TO "dictionaryPath"@, and in both cases the symbols
      are made visible to the user
  SeeAlso
    "dictionaryPath"
    (debug, Package)
    (debug, String)
///
