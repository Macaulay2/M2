--- status: Rewritten August 2020
--- author(s): Mahrud
--- notes: functions below are all defined in packages.m2

doc ///
Node
  Key
     export
    (export, List)
    (export, String)
  Headline
    export symbols from a package
  Usage
    export {"symbol1", "symbol2", ...}
  Inputs
    :List
      containing strings or options with string keys and values belonging to the package
  Outputs
    :List
      the list of exported symbols
  Consequences
    Item
      the symbols whose names are in the list as strings, which should refer to functions or other symbols
      defined in the package, are made available to the user of the package, and are marked non-mutable.
      The strings are converted to symbols with those names in the dictionary of the package.
      An option of the form @TT "\"name\" => \"symbol\""@ creates a symbol with the name @TT "name"@
      that is a synonym of the symbol @TT "symbol"@.
  Description
    Text
      A package can contain the code for many functions, only some of which should be made visible to
      the user. The function @TT "export"@ allows one to specify which symbols are to be made visible.
      For an example see @TO "an example of a package"@.

      No single-letter symbol should be exported, since such symbols are reserved as variables for the user.

      Use @TO exportMutable@ to export symbols whose values the user is permitted to modify.

      Use @TO (debug, Package)@ to import private symbols of a package.
  SeeAlso
    exportMutable
    (debug, Package)

Node
  Key
     exportMutable
    (exportMutable, List)
    (exportMutable, String)
  Headline
    export mutable symbols from a package
  Usage
    exportMutable {"symbol1", "symbol2", ...}
  Inputs
    :List
      containing strings interpreted as names of symbols belonging to the package
  Outputs
    :List
      the list of exported symbols
  Consequences
    Item
      the names of symbols in the sequence, which should refer to variables defined in the package, are
      made available to the user of the package, in such a way that their values may be modified by the
      user This function is needed much less frequently than @TO export@.
      For an example, see @TO "an example of a package"@.

      No single-letter symbol should be exported, as such symbols are reserved as variables for the user.

      Use @TO (debug, Package)@ to import private symbols of a package.
  SeeAlso
    export
    (debug, Package)

Node
  Key
     exportFrom
    (exportFrom, Package, List)
  Headline
    export symbols from a package's private dictionary
  Usage
    exportFrom(pkg, {"symbol1", "symbol2"})
  Inputs
    :Package
      the package containing the symbols
    :List
      of strings, corresponding to the symbols to export
  Consequences
    Item
      package symbols provided in the list will be made available for use
  Description
    Text
      This function can be used to export specific symbols from another packages private dictionary.
    Example
      exportFrom(Core, {"HTML"})
///
