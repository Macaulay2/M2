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

      Use @TO importFrom@ to import private symbols of a package.
  SeeAlso
    exportFrom
    exportMutable
    importFrom

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

      Use @TO importFrom@ to import private symbols of a package.
  SeeAlso
    export

Node
  Key
     exportFrom
    (exportFrom, Package, List)
  Headline
    export symbols from a package's private dictionary
  Usage
    exportFrom(pkg, {"symbol1", "symbol2"})
  Inputs
    pkg:Package
      the package containing the symbols
    :List
      of strings, corresponding to the symbols to export
  Consequences
    Item
      package symbols provided in the list will be added to the @TO2 {Dictionary, "dictionary"}@
      of the @TO2 {"currentPackage", "current package"}@, and will be exported
  Description
    Text
      This function can be used to export specific symbols from the private dictionary of another package.
    Example
      exportFrom_Core {"HTML"}
  SeeAlso
    export
    importFrom
    Dictionary

Node
  Key
     importFrom
    (importFrom, Package, List)
    (importFrom, String,  List)
  Headline
    import symbols to the current private dictionary
  Usage
    importFrom(pkg, {"symbol1", "symbol2"})
  Inputs
    pkg:{Package,String}
      the package containing the private symbols
    :List
      of strings, corresponding to the private @TO2 {Symbol, "symbols"}@ to export from the package
  Outputs
    :List
      of @TO2 {Symbol, "symbols"}@
  Consequences
    Item
      package symbols provided in the list will be added to the private @TO2 {Dictionary, "dictionary"}@
      of the @TO2 {"currentPackage", "current package"}@, but will not be exported
  Description
    Text
      This function can be used to import specific symbols from the private dictionary of another package
      into the private dictionary of the @TO2 {"currentPackage", "current package"}@.
    Example
      importFrom_Core {"raw"}
      raw random(ZZ^2, ZZ^2)
    Text
      To import all private symbols of a package, use @TO (debug, Package)@ instead.
      The symbols imported with this function can then be exported using @TO export@ or @TO exportMutable@.
  SeeAlso
    (debug, Package)
    exportFrom
    Dictionary
///
