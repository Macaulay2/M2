--- status: Rewritten September 2020
--- author(s): Mahrud
--- notes: functions below are all defined in packages.m2

doc ///
  Key
     debug
    (debug, Package)
  Headline
    open the private dictionary of a package
  Usage
    debug pkg
  Inputs
    pkg:Package
  Consequences
    Item
      the private dictionary of the package @TT "pkg"@ is added to @TO "dictionaryPath"@
      so its non-exported symbols are visible to the user
  Description
    Text
       For example, the private dictionary for Macaulay2 may be opened using
    Example
      debug Core
    Text
      This allows access to the low level ("raw") routines implemented by the Macaulay2 engine,
      although this is mainly useful for debugging Macaulay2 itself.
    Example
      R = QQ[a..d];
      raw R
  SeeAlso
    export
    "dictionaryPath"
///

-- TODO: (debug, ZZ)
