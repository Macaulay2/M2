doc ///
  Key
    formation
    (formation, Module)
  Headline
    recover the methods used to make a module
  Usage
    formation M
  Inputs
    M:Module
      a module
  Outputs
    :Expression
      @ofClass Expression@ whose value is the module itself
  Description
    Text
      If the module was created as a direct sum, tensor product, of Hom-module,
      then the expression will reflect that. In each case, the result is a
      function application, and the sequence of arguments is easily obtained.
    Example
      M = ZZ^2 ++ ZZ^3
      t = formation M
      peek t
      t#1
      value t
      M = directSum(ZZ^2, ZZ^3, ZZ^4)
      t = formation M
      t#1
      M = ZZ^2 ** ZZ^3
      t = formation M
      t#1
    Text
      If the module was not obtained that way, then @TO "null"@ is returned.
    Example
      formation ZZ^6
    Text
      The same remarks apply to certain other types of objects, such as chain
      complexes.
    Example
      R = QQ[x,y];
      C = res coker vars R;
      D = C ++ C
      formation D
  SeeAlso
    directSum
    (symbol ++, Module, Module)
    (symbol **, Module, Module)
    (Hom, Module, Module)
    FunctionApplication
    Expression
///
