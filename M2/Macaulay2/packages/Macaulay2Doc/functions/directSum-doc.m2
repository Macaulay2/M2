--- status: moved December 2020
--- includes ++, directSum, isDirectSum, components, indices, IndexComponents

undocumented {
     (symbol ++,RingElement,ZZ),
     (symbol ++,ZZ,RingElement),
     (symbol ++,Matrix,ZZ),
     (symbol ++,ZZ,Matrix)
     }

doc ///
  Key
    isDirectSum
    (isDirectSum, Module)
  Headline
    whether something is a direct sum
  Description
    Text
      Works for modules, chain complexes, etc. The components of the sum can be
      recovered with @TO "components"@.
    Example
      isDirectSum ZZ^6
      F = ZZ^2 ++ ZZ^3
      isDirectSum F
      components F
///

doc ///
  Key
    (symbol ++,Module,Module)
  Headline
    direct sum of modules
  Description
    Text
      @TT "M++N"@ -- computes the direct sum of two modules (or coherent
      sheaves).
    Example
      R = ZZ/101[a..c];
      image vars R ++ kernel vars R
    Text
      The projection and inclusion maps for direct sums can be obtained with
      the following methods.

      @UL {TO (symbol ^, Module, Array), TO (symbol _, Module, Array)}@

      The components can be recovered later with @TO "components"@ or with
      @TO "formation"@.
  SeeAlso
    directSum
///

doc ///
  Key
    (symbol ++,Matrix,Matrix)
    (symbol ++,RingElement,Matrix)
    (symbol ++,Matrix,RingElement)
    (symbol ++,RingElement,RingElement)
  Headline
    direct sum of maps
  Description
    Text
      @TT "f++g"@ -- computes the direct sum of two maps between modules.

      If an argument is a ring element or integer, it is promoted to a one by
      one matrix.
    Example
      R = ZZ/101[a..c];
      vars R ++ transpose vars R
      oo^[1]
      a++b++c
    Text
      Selecting rows or columns of blocks:

      @UL {TO (symbol ^, Matrix, Array), TO (symbol _, Matrix, Array)}@
  SeeAlso
    directSum
    (symbol |, Matrix, Matrix)
    (symbol ||, Matrix, Matrix)
///

doc ///
  Key
    directSum
    (directSum, Module)
    (directSum, Matrix)
    (directSum, List)
    (directSum, Option)
    (directSum, Sequence)
    (symbol++, Option, Option)
  Headline
    direct sum of modules or maps
  Description
    Text
      @TT "directSum(M,N,...)"@ -- forms the direct sum of matrices or modules.

      The components can be recovered later with @TO "components"@ or with
      @TO "formation"@.

      Projection and inclusion maps for direct sums:

      @UL {
          TO (symbol ^, Module, Array),
          TO (symbol _, Module, Array),
          TO (symbol ^, Matrix, List),
          TO (symbol _, Matrix, List)
          }@

      It sometimes happens that the user has indices for the components of a
      direct sum preferable to the usual consecutive small integers. In this
      case the preferred indices can be specified with code like
      @TT "directSum(a=>M,b=>N,...)"@, as in the following example.
    Example
      F = directSum(a=>ZZ^1, b=>ZZ^2, c=>ZZ^3)
      F_[b]
      F^[c]
    Text
      Similar syntax works with @TO "++"@.
    Example
      F = (a => ZZ^1) ++ (b => ZZ^2)
      F_[b]
  SeeAlso
    "++"
    "indices"
    formation
  Subnodes
    isDirectSum
    components
///

doc ///
  Key
    components
    (components, Module)
    (components, Matrix)
  Headline
    list the components of a direct sum
  Description
    Text
      @TT "components x"@ -- produces a list of the components of an element of
      a free module.@BR{}@@TT "components M"@ -- the list of components for a
      module @TT "M"@ which was formed as a direct sum, or @TT "{M}"@ if
      @TT "M"@ was not formed as a direct sum. Works also for matrices, chain
      complexes, etc.
  SeeAlso
    "vector"
    "directSum"
    "++"
  Subnodes
    indexComponents
///

-- TODO:
-- document { 
--      Key => (indices,HashTable),
--      Headline => "preferred indices of a direct sum",
--      Usage => "",
--      Inputs => {
-- 	  },
--      Outputs => {
-- 	  },
--      Consequences => {
-- 	  },     
--      "description",
--      EXAMPLE {
-- 	  },
--      Caveat => {},
--      SeeAlso => {"directSum", "components", "indexComponents"}
--      }

doc ///
  Key
    indexComponents
  Headline
    specify keys for components of a direct sum
  Description
    Text
      @TT "indexComponents"@ -- a symbol used as a key in a direct sum under
      which to store a hash table in which to register preferred keys used to
      index the components of the direct sum.
  SeeAlso
    "directSum"
    "components"
    "indices"
///
