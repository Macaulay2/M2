undocumented {
    sheafMap, -- deprecated
    -- TODO: document some of these
    (symbol SPACE, SheafMap, ZZ),
    (symbol ^,   SheafMap, ZZ),
    (symbol ^**, SheafMap, ZZ),
    (symbol ++,  SheafMap, SheafMap),
    (symbol +,   SheafMap, SheafMap),
    (symbol *,   SheafMap, SheafMap),
    (symbol *,   RingElement, SheafMap), (symbol *, ZZ, SheafMap),
    (symbol -,   SheafMap, SheafMap), (symbol -, SheafMap),
    (symbol **,  SheafMap, SheafMap),
    (symbol **,  SheafMap, SheafOfRings),  (symbol **, SheafOfRings, SheafMap),
    (symbol **,  SheafMap, CoherentSheaf), (symbol **, CoherentSheaf, SheafMap),
    (symbol |,   SheafMap, SheafMap),
    (symbol ||,  SheafMap, SheafMap),
    (symbol ==,  SheafMap, SheafMap),
    (symbol ==,  SheafMap, ZZ), (symbol ==, ZZ, SheafMap),
    }

-----------------------------------------------------------------------------
-- Types and basic constructors and methods that return a coherent sheaf map
-----------------------------------------------------------------------------

doc ///
Node
  Key
    SheafMap
  Headline
    the class of morphisms of coherent sheaves
  Subnodes
    (map, CoherentSheaf, CoherentSheaf, Matrix)

Node
  Key
    (map, CoherentSheaf, CoherentSheaf, Matrix)
    (map, CoherentSheaf, CoherentSheaf, Matrix, InfiniteNumber)
    (map, CoherentSheaf, CoherentSheaf, Matrix, ZZ)
    (map, CoherentSheaf, Nothing,       Matrix)
    (map, Nothing,       CoherentSheaf, Matrix)
    (sheaf, Matrix)
    (sheaf, Matrix, ZZ)
    (sheaf, Variety, Matrix)
    (sheaf, Variety, Matrix, ZZ)
  Headline
    the constructor of morphisms of coherent sheaves

Node
  Key
    (random, CoherentSheaf, CoherentSheaf)
  Headline
    generate a random map of coherent sheaves
  Usage
    random(F, G)
  Inputs
    F:CoherentSheaf
    G:CoherentSheaf
  Outputs
    :SheafMap
--  Description
--    Text
--    Example
  SeeAlso
    setRandomSeed
    (random, Module, Module)
    (map, CoherentSheaf, CoherentSheaf, Matrix)

-----------------------------------------------------------------------------
-- Basic methods for sheaves
-----------------------------------------------------------------------------

Node
  Key
    (matrix, SheafMap)
  Headline
    the morphism of modules representing a morphisms of coherent sheaves
  Usage
    matrix phi
    phi.map
  Inputs
    phi:SheafMap
  Outputs
    :Matrix
  SeeAlso
    (truncate, List, Module)
    (truncate, Nothing, List, Matrix)

Node
  Key
    (isWellDefined, SheafMap)
  Headline
    whether a morphism of coherent sheaves is well-defined
  Usage
    isWellDefined phi
  Inputs
    phi:SheafMap
  Outputs
    :Boolean
--  Description
--    Text
--    Example
  SeeAlso
    (isWellDefined, CoherentSheaf)
    (isWellDefined, Variety)
    (isWellDefined, Matrix)
///

-----------------------------------------------------------------------------
-- Arithmetic operations
-----------------------------------------------------------------------------

doc ///
Node
  Key
    (quotient,  SheafMap, SheafMap)
    (quotient', SheafMap, SheafMap)
    (symbol //, SheafMap, SheafMap)
    (symbol \\, SheafMap, SheafMap)
  Headline
    factoring a morphism of coherent sheaves through another
///

///
Node
  Key
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
  SeeAlso
///
