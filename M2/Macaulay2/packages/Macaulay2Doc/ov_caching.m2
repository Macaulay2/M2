undocumented {
    ((symbol SPACE, symbol =), Function, Thing),
}

doc ///
Node
  Key
    "caching computation results"
  Description
    Text
      Here is a simple example of caching a computation in a @TO CacheTable@,
      using the augmented null coalescing operator @TO symbol ??=@.
    Example
      pdim' = M -> M.cache.pdim' ??= ( printerr "computing pdim'"; pdim M );
      R = QQ[x,y,z];
      M = coker vars R;
      elapsedTime pdim' M
      elapsedTime pdim' M
      peek M.cache
    Text
      In the example above, the function is only executed if @TT "M.cache.pdim'"@ is not already defined,
      and once it is executed the result is stored there for future reference.
    Text
      In more complicated situations, one may need to create a new table under @TO cache@ first.
    Example
      code(hh, Sequence, ProjectiveVariety)
  SeeAlso
    CacheTable
  Subnodes
    cache
    memoize
    status

Node
  Key
    cache
  Headline
    a key under which to store cache tables
  Description
    Example
      F = ZZ^3
      peek F
      F.cache#Foo = Bar
      peek F
      peek F.cache
      F === ZZ^3
  SeeAlso
    CacheTable

Node
  Key
    CacheTable
  Headline
    hash tables for caching
  Description
    Text
      A cache table is a type of mutable hash table designed for caching computed values that
      may be recalled later. Cache tables are designed so their contents will not participate
      in any comparisons by the strict comparison operator @TT "==="@.
    Example
      A = { 1, 2, new CacheTable from { 1 => 10,  2 => 20 }}
      B = { 1, 2, new CacheTable from { 1 => 100, 2 => 200 }}
      A === B
    Text
      To that end, any two cache tables with the same class and parent are considered equal
      to each other and have hash code equal to 0.
    Example
      hash A#2
      hash B#2
///
