--------------------------------------------------------------------
-- interface code to legacy types ----------------------------------
--------------------------------------------------------------------
if isPackageLoaded "OldChainComplexes" then
  importFrom_"OldChainComplexes" { "ChainComplex", "ChainComplexMap", "chainComplex" }

if isPackageLoaded "Complexes" then
  importFrom_"Complexes" { "Complex", "ComplexMap", "complex", "concentration" }

chainComplex Complex := ChainComplex => (cacheValue symbol ChainComplex) (C -> (
    (lo,hi) := concentration C;
    D := new ChainComplex;
    D.ring = ring C;
    for i from lo to hi do D#i = C_i;
    for i from lo+1 to hi do D.dd#i = dd^C_i;
    D
    ))

complex ChainComplex := Complex => {} >> opts -> (cacheValue symbol Complex)(D -> (
    (lo,hi) := (min D, max D);
    while lo < hi and (D_lo).numgens == 0 do lo = lo+1;
    while lo < hi and (D_hi).numgens == 0 do hi = hi-1;
    if lo === hi then
        complex(D_lo, Base => lo)
    else
        complex hashTable for i from lo+1 to hi list i => D.dd_i
    ))

chainComplex ComplexMap := ChainComplexMap => f -> (
    g := new ChainComplexMap;
    g.cache = new CacheTable;
    g.source = chainComplex source f;
    g.target = chainComplex target f;
    g.degree = degree f;
    (lo,hi) := concentration f;
    for i from lo to hi do g#i = f_i;
    g
    )

complex ChainComplexMap := ComplexMap => {} >> opts -> g -> (
    map(complex target g, complex source g, i -> g_i, Degree => degree g)
    )
