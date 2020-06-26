newPackage(
        "Complexes",
        Version => "0.9", 
        Date => "22 June 2020",
    	Authors => {
	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
        Headline => "development package for beta testing new version of chain complexes",
        AuxiliaryFiles => false,
        DebuggingMode => false
        )

export {
    "component",
    -- types
    "Complex",
    "ComplexMap",
    -- functions/methods
    "augmentationMap",
    "canonicalMap",
    "canonicalTruncation",
    "complex",
    "concentration",
    "connectingMap",
    "cylinder",
    "freeResolution",
    "homotopic",
    "horseshoeResolution",
    "isComplexMorphism",
    "isExact",
    "isFree", -- TODO: move to Core, use for freemodules too
    "isQuasiIsomorphism",
    "isNullHomotopic",
    "isNullHomotopyOf",
    "isShortExactSequence",
    "liftMapAlongQuasiIsomorphism",
    "minimize",
    "minimizingMap",
    "nullHomotopy",
    "naiveTruncation",
    "randomComplexMap",
    "resolutionMap",
    "yonedaExtension",
    "yonedaExtension'",
    "yonedaMap",
    "yonedaMap'",
    "yonedaProduct",
    -- options used
    "Concentration",
    "Cycle",
    "Boundary",
    "InternalDegree",
    "homotopy",
    "Base",
    "ResolutionMap",
    "UseTarget",
    "HomWithComponents"
    }

-- << "----------------------------------------------------------------------------" << endl
-- << "-- Experimental package                                                   --" << endl;
-- << "-- This package will replace ChainComplexes at                            --" << endl;
-- << "-- a future date.  The type 'Complex' here will                           --" << endl;
-- << "-- be changed to 'ChainComplex'.                                          --" << endl;
-- << "--                                                                        --" << endl;
-- << "-- Purpose: to more fully implement functoriality in homological algebra  --" << endl;
-- << "-- Primary authors: Greg Smith and Mike Stillman                          --" << endl;
-- << "--                                                                        --" << endl;
-- << "-- For questions, suggestions, comments, bugs, please email either author --" << endl;
-- << "----------------------------------------------------------------------------" << endl

--load(currentFileDirectory | "Complexes/res.m2")
unimplemented = str -> error(str|": not yet implemented")

-- keys into Complex
protect modules

Complex = new Type of MutableHashTable --
  -- note: we make this mutable in order to construct the
  --   differential as a ComplexMap referring to this Complex
  -- BUT: after construction, it is should be IMMUTABLE!!
  -- at some point, we might want to allow lazy determination of the modules and maps
  -- but for now, we insist that all modules and maps are explicit.
  -- key:
  --  ring
  --  modules: hash table: ZZ => Module
  --  differential: ComplexMap from C --> C, degree -1.
  --  concentration: (lo:ZZ,hi:ZZ) C_i = 0 for i < lo and i > hi.
  --    not all of the keys modules#i, for lo <= i <= hi need be present.
  --    missing ones are presumed to be the zero module.
  --  cache: a CacheTable

ComplexMap = new Type of HashTable
  -- keys:
  --   degree: ZZ
  --   source: Complex over a ring R
  --   target: Complex over the same ring R
  --   maps themselves (HashTable of Matrices), keys lying in the concentration (lo,hi) of the source.
  --    not all of the keys maps#i, for lo <= i <= hi need be present.
  --    missing ones are presumed to be zero maps.
  --   cache: a CacheTable
  --    cache.isCommutative: whether this map commutes with the differentials
  --      not set until needed.  unset means we have not checked yet, 
  --          and the user hasn't declared it to be true/false yet.

Complex.synonym = "complex"
ComplexMap.synonym = "map of complexes"

-- net, expression
-- construction:
--  complex (Sequence of maps, optional argument telling you the start location)
--  complex (Module)
-- A Complex is indexed by the integers

-----------------------------------
-- Complex ------------------------
-----------------------------------
ring Complex := C -> C.ring

complex = method(Options => {Base=>0})
complex HashTable := opts -> maps -> (
    spots := sort keys maps;
    if #spots === 0 then
      error "expected at least one matrix";
    if not all(spots, k -> instance(k,ZZ)) then
      error "expected matrices to be labelled by integers";
    if not all(spots, k -> instance(maps#k,Matrix)) then
      error "expected hash table or list of matrices";
    R := ring maps#(spots#0);
    if not all(values maps, f -> ring f === R) then
      error "expected all matrices to be over the same ring";
    moduleList := new MutableHashTable;
    for k in spots do (
        if not moduleList#?(k-1) 
          then moduleList#(k-1) = target maps#k;
        moduleList#k = source maps#k;
        );
    C := new Complex from {
           symbol ring => R,
           symbol module => new HashTable from moduleList,
           symbol concentration => (first spots - 1, last spots),
           symbol cache => new CacheTable
           };
    C.dd = map(C,C,maps,Degree=>-1);
    C
    )
complex List := opts -> L -> (
    -- L is a list of matrices or a list of modules
    if not instance(opts.Base, ZZ) then
      error "expected Base to be an integer";
    if all(L, ell -> instance(ell,Matrix)) then (
        mapHash := hashTable for i from 0 to #L-1 list opts.Base+i+1 => L#i;
        return complex(mapHash, opts)
        );
    if all(L, ell -> instance(ell,Module)) then (
        R := ring L#0;
        if any(L, ell -> ring ell =!= R) then
            error "expected modules all over the same ring";
        moduleHash := hashTable for i from 0 to #L-1 list opts.Base + i => L#i;
        C := new Complex from {
            symbol ring => R,
            symbol concentration => (opts.Base, opts.Base + #L - 1),
            symbol module => moduleHash,
            symbol cache => new CacheTable
            };
        C.dd = map(C,C,0,Degree=>-1);
        return C;
        );
    error "expected a list of matrices or a list of modules";
    )
complex Module := opts -> (M) -> (
    if not instance(opts.Base, ZZ) then
      error "complex: expected base to be an integer";
    C := new Complex from {
           symbol ring => ring M,
           symbol concentration => (opts.Base,opts.Base),
           symbol module => hashTable {opts.Base => M},
           symbol cache => new CacheTable
           };
    C.dd = map(C,C,0,Degree=>-1);
    C
    )
complex Ring := opts -> R -> complex(R^1, opts)
complex Ideal := opts -> I -> complex(module I, opts)
complex ComplexMap := opts -> (f) -> (
    if degree f === -1 then (
        if source f =!= target f then error "expected a differential";
        (lo,hi) := concentration source f;
        if lo === hi then return complex((source f)_lo, Base=>lo);
        newmaps := hashTable for i from lo+1 to hi list i => f_i;
        return complex newmaps
        );
    -- TODO: keep this??  implement it??  what is it??
    -- f : C --> C, degree -1, then return (C,f) as a complex.
    -- f : C --> C[-1], return (C,f) as a complex
    -- complex (0 * dd^C)
    )
complex Complex := opts -> C -> (
    -- all this does is change the homological degrees 
    -- so the concentration begins at opts.Base
    (lo,hi) := concentration C;
    if lo === opts.Base then
        C
    else if lo === hi then 
        complex(C_lo, Base=>opts.Base)
    else (
        L := for i from lo+1 to hi list dd^C_i;
        complex(L, Base=>opts.Base)
        )
    )

isWellDefined Complex := C -> (
    k := keys C;
    expectedKeys := set {
        symbol ring, 
        symbol concentration, 
        symbol module, 
        symbol dd,
        symbol cache
        };
    if set k =!= expectedKeys
    then (
        if debugLevel > 0 then (
            added := toList(k - expectedKeys);
            missing := toList(expectedKeys - k);
            if #added > 0 then << "-- unexpected key(s): " << toString added << endl;
            if #missing > 0 then << "-- missing key(s): " << toString missing << endl;
            );
        return false;
        );
    -- check keys, check their types
    if not instance(C.ring, Ring) then (
        if debugLevel > 0 then (
            << "-- expected 'ring C' to be a ring" << endl;
            );
        return false;
        );
    (lo,hi) := C.concentration;
    if not instance(lo,ZZ) or not instance(hi,ZZ) or lo > hi then (
        if debugLevel > 0 then (
            << "-- expected C.concentration of the form (lo,hi) with lo<=hi integers" << endl;
            );
        return false;
        );
    if not instance(C.module, HashTable) then (
        if debugLevel > 0 then (
            << "-- expected C.module to be a HashTable" << endl;
            );
        return false;
        );    
    if not instance(C.dd, ComplexMap) then (
        if debugLevel > 0 then (
            << "-- expected dd^C to be a ComplexMap" << endl;
            );
        return false;
        );
    if not instance(C.cache, CacheTable) then (
        if debugLevel > 0 then (
            << "-- expected 'C.cache' to be a CacheTable" << endl;
            );
        return false;
        );
    -- check ring matches modules
    if not all(keys C.module, i -> instance(i,ZZ) and i >= lo and i <= hi) then (
        if debugLevel > 0 then (
            << "-- expected all keys of C.module to be integers in concentration range " << [lo,hi] << endl;
            );
        return false;
        );
    if not all(values C.module, m -> ring m === ring C) then (
        if debugLevel > 0 then (
            << "-- expected all modules in C.module to be over 'ring C'" << endl;
            );
        return false;
        );
    -- check differential
    if ring C.dd =!= ring C then (
        if debugLevel > 0 then (
            << "-- expected ring of the differential to be the ring of the complex" << endl;
            );
        return false;
        );
    if degree C.dd =!= -1 then (
        if debugLevel > 0 then (
            << "-- expected degree of the differential to be -1" << endl;
            );
        return false;
        );
    if not all(keys (dd^C).map, i -> instance(i,ZZ) and i >= lo+1 and i <= hi) then (
        if debugLevel > 0 then (
            << "-- expected all maps in the differential to be indexed by integers in the concentration [lo+1,hi]" << endl;
            );
        return false;
        );
    for i from lo+1 to hi do (
        f := dd^C_i;
        if source f =!= C_i or target f =!= C_(i-1)
        then (
            if debugLevel > 0 then (
                << "-- expected source and target of maps in differential to be modules in the complex " << endl;
                << "--   differential at index " << i << " fails this condition" << endl;                
            );    
            return false;
            );
        );
    for i from lo+2 to hi do (
        f := dd^C_i;
        g := dd^C_(i-1);
        if g*f != 0 then (
            if debugLevel > 0 then (
                << "-- expected maps in the differential to compose to zero " << endl;
                << "--   differentials at indices " << (i,i-1) << " fail this condition" << endl;                
            );    
            return false;
            );
        );
    true
    )

concentration = method()
concentration Complex := Sequence => C -> C.concentration
concentration ComplexMap := Sequence => f -> (
    C := source f;
    D := target f;
    deg := degree f;
    (loC, hiC) := concentration C;
    (loD, hiD) := concentration D;
    lo := max(loC, loD-deg);
    hi := min(hiC, hiD-deg);
    if lo > hi then (loC, loC) else (lo, hi)
    )

max Complex := C -> max concentration C
min Complex := C -> min concentration C

isFree = method()
isFree Complex := Boolean => C -> (
    (lo, hi) := concentration C;
    all(lo..hi, i -> isFreeModule C_i)
    )

isExact = method()
isExact(Complex, Number, Number) := 
isExact(Complex, Number, InfiniteNumber) := 
isExact(Complex, InfiniteNumber, Number) := 
isExact(Complex, InfiniteNumber, InfiniteNumber) := Boolean => (C, lo, hi) -> (
    (loC,hiC) := concentration C;
    lo = max(lo,loC);
    hi = min(hi, hiC);
    all(lo..hi, i -> kernel dd^C_i == image dd^C_(i+1))
    )
isExact Complex := Boolean => C -> (
    (lo,hi) := concentration C;
    isExact(C, lo, hi)
    )

sum Complex := Module => C -> (
    (lo,hi) := concentration C;
    directSum for i from lo to hi list C_i
    )

sum ComplexMap := Matrix => f -> (
    -- f : C --> D
    C := source f;
    D := target f;
    (loC, hiC) := concentration C;
    (loD, hiD) := concentration D;
    d := degree f;
    mats := matrix for j from loD to hiD list (
        for i from loC to hiC list (
            if j == i+d then f_i 
            else
            map(D_j, C_i, 0)
        ));
    map(sum D, sum C, mats)
    )
    
-*
          
    directSum 
    t := sort spots T;
    s := sort spots S;
    u := spots f;
    if #t === 0 and #s === 0 then map(R^0,0)
    else (
	    tar := if #t === 0 then R^0 else directSum apply(t,i->T_i);
	    src := if #s === 0 then R^0 else directSum apply(s,i->S_i);
	    if #u > 0 and same(apply(u, i -> degree f#i))
	    then (
	        deg := degree f#(u#0);
	        map(tar, src, matrix table(t,s,
			        (j,i) -> if j == i+d then f_i else map(T_j,S_i,0)), Degree=>deg)
	        )
	    else (
	        map(tar, src, matrix table(t,s,
		    	    (j,i) -> if j == i+d then f_i else map(T_j,S_i,0))
                ))))
*-

-- This belongs in M2 Core!!!
-- make this into a git issue.
single = v -> (
     if not same v 
     then error "incompatible objects in direct sum";
     v#0)
directSum Sequence := args -> (
    if #args === 0 then error "expected more than 0 arguments";
    y := youngest args;
    key := (directSum, args);
    if y =!= null and y.cache#?key then y.cache#key else (
        type := single apply(args, class);
        meth := lookup(symbol directSum, type);
        if meth === null then error "no method for direct sum";
        S := meth args;
        if y =!= null then y.cache#key = S;
        S))
///
  restart
  S = ZZ/101[a..d]
  C = res coker vars S
  D = C ++ C
  keys C -- (directSum,C,C) this should be in the cache table, not in the mutable hash table itself.
  keys C.cache
  -- The proposed code solves this issue.
///

Complex.directSum = args -> (
    assert(#args > 0);
    R := ring args#0;
    if not all(args, C -> ring C === R) then error "expected all complexes to be over the same ring";
    concentrations := for C in args list C.concentration;
    lo := concentrations/first//min;
    hi := concentrations/last//max;
    D := if lo === hi then (
        complex(directSum for C in args list C_lo, Base=>lo)
        )
    else (
        maps := hashTable for i from lo+1 to hi list i => (
            directSum for C in args list dd^C_i
            );
        complex maps
        );
    D.cache.components = toList args;
    D    
    )
Complex ++ Complex := Complex => (C,D) -> directSum(C,D)
directSum Complex := C -> directSum(1 : C)

components Complex := C -> if C.cache.?components then C.cache.components else {C}

trans := (C,v) -> (
    if C.cache.?indexComponents then (
	    Ci := C.cache.indexComponents;
	    apply(v, i -> if Ci#?i then Ci#i else error "expected an index of a component of the direct sum"))
    else (
        if not C.cache.?components then error "expected a direct sum of complexes";
	    Cc := C.cache.components;
	    apply(v, i -> if not Cc#?i then error "expected an index of a component of the direct sum");
	    v)
    )

Complex _ Array := ComplexMap => (C,v) -> (
    v = trans(C,v);
    D := directSum apply(toList v, j -> C.cache.components#j);
    (lo,hi) := D.concentration;
    maps := hashTable for i from lo to hi list i => C_i_v;
    result := map(C,D,maps);
    result.cache.isCommutative = true;
    result
    )

Complex ^ Array := ComplexMap => (C,v) -> (
    v = trans(C,v);
    D := directSum apply(toList v, j -> C.cache.components#j);
    (lo,hi) := C.concentration;
    maps := hashTable for i from lo to hi list i => C_i^v;
    result := map(D,C,maps);
    result.cache.isCommutative = true;
    result
    )
------------------

Complex Array := (C, L) -> (
    if #L != 1 or not instance(L#0,ZZ) then error "expected an integer shift";
    (lo,hi) := C.concentration;
    if L#0 === 0 then C
    else if lo === hi then (
        complex(C_lo, Base => lo - L#0)
        )
    else (
        newmaps := hashTable for i from lo+1 to hi list (i - L#0) => if odd L#0 then -dd^C_i else dd^C_i;
        complex newmaps
        )
    )

Complex _ ZZ := Module => (C,i) -> if C.module#?i then C.module#i else (ring C)^0
Complex ^ ZZ := Module => (C,i) -> C_(-i)

length Complex := (C) -> (
    C1 := prune C;
    (lo,hi) := C1.concentration;
    hi-lo
    )

Complex == Complex := (C,D) -> (
    if C === D then return true;
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    if ring C =!= ring D then return false;
    for i from min(loC,loD) to max(hiC,hiD) do (
        if C_i != D_i then return false;
        if dd^C_i != dd^D_i then return false;
        );
    true    
    )

Complex == ZZ := (C,n) -> (
    if n =!= 0 then error "cannot compare Complex to non-zero integer";
    (lo,hi) := C.concentration;
    for i from lo to hi do if C_i != 0 then return false;
    true
    )
ZZ == Complex := (n,C) -> C == n

Complex#id = (C) -> (
    (lo,hi) := C.concentration;
    maps := hashTable for i from lo to hi list i => id_(C_i);
    result := map(C,C,maps);
    result.cache.isCommutative = true;
    result
    )

-- Does this even make any sense?  Is the version
-- for ChainComplex actually used?
--RingMap Complex := (phi,C) -> unimplemented "RingMap Complex"

net Complex := C -> (
     (lo,hi) := C.concentration;
     if lo > hi then 
         error "In a complex, lo <= hi should always hold in the concentration"
         --"0"
     else if lo == hi and C_lo === 0 then 
         "0"
     else
         horizontalJoin between(" <-- ", 
             for i from lo to hi list
                 stack (net C_i, " ", net i))
     )

Symbol ^ Complex := (sym, C) -> (
    if sym === dd then C.dd
    else error "expected symbol to be 'dd'"
    )
Symbol _ Complex := (sym, C) -> sym^C

gradedModule Complex := Complex => C -> (
    (lo,hi) := concentration C;
    complex(for i from lo to hi list C_i, Base=>lo)
    )

defaultLengthLimit = (R, baselen, len) -> (
    -- R: Ring
    -- baselen: generally, the length of an input complex
    -- len: integer or infinity
    -- result: is an integer
    if len === infinity then 
      baselen + numgens R + 1 + if ZZ === ultimate(coefficientRing, R) then 1 else 0
    else
      len
    )

freeResolution = method(Options => options resolution)
freeResolution Module := opts -> M -> (
    if opts.LengthLimit < 0 then error "expected a non-negative value for LengthLimit";
    if not M.cache.?freeResolution
      or M.cache.freeResolution.cache.LengthLimit < opts.LengthLimit
      then (
          lengthlimit := defaultLengthLimit(ring M, 0, opts.LengthLimit);
          -- note: we currently suppress other options of freeResolution available in 'res'.
          C := res(M,opts,LengthLimit=>lengthlimit);
          complete C;
          FC := if length C == 0 then complex C_0
                else (
                    maps := for i from 1 to min(length C, opts.LengthLimit) list C.dd_i;
                    complex maps
                    );
          FC.cache.LengthLimit = if length C < lengthlimit then infinity else lengthlimit;
          FC.cache.Module = M;
          M.cache.freeResolution = FC;
         );
    FM := M.cache.freeResolution;
    if opts.LengthLimit < length FM
    then (
        FM = naiveTruncation(FM, 0, opts.LengthLimit);
        FM.cache.Module = M;
        );
    FM
    )
freeResolution Ideal := opts -> I -> freeResolution(comodule I, opts)

isHomogeneous Complex := (C) -> isHomogeneous dd^C
isHomogeneous ComplexMap := (f) -> all(values f.map, isHomogeneous)

-- These next two local functions are lifted from previous code in m2/chaincomplexes.m2
heftfun0 = wt -> d -> sum( min(#wt, #d), i -> wt#i * d#i )
heftfun = (wt1,wt2) -> (
     if wt1 =!= null then heftfun0 wt1
     else if wt2 =!= null then heftfun0 wt2
     else d -> 0
     )

betti Complex := opts -> C -> (
    heftfn := heftfun(opts.Weights, heft ring C);
    (lo,hi) := C.concentration;
    new BettiTally from flatten for i from lo to hi list (
        apply(pairs tally degrees C_i, (d,n) -> (i,d,heftfn d) => n)
        )
    )

regularity Complex := opts -> C -> regularity betti(C,opts)

poincare Complex := C -> (
    S := degreesRing ring C;
    (lo,hi) := concentration C;
    -- WARNING: older ChainComplex code uncomments the following line
    -- use S;
    f := 0_S;
    for i from lo to hi do (
        scanPairs(tally degrees C_i, 
            (d,m) -> f = f + m * (-1)^i * product(# d, j -> S_j^(d_j)));
        );
    f
    )

poincareN Complex := C -> (
    S := degreesRing ring C;
    if not S.?poincareN then S.poincareN = (
        s := getSymbol "S";
        t := getSymbol "T";
        ZZ (monoid[s, t_0 .. t_(degreeLength ring C - 1), 
                Inverses=>true, 
                MonomialOrder => RevLex, 
                Global => false])
        );
    R := S.poincareN;
    (lo,hi) := concentration C;
    f := 0_R;
    for i from lo to hi do (
        scanPairs(tally degrees C_i,
            (d,m) -> f = f + m * R_0^i * product(# d, j -> R_(j+1)^(d_j)))
        );
    f
    )

minimalPresentation Complex := 
prune Complex := Complex => opts -> (cacheValue symbol minimalPresentation)(C -> (
    -- opts is ignored here
    -- to be cached: in the input C: cache the result D
    --               in the result: cache pruningMap: D --> C
    (lo,hi) := C.concentration;
    nonzeros := select(lo..hi, i -> minimalPresentation C_i != 0);
    D := if #nonzeros === 0 
         then (
             complex (ring C)^0
             )
         else (
             lo = min nonzeros;
             hi = max nonzeros;
             if lo === hi 
             then complex(minimalPresentation C_lo, Base=>lo)
             else (
                 maps := hashTable for i from lo+1 to hi list i => minimalPresentation dd^C_i;
                 complex maps
                 )
             );
    -- create the isomorphism D --> C
    (lo,hi) = D.concentration;
    pruning := hashTable for i from lo to hi list i => (minimalPresentation C_i).cache.pruningMap;
    D.cache.pruningMap = map(C,D,pruning);
    D.cache.pruningMap.cache.isCommutative = true;
    D
    ))

homology Complex := Complex => opts -> C -> (
    (lo, hi) := C.concentration;
    modules := hashTable for i from lo to hi list i => homology(i,C);
    if lo == hi then return complex(modules#lo, Base=>lo);
    maps := hashTable for i from lo+1 to hi list i => map(modules#(i-1), modules#i, 0);
    complex maps
    )
homology(ZZ, Complex) := opts -> (i,C) -> homology(dd^C_i, dd^C_(i+1))
cohomology(ZZ,Complex) := opts -> (i,C) -> homology(-i, C)

-----------------------------------
-- ComplexMap ------------
-----------------------------------
source ComplexMap := Complex => f -> f.source
target ComplexMap := Complex => f -> f.target
ring ComplexMap := Complex => f -> ring source f
degree ComplexMap := ZZ => f -> f.degree

map(Complex, Complex, HashTable) := ComplexMap => opts -> (tar, src, maps) -> (
    R := ring tar;
    if ring src =!= R or any(values maps, f -> ring f =!= R) then
        error "expected source, target and maps to be over the same ring";
    deg := if opts.Degree === null 
           then 0 
           else if instance(opts.Degree, ZZ) then 
             opts.Degree
           else
             error "expected integer degree";
    (lo,hi) := src.concentration;
    maps' := hashTable for k in keys maps list (
        if not instance(k, ZZ) then error "expected integer keys";
        f := maps#k;
        -- note: we use != instead of =!= in the next 2 tests,
        -- since we want to ignore any term order differences
        if source f != src_k then
            error ("map with index "|k|" has inconsistent source");
        if target f != tar_(k+deg) then
            error ("map with index "|k|" has inconsistent target");
        if k < lo or k > hi then continue else (k,f)
        );
    new ComplexMap from {
        symbol source => src,
        symbol target => tar,
        symbol degree => deg,
        symbol map => maps',
        symbol cache => new CacheTable
        }
    )
map(Complex, Complex, List) := opts -> (tar, src, maps) -> (
    -- case 1: maps is a (single) list of matrices (maps between components of the complex)
    -- case 2: maps is a double list of ComplexMap's
    --    in this case, if the maps all commute with differentials, and are diagonal, then
    --    we could declare the result to be commutative as well. Should we do this?
    --  Can tell, depending on the class of maps#0.
    (lo,hi) := src.concentration;
    if not instance(maps#0, List) then (
        mapHash := hashTable for i from lo to hi list i => (
            h := maps#(i-lo);
            if h == 0 then continue else h
            );
        return map(tar,src,mapHash,opts)
        );
    -- At this point, the first entry of 'maps' is a List.
    -- Check: it is a table of ComplexMap
    R := ring tar;
    if R =!= ring src then error "expected complexes over the same ring";
    if not isTable maps then error "expected a table of ComplexMaps";
    -- check: all entries which are ComplexMaps have the same homological degree
    deg := if opts.Degree === null 
           then null
           else if instance(opts.Degree, ZZ) then 
             opts.Degree
           else
             error "expected integer degree";
    degs := unique for f in flatten maps list 
        if instance(f,ComplexMap) 
            then degree f 
            else continue;
    if #degs > 1 then error "expected all ComplexMaps to have the same degree";
    if deg =!= null and #degs == 1 and degs#0 =!= deg then error "Supplied degree is incompatible with the ComplexMaps";
    if deg === null then deg = (if #degs == 1 then degs#0 else 0);
    -- At this point, we need to create (block) matrices for each component of the complex.
    mapHash = hashTable for i from lo to hi list i => (
        newmaps := applyTable(maps, f -> if instance(f,ComplexMap) then f_i else f);
        h := map(tar_(i+deg), src_i, matrix newmaps);
        if h == 0 then continue else h
        );
    map(tar,src,mapHash,opts, Degree=>deg)
    )

map(Complex, Complex, Function) := ComplexMap => opts -> (D,C,f) -> (
    deg := if opts.Degree === null then 0 else opts.Degree;
    (loC,hiC) := concentration C;
    (loD,hiD) := concentration D;
    maps := hashTable for i from max(loC,loD-deg) to min(hiC,hiD-deg) list (
        if C_i == 0 or D_(i+deg) == 0 then continue;
        g := f(i);
        if g === null or g == 0 then continue;
        i => g
        );
    map(D,C,maps,Degree=>deg)
    )

map(Complex, Complex, ZZ) := ComplexMap => opts -> (D, C, j) -> (
    if j === 0 then (
        result := map(D,C,hashTable{},opts);
        result.cache.isCommutative = true;
        return result
        );
    if j === 1 then (
        if C == D and (opts.Degree === null or opts.Degree === 0) then
            return id_C;
        error "expected source and target to be the same";
        );
    error "expected integer to be zero or one";
    )

map(Complex, Complex, ComplexMap) := ComplexMap => opts -> (tar, src, f) -> (
    deg := if opts.Degree === null then degree f else opts.Degree;
    H := hashTable for k in keys f.map list k => map(tar_(deg+k), src_k, f.map#k);
    map(tar,src,H, Degree=>deg)
    )

ComplexMap | ComplexMap := ComplexMap => (f,g) -> (
    if target f =!= target g then error "expected targets to be the same";
    deg := degree f;
    if deg =!= degree g then error "expected maps with the same degree";
    result := map(target f, source f ++ source g, {{f,g}}, Degree=>deg);
    if f.cache.?isCommutative and g.cache.?isCommutative then (
      result.cache.isCommutative = f.cache.isCommutative and g.cache.isCommutative;
      );
    result
    )

ComplexMap || ComplexMap := ComplexMap => (f,g) -> (
    if source f =!= source g then error "expected sources to be the same";
    deg := degree f;
    if deg =!= degree g then error "expected maps with the same degree";
    result := map(target f ++ target g, source f, {{f},{g}}, Degree=>deg);
    if f.cache.?isCommutative and g.cache.?isCommutative then (
      result.cache.isCommutative = f.cache.isCommutative and g.cache.isCommutative;
      );
    result
    )

isWellDefined ComplexMap := f -> (
    k := keys f;
    expectedKeys := set {
        symbol source, 
        symbol target, 
        symbol degree, 
        symbol map,
        symbol cache
        };
    if set k =!= expectedKeys
    then (
        if debugLevel > 0 then (
            added := toList(k - expectedKeys);
            missing := toList(expectedKeys - k);
            if #added > 0 then << "-- unexpected key(s): " << toString added << endl;
            if #missing > 0 then << "-- missing key(s): " << toString missing << endl;
            );
        return false;
        );
    -- source and target
    if ring f.source =!= ring f.target then (
        if debugLevel > 0 then (
            << "-- expected source and target to have the same ring" << endl;
            );
        return false;
        );
    if not isWellDefined f.source or not isWellDefined f.target then (
        if debugLevel > 0 then (
            << "-- expected source and target to be well-defined complexes" << endl;
            );
        return false;
        );
    if not instance(f.degree, ZZ) then (
        if debugLevel > 0 then (
            << "-- expected degree of complex homomorphism to be an integer" << endl;
            );
        return false;
        );
    (lo,hi) := f.source.concentration;
    if not all(keys f.map, i -> instance(i,ZZ) and i >= lo and i <= hi) then (
        if debugLevel > 0 then (
            << "-- expected all maps to be indexed by integers in the concentration [lo,hi] of the source" << endl;
            );
        return false;
        );
    for i from lo to hi do (
        g := f_i;
        if source g =!= f.source_i or target g =!= f.target_(i+f.degree)
        then (
            if debugLevel > 0 then (
                << "-- expected source and target of maps in differential to be modules in the complex " << endl;
                << "--   differential at index " << i << " fails this condition" << endl;                
            );    
            return false;
            );
        );
    if f.cache.?isCommutative then (
        deg := degree f;
        C := f.source;
        D := f.target;
        (loC,hiC) := C.concentration;
        (loD,hiD) := D.concentration;
        iscommutative := true;
        for i from loC to hiC do (
            if i+deg-1 >= loD and i+deg-1 <= hiD then (
                if not (dd^D_(i+deg) * f_i == (-1)^deg * (f_(i-1) * dd^C_i))
                then (
                    iscommutative = false;
                    if f.cache.isCommutative then (
                        if debugLevel > 0 then (
                            << "-- the cache table incorrectly asserts that the maps commute with the differentials " << endl;
                            << "--   differential at index " << i << " fails this condition" << endl;                
                            );
                        return false;
                        );
                    )));
        if iscommutative and not f.cache.isCommutative then (
            if debugLevel > 0 then (
                << "-- the cache table incorrectly asserts that the maps do not commute with the differentials " << endl;
                );
            return false;
            );
        );
    true
    )

lineOnTop := (s) -> concatenate(width s : "-") || s

net ComplexMap := f -> (
     v := between("",
            for i in sort keys f.map list (
                horizontalJoin(
		            net (i+f.degree), " : ", net target f_i, " <--",
		            lineOnTop net f_i,
		            "-- ", net source f_i, " : ", net i
                    )
                ));
     if # v === 0 then "0"
     else stack v
     )

ComplexMap _ ZZ := Matrix => (f,i) -> (
    if f.map#?i then f.map#i else map((target f)_(i + degree f), (source f)_i, 0))
ComplexMap ^ ZZ := ComplexMap => (f,n) -> (
    (lo,hi) := (source f).concentration;
    df := degree f;
    if n === -1 then (
        maps := hashTable for i from lo to hi list (i+df) => (
            f_i^(-1)
            );
        result := map(source f, target f, maps, Degree=>-df);
        if f.cache.?isCommutative then result.cache.isCommutative = f.cache.isCommutative;
        result
	    )
    else if n < 0 then (f^-1)^(-n)
    else if n === 0 then id_(source f)
    else if n === 1 then f
    else (
      --if source f != target f then error "expected source and target to be the same";
      maps = hashTable for i from lo to hi list i => (
          s := f_i;
          j := 1;
          while j < n do (
              s = f_(i+j*df) * s;
              j = j+1;
              );
          if s == 0 then continue else s
          );
      result = map(source f, source f, maps, Degree=> n * df);
      if f.cache.?isCommutative then result.cache.isCommutative = f.cache.isCommutative;
      result
      )
  )

ComplexMap == ComplexMap := (f,g) -> (
    if f === g then return true;    
    if source f != source g or target f != target g 
      then return false;
    (lo1,hi1) := (source f).concentration;
    (lo2,hi2) := (source g).concentration;
    for i from min(lo1,lo2) to max(hi1,hi2) do (
        if f_i != g_i then return false;
        );
    true    
    )
ComplexMap == ZZ := Boolean => (f,n) -> (
    if n === 0 then 
        all(keys f.map, k -> f.map#k == 0)
    else if n === 1 then (
        if source f != target f then return false;
        if degree f =!= 0 then return false;
        (lo,hi) := (source f).concentration;
        for i from lo to hi do
            if f_i != 1 then return false;
        f.cache.isCommutative = true;  -- this is the identity, after all!        
        true
        )
    else 
        error "cannot compare ComplexMap to integer other than 0 or 1"
    )
ZZ == ComplexMap := Boolean => (n,f) -> f == n

RingElement * ComplexMap := (r,f) -> (
    -- remark: if isInCenter(RingElement) is implemented,
    -- we could use that here to recognize commutativity
    -- in that case, replace 'isCommutative ring f' with 'isInCenter r'
    -- and simplify '- ComplexMap' by removing the cache test and assignment.
    df := degree f;
    (lo,hi) := (source f).concentration;
    maps := hashTable for i from lo to hi list i => (
        h := r * f_i;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    if isCommutativeCached f and isCommutative ring f then
        result.cache.isCommutative = true;
    result
    )

Number * ComplexMap := (r,f) -> (
    try r = promote(r,ring f) else error "can't promote scalar to ring of complex homomorphism";
    r * f
    )

- ComplexMap := (f) -> (
    result := (-1)*f;
    if isCommutativeCached f then
        result.cache.isCommutative = true;
    result
    )

ComplexMap + ComplexMap := (f,g) -> (
    df := degree f;
    dg := degree g;
    if source f != source g then error "expected complex homomorphisms with the same source";
    if target f != target g then error "expected complex homomorphisms with the same target";
    if df =!= dg then error "expected complex homomorphisms with the same degree";
    (lo,hi) := (source g).concentration;
    maps := hashTable for i from lo to hi list i => (
        h := f_i + g_i;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    if isCommutativeCached f and isCommutativeCached g then 
        result.cache.isCommutative = true;
    result
    )
ComplexMap + Number :=
ComplexMap + RingElement := ComplexMap => (f,r) -> (
    if r == 0 then f
    else (
        if source f != target f
        then error "expected same source and target"
        else f + r*id_(target f))
    )
Number + ComplexMap :=
RingElement + ComplexMap := ComplexMap => (r,f) -> f + r

ComplexMap - Number :=
ComplexMap - RingElement :=
ComplexMap - ComplexMap := ComplexMap => (f,g) -> f + (-1)*g

Number - ComplexMap :=
RingElement - ComplexMap := ComplexMap => (r,f) -> -f + r

ComplexMap * ComplexMap := (f,g) -> (
    df := degree f;
    dg := degree g;
    (lo,hi) := (source g).concentration;
    maps := hashTable for i from lo to hi list i => (
        h := f_(dg + i) * g_i;
        if h == 0 then continue else h
        );
    result := map(target f, source g, maps, Degree=>df+dg);
    if isCommutativeCached f and isCommutativeCached g then 
        result.cache.isCommutative = true;
    result
    )

ComplexMap.directSum = args -> (
    -- args: sequence of ComplexMap's
    -- args: f_i : C_i --> D_i, having same degree deg
    -- result : sum(C_i) --> sum(D_i)
    R := ring args#0;
    deg := degree args#0;
    if not all(args, f -> ring f === R) then 
        error "expected maps all over the same ring";
    if not all(args, f -> degree f === deg) then
        error "expected maps to all have the same degree";
    src := directSum (args/source);
    tar := directSum (args/target);
    -- only keep matrices in the homomorphism that are non-zero
    spots := unique flatten(args/(f -> keys f.map));
    maps := hashTable for i in spots list i => directSum(args/(f -> f_i));
    result := map(tar,src,maps,Degree=>deg);
    result.cache.components = toList args;
    if all(args, isCommutativeCached) then 
        result.cache.isCommutative = true;
    result
    )

ComplexMap ++ ComplexMap := ComplexMap => (f,g) -> directSum(f,g)
directSum ComplexMap := f -> directSum(1 : f)
components ComplexMap := f -> if f.cache.?components then f.cache.components else {f}
ComplexMap ^ Array := ComplexMap => (f,v) -> (target f)^v * f
ComplexMap _ Array := ComplexMap => (f,v) -> f * (source f)_v

ComplexMap Array := ComplexMap => (f,L) -> (
    if #L != 1 or not instance(L#0,ZZ) then error "expected an integer shift";
    maps := hashTable for k in keys f.map list (k - L#0) => f.map#k;
    result := map((target f)[L#0], (source f)[L#0], maps, Degree=> degree f);
    if isCommutativeCached f then result.cache.isCommutative = true;
    result
    )

isCommutative ComplexMap := Boolean => f -> (
    if debugLevel == 0 and f.cache.?isCommutative then 
       return f.cache.isCommutative;
    C := source f;
    D := target f;
    deg := degree f;
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    for i from loC to hiC do (
        if i+deg-1 >= loD and i+deg-1 <= hiD then (
            if not (dd^D_(i+deg) * f_i == (-1)^deg * (f_(i-1) * dd^C_i))
            then (
                if debugLevel > 0 then (
                    << "-- block " << (i,i-1) << " fails to commute" << endl;
                    );
                f.cache.isCommutative = false;
                return false;
                )
            )
        );
    f.cache.isCommutative = true;
    true
    )

-- the following method is not exported:
isCommutativeCached = method()
isCommutativeCached ComplexMap := Boolean => f -> f.cache.?isCommutative and f.cache.isCommutative

isComplexMorphism = method(TypicalValue => Boolean)
isComplexMorphism ComplexMap := (f) -> (
    if debugLevel > 0 and degree f =!= 0 then (
        << "-- the complex map has non-zero degree" << endl;
        return false;
        );
    degree f === 0 and isCommutative f
    )

------------------------
-- truncations ---------
------------------------
naiveTruncation = method()
naiveTruncation(Complex,Sequence) := Complex => (C,loHi) -> (
    if #loHi =!= 2 then error "expected a truncation interval";
    (lo,hi) := loHi;
    if lo === null then lo = -infinity;
    if hi === null then hi = infinity;
    if lo > hi then error "interval of truncation is empty";
    (loC,hiC) := concentration C;
    lo = max(lo,loC);
    hi = min(hi,hiC);
    if lo === loC and hi === hiC then C
    else if lo >= hi then complex(C_lo, Base=>lo) -- note: if lo > hi, this is the zero complex.
    else complex(hashTable for i from lo+1 to hi list i => dd^C_i, Base=>lo)
    )
naiveTruncation(Complex,ZZ,ZZ) := 
naiveTruncation(Complex,ZZ,InfiniteNumber) := 
naiveTruncation(Complex,InfiniteNumber,ZZ) := 
naiveTruncation(Complex,ZZ,Nothing) := 
naiveTruncation(Complex,Nothing,ZZ) := Complex => (C,lo,hi) -> naiveTruncation(C, (lo,hi))

naiveTruncation(ComplexMap,Sequence,Sequence) := ComplexMap => (f, targetLoHi, sourceLoHi) -> (
    D := naiveTruncation(target f, targetLoHi);
    C := naiveTruncation(source f, sourceLoHi);
    map(D,C,i -> f_i)
    )
naiveTruncation(ComplexMap,Sequence) := ComplexMap => (f,loHi) -> naiveTruncation(f,loHi,loHi)

-- defining property of canonical truncation:
--  ... <- C_lo <-- C_(lo+1) <-- C_(lo+2) <-- ... <- C_hi <-- C_(hi+1) <-- ...
-- if C' is the canonical truncation(>=lo) of C, place C'_lo := ker(dd^C_(lo)) 
-- and for <= hi: place C'_hi := coker(dd^C_(hi+1))
canonicalTruncation = method()
canonicalTruncation(Complex,Sequence) := Complex => (C,loHi) -> (
    if #loHi =!= 2 then error "expected a truncation interval";
    (lo,hi) := loHi;
    if lo === null then lo = -infinity;
    if hi === null then hi = infinity;
    if lo > hi then error "interval of truncation is empty";
    (loC,hiC) := concentration C;
    if lo <= loC and hi >= hiC then C
    else if lo === hi then complex(HH_lo(C), Base=>lo)
    else if lo === hiC then complex(ker dd^C_lo, Base=>lo)
    else if lo > hiC then complex((ring C)^0, Base=>lo)
    else if loC === hi then complex(coker dd^C_(hi+1), Base=>hi)
    else if loC > hi then complex((ring C)^0, Base=>hi)
    else complex(hashTable for i from max(lo+1,loC+1) to min(hi,hiC) list i => (
            if i === lo+1 then (
                K := ker dd^C_lo;
                g := dd^C_(lo+1) // inducedMap(C_lo,K); -- induced map C_(lo+1) --> K.
                if i === hi then map(K, coker dd^C_(hi+1), g) else g
                )
            else if i === hi then map(C_(hi-1), coker dd^C_(hi+1), dd^C_hi)
            else dd^C_i
            )
        )
    )
canonicalTruncation(Complex,ZZ,ZZ) := 
canonicalTruncation(Complex,ZZ,InfiniteNumber) := 
canonicalTruncation(Complex,InfiniteNumber,ZZ) := 
canonicalTruncation(Complex,InfiniteNumber,InfiniteNumber) := 
canonicalTruncation(Complex,ZZ,Nothing) := 
canonicalTruncation(Complex,Nothing,ZZ) := Complex => (C,lo,hi) -> canonicalTruncation(C, (lo,hi))

canonicalTruncation(ComplexMap,Sequence) := ComplexMap => (f, loHi) -> (
    D := target f;
    C := source f;
    deg := degree f;
    lo := if loHi#0 === null then -infinity else loHi#0;
    hi := if loHi#1 === null then infinity else loHi#1;
    D' := canonicalTruncation(D, (lo+deg,hi+deg));
    C' := canonicalTruncation(C, (lo,hi));
    if lo+deg === hi then map(D', C', i -> HH_lo(f), Degree => deg)
    else map(D', C', Degree => deg, i -> if i === lo then (
            g := f_lo * inducedMap(C_lo, C'_lo);
            h := inducedMap(D_(lo+deg), D'_(lo+deg));
            result := g // h;
            if not isWellDefined result then error "canonical truncation is not well defined";
            result
            )
        else if i === hi then (
            map(D'_hi, C'_hi, f_hi)
            )
        else f_i
        )
    )

------------------------
-- homology ------------
------------------------
minimalPresentation ComplexMap := 
prune ComplexMap := ComplexMap => opts -> f -> (
    C := source f;
    if not C.cache.?pruningMap then f = f * (minimalPresentation C).cache.pruningMap;
    D := target f;
    if not D.cache.?pruningMap then f = (minimalPresentation D).cache.pruningMap^-1 * f;
    f
    --map(minimalPresentation target f, minimalPresentation source f, k -> minimalPresentation f_k)
    )

homology(ZZ,ComplexMap) := Matrix => opts -> (i,f) -> (
    inducedMap(homology(i+degree f,target f), homology(i,source f),f_i)
    )
cohomology(ZZ,ComplexMap) := Matrix => opts -> (i,f) -> homology(-i, f)
homology ComplexMap := ComplexMap => opts -> (f) -> (
    C := source f;
    D := target f;
    deg := degree f;
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    HC := homology C;
    HD := homology D;
    maps := hashTable for i from max(loC,deg+loD) to min(hiC,deg+hiD) list
        i => inducedMap(HD_(i+deg), HC_i, f_i);
    result := map(HD, HC, maps, Degree => deg);
    result.cache.isCommutative = true; -- all differentials are zero
    result
    )

--------------
-- Hom -------
--------------
component = method()
component(Module,Thing) := (M,k) -> (
    if not M.cache.?indexComponents then error "expected Module to be a direct sum with indexed components";
    if not M.cache.indexComponents#?k then error("expected "|toString k|" to be the index of a component");
    (components M)#(M.cache.indexComponents#k)
    )
Hom(Complex, Complex) := Complex => (C,D) -> (
    -- signs here are based from Christensen and Foxby
    -- which agrees with Conrad (Grothendieck duality book)
    Y := youngest(C,D);
    if Y.cache#?(Hom,C,D) then return Y.cache#(Hom,C,D);
    R := ring C;
    if ring D =!= R then error "expected complexes over the same ring";
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    modules := hashTable for i from loD-hiC to hiD-loC list i => (
        directSum for j from loC to hiC list {j,j+i} => Hom(C_j, D_(j+i))
        );
    if loC === hiC and loD === hiD then (
        result := complex(modules#(loD-hiC), Base => loD-loC);
        result.cache.homomorphism = (C,D); -- source first, then target        
        Y.cache#(Hom,C,D) = result;
        return result;
        );
    maps := hashTable for i from loD-hiC+1 to hiD-loC list i => (
        map(modules#(i-1),
            modules#i,
            matrix table(
                indices modules#(i-1),
                indices modules#i,
                (j,k) -> (
                    tar := component(modules#(i-1), j);
                    src := component(modules#i, k);
                    map(tar, src, 
                        if k-j === {0,1} then (-1)^(k#1-k#0+1) * Hom(C_(k#0),dd^D_(k#1))
                        else if k-j === { -1,0 } then Hom(dd^C_(j#0),D_(k#1))
                        else 0)
                    ))));
    result = complex maps;
    result.cache.homomorphism = (C,D); -- source first, then target
    Y.cache#(Hom,C,D) = result;
    result
    )
Hom(Module, Complex) := Complex => (M,C) -> Hom(complex M, C)
Hom(Complex, Module) := Complex => (C,M) -> Hom(C, complex M)
Hom(Complex, Ring) := Complex => (C,R) -> Hom(C, complex R)
Hom(Ring, Complex) := Complex => (R,C) -> Hom(complex R, C)

dual Complex := Complex => {} >> o -> (C) -> Hom(C, (ring C)^1)

Hom(ComplexMap, ComplexMap) := ComplexMap => (f,g) -> (
    df := degree f;
    dg := degree g;
    src := Hom(target f, source g);
    tar := Hom(source f, target g);
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    (lo,hi) := src.concentration;
    (loTar, hiTar) := tar.concentration;
    maps := hashTable for i from lo to hi list (
      if i+df+dg < loTar or i+df+dg > hiTar then continue;
      i => (
        m := for q in indices tar_(i+df+dg) list (
               -- so q == {k,k+i+df+dg}
               for p in indices src_i list (
                   -- so p == {j,j+i}, for various j
                   if p#0 - df == q#0 
                   then (
                       sgn := 1; -- function of df, dg, i
                       sgn = (-1)^(df * (i + dg));
                       sgn * Hom(f_(q#0), g_(p#1)) 
                       )
                   else map(component(tar_(i+df+dg), q),
                            component(src_i, p),
                            0)
                   ));
        map(tar_(i+df+dg),
            src_i,
            matrix m
            )
      ));
    result := map(tar, src, maps, Degree=>df+dg);
    if isCommutativeCached f and isCommutativeCached g then
        result.cache.isCommutative = true;
    result    
    -- f : C1 --> C2, g : D1 --> D2
    -- Hom(f,g) : Hom(C2,D1)_i --> Hom(C1,D2)_(i+df+dg) (indices?)
    -- Hom(C2,D1)_i = sum_j Hom(C2_j, D1_(j+i))
    -- Hom(C1,D2)_(i+df+dg) = sum_k Hom(C1_k, D2_(k+i+df+dg))
    -- map takes j-th part of k=j-df part, block diagonal, apparently.
    -- need to premultiply by f_(j-df), postmult by g_(j+i)
    -- Hom(f_(j-df), g_(j+i))
    )
Hom(Complex, ComplexMap) := ComplexMap => (C,g) -> Hom(id_C, g)
Hom(ComplexMap, Complex) := ComplexMap => (f,D) -> Hom(f, id_D)
Hom(Module, ComplexMap) := ComplexMap => (M,g) -> Hom(complex M, g)
Hom(ComplexMap, Module) := ComplexMap => (f,N) -> Hom(f, complex N)
Hom(Ring, ComplexMap) := ComplexMap => (R,f) -> Hom(complex R, f)
Hom(ComplexMap, Ring) := ComplexMap => (f,R) -> Hom(f, complex R)
Hom(Complex, Matrix) := ComplexMap => (C,g) -> 
    Hom(C, map(complex target g, complex source g, i -> if i === 0 then g))
Hom(Matrix, Complex) := ComplexMap => (f,D) -> 
    Hom(map(complex target f, complex source f, i -> if i === 0 then f), D)
Hom(ComplexMap, Matrix) := ComplexMap => (f,g) -> 
    Hom(f, map(complex target g, complex source g, i -> if i === 0 then g))
Hom(Matrix, ComplexMap) := ComplexMap => (f,g) -> 
    Hom(map(complex target f, complex source f, i -> if i === 0 then f), g)



dual ComplexMap := ComplexMap => {} >> o -> f -> Hom(f, (ring f)^1)

homomorphism(ZZ, Matrix, Complex) := ComplexMap => (i, f, E) -> (
    -- f: R^1 --> E_i (R is the ring of E and phi)
    -- E: is a Complex, the output of Hom(C,D), C,D complexes.
    -- return the complex homomorphism g : C --> D of degree i.
    -- The following local function is a bit of a hack: finds the right components buried in
    -- a cache table.
    fixme := (g) -> map(((target g).cache).components#0, source g, g);
    if not E.cache.?homomorphism then error "expected target of map to be of the form 'Hom(C,D)'";
    if not isFreeModule source f
    or not rank source f == 1 then error "expected source of map to be free of rank 1";
    if E_i =!= target f then (
        -- if f arises from a kernel computation, then the target is not E_i
        -- it is instead a submodule of E_i.  The next line provides the 'f'
        -- that maps directly to E_i.
        -- BUT: if you just use 'ambient f', which seems like it should
        -- work, the problem is that the target of the map 'ambient f'
        -- doesn't retain the information about the components of E_i
        f = map(E_i, source f, super f);
        );
    (C,D) := E.cache.homomorphism;
    (lo,hi) := concentration C;
    H := hashTable for j from lo to hi list j => 
      homomorphism fixme f^[{j,j+i}];
    map(D,C,H, Degree=>i)
    )
homomorphism ComplexMap := ComplexMap => (h) -> (
    -- h should be a homomorphism of complexes from R^1[-i] --> E = Hom(C,D)
    -- returns the corresponding f : C --> D.
    R1 := source h;
    (lo,hi) := R1.concentration;
    i := if lo === hi then lo else (
        which := null;
        for j from lo to hi do if R1_j != 0 then (
            if which =!= null then error "expected source of map to be supported in one homological degree";            
            which = j;
            );
        if which === null then error "expected source of map to be supported in one homological degree";            
        which
        );
    homomorphism(i, h_i, target h)
    )

homomorphism' ComplexMap := ComplexMap => (f) -> (
    R := ring f;
    C := source f;
    D := target f;
    d := degree f;
    H := Hom(C,D);
    (lo,hi) := concentration C;
    -- want R^1[0] --> H
    g := map(H_d, R^1, matrix(for i from lo to hi list {matrix homomorphism' f_i}));
    map(H, complex R, hashTable {0 => g}, Degree=>d)
    )
    
----------------------
-- Tensor products ---
----------------------
tensor(Complex, Complex) := Complex => opts -> (C, D) -> (
    Y := youngest(C,D);
    if Y.cache#?(tensor,C,D) then return Y.cache#(tensor,C,D);
    R := ring C;
    if ring D =!= R then error "expected complexes over the same ring";
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    modules := hashTable for i from loC+loD to hiC+hiD list i => (
        directSum for j from loC to hiC list {j,i-j} => C_j ** D_(i-j)
        );
    if loC === hiC and loD === hiD then (
        result := complex(modules#(loC+loD), Base => loC+loD);
        result.cache.tensor = (C,D);
        Y.cache#(tensor,C,D) = result;
        return result;
        );
    maps := hashTable for i from loC+loD+1 to hiC+hiD list i => (
        map(modules#(i-1),
            modules#i,
            matrix table(
                indices modules#(i-1),
                indices modules#i,
                (j,k) -> (
                    tar := component(modules#(i-1), j);
                    src := component(modules#i, k);
                    m := map(tar, src, 
                        if k-j === {0,1} then (-1)^(k#0) * (C_(k#0) ** dd^D_(k#1))
                        else if k-j === {1,0} then (dd^C_(k#0) ** D_(k#1))
                        else 0);
                    m
                    ))));
    result = complex maps;
    result.cache.tensor = (C,D);
    Y.cache#(tensor,C,D) = result;
    result
    )

Complex ** Complex := Complex => (C,D) -> tensor(C,D)
Module ** Complex := Complex => (M,D) -> (complex M) ** D
Complex ** Module := Complex => (C,N) -> C ** (complex N)

Complex ** Ring := Complex => (C,R) -> (
    (lo,hi) := concentration C;
    moduleHash := hashTable for i from lo to hi list i => C_i ** R;
    if lo === hi then 
        return complex(moduleHash#lo, Base=>lo);
    mapHash := hashTable for i from lo+1 to hi list i => 
        map(moduleHash#(i-1), moduleHash#i, (cover dd^C_i) ** R);
    complex mapHash
    )

ComplexMap ** Ring := ComplexMap => (f,R) -> (
    C := (source f) ** R;
    D := (target f) ** R;
    deg := degree f;
    (lo,hi) := concentration f;
    maps := hashTable for i from lo to hi list i => map(D_(i+deg), C_i, (cover f_i) ** R);
    result := map(D, C, maps, Degree => deg);
    if isCommutativeCached f then
        result.cache.isCommutative = true;
    result
    )

RingMap Complex := Complex => (phi,C) -> (
    (lo,hi) := concentration C;
    moduleHash := hashTable for i from lo to hi list i => phi C_i;
    if lo === hi then 
        return complex(moduleHash#lo, Base=>lo);
    mapHash := hashTable for i from lo+1 to hi list i => 
        map(moduleHash#(i-1), moduleHash#i, phi dd^C_i);
    complex mapHash
    )

RingMap ComplexMap := ComplexMap => (phi,f) ->
    map(phi target f, phi source f, i -> phi f_i)

tensor(RingMap, Complex) := Complex => opts -> (phi, C) -> (
    if source phi =!= ring C then error "expected the source of the ring map to be the ring of the complex";
    (lo,hi) := concentration C;
    modules := hashTable for i from lo to hi list i => tensor(phi, C_i);
    if lo === hi then 
        return complex(modules#lo, Base=>lo);
    maps := hashTable for i from lo+1 to hi list i => 
        map(modules#(i-1), modules#i, tensor(phi, matrix dd^C_i));
    complex maps
    )
tensor(RingMap, ComplexMap) := ComplexMap => opts -> (phi, f) -> (
    if source phi =!= ring f then error "expected the source of the ring map to be the ring of the complex map";
    map(tensor(phi, target f), tensor(phi, source f), i -> tensor(phi, matrix f_i))
    )
RingMap ** Complex := Complex => (phi, C) -> tensor(phi, C)
RingMap ** ComplexMap := ComplexMap => (phi, f) -> tensor(phi, f)

tensor(ComplexMap, ComplexMap) := ComplexMap => opts -> (f,g) -> (
    -- f : C1 --> C2, g : D1 --> D2
    -- f**g : C1**D1 --> C2**D2
    -- (f**g)_i : sum_j(C1_j ** D1_(i-j) --> C2_(j+df) ** D2_(i-j+dg))
    df := degree f;
    dg := degree g;
    src := (source f) ** (source g);
    tar := (target f) ** (target g);
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    (lo,hi) := src.concentration;
    maps := hashTable for i from lo to hi list i => (
        if tar_(i+df+dg) == 0 or src_i == 0 then (
            map(tar_(i+df+dg), src_i, 0)
            )
        else (
            m := for q in indices tar_(i+df+dg) list (
                -- so q == {k,i+df+dg-k}
                for p in indices src_i list (
                    -- so p == {j,i-j}, for various j
                    if p#0 == q#0 - df
                    then (
                        sgn := 1; -- function of df, dg, i
                        sgn = (-1)^(i * dg);
                        sgn * (f_(p#0) **  g_(p#1))
                        )
                    else map(component(tar_(i+df+dg), q),
                        component(src_i, p),
                        0)
                    ));
            map(tar_(i+df+dg), src_i, matrix m)
            )
        );
    result := map(tar, src, maps, Degree=>df+dg);
    if isCommutativeCached f and isCommutativeCached g then
        result.cache.isCommutative = true;
    result    
    )
ComplexMap ** ComplexMap := ComplexMap => (f,g) -> tensor(f,g)
Complex ** ComplexMap := ComplexMap => (C,g) -> id_C ** g
ComplexMap ** Complex := ComplexMap => (f,D) -> f ** id_D
Module ** ComplexMap := ComplexMap => (M,g) -> (complex M) ** g
ComplexMap ** Module := ComplexMap => (f,N) -> f ** (complex N)
--------------
-- sign convention: Using Conrad (Grothendieck Duality) sign choice for cone, pg 8 of intro. 
-- NOTE: one could extend this to complex maps which commute, but have nonzero degree,
--  IF this would be useful at all.
cone ComplexMap := Complex => f -> (
    if not isComplexMorphism f then 
        error "expected a complex morphism";
    B := source f;
    C := target f;
    (loB,hiB) := B.concentration;
    (loC,hiC) := C.concentration;
    lo := min(loB+1,loC);
    hi := max(hiB+1,hiC);
    modules := hashTable for i from lo to hi list i => B_(i-1) ++ C_i;
    maps := hashTable for i from lo+1 to hi list i => (
        map(modules#(i-1), modules#i, matrix{
                { - dd^B_(i-1), map(B_(i-2), C_i, 0) }, 
                { f_(i-1),      dd^C_i               }
                }
            )
        );
    result := if lo === hi then complex(modules#lo, Base=>lo) else complex maps;
    result.cache.cone = f;
    result
    )

cylinder = method()
cylinder ComplexMap := Complex => f -> (
    if not isComplexMorphism f then 
        error "expected a complex morphism";
    B := source f;
    C := target f;
    (loB,hiB) := B.concentration;
    (loC,hiC) := C.concentration;
    lo := min(loB,loC);
    hi := max(hiB+1,hiC);
    modules := hashTable for i from lo to hi list i => B_(i-1) ++ B_i ++ C_i;
    maps := hashTable for i from lo+1 to hi list i => (
        map(modules#(i-1), modules#i, matrix{
                { - dd^B_(i-1)  , 0      , 0     }, 
                { -id_(B_(i-1)) , dd^B_i , 0     },
                { f_(i-1)       , 0      , dd^C_i}
                }
            )
        );
    result := complex maps;
    result.cache.cylinder = f;
    result
    )

isQuasiIsomorphism = method(Options => {Concentration => (-infinity,infinity)})
-- 25 May 2018: possible new code, but indices seem wrong.
isQuasiIsomorphism ComplexMap := Boolean => opts -> f -> (
    (lof,hif) := concentration f;
    (loO,hiO) := opts.Concentration;
    all(max(lof,loO)..min(hif,hiO),
        i -> HH_(i+1) cone f == 0
        )
    )

isShortExactSequence = method()
isShortExactSequence(ComplexMap, ComplexMap) := Boolean => (g, f) -> (
    -- f : A --> B, g : B --> C
    -- the SES is 0 --> A --> B --> C --> 0.
    isWellDefined g and 
    isWellDefined f and
    isComplexMorphism g and
    isComplexMorphism f and
    g*f == 0 and
    image f == kernel g and
    kernel f == 0 and
    coker g == 0
    )  
isShortExactSequence(Matrix, Matrix) := Boolean => (g, f) -> (
    -- f : A --> B, g : B --> C
    -- the SES is 0 --> A --> B --> C --> 0.
    g*f == 0 and
    image f == kernel g and
    kernel f == 0 and
    coker g == 0
    )  

-- TODO: this function needs to be more completely debugged
canonicalMap = method(Options => {UseTarget=>null})
  -- UseTarget is used to disambiguate the two canonical maps
  --   B --> cylinder f
  --   C --> cylinder f
  -- when B == C, f : B --> C.  
  -- UseTarget=>true selects the second of these.
canonicalMap(Complex, Complex) := opts -> (E,D) -> (
    local f;
    local B;
    local C;
    local lo;
    local hi;
    local maps;
    -- D --> E
    -- case: D = ker f, f : B --> C (so E == B)
    if D.cache.?kernel then (
        f = D.cache.kernel;
        if E == source f then (
            (lo,hi) = D.concentration;
            maps = hashTable for i from lo to hi list i => 
              map(E_i, D_i, generators D_i);
            return map(E,D,maps);
            );
        );
    -- case: E = coker f, f : B --> C (so D == C)
    if E.cache.?cokernel then (
        f = E.cache.cokernel;
        if D == target f then (
            (lo,hi) = D.concentration;
            maps = hashTable for i from lo to hi list i => 
              map(E_i, D_i, id_(D_i));
            return map(E,D,maps);
            );
        );
    -- case: D = image f, f : B --> C, (so E == C)
    --  result: image f --> target f
    if D.cache.?image then (
        f = D.cache.image;
        if E == target f then (
            (lo,hi) = D.concentration;
            maps = hashTable for i from lo to hi list i => 
              map(E_i, D_i, generators D_i);
            return map(E,D,maps);
            );
        );
    -- case: coimage
    --  result : source f --> coimage f
    if E.cache.?coimage then (
        f = E.cache.coimage;
        if D == source f then (
            (lo,hi) = D.concentration;
            maps = hashTable for i from lo to hi list i => 
              map(E_i, D_i, id_(D_i));
            return map(E,D,maps);
            );
        );
    -- case: E is a cone
    if E.cache.?cone then (
        f = E.cache.cone; -- f : B --> C
        C = target f;
        if D == C then (
            -- C==target f --> E==cone f
            return map(E, D, {{map((source f)[-1], C, 0)}, {id_C}})
        ));
    -- case: D is a cone
    if D.cache.?cone then (
        f = D.cache.cone; -- f : B --> C
        B' := (source f)[-1];
        if E == B' then (
            -- cone f --> B[-1]
            return map(E, D, {{id_B', map(B', target f, 0)}})
        ));
--XXX
    -- case: E is a cylinder
    if E.cache.?cylinder then (
        f = E.cache.cylinder; -- f : B --> C
        C = target f;
        B = source f;
        if opts.UseTarget === null and B == C then 
          error "Employ the option UseTarget=> to disambiguate choice of canonical map";
        if (opts.UseTarget === true or opts.UseTarget === null) and D == C then (
            -- case A: target f --> cylinder f
            return map(E, D, {
                    {map(B[-1], D, 0)},
                    {map(B, D, 0)},
                    {id_C}});
            );
        if (opts.UseTarget === false or opts.UseTarget === null) and D == B then (
            -- case B: source f --> cylinder f
            return map(E, D, {
                    {map(B[-1], D, 0)},
                    {id_B},
                    {map(C, D, 0)}});
            );
        );
    -- case: D is a cylinder
    if D.cache.?cylinder then (
        f = D.cache.cylinder; -- f : B --> C
        C = target f;
        B = source f;
        B' = B[-1];
        if E == C then (
            -- case A: cylinder f --> target f
            return map(E, D, {{map(E, B', 0), f, id_C}});
            );
        if (E.cache.?cone and E.cache.cone == f) or
           E == cone f then (
            -- case B: cylinder f --> cone f
            return map(E, D, {
                    {id_B'      , map(B', B, 0), map(B', C, 0) },
                    {map(C,B',0), map(C, B, 0) , id_C          }
                    });
            );
        );
    -- Place other possible canonical maps here, and
    -- return the map, if it exists, otherwise fall through.
    error "no canonical map between given complexes";
    )

inducedMap(Complex, Complex) := ComplexMap => opts -> (D,C) -> (
    -- compute f : C --> D the map induced by the identity matrix.
    deg := if opts.Degree === null then 0 else opts.Degree;
    (loC,hiC) := concentration C;
    (loD,hiD) := concentration D;
    maps := hashTable for i from max(loC,loD-deg) to min(hiC,hiD-deg) list i => inducedMap(D_(i+deg),C_i);
    map(D,C,maps,Degree=>deg)
    )

kernel ComplexMap := Complex => opts -> f -> (
    -- f : B --> C
    B := source f;
    (lo,hi) := B.concentration;
    modules := hashTable for i from lo to hi list i => kernel f_i;
    result := if lo === hi then complex(modules#lo, Base => lo)
        else (
            inducedMaps := hashTable for i from lo to hi list i => inducedMap(B_i, modules#i);
            maps := hashTable for i from lo+1 to hi list i => (
                (dd^B_i * inducedMaps#i) // inducedMaps#(i-1)
                );
            complex maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the source to induce a well-defined differential on the kernel";
    result.cache.kernel = f;
    result
    )
cokernel ComplexMap := Complex => f -> (
    -- f : B --> C
    C := target f;
    (lo,hi) := C.concentration;
    deg := degree f;
    modules := hashTable for i from lo to hi list i => cokernel f_(i-deg);
    result := if lo === hi then complex(modules#lo, Base => lo)
        else (
            maps := hashTable for i from lo+1 to hi list i => (
                map(modules#(i-1), modules#i, matrix dd^C_i)
                );
            complex maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the target to induce a well-defined differential on the cokernel";
    result.cache.cokernel = f;
    result
    )

image ComplexMap := Complex => f -> (
    -- f : B --> C
    B := source f;
    C := target f;
    deg := degree f;
    (lo,hi) := C.concentration;
    modules := hashTable for i from lo to hi list i => image f_(i-deg);
    result :=  if lo === hi then complex(modules#lo, Base => lo)
        else (
            maps := if isCommutativeCached f then (
                hashTable for i from lo+1 to hi list i => (
                    map(modules#(i-1), modules#i, matrix dd^B_(i-deg))
                )) 
                else (
                    inducedMaps := hashTable for i from lo to hi list i => inducedMap(C_i, modules#i);
                    hashTable for i from lo+1 to hi list i => (
                        map(modules#(i-1), modules#i, (dd^C_i * inducedMaps#i) // inducedMaps#(i-1))
                        ));
            complex maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the target to induce a well-defined differential on the image";
    result.cache.image = f;
    result
    )

coimage ComplexMap := Complex => f -> (
    -- f : B --> C
    B := source f;
    (lo,hi) := B.concentration;
    modules := hashTable for i from lo to hi list i => coimage f_i;
    result := if lo === hi then complex(modules#lo, Base => lo)
        else (
            maps := hashTable for i from lo+1 to hi list i => (
                map(modules#(i-1), modules#i, matrix dd^B_i)
                );
            complex maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the source to induce a well-defined differential on the coimage";
    result.cache.coimage = f;
    result
    )

extend(Complex,Complex,Matrix) := ComplexMap => opts -> (D,C,f)-> (
    -- assumptions:
    -- (a) f : C_lo --> D_lo, where lo is the smallest homological index in C.
    -- (b) C should be a complex of free modules (free objects?)
    -- (c) D should be an acyclic complex (i.e. only homology is at homological index 'lo')
    -- output:
    --   a ComplexMorphism, g : C --> D such that g_lo = f.
    (lo,hi) := C.concentration;
    g := f; -- at each step, g : C_(i-1) -> D_(i-1)
    maps := hashTable for i from lo to hi list i => (
        if i === lo then f
        else (
            g = (g * dd^C_i) // dd^D_i;
            map(D_i, C_i, g)
            )
        );
    result := map(D,C,maps);
    if false and opts.Verify then (
        if not isComplexMorphism result
        then error "map cannot be extended";
        );
    --result.cache.isCommutative = true;
    result
    )

-- TODO: KLUDGE NEEDING FIXING: the degree argument should be an optional argument: Degree => d
extend(Complex,Complex,Matrix,ZZ) := ComplexMap => opts -> (D,C,f,d)-> (
    -- assumptions:
    -- (a) f : C_(lo-d) --> D_lo, where lo is the smallest homological index in D.
    -- (b) C should be a complex of free modules (free objects?)
    -- (c) D should be an acyclic complex (i.e. only homology is at homological index 'lo')
    -- output:
    --   a ComplexMorphism, g : C --> D of degree d such that g_lo = f.
    (loC, hiC) := C.concentration;
    (loD, hiD) := D.concentration;
    if target f =!= D_loD then error "expected the target of the map to be the beginning of the target complex";
    if source f =!= C_(loD-d) then error "expected the map and the degree to be compatible";
    g := f; -- at each step, g : C_(i-1) -> D_(i-1+d)
    maps := hashTable for i from loD-d to hiC list i => (
        if i === loD-d then f
        else (
            if odd d then g = -g;
            g = (g * dd^C_i) // dd^D_(i+d);
            map(D_(i+d), C_i, g)
            --if odd d and odd (i-loD+d) then imap = -imap;
            )
        );
    result := map(D, C, maps, Degree => d);
    if false and opts.Verify then (
        if not isCommutative result
        then error "map cannot be extended";
        if degree result != d then error "map has incorrect degree";
        );
    --result.cache.isCommutative = true;
    result
    )

-- possible todo: allow to choose the homological degree of the map, and the internal degree of the map?
randomComplexMap = method(Options=>{
        Degree => 0,
        InternalDegree => null,
        Cycle => false,
        Boundary => false
        }) -- should this overload 'random'?  Probably.
randomComplexMap(Complex, Complex) := ComplexMap => o -> (D,C) -> (
    deg := o.Degree;
    E := Hom(C,D);
    S := ring E;
    ideg := if o.InternalDegree === null then degree 1_S else o.InternalDegree;
    G := if o.Boundary then image dd^E_(deg+1)
         else if o.Cycle then ker dd^E_deg
         else E_deg;
    B := basis(ideg, G);
    g := B * random(source B, S^{-ideg});
    if o.Boundary then (
        g = map(E_deg, G, gens G) * g
        )
    else if o.Cycle then (
        g = inducedMap(E_deg,G) * g
        );
    homomorphism(deg, g, E)
    )

isNullHomotopyOf = method()
isNullHomotopic = method()
nullHomotopy = method() -- this function attempts to construct one, might fail

isNullHomotopyOf(ComplexMap, ComplexMap) := (h, f) -> (
    -- returns true if h is a null homotopy for f : C --> D.
    -- if debugLevel > 0, then more info as to where it is not, is given
    C := source f;
    D := target f;
    if debugLevel == 0 then h * dd^C + dd^D * h == f
    else (
        result := true;
        deg := degree h;
        (lo,hi) := concentration h;
        for i from lo to hi do (
            if h_(i-1) * dd^C_i + dd^D_(deg+i) * h_i != f_i then (
                << "fails to be a null homotopy at location " << i << endl;
                result = false;
                );
            );
        result
        )
    )

-- TODO: we are keeping this version so that we may compare the 
--   more general version with this version, at a later date.
--   WARNING: this code is not used: it is written over 2 functions below.
nullHomotopy(ComplexMap,Thing) := (f,notused) -> (
    -- key assumption: 'source f' is a complex of free modules
    C := source f;
    D := target f;
    deg := degree f + 1;
    hs := new MutableHashTable;
    (lo,hi) := concentration f;
    for i from lo to hi do (
        if hs#?(i-1) then ( 
            rem := (f_i - hs#(i-1) * dd^C_i) % (dd^D_(i+deg));
            if rem != 0 then error "can't construct homotopy";
            hs#i = (f_i - hs#(i-1) * dd^C_i) // (dd^D_(i+deg))
            )
        else (
            rem = f_i % dd^D_(i+deg);
            if rem != 0 then error "can't construct homotopy";
            hs#i = f_i // dd^D_(i+deg)
            )
        );
    -- result is a ComplexMap h : C --> D, of degree degree(f)+1
    map(D, C, new HashTable from hs, Degree => deg)
    )

isNullHomotopic ComplexMap := Boolean => f -> (
    g := homomorphism' f;
    H := target g; 
    d := degree f;
    g1 := g_d // dd^H_(d+1); 
    g_d == dd^H_(d+1) * g1
    )

nullHomotopy ComplexMap := ComplexMap => f -> (
    g := homomorphism' f;
    H := target g; 
    d := degree f;
    g1 := g_d // dd^H_(d+1);
    homomorphism(d+1,g1,H)
    )
--------------------------------
-- Standard maps of complexes --
--------------------------------
tensorAssociativity(Complex,Complex,Complex) := (A,B,C) -> (
    -- implements the isomorphism A ** (B ** C) --> (A ** B) ** C
    AB := A ** B;
    BC := B ** C;
    E := A ** BC; -- source
    F := AB ** C; -- target
    (lo,hi) := concentration E;
    maps := new HashTable from for i from lo to hi list i => (
        -- want the map E_i --> F_i
        matrix for ab'c in indices F_i list
                for a'bc in indices E_i list (
                    a := a'bc#0;
                    b := a'bc#1 - ab'c#1;
                    c := ab'c#1;
                    bc := a'bc#1;
                    ab := ab'c#0;
                    if A_a != 0 and B_b != 0 and C_c != 0 then (
                        ((AB_ab)_[{a,b}] ** C_c)
                        * tensorAssociativity(A_a, B_b, C_c)
                        * (A_a ** (BC_bc)^[{b,c}])
                        )
                    else (
                        map(F_i.cache.components#(F_i.cache.indexComponents#{ab,c}),
                            E_i.cache.components#(E_i.cache.indexComponents#{a,bc}),
                            0)
                        )
                    )
        );
    map(F,E,maps)
    )

isDirectSum Complex := (C) -> C.cache.?components

nextLambda = method()
nextLambda ComplexMap := ComplexMap => (lambda) -> (
    C := target lambda;
    L0 := source lambda;
    (lo,hi) := concentration L0;
    D := cone naiveTruncation(lambda, (hi,hi+2), (hi-1, hi));
    HC1 := HH_(hi+1) D;
    pHC1 := prune HC1;
    if pHC1 == 0 then return null;
    a1 := inducedMap(pHC1, cover pHC1);
    a2 := pHC1.cache.pruningMap;
    g1 := map(D_(hi+1), source gens HC1, (gens HC1) // (gens D_(hi+1)));
    g2 := map(HC1, source gens HC1, 1);
    h := g1 * ((a2 * a1)//g2);
    L1 := complex(append(for i from lo+1 to hi list dd^L0_i, h^[0]), Base=>lo);
    map(C,L1,i -> if i === hi+1 then -h^[1] else lambda_i)
    )

resolutionMap = method(Options => options freeResolution)
resolutionMap Complex := opts -> C -> (
    if opts.LengthLimit < 0 then error "expected a non-negative value for LengthLimit";
    if not C.cache.?resolutionMap
      or C.cache.resolutionMap.cache.LengthLimit < opts.LengthLimit then (
        (lo,hi) := concentration C;
        local f;
        lengthlimit := defaultLengthLimit(ring C, length C, opts.LengthLimit);
        if lo === hi then (
            -- if C has only one nonzero module, use the faster free resolution code
            -- which is also important for Yoneda ext.
            FC := complex(freeResolution(C_lo, LengthLimit=>opts.LengthLimit), Base=>lo);
            f = map(C, FC, i -> if i === lo then map(C_lo, FC_lo, 1) else map(C_i, FC_i, 0));
            )
        else (
            len := 0;
            f = map(C, complex((ring C)^0, Base=>lo-1), 0);
            local g;
            -- how to implement length limit here.  What does length limit mean?
            while (
                g = nextLambda f;
                g =!= null and len <= lengthlimit
                ) do (
                f = g;
                len = len+1;
                );
            -- the following line removes a 0 in the lo-1 spot, which is a byproduct
            -- of the base case above.
            f = naiveTruncation(f,(lo,infinity));
            );
        f.cache.LengthLimit = if length source f < lengthlimit then infinity else lengthlimit;
        C.cache.resolutionMap = f;
        );
    fC := C.cache.resolutionMap;
    if opts.LengthLimit < length source fC
    then naiveTruncation(fC, (0, opts.LengthLimit))
    else fC
    )

resolution Complex := opts -> C -> source resolutionMap(C, opts)

liftMapAlongQuasiIsomorphism = method()
liftMapAlongQuasiIsomorphism(ComplexMap, ComplexMap) := (alpha,beta) -> (
    -- alpha: P --> N, P is semi-free
    -- beta: M --> N, a quasi-isomorphism
    -- result: gamma: P --> M a morphism of chain complexes
    --         h:     P --> N, a homomorphism of degree 1, such that
    -- alpha - beta.gamma = h dd^P + dd^N h.
    P := source alpha;
    N := target alpha;
    M := source beta;
    if N =!= target beta then error "expected targets of two maps to be the same complex";
    (loP, hiP) := concentration P;
    Cbeta := cone beta;
    gamma := new MutableHashTable;
    h := new MutableHashTable;
    -- start with lo-1
    gamma#(loP-1) = map(M_(loP-1), P_(loP-1), 0);
    h#(loP-1) = map(N_loP, P_(loP-1), 0);
    -- once we have defined: gamma_(i-1) : P_(i-1) --> M_(i-1)
    -- and h_(i-1) : P_(i-1) --> N_i
    -- get a map P_i --> M_(i-1) ++ N_i
    for i from loP to hiP do (
        delta := map(Cbeta_i, P_i, matrix{{- gamma#(i-1) * dd^P_i},
                                        {alpha_i - h#(i-1) * dd^P_i}}
                     );
        --if delta % dd^Cbeta_(i+1) != 0 then << "oops, our logic is wrong!" << endl;
        -- note: if Cbeta_(i+1) is 0, then it is not a direct sum module.  We make it one:
        eps := map(M_i ++ N_(i+1), P_i, delta // dd^Cbeta_(i+1));
        gamma#i = map(M_i, P_i, eps^[0]);
        h#i = map(N_(i+1), P_i, eps^[1]);
        );
    gamma = hashTable for i from loP to hiP list i => gamma#i;
    h = hashTable for i from loP to hiP list i => h#i;
    gamma1 := map(M, P, gamma);
    gamma1.cache.homotopy = map(N, P, h, Degree => 1);
    gamma1
    )

-- AAAA

------------------------------------------------
-- Minimization of a complex -------------------
-- adapted from ChainComplexExtras.m2 ----------
------------------------------------------------
-- TODO: get this to work over fields, poly rings, quotients, and also the local case.
--       improve the performance of this function
minimize = method ()
minimize Complex := C -> (
    if not isFree C then error "expected a complex of free modules";
    (lo,hi) := concentration C;
    S := ring C;
    ev0 := map(S, S, for x in gens S list 0_S);
    rho := hashTable for i from lo to hi+1 list i => (
        g := syz ev0 dd^C_i;
        id_(C_i) - g * (id_(target g)//g)
        );
    maps := hashTable for i from lo to hi list i => rho#i | C.dd_(i+1)*rho#(i+1);
    C' := coker map(C, C ++ C[1], maps, Degree=>0);
    D := prune C';  -- TODO: this appears to be overkill.  write a specialized prune
                      -- that works in this specific case, where the result will consist of
                      -- free modules.
    phi := map(D, C, i -> (D_i.cache.pruningMap)^(-1) * inducedMap(C'_i, C_i));
    D.cache.minimizingMap = phi;
    D
    )

--------------
-- Ext data --
--------------
-- WARNING: this function replaces the one in m2/ext.m2
Ext(ZZ, Module, Module) := Module => opts -> (i,M,N) -> (
    H := null; -- result
    liftmap := null; -- given f : R^1 --> H, returns g : R^1 --> Hom(FM_i, N)
    invmap := null; -- given g : R^1 --> Hom(FM_i, N), returns f : R^1 --> H = Ext^i(M,N)
    Y := youngest(M.cache.cache,N.cache.cache);
    if not Y#?(Ext,i,M,N) then Y#(Ext,i,M,N) = (
        R := ring M;
        if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
        if R =!= ring N then error "expected modules over the same ring";
        if i < 0 then (
            H = R^0;
            liftmap = (f) -> map(Hom(R^0,N), source f, 0);
            invmap = (g) -> map(H, source g, 0);
            )
        else if i === 0 then (
            H = Hom(M,N);
            liftmap = (f) -> Hom(map(M, cover M, 1), N) * f;
            invmap = (g) -> (
                h := Hom(map(M, cover M, 1), N);
                g // h
                );
            )
        else (
            FM := freeResolution(M, LengthLimit => i+1);
            b := dd^FM;
            g0 := Hom(b_i, N);
            g1 := Hom(b_(i+1), N); -- Hom(FM_i, N) is source g1 == target g0.
            kerg1 := ker g1; 
            H = kerg1 / (image g0); 
            -- note: we compute H like this in order to have access to ker g1.
            -- using "H = homology(g1, g0)" does not provide us with ker g1.
            liftmap = (f) -> (
                h := ((generators H) * (matrix f)) // generators source g1;
                map(source g1, source f, h)
                );
            invmap = (g) -> (
                -- given g : R^1 --> Hom(FM_i, N)
                -- given h : ker g1 --> Hom(FM_i, N) inclusion map
                -- note g1: Hom(FM_i, N) --> Hom(FM_(i+1), N)
                -- output: R^1 --> H = ker g1 / image g0
                h := inducedMap(target g0, kerg1);
                f1 := g // h;
                map(H, source f1, f1)
                );
            );
        H.cache.yonedaExtension = liftmap;
        H.cache.yonedaExtension' = invmap;
        H.cache.Ext = (i,M,N);
        H
        );
    Y#(Ext,i,M,N)
    )

yonedaExtension = method()
yonedaExtension Matrix := Complex => f -> (
    -- f: R^1 --> Ext^d(M,N) = E
    -- construct the chain complex:
    --   0 <-- M <-- FM_0 <-- FM_1 <-- ... <-- FM_(d-2) <-- P <-- N <-- 0
    -- where P = pushout of g:FM_d --> N and FM_d --> FM_(d-1).
    -- P = coker(dd^FM_d || g)
    if not isFreeModule source f
    or not rank source f == 1 then error "expected source of map to be free of rank 1";
    E := target f;
    if not E.?cache or not E.cache.?Ext then 
      error "expected target of map to be an Ext^d(M,N) module";
    (d,M,N) := E.cache.Ext;
    FM := freeResolution(M, LengthLimit => d+1); -- WARNING: need it to match computation from Ext^d(M,N)...
    g := homomorphism E.cache.yonedaExtension f; -- g: FM_d --> N
    if d <= 0 then error "Yoneda extension only defined for Ext^d module for d at least 1";
    h := dd^FM_d || g;
    P := coker h; -- FM_d --> FM_(d-1) ++ N --> P --> 0
    D := target h; -- direct sum
    delta0 := map(P,D,id_D) * D_[1]; -- N --> P
    if d === 1 then (
        complex {map(M, P, D^[0]), delta0}
        )
    else (
        -- the signs of dd^FM must be negated since we have shifted the resolution FM by 1
        delta1 := -dd^FM_(d-1) * map(FM_(d-1), P, D^[0]);
        complex join({map(M,FM_0,id_(FM_0))},
            for i from 1 to d-2 list -dd^FM_i,
            {delta1, delta0}
            )
        )
    )

yonedaExtension' = method()
yonedaExtension' Complex := Matrix => C -> (
    -- given an exact complex of R-modules of the form
    --  0 <-- M <-- C0 <-- C1 <-- ... <-- C(d-1) <-- N <-- 0
    -- return the corresponding map R^1 --> Ext^d(M,N).
    -- 
    (lo,hi) := concentration C;
    M := complex(C_lo, Base=>lo);
    -- notice that D is a shifted complex which changes the sign of the differential
    D := naiveTruncation(C, (lo+1,hi))[1];
    s := map(M,D,i -> if i == lo then dd^C_(lo+1) else map(M_i, D_i, 0));
    g := resolutionMap M;
    sinverse := liftMapAlongQuasiIsomorphism(g, s);
    yonedaMap := sinverse_(hi-1);  -- map FM_d --> N
    extd := Ext^(hi-lo-1)(C_lo, C_hi);
    extd.cache.yonedaExtension' homomorphism' yonedaMap
    )

yonedaMap = method(Options => {LengthLimit => infinity})
yonedaMap Matrix := ComplexMap => opts -> f -> (
    -- f: R^1 --> Ext^d(M,N) = E
    -- construct the chain complex map Ff : FM --> FN of degree -d.
    if not isFreeModule source f
    or not rank source f == 1 then error "expected source of map to be free of rank 1";
    E := target f;
    if not E.?cache or not E.cache.?Ext then 
      error "expected target of map to be an Ext^d(M,N) module";
    (d,M,N) := E.cache.Ext;
    FM := freeResolution(M, LengthLimit => opts.LengthLimit); -- WARNING: need it to match computation from Ext^d(M,N)...
    FN := freeResolution(N, LengthLimit => opts.LengthLimit - d);
    g := homomorphism E.cache.yonedaExtension f; -- g: FM_d --> N
    g0 := map(FN_0, FM_d, g);
    extend(FN, FM, g0, -d)
    )

yonedaMap' = method(Options => {LengthLimit => infinity})
yonedaMap' ComplexMap := Matrix => opts -> f -> (
    -- given a map f : FM --> FN of degree -d, construct the corresponding element
    -- R^1 --> Ext^d(M,N), which is unique up to homotopy.
    -- We only need part of the ComplexMap.
    --  In fact, we need just the map FM_d --> FN_0, and the fact that it is a complex map.
    FM := source f;
    FN := target f;
    d := - degree f;
    -- check: FM, FN are free acyclic complexes
    M := if FM.cache.?Module then FM.cache.Module else error "expected a free resolution of a module";
    N := if FN.cache.?Module then FN.cache.Module else error "expected a free resolution of a module";
    extd := Ext^d(M, N);
    g := map(N, FM_d, f_d);
    extd.cache.yonedaExtension' homomorphism' g
    )

yonedaProduct = method()
yonedaProduct(Matrix, Matrix) := Matrix => (a,b) -> (
    -- CCC
    -- Ext(B,C) ** Ext(A,B) --> Ext(A,C)
    -- a: R^1 --> Ext^d(A,B)
    -- b: R^1 --> Ext^e(B,C)
    -- E:  0 <-- A <-- FA0 <-- FA1 <-- ... <-- FA(d-2) <-- EA <-- B <-- 0
    -- F:  0 <-- B <-- FB0 <-- FB1 <-- ... <-- FB(e-2) <-- EB <-- C <-- 0
    if not (target a).?cache or not (target a).cache.?Ext then 
      error "expected target of map to be an Ext^d(M,N) module";
    if not (target b).?cache or not (target b).cache.?Ext then 
      error "expected target of map to be an Ext^d(M,N) module";
    (d,A,B) := (target a).cache.Ext;
    (e,B1,C) := (target b).cache.Ext;
    if B != B1 then error "expected composable Ext modules";
    fa := yonedaMap(a, LengthLimit=>d+e);
    fb := yonedaMap(b, LengthLimit=>e);
    yonedaMap'(fb * fa)
    )
yonedaProduct(Module, Module) := Matrix => (E,F) -> (
    if not E.?cache or not E.cache.?Ext then 
      error "expected module to be an Ext^d(M,N) module";
    if not F.?cache or not F.cache.?Ext then 
      error "expected module to be an Ext^d(M,N) module";
    (d,A,B) := E.cache.Ext;
    (e,B1,C) := F.cache.Ext;
    if B != B1 then error "expected composable Ext modules";
    EF := Ext^(d+e)(A,C);
    elems := flatten for i from 0 to numgens E-1 list 
      for j from 0 to numgens F-1 list (
          yonedaProduct(E_{i}, F_{j})
          );
    map(EF, E ** F, matrix {elems})
    )

-*
-- Ext over a complete intersection (code/method of Dan Grayson and Lucho Avramov).
Ext(Module,Module) := Module => (M,N) -> (
  cacheModule := M; -- we have no way to tell whether N is younger than M, sigh
  cacheKey := (Ext,M,N);
  if cacheModule.cache#?cacheKey then return cacheModule.cache#cacheKey;
  B := ring M;
  if B =!= ring N
  then error "expected modules over the same ring";
  if not isCommutative B
  then error "'Ext' not implemented yet for noncommutative rings.";
  if not isHomogeneous B
  then error "'Ext' received modules over an inhomogeneous ring";
  if not isHomogeneous N or not isHomogeneous M
  then error "'Ext' received an inhomogeneous module";
  if N == 0 or M == 0 then return cacheModule.cache#cacheKey = B^0;
  p := presentation B;
  A := ring p;
  I := ideal mingens ideal p;
  n := numgens A;
  c := numgens I;
  if c =!= codim B 
  then error "total Ext available only for complete intersections";
  f := I_*;
  pM := lift(presentation M,A);
  pN := lift(presentation N,A);
  M' := cokernel ( pM | p ** id_(target pM) );
  N' := cokernel ( pN | p ** id_(target pN) );
  assert isHomogeneous M';
  assert isHomogeneous N';
  C := complete resolution M';
  X := getSymbol "X";
  K := coefficientRing A;
  S := K(monoid [X_1 .. X_c, toSequence A.generatorSymbols,
    Degrees => {
      apply(0 .. c-1, i -> prepend(-2, - degree f_i)),
      apply(0 .. n-1, j -> prepend( 0,   degree A_j))
      }]);
  -- make a monoid whose monomials can be used as indices
  Rmon := monoid [X_1 .. X_c,Degrees=>{c:{2}}];
  -- make group ring, so 'basis' can enumerate the monomials
  R := K Rmon;
  -- make a hash table to store the blocks of the matrix
  blks := new MutableHashTable;
  blks#(exponents 1_Rmon) = C.dd;
  scan(0 .. c-1, i -> 
       blks#(exponents Rmon_i) = nullhomotopy (- f_i*id_C));
  -- a helper function to list the factorizations of a monomial
  factorizations := (gamma) -> (
    -- Input: gamma is the list of exponents for a monomial
    -- Return a list of pairs of lists of exponents showing the
    -- possible factorizations of gamma.
    if gamma === {} then { ({}, {}) }
    else (
      i := gamma#-1;
      splice apply(factorizations drop(gamma,-1), 
	(alpha,beta) -> apply (0..i, 
	     j -> (append(alpha,j), append(beta,i-j))))));
  scan(4 .. length C + 1, 
    d -> if even d then (
      scan( flatten \ exponents \ leadMonomial \ first entries basis(d,R), 
	gamma -> (
	  s := - sum(factorizations gamma,
	    (alpha,beta) -> (
	      if blks#?alpha and blks#?beta
	      then blks#alpha * blks#beta
	      else 0));
	  -- compute and save the nonzero nullhomotopies
	  if s != 0 then blks#gamma = nullhomotopy s;
	  ))));
  -- make a free module whose basis elements have the right degrees
  spots := C -> sort select(keys C, i -> class i === ZZ);
  Cstar := S^(apply(spots C,
      i -> toSequence apply(degrees C_i, d -> prepend(i,d))));
  -- assemble the matrix from its blocks.
  -- We omit the sign (-1)^(n+1) which would ordinarily be used,
  -- which does not affect the homology.
  toS := map(S,A,apply(toList(c .. c+n-1), i -> S_i),
    DegreeMap => prepend_0);
  Delta := map(Cstar, Cstar, 
    transpose sum(keys blks, m -> S_m * toS sum blks#m),
    Degree => { -1, degreeLength A:0 });
  DeltaBar := Delta ** (toS ** N');
  if debugLevel > 10 then (
       assert isHomogeneous DeltaBar;
       assert(DeltaBar * DeltaBar == 0);
       stderr << describe ring DeltaBar <<endl;
       stderr << toExternalString DeltaBar << endl;
       );
  -- now compute the total Ext as a single homology module
  tot := minimalPresentation homology(DeltaBar,DeltaBar);
  cacheModule.cache#cacheKey = tot;
  tot)

Ext(Module, Ring) := (M,R) -> Ext(M,R^1)
Ext(Module, Ideal) := (M,J) -> Ext(M,module J)
Ext(Ideal, Ring) := (I,R) -> Ext(module I,R^1)
Ext(Ideal, Ideal) := (I,J) -> Ext(module I,module J)
Ext(Ideal, Module) := (I,N) -> Ext(module I,N)
*-

---------------------
-- Connecting maps --
---------------------
-- BBB
connectingMap = method()
connectingMap(ComplexMap, ComplexMap) := ComplexMap => (g, f) -> (
    -- 0 <-- C <--g-- B <--f-- A <-- 0
    if debugLevel > 0 and not isShortExactSequence(g, f) then
        error "expected a short exact sequence of complexes";
    cylf := cylinder f;
    cf := cone f;
    alpha := canonicalMap(cylf, source f);
    beta := canonicalMap(cf, cylf);
    -- p is a quasi-isomorphism cone(f) --> C.
    p := map(target g, cf, {{ 
                map(target g, source f[-1], 0), g
                }});
    -- q is cone(f) --> A[-1]
    q := map(source f[-1], cf, {{ 
                id_(source f[-1]), map(source f[-1], source g, 0)
                }});
    if debugLevel > 1 then (
        assert isWellDefined p;
        assert isWellDefined q;
        );
    HH(q) * (HH(p))^-1
    )


augmentationMap = method()
augmentationMap Complex := ComplexMap => C -> (
    if not C.cache.?Module then error "expected a free resolution";
    M := C.cache.Module;
    map(complex M, C, i -> if i === 0 then map(M, C_0, 1))
    )

horseshoeResolution = method(Options => {LengthLimit=>infinity})
horseshoeResolution Complex := Sequence => opts -> ses -> (
    -- check that ses is a short exact sequence of modules
    -- occuring in homological degrees 0,1,2.
    -- at least check that the length is correct.
    f := yonedaExtension' ses;
    g := yonedaMap(f, LengthLimit => opts.LengthLimit);
    M := ses_0;
    N := ses_2;
    -- the following will have correct length, since 
    -- they have been constructed during yonedaMap.
    FM := freeResolution M;
    FN := freeResolution N;
    HS := complex hashTable for i from 1 to length FM list (
      i => map(FN_(i-1) ++ FM_(i-1), 
             FN_i ++ FM_i, 
             matrix{{dd^FN_i, g_i},{0,dd^FM_i}})
      );
    alpha := map(HS, FN, i -> map(HS_i, FN_i, (HS_i)_[0]));
    beta := map(FM, HS, i -> map(FM_i, HS_i, (HS_i)^[1]));
    (beta, alpha)
    )  

TEST ///
  debug Core
  R = ZZ/101[a..d]
  M = image vars R
  M1 = image matrix{{b,a,c,d}}
  M == M1
  N = R^{4:-1}
  f0 = map(R^1,, matrix{{a,b,c,d}})
  -- example 1: sources are identical, targets are same image module.
  f = inducedMap(M,source f0,f0)
  g = inducedMap(M1,source f0,f0)
  assert(source f === source g)
  assert(source f == source g)
  assert(raw super f === raw super g)
  assert(f == g)
  -- example 2: sources are identical, targets are same subquotient
  --  (but with different image parts).
  M = (image vars R)/(image matrix{{c,d}})
  M1 = (image matrix{{b,a,c,d}})/(image matrix{{d,c}})
  assert(M == M1)
  assert(M =!= M1)
  f = inducedMap(M,source f0,f0)
  g = inducedMap(M1,source f0,f0)
  assert(source f === source g)
  assert(source f == source g)
  assert(raw super f === raw super g)
  assert(f == g)
  -- next case: sources are the same image, but with different gen order
  M = image vars R  
  N = image matrix {{a,b,c,d}}
  N1 = image matrix {{c,a,b,d}}
  f = map(R^1, N, {{a,b,c,d}})
  g = map(R^1, N1, {{c,a,b,d}})
  assert isWellDefined f
  assert isWellDefined g
  assert(f == g)
///

part(List, Complex) := Complex => (deg, C) -> (
    -- return a Complex over the coefficient ring
    R := ring C;
    A := coefficientRing R;
    psi := map(A,R, DegreeMap => degR -> take(degR, - degreeLength A));
    (lo, hi) := concentration C;
    if lo === hi 
    then complex(psi source basis(deg, C_lo), Base => lo)
    else (
        maps := hashTable for i from lo+1 to hi list (
            f := psi matrix basis(deg, dd^C_i);
            if source f == 0 then continue else i => f
            );
        if # keys maps === 0 then complex(psi source basis(deg, C_lo), Base => lo)  else complex maps
        )
    )
part(List, ComplexMap) := ComplexMap => (deg, f) -> (
    R := ring f;
    A := coefficientRing R;
    psi := map(A,R, DegreeMap => degR -> take(degR, - degreeLength A));
    C := part(deg, source f);
    D := part(deg, target f);
    map(D, C, i -> map(D_i, C_i, psi matrix basis(deg, f_i)))
    )
part(ZZ, Complex) := Complex => (deg, C) -> part({deg}, C)
part(ZZ, ComplexMap) := ComplexMap => (deg, f) -> part({deg}, f)

TEST ///
  -- example of computing part.
-*
  restart
  needsPackage "Complexes" 
*-
  kk = ZZ/32003
  S = kk[a..d]
  I = ideal"ab, ad, bc, c3"
  F = freeResolution comodule I
  assert(part(-10, F) == 0)
  assert isWellDefined part(-10, F)
  assert(part(-1, F) == 0)
  assert isWellDefined part(-1, F)
  assert (part(0, F) == complex kk^1)
  assert isWellDefined part(0, F)
  assert (part(1, F) == complex kk^4)
  assert isWellDefined part(1, F)
  assert (C = part(2, F); rank C_0 == 10 and rank C_1 == 3)
  assert isWellDefined part(2, F)
  assert(ring part(2, F) === kk)

  assert (C = part(6, F); 
      (for i from 0 to length C list rank C_i) == {84, 125, 54, 1})
  assert isWellDefined part(6, F)

  kk = ZZ/32003[s,t]
  S = kk[a..d]
  psi = map(kk, S)
  I = ideal"sab, tad, (s-t)bc, tc3"

  isHomogeneous I
  F = freeResolution comodule I

  assert(part(-10, F) == 0)
  assert isWellDefined part(-10, F)
  assert(part(-1, F) == 0)
  assert isWellDefined part(-1, F)
  assert (part(0, F) == complex kk^1)
  assert isHomogeneous part(0, F)
  assert isWellDefined part(0, F)
  assert (part(1, F) == complex kk^4)
  assert isHomogeneous part(1, F) 
  assert isWellDefined part(1, F)
  assert (C = part(2, F); rank C_0 == 10 and rank C_1 == 3)
  assert isHomogeneous part(2, F)
  assert isWellDefined part(2, F)
  assert (C = part(6, F); 
      (for i from 0 to length C list rank C_i) == {84, 125, 68, 15})
  assert isHomogeneous part(6, F)
  assert isWellDefined part(6, F)
  assert(ring part(6, F) === kk)

  C = part(2, F)
  dd^C
  isHomogeneous dd^C_1 -- want this to be true.
///

TEST ///  
-*
  restart
  needsPackage "Complexes"
*-
  -- multi-graded example of part(d, C)
  needsPackage "NormalToricVarieties"
  kk = ZZ/32003
  X = hirzebruchSurface(6, CoefficientRing => kk)
  S = ring X
  F = random({-4,2}, S)
  I = ideal jacobian ideal F
  C = freeResolution I
  Cd = part({1,1}, C)
  assert isWellDefined Cd
  assert(ring Cd === coefficientRing S)
  
  Cd = part({-1,3}, C)
  assert isWellDefined Cd
  assert(prune HH Cd == 0)
  
  J = I + ideal X
  CJ = freeResolution J
  f = randomComplexMap(CJ, C, Cycle => true)
  fd = part({1,1}, f)
  assert isWellDefined fd
  assert(ring fd === coefficientRing S)
  assert isComplexMorphism fd
  prune HH fd
///

TEST ///
-*
  restart
  needsPackage "Complexes" 
*-
  kk = ZZ/32003
  S = kk[a..d]
  I = ideal"ab, ad, bc, c3"
  J = I + ideal"c2"
  FI = freeResolution comodule I
  FJ = freeResolution comodule J
  H = Hom(FI, FJ)
  f = randomComplexMap(FJ, FI, Cycle => true)
  assert isWellDefined f
  assert isCommutative f
  assert isComplexMorphism f

  f2 = part(2, f)
  assert(ring f2 === kk) 
  assert isWellDefined f2
  assert isComplexMorphism f2

  f5 = part(5, f);
  assert(ring f5 === kk) 
  assert isWellDefined f5
  assert isComplexMorphism f5
///

chainComplex Complex := ChainComplex => (cacheValue symbol ChainComplex) (C -> (
    (lo,hi) := concentration C;
    D := new ChainComplex;
    D.ring = ring C;
    for i from lo to hi do D#i = C_i;
    for i from lo+1 to hi do D.dd#i = dd^C_i;
    D
    ))

complex ChainComplex := Complex => opts -> (cacheValue symbol Complex)(D -> (
    (lo,hi) := (min D, max D);
    while lo < hi and (D_lo).numgens == 0 do lo = lo+1;
    while lo < hi and (D_hi).numgens == 0 do hi = hi-1;
    if lo === hi then
        complex D_lo
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

complex ChainComplexMap := ComplexMap => opts -> g -> (
    map(complex target g, complex source g, i -> g_i, Degree => degree g)
    )

///
  restart
  needsPackage "Complexes"
  R = ZZ/101[a..d]
  D = res coker vars R
  assert isWellDefined complex D
  C = freeResolution coker vars R
  assert(complex D == C)
  assert(complex D === complex D)
  assert(concentration complex D == (0,4))
  assert(concentration C == concentration complex D)
  assert isWellDefined complex(D.dd)
    
  f = complex D.dd    
  source f 
  target f
  g = chainComplex complex D.dd
  assert(g == D.dd)
  h = complex g
  assert(f == h)
  
  D = new ChainComplex
  D.ring = R;
  D#-3 = R^0
  D#-2 = R^2
  D#-1 = R^1
  D#4 = R^2
  D#5 = R^0
  D.dd_-1
  C = complex D
  assert isWellDefined C
  assert(chainComplex C == D)
///

beginDocumentation()

undocumented{
    (net, Complex),
    (net, ComplexMap),
    (component,Module,Thing),
    component
    }

doc ///
    Key
        Complexes
    Headline
        development package for beta testing new version of chain complexes    
    Description
        Text
            This package develops new data types and routines for homological algebra.
            Eventually, it will replace the current facilities for homological algebra.
            We are making this available in order to get feedback from users before
            making this change.  Please email the authors with any and all comments or
            suggestions.
        Text
            The major change is replacing the @TO ChainComplex@ data type with @TO Complex@.
            The internal structure of this new data type is somewhat different, but more
            importantly, it has a richer set of constructors.  Use the functions
            @TO (complex, ChainComplex)@, @TO (complex, ChainComplexMap)@, @TO (chainComplex, Complex)@, @TO (chainComplex, ComplexMap)@, 
            to translate between these representations.
        Text
            The overarching goal is to make all of the homological algebra routines functorial.
            For instance, we have @TO2(canonicalMap, "canonical maps")@ associated to kernels,
            cokernels, images, coimages, cones, and cylinders.
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Making chain complexes"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (complex, HashTable),
                TO (complex, List),
                TO (complex, Module), 
                TO (complex, Complex),
                TO (symbol SPACE, Complex, Array),
                TO (isWellDefined, Complex)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new complexes"@
	Text
    	    @UL {
                TO (freeResolution, Module),
                TO (resolution, Complex),
                TO (homology, Complex)
            }@
    	Text
    	    @SUBSECTION "More advanced constructors"@
	Text
    	    @UL {
                TO (symbol++, Complex, Complex),
                TO (symbol**, Complex, Complex),
                TO (Hom, Complex, Complex),
                TO (dual, Complex),
                TO (symbol SPACE, RingMap, Complex),
                TO (symbol **, RingMap, Complex),
                TO (naiveTruncation, Complex, ZZ, ZZ),
                TO (canonicalTruncation, Complex, ZZ, ZZ),
                TO (minimalPresentation, Complex),
                TO (minimize, Complex),
                TO (gradedModule, Complex),
                TO (part, List, Complex),
                TO (yonedaExtension, Matrix)
            }@
    	Text
    	    @SUBSECTION "Extracting complexes from complex maps"@
        Text
    	    @UL {
                TO (source, ComplexMap),
                TO (target, ComplexMap),
                TO (kernel, ComplexMap),
                TO (cokernel, ComplexMap),
                TO (image, ComplexMap),
                TO (coimage, ComplexMap),
                TO (cone, ComplexMap),
                TO (cylinder, ComplexMap)
            }@
    SeeAlso
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Making maps between chain complexes"
    Headline
        information about the basic constructors
    Description
    	Text
    	    @SUBSECTION "Basic constructors"@
	Text
    	    @UL {
                TO (map, Complex, Complex, HashTable),
                TO (map, Complex, Complex, ZZ),
                TO (map, Complex, Complex, Function),
                TO (map, Complex, Complex, ComplexMap),
                TO (id, Complex),
                TO "differential of a chain complex",
                TO (inducedMap, Complex, Complex),
                TO (symbol SPACE, ComplexMap, Array),
                TO (isWellDefined, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Important computations creating new complex maps"@
        Text 
            @UL {
                TO (resolutionMap, Complex),
                TO (homology, ComplexMap),
                TO (augmentationMap, Complex),
                TO (extend, Complex, Complex, Matrix)
           }@
    	Text
    	    @SUBSECTION "Canonical maps between complexes"@
        Text
            Some complexes come with canonical maps.
            To access the complex map, 
            one uses @TO (canonicalMap, Complex, Complex)@.
            The following operations have associated canonical maps.
	Text
    	    @UL {
                TO (kernel, ComplexMap),
                TO (cokernel, ComplexMap),
                TO (image, ComplexMap),
                TO (coimage, ComplexMap),
                TO (cone, ComplexMap),
                TO (cylinder, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Random maps of chain complexes"@
        Text
            The method @TO (randomComplexMap, Complex, Complex)@
            allows one to construct random complex maps,
            random morphisms between complexes, and random
            null homotopies between complexes.
	Text
    	    @UL {
                TO (isCommutative, ComplexMap),
                TO (isComplexMorphism, ComplexMap),
                TO (isNullHomotopic, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Elementary operations on complex maps"@
        Text
    	    @UL {
                TO (symbol +, ComplexMap, ComplexMap),
                TO (symbol |, ComplexMap, ComplexMap),
                TO (symbol ||, ComplexMap, ComplexMap),
                TO (symbol ++, ComplexMap, ComplexMap),
                TO (symbol **, ComplexMap, ComplexMap),
                TO (Hom, ComplexMap, ComplexMap),
                TO (dual, ComplexMap),
                TO (symbol _, ComplexMap, Array),
                TO (symbol ^, ComplexMap, Array),
                TO (naiveTruncation, ComplexMap, Sequence),
                TO (canonicalTruncation, ComplexMap, Sequence),
                TO (symbol SPACE, RingMap, ComplexMap),
                TO (symbol **, RingMap, ComplexMap)
            }@
    SeeAlso
        "Making chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Basic invariants and properties"
    Headline
        information about accessing basic features
    Description
    	Text
    	    @SUBSECTION "Predicates for complexes and complex maps"@
        Text
    	    @UL {
                TO (isWellDefined, Complex),
                TO (isExact, Complex),
                TO (isHomogeneous, Complex),
                TO (isFree, Complex),
                TO (isWellDefined, ComplexMap),
                TO (isCommutative, ComplexMap),
                TO (isComplexMorphism, ComplexMap),
                TO (isShortExactSequence, ComplexMap, ComplexMap),
                TO (isQuasiIsomorphism, ComplexMap),
                TO (isNullHomotopic, ComplexMap),
                TO (isNullHomotopyOf, ComplexMap, ComplexMap)
            }@
    	Text
    	    @SUBSECTION "Other invariants for complexes"@
        Text
    	    @UL {
                TO (ring, Complex),
                TO (concentration, Complex),
                TO (max, Complex),
                TO (min, Complex),
                TO (length, Complex),
                TO (regularity, Complex),
                TO (betti, Complex),
                TO (poincare, Complex),
                TO (poincareN, Complex),
                TO (components, Complex)
            }@
    	Text
    	    @SUBSECTION "Other invariants for complex maps"@
        Text
    	    @UL {
                TO (ring, ComplexMap),
                TO (degree, ComplexMap),
                TO (concentration, ComplexMap),
                TO (components, ComplexMap),
                TO (isHomogeneous, ComplexMap)
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Working with Ext"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Working with Ext"
    Headline
        information about functorial properties
    Description
    	Text
    	    @SUBSECTION "Hom and Ext"@
        Text
    	    @UL {
                TO (Ext, ZZ, Module, Module),
                TO "(Ext, ZZ, Matrix, Matrix)",
                TO "(Ext, ZZ, Matrix, Module)",
                TO "(Ext, ZZ, Module, Matrix)",
                TO (Hom, Complex, Complex),
                TO (Hom, ComplexMap, ComplexMap),
                TO (homomorphism, ComplexMap),
                TO (homomorphism', ComplexMap)
            }@
        Text
    	    @SUBSECTION "Yoneda extensions and elements of Ext"@
        Text
    	    @UL {
                TO (yonedaMap, Matrix),
                TO (yonedaMap', ComplexMap),
                TO (yonedaExtension, Matrix),
                TO (yonedaExtension', Complex),
                TO (yonedaProduct, Matrix, Matrix),
                TO (yonedaProduct, Module, Module)
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Tor"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Working with Tor"
    Headline
        information about functorial properties
    Description
        Text
    	    @UL {
                TO (Tor, ZZ, Module, Module),
                TO "(Tor, ZZ, Module, Matrix)",
                TO "(Tor, ZZ, Matrix, Module)",
                TO "(Tor, ZZ, Matrix, Matrix)",
                TO (tensor, Complex, Complex),
                TO "(tensor, ComplexMap, ComplexMap)",
                TO (tensorAssociativity, Complex, Complex, Complex),
                TO "symmetry of Tor"
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Towards computing in the derived category"
///

doc ///
    Key
        "Towards computing in the derived category"
    Description
        Text
            @UL {
                TO (resolution, Complex),
                TO (resolutionMap, Complex),
                TO (isShortExactSequence, ComplexMap, ComplexMap),
                TO (isQuasiIsomorphism, ComplexMap),
                TO (liftMapAlongQuasiIsomorphism, ComplexMap, ComplexMap),
                TO (connectingMap, ComplexMap, ComplexMap),
                TO (horseshoeResolution, Complex)
            }@
    SeeAlso
        "Making chain complexes"
        "Making maps between chain complexes"
        "Basic invariants and properties"
        "Working with Ext"
        "Working with Tor"
///


doc ///
   Key
     Complex
   Headline
     the class of all chain complexes
   Description
    Text
      A complex is a sequence of objects $C_i$, connected by maps $dd^C_i : C_i \rightarrow C_{i-1}$
      such that the composition of any two consecutive maps is zero.
      
      TODO: more needs to be added here explaining how to used complexes.
///

doc ///
   Key
     "Making complexes"
   Description
    Text
      Here we have some cool examples
   SeeAlso
     Complexes
///

      
-- TODO: After we make more doc nodes, let's revisit this to add the programming details part.
doc ///
   Key
     (isWellDefined, Complex)
   Headline
     whether a complex is well-defined
   Usage
     isWellDefined C
   Inputs
     C:Complex
   Outputs
     :Boolean
       that is true when {\tt C} determines a well defined complex
   Description
    Text
      This routine checks that the differential of {\tt C} composes to zero.
      Additionally, it checks that the underlying data in {\tt C} is a properly formed
      Complex object in Macaulay2. If the variable {\tt debugLevel} is set to a value greater than zero,
      then information about the nature of any failure is displayed.
    Text

      As a first example, we construct by hand the free resolution of the twisted
      cubic.  One must work with maps rather than matrices, because the source and the target
      of adjacent maps must be the same (including degree information).
    Example
      R = QQ[a..d];
      f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
      f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
      C = complex {f0, f1}
      isWellDefined C
      dd^C
      (dd^C)^2
    Text
      The zero complex is well-defined.
    Example
      C = complex R^0
      isWellDefined C
    Text
    
      The next example demonstrates the case when the sequence maps do not compose to 0.
    Example
      g1 = map(source f0,, {{-d, c}, {c, b}, {b, a}})
      C = complex {f0, g1}
      isWellDefined C
      debugLevel = 1
      isWellDefined C
      (dd^C)^2
   SeeAlso
     (isWellDefined, ComplexMap)
     map
///

-* -- The following block of text doesn't validate anymore, and goes right after the last Example above this.
    Text
      @SUBSECTION "Programming Details"@
      The function also checks the data structure for the following:
      @UL {
          {"The keys are exactly ring, concentration, module, dd, cache"},
          }@
*-
doc ///
   Key
     (complex, Module)
     (complex, Ideal)
     (complex, Ring)
   Headline
     make a chain complex of length zero
   Usage
     complex M
   Inputs
     M:Module
       or @TO "Ideal"@, or @TO "Ring"@.
     Base => ZZ
       index for {\tt M}
   Outputs
     :Complex
       returns the complex whose 0-th component is {\tt M}.
   Description
    Text
      In contrast to @TO (complex,HashTable)@ and @TO (complex,List)@, this constructor
      provides a convenient method to construct a complex with
      only one non-zero component.
      
      We illustrate this with a free module.
    Example
      S = ZZ/101[a..d]
      C0 = complex S^2
      f = dd^C0
      source f, target f
      f == 0
      isWellDefined C0
      C0 == 0
      length C0
    Example
      C1 = complex(S^2, Base=>3)
      C1 == C0[-3]
      C1_3
      C1_0
    Text
      A ring or an ideal will be converted to a module first.
    Example
      C2 = complex S
      I = ideal(a^2-b, c^3)
      C3 = complex I
      C4 = complex (S/I)
      (ring C3, ring C4)
    Text
      The zero complex over a ring {\tt S} is most conveniently
      created by giving the zero module.
    Example
      C5 = complex S^0
      C5 == 0
      dd^C5 == 0
      C5_0
   SeeAlso
     complex
///

doc ///
   Key
     (complex, HashTable)
   Headline
     make a chain complex
   Usage
     complex H
   Inputs
     H:HashTable
       each key is an integer indexing a differential, and the 
         value at that key is the map
     Base => ZZ
       ignored when the input is a hash table
   Outputs
     :Complex
   Description
    Text
      A complex is a sequence of objects (e.g. modules), connected by
      maps called differentials.  The composition of any two
      consecutive maps is zero.
      
      The same data type is used for both chain and cochain complexes.
      If {\tt C} is a complex, then we have {\tt C^i = C_{-i}}.

      We construct the Koszul complex on the generators for the ideal
      of the twisted cubic curve.          
    Example
      S = ZZ/101[a..d]
      I = ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      F1 = map(S^1,,matrix{{I_0, I_1, I_2}})
      F2 = map(source F1,,matrix{
          {0, I_2, -I_1},
          {-I_2, 0, I_0},
          {I_1, -I_0, 0}
          })
      F3 = map(source F2,,matrix{{I_0}, {I_1}, {I_2}})
      C = complex hashTable{1 => F1, 2 => F2, 3 => F3}
      isWellDefined C
    Text
      This is the primary constructor used by all of the more user friendly
      methods for constructing a chain complex.
   Caveat
      This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, Complex)@.
   SeeAlso
     (isWellDefined, Complex)
     (complex, List)
     (complex, Module)
///

doc ///
   Key
     complex
     (complex, List)
     Base
   Headline
     make a chain complex
   Usage
     complex L
   Inputs
     L:List
       of maps
     Base => ZZ
         the index of the target of the first map 
         in the differential.
   Outputs
     :Complex
   Description
    Text
      A complex is a sequence of objects (e.g. modules), connected by
      maps called differentials.  The composition of any two
      consecutive maps is zero.
      
      The same data type is used for both chain and cochain complexes.
      If {\tt C} is a complex, then we have {\tt C^i = C_{-i}}.

      Often, a complex is most easily described by giving a list of
      consecutive maps which form the differential.  

      We construct the Koszul complex on the generators for the ideal
      of the twisted cubic curve.
    Example
      S = ZZ/101[a..d]
      I = ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      F1 = map(S^1,,matrix{{I_0, I_1, I_2}})
      F2 = map(source F1,,matrix{
          {0, I_2, -I_1},
          {-I_2, 0, I_0},
          {I_1, -I_0, 0}
          })
      F3 = map(source F2,,matrix{{I_0}, {I_1}, {I_2}})
      C = complex {F1, F2, F3}
      isWellDefined C
    Text
      To start a complex at a base different from zero, use the optional
      argument {\tt Base}.
    Example
      C1 = complex({F1, F2, F3}, Base => 1)
      isWellDefined C1
    Text
      Notice that this changes the homological degrees of the maps,
      but is not the same as the shift of the complex (which for odd shifts
      negates the maps).
    Example
      dd^C1
      dd^(C[-1])
    Text
      Having constructed this complex, we can access individual
      terms and maps.
    Example
      C_2
      C^(-1)
      C^(-1) == C_1
      C_7
      dd^C
      dd^C_2
      length C
    Text
      By computing the homology of this complex, we 
      see that these generators do not form a
      regular sequence, because $H_1(C)$ is non-zero.
    Example
      HH C
      prune HH C
      prune HH_1 C
   Caveat
      This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, Complex)@.
   SeeAlso
     (isWellDefined, Complex)
     (complex, Module)
     (complex, HashTable)
     (symbol SPACE, Complex, Array)
///

doc ///
    Key
        "differential of a chain complex"
        (symbol^, Symbol, Complex)
    Headline
        get the maps between the terms in a complex
    Usage
        dd^C
    Inputs
        C:Complex
    Outputs
        :ComplexMap
            a map of degree -1
    Description
        Text
            A chain complex is a sequence of modules connected
            by homomorphisms, called differentials, such that
            the composition of any two consecutive maps is zero.
        Text
            One can access the differential of a complex as follows.
        Example
            R = QQ[a..d];
            I = ideal(a*d-b*c, b^2-a*c, c^2-b*d);
            C = freeResolution(R^1/I)
            dd^C
            C.dd
            assert(dd^C === C.dd)
            assert(source dd^C === C)
            assert(target dd^C === C)
            assert(degree dd^C === -1)
        Text
            The composition of the differential with itself is zero.
        Example
            (dd^C)^2 == 0
        Text
            The individual maps between terms are indexed by their
            source.
        Example
            dd^C_2
            assert(source dd^C_2 === C_2)
            assert(target dd^C_2 === C_1)
    SeeAlso
        "Making maps between chain complexes"
        (symbol_, ComplexMap, ZZ)
        (symbol_, Complex, ZZ)
        (source, ComplexMap)
        (target, ComplexMap)
        (degree, ComplexMap)
///

doc ///
   Key
     (symbol SPACE, Complex, Array)
     (symbol SPACE, ComplexMap, Array)
   Headline
     shift a complex or complex map
   Usage
     D = C[i]
     g = f[i]
   Inputs
     C:Complex
       or {\tt f}, a @TO ComplexMap@
     :Array
       {\tt [i]}, where {\tt i} is an integer
   Outputs
     D:Complex
       or {\tt g}, a @TO ComplexMap@.
   Description
    Text
      The shifted complex $D$ is defined by $D_j = C_{i+j}$ for all $j$
      and the sign of the differential is changed if $i$ is odd.
       
      The shifted complex map $g$ is defined by $g_j = f_{i+j}$ for all $j$.
    
      The shift defines a natural automorphism on the category of complexes. 
      Topologists often call the shifted complex $C[1]$ the {\it suspension} of $C$.
    Example
      S = ZZ/101[a..d]
      C = freeResolution coker vars S
      dd^C_3
      D = C[1]
      assert isWellDefined D
      assert(dd^D_2 == -dd^C_3)
    Text
      In order to shift the complex one step, and not change the differential, one
      can do the following.
    Example
      E = complex(C, Base => -1)
      assert isWellDefined E
      assert(dd^E_2 == dd^C_3)
    Text
      The shift operator is functorial, as illustrated below.
    Example
      C2 = freeResolution (S^1/(a^2, b^2, c^2, d^2))
      C3 = freeResolution (S^1/(a^2, b^3, c^4, d^5))
      f2 = extend(C, C2, map(C_0, C2_0, 1))
      f3 = extend(C2, C3, map(C2_0, C3_0, 1))
      assert((f2*f3)[1] == (f2[1]) * (f3[1]))
      assert(source(f2[1]) == C2[1])
      assert(target(f2[1]) == C[1])
   SeeAlso
     concentration
     (complex, Complex)
     (extend, Complex, Complex, Matrix)
///

doc ///
   Key
     (complex, Complex)
   Headline
     make a complex by reindexing the terms of the complex
   Usage
     D = complex(C, Base => i)
   Inputs
     C:Complex
     Base => ZZ
       the target of the lowest differential
   Outputs
     D:Complex
       where if $d$ is the smallest index of the target of the differentials of C,
       then for all integers $j$ we have $D_{i+j} = C_{d+j}$
   Description
    Text
      This returns an alteration of the input complex, reindexing the terms of the complex.
    Example
      S = ZZ/101[a..d]
      C = freeResolution coker vars S
      D = complex(C, Base => 1)
      E = complex(D, Base => -11)
      dd^D_2 == dd^C_1
      dd^E_-9 == dd^C_2
    Text
      Rather than specifying the homological degree of the lowest target,
      one can also shift the homological degree, which may simultaneously negate
      the maps.
    Example
      F = C[-1]
      for i from min F to max F list
          dd^F_i == - dd^D_i
   SeeAlso
     (symbol SPACE, Complex, Array)
     (complex, List)
     (complex, Module)
     (complex, HashTable)
///

doc ///
   Key
     concentration
     (concentration, Complex)
   Headline
     indices on which a complex may be non-zero
   Usage
     (lo,hi) = concentration C
   Inputs
     C:Complex
   Outputs
     lo:ZZ
     hi:ZZ
       a pair of integers {\tt lo}, {\tt hi} such that {\tt C_i = 0}
       for {\tt i < lo} or {\tt i > hi}.
   Description
    Text
      In this package, each complex has a concentration {\tt (lo, hi)} 
      such that {\tt lo <= hi}.  When {\tt lo <= i <= hi}, the module
      {\tt C_i} might be zero.
      
      This function is mainly used in programming, to loop over all
      non-zero modules or maps in the complex.  This should not be confused
      with the support of a complex.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      concentration C
      D = C ++ C[5]
      concentration D
    Text
      Indices that are outside of the concentration automatically
      return the zero object.
    Example
      C_-1
      D_4
    Text
      The function {\tt concentration} does no computation.
      To eliminate extraneous zeros, use @TO (prune,Complex)@.
    Example
      f1 = a*id_C  
      E = ker f1
      concentration E
      concentration prune E
    Text
      The concentration of a zero complex can be arbitrary, however,
      after pruning, its concentration will be {\tt (0,0)}.
    Example      
      C0 = (complex S^0)[4]
      concentration C0
      prune C0
      concentration oo
   SeeAlso
     (symbol _, Complex, ZZ)
///

doc ///
   Key
     (symbol _, Complex, ZZ)
     (symbol ^, Complex, ZZ)
   Headline
     access individual object in a complex
   Usage
     C_i
     C^i
   Inputs
     C:Complex
     i:ZZ
       either the homological or cohomological index
   Outputs
     :Module
       the {\tt i}-th object
   Description
    Text
       Complexes can be either chain complexes or cochain complexes.  Subscripts
       refer to homological indices, and superscripts refer to
       cohomological indices.
     
       In this package homological indices are used by default.  For
       example, the @TO "concentration"@ references homological indices.
       Nevertheless, we always have the equation $C^i = C_{-i}$.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      C_2
      C^(-2)
      C_2 == C^(-2)
    Text
      Indices that are outside of the concentration automatically
      return the zero object.
    Example
      C_-7
   SeeAlso
///

doc ///
   Key
     (symbol ==, Complex, Complex)
     (symbol ==, Complex, ZZ)
     (symbol ==, ZZ, Complex)
   Headline
     whether two complexes are equal
   Usage
     C == D
     C == 0
   Inputs
     C:Complex
     D:Complex
   Outputs
     :Boolean
       that is true when {\tt C} and {\tt D} are equal
   Description
    Text
      Two complexes are equal if the corresponding 
      objects and corresponding maps at each index are equal.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      D = C[3][-3]
      C === D
      C == D
    Text
      Both the maps and the objects must be equal.
    Example
      (lo,hi) = concentration C
      E = complex for i from lo+1 to hi list 0*dd^C_i
      dd^E
      C == E
      E == 0
    Text
      A complex is equal to zero if all the objects and maps are zero.
      This could require computation to determine if something that
      is superficially not zero is in fact zero.
    Example
      f = id_C
      D = coker f
      D == 0
    Example
      C0 = complex S^0
      C1 = C0[4]
      concentration C0 == concentration C1
      C0 == C1
      C0 == 0
      C1 == 0
    Text
      Testing for equality is not the same testing for isomorphism.
      In particular, different presentations of a complex need not be equal.
    Example
      R = QQ[a..d];
      f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
      f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
      C = complex {f0, f1}
      HH C != complex coker f0
      prune HH C == complex coker f0
   Caveat
   SeeAlso
///

doc ///
   Key
     (symbol ==, ComplexMap, ComplexMap)
     (symbol ==, ComplexMap, ZZ)
     (symbol ==, ZZ, ComplexMap)
   Headline
     whether two complex maps are equal
   Usage
     f == g
     f == 0
     f == 1
   Inputs
     f:ComplexMap
       or 0, or 1.
     g:ComplexMap
       or 0, or 1.
   Outputs
     :Boolean
       that is true when {\tt f} and {\tt g} are equal
   Description
     Text
       Two complex maps are equal if they have the same source,
       the same target, and $f_i = g_i$ for all $i$.
     Example
       S = ZZ/101[a..c]
       C = freeResolution coker vars S
       f = id_C
       assert(f == 1)
       f === id_C[-1][1]
       f == id_C[-1][1]
     Text
       A complex map is equal to zero if all the maps are zero.
       This could require computation to determine if something that
       is superficially not zero is in fact zero.
     Example
       assert(0 * id_C == 0)
     Example
       g = randomComplexMap(C, C)
       h = canonicalMap(coker g, target g)
       assert(h == 0)
     Text
       Testing whether a map is equal to 1 is a shorthand for determining
       if the complex map is the identity.
       Although the matrices may appear to be the identity, the map is not the
       identity when the source and target are not equal.
     Example
       g = randomComplexMap(C, C, InternalDegree=>1, Cycle=>true)
       h = canonicalMap(coker g, target g)
       assert(h != 1)
     Text
       Testing for equality is not the same testing for isomorphism.
       In particular, different presentations of a complex need not be equal.
     Example
       D = prune image g
       p = D.cache.pruningMap
       p == 1
       assert(coker p == 0 and ker p == 0)
       assert(prune p == 1)
   SeeAlso
     (symbol ==, Complex, Complex)
     (symbol SPACE, ComplexMap, Array)
     randomComplexMap
     canonicalMap
     (prune, Complex)
///

doc ///
    Key
        freeResolution
        (freeResolution, Module)
        (freeResolution, Ideal)
    Headline
        compute a free resolution of a module or ideal
    Usage
        freeResolution M
    Inputs
        M:Module
            or @ofClass Ideal@, an ideal {\tt I} in a ring {\tt R}
    Outputs
        :Complex
            a free resolution of the module {\tt M} or of the
            quotient module {\tt R^1/I}
    Description
        Text
            A free resolution of a module $M$ is a complex
            $ F_0 \leftarrow F_1 \leftarrow F_2 \leftarrow \ldots$
            of free modules, which is acyclic: the cokernel of the map
            to $F_0$ is $M$ and the complex is exact at all other 
            locations.
        Example
            R = QQ[a..d]
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            M = R^1/I
            C = freeResolution M
            betti C
            length C
            dd^C
            assert isWellDefined C
            assert(prune HH C == complex M)
        Text
            Giving an ideal as the input produces a free resolution
            not of the module {\tt I}, but of the module {\tt R^1/I}.
        Example
            assert(freeResolution I == C)
            resolution complex M == freeResolution M
        Text
            Over a quotient ring, free resolutions are often infinite.
            Use the optional argument {\tt LengthLimit} to obtain
            part of the resolution.
        Example
            S = ZZ/101[a,b]
            R = S/(a^3+b^3)
            C = freeResolution (coker vars R, LengthLimit => 7)
            dd^C
    SeeAlso
        (resolution, Complex)
        (resolutionMap, Complex)
        (betti, Complex)
///

doc ///
   Key
     (homology, Complex)
   Headline
     homology of a complex
   Usage
     H = HH C
   Inputs
     C:Complex
   Outputs
     H:Complex
   Description
    Text
      The homology complex $H$ is defined by {\tt ker dd^C}/{\tt image dd^C}.
      The differential of the homology complex is the zero map.
      
      The first example is the complex associated to
      a triangulation of the real projective plane, having
      6 vertices, 15 edges, and 10 triangles.
    Example
      d1 = matrix {
          {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
          {0, -1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0}, 
          {0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 1, 0}, 
          {0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, -1}}
      d2 = matrix {
          {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 0, -1, -1, 0, 0, 0, 0, 0, 0}, 
          {1, 0, 1, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 1, 0, 0, -1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 1, 1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, -1, -1, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 0, 0, -1, 0, 0}, 
          {0, -1, 0, 0, 0, 1, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 1, 1, 0, 0}, 
          {0, 0, -1, 0, 0, 0, 0, 0, -1, 0}, 
          {0, 0, 0, 0, 0, -1, 0, 0, 1, 0}, 
          {0, 0, 0, -1, 0, 0, -1, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 0, 0, -1, -1}, 
          {0, 0, 0, 0, 0, 0, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, 0, -1}}
      C = complex {d1,d2}
      dd^C
      H = HH C
      dd^H == 0
    Text
      To see that the first homology group has torsion,
      we compute a minimal presentation of the homology.
    Example
      Hpruned = prune HH C
      dd^Hpruned == 0
    Text
      By dualizing the minimal free resolution of a monomial ideal,
      we get a free complex with non-trivial homology.  This particular
      complex is related to the local cohomology supported at the
      monomial ideal.
    Example
      S = ZZ/101[a..d, DegreeRank=>4];
      I = intersect(ideal(a,b),ideal(c,d))
      C = freeResolution (S^1/I)
      prune HH C
      Cdual = dual C
      prune HH Cdual
      prune HH_(-2) Cdual
   SeeAlso
     (dual, Complex)
     (prune, Complex)
///



doc ///
   Key
     (homology,ZZ,Complex)
     (cohomology,ZZ,Complex)
   Headline
     homology or cohomology module of a complex
   Usage
     HH_i C
     HH^i C
   Inputs
     i:ZZ
     C:Complex
   Outputs
     :Module
       the $i$-th homology or cohomology of the complex
   Description
    Text
      The $i$-th homology of a complex $C$ is the quotient
      ({\tt ker dd^C_i/image dd^C_(i+1)}).

      The first example is the complex associated to
      a triangulation of the real projective plane, having
      6 vertices, 15 edges, and 10 triangles.
    Example
      d1 = matrix {
          {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
          {0, -1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0}, 
          {0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 1, 0}, 
          {0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, -1}};
      d2 = matrix {
          {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 0, -1, -1, 0, 0, 0, 0, 0, 0}, 
          {1, 0, 1, 0, 0, 0, 0, 0, 0, 0}, 
          {0, 1, 0, 0, -1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 1, 1, 0, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, -1, -1, 0, 0, 0}, 
          {-1, 0, 0, 0, 0, 0, 0, -1, 0, 0}, 
          {0, -1, 0, 0, 0, 1, 0, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 1, 1, 0, 0}, 
          {0, 0, -1, 0, 0, 0, 0, 0, -1, 0}, 
          {0, 0, 0, 0, 0, -1, 0, 0, 1, 0}, 
          {0, 0, 0, -1, 0, 0, -1, 0, 0, 0}, 
          {0, 0, 0, 0, 0, 0, 0, 0, -1, -1}, 
          {0, 0, 0, 0, 0, 0, 0, -1, 0, 1}, 
          {0, 0, 0, 0, -1, 0, 0, 0, 0, -1}};
      C = complex {d1,d2}
      dd^C
      HH C
      prune HH_0 C
      prune HH_1 C
      prune HH_2 C
    Text
      The $i$-th cohomology of a complex $C$ is the $(-i)$-th
      homology of $C$.
    Example
      S = ZZ/101[a..d, DegreeRank=>4];
      I = intersect(ideal(a,b),ideal(c,d))
      C = dual freeResolution (S^1/I)
      prune HH^1 C
      prune HH^2 C
      prune HH^3 C
   SeeAlso
     prune
     (dual, Complex)
///

doc ///
   Key
     (directSum, Complex)
     (symbol++, Complex, Complex)
   Headline
     direct sum of complexes
   Usage
     D = C1 ++ C2
     D = directSum(C1,C2,...)
     D = directSum(name1 => C1, name2 => C2, ...)
   Inputs
     Ci:Complex
   Outputs
     D:Complex
       the direct sum of the input complexes
   Description
    Text
      The direct sum of two complexes is another complex.
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C1 ++ complex(S^13)[-2]
      C2 = complex (ideal(a,b,c))
      C1 ++ C2
      assert isWellDefined(C1 ++ C2)
    Text
      The direct sum of a sequence of complexes can be computed as follows.
    Example
      C3 = directSum(C1,C2,C2[-2])
      assert isWellDefined C3
    Text
      The direct sum is an n-ary operator with projection and
      inclusion maps from each component satisfying appropriate
      identities.
    Example
      C4 = directSum(first => C1, second => C2)
      C4_[first] -- inclusion map C1 --> C4
      C4^[first] -- projection map C4 --> C1
      C4^[first] * C4_[first] == 1
      C4^[second] * C4_[second] == 1
      C4^[first] * C4_[second] == 0
      C4^[second] * C4_[first] == 0
      C4_[first] * C4^[first] + C4_[second] * C4^[second] == 1
    Text
      There are two short exact sequences associated to a direct sum.
    Example
      isShortExactSequence(C4^[first], C4_[second])
      isShortExactSequence(C4^[second], C4_[first])
    Text
      Given a complex which is a direct sum, we obtain the component
      complexes and their names (indices) as follows.
    Example
      components C3
      indices C3
      components C4
      indices C4
   SeeAlso
     (components,Complex)
     indices
     (symbol^, Complex, Array)
     (symbol_, Complex, Array)
     (isShortExactSequence, ComplexMap, ComplexMap)
///

doc ///
   Key
     (symbol_, Complex, Array)
     (symbol^, Complex, Array)
   Headline
     the canonical inclusion or projection map of a direct sum
   Usage
     i = C_[name]
     p = C^[name]
   Inputs
     C:Complex
     name:
   Outputs
     :ComplexMap
       {\tt i} is the canonical inclusion and {\tt p} is
       the canonical projection
   Description
    Text
      The direct sum is an n-ary operator with projection and
      inclusion maps from each component satisfying appropriate
      identities.

      One can access these maps as follows.      
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C2 = complex (ideal(a,b,c))
      D = C1 ++ C2
      D_[0]
      D_[1]
      D^[0] * D_[0] == 1
      D^[1] * D_[1] == 1
      D^[0] * D_[1] == 0
      D^[1] * D_[0] == 0
      D_[0] * D^[0] + D_[1] * D^[1] == 1
    Text
      The default names for the components are the non-negative
      integers.  However, one can choose any name.
    Example
      E = (mike => C1) ++ (greg => C2)
      E_[mike]
      E_[greg]
      E^[mike] * E_[mike] == 1
      E^[greg] * E_[greg] == 1
      E^[mike] * E_[greg] == 0
      E^[greg] * E_[mike] == 0
      E_[mike] * E^[mike] + E_[greg] * E^[greg] == 1
    Text
      One can also access inclusion and projection maps of sub-direct sums.
    Example
      F = directSum(C1, C2, (complex S^13)[-4])
      F^[0,1]
      F_[0,2]
   SeeAlso
     (directSum, Complex)
     (components, Complex)
     indices
///

doc ///
   Key
     (components,Complex)
   Headline
     list the components of a direct sum
   Usage
     components C
   Inputs
     C:Complex
   Outputs
     :List
       the component complexes of a direct sum (of complexes)
   Description
    Text
      A complex which has been constructed as a direct sum
      stores its component complexes.
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C2 = complex (ideal(a,b,c))
      D = C1 ++ C2
      L = components D
      L_0 === C1
      L_1 === C2
      E = (mike => C1) ++ (greg => C2)
      components E
    Text
      The names of the component complexes are called indices, 
      and are used to access the relevant inclusion and projection maps.
    Example
      indices D
      D^[0]
      indices E
      E_[greg]
   SeeAlso
     (directSum, Complex)
     indices
     (symbol_, Complex, Array)
     (symbol^, Complex, Array)
///

doc ///
   Key
     (length, Complex)
   Headline
     length of a complex
   Usage
     length C
   Inputs
     C:Complex
   Outputs
     :ZZ
   Description
    Text
      The length of a complex is the difference between the highest index
      of a non-zero object and the lowest index of a non-zero object.
      
      Typically, it counts the number of non-zero differentials, e.g. in
      a free resolution.
    Example
      S = ZZ/101[a,b,c,d];
      C1 = freeResolution coker vars S
      length C1
      C2 = C1[5]
      length C2
      C3 = C1 ++ C1[6]
      length C3
    Text      
      This function always prunes the input complex, so might involve 
      computation.
    Example
      M1 = S^1/(a*b, c*d, a*c, b*c)
      M2 = S^1/(a*b, c*d, a*c)
      C4 = freeResolution M1
      C5 = freeResolution M2
      f = map(M1, M2, 1)
      C6 = coker extend(C4, C5, matrix f)
      concentration C6
      length C6
      prune C6
      concentration prune C6
   SeeAlso
     (prune,Complex)
     (concentration,Complex)
///

doc ///
   Key
     (ring, Complex)
     (ring, ComplexMap)
   Headline
     access the ring of a complex or a complex map
   Usage
     ring C
   Inputs
     C:Complex
       or a @TO "ComplexMap"@
   Outputs
     :Ring
   Description
    Text
      Every complex or complex map has a base ring.  This function access that information.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution coker vars S
      ring C
      assert(ring C === S)
      ring id_C
      assert(ring id_C === S)
   SeeAlso
     ring
///

doc ///
   Key
     (isHomogeneous, Complex)
   Headline
     whether a complex is homogeneous
   Usage
     isHomogeneous C
   Inputs
     C:Complex
   Outputs
     :Boolean
       that is true when {\tt C} is a homogeneous (i.e. graded) complex
   Description
    Text
      A complex is homogeneous (graded) if the base ring is graded,
      all of the component objects are graded, and
      all the component maps are graded of degree zero.
    Example
      S = ZZ/101[a,b,c,d];
      I = minors(2, matrix{{a,b,c},{b,c,d}})
      C = freeResolution (S^1/I)
      isHomogeneous C
      J = minors(2, matrix{{a,b,c},{b,c,d^2}})
      D = freeResolution (S^1/J)
      isHomogeneous D
   SeeAlso
     isHomogeneous
///

doc ///
   Key
     (symbol**, Complex, Complex)
     (symbol**, Complex, Module)
     (symbol**, Module, Complex)
     (tensor, Complex, Complex)
   Headline
     tensor product of complexes
   Usage
     D = C1 ** C2
   Inputs
     C1:Complex
       or @ofClass Module@
     C2:Complex
       or @ofClass Module@
   Outputs
     D:Complex
       tensor product of {\tt C1} and {\tt C2}
   Description
    Text
      The tensor product is a complex $D$ whose $i$th component is
      the direct sum of $C1_j \otimes C2_k$ over all $i = j+k$.
      The differential on $C1_j \otimes C2_k$ is the differential 
      $dd^{C1} \otimes id_{C2} + (-1)^j id_{C1} \otimes dd^{C2}$.
      
      As the next example illustrates, the Koszul complex can be constructed via iterated tensor products.
    Example
      S = ZZ/101[a..c]
      Ca = complex {matrix{{a}}}
      Cb = complex {matrix{{b}}}
      Cc = complex {matrix{{c}}}
      Cab = Cb ** Ca
      dd^Cab
      assert isWellDefined Cab
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      dd^Cabc
      assert isWellDefined Cabc
    Text
      If one of the arguments is a module, it is considered as a complex concentrated in homological degree 0.
    Example
      Cabc ** (S^1/(a,b,c))
      S^2 ** Cabc
    Text
      Because the tensor product can be regarded as the total complex of a double complex,
      each term of the tensor product comes with pairs of indices, labelling the summands.
    Example
      indices Cabc_1
      components Cabc_1
      Cabc_1_[{1,0}]
      indices Cabc_2
      components Cabc_2
      Cabc_2_[{0,2}]
   SeeAlso
     indices
     components
     directSum
///

doc ///
   Key
     (Hom, Complex, Complex)
     (Hom, Complex, Module)
     (Hom, Module, Complex)     
   Headline
     the complex of homomorphisms between two complexes
   Usage
     D = Hom(C1,C2)
   Inputs
     C1:Complex
       or @ofClass Module@
     C2:Complex
       or @ofClass Module@
   Outputs
     D:Complex
       the complex of homomorphisms between {\tt C1} and {\tt C2}
   Description
    Text
      The complex of homomorphisms is a complex $D$ whose $i$th component is
      the direct sum of $Hom(C1_j, C2_{j+i})$ over all $j$.
      The differential on $Hom(C1_j, C2_{j+i})$ is the differential 
      $Hom(id_{C1}, dd^{C2}) + (-1)^j Hom(dd^{C1}, id_{C2})$.
      $dd^{C1} \otimes id_{C2} + (-1)^j id_{C1} \otimes dd^{C2}$.

    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      D = Hom(C,C)
      dd^D
      assert isWellDefined D
    Text
      The homology of this complex is $Hom(C, ZZ/101)$
    Example
      prune HH D == Hom(C, coker vars S)
    Text
      If one of the arguments is a module, it is considered as a complex concentrated in homological degree 0.
    Example
      E = Hom(C, S^2)
      prune HH E
    Text
      There is a simple relationship between Hom complexes and @TO2 ((symbol SPACE, Complex, Array), "shifts")@.
      Specifically, shifting the first argument is the same as the negative shift of the result.  But
      shifting the second argument is only the same as the positive shift of the result
      up to a sign.
    Example
      Hom(C[3], C) == D[-3]
      Hom(C, C[-2]) == D[-2]
      Hom(C, C[-3]) != D[-3]
      Hom(C, C[-3]) == complex(- dd^(D[-3]))
    Text
      Specific maps and morphisms between complexes can be obtained
      with @TO (homomorphism, ComplexMap)@.
    Text
      Because the Hom complex can be regarded as the total complex of a double complex,
      each term comes with pairs of indices, labelling the summands.
    Example
      indices D_-1
      components D_-1
      indices D_-2
      components D_-2
   SeeAlso
     (homomorphism, ComplexMap)
     (homomorphism', ComplexMap)
     (randomComplexMap, Complex, Complex)
     indices
     components
     (Hom, ComplexMap, ComplexMap)
///


doc ///
   Key
     (betti,Complex)
   Headline
     display of degrees in a complex
   Usage
     betti C
   Inputs
     C:Complex
     Weights => List
	   a list of integers whose dot product with the multidegree of a basis
	   element is enumerated in the display returned.  The default is the
	   heft vector of the ring.  See @TO "heft vectors"@.
   Outputs
     :BettiTally
       a diagram showing the degrees of the generators of the components in {\tt C}
   Description
    Text
      Column $j$ of the top row of the diagram gives the rank of the
      $j$-th component $C_j$ of the complex $C$.  The entry in column $j$ in the row labelled
      $i$ is the number of basis elements of (weighted) degree $i+j$ in $C_j$.
      When the complex is the free resolution of a module the entries are
	  the total and the graded Betti numbers of the module.
      
      As a first example, we consider the ideal 
      in 18 variables which cuts out the variety
      of commuting 3 by 3 matrices.
    Example
      S = ZZ/101[vars(0..17)]
      m1 = genericMatrix(S,a,3,3)
      m2 = genericMatrix(S,j,3,3)
      J = ideal(m1*m2-m2*m1)
      C0 = freeResolution J
      betti C0
    Text
      From the display, we see that $J$ has 8 minimal generators, all
      in degree 2, and that there are 2 linear syzygies on these
      generators, and 31 quadratic syzygies.  
      Since this complex is the free resolution of $S/J$, 
      the projective dimension
      is 6, the index of the last column, and the regularity of $S/J$ is 4, 
      the index of the last row in the diagram.
    Example
      length C0
      pdim betti C0
      regularity betti C0
    Text
      The betti display still makes sense if the complex is not a free resolution.
    Example
      betti dual C0
      C1 = Hom(C0, image matrix{{a,b}});
      betti C1
      C1_-6
    Text
      This module has 10 generators, 2 in degree $-9=(-6)+(-3)$, and 8 in degree $-8=(-6)+(-2)$.
    Text
      In the multi-graded case, the heft vector is used, by default, as the weight vector for weighting the
	  components of the degree vectors of basis elements.
      
      The following example is a nonstandard $\mathbb{Z}$-graded polynomial ring.
    Example
      R = ZZ/101[a,b,c,Degrees=>{-1,-2,-3}];
      heft R
      C2 = freeResolution coker vars R
      betti C2
      betti(C2, Weights => {1})
    Text
      The following example is the Cox ring of the second Hirzebruch surface, and the complex
      is the free resolution of the irrelevant ideal.
    Example
      T = QQ[a,b,c,d,Degrees=>{{1,0},{-2,1},{1,0},{0,1}}];
      B = intersect(ideal(a,c),ideal(b,d))
      C3 = freeResolution B
      dd^C3
      heft T
      betti C3
      betti(C3, Weights => {1,0})
      betti(C3, Weights => {0,1})
      degrees C3_1
   SeeAlso
     betti
     BettiTally
///

-- TODO: once we have Hom evaluation map,
-- let's add in the map from C to dual dual C.
doc ///
   Key
     (dual, Complex)
   Headline
     make the dual of a complex
   Usage
     dual C
   Inputs
     C:Complex
   Outputs
     :Complex
   Description
    Text
      The dual of a complex $C$ is by definition $Hom(C, R)$, where $R$ is the ring of $C$.
    Example
      S = ZZ/101[a..d];
      B = intersect(ideal(a,c),ideal(b,d))
      C1 = freeResolution B
      C2 = dual C1
      assert(C2 == Hom(C1, S^1))
      C1 == dual dual C1
      prune HH C2
    Text
      The double dual is not necessarily isomorphic to the original complex.
    Example
      I = ideal(a^2, a*b, b^2)
      J = ideal(b^3, b*c, c^3)
      K = intersect(I,J)
      f = map(S^1/I ++ S^1/J, S^1/K, {{1},{1}})
      g = map(S^1/(I+J), S^1/I ++ S^1/J, {{1,-1}})
      C = complex{g,f}
      assert isWellDefined C
      assert isExact C
      assert(dual C == 0)
      assert(C != dual dual C)
   SeeAlso
     (Hom, Complex, Complex)
     (dual, Module)
///

doc ///
    Key
        (part, List, Complex)
        (part, ZZ, Complex)
    Headline
        extract a graded component of a complex
    Usage
        part(d, C)
    Inputs
        d:List
            or @TO "ZZ"@, if the underlying ring $R$ is singly graded.
        C:Complex
            that is homogeneous over $R$
    Outputs
        :Complex
          a complex over the coefficient ring of $R$
    Description
        Text
          If $C$ is a graded (homogeneous) complex over a ring $R$, and $d$ is a degree, this method
          computes the degree $d$ part of the complex over the coefficient ring of $R$.
        Text
          Taking parts of a graded (homogeneous) complex commutes with taking homology.
        Example
          R = QQ[a,b,c,d];
          I = ideal(a*b, a*c, b*c, a*d)
          C = freeResolution I
          D = part(4,C)
          prune HH D == part(4, HH C)
          prune HH D == part(4, complex(R^1/I))
        Text
          Given a squarefree monomial ideal corresponding to a
          simplicial complex, in a polynomial ring equipped with the
          fine grading, parts of the dual of the free resolution of the monomial
          ideal are the chain complexes of the induced simplicial
          subcomplexes.
        Example
          S = QQ[a..d, DegreeRank=>4];
          I = intersect(ideal(a,b), ideal(c,d))
          C = dual freeResolution I
          prune HH (part({-1,-1,-1,-1}, C)) -- empty quadrilateral
          prune HH part({-1,-1,0,0}, C) -- 2 points
          prune HH part({0,0,-1,-1}, C) -- 2 points
          prune HH part({0,0,0,0}, C) -- solid quadrilateral
        Text
          Obtain the implicit equation of a surface by using this method on a resolution over Rees algebra
          DDD Mike thinks about an example here.
    SeeAlso
      "DDD"
///

doc ///
    Key
        (naiveTruncation, Complex, ZZ, ZZ)
        (naiveTruncation, Complex, InfiniteNumber, ZZ)
        (naiveTruncation, Complex, Nothing, ZZ)
        (naiveTruncation, Complex, Sequence)
        (naiveTruncation, Complex, ZZ, InfiniteNumber)
        (naiveTruncation, Complex, ZZ, Nothing)
        naiveTruncation
    Headline
        drops all terms of a complex outside a given interval
    Usage
        naiveTruncation(C, lo, hi)
    Inputs
        C:Complex
        lo:ZZ
            or {\tt -infinity} or {\tt null} (the latter two give no lower bound)
        hi:ZZ
            or {\tt infinity} or {\tt null} (the latter two give no upper bound)
    Outputs
        :Complex
    Description
        Text
            Returns a new complex which drops (sets to zero) all modules 
            outside the given range.
        Example
            R = ZZ/101[a,b,c,d,e];
            I = intersect(ideal(a,b),ideal(c,d,e))
            C = freeResolution I
            naiveTruncation(C, 1, 2)
            C16 = naiveTruncation(C, 1, 6)
            isWellDefined C16
            naiveTruncation(C, 1, infinity)
            naiveTruncation(C, -13, 2)
            naiveTruncation(C, -infinity, 2)
            naiveTruncation(C, , 2)
        Text
            Truncation gives rise to a natural short exact sequence of complexes.
        Example
            C' = naiveTruncation(C,, 1)
            C'' = naiveTruncation(C, 2, infinity)
            f = inducedMap(C, C')
            g = inducedMap(C'', C)
            assert isShortExactSequence(g,f)
        Text
            There is another type of truncation, @TO "canonicalTruncation"@, which induces
            an isomorphism on (a range) of homology.
    SeeAlso
        (canonicalTruncation, Complex, ZZ, ZZ)
        (naiveTruncation, ComplexMap, Sequence, Sequence)
        (naiveTruncation, ComplexMap, Sequence)
///

doc ///
    Key
        (canonicalTruncation, Complex, ZZ, ZZ)
        (canonicalTruncation, Complex, InfiniteNumber, ZZ)
        (canonicalTruncation, Complex, Nothing, ZZ)
        (canonicalTruncation, Complex, Sequence)
        (canonicalTruncation, Complex, ZZ, InfiniteNumber)
        (canonicalTruncation, Complex, ZZ, Nothing)
        canonicalTruncation
    Headline
        reducing the number of non-zero terms of a complex
    Usage
        canonicalTruncation(C, lo, hi)
    Inputs
        C:Complex
        lo:ZZ
            or {\tt -infinity} or {\tt null} (the latter two give no lower bound)
        hi:ZZ
            or {\tt infinity} or {\tt null} (the latter two give no upper bound)
    Outputs
        :Complex
    Description
        Text
            Returns a new complex which drops (sets to zero) all modules 
            outside the given range, and modifies the ends to preserve homology
            in the given range.
        Example
            R = ZZ/101[a,b,c,d,e];
            I = intersect(ideal(a,b),ideal(c,d,e))
            C = (dual freeResolution I)[-4]
            C1 = canonicalTruncation(C, 1, 2)
            assert isWellDefined C1
            HH C1
            naiveTruncation(HH C, 1, 2) == HH C1
            prune HH C1
        Text
            We illustrate various possibilities for the truncation interval.
        Example
            C2 = canonicalTruncation(C, 1, 6)
            assert isWellDefined C2
            C3 = canonicalTruncation(C, 1, infinity)
            C2 == C3
            C4 = canonicalTruncation(C, -13, 2)
            C5 = canonicalTruncation(C, -infinity, 2)
            C4 == C5
            C6 = canonicalTruncation(C, , 2)
            C4 == C6
        Text
            If the lower and upper bounds are equal in the canonical truncation, 
            the resulting complex has a single nonzero term consisting of the
            homology in that location.
        Example
            assert(canonicalTruncation(C, 1, 1) == naiveTruncation(HH C, 1, 1))
        Text
            If we truncate only from below, then we get an injection
            from the truncation into the original complex, whereas if
            we truncate only from above, we get a surjection onto
            the truncated complex.
        Example
            f = inducedMap(C, C3)
            assert isWellDefined f
            assert(ker f == 0)
            prune coker f
            C7 = canonicalTruncation(C, -infinity, 1)
            C7 != coker f
        Example
            g = inducedMap(C5, C)
            assert isWellDefined g
            assert(coker g == 0)
            C8 = canonicalTruncation(C, 2, infinity)
            prune C8
            prune ker g
        Text
            There is another type of truncation, @TO "naiveTruncation"@, which yields
            a short exact sequence of complexes.
    SeeAlso
        (naiveTruncation, Complex, ZZ, ZZ)
        (canonicalTruncation, ComplexMap, Sequence)
///
 
doc ///
    Key
        (gradedModule, Complex)
    Headline
        a new complex in which the differential is zero
    Usage
        gradedModule C
    Inputs
        C:Complex
    Outputs
        :Complex
            whose differential is the zero map
    Description
        Text
            This routine isolates the terms in the complex
            and forgets the differentials
        Example
            R = ZZ/101[a,b,c,d,e];
            I = intersect(ideal(a,b),ideal(c,d,e))
            C = (dual freeResolution I)[-4]
            dd^C
            G = gradedModule C
            dd^G
            assert(isWellDefined G)
            assert(G != C)
        Text
            The homology of a complex already has zero differential.
        Example
            H = HH C
            prune H
            dd^H == 0
            assert(H == gradedModule H)
    SeeAlso
        (homology, Complex)
///

doc ///
    Key
        (symbol SPACE, RingMap, Complex)
    Headline
        apply a ring map
    Usage
        phi C
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        C:Complex
            over the ring $R$
    Outputs
        :Complex
            over the ring $S$
    Description
        Text
            We illustrate the image of a complex under a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = freeResolution I
            D = phi C
            isWellDefined D
            dd^D
            prune HH D
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the complex.
        Example
            R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            phi = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = freeResolution coker vars R
            D = phi C
            assert isWellDefined D
            assert isHomogeneous D
            prune HH D
    Caveat
        Every term in the complex must be free or a submodule of a free module.
        Otherwise, use @TO (tensor, RingMap, Complex)@.
    SeeAlso
        (symbol SPACE, RingMap, ComplexMap)
        (symbol **, RingMap, Complex)
///

doc ///
    Key
        (symbol SPACE, RingMap, ComplexMap)
    Headline
        apply a ring map to a map of complexes
    Usage
        phi f
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:ComplexMap
            over the ring $R$
    Outputs
        :ComplexMap
            over the ring $S$
    Description
        Text
            We illustrate the image of a complex map along a ring map.
        Example
            R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = freeResolution I
            CJ = freeResolution J
            f = extend(CJ, CI, map(CJ_0, CI_0, 1))
            assert isWellDefined f
            g = phi f
            assert isWellDefined g
            dd^(source g)
            dd^(target g)
            prune HH g
    SeeAlso
        (symbol SPACE, RingMap, Complex)
        (symbol **, RingMap, ComplexMap)
///

doc ///
    Key
        (symbol**, RingMap, Complex)
        (tensor, RingMap, Complex)
    Headline
        tensor a complex along a ring map
    Usage
        phi ** C
        tensor(phi, C)
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        C:Complex
            over the ring $R$
    Outputs
        :Complex
            over the ring $S$
    Description
        Text
            We illustrate the tensor product of a complex along a ring map.
        Example
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = freeResolution I
            D = phi ** C
            assert isWellDefined D
            dd^D
            prune HH D
        Text
            When the modules in the complex are not free modules,
            this is different than the image of a complex 
            under a ring map.
        Example
            I = ideal(x*y, x*z, y*z);
            J = I + ideal(x^2, y^2);
            g = inducedMap(module J, module I)
            assert isWellDefined g
            C = complex {g}
            D1 = phi C
            assert isWellDefined D1
            D2 = phi ** C
            assert isWellDefined D2
            prune D1
            prune D2
        Text
            When the ring map doesn't preserve homogeneity,
            the @TO "DegreeMap"@ option is needed to determine
            the degrees of the image free modules in the complex.
        Example
            R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            f = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = freeResolution coker vars R
            D = f ** C
            D == f C
            assert isWellDefined D
            assert isHomogeneous D
            prune HH D
            C1 = Hom(C, image vars R)
            D1 = f ** C1
            isWellDefined D1
            assert isHomogeneous D1
    SeeAlso
        (symbol **, RingMap, ComplexMap)
        (symbol SPACE, RingMap, Complex)
///

doc ///
    Key
        (symbol**, RingMap, ComplexMap)
        (tensor, RingMap, ComplexMap)
    Headline
        tensor a map of complexes along a ring map
    Usage
        phi ** f
        tensor(phi, f)
    Inputs
        phi:RingMap
            whose source is a ring $R$, and whose target is a ring $S$
        f:ComplexMap
            over the ring $R$
    Outputs
        :ComplexMap
            over the ring $S$
    Description
        Text
            We illustrate the tensor product of a map of complexes along a ring map.
        Example
            R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = freeResolution I
            CJ = freeResolution J
            f = extend(CJ, CI, map(CJ_0, CI_0, 1))
            assert isWellDefined f
            g = phi ** f
            assert isWellDefined g
            dd^(source g)
            dd^(target g)
            prune HH g
    SeeAlso
        (symbol **, RingMap, Complex)
        (symbol SPACE, RingMap, ComplexMap)
///
 
doc ///
    Key
        (resolution, Complex)
    Headline
        minimal free resolution of a complex
    Usage
        resolution C
    Inputs
        C:Complex
        LengthLimit => ZZ
          this is used to limit somehow the computation where resolutions might be too long or infinite
        DegreeLimit =>
          unused
        FastNonminimal =>
          unused
        HardDegreeLimit =>
          unused
        PairLimit =>
          unused
        SortStrategy =>
          unused
        StopBeforeComputation =>
          unused
        Strategy =>
          unused
        SyzygyLimit => 
          unused
    Outputs
        :Complex
    Description
        Text
            Given a complex $C$, this method produces a quasi-isomorphic complex $F$ 
            all of whose terms are free modules.  The algorithm used minimizes the
            ranks of the free modules in $F$.
        Example
            R = ZZ/101[a,b,c];
            I = ideal(a^2, a*b, b*c)
            C = Hom(freeResolution I, R^1/I)
            assert all(min C .. max C, i -> not isFreeModule C_i)
            fC = resolutionMap C
            FC = resolution C
            prune HH FC
            assert isQuasiIsomorphism fC
            assert isFree FC
            assert isWellDefined fC
            assert(0 == coker fC) -- showing that fC is surjective.
        Text
            The resolution of a short exact sequence is simply the 
            zero complex.
        Example
            J = ideal(a,b)
            K = ideal(b^2,c)
            g1 = map(R^1/(J+K), R^1/J ++ R^1/K, {{1,-1}})
            g2 = map(R^1/J ++ R^1/K, R^1/(intersect(J,K)), {{1},{1}})
            D = complex{g1, g2}
            assert isWellDefined D
            assert isShortExactSequence(g1,g2)
            fD = resolutionMap D
            assert isWellDefined fD
            assert isQuasiIsomorphism fD
            assert(0 == source fD) -- so fD is certainly not surjective!
        Text
            This method just accesses the cached value from
            the method @TO (resolutionMap, Complex)@, which 
            does the actual computation.
    SeeAlso
        (resolutionMap, Complex)
        (freeResolution, Module)
        (isQuasiIsomorphism, ComplexMap)
///

doc ///
    Key
        resolutionMap
        (resolutionMap, Complex)
    Headline
        map from a free resolution to the given complex
    Usage
        resolutionMap C
    Inputs
        C:Complex
        LengthLimit => ZZ
          this is used to limit somehow the computation where resolutions might be too long or infinite
        DegreeLimit =>
          unused
        FastNonminimal =>
          unused
        HardDegreeLimit =>
          unused
        PairLimit =>
          unused
        SortStrategy =>
          unused
        StopBeforeComputation =>
          unused
        Strategy =>
          unused
        SyzygyLimit => 
          unused
    Outputs
        :ComplexMap
            a quasi-isomorphism whose source is a free resolution and whose target is $C$
    Description
        Text
            Given a complex $C$, this method produces the natural quasi-isomorphism
            from a complex $F$ all of whose terms are free modules to the complex $C$.
            The algorithm used minimizes the ranks of the free modules in $F$.
        Example
            R = ZZ/101[a,b,c];
            I = ideal(a^2, a*b, b*c)
            C = Hom(freeResolution I, R^1/I)
            assert all(min C .. max C, i -> not isFreeModule C_i)
            fC = resolutionMap C
            FC = resolution C
            prune HH FC
            assert isQuasiIsomorphism fC
            assert isFree FC
            assert isWellDefined fC
            assert(0 == coker fC) -- showing that fC is surjective.
        Text
            The resolution of a short exact sequence is simply the 
            zero complex.
        Example
            J = ideal(a,b)
            K = ideal(b^2,c)
            g1 = map(R^1/(J+K), R^1/J ++ R^1/K, {{1,-1}})
            g2 = map(R^1/J ++ R^1/K, R^1/(intersect(J,K)), {{1},{1}})
            D = complex{g1, g2}
            assert isWellDefined D
            assert isShortExactSequence(g1,g2)
            fD = resolutionMap D
            assert isWellDefined fD
            assert isQuasiIsomorphism fD
            assert(0 == source fD) -- so fD is certainly not surjective!
        Text
            To avoid recomputation, this method caches its value.
    SeeAlso
        (resolution, Complex)
        (freeResolution, Module)
        (isQuasiIsomorphism, ComplexMap)
///

doc ///
    Key
        (minimalPresentation, Complex)
        (prune, Complex)
    Headline
        minimal presentation of all terms in a complex
    Usage
        D = minimalPresentation C
        D = prune C
    Inputs
        C:Complex
        Exclude => 
            unused
    Outputs
        D:Complex
            isomorphic to the input, where each term is replaced
            by a minimally presented model
    Consequences
        Item
            The isomorphism $g : D \to C$ is available as 
            @TT "g = D.cache.pruningMap"@.  The inverse isomorphism
            can be obtained as @TT "g^-1"@
    Description
        Text
            This is frequently useful to make the output of certain
            operations readable or understandable.
        Text
            In particular, homology often needs to be pruned to
            be understood.  For instance, this is useful 
            for recognizing when terms given by subquotient modules 
            are actually zero.
        Example
            S = ZZ/101[a,b,c,d,e];
            I = ideal(a,b) * ideal(c,d,e)
            F = dual freeResolution I
            C = HH F
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isComplexMorphism g
            assert (target g == C)
            assert (source g == D)
            h = g^-1
            assert(g*h == 1 and h*g == 1)
        Text
            The image of a map of complexes also becomes more
            understandable via pruning.
        Example
            S = ZZ/101[a,b,c];
            I = ideal(a^2,b^2,c^2);
            J = I + ideal(a*b*c);
            FI = freeResolution I
            FJ = freeResolution J
            f = randomComplexMap(FJ, FI ** S^{-1}, Cycle => true)
            C = image f
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isComplexMorphism g
            assert (target g == C)
            assert (source g == D)
            h = g^-1
            assert(g*h == 1 and h*g == 1)
   SeeAlso
       (minimize, Complex)
       (minimalPresentation, Module)
       randomComplexMap
       freeResolution
       isComplexMorphism
///

doc ///
    Key
        (minimize, Complex)
        minimize
    Headline
        a quasi-isomorphic complex whose terms have minimal rank
    Usage
        D = minimize C
    Inputs
        C:Complex
            graded, whose terms are all free modules
    Outputs
        D:Complex
            graded, whose terms are all free modules of minimal rank
    Consequences
        Item
            The projection morphism $g : C \to D$ is available as 
            @TT "g = D.cache.minimizingMap"@.  
    Description
        Text
            This method essentially removes all scalar units 
            from the matrices in the differential of $C$.
            
            We illustrate this in a simple example.
        Example
            S = ZZ/32003[a,b];
            I = ideal(a^2-b^2, a*b)
            C = freeResolution(I, FastNonminimal=>true)
            betti C
            D = minimize C
            assert(isWellDefined D and isHomogeneous D)
            betti D
            g = D.cache.minimizingMap
            assert isWellDefined g
            assert(isComplexMorphism g and isQuasiIsomorphism g)
            assert(source g == C)
            assert(target g == D)
            assert(coker g == 0)
        Text
            The minimal complex $D$ is a direct summand of the
            original complex $C$.  The natural inclusion
            of $D$ into $C$ can be constructed as follows.
        Example
            f = liftMapAlongQuasiIsomorphism(id_D, g)
            g*f == id_D
            assert(source f == D)
            assert(target f == C)
            assert(ker f == 0)
            f*g
        Text
            The chain complex $D$ is a direct summand of $C$,
            giving rise to a split short exact sequence of
            chain complexes.
        Example
            h = prune canonicalMap(C, ker g)
            assert isShortExactSequence(g, h)
        Text
            Warning: If the input complex is not homogeneous, then
            the output is probably not what one would expect.
        Example
            S = ZZ/32003[a..d]
            J = ideal(a*b*c-b*c, a*d-c, a^3-d^2*c)
            CJ = freeResolution J
            assert not isHomogeneous CJ
            D = minimize CJ
            isWellDefined D
            prune HH D == prune HH CJ
   SeeAlso
       freeResolution
       (resolution, Complex)
       (resolutionMap, Complex)
       (minimalPresentation, Complex)
///

/// -- comment about the above node
-- The following fails due to 'prune' (May 2020, see git issue #1116)
            SZZ = ZZ (monoid S);
            CZZ = complex hashTable for i from min C + 1 to max C list i => sub(dd^C_i, SZZ)
            isWellDefined CZZ
            betti CZZ
            DZZ = minimize CZZ
            assert isWellDefined DZZ
            assert isHomogeneous DZZ -- !! BUG
            betti DZZ
            g = D.cache.minimizingMap
            assert isWellDefined g
            assert isComplexMorphism g
            assert isQuasiIsomorphism g
            assert(source g == C)
            assert(target g == D)
            assert(coker g == 0)
///

/// -- comment about minimize and pruneComplex:
  -- this code can be run for the example ini (minimize,Complex).
  needsPackage "PruneComplex"
  C' = chainComplex C
  D' = pruneComplex(C', UnitTest => isScalar)
  g' = D'.cache.pruningMap
  D = complex D'
  g = complex g'
  source g == D
  target g == C
  isComplexMorphism g
///

doc ///
  Key
    ComplexMap
  Headline
    the class of all maps between chain complexes
  Description
    Text
      A map of chain complexes $f : C \rightarrow D$ of degree $d$ is a
      sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
      No relationship between the maps $f_i$ and 
      and the differentials of either $C$ or $D$ is assumed.
      
      The set of all maps from $C$ to $D$ form
      the complex $Hom(C,D)$ where $Hom(C,D)_d$ consists of the
      maps of degree $d$.

      The usual algebraic operations are available: addition,
      subtraction, scalar multiplication, and composition. The
      identity map from a chain complex to itself can be produced with
      @TO "id"@. An attempt to add (subtract, or compare) a ring
      element to a chain complex will result in the ring element being
      multiplied by the appropriate identity map.
  SeeAlso
    Complex
///

doc ///
    Key
        (map, Complex, Complex, HashTable)
    Headline
        make a map of chain complexes
    Usage
        f = map(D, C, H)
    Inputs
        C:Complex
        D:Complex
        H:HashTable
            whose keys are integers, and whose values are the maps between
            the corresponding terms
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
    Description
        Text
            A map of complexes $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and 
            and the differentials of either $C$ or $D$ is assumed.
            
            We construct a map of chain complexes by specifying the
            individual maps between the terms.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}
            D = freeResolution coker vars R
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{a*b^2, 0, 0}, {-a*c^2, a*c^3, 0}, {b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{a*b^2*c^3}})
                }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isComplexMorphism f
        Text
            The keys in the hash table index the terms in the source of the
            map.  If a key is missing, that map is taken to be the zero map.
            We illustrate by constructing a map of chain complexes
            having nonzero degree, and omitting one key in the hash table.
        Example
            E = D[-3]
            H = hashTable { 0 => map(E_3, C_0, 1),
                1 => map(E_4, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                3 => map(E_6, C_3, {{a*b^2*c^3}})
                }
            g = map(E, C, H, Degree => 3)
            g_2
            assert(g_1 == f_1)
            assert(g != f)
            assert isWellDefined g
            assert isHomogeneous g
            assert(degree g == 3)
            assert not isComplexMorphism g
            assert not isCommutative g
            assert(source g == C)
            assert(target g == E)
        Text
            This is the primary constructor used by all of the more user friendly
            methods for constructing a chain complex.
    Caveat
        This constructor minimizes computation
        and does very little error checking. To verify that a complex
        is well constructed, use @TO (isWellDefined, ComplexMap)@.
    SeeAlso
        ComplexMap
        (map, Complex, Complex, Function)
        (isWellDefined, ComplexMap)
        (isHomogeneous, ComplexMap)
        (degree, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (isCommutative, ComplexMap)
        (source, ComplexMap)
        (target, ComplexMap)
///

doc ///
    Key
        (map, Complex, Complex, ZZ)
    Headline
        make the zero map or identity between chain complexes
    Usage
        f = map(D, C, 0)
        f = map(C, C, 1)
    Inputs
        C:Complex
        D:Complex
        0:ZZ
            or 1
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
            the zero map from $C$ to $D$ or the identity map from $C$ to $C$
    Description
        Text
            A map of complexes $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            
            We construct the zero map between two
            chain complexes.
        Example
            R = QQ[a,b,c]
            C = freeResolution coker vars R
            D = freeResolution coker matrix{{a^2, b^2, c^2}}
            f = map(D, C, 0)
            assert isWellDefined f
            assert isComplexMorphism f
            g = map(C, C, 0, Degree => 13)
            assert isWellDefined g
            assert(degree g == 13)
            assert not isComplexMorphism g
            assert isCommutative g
            assert isHomogeneous g
            assert(source g == C)
            assert(target g == C)
        Text
            Using this function to create the identity map
            is the same as using @TO (id, Complex)@.
        Example
            assert(map(C, C, 1) === id_C)
   SeeAlso
        ComplexMap
        (map, Complex, Complex, Function)
        (isWellDefined, ComplexMap)
        (isHomogeneous, ComplexMap)
        (degree, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (isCommutative, ComplexMap)
        (source, ComplexMap)
        (target, ComplexMap)
        (id, Complex)
///

doc ///
    Key
        (map, Complex, Complex, Function)
    Headline
        make a map of chain complexes
    Usage
        f = map(D, C, fcn)
    Inputs
        C:Complex
        D:Complex
        fcn:Function
            whose values at integers are the maps between
            the corresponding terms
        Degree => ZZ
            the degree of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        f:ComplexMap
    Description
        Text
            A map of complexes $f : C \rightarrow D$ of degree $d$ is a
            sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
            No relationship between the maps $f_i$ and 
            and the differentials of either $C$ or $D$ is assumed.
            
            We construct a map of chain complexes by specifying a
            function which determines the maps between the terms.
        Example
            R = ZZ/101[x]/x^3;
            M = coker vars R
            C = freeResolution(M, LengthLimit => 6)
            D = C[1]
            f = map(D, C, i -> 
                if odd i then 
                    map(D_i, C_i, {{x}})
                else map(D_i, C_i, {{x^2}})
                )
            assert isWellDefined f
            assert isCommutative f
            assert(source f == C)
            assert(target f == D)
    SeeAlso
        ComplexMap
        (isWellDefined, ComplexMap)
        (isCommutative, ComplexMap)
        (source, ComplexMap)
        (target, ComplexMap)
///

doc ///
    Key
        (map, Complex, Complex, ComplexMap)
    Headline
        make a new map of chain complexes from an existing one
    Usage
        g = map(D, C, f)
    Inputs
        C:Complex
        D:Complex
        f:ComplexMap
            regarded as providing matrices which induce maps between the terms of $C$ and $D$
        Degree => ZZ
            the degree $d$ of the resulting map
        DegreeLift => 
            unused
        DegreeMap =>
            unused
    Outputs
        g:ComplexMap
    Description
        Text
            A map of complexes $f : C' \rightarrow D'$ is a
            sequence of maps $f_i : C'_i \rightarrow D'_{d'+i}$.  
            The new map $g : C \rightarrow D$ is the sequence of maps $g_i : C_i \rightarrow D_{d+i}$
            induced by the matrix of $f_i$.
            
            One use for this function is to get the new map of chain complexes obtained by shifting 
            the source or target of an existing chain map.  For example, one can regard the differential
            on a complex can be regarded as a map of degree zero between shifted complexes.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker vars R
            f = map(C[-1], C, dd^C, Degree => 0)
            assert isWellDefined f
            assert(degree f == 0)
            assert isCommutative f
            assert isComplexMorphism f
            assert not isComplexMorphism dd^C
    SeeAlso
        ComplexMap
        (map, Complex, Complex, Function)
        (isWellDefined, ComplexMap)
        (degree, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (isCommutative, ComplexMap)
        (symbol SPACE, Complex, Array)
///

doc ///
    Key
        (id, Complex)
    Headline
        the identity map of a chain complex
    Usage
        f = id_C
    Inputs
        C:Complex
    Outputs
        f:ComplexMap
          the identity map from $C$ to itself
    Description
        Text
            The chain complexes together with complex morphisms
            forms a category.  In particular, every chain 
            complex has an identity map.
        Example
            R = ZZ/101[x,y]/(x^3, y^3)
            C = freeResolution(coker vars R, LengthLimit=>6)
            f = id_C
            assert isWellDefined f
            assert isComplexMorphism f
        Text
            The identity map corresponds to an element of
            the Hom complex.
        Example
            R = ZZ/101[a,b,c]
            I = ideal(a^2, b^2, b*c, c^3)
            C = freeResolution I
            D = Hom(C, C)
            homomorphism' id_C
    SeeAlso
        (map, Complex, Complex, ZZ)
        (isWellDefined, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (Hom, Complex, Complex)
        (homomorphism', ComplexMap)
///

doc /// 
    Key
        (isWellDefined, ComplexMap)
    Headline
        whether a map of chain complexes is well-defined
    Usage
        isWellDefined f
    Inputs
        f:ComplexMap
    Outputs
        :Boolean
            that is true when {\tt f} determines a well defined complex map
    Description
        Text
            A map of chain complexes $f : C \to D$ of degree $d$ is a sequence of
            maps $f_i : C_i \to D_{d+i}$.  No relationship is required between
            these maps and the differentials in the source and target.

            This routine checks that $C$ and $D$ are well-defined
            chain complexes, and that, for each $f_i$, the source and
            target equal $C_i$ and $D_{d+i}$, respectively.  If the
            variable {\tt debugLevel} is set to a value greater than
            zero, then information about the nature of any failure is
            displayed.
        Text
            Unlike the @TO2((isWellDefined, Complex), 
                "corresponding function for Complexes")@,
            the basic constructors for complex maps are all but
            assured to be well defined. The only case that could cause
            a problem is if one constructs the source or target
            complex, and those are not well defined.
        Example
            R = ZZ/101[a,b,c];
            C = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}
            D = freeResolution coker vars R
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{a, 0, 0}, {-b, b^2, 0}, {0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{a*b^2, 0, 0}, {-a*c^2, a*c^3, 0}, {b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{a*b^2*c^3}})
                }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isComplexMorphism f
        Text
            We construct two random maps of chain complexes,
            and check to see that, as should be the case, 
            both are well defined.
        Example
            g = randomComplexMap(D,C)
            assert isWellDefined g
            assert not isCommutative g
        Example
            h = randomComplexMap(D,C, Cycle => true)
            assert isWellDefined h
            assert isComplexMorphism h
        Text
            This method also checks the following aspects of 
            the data structure:
        Text
            @UL {
                TEX "The underlying hash table has exactly the expected keys,
                namely, {\\tt source, target, degree, map, cache}",
                "The ring of the source and target are the same",
                "The source and target are well defined complexes",
                "The degree is an integer",
                TEX "All keys in the {\\tt map} field are integers,
                in the range of the concentration of the source",
                TEX "The source and target of each $f_i$ is as expected",
                TEX "If the {\\tt isCommutative} key is present in the cache
                table, then commutativity of the map with the differentials
                is checked"
                }@
    SeeAlso
        (isWellDefined, Complex)
        (isCommutative, ComplexMap)
        (isComplexMorphism, ComplexMap)
        (map, Complex, Complex, HashTable)
///

-- DDD
doc ///
    Key
        canonicalMap
        (canonicalMap, Complex, Complex)
        [canonicalMap, UseTarget]
    Headline
        gets the natural map arising from various constructions
    Usage
        g = canonicalMap(D, C)
    Inputs
        C:Complex
        D:Complex
        UseTarget => Boolean
            determines the choice of canonical map
            when $D$ is a cylinder of a map $f$
            and the source and target of $f$ are the same
    Outputs
        g:ComplexMap
    Description
        Text
            A canonical map, also called a natural map, is a 
            map that arises naturally from the definition or
            the construction of the object.
            
            The following six constructions are supported: kernel, 
            cokernel, image, coimage, cone, and cylinder.
        Text
            The @TO2((kernel, ComplexMap), "kernel of a complex map")@ 
            comes with a natural injection into the source complex.
            This natural map is always a complex morphism.
        Example
            R = ZZ/101[a,b,c,d];
            D = freeResolution coker vars R
            C = (freeResolution coker matrix"a,b,c")[1]
            f = randomComplexMap(D, C, Cycle=>true)
            assert isComplexMorphism f
            K1 = kernel f
            g = canonicalMap(source f, K1)
            degree g
            assert(isWellDefined g and isComplexMorphism g)
        Example
            f2 = randomComplexMap(D, C)
            assert not isComplexMorphism f2
            K2 = kernel f2
            g2 = canonicalMap(source f2, K2)
            assert(isWellDefined g2 and isComplexMorphism g2)
        Text
            The @TO2((cokernel, ComplexMap), "cokernel of a complex map")@
            comes with a natural surjection from the target complex.
        Example
            Q = cokernel f
            g3 = canonicalMap(Q, target f)
            assert(isWellDefined g3 and isComplexMorphism g3)
        Text
            The @TO2((image, ComplexMap), "image of a complex map")@ 
            comes with a natural injection into the target complex.
        Example
            I = image f
            g4 = canonicalMap(target f, I)
            assert(isWellDefined g4 and isComplexMorphism g4)
        Text
            The @TO2((coimage, ComplexMap), "coimage of a complex map")@
            comes with a natural surjection from the source complex.
            This natural map is always a complex morphism.
        Example
            J = coimage f
            g5 = canonicalMap(J, source f)
            assert(isWellDefined g5 and isComplexMorphism g5)
        Example
            J2 = coimage f2
            g6 = canonicalMap(J2, source f2)
            assert(isWellDefined g6 and isComplexMorphism g6)
        Text
            The @TO2((cone, ComplexMap), "cone of a complex morphism")@
            comes with two natural maps.  Given a map $f : C \to D$,
            let $E$ denote the cone of $f$.  The first is a natural
            injection from the target $D$ of $f$ into $E$.  The
            second is a natural surjection from $E$ to $C[-1]$.
            Together, these maps form a short exact sequence of
            complexes.
        Example
            E = cone f
            g = canonicalMap(E, target f)
            h = canonicalMap((source f)[-1], E)
            assert(isWellDefined g and isWellDefined h)
            assert(isComplexMorphism g and isComplexMorphism h)
            assert isShortExactSequence(h,g)
        Text
            The @TO2((cylinder, ComplexMap), "cylinder of a complex
            map")@ comes with four natural maps.  Given a map $f : C
            \to D$, let $F$ denote the cylinder of $f$.  The first is
            the natural injection from the source $C$ of $f$ into the
            cylinder $F$.  Together these two maps form a short exact
            sequence of complexes.
        Example
            F = cylinder f
            g = canonicalMap(F, source f)
            h = canonicalMap(E, F)
            assert(isWellDefined g and isWellDefined h)
            assert(isComplexMorphism g and isComplexMorphism h)
            assert isShortExactSequence(h,g)
        Text
            The third is the natural injection from the target $D$ of $F$
            into the cylinder $F$.
            The fourth is the natural surjection from the cylinder $F$ to the 
            target $D$ of $f$.
            However, these two maps do not form a short exact
            sequence of complexes.
        Example
            g' = canonicalMap(F, target f)
            h' = canonicalMap(target f, F)
            assert(isWellDefined g' and isWellDefined h')
            assert(isComplexMorphism g' and isComplexMorphism h')
            assert not isShortExactSequence(h',g')
        Text
            When $D == C$, the optional argument {\tt UseTarget} 
            selects the appropriate natural map.
        Example
            f' = id_C
            F' = cylinder f'
            g = canonicalMap(F', C, UseTarget=>true)
            h = canonicalMap(F', C, UseTarget=>false)
            assert(isWellDefined g and isWellDefined h)
            assert(g != h)
            assert(isComplexMorphism g and isComplexMorphism h)
    SeeAlso
        (inducedMap, Complex, Complex)
///



-- DDD


-- this is deleted, but maybe we want the prose.
///
  Key
    ComplexMorphism
  Headline
    the class of all morphisms of complexes
  Description
    Text
      A complex morphism $f : C \rightarrow D$ is a
      complex homomorphism of degree zero, which commutes with the
      differentials.  Specifically, if $f_i : C_i \rightarrow D_i$,
      then we have $dd^D_i f_i = f_{i-1} dd^C_i$, for all $i$.

      The set of all complex homomorphisms from $C$ to $D$ form
      the complex $Hom(C,D)$ where $Hom(C,D)_d$ consists of the
      homomorphisms of degree $d$.  The kernel of the
      differential in degree 0, $dd^{Hom(C,D)}_0$ consists of 
      all complex morphisms from $C$ to $D$.

      The usual algebraic operations are available: addition,
      subtraction, scalar multiplication, and composition. The
      identity map from a chain complex to itself can be produced with
      @TO "id"@. An attempt to add (subtract, or compare) a ring
      element to a chain complex will result in the ring element being
      multiplied by the appropriate identity map.
  SeeAlso
    Complex
    ComplexMap
///

doc ///
    Key
        (source, ComplexMap)
    Headline
        get the source of a map of chain complexes
    Usage
        C = source f
    Inputs
      f:ComplexMap
    Outputs
      C:Complex
    Description
        Text
            Given a complex map $f : C \to D$
            this method returns the chain complex $C$.
        Example
            R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = freeResolution I
            FJ = freeResolution J
            f = randomComplexMap(FJ, FI, Cycle=>true)
            source f
            assert isWellDefined f
            assert isComplexMorphism f
            assert(source f == FI)
            assert(target f == FJ)
        Text
            The differential in a complex is a map of chain complexes.
        Example
            kk = coker vars R
            F = freeResolution kk
            source dd^F == F
            target dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making chain complexes"
       (target, ComplexMap)
       (freeResolution, Ideal)
       (randomComplexMap, Complex, Complex)
///

doc ///
    Key
        (target, ComplexMap)
    Headline
        get the target of a map of chain complexes
    Usage
        C = target f
    Inputs
      f:ComplexMap
    Outputs
      C:Complex
    Description
        Text
            Given a complex map $f : C \to D$
            this method returns the chain complex $D$.
        Example
            R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = freeResolution I
            FJ = freeResolution J
            f = randomComplexMap(FJ, FI, Cycle=>true)
            target f
            assert isWellDefined f
            assert isComplexMorphism f
            assert(target f == FJ)
            assert(source f == FI)
        Text
            The differential in a complex is a map of chain complexes.
        Example
            kk = coker vars R
            F = freeResolution kk
            target dd^F == F
            source dd^F == F
            degree dd^F == -1
   SeeAlso
       "Making chain complexes"
       (source, ComplexMap)
       (freeResolution, Ideal)
       (randomComplexMap, Complex, Complex)
///

-- XXX
///
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

doc ///
  Key
    (symbol*, ComplexMap, ComplexMap)
    (symbol*, RingElement, ComplexMap)
    (symbol*, Number, ComplexMap)
  Headline
    composition of homomorphisms of complexes
  Usage
    f = h * g
  Inputs
    h:ComplexMap
      if a ring element or integer, then we multiply the ring element
      by the appropriate identity map
    g:ComplexMap
  Outputs
    f:ComplexMap
      the composition of $g$ followed by $h$
  Description
    Text
      If $g_i : C_i \rightarrow D_{d+i}$, and $h_j : D_j \rightarrow E_{e+j}$,
      then the composition corresponds to 
      $f_i := h_{d+i} * g_i : C_i \rightarrow E_{i+d+e}$.  In particular,
      the degree of the composition $f$ is the sum of the degrees of
      $g$ and $h$.
    Example
      R = ZZ/101[a..d]
      C = freeResolution coker vars R
      3 * dd^C
      0 * dd^C
      dd^C * dd^C
  Caveat
  SeeAlso
///

doc ///
   Key
     isExact
     (isExact, Complex)
     (isExact, Complex, InfiniteNumber, InfiniteNumber)
     (isExact, Complex, InfiniteNumber, Number)
     (isExact, Complex, Number, InfiniteNumber)
     (isExact, Complex, Number, Number)
   Headline
     whether a complex is exact
   Usage
     isExact C
     isExact(C, lo, hi)
   Inputs
     C:Complex
     lo:Number
       or -infinity
     hi:Number
       or infinity
   Outputs
     :Boolean
       that is true when {\tt C} is exact
   Description
    Text
      The complex $C$ is exact if and only if the homology group
      $H^i(C)$ is the zero module, for all $i$.  If bounds are given,
      then true is returned if $H^i(C) = 0$ for all $lo \le i \le
      hi$.
    Text
      A resolution $C$ is an exact complex except in homological degree 0. 
      The augmented complex $C'$ is exact everywhere.
    Example
      S = ZZ/101[a..d];
      I = monomialCurveIdeal(S, {1,3,4})
      C = freeResolution I
      prune HH C
      assert not isExact C
      assert isExact(C, 1, infinity)
      C' = cone inducedMap(complex(S^1/I), C)[1]
      prune HH C'
      assert isExact C'
   SeeAlso
     (homology, Complex)
     cone
     freeResolution
     prune
///
 
doc ///
  Key
    (isFree, Complex)
    isFree
  Headline
    whether a complex consists of free modules
  Usage
    isFree C
  Inputs
    C:Complex
  Outputs
    :Boolean
      that is true when each $C_i$ is a free module
  Description
    Text
      This method checks whether the given representation of each
      module $C_i$ is free. To determine whether the complex $C$ is
      isomorphic to a free complex, use @TO2("(prune,Complex)", "prune")@.
    Text
      The following example demonstrates that the presentation of a module
      might not reveal the property of being free.
    Example
      S = ZZ/101[a,b];
      M = kernel vars S
      assert not isFreeModule M
      assert isFreeModule prune M
    Text
      By definition, a free resolution $C$ consists of free modules.
      In contrast, the augmented complex $C'$ might or might not
      consist of free modules.
    Example
      C = freeResolution M
      assert isFree C
      C' = cone map(complex M, C, i -> map(M, C_0, 1))[1]
      isWellDefined C'
      assert not isFree C'
      prune C'
      assert isFree prune C'
  SeeAlso
    isFreeModule
    freeResolution
    (prune, Complex)
///

doc ///
  Key
    (isCommutative, ComplexMap)
  Headline
    whether a complex map commutes with the differentials
  Usage
    isCommutative f
  Inputs
    f:ComplexMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the differentials
  Description
    Text
      For a complex map $f : C \to D$ of degree $d$, this method
      checks whether, for all $i$, we have
      $dd^D_{i+d} * f_i = (-1)^d * (f_{i-1} * dd^C_i)$.
    Text
      We first construct a random complex map which commutes with the differential.
    Example
      S = ZZ/101[a,b,c];
      C = freeResolution coker vars S
      D = C ** C
      f1 = randomComplexMap(D, C, Boundary => true, InternalDegree => 1)
      isCommutative f1
      assert(degree f1 == 0)
      assert isNullHomotopic f1
      assert(source f1 == C and target f1 == D)
    Text
      We next generate a complex map that is commutative and (likely) 
      induces a nontrivial map on homology.
    Example
      f2 = randomComplexMap(D, C, Cycle => true)
      isCommutative f2
      assert(degree f2 == 0)
      assert isComplexMorphism f2
    Text
      When the degree of the complex map is odd, isCommutative determines
      whether the map is anti-commutative.  We illustrate
      this for one square.
    Example
      f3 = randomComplexMap(D, C, Cycle => true, Degree=>1, InternalDegree => 1)
      isCommutative f3
      assert(degree f3 == 1)
      part1 = dd^D_3 * f3_2
      part2 = f3_1 * dd^C_2
      assert(part1 + part2 == 0)
    Text
      If the @TO "debugLevel"@ is greater than zero, then
      the location of the first failure of commutativity is displayed.
    Example
      f4 = randomComplexMap(D, C)
      isCommutative f4
      debugLevel = 1
      isCommutative f4
  SeeAlso
    isComplexMorphism
    randomComplexMap
    freeResolution
///

doc ///
  Key
    (isComplexMorphism, ComplexMap)
    isComplexMorphism
  Headline
    whether a complex map is a morphism of complexes
  Usage
    isComplexMorphism f
  Inputs
    f:ComplexMap
  Outputs
    :Boolean
      that is true when $f$ commutes with the differentials and has degree $0$
  Description
    Text
      For a complex map $f : C \to D$ of degree $d$, this method
      checks whether $d = 0$ and, for all $i$, we have
      $dd^D_{i+d} * f_i = (-1)^d * (f_{i-1} * dd^C_i)$.
    Text
      We first construct a random complex morphism.
    Example
      S = ZZ/101[a,b,c];
      C = freeResolution coker vars S
      D = C ** C
      f1 = randomComplexMap(D, C, Boundary => true, InternalDegree => 1)
      isComplexMorphism f1
      assert(degree f1 == 0)
      assert isNullHomotopic f1
      assert(source f1 == C and target f1 == D)
    Text
      We next generate a complex morphism that (likely) 
      induces a nontrivial map on homology.
    Example
      f2 = randomComplexMap(D, C, Cycle => true)
      isComplexMorphism f2
      assert(degree f2 == 0)
      assert isComplexMorphism f2
    Text
      When the degree is non-zero, the map is not a complex morphism.
      If the @TO "debugLevel"@ is greater than zero, then
      information about the failure is displayed.
    Example
      f3 = randomComplexMap(D, C, Cycle => true, Degree=>1, InternalDegree => 1)
      assert(degree f3 == 1)
      isComplexMorphism f3
      debugLevel = 1
      isComplexMorphism f3
      assert isCommutative f3
    Example
      f4 = randomComplexMap(D, C)
      assert(degree f4 == 0)
      debugLevel = 0
      isComplexMorphism f4
      debugLevel = 1
      isComplexMorphism f4
  SeeAlso
    (isCommutative, ComplexMap)
    randomComplexMap
    freeResolution
///

-- for the next 4 nodes:
-- make better examples
-- add text (when are these actually defined?  Maybe change code)
-- also add SeeAlso canonicalMap's.
doc ///
  Key
    (image, ComplexMap)
  Headline
    make the image of a map of complexes
  Usage
    E = image f
  Inputs
    f : ComplexMap
  Outputs
    E : Complex
  Description
    Text
      If $f : C \to D$ is a map of chain complexes of degree $d$,
      then the image is the complex $E$ whose $i-th$ is $image(f_{i-d})$,
      and whose differential is induced from the differential 
      on the target.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the image.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      prune image g == D
      prune image h == C[-1]
    Text
      There is a canonical map of complexes from the image to the target.
    Example
      g1 = canonicalMap(target g, image g)
      ker g1 == 0
      image g1 == image g
      h1 = canonicalMap(target h, image h)
      ker h1 == 0
      image h1 == image h
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    image
    (coimage, ComplexMap)
    (kernel, ComplexMap)
    (cokernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (coimage, ComplexMap)
  Headline
    make the coimage of a map of complexes
  Usage
    coimage f
  Inputs
    f : ComplexMap
  Outputs
    : Complex
  Description
    Text
      The coimage of a chain complex map $f : C \to D$
      is the complex $E$ whose $i-th$ term is $coimage(f_i)$,
      and whose differential is induced from the differential 
      on the source.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the coimage.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      coimage g == D
      prune coimage h == C[-1]
    Text
      There is a canonical map of complexes from the source to the coimage.
    Example
      g1 = canonicalMap(coimage g, source g)
      coimage g1 == coimage g
      coker g1 == 0
      h1 = canonicalMap(coimage h, source h)
      coimage h1 == coimage h
      coker h1 == 0
  Caveat
    The coimage is more computationally intensive than @TO (image, ComplexMap)@
    because, unlike {\tt image}, it computes kernels of maps of modules.
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    coimage
    (image, ComplexMap)
    (kernel, ComplexMap)
    (cokernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (kernel, ComplexMap)
  Headline
    make the kernel of a map of complexes
  Usage
    kernel f
    ker f
  Inputs
    f : ComplexMap
  Outputs
    : Complex
  Description
    Text
      The kernel of a chain complex map $f : C \to D$
      is the complex $E$ whose $i-th$ term is $kernel(f_i)$,
      and whose differential is induced from the differential 
      on the source.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the kernel.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      ker g == 0
      prune ker h == D
    Text
      There is a canonical map of complexes from the kernel to the source.
    Example
      h1 = canonicalMap(source h, ker h)
      ker h == image h1
      ker h1 == 0
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    ker
    (image, ComplexMap)
    (coimage, ComplexMap)
    (cokernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (cokernel, ComplexMap)
  Headline
    make the cokernel of a map of complexes
  Usage
    cokernel f
    coker f
  Inputs
    f : ComplexMap
  Outputs
    : Complex
  Description
    Text
      If $f : C \to D$ is a map of chain complexes of degree $d$,
      then the cokernel is the complex $E$ whose $i-th$ is $cokernel(f_{i-d})$,
      and whose differential is induced from the differential 
      on the target.
    Text
      In the following example, we first construct a random
      complex morphism $f : C \to D$.  We consider 
      the exact sequence $0 \to D \to cone(f) \to C[-1] \to 0$.
      For the maps $g : D \to cone(f)$ and $h : cone(f) \to C[-1]$,
      we compute the kernel.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d)
      D = freeResolution ideal(a,b,c)
      f = randomComplexMap(D, C, Cycle => true, InternalDegree => 0)
      Cf = cone f
      g = canonicalMap(Cf, D)
      h = canonicalMap(C[-1], Cf)
      prune coker g == C[-1]
      coker h == 0
    Text
      There is a canonical map of complexes from the target to the cokernel.
    Example
      g1 = canonicalMap(coker g, target g)
      coker g == image g1
      coker g1 == 0
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    cokernel
    (image, ComplexMap)
    (coimage, ComplexMap)
    (kernel, ComplexMap)
    canonicalMap
///

doc ///
  Key
    (cone, ComplexMap)
  Headline
    make the mapping cone of a morphism of chain complexes
  Usage
    cone f
  Inputs
    f:ComplexMap
      which is a morphism of complexes
  Outputs
    :Complex
  Description
    Text
      Given a morphism $f : B \to C$, the mapping cone is the complex
      whose $i$-th term is $B_{i-1} \oplus\ C_i$, and whose $i$-th 
      differential is given by
      {\tt matrix\{\{-dd^{B[-1]}, 0\}, \{f[-1], dd^C\}\}}.
    Text
      A map between modules induces a map between their free resolutions,
      and we compute the associated mapping cone.
    Example
      S = ZZ/32003[x,y,z];
      M = ideal vars S
      B = freeResolution(S^1/M^2)
      C = freeResolution(S^1/M)
      f = extend(C,B,id_(S^1))
      Cf = cone f
      dd^Cf
      prune HH Cf
      assert(prune HH_1 Cf == prune(M/M^2))
    Text
      The mapping cone fits into a canonical short exact
      sequence of chain complexes:
      $$0 \to C \to cone(f) \to B[-1] \to 0.$$
    Example
      g = canonicalMap(Cf,C)
      h = canonicalMap(B[-1],Cf)
      assert(isWellDefined g and isWellDefined h)
      assert(isShortExactSequence(h,g))
    Text
      The most important application of mapping cones is to 
      identify quasi-isomorphisms: $f$ is a quasi-isomorphism 
      if and only if the mapping cone is acyclic.
    Example
      aug = augmentationMap C
      assert isWellDefined aug
      cone aug
      assert(0 == prune HH cone aug)
      assert isQuasiIsomorphism aug
    Text
      Mapping cones can also be used to construct free resolutions
      of subschemes linked via a complete intersection to a
      arithmetically Cohen-Macaulay subscheme;
      see Peskine-Szpiro, Liaison des varieties algebrique I, 
          {\it Invent. math.} {\bf 26} (1974) 271-302.
    Text
      Here, we consider a random complete intersection of 2 cubics
      contained in the ideal of the twisted cubic curve, and we
      compute a free resolution of the linked curve of degree 6.
    Example
      S = ZZ/32003[a..d]
      I = monomialCurveIdeal(S, {1,2,3})
      K = ideal((gens I) * random(source gens I, S^{-3,-3}))
      C = freeResolution(S^1/I)
      B = freeResolution(S^1/K)
      f = dual extend(C,B,id_(S^1))
      Cf = (cone f)[-2]
      prune HH Cf
      Cf' = minimize Cf
      J = ideal dd^Cf'_1
      freeResolution J
      assert(degree J == 6)
  Caveat
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    (cylinder, ComplexMap)
    (augmentationMap, Complex)
    (extend, Complex, Complex, Matrix)
    (freeResolution, Module)
    canonicalMap
    isQuasiIsomorphism
    isShortExactSequence
///

-- YYY
-- This TODO refers to the following cylinder node:
--    Text
--      YYY.  Start here.  Should we relate cylinders to connecting
--      homomorphisms?  If not, what other application can we show?

doc ///
  Key
    cylinder
    (cylinder, ComplexMap)
  Headline
    make the mapping cylinder of a morphism of chain complexes
  Usage
    cylinder f
  Inputs
    f:ComplexMap
      which is a morphism of complexes
  Outputs
    :Complex
  Description
    Text
      Given a morphism $f : B \to C$, the mapping cylinder 
      is the complex whose the $i$-th term is $B_{i-1} \oplus B_i \oplus C_i$
      and whose $i$-th differential is given in block form by
              {\tt matrix \{\{ - dd^B_{i-1}, 0, 0 \}, 
                \{ -id_{B_{i-1}}, dd^B_i, 0 \},
                \{ f_{i-1}, 0, dd^C_i\}\}}.
      Alternatively, the cylinder is the
      mapping cone of the morphism $g : B \to B \oplus C$ given in block form
      by
        {\tt matrix\{\{-id_B\}, \{f\}\}}.
    Text
      A map between modules induces a map between their free resolutions,
      and we compute the associated mapping cylinder.
    Example
      S = ZZ/32003[x,y,z];
      M = ideal vars S
      B = freeResolution(S^1/M^2)
      C = freeResolution(S^1/M)
      f = extend(C,B,id_(S^1))
      cylf = cylinder f
      dd^cylf
      assert isWellDefined cylf
    Text
      The mapping cylinder fits into a canonical short exact
      sequence of chain complexes,
      $$0 \to B \to cyl(f) \to cone(f) \to 0.$$
    Example
      Cf = cone f
      g = canonicalMap(cylf, B)
      h = canonicalMap(Cf, cylf)
      assert(isWellDefined g and isWellDefined h)
      assert(isShortExactSequence(h,g))
    Text
      The alternative interpretation of the cylinder, defined above,
      can be demonstrated as follows.
    Example
      g = map(B ++ C, B, {{-id_B},{f}})
      cone g == cylf
  SeeAlso
    "Making chain complexes"
    "Making maps between chain complexes"
    (cone, ComplexMap)
    (extend, Complex, Complex, Matrix)
    (freeResolution, Module)
    canonicalMap
    isShortExactSequence
///

-- end of doc nodes DDD

TEST ///
  -- test creation of complexes 1: via free resolutions
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/32003[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  CR = freeResolution I
  assert(ring CR === R)
  assert(length CR === 6)
  assert(isWellDefined CR)
  betti'ans = new BettiTally from {
      (0,{0},0) => 1, 
      (1,{2},2) => 8, 
      (2,{3},3) => 2, 
      (2,{4},4) => 31, 
      (3,{5},5) => 32,
      (3,{6},6) => 28, 
      (4,{6},6) => 3, 
      (4,{7},7) => 58, 
      (5,{8},8) => 32, 
      (6,{9},9) => 4, 
      (6,{10},10) => 1
      }
  assert(betti CR === betti'ans)
  assert(isWellDefined dd^CR)
  assert(CR_0 === R^1)
  assert(CR_-1 == 0)
  assert(rank CR_6 == 5)
  assert(CR_7 == 0)
  assert((0,6) == concentration CR)
  assert(isHomogeneous CR)
  assert(source dd^CR == CR)
  assert(target dd^CR == CR)  
  assert(degree dd^CR == -1)

  fC = map(CR[-1], CR, dd^CR, Degree=>0);

  assert(isWellDefined fC)
  assert(degree fC == 0)
  assert(source fC == CR)
  assert(target fC == CR[-1])
///

TEST ///
  -- test creation of complexes 2: from modules
-*
  restart
  needsPackage "Complexes"
*-

  S = ZZ/101[a..d]
  C0 = complex S^2
  f = dd^C0
  assert(source f == C0)
  assert(target f == C0)
  assert(degree f == -1)
  assert(f == 0)
  assert isWellDefined C0
  assert(C0 != 0)
  assert(length C0 == 0)
  assert(concentration C0 == (0,0))
  assert(not isExact C0)
  assert(isExact(C0, -3, -2))

  C1 = complex(S^2, Base=>3)
  assert(ring C1 === S)
  assert(C1 == C0[-3])
  assert(C1_3 == S^2)
  assert(C1_0 == 0)
  assert(concentration C1 == (3,3))

  C2 = complex S
  assert(ring C2 === S)
  I = ideal(a^2-b, c^3)
  C3 = complex I
  C4 = complex (S/I)
  assert(ring C3 === S)
  assert(ring C4 === S/I)
  assert(length C3 == 0)
  assert(length C4 == 0)
  
  C5 = complex S^0
  assert(C5 == 0)
  assert(0 == C5)
  assert(dd^C5 == 0)
  assert(C5_0 == 0)
  assert(ring C5 === S)
  assert(concentration C5 == (0,0))
  assert(concentration(C5[4]) == (-4,-4))
  assert(concentration prune(C5[4]) == (0,0))
  assert(isExact C5)

  R = QQ
  C = complex QQ
  D = C[3] ++ (complex QQ^2)
  assert(dd^D == 0)
  assert(D_-3 === D^3)
  assert(concentration C == (0,0))
  assert(concentration D == (-3,0))
///

TEST ///
  -- isWellDefined
  R = QQ[a..d];
  f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
  f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
  C = complex {f0, f1}
  assert isWellDefined C
  assert((dd^C)^2 == 0)
  assert(HH C != complex coker f0)
  assert(prune HH C == complex coker f0)

  -- a non-example
  g1 = map(source f0,, {{-d, c}, {c, b}, {b, a}})
  C = complex {f0, g1}
  assert not isWellDefined C
  assert((dd^C)^2 != 0)
///
TEST ///
  -- test creation of complexes 3: via constructors
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = ideal(b^2-a*c, b*c-a*d, c^2-b*d)
  F1 = map(S^1,,matrix{{I_0, I_1, I_2}})
  F2 = map(source F1,,matrix{
         {0, I_2, -I_1},
         {-I_2, 0, I_0},
         {I_1, -I_0, 0}
         })
  F3 = map(source F2,,matrix{{I_0}, {I_1}, {I_2}})
  C = complex hashTable{1 => F1, 2 => F2, 3 => F3}
  assert isWellDefined C
  assert(ring C === S)
  assert(concentration C == (0,3))

  assert(rank C_2 == 3)
  assert(C^(-1) == C_1)
  assert(C_7 == 0)
  assert isWellDefined dd^C
  assert(dd^C_2 == F2)
  assert(length C == 3)

  G = gradedModule C
  assert(isWellDefined G)
  assert(G.dd == 0)
  assert(source G.dd == G)
  assert(target G.dd == G)
  assert(degree G.dd == -1)
  assert(concentration G == concentration C)
  (lo,hi) = concentration C
  for i from lo to hi do assert(C_i === G_i)

  G = gradedModule(complex S^0)
  assert(G == 0)
  assert(isWellDefined G)
  assert(G.dd == 0)
  assert(source G.dd == G)
  assert(target G.dd == G)
  assert(degree G.dd == -1)
  assert(concentration G == (0,0))

  C0 = complex({S^1, S^3, S^{-2}}, Base=>4)
  G = gradedModule C0
  assert(C0 == G)
  assert(isWellDefined C0)
  assert(isWellDefined G)
  
  concentration G
  C1 = complex{F1,F2,F3}
  assert isWellDefined C1
  assert(C1 == C)

  C2 = complex({F1,F2,F3}, Base => 3)
  assert(C2 != C1[-3])
  assert(concentration (C1[-3]) == concentration C2)
  assert(dd^C2_2 + dd^(C1[-3])_2 == 0)

  assert(HH C != 0)
  assert(prune HH_0 C == comodule I)
///

TEST ///
-*
restart
needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  C = freeResolution coker matrix{{a,b^2,c^3,d^4}}
  assert not isExact C
  assert(isExact(C,1,infinity))
  assert(not isExact(C,-infinity,infinity))
  assert(regularity C == 6)
  assert(length C == 4)
  f = poincare C
  use ring f
  assert(f == 1-T-T^2+2*T^5-T^8-T^9+T^10)
  p = poincareN C 
  use ring p
  assert(p == 1+S*T_0+S*T_0^2+S*T_0^3+S*T_0^4+S^2*T_0^3+S^2*T_0^4+2*S^2*T_0^5+S^2*T_0^6+
      S^2*T_0^7+S^3*T_0^6+S^3*T_0^7+S^3*T_0^8+S^3*T_0^9+S^4*T_0^10)
  D = C[3]
  assert(poincare D == -f)
  assert(ring poincareN C === ring p)
///

TEST ///
-- test of sum of a Complex, TODO: ComplexMap
-*
restart
needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  C = freeResolution coker matrix{{a,b^2,c^3,d^4}}
  F = sum C
  assert(degrees F == 
      {{0}, {1}, {2}, {3}, {4}, {3}, {4}, {5}, {5}, {6}, {7}, {6}, {7}, {8}, {9}, {10}}
      )
  D = freeResolution coker matrix{{a,b^3,c^3,c*d^4}}
  f = extend(C,D,id_(S^1))
  sum f
  g = map(C[3], D, f, Degree=>-3)
  assert isWellDefined g
  sum g
///

TEST ///
  -- Complex Array
  S = ZZ/101[a..d]
  C = freeResolution coker vars S
  dd^C_3
  D = C[1]
  assert(dd^D_2 == -dd^C_3)

  (lo,hi) = concentration C
  E = complex(for i from lo+1 to hi list dd^C_i, Base=>-1)
  assert(dd^E_2 == dd^C_3)
///

TEST ///
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  assert(concentration C == (0,3))
  D = C ++ C[5]
  assert(concentration D == (-5,3))

  assert(C_-1 == 0)
  assert(D_4 == 0)

  f1 = a*id_C
  E = ker f1
  assert(concentration E == (0,3))
  assert(concentration prune E == (0,0))
  assert(prune E == 0)
  assert(E == 0)

  C0 = (complex S^0)[4]
  assert(concentration C0 == (-4,-4))
  assert(prune C0 == 0)
  assert(concentration prune C0 == (0,0))
///

TEST ///
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  D = C[0]
  assert(C === D)

  (lo,hi) = concentration C
  E = complex for i from lo+1 to hi list 0*dd^C_i
  assert(dd^E == 0)
  assert(degree map(C,C,0*dd^C) == -1)
  assert(C != E)
  assert(E != 0)
  f = id_C
  D = coker f
  assert(D == 0)
///

TEST ///
  -- test of equality of complexes, mainly from doc examples
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  D = C[0]
  assert(C === D)

  (lo,hi) = concentration C
  assert((lo,hi) == (0,3))
  E = complex for i from lo+1 to hi list 0*dd^C_i
  assert all(toList (lo..hi), i -> C_i == E_i)
  isWellDefined E
  assert(dd^E == 0)
  assert(C != E)
  assert(E != 0)

  f = id_C
  D = coker f
  assert(D == 0)
  assert(D =!= 0)

  C0 = complex S^0
  C1 = C0[4]
  assert(concentration C0 == (0,0))
  assert(concentration C1 == (-4,-4))
  assert(C0 == C1)
  assert(C0 == 0)
  assert(C1 == 0)
  assert(concentration prune C1 == (0,0))
  
  R = QQ[a..d];
  f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
  f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
  C = complex {f0, f1}
  assert isWellDefined C
  assert(HH C != complex coker f0)
  assert(prune HH C == complex coker f0)
///

TEST ///
  -- test of homology, mainly from doc examples
  d1 = matrix {
      {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
      {-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
      {0, -1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0}, 
      {0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 1, 0}, 
      {0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, 0, 1}, 
      {0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, -1}}
  d2 = matrix {
      {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, 
      {0, 0, -1, -1, 0, 0, 0, 0, 0, 0}, 
      {1, 0, 1, 0, 0, 0, 0, 0, 0, 0}, 
      {0, 1, 0, 0, -1, 0, 0, 0, 0, 0}, 
      {0, 0, 0, 1, 1, 0, 0, 0, 0, 0}, 
      {0, 0, 0, 0, 0, -1, -1, 0, 0, 0}, 
      {-1, 0, 0, 0, 0, 0, 0, -1, 0, 0}, 
      {0, -1, 0, 0, 0, 1, 0, 0, 0, 0}, 
      {0, 0, 0, 0, 0, 0, 1, 1, 0, 0}, 
      {0, 0, -1, 0, 0, 0, 0, 0, -1, 0}, 
      {0, 0, 0, 0, 0, -1, 0, 0, 1, 0}, 
      {0, 0, 0, -1, 0, 0, -1, 0, 0, 0}, 
      {0, 0, 0, 0, 0, 0, 0, 0, -1, -1}, 
      {0, 0, 0, 0, 0, 0, 0, -1, 0, 1}, 
      {0, 0, 0, 0, -1, 0, 0, 0, 0, -1}}
  C = complex {d1,d2}
  assert isWellDefined C
  assert(concentration C == (0,2))
  H = HH C
  assert(dd^H == 0)
  D = complex{map(ZZ^1, ZZ^1/(ideal 2), 0)}
  assert(prune H == D)

  S = ZZ/101[a..d, DegreeRank=>4];
  I = intersect(ideal(a,b),ideal(c,d))
  C = dual freeResolution (S^1/I)
  Hd = prune HH C
  assert(isWellDefined Hd)
  assert(dd^Hd == 0)
  M = cokernel map(
      S^{{1,1,0,0},{0,0,1,1}},
      S^{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},
      {{b, a, 0, 0}, {0, 0, d, c}})
  assert(prune HH^2 C == M)
  assert(prune HH^1 C == 0)
  assert(prune HH^3 C == coker (vars S ** S^{{1,1,1,1}}))
///

TEST ///
  -- tests of direct sum
  S = ZZ/101[a,b,c];
  C1 = freeResolution coker vars S
  C1 ++ complex(S^13)[-2]
  C2 = complex (ideal(a,b,c))
  C1 ++ C2
  C4 = directSum(first => C1, second => C2)
  C4_[first] -- inclusion map C1 --> C4
  C4^[first] -- projection map C4 --> C1
  assert(C4^[first] * C4_[first] == 1)
  assert(C4^[second] * C4_[second] == 1)
  assert(C4^[first] * C4_[second] == 0)
  assert(C4^[second] * C4_[first] == 0)
  assert(C4_[first] * C4^[first] + C4_[second] * C4^[second] == 1)

  -- test zero complexes
  C0 = complex(S^0)
  C5 = (first => C1) ++ (second => C0)
  assert(C5^[first] * C5_[first] == 1)
  assert(C5^[second] * C5_[second] == 1)
  assert(C5^[first] * C5_[second] == 0)
  assert(C5^[second] * C5_[first] == 0)
  assert(C5_[first] * C5^[first] + C5_[second] * C5^[second] == 1)
///

TEST ///
  -- test of components, mainly from doc examples
  S = ZZ/101[a,b,c];
  C1 = freeResolution coker vars S
  C2 = complex (ideal(a,b,c))
  D = C1 ++ C2
  L = components D
  assert(L_0 === C1)
  assert(L_1 === C2)
  E = (mike => C1) ++ (greg => C2)
  assert(components E === L)

  assert(indices D == {0,1})
  assert(D^[0] == E^[mike])
  assert(indices E == {mike, greg})
  assert(E_[greg] == D_[1])
  
  C3 = complex S^0
  F = C3 ++ C2
  assert(indices F == {0,1})
  assert(prune F == prune C2)
///

TEST ///
  -- test of length, mainly from doc examples
  S = ZZ/101[a,b,c,d];
  C1 = freeResolution coker vars S
  assert(ring C1 === S)
  assert(length C1 == 4)
  assert(length(C1[5]) == 4)
  assert(length (C1 ++ C1[6]) == 10)

  M1 = S^1/(a*b, c*d, a*c, b*c)
  M2 = S^1/(a*b, c*d, a*c)
  C4 = freeResolution M1
  C5 = freeResolution M2
  f = map(M1, M2, 1)
  C6 = coker extend(C4, C5, matrix f)
  assert(concentration C6 == (0,3))
  assert(length C6 == 2)
  assert(length prune C6 == 2)
  assert(concentration prune C6 == (1,3))
///

TEST ///
  -- isHomogeneous, mainly from doc examples
  S = ZZ/101[a,b,c,d];
  I = minors(2, matrix{{a,b,c},{b,c,d}})
  C = freeResolution (S^1/I)
  assert isHomogeneous C
  J = minors(2, matrix{{a,b,c},{b,c,d^2}})
  D = freeResolution (S^1/J)
  assert not isHomogeneous D
///

TEST ///
  -- tensor product of complexes
  S = ZZ/101[a..c]
  Ca = complex {matrix{{a}}}
  Cb = complex {matrix{{b}}}
  Cc = complex {matrix{{c}}}
  Cab = Cb ** Ca
  dd^Cab
  assert isWellDefined Cab
  assert(prune HH Cab == complex coker matrix{{b,a}})
  assert(indices Cab_1 == {{0, 1}, {1, 0}})
  for i from 0 to 2 do assert (rank Cab_i == {1,2,1}_i)

  Cabc = Cc ** Cab
  assert isWellDefined Cabc
  assert(prune HH Cabc == complex coker matrix{{c,b,a}})
  assert(indices Cabc_1 == {{0, 1}, {1, 0}})
  for i from 0 to 3 do assert (rank Cabc_i == {1,3,3,1}_i)

  Cabc2 = (Cc ** Cb) ** Ca
  assert(Cabc2 == Cabc)
  assert(Ca ** Cb != Cab)

  D = (Cabc ** (S^1/(a,b,c)))
  assert(dd^D == 0)
  S^2 ** Cabc

  assert(Cab[1] == complex(S^1, Base=>-1) ** Cab)
  assert(Cab[-4] == complex(S^1, Base=>4) ** Cab)
  
  F = dd^Cc ** id_Cab
  G = - id_Cc ** dd^Cab
  source F == source G
  target F == target G
  source F == Cabc
  target F == Cabc
  F+G
  dd^Cabc
  -- WARNING TODO: are the signs consistent here?
///

TEST ///
  -- test of Hom of complexes, mainly from doc
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  D = Hom(C,C)
  dd^D
  assert(prune HH D == Hom(C, coker vars S))
  E = Hom(C, S^1)
  assert(prune HH E == complex(coker matrix{{c,b,a}} ** S^{3}, Base=>-3))
///

TEST ///
  -- f: C --> D
  -- cone(Hom(f,D)) == Hom(cone(f),D[1])
  S = ZZ/101[a..e]
  I = ideal(a*b,c*d,a*e)
  J = ideal(a*b,c*d)
  FI = freeResolution I
  FJ = freeResolution J
  g = extend(FI,FJ,id_(S^1))
  f = Hom(g, complex S^1)
  assert isWellDefined f
  E1 = cone Hom(f, target f)
  E2 = Hom(cone(f),(target f)[-1])
  -- TODO: it would be very nice if these would be equal on the nose.
  -- Can this be made to happen?
  E1 == E2
  (dd^E1_1, dd^E2_1)
///

TEST ///
  -- cone f.  Trivial and strange cases
  f1 = map(complex ZZ^1, complex ZZ^0, 0)
  f2 = map(complex ZZ^0, complex ZZ^1, 0)
  f3 = map(complex ZZ^1, complex(ZZ^0,Base=>-1), 0)
  f4 = map(complex ZZ^0, complex(ZZ^1,Base=>-1), 0)
  f5 = map(complex(ZZ^1, Base=>4), complex(ZZ^0,Base=>-3), 0)
  f6 = map(complex(ZZ^0, Base=>4), complex(ZZ^1,Base=>-3), 0)
  concentration source f5
  concentration target f5
  cone f1
  cone f2
  cone f3 
  cone f4 
  cone f5 -- lot's of zeros.... prune to get rid of zeros
  prune cone f5
  cone f6 -- lot's of zeros.... prune to get rid of zeros.
  prune cone f6
  assert(cone f5 == complex(ZZ^1, Base=>4))
///

TEST ///
  -- how to find a morphism of complexes
-*
restart
*-
  needsPackage "Complexes"
  -- Hom(C,D) --> f : C --> D
  S = ZZ/101[a..e]
  I = ideal(a*b,c*d,a*e)
  J = ideal(a*b,c*d)
  D = freeResolution I
  C = freeResolution J
  E = Hom(C,D)

  KE = ker dd^E_0
  g = a^2**KE_{0} + b**KE_{1}
  assert isHomogeneous g
  f = homomorphism(0, g, E)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isCommutative f
  assert isHomogeneous f
  assert(source f === C)
  assert(target f === D)

  f = randomComplexMap(D,C)
  assert isWellDefined f
  assert isHomogeneous f
  assert(degree f === 0)

  f = randomComplexMap(D,C,Degree=>-2)
  assert isWellDefined f
  assert isHomogeneous f
  assert(degree f === -2)

  f = randomComplexMap(D,C,Degree=>2)
  assert isWellDefined f
  assert isHomogeneous f
  assert(degree f === 2)
  assert(f == 0)

  f = randomComplexMap(D,C, Cycle=>true)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isCommutative f
  assert isHomogeneous f

  f = randomComplexMap(D,C,InternalDegree=>-1)
  assert isWellDefined f
  assert isHomogeneous f

  f = randomComplexMap(D,C ** S^{-1})
  assert isWellDefined f
  assert isHomogeneous f

  f = randomComplexMap(D, C ** S^{-1}, Cycle=>true)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isHomogeneous f

  f = randomComplexMap(D, C ** S^{-1}, Cycle=>true, InternalDegree=>1)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isHomogeneous f

  f = randomComplexMap(D, C, Cycle=>true, InternalDegree=>1)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isHomogeneous f

  C1 = C ** S^{-1}
  f = randomComplexMap(D, C1, Cycle=>true, Degree=>-1)
  assert isWellDefined f
  assert isCommutative f
  assert isHomogeneous f
  degree f
  f * dd^C1 + dd^D * f

  C1 = C ** S^{-1}
  f = randomComplexMap(D, C1, Cycle=>true, InternalDegree=>1, Degree=>-1)
  assert isWellDefined f
  assert isCommutative f
  assert isHomogeneous f
  degree f
  f * dd^C1 + dd^D * f
  assert(degree f_1 === {1})
  assert(degree f === -1)

  C1 = C ** S^{-1}
  f = randomComplexMap(D, C1, Boundary=>true)
  assert isNullHomotopic f
  h = nullHomotopy f
  assert isNullHomotopyOf(h,f)
  assert isWellDefined f
  assert isWellDefined h
  
  f2 = randomComplexMap(D, C1, Cycle=>true)
  assert not isNullHomotopic f2
  h2 = nullHomotopy f2
  assert not isNullHomotopyOf(h2,f2)
  assert isWellDefined f2
  assert isWellDefined h2
  assert not isCommutative h2

  E = Hom(C ** S^{-1}, D)
  B = basis(0,ker dd^E_0)
  mors = for i from 0 to numColumns B-1 list homomorphism(0, B_{i}, E)
  -- maps which are null-homotopic:
  bd = basis(0, image dd^E_(-1))
  bd = image dd^E_1
  -- I want the map: bd -->E_0, so I can compose: 
  map(E_0, bd, gens bd)
  bds = for i from 0 to numgens bd-1 list homomorphism(0, map(E_0, bd, gens bd) * bd_{i}, E)
  for f in mors do assert(isComplexMorphism f)
  for f in bds do assert(isComplexMorphism f)

  h = nullHomotopy bds_0
  isNullHomotopyOf(h, bds_0)

  isNullHomotopic bds_0

  for f in bds do (
      h := nullHomotopy f;
      assert(f == h * dd^(source h) + dd^(target h) * h)
      );
  for f in bds list (
      h := nullHomotopy f;
      assert isNullHomotopyOf(h, f)
      )
  
  assert(homomorphism(0, B_{0} + B_{5} + B_{6} + B_{7}, E) == mors_0 + mors_5 + mors_6 + mors_7)
  
  prune HH_0(E)
///

TEST ///
-*
restart
*-
  needsPackage "Complexes"
  -- Hom(C,D) --> f : C --> D
  S = ZZ/101[a..e]
  I = ideal(a*b,c*d,a*e)
  J = ideal(a*b,c*d)
  D = freeResolution I
  C = freeResolution J
  E = Hom(C,D)
  f = homomorphism(1,E_1_{2},E)
  assert isWellDefined f
  assert isWellDefined homomorphism(0, (E_0)_{7}, E)
  assert isWellDefined homomorphism(1, a * (E_1)_{6}, E)
  assert isWellDefined homomorphism(-1, a * (E_-1)_{1}, E)
  assert isHomogeneous f
  assert isHomogeneous homomorphism(0, (E_0)_{7}, E)
  --assert isHomogeneous homomorphism(1, a ** (E_1)_{6}, E)
  --assert isHomogeneous homomorphism(-1, a ** (E_-1)_{1}, E)

  fh = homomorphism' f
  isWellDefined fh

  -- ZZZ
  -- to do: 
  -- (1) cache tensor, check the signs in tensor products
  -- (2) TEST homomorphism'
  -- (3) now use this code to check signs for Hom
  h = E_1_{2}
  g = map(E, (complex source h)[-1], hashTable {1 => h})
  f1 = homomorphism g -- this should give a homomorphism f : C --> D of degree 1
  assert(f1 == f)

  assert(HH f1 == 0)
  assert isWellDefined HH f1
  prune HH f1 -- not yet
  
  E = Hom(C,D)
  -- the next test makes sure that Hom is being cached in the youngest complex (here that is C).
  homs = select(keys C.cache, x -> instance(x, Sequence) and first x === Hom)
  assert(#homs === 1 and homs#0 === (Hom, C, D))

  -- f|g, f||g
  f = homomorphism(1,E_1_{2},E)
  g = homomorphism(1,E_1_{3},E)
  target f === target g
  source f === source g

  h1 = f || g  
  assert not h1.cache.?isCommutative
  assert isWellDefined h1
  assert not isCommutative h1
  assert h1.cache.?isCommutative
  assert not h1.cache.isCommutative
  assert(source h1 === C)
  assert(target h1 == D ++ D)
  h1.cache.isCommutative = true;
  debugLevel = 1
  assert not isWellDefined h1
  debugLevel = 0
  h1.cache.isCommutative = false; -- set it back to be the correct value
  
  h2 = f | g
  assert isWellDefined h2
  assert not isCommutative h2
  assert(target h2 === D)
  assert(source h2 === C ++ C)

  KE = ker dd^E_0
  g = a^2**KE_{0} + b**KE_{1}
  assert isHomogeneous g
  f = homomorphism(0, g, E)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isCommutative f
  assert isHomogeneous f
  assert(source f === C)
  assert(target f === D)
  f.cache.isCommutative = false
  debugLevel = 1
  assert not isWellDefined f
  debugLevel = 0
  f.cache.isCommutative = true
  assert isWellDefined HH f
  assert(HH f != 0)

  -- test map(Complex,Complex,Function)
  assert(map(target f, source f, i -> f_i)  == f)
  -- test of prune ComplexMap
  assert(prune f == f)
  g = canonicalMap(target f, image f)
  g' = prune g
  assert isWellDefined g'
  source g' == source f
  assert(target g' == target f)

  f1 = f | f
  assert isWellDefined f1
  assert isComplexMorphism f1
  assert isCommutative f1
  --assert isHomogeneous f1 -- fails, see github issue #607.
  assert(source f1 === C ++ C)
  assert(target f1 === D)
  degrees source f1_1
  degrees source f_1
  degrees target f1_1
  degrees target f_1
  degree f_1

  dual target f1 
  dual source f1
  Hom(f1, complex ring f1)
  f2 = Hom(f1, S^1)
  assert isWellDefined f2
  isWellDefined HH f2

  H' = Hom(dual D, dual C)
  f = homomorphism(1,H'_0_{7},H')
  isCommutative f  
  assert try (HH f; false) else true
  KH' = ker dd^H'_0
  g = homomorphism(0, c**KH'_{0},  H')
  assert(isWellDefined HH g)
  assert(HH g != 0)

  -- Test of tensor product with a ring  
  C
  R = S/(a*b)
  CR = C ** R
  assert isCommutative g
  assert isWellDefined CR
  gR = g ** R
  assert isWellDefined gR
  assert isCommutative gR
  
  -- now do ring maps
  phi = map(R,S)
  CR1 = phi C
  assert(isWellDefined CR1)
  assert(CR1 == CR)
  gR1 = phi g 
  assert isWellDefined gR1
  assert(gR1 == gR)
  
  -- ZZZ, should more tests in
///

TEST ///
  -- This doesn't belong in this package, move it to tests elsewhere once the
  -- github issue has been handled.
  
  -- Made a github issue of this (15 Nov 2016, #535)
  -- Problem: have === for free modules, where one
  -- has components and one doesn't.
  -- Would be nice if: target ambient g
  -- actually had these components.  
  -- Why doesn't it?
  S = ZZ/101[a..d]
  M = S^1 ++ S^2
  F = S^2
  f = random(F,M)
  K = kernel f
  assert(target generators K == M)
  assert(target generators K === M)
  --assert(components target generators K == components M) -- fails
  g = K_{0}
  ambient g
  target ambient g === M
  components M
  components target ambient g
  --assert(components M == components target ambient g) -- fails
  -- Need/want to cache Hom
  S = ZZ/101[a..d]
  M = module ideal(a,b,c)
  N = S^2
  H = Hom(M,N)
  peek M.cache
  peek N.cache.cache -- why does N.cache have its own cache table?
  -----------------------
///

TEST ///
  -- resolution of a module thought of as a complex
-*  
  restart
  needsPackage "Complexes"
*-  
  
  R = ZZ/101[a,b,c,d,e]
  
  -- case 1: free module:
  M = complex(R^3, Base=>1)
  f = resolutionMap M
  assert(isWellDefined f)
  assert(target f == M)
  assert(f_1 == 1)

  -- case 2: cokernel module
  M0 = coker vars R
  M = complex(M0)
  f = resolutionMap M
  assert(target f == M)
  assert(isWellDefined f)
  assert(source f == freeResolution M0)
  assert(prune HH source f == M) -- this might not need to be true in general, but is true here.

  -- case 3: image module
  M0 = image vars R
  M = complex(M0)
  f = resolutionMap M
  assert(target f == M)
  assert(isWellDefined f)
  assert(source f == freeResolution M0)
  assert(prune HH source f == prune M) -- this might not need to be true in general, but is true here.

  -- case 4: subquotient module
  M0 = image vars R ++ coker vars R
  M = complex(M0)
  f = resolutionMap M
  assert(target f == M)
  assert(isWellDefined f)
  assert(source f == freeResolution M0)
  assert(prune HH source f == prune M) -- this might not need to be true in general, but is true here.
///

TEST ///
  -- resolution of a complex
-*  
  restart
  needsPackage "Complexes"
*-  
  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = (dual freeResolution I) [-4]
  f = resolutionMap C
  assert(target f === C)
  assert(isWellDefined f)
  assert(isQuasiIsomorphism f)
  assert(isQuasiIsomorphism(f, Concentration=>(-5,3)))
  assert(isQuasiIsomorphism(f, Concentration=>(3,3)))
  assert(isComplexMorphism f)
  assert(coker f == 0)
  assert(kernel HH f == 0)
  assert(cokernel HH f == 0)
  assert(resolution C == source f)
///

TEST ///
  -- naive truncation
-*  
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = freeResolution I
  naiveTruncation(C,1,2)
  naiveTruncation(C,1,6)
  naiveTruncation(C,-13,2)
  naiveTruncation(C,-infinity,2)
  assert try (naiveTruncation(C,4,3); false) else true
  naiveTruncation(C,4,infinity)
  assert try (naiveTruncation(C,-infinity,infinity); false) else true -- method doesn't even exist.

  g = naiveTruncation(id_C, (0,2), (1,3))
  assert isWellDefined g
  assert not isComplexMorphism g
  
  g = naiveTruncation(id_C, (1,3))
  assert isWellDefined g
  assert isComplexMorphism g

  g = naiveTruncation(id_C, (0,1), (2,3))
  assert isWellDefined g
  assert isComplexMorphism g
  assert(g == 0)
///

TEST ///
  -- canonical truncation
-*  
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = freeResolution I
  C1 = canonicalTruncation(C,1,2)
  assert isWellDefined C1
  C2 = canonicalTruncation(C,1,6)
  assert(C2 == canonicalTruncation(C,1,))
  C3 = canonicalTruncation(C,-13,2)
  assert(C3 == canonicalTruncation(C,,2))
  C4 = canonicalTruncation(C,-infinity,2)
  assert(C3 == C4)
  assert try (canonicalTruncation(C,4,3); false) else true
  canonicalTruncation(C,4,infinity)
  canonicalTruncation(C,-infinity,infinity) == C

  g = canonicalTruncation(id_C, (1, infinity))
  assert isWellDefined g
  assert isComplexMorphism g

  g = canonicalTruncation(id_C, (0,2))
  assert isWellDefined g
  assert isComplexMorphism g
  
  g = canonicalTruncation(id_C, (1,3))
  assert isWellDefined g
  assert isComplexMorphism g

  g = canonicalTruncation(id_C, (1,1))
  assert isWellDefined g
  assert isComplexMorphism g

///


TEST ///
  -- canonical truncation, more interesting example(s)
-*  
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  J = ideal(a^2, b^2, a*d, b*c^2)
  K = intersect(ideal(a,b,c^2),ideal(c,d,e))
  
  I = intersect(ideal(a,b), ideal(c,d,e))
  J = intersect(ideal(a,b,c^2), ideal(c,d,e))
  K = intersect(ideal(a,b,c^2), ideal(b^2,c,d,e))
  C = dual freeResolution I
  D = dual freeResolution J
  E = dual freeResolution K
  HCD = Hom(C,D)
  prune HH^0(HCD)
  HDE = Hom(D,E)
  prune HH^0(HDE)
  HCE = Hom(C,E)
  prune HH^0(HCE)  
  ZHCD0 = ker dd^HCD_0;
  ZHDE0 = ker dd^HDE_0;
  ZHCE0 = ker dd^HCE_0;
  f0 = map(ZHCD0, R^1,  random(R^(numgens ZHCD0), R^1))
  f = homomorphism(0, f0, HCD)
  isWellDefined f
  isNullHomotopic f
  g0 = map(ZHDE0, R^1,  random(R^(numgens ZHDE0), R^1))
  g = homomorphism(0, g0, HDE)
  isWellDefined g
  isNullHomotopic g
  h = g * f
  isWellDefined h
  isNullHomotopic h
  f' = canonicalTruncation(f, (-3,-1));
  g' = canonicalTruncation(g, (-3,-1));
  h' = canonicalTruncation(h, (-3,-1));
  assert(g' * f' == h')

  f' = prune canonicalTruncation(f, (-3,-3));
  g' = prune canonicalTruncation(g, (-3,-3));
  h' = prune canonicalTruncation(h, (-3,-3));
  assert(g' * f' == h')
  f' = canonicalTruncation(f, (-3,-3));
  g' = canonicalTruncation(g, (-3,-3));
  h' = canonicalTruncation(h, (-3,-3));
  assert(g' * f' == h')

  f' = canonicalTruncation(f, (-4,-4));
  g' = canonicalTruncation(g, (-4,-4));
  h' = canonicalTruncation(h, (-4,-4));
  assert(g' * f' == h')

  f' = canonicalTruncation(f, (-4,-2));
  g' = canonicalTruncation(g, (-4,-2));
  h' = canonicalTruncation(h, (-4,-2));
  assert(g' * f' == h')

  g = canonicalTruncation(f, (-3,-2))
  isWellDefined g
  isComplexMorphism g
  
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  kk = ZZ/101
  S = kk[a..d]
  I = ideal"a2-bc,ab-cd"
  C = freeResolution(I, FastNonminimal=>true)
  dd^C
  minC = minimize C
  minC.dd

  kk = ZZ/101  
  S = kk[a..e]
  F = random(3,S)
  I = inverseSystem F
  C = freeResolution(I, FastNonminimal=>true)
  minC = minimize C
  betti minC  
  minimize(C ++ C[9])
///

TEST ///
  -- resolution of a complex
-*  
  restart
  debug needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = Hom(freeResolution I, R^1/I)
  assert not isFree C
  elapsedTime f = resolutionMap C;
  assert(target f === C)
  assert(isWellDefined f)
  assert(isQuasiIsomorphism f)
  assert(isComplexMorphism f)
  assert(coker f == 0)
  assert(kernel HH f == 0)
  assert(cokernel HH f == 0)
  assert(resolution C == source f)
  assert(isFree source f)
  D = resolution C
  prune HH C

  MD = minimize D;
  assert(MD == D)
  assert isWellDefined MD
  p1 = MD.cache.minimizingMap;
  assert isWellDefined p1
  assert isComplexMorphism p1
  assert isQuasiIsomorphism p1
  
  assert(dd^MD ** coker vars R == 0)
  assert(dd^D ** coker vars R == 0)

  I = ideal"b2-ac,c2-bd,bcd-ad2"
  C = Hom(freeResolution I, R^1/I)
  elapsedTime D = resolution C;
  
  C1 = complex for i from -2 to 0 list dd^D_i
  isWellDefined C1
  dd^C1 ** coker vars ring D == 0
  D1 = minimize C1
  p1 = D1.cache.minimizingMap;
  assert isWellDefined p1
  assert isComplexMorphism p1
  assert isQuasiIsomorphism p1  
///

TEST ///
-*
  -- of minimize
  restart
  needsPackage "Complexes"
*-
  kk = ZZ/32003
  S = kk[a..e]
  F = random(3,S)
  I = inverseSystem F
  C = freeResolution(I, FastNonminimal=>true)
  elapsedTime minimize C

  S = kk[vars(0..8)]
  F = random(3,S)
  I = inverseSystem F;
  C = freeResolution(I, FastNonminimal=>true)
  ---- elapsedTime minimize C  -- very slow TODO: fix this

  -- good benchmark test:
  kk = ZZ/32003
  S = kk[vars(0..6)]
  F = random(3,S)
  I = inverseSystem F;
  C = freeResolution(I, FastNonminimal=>true)
  ---- elapsedTime minimize C  -- very slow, TODO: fix this
-*
  needsPackage "PruneComplex"  
  needsPackage "ChainComplexExtras"
  C' = res(I, FastNonminimal=>true)
  elapsedTime pruneComplex(C', UnitTest=>isScalar) -- 17.7 sec on my MBP
  C'' = res(ideal I_*, FastNonminimal=>true)  
  elapsedTime minimize C''  -- very slow, TODO: fix this
*-  
///

TEST ///  
-*
  restart
  debug needsPackage "Complexes"
*-
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = Hom(freeResolution I, R^1/I)
  f = resolutionMap C
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

  R = ZZ/101[a,b,c,d]
  I = monomialCurveIdeal(R,{1,2,3})
  C = freeResolution I
  f = resolutionMap C

  -- the point of this example: the map is not the identity map, due to some
  -- "strange" choice of signs... 
  
  assert(target f === C)
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

  -- a trivial complex
  debug needsPackage "Complexes"
  R = ZZ/101[a,b,c,d,e]
  C = complex {map(R^1, R^1, 1)}
  f = resolutionMap C
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

-*
  -- this computation is way too slow.
  needsPackage "Complexes"
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  C = freeResolution I
  f = resolutionMap C -- this is slow.
*-
  
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  
  
-*  
  needsPackage "SVDComplexes"
*-
  
///


TEST ///  
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a,b,c,d,e]
  C = complex {id_(S^1)}
  f = resolutionMap C
  assert(target f === C)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

  R = ZZ/101[a,b,c,d]
  I = monomialCurveIdeal(R,{1,2,3})
  C = freeResolution I
  f = resolutionMap C
  assert(target f === C)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  
///

TEST ///  
  -- resolutions and lifting maps
-*
  restart
  debug needsPackage "Complexes"
*-
  S = ZZ/101[a,b,c,d,e]
  J = ideal(a*b, b*c*d, a*e, c*e)
  FJ = freeResolution J
  N = complex (S^1/J)
  f = map(N, FJ, hashTable{0=> map(N_0, FJ_0, 1)})
  isWellDefined f
  assert(liftMapAlongQuasiIsomorphism(f,f) == 1)

  -- test #2
  -- take a random morphism between two non-free complexes
  -- obtain the correesponding map between their resolutions.

  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  FI = freeResolution I

  C = prune Hom(FI, S^1/I)
  D = prune Hom(FJ, S^1/J)
  
  fC = resolutionMap C
  fD = resolutionMap D

  g = randomComplexMap(D,C,Cycle=>true)
  g' = liftMapAlongQuasiIsomorphism(g * fC, fD)
  g'.cache.homotopy
  assert not isQuasiIsomorphism g
  assert isWellDefined g'
  assert isCommutative g'
  assert(degree g' == 0)
  assert(g * fC == fD * g')
  h = g'.cache.homotopy
  assert isWellDefined h
  assert(degree h == 1)
  assert isNullHomotopyOf(h, g*fC-fD*g')
    -- warning: since h is 0 here, we could still be off by a sign.

  -- test #3
  C1 = C ** S^{-1}
  g = randomComplexMap(D,C1,Cycle=>true)
  fC1 = resolutionMap C1
  fD = resolutionMap D
  g' = liftMapAlongQuasiIsomorphism(g * fC1, fD)
  assert not isQuasiIsomorphism g
  assert isWellDefined g'
  assert isComplexMorphism g'
  assert(g * fC1 == fD * g')
  h = g'.cache.homotopy
  assert isWellDefined h
  assert(degree h == 1)
  assert isNullHomotopyOf(h, g*fC1-fD*g')

  -- test #4
  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  J = I + ideal(a*b-c*d)
  FI = freeResolution I
  FJ = freeResolution J

  C = prune Hom(FI, S^1/I)
  D = prune Hom(FJ, S^1/J)
  C1 = C ** S^{-3}
  g = randomComplexMap(D,C1,Cycle=>true)
  fC1 = resolutionMap C1
  fD = resolutionMap D
  g' = liftMapAlongQuasiIsomorphism(g * fC1, fD);
  assert not isQuasiIsomorphism g
  assert isWellDefined g'
  assert isComplexMorphism g'
  assert(g * fC1 == fD * g')
  h = g'.cache.homotopy
  assert isWellDefined h
  assert(degree h == 1)
  assert isNullHomotopyOf(h, g*fC1-fD*g')
///

TEST ///
  -- test #6
-*  
  restart
  needsPackage "Complexes"
*-  
  S = ZZ/101[a,b,c]
  I = ideal"ab,ac"
  J = I + ideal(b*c-a^2)
  K = J + ideal(a^3+c^3)
  FI = freeResolution I
  FJ = freeResolution J
  FK = freeResolution K
  CI = (prune Hom(FI, S^1/I))
  CJ = (prune Hom(FJ, S^1/J))[-1]
  CK = (prune Hom(FK, S^1/K))[-1]
  g1 = randomComplexMap(CJ, CI, Cycle=>true)
  g2 = randomComplexMap(CK, CJ, Cycle=>true)
  assert isWellDefined g1
  assert isCommutative g1
  assert isWellDefined g2
  assert isCommutative g2
  fCI = resolutionMap CI
  fCJ = resolutionMap CJ
  fCK = resolutionMap CK
  g = g2 * g1;
  assert isWellDefined g
  assert isCommutative g
  g1' = liftMapAlongQuasiIsomorphism(g1 * fCI, fCJ);
  g2' = liftMapAlongQuasiIsomorphism(g2 * fCJ, fCK);
  assert isWellDefined g1'
  assert isCommutative g1'
  assert isWellDefined g2'
  assert isCommutative g2'
  g' = liftMapAlongQuasiIsomorphism(g * fCI, fCK);
  diffg' = g2' * g1' - g';
  assert isNullHomotopic diffg'
  h = nullHomotopy diffg';
  assert isWellDefined h
  assert isNullHomotopyOf(h, diffg')
  diffg'_-1 -- just to see the nontrivial-ness of the differentials
///

TEST ///
-*
  -- YYY
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S,{1,3,4})
  E = Ext^2(S^1/I, S)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,3))
  assert(C_3 == S^1)
  assert(C_0 == S^1/I)
  assert(C_1 == S^1)-- since we start with a free res of S^1/I

  g = yonedaMap f
  assert(degree g === -2)
  f2 = yonedaMap' g
  assert(f == f2)
  
  -- let's try the Yoneda extension corresponding to the zero map:
  f1 = 0*f
  source f1
  target f1
  degree f1
  C1 = yonedaExtension f1
  assert isWellDefined C1
  assert(prune HH C1 == 0)
  assert(concentration C1 == (0,3))
  assert(C1_3 == S^1)
  assert(C1_0 == S^1/I)
  assert(C1_1 == S^1)-- since we start with a free res of S^1/I
  g = yonedaMap f1
  f2 = yonedaMap' g
  assert(g == 0)
  assert(f1 == f2)
  assert(degree g == -2)

  -- now try an index larger than the projective dimension:  
  E = Ext^4(S^1/I, S)
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,5))
  assert(C_5 == S^1)
  assert(C_0 == S^1/I)
  assert(C_1 == S^1)-- since we start with a free res of S^1/I
  g = yonedaMap f
  assert(g == 0)
  f2 = yonedaMap' g
  assert(degree g == -4)
  assert(f == f2)

  -- now try an index larger than the projective dimension:  
  E = Ext^6(S^1/I, S)
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,7))
  assert(C_7 == S^1)
  assert(C_0 == S^1/I)
  assert(C_1 == S^1)-- since we start with a free res of S^1/I
  assert(C_5 == 0)
  g = yonedaMap f
  assert(g == 0)
///

TEST ///
-*
  -- CCC
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S,{1,3,4})
  E = Ext^0(module I, module I)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)

  g = yonedaMap f
  assert(degree g === 0)
  f2 = yonedaMap' g
  assert(f == f2)
  
  E = Ext^0(module I, comodule I)
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  g = yonedaMap f
  assert(degree g === 0)
  f2 = yonedaMap' g
  assert(f == f2)

  E = Ext^(-1)(module I, comodule I)  
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  g = yonedaMap f
  assert(degree g === 1)
  f2 = yonedaMap' g
  assert(f == f2)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z))
  M = truncate(1,S^1)
  E = Ext^1(M, S^1)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,2))
  assert(C_2 == S^1)
  assert(C_0 == M)

  assert(yonedaExtension' C == f)
  fC = resolutionMap(C, LengthLimit => 5)
  -- as C is exact, fC is the zero map
  assert(fC == 0)
  assert(isWellDefined fC)
  assert(target fC == C)

  g = yonedaMap f
  assert isWellDefined g
  assert isCommutative g
  assert (degree g == -1)
  assert(g_1 != 0)
  f2 = yonedaMap' g
  assert(f2 == f)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]
  M = truncate(1,S^1)
  N = S^{{-2}}/(x)
  E = Ext^1(M, N)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,2))
  assert(C_2 == N)
  assert(C_0 == M)

  f1 = yonedaExtension' C
  C1 = yonedaExtension f1
  assert(f == f1)
  assert(C1 == C)
  assert isWellDefined f1
  assert isWellDefined C1

  g = yonedaMap f
  assert isWellDefined g
  assert isCommutative g
  assert (degree g == -1)
  f2 = yonedaMap' g
  assert(f2 == f)

  E = Ext^2(M, N ** S^{{-1}})  
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  f1 = yonedaExtension' C
  C1 = yonedaExtension f1
  assert(f == f1)
  assert(C1 == C)
  assert isWellDefined f1
  assert isWellDefined C1
  
  --  go from FM_d --> N to an element R^1 --> Ext^d(M,N).
  --  (and vice versa)
  --  length limit on resolutionMap (maybe), also can we just use freeResolution
  --    if it is a module?
  --  make this functorial?  Actually: perhaps only place this in as an example.
  --  Yoneda product
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z))
  M = truncate(1,S^1)
  E = Ext^0(M, M)
  
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  assert try (C = yonedaExtension f; false) else true
///

TEST ///
-- CCC
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]
  M = truncate(1,S^1)
  N = S^{{-2}}/(x)
  E2 = Ext^1(M, N)
  E1 = Ext^1(M ** S^{{1}},M)
  
  h2 = basis(0,E2)
  f2 = h2 * random(S^(numColumns h2), S^1)
  h1 = basis(0,E1)
  f1 = h1 * random(S^(numColumns h1), S^1)

  g = yonedaProduct(f1,f2)
  yonedaExtension g
  
  yonedaProduct(E1, E2)
  Ext^2(M ** S^{{1}}, N)

  yonedaProduct(Ext^0(M, M), E2)
///

TEST ///
  S = ZZ/101[x,y,z]
  R = S/(y^2*z-x^3)
  I = ideal(x,y)
  E0 = Ext^0(I,I)
  E1 = Ext^1(I,R^1/I)
  E0_{1}
  E1_{0}
  f = yonedaProduct(E0_{1}, E1_{0}+E1_{1})
  assert(f == yonedaExtension' yonedaExtension f)
///

TEST ///
  -- of length limits and free resolutions
  R = ZZ/32003[a..d]/(a^2-b*c)
  M = coker vars R;
  C1 = freeResolution M;
  assert(M.cache.?freeResolution)
  assert(M.cache.freeResolution === C1)
  assert(M.cache.freeResolution.cache.LengthLimit === length C1)
  assert(C1.cache.Module === M)
  C2 = freeResolution(M, LengthLimit=>10)
  assert(length C2 == 10)
  assert(M.cache.?freeResolution)
  assert(M.cache.freeResolution === C2)
  assert(M.cache.freeResolution.cache.LengthLimit === length C2)
  assert(C2.cache.Module === M)
  C3 = freeResolution(M, LengthLimit=>9)
  assert(M.cache.?freeResolution)
  assert(M.cache.freeResolution === C2)
  assert(M.cache.freeResolution =!= C3)
  assert(M.cache.freeResolution.cache.LengthLimit === length C2)
  assert(length C3 == 9)
  assert(C3.cache.Module === M)
  C4 = freeResolution(M, LengthLimit => 1)
  assert(M.cache.?freeResolution)
  assert(M.cache.freeResolution === C2)
  assert(M.cache.freeResolution.cache.LengthLimit === length C2)
  assert(length C4 == 1)
  assert(C4.cache.Module === M)
  C5 = freeResolution(M, LengthLimit => 0)
  assert(M.cache.?freeResolution)
  assert(M.cache.freeResolution === C2)
  assert(M.cache.freeResolution.cache.LengthLimit === length C2)
  assert(length C5 == 0) 
  assert(C5.cache.Module === M)
  assert try (C6 = freeResolution(M, LengthLimit => -1); false) else true
  assert(M.cache.?freeResolution)
  assert(M.cache.freeResolution === C2)
  assert(M.cache.freeResolution.cache.LengthLimit === length C2)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/101[a..d]/(a^2-b*c, b^2-c*d)
  C = freeResolution coker vars R
  f = dd^C_2
  g = Hom(f, R^1/(a*c, b*d))
  D = complex{g}
  fD = resolutionMap D
  fD.cache.LengthLimit
  isWellDefined fD
  source fD
  fD = resolutionMap(D, LengthLimit=>7)
  assert(length source fD == 7)
  assert(fD.cache.LengthLimit == 7)
  fD = resolutionMap(D, LengthLimit=>4)
  assert(length source fD == 4)
  assert(D.cache.resolutionMap.cache.LengthLimit == 7)
///

TEST ///
  -- creation of chain complexes
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution(coker vars R)

  CR2 = res coker matrix{{a,b,c,d}}
  maps = for i from 1 to 4 list CR2.dd_i
  C2 = complex maps

  assert(C == C2)
  assert(C != C[1])
  assert(C != 0)  

  -- test C_i for various values, some out of range
  for i from -3 to 5 do assert(isFreeModule C_i)
  ranks = {0,0,0,1,4,6,4,1,0}
  for i from 0 to #ranks-1 do assert(rank C_(i-3) == ranks#i)

  -- create identity
  id_C == 1
  
  -- test map access
  assert(dd^C != 0)
  assert((dd^C)^2 == 0)
  assert((dd^C)^2 != 1)
  for i from 1 to 4 do assert(dd^C_i == maps_(i-1))
  assert(dd^C_100 == 0)
  
  assert(dd^C * dd^C == 0)
  assert((dd^C)^2 == 0)
  assert try ((dd^C)^-1; false) else true
///

TEST ///
  -- creation of a complex with 2 non-zero maps, not contiguous
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  f1 = random(R^3, R^2)
  f2 = random(R^1, R^4)
  C = complex hashTable {1 => f1, 6 => f2}
  for i from 0 to 7 list C_i
  assert(C_0 === target f1)
  assert(C_1 == source f1)
  assert(C_5 == target f2)
  assert(C_6 == source f2)
  assert(dd^C_1 == f1)
  assert(dd^C_6 == f2)
  for i from -10 to 10 do if i != 1 and i != 6 then (
      if i != 0 and i != 5 then assert(C_i == 0);
      assert(dd^C_i == 0)
      );
  assert((dd^C)^2 == 0)
  assert(id_C == 1)
  assert((id_C)^(-1) == id_C)
///

TEST ///
  -- of Hom(f,g)
  -- Hom(f,source g) * Hom(target f,g) === Hom(f,g)
  -- Hom(target f,g) * Hom(g,source g) === (sign) Hom(f,g)
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C
  f = map(C[1],C,f1,Degree=>-1)
  g1 = d*id_D
  g = map(D[-3],D,g1,Degree=>3)
  h = Hom(f,g)
  -- necessary properties for the signs of Hom(f,g):
  assert(Hom(f,target g) * Hom(target f, g) == h)
  assert(Hom(source f,g) * Hom(f,source g) == -h)
///

TEST ///
  -- of f**g
  -- Hom(f,source g) * Hom(target f,g) === Hom(f,g)
  -- Hom(target f,g) * Hom(g,source g) === (sign) Hom(f,g)
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C
  f = map(C[1],C,f1,Degree=>-1)
  g1 = d*id_D
  g = map(D[-3],D,g1,Degree=>3)
  h = f**g

  assert(id_C ** id_D == C ** id_D)
  assert(id_C ** id_D == id_C ** D)
  assert(id_C ** id_D == id_(C**D))
  
  assert((f ** (target g)) * ((source f) ** g) == f**g)
  assert(((target f) ** g) * (f ** (source g)) == (-1)^((degree g) * (degree f)) * (f**g))
///

TEST ///
  -- of isComplexMorphism, isCommutative
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C
  assert isComplexMorphism f1
  f = map(C[1],C,f1,Degree=>-1)
  assert not isComplexMorphism f
  g1 = d*id_D
  g = map(D[-1],D,g1,Degree=>1)
  h = f**g -- differential anti-commutes with h
  assert not isCommutative h
  assert not isComplexMorphism h
///

TEST ///
-- test: creating complex morphism's
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C  
  f2 = (b^2-c)*id_D
  assert isComplexMorphism f1
  assert isComplexMorphism f2
  assert isComplexMorphism(f1 ++ f2)
  assert isComplexMorphism Hom(f1,f2)
  assert isComplexMorphism(f1 ** f2)
  assert isComplexMorphism (f1[3])
  assert isComplexMorphism(f1[-1] ** f2)
///

TEST ///
  -- note: inducedMap is here only for backward compatibility
  -- prefer canonicalMap when possible.
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = ideal(a^3+b^3+c^3+d^3)
  J = ideal(a+b,c+d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  g = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  h = inducedMap(coker g, target g)
  assert isWellDefined h
  assert isComplexMorphism h
  assert(degree h == 0)

  assert(canonicalMap(coker g, target g) == h)
  i = inducedMap(source g, ker g)
  assert(canonicalMap(source h, ker h) ==  inducedMap(source h, ker h))
  
  -- test dual of a complex map
  assert(dual dual g == g)
  assert(isWellDefined dual g)
  assert(isWellDefined dual h)
  assert(isWellDefined dual i)
  assert(dual i == 0)
///


TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = ideal(a^2, b^2, c^2, d^2)
  J = ideal(a,b,c,d)
  J1 = monomialIdeal J
  assert isWellDefined complex J
  assert isWellDefined complex J1
  M = (complex S^0)[5]
  assert isWellDefined M
  C = freeResolution comodule I
  D = freeResolution comodule J
  assert isWellDefined C
  assert isWellDefined D
  C1 = complex(I/I)
  assert isWellDefined C1
  assert isWellDefined (dd^C)
  assert isWellDefined (dd^C1)
  C1 ++ C1[3]
  (complex (S^1/I))
  (complex (S/I)^1)
  (complex (S/I))[6]
  assert try (C1 ++ C1[3] ++ (complex (S/I))[6]; false) else true  -- gives error message as desired.
  assert isWellDefined (C1 ++ C1[3] ++ (complex (S^1/I))[6])
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  F = extend(D[4],C[4],map(S^1,S^1,1))
  assert isComplexMorphism F
  assert isWellDefined F
  F = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism F
  assert isWellDefined F
  betti C
  betti D
  betti (C[4])
  betti (C**D)
  assert isHomogeneous F  
  assert isHomogeneous source F
  assert isHomogeneous target F
  kerF = ker F
  assert(prune kerF == 0)
  
  S = ZZ/101[a..d]
  I = ideal(a^2, b^2, c^2, d^2-a)
  J = ideal(a,b,c,d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D[4],C[4],map(S^1,S^1,1))
  F = extend(D,C,map(D_0,C_0,1))
  assert not isHomogeneous F  
  assert not isHomogeneous source F
  assert isHomogeneous target F

  S = ZZ/101[a..d]
  I = ideal(a^3+b^3+c^3+d^3)
  J = ideal(a+b,c+d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  g = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))

  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S, {1,3,4})
  J = truncate(4, I)
  C = freeResolution comodule J
  D = freeResolution comodule I
  g = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C  
  assert(ker f1 == 0)
  Cf = coker f1
  assert isWellDefined Cf
  imf = image f1
  assert isWellDefined imf

  E = cone f1
  assert isWellDefined E
  F1 = canonicalMap(cone f1, target f1)
  assert isWellDefined F1
  F2 = canonicalMap((source f1)[-1], cone f1)
  assert isWellDefined F2
  assert(F2 * F1 == 0)
  assert(ker F2 == image F1)
  imf2 = prune imf
  g = imf2.cache.pruningMap
  assert(coker g == 0 and ker g == 0)
  
  E = cylinder f1
  assert isWellDefined E
  G1 = canonicalMap(E, target f1, UseTarget=>true)
  G2 = canonicalMap(E, source f1, UseTarget=>false)
  G3 = canonicalMap(target f1, E)
  G4 = canonicalMap(cone f1, E)  
  assert isWellDefined G1
  assert isWellDefined G2
  assert isWellDefined G3
  assert isWellDefined G4
  assert(G4 * G2 == 0)
  assert(kernel G4 == image G2)

  assert(coimage F1 == prune image F1)
  assert(coimage G2 == prune image G2)

  -- ker, coker, image, coimage canonical maps
  f = G2
  h1 = canonicalMap(source f, kernel f)
  h2 = canonicalMap(coimage f, source f)
  h3 = canonicalMap(target f, image f)
  h4 = canonicalMap(cokernel f, target f)
  assert isWellDefined h1
  assert isWellDefined h2
  assert isWellDefined h3
  assert isWellDefined h4
  assert(h2 * h1 == 0)
  assert(kernel h2 == image h1)
  assert(h4 * h3 == 0)
  assert(kernel h4 == image h3)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/101[a..f]
  A = freeResolution coker matrix{{a,b}}
  B = freeResolution monomialCurveIdeal(R,{1,2,3})
  C = freeResolution monomialCurveIdeal(R,{1,3,4})

  f = tensorAssociativity(A,B,C);
  isWellDefined f
  assert(ker f == 0)
  assert(coker f == 0)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-

  S = ZZ/101[a..d, Degrees=>{2:{1,0},2:{0,1}}]
  B = ideal(a,b) * ideal(c,d)
  Ext^1(B, S)
  F = random({1,2}, S)
  f = map(S^1, S^{-degree F}, {{F}})
  assert isHomogeneous f
  g = map(S^1/F, S^1, 1)
  FB = freeResolution comodule B
  Hg = Hom(FB, g)
  Hf = Hom(FB, f)
  assert isWellDefined Hg
  assert isWellDefined Hf
  assert isShortExactSequence(Hg, Hf)
  delta = connectingMap(Hg, Hf)
  assert isWellDefined delta
  delta' = prune delta
  det matrix delta'_-1 
///

TEST ///
  -- Slightly different version, matching construction of vector bundle of
  -- rank 2 on an elliptic curve.  
-*
-- BBB
restart
needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]
  I = ideal(y^2*z-x^3-x*z^2-z^3)
  OC = S^1/I
  OCp 
  R = S/I
  OCp = coker lift(relations prune Hom(ideal(x,z), R), S)
  basis(0, Ext^1(truncate(1,OC), OCp))

  M = prune truncate(1, Hom(ideal(x,z), R))
  N = truncate(1,R^1)
  E = Ext^1(M, N)
  f = basis(0, E)
  source f
  target f == E

  pses = prune yonedaExtension f
  mods = for i from 0 to 2 list coker lift(relations pses_i, S)
  maps = hashTable for i from 1 to 2 list i => map(mods_(i-1), mods_i, lift(matrix dd^pses_i, S))
  ses = complex maps  
  assert isWellDefined ses
  assert isShortExactSequence(dd^ses_1, dd^ses_2)

  B = module (ideal vars S)^[1]
  FB = freeResolution B
  m1 = Hom(FB, dd^ses_1)
  m2 = Hom(FB, dd^ses_2)
  assert isShortExactSequence(m1,m2)  

  prune HH target m1
  prune HH source m1
  prune connectingMap(m1,m2) 
  assert(oo != 0)

  -- TODO: do more examples, 
  --   make sure that both connecting homom functions are computing
  --   the same thing.  Only then, which is faster?
   
///

doc ///
    Key
        (complex, ChainComplex)
    Headline
        translate between data types for chain complexes
    Usage
        D = complex C
    Inputs
        C:ChainComplex
    Outputs
        D:Complex
    Description
        Text
            Both ChainComplex and Complex are Macaulay2 types that
            implement chain complexes of modules over rings.
            The plan is to replace ChainComplex with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/32003[a..d]
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            C = resolution M
            D = complex C
            D1 = freeResolution M
            assert(D == D1)
        Text
            In the following example, note that a different choice of sign
            is chosen in the new Complexes package.
        Example
            C1 = Hom(C, R^1)
            D1 = complex C1
            D2 = Hom(D, R^1)
            D1.dd_-1
            D2.dd_-1
            assert(D1 != D2)
    Caveat
        This is a temporary method to allow comparisions among the different data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (chainComplex, Complex)
        (chainComplex, ComplexMap)
        (complex, ChainComplexMap)
///

doc ///
    Key
        (chainComplex, Complex)
    Headline
        translate between data types for chain complexes
    Usage
        C = chainComplex D
    Inputs
        D:Complex
    Outputs
        C:ChainComplex
    Description
        Text
            Both ChainComplex and Complex are Macaulay2 types that
            implement chain complexes of modules over rings.
            The plan is to replace ChainComplex with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/32003[a..d]
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            C = resolution M
            D = freeResolution M
            C1 = chainComplex D
            assert(C == C1)
        Text
            The tensor products make the same choice of signs.
        Example
            D2 = D ** D
            C2 = chainComplex D2
            assert(C2 == C1 ** C1)
    Caveat
        This is a temporary method to allow comparisions among the different data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (complex, ChainComplex)
        (complex, ChainComplexMap)
        (chainComplex, ComplexMap)
///


doc ///
    Key
        (complex, ChainComplexMap)
    Headline
        translate between data types for chain complex maps
    Usage
        g = complex f
    Inputs
        f:ChainComplexMap
    Outputs
        g:ComplexMap
    Description
        Text
            Both ChainComplexMap and ComplexMap are Macaulay2 types that
            implement maps between chain complexes.
            The plan is to replace ChainComplexMap with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/32003[a..d]
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            C = resolution M
            f = C.dd
            g = complex f
            isWellDefined g
            D = freeResolution M
            assert(D.dd == g)
        Text
            The following two extension of maps between modules to
            maps between chain complexes agree.
        Example
            J = ideal vars R
            C1 = resolution(R^1/J)
            D1 = freeResolution(R^1/J)
            f = extend(C1, C, matrix{{1_R}})
            g = complex f
            g1 = extend(D1, D, matrix{{1_R}})
            assert(g == g1)
    Caveat
        This is a temporary method to allow comparisions among the different data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (chainComplex, ComplexMap)
        (complex, ChainComplex)
        (chainComplex, Complex)
///

doc ///
    Key
        (chainComplex, ComplexMap)
    Headline
        translate between data types for chain complexes
    Usage
        f = chainComplex g
    Inputs
        g:ComplexMap
    Outputs
        f:ChainComplexMap
    Description
        Text
            Both ChainComplexMap and ComplexMap are Macaulay2 types that
            implement maps between chain complexes.
            The plan is to replace ChainComplexMap with this new type.
            Before this happens, this function allows interoperability
            between these types.
        Text
            The first example is the minimal free resolution of the
            twisted cubic curve.
        Example
            R = ZZ/101[a..d]
            I = monomialCurveIdeal(R, {1,2,3})
            M = R^1/I
            D = freeResolution M
            C = resolution M
            g = D.dd
            f = chainComplex g
            assert(f == C.dd)
        Text
            We construct a random morphism of chain complexes.
        Example
            J = ideal vars R
            C1 = resolution(R^1/J)
            D1 = freeResolution(R^1/J)
            g = randomComplexMap(D1, D, Cycle => true)
            f = chainComplex g
            assert(g == complex f)
            assert(isComplexMorphism g)
    Caveat
        This is a temporary method to allow comparisions among the different data types,
        and will be removed once the older data structure is replaced
    SeeAlso
        (complex, ChainComplexMap)
        (complex, ChainComplex)
        (chainComplex, Complex)
///


end------------------------------------------------------------

restart
uninstallPackage "Complexes"
restart
installPackage "Complexes"
check "Complexes"
restart
needsPackage "Complexes"
viewHelp
viewHelp "(isCommutative,ComplexMap)"

doc ///
    Key
    Headline
    Usage
    Inputs
    Outputs
    Description
        Text
        Example
    Caveat
    SeeAlso
///

NEWCODE FOR ENGINE DESIRED BY DE ///
HomWithComponents = method()
HomWithComponents (Module, Module) :=  (M,N) ->(
   if not isDirectSum M and not isDirectSum N then (
       return Hom(M,N)
       );
   if isDirectSum M and not isDirectSum N then (
       indicesM := indices M;
       return directSum for i from 0 to #indicesM - 1 list 
           indicesM#i => Hom((components M)#i, N);
       );
   if not isDirectSum M and isDirectSum N then (
       indicesN := indices N;
       return directSum for i from 0 to #indicesN - 1 list 
           indicesN#i => Hom(M, (components N)#i);
       );
   if isDirectSum M and isDirectSum N then (
       indicesM = indices M;
       indicesN = indices N;
       return directSum flatten (
         for i from 0 to #indicesM - 1 list
           for j from 0 to #indicesN - 1 list
             {indicesM#i, indicesN#j} => 
               Hom((components M)#i, (components N)#i)
       ));
   )
///


UNTEST ///
restart
needsPackage "Complexes"
  R = ZZ/101[a..d]
  C = freeResolution(coker vars R)
  D = complex{matrix{{a}}}
  E = complex R^1
  F = Hom(C ++ D, E)
  indices F
  components F
  F1 = (C++D) ** E
  components F1
  indices F1
  F2 = ((a=>C)++(b^2=>E)) ** D
  components F2
  indices F2

  -- want Hom(C++D,E) --> Hom(C,E) ++ Hom(D,E)
  map(Hom(C++D,E), Hom(C,E) ++ Hom(D,E), 1)
  F1 = Hom(C++D,E)
  F2 = Hom(C,E) ++ Hom(D,E)
  F1 == F2
  id_F1
  -- below is just to test out code that won't remain here
  F = R^2
  G = R^3
  H = (a=>F) ++ (b=>G)
  indices H
  components H
  hom2 = HomWithComponents(H, R^2)
  indices hom2
  hom3 = HomWithComponents(R^7, H)
  indices hom3
  components hom3
  hom4 = HomWithComponents(H, H)
  indices hom4
  components hom4

///

-- The following shows a efficiency issue with isNullHomotopic
///
  S = ZZ/101[a..e]
  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  J = I + ideal(a*b-c*d)
  K = J + ideal(a^4)
  FI = freeResolution I
  FJ = freeResolution J
  FK = freeResolution K
  CI = prune Hom(FI, S^1/I)
  CJ = prune Hom(FJ, S^1/J) ** S^{-1}
  CK = prune Hom(FK, S^1/K) ** S^{-2}
  g1 = randomComplexMap(CI, CJ, Cycle=>true)
  g2 = randomComplexMap(CJ, CK, Cycle=>true)
  assert isWellDefined g2
  assert isComplexMorphism g2
  fCI = resolutionMap CI
  fCJ = resolutionMap CJ
  fCK = resolutionMap CK
  g = g1 * g2
  g1' = liftMapAlongQuasiIsomorphism(g1 * fCJ, fCI);
  g2' = liftMapAlongQuasiIsomorphism(g2 * fCK, fCJ);
  assert isWellDefined g2'
  assert isComplexMorphism g2'
  g' = liftMapAlongQuasiIsomorphism(g * fCK, fCI);
  diffg' = g1' * g2' - g';
  isNullHomotopic diffg' -- this takes seemingly a long time...
  debugLevel = 1
  h = nullHomotopy(diffg', UseOriginalMethod);
  isWellDefined h
  isNullHomotopyOf(h, diffg')
  h_-2 * dd^(source diffg')_-1 + dd^(target diffg')_0 * h_-1 - diffg'_-1
  diffg'_-1;
  target g1 === CI
  target fCI === CI
  
  -- AAAA
  -- we stopped here and need more tests of liftMapAlongQuasiIsomorphism
  -- including ones where h is non-zero.
  -- need to redo lines below in this test.
///
