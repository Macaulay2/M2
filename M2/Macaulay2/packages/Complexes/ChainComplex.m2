--------------------------------------------------------------------
-- type declarations -----------------------------------------------
--------------------------------------------------------------------
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

--------------------------------------------------------------------
-- basic methods for chain complexes -------------------------------
--------------------------------------------------------------------
ring Complex := Ring => C -> C.ring

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

max Complex := ZZ => C -> max concentration C
min Complex := ZZ => C -> min concentration C

complex = method(Options => {Base=>0})
complex HashTable := Complex => opts -> maps -> (
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
complex List := Complex => opts -> L -> (
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
complex Module := Complex => opts -> (M) -> (
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
complex Ring := Complex => opts -> R -> complex(R^1, opts)
complex Ideal := Complex => opts -> I -> complex(module I, opts)
complex Complex := Complex => opts -> C -> (
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
complex ComplexMap := Complex => opts -> f -> (
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

isWellDefined Complex := Boolean => C -> (
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

Symbol ^ Complex := ComplexMap => (sym, C) -> (
    if sym === dd then C.dd
    else error "expected symbol to be 'dd'"
    )

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


Complex#id = (C) -> (
    (lo,hi) := C.concentration;
    maps := hashTable for i from lo to hi list i => id_(C_i);
    result := map(C,C,maps);
    result.cache.isCommutative = true;
    result
    )

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

texUnder = (x,y) -> "\\underset{\\vphantom{\\Bigg|}"|y|"}{"|x|"}"

-- NOTE: there are hardcoded constant values (8, 10) 
-- in the next function.
texMatrixShort = method()
texMatrixShort Matrix := String => m -> (
    if m == 0 then return "0";
    e := entries m;
    texRow := row -> if #row > 8 then 
            { texMath first row, "\\cdots", texMath last row } 
        else 
            texMath\row;
    e = if #e > 10 then ( 
            t := texRow first e; 
            {t, toList(#t:"\\vphantom{\\Big|}\\vdots"), texRow last e} 
            )
        else 
            texRow\e;
    concatenate(
	"\\begin{bmatrix}" | newline,
	between(///\\/// | newline, 
            for row in e list concatenate between("&", row)),
	"\\end{bmatrix}"
        )
    )

texMath Complex := String => C -> (
    (lo, hi) := concentration C;
    if C == 0 then 
        "0" 
    else (
        concatenate for i from lo to hi list (
            if i === lo then 
                texUnder(texMath C_i,i) 
            else (
                "\\,\\xleftarrow{\\scriptsize " 
                | texMatrixShort dd^C_i 
                | "}\\," 
                | texUnder(texMath C_i,i)
                )
            )
        )
    )

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
freeResolution Module := Complex => opts -> M -> (
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
freeResolution Ideal := Complex => opts -> I -> freeResolution(comodule I, opts)
freeResolution MonomialIdeal := Complex => opts -> I -> freeResolution(comodule ideal I, opts)
freeResolution Matrix := ComplexMap => opts -> f -> extend(
    freeResolution(target f, opts), 
    freeResolution(source f, opts),
    matrix f
    )

isHomogeneous Complex := (C) -> isHomogeneous dd^C

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

regularity Complex := opts -> C -> (
    if numgens degreesRing ring C =!= 1 then 
        error "expected the underlying ring to be standard graded";
    if not isFree C then 
        error "expected a complex whose terms are all free";
    regularity betti(C,opts)
    )

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

--------------------------------------------------------------------
-- truncations -----------------------------------------------------
--------------------------------------------------------------------
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
    else if lo === hi then complex(C_lo, Base=>lo)
    else if lo > hi then complex (ring C)^0
    else complex(hashTable for i from lo+1 to hi list i => dd^C_i, Base=>lo)
    )
naiveTruncation(Complex,ZZ,ZZ) := 
naiveTruncation(Complex,ZZ,InfiniteNumber) := 
naiveTruncation(Complex,InfiniteNumber,ZZ) := 
naiveTruncation(Complex,InfiniteNumber,InfiniteNumber) := 
naiveTruncation(Complex,ZZ,Nothing) := 
naiveTruncation(Complex,Nothing,ZZ) := Complex => (C,lo,hi) -> naiveTruncation(C, (lo,hi))

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
part(ZZ, Complex) := Complex => (deg, C) -> part({deg}, C)

truncate(List, Complex) := Complex => {} >> o -> (e, C) -> (
    (lo, hi) := concentration C;
    if lo === hi then return complex truncate(e, C_lo);
    complex hashTable for i from lo+1 to hi list i => truncate(e, dd^C_i)
    )
truncate(ZZ, Complex) := Complex => {} >> o -> (e, C) -> truncate({e}, C)

--------------------------------------------------------------------
-- homology --------------------------------------------------------
--------------------------------------------------------------------
homology Complex := Complex => opts -> C -> (
    (lo, hi) := C.concentration;
    modules := hashTable for i from lo to hi list i => homology(i,C);
    if lo == hi then return complex(modules#lo, Base=>lo);
    maps := hashTable for i from lo+1 to hi list i => map(modules#(i-1), modules#i, 0);
    complex maps
    )
homology(ZZ, Complex) := opts -> (i,C) -> homology(dd^C_i, dd^C_(i+1))
cohomology(ZZ,Complex) := opts -> (i,C) -> homology(-i, C)

--------------------------------------------------------------------
-- Hom -------------------------------------------------------------
--------------------------------------------------------------------
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

--------------------------------------------------------------------
-- Tensor products -------------------------------------------------
--------------------------------------------------------------------
tensor(Complex, Complex) := Complex => {} >> opts -> (C, D) -> (
    Y := youngest(C,D);
    if Y.cache#?(tensor,C,D) then return Y.cache#(tensor,C,D);
    R := ring C;
    if ring D =!= R then error "expected complexes over the same ring";
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    modules := hashTable for i from loC+loD to hiC+hiD list i => (
        directSum for j from loC to hiC list (
            if i-j >= loD and i-j <= hiD then
                {j,i-j} => C_j ** D_(i-j)
            else
                continue
            )
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

Complex ** Matrix := ComplexMap => (C, f) -> (
    if ring C =!= ring f then error "expected Complex and Matrix over the same ring";
    src := C ** source f;
    tar := C ** target f;
    map(tar, src, i -> map(tar_i, src_i, C_i ** f))
    )
Matrix ** Complex := ComplexMap => (f, C) -> (
    if ring C =!= ring f then error "expected Complex and Matrix over the same ring";
    src := source f ** C;
    tar := target f ** C;
    map(tar, src, i -> map(tar_i, src_i, f ** C_i))
    )

Complex ** Ring := Complex => (C,R) -> (
    (lo,hi) := concentration C;
    moduleHash := hashTable for i from lo to hi list i => C_i ** R;
    if lo === hi then 
        return complex(moduleHash#lo, Base=>lo);
    mapHash := hashTable for i from lo+1 to hi list i => 
        map(moduleHash#(i-1), moduleHash#i, (cover dd^C_i) ** R);
    complex mapHash
    )
Ring ** Complex := Complex => (R,C) -> C ** R

RingMap Complex := Complex => (phi,C) -> (
    (lo,hi) := concentration C;
    moduleHash := hashTable for i from lo to hi list i => phi C_i;
    if lo === hi then 
        return complex(moduleHash#lo, Base=>lo);
    mapHash := hashTable for i from lo+1 to hi list i => 
        map(moduleHash#(i-1), moduleHash#i, phi dd^C_i);
    complex mapHash
    )

tensor(RingMap, Complex) := Complex => {} >> opts -> (phi, C) -> (
    if source phi =!= ring C then error "expected the source of the ring map to be the ring of the complex";
    (lo,hi) := concentration C;
    modules := hashTable for i from lo to hi list i => tensor(phi, C_i);
    if lo === hi then 
        return complex(modules#lo, Base=>lo);
    maps := hashTable for i from lo+1 to hi list i => 
        map(modules#(i-1), modules#i, tensor(phi, matrix dd^C_i));
    complex maps
    )
tensor(Complex, RingMap) := Complex => {} >> opts -> (C, phi) -> tensor(phi, C)

RingMap ** Complex := Complex => (phi, C) -> tensor(phi, C)
Complex ** RingMap := Complex => (C, phi) -> tensor(phi, C)

--------------------------------------------------------------------
-- resolutions -----------------------------------------------------
--------------------------------------------------------------------
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
resolutionMap Complex := ComplexMap => opts -> C -> (
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
                (len <= hi - lo or g =!= null) and len <= lengthlimit
                ) do (
                if g === null then (
                    -- if g is null but the complex C has terms in position >= lo+len 
                    -- then we modify the source to increase the concentration of the source of f (i.e. add zero module at position lo+len
                    src := source f;
                    src.concentration = (lo-1, lo + len);
                    f = map(C, src, f);
                    )
                else
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

augmentationMap = method()
augmentationMap Complex := ComplexMap => C -> (
    if not C.cache.?Module then error "expected a free resolution";
    M := C.cache.Module;
    map(complex M, C, i -> if i === 0 then map(M, C_0, 1))
    )

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

--------------------------------------------------------------------
-- Yoneda ext ------------------------------------------------------
--------------------------------------------------------------------
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
                map(source g1, source f, h, Degree => degree f)
                );
            invmap = (g) -> (
                -- given g : R^1 --> Hom(FM_i, N)
                -- given h : ker g1 --> Hom(FM_i, N) inclusion map
                -- note g1: Hom(FM_i, N) --> Hom(FM_(i+1), N)
                -- output: R^1 --> H = ker g1 / image g0
                h := inducedMap(target g0, kerg1);
                f1 := g // h;
                map(H, source f1, f1, Degree => degree g)
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
    -- if g has a non-zero degree, we must twist the target to preserve homogeneity
    gdegree := degree g;
    g = map(N ** (ring g)^gdegree, source g, g);
    if d <= 0 then error "Yoneda extension only defined for Ext^d module for d at least 1";
    h := dd^FM_d || g;
    P := coker h; -- FM_d --> FM_(d-1) ++ N --> P --> 0
    D := target h; -- direct sum
    -- notice that the inclusion map into the direct sum may have a non-zero internal degree.
    delta0 := map(P,D,id_D) * map(D, N, D_[1], Degree => -gdegree); -- N --> P
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
    -- the " + degree f" is needed because of the behavior of homomorphism:
    -- that function appears to ignore the degree of the incoming map.
    g0 := map(FN_0, FM_d, g, Degree => degree g);
    extend(FN, FM, g0, (0,d))
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
    g := map(N, FM_d, f_d, Degree => degree f_d);
    extd.cache.yonedaExtension' homomorphism' g
    )

yonedaProduct = method()
yonedaProduct(Matrix, Matrix) := Matrix => (a,b) -> (
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

koszulComplex = method(Options => true)
koszulComplex Matrix := Complex => {Concentration => null} >> opts -> f -> (
    if numrows f =!= 1 then error "expected a matrix with one row";
    (lo, hi) := if opts.Concentration === null then (0, numcols f) else opts.Concentration;
    if lo < 0 then lo = 0;
    if hi > numcols f then hi = numcols f;
    if lo === hi then 
        complex(target koszul(lo+1, f), Base => lo)
    else (
        complex(for i from lo+1 to hi list koszul(i, f), Base => lo)
        )
    )
koszulComplex List := Complex => {Concentration => null} >> opts -> L -> (
    if #L === 0 then error "expected a non-empty list";
    koszulComplex(matrix{L}, opts)
    )
