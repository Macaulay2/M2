-----------------------------------------------
-- NOTE!!!!!  The version in bitbucket is the most up to date
-- For now, the version in M2.git is a relatively recent version.
-- Jan 5, 2017: first time Complexes.m2 was added to M2.git
-----------------------------------------------
-- todo for 1 Mar 2018
-- notes:
--   randomComplexMap: we have not done the boundary case yet.
--   nullHomotopy and friends: use Hom complex in general case, 
--     eventually compare with the current implementation for source=free complex
--   isSemifreeComplex: we will need this.
--   code to generate a basis of all cycles, as maps, also to generate a basis of all boundaries, as maps.
--   fix cone tests (code before CCCC example)
--   get back to lifting along a quasi-isomorphism.
--  we have several bugs:
--   a. resolutionMap doesn't seem to handle complexes with a single module.
--   b. we need a *convenient* way to get elements of Z(Hom(C,D))_0
--     as a morphism.
--     Greg, take a look at "CCCC" in tests below.
--   c. test liftMapAlongQuasiIsomorphism, maybe change its name.
--   d. need a isNullHomotopy function (similar to (b), except we test boundaries, not cycles).
--     probably use h d + d h = 0
--     but for constructing non-trivial null homotopies we will use Hom complex.
--  todo for 19 Jan 2018
--    1. cache free resolution information (especially if it is a minimal res).
--       use this info to immediately return self for resolution of a 
--       (free minimal resolution). (MAYBE)
--       Can we speed up the computation of homology, mingens, etc?  
--         (really, this is a core M2 issue).
--    2. Ext stuff
--         (a) elem of Ext^1(M,N): get a map FN_1 --> FM_0, map FM[-1] --> FN, ses.
--         (b) ses from either of these
--         (c) given a ses, determine these.
--         (d) do all of this for Ext^d(M,N)
--         (e) functoriality
--    3. morphisms in derived category (start here).
--       Mike's meanderings: f : C --> D, get Ff : FC --> FD
--       We need to implement the following:
--        (a): given g : C --> D, find Fg : FC --> FD.
--        (b): given g : C --> D a quasi-isomorphism, find Fg^-1 : FD --> FC.
--        (c): given g : C --> D surjective, and E --> D is given, E semi-free.
--           find the lift: E --> FC. (or E --> C) (this might be the main primitive).
--       Generate interesting examples of:
--        (a) quasi-isomorphisms
--        (b) interesting maps between complexes.
--        (c) given complexes C,D, return a random complex morphism.
--            also: return a basis of the set of f : C --> D of degree 0
--  todo for next meeting Dec 14 BBBB
--    1. can we make resolution of a complex return a minimal one directly?
--    2. interface code for PruneComplex.
--    first: change the output of minimize, cache the map
--    can we make resolution of a complex return a minimal one directly, w/o calling minimize?
--    is there a faster way to implement 'minimize'.
--    examples:
--     cotangent complex
--     Hom(res I, (ring I)^1/I)
-- 6 Sep 2017 notes
--  In September: do resolution of a complex, and figure out maps/functoriality/roofs
--  (e.g. Christensen-Foxby notes).
--  NEXT STEP: look for: AAAA.
--  implement construction 3.1.7 (theorem 3.1.6), and the lifting map 3.1.8.
-- 
-- 27 June 2017 notes
--  res.m2, chaincomplexes.m2
--  
--  want minimal semi-projective cover of a complex C
--    a complex which is free, and also comes from a filtration (via cohomological degree).
--    and maps surjectively to C.
--    f : FC --> C, which is a surjective quasi-isomorphism.
--    given C, D complexes, g : C --> D
--    want Fg : FC --> FD, but also:
--      morphisms are roof maps: a map C --> D is a ...
--      this needs to be worked out.
--    
--    
-- for 30 May 2017: discuss priorities for completion this summer.
--   need good (killer app) examples TODO for:
--     a. connecting homomorphisms
--     b. derived category computations
--       BGG:
--         L,R functors
--         M S-module --> T(M) Tate resolution over E (exterior algebra).
--         Beilinson functor: B(E-module) = U^a
--     c. (elems of Ext^1 <--> SES <--> connecting homo <--> map of complexes
--       want this also for Ext^i
-- 11 April 2017 cleaned up todo list:
-- 1. Add in functionality present in current ChainComplexes code.
--     resolution
--     homology(ComplexMap)
--     a graded module will be a Complex, whose differential is 0.
--       add info in the cache
--       functions to add here (that will work on (some) Complex(es) as well:
--          ambient
--          cover
--          super
--     substitute
--
--     eagonNorthcott
--     koszul, koszulComplex
--     taylorResolution
--     Module Array (can't do this until we replace old code)
--     status (of a resolution)
--     nullHomotopy f -- if f is null homotopic, then return such a null homotopy.
--       -- isHomotopic(f,0), (f,g)
--       -- nullHomotopy f -- produces one, if it exists.
--       -- another routine to check if g is a nullhomotopy of f (won't need this)
--       -- nullhomotopy -- synonym for nullHomotopy?
--     transpose ComplexMap (modules are free modules, we expect)
--   texMath/tex (of Complex, ComplexMap?  Current code is not so useful)
--
-- 2. Fill in obvious holes (including code from ChainComplexExtras,TateOnProducts)
--  tensor product along a ring map?
--  Hom and ** should play well with direct sums
--  turn HH^* into a functor
--  connecting homomorphisms
--  Ext elements to Ext modules (and back)
--  isMinimalChainComplex?  Maybe have: isFreeComplex
--  isExact, isQuasiIsomorphism
--  minimize
--  resolutionOfChainComplex (same name as resolution)
--  truncation (4 truncations: naive/smart, either side) (naive, canonical).
-- 6 standard homomorphisms
--   tensor commutativity
--   tensor associativity
--   adjointness
--   Hom swap Hom(C,Hom(D,E)) = Hom(D,Hom(C,E)).
--   Tensor evaluation (Hom(C,D)**E --> Hom(C,D**E)).
--   Hom evaluation: C**Hom(D,E) --> Hom(Hom(C,D),E)
--
--   go through all methods, and decide which ones should have a LengthLimit option
--   e.g.: isExact, isQuasiIsomorphism.
--
-- make a new constructor:
--   complex ComplexMap
--   which: takes an f : C --> C of degree -1
--   and creates a new complex with this as differential
--   (assuming f^2 = 0, which will not be implicitly checked).

-- question about ** and Hom:
--   add tensorProduct{F0, F1, F2, ...} ?
--   what if A_i, B_j already have a direct sum structure
--     what are the indices of A ** B ? or A ++ B
--   tensorAssociativity, with an option to go to tensorProduct(A,B,C)
--   tensorProduct{A,B,C,...}
--   tensorCommutativity(A,B)
--
-- 3. Other functionality
--   Quotient complexes C/D.
--   cartanEilenbergResolution
--   complexes of Eagon-Northcott type
--
-- 4. This package should be able to handle the case where the complex
--   is a complex of Modules, Complexes, or CoherentSheaves

newPackage(
        "Complexes",
        Version => "0.3", 
        Date => "16 November 2017",
    	Authors => {
	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
        Headline => "Chain complexes",
        AuxiliaryFiles => false,
        DebuggingMode => true
        )

export {
    "component",
    -- types
    "Complex",
    "ComplexMap",
    -- functions/methods
    "canonicalMap",
    "complex",
    "concentration",
    "cylinder",
    "freeResolution",
    "homotopic",
    "isComplexMorphism",
    "isQuasiIsomorphism",
    "isNullHomotopyOf",
    "isSemifreeComplex",
    "liftMapAlongQuasiIsomorphism",
    "minimize",
    "nullHomotopic",
    "nullHomotopy",
    "naiveTruncation",
    "randomComplexMap",
    "resolutionMap",
    -- options used
    "Cycle",
    "Boundary",
    "InternalDegree",
    "homotopy",
    "Base",
    "ResolutionMap",
    "UseTarget",
    "HomWithComponents"
    }

<< "----------------------------------------------------------------------------" << endl
<< "-- Experimental package                                                   --" << endl;
<< "-- This package will replace ChainComplexes at                            --" << endl;
<< "-- a future date.  The type 'Complex' here will                           --" << endl;
<< "-- be changed to 'ChainComplex'.                                          --"<< endl;
<< "--                                                                        --" << endl;
<< "-- Purpose: to more fully implement functoriality in homological algebra  --" << endl;
<< "-- Primary authors: Greg Smith and Mike Stillman                          --" << endl;
<< "--                                                                        --" << endl;
<< "-- For questions, suggestions, comments, bugs, please email either author --" << endl;
<< "----------------------------------------------------------------------------" << endl

--load(currentFileDirectory | "Complexes/res.m2")
unimplemented = str -> error(str|": not yet implemented")
UNTEST = (str) -> null

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
ComplexMap.synonym = "homomorphism of complexes"

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
complex ChainComplexMap := opts -> (f) -> (
    -- f : C --> C, degree -1, then return (C,f) as a complex.
    -- f : C --> C[-1], return (C,f) as a complex
    -- complex (0 * dd^C)
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
    lo := max(loC, loD+deg);
    hi := min(hiC, hiD+deg);
    if lo > hi then (loC, loC) else (lo, hi)
    )

max Complex := C -> max concentration C
min Complex := C -> min concentration C

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
    
{*          
          
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
*}

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
    if lo === hi then (
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

freeResolution = method(Options => options resolution)
freeResolution Ideal :=
freeResolution Module := opts -> M -> (
    C := res(M,opts);
    complete C;
    maps := for i from 1 to length C list C.dd_i;
    complex maps
    )

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
degree ComplexMap := Complex => f -> f.degree

map(Complex, Complex, HashTable) := opts -> (tar, src, maps) -> (
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
    for k in keys maps do (
        if not instance(k,ZZ) or k < lo or k > hi then
            error("expected keys to be integers in the range "|toString lo|".."|toString hi);
        f := maps#k;
        if source f =!= src_k then
            error ("map with index "|k|" has inconsistent source");
        if target f =!= tar_(k+deg) then
            error ("map with index "|k|" has inconsistent target");
        );
    new ComplexMap from {
        symbol source => src,
        symbol target => tar,
        symbol degree => deg,
        symbol map => maps,
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

map(Complex, Complex, ZZ) := ComplexMap => opts -> (tar, src, j) -> (
    if j != 0 then error "expected integer to be zero";
    result := map(tar,src,hashTable{},opts);
    result.cache.isCommutative = true;
    result
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
    if f.cache.?isCommutative and f.cache.isCommutative and isCommutative ring f then
        result.cache.isCommutative = true;
    result
    )

Number * ComplexMap := (r,f) -> (
    try r = promote(r,ring f) else error "can't promote scalar to ring of complex homomorphism";
    r * f
    )

- ComplexMap := (f) -> (
    result := (-1)*f;
    if f.cache.?isCommutative and f.cache.isCommutative then
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
    if f.cache.?isCommutative and g.cache.?isCommutative 
      and f.cache.isCommutative and g.cache.isCommutative then
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
    if f.cache.?isCommutative and g.cache.?isCommutative 
      and f.cache.isCommutative and g.cache.isCommutative then
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
    if all(args, f -> f.cache.?isCommutative and f.cache.isCommutative) then 
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
    if f.cache.?isCommutative and f.cache.isCommutative then
        result.cache.isCommutative = true;
    result
    )

isCommutative ComplexMap := (cacheValue symbol isCommutative)(f -> (
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
                    << "block " << (i,i-1) << " fails to commute" << endl;
                    );
                return false;
                )
            )
        );
    true
    ))

isComplexMorphism = method(TypicalValue => Boolean)
isComplexMorphism ComplexMap := (f) -> degree f === 0 and isCommutative f
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

------------------------
-- homology ------------
------------------------
minimalPresentation ComplexMap := 
prune ComplexMap := ComplexMap => opts -> f -> (
    map(minimalPresentation target f, minimalPresentation source f, k -> minimalPresentation f_k)
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
Hom(Module, Complex) := Complex => (M,D) -> Hom(complex M, D)
Hom(Complex, Module) := Complex => (C,N) -> Hom(C, complex N)
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
    if f.cache.?isCommutative and f.cache.isCommutative 
        and g.cache.?isCommutative and g.cache.isCommutative then
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
    if f.cache.?isCommutative and f.cache.isCommutative then
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

ComplexMap ** ComplexMap := ComplexMap => (f,g) -> (
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
    if f.cache.?isCommutative and f.cache.isCommutative 
        and g.cache.?isCommutative and g.cache.isCommutative then
        result.cache.isCommutative = true;
    result    
    -- f : C1 --> C2, g : D1 --> D2
    -- f**g : C1**D1 --> C2**D2
    -- (f**g)_i : C1_j ** D1_(i-j) --> C2_(j+df) ** D2_(i-j+dg)
    )
Complex ** ComplexMap := ComplexMap => (C,g) -> id_C ** g
ComplexMap ** Complex := ComplexMap => (f,D) -> f ** id_D
Module ** ComplexMap := ComplexMap => (M,g) -> (complex M) ** g
ComplexMap ** Module := ComplexMap => (f,N) -> f ** (complex N)
--------------
-- sign convection: Using Conrad (Grothendieck Duality) sign choice for cone, pg 8 of intro. 
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
    result := complex maps;
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
    lo := min(loB+1,loC);
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

isQuasiIsomorphism = method()
isQuasiIsomorphism ComplexMap := f -> HH cone f == 0

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

ker ComplexMap := opts -> f -> (
    -- TODO: we only need that the map is commutative, but
    -- then the degrees need to be shifted.
    if not isComplexMorphism f then 
        error "expected a complex morphism";
    -- f : B --> C
    -- NOTE: options of ker do not apply here.
    B := source f;
    (lo,hi) := B.concentration;
    modules := hashTable for i from lo to hi list i => kernel f_i;
    inducedMaps := hashTable for i from lo to hi list i => inducedMap(B_i, modules#i);
    maps := hashTable for i from lo+1 to hi list i => (
           (dd^B_i * inducedMaps#i) // inducedMaps#(i-1)
        );
    result := complex maps;
    result.cache.kernel = f;
    result
    )
coker ComplexMap := f -> (
    -- f : B --> C
    C := target f;
    (lo,hi) := C.concentration;
    deg := degree f;
    modules := hashTable for i from lo to hi list i => cokernel f_(i-deg);
    maps := hashTable for i from lo+1 to hi list i => (
           map(modules#(i-1), modules#i, matrix dd^C_i)
        );
    result := complex maps;
    if not isWellDefined result then
        error "expected chain map to induce a well-defined differential on the cokernel";
    result.cache.cokernel = f;
    result
    )
image ComplexMap := f -> (
    -- f : B --> C
    -- TODO: we only need that the map is commutative, but
    -- then the degrees need to be shifted.
    if not isComplexMorphism f then 
        error "expected a complex morphism";
    B := source f;
    C := target f;
    (lo,hi) := C.concentration;
    modules := hashTable for i from lo to hi list i => image f_i;
    maps := hashTable for i from lo+1 to hi list i => (
           map(modules#(i-1), modules#i, matrix dd^B_i)
        );
    result := complex maps;
    result.cache.image = f;
    result
    )
coimage ComplexMap := f -> (
    -- f : B --> C
    -- TODO: we only need that the map is commutative, but
    -- then the degrees need to be shifted.
    if not isComplexMorphism f then 
        error "expected a complex morphism";
    B := source f;
    C := target f;
    (lo,hi) := C.concentration;
    modules := hashTable for i from lo to hi list i => coimage f_i;
    maps := hashTable for i from lo+1 to hi list i => (
           map(modules#(i-1), modules#i, matrix dd^B_i)
        );
    result := complex maps;
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
    if opts.Verify then (
        if not isComplexMorphism result
        then error "map cannot be extended";
        );
    result.cache.isCommutative = true;
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
    G := if o.Boundary then image dd^E_(deg-1)
         else if o.Cycle then ker dd^E_deg
         else E_deg;
    B := basis(ideg, G);
    g := B * random(source B, S^{-ideg});
    if o.Boundary then (
        g = map(E_deg, G, gens g)
        );
    homomorphism(deg, g, E)
    )
-*
    E := Hom(C,D);
    KE := ker dd^E_0;
    B := basis(0, KE);
    S := ring E;
    g := B * random(source B, S^1);
    homomorphism(0, g, E)
    )  
*-
-- CCCC

isNullHomotopyOf = method()
homotopic = method()
nullHomotopic = method() -- this function checks whether f is null homotopic
nullHomotopy = method() -- this function attempts to construct one, might fail

isNullHomotopyOf(ComplexMap, ComplexMap) := (h, f) -> (
    -- returns true if h is a null homotopy for f : C --> D.
    -- if debugLevel > 0, then more info as to where it is not, is given
    C := source f;
    D := target f;
    deg := degree h;
    (lo,hi) := concentration h;
    if debugLevel == 0 then (
        for i from lo to hi do (
            if h_(i-1) * dd^C_i + dd^D_(deg+i) * h_i != f_i then return false;
            );
        true
        )
    else (
        result := true;
        for i from lo to hi do (
            if h_(i-1) * dd^C_i + dd^D_(deg+i) * h_i != f_i then (
                << "fails to be a null homotopy at location " << i << endl;
                result = false;
                );
            );
        result
        )
    )

nullHomotopy ComplexMap := (f) -> (
    -- key assumption: 'source f' is a complex of free modules
    C := source f;
    D := target f;
    deg := degree f + 1;
    hs := new MutableHashTable;
    (lo,hi) := concentration f;
    for i from lo to hi do (
        if hs#?(i-1) then 
            hs#i = (f_i - hs#(i-1) * dd^C_i) // (dd^D_(i+deg))
        else
            hs#i = f_i // dd^D_(i+deg)
        );
    -- result is a ComplexMap h : C --> D, of degree degree(f)+1
    map(D, C, new HashTable from hs, Degree => deg)
    )

homotopic(ComplexMap, ComplexMap) := (f,g) -> nullHomotopic(f-g)
    -- key assumptions: 
    --  f, g: C --> D
    --  C is a free complex.

nullHomotopic ComplexMap := f -> isNullHomotopyOf(nullHomotopy f, f)

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

-* -- Greg + Mike think this is old code that might be useful.  Are we right?!
basicHom = (C,D) -> (
    R := ring C;
    if ring D =!= R then error "expected complexes over the same ring";
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    modules := hashTable for i from loD-hiC to hiD-loC list i => (
        directSum for j from loC to hiC list {j,j+i} => Hom(C_j, D_(j+i))
        );
    if loC === hiC and loD === hiD then (
        return complex(modules#(loD-hiC), Base => loD-loC)
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
                        if k-j === {0,1} then (-1)^(k#0) * Hom(C_(k#0),dd^D_(k#1))
                        else if k-j === { -1,0 } then Hom(dd^C_(j#0),D_(k#1))
                        else 0)
                    ))));
    complex maps
    )

Hom(Complex, Complex) := Complex => (C, D) -> (
   if not isDirectSum C and not isDirectSum D then (
       return basicHom(C,D)
       );
   if isDirectSum C and not isDirectSum D then (
       return directSum for i from 0 to #(indices C) - 1 list 
           (indices C)#i => Hom((components C)#i, D);
       );
   if not isDirectSum C and isDirectSum D then (
       return directSum for i from 0 to #(indices D) - 1 list 
           (indices D)#i => Hom(C, (components D)#i);
       );
   if isDirectSum C and isDirectSum D then (
       return directSum flatten (
         for i from 0 to #(indices C) - 1 list
           for j from 0 to #(indices D) - 1 list
             {(indices C)#i, (indices D)#j} => 
               Hom((components C)#i, (components D)#i)
       ));
   )
*-

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

-- BBBB
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
    g1 := map(D_(hi+1), source gens HC1, gens HC1);
    g2 := map(HC1, source gens HC1, 1);
    h := g1 * ((a2 * a1)//g2);
    L1 := complex(append(for i from lo+1 to hi list dd^L0_i, h^[0]), Base=>lo);
    map(C,L1,i -> if i === hi+1 then -h^[1] else lambda_i)
    )

resolutionMap = method(Options => {Minimize=>true})
resolutionMap Complex := opts -> (cacheValue symbol ResolutionMap)(C -> (
    (lo,hi) := concentration C;
    f := map(C, complex((ring C)^0, Base=>lo-1), 0);
    local g;
    while (
        g = nextLambda f;
        g =!= null
    ) do f = g;
    -- the following line removes a 0 in the lo-1 spot, which is a byproduct
    -- of the base case above.
    naiveTruncation(f,(lo,infinity))
    ))

resolution Complex := opts -> C -> source resolutionMap C    

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
        if delta % dd^Cbeta_(i+1) != 0 then << "oops, our logic is wrong!" << endl;
        eps := delta // dd^Cbeta_(i+1);
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
minimize = method ()
minimize Complex := C -> (
    (lo,hi) := concentration C;
    S := ring C;
    ev0 := map(S, S, for x in gens S list 0_S);
    g := hashTable for i from lo to hi+1 list i => syz ev0 C.dd_i;
    rho := hashTable for i from lo to hi+1 list i => (
        id_(C_i) - g#i * (id_(target g#i)//g#i)
        );
    maps := hashTable for i from lo to hi list i => rho#i | C.dd_(i+1)*rho#(i+1);
    C' := coker map(C, C ++ C[1], maps, Degree=>0);
    pmC := prune C';
    -- here is a way to get the inverse of the pruning map.
    --phiInv := map(pmC, C, i -> (pmC_i.cache.pruningMap)^(-1) * inducedMap(C'_i, C_i));
    phi := map(C, pmC, i -> map(C_i, pmC_i, pmC_i.cache.pruningMap));
    pmC.cache.pruningMap = phi;
    pmC
    )

  -- problem currently: in code below, phi is not a complex morphism.
  --  so the differential in the cokernel isn't (doesn't appear to be) well-defined.
  --  Let's do this on a simpler example, to see.
  minimize ChainComplex := E ->(
    --To simplify the notation consider the complex C = E[min E] that
    --is shifted so that the first nonzero module is C_0.
    --The algorithm:
    --Set dbar = the reduction of the differential d mod the maximal ideal.
    --choose a complement of ker dbar, and compute the idempotent rho: E -> E.
    -- the map rho is not a chain complex map, but the image of 
    --(rho | d*rho): C ++ C[1] --> C is a subcomplex and 
    --the minimization of  C is the complex C/image(rho|d*rho).
    --The script returns the ChainComplexMap from the minimization to C.
    complete E;
    C:= E[min E]; -- now min C == 0.
    M := max C;
    S := ring C;
    red := map(S,S,toList(numgens S:0_S));
    --make maps g_i: ker(red C.dd_i) -> C_i
    g := hashTable for i from 0 to M+1 list {i,syz red C.dd_i};
    --For each i choose an idempotent rho#i:C_i\to C_i
    --whose image is the complement
    --image g#i, Note that rho#0 = 0.
    rho := hashTable for i from 0 to M+1 list 
	{i,id_(C_i) - g#i*(id_(target g#i)//g#i)};
    phi := map(C, C++C[1], i-> rho#i | C.dd_(i+1)*rho#(i+1));
    minC := coker phi;
    pmC := prune minC;
    m := map(pmC, C, i-> (pmC_i.cache.pruningMap)^(-1) * inducedMap(minC_i, C_i));
    error "debug me";
    m[-min E]
    )

beginDocumentation()

undocumented{
    (net, Complex),
    (net, ComplexMap),
    (component,Module,Thing),
    component
    }

UNTEST ///
restart
needsPackage "Complexes"
  R = ZZ/101[a..c, DegreeRank=>3]
  A = complex R^{{-1,0,0},{-2,0,0},{-3,0,0}}
  B = complex R^{{0,-1,0},{0,-2,0}}
  C = complex R^{{0,0,-1},{0,0,-2}}
  F = tensorAssociativity(A,B,C)
  assert(degrees source F_0 == degrees target F_0)
  A**B
  
  R = ZZ/101
  A = (complex R^2)[3]
  B = complex R^4
  A**B
  Hom(A,B)
///

UNTEST ///
{*
restart
needsPackage "Complexes"
*}
  R = ZZ/101[a..f]
  A = freeResolution coker matrix{{a,b}}
  B = freeResolution monomialCurveIdeal(R,{1,2,3})
  C = freeResolution monomialCurveIdeal(R,{1,3,4})

  A' = res coker matrix{{a,b}}
  B' = res monomialCurveIdeal(R,{1,2,3})
  C' = res monomialCurveIdeal(R,{1,3,4})
  indices(A**B)_2
  indices ((A**B)**C)_2
  indices (A**(B**C))_2

  f = tensorAssociativity(A,B,C);
  isWellDefined f
  assert(ker f == 0)
  assert(coker f == 0)
  (lo,hi) = concentration source f
  ((A**B)**C)_2
  (A**(B**C))_2
  (A**B)_2
  ((A**B)**C)_2
  
  R = ZZ/101
  A = (complex R^2)[3]
  B = complex R^4
  A**B
  Hom(A,B)
///

doc ///
   Key
     Complexes
   Headline
     Homological data types and routines
   Description
    Text
      This package provides routines for studying homology in a general algebraic setting.
   Subnodes
     "Making complexes"
///

doc ///
   Key
     Complex
   Headline
     the class of all chain or cochain complexes
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
       true if {\tt C} determines a well defined complex
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

{* -- The following block of text doesn't validate anymore, and goes right after the last Example above this.
    Text
      @SUBSECTION "Programming Details"@
      The function also checks the data structure for the following:
      @UL {
          {"The keys are exactly ring, concentration, module, dd, cache"},
          }@
*}
doc ///
   Key
     (complex, Module)
     (complex, Ideal)
     (complex, Ring)
   Headline
     make a chain or cochain complex of length zero
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
      In contrast to @TO (complex,HashTable)@, this constructor
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
     complex
     (complex, HashTable)
     (complex, List)
     Base
   Headline
     make a chain or cochain complex
   Usage
     complex H
     complex L
   Inputs
     H:HashTable
       each key is an integer indexing a differential, and the 
         value at that key is the map
     L:List
       of maps
     Base => ZZ
       this argument is unused if the input is a hash table, 
         but if the input is a list, this
         determines the index of the target of the first map 
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
      Often, a complex is most easily described by giving a list of
      consecutive maps which form the differential.  
    Example
      C1 = complex{F1,F2,F3}
      isWellDefined C1
      C1 == C
      C2 = complex({F1,F2,F3}, Base => 3)
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
///

doc ///
   Key
     (symbol SPACE, Complex, Array)
   Headline
     shift a complex
   Usage
     D = C[i]
   Inputs
     C:Complex
     :Array
       {\tt [i]}, where {\tt i} is an integer
   Outputs
     D:Complex
       where $D_j = C_{i+j}$, and the sign of the differential is changed if $i$ is odd
   Description
    Text
      The shift defines a natural automorphism on the category of complexes. 
      Topologists often call the shifted complex $C[1]$ the {\it suspension} of $C$.
    Example
      S = ZZ/101[a..d]
      C = freeResolution coker vars S
      dd^C_3
      D = C[1]
      dd^D_2 == -dd^C_3
    Text
      In order to shift the complex one step, and not change the differential, one
      can do the following.
    Example
      (lo,hi) = concentration C
      E = complex(for i from lo+1 to hi list dd^C_i, Base=>-1)
      dd^E_2 == dd^C_3
   SeeAlso
     concentration
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
       whether {\tt C} and {\tt D} are equal
   Description
    Text
      Two complexes are equal if the corresponding 
      objects and corresponding maps at each index are equal.
    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      D = C[0]
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
      The direct sum of two complexes is another complex in the same category.
    Example
      S = ZZ/101[a,b,c];
      C1 = freeResolution coker vars S
      C1 ++ complex(S^13)[-2]
      C2 = complex (ideal(a,b,c))
      C1 ++ C2
    Text
      The direct sum of a sequence of complexes can be computed as follows.
    Example
      C3 = directSum(C1,C2,C2[-2])
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
      Given a complex which is a direct sum, we obtain the component
      complexes and their names (indices) as follows.
    Example
      components C4
      indices C4
   SeeAlso
     (components,Complex)
     indices
     (symbol^, Complex, Array)
     (symbol_, Complex, Array)
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
     (ring,Complex)
   Headline
     access the ring of a complex
   Usage
     ring C
   Inputs
     C:Complex
   Outputs
     :Ring
   Description
    Text
      Every complex has a base ring.  This function access that information.
    Example
      S = ZZ/101[a,b,c,d];
      C = freeResolution coker vars S
      ring C
      ring C === S
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
      The differential on $C1_j \otimes C2_k$ is the differential $dd^{C1} \otimes id_{C2} + (-1)^j id_{C1} \otimes dd^{C2}$.
      
      As the next example illustrates, the Koszul complex can be constructed via iterated tensor products.
    Example
      S = ZZ/101[a..c]
      Ca = complex {matrix{{a}}}
      Cb = complex {matrix{{b}}}
      Cc = complex {matrix{{c}}}
      Cab = Cb ** Ca
      dd^Cab
      indices Cab_1
      Cab_1_[{1,0}]
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      dd^Cabc
    Text
      If one of the arguments is a module, it is considered as a complex concentrated in homological degree 0.
    Example
      Cabc ** (S^1/(a,b,c))
      S^2 ** Cabc
    Text
      Let's check the differential (Once the BUG is fixed TODO)!!
    Example
      Cabc_2
      --indices Cabc_2
      --dd^Cc ** id_Cab
   Caveat
   SeeAlso
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
      the direct sum of $Hom(C1_j, C2_(j+i))$ over all $j$.
      The differential on $Hom(C1_j, C2_(j+i))$ is the differential $Hom(id_{C1}, dd^{C2}) + (-1)^j Hom(dd^{C1}, id_{C2})$.
      $dd^{C1} \otimes id_{C2} + (-1)^j id_{C1} \otimes dd^{C2}$.

    Example
      S = ZZ/101[a..c]
      C = freeResolution coker vars S
      D = Hom(C,C)
      dd^D
    Text
      The homology of this complex is Hom(C, ZZ/101)
    Example
      prune HH D == Hom(C, coker vars S)
    Text
      If one of the arguments is a module, it is considered as a complex concentrated in homological degree 0.
    Example
      E = Hom(C, S^1)
      prune HH E
   SeeAlso
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

doc ///
   Key
     (dual,Complex)
   Headline
     makes the dual of a complex
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
      S = ZZ/101[a..d]
      B = intersect(ideal(a,c),ideal(b,d))
      C1 = freeResolution B
      C2 = dual C1
      prune HH C2
      Ext^2(S^1/B, S)
      Ext^3(S^1/B, S)
   SeeAlso
     (Hom, Complex, Complex)
     Ext
///

doc ///
  Key
    ComplexMap
  Headline
    the class of all homomorphisms of complexes
  Description
    Text
      A complex homomorphism $f : C \rightarrow D$ of degree $d$ is a
      sequence of maps $f_i : C_i \rightarrow D_{d+i}$.  
      No relationship between the maps $f_i$ and 
      and the differentials of either $C$ or $D$ is assumed.
      
      The set of all complex homomorphisms from $C$ to $D$ form
      the complex $Hom(C,D)$ where $Hom(C,D)_d$ consists of the
      homomorphisms of degree $d$.

      The usual algebraic operations are available: addition,
      subtraction, scalar multiplication, and composition. The
      identity map from a chain complex to itself can be produced with
      @TO "id"@. An attempt to add (subtract, or compare) a ring
      element to a chain complex will result in the ring element being
      multiplied by the appropriate identity map.
  SeeAlso
    Complex
///


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


TEST ///
  -- test creation of complexes 1: via free resolutions
{*
  restart
  needsPackage "Complexes"
*}
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
{*
  restart
  needsPackage "Complexes"
*}

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
{*
  restart
  needsPackage "Complexes"
*}
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
{*
restart
needsPackage "Complexes"
*}
  S = ZZ/101[a..d]
  C = freeResolution coker matrix{{a,b^2,c^3,d^4}}
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
{*
restart
needsPackage "Complexes"
*}
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
  assert(C =!= D)
  assert(C == D)

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
  assert(C =!= D)
  assert(C == D)

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
  cone f2
  --- cone f3 -- fails -- TODO: fix this bug
  --- cone f4 -- fails in same way -- TODO: fix this bug
  cone f5 -- lot's of zeros....
  cone f6 -- lot's of zeros....
  assert(cone f5 == complex(ZZ^1, Base=>4))
///

TEST ///
  -- Greg: CCCC
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

  nullHomotopic bds_0

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
{*
restart
*}
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
  homs = select(keys C.cache, x -> first x === Hom)
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
  -- resolution of a complex
-*  
  restart
  debug needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = Hom(freeResolution I, R^1/I)

  elapsedTime f = resolutionMap C;
  assert(target f === C)
  assert(isWellDefined f)
  assert(isQuasiIsomorphism f)
  assert(isComplexMorphism f)
  assert(coker f == 0)
  assert(kernel HH f == 0)
  assert(cokernel HH f == 0)
  assert(resolution C == source f)

  D = resolution C
  prune HH C

  MD = minimize D;
  assert(MD == D)
  assert isWellDefined MD
  p1 = MD.cache.pruningMap;
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
  p1 = D1.cache.pruningMap;
  assert isWellDefined p1
  assert isComplexMorphism p1
  assert isQuasiIsomorphism p1  
///

TEST ///  
-- BBBB 14 Dec 2017 playing 
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
  chainComplex Complex := (C) -> (
      (lo,hi) := concentration C;
      chainComplex hashTable for i from lo+1 to hi list i => dd^C_i
      )
*-
  
///


TEST ///  
-- BBBB 14 Dec 2017 playing 
-*
  restart
  debug needsPackage "Complexes"
*-
  S = ZZ/101[a,b,c,d,e]
  C = complex {id_(S^1)}
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
///

-- the following test tests code that is not yet ready
UNTEST ///  
-- AAA 18 Feb 2018 resolutions and lifting maps
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
  liftMapAlongQuasiIsomorphism(f,f)  
  beta = resolutionMap f

  
  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  FI = freeResolution I

  resolutionMap complex {S^1} -- fails.

  C = Hom(FI, S^1/I)
  D = Hom(FJ, S^1/J)
  fC = resolutionMap C
  assert(dd^(source fC ** coker vars S) == 0)

  red = map(S^1/I, S^1/J, 1)
  isWellDefined red
  h = extend(FI, FJ, map(FI_0, FJ_0, 1))
  C = freeResolution I
  -- TODO: what is this line supposed to be doing?? f = resolutionMap C


  -- AAA
  -- start with two complexes M, N (better if not free)
  -- find a morphism M --> N, via Z_0(Hom(M,N))
  -- compute resolutions FM, FN of M, N.
  -- lift map FM --> M --> N along FN --> N.

  S = ZZ/101[a,b,c,d,e]
  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  J = ideal(a*b, b*c*d, a*e, c*e)
  FI = freeResolution I
  FJ = freeResolution J  
  M = Hom(FI, S^1/I)
  N = Hom(FJ, S^1/J)
  HMN = Hom(M,N);
  KMN = ker dd^HMN_0;
  map(HMN, complex {source KMN_{2}}, {(generators KMN)_{2}})
  
///

end------------------------------------------------------------

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


restart
uninstallPackage "Complexes"

restart
needsPackage "Complexes"
check oo

restart
installPackage "Complexes"
viewHelp

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

doc ///
   Key
   Headline
   Usage
   Inputs
   Outputs
   Consequences
    Item
   Description
    Text
    Code
    Pre
    Example
    CannedExample
   Subnodes
   Caveat
   SeeAlso
///

TEST ///
  -- creation of chain complexes
  restart
  needsPackage "Complexes"
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
  needsPackage "Complexes"
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
  restart
  needsPackage "Complexes"
  R = QQ[a..d]
  CR = res coker vars R
  maps = for i from 1 to 4 list CR.dd_i
  C = complex maps
  D = C[1]
  E = C ++ D
  E = (symbol i => C) ++ (symbol j => D)
  indices E
  components E
  f1 = E^[i]
  f2 = E_[i]
  f1 * f2 == id_(source f2)
  f2 * f1 

  E = C ++ D
  f1 = E^[0]
  f2 = E_[0]
  f1*f2 == 1
  f2 * f1

  D = C[10]
  E = C ++ D

  E^[i]
  -- given f : C --> D, a ComplexMap
  --  want composition of f and D --> D[deg]
  -- want: map((target f)[d], source f, f)
  -- f : C --> C[d], f has degree d.
  -- inducedMap(C[d], C)
  -- map(D, C, f)
  -- map(D[deg], D, 1, Degree=>deg)
  -- map(D,C,f,Degree=>deg)
  --   input: f_i : (source f)_i --> (target f)_(degree f+i)
  --   output: 
  f = id_C
  dC = map(C[-1], C, dd^C, Degree=>0)
  C_0
  (C[1])_0 === C_1

  f ++ dd^C
  
  m = dd^C_2
  m1 = map((target m) ** R^{-1}, source m, m, Degree=>1)
  degree m1
  isHomogeneous m1
///

TEST ///
  restart
  needsPackage "Complexes"
  R = QQ[a..d]
  CR = res coker vars R
  maps = for i from 1 to 4 list CR.dd_i
  C = complex maps
    
  f = dd^C
  assert(source (f[1]) == (source f)[1])
  assert(target (f[1]) == (target f)[1])

  f == f[1]

///

TEST ///
  -- components of a direct sum
  -- functions supporting this:
  --   directSum
  --   ++
  --   components
  --   indices
  --   M_[...], M^[...]
  --   M_{...}, M^{...} NOT THESE
  --   indexComponents: key where this info is stored in the cacheTable
  --   
  restart
  loadPackage "Complexes"
  
  R = QQ[a..d]
  f1 = matrix{{a,b},{c,d}}
  components source f1 -- single free module
  F = (symbol i => R^2) ++ (symbol j => R^3)
  F^[j,i]
  F_[i]
  F.cache.indexComponents
  F.cache.components
  F.cache.indices

  indices F
  components F -- actual components, not indices

  -- directSum is implemented by a function with this key
  -- e.g. Module.directSum is in matrix.m2 line 254
  -- actually, indexComponents is set in Option.directSum, matrix.m2
  --  so: directSum maintains 'indexComponents'
  --  but other functions also use and set them:
  -- chaincomplexes.m2:
  --   ChainComplex ** ChainComplex
  --   ChainComplexMap ** ChainComplexMap
  --   TensorAssociativity
  --   trans, for ChainComplex _ Array, ChainComplex ^ Array
  -- gradedmodules.m2
  --   tensorAssociativity(GradedModule,GradedModule,GradedModule)
  -- matrix.m2
  --   Option.directSum
  -- modules2.m2
  --   Module ^ Array
  --   Module _ Array
///

doc ///
Key
  Complexes
Headline
  New implementation of chain complexes
Description
  Text
  Example
Caveat
  not writtten yet!
SeeAlso
  ChainComplex
///

end

TEST ///

  -- test of creation of new complexes from old
  restart
  needsPackage "Complexes"
  R = QQ[a..d]
  CR = res coker vars R
  maps = for i from 1 to 4 list CR.dd_i
  C = complex maps
  D = complex (R^6)
  
  assert((C[1])[-1] == C)
  C ++ D
  C ** D
  dual C  -- error: not yet implemented
  Hom(C,D) 
  
  f = dd^C_1
  g = dd^C_3
  f = id_C
  g = id_D
  h = Hom(f,g)
  source h
  target h

  E = Hom(C,C)
  indices E_-2
  components E_-2
  E_-2^[{3,1}]
  target ((E_-2)^[{3,1}]) == source ((E_-2)_[{3,1}])
  (E_-2)^[{1,-1}]
  source ((E_-3)^[{2,0}])

  f = id_C
  g = a*id_C
  map g
  g == map g
///
TEST //
  -- of Hom(f,g)
  -- Hom(f,source g) * Hom(target f,g) === Hom(f,g)
  -- Hom(target f,g) * Hom(g,source g) === (sign) Hom(f,g)
  restart
  needsPackage "Complexes"
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
  -- test of ** 
  restart
  needsPackage "Complexes"
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  C_1 ** dd^D_2
  E = C**D
  F = Hom(C,D)
  
  C1 = res minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D1 = res coker matrix{{a^2, b^2, c^2}}
  E1 = C1**D1
  needsPackage "ChainComplexExtras"
  F1 = Hom(C1,D1)

  for i from 1 to 6 do assert(dd^E_i == E1.dd_i)
  for i from 1 to 6 do assert(dd^F_i == F1.dd_i)
  dd^E_2 == E1.dd_2
  (dd^F_-3, F1.dd_-3)
  (dd^F_3, F1.dd_3)  
  (dd^F_2, F1.dd_2)  
  (dd^F)^2
  F1.dd^2
///
TEST //
  -- of f**g
  -- Hom(f,source g) * Hom(target f,g) === Hom(f,g)
  -- Hom(target f,g) * Hom(g,source g) === (sign) Hom(f,g)
  restart
  needsPackage "Complexes"
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

  E = freeResolution cokernel matrix{{a*b, c*d}}
  -- if f and g are composable, then should have:
  -- (f ** E) * (g ** E) == (f*g) ** E
///

TEST ///
  -- of isComplexMorphism, isCommutative
  restart
  needsPackage "Complexes"
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
  restart
  needsPackage "Complexes"
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
{*
  restart
  needsPackage "Complexes"
*}

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
  restart
  needsPackage "Complexes"
  
  S = ZZ/101[a..d]
  I = ideal(a^2, b^2, c^2, d^2)
  J = ideal(a,b,c,d)
  J1 = monomialIdeal J
  isWellDefined complex J
  isWellDefined complex J1
  M = (complex S^0)[5]
  isWellDefined M
  C = freeResolution comodule I
  D = freeResolution comodule J
  isWellDefined C
  isWellDefined D
  C1 = complex(I/I)
  isWellDefined C1
  isWellDefined (dd^C)
  isWellDefined (dd^C1)
  C1 ++ C1[3]
  (complex (S^1/I))
  (complex (S/I)^1)
  (complex (S/I))[6]
  C1 ++ C1[3] ++ (complex (S/I))[6] -- gives error message as desired.
  assert isWellDefined (C1 ++ C1[3] ++ (complex (S^1/I))[6])
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  F = extend(D[4],C[4],map(S^1,S^1,1))
  isComplexMorphism F
  isWellDefined F
  F = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism F
  assert isWellDefined F
  betti C
  betti D
  betti (C[4])
  betti (C**D)
  isHomogeneous F  
  isHomogeneous source F
  isHomogeneous target F
  kerF = ker F
  prune kerF == 0
  prune kerF
  (prune kerF).cache.pruningMap
  
  S = ZZ/101[a..d]
  I = ideal(a^2, b^2, c^2, d^2-a)
  J = ideal(a,b,c,d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  isComplexMorphism extend(D[4],C[4],map(S^1,S^1,1))
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
  restart
  needsPackage "Complexes"

  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C  
  ker f1
  Cf = coker f1
  (dd^Cf)^2 == 0
  imf = image f1
  (dd^imf)^2
  E = cone f1
  F1 = canonicalMap(cone f1, target f1)
  F2 = canonicalMap((source f1)[-1], cone f1)
  F2 * F1 == 0
  assert(ker F2 == image F1)
  imf2 = prune imf
  g = imf2.cache.pruningMap
  assert(coker g == 0 and ker g == 0)
  
  E = cylinder f1
  G1 = canonicalMap(E, target f1, UseTarget=>true)
  G2 = canonicalMap(E, source f1, UseTarget=>false)
  G3 = canonicalMap(target f1, E)
  G4 = canonicalMap(cone f1, E)  
  assert(G4 * G2 == 0)
  assert(kernel G4 == image G2)

  coimage F1 == image F1
  coimage G2 == image G2


  -- ker, coker, image, coimage canonical maps
  f = G2
  h1 = canonicalMap(source f, kernel f)
  h2 = canonicalMap(coimage f, source f)
  h3 = canonicalMap(target f, image f)
  h4 = canonicalMap(cokernel f, target f)
  assert(h2 * h1 == 0)
  assert(kernel h2 == image h1)
  assert(h4 * h3 == 0)
  assert(kernel h4 == image h3)
  
  
  E1 = (target f1) ++ (source f1)[-1]
  E1^[0]    
  E1_[1]
  R = QQ[a..d]
  C3 = complex {matrix{{d}}}
  C2 = complex {matrix{{c}}}
  C1 = complex {matrix{{b}}}
  C0 = complex {matrix{{a}}}
  C3a = map(C3, C3 ** R^{-1}, c * id_C3)
  isHomogeneous(C3a_1)
  source C3a_1
  target C3a_1
  degree C3a_1
  K2 = cone C3a
  K3 = cone map(K2, K2 ** R^{-1}, b * id_K2)
  K4 = cone map(K3, K3 ** R^{-1}, a * id_K3)
  dd^K3
  dd^K2
  dd^K4
  C0 ** (C1 ** (C2 ** C3))
  C1 ** C2
  C1 ** C2
  C1_0
  C1_1
  isHomogeneous (dd^C1_1)
  source(dd^C1_1) == C1_1
  target(dd^C1_1) == C1_0
  dd^C1
  f1 = c * id_C1
  isHomogeneous(f1_1)
  isHomogeneous(f1_0)  
  source f1_0
  target f1_0
  isHomogeneous f1_0
  degree f1_0

  C2 = cone f1
  dd^C2
  
  map(E ++ E, E ++ E, {{id_E, 0}, {0, -id_E}})
  
  -- starting with f1, get the differential of the cone.
  C = source f1
  D = target f1
  f1' = (map(D[1], C, f1, Degree=>-1))[-1]
  map(C[-1] ++ D,
      C[-1] ++ D, 
      {{dd^(C[-1]), 0}, 
       {f1', dd^(target f1)}})
///

TEST ///
  -- cone short exact sequence
  R = QQ[a..f]

  F : B --> C
  C --> cone F
  cone F --> B[-1]

  ker F --> B
  C --> coker F
  image F --> C
  B --> coimage F (or coimage F)
  coimage F --> image F (an isomorphism)

  id_B || F : B --> B ++ C
  cylinder F == cone(id_B || F)
  -- 4 canonical maps
  C --> cylinder F
  B --> cylinder F
  cylinder F --> cone F
  cylinder F --> C
  -- important cylinder diagram:
  0 --> 0 --> C     --> cone F  --> B[-1]
  0 --> B --> cyl F --> cone F  --> 0 is exact
        B --> C     
  canonicalMap(cyl F, B)
  canonicalMap(cyl F, C)
  canonicalMap(cone F, cyl F)

  canonicalMap(cone F, C)
  canonicalMap(B[-1], cone F)
  
  R = QQ[a..f]
  F = genericMatrix(R,a,2,3)
  inducedMap(source F, ker F)
  inducedMap(coker F, target F)
  -- but 
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

end
restart
loadPackage "Complexes"
check oo
TEST ///
  restart
  needsPackage "Complexes"
  R = QQ[a..d]
  CR = res coker vars R
  maps = for i from 1 to 4 list CR.dd_i
  C = complex maps
  dd^C_2
  dd_C^(-1)
  (dd^C)^(-2)
  dd^C^-2
  Ca = complex(maps, Base=>-3)
  complex(R^1)
  map(C,C,0)  
  map(C,C,0,Degree=>-1)
  ring C
  debug Complexes
  f = C.dd
  f_4
  f_5
  f_0
  f_2
  
  C_2
  C_27
  C^(-2)
  
  C[1]
  complex(R^0)
  ring oo
///

///
for i from 0 to 5 list i => (if i == 2 then continue else i)
///

TEST ///
  -- creation of chain complexes
  restart
  needsPackage "Complexes"
  R = QQ[a..d]
  C = freeResolution(coker vars R)
  D = C[3]
  E = Hom(C,D)
  dd^E_1
  dd^E * dd^E
  dd^E_0
  dd^E
///

A = new Type of HashTable
B = new Type of A
a = new A from hashTable{1=>"hi"}
b = new B from a
a1 = new A from b

NEWCODE FOR ENGINE ///
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
