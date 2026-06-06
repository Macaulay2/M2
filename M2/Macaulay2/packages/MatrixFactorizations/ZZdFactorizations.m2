--needsPackage "Complexes"

ZZdFactorization = new Type of MutableHashTable --
  -- note: we make this mutable in order to construct the
  --   differential as a morphism of ZZdFactorizations (in the style of Complexes)
  -- BUT: after construction, it is should be IMMUTABLE!!
  -- at some point, we might want to allow lazy determination of the modules and maps
  -- but for now, we insist that all modules and maps are explicit.
  -- key:
  --  ring
  --  modules: hash table: ZZ => Module
  --  differential: ComplexMap from C --> C, degree -1 (mod d)
  --  period: an integer d, tells us for which d the factorization C is ZZ/d-graded
  --  cache: a CacheTable
  
 ZZdFactorizationMap = new Type of HashTable
  -- keys:
  --   degree: ZZ
  --   source: ZZ/d-graded factorization over a ring R
  --   target: ZZ/d-graded factorization over the same ring R
  --   maps themselves (HashTable of Matrices), keys lying in the concentration period of the source.
  --    not all of the keys maps#i, need be present.
  --    missing ones are presumed to be zero maps.
  --   cache: a CacheTable
  --    cache.isCommutative: whether this map commutes with the differentials
  --      not set until needed.  unset means we have not checked yet, 
  --          and the user hasn't declared it to be true/false yet.

ZZdFactorization.synonym = "ZZ/d graded factorization"
ZZdFactorizationMap.synonym = "map of ZZ/d-graded factorizations"
 
  
ring ZZdFactorization := Ring => C -> C.ring

period = method()
period ZZdFactorization := ZZ => C -> C.period

period ZZdFactorizationMap := ZZ => f -> (source f).period


ZZdfactorization = method(Options => {Base=>0})
ZZdfactorization HashTable := ZZdFactorization => opts -> maps -> (
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
    C := new ZZdFactorization from {
           symbol ring => R,
           symbol module => new HashTable from moduleList,
           symbol period => #spots,
           symbol cache => new CacheTable
           };
    C.dd = map(C,C,maps,Degree=>-1);
    if R.?rootOfUnity then C.cache.rootOfUnity = R.rootOfUnity;
    C
    )
ZZdfactorization List := ZZdFactorization => opts -> L -> (
    d:=length L;
    -- L is a list of matrices,  a list of modules, or a list of ring elements
    if not instance(opts.Base, ZZ) then
      error "expected Base to be an integer"; 
    if all(L, ell -> instance(ell,Matrix)) then (
        --trg := target L#0;
	mapHash := hashTable for i from 0 to #L-1 list
	         (if (opts.Base+i+1)%(#L) == 0 then #L else (opts.Base+i+1)%(#L)) => map(source(L#((i-1)%d)),source(L#(i%d)) , L#i);
        return ZZdfactorization(mapHash, opts)
        );
    --
    if all(L, ell -> instance(ell,RingElement)) then (
        trg2 := (ring(L#0))^1;
	mapHash2 := hashTable for i from 0 to #L-1 list
	            (if (opts.Base+i+1)%(#L) == 0 then #L else (opts.Base+i+1)%(#L))  => map(trg2, trg2, matrix{{L#i}});
        return ZZdfactorization(mapHash2, opts)
        );
    --
    if all(L, ell -> instance(ell,Module)) then (
        R := ring L#0;
        if any(L, ell -> ring ell =!= R) then
            error "expected modules all over the same ring";
        moduleHash := hashTable for i from 0 to #L-1 list (opts.Base + i)%(#L) => L#i;
        C := new ZZdFactorization from {
            symbol ring => R,
            symbol period => #L,
            symbol module => moduleHash,
            symbol cache => new CacheTable
            };
        C.dd = map(C,C,0,Degree=>-1);
        return C;
        );
    error "expected a list of matrices, a list of modules, or a list of ring elements";
    )

ZZdfactorization(Module, ZZ) := ZZdFactorization => opts -> (M,d) -> (
    if not instance(opts.Base, ZZ) then
      error "ZZ/d factorization: expected base to be an integer";
    if M.cache.?ZZdFactorization and opts.Base === 0 and period M.cache.ZZdFactorization == d then return M.cache.ZZdFactorization;
    C := new ZZdFactorization from {
           symbol ring => ring M,
           symbol period => d,
           symbol module => hashTable {opts.Base => M},
           symbol cache => new CacheTable
           };
    if opts.Base === 0 then M.cache.ZZdFactorization = C;
    C.dd = map(C,C,0,Degree=>-1);
    C
    ) 
ZZdfactorization(Ring, ZZ) := ZZdFactorization => opts -> (R, d) -> ZZdfactorization(R^1, d, opts)
ZZdfactorization(Ideal, ZZ) := ZZdFactorization => opts -> (I, d) -> ZZdfactorization(module I, d, opts)

ZZdfactorization ZZdFactorization := ZZdFactorization => opts -> C -> (
    -- all this does is change the homological degrees 
    -- so the factorization starts at a new base
    p := period C;
    if 0 === opts.Base then
        C
    else (
        L := for i from 1 to p list dd^C_i;
        ZZdfactorization(L, Base=>opts.Base)
        )
    )


ZZdFactorization _ ZZ := Module => (C,i) -> if C.module#?(i%C.period) then C.module#(i%C.period) else (ring C)^0

ZZdFactorization ^ ZZ := Module => (C,i) -> C_(-i)


Symbol ^ ZZdFactorization := ZZdFactorizationMap => (sym, C) -> (
    if sym === dd then C.dd
    else error "expected symbol to be 'dd'"
    )

net ZZdFactorization := C -> (
     (lo,hi) := (0,C.period);
     if lo > hi then 
         error "in a factorization, the period should be positive"
         --"0"
     else if lo == hi and C_lo === 0 then 
         "0"
     else
         horizontalJoin between(" <-- ", 
             for i from lo to hi list
                 stack (net C_i, " ", net (i%hi)))
     )



lineOnTop := (s) -> concatenate(width s : "-") || s

source ZZdFactorizationMap := ZZdFactorization => f -> f.source
target ZZdFactorizationMap := ZZdFactorization => f -> f.target
ring ZZdFactorizationMap := ZZdFactorization => f -> ring source f
degree ZZdFactorizationMap := ZZ => f -> f.degree

isHomogeneous ZZdFactorizationMap := (f) -> all(values f.map, isHomogeneous)

map(ZZdFactorization, ZZdFactorization, HashTable) := ZZdFactorizationMap => opts -> (tar, src, maps) -> (
    R := ring tar;
    if ring src =!= R or any(values maps, f -> ring f =!= R) then
        error "expected source, target and maps to be over the same ring";
    deg := if opts.Degree === null 
           then 0 
           else if instance(opts.Degree, ZZ) then 
             opts.Degree
           else
             error "expected integer degree";
    (lo,hi) := (0,src.period);
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
    new ZZdFactorizationMap from {
        symbol source => src,
        symbol target => tar,
        symbol degree => deg,
        symbol map => maps',
        symbol cache => new CacheTable
        }
    )
map(ZZdFactorization, ZZdFactorization, List) := ZZdFactorizationMap => opts -> (tar, src, maps) -> (
    -- case 1: maps is a (single) list of matrices (maps between components of the factorization)
    -- case 2: maps is a double list of ZZdFactorizationMap's
    -- 
    --  Can tell, depending on the class of maps#0.
    (lo,hi) := (0,src.period);
    if not instance(maps#0, List) then (
        mapHash := hashTable for i from lo to hi list i => (
            h := maps#((i-lo)%hi);
            if h == 0 then continue else h
            );
        return map(tar,src,mapHash,opts)
        );
    -- At this point, the first entry of 'maps' is a List.
    -- Check: it is a table of ZZdFactorizationMap
    R := ring tar;
    if R =!= ring src then error "expected factorizations over the same ring";
    if not isTable maps then error "expected a table of ZZdFactorizationMaps";
    -- check: all entries which are ZZdFactorizationMaps have the same homological degree
    deg := if opts.Degree === null 
           then null
           else if instance(opts.Degree, ZZ) then 
             opts.Degree
           else
             error "expected integer degree";
    degs := unique for f in flatten maps list 
        if instance(f,ZZdFactorizationMap) 
            then degree f 
            else continue;
    if #degs > 1 then error "expected all ZZdFactorizationMaps to have the same degree";
    if deg =!= null and #degs == 1 and degs#0 =!= deg then error "Supplied degree is incompatible with the ComplexMaps";
    if deg === null then deg = (if #degs == 1 then degs#0 else 0);
    -- At this point, we need to create (block) matrices for each component of the complex.
    mapHash = hashTable for i from lo to hi list i => (
        newmaps := applyTable(maps, f -> if instance(f,ZZdFactorizationMap) then f_i else f);
        h := map(tar_(i+deg), src_i, matrix newmaps);
        if h == 0 then continue else h
        );
    map(tar,src,mapHash,opts, Degree=>deg)
    )

map(ZZdFactorization, ZZdFactorization, Function) := ZZdFactorizationMap => opts -> (D,C,f) -> (
    deg := if opts.Degree === null then 0 else opts.Degree;
    p := period C;
    maps := hashTable for i from 1 to p list (
        if C_i == 0 or D_(i+deg) == 0 then continue;
        g := f(i);
        if g === null or g == 0 then continue;
        i => g
        );
    map(D,C,maps,Degree=>deg)
    )


map(ZZdFactorization, ZZdFactorization, ZZ) := ZZdFactorizationMap => opts -> (D, C, j) -> (
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

map(ZZdFactorization, ZZdFactorization, ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (tar, src, f) -> (
    p := period f;
    deg := if opts.Degree === null then degree f else opts.Degree;
    H := hashTable for k in keys f.map list k => map(tar_(deg+k), src_k, f_k);
    map(tar,src,H, Degree=>deg)
    )


ZZdFactorizationMap _ ZZ := Matrix => (f,i) -> (
    if f==0 then return map((target f)_(i + degree f), (source f)_i , 0);
    if f.map#?((source f).period) and i%((source f).period)==0 then return f.map#((source f).period);
    if f.map#?(i%(source f).period) then f.map#(i%(source f).period) else map((target f)_(i + degree f), (source f)_i, 0))
ZZdFactorizationMap ^ ZZ := ZZdFactorizationMap => (f,n) -> (
    (lo,hi) := (0,(source f).period);
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
      if source f != target f then error "expected source and target to be the same";
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


expression ZZdFactorizationMap := Expression => f -> (
    p := (source f).period;
    d := degree f;
    s := (sort keys (f.map))_{0..p-1};
    if #s === 0 then 
        new ZeroExpression from {0}
    else new VerticalList from for i from 0 to p-1 list
        RowExpression {(i+d)%p, ":", MapExpression { target f_i, source f_i, f_i }, ":", i%p}
    )

net ZZdFactorizationMap := Net => f -> (p:=(source f).period;
    if (# keys (f.map))===0 then return net "0";
     v := between("",
            for i from 0 to p-1 list (
                horizontalJoin(
		            net ((i+f.degree)%p), " : ", net target f_i, " <--",
		            lineOnTop net f_i,
		            "-- ", net source f_i, " : ", net (i%p)
                    )
                ));
     if # v === 0 then net "0"
     else stack v
     )
 
 

Fold = method();
Fold(Complex,ZZ) := (C,d) -> (
    (lo,hi) := concentration C;
    L := for i to d-1 list select(lo..hi+1,j->j%d==i);
    ZZdfactorization (apply(L_{1..d-1,0},i->if #i > 0 then directSum apply(i,j->C.dd_j) else id_((ring C)^0)))
    )

Fold(ComplexMap,ZZ) := (phi,d) -> (
    (lo,hi) := concentration phi;
    L := (for i to d-1 list select(lo-1..hi+1,j->j%d==i));
    Lk := for i from 1 to d list i=> if #(L_(i%d))>0 then directSum apply(L_(i%d),j->phi_j) else id_((ring phi)^0);
    map(Fold(target phi,d),Fold(source phi,d),P:=new HashTable from Lk,Degree => degree phi)
    )


 
--component = method()
component(Module,Thing) := (M,k) -> (
    if not M.cache.?indexComponents then error "expected Module to be a direct sum with indexed components";
    if not M.cache.indexComponents#?k then error("expected "|toString k|" to be the index of a component");
    (components M)#(M.cache.indexComponents#k)
    )


 
 -*tensor(ZZdFactorization, ZZdFactorization) := ZZdFactorization => {} >> opts -> (C, D) -> (
    Y := youngest(C,D);
    if Y.cache#?(tensor,C,D) then return Y.cache#(tensor,C,D);
    R := ring C;
    if ring D =!= R then error "expected factorizations over the same ring";
    (loC,hiC) := (0,C.period);
    (loD,hiD) := (0,D.period);
    modules := hashTable for i from 0 to C.period list i => (
        directSum for j from loC to 2*hiC-1 list (
            if (i-j) >= loD and (i-j)%d <= hiD then
                {j,i-j} => C_(j%d) ** D_((i-j)%d)
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
    maps := hashTable for i from 1 to C.period list i => (
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
    result = ZZdfactorization maps;
    result.cache.tensor = (C,D);
    Y.cache#(tensor,C,D) = result;
    result
    )*-
--ZZdFactorization ** ZZdFactorization := ZZdFactorization => (C,D) -> tensor(C,D)
Module ** ZZdFactorization := ZZdFactorization => (M,D) -> (maps := (dd^D).map;
    ZZdfactorization for i in keys maps list M**(maps#i)
    )
ZZdFactorization ** Module := ZZdFactorization => (C,N) -> (maps := (dd^C).map;
    ZZdfactorization for i in keys maps list (maps#i)**N
    )

ZZdFactorization ** Matrix := ZZdFactorizationMap => (C, f) -> (
    if ring C =!= ring f then error "expected Factorization and Matrix over the same ring";
    src := C ** source f;
    tar := C ** target f;
    map(tar, src, hashTable for i from 1 to period C list i => map(tar_i, src_i, C_i ** f))
    )
Matrix ** ZZdFactorization := ZZdFactorizationMap => (f, C) -> (
    if ring C =!= ring f then error "expected Factorization and Matrix over the same ring";
    src := source f ** C;
    tar := target f ** C;
    map(tar, src, hashTable for i from 1 to period C list i => map(tar_i, src_i, f ** C_i))
    )

ZZdFactorization ** Ring := ZZdFactorization => (C,R) -> (
    (lo,hi) := (0,C.period);
    moduleHash := hashTable for i from lo to hi list i => C_i ** R;
    if lo === hi then 
        return ZZdfactorization(moduleHash#lo, Base=>lo);
    mapHash := hashTable for i from lo+1 to hi list i => 
        map(moduleHash#(i-1), moduleHash#i, (cover dd^C_i) ** R);
    ZZdfactorization mapHash
    )
Ring ** ZZdFactorization := ZZdFactorization => (R,C) -> C ** R

RingMap ZZdFactorization := ZZdFactorization => (phi,C) -> (
    (lo,hi) := (0,C.period);
    moduleHash := hashTable for i from lo to hi list i => phi C_i;
    if lo === hi then 
        return ZZdfactorization(moduleHash#lo, Base=>lo);
    mapHash := hashTable for i from lo+1 to hi list i => 
        map(moduleHash#(i-1), moduleHash#i, phi dd^C_i);
    ZZdfactorization mapHash
    )

tensor(RingMap, ZZdFactorization) := ZZdFactorization => {} >> opts -> (phi, C) -> (
    if source phi =!= ring C then error "expected the source of the ring map to be the ring of the factorization";
    (lo,hi) := (0,C.period);
    modules := hashTable for i from lo to hi list i => tensor(phi, C_i);
    if lo === hi then 
        return ZZdfactorization(modules#lo, Base=>lo);
    maps := hashTable for i from lo+1 to hi list i => 
        map(modules#(i-1), modules#i, tensor(phi, matrix dd^C_i));
    ZZdfactorization maps
    )
tensor(ZZdFactorization, RingMap) := ZZdFactorization => {} >> opts -> (C, phi) -> tensor(phi, C)

RingMap ** ZZdFactorization := ZZdFactorization => (phi, C) -> tensor(phi, C)
ZZdFactorization ** RingMap := ZZdFactorization => (C, phi) -> tensor(phi, C)



isWellDefined ZZdFactorization := Boolean => C -> (
    k := keys C;
    expectedKeys := set {
        symbol ring, 
        symbol period, 
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
    (lo,hi) := (0,C.period);
    if not instance(lo,ZZ) or not instance(hi,ZZ) or lo > hi then (
        if debugLevel > 0 then (
            << "-- expected period to be nonnegative integer" << endl;
            );
        return false;
        );
    if not instance(C.module, HashTable) then (
        if debugLevel > 0 then (
            << "-- expected C.module to be a HashTable" << endl;
            );
        return false;
        );    
    if not instance(C.dd, ZZdFactorizationMap) then (
        if debugLevel > 0 then (
            << "-- expected dd^C to be a ZZdFactorizationMap" << endl;
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
            << "-- expected ring of the differential to be the ring of the factorization" << endl;
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
                << "-- expected source and target of maps in differential to be modules in the factorization " << endl;
                << "--   differential at index " << i << " fails this condition" << endl;                
            );    
            return false;
            );
        );
    -*for i from lo+2 to hi do (
        f := dd^C_i;
        g := dd^C_(i-1);
        if g*f != 0 then (
            if debugLevel > 0 then (
                << "-- expected maps in the differential to compose to zero " << endl;
                << "--   differentials at indices " << (i,i-1) << " fail this condition" << endl;                
            );    
            return false;
            );
        );*-
    true
    )

isWellDefined ZZdFactorizationMap := f -> (
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
            << "-- expected source and target to be well-defined ZZdFactorizations" << endl;
            );
        return false;
        );
    if not instance(f.degree, ZZ) then (
        if debugLevel > 0 then (
            << "-- expected degree of factorization homomorphism to be an integer" << endl;
            );
        return false;
        );
    (lo,hi) := (0,f.source.period);
    if not all(keys f.map, i -> instance(i,ZZ) and i >= lo and i <= hi) then (
        if debugLevel > 0 then (
            << "-- expected all maps to be indexed by integers in the period of the source" << endl;
            );
        return false;
        );
    for i from lo to hi do (
        g := f_i;
        if source g =!= f.source_i or target g =!= f.target_(i+f.degree)
        then (
            if debugLevel > 0 then (
                << "-- expected source and target of maps in differential to be modules in the factorization " << endl;
                << "--   differential at index " << i << " fails this condition" << endl;                
            );    
            return false;
            );
        );
    if f.cache.?isCommutative then (
        deg := degree f;
        C := f.source;
        D := f.target;
        (loC,hiC) := (0,C.period);
        (loD,hiD) := (0,D.period);
	if not(hiC==hiD) then (
	    << "--expected source and target to have the same period" << endl;
	    return false;
	    );
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

ZZdFactorization == ZZdFactorization := (C,D) -> (
    if C === D then return true;
    (loC,hiC) := (0,C.period);
    (loD,hiD) := (0,D.period);
    if hiD=!=hiC then return false;
    if ring C =!= ring D then return false;
    for i from 0 to hiC-1 do (
        if C_i != D_i then return false;
        if dd^C_i != dd^D_i then return false;
        );
    true    
    )

ZZdFactorization == ZZ := (C,n) -> (
    if n =!= 0 then error "cannot compare ZZdFactorization to non-zero integer";
    (lo,hi) := (0,C.period);
    for i from lo to hi do if C_i != 0 then return false;
    true
    )
ZZ == ZZdFactorization := (n,C) -> C == n

ZZdFactorization#id = (C) -> (
    (lo,hi) := (0,C.period);
    maps := hashTable for i from lo to hi list i => id_(C_i);
    result := map(C,C,maps);
    result.cache.isCommutative = true;
    result
    )

-*heftfun0 = wt -> d -> sum( min(#wt, #d), i -> wt#i * d#i )
heftfun = (wt1,wt2) -> (
     if wt1 =!= null then heftfun0 wt1
     else if wt2 =!= null then heftfun0 wt2
     else d -> 0
     )

betti ZZdFactorization := opts -> C -> (
    heftfn := heftfun(opts.Weights, heft ring C);
    (lo,hi) := (0,C.period);
    new BettiTally from flatten for i from lo to hi list (
        apply(pairs tally degrees C_i, (d,n) -> (i,d,heftfn d) => n)
        )
    )*-

ZZdFactorization.directSum = args -> (
    assert(#args > 0);
    R := ring args#0;
    if not all(args, C -> ring C === R) then error "expected all factorizations to be over the same ring";
    periods := for C in args list C.period;
    if #(unique periods)=!=1 then error "expected all factorizations to have same period";
    lo := 0;
    hi := periods_0;
    D := if lo === hi then (
        ZZdfactorization(directSum for C in args list C_lo, Base=>lo)
        )
    else (
        maps := hashTable for i from lo+1 to hi list i => (
            directSum for C in args list dd^C_i
            );
        ZZdfactorization maps
        );
    D.cache.components = toList args;
    D    
    )
ZZdFactorization ++ ZZdFactorization := ZZdFactorization => (C,D) -> directSum(C,D)
directSum ZZdFactorization := C -> directSum(1 : C)




ZZdFactorizationMap == ZZdFactorizationMap := (f,g) -> (
    if f === g then return true;    
    if source f != source g or target f != target g 
      then return false;
    if f==0 and g==0 then return true;
    if f!=0 and g==0 then return false;
    if f==0 and g!=0 then return false;
    (lo1,hi1) := (0,(source f).period);
    (lo2,hi2) := (0,(source g).period);
    for i from 0 to max(hi1,hi2) do (
        if f_i != g_i then return false;
        );
    true    
    )

ZZdFactorizationMap == RingElement := Boolean => (phi,r) -> (phi == r*id_(source phi))

RingElement == ZZdFactorizationMap := Boolean => (r,phi) -> (phi == r)

ZZdFactorizationMap == ZZ := Boolean => (f,n) -> (
    if n === 0 then 
        all(keys f.map, k -> f.map#k == 0)
    else if n === 1 then (
        if source f != target f then return false;
        if degree f =!= 0 then return false;
        (lo,hi) := (0,(source f).period);
        for i from lo to hi do
            if f_i != 1 then return false;
        f.cache.isCommutative = true;  -- this is the identity, after all!        
        true
        )
    else 
        error "cannot compare ZZdFactorizationMap to integer other than 0 or 1"
    )
ZZ == ZZdFactorizationMap := Boolean => (n,f) -> f == n

RingElement * ZZdFactorizationMap := (r,f) -> (
    df := degree f;
    (lo,hi) := (0,(source f).period);
    maps := hashTable for i from lo to hi list i => (
        h := r * f_i;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    if isCommutativeCached f and isCommutative ring f then
        result.cache.isCommutative = true;
    result
    )

ZZdFactorizationMap * RingElement := (f,r) -> (
    df := degree f;
    (lo,hi) := (0,(source f).period);
    maps := hashTable for i from lo to hi list i => (
        h := f_i * r;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    if isCommutativeCached f and isCommutative ring f then
        result.cache.isCommutative = true;
    result
    )

Number * ZZdFactorizationMap := (r,f) -> (
    try r = promote(r,ring f) else error "can't promote scalar to ring of complex homomorphism";
    r * f
    )

ZZdFactorizationMap * Number := (f,r) -> (
    try r = promote(r,ring f) else error "can't promote scalar to ring of complex homomorphism";
    f * r
    )

- ZZdFactorizationMap := (f) -> (
    result := (-1)*f;
    if isCommutativeCached f then
        result.cache.isCommutative = true;
    result
    )

ZZdFactorizationMap + ZZdFactorizationMap := (f,g) -> (
    df := degree f;
    dg := degree g;
    if source f != source g then error "expected ZZdFactorization homomorphisms with the same source";
    if target f != target g then error "expected ZZdFactorization homomorphisms with the same target";
    if df =!= dg then error "expected ZZdFactorization homomorphisms with the same degree";
    (lo,hi) := (0,(source g).period);
    maps := hashTable for i from lo to hi list i => (
        h := f_i + g_i;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    if isCommutativeCached f and isCommutativeCached g then 
        result.cache.isCommutative = true;
    result
    )
ZZdFactorizationMap + Number :=
ZZdFactorizationMap + RingElement := ZZdFactorizationMap => (f,r) -> (
    if r == 0 then f
    else (
        if source f != target f
        then error "expected same source and target"
        else f + r*id_(target f))
    )
Number + ZZdFactorizationMap :=
RingElement + ZZdFactorizationMap := ZZdFactorizationMap => (r,f) -> f + r

ZZdFactorizationMap - Number :=
ZZdFactorizationMap - RingElement :=
ZZdFactorizationMap - ZZdFactorizationMap := ZZdFactorizationMap => (f,g) -> f + (-1)*g

Number - ZZdFactorizationMap :=
RingElement - ZZdFactorizationMap := ZZdFactorizationMap => (r,f) -> -f + r

ZZdFactorizationMap * ZZdFactorizationMap := (f,g) -> (
    df := degree f;
    dg := degree g;
    (lo,hi) := (0,(source g).period);
    maps := hashTable for i from lo to hi list i => (
        h := f_(dg + i) * g_i;
        if h == 0 then continue else h
        );
    result := map(target f, source g, maps, Degree=>df+dg);
    if isCommutativeCached f and isCommutativeCached g then 
        result.cache.isCommutative = true;
    result
    )

ZZdFactorizationMap.directSum = args -> (
    -- args: sequence of ZZdFactorizationMap's
    -- args: f_i : C_i --> D_i, having same degree deg
    -- result : sum(C_i) --> sum(D_i)
    R := ring args#0;
    deg := degree args#0;
    if not all(args, f -> ring f === R) then 
        error "expected maps all over the same ring";
    if not all(args, f -> degree f === deg) then
        error "expected maps to all have the same degree";
    -- WARNING: we call ZZdFactorization.directSum directly rather than using
    -- just directSum to avoid getting a cached copy of the direct
    -- sum.  Otherwise the labels of the cached copies might get
    -- changed (in Options.directSum).
    src := ZZdFactorization.directSum (args/source);
    tar := ZZdFactorization.directSum (args/target);
    -- only keep matrices in the homomorphism that are non-zero
    spots := unique flatten(args/(f -> keys f.map));
    maps := hashTable for i in spots list i => directSum(args/(f -> f_i));
    result := map(tar,src,maps,Degree=>deg);
    result.cache.components = toList args;
    if all(args, isCommutativeCached) then 
        result.cache.isCommutative = true;
    result
    )

ZZdFactorizationMap ++ ZZdFactorizationMap := ZZdFactorizationMap => (f,g) -> directSum(f,g)
directSum ZZdFactorizationMap := f -> directSum(1 : f)


isCommutative ZZdFactorizationMap := Boolean => f -> (
    if debugLevel == 0 and f.cache.?isCommutative then 
       return f.cache.isCommutative;
    C := source f;
    D := target f;
    deg := degree f;
    (loC,hiC) := (0,C.period);
    (loD,hiD) := (0,D.period);
    t := getSymbol "t";
    if period C == 2 then t = -1;
    if deg == 0 and not(period C==2) then t = 1;
    if not(deg==0) and not(period C==2) and (ring C).?rootOfUnity then t = (ring C).rootOfUnity;
    for i from loC to hiC-1 do (
        if i+deg-1 >= loD and i+deg-1 <= hiD then (
            if not (dd^D_(i+deg) * f_i == t^deg * (f_(i-1) * dd^C_i))
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
isCommutativeCached ZZdFactorizationMap := Boolean => f -> f.cache.?isCommutative and f.cache.isCommutative

isFactorizationMorphism = method(TypicalValue => Boolean)
isFactorizationMorphism ZZdFactorizationMap := (f) -> (
    if debugLevel > 0 and degree f =!= 0 then (
        << "-- the factorization map has non-zero degree" << endl;
        return false;
        );
    degree f === 0 and isCommutative f
    )



dHom = method(Options => {RootOfUnity => true})
dHom(ZZdFactorization,ZZdFactorization,RingElement) := ZZdFactorization => opts -> (F,G,t) -> (
     if not(opts.RootOfUnity) then (
	 S := adjoinRoot(period F,ring F,t);
	 return Hom(F**S, G**S);
	 );
     Hom(F,G)
     )

 
 
 dHom(ZZdFactorization,ZZdFactorization,Symbol) := ZZdFactorization => opts -> (F,G,t) -> (
      S := adjoinRoot(period F,ring F,t);
     dHom(F**S, G**S)
     )
 
 dHom(ZZdFactorizationMap,ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => opts -> (f,g,t) -> (
     if not(opts.RootOfUnity) then (
	 S := adjoinRoot(period f,ring f,t);
	  Hom(f**S, g**S);
	 );
     Hom(f,g)
     )
 

gradedModule(ZZdFactorization) := ZZdFactorization => F -> (
    ZZdfactorization for i from 0 to F.period-1 list F_i
    )

Hom(ZZdFactorization,ZZdFactorization) := ZZdFactorization => opts -> (C,D) -> (
    if not(C.period == D.period) then error "Expected factorizations with the same period";
    t := if (ring C).?rootOfUnity then (ring C).rootOfUnity else -1;
    if not((ring C).?rootOfUnity) and not(C.period==2) then error "Must adjoin dth root of unity when input has period d > 2";
    -- signs here are based from Christensen and Foxby
    -- which agrees with Conrad (Grothendieck duality book)
    Y := youngest(C,D);
    if Y.cache#?(Hom,C,D) then return Y.cache#(Hom,C,D);
    R := ring C;
    p := C.period;
    if ring D =!= R then error "expected factorizations over the same ring";
    modules := hashTable for i from 0 to p-1 list i => (
        directSum for j from 0 to p-1 list {j,(j+i)%p} => Hom(C_j, D_(j+i), opts)
        );
    maps := hashTable for i from 1 to p list i => (
        map(modules#((i-1)%p),
            modules#(i%p),
            matrix table(
                indices modules#((i-1)%p),
                indices modules#(i%p),
                (j,k) -> (
                    tar := component(modules#((i-1)%p), j);
                    src := component(modules#(i%p), k);
                    map(tar, src, 
                        if ({(k#0-j#0)%p,(k#1-j#1)%p} == {0,1}) then -(t)^(k#1-k#0) * Hom(C_(k#0), dd^D_(k#1), opts)
                        else if ({(k#0-j#0)%p,(k#1-j#1)%p} == { p-1,0 }) then Hom(dd^C_(j#0), D_(k#1), opts)
                        else 0)
                    ))));
    result := ZZdfactorization maps;
    result.cache.homomorphism = (C,D); -- source first, then target
    Y.cache#(Hom,C,D) = result;
    result
    )
    

Hom(ZZdFactorization,ZZdFactorization,RingElement) := ZZdFactorization => opts -> (F,G,t) -> (
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(F,G,t,RootOfUnity=>false)
    )

Hom(ZZdFactorization,ZZdFactorization,Symbol) := ZZdFactorization => opts -> (F,G,t) -> (
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(F,G,t)
    )

Hom(ZZdFactorizationMap,ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (f,g) -> (
    if not(period f == period g) then error "Expected factorizations with the same period";
    t := if (ring f).?rootOfUnity then (ring f).rootOfUnity else -1;
    if not((ring f).?rootOfUnity) and not(period f==2) then error "Must adjoin dth root of unity when input has period d > 2";
    df := degree f;
    dg := degree g;
    src := Hom(target f, source g, opts);
    tar := Hom(source f, target g, opts);
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    per := period f;
    maps := hashTable for i from 1 to per list (
      i => (
        m := for q in indices tar_(i+df+dg) list (
               -- so q == {k,k+i+df+dg}
               for p in indices src_i list (
                   -- so p == {j,j+i}, for various j
                   if (p#0 - df)%per == q#0 
                   then (
                       sgn := 1; -- function of df, dg, i
                       sgn = -(t)^(df * (i + dg));
                       sgn * Hom(f_(q#0), g_(p#1), opts)
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

Hom(ZZdFactorizationMap,ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => opts -> (f,g,t) -> (
    F := source f;
    G := source g;
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(f,g,t,RootOfUnity=>false)
    )

Hom(ZZdFactorizationMap,ZZdFactorizationMap,Symbol) := ZZdFactorizationMap => opts -> (f,g,t) -> (
    F := source f;
    G := source g;
    if not(F.period==G.period) then error "Expected factorizations with the same period";
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(f,g,t)
    )
    





Hom(ZZdFactorization, ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (C,g) -> Hom(id_C, g)
Hom(ZZdFactorization, ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => opts -> (C,g,t) -> Hom(id_C, g,t)
Hom(ZZdFactorization, ZZdFactorizationMap,Symbol) := ZZdFactorizationMap => opts -> (C,g,t) -> Hom(id_C, g,t)
Hom(ZZdFactorizationMap, ZZdFactorization) := ZZdFactorizationMap => opts -> (f,D) -> Hom(f, id_D)
Hom(ZZdFactorizationMap, ZZdFactorization,RingElement) := ZZdFactorizationMap => opts -> (f,D,t) -> Hom(f, id_D,t)
Hom(ZZdFactorizationMap, ZZdFactorization,Symbol) := ZZdFactorizationMap => opts -> (f,D,t) -> Hom(f, id_D,t)
Hom(Module,ZZdFactorization) := ZZdFactorization => opts -> (M,F) -> (
    ZZdfactorization for i in keys (F.dd.map) list Hom(M,(F.dd.map)#i)
    )
Hom(ZZdFactorization,Module) := ZZdFactorization => opts -> (F,M) -> (
    Hom(F, ZZdfactorization(M, period F))
    )
Hom(ZZdFactorization, Ring) := ZZdFactorization => opts -> (F,R) -> Hom(F, R^1)
Hom(ZZdFactorization,Module,RingElement) := ZZdFactorization => opts -> (F,M,t) -> (
    Hom(F, ZZdfactorization(M, period F), t)
    )
Hom(ZZdFactorization,Module,Symbol) := ZZdFactorization => opts -> (F,M,t) -> (
    Hom(F, ZZdfactorization(M, period F), t)
    )
Hom(Module, ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (M,g) -> (
    trg := target g;
    src := source g;
    map(Hom(M, trg), Hom(M,src), hashTable for i in keys g.map list i => Hom(M, g.map#i) , Degree => g.degree)
    )
Hom(ZZdFactorizationMap, Module) := ZZdFactorizationMap => opts -> (f,N) -> Hom(f, id_(ZZdfactorization(N,period f)))
Hom(ZZdFactorizationMap, Module,RingElement) := ZZdFactorizationMap => opts -> (f,N,t) -> (
    Hom(f, id_(ZZdfactorization(N,period f)), t)
    )
Hom(ZZdFactorizationMap, Module,Symbol) := ZZdFactorizationMap => opts -> (f,N,t) -> (
    Hom(f, id_(ZZdfactorization(N,period f)), t)
    )
Hom(Ring, ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (R,f) -> Hom(R^1, f)
Hom(Ring, ZZdFactorization) := ZZdFactorizationMap => opts -> (R,D) -> Hom(R^1, id_D)
Hom(ZZdFactorizationMap, Ring) := ZZdFactorizationMap => opts -> (f,R) -> Hom(f, module R)
Hom(ZZdFactorizationMap, Ring,RingElement) := ZZdFactorizationMap => opts -> (f,R,t) -> (
    Hom(f, module R, t)
    )
Hom(ZZdFactorizationMap, Ring,Symbol) := ZZdFactorizationMap => opts -> (f,R,t) -> (
    Hom(f, module R, t)
    )
Hom(ZZdFactorization, Matrix) := ZZdFactorizationMap => opts -> (C,g) -> (dual C)**g
Hom(ZZdFactorization, Matrix,RingElement) := ZZdFactorizationMap => opts -> (C,g,t) -> (
    dc := dual(C,t);
    dc**(sub(g,ring dc))
    )
Hom(ZZdFactorization, Matrix,Symbol) := ZZdFactorizationMap => opts -> (C,g,t) -> (
    dc := dual(C,t);
    dc**(sub(g,ring dc))
    )

Hom(ZZdFactorizationMap, Matrix) := ZZdFactorizationMap => opts -> (f,g) -> (dual f)**g
Hom(ZZdFactorizationMap, Matrix,RingElement) := ZZdFactorizationMap => opts -> (f,g,t) -> (
    df := dual(f,t);
    df**(sub(g,ring df))
    )
Hom(ZZdFactorizationMap, Matrix,Symbol) := ZZdFactorizationMap => opts -> (f,g,t) -> (
    df := dual(f,t);
    df**(sub(g,ring df))
    )
Hom(Matrix, ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (f,g) -> (
    trg := target g;
    src := source g;
    map(target f ** trg, source g ** src, hashTable for i in keys g.map list i=>Hom(f,g#i), Degree => g.degree)
    )
Hom(Matrix, ZZdFactorization) := ZZdFactorizationMap => opts -> (f,D) -> Hom(f, id_D)

Hom(Complex, ZZdFactorization) := ZZdFactorization => opts -> (C,D) -> Hom(Fold(C, period D), D)
Hom(Complex, ZZdFactorization, Symbol) := ZZdFactorization => opts -> (C,D,t) -> Hom(Fold(C, period D), D,t)
Hom(Complex, ZZdFactorization, RingElement) := ZZdFactorization => opts -> (C,D,t) -> Hom(Fold(C, period D), D, t)
Hom(ZZdFactorization, Complex) := ZZdFactorization => opts -> (D,C) -> Hom(D, Fold(C, period D))
Hom(ZZdFactorization, Complex, Symbol) := ZZdFactorization => opts -> (D,C,t) -> Hom(D, Fold(C, period D), t)
Hom(ZZdFactorization, Complex, RingElement) := ZZdFactorization => opts -> (D,C,t) -> Hom(D, Fold(C, period D),t)

Hom(ComplexMap, ZZdFactorizationMap) := ZZdFactorizationMap => (f, g) -> Hom(Fold(f, period source g), g)
Hom(ComplexMap, ZZdFactorizationMap, Symbol) := ZZdFactorizationMap => (f, g, t) -> Hom(Fold(f, period source g), g, t)
Hom(ComplexMap, ZZdFactorizationMap, RingElement) := ZZdFactorizationMap => (f, g, t) -> Hom(Fold(f, period source g), g, t)
Hom(ZZdFactorizationMap, ComplexMap) := ZZdFactorizationMap => (g, f) -> Hom(g, Fold(f, period source g))
Hom(ZZdFactorizationMap, ComplexMap, Symbol) := ZZdFactorizationMap => (g, f, t) -> Hom(g, Fold(f, period source g), t)
Hom(ZZdFactorizationMap, ComplexMap, RingElement) := ZZdFactorizationMap => (g, f, t) -> Hom(g, Fold(f, period source g), t)




homomorphism(ZZ, Matrix, ZZdFactorization) := ZZdFactorizationMap => (i, f, E) -> (
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
    p := period C;
    H := hashTable for j from 1 to p list j => 
      homomorphism fixme f^[{j%p,(j+i)%p}];
    map(D,C,H, Degree=>i)
    )


-*
End(ZZdFactorization) := ZZdFactorization => opts -> F -> (
    if F.period==2 then return (dual F)**F;
    if (ring F).?rootOfUnity then return dHom(F,F,(ring F).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

End(ZZdFactorization,RingElement) := ZZdFactorization => opts -> (F,t) -> (
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(F,F,t,RootOfUnity=>false)
    )

End(ZZdFactorization,Symbol) := ZZdFactorization => opts -> (F,t) -> (
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(F,F,t)
    )

End(ZZdFactorizationMap) := ZZdFactorizationMap => (f) -> (
    F := source f;
    if F.period==2 then return (dual f)**f;
    if (ring F).?rootOfUnity then return dHom(f,f,(ring F).rootOfUnity)
    else error "Must adjoin dth root of unity when input has period d > 2";
    )

End(ZZdFactorizationMap,RingElement) := ZZdFactorizationMap => (f,t) -> (
    F := source f;
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(f,f,t,RootOfUnity=>false)
    )

End(ZZdFactorizationMap,Symbol) := ZZdFactorizationMap => (f,t) -> (
    F := source f;
    if F.period==2 then error "No need to specify root of unity for ZZ/2-graded factorization";
    dHom(f,f,t)
    )
*-

components ZZdFactorization := C -> if C.cache.?components then C.cache.components else {C}
components ZZdFactorizationMap := f -> if f.cache.?components then f.cache.components else {f}


trans := (C,v) -> (
    if C.cache.?indexComponents then (
	    Ci := C.cache.indexComponents;
	    apply(v, i -> if Ci#?i then Ci#i else error "expected an index of a component of the direct sum"))
    else (
        if not C.cache.?components then error "expected a direct sum of factorizations";
	    Cc := C.cache.components;
	    apply(v, i -> if not Cc#?i then error "expected an index of a component of the direct sum");
	    v)
    )


component(Module,Thing) := (M,k) -> (
    if not M.cache.?indexComponents then error "expected Module to be a direct sum with indexed components";
    if not M.cache.indexComponents#?k then error("expected "|toString k|" to be the index of a component");
    (components M)#(M.cache.indexComponents#k)
    )

ZZdFactorization _ Array := ZZdFactorizationMap => (C,v) -> (
    v = trans(C,v);
    D := directSum apply(toList v, j -> C.cache.components#j);
    (lo,hi) := (0,period D);
    maps := hashTable for i from lo to hi list i => C_i_v;
    result := map(C,D,maps);
    result.cache.isCommutative = true;
    result
    )

ZZdFactorization ^ Array := ZZdFactorizationMap => (C,v) -> (
    v = trans(C,v);
    D := directSum apply(toList v, j -> C.cache.components#j);
    (lo,hi) := (0,period C);
    maps := hashTable for i from lo to hi list i => C_i^v;
    result := map(D,C,maps);
    result.cache.isCommutative = true;
    result
    )

ZZdFactorizationMap ^ Array := ZZdFactorizationMap => (f,v) -> (target f)^v * f
ZZdFactorizationMap _ Array := ZZdFactorizationMap => (f,v) -> f * (source f)_v




homomorphism ZZdFactorizationMap := ZZdFactorizationMap => (h) -> (
    -- h should be a homomorphism of factorizations from R^1[-i] --> E = Hom(C,D)
    -- returns the corresponding f : C --> D.
    R1 := source h;
    p := period R1;
    i := if p == 0 then error "period 0" else (
        which := null;
        for j from 0 to p-1 do if R1_j != 0 then (
            if which =!= null then error "expected source of map to be supported in one homological degree";            
            which = j;
            );
        if which === null then error "expected source of map to be supported in one homological degree";            
        which
        );
    homomorphism(i + degree h, h_i, target h)
    )

homomorphism' ZZdFactorizationMap := ZZdFactorizationMap => opts -> (f) -> (
    R := ring f;
    C := source f;
    D := target f;
    d := degree f;
    H := Hom(C,D);
    p := period f;
    -- want R^1[0] --> H
    -- TODO: remove this line if the next actually works...: g := map(H_d, R^1, matrix(for i from lo to hi list {matrix homomorphism' f_i}));
    g := map(H_d,, matrix(for i from 0 to p-1 list {matrix homomorphism'(f_i, opts)}));
    map(H, ZZdfactorization(source g, p), hashTable {p => g}, Degree=>d)
    )


-- possible todo: allow to choose the homological degree of the map, and the internal degree of the map?
randomFactorizationMap = method(Options=>{
        Degree => 0,
        InternalDegree => null,
        Cycle => false,
        Boundary => false
        }) -- should this overload 'random'?  Probably.
randomFactorizationMap(ZZdFactorization,ZZdFactorization) := ZZdFactorizationMap => o -> (D,C) -> (
    if not(potential C == potential D) and o.Boundary then error "factorizations should have the same potential";
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




tensorCommutativity(ZZdFactorization, ZZdFactorization) := ZZdFactorizationMap => (C,D) -> (
    -- implement the isomorphism C ** D --> D ** C
    t := if C.period==2 then -1 else (ring C).rootOfUnity;
    CD := C ** D; 
    DC := D ** C;
    p := period C;
    (lo,hi) := (0,period C);
    maps := new HashTable from for i from 1 to p list i => (
       mats := for ba in indices DC_i list
           for ab in indices CD_i list (
              a := ab#0; -- summand C_a ** D_(i-a)
              b := ba#0; -- summand D_b ** C_(i-b)
              -- should be the zero map, unless a+b == i
              if (a+b)%p === i%p then 
                 (t)^(a*b) * tensorCommutativity(C_a, D_b)
              else map(
                  DC_i.cache.components#(DC_i.cache.indexComponents#ba),
                  CD_i.cache.components#(CD_i.cache.indexComponents#ab),
                  0)
        );
        matrix mats
        );
    map(DC,CD,maps)
    )

tensorAssociativity(ZZdFactorization, ZZdFactorization, ZZdFactorization) := ZZdFactorizationMap => (A,B,C) -> (
    -- implements the isomorphism A ** (B ** C) --> (A ** B) ** C
    AB := A ** B;
    BC := B ** C;
    E := A ** BC; -- source
    F := AB ** C; -- target
    p := period A;
    maps := new HashTable from for i from 1 to p list i => (
        map(F_i, E_i, 
        -- want the map E_i --> F_i
        matrix for ab'c in indices F_i list
                for a'bc in indices E_i list (
                    a := a'bc#0;
                    b := (a'bc#1 - ab'c#1);
                    c := ab'c#1;
                    bc := a'bc#1;
                    ab := ab'c#0;
                    if A_a != 0 and B_b != 0 and C_c != 0 then (
                        ((AB_ab)_[{a,b%p}] ** C_c)
                        * tensorAssociativity(A_a, B_b, C_c)
                        * (A_a ** (BC_bc)^[{b%p,c}])
                        )
                    else (
                         map(F_i.cache.components#(F_i.cache.indexComponents#{ab,c}),
                            E_i.cache.components#(E_i.cache.indexComponents#{a,bc}),
                            0)
                        )
                    )
		)
        );
    map(F,E,maps)
    )





ZZdFactorizationMap ** Ring := ZZdFactorizationMap => (f,R) -> (
    C := (source f) ** R;
    D := (target f) ** R;
    deg := degree f;
    (lo,hi) := (0,period source f);
    maps := hashTable for i from lo to hi list i => map(D_(i+deg), C_i, (cover f_i) ** R);
    result := map(D, C, maps, Degree => deg);
    if isCommutativeCached f then
        result.cache.isCommutative = true;
    result
    )
Ring ** ZZdFactorizationMap := ZZdFactorizationMap => (R,f) -> f ** R

ZZdFactorizationMap ** Module := ZZdFactorizationMap => (f,R) -> (
    C := (source f) ** R;
    D := (target f) ** R;
    deg := degree f;
    (lo,hi) := (0,period source f);
    maps := hashTable for i from lo to hi list i => map(D_(i+deg), C_i, (cover f_i) ** R);
    result := map(D, C, maps, Degree => deg);
    if isCommutativeCached f then
        result.cache.isCommutative = true;
    result
    )
Module ** ZZdFactorizationMap := ZZdFactorizationMap => (R,f) -> f ** R



RingMap ZZdFactorizationMap := ZZdFactorizationMap => (phi,f) ->
    map(phi target f, phi source f, hashTable for i from 1 to period f list i => phi f_i)

tensor(RingMap, ZZdFactorizationMap) := ZZdFactorizationMap => {} >> opts -> (phi, f) -> (
    if source phi =!= ring f then error "expected the source of the ring map to be the ring of the factorization map";
    map(tensor(phi, target f), tensor(phi, source f), hashTable for i from 1 to period f list i => tensor(phi, matrix f_i))
    )
tensor(ZZdFactorizationMap, RingMap) := ZZdFactorizationMap => {} >> opts -> (f, phi) -> tensor(phi, f)

RingMap ** ZZdFactorizationMap := ZZdFactorizationMap => (phi, f) -> tensor(phi, f)
ZZdFactorizationMap ** RingMap := ZZdFactorizationMap => (f, phi) -> tensor(phi, f)



--------------------------------------------------------------------
-- canonical maps --------------------------------------------------
--------------------------------------------------------------------
extend(ZZdFactorization,ZZdFactorization,Matrix,Sequence) := ZZdFactorizationMap => opts -> (D,C,f,p)-> (
    -- assumptions:
    -- let p == (j,i) 
    -- (a) f : C_i --> D_j
    -- (b) C should be a factorization of free modules
    -- (c) D should be exact at D_k, for all k > j, not checked explicitly
    -- (d) f * dd^C_(i+1) lies in the image of dd^D_(j+1), not checked explicitly
    -- output:
    --   a ZZdFactorizationMorphism, g : C --> D of degree j-i such that g_i = f.
    (j, i) := p;
    (loC, hiC) := (0,C.period);
    if target f =!= D_j then 
        error("expected the matrix to define a map to the "|j|"-th term of the target factorization");
    if source f =!= C_i then 
        error("expected the matrix to define a map from the "|i|"-th term of the source factorization");
    d := j-i;
    g := f; -- at each step, g : C_(i-1) -> D_(i-1+d)
    maps := hashTable for k from i to hiC list k => (
        if k === i then f
        else (
            if odd d then g = -g;
            g = (g * dd^C_k) // dd^D_(k+d);
            map(D_(k+d), C_k, g)
            )
        );
    result := map(D, C, maps, Degree => d);
    -- TODO: the following line: "false and" should be removed when we
    -- switch Verify to have default value false.
    if false and opts.Verify then (
        if not isCommutative result
        then error "map cannot be extended";
        if degree result != d then error "map has incorrect degree";
        );
    result
    )

extend(ZZdFactorization,ZZdFactorization,Matrix) := ZZdFactorizationMap => opts -> (D, C, f) -> extend(D, C, f, (0,0))


inducedMap(ZZdFactorization, ZZdFactorization) := ZZdFactorizationMap => opts -> (D,C) -> (
    -- compute f : C --> D the map induced by the identity matrix.
    deg := if opts.Degree === null then 0 else opts.Degree;
    (loC,hiC) := (0,period C);
    (loD,hiD) := (0,period D);
    maps := hashTable for i from max(loC,loD-deg) to min(hiC,hiD-deg) list i => inducedMap(D_(i+deg),C_i, Verify => opts.Verify);
    map(D,C,maps,Degree=>deg)
    )

inducedMap(ZZdFactorization, ZZdFactorization,ZZdFactorizationMap) := ZZdFactorizationMap => opts -> (D,C,phi) -> (
    -- compute f : C --> D the map induced by the identity matrix.
    deg := if opts.Degree === null then degree phi else opts.Degree;
    (loC,hiC) := (0,period C);
    (loD,hiD) := (0,period D);
    maps := hashTable for i from max(loC,loD-deg) to min(hiC,hiD-deg) list i => inducedMap(D_(i+deg),C_i, phi_i, Verify => opts.Verify);
    map(D,C,maps,Degree=>deg)
    )



kernel ZZdFactorizationMap := ZZdFactorization => opts -> f -> (
    -- f : B --> C
    B := source f;
    (lo,hi) := (0,B.period);
    modules := hashTable for i from lo to hi list i => kernel f_i;
    result := if lo === hi then ZZdfactorization(modules#lo, Base => lo)
        else (
            inducedMaps := hashTable for i from lo to hi list i => inducedMap(B_i, modules#i);
            maps := hashTable for i from lo+1 to hi list i => (
                (dd^B_i * inducedMaps#i) // inducedMaps#(i-1)
                );
            ZZdfactorization maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the source to induce a well-defined differential on the kernel";
    result.cache.kernel = f;
    result
    )
cokernel ZZdFactorizationMap := ZZdFactorization => f -> (
    -- f : B --> C
    C := target f;
    (lo,hi) := (0,C.period);
    deg := degree f;
    modules := hashTable for i from lo to hi list i => cokernel f_(i-deg);
    result := if lo === hi then ZZdfactorization(modules#lo, Base => lo)
        else (
            maps := hashTable for i from lo+1 to hi list i => (
                map(modules#(i-1), modules#i, matrix dd^C_i)
                );
            ZZdfactorization maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the target to induce a well-defined differential on the cokernel";
    result.cache.cokernel = f;
    result
    )

image ZZdFactorizationMap := ZZdFactorization => f -> (
    -- f : B --> C
    B := source f;
    C := target f;
    deg := degree f;
    (lo,hi) := (0,C.period);
    modules := hashTable for i from lo to hi list i => image f_(i-deg);
    result :=  if lo === hi then ZZdfactorization(modules#lo, Base => lo)
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
            ZZdfactorization maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the target to induce a well-defined differential on the image";
    result.cache.image = f;
    result
    )

coimage ZZdFactorizationMap := ZZdFactorization => f -> (
    -- f : B --> C
    B := source f;
    (lo,hi) := (0,B.period);
    modules := hashTable for i from lo to hi list i => coimage f_i;
    result := if lo === hi then ZZdfactorization(modules#lo, Base => lo)
        else (
            maps := hashTable for i from lo+1 to hi list i => (
                map(modules#(i-1), modules#i, matrix dd^B_i)
                );
            ZZdfactorization maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the source to induce a well-defined differential on the coimage";
    result.cache.coimage = f;
    result
    )


--isNullHomotopyOf = method()
--isNullHomotopic = method()
--nullHomotopy = method() -- this function attempts to construct one, might fail

isNullHomotopyOf(ZZdFactorizationMap, ZZdFactorizationMap) := (h, f) -> (
    -- returns true if h is a null homotopy for f : C --> D.
    -- if debugLevel > 0, then more info as to where it is not, is given
    C := source f;
    D := target f;
    degf := degree f;
    degh := degree h;
    if target f != target h then (
        if debugLevel > 0 then << "expected targets to be the same" << endl;
        return false;
        );
    if source f != source h then (
        if debugLevel > 0 then << "expected sources to be the same" << endl;
        return false;
        );
    if degh - degf =!= 1 then (
        if debugLevel > 0 then << "expected degree of first map to be one more than degree of the second" << endl;
        return false;
        );
    if debugLevel == 0 then h * dd^C + (-1)^degf * dd^D * h == f
    else (
        result := true;
        (lo,hi) := (0,period source h);
        for i from lo to hi do (
            if h_(i-1) * dd^C_i + (-1)^degf * dd^D_(degh+i) * h_i != f_i then (
                << "fails to be a null homotopy at location " << i << endl;
                result = false;
                );
            );
        result
        )
    )


nullHomotopyFreeSource = f -> (
    -- f:ZZdFactorizationMap
    -- key assumption: 'source f' is a complex of free modules
    -- result is a ZZdFactorizationMap h : C --> D, of degree degree(f)+1
    C := source f;
    D := target f;
    deg := degree f + 1;
    hs := new MutableHashTable;
    (lo,hi) := (0, period f);
    for i from lo to hi do (
        if hs#?(i-1) then ( 
            rem := (f_i - hs#(i-1) * dd^C_i) % (dd^D_(i+deg));
            if rem != 0 then return null; -- error "can't construct homotopy";
            hs#i = (f_i - hs#(i-1) * dd^C_i) // (dd^D_(i+deg))
            )
        else (
            rem = f_i % dd^D_(i+deg);
            if rem != 0 then return null; -- error "can't construct homotopy";
            hs#i = f_i // dd^D_(i+deg)
            )
        );
    map(D, C, new HashTable from hs, Degree => deg)
    )

isNullHomotopic ZZdFactorizationMap := Boolean => f -> (
    g := homomorphism' f;
    H := target g; 
    d := degree f;
    g1 := g_0 // dd^H_(d+1); 
    g_0 == dd^H_(d+1) * g1
    )



nullHomotopy ZZdFactorizationMap := ZZdFactorizationMap => {}  >> opts -> f -> (
    -- we check that the source is free, as that can be much faster
    -- TODO: nullHomotopy should perhaps be hook-ified.
    result := if isFree source f then nullHomotopyFreeSource f;
    if result =!= null then return result;
    g := homomorphism' f;
    H := target g; 
    d := degree f;
    g1 := g_0 // dd^H_(d+1);
    homomorphism(d+1,g1,H)
    )

isFree ZZdFactorization := Boolean => C -> (
    all(0..period(C)-1, i -> isFreeModule C_i)
    )

homology(ZZ,ZZdFactorizationMap) := Matrix => opts -> (i,f) -> (
    inducedMap(homology(i+degree f,target f), homology(i,source f),f_i)
    )
cohomology(ZZ,ZZdFactorizationMap) := Matrix => opts -> (i,f) -> homology(-i, f)

--no check for whether the periods are the same
homology ZZdFactorizationMap := ZZdFactorizationMap => opts -> (f) -> (
    C := source f;
    D := target f;
    deg := degree f;
    d := period C;
    HC := homology C;
    HD := homology D;
    maps := hashTable for i from 0 to d list
        i => inducedMap(HD_(i+deg), HC_i, f_i);
    result := map(HD, HC, maps, Degree => deg);
    result.cache.isCommutative = true; -- all differentials are zero
    result
    )


isQuasiIsomorphism ZZdFactorizationMap := Boolean => opts -> f -> (
    p := period source f;
    all(0..p-1,
        i -> HH_(i+1) cone f == 0
        )
    )


 

--------------------------------------------------------------------
-- short exact sequences -------------------------------------------
--------------------------------------------------------------------
--isShortExactSequence = method()
isShortExactSequence(ZZdFactorizationMap, ZZdFactorizationMap) := Boolean => (g, f) -> (
    -- f : A --> B, g : B --> C
    -- the SES is 0 --> A --> B --> C --> 0.
    isWellDefined g and 
    isWellDefined f and
    isFactorizationMorphism g and
    isFactorizationMorphism f and
    g*f == 0 and
    image f == kernel g and
    kernel f == 0 and
    coker g == 0
    )  



minimalPresentation ZZdFactorization := 
prune ZZdFactorization := ZZdFactorization => opts -> (cacheValue symbol minimalPresentation)(C -> (
    -- opts is ignored here
    -- to be cached: in the input C: cache the result D
    --               in the result: cache pruningMap: D --> C
    (lo,hi) := (0,C.period-1);
    --if C == 0 then return ZZdfactorization((ring C)^0, period C);
    nonzeros := select(lo..hi, i -> minimalPresentation C_i != 0);
    D := if #nonzeros === 0 
         then (
             ZZdfactorization((ring C)^0, period C)
             )
         else (
             lo = min nonzeros;
             hi = max nonzeros;
             if lo === hi 
             then ZZdfactorization(minimalPresentation C_lo, period C, Base=>lo)
             else (
                 maps := hashTable for i from lo+1 to hi+1 list i => minimalPresentation dd^C_i;
                 ZZdfactorization maps
                 )
             );
    -- create the isomorphism D --> C
    (lo,hi) = (0,D.period);
    pruning := hashTable for i from lo to hi list i => (minimalPresentation C_i).cache.pruningMap;
    D.cache.pruningMap = map(C,D,pruning);
    D.cache.pruningMap.cache.isCommutative = true;
    D
    ))


minimalPresentation ZZdFactorizationMap := 
prune ZZdFactorizationMap := ZZdFactorizationMap => opts -> f -> (
    C := source f;
    if not C.cache.?pruningMap then f = f * (minimalPresentation C).cache.pruningMap;
    D := target f;
    if not D.cache.?pruningMap then f = (minimalPresentation D).cache.pruningMap^-1 * f;
    f
    )


