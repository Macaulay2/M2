 needsPackage "Complexes"
 
 SimplicialModule = new Type of MutableHashTable --
  -- note: we make this mutable in order to construct the
  --   differential as a morphism of Simplicial modules (in the style of Complexes)
  -- BUT: after construction, it is should be IMMUTABLE!!
  -- at some point, we might want to allow lazy determination of the modules and maps
  -- but for now, we insist that all modules and maps are explicit.
  -- key:
  --  ring
  --  modules: hash table: ZZ => Module
  --  differential: SimplicialModuleMap from C --> C, degree -1
  
  
 SimplicialModuleMap = new Type of HashTable
  -- keys:
  --   degree: ZZ
  --   source: Simplicial module over a ring R
  --   target: simplicial module over the same ring R
  --   maps themselves (HashTable of Matrices), keys lying in the concentration period of the source.
  --    not all of the keys maps#i, need be present.
  --    missing ones are presumed to be zero maps.
  --   cache: a CacheTable
  --    cache.isCommutative: whether this map commutes with the face/degeneracy maps
  --      not set until needed.  unset means we have not checked yet, 
  --          and the user hasn't declared it to be true/false yet.

SimplicialModule.synonym = "Simplicial Module"
SimplicialModuleMap.synonym = "Map of Simplicial Modules"

topDegree = method();
topDegree SimplicialModule := ZZ => S -> S.topDegree
topDegree SimplicialModuleMap := ZZ => f -> max(topDegree source f, topDegree target f)
   
ring SimplicialModule := Ring => S -> S.ring

moduleMaker = (C,d) -> (
    moduleList := new MutableHashTable;
    	maxK := min (d, length C);
	for k to maxK do (
	moduleList#(d,k) = directSum toList(
	    binomial(d,k):(C_k));
	    );    
    for i in (sort keys moduleList) list (i,moduleList#i)
    )

mapMaker = (phi,d) -> (
    mapList := new MutableHashTable;
    	maxK := min (d, max(length source phi,length target phi));
	for k to maxK do (
	mapList#(d,k) = directSum toList(
	    binomial(d,k):(phi_k));
	    );  
    for i in (sort keys mapList) list (i,mapList#i)
    )


   

--H1 is the face maps, H2 is the degeneracy maps
simplicialModule = method(Options => {Base=>0,Degeneracy => false})
simplicialModule(Complex,HashTable,HashTable,ZZ) := SimplicialModule => opts -> (C,H1,H2,d) -> (
    spots := sort keys H1;
    if #spots === 0 then
      error "expected at least one map";
    R := ring C;
    moduleList := new MutableHashTable;
    for b to d do (
    	maxK := min (b, max C);
	for k to maxK do (
	    modwComps := directSum toList(binomial(b,k):(C_k));
	    modwComps.cache.components = flatten toList(binomial(b,k):(components (C_k)));
	    moduleList#(b,k) = modwComps
	);
	);
    S := new SimplicialModule from {
	symbol ring => R,
	symbol topDegree => d,
	symbol module => new HashTable from moduleList,
	symbol cache => new CacheTable,
    	symbol complexLength => max C,
	symbol complex => C
	};
    S.dd = map(S,S,H1,Degree=>-1);
    S.ss = map(S,S,H2,Degree=>1);
    S
    )

--H1 is the face maps
simplicialModule(Complex,HashTable,ZZ) := SimplicialModule => opts -> (C,H1,d) -> (--print("made it here!");
    spots := sort keys H1;
    if #spots === 0 then
      error "expected at least one map";
    (lo, hi) := concentration C;
    R := ring C;
    moduleList := new MutableHashTable;
    for b to d do (
    	maxK := min (b, hi);
	for k to maxK do (
	modwComps := directSum toList(binomial(b,k):(C_k));
	    modwComps.cache.components = flatten toList(binomial(b,k):(components (C_k)));
	    moduleList#(b,k) = modwComps
	);
	);
    S := new SimplicialModule from {
	symbol ring => R,
	symbol topDegree => d,
	symbol module => new HashTable from moduleList,
	symbol cache => new CacheTable,
    	symbol complexLength => hi,
	symbol complex => C
	};
    S.dd = map(S,S,H1,Degree=>-1);
    S
    )

simplicialModule(HashTable,HashTable,HashTable,ZZ) := SimplicialModule => opts -> (L,H1,H2,d) -> (--print("we got started");
    R := ring (L#((keys L)#0));
    S := new SimplicialModule from {
	symbol ring => R,
	symbol topDegree => d,
	symbol module => L,
	symbol cache => new CacheTable,
	};
    S.dd = map(S,S,H1,Degree=>-1);
    S.ss = map(S,S,H2,Degree=>1);
    S
    )

simplicialModule(HashTable,HashTable,ZZ) := SimplicialModule => opts -> (L,H1,d) -> (--print("we got started");
    R := ring (L#((keys L)#0));
    S := new SimplicialModule from {
	symbol ring => R,
	symbol topDegree => d,
	symbol module => L,
	symbol cache => new CacheTable,
	};
    S.dd = map(S,S,H1,Degree=>-1);
    S
    )


simplicialModule(Complex,ZZ) := SimplicialModule => opts -> (C,d) -> (
     --C is a chain complex, output is the Dold-Kan image of C in the category of simplicial modules
     if not instance(opts.Base, ZZ) then
      error "expected Base to be an integer";
     (lo, hi) := concentration C;
     --if lo == hi then
     if instance(C,Complex) then (
	 if opts.Degeneracy === true then (degenmapHash := hashTable flatten for n from 0 to d-1 list (
	    for i from 0 to n list (
		(n,i) => degenMapi(n,i,C)
		)
	     ););
	 facemapHash := hashTable flatten for n from 1 to d list (
	     for i from 0 to n list (
	 	 (n,i) => faceMapi(n,i,C)
	 	 )
	     );
	 --print("mde it here first");
	 if opts.Degeneracy === true then break return simplicialModule(C,facemapHash,degenmapHash,d);
	 --print("made it here");
	 return simplicialModule(C,facemapHash,d)
	 );
     )
 
 simplicialModule(Complex) := SimplicialModule => opts -> C -> (simplicialModule(C,max C,Degeneracy => opts.Degeneracy))
 

 simplicialModule(Module,ZZ) := SimplicialModule => opts -> (M,d) -> (simplicialModule(complex M,d,Degeneracy => opts.Degeneracy))
 
 simplicialModule(Ring,ZZ) := SimplicialModule => opts -> (R,d) -> (simplicialModule(R^1,d,Degeneracy => opts.Degeneracy))

simplicialModule(Ideal, ZZ) := SimplicialModule => opts -> (I,d) -> simplicialModule(module I, d, opts)
 
simplicialModule(ComplexMap,ZZ) := SimplicialModuleMap => opts -> (phi,d) -> (
    deg := degree phi;
    src := simplicialModule(source phi,d, opts);
    trg := simplicialModule(target phi, d, opts);
    if deg > 0 then return simplicialModule map((target phi)[deg], source phi, phi, Degree => 0);
    if deg < 0 then return simplicialModule map(target phi, (source phi)[-deg], phi[-deg], Degree => 0);
    result := map(trg,src,new HashTable from for i to d list i => directSum apply(mapMaker(phi,i),j->j_1),Degree => degree phi);
    result.cache.complexMap  = phi;
    result
    )

simplicialModule(ComplexMap) := SimplicialModuleMap => opts -> phi -> (tDeg := max((source phi).concentration_1,(target phi).concentration_1);
    simplicialModule(phi,tDeg, opts)
    )



isWellDefined SimplicialModule := Boolean => C -> (
    k := keys C;
    -- check keys, check their types
    if not instance(C.ring, Ring) then (
        if debugLevel > 0 then printerr "expected 'ring C' to be a ring";
        return false;
        );
    (lo,hi) := (0, C.topDegree);
    if not instance(hi,ZZ) or lo > hi then (
        if debugLevel > 0 then printerr "expected topDegree to be a nonnegative integer";
        return false;
        );
    if not instance(C.module, HashTable) then (
        if debugLevel > 0 then printerr "expected C.module to be a HashTable";
        return false;
        );
    if not instance(C.dd, SimplicialModuleMap) then (
        if debugLevel > 0 then printerr "expected dd^C to be a SimplicialModuleMap";
        return false;
        );
    if not instance(C.cache, CacheTable) then (
        if debugLevel > 0 then printerr "expected 'C.cache' to be a CacheTable";
        return false;
        );
    -- check ring matches modules
    if not all(keys C.module, i -> instance(i,Sequence) and i_0 >= lo and i_0 <= hi)
       and not all(keys C.module, i -> instance(i,ZZ) and i >= lo and i <= hi) then (
        if debugLevel > 0 then printerr("expected all keys of C.module to be sequences or integers with nonnegative first entry, bounded by the top degree", toString [lo,hi]);
        return false;
        );
    if not all(values C.module, m -> ring m === ring C) then (
        if debugLevel > 0 then printerr "expected all modules in C.module to be over 'ring C'";
        return false;
        );
    -- check face maps
    if ring C.dd =!= ring C then (
        if debugLevel > 0 then printerr "expected ring of the face maps to be the ring of the simplicial module";
        return false;
        );
    if degree C.dd =!= -1 then (
        if debugLevel > 0 then printerr "expected degree of the face maps to be -1";
        return false;
        );
    if not all(keys (dd^C).map, i -> instance(i,Sequence) and i_0 >= lo+1 and i_0 <= hi) then (
        if debugLevel > 0 then printerr "expected all maps of the face maps to be indexed by integers in the concentration [lo+1,hi]";
        return false;
        );
    for i from lo+1 to hi do (
        f := dd^C_i;
        if source f != C_i or target f != C_(i-1)
        then (
            if debugLevel > 0 then (
                printerr "expected source and target of the face maps to be modules in the simplicial module";
                printerr("  face map at index ", toString i, " fails this condition");
            );
            return false;
            );
        );
    if not(C.?ss) then (
    D := naiveNorm C;
    if not isWellDefined D then (
            if debugLevel > 0 then printerr "expected naive normalization to be a well-defined complex";
            return false;
            );
	);
    if C.?ss then (
	if not isSimplicialModule C then (
	    if debugLevel > 0 then printerr "object fails to satisfy simplicial identities; run isSimplicialModule to see where it fails";
	    return false;
	    );
	);
    true
    )


-- helpers for comparing map compositions (handles both free and non-free modules)
-- tries direct composition first; falls back to matrix-level entry comparison
mapsEq = (A, B, C, D) -> (
    -- checks if A*B == C*D as maps
    if target B === source A and target D === source C
    then try (A * B == C * D) else matEq(matrix A * matrix B, matrix C * matrix D)
    else matEq(matrix A * matrix B, matrix C * matrix D)
    )
mapIsId = (A, B) -> (
    -- checks if A*B == identity
    if target B === source A
    then try (A * B == id_(source B)) else matIsId(matrix A * matrix B)
    else matIsId(matrix A * matrix B)
    )
matEq = (A, B) -> entries A == entries B
matIsId = A -> entries A == entries id_((ring A)^(numrows A))

--this method checks if the simplicial identities hold for simplicial objects with degeneracy map keys
isSimplicialModule = method()
isSimplicialModule(SimplicialModule) := Boolean => S -> (
    if not S.?ss then error "expected S to have both face and degeneracy maps";
    faceMaps := S.dd;
    degenMaps := S.ss;
    t := S.topDegree;
    for i from 1 to t do (
	for j from 2 to i do (
	    for k from 1 to j-1 do (
		if not mapsEq(dd^S_(i-1,k), dd^S_(i,j), dd^S_(i-1,j-1), dd^S_(i,k))
		then (
		    if debugLevel > 0 then (
			printerr "simplicial map identities fail for face/face compositions";
			printerr("face/face composition for indices ", toString (i,j,k), " fail");
			);
		    return false;
		    );
		);
	    );
	);
    for i from 0 to t-1 do (
	for j from 0 to i do (
	    for k from 0 to j-1 do (
		if not mapsEq(dd^S_(i+1,k), ss^S_(i,j), ss^S_(i-1,j-1), dd^S_(i,k))
		then (
		    if debugLevel > 0 then (
			printerr "simplicial map identities fail for face/degeneracy compositions";
			printerr("face/degeneracy composition for indices ", toString (i,j,k), " fail");
			);
		    return false;
		    );
		);
	    );
	);
    for i from 0 to t-1 do (
	for j from 0 to i do (
	    if not mapIsId(dd^S_(i+1,j), ss^S_(i,j)) or not mapIsId(dd^S_(i+1,j+1), ss^S_(i,j))
	    then (
		if debugLevel > 0 then (
			printerr "simplicial map identities fail for face/degeneracy compositions";
			printerr("face/degeneracy composition for indices ", toString (i,j,j), " fail");
			);
		    return false;
		    );
		);
	    );
    for i from 0 to t-1 do (
	for j from 0 to i do (
	    for k from j+2 to i do (
		if not mapsEq(dd^S_(i+1,k), ss^S_(i,j), ss^S_(i-1,j), dd^S_(i,k-1))
		then (
		    if debugLevel > 0 then (
			printerr "simplicial map identities fail for face/degeneracy compositions";
			printerr("face/degeneracy composition for indices ", toString (i,j,k), " fail");
			);
		    return false;
		    );
		);
	    );
	);
    for i from 0 to t-1 do (
	for j from 0 to i do (
	    for k from 0 to j do (
		if not mapsEq(ss^S_(i+1,k), ss^S_(i,j), ss^S_(i+1,j+1), ss^S_(i,k))
		then (
		    if debugLevel > 0 then (
			printerr "simplicial map identities fail for degeneracy/degeneracy compositions";
			printerr("degeneracy/degeneracy composition for indices ", toString (i,j,k), " fail");
			);
		    return false;
		    );
		);
	    );
	);
    true
    )
    
    

isWellDefined SimplicialModuleMap := f -> (
    k := keys f;
    -- source and target
    if ring f.source =!= ring f.target then (
        if debugLevel > 0 then printerr "expected source and target to have the same ring";
        return false;
        );
    if not isWellDefined f.source or not isWellDefined f.target then (
        if debugLevel > 0 then printerr "expected source and target to be well-defined simplicial modules";
        return false;
        );
    if not instance(f.degree, ZZ) then (
        if debugLevel > 0 then printerr "expected degree of homomorphism to be an integer";
        return false;
        );
    (lo,hi) := (0,f.source.topDegree);
    if not all(keys f.map, i -> instance(i,ZZ) or instance(i, Sequence)) then (
        if debugLevel > 0 then printerr "expected all maps to be indexed by integers or sequences of two integers";
        return false;
        );
    if all(keys f.map, i -> instance(i,ZZ)) then for i from lo to hi do (
        g := f_i;
        if source g != f.source_i or target g != f.target_(i+f.degree)
        then (
            if debugLevel > 0 then (
                printerr "expected source and target of maps to agree with those of simplicial module";
                printerr("  the map at index ", toString i, " fails this condition");
            );
            return false;
            );
        );
    --print "here we are";
    if all(keys f.map, i -> instance(i,ZZ)) and f.cache.?isCommutative then (
        deg := degree f;
        C := f.source;
        D := f.target;
        (loC,hiC) := (0, C.topDegree);
        (loD,hiD) := (0, D.topDegree);
        iscommutative := true;
        for i from loC to hiC do (
            if i+deg-1 >= loD and i+deg-1 <= hiD then (
		for l from 0 to i do (
                if not (try (dd^D_(i+deg,l) * f_i == (-1)^deg * (f_(i-1) * dd^C_(i,l))) else matEq(matrix(dd^D_(i+deg,l)) * matrix(f_i), (-1)^deg * (matrix(f_(i-1)) * matrix(dd^C_(i,l)))))
                then (
                    iscommutative = false;
                    if f.cache.isCommutative then (
                        if debugLevel > 0 then (
                            printerr "the cache table incorrectly asserts that the maps commute with the differentials";
                            printerr("  differential at index ", toString i, " fails this condition");
                            );
                        return false;
                        );
                    )
	    );));
        if iscommutative and not f.cache.isCommutative then (
            if debugLevel > 0 then printerr "the cache table incorrectly asserts that the maps do not commute with the differentials";
            return false;
            );
        );
    true
    )


isCommutative SimplicialModuleMap := Boolean => f -> (
    if debugLevel == 0 and f.cache.?isCommutative then
       return f.cache.isCommutative;
    C := source f;
    D := target f;
    deg := degree f;
    hasDegens := C.?ss and D.?ss;
    (loC,hiC) := (0, C.topDegree);
    (loD,hiD) := (0, D.topDegree);
    for i from loC to hiC do (
        if i+deg-1 >= loD and i+deg-1 <= hiD then (
	    for l from 0 to i do (
            if not (try (dd^D_(i+deg,l) * f_i == (-1)^deg * (f_(i-1) * dd^C_(i,l))) else matEq(matrix(dd^D_(i+deg,l)) * matrix(f_i), (-1)^deg * (matrix(f_(i-1)) * matrix(dd^C_(i,l)))))
	    or not (if hasDegens then (try (ss^D_(i+deg,l) * f_i == (-1)^deg * (f_(i+1) * ss^C_(i,l))) else matEq(matrix(ss^D_(i+deg,l)) * matrix(f_i), (-1)^deg * (matrix(f_(i+1)) * matrix(ss^C_(i,l))))) else true)
            then (
                if debugLevel > 0 then printerr("block ", toString (i,i-1), " fails to commute");
                f.cache.isCommutative = false;
                return false;
                )
	    );
            )
        );
    f.cache.isCommutative = true;
    true
    )

isSimplicialMorphism = method(TypicalValue => Boolean)
isSimplicialMorphism SimplicialModuleMap := (f) -> (
    if debugLevel > 0 and degree f =!= 0 then (
        printerr "the complex map has non-zero degree";
        return false;
        );
    degree f === 0 and isCommutative f
    )


--this function forgets the data of the underlying complex of a simplicial module, if the simplicial module S
--is obtained as a Dold-Kan image
 forgetComplex = method(Options => {RememberSummands => true});
 forgetComplex(SimplicialModule) := SimplicialModule => opts -> S -> (
     if not S.?complex then return S;
     L := hashTable for i to S.topDegree list i => combineSFactors(S,i,RememberSummands => opts.RememberSummands);
     -- rewrite maps so their source/target match the new combined modules
     H1 := applyPairs(S.dd.map, (k, f) -> (k, map(L#(first k - 1), L#(first k), matrix f)));
     if S.?ss then (
	 H2 := applyPairs(S.ss.map, (k, f) -> (k, map(L#(first k + 1), L#(first k), matrix f)));
	 return simplicialModule(L, H1, H2, S.topDegree);
	 );
     D := simplicialModule(L, H1, S.topDegree);
     D.cache.components = components S;
     D
     )

--totalizing operation
combineSFactors = method(Options => {RememberSummands => true});
combineSFactors(SimplicialModule,ZZ) := Module => opts -> (S,d) -> (
    if d<0 or d > S.topDegree then return (ring S)^0;
    modwComps := directSum for j in components S list directSum for i to min(d,j.complexLength) list (j.module)#(d,i);
    if opts.RememberSummands then modwComps.cache.components = flatten flatten for j in components S list for i to min(d,S.complexLength) list components (S.module#(d,i));
    modwComps
    )


--forgets the data of degeneracy maps of a simplicial object 
 --useful for when the user wants to ignore degeneracy maps
 --for the purposes of speeding up computations
 forgetDegeneracy = method();
 forgetDegeneracy(SimplicialModule) := S -> (
     if not S.?ss then return S;
     if S.?complex then return simplicialModule(S.complex,S.dd.map,S.topDegree);
     simplicialModule(S.module,S.dd.map,S.topDegree)
     )
 
 
SimplicialModule _ Sequence := Module => (S,p) -> (
    if #p =!= 2 then
    	error ("Expected a pair of integer indices");
    if S.module#?(p#0,p#1) then S.module#(p#0,p#1) else (ring S)^0
    )


SimplicialModule _ ZZ := Module => (S,n) -> (if S.module#?n then S.module#n else combineSFactors(S,n)) 



net SimplicialModule := S -> (
     (lo,hi) := (0,topDegree S);
     if lo > hi then 
         error "in a simplicial module, top degree should be nonnegative"
         --"0"
     else if lo == hi and S_lo === 0 then 
         "0"
     else if S.?complex then
         (horizontalJoin (between(" <-- ", 
             for i from lo to hi list
                 stack (net directSum(for k from 0 to min(i,S.complexLength) list (S.module)#(i,k)), " ", net i)) | {"<-- ..."}) )
     else 
         horizontalJoin (between(" <-- ", 
             for i from lo to hi list
                 stack (net ((S.module)#i), " ", net i)) | {"<-- ..."})
     )
 
 
 Symbol ^ SimplicialModule := SimplicialModuleMap => (sym, C) -> (
    if sym === dd then return C.dd;
    if sym === ss then C.ss
    else error "expected symbol to be 'dd' or 'ss'"
    )
 
lineOnTop := (s) -> concatenate(width s : "-") || s

source SimplicialModuleMap := SimplicialModule => f -> f.source
target SimplicialModuleMap := SimplicialModule => f -> f.target
ring SimplicialModuleMap := SimplicialModule => f -> ring source f
degree SimplicialModuleMap := ZZ => f -> f.degree

isHomogeneous SimplicialModuleMap := (f) -> all(values f.map, isHomogeneous)

--simplicialModuleMap = method(Options => {Degeneracy => false});




map(SimplicialModule, SimplicialModule, HashTable) := SimplicialModuleMap => opts -> (tar, src, maps) -> (
    if not(topDegree tar == topDegree src) then error "expected source and target to have the same top degree";
    R := ring tar;
    if ring src =!= R or any(values maps, f -> ring f =!= R) then
        error "expected source, target and maps to be over the same ring";
    deg := if opts.Degree === null 
           then 0 
           else if instance(opts.Degree, ZZ) then 
             opts.Degree
           else
             error "expected integer degree";
    (lo,hi) := (0,topDegree tar);
    maps' := hashTable for k in keys maps list (
        if instance(k, Sequence) then (
        f := maps#k;
        -- note: we use != instead of =!= in the next 2 tests,
        -- since we want to ignore any term order differences
	--print(k);
	--print(source f);
	--print(src_(first k));
        -*if rank source f != rank src_( first k) then (
            error ("map with index "|toString(k)|" has inconsistent source");
	);
        if rank target f !=  rank tar_(first(k)+deg) then
            error ("map with index "|toString(k)|" has inconsistent target");
        if first k < lo or first k > hi then continue else*- (k,f)
	)
       else (
	    f = maps#k;
        -- note: we use != instead of =!= in the next 2 tests,
        -- since we want to ignore any term order differences
	--print(k);
	--print(source f);
	--print(src_(first k));
        -*if source f !=  src_(k) then (
            error ("map with index "|toString(k)|" has inconsistent source");
	);
        if target f !=   tar_(k+deg) then
            error ("map with index "|toString(k)|" has inconsistent target");
        if k < lo or k > hi then continue else*- (k,f)
        ));
    new SimplicialModuleMap from {
        symbol source => src,
        symbol target => tar,
        symbol degree => deg,
        symbol map => maps',
        symbol cache => new CacheTable
        }
    )

map(SimplicialModule, SimplicialModule, ZZ) := SimplicialModuleMap => opts -> (D, C, j) -> (
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

---this method is good for inducing maps on subcomplexes
map(SimplicialModule, SimplicialModule, SimplicialModuleMap) := SimplicialModuleMap => opts -> (tar, src, f) -> (
    deg := if opts.Degree === null then degree f else opts.Degree;
    H := applyPairs(f.map, (k, v) -> (k, map(tar_(deg+k), src_k, v)));
    map(tar,src,H, Degree=>deg)
    )




SimplicialModuleMap _ ZZ := Matrix => (f,i) -> (
    if f.map#?i then f.map#i else map((target f)_(i + degree f), (source f)_i, 0))

SimplicialModuleMap _ Sequence := Matrix => (f,s) -> (
    if f.map#?s then f.map#s else map((target f)_(s#0 + degree f), (source f)_(s#0), 0))




expression SimplicialModuleMap := Expression => f -> (
    d := degree f;
    s := sort keys f.map;
    if #s === 0 then 
        new ZeroExpression from {0}
    else if instance(s_0,Sequence) then new VerticalList from for i in s list
        RowExpression {(i#0+d,i#1), ":", MapExpression { target f_i, source f_i, f_i }, ":", i}
    else if instance(s_0,ZZ) then return new VerticalList from for i in s list
        RowExpression {i+d, ":", MapExpression { target f_i, source f_i, f_i }, ":", i}
    )


 
 net SimplicialModuleMap := Net => f -> (
     v := between("",
            for i in sort keys f.map list (
                if instance(i,Sequence) then (horizontalJoin(
		            net ((i#0+f.degree,i#1)), " : ", net target f_i, " <--",
		            lineOnTop net f_i,
		            "-- ", net source f_i, " : ", net i
                    ))
	        else  (horizontalJoin(
		            net (i+f.degree), " : ", net target f_i, " <--",
		            lineOnTop net f_i,
		            "-- ", net source f_i, " : ", net i
			    ))
                ));
     if # v === 0 then net "0"
     else stack v
     )
 



SimplicialModule == ZZ := (C,n) -> (
    if n =!= 0 then error "cannot compare Simplicial module to non-zero integer";
    (lo,hi) := (0,C.topDegree);
    for i from lo to hi do if C_i != 0 then return false;
    true
    )
ZZ == SimplicialModule := (n,C) -> C == n
 
--as written, the code assumes one only takes direct sums of simplicial modules with same top degree
SimplicialModule.directSum = args -> (
    local D;
    assert(#args > 0);
    R := ring args#0;
    if not all(args, C -> ring C === R) then error "expected all simplicial modules to be over the same ring";
    concentrations := for C in args list (0,C.topDegree);
    --strip complex data to ensure consistent module objects in direct sums
    allDegens := all(args, i -> i.?ss);
    origArgs := args;
    args = apply(args, i -> forgetComplex(i));
    lo := concentrations/first//min;
    hi := concentrations/last//max;
    if not(all(args,i->i.topDegree==hi)) then error "all objects should have the same top degree";
    S := first args;
    LM := new HashTable from for i in keys S.module list i => directSum for j in args list if j.module#?i then j_i else continue;
    faceHashM := new HashTable from for i in keys S.dd.map list i => directSum for j in args list if j.dd.map#?i then j.dd_i;
    if allDegens then (
	degenMapHashM := new HashTable from for i in keys S.ss.map list i => directSum for j in args list if j.ss.map#?i then j.ss_i;
	D = simplicialModule(LM,faceHashM,degenMapHashM,hi);
	D.cache.components = toList origArgs;
	return D;
	);
    D = simplicialModule(LM,faceHashM,S.topDegree);
    D.cache.components = toList origArgs;
    D    
    )
SimplicialModule ++ SimplicialModule := SimplicialModule => (C,D) -> directSum(C,D)
directSum SimplicialModule := C -> directSum(1 : C)

components SimplicialModule := C -> if C.cache.?components then C.cache.components else {C}

SimplicialModule#id = (C) -> (
    (lo,hi) := (0,C.topDegree);
    maps := hashTable for i from lo to hi list i => id_(C_i);
    result := map(C,C,maps);
    result.cache.isCommutative = true;
    result
    )


SimplicialModuleMap ^ ZZ := SimplicialModuleMap => (f,n) -> (
    tDeg := (source f).topDegree;
    df := degree f;
    if n === -1 then (
        maps := hashTable for i from 0 to tDeg list (i+df) => (
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
      maps = hashTable for i from 0 to tDeg list i => (
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


SimplicialModule ** SimplicialModule := SimplicialModule => (C,D) -> simplicialTensor(C,D)

--it seemed more efficient to directly define the tensor product with a module
--as opposed to converting the module into a simplicial object then using simplicialTensor
Module ** SimplicialModule := SimplicialModule => (M,S) -> (
    if S.?complex then S = forgetComplex S;
    LM := applyValues(S.module, x -> M**x);
    faceHashM := applyPairs(S.dd.map, (i, v) -> (i, map(M ** S_(i_0-1), M ** S_(i_0), M**v)));
    if S.?ss then (
	degenMapHashM := applyPairs(S.ss.map, (i, v) -> (i, map(M ** S_(i_0+1), M ** S_(i_0), M**v)));
	return simplicialModule(LM,faceHashM,degenMapHashM,S.topDegree);
	);
    simplicialModule(LM,faceHashM,S.topDegree)
    )

SimplicialModule ** Module := SimplicialModule => (S,M) -> (
    if S.?complex then S = forgetComplex S;
    LM := applyValues(S.module, x -> x**M);
    faceHashM := applyPairs(S.dd.map, (i, v) -> (i, map(S_(i_0-1) ** M, S_(i_0) ** M, v**M)));
    if S.?ss then (
	degenMapHashM := applyPairs(S.ss.map, (i, v) -> (i, map(S_(i_0+1) ** M, S_(i_0) ** M, v**M)));
	return simplicialModule(LM,faceHashM,degenMapHashM,S.topDegree);
	);
    simplicialModule(LM,faceHashM,S.topDegree)
    )

SimplicialModule ** Matrix := SimplicialModuleMap => (S, f) -> (
    if S.?complex then S = forgetComplex S;
    if ring S =!= ring f then error "expected Simplicial module and Matrix over the same ring";
    src := S ** source f;
    tar := S ** target f;
    map(tar, src, new HashTable from for i to S.topDegree list i => map(tar_i, src_i, S_i ** f))
    )

Matrix ** SimplicialModule := SimplicialModuleMap => (f, S) -> (
    if S.?complex then S = forgetComplex S;
    if ring S =!= ring f then error "expected Simplicial module and Matrix over the same ring";
    src := (source f) ** S;
    tar := (target f) ** S;
    map(tar, src, new HashTable from for i to S.topDegree list i => map(tar_i, src_i,  f ** S_i))
    )

SimplicialModule ** Ring := SimplicialModule => (S,R) -> (
    if S.?complex then S = forgetComplex S;
    LM := applyValues(S.module, m -> R**m);
    faceHashM := applyValues(S.dd.map, m -> R**m);
    if S.?ss then (
	degenMapHashM := applyValues(S.ss.map, m -> R**m);
	return simplicialModule(LM,faceHashM,degenMapHashM,S.topDegree);
	);
    simplicialModule(LM,faceHashM,S.topDegree)
    )

Ring ** SimplicialModule := SimplicialModule => (R,S) -> S ** R

RingMap SimplicialModule := SimplicialModule => (phi,S) -> (
    if S.?complex then return simplicialModule(tensor(phi, S.complex), S.topDegree, Degeneracy => S.?ss);
    LM := applyValues(S.module, m -> phi m);
    faceHashM := applyValues(S.dd.map, f -> phi f);
    if S.?ss then (
	degenMapHashM := applyValues(S.ss.map, f -> phi f);
	return simplicialModule(LM,faceHashM, degenMapHashM, S.topDegree);
	);
    simplicialModule(LM,faceHashM,S.topDegree)
    )

tensor(RingMap, SimplicialModule) := SimplicialModule => {} >> opts -> (phi, S) -> (
    if source phi =!= ring S then error "expected the source of the ring map to be the ring of the simplicial module";
    if S.?complex then return simplicialModule(tensor(phi, S.complex), S.topDegree, Degeneracy => S.?ss);
    LM := applyValues(S.module, m -> tensor(phi, m));
    faceHashM := applyValues(S.dd.map, m -> tensor(phi, m));
    if S.?ss then (
	degenMapHashM := applyValues(S.ss.map, m -> tensor(phi, m));
	return simplicialModule(LM,faceHashM,degenMapHashM,S.topDegree);
	);
    simplicialModule(LM,faceHashM,S.topDegree)
    )
tensor(SimplicialModule, RingMap) := SimplicialModule => {} >> opts -> (S, phi) -> tensor(phi, S)

RingMap ** SimplicialModule := SimplicialModule => (phi, S) -> tensor(phi, S)
SimplicialModule ** RingMap := SimplicialModule => (S, phi) -> tensor(phi, S)


SimplicialModuleMap | SimplicialModuleMap := SimplicialModuleMap => (f,g) -> (
    if target f != target g then error "expected targets to be the same";
    if (source f).topDegree =!= (source g).topDegree then error "expected sources to have same top degree";
    deg := degree f;
    if deg =!= degree g then error "expected maps with the same degree";
    map(target f, source f ++ source g, new HashTable from for i to (source f).topDegree list i => (f_i|g_i), Degree=>deg)
    )

SimplicialModuleMap || SimplicialModuleMap := SimplicialModuleMap => (f,g) -> (
    if source f != source g then error "expected sources to be the same";
    if (target f).topDegree =!= (target g).topDegree then error "expected targets to have same top degree";
    deg := degree f;
    if deg =!= degree g then error "expected maps with the same degree";
    map(target f ++ target g, source f, new HashTable from for i to (source f).topDegree list i => (f_i||g_i), Degree=>deg)
    )

SimplicialModule == SimplicialModule := (C,D) -> (
    --note: we don't check for equality of keys since some operations
    --on simplicial modules may add a key
    if C === D then return true;
    if topDegree C =!= topDegree D then return false;
    if ring C =!= ring D then return false;
    for i from 0 to C.topDegree do (
        if C_i != D_i then return false;
        );
    for i in keys C.dd.map do (
	if not matEq(matrix C.dd.map#i, matrix D.dd.map#i) then return false;
	);
    if C.?ss then for i in keys C.ss.map do (
	if not matEq(matrix C.ss.map#i, matrix D.ss.map#i) then return false;
	);
    true    
    )

SimplicialModule == ZZ := (C,n) -> (
    if n =!= 0 then error "cannot compare simplicial module to non-zero integer";
    for i from 0 to C.topDegree do if C_i != 0 then return false;
    true
    )
ZZ == SimplicialModule := (n,C) -> C == n

transs := (C,v) -> (
    if C.cache.?indexComponents then (
	    Ci := C.cache.indexComponents;
	    apply(v, i -> if Ci#?i then Ci#i else error "expected an index of a component of the direct sum"))
    else (
        if not C.cache.?components then error "expected a direct sum of simplicialmodules";
	    Cc := C.cache.components;
	    apply(v, i -> if not Cc#?i then error "expected an index of a component of the direct sum");
	    v)
    )


SimplicialModule _ Array := SimplicialModuleMap => (C,v) -> (
    v = transs(C,v);
    D := directSum apply(toList v, j -> C.cache.components#j);
    Cc := if C.?complex then forgetComplex(C,RememberSummands => false) else C;
    maps := hashTable for i from 0 to Cc.topDegree list i => map(C_i,D_i,Cc_i_v);
    result := map(C,D,maps);
    result.cache.isCommutative = true;
    result
    )

SimplicialModule ^ Array := SimplicialModuleMap => (C,v) -> (
    v = transs(C,v);
    D := directSum apply(toList v, j -> C.cache.components#j);
    Cc := if C.?complex then forgetComplex(C,RememberSummands => false) else C;
    maps := hashTable for i from 0 to Cc.topDegree list i => map(D_i,C_i,Cc_i^v);
    result := map(D,C,maps);
    result.cache.isCommutative = true;
    result
    )


SimplicialModuleMap == SimplicialModuleMap := (f,g) -> (
    if f === g then return true;    
    if source f != source g or target f != target g 
      then return false;
    for i from 0 to (source f).topDegree do (
        if not (try (f_i == g_i) else matEq(matrix f_i, matrix g_i)) then return false;
        );
    true
    )
SimplicialModuleMap == ZZ := Boolean => (f,n) -> (
    if n === 0 then
        all(keys f.map, k -> f.map#k == 0)
    else if n === 1 then (
        if source f != target f then return false;
        if degree f =!= 0 then return false;
        (lo,hi) := (0,(source f).topDegree);
        for i from lo to hi do
            if not (try (f_i == id_((source f)_i)) else matIsId(matrix f_i)) then return false;
        f.cache.isCommutative = true;  -- this is the identity, after all!
        true
        )
    else 
        error "cannot compare ComplexMap to integer other than 0 or 1"
    )
ZZ == SimplicialModuleMap := Boolean => (n,f) -> f == n

RingElement * SimplicialModuleMap := (r,f) -> (
    df := degree f;
    maps := hashTable for i to (source f).topDegree list i => (
        h := r * f_i;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    result
    )

SimplicialModuleMap * RingElement := (f,r) -> (r*f)

Number * SimplicialModuleMap := (r,f) -> (
    try r = promote(r,ring f) else error "can't promote scalar to ring of complex homomorphism";
    r * f
    )

SimplicialModuleMap * Number := (f,r) -> (
    try r = promote(r,ring f) else error "can't promote scalar to ring of complex homomorphism";
    f * r
    )

- SimplicialModuleMap := (f) -> (
    result := (-1)*f;
    if isCommutativeCached f then
        result.cache.isCommutative = true;
    result
    )

SimplicialModuleMap + SimplicialModuleMap := (f,g) -> (
    df := degree f;
    dg := degree g;
    if source f != source g then error "expected simplicial module homomorphisms with the same source";
    if target f != target g then error "expected simplicial homomorphisms with the same target";
    if df =!= dg then error "expected complex homomorphisms with the same degree";
    maps := hashTable for i from 0 to (source f).topDegree list i => (
        h := f_i + g_i;
        if h == 0 then continue else h
        );
    result := map(target f, source f, maps, Degree=>df);
    result
    )
SimplicialModuleMap + Number :=
SimplicialModuleMap + RingElement := SimplicialModuleMap => (f,r) -> (
    if r == 0 then f
    else (
        if source f != target f
        then error "expected same source and target"
        else f + r*id_(target f))
    )
Number + SimplicialModuleMap :=
RingElement + SimplicialModuleMap := SimplicialModuleMap => (r,f) -> f + r

SimplicialModuleMap - Number :=
SimplicialModuleMap - RingElement :=
SimplicialModuleMap - SimplicialModuleMap := SimplicialModuleMap => (f,g) -> f + (-1)*g

Number - SimplicialModuleMap :=
RingElement - SimplicialModuleMap := SimplicialModuleMap => (r,f) -> -f + r

SimplicialModuleMap * SimplicialModuleMap := (f,g) -> (
    --this is the case where just composing normal maps
    if all(keys f.map, i-> instance(i, ZZ)) then (
    df := degree f;
    dg := degree g;
    src := source g;
    tar := target f;
    maps := hashTable for i from 0 to src.topDegree list i => (
        h := if target(g_i) === source(f_(dg + i))
             then f_(dg + i) * g_i
             else map(tar_(df+dg+i), src_i, matrix(f_(dg + i)) * matrix(g_i));
        if h == 0 then continue else h
        );
    result := map(tar, src, maps, Degree=>df+dg);
    return result;
    );
    --this is the case where face/degeneracy maps are being composed
    --note: this case is not really used, since composition of face maps
    --should be treated as a sort of totalized operation
    if all(keys f.map, i-> instance(i, Sequence)) then (
	    df = degree f;
	    dg = degree g;
	    maps = hashTable for i in keys g.map list i => (
		h := matrix(f_((dg + i_0, i_1))) * matrix(g_i);
		if h == 0 then continue else h
		);
	    result = map(target f, source g, maps, Degree=>df+dg);
	    return result;
	    );
    )

--need to complete this
SimplicialModuleMap.directSum = args -> (
    -- args: sequence of SimplicialModuleMap's
    -- args: f_i : C_i --> D_i, having same degree deg
    -- result : sum(C_i) --> sum(D_i)
    R := ring args#0;
    deg := degree args#0;
    if not all(args, f -> ring f === R) then 
        error "expected maps all over the same ring";
    if not all(args, f -> degree f === deg) then
        error "expected maps to all have the same degree";
    -- WARNING: we call simplicialModule.directSum directly rather than using
    -- just directSum to avoid getting a cached copy of the direct
    -- sum.  Otherwise the labels of the cached copies might get
    -- changed (in Options.directSum).
    src := SimplicialModule.directSum (args/source);
    tar := SimplicialModule.directSum (args/target);
    -- only keep matrices in the homomorphism that are non-zero
    spots := unique flatten(args/(f -> keys f.map));
    maps := hashTable for i in spots list i => directSum(args/(f -> f_i));
    result := map(tar,src,maps,Degree=>deg);
    result.cache.components = toList args;
    result
    )

SimplicialModuleMap ++ SimplicialModuleMap := SimplicialModuleMap => (f,g) -> directSum(f,g)
directSum SimplicialModuleMap := f -> directSum(1 : f)
components SimplicialModuleMap := f -> if f.cache.?components then f.cache.components else {f}
SimplicialModuleMap ^ Array := SimplicialModuleMap => (f,v) -> (target f)^v * f
SimplicialModuleMap _ Array := SimplicialModuleMap => (f,v) -> f * (source f)_v




-- the following method is not exported:
isCommutativeCached = method()
isCommutativeCached SimplicialModuleMap := Boolean => f -> f.cache.?isCommutative and f.cache.isCommutative


--------------------------------------------------------------------
-- tensor products of simplicial maps ------------------------------
--------------------------------------------------------------------
tensor(SimplicialModuleMap, SimplicialModuleMap) := SimplicialModuleMap => {} >> opts -> (f,g) -> (
    -- f : C1 --> C2, g : D1 --> D2
    -- f**g : C1**D1 --> C2**D2
    -- (f**g)_i : sum_j(C1_j ** D1_(i-j) --> C2_(j+df) ** D2_(i-j+dg))
    df := degree f;
    dg := degree g;
    src := (source f) ** (source g);
    tar := (target f) ** (target g);
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    (lo,hi) := (0,src.topDegree);
    maps := hashTable for i from lo to hi list i => f_i**g_i;
    result := map(tar, src, maps, Degree=>df+dg);
    result    
    )
SimplicialModuleMap ** SimplicialModuleMap := SimplicialModuleMap => (f,g) -> tensor(f,g)
SimplicialModule ** SimplicialModuleMap := SimplicialModuleMap => (C,g) -> id_C ** g
SimplicialModuleMap ** SimplicialModule := SimplicialModuleMap => (f,D) -> f ** id_D
Module ** SimplicialModuleMap := SimplicialModuleMap => (M,g) -> (
    map(M**(target g),M**(source g), applyValues(g.map, v -> M**v), Degree => degree g)
    )
SimplicialModuleMap ** Module := SimplicialModuleMap => (g,N) -> (
    map((target g) ** N,(source g) ** N, applyValues(g.map, v -> v**N), Degree => degree g)
    )


SimplicialModuleMap ** Ring := SimplicialModuleMap => (g,R) -> (
    map((target g)**R,(source g)**R, applyValues(g.map, v -> v**R), Degree => degree g)
    )
Ring ** SimplicialModuleMap := SimplicialModuleMap => (R,f) -> f ** R

RingMap SimplicialModuleMap := SimplicialModuleMap => (phi,f) -> (
    map(phi (target f), phi (source f), applyValues(f.map, v -> phi v), Degree => degree f)
    )

tensor(RingMap, SimplicialModuleMap) := SimplicialModuleMap => {} >> opts -> (phi, f) -> (
    if source phi =!= ring f then error "expected the source of the ring map to be the ring of the complex map";
    map(tensor(phi, target f), tensor(phi, source f), applyValues(f.map, v -> tensor(phi, v)), Degree => degree f)
    )
tensor(SimplicialModuleMap, RingMap) := SimplicialModuleMap => {} >> opts -> (f, phi) -> tensor(phi, f)

RingMap ** SimplicialModuleMap := SimplicialModuleMap => (phi, f) -> tensor(phi, f)
SimplicialModuleMap ** RingMap := SimplicialModuleMap => (f, phi) -> tensor(phi, f)

----------------------------------------------------------------------------------------
------------- some functionality for complexes acting on simplicial modules ------------

Complex ** SimplicialModule := SimplicialModule => (C,S) -> (simplicialModule(C,S.topDegree)**S)

SimplicialModule ** Complex := SimplicialModule => (S,C) -> (S**simplicialModule(C,S.topDegree))

ComplexMap ** SimplicialModuleMap := SimplicialModuleMap => (f,g) -> (tDeg := max((source g).topDegree,(target g).topDegree);
    simplicialModule(f,tDeg)**g
    )

SimplicialModuleMap ** ComplexMap := SimplicialModuleMap => (g,f) -> (tDeg := max((source g).topDegree,(target g).topDegree);
    g**simplicialModule(f,tDeg)
    )

Complex ** SimplicialModuleMap := SimplicialModuleMap => (C,f) -> (id_C**f)
SimplicialModuleMap ** Complex := SimplicialModuleMap => (f,C) -> (f**id_C)



kernel SimplicialModuleMap := SimplicialModule => opts -> f -> (
    -- f : B --> C
    local result;
    B := source f;
    modules := hashTable for i from 0 to B.topDegree list i => kernel f_i;
    inducedMaps := hashTable for i from 0 to B.topDegree list i => inducedMap(B_i, modules#i);
    facemaps := applyPairs(B.dd.map, (i, v) -> (
	    b1 := v * inducedMaps#(i_0);
	    b2 := map(target b1, source inducedMaps#(i_0-1), inducedMaps#(i_0-1));
	    (i, b1 // b2)
	    ));
    if B.?ss then (
	degenmaps := applyPairs(B.ss.map, (i, v) -> (
		b1 := v * inducedMaps#(i_0);
		b2 := map(target b1, source inducedMaps#(i_0+1), inducedMaps#(i_0+1));
		(i, b1 // b2)
		));
	result =  simplicialModule(modules,facemaps,degenmaps,B.topDegree);
	result.cache.kernel = f;
	return result;
	);
    result = simplicialModule(modules,facemaps,B.topDegree);
    result.cache.kernel = f;
    result
    )
cokernel SimplicialModuleMap := SimplicialModule => f -> (
    -- f : B --> C
    local result;
    C := target f;
    deg := degree f;
    modules := hashTable for i from 0 to C.topDegree list i => cokernel f_(i-deg);
    facemaps := applyPairs(C.dd.map, (i, v) -> (i, map(modules#(i_0-1), modules#(i_0), matrix v)));
    if C.?ss then (
	degenmaps := applyPairs(C.ss.map, (i, v) -> (i, map(modules#(i_0+1), modules#(i_0), matrix v)));
	    result =  simplicialModule(modules,facemaps,degenmaps,C.topDegree);
	    result.cache.cokernel = f;
	    return result;
	    );
    result = simplicialModule(modules,facemaps,C.topDegree);
    result.cache.cokernel = f;
    result
    )

image SimplicialModuleMap := SimplicialModule => f -> (
    -- f : B --> C
    local result;
    B := source f;
    C := target f;
    deg := degree f;
    modules := hashTable for i from 0 to C.topDegree list i => image f_(i-deg);
    inducedMaps := hashTable for i from 0 to B.topDegree list i => inducedMap(C_i, modules#i);
    facemaps := applyPairs(C.dd.map, (i, v) -> (
	    b1 := v * inducedMaps#(i_0);
	    b2 := map(target b1, source inducedMaps#(i_0-1), inducedMaps#(i_0-1));
	    (i, b1 // b2)
	    ));
    if C.?ss then (
	degenmaps := applyPairs(C.ss.map, (i, v) -> (
		b1 := v * inducedMaps#(i_0);
		b2 := map(target b1, source inducedMaps#(i_0+1), inducedMaps#(i_0+1));
		(i, b1 // b2)
		));
	result =  simplicialModule(modules,facemaps,degenmaps,C.topDegree);
	result.cache.image = f;
	return result;
	);
    result = simplicialModule(modules,facemaps,C.topDegree);
    result.cache.image = f;
    result
    )

coimage SimplicialModuleMap := SimplicialModule => f -> (
    -- f : B --> C
    local result;
    B := source f;
    modules := hashTable for i from 0 to B.topDegree list i => coimage f_(i);
    facemaps := applyPairs(B.dd.map, (i, v) -> (i, map(modules#(i_0-1), modules#(i_0), matrix v)));
    if B.?ss then (
	degenmaps := applyPairs(B.ss.map, (i, v) -> (i, map(modules#(i_0+1), modules#(i_0), matrix v)));
	    result =  simplicialModule(modules,facemaps,degenmaps,B.topDegree);
	    result.cache.coimage = f;
	    return result;
	    );
    result = simplicialModule(modules,facemaps,B.topDegree);
    result.cache.coimage = f;
    result
    )

--this function takes the tensor product of a direct sum, but caches the components
--of the resulting tensor product based on the component indices of the original modules.
--useful for accessing induced maps on components of tensor products of direct sums
tensorwithComponents = method();
tensorwithComponents(Module,Module) := (M,N) -> (
    T := if M.cache.?indexComponents then flatten table(keys (M.cache.indexComponents),toList(0..length components N-1),(u,v) -> {u,v})
    else  flatten table(toList(0..length components M-1),toList(0..length components N-1),(u,v) -> {u,v});
    result := M**N;
    if M.cache.?indexComponents then result.cache.components = apply(T,i->(M.cache.components#(M.cache.indexComponents#(i_0)))**(N.cache.components#(i_1)))
    else result.cache.components =  apply(T,i->((components M)#(i_0))**((components N)#(i_1)));
    result.cache.indexComponents = hashTable for i to length(T)-1 list flatten (T_i) => i;
    result.cache.indices = for i to length(T)-1 list flatten (T_i);
    result
    )

tensorwithComponents(Matrix,Matrix) := (A,B) -> (
    srcA := source A;
    srcB := source B;
    trgA := target A;
    trgB := target B;
    map(tensorwithComponents(trgA,trgB),tensorwithComponents(srcA,srcB),A**B)
    )

tensorwithComponents List := L -> (
    if length L == 2 then return tensorwithComponents(L_0,L_1);
    Ln := for i from 2 to length L-1 list L_i;
    tensorwithComponents({tensorwithComponents(L_0,L_1)}|Ln)
    )


homology(SimplicialModule) := SimplicialModule => opts -> C -> (
    t := topDegree C;
    simplicialModule( HH normalize C, t, Degeneracy => C.?ss)
    )

homology(SimplicialModuleMap) := SimplicialModuleMap => opts -> f -> (
    t := topDegree f;
    degens := (source f).?ss and (target f).?ss;
    simplicialModule( HH normalize f, t, Degeneracy => degens)
    )


SimplicialModule Array := (C, L) -> (t := topDegree C;
    simplicialModule( (normalize C) L, t, Degeneracy => C.?ss )
    )

SimplicialModuleMap Array := (f, L) -> (t := topDegree f;
    simplicialModule( (normalize f) L, t, Degeneracy => (source f).?ss)
    )

minimalPresentation SimplicialModule :=
prune SimplicialModule := SimplicialModule => opts -> (cacheValue symbol minimalPresentation)(C -> (
	if C.?complex then C = forgetComplex C;
	R := ring C;
    -- opts is ignored here
    -- to be cached: in the input C: cache the result D
    --               in the result: cache pruningMap: D --> C
    faceKeys := keys C.dd.map;
    if C.?ss then degenKeys := keys C.ss.map; 
    prunedMods := new MutableHashTable from for i to C.topDegree list i => prune C_i;
    prunedFaceMaps := new MutableHashTable from for i in faceKeys list i => map(prune C_(i_0-1),prune C_(i_0),0);
    if C.?ss then prunedDegenMaps := new MutableHashTable from for i in degenKeys list i=>map(prune C_(i_0+1),prune C_(i_0),0); 
    nonzeros := select(0..C.topDegree, i -> minimalPresentation C_i != 0);
    D := if #nonzeros === 0 
         then (
             simplicialModule((ring C)^0,C.topDegree)
             )
         else (
             lo := min nonzeros;
             hi := max nonzeros;
	     for i from lo to hi do prunedMods#i = minimalPresentation(C_i);
             for i in select(faceKeys,i->(i_0>=lo and i_0<=hi)) do prunedFaceMaps#i = minimalPresentation C.dd_i;
	     if C.?ss then (
		 for i in select(degenKeys,i->(i_0>=lo and i_0<hi)) do prunedDegenMaps#i = minimalPresentation C.ss_i;
		 );
             nmMods := new HashTable from prunedMods;
	     nmFaces := new HashTable from prunedFaceMaps;
	     if C.?ss then (
		 nmDegens := new HashTable from prunedDegenMaps;
		 );
	     if C.?ss then simplicialModule(nmMods,nmFaces,nmDegens,C.topDegree) else simplicialModule(nmMods,nmFaces,C.topDegree)
                 );
    -- create the isomorphism D --> C
    pruning := hashTable for i from 0 to C.topDegree list i => (minimalPresentation C_i).cache.pruningMap;
    D.cache.pruningMap = map(C,D,pruning);
    D.cache.pruningMap.cache.isCommutative = true;
    D
    ))


minimalPresentation SimplicialModuleMap := 
prune SimplicialModuleMap := SimplicialModuleMap => opts -> f -> (
    if all(keys f.map, i -> instance(i, Sequence)) then return (if degree f == -1 then (prune source f).dd else (prune source f).ss);
    C := source f;
    if not C.cache.?pruningMap then f = f * (minimalPresentation C).cache.pruningMap;
    D := target f;
    if not D.cache.?pruningMap then f = (minimalPresentation D).cache.pruningMap^-1 * f;
    f
    )

inducedMap(SimplicialModule, SimplicialModule) := SimplicialModuleMap => opts -> (D,C) -> (
    -- compute f : C --> D the map induced by the identity matrix.
    deg := if opts.Degree === null then 0 else opts.Degree;
    (loC,hiC) := (0,C.topDegree);
    (loD,hiD) := (0,D.topDegree);
    maps := hashTable for i from max(loC,loD-deg) to min(hiC,hiD-deg) list i => inducedMap(D_(i+deg),C_i, Verify => opts.Verify);
    map(D,C,maps,Degree=>deg)
    )


randomSimplicialMap = method(Options=>{
        Degree => 0,
        InternalDegree => null,
        Cycle => false,
        Boundary => false
        })
randomSimplicialMap(SimplicialModule, SimplicialModule) := SimplicialModuleMap => opts -> (C,D) -> (
    simplicialModule(randomComplexMap(normalize C, normalize D, opts), min(C.topDegree, D.topDegree), Degeneracy => C.?ss)
    )

basis(List, Complex) := Complex => opts -> (deg, C) -> (
          (a,b) := concentration C;
          L := for i from a+1 to b list basis(deg, C.dd_i, opts);
          complex(L, Base => a))

basis(ZZ, Complex) := Complex => opts -> (deg, C) -> (
          (a,b) := concentration C;
          L := for i from a+1 to b list basis(deg, C.dd_i, opts);
          complex(L, Base => a))

basis(List, SimplicialModule) := SimplicialModule => opts -> (L, S) -> (
    if S.?complex then return simplicialModule(basis(L,S.complex), S.topDegree, Degeneracy => S.?ss);
    mods := applyValues(S.module, m -> image basis(L, m, opts));
    H1 := applyValues(S.dd.map, f -> basis(L, f, opts));
    if S.?ss then H2 := applyValues(S.ss.map, f -> basis(L, f, opts));
    if S.?ss then return simplicialModule(mods, H1, H2, S.topDegree);
    simplicialModule(mods, H1, S.topDegree)
    )

basis(ZZ, SimplicialModule) := SimplicialModule => opts -> (L, S) -> (
    if S.?complex then return simplicialModule(basis(L,S.complex), S.topDegree, Degeneracy => S.?ss);
    mods := applyValues(S.module, m -> image basis(L, m, opts));
    H1 := applyValues(S.dd.map, f -> basis(L, f, opts));
    if S.?ss then H2 := applyValues(S.ss.map, f -> basis(L, f, opts));
    if S.?ss then return simplicialModule(mods, H1, H2, S.topDegree);
    simplicialModule(mods, H1, S.topDegree)
    )

basis(List, SimplicialModuleMap) := SimplicialModuleMap => opts -> (L, phi) -> (
    map(basis(L, target phi, opts), basis(L, source phi, opts), hashTable for i in keys phi.map list i => (
	    if (f := basis(L, phi_i, opts)) == 0 then continue else f) )
    )

basis(ZZ, SimplicialModuleMap) := SimplicialModuleMap => opts -> (L, phi) -> (
    map(basis(L, target phi, opts), basis(L, source phi, opts), hashTable for i in keys phi.map list i => (
	    if (f := basis(L, phi_i, opts)) == 0 then continue else f) )
    )


truncate(List, SimplicialModule) := SimplicialModule => {} >> opts -> (L, S) -> (
    if S.?complex then return simplicialModule(truncate(L, S.complex, opts), S.topDegree, Degeneracy => S.?ss);
    mods := applyValues(S.module, m -> truncate(L, m, opts));
    H1 := applyValues(S.dd.map, f -> truncate(L, f, opts));
    if S.?ss then H2 := applyValues(S.ss.map, f -> truncate(L, f, opts));
    if S.?ss then return simplicialModule(mods, H1, H2, S.topDegree);
    simplicialModule(mods, H1, S.topDegree)
    )
truncate(ZZ, SimplicialModule) := SimplicialModule => {} >> opts -> (e, S) -> truncate({e}, S)

truncate(List, SimplicialModuleMap) := SimplicialModuleMap => {} >> opts -> (L, phi) -> (
    map(truncate(L, target phi, opts), truncate(L, source phi, opts), hashTable for i in keys phi.map list i => (
	    if (f := truncate(L, phi_i, opts)) == 0 then continue else f) )
    )

truncate(ZZ, SimplicialModuleMap) := SimplicialModuleMap => {} >> opts -> (L, phi) -> truncate({L}, phi, opts)




isShortExactSequence(SimplicialModuleMap, SimplicialModuleMap) := Boolean => (g, f) -> (
    -- f : A --> B, g : B --> C
    -- the SES is 0 --> A --> B --> C --> 0.
    isWellDefined g and
    isWellDefined f and
    isSimplicialMorphism g and
    isSimplicialMorphism f and
    g*f == 0 and
    -- check exactness degree by degree (image and kernel may have
    -- different generator representations, so comparing as simplicial
    -- modules can fail even when the underlying submodules agree)
    all(0..topDegree(source g), i -> image f_(i - degree f) == kernel g_i) and
    kernel f == 0 and
    coker g == 0
    )  
