-----------------------------------
-- ComplexMap ------------
-----------------------------------
source ComplexMap := Complex => f -> f.source
target ComplexMap := Complex => f -> f.target
ring ComplexMap := Complex => f -> ring source f
degree ComplexMap := ZZ => f -> f.degree

isHomogeneous ComplexMap := (f) -> all(values f.map, isHomogeneous)

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

expression ComplexMap := Expression => f -> (
    d := degree f;
    s := sort keys f.map;
    if #s === 0 then 
        new ZeroExpression from {0}
    else new VerticalList from for i in s list
        RowExpression {i+d, ":", MapExpression { target f_i, source f_i, f_i }, ":", i}
    )

net ComplexMap := Net => f -> (
     v := between("",
            for i in sort keys f.map list (
                horizontalJoin(
		            net (i+f.degree), " : ", net target f_i, " <--",
		            lineOnTop net f_i,
		            "-- ", net source f_i, " : ", net i
                    )
                ));
     if # v === 0 then net "0"
     else stack v
     )

texMath ComplexMap := String => f -> texMath expression f

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

--------------------------------------------------------------------
-- truncations -----------------------------------------------------
--------------------------------------------------------------------
naiveTruncation(ComplexMap,Sequence,Sequence) := ComplexMap => (f, targetLoHi, sourceLoHi) -> (
    D := naiveTruncation(target f, targetLoHi);
    C := naiveTruncation(source f, sourceLoHi);
    map(D, C, i -> f_i, Degree => degree f)
    )

naiveTruncation(ComplexMap,Sequence) := ComplexMap => (f,loHi) -> naiveTruncation(f, loHi, loHi)

naiveTruncation(ComplexMap,ZZ,ZZ) := 
naiveTruncation(ComplexMap,ZZ,InfiniteNumber) := 
naiveTruncation(ComplexMap,InfiniteNumber,ZZ) := 
naiveTruncation(ComplexMap,InfiniteNumber,InfiniteNumber) :=
naiveTruncation(ComplexMap,ZZ,Nothing) := 
naiveTruncation(ComplexMap,Nothing,ZZ) := ComplexMap => (f,lo,hi) -> naiveTruncation(f, (lo,hi))


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
canonicalTruncation(ComplexMap,ZZ,ZZ) := 
canonicalTruncation(ComplexMap,ZZ,InfiniteNumber) := 
canonicalTruncation(ComplexMap,InfiniteNumber,ZZ) := 
canonicalTruncation(ComplexMap,InfiniteNumber,InfiniteNumber) := 
canonicalTruncation(ComplexMap,ZZ,Nothing) := 
canonicalTruncation(ComplexMap,Nothing,ZZ) := ComplexMap => (f,lo,hi) -> canonicalTruncation(f, (lo,hi))

part(List, ComplexMap) := ComplexMap => (deg, f) -> (
    R := ring f;
    A := coefficientRing R;
    psi := map(A,R, DegreeMap => degR -> take(degR, - degreeLength A));
    C := part(deg, source f);
    D := part(deg, target f);
    d := degree f;
    map(D, C, i -> map(D_(i+d), C_i, psi matrix basis(deg, f_i)), Degree => d)
    )
part(ZZ, ComplexMap) := ComplexMap => (deg, f) -> part({deg}, f)

truncate(List, ComplexMap) := ComplexMap => (e, f) -> (
    C := truncate(e, source f);
    D := truncate(e, target f);
    d := degree f;
    map(D, C, i -> map(D_(i+d), C_i, truncate(e, f_i)), Degree => d)
    )
truncate(ZZ, ComplexMap) := ComplexMap => (e, f) -> truncate({e}, f)

--------------------------------------------------------------------
-- homology --------------------------------------------------------
--------------------------------------------------------------------
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

--------------------------------------------------------------------
-- Hom -------------------------------------------------------------
--------------------------------------------------------------------
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
    -- TODO: remove this line if the next actually works...: g := map(H_d, R^1, matrix(for i from lo to hi list {matrix homomorphism' f_i}));
    g := map(H_d,, matrix(for i from lo to hi list {matrix homomorphism' f_i}));
    map(H, complex source g, hashTable {0 => g}, Degree=>d)
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

--------------------------------------------------------------------
-- tensor products -------------------------------------------------
--------------------------------------------------------------------
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
Ring ** ComplexMap := ComplexMap => (R,f) -> f ** R

RingMap ComplexMap := ComplexMap => (phi,f) ->
    map(phi target f, phi source f, i -> phi f_i)

tensor(RingMap, ComplexMap) := ComplexMap => opts -> (phi, f) -> (
    if source phi =!= ring f then error "expected the source of the ring map to be the ring of the complex map";
    map(tensor(phi, target f), tensor(phi, source f), i -> tensor(phi, matrix f_i))
    )
tensor(ComplexMap, RingMap) := ComplexMap => opts -> (f, phi) -> tensor(phi, f)

RingMap ** ComplexMap := ComplexMap => (phi, f) -> tensor(phi, f)
ComplexMap ** RingMap := ComplexMap => (f, phi) -> tensor(phi, f)

--------------------------------------------------------------------
-- canonical maps --------------------------------------------------
--------------------------------------------------------------------
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


-- TODO: this function needs to be more completely debugged
canonicalMap = method(Options => {UseTarget=>null})
  -- UseTarget is used to disambiguate the two canonical maps
  --   B --> cylinder f
  --   C --> cylinder f
  -- when B == C, f : B --> C.  
  -- UseTarget=>true selects the second of these.
canonicalMap(Complex, Complex) := ComplexMap => opts -> (E,D) -> (
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
    maps := hashTable for i from max(loC,loD-deg) to min(hiC,hiD-deg) list i => inducedMap(D_(i+deg),C_i, Verify => opts.Verify);
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

--------------------------------------------------------------------
-- homotopy --------------------------------------------------------
--------------------------------------------------------------------
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
nullHomotopyFreeSource = f -> (
    -- f:ComplexMap
    -- key assumption: 'source f' is a complex of free modules
    -- result is a ComplexMap h : C --> D, of degree degree(f)+1
    C := source f;
    D := target f;
    deg := degree f + 1;
    hs := new MutableHashTable;
    (lo,hi) := concentration f;
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

isNullHomotopic ComplexMap := Boolean => f -> (
    g := homomorphism' f;
    H := target g; 
    d := degree f;
    g1 := g_d // dd^H_(d+1); 
    g_d == dd^H_(d+1) * g1
    )

nullHomotopy ComplexMap := ComplexMap => f -> (
    -- we check that the source is free, as that can be much faster
    -- TODO: nullHomotopy should perhaps be hook-ified.
    result := if isFree source f then nullHomotopyFreeSource f;
    if result =!= null then return result;
    g := homomorphism' f;
    H := target g; 
    d := degree f;
    g1 := g_d // dd^H_(d+1);
    homomorphism(d+1,g1,H)
    )

--------------------------------------------------------------------
-- six standard maps -----------------------------------------------
--------------------------------------------------------------------
tensorCommutativity(Complex, Complex) := ComplexMap => (C,D) -> (
    -- implement the isomorphism C ** D --> D ** C
    CD := C ** D; 
    DC := D ** C;
    (lo,hi) := concentration CD;
    maps := new HashTable from for i from lo to hi list i => (
       mats := for ba in indices DC_i list
           for ab in indices CD_i list (
              a := ab#0; -- summand C_a ** D_(i-a)
              b := ba#0; -- summand D_b ** C_(i-b)
              -- should be the zero map, unless a+b == i
              if a+b === i then 
                  tensorCommutativity(C_a, D_b)
              else map(
                  DC_i.cache.components#(DC_i.cache.indexComponents#ba),
                  CD_i.cache.components#(CD_i.cache.indexComponents#ab),
                  0)
        );
        matrix mats
        );
    map(DC,CD,maps)
    )

tensorAssociativity(Complex, Complex, Complex) := ComplexMap => (A,B,C) -> (
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

--------------------------------------------------------------------
-- quasi-isomorphisms ----------------------------------------------
--------------------------------------------------------------------
isQuasiIsomorphism = method(Options => {Concentration => (-infinity,infinity)})
-- TODO: check this function for correctness, in the case when Concentration is given
isQuasiIsomorphism ComplexMap := Boolean => opts -> f -> (
    (lof,hif) := concentration f;
    (loO,hiO) := opts.Concentration;
    all(max(lof,loO)..min(hif,hiO),
        i -> HH_(i+1) cone f == 0
        )
    )

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

--------------------------------------------------------------------
-- short exact sequences -------------------------------------------
--------------------------------------------------------------------
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

horseshoeResolution = method(Options => {LengthLimit=>infinity})
horseshoeResolution Complex := Sequence => opts -> ses -> (
    -- check that ses is a short exact sequence of modules
    -- occurring in homological degrees 0,1,2.
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
