complex' = method(Options => {Base=>0})
complex' HashTable := Complex => opts -> maps -> (
    spots := sort keys maps;
    if #spots === 0 then
      error "expected at least one matrix";
    if not all(spots, k -> instance(k,ZZ)) then
      error "expected matrices to be labelled by integers";
    if not all(spots, k -> instance(maps#k, SheafMap)) then
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
complex' List := Complex => opts -> L -> (
    -- L is a list of matrices or a list of modules
    if not instance(opts.Base, ZZ) then
      error "expected Base to be an integer";
    if all(L, ell -> instance(ell, SheafMap)) then (
        mapHash := hashTable for i from 0 to #L-1 list opts.Base+i+1 => L#i;
        return complex'(mapHash, opts)
        );
    -- if all(L, ell -> instance(ell,Module)) then (
    --     R := ring L#0;
    --     if any(L, ell -> ring ell =!= R) then
    --         error "expected modules all over the same ring";
    --     moduleHash := hashTable for i from 0 to #L-1 list opts.Base + i => L#i;
    --     C := new Complex from {
    --         symbol ring => R,
    --         symbol concentration => (opts.Base, opts.Base + #L - 1),
    --         symbol module => moduleHash,
    --         symbol cache => new CacheTable
    --         };
    --     C.dd = map(C,C,0,Degree=>-1);
    --     return C;
    --     );
    error "expected a list of matrices or a list of modules";
    )

-*
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
*-

end--

restart
debug Varieties

  S = QQ[x,y]
  X = Proj S
  d = 1
  F = OO_X(2)
  G = OO_X(0)
  E = Ext^d(F, G)
  f = E_{0}
  C = complex' yonedaSheafExtension f

Complex _ ZZ := (C,i) -> if C.module#?i then C.module#i else OO_(variety C)^0 -- (ring C)^0
variety Complex := Variety => C -> variety C_0

  HH C
  

  assert(source i === G)
  assert(target i === source p)
  assert(target p == F) -- FIXME
  assert(prune p === map(OO_X^1(2),OO_X^2(1), map(S^{2}, , {{x, -y}})))
  assert(prune i === map(OO_X^2(1),OO_X^1, map(S^{2:1}, , {{y}, {-x}})))
  assert(coker i == F)
  assert(image i == ker p)
  assert(ker p == G)
  assert(0 == p * i)
  assert(0 == homology(p, i))
  -- FIXME: somehow the generators are changed
  -- assert(0 == homology(prune \ (p, i)))
  assert(0 == ker i)
  assert(0 == coker p)

  --
  S = QQ[x,y,z]
  X = Proj S
  d = 1
  F = tangentSheaf X
  G = OO_X^1
  E = Ext^d(F, G)
  f = E_{0}
  -- 0 <-- T_X <-- O_X(1)^3 <-- O_X <-- 0
  (p, i) = toSequence yonedaSheafExtension f
  assert(source i === G)
  assert(target i === source p)
  assert(source p == OO_X^{3:1})
  assert(target p === F)
  assert(0 == p * i)
  assert(0 == homology(p, i))
  assert(0 == homology(prune \ (p, i)))
  assert(0 == ker i)
  assert(0 == coker p)
