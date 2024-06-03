-- override the complex to work with sheaves, already defined in Complexes
addHook((complex, HashTable), Strategy => "sheaves", (opts, maps) -> (
    spots := sort keys maps;
    if #spots === 0 then
      error "expected at least one matrix";
    if not all(spots, k -> instance(k,ZZ)) then
      error "expected matrices to be labelled by integers";
    if not all(spots, k -> instance(maps#k, SheafMap)) then return null;
    --
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
    ))
addHook((complex, List), Strategy => "sheaves", (opts, L) -> (
    -- L is a list of sheaf maps or a list of coherent sheaves
    if not instance(opts.Base, ZZ) then
      error "expected Base to be an integer";
    if not all(L, ell -> instance(ell, SheafMap)) then return null;
    mapHash := hashTable for i from 0 to #L-1 list opts.Base+i+1 => L#i;
    complex(mapHash, opts)))

--addHook((symbol _, Complex, ZZ), Strategy => "sheaves",  (C,i) -> (
--    (lo,hi) := concentration C;
--    if not instance(C_lo,CoherentSheaf) then return null;
--    if C.module#?i then C.module#i else sheaf (ring C)^0)
--)

addHook((tensor, Complex, Complex), Strategy => "sheaves", (opts, C, D) -> (
      Y := youngest(C,D);
      if Y.cache#?(tensor,C,D) then return Y.cache#(tensor,C,D);
      R := ring C;
      if ring D =!= R then error "expected complexes over the same ring";
      (loC,hiC) := C.concentration;
      if not instance(C_loC,CoherentSheaf) then return null;
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
                      matrix m
                      ))));
      result = complex maps;
      result.cache.tensor = (C,D);
      Y.cache#(tensor,C,D) = result;
      result
      ))

sheaf Complex := Complex => C -> (
    (lo,hi) := concentration C;
    complex for i from lo+1 to hi list sheaf C.dd_i
    )

sheafHom(Complex, Complex) := Complex => opts -> (C,D) -> (
    -- signs here are based from Christensen and Foxby
    -- which agrees with Conrad (Grothendieck duality book)
    Y := youngest(C,D);
    if Y.cache#?(sheafHom,C,D) then return Y.cache#(sheafHom,C,D);
    R := ring C;
    if ring D =!= R then error "expected complexes over the same ring";
    (loC,hiC) := C.concentration;
    (loD,hiD) := D.concentration;
    modules := hashTable for i from loD-hiC to hiD-loC list i => (
        directSum for j from loC to hiC list {j,j+i} => sheafHom(C_j, D_(j+i), opts)
        );
    if loC === hiC and loD === hiD then (
        result := complex(modules#(loD-hiC), Base => loD-loC);
        result.cache.homomorphism = (C,D); -- source first, then target        
        Y.cache#(sheafHom,C,D) = result;
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
                    m := map(tar, src, 
                        if k-j === {0,1} then (-1)^(k#1-k#0+1) * sheafHom(C_(k#0), dd^D_(k#1), opts)
                        else if k-j === { -1,0 } then sheafHom(dd^C_(j#0), D_(k#1), opts)
                        else 0);
		    if instance(m, Matrix) then m else matrix m
                    ))));
    result = complex maps;
    result.cache.homomorphism = (C,D); -- source first, then target
    Y.cache#(sheafHom,C,D) = result;
    result
    )



sheafHom(CoherentSheaf, Complex) := Complex => opts -> (M,C) -> sheafHom(complex M, C, opts)
sheafHom(Complex, CoherentSheaf) := Complex => opts -> (C,M) -> sheafHom(C, complex M, opts)
sheafHom(Complex, SheafOfRings) := Complex => opts -> (C,R) -> sheafHom(C, complex R, opts)
sheafHom(SheafOfRings, Complex) := Complex => opts -> (R,C) -> sheafHom(complex R, C, opts)

sheafDual = method();
sheafDual Complex := Complex => (C) -> sheafHom(C, sheaf (ring C)^1)

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
  C = complex yonedaSheafExtension f

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
