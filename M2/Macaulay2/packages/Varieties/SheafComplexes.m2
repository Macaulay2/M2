export {
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

clearHom = (M, N) -> (
    H := youngest(M.cache.cache, N.cache.cache);
    apply(keys H, k -> remove(H, k)))

freeResolution' = X -> freeResolution(X, LengthLimit => 3)

-----------------------------------------------------------------------------
-- Basic constructors for complexes of sheaves
-----------------------------------------------------------------------------

-- After this, complexes may be defined with sheaf maps
importFrom_Complexes { "isMorphism", "isAbelianCategory" }
isMorphism SheafMap := isAbelianCategory CoherentSheaf := x -> true

complex CoherentSheaf := Complex => lookup(complex, Module)

-----------------------------------------------------------------------------
-- Basic operations between sheaves, complexes, etc.
-----------------------------------------------------------------------------

tensor(CoherentSheaf, Complex) := Complex => {} >> opts -> (F, C) -> (
    if not isSheafComplex C then error "expected to tensor with a complex of sheaves";
    (lo, hi) := concentration C;
    if lo === hi
    then complex(tensor(F, C_lo, opts), Base => lo)
    else complex applyValues(C.dd.map, f -> tensor(id_F, f, opts)))

tensor(Complex, CoherentSheaf) := Complex => {} >> opts -> (C, F) -> tensor(F, C, opts)

CoherentSheaf ** Complex := Complex => {} >> opts -> (F, C) -> tensor(F, C, opts)
Complex ** CoherentSheaf := Complex => {} >> opts -> (C, F) -> tensor(C, F, opts)

-----------------------------------------------------------------------------
-- Specialized methods for complexes of sheaves
-----------------------------------------------------------------------------

isSheafComplex = C -> instance(C_(C.concentration#0), CoherentSheaf)
variety Complex := C -> variety C_(C.concentration#0)

sheaf Complex := Complex => C -> C.cache.sheaf ??= (
    if isSheafComplex C then return C;
    (lo, hi) := C.concentration;
    if lo === hi then return complex(sheaf C_lo, Base => lo);
    D := complex applyValues(C.dd.map, sheaf);
    D.cache.module = C;
    D)

sheaf ComplexMap := ComplexMap => phi -> phi.cache.sheaf ??= (
    S := source phi;
    T := target phi;
    if isSheafComplex S and isSheafComplex T then return phi;
    sphi := map(sheaf T, sheaf S, applyValues(phi.map, sheaf));
    sphi.cache.module = phi;
    sphi)

module Complex := Complex => D -> D.cache.module ??= (
    if not isSheafComplex D then return D;
    (lo, hi) := D.concentration;
    if lo === hi then return complex(module D_lo, Base => lo);
    maxTruncDeg := max apply(values D.dd.map, f -> f.degree);
    C := complex applyValues(D.dd.map, f -> truncate(maxTruncDeg, f.map));
    C.cache.sheaf = D;
    C)

module ComplexMap := ComplexMap => phi -> phi.cache.module ??= (
    S := source phi;
    T := target phi;
    if not isSheafComplex S or not isSheafComplex T then return phi;
    maxTruncDeg := max ( apply(values S.dd.map, f -> f.degree) | apply(values T.dd.map, f -> f.degree) );
    sphi := map(truncate(maxTruncDeg,module T), truncate(maxTruncDeg,module S), applyValues(phi.map, i -> truncate(maxTruncDeg, matrix i)));
    sphi.cache.sheaf = phi;
    sphi)

-- TODO: move to Complexes
freeResolution Complex := Complex => opts -> C -> resolution(C, opts)

sheafRes = method(Options => options freeResolution)
sheafRes Complex       :=
sheafRes CoherentSheaf := Complex => opts -> F -> sheaf freeResolution'(module F, opts)

Complex(ZZ) := Complex(Sequence) := Complex => (C, a) -> complex applyValues(C.dd.map, f -> f(a))

sheafHom(Complex, Complex) := Complex => opts -> (C,D) -> (
    -- signs here are based from Christensen and Foxby
    -- which agrees with Conrad (Grothendieck duality book)
    Y := youngest(C,D);
    if Y.cache#?(sheafHom,C,D) then return Y.cache#(sheafHom,C,D);
    if ring C =!= ring D then error "expected complexes over the same ring";
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
		    m))));
    -- TODO: switch to complex applyValues
    result = complex maps;
    result.cache.homomorphism = (C,D); -- source first, then target
    Y.cache#(sheafHom,C,D) = result;
    result
    )



sheafHom(CoherentSheaf, Complex) := Complex => opts -> (M,C) -> sheafHom(complex M, C, opts)
sheafHom(Complex, CoherentSheaf) := Complex => opts -> (C,M) -> sheafHom(C, complex M, opts)
sheafHom(Complex, SheafOfRings) := Complex => opts -> (C,R) -> sheafHom(C, complex R, opts)
sheafHom(SheafOfRings, Complex) := Complex => opts -> (R,C) -> sheafHom(complex R, C, opts)

sheafDual = method()
sheafDual Complex := Complex => C -> sheafHom(C, ring C)

-----------------------------------------------------------------------------
-- RHom and Ext
-----------------------------------------------------------------------------

-- TODO: turn this into a functor
RHom = method()
RHom(CoherentSheaf, CoherentSheaf) :=
RHom(CoherentSheaf, Complex) :=
RHom(Complex, CoherentSheaf) := Complex => (C, D) -> RHom(complex C, complex D)
RHom(Complex,       Complex) := Complex => (C, D) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := flattenComplex module C;
    N := flattenComplex module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    H := prune HH N;
    (loH, hiH) := concentration H;
    L := for i from loH to hiH list dim H_i;
    l := max L;
    Resns := for i from loH to hiH list resolution flattenModule H_i;
    P := for i in Resns list length i;
    p := max P;
    n := dim ring (H_loH) - 1;
    if p >= n - l then (
	a := max for i from 0 to length(Resns)-1 list max apply(n - L_i .. P_i, j-> (max degrees (Resns_i)_j)#0 - j);
	r := a - l + 1;
	M = truncate(r, M));
    part(0, Hom(res M, N, DegreeLimit => 0))
    )

--this version of RHom computes the complex for all twists above a certain point
RHom(CoherentSheaf, CoherentSheaf, ZZ) :=
RHom(CoherentSheaf, Complex,       ZZ) :=
RHom(Complex, CoherentSheaf, ZZ) := Complex => (C, D, d) -> RHom(complex C, complex D, d)
RHom(Complex, Complex,       ZZ) := Complex => (C, D, d) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := flattenComplex module C;
    N := flattenComplex module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    H := prune HH N;
    (loH, hiH) := concentration H;
    L := for i from loH to hiH list dim H_i;
    l := max L;
    Resns := for i from loH to hiH list resolution flattenModule H_i;
    P := for i in Resns list length i;
    p := max P;
    n := dim ring (H_loH) - 1;
    if p >= n - l then (
	a := max for i from 0 to length(Resns)-1 list max apply(n - L_i .. P_i, j-> (max degrees (Resns_i)_j)#0 - j);
	r := a - l - d + 1;
	M = truncate(r, M));
    truncate(d, Hom(res M, N))
    )

Ext(ZZ, SheafOfRings,  Complex) := Complex => opts -> (m, O, D) -> Ext(m, O^1, D, opts)
Ext(ZZ, CoherentSheaf, Complex) := Complex => opts -> (m, C, D) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := flattenModule module C;
    N := flattenComplex module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    H := prune HH N;
    (loH, hiH) := concentration H;
    L := for i from loH to hiH list min(dim H_i,m);
    l := max L;
    Resns := for i from loH to hiH list resolution flattenModule H_i;
    P := for i in Resns list length i;
    p := max P;
    n := dim ring (H_loH) - 1;
    if p >= n - l then (
	a := max for i from 0 to length(Resns)-1 list max apply(n - L_i .. P_i, j-> (max degrees (Resns_i)_j)#0 - j);
	r := a - l + 1;
	M = truncate(r, M));
    complex applyValues(D.dd.map, f -> part(0, Ext^m(M, matrix f, opts))))

cohomology(ZZ, ProjectiveVariety, Complex) := Complex => opts -> (p, X, C) -> (
    C.cache.cohomology   ??= new MutableHashTable;
    C.cache.cohomology#p ??= Ext^p(sheaf X, C, opts))

-----------------------------------------------------------------------------

euler Complex := C -> sum(pairs C.module, (i, M) -> (-1)^i * euler M)

-----------------------------------------------------------------------------

end--

restart
loadPackage("Truncations", FileName => currentDirectory() | "Truncations.m2", Reload => true)
loadPackage("Complexes",   FileName => currentDirectory() | "Complexes.m2",   Reload => true)
debug loadPackage("Varieties",   FileName => currentDirectory() | "Varieties.m2",   Reload => true)
installPackage("Varieties",   FileName => currentDirectory() | "Varieties.m2")

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
