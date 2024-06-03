export {
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Basic constructors for complexes of sheaves
-----------------------------------------------------------------------------

-- After this, complexes may be defined with sheaf maps
importFrom_Complexes { "isMorphism", "isAbelianCategory" }
isMorphism SheafMap := isAbelianCategory CoherentSheaf := x -> true

complex CoherentSheaf := Complex => lookup(complex, Module)

-----------------------------------------------------------------------------
-- Specialized methods for complexes of sheaves
-----------------------------------------------------------------------------

sheaf Complex := Complex => C -> (
    (lo,hi) := concentration C;
    complex for i from lo+1 to hi list sheaf C.dd_i
    )

Complex(ZZ) := Complex Sequence := Complex => (C,a) -> (
    (loC, hiC) := concentration C;
    tC := complex for i from loC+1 to hiC list (C.dd_i)(a);
    if C.?sheafOf then tC.sheafOf = C.sheafOf**(ring C)^{a};
    tC
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
		    m))));
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

-- TODO: turn this into a functor
RHom = method()
RHom(Complex, Complex) := Complex => (C, D) -> (
    (loC, hiC) := C.concentration;
    if not instance(variety C_loC, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := moduleComplex C;
    N := flattenComplex moduleComplex D;
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
    cxToField basis(0, Hom(res M, N))
    )

RHom(CoherentSheaf, Complex) := Complex => (C, D) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module C;
    N := flattenComplex moduleComplex D;
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
    cxToField basis(0, Hom(freeResolution M, N))
    )

RHom(Complex, CoherentSheaf) := Complex => (C,D) -> (
    (loC, hiC) := C.concentration;
    if not instance(variety C_loC, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := moduleComplex C;
    N := flattenModule module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    l := dim N;
    Resns := res N;
    p := length Resns;
    n := dim ring N - 1;
    if p >= n - l then (
	a := max apply(max(n - l,0) .. p, j-> ((max degrees (Resns_j))#0 - j));
	r := a - l + 1;
	M = truncate(r, M));
    cxToField basis(0, Hom(res M, N))
    )

RHom(CoherentSheaf, CoherentSheaf) := Complex => (C,D) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module C;
    N := flattenModule module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    l := dim N;
    Resns := res N;
    p := length Resns;
    n := dim ring N - 1;
    if p >= n - l then (
	a := max apply(max(n - l,0) .. p, j-> ((max degrees (Resns_j))#0 - j));
	r := a - l + 1;
	M = truncate(r, M));
    cxToField basis(0, Hom(freeResolution M, N))
    )

--this version of RHom computes the complex for all twists above a certain point
RHom(Complex, Complex, ZZ) := Complex => (C, D, d) -> (
    (loC, hiC) := C.concentration;
    if not instance(variety C_loC, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := moduleComplex C;
    N := flattenComplex moduleComplex D;
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

RHom(CoherentSheaf, Complex, ZZ) := Complex => (C, D, d) -> (
if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module C;
    N := flattenComplex moduleComplex D;
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
    truncate(d, Hom(freeResolution M, N))
    )

RHom(Complex, CoherentSheaf, ZZ) := Complex => (C,D,d) -> (
    (loC, hiC) := C.concentration;
    if not instance(variety C_loC, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := moduleComplex C;
    N := flattenModule module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    l := dim N;
    Resns := res N;
    p := length Resns;
    n := dim ring N - 1;
    if p >= n - l then (
	a := max apply(max(n - l,0) .. p, j-> ((max degrees (Resns_j))#0 - j));
	r := a - l - d + 1;
	M = truncate(r, M));
    truncate(d, Hom(res M, N))
    )

RHom(CoherentSheaf, CoherentSheaf, ZZ) := Complex => (C,D,d) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module C;
    N := flattenModule module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    l := dim N;
    Resns := res N;
    p := length Resns;
    n := dim ring N - 1;
    if p >= n - l then (
	a := max apply(max(n - l,0) .. p, j-> ((max degrees (Resns_j))#0 - j));
	r := a - l - d + 1;
	M = truncate(r, M));
    truncate(d, Hom(freeResolution M, N))
    )


Ext(ZZ, CoherentSheaf, Complex) := Complex => opts -> (m, C, D) -> (
    if not instance(variety C, ProjectiveVariety)
    then error "expected sheaves on a projective variety";
    M := module C;
    N := moduleComplex D;
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
    (loD, hiD) := D.concentration;
    complex for i from loD+1 to hiD list moveToField basis(0, Ext^m(M, matrix D.dd_i))
    )

cohomology(ZZ, Complex) := Complex => (p,C) -> (
    (loC, hiC) := concentration C;
    X := variety C_loC;
    if not C.cache.?HH then C.cache.HH = new MutableHashTable;
    if C.cache.HH#?p   then return C.cache.HH#p;
    C.cache.HH#p = Ext^p(OO_X^1, C)
    )


moduleComplex = method()
moduleComplex(Complex) := Complex => C -> (
    (lo, hi) := concentration C;
    maxTruncDeg := max for i from lo+1 to hi list degree C.dd_i;
    complex(for i from lo+1 to hi list truncate(maxTruncDeg, matrix C.dd_i), Base => lo)
    )

sheafRes = method()
sheafRes(Complex) := Complex => C -> (
    sheaf res moduleComplex C
    )

sheafRes(CoherentSheaf) := Complex => F -> (
    sheaf freeResolution module F
    )

    


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
