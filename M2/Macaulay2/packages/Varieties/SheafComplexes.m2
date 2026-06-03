export {
    "RHom",
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- pushforward the complex to PP^n via S/I <-- S
-- TODO: move to Complexes?
flattenComplex = C -> C.cache#"flattenComplex" ??= (
    if instance(ring C, PolynomialRing) then return C;
    (lo, hi) := C.concentration;
    if lo === hi
    then complex(flattenModule C_lo, Base => lo)
    else complex applyValues(C.dd.map, flattenMorphism))

clearHom = (M, N) -> (
    H := youngest(M.cache.cache, N.cache.cache);
    apply(keys H, k -> remove(H, k)))

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

sheaf          Complex  := Complex =>     C  -> sheaf(variety ring C, C)
sheaf(Variety, Complex) := Complex => (X, C) -> C.cache.sheaf ??= (
    if isSheafComplex C then return C;
    (lo, hi) := C.concentration;
    if lo === hi then return complex(sheaf_X C_lo, Base => lo);
    D := complex applyValues(C.dd.map, sheaf_X);
    D.cache.module = C;
    D)

sheaf          ComplexMap  := ComplexMap =>     phi  -> sheaf(variety ring phi, phi)
sheaf(Variety, ComplexMap) := ComplexMap => (X, phi) -> phi.cache.sheaf ??= (
    S := source phi;
    T := target phi;
    if isSheafComplex S and isSheafComplex T then return phi;
    sphi := map(sheaf_X T, sheaf_X S, applyValues(phi.map, sheaf_X));
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

-- see Complexes/ChainComplexMap.m2
extend(Complex, Complex, SheafMap) := ComplexMap =>
    lookup(extend, Complex, Complex, Matrix)
extend(Complex, Complex, SheafMap, Sequence) := ComplexMap =>
    lookup(extend, Complex, Complex, Matrix, Sequence)

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
-- naiveCotangentComplex
-----------------------------------------------------------------------------

naiveCotangentComplex = method(TypicalValue => Complex, Options => options exteriorPower)

-- The cotangent complex of a closed substack X of a weighted projective space P over a field k
-- lives in cohomological degrees <= 1. This function computes its truncation to degrees >= 0,
-- which lives in degrees 0 and 1. The Macaulay2 package Complexes is needed.
--
-- The input is a ProjectiveVariety, that is, a closed substack of a weighted projective space.
-- If Y denotes the affine cone over X in A^{n+1}, X is the quotient stack by the multiplicative group G_m,
-- [(Y-0)/G_m].
--
-- The output is a complex of graded modules in cohomological degrees 0 and 1. (Eventually,
-- this should become a complex of sheaves.) It can be viewed as
-- the complex of G_m-equivariant sheaves 0 -> Omega^1_Y -> g^* tensor O_Y -> 0, where g is the Lie algebra of G_m.
-- (The boundary map is given by plugging in the vector field associated to the G_m-action on Y.)
-- The cohomology sheaf of this complex in degree 0 is computed by cotangentSheaf(R).
-- If X is a smooth substack of P (that is, if X is "quasi-smooth"
-- in the coarse moduli space), then naiveCotangentComplex(X) is equivalent (in the derived category of X)
-- to the whole cotangent complex of X.
--
-- If the characteristic is 0 or the characteristic p > 0 does not divide any of the weights,
-- then X is a Deligne-Mumford stack, and so the degree-1 cohomology sheaf of this complex is zero.
-- (In that case, in terms of graded modules, the degree-1 cohomology of this complex is killed by a power
-- of the irrelevant ideal. That is, it is supported at the origin of the cone Y.)
-- In that case, naiveCotangentComplex(X) is equivalent (in the derived category of X)
-- to cotangentSheaf(X). But in general, naiveCotangentComplex(X) should be considered as more natural
-- than its cohomology sheaf in degree 0. Eventually, one might want to consider the full cotangent complex of X,
-- or at least its truncation to degrees >= -1 rather than >= 0.
--
naiveCotangentComplex ProjectiveVariety := opts -> X -> X.cache.naiveCotangentComplex ??= (
    R := assertWeightedZZGraded ring X; -- a graded ring with some positive integer grading
    -- and X = Proj R is the corresponding subspace of a weighted projective space, viewed as a stack.
    S := ring (F := presentation R);
    -- Thus S is a graded polynomial ring, and R is the quotient of S by the ideal generated by the image of the matrix F over S.
    -- For example, S could be something like: S = QQ[x,y,z,w,Degrees=>{1,9,15,22}]. We need the relations F, below.
    degs := flatten degrees S; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj S has dimension n-1.
    M0orig := R^(-degs);
    M1 := R^{0};
    d1 := matrix {apply(gens R, i -> first degree(i)*i)};
    -- Thus d: R^n -> R sends the basis element dx_i to a_i x_i, for 1 <= i <= n.
    d := map(M1, M0orig, d1, Degree => 0); -- This is d viewed as being homogeneous.
    e := jacobian F ** R; -- Thus e: R^r -> R^n sends the jth basis element (corresponding to the jth relation
    -- of f_1,...,f_r) to df_j = sum_{i=1}^n df_j/dx_i dx_i. It is viewed as being homogeneous.
    -- assert(d * e == 0);
    assert(isHomogeneous d);
    assert(isHomogeneous e);
    assert(d * e == 0); -- Delete these assertions when this program has been checked.
    M0 := cokernel e; -- This graded module represents Omega^1_Y, where Y is the affine cone over X, with its G_m-action.
    map0 := map(M1, M0, d1, Degree => 0);
    complex({map0}, Base => -1) -- The complex is in homological degrees 0 and -1, that is, cohomological degrees 0 and 1.
    )

-- The cotangent complex of a closed substack X of a weighted projective space P over a field k
-- lives in cohomological degrees <= 1. This function computes its ith exterior power truncated to degrees >= 0,
-- which lives in degrees from 0 to i. The Macaulay2 package Complexes must be loaded.
--
-- The input X is a ProjectiveVariety, that is, a closed substack of a weighted projective space.
-- If Y denotes the affine cone over X in A^{n+1}, X is the quotient stack by the multiplicative group G_m,
-- [(Y-0)/G_m].
--
-- The output is a complex of graded modules in cohomological degrees from 0 to i. (Eventually,
-- this should become a complex of sheaves.) It can be viewed as
-- the complex of G_m-equivariant sheaves
--      0 -> Omega^i_Y -> Omega^{i-1}_Y tensor g^* -> ... -> O_Y tensor S^i(g^*) -> 0,
-- where g is the Lie algebra of G_m.
-- The cohomology sheaf of this complex in degree 0 is computed by cotangentSheaf(i,R).
-- If X is a smooth substack of P (that is, if X is "quasi-smooth"
-- in the coarse moduli space), then naiveCotangentComplex(X) is equivalent (in the derived category of X)
-- to the whole cotangent complex of X.
--
-- If the characteristic is 0 or the characteristic p > 0 does not divide any of the weights,
-- then X is a Deligne-Mumford stack, and so HH^i of this complex is zero except when i=0.
-- In that case, naiveCotangentComplex(i,X) is equivalent (in the derived category of X)
-- to cotangentSheaf(i,X). But in general, naiveCotangentComplex(i,X) should be considered as more natural
-- than its cohomology sheaf in degree 0. Eventually, one might want to consider exterior powers
-- of the full cotangent complex of X, or at least its truncation to degrees >= -1 rather than >= 0.
--
naiveCotangentComplex(ZZ, ProjectiveVariety) := opts -> (i, X) -> (
    R := ring X;
    -- Here R is a graded ring with some positive integer grading,
    -- and X = Proj R is the corresponding subspace of a weighted projective space, viewed as a stack.
    if i == 0 then (
	return complex R);
    S := ring (F := presentation R);
    -- Thus S is a graded polynomial ring, and R is the quotient of S by the ideal generated by the image of the matrix F over S.
    -- For example, S could be something like: S = QQ[x,y,z,w,Degrees=>{1,9,15,22}]. We need the relations F, below.
    degs := flatten degrees S; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj S has dimension n-1.
    j := 0;
    M0orig := R^(-degs);
    M1 := R^{0};
    d1 := matrix {apply(gens R, i -> first degree(i)*i)};
    -- Thus d: R^n -> R sends the basis element dx_i to a_i x_i, for 1 <= i <= n.
    d := map(M1, M0orig, d1, Degree => 0); -- This is d viewed as being homogeneous.
    e := jacobian F ** R; -- Thus e: R^r -> R^n sends the jth basis element (corresponding to the jth relation
    -- of f_1,...,f_r) to df_j = sum_{i=1}^n df_j/dx_i dx_i. It is viewed as being homogeneous.
    -- assert(d * e == 0);
    assert(isHomogeneous d);
    assert(isHomogeneous e);
    assert(d * e == 0); -- DELETE these assertions when this program has been checked.
    M0 := cokernel e; -- This graded module represents Omega^1_Y, where Y is the affine cone over X, with its G_m-action.
    -- map0 := map(M1, M0, d1, Degree => 0); -- This is the map Omega^1_Y -> O_Y (the case i=1).
    modulelist := apply(i+1, j -> exteriorPower(j, M0, Strategy => opts.Strategy)); -- This lists the modules O_Y, Omega^1_Y, ..., Omega^i_Y
    -- as quotient modules, with the same order of generators used by koszulComplex.
    koszultrunc := koszulComplex(d1, Concentration => (0,i)); -- The Koszul complex of d1,
    -- truncated to: 0 -> R^(n choose i) -> ... -> R^(n choose 0) -> 0, viewed as in homological degrees i,...,0.
    maplist := apply(i, j -> map(modulelist#j, modulelist#(j+1), dd^koszultrunc_(j+1), Degree => 0));
    -- That lists the maps in the complex we want.
    complex(maplist, Base => -i)) -- The complex is in homological degrees 0,-1,...,-i,
-- that is, cohomological degrees 0,1,...,i.

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
