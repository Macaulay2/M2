export {
    "RHom",
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- pushforward the complex to PP^n via S/I <-- S
-- TODO: move to Complexes?
liftComplex = C -> C.cache#"liftComplex" ??= (
    if instance(ring C, PolynomialRing) then return C;
    (lo, hi) := C.concentration;
    if lo === hi
    then complex(liftModule C_lo, Base => lo)
    else complex applyValues(C.dd.map, liftMorphism))

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
-- hh: Cohomology with coefficients in a complex of coherent sheaves
-----------------------------------------------------------------------------

SumOfTwistsComplex = new Type of BasicList
SumOfTwistsComplex.synonym = "sum of twists of a complex of sheaves"

-- constructors
Complex(*) := SumOfTwistsComplex => C -> C(>=-infinity)
Complex LowerBound := SumOfTwistsComplex => (C, b) -> (new SumOfTwistsComplex from {C, b})

-- basic methods
ring    SumOfTwistsComplex := C ->    ring C#0
variety SumOfTwistsComplex := C -> variety C#0

-- printing
expression SumOfTwistsComplex := C -> (expression C#0) (if C#1#0 === -infinity then expression symbol(*)
    else (expression symbol>=) (expression C#1#0))
net        SumOfTwistsComplex :=      net @@ expression
texMath    SumOfTwistsComplex :=  texMath @@ expression
toString   SumOfTwistsComplex := toString @@ expression


-- This function hh^i(C) computes coherent sheaf cohomology with coefficients in a complex of sheaves
-- (sometimes called "hypercohomology") on a closed subspace of a projective space,
-- or more generally in a weighted projective space.
-- If you want to compute cohomology with many twists, the functions hh^i(C,b1,b2) or hh^i(C(*))
-- should be faster than running this program repeatedly.
-- The base ring should be a field. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- The input complex can also be a complex of graded R-modules,
-- in which case it is interpreted as the associated complex of coherent sheaves.
--
-- This function computes only the dimension of the cohomology, not the cohomology as a vector space.
-- The algorithm uses local duality. The input complex can also be a complex of graded R-modules,
-- in which case it is interpreted as the associated complex of coherent sheaves.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Twists are interpreted by tensoring with the line bundles O(a) on the stack.
--
hh(ZZ, Complex) := ZZ => opts -> (cohodeg, C) -> (
    R2 := ring module C;
    M := liftComplex module C;
    R1 := ring M; -- R1 is a graded polynomial ring, and M is a complex of graded R1-modules that represents the complex C of sheaves.
    if degreeLength R1 =!= 1 then error "expected degree length 1";
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    R1.cache ??= new MutableHashTable;
    w := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module w, as a graded R1-module.
    -- Our formula for the Hilbert series of H^i(P, M~(*)) (where i = cohodeg) is:
    --   hilb(H^i(M)) - phi(hilb(im(Ext^(n-i)(tau^{>=i}M, w) -> Ext^(n-i)(M, w))))
    --   + phi(hilb(Ext^(n-i-1)(M, w))) - phi(hilb(im(Ext^(n-1-i)(tau^{>=i+1}M, w) -> Ext^(n-i-1)(M, w)))).
    -- Here phi: Z[T,T^(-1)] -> Z[T,T^(-1)] is the ring homomorphism with phi(T) = T^(-1),
    -- and M -> tau^(>=j)M is the canonical truncation.
    mapi := inducedMap(canonicalTruncation(M, -infinity, -cohodeg), M);
    mapiplus1 := inducedMap(canonicalTruncation(M, -infinity, -cohodeg-1), M);
    Cresmap := resolutionMap M;
    tauiCresmap := resolutionMap target mapi;
    tauiplus1Cresmap := resolutionMap target mapiplus1;
    mapofresi := liftMapAlongQuasiIsomorphism(mapi*Cresmap, tauiCresmap);
    mapofresiplus1 := liftMapAlongQuasiIsomorphism(mapiplus1*Cresmap, tauiplus1Cresmap);
    rightpart := hilbertFunction(0, HH^cohodeg(M));
    poincimageextmapi := hilbertFunction(0, image HH^(n-cohodeg)(Hom(mapofresi, w)));
    poincimageextmapiplus1 := hilbertFunction(0, image HH^(n-1-cohodeg)(Hom(mapofresiplus1, w)));
    poincextiplus1 := hilbertFunction(0, HH^(n-cohodeg-1)(Hom(source Cresmap, w)));
    rightpart-poincimageextmapi+poincextiplus1-poincimageextmapiplus1)


-- This function hh^i(C,b1,b2) computes the cohomology of a closed subspace of a weighted projective space
-- with coefficients in a complex of sheaves (sometimes called "hypercohomology"),
-- with all twists in an interval [b1,b2]. The base ring should be a field.
--
-- The input complex can also be a complex of graded R-modules,
-- in which case it is interpreted as the associated complex of coherent sheaves.
--
-- This function computes only the Hilbert series of the cohomology, not the cohomology as a module.
-- The algorithm, using local duality, is the same as that used for hh^i(C) and hh^i(C(*)).
-- Note that it is usually faster to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Twists are interpreted by tensoring with the line bundles O(a) on the stack.
--
hh(ZZ, Complex, ZZ, ZZ) := RingElement => opts -> (cohodeg, C, b1, b2) -> (
    if not instance(b1, ZZ) or not instance(b2, ZZ) or b1>b2 then (
	error "the input should be in the form hh^i(C,b1,b2), with C a complex of sheaves and b1 <= b2 integers");
    R2 := ring module C;
    M := liftComplex module C;
    R1 := ring M; -- R1 is a graded polynomial ring, and M is a complex of graded R1-modules that represents the complex C of sheaves.
    if degreeLength R1 =!= 1 then error "expected degree length 1";
    A := degreesRing R1; -- This is a ring of the form "ZZ[T]" (meaning Z[T,T^(-1)]).
    T := A_0; -- This is the variable in the ring A.
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    R1.cache ??= new MutableHashTable;
    ww := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module ww, as a graded R1-module.
    -- Our formula for the Hilbert series of H^i(P, M~(*)) (where i = cohodeg) is:
    --   hilb(H^i(M)) - phi(hilb(im(Ext^(n-i)(tau^{>=i}M, ww) -> Ext^(n-i)(M, ww))))
    --   + phi(hilb(Ext^(n-i-1)(M, ww))) - phi(hilb(im(Ext^(n-1-i)(tau^{>=i+1}M, ww) -> Ext^(n-i-1)(M, ww)))).
    -- Here phi: Z[T,T^(-1)] -> Z[T,T^(-1)] is the ring homomorphism with phi(T) = T^(-1),
    -- and M -> tau^(>=j)M is the canonical truncation.
    output := 0;
    mapi := inducedMap(canonicalTruncation(M, -infinity, -cohodeg), M);
    mapiplus1 := inducedMap(canonicalTruncation(M, -infinity, -cohodeg-1), M);
    Cresmap := resolutionMap M;
    tauiCresmap := resolutionMap target mapi;
    tauiplus1Cresmap := resolutionMap target mapiplus1;
    mapofresi := liftMapAlongQuasiIsomorphism(mapi*Cresmap, tauiCresmap);
    mapofresiplus1 := liftMapAlongQuasiIsomorphism(mapiplus1*Cresmap, tauiplus1Cresmap);
    rightpart := hilbertSeries(HH^cohodeg(M), Order => b2+1); -- The Hilbert series in degrees <= b2.
    poincimageextmapi := hilbertSeries(image HH^(n-cohodeg)(Hom(mapofresi, ww)), Order => -b1+1);
    poincimageextmapiplus1 := hilbertSeries(image HH^(n-1-cohodeg)(Hom(mapofresiplus1, ww)), Order => -b1+1);
    poincextiplus1 := hilbertSeries(HH^(n-cohodeg-1)(Hom(source Cresmap, ww)), Order => -b1+1);
    leftpart := invertvar(-poincimageextmapi+poincextiplus1-poincimageextmapiplus1);
    -- The output is the sum of the Laurent polynomials rightpart and leftpart in T, restricted to exponents in [b1, b2].
    part(b1, b2, rightpart + leftpart))


-- This function (usually called as hh^i(C(*)))
-- computes the cohomology of a closed subspace of a weighted projective space with coefficients
-- in a complex of sheaves (sometimes called "hypercohomology"), with all twists. For the input hh^i(C(>=b)),
-- the number b is ignored, as the output explains. The base ring should be a field.
-- The output is a sequence listing the infimum of weights a such that H^i(X,C(a)) is not zero (possibly -infinity or,
-- if the cohomology is zero in all weights, infinity), the supremum of such weights,
-- a rational function in T, and a rational function in U = T^{-1}, such that the sum of these two functions
-- as Laurent series is sum_a h^i(X, C(a)) T^a (the sum over all integers a). (The two functions do not overlap,
-- as formal series in T.)
--
-- The input complex can also be a complex of graded R-modules,
-- in which case it is interpreted as the associated complex of coherent sheaves.
--
-- This program computes only the Hilbert series of the cohomology, not the cohomology as a module.
-- The algorithm, using local duality, is the same as that used for hh^i(C) and hh^i(C,b1,b2),
-- and slightly different from that used for the module HH^i(C(>=b)). It should be faster, in most cases.
-- Note that it is usually faster to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Twists are interpreted by tensoring with the line bundles O(a) on the stack.
--
hh(ZZ, SumOfTwistsComplex) := Sequence => opts -> (cohodeg, sumoftwists) -> (
    -- Compute H^cohodeg(X,C(a)) for all integers a, as a sum of two Laurent series,
    -- where C is a complex of sheaves on a closed subspace of a weighted projective space.
    C := sumoftwists#0; -- For an input of the form hh^i(C(>=b)), the number b is ignored.
    -- Let's not cache whether C was defined as a twist.
    R2 := ring module C;
    M := liftComplex module C;
    R1 := ring M; -- R1 is a graded polynomial ring, and M is a complex of graded R1-modules that represents the complex C of sheaves.
    if degreeLength R1 =!= 1 then error "expected degree length 1";
    hft := heft R1;
    A := degreesRing R1; -- This is a ring of the form "ZZ[T]" (meaning Z[T,T^(-1)]).
    T := A_0; -- This is the variable in the ring A.
    U := getSymbol "U";
    B := newRing(A,Variables=>{U},MonomialOrder=>{MonomialSize=>32,Weights=>{-1},GroupLex=>1,Position=>Up},Inverses=>true);
    U = B_0;
    -- Thus B is a ring of the form "ZZ[U]" (meaning Z[U,U^(-1)]). We think of U as meaning T^(-1).
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    R1.cache ??= new MutableHashTable;
    ww := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module ww, as a graded R1-module.
    -- Our formula for the Hilbert series of H^i(P, M~(*)) (where i = cohodeg) is:
    --   hilb(H^i(M)) - phi(hilb(im(Ext^(n-i)(tau^{>=i}M, ww) -> Ext^(n-i)(M, ww))))
    --   + phi(hilb(Ext^(n-i-1)(M, ww))) - phi(hilb(im(Ext^(n-1-i)(tau^{>=i+1}M, ww) -> Ext^(n-i-1)(M, ww)))).
    -- Here phi: Z[T,T^(-1)] -> Z[T,T^(-1)] is the ring homomorphism with phi(T) = T^(-1),
    -- and M -> tau^(>=j)M is the canonical truncation.
    mapi := inducedMap(canonicalTruncation(M, -infinity, -cohodeg), M);
    mapiplus1 := inducedMap(canonicalTruncation(M, -infinity, -cohodeg-1), M);
    Cresmap := resolutionMap M;
    tauiCresmap := resolutionMap target mapi;
    tauiplus1Cresmap := resolutionMap target mapiplus1;
    mapofresi := liftMapAlongQuasiIsomorphism(mapi*Cresmap, tauiCresmap);
    mapofresiplus1 := liftMapAlongQuasiIsomorphism(mapiplus1*Cresmap, tauiplus1Cresmap);
    rightpart := hilbertSeries(HH^cohodeg(M), Reduce => true);
    denom := denominator hilbertSeries R1;
    poincimageextmapi := poincare image HH^(n-cohodeg)(Hom(mapofresi, ww));
    poincimageextmapiplus1 := poincare image HH^(n-1-cohodeg)(Hom(mapofresiplus1, ww));
    poincextiplus1 := poincare HH^(n-cohodeg-1)(Hom(source Cresmap, ww));
    leftpart := substitute(reduceHilbert Divide{-poincimageextmapi+poincextiplus1-poincimageextmapiplus1, denom}, T => U);
    -- The output is the sum of rightpart as a Laurent series in T and leftpart as a Laurent series in U, meaning T^(-1).
    -- We look for a neat way to describe this output, since it is a series in T that may be infinite to the left and the right.
    bottomdeg := 0;
    topdeg := 0;
    positiveseries := 0;
    negativeseries := 0;
    hilb1 := 0; hilb0 := 0;
    if value denominator leftpart == 1 then ( -- Here Ext^cohodeg(X,C(*)) is bounded below.
	-- In this case, we will return the output as a rational function in T.
	hilb10 := substitute(value numerator leftpart, U => T^(-1));
	-- This is a Laurent polynomial in ZZ[T] = Z[T, T^(-1)].
	positiveseries = add(rightpart, hilb10);
	negativeseries = 0;
	if value numerator positiveseries == 0 then (  -- In this case, Ext^cohodeg(X,C(*)) = 0.
	    topdeg = -infinity;
	    bottomdeg = infinity;
	    positiveseries = 0)
	else ( -- Here Ext^(cohodeg)(X,C(*)) is bounded below and not zero.
	    bottomdeg = first min exponents value numerator positiveseries; -- This gives the bottom degree of the output.
	    if value denominator positiveseries == 1 then topdeg = first max exponents value numerator positiveseries
	    else topdeg = infinity))
    else ( -- Here Ext^(cohodeg)(X,C(*)) is not bounded below.
	bottomdeg = -infinity;
	if value denominator rightpart == 1 then ( -- Here Ext^(cohodeg) is bounded above but not below.
	    -- In this case, we will return the output as a rational function in U, meaning T^(-1).
	    hilb10 = substitute(value numerator rightpart, T => U^(-1));
	    -- This is a Laurent polynomial in ZZ[U] = Z[U, U^(-1)].
	    negativeseries = add(leftpart, hilb10);
	    positiveseries = 0;
	    topdeg = - first min exponents value numerator negativeseries)
	else ( -- Here Ext^(cohodeg) is unbounded in both directions.
	    topdeg = infinity;
	    negpart := truncateSeries(0, hft, rightpart);
	    -- The part of "rightpart" of degree < 0 in T, a Laurent polynomial.
	    pospart := truncateSeries(1, hft, leftpart);
	    -- The part of "leftpart" of degree <= 0 in U, a Laurent polynomial.
	    positiveseries = add(rightpart, -negpart + substitute(pospart, U => T^(-1))); -- A Divide of degree >=0 in T.
	    negativeseries = add(leftpart, -pospart + substitute(negpart, T => U^(-1))))); -- A Divide of degree >0 in U (meaning T^(-1)).
        ("The following is correct in all degrees. Bottom degree:", bottomdeg, "top degree:", topdeg, "cohomology as a series in T:",
	positiveseries, "plus cohomology as a series in U = T^(-1):", negativeseries))


-- extOptions = new OptionTable from {
--     Degree => 0
--     }

-- -- Modeled on hh, defined as a ScriptedFunctor in Functors.m2.
-- ext = new ScriptedFunctor from {
--     superscript => i -> new ScriptedFunctor from {
-- 	-- ext^i(C, D), ext^i(C, D(*)), and so on
-- 	argument => extOptions >> opts -> X -> applyMethodWithOpts''(ext, functorArgs(i, X), opts)
-- 	},
--     argument => extOptions >> opts -> X -> applyMethodWithOpts''(ext, X, opts)
--     }

-- This function ext^i(C,D) computes the dimension of Ext^i_X(C,D).
-- Here X is a closed subspace of a projective space, or more generally of a weighted projective space.
-- Also, C and D are complexes of sheaves on X. The base ring should be a field.
-- If X is a subspace of a weighted projective space, it is viewed as an algebraic stack when that makes a difference.
-- The algorithm uses local duality. 
--
-- If you want to compute Ext with many twists, the functions ext^i(C,D,b1,b2) or ext^i(C,D(*))
-- may be convenient. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- The input complexes can also be complexes of graded R-modules,
-- in which case they are interpreted as the associated complexes of coherent sheaves.
--
-- ext(ZZ, Complex, Complex) := Sequence => opts -> (cohodeg, C, D) -> (
--     M := module C;
--     N := module D;
--     R2 := ring M;
--     if ring N =!= R2 then error "for Ext, the two complexes should be defined on the same space";
--     lengthlimit := cohodeg+1+(max N)-(min M);
--     -- We need a free resolution of the complex M over R2 out to this length.
--     if lengthlimit <= 0 then 0 -- In this case, inspecting the proof shows that the output is zero.
--     else (
-- 	Mres := freeResolution(M, LengthLimit => lengthlimit);
-- 	hh^cohodeg(Hom(Mres,N))))

-*
-- This function (usually called as ext^i(C,D,b1,b2)) computes the dimension of Ext^i_X(C,D(a)) for all integers a
-- in an interval [b1,b2]. Here X is a closed subspace of a projective space,
-- or more generally of a weighted projective space.
-- Also, C and D are complexes of sheaves on X. In some cases, you might prefer ext^i(C,D(*)),
-- which computes Ext with all twists. The base ring should be a field.
-- If X is a subspace of a weighted projective space, it is viewed as an algebraic stack when that makes a difference.
-- In particular, twists are interpreted by tensoring with the line bundles O(a) on the stack.
-- The algorithm uses local duality.
--
-- The input complexes can also be complexes of graded R-modules,
-- in which case they are interpreted as the associated complexes of coherent sheaves.
--
-- Note that it is usually faster to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
ext(ZZ, Complex, Complex, ZZ, ZZ) := Sequence => opts -> (cohodeg, C, D, b1, b2) -> (
    M := module C;
    N := module D;
    R2 := ring M;
    if ring N =!= R2 then error "for Ext, the two complexes should be defined on the same space";
    lengthlimit := cohodeg+1+(max N)-(min M);
    -- We need a free resolution of the complex M over R2 out to this length.
    if lengthlimit <= 0 then 0 -- In this case, inspecting the proof shows that the output is zero.
    else (
	Mres := freeResolution(M, LengthLimit => lengthlimit);
	hh^cohodeg(Hom(Mres,N),b1,b2)))
*-

-- This function (usually called as ext^i(C,D(*))) computes the dimension of Ext^i_X(C,D(a)) for all integers a.
-- Here X is a closed subspace of a projective space, or more generally of a weighted projective space.
-- Also, C and D are complexes of sheaves on X. For the input ext^i(C,D(>=b)),
-- the number b is ignored, as the output explains. The base ring should be a field.
-- If X is a subspace of a weighted projective space, it is viewed as an algebraic stack when that makes a difference.
-- In particular, twists are interpreted by tensoring with the line bundles O(a) on the stack.
-- The algorithm uses local duality.
--
-- The output is a sequence listing the infimum of weights a such that Ext^i_X(C,D(a)) is not zero
-- (possibly -infinity or, if Ext^i is zero in all weights, infinity), the supremum of such weights,
-- a rational function in T, and a rational function in U = T^{-1}, such that the sum of these two functions
-- as Laurent series is sum_a ext^i_X(C, D(a)) T^a (the sum over all integers a).
-- (The two functions do not overlap, as formal series in T.)
--
-- The input complexes can also be complexes of graded R-modules,
-- in which case they are interpreted as the associated complexes of coherent sheaves.
--
-- This function should be faster than computing the module Ext^i_X(C,D(>=b)), in most cases.
-- Note that it is usually faster to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- ext(ZZ, Complex, SumOfTwistsComplex) := Sequence => opts -> (cohodeg, C, sumoftwists) -> (
--     -- Compute Ext^cohodeg_X(C,D(a)) for all integers a, as a sum of two Laurent series,
--     -- where C and D are complexes of sheaves on a closed subspace X of a weighted projective space.
--     D := sumoftwists#0; -- For an input of the form ext^i(C,D(>=b)), the number b is ignored.
--     M := module C;
--     N := module D;
--     R2 := ring M;
--     if ring N =!= R2 then error "for Ext, the two complexes should be defined on the same space";
--     lengthlimit := cohodeg+1+(max N)-(min M);
--     -- We need a free resolution of the complex M over R2 out to this length.
--     if lengthlimit <= 0 then ( -- In this case, inspecting the proof shows that the output is zero.
-- 	bottomdeg := infinity;
-- 	topdeg := -infinity;
-- 	positiveseries := 0;
-- 	negativeseries := 0)
--     else (
-- 	Mres := freeResolution(M, LengthLimit => lengthlimit);
-- 	output := hh^cohodeg((Hom(Mres,N))(*), opts);
-- 	bottomdeg = output_1;
-- 	topdeg = output_3;
-- 	positiveseries = output_5;
-- 	negativeseries = output_7);
--     ("The following is correct in all degrees. Bottom degree:", bottomdeg, "top degree:", topdeg, "Ext as a series in T:",
-- 	positiveseries, "plus Ext as a series in U = T^(-1):", negativeseries))

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
    M := liftComplex module C;
    N := liftComplex module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    H := prune HH N;
    (loH, hiH) := concentration H;
    L := for i from loH to hiH list dim H_i;
    l := max L;
    Resns := for i from loH to hiH list resolution liftModule H_i;
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
    M := liftComplex module C;
    N := liftComplex module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    H := prune HH N;
    (loH, hiH) := concentration H;
    L := for i from loH to hiH list dim H_i;
    l := max L;
    Resns := for i from loH to hiH list resolution liftModule H_i;
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
    M := liftModule module C;
    N := liftComplex module D;
    R := ring M;
    if not isAffineRing R
    then error "expected sheaves on a variety over a field";
    H := prune HH N;
    (loH, hiH) := concentration H;
    L := for i from loH to hiH list min(dim H_i,m);
    l := max L;
    Resns := for i from loH to hiH list resolution liftModule H_i;
    P := for i in Resns list length i;
    p := max P;
    n := dim ring (H_loH) - 1;
    if p >= n - l then (
	a := max for i from 0 to length(Resns)-1 list max apply(n - L_i .. P_i, j-> (max degrees (Resns_i)_j)#0 - j);
	r := a - l + 1;
	M = truncate(r, M));
    complex applyValues(D.dd.map, f -> part(0, Ext^m(M, matrix f, opts))))

-- The following would be faster than the current function. It is omitted for now, because
-- it is not obviously functorial. But it could be combined with functorial constructions,
-- by choosing a basis for HH^p(X, C) in a fixed way when needed.
--cohomology(ZZ, ProjectiveVariety, Complex) := Complex => opts -> (p, X, C) -> (
--    checkVariety(X, C);    
--    C.cache.cohomology   ??= new MutableHashTable;
--    k := coefficientRing ring variety C;
--    C.cache.cohomology#p ??= k^(hh^p(C))) -- Ext^p(sheaf X, C, opts)

cohomology(ZZ, ProjectiveVariety, Complex) := Complex => opts -> (p, X, C) -> (
    C.cache.cohomology   ??= new MutableHashTable;
    C.cache.cohomology#p ??= Ext^p(sheaf X, C, opts))

-----------------------------------------------------------------------------

euler Complex := C -> sum(pairs C.module, (i, M) -> (-1)^i * euler M)

-----------------------------------------------------------------------------
-- Common complexes of sheaves
-----------------------------------------------------------------------------

-- TODO: Beilinson resolution of the diagonal for PP^n

eulerSequence = method()
eulerSequence ProjectiveVariety := Complex => X -> (
    -- Given a projective variety X \subset PP^n, returns the two maps
    -- 0 <-- OO_X^1 <-- OO_X^(n+1)(-1) <-- Omega_PP^n|X <-- 0
    complex { sheaf_X vars(S := ring X), sheaf_X inducedMap(source vars S, ker vars S) })

cotangentSequence = method();
cotangentSequence ProjectiveVariety := Complex => X -> (
    p := cotangentSurjection X;
    i := inducedMap(source p, ker p);
    complex {p, i})

idealSheafSequence = method()
idealSheafSequence ProjectiveVariety := Complex => X -> (
    -- Given a projective variety X \subset PP^n, returns the sequence
    -- 0 -> I_X -> OO_P -> OO_P/I_X -> 0
    IX := idealSheaf(X);
    i := inducedMap(ambient IX, IX);
    p := inducedMap(coker i, ambient IX);
    complex {p, i})

-----------------------------------------------------------------------------
-- naiveCotangentComplex
-----------------------------------------------------------------------------

naiveCotangentComplex = method(TypicalValue => Complex,
    Options => options exteriorPower ++ { MinimalGenerators => true })

-- The cotangent complex of a closed substack X of a weighted projective space P over a field k
-- lives in cohomological degrees <= 1. This function computes its truncation to degrees >= 0,
-- which lives in degrees 0 and 1. The Macaulay2 package Complexes is needed.
--
-- The input is a ProjectiveVariety, that is, a closed substack of a weighted projective space.
-- If Y denotes the affine cone over X in A^{n+1}, X is the quotient stack by the multiplicative group G_m,
-- [(Y-0)/G_m].
--
-- The output is a complex of sheaves on X in cohomological degrees 0 and 1. It can be viewed as the complex
-- of G_m-equivariant sheaves 0 -> Omega^1_Y -> g^* tensor O_Y -> 0, where g is the Lie algebra of G_m.
-- (The boundary map is given by plugging in the vector field associated to the G_m-action on Y.)
-- The cohomology sheaf of this complex in degree 0 is computed by cotangentSheaf(X).
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
    sheaf complex({map0}, Base => -1) -- The complex is in homological degrees 0 and -1, that is, cohomological degrees 0 and 1.
    )

-- The cotangent complex of a closed substack X of a weighted projective space P over a field k
-- lives in cohomological degrees <= 1. This function computes its ith exterior power truncated to degrees >= 0,
-- which lives in degrees from 0 to i. The Macaulay2 package Complexes must be loaded.
--
-- The input X is a ProjectiveVariety, that is, a closed substack of a weighted projective space.
-- If Y denotes the affine cone over X in A^{n+1}, X is the quotient stack by the multiplicative group G_m,
-- [(Y-0)/G_m].
--
-- The output is a complex of sheaves on X in cohomological degrees from 0 to i. It can be viewed
-- as the complex of G_m-equivariant sheaves
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
-- than its cohomology sheaf in degree 0.
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
    sheaf complex(maplist, Base => -i)) -- The complex is in homological degrees 0,-1,...,-i,
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
