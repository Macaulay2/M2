needs "shared.m2"  -- for isMorphism
needs "gateway.m2" -- for ScriptedFunctor
needs "matrix1.m2" -- for Ideal
needs "modules.m2" -- for Module

-- whether to use Complexes or OldChainComplexes
HomologicalAlgebraPackage = "OldChainComplexes"

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- called from OldChainComplexes
baseRing' = R -> ultimate(coefficientRing, R) -- Note: different from first R.baseRings

-----------------------------------------------------------------------------
-- unexported helper functions used in several packages
-----------------------------------------------------------------------------

resolutionLengthLimit = (R, lengthLimit) -> (
    if lengthLimit == infinity then (
	A := baseRing' R;
	nvars := # generators(R, CoefficientRing => A);
	nvars + 1 + if A === ZZ then 1 else 0)
    else lengthLimit )

-- also used by Saturation
resolutionDegreeLimit = (R, degreeLimit) -> (
    degreeLimit = if degreeLimit =!= null      then  degreeLimit  else {};
    degreeLimit = if instance(degreeLimit, ZZ) then {degreeLimit} else degreeLimit;
    if #degreeLimit == degreeLength R and all(degreeLimit, d -> instance(d, ZZ))
    or #degreeLimit == 0 then degreeLimit
    else error "expected DegreeLimit or HardDegreeLimit to be a valid degree, multidegree, or null")

-----------------------------------------------------------------------------
-- this is here because currently both Complexes and OldChainComplexes use it

Resolution = new Type of MutableHashTable
Resolution.synonym = "resolution"

-----------------------------------------------------------------------------
-- Categories for which complexes are currently implemented in Macaulay2
-----------------------------------------------------------------------------
-- SheafMap and CoherentSheaf are added later in Varieties

-- note: this is also used matrix(List)
isMorphism Thing    := x -> false
isMorphism Matrix   := f -> true

isAbelianCategory Thing         := x -> false
isAbelianCategory Module        := M -> true

Module Array := (M, v) -> missingPackage "either Complexes or OldChainComplexes"

-----------------------------------------------------------------------------
-- Ext
-----------------------------------------------------------------------------

-- TODO: should this be fixed for all Ext methods,
-- or should they each have their own options?
ExtOptions = new OptionTable from {
    MinimalGenerators => true
}

Ext = new ScriptedFunctor from {
    superscript => i -> new ScriptedFunctor from {
	-- Ext^1(F, G)
	argument => ExtOptions >> opts -> X -> applyMethodWithOpts''(Ext, functorArgs(i, X), opts)
    },
    argument => ExtOptions >> opts -> X -> applyMethodWithOpts''(Ext, X, opts)
}

-- TODO: Ext^i(R,S) should work as well, e.g. via pushForward
Ext(ZZ, Ring,   Ring)   := Ext(ZZ, Ring,  Ideal) := Ext(ZZ, Ring,  Module) :=
Ext(ZZ, Ideal,  Ring)   := Ext(ZZ, Ideal, Ideal) := Ext(ZZ, Ideal, Module) :=
Ext(ZZ, Module, Ring)   :=
Ext(ZZ, Module, Ideal)  := Module => o -> (i, M, N) -> Ext^i(module M, module N, o)
Ext(ZZ, Module, Module) := Module => o -> (i, M, N) -> missingPackage "either Complexes or OldChainComplexes"

Ext(ZZ, Ring,   Matrix) :=
Ext(ZZ, Ideal,  Matrix) := Matrix => o -> (i, M, g) -> Ext^i(module M, g, o)
Ext(ZZ, Module, Matrix) := Matrix => o -> (i, M, g) -> missingPackage "OldChainComplexes"

Ext(ZZ, Matrix, Ring)   :=
Ext(ZZ, Matrix, Ideal)  := Matrix => o -> (i, f, N) -> Ext^i(f, module N, o)
Ext(ZZ, Matrix, Module) := Matrix => o -> (i, f, N) -> missingPackage "OldChainComplexes"

-- TODO: Ext(R,S) should work as well, e.g. via pushForward
Ext(Ring,   Ring)   := Ext(Ring,  Ideal) := Ext(Ring,  Module) :=
Ext(Ideal,  Ring)   := Ext(Ideal, Ideal) := Ext(Ideal, Module) :=
Ext(Module, Ring)   :=
Ext(Module, Ideal)  := Module => o -> (M, N) -> Ext(module M, module N, o)
Ext(Module, Module) := Module => o -> (M, N) -> missingPackage "either Complexes or OldChainComplexes"

-----------------------------------------------------------------------------
-- Tor
-----------------------------------------------------------------------------

-- TODO: should this be fixed for all Tor methods,
-- or should they each have their own options?
TorOptions = new OptionTable from {
    MinimalGenerators => true
}

Tor = new ScriptedFunctor from {
    subscript => i -> new ScriptedFunctor from {
	-- Tor_i(F, G)
	argument => TorOptions >> opts -> X -> applyMethodWithOpts''(Tor, functorArgs(i, X), opts)
    },
    argument => TorOptions >> opts -> X -> applyMethodWithOpts''(Tor, X, opts)
}

-- TODO: Tor_i(R,S) should work as well, e.g. via pushForward
Tor(ZZ, Ring,   Ring)   := Tor(ZZ, Ring,  Ideal) := Tor(ZZ, Ring,  Module) :=
Tor(ZZ, Ideal,  Ring)   := Tor(ZZ, Ideal, Ideal) := Tor(ZZ, Ideal, Module) :=
Tor(ZZ, Module, Ring)   :=
Tor(ZZ, Module, Ideal)  := Module => o -> (i, M, N) -> Tor_i(module M, module N, o)
Tor(ZZ, Module, Module) := Module => o -> (i, M, N) -> missingPackage "either Complexes or OldChainComplexes"

-- see packages/Complexes/Tor.m2 for Tor(ZZ, Module, Matrix) and Tor(ZZ, Matrix, Module)
Tor(ZZ, Ring,   Matrix) :=
Tor(ZZ, Ideal,  Matrix) := Matrix => o -> (i, M, g) -> Tor_i(module M, g, o)
Tor(ZZ, Module, Matrix) := Matrix => o -> (i, M, g) -> missingPackage "Complexes"

Tor(ZZ, Matrix, Ring)   :=
Tor(ZZ, Matrix, Ideal)  := Matrix => o -> (i, f, N) -> Tor_i(f, module N, o)
Tor(ZZ, Matrix, Module) := Matrix => o -> (i, f, N) -> missingPackage "Complexes"
