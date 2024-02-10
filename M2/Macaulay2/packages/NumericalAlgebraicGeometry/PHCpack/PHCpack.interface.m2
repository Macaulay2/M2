-- PHCpack interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2
-- needsPackage "PHCpack"

-- export {Xphc} -- a global variable 
-- protect XphcRing;

toRingXphc = method()
toRingXphc (List,List) := (F,G) -> (
    R := ring ideal (F|G); 
    Xphc := getSymbol "phcX";
    XphcRing := (coefficientRing R)[Xphc_1..Xphc_(numgens R)];
    M := map(XphcRing,R,gens XphcRing);
    ((M ideal F)_*, (M ideal G)_*)
    )
toRingXphc List := F -> first toRingXphc(F,F)

fromRingXphc = method()
fromRingXphc (List,Ring) := (F,R) -> (
    XphcRing := ring ideal F;
    M := map(R,XphcRing,gens R);
    (M ideal F)_*
    )
 
solvePHCpack = method(TypicalValue => List)
solvePHCpack (List,HashTable) := List => (F,o) -> (
     -- Anton: options are not used at the moment
     PHCpack$solveSystem toRingXphc F
     )

trackPHCpack = method(TypicalValue => List)
trackPHCpack (PolySystem, PolySystem, List, HashTable) := List => (S,T,sols,o) -> trackPHCpack(equations S, equations T, sols, o)
trackPHCpack (List,List,List,HashTable) := List => (S,T,sols,o) -> (
     -- Anton: options are not used at the moment
     --trackPaths(S,T,sols,PHCpack$gamma=>o.NAG$gamma,PHCpack$tDegree=>o.NAG$tDegree)     
     (T',S') := toRingXphc (T,S);
     sols' := sols / (s-> if instance(s,AbstractPoint) then s else point {s});
     trackPaths(T',S',sols',gamma=>o.NumericalAlgebraicGeometry$gamma,tDegree=>o.NumericalAlgebraicGeometry$tDegree,Verbose=>(DBG>0))
     )

refinePHCpack = method(TypicalValue => List)
refinePHCpack (List,List,HashTable) := List => (T,sols,o) -> 
     refineSolutions(toRingXphc T, sols, 
	 if o.Bits === infinity 
	 then getDefault Precision 
	 else ceiling(log(10,2)*o.Bits)
	 )

solveGenericSystemInTorus = method()
solveGenericSystemInTorus PolySystem := F -> solveGenericSystemInTorus equations F
solveGenericSystemInTorus List := F -> (
    (vol,S,solsS) := mixedVolume(toRingXphc F, StartSystem=>true);
    assert(vol==#solsS);
    (fromRingXphc(S,ring ideal F), solsS)
    )
 
numericalIrreducibleDecompositionPHCpack = (I,o) -> PHCpack$numericalIrreducibleDecomposition toRingXphc I_*

dismiss PHCpack
