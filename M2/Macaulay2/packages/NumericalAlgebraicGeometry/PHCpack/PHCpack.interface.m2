-- PHCpack interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2
-- needsPackage "PHCpack"

solvePHCpack = method(TypicalValue => List)
solvePHCpack (List,HashTable) := List => (F,o) -> (
     -- Anton: options are not used at the moment
     PHCpack$solveSystem F
     )

trackPHCpack = method(TypicalValue => List)
trackPHCpack (PolySystem, PolySystem, List, HashTable) := List => (S,T,sols,o) -> trackPHCpack(equations S, equations T, sols, o)
trackPHCpack (List,List,List,HashTable) := List => (S,T,sols,o) -> (
     -- Anton: options are not used at the moment
     --trackPaths(S,T,sols,PHCpack$gamma=>o.NAG$gamma,PHCpack$tDegree=>o.NAG$tDegree)     
     trackPaths(S,T,sols,gamma=>o.NumericalAlgebraicGeometry$gamma,tDegree=>o.NumericalAlgebraicGeometry$tDegree)
     )

refinePHCpack = method(TypicalValue => List)
refinePHCpack (List,List,HashTable) := List => (T,sols,o) -> (
     refineSolutions(T, sols, ceiling(log(10,2)*o.Bits))
     )

numericalIrreducibleDecompositionPHCpack = I -> PHCpack$numericalIrreducibleDecomposition I_*

dismiss PHCpack
