-- PHCpack interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2
-- needsPackage "PHCpack"

solvePHCpack = method(TypicalValue => List)
solvePHCpack (List,HashTable) := List => (F,o) -> (
     -- Anton: options are not used at the moment
     PHCpack$solveSystem F
     )

trackPHCpack = method(TypicalValue => List)
trackPHCpack (List,List,List,HashTable) := List => (S,T,sols,o) -> (
     -- Anton: options are not used at the moment
     --trackPaths(S,T,sols,PHCpack$gamma=>o.NAG$gamma,PHCpack$tDegree=>o.NAG$tDegree)     
     trackPaths(S,T,sols,gamma=>o.NumericalAlgebraicGeometry$gamma,tDegree=>o.NumericalAlgebraicGeometry$tDegree)
     )

refinePHCpack = method(TypicalValue => List)
refinePHCpack (List,List,HashTable) := List => (T,sols,o) -> (
     -- Anton: options are not used at the moment
     refineSolutions(T,sols,
	  ResidualTolerance => o.ResidualTolerance, 
	  ErrorTolerance => o.ErrorTolerance,
	  Iterations => o.Iterations,
	  Bits => o.Bits)
     )

dismiss PHCpack
