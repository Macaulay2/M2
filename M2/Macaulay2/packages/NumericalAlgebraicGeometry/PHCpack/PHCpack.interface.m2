-- PHCpack interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2
-- needsPackage "PHCpack"

solvePHCpack = method(TypicalValue => List)
solvePHCpack (List,HashTable) := List => (F,o) -> (
     -- Anton: options are not used at the moment
     phcSolve F
     )

trackPHCpack = method(TypicalValue => List)
trackPHCpack (List,List,List,HashTable) := List => (S,T,sols,o) -> (
     -- Anton: options are not used at the moment
     --trackPaths(S,T,sols,PHCpack$gamma=>o.NAG$gamma,PHCpack$tDegree=>o.NAG$tDegree)     
     trackPaths(S,T,sols,gamma=>o.NAG$gamma,tDegree=>o.NAG$tDegree)
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

-- service functions ------------------------------------------
-- deleted, since duplicated in PHCpackInterface

/// -- examples
restart
-- notify = true
needsPackage "NumericalAlgebraicGeometry"
--debug PHCpackInterface
--debug NumericalAlgebraicGeometry
--peek loadedFiles
PHCpackInterface#"exported symbols"
NumericalAlgebraicGeometry#"exported symbols"

R = CC[x]
L = {x^2-2}
solveSystem(L, Software=>PHCpack)
refine(L, {{1.7}}, Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130, Software=>PHCpack)

R = CC_200[x,y,z]
L = {y-x^2,z-x^3,x+y+z-1}
B = solveSystem(L,Software=>PHCpack)
B = B/first
C = apply(B, b -> refinePHCpack(L, {b}, Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130))
C/first/first

-- Using higher precision
R = CC_53[x,y,z]
R200 = CC_200[x,y,z]
L = {y-x^2,z-x^3,x+y+z-.5p200}
B = solveSystem(L,Software=>PHCpack)
B = solveSystem(L)
pt = B_0_0

C = refinePHCpack(L, {pt}, Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130)
pt1 = C_0_0
pt_0
pt1_0
///

