--recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"

setVerboseLevel 0;

setDefault(Software=>M2engine) --works 177.367 secs
--setDefault(Software=>BERTINI) -- works
--setDefault(Software=>PHCPACK) -- works 70secs

setRandomSeed 0

--Problem X^9 in G(3,6)
 --a problem with 42 solutions

print("Solving problem X^9 in G(3,6)");

SchPblm = randomSchubertProblemInstance(
  {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6);
elapsedTime S = solveSchubertProblem(SchPblm, 3,6);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG36/X^9-G36.m2"

