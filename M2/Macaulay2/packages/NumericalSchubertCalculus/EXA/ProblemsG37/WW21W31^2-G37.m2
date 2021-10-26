needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem WW21W31^2 in G(3,7)
 --a problem with 4 solutions

print("Solving problem WW21W31^2 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3, 1},{3, 1},{2, 1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/WW21W31^2-G37.m2"

