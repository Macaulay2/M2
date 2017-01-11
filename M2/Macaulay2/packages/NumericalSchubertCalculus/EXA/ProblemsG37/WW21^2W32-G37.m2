needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem WW21^2W32 in G(3,7)
 --a problem with 4 solutions

print("Solving problem WW21^2W32 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3, 2},{2, 1},{2, 1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/WW21^2W32-G37.m2"

