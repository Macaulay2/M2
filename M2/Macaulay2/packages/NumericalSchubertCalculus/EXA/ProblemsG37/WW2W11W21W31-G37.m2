needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem WW2W11W21W31 in G(3,7)
 --a problem with 6 solutions

print("Solving problem WW2W11W21W31 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3, 1},{2, 1},{1, 1},{2},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/WW2W11W21W31-G37.m2"

