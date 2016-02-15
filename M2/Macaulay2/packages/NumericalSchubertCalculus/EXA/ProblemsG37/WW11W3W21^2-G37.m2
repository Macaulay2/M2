needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem WW11W3W21^2 in G(3,7)
 --a problem with 5 solutions

print("Solving problem WW11W3W21^2 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{2, 1},{2, 1},{3},{1, 1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/WW11W3W21^2-G37.m2"

