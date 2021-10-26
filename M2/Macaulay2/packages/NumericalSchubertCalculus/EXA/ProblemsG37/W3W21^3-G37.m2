needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W3W21^3 in G(3,7)
 --a problem with 4 solutions

print("Solving problem W3W21^3 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{2, 1},{2, 1},{2, 1},{3}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W3W21^3-G37.m2"

