needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem WW11^4W3 in G(3,7)
 --a problem with 4 solutions

print("Solving problem WW11^4W3 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3},{1, 1},{1, 1},{1, 1},{1, 1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/WW11^4W3-G37.m2"

