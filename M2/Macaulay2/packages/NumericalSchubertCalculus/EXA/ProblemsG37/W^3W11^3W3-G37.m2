needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W^3W11^3W3 in G(3,7)
 --a problem with 7 solutions

print("Solving problem W^3W11^3W3 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3},{1, 1},{1, 1},{1, 1},{1},{1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W^3W11^3W3-G37.m2"

