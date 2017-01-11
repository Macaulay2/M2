needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W2^4W11^2 in G(3,7)
 --a problem with 9 solutions

print("Solving problem W2^4W11^2 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{1, 1},{1, 1},{2},{2},{2},{2}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W2^4W11^2-G37.m2"

