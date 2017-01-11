needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W^2W2^2W3^2 in G(3,7)
 --a problem with 7 solutions

print("Solving problem W^2W2^2W3^2 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3},{3},{2},{2},{1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W^2W2^2W3^2-G37.m2"

