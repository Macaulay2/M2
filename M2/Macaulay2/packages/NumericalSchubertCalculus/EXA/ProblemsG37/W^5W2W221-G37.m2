needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W^5W2W221 in G(3,7)
 --a problem with 10 solutions

print("Solving problem W^5W2W221 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{2, 2, 1},{2},{1},{1},{1},{1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W^5W2W221-G37.m2"

