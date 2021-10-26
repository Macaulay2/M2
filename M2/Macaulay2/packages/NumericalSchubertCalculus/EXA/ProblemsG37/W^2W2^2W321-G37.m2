needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem W^2W2^2W321 in G(3,7)
 --a problem with 4 solutions

print("Solving problem W^2W2^2W321 in G(3,7)");

SchPblm = randomSchubertProblemInstance(
  {{3, 2, 1},{2},{2},{1},{1}},3,7);
time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG37/W^2W2^2W321-G37.m2"

