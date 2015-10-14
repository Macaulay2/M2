needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem X^2X2^2X21 in G(3,6)
 --a problem with 4 solutions
SchPblm = randomSchubertProblemInstance(
  {{2, 1},{2},{2},{1},{1}},3,6);
time S = solveSchubertProblem(SchPblm, 3,6);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

