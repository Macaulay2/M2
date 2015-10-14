needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem X^9 in G(3,6)
 --a problem with 42 solutions
SchPblm = randomSchubertProblemInstance(
  {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6);
time S = solveSchubertProblem(SchPblm, 3,6);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

