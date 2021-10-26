needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

--Problem X^3X2X11^2 in G(3,6)
 --a problem with 5 solutions

print("Solving problem X^3X2X11^2 in G(3,6)");

SchPblm = randomSchubertProblemInstance(
  {{1, 1},{1, 1},{2},{1},{1},{1}},3,6);
time S = solveSchubertProblem(SchPblm, 3,6);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

 end
 ------

restart
 load"NumericalSchubertCalculus/EXA/ProblemsG36/X^3X2X11^2-G36.m2"

