--restart
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

------------------------------
-- problem (2,1),(1,1),1^3 =  in G(2,6)
n=6;
k=2;
l = {2,1};
m = {1,1};
conds = {l,m}|toList(3:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);

solveSimpleSchubert(SchbPblm,2,6)

