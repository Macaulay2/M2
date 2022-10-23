restart
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 730

------------------------------
-- problem (2,1),(1,1),1^3 = 2 in G(2,6)
n=6;
k=2;
l = {2,1};
m = {1,1};
conds = {l,m}|toList(3:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);

Sols=solveSimpleSchubert(SchbPblm,2,6)
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm))

---------------------------------------------
-- problem (2),(2),1^5 = 11  in G(3,6)
k=3;n=6;
conds = {{2},{2}}|toList(5:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

--------------------------------------------------
-- problem (2,1),(3,1),1^5 = 25  in G(3,7)
k=3;n=7;
conds = {{2,1},{3,1}}|toList(5:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- problem (2,1),(1,1),1^7 = 77  in G(3,7)   -- used 0.52211 seconds
k=3;n=7;
conds = {{2,1},{1,1}}|toList(7:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));
---------------------------------------------------------------------
-- problem (1),(1),1^10 = 462  in G(3,7)   -- used 10.3957 seconds
k=3;n=7;
conds = toList(12:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- problem (3,2),(3,2),1^5 = 30  in G(3,8)   -- used 3.5263 seconds
k=3;n=8;
conds = {{3,2},{3,2}}|toList(5:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));


---------------------------------------------------------------------
-- problem (2,1),(3,2),1^7 = 155  in G(3,8)   -- used 7.58889 seconds
k=3;n=8;
conds = {{2,1},{3,2}}|toList(7:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- problem (2,1),(2,1),1^9 = 744  in G(3,8)   -- used 27.1661 seconds
k=3;n=8;
conds = {{2,1},{2,1}}|toList(9:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- problem (1,1),(1,1),1^11 = 1122  in G(3,8)   -- used 57.594 seconds
k=3;n=8;
conds = {{1,1},{1,1}}|toList(11:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));


---------------------------------------------------------------------
-- problem (3,2),(3,2),1^8 = 449  in G(3,9)   -- used 37.6086 seconds
k=3;n=9;
conds = {{3,2},{3,2}}|toList(8:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- problem (3,2),(3,1),1^9 = 1101  in G(3,9)   -- used  86.4608 seconds
k=3;n=9;
conds = {{3,2},{3,1}}|toList(9:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- problem (2,2),(3,1),1^10 = 1515  in G(3,9)   -- used  139.109 seconds
k=3;n=9;
conds = {{2,2},{3,1}}|toList(10:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));


---------------------------------------------------------------------
-- This failed, too, for one seed, but not the other
---------------------------------------------------------------------
-- problem (2,1),(3,1),1^11 = 5115  in G(3,9)   -- used  390.581 seconds
k=3;n=9;
conds = {{2,1},{3,1}}|toList(11:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- 
---------------------------------------------------------------------
-- problem (2,1),(3),1^12 = 7260  in G(3,9)    -- used 698.935 seconds
k=3;n=9;
conds = {{2,1},{3}}|toList(12:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));


---------------------------------------------------------------------
-- 3 failures
---------------------------------------------------------------------
-- problem (2,1),(2),1^13 =   in G(3,9)    -- used 
k=3;n=9;
conds = {{2,1},{2}}|toList(13:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));




---------------------------------------------------------------------
-- problem (3,1),(3,1),1^8 =  398 in G(4,8)   -- used 10.7846 seconds
k=4;n=8;
conds = {{3,1},{3,1}}|toList(8:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

--------------------------------------------------------------------
-- problem (2,1),(2,1),1^10 = 3060  in G(4,8)   -- used 94.595 seconds
k=4;n=8;
conds = {{2,1},{2,1}}|toList(10:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));

---------------------------------------------------------------------
-- This has a problem (Frank 20 Oct 2016)
---------------------------------------------------------------------
-- problem (1,1),(2,1),1^11 =  in G(4,8)   -- 
k=4;n=8;
conds = {{2,1},{1,1}}|toList(11:{1})
SchbPblm = randomSchubertProblemInstance(conds, k,n);
time Sols=solveSimpleSchubert(SchbPblm,k,n);
#Sols
scan(Sols,s -> checkIncidenceSolution(s,SchbPblm));
