restart
loadPackage ("NAG", FileName=>"../NAG.m2",
          Configuration=>{"PHCpack"=>"./phc", "Bertini"=>"./bertini", "HOM4PS2"=>"./hom4ps2_in_out"})
debug Core; debug NAG; DBG = 1; printingPrecision = 20; 
load "benchmarks.m2"

-- PICK A SYSTEM: ---------------------------------------------------------

-- random system in n variables of degree d 
n = 5; d = 4; -- 
setRandomSeed 0; T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T; -- n=1024, M2:7, H:11, B:51, P:68
n = 5; d = 5; -- 
setRandomSeed 0; T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T; -- n=3125, M2:46, H:69, B:402, P:67

-- katsura
T = (katsuraBench 12)_*; (S,solsS) = totalDegreeStartSystem T; -- n=2048, M2:11, H:19, B:37, P:67
T = (katsuraBench 14)_*; (S,solsS) = totalDegreeStartSystem T; -- n=8192, M2:161, H:125, 

-- random generalized eigenvalue problem
setRandomSeed 0; (S,T,solsS) = randomGeneralizedEigenvalueProblem 35; -- n=35, M2:5, B:22, P:67 

-- PICK WHAT TO RUN: ------------------------------------------------------
-- 1st run
M = track(S,T,solsS, gamma=>1+ii,
     --gamma =>0.6+0.8*ii, tDegree=>2,
     Software=>M2engine,
     Projectivize=>false,
     SLP=>HornerForm
     ); 

-- another run (CompiledHornerForm needs Mac Os X with gcc) 
P = track(S,T,solsS, gamma=>1+ii,
     Software=>M2engine,
     Projectivize=>false,
     --Predictor=>Tangent,
     Predictor=>Euler,
     SLP=>HornerForm
     --SLP=>CompiledHornerForm                                                                                                                                           
     );

-- another run (needs PHCpack/HOM4PS/Bertini)
P = track(S,T,solsS,gamma=>1+ii,Software=>PHCpack);
P = solveSystem(T,Software=>HOM4PS2);
P = track(S,T,solsS,gamma=>1+ii,Software=>Bertini); 
 
Ms = sortSolutions M;
Ps = sortSolutions P;
<< "Solutions that differ: " << (dif = diffSolutions(Ms/first, Ps/first)) << endl;
<< "Multiple solutions: " << select(toList(0..#Ms-2), i->areEqual(first Ms#i,first Ms#(i+1))) << endl;
<< "Large solutions: " << select(toList(0..#Ms-1),i->norm(matrix{first Ms#i})>100) << endl;
