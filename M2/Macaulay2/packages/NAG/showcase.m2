restart
loadPackage ("NAG", FileName=>"../NAG.m2",
          Configuration=>{"PHCpack"=>"./phc", "Bertini"=>"./bertini", "HOM4PS2"=>"./hom4ps2_in_out"})
debug Core; debug NAG; DBG = 1; printingPrecision = 20; 
load "benchmarks.m2"

-- PICK A SYSTEM: ---------------------------------------------------------

-- random system in n variables of degree d 

n = 5; d = 4; -- beats PHC by a factor of 3
setRandomSeed 0; T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T;

n = 5; d = 5; -- PHC does not finish in 30 min
setRandomSeed 0; T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T;

-- PICK WHAT TO RUN: ------------------------------------------------------
-- 1st run
M = track(S,T,solsS, gamma=>1+ii,
     Projectivize=>false,
     SLPpredictor=>true,
     SLPcorrector=>true,
     SLP=>HornerForm
     ); 

-- another run (needs Mac Os X with gcc) 
P = track(S,T,solsS, gamma=>1+ii,
     Projectivize=>false,
     SLPpredictor=>true,
     SLPcorrector=>true,
     SLP=>CompiledHornerForm                                                                                                                                           
     );

-- another run (needs PHCpack)
t = currentTime(); P = track(S,T,solsS,gamma=>1+ii,Software=>PHCpack); << "RUNNING TIME = " << currentTime()-t << " sec." << endl;

Ms = sortSolutions M;
Ps = sortSolutions P;
<< "Solutions that differ: " << (dif = diffSolutions(Ms/first, Ps/first)) << endl;
<< "Multiple solutions: " << select(toList(0..#Ms-2), i->areEqual(first Ms#i,first Ms#(i+1))) << endl;
