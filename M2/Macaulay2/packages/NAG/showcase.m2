loadPackage "NAG"
NAGtrace 2
load "benchmarks.m2"

-- BENCHMARK SYSTEMS: ---------------------------------------------------------
systems = {
     -- random system in n variables of degree d 
     (n = 5; d = 4; setRandomSeed 0; T = (randomSystem(n,d,CC))_*), -- #sols=1024, M2:4, H:11, B:51, P:63
     (n = 5; d = 5; setRandomSeed 0; T = (randomSystem(n,d,CC))_*), -- #sols=3125, M2:30, H:78, B:402, P:550
     -- katsura
     (katsuraBench 11)_*, -- #sols=1024, M2:4, H:7, B:15, P:37
     (katsuraBench 12)_*, -- #sols=2048, M2:11, H:19, B:37, P:134
     -- random generalized eigenvalue problem
     (setRandomSeed 0; (S,T,solsS) = randomGeneralizedEigenvalueProblem 35) -- #sols=35, M2:2, B:22, P:432 
};
softwares = {M2engine,HOM4PS2,Bertini,PHCpack};

for T in drop(systems,2) do (
     (S,solsS) = totalDegreeStartSystem T;
     sols = new HashTable from apply(softwares, soft->(
	       << "---------------------------------------------------------" << endl;
	       << "---------- COMPUTING with " << soft << "-----------------" << endl; 
	       << "---------------------------------------------------------" << endl;
	       soft=>sortSolutions if soft===HOM4PS2 then solveSystem(T,Software=>soft)
	       else track(S,T,solsS,gamma=>1+ii,Software=>soft))
	       );
     assert all(drop(softwares,1), soft->areEqual(sols#soft/(s->{first s}),sols#(first softwares),Tolerance=>1e-3));
     )
 
end
restart
load "showcase.m2"
M = sols#(first softwares);
M = sols#Bertini;
<< "Multiple solutions: " << select(toList(0..#M-2), i->areEqual(first M#i,first M#(i+1),Tolerance=>1e-3)) << endl;
assert all(#M, i->getSolution(i,SolutionAttributes=>SolutionStatus)=="REGULAR") 
<< "Large residual: " << select(toList(0..#M-2), i->norm sub(matrix {T}, matrix M#i)>0.001) << endl;

