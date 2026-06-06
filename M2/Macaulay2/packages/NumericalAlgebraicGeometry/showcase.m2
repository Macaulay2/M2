debug needsPackage "NumericalAlgebraicGeometry"
NAGtrace 0
load (currentFileDirectory|"benchmarks.m2")

-- BENCHMARK SYSTEMS: ---------------------------------------------------------
systems = {
     -- random system in n variables of degree d 
     (n = 5; d = 4; setRandomSeed 0; -- #sols=1024, M2:1/4/5, B:20/18, P:20/18; (old... M2:4, H:11, B:51, P:63)
	  T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T; (T,S,solsS)), 
     (n = 5; d = 5; setRandomSeed 0; -- #sols=3125, M2:8/37/43, B:159/153, P:136/206; (old... M2:30, H:78, B:402, P:550)
	  T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T; (T,S,solsS)), 
     -- katsura
     (T = (katsuraBench 11)_*; -- #sols=1024, M2:2/3/3, B:12/13, P:14/52; (old... M2:4, H:7, B:15, P:37) 
	  (S,solsS) = totalDegreeStartSystem T; (T,S,solsS)), 
     (T = (katsuraBench 12)_*, -- #sols=2048, M2:5/7/8, B:43/46, P:37/182; (old... M2:11, H:19, B:37, P:102) -- PHCpack loses some sols in solveSystem
	  (S,solsS) = totalDegreeStartSystem T; (T,S,solsS)), 
     -- random generalized eigenvalue problem
     (setRandomSeed 0; randomGeneralizedEigenvalueProblem 35) -- #sols=35, M2:2/10/???, B:136/???, P:47: (old... M2:3, B:40, P:323) 
                                                               -- PHCpack thinks 4 solutions are singular
							       -- M2 goes to precision=100 (with multiprecision)  
     };

software = {
     M2engine,
     BERTINI,
     PHCPACK
     -- , HOM4PS2
     };

timeSystemSoftware = (system,soft) -> (     
    (T,S,solsS) = system;
    << "---------------------------------------------------------" << endl;
    << "---------- COMPUTING with " << soft << "-----------------" << endl; 
    << "---------------------------------------------------------" << endl;
    soft=>sortSolutions if soft===HOM4PS2 then elapsedTime solveSystem(T,Software=>soft)/point
    else (
	print "track";
	elapsedTime print tally (status \ track(S,T,solsS,gamma=>1+ii,Software=>soft)) ;
	if soft===M2engine then ( 
	    print "trackHomotopy";
	    elapsedTime print tally (status \ trackHomotopy(segmentHomotopy(polySystem S,polySystem T, gamma=>1+ii), solsS,EndZoneFactor=>0))
	    );
	print "solveSystem";
	s := elapsedTime solveSystem(T,Software=>soft);
	print tally (status \ s);
	s
	)
    )
 
end -----------------------------------------------------------------------------------
restart
load "NumericalAlgebraicGeometry/showcase.m2"
sols = new HashTable from apply(drop(systems,-1), system->( 
	(system,soft) => apply(software, soft->timeSystemSoftware(system,soft)) 
	))
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
system = first systems;
--...
system = last systems;
soft = M2engine
soft = BERTINI  
soft = PHCPACK

timeSystemSoftware(system,soft)

