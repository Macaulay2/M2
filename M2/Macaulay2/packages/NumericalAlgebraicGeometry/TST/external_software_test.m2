restart
setRandomSeed 0
NAG = needsPackage "NumericalAlgebraicGeometry"
NAGtrace 1;
------------------------------------------------------
-- COMPARISON OF  -- M2 to PHCpack/Bertini/hom4ps2 --
load (NAG#"auxiliary files"|"benchmarks.m2");
I = katsuraBench 8; -- PHCpack may lose solutions	    	
I = stewartGough40real();  -- 137 sec from M2engine (multiprecision = double precision) 
                           -- 62 sec for PHCpack (double precision) 
			   -- 2120 sec for Bertini (multiprecision)
I = example2(); -- Bertini does not discard solutions at infinity

elapsedTime Mdouble = pointSet solveSystem (I_*,Software=>M2engine,Precision=>DoublePrecision);
elapsedTime Mfast = pointSet solveSystem (I_*,Software=>M2engine,Precision=>DoublePrecision,PostProcess=>false);
elapsedTime M = pointSet solveSystem (I_*,Software=>M2engine,Precision=>infinity); -- should be the same
elapsedTime P = pointSet solveSystem (I_*,Software=>PHCPACK);
elapsedTime B = pointSet solveSystem (I_*,Software=>BERTINI);
-- H = pointSet solveSystem (I_*,Software=>HOM4PS2); 

-- check if P, B, and H are approx. same 
assert (M == B)
assert (M == P)
-- assert (M == H)
