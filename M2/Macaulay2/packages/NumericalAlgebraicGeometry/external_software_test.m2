restart
NAG = loadPackage "NumericalAlgebraicGeometry"
DBG = 2;
------------------------------------------------------
-- COMPARISON OF              -- PHCpack -- Bertini -- hom4ps2 --
load (NAG#"auxiliary files"|"benchmarks.m2");
I = katsuraBench 8; -- PHCpack may lose solutions	    	
--I = stewartGough40real();  -- 2216 sec for Bertini
I = example2(); -- Bertini does not discard solutions at infinity

elapsedTime M = pointSet solveSystem (I_*,Software=>M2engine);
elapsedTime P = pointSet solveSystem (I_*,Software=>PHCPACK);
elapsedTime B = pointSet solveSystem (I_*,Software=>BERTINI);
-- H = pointSet solveSystem (I_*,Software=>HOM4PS2); 

-- check if P, B, and H are approx. same 
assert (M == B)
assert (M == P)
-- assert (M == H)
