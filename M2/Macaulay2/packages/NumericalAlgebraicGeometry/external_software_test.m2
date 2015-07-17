restart
debug loadPackage "NAGtypes"
NAG = loadPackage "NumericalAlgebraicGeometry"
DBG = 2;
------------------------------------------------------
-- COMPARISON OF              -- PHCpack -- Bertini -- hom4ps2 --
load (NAG#"auxiliary files"|"benchmarks.m2");
I = katsuraBench 8;	    	
--I = stewartGough40real();  
--I = example2();

P = solveSystem (I_*,Software=>PHCPACK);
B = solveSystem (I_*,Software=>BERTINI);
-- H = solveSystem (I_*,Software=>HOM4PS2); 

-- check if P, B, and H are approx. same 
Bs = sortSolutions B;
Ps = sortSolutions P;
-- Hs = sortSolutions H;
difBP = diffSolutions(Bs, Ps)
assert(#difBP#0==0)
-- difBH = diffSolutions(Bs, Hs);
-- assert(#difBP#0+#difBP#1+#difBH#0+#difBH#1==0) 
