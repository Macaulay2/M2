restart
debug loadPackage "NAGtypes"
debug loadPackage "NumericalAlgebraicGeometry"
DBG = 2;
------------------------------------------------------
-- COMPARISON OF              -- PHCpack -- Bertini -- hom4ps2 --
load "benchmarks.m2";
I = katsuraBench 8;	    	
-- I = stewartGough40real();     -- 883     -- >>0     -- 31      --
--I = example2();

P = solveSystem (I_*,Software=>PHCpack);
B = solveSystem (I_*,Software=>Bertini);
H = solveSystem (I_*,Software=>HOM4PS2); 

-- check if P, B, and H are approx. same 
Bs = sortSolutions B;
Ps = sortSolutions P;
Hs = sortSolutions H;
difBP = diffSolutions(Bs/first, Ps/first);
difBH = diffSolutions(Bs/first, Hs/first);
assert(#difBP#0+#difBP#1+#difBH#0+#difBH#1==0) 
