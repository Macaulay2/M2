restart
loadPackage ("NAG", FileName=>"../NAG.m2", 
     Configuration=>{"PHCpack"=>"./phc", "Bertini"=>"./bertini", "HOM4PS2"=>"./hom4ps2_in_out"})
debug NAG
DBG = 2;
printingPrecision = 10;
------------------------------------------------------
-- COMPARISON OF              -- PHCpack -- Bertini -- hom4ps2 --
load "benchmarks.m2";
I = katsura(8,CC);	    	-- 26      -- 18      -- 4       --
I = stewartGough40real();     -- 883     -- >>0     -- 31      --
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

--remove temp files
removeFile "tasols"; removeFile "target"; removeFile "output"; removeFile "input";
for f in {"failed_paths", "nonsingular_solutions",
               "raw_data", "start", "input", "output", "raw_solutions",
               "main_data", "real_finite_solutions", "finite_solutions",
               "midpath_data", "singular_solutions", "real_solutions",
               "singular_solutions", "midpath_data", "arr.out", "deg.out", 
	       "eval2.out", "jacP.out", "names.out", "par.out", "const.out",  
	       "eval.out",  "finalFile.out",  "jacV.out", "num.out", "paramDerivs.out", 
	       "config", "func_input", "nonhom_start", "preproc_data"} do
          if fileExists f then removeFile f; 

