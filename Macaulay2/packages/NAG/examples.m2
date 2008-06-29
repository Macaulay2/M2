restart
loadPackage ("NAG", FileName=>"../NAG.m2", 
     Configuration=>{"PHCpack"=>"./phc", "Bertini"=>"./bertini", "HOM4PS2"=>"./hom4ps2_in_out"})
debug NAG
printingPrecision = 10;
------------------------------------------------------
-- COMPARISON OF              -- PHCpack -- Bertini -- hom4ps2 --
load "benchmarks.m2";
--I = katsura(8);	    	-- 26      -- 18      -- 4       --
--I = stewartGough40real();     -- 883     -- >>0     -- 31      --
I = example2();

t = currentTime(); P = solveSystem I_*; currentTime()-t
t = currentTime(); B = solveSystem (I_*,Software=>Bertini); currentTime()-t
t = currentTime(); H = solveSystem (I_*,Software=>hom4ps2); currentTime()-t

-- check if B and H approx. same 
apply(sort(B#0/(c->c/realPart)) - sort(H#0/(c->c/realPart)), a->apply(a, abs)) 

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

