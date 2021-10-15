-- 1e8_22e2-G48.m2
---------------------------------------------------------------------------------
-- Problem 1^8*(22)^2 = 280 in Gr(4,8)
-- First run this using compiled (PHCPack) version
n = 8;
SchubProb =  matrix{{2, 3,4,7,8}, {8, 4,6,7,8}};   -- 1^8*(22)^2
------------------   The order here is important, switching it increases the run time by a factor of 25
print LRrule(n,SchubProb);       --  280 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              -- 280 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
-- setRandomSeed 0,1   error: trackHomotopy: singularity encountered
setRandomSeed 2  -- Needed for numerical instability 
SchPblm = {
    ({2,2}, id_(CC^8)),
    ({2,2}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 4,8);
print(#S)                           --  280 Solutions

