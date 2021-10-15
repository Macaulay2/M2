-- 1e10_2-G37.m2
---------------------------------------------------------------------------------
-- Problem (1)^10*2  = 252 in Gr(3,7)
-- First run this using compiled (PHCPack) version
n = 7;
SchubProb =  matrix{{10, 4,6,7}, {1, 3,6,7}};   -- (1)^10*2
print LRrule(n,SchubProb);       -- 252  solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              --  252 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
setRandomSeed 7   --- This was tricky to get to run without numerical instability
SchPblm = {
    ({2}, id_(CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7)),
    ({1}, random(CC^7,CC^7))
    };
time S = solveSchubertProblem(SchPblm, 3,7);
print(#S)                           --  252 Solutions

