-- 21e5-G38.m2
---------------------------------------------------------------------------------
-- Problem (21)^5 = 32 in Gr(3,8)
-- First run this using compiled (PHCPack) version
n = 8;
SchubProb =  matrix{{5, 4,6,8}};   -- (21)^5
print LRrule(n,SchubProb);       --  32 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              --  32 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
SchPblm = {
    ({2,1}, id_(CC^8)),
    ({2,1}, random(CC^8,CC^8)),
    ({2,1}, random(CC^8,CC^8)),
    ({2,1}, random(CC^8,CC^8)),
    ({2,1}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 3,8);
print(#S)                           --  32 Solutions

