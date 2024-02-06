-- 22e4-G48.m2
---------------------------------------------------------------------------------
-- Problem (22)^4 = 6 in Gr(4,8)
-- First run this using compiled (PHCPack) version
n = 8;
SchubProb =  matrix{{4, 3,4,7,8}};   -- (22)^4
print LRrule(n,SchubProb);       --  6 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              -- 6 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
SchPblm = {
    ({2,2}, id_(CC^8)),
    ({2,2}, random(CC^8,CC^8)),
    ({2,2}, random(CC^8,CC^8)),
    ({2,2}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 4,8);
print(#S)                           --  6 Solutions

