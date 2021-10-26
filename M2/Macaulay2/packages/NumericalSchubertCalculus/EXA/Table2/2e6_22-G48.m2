-- 2e6_22-G48.m2
---------------------------------------------------------------------------------
-- Problem (2)^6*(22) = 50 in Gr(4,8)
-- First run this using compiled (PHCPack) version
n = 8;
SchubProb =  matrix{ {1, 3,4,7,8}, {6, 3,6,7,8}};   -- (22)*2^6
print LRrule(n,SchubProb);       --  50 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              -- 50 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
SchPblm = {
    ({2,2}, id_(CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 4,8);
print(#S)                           --  50 Solutions

