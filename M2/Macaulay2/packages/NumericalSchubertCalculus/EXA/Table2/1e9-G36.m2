-- 1e9-G36.m2
---------------------------------------------------------------------------------
-- Problem (1)^9 = 42 in Gr(3,6)
-- First run this using compiled (PHCPack) version
n = 6;
SchubProb =  matrix{{9, 3,5,6}};   -- (1)^9
print LRrule(n,SchubProb);       --  42 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              --  42 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
SchPblm = {
    ({1}, id_(CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6)),
    ({1}, random(CC^6,CC^6))
    };
time S = solveSchubertProblem(SchPblm, 3,6);
print(#S)                           --  42 Solutions

