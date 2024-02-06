-- 2e8-G210.m2
---------------------------------------------------------------------------------
-- Problem (2)^8 = 91 in Gr(2,10)
-- First run this using compiled (PHCPack) version
n = 10;
SchubProb =  matrix{{8, 7,10}};   -- (2)^8
print LRrule(n,SchubProb);       --  91 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              --  91 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
SchPblm = {
    ({2}, id_(CC^10)),
    ({2}, random(CC^10,CC^10)),
    ({2}, random(CC^10,CC^10)),
    ({2}, random(CC^10,CC^10)),
    ({2}, random(CC^10,CC^10)),
    ({2}, random(CC^10,CC^10)),
    ({2}, random(CC^10,CC^10)),
    ({2}, random(CC^10,CC^10))
    };
time S = solveSchubertProblem(SchPblm, 2,10);
print(#S)                           --  91 Solutions

