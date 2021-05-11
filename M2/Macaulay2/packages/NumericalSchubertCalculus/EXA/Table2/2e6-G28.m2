-- 2e6-G28.m2
---------------------------------------------------------------------------------
-- Problem (2)^6 = 15 in Gr(2,8)
-- First run this using compiled (PHCPack) version
n = 8;
SchubProb =  matrix{{6, 5,8}};   -- (2)^6
print LRrule(n,SchubProb);       --  15 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              -- 15 solutions
L_(#L-10)                        -- timing information
-----   The same problem, but using the NSC interpreted version
SchPblm = {
    ({2}, id_(CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 2,8);
print(#S)

