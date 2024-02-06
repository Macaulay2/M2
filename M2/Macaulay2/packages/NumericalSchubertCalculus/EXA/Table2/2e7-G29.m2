-- 2e7-G29.m2
---------------------------------------------------------------------------------
-- Problem (2)^7 = 36 in Gr(2,9)
-- First run this using compiled (PHCPack) version
n = 9;
SchubProb =  matrix{{7, 6,9}};   -- (2)^7
print LRrule(n,SchubProb);       -- 36  solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              -- 36 solutions
L_(#L-10)                        -- timing information
--   The same problem, but using the NSC interpreted version
SchPblm = {
    ({2}, id_(CC^9)),
    ({2}, random(CC^9,CC^9)),
    ({2}, random(CC^9,CC^9)),
    ({2}, random(CC^9,CC^9)),
    ({2}, random(CC^9,CC^9)),
    ({2}, random(CC^9,CC^9)),
    ({2}, random(CC^9,CC^9))
    };
time S = solveSchubertProblem(SchPblm, 2,9);
print(#S)

