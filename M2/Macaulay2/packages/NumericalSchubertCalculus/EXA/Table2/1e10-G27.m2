-- 1e10-G27.m2
---------------------------------------------------------------------------------
-- Problem (1)^10 = 42 in Gr(2,7)
-- First run this using compiled (PHCPack) version
n = 7;
SchubProb =  matrix{{10, 5,7}};  -- (1)^10
print LRrule(n,SchubProb);       -- 42 solutions
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_1                              -- 42 solutions
L_(#L-10)                        -- timing information
-------   The same problem, but using the NSC interpreted version
SchPblm = {
    ({1}, id_(CC^7)),
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
time S = solveSchubertProblem(SchPblm, 2,7);
print(#S)
