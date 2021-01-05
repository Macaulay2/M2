restart
--path = {"./M2-NSC/M2/Macaulay2/packages"} | path
----------    This had a problem with Keywords
path = {"/Users/sottile/papers/completed/LR/NoKey/packages"} | path

--  This file studies the effect on the performance (or not) of changing
--   the order of the Schubert conditions in a Schubert problem.
--   It involves the Schubert problem 1^2*2^2*3^2 = 10 on G(2,8) 
--  Executive summary:  It is problematic

recursionLimit=10000
needsPackage "NumericalSchubertCalculus";
setVerboseLevel 0
setRandomSeed 0
--  Consider this ordering of the conditions:
setRandomSeed 0
SchPblm = {
    ({3}, id_(CC^8)),
    ({3}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 2,8);
print(#S)
--  This other order works, as do most with {3} among the first two conditions
--
setRandomSeed 0
SchPblm = {
    ({3}, id_(CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 2,8);
print(#S)
--v
--  This one also works, too (but look below)
--
setRandomSeed 0
SchPblm = {
    ({2}, id_(CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 2,8);
print(#S)
--
-----   The same problem, but with the conditions in the other order does not run very well
setRandomSeed 0  -- ,3,4,5,6,7,8,9,12,13  stdio:25:10:(3): error: assertion failed
setRandomSeed 1  -- ,2,10,11  stdio:15:10:(3): error: zgesvd did not converge
setRandomSeed 13
SchPblm = {
    ({1}, id_(CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 2,8);
print(#S)
--
--  This also does not work
--
-- setRandomSeed 0,1,2,3  error: a solution does not fit the expected pattern (numerical error occured)
setRandomSeed 3
SchPblm = {
    ({2}, id_(CC^8)),
    ({2}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8)),
    ({3}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8)),
    ({1}, random(CC^8,CC^8))
    };
time S = solveSchubertProblem(SchPblm, 2,8);
print(#S)
--
--  As shown above swapping the order of the last four, from 3311 to 1133 makes it work
