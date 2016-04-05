needsPackage "NumericalSchubertCalculus"
--needsPackage "LRhomotopies"
setRandomSeed 2

-- Problem 1: {3,2,1} {1,1} {1} in Gr(3,6)
SchPblem = matrix{
    {1,1,3,5},
    {1,3,4,6},
    {1,3,5,6}
    }

-- Problem 2: {2,1} {2,1} {2,1} in Gr(3,6)
-- a problem with 2 solutions
SchPblm = matrix{
    {3,2,4,6}
    }

-- Problem with 3 solutions
-- Problem {4,2} {4,2} {4,2} in Gr(3,9)
SchPblm = matrix{
    {3,3,6,9}
    }

-- Problem with 5 solutions
-- {5,3,1}*{4,2,1}*{4,3,1} in G(4,10)
SchPblm = matrix{
    {2,5,8,10},
    {3,6,8,10},
    {3,5,8,10}
    }


end

restart
load "TripleIntersection_PHC.m2"