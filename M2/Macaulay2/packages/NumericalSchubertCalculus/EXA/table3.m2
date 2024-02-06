-- table3.m2
--
--  This file contains code and commented out results from Table 3
---   Schubert is a Linux computational Server that Sottile used for these in 2016
--
recursionLimit=10000
needsPackage "NumericalSchubertCalculus";
needsPackage "NumericalAlgebraicGeometry";
setVerboseLevel 1
setRandomSeed 0 


n = 9;
SchubProb =  matrix{{14, 6,8,9},{2, 5,8,9}}; --- (1)^14*(2)^2
print LRrule(n,SchubProb); --  30459 solutions
--quit;
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_0
L_(#L-10)
--This took  213110.504000000 = 59h11m50s504ms in user time on Schubert

---------------------------------------------------------------------------------

n = 8;
SchubProb =  matrix{{16, 4,6,7,8}}; --- (1)^16
print LRrule(n,SchubProb); -- 24024 solutions
--quit;
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_0
L_(#L-10)
--This took   122986.324000000 = 34h 9m46s324ms in user time on Schubert

---------------------------------------------------------------------------------


n =9;
SchubProb =  matrix{{8, 5,7,8,9},{4, 4,6,8,9}}; --- (1)^8(2,1)^4
print LRrule(n,SchubProb); -- 25142 solutions
--quit;
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_0
L_(#L-10)
--This took 1054974.876000000 = 293h 2m54s876ms in user time on Schubert

---------------------------------------------------------------------------------

n =10;
--SchubProb =  matrix{{6, 5,7,8,9,10},{2, 4,7,8,9,10},{3, 3,6,7,9,10}}; --- (1)^6*(2)^2*(3,1,1)^3
SchubProb =  matrix{{0, 5,7,8,9,10},{5, 4,6,8,9,10},{2, 3,6,7,9,10}}; --- (1)*(2,1)^5*(3,1,1)^2
print LRrule(n,SchubProb); -- 8860 solutions
--quit;
result := LRtriple(n,SchubProb);
L= lines(result_2);
L_0
L_(#L-10)
--This took 777834.444000000 = 216h 3m54s444ms in user time on Schubert

---------------------------------------------------------------------------------
