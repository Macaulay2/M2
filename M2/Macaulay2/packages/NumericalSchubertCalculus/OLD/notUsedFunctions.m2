-- Functions that are not used anymore
-- Oct 2016
-----------------------------------

------------------------
-- dist
------------------------
-- function to measure the 
-- euclidean distance between
-- two points: (x1,...,xn) and (y1,...,yn)
-- it computes:
--    sqrt(sum( (xi-yi)^2 ))
-- 
-- Input: two Lists representing two set of solutions to a system
-- Output: list of distances between the elements of the list
--   
--- Notice that solns1 could come from a numerical Newton step,
-- so the function first check if the solution is of type List or 
-- of type Point
--
--------- FOR DAN AND MIKE -------
-- Notice we had to do this function because
-- M2 function norm(2,_) does not give the 2-norm
-- of the complex vector (x1,...,xn)
----------------------- 
-- ** NOT USED ANYMORE
dist = method()
dist(List,List) := (solns1,solns2) -> (
    apply(#solns1, i->(
	    v1 := solns1#i;
	    v2 := solns2#i;
	    if instance(v1,AbstractPoint) then v1 = coordinates(v1);
	    if instance(v2,AbstractPoint) then v2 = coordinates(v2);
	    sqrt(sum(apply(v2-v1, i->i^2)))
   ))
)

--------------------------
-- Given the partitions l, and m for the Grassmannian Gr(k,n)
-- it creates a flags as random numeric matrices G_1,...,G_m
-- for solving the system defined by these random matrices
-- using Homotopy Continuation
--------------------------
-- input: 
--    kn   - sequence with two numbers (k,n) specifying Gr(k,n)
--    l,m  - Lists each a partition 
-------------------------- 
createRandomFlagsForSimpleSchubert = method( )
createRandomFlagsForSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
	 (k,n) := kn;
	 l = verifyLength(l, k);
	 m = verifyLength(m, k);
   d := k*(n-k)-sum(l)-sum(m);
   --apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)))
   apply(d, i-> random(FFF^(n-k),FFF^n))
   )

