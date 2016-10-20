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
	    if instance(v1,Point) then v1 = coordinates(v1);
	    if instance(v2,Point) then v2 = coordinates(v2);
	    sqrt(sum(apply(v2-v1, i->i^2)))
   ))
)
