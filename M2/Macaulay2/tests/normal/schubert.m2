-- In-Reply-To: <200610201925.k9KJPC98001554@u123.math.uiuc.edu>
-- Cc: bernd@Math.Berkeley.EDU, mike@math.cornell.edu, degraaf@science.unitn.it
-- Subject: Re: equations for Schubert varieties
-- Date: Sat, 28 Oct 2006 00:09:17 -0700
-- To: dan@math.uiuc.edu
-- From: Josephine Yu <josephine.yu@gmail.com>
-- 
-- Dear Dan,
-- 
-- I think the code is fine as it is now.  I'm pretty sure we want to  
-- keep only the variables that are <= the given one.  Well of course,  
-- we didn't define what the Schubert variety "indexed by" a set really  
-- is.  (It seems more common for them to be indexed by Young diagrams,  
-- or partitions.  Maybe someone, or I, can implement that at some  
-- point.  It won't be very hard.)
-- 
-- Here in this example, the Schubert variety indexed by sigma = {1,2,4}  
-- in Gr(2,4) is the set of 3-planes in K^5 that intersects V_(sigma_i)  
-- at dimension at least i, where V_j is spanned by e_1, ... , e_j.  So  
-- we surely want to keep the variable p_{1,2,4} and not throw it in the  
-- ideal.  And p_tau = 0 if tau is not <= sigma, so p_tau should be in  
-- the ideal.
-- 
-- Josephine
-- 
-- On Oct 20, 2006, at 12:25 PM, Dan Grayson wrote:
-- 
-- >
-- > Bernd,
-- >
-- > I think it's throwing in the wrong variables.  I've looked at the code, and it
-- > throws in the variables whose tuples are not <= the given one in the product
-- > ordering.  This keeps the variables whose tuples that are <= the given one.
-- > Since (2,3,4), (1,3,4), and (0,3,4) are greater than (1,2,4) (in the middle
-- > spot) it throws the corresponding variables in.
-- >
-- >     i1 : I = Schubert(2,4,{1,2,4})
-- >
-- >     o1 = ideal (p     , p     , p     , p     p      - p     p     , p     p      - p     p     , p     p      - p     p     )
-- >                  2,3,4   1,3,4   0,3,4   1,2,3 0,2,4    0,2,3 1,2,4   1,2,3 0,1,4    0,1,3 1,2,4   0,2,3 0,1,4    0,1,3 0,2,4
-- >
-- >     o1 : Ideal of ZZ [p     , p     , p     , p     , p     , p     , p     , p     , p     , p     ]
-- >                        0,1,2   0,1,3   0,2,3   1,2,3   0,1,4   0,2,4   1,2,4   0,3,4   1,3,4   2,3,4
-- >
-- > It seems from your remarks that it should really be throwing in all variables
-- > whose tuples are greater than or equal to {1,2,4} in the product ordering.
-- > That retains the variables that are not >= {1,2,4}.



-- this was the proposed change
-- --- schubert.m2	(revision 4731)
-- +++ schubert.m2	(working copy)
-- @@ -49,7 +49,7 @@
--       L := toSequence \ subsets(n+1,k+1);
--       R := o.CoefficientRing (monoid [apply(L, i -> new IndexedVariable from {baseName o.Variable,unsequence i})]);
--       vr := new HashTable from apply(#L, i -> L#i => R_i);
-- -     higher := apply( select( L, s -> any(s, sigma, (a,b) -> a>b)), s -> vr#s );
-- +     higher := apply( select( L, s -> all(s, sigma, (a,b) -> a>=b)), s -> vr#s );
--       G := flatten for i from 0 to #L-1 list for j from i+1 to #L-1 list (
--  	  r := L#i;
--  	  s := L#j;

I = Schubert(2,4,sigma = {1,2,4})
use ((ring I)/I)
assert ( p_(1,2,4) != 0 )
assert all( toSequence \ subsets( {0,1,2,3,4}, 3 ), tau -> if all(tau,sigma,(i,j)->i<=j) then p_tau != 0 else p_tau == 0)
