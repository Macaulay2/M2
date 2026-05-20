--------------------------
-- MATHEMATICAL BACKGROUND
--------------------------
--
-- The gonality of a curve is defined to be
-- the smallest degree of a morphism from the
-- curve to the projective line $\PP^1$.
-- It is known that a curve $C$ of genus $g$
-- admits a map to $\PP^1$ of degree at most
-- $[(g+3)/2]$.  Further, if $C$ is $d$-gonal,
-- then in its canonical embedding $C$
-- lies on a rational normal scroll of 
-- dimension $d-1$, and the free resolution
-- of the homogeneous coordinate ring of the 
-- scroll is a subcomplex of the 
-- free resolution of the homogeneous coordinate
-- ring of  $C$.  Thus 
-- for example the $2$-linear part of that 
-- resolution has length at least $g-d$, and
-- ``Green's Conjecture'' states that if one 
-- computes Clifford index instead of gonality,
-- a slight refinement, then equality holds.
-- For example, Green's conjecture predicts
-- that the resolution of the homogeneous 
-- coordinate ring of a general curve of
-- genus 7 and gonality 4 is:
--PRE^
--     total: 1 10 25 25 10 1
--         0: 1 .  .  .  .  .
--         1: . 10 16 9  .  .
--         2: . .  9  16 10 .
--         3: . .  .  .  .  1
--PRE$
-- (Green's conjecture has actually been proven
-- by Frank Schreyer in this case; in any case
-- the result that the two-linear part is AT LEAST
-- as long as predicted in Green's conjecture is
-- easy.)
-- 
-- If a curve can be represented as a plane curve
-- of degree $e$ with an ordinary multiple point
-- (that is, the branches have distinct tangents)
-- of multiplicity $m$, then projection from the
-- point defines a map to $\PP^1$ of degree $e-m$.
-- In this example we will illustrate the 
-- ``principle'' that this is often the gonality
-- of the curve by computing the canonical 
-- embedding and its resolution.  To compute the
-- canonical embedding, we will use ``adjunction'':
-- the canonical 
-- series of a plane curve $C$ of degree $e$ with
-- only ordinary multiple points of degrees $m_i$
-- as singularities is obtained as the 
-- linear series cut out by plane curves $D$ of degree 
-- $e-3$ passing through the nodes with multiplicities
-- $m_i-1$; at a tacnode of multiplicity 2 the
-- condition is that $D$ passes through the 
-- singular point and is tangent to the
-- tangent line of $C$ at that point.
--
-- We will make these computations for three
-- types of plane sextic curves (of genus seven):
--
-- {\tt C1} will have 3 ordinary nodes;
--
-- {\tt C2} will have one ordinary triple point;
--
-- {\tt C3} will have one tacnode and one ordinary node.

--------------
-- Computation
--------------
-- We take {\tt C1} to be a curve of degree $6$
-- having $3$ ordinary double points:
--
R = ZZ/31991[a,b,c] -- the coordinate ring of P^2

-- We define the ideals of the points.
-- We could write
ipoint1 = ideal matrix({{a,b}})
-- but the following shortcut is faster!
ipoint1 = ideal(a,b)
ipoint2 = ideal(a,c)
ipoint3 = ideal(b,c)

-- For a curve to be double at the $3$ points,
-- its equation must lie in the ideal
--^
icurves1 = intersect(
               ipoint1^2,
               ipoint2^2,
               ipoint3^2
           )
--$
-- The matrix with the generators of {\tt icurves1}
-- as its entries is obtained by
Icurves1 = gens icurves1

-- We find the equation {\tt F1} of a general curve
-- of degree $6$ with these double points by
-- composing a random matrix of forms having
-- the correct degree with the  matrix of 
-- generators of  {\tt icurves1}.

F1 = Icurves1 * random(source Icurves1, R^{-6})
betti F1


-- We now look for the equation {\tt F2} of {\tt C2},
-- a curve with an ordinary triple point at
-- {\tt point1}.  It must lie in the cube of the
-- ideal {\tt ipoint1}.  
Icurves2 = gens (ipoint1^3)
F2 = Icurves2 * random(source Icurves2, R^{-6})
betti F2

-- Finally, the equation of a curve with
-- a tacnode at $a=b=0$ having tangent line
-- $a-b=0$  there must lie in the ideal
i = ideal((a-b)^2) + (ipoint1^4)

-- and adding a node at {\tt point3} we get
icurves3 = intersect(i, ipoint3^2)
Icurves3 = gens icurves3

-- so
F3 = Icurves3 * random(source Icurves3, R^{-6})
betti F3


-- It is evident from the discussion above
-- that {\tt C1} and {\tt C3} have gonality $\leq 5$ (indeed,
-- every curve of genus $7$ has gonality $\leq 5$)
-- and that {\tt C2} has gonality $\leq 4$.  We can
-- establish lower bounds for the gonalities
-- by looking at the canonical embeddings.
-- The canonical series of {\tt C1} is cut out by

can1 = basis(3, intersect(ipoint1,ipoint2,ipoint3))

-- Some explanation regarding the {\tt basis} command
-- is needed here.  {\tt can1} is a matrix whose target
-- is the ideal of the intersection of these three points:
target can1
-- and whose source is a free module over the coefficient ring:
source can1
-- For our purposes, there are two problems with this.
-- The first is that we want a map where both the
-- source and target have the base ring $R$.  This can
-- be accomplished by tensoring with $R$:
can1 = can1 ** R
-- The second problem is that the image of a basis 
-- element is not obviously in the ideal: it is represented
-- in terms of the generators of $I$.  This can be 
-- alleviated by applying {\tt super}: this takes
-- a homomorphism $f : M \rightarrow{} N$, where $N$ is a submodule of
-- a quotient module $F/I$, and returns
-- the homomorphism $f : M \rightarrow{} F/I$.
can1 = super can1
-- similarly, for {\tt C2} and {\tt C3}:
can2 = basis(3, ipoint1^2)
can2 = super (can2 ** R)

can3 = basis(3, intersect(ideal(a-b) + ipoint1^2, ipoint3))
--
can3 = super (can3 ** R)
          
-- These should all give embeddings of the
-- curves in $\PP^6$, so we expect them to be
-- linear series of vector space dimension $7$.
-- Here's how to check:
betti  can1
betti  can2
betti  can3

-- To compute the homogeneous coordinate
-- rings of the canonical curves, we must
-- find the algebraic relations among the
-- generators of {\tt cani} modulo {\tt Fi}.
--
-- The coordinate ring of $\PP^6$
S = (coefficientRing R)[x_0..x_6]

-- Find the canonical ideal {\tt IC1}
-- of {\tt C1}, that is, the
-- kernel of the map $S \rightarrow{} R/(F1)$
-- defined by the canonical series.

T1 = R/ideal F1
f1 = map(T1,S,substitute(can1, T1))
IC1 = mingens ker f1

-- and similarly for {\tt C2, C3}
T2 = R/ideal F2
f2 = map(T2,S,substitute(can2, T2))
IC2 = mingens ker f2
 
T3 = R/ideal F3
f3 = map(T3,S,substitute(can3, T3))
IC3 = mingens ker f3

-- We now find the $2$-linear part of the 
-- free resolution of {\tt IC1}
-- and its betti numbers.

IC1res = res(coker IC1)
betti  IC1res

-- From this computation, and the easy 
-- implication of Green's conjecture 
-- explained above, we see that the 
-- gonality of {\tt C1} is exactly 4, the
-- gonality of the linear series obtained by
-- projection from any one of the three double
-- points.

-- We now do the same for {\tt IC2} and {\tt IC3}:

IC2res = res(coker IC2)
betti  IC2res

IC3res = res(coker IC3)
betti  IC3res

-- and we find that in the tacnodal case
-- the gonality is still 4, while in the
-- triple point case the gonality is 3.

-- Note that we could have made the computation
-- faster, as in the following example.  In 
-- these cases the resolution is so fast that
-- the speedup is not noticeable, but in 
-- larger cases it would be worthwhile.

-- First clear the info computed in {\tt IC1}
IC1 = matrix entries IC1
-- Now redo the resolution, this time bounding
-- the degree to which the computation is
-- carried.  
IC1res = res(coker IC1, DegreeLimit => {1})
betti IC1res

-- Instead of computing the canonical model
-- of {\tt C1} directly, we could have treated 

-- the structure
-- sheaf of {\tt C1} as a sheaf on the projective
-- plane, and compute its push-forward under
-- the map to $\PP^6$ given by {\tt can1} (the image of
-- the plane under this map is a Del Pezzo
-- surface on which the canonical curve lies.)
-- This is done as follows:

ff1 = map(R,S,can1)
G = map(coker F1,ff1)
trim coimage G
