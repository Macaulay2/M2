-- Multigraded stuff is giving wrong answers as follows:

errorDepth = 0

-- The following fail with one or both of these rings
R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}}]
R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}}, Heft=>{1,1}]
I = ideal(a^2,b^3)
J = ideal(b^3)
J' = ideal(a^2)

-- this one wasn't a problem:
-- basis(1,I)

basis({1,2},I)
-- new behavior:
-- Macaulay 2, version 0.9.94
-- with packages: Classic, Core, Elimination, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone
-- 
-- i1 : R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}}]
-- -- warning: some variables have non-positive first component of degree
-- --          use Heft option to specify a positive form
-- --          or use Adjust and Repair to set up degrees internally
-- 
-- o1 = R
-- 
-- o1 : PolynomialRing
-- 
-- i2 : I = ideal(a^2,b^3)
-- 
--              2   3
-- o2 = ideal (a , b )
-- 
-- o2 : Ideal of R
-- 
-- i3 : basis({1,2},I)
-- stdio:3:1:(2):[2]: basis: non-positive heft form encountered (consider using Heft option or Adjust and Repair)

hilbertFunction(1,I) -- this should not be allowed
-- new behavior:
--    i4 : hilbertFunction(1,I) -- this should not be allowed
--    stdio:4:1:(2):[2]: degree length mismatch

poincare I
hilbertFunction({1,6},R)				    -- I've sped this up
codim I
dim I
degree I -- fixed
degree J -- fixed
degree J' -- fixed

h = hilbertSeries J
n = numerator h
d = value denominator h
n%d
n' = n//d



regularity I -- probably wrong
-- degree should require a Weight vector and compute it with a singly graded ring.

Order option isn't quite obeyed:

    i26 : R = QQ[a,b,Degrees=>{{1,2},{0,4}}, Heft=>{1,1}]

    o26 = R

    o26 : PolynomialRing

    i27 : hilbertSeries(R,Order => 3)

	       4    8      2      6      10    2 4    2 8    2 12
    o27 = 1 + T  + T  + T T  + T T  + T T   + T T  + T T  + T T
	       1    1    0 1    0 1    0 1     0 1    0 1    0 1

    o27 : ZZ [T , T , MonomialOrder => RevLex, Inverses => true]
	       0   1


-- other problems:
-- negative or zero gradings
--  

-- rewrite: degree, hilbertFunction, and others.
-- regularity, degree -- these should take a Weight option

R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}}]
I = ideal(a^2,b^3)
basis(1,I)
basis({1,2},I)
hilbertFunction(1,I) -- this should not be allowed
hilbertFunction({4,6},R) -- error message inappropriate
degree I -- This is a poly in T_1, not correct at all.
codim I
dim I
poincare I
regularity I -- probably wrong

R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}},Heft=>{1,1}]
I = ideal(a^2,b^3)
basis(1,I)
basis({1,2},R)
hilbertFunction(1,I) -- this should not be allowed
hilbertFunction({4,6},R) -- error message inappropriate
degree I -- This is a poly in T_1, not correct at all.
codim I
dim I
poincare I
regularity I -- probably wrong


done:
-- From: Sorin Popescu <sorin@math.sunysb.edu>
-- Date: October 23, 2006 7:36:15 PM EDT
-- To: Michael Stillman <mike@math.cornell.edu>
-- Subject: weightedBetti
weightedBetti = method()
weightedBetti (BettiTally,List) := (B,W) -> (
     dot := (X,Y) -> sum apply(#X, j-> X#j*Y#j);
     new BettiTally from apply(pairs B, i-> (
	       (key,bettivalue) := i;
	       (key#0, prepend(dot(W,key#1),key#1)),
	       bettivalue)))


done:
-- From: Sorin Popescu <sorin@math.sunysb.edu>
-- Date: October 23, 2006 7:33:02 PM EDT
-- To: Michael Stillman <mike@math.cornell.edu>
-- Subject: regularity of BettiTally
regularity BettiTally := (B) -> (
     max apply(pairs B, i-> first last i#0 - first i#0))

-----------------------------------------------------------------------------

does degree work (meaningfully) for single but differing degrees?

shall we change DegreeRank to DegreeLength to match degreeLength??

check degree ProjectiveHilbertPolynomial and hilbertPolynomial for multi-graded modules

degree ProjectiveHilbertPolynomial := opts -> (P) -> if #P === 0 then 0 else P#(dim P)

degree MonomialIdeal := opt -> I -> degree(cokernel generators I, opts)   -- maybe it's faster with 'poincare'

-----------------------------------------------------------------------------

symmetricAlgebra -- Amelia might fix it

    i6 : R = QQ[]

    o6 = R

    o6 : PolynomialRing

    i7 : symmetricAlgebra R^3

    o7 = QQ [x , x , x , Degrees => {{1, 0}, {1, 0}, {1, 0}}]
	      0   1   2

    o7 : PolynomialRing

     	  should be a tower!
	  
    i8 : symmetricAlgebra (R^{-1,-2,-3})
    stdio:8:20:(2):[2]: expected each multidegree to be of length 0

    i9 : degreeLength R

    o9 = 0

    i4 : symmetricAlgebra R^{1,3,4}
    -- warning: some variables have non-positive first component of degree
    --          use Heft option to specify a positive form
    --          or use Adjust and Repair to set up degrees internally
    -- warning: some variables have non-positive first component of degree
    --          use Heft option to specify a positive form
    --          or use Adjust and Repair to set up degrees internally

    o4 = QQ [x , x , x , x, y, Degrees => {{1, 0}, {1, 0}, {1, 0}, {0, 1}, {0, 1}}]
	      0   1   2


     should use the degrees of the free module

-----------------------------------------------------------------------------

