-- Multigraded stuff is giving wrong answers as follows:

-- The following fail with one or both of these rings
R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}}]
R = ZZ/101[a,b,Degrees=>{{1,2},{0,4}}, Heft=>{1,1}]

I = ideal(a^2,b^3)
basis(1,I)
basis({1,2},I)
hilbertFunction(1,I) -- this should not be allowed
hilbertFunction({4,6},R) -- this potentially works?
degree I -- This is a poly in T_1, not correct at all.
codim I
dim I
poincare I
regularity I -- probably wrong
-- degree should require a Weight vector and compute it with a singly graded ring.

-- other problems:
-- negative or zero gradings
--  

-- rewrite: degree, hilbertFunction, and others.
-- regularity, degree -- these should take a Weight option

restart
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


restart
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
