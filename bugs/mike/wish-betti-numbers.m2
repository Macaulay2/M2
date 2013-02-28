-- Wish: compute the graded Betti numbers of a free resolution without minimalizing it
-- This is my "scratch" area for experimentation.  It is checked in because I use it on several 
-- machines
restart
N = 10
N = 9
D = 3
N = 7
D = 3
R = ZZ/101[vars(0..N-1), MonomialSize=>8]
F = random(R^1, R^{-D})
J = ideal fromDual F
gbTrace=1
time C = res(J, Strategy=>0)

J = ideal J_*
time C = res(J, Strategy=>1)

J = ideal J_*
time C = res(J, Strategy=>2) -- very bad!

J = ideal J_*
time C = res(J, Strategy=>3) -- 

betti C


-- Let's recall what the options are in Strategy 2!
skeletonSort: & 63
reductionSort: 2^6 + (63)
do_by_level: FLAGS_LEVEL (1 << 13)

gbTrace=3
time res(J, SortStrategy=>0, Strategy=>0)
J = ideal J_*;
res(J, StopBeforeComputation=>true, SortStrategy=>3, Strategy=>0)
time C = res(J, SortStrategy=>2^16, Strategy=>0)
time res(J, SortStrategy=>2^16+2^17, Strategy=>0)
time time C = res(J, SortStrategy=>2^16 + 2^17 + 2^18, Strategy=>0)
time res J;

debug Core
rawResolution

restart
R = ZZ/101[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = ideal(m1*m2-m2*m1)
time res J
J = ideal J_*;
time res(J, SortStrategy=>2^16 + 2^17 + 2^18, Strategy=>0)

-- simple example
R = ZZ/101[a..e]
J = ideal(a^2-b*c, a*c^2-b*d^2, a^2*e^5-b^3*c*d^3)

gens gb J
C = res J


time C = res(J, SortStrategy=>2^13 + 2^16, Strategy=>0)
time C = res(J, SortStrategy=>2^16, Strategy=>0)
time for i from 1 to 7 list C.dd_i;
C.dd_2
leadTerm oo

submatDegree = (d, M) -> (
    f := positions(degrees target M, x -> x == d);
    g := positions(degrees source M, x -> x == d);
    lift(submatrix(M, f, g), coefficientRing ring M)
    )

submatDegree = (d, M) -> (
    f := positions(degrees target M, x -> x == d);
    g := positions(degrees source M, x -> x == d);
    submatrix(M, f, g)
    )

time M = submatDegree({3}, C.dd_2);
time submatDegree({4}, C.dd_2);
time submatDegree({4}, C.dd_3);
time submatDegree({5}, C.dd_3);
time M = submatDegree({5}, C.dd_4);
time submatDegree({6}, C.dd_4);
time submatDegree({6}, C.dd_5);
time submatDegree({7}, C.dd_5);
time submatDegree({7}, C.dd_6);
time gens gb oo;

restart
loadPackage "FastLinearAlgebra"
debug Core
kk = ZZp 101
time sub(M, kk);
time mutableMatrix oo;
time rank oo
betti C

-- We could write a routine which returns minimal Betti numbers, given:
--   a non-minimal res, but where it is graded.
-- a. find its Betti tally
-- b. determine which matrices and degrees we need
-- c. find these matrices
-- d. compute their ranks (use linbox/ffpack routines over finite fields)
-- e. use this info to return a new Betti tally, with minimal betti numbers
