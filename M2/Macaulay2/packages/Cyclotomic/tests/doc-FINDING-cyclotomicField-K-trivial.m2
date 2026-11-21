-- doc-FINDING-cyclotomicField-K-trivial.m2 -- a finding of the independent tester.
-- Documentation of cyclotomicField: "In characteristic 0 it is K if K
-- contains the N-th roots of unity".  K = cyclotomicField 1 and
-- K = cyclotomicField 2 are fields made by cyclotomicField (valid inputs)
-- and contain the first and second roots of unity, so the result should be
-- K itself.  Observed: QQ.
for n in {1, 2} do for N in {1, 2} do (
    K := cyclotomicField n;
    assert(cyclotomicField(N, K) === K));
