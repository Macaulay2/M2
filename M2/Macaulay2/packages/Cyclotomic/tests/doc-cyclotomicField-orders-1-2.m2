-- doc-cyclotomicField-orders-1-2.m2
-- Documentation of cyclotomicField: "In characteristic 0 it is K if K
-- contains the N-th roots of unity".  This holds also for K =
-- cyclotomicField 1 and K = cyclotomicField 2 (fields made by
-- cyclotomicField) and N = 1, 2: the result is K itself.
for n in {1, 2} do for N in {1, 2} do (
    K := cyclotomicField n;
    assert(cyclotomicField(N, K) === K));
