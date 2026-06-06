R = ZZ[symbol a..symbol f]
I = monomialIdeal(a^3,b^2*c,a*c*d)     
J = saturate(I,a)
J1 = monomialIdeal(1_R)
assert(J == monomialIdeal(1_R))

R = QQ[x_1..x_10]
I = monomialIdeal vars R
(t, J) = toSequence elapsedTiming I^6;
assert(t < 1 and instance(J, MonomialIdeal) and numgens J == 5005)
(t, K) = toSequence elapsedTiming saturate J;
assert(t < 1 and instance(K, MonomialIdeal) and K == 1)
(t, L) = toSequence elapsedTiming radical J;
assert(t < 1 and instance(L, MonomialIdeal) and L == I)
