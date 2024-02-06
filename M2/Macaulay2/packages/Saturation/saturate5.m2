-- This proves that we had a bug before 1.3 that made the code in the monomial
-- ideals chapter give the wrong answer.  Here's the difference:

    --  i59 : IP(A, w2, b2)

    -- -o59 = | 2 0 1 2 0 |
    -- +o59 = | 0 2 3 0 0 |

    --                 1        5
    --  o59 : Matrix ZZ  <--- ZZ

S = QQ[x_1..x_5, Weights => {2,3,5,7,11}];
J = ideal(-x_2^3+x_1^2*x_3,x_1^3*x_4-x_2^4,x_1^4*x_5-x_2^5)
K = saturate(J,x_1)					    -- the answer
K12 = ideal(x_2^3-x_1^2*x_3,x_1^3*x_4-x_1^2*x_2*x_3,x_1^4*x_5-x_1^2*x_2^2*x_3) -- the old answer from 1.2 and before 10/14/2009, wrong, proof below
K13 = ideal(x_1*x_4-x_2*x_3,x_1*x_5-x_2*x_4,x_2*x_5-x_3^2,x_3*x_5-x_4^2,x_2^3-x_1^2*x_3) -- the new answer from 10/16/2009, perhaps right
-- plausibility tests:
n = 4;					     -- this seems large enough for now
assert isSubset(J,K)
assert isSubset(x_1^n*K, J)
assert( K12 == J )
assert isSubset(J,K13)
assert isSubset(x_1^n*K13, J)
-- comparison
assert( isSubset(K12,K13) and K12 != K13 ) -- the new answer, K13, is bigger, hence more of a saturation, hence better
-- compare with what 1.3 says on 10/15/2009, which might be right
assert isSubset(K13,K) -- we should keep doing at least as well (this is the first assertion failure for 1.2 and 1.1)
assert( K == K13 )     -- if we do better, get notified (this works for 1.3 but not for 1.2 and 1.1)
-- everything above this point works for 1.3

-- Here's a proof that K13 is primary, hence right if it contains no power of x_1
pd = select(primaryDecomposition J, I -> x_1^n % I != 0);
assert( length pd == 1 )
assert( K13 == intersect pd )

-- # Local Variables:
-- # compile-command: "make -k -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test saturate5.out "
-- # End:
