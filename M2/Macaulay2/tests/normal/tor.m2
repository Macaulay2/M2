-- copyright 1995 Michael E. Stillman
-- several tests of tensor products and Tor
-- many of these examples were created by David Eisenbud

-- Test 1.  Checking that Tor_i(M,k) and Tor_i(k,M) both give
-- the graded betti numbers of M.

R = ZZ/101[a..d]
k = cokernel vars R
M = cokernel matrix {{a*d - b*c, a^2*c - b^3, c^3 - b*d^2, a*c^2 - b^2*d}}

T0 = Tor_0(M,k)
S0 = Tor_0(k,M)
T1 = Tor_1(M,k)
S1 = Tor_1(k,M)
T2 = Tor_2(M,k)
S2 = Tor_2(k,M)
T3 = Tor_3(M,k)
S3 = Tor_3(k,M)
T4 = Tor_4(M,k)
S4 = Tor_4(k,M)

T = (degreesRing R)_0

assert(poincare T0 ==             (1-T)^4)
assert(poincare T1 == (T^2+3*T^3)*(1-T)^4)
assert(poincare T2 == 4*T^4*      (1-T)^4)
assert(poincare T3 ==  T^5 *      (1-T)^4)
assert(poincare T4 ==  0)

assert(poincare T0 == poincare S0)
assert(poincare T1 == poincare S1)
assert(poincare T2 == poincare S2)
assert(poincare T3 == poincare S3)
assert(poincare T4 == poincare S4)

-- notice that degree Tor_i(M,k) gives the i th betti number of M,
-- as does Tor_i(k,M), and the graded betti numbers can be seen by using
-- 'see target prune Tor_i(k,M)'
-- or by using, for example

hf = poincare T2;
if hf != 0 then while substitute(hf,{T=>1}) == 0 do hf = hf // (1-T);
hf

-- Test 2.  Intersection multiplicity according to Serre
-- The intersection of two planes in 4-space meeting another such.
-- The multiplicity should be 4.  Serre's technique says that this
-- number should be the alternating sum of the 'degree Tor_i(R/I,R/J)':

R = ZZ/101[a..d]
I = generators intersect(image matrix {{a,b}}, image matrix {{c,d}});
J = generators intersect(image matrix {{a-c-b, b-d}}, image matrix {{a-d, b-c}});

U0 = Tor_0(cokernel I, cokernel J);
U1 = Tor_1(cokernel I, cokernel J);
U2 = Tor_2(cokernel I, cokernel J);
U3 = Tor_3(cokernel I, cokernel J);
U4 = Tor_4(cokernel I, cokernel J)

U0 = prune U0
assert( numgens target presentation U0 == 1 )
assert( numgens source presentation U0 == 8 )

U1 = prune U1
assert( numgens target presentation U1 == 4 )
assert( numgens source presentation U1 == 16 )

U2 = prune U2
assert( numgens target presentation U2 == 1 )
assert( numgens source presentation U2 == 4 )

U3 = prune U3
assert( numgens target presentation U3 == 0 )
assert( numgens source presentation U3 == 0 )

U4 = prune U4
assert( numgens target presentation U4 == 0 )
assert( numgens source presentation U4 == 0 )

assert( degree U0 == 7 )
assert( degree U1 == 4 )
assert( degree U2 == 1 )
assert( degree U3 == 0 )
assert( degree U4 == 0 )
