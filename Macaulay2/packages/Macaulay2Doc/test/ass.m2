--8/13/02
--author: Jessica Sidman

--Goal: Check the associated primes function in Macaulay2, version 0.9.  The
--ideal P is the product of three linear ideals.  By a recent paper of Conca
--and Herzog "Castelnuovo-Mumford regularity of products of ideals,"
--Preprint, we already know a formula for the primary decomposition.

S = ZZ/103[x_1 ..x_5]
m = ideal(x_1 ..x_5)


--The three linear ideals
I1 = ideal(x_1, x_2, x_3)
I2 = ideal(x_1, x_2, x_4)
I3 = ideal(x_1, x_5)

--The product ideal.
P = I1*I2*I3

--Macaulay2 says that P has associated primes I1, I2, I3, and I1+I2.
associatedPrimes P

--A result of Conca and Herzog says that if the associated primes are as
--listed above, the following should be a primary decomposition of P.

test1 = intersect(I1, I2, I3, (I1+I2)*(I1+I2))

--However, the following test shows that this cannot be the case.
isSubset(test1, P)


--Using the same result of Conca and Herzog we can compute a correct primary decomposition of P.
test2 = intersect(test1, (I1+I3)*(I1+I3), (I2+I3)*(I2+I3), m*m*m)
isSubset(test2, P)
isSubset(P, test2)

--We can also find elements of R = S/P that have annihilators that Macaulay2
--does not list in the output from the command "associatedPrimes P."
R = S/P
--x_1*x_3 has annihilator I2+I3
annihilator (x_1*x_3)

--x_1*x_1 has annihilator m
annihilator (x_1*x_1)

--x_1*x_4 has annihiltor I1+I3.
annihilator (x_1*x_4)

end
-- Mike's playing code:
S = ZZ/103[x_1 ..x_5]
I1 = ideal(x_1, x_2, x_3)
I2 = ideal(x_1, x_2, x_4)
I3 = ideal(x_1, x_5)
P = I1*I2*I3
C = primaryDecomposition(P, Strategy=>ShimoyamaYokoyama)

J2 = topComponents P
L2 = P : J2
primaryDecomposition(L2,Strategy=>ShimoyamaYokoyama)
C2 = apply(C, p -> p : J2)
intersect C2 == L2
C2/toString/print
