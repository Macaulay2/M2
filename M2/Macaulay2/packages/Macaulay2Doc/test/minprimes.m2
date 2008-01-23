--status: This test points out that homogeneity can be destroyed during the integral
--status: closure algorithm.  This might not be an error, but should be checked
--status: by Amelia.

debugLevel = 1 -- I installed some error detection in minimalPrimes.  This turns it on.
errorDepth = 0
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
F = ICmap R						    -- this triggers the message

R = QQ[w_1, w_2, x, y, z, Degrees => {{3}, {3}, {1}, {1}, {1}}, MonomialOrder => GRevLex => 5]
I = ideal (x,y^2*z+z^3,w_2*z,w_1*z,w_2^2,w_1*w_2,w_1^2)
assert isHomogeneous I
(flagInhomogeneity = true; minimalPrimes I) -- shouldn't I be able to get this to give an error message, too?
