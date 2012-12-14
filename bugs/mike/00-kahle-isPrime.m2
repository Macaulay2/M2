kk = ZZ/32003 -- this one just seems to go off to never never land
kk = QQ -- this is the one that crashes early
R = kk[x1,x2,x3,x4,x5,x6]
I = ideal(
    x1^2*x2^2*x5^8*x6^8-x3^4*x4^4,
    x2^8*x3^2*x4^8*x5^2-x1^4*x6^4,
    x1^8*x3^8*x4^2*x6^2-x2^4*x5^4)
gbTrace=3
isPrime I

end
-- mike trying to debug this (done sometime in November 2012)
minimalPrimes I
code methods isPrime
code methods minimalPrimes

restart
kk = QQ
R = kk[x1,x2,x3,x4,x5,x6]
I = ideal(x1^2*x2^2*x5^8*x6^8-x3^4*x4^4,x2^8*x3^2*x4^8*x5^2-x1^4*x6^4,x1^8*x3^8*x4^2*x6^2-x2^4*x5^4)
debug Core
m = gens I
rawCharSeries raw m
