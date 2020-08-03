restart
setRandomSeed 1
needsPackage "NumericalAlgebraicGeometry"
needsPackage "Bertini"
loadPackage ("NoetherianOperators", Reload => true)
R = QQ[x_0..x_5]
P = minors(2,matrix{{x_0,x_1,x_3,x_4},{x_1,x_2,x_4,x_5}});
f1 = x_1^4 - 2*x_0*x_1^2*x_2 + x_0^2*x_2^2 + x_1*x_2*x_3*x_4 - x_0*x_2*x_4^2 - x_1^2*x_3*x_5 + x_0*x_1*x_4*x_5
f2 = x_1^4 - 2*x_0*x_1^2*x_2 + x_0^2*x_2^2 + x_1*x_2*x_3*x_4 - x_1^2*x_4^2 - x_0*x_2*x_3*x_5 + x_0*x_1*x_4*x_5
f3 = x_2^2*x_3*x_4 - x_1*x_2*x_4^2 + x_4^4 - x_1*x_2*x_3*x_5 + x_1^2*x_4*x_5 - 2*x_3*x_4^2*x_5 + x_3^2*x_5^2
I = ideal(f1,f2,f3)
primes = minimalPrimes I

nops = new MutableList
elapsedTime nops#0 = noetherianOperators(I, primes#0)
elapsedTime nops#1 = noetherianOperators(I, primes#1)
elapsedTime nops#2 = noetherianOperators(I, primes#2)
elapsedTime nops#3 = noetherianOperators(I, primes#3)
-- This takes very long
elapsedTime nops#4 = noetherianOperators(I, primes#4)
-- The hybrid method performs much faster
elapsedTime nops#4 = hybridNoetherianOperators(I, primes#4)

netList nops#0
netList nops#1
netList nops#2
netList nops#3
netList nops#4