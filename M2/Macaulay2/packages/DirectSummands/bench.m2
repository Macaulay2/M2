restart
debug needsPackage "DirectSummands"
debugLevel = 1

kk = ZZ/3
R = quotient Grassmannian(1,3, CoefficientRing => kk)
X = Proj R
elapsedTime M = module frobeniusPushforward(1, OO_X); -- <1s in char 2 & 3
elapsedTime L = summands M;

tally apply(L, N -> (rank N, degrees N))

-*
Timing results:
 q   | F_* | Tot | End |basis| misc
-----------------------------------
 2   | <1s | <1s |     |     |
 4   | ~6s |     |     |     |
 3   | <1s | 43s | 13s | 2.3 |
 5   | 64s |     |     |     |

ZZ/2 q = 2^1 rk 16: 1:0 14:1 1:2
ZZ/2 q = 2^2 rk 256

ZZ/3 q = 3^1 rk 81: 1:0 44:1 20:2 8:rk 2 bundles


*-


kk = ZZ/3
R = quotient Grassmannian(2,5, CoefficientRing => kk)
X = Proj R
elapsedTime M = module frobeniusPushforward(1, OO_X); -- <1s
rank M
elapsedTime L = summands M;
