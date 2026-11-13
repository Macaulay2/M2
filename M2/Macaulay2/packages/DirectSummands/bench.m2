restart
debug needsPackage "DirectSummands"
debugLevel = 1
errorDepth = 2

kk = ZZ/5
R = quotient Grassmannian(1,3, CoefficientRing => kk)
X = Proj R
elapsedTime M = module frobeniusPushforward(1, OO_X); -- <1s in char 2 & 3
rank M
elapsedTime L = summands(M, Verbose => true);
tally apply(summands M, N -> (rank N, degrees N))

N = last M.cache#"FreeSummands";
elapsedTime projs = findProjectors N;
elapsedTime L0 = flatten(summands \ prune \ coker \ projs);
elapsedTime L = summands(keys tallySummands L0, N, Verbose => true);
M.cache.summands = drop(M.cache#"FreeSummands", -1) | L;

-* Timing results:
-----------------------------------
 q   | F_* | Tot | End | misc
-----------------------------------
 2   | <1s | <1s | <1s | noether
 4   | ~6s | 33m |  8m | Fields
 3   | <1s | 10s | <1s | noether
 5   | 43s | ~3h | ~3h | noether (once End is computed, pretty fast with some manual work)
 7   |     |     |     |
 9   |     |     |     |
-----------------------------------
field	q	rk	free summands		non-free summands
-----------------------------------
ZZ/2	2^1	 16	1:0  14:1   1:2
ZZ/2	2^2	256	1:0  99:1  99:2  1:3	28:rk 2 bundles
ZZ/3	3^1	 81	1:0  44:1  20:2		8: rk 2 bundles
ZZ/3	3^2
ZZ/5	5^1	625	1:0 190:1 300:2  6:3	64:rk 2 bundles
ZZ/7	7^1
*-


kk = ZZ/2
R = quotient Grassmannian(2,5, CoefficientRing => kk)
X = Proj R
elapsedTime M = module frobeniusPushforward(1, OO_X); -- ~20min in char 2
rank M
elapsedTime L = summands(M, Verbose => true);
tally apply(L, N -> (rank N, degrees N))

-* Timing results:
-----------------------------------
 q   | F_* | Tot | End | misc
-----------------------------------
 2   | 20m |     |     | Fields
-----------------------------------
field	q	rk	free summands		non-free summands
-----------------------------------
ZZ/2	2^1	512	(312)			(200)
*-
