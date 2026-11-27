-- findRootPower and joinCyclotomic on inputs that need care.
-- 1. findRootPower stops, with an error, for a generator that is not a root
--    of unity.
Sa = QQ[getSymbol "a"];
K = toField(Sa/(Sa_0^2 - 2));
assert(try (findRootPower(K[getSymbol "x"]); false) else true);
-- also for a field instead of a ring, as in the package's own test
assert(findRootPower cyclotomicField 7 == 7);
-- 2. joinCyclotomic keeps the degrees and the monomial order.
F = cyclotomicField 3; G = cyclotomicField 4;
A = F[getSymbol "t", Degrees => {2}, MonomialOrder => Lex];
B = G[getSymbol "t", Degrees => {2}, MonomialOrder => Lex];
I = ideal(A_0 - F_0^2); J = ideal(B_0^2 - G_0);
L = joinCyclotomic {I, J};
assert(degrees ring L#0 == {{2}});
assert((options ring L#0).MonomialOrder === (options A).MonomialOrder);
-- 3. joinCyclotomic makes one ring for every ring and field.
assert(ring first joinCyclotomic {I, J} === ring first joinCyclotomic {I, J});
-- 4. joinCyclotomic raises an error for rings with different variables.
U = F[getSymbol "u"];
assert(try (joinCyclotomic {ideal U_0, I}; false) else true);
-- ideals over QQ in different rings with the same variables end up in one ring
P = QQ[getSymbol "t"]; P' = QQ[getSymbol "t"];
M = joinCyclotomic {ideal(P_0 - 1), ideal(P'_0 + 1)};
assert(ring M#0 === ring M#1);
-- the error for a bad order
assert(try (cyclotomicField 0; false) else true);
