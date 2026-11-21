-- Fraction fields as coefficient fields (decided by Thomas on 2026-11-02):
-- with parameters of positive degree Macaulay2's saturate fails, so the
-- package raises a clear error; with parameters of degree 0 it works.
A = QQ[getSymbol "a", getSymbol "b"];
K = frac A; R = K[getSymbol "x", getSymbol "y"];
I = ideal(R_0 - promote(A_0, R) * R_1);
msg = try (binomialIsPrime I; "no error") else "error";
assert(msg == "error");
assert(try (binomialPrimaryDecomposition I; false) else true);
A0 = QQ[getSymbol "a", getSymbol "b", Degrees => {0, 0}];
K0 = frac A0; R0 = K0[getSymbol "x", getSymbol "y"];
I0 = ideal(R0_0 - promote(A0_0, R0) * R0_1);
assert(binomialIsPrime I0);
assert(binomialPrimaryDecomposition I0 == {I0});
assert(binomialRadical ideal(R0_0^2, R0_1 - promote(A0_1, R0)) == ideal(R0_0, R0_1 - promote(A0_1, R0)));
