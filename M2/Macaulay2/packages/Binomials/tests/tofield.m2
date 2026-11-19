-- Binomial ideals over a finite field made with toField(ZZ/p[c]/(f)).
-- The results equal those over the Galois
-- field GF(ZZ/p[c]/(f)) of the same presentation, moved by c -> c; results
-- over an extension GF(5^m) live over the same field in both cases.
needsPackage "Cyclotomic";   -- cyclotomicInclusion
S5 = (ZZ/5)[getSymbol "c"];
A = S5/(S5_0^2 - 2);
K = toField A; G = GF A;
RK = K[getSymbol "x", getSymbol "y"]; RG = G[getSymbol "x", getSymbol "y"];
toRG = map(RG, RK, gens RG | {G_0});
cK = promote(K_0, RK); cG = promote(G_0, RG);
moveTo = (J, H) -> if ring J === RK then toRG J else sub(J, ring H);
sameLists = (LK, LG) -> #LK == #LG and all(#LK, i -> moveTo(LK#i, LG#i) == LG#i);
idealPairs = {
    (ideal(RK_0^4 - cK * RK_1^4), ideal(RG_0^4 - cG * RG_1^4)),
    (ideal(RK_0^3 - RK_1^3), ideal(RG_0^3 - RG_1^3)),
    (ideal(RK_0^2 - cK, RK_1^13 - 1), ideal(RG_0^2 - cG, RG_1^13 - 1)),
    (ideal(RK_0 * RK_1 - cK, RK_0^2 * (RK_1 - 1)), ideal(RG_0 * RG_1 - cG, RG_0^2 * (RG_1 - 1)))};
scan(idealPairs, IJ -> (
    (I, J) := IJ;
    pK := binomialPrimaryDecomposition I; pG := binomialPrimaryDecomposition J;
    assert sameLists(pK, pG);
    T := ring first pK;
    assert(intersect pK == (if T === RK then I else (cyclotomicInclusion(RK, T)) I));
    assert sameLists(binomialMinimalPrimes I, binomialMinimalPrimes J);
    assert sameLists(binomialAssociatedPrimes I, binomialAssociatedPrimes J);
    assert(toRG binomialRadical I == binomialRadical J)));
-- binomialSolve: 2 * 13 points with x^2 = c, y^13 = 1, in GF(5^4)
pts = binomialSolve ideal(RK_0^2 - cK, RK_1^13 - 1);
assert(#pts == 26 and #unique pts == 26);
assert(all(pts, pt -> pt#0^2 == (cyclotomicInclusion(K, ring pt#0)) K_0 and pt#1^13 == 1));
