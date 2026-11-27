-- isBinomial(I, GroebnerFree => true) and binomialBasis: the algorithm of
-- Conradi and Kahle ([CK15] Algorithm 3.3 and Recipe 4.5), an opt-in option,
-- after the code of Zhuang and Iosif for the unreleased version 1.3.
-- Checked against the reduced Groebner basis.
debug Binomials;
checkOne = I -> (
    gb0 := isBinomial I;
    assert(isBinomial(I, GroebnerFree => true) == gb0);
    bb := binomialBasis I;
    if gb0 then assert(bb =!= null and ideal bb == I and all(flatten entries bb, g -> #terms g <= 2))
    else assert(bb === null));
R = QQ[x,y,z];
-- edge cases: zero ideal, unit ideal
scan({ideal 0_R, ideal(0_R, 0_R), ideal 1_R, ideal(0_R, x - y)}, checkOne);
assert(binomialBasis ideal 0_R == map(R^1, R^0, 0));
-- [CK15] Example 2.8: decided by row reduction of the generators (Recipe 4.5,
-- step 1), without a Groebner basis
I = ideal(x - y + x^2 + y^2 + z^2, x^2 + y^2 + z^2);
assert(first isBinomialGroebnerFree I);
checkOne I;
-- a ring variable named ttt: the homogenizing variable is a variable of a
-- new ring and cannot clash with the names of the input ring
S = QQ[getSymbol "ttt", getSymbol "u"];
checkOne ideal(S_0^2 - 1, S_0 * S_1 - S_1);
-- a non-standard grading: the Groebner basis decides
Q = QQ[getSymbol "a", getSymbol "b", Degrees => {1, 2}];
assert(isBinomial(ideal(Q_0^2 - Q_1), GroebnerFree => true));
-- random ideals: binomial, binomial disguised by row operations, binomial
-- plus a random polynomial, homogeneous and not, over QQ and ZZ/32003
setRandomSeed 2026;
scan({QQ, ZZ/32003}, K -> (
    T := K[getSymbol "a", getSymbol "b", getSymbol "c", getSymbol "d"];
    scan(30, k -> (
        homog := odd k;
        B := randomBinomialIdeal(T, 3, 2, 2, homog);
        gs := B_*;
        disguised := apply(#gs, i -> gs#i + (if i + 1 < #gs then random(K) * gs#(i + 1) else 0));
        checkOne B;
        checkOne ideal disguised;
        f := random(if homog then first degree first gs else 1, T);
        checkOne(B + ideal f)))));
