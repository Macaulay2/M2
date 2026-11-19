-- randomBinomialIdeal: the documented properties of the generator.
R = QQ[a..h];
setRandomSeed 11;
for trial from 1 to 30 do (
    n := random(1, 5); d := random(1, 4); w := random(1, 8); h := (random 2 == 0);
    I := randomBinomialIdeal(R, n, d, w, h);
    assert(instance(I, Ideal) and ring I === R);
    assert(numgens I <= n);                                   -- version 1.2.1 made n + 1 binomials
    assert all(I_*, g -> #terms g <= 2));
-- the binomials themselves, when they are few enough to be minimal generators
S = QQ[x_1..x_10];
setRandomSeed 5;
exponentsSeen = new MutableHashTable;
for trial from 1 to 60 do (
    I := randomBinomialIdeal(S, 1, 3, 4, false);
    g := I_0;
    assert(#terms g == 2);
    e := first exponents g - last exponents g;
    assert(#select(e, t -> t != 0) <= 4 and all(e, t -> abs t <= 3));
    scan(e, t -> if t != 0 then exponentsSeen#t = true));
assert(sort keys exponentsSeen == {-3, -2, -1, 1, 2, 3});   -- version 1.2.1 never produced -1
-- with the last argument true both monomials have equally many variables
for trial from 1 to 20 do (
    I := randomBinomialIdeal(S, 1, 2, 4, true);
    assert(apply(exponents I_0, e -> #select(e, t -> t != 0)) == {2, 2}));
-- odd widths in balanced mode, also when there are no more variables
T = QQ[u,v,w];
assert instance(randomBinomialIdeal(T, 2, 2, 3, true), Ideal);            -- an error in version 1.2.1
assert instance(randomBinomialIdeal(S, 2, 2, 3, true), Ideal);
assert(try (randomBinomialIdeal(T, 2, 2, 4, false); false) else true);    -- more variables asked for than there are
-- the same seed gives the same ideal
setRandomSeed 3; I1 = randomBinomialIdeal(R, 4, 3, 4, true);
setRandomSeed 3; I2 = randomBinomialIdeal(R, 4, 3, 4, true);
assert(I1 == I2);
