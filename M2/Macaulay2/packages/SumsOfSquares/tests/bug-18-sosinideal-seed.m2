-- BUGS.md 18: sosInIdeal depends on the random seed and misses SOS in the
-- ideal.  Decided (LOG.md 1.7 item 3): a basis of the multiplier parameters
-- and trace Q = 1, so that the result is deterministic and an SOS of degree
-- D in the ideal is found when one exists.
R = QQ[x,y];
grams = for seed in 0..4 list (
    setRandomSeed seed;
    (sol, mult) := sosInIdeal(matrix{{x - y, x^2 - x*y}}, 2);
    assert(sol#GramMatrix =!= null);
    assert(matrix{{x - y, x^2 - x*y}} * mult == sosPoly sol);
    sol#GramMatrix);
assert(#unique grams == 1)
-- (x-y)^2 = x*(x-y) - y*(x-y) lies in the ideal with multipliers of degree 1
for seed in 0..4 do (
    setRandomSeed seed;
    (sol, mult) := sosInIdeal(matrix{{x^2 - y^2, x - y}}, 2);
    assert(sol#GramMatrix =!= null and value sosPoly sol != 0);
    assert(matrix{{x^2 - y^2, x - y}} * mult == sosPoly sol))
