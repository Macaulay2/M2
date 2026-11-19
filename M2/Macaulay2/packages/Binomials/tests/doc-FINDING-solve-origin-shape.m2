-- doc-FINDING-solve-origin-shape.m2
-- FINDING: binomialSolve returns an extra level of list nesting for some
-- ideals whose only solution is the origin.
-- Documentation: "the list of points in the zero locus of I"; the documented
-- examples return lists of coordinate lists, e.g. {{1,1,1,1},{1,1,1,-1},{0,0,0,0}}.
-- Observed (R = QQ[x,y]):
--   binomialSolve ideal(x,y)        -> {{{0, 0}}}
--   binomialSolve ideal(x^2,y^2)    -> {{{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}}
--   binomialSolve ideal(x^2-x*y,y^3)-> {{{0, 0}}, ... 6 times}
-- Expected: {{0, 0}} (a list of points, each a list of numgens R coordinates).

R = QQ[x,y];
for I in {ideal(x,y), ideal(x^2,y^2), ideal(x^2-x*y, y^3)} do (
    sols := binomialSolve I;
    assert(#sols > 0);
    assert(all(sols, p -> instance(p, List) and #p == 2 and all(p, c -> not instance(c, List))));
    assert(all(sols, p -> p#0 == 0 and p#1 == 0)));
