-- doc-solve-origin-shape.m2
-- binomialSolve returns a list of points, each a list of coordinates, also
-- for ideals whose only solution is the origin.
-- Documented: a list "of the points of the variety of I"; the documented
-- examples return lists of coordinate lists.
-- Cases (R = QQ[x,y]): ideal(x,y), ideal(x^2,y^2), ideal(x^2-x*y,y^3).
-- Expected: points {0, 0}, each a list of numgens R coordinates.

R = QQ[x,y];
for I in {ideal(x,y), ideal(x^2,y^2), ideal(x^2-x*y, y^3)} do (
    sols := binomialSolve I;
    assert(#sols > 0);
    assert(all(sols, p -> instance(p, List) and #p == 2 and all(p, c -> not instance(c, List))));
    assert(all(sols, p -> p#0 == 0 and p#1 == 0)));
