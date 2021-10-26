TEST ///
-- Test cayley_polytope/1.poly
-- Checking ehrhart and volume
verticesP0 = matrix {{0,1,0,1},{0,0,1,1}};
raysP0 = map(QQ^2, QQ^0, 0);
linealityP0 = map(QQ^2, QQ^0, 0);
P0 = convexHull(verticesP0,raysP0,linealityP0);
ineqlhsPd0 = matrix {{0,-1},{-1,0},{0,1},{1,0}};
ineqrhsPd0 = matrix {{0},{0},{1},{1}};
eqlhsPd0 = map(QQ^0, QQ^2, 0);
eqrhsPd0 = map(QQ^0, QQ^1, 0);
Pd0 = polyhedronFromHData(ineqlhsPd0, ineqrhsPd0, eqlhsPd0, eqrhsPd0);
assert(volume P0 == 1)
assert(volume Pd0 == 1)
LE0 = reverse {1,2,1};
etest0 = vector apply(LE0, e->promote(e, QQ));
ep0 = vector apply(flatten entries (coefficients ehrhart P0)#1, e->lift(e, QQ));
epd0 = vector apply(flatten entries (coefficients ehrhart Pd0)#1, e->lift(e, QQ));
assert(ep0 == etest0)
assert(epd0 == etest0)

-- Test cayley_polytope/4.poly
-- Checking ehrhart and volume
verticesP1 = matrix {{3},{4}};
raysP1 = map(QQ^2, QQ^0, 0);
linealityP1 = map(QQ^2, QQ^0, 0);
P1 = convexHull(verticesP1,raysP1,linealityP1);
ineqlhsPd1 = matrix {{0,0}};
ineqrhsPd1 = matrix {{1}};
eqlhsPd1 = matrix {{-1,0},{0,-1}};
eqrhsPd1 = matrix {{-3},{-4}};
Pd1 = polyhedronFromHData(ineqlhsPd1, ineqrhsPd1, eqlhsPd1, eqrhsPd1);
assert(volume P1 == 1)
assert(volume Pd1 == 1)
LE1 = reverse {1};
etest1 = vector apply(LE1, e->promote(e, QQ));
ep1 = vector apply(flatten entries (coefficients ehrhart P1)#1, e->lift(e, QQ));
epd1 = vector apply(flatten entries (coefficients ehrhart Pd1)#1, e->lift(e, QQ));
assert(ep1 == etest1)
assert(epd1 == etest1)

-- Test cayley_polytope/c2.poly
-- Checking ehrhart and volume
verticesP2 = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0}};
raysP2 = map(QQ^4, QQ^0, 0);
linealityP2 = map(QQ^4, QQ^0, 0);
P2 = convexHull(verticesP2,raysP2,linealityP2);
ineqlhsPd2 = matrix {{0,0,-1,0},{-1,0,0,0},{0,-1,0,0},{1,1,-1,-1},{0,0,0,-1},{0,1,0,0},{1,0,0,0},{0,0,1,1}};
ineqrhsPd2 = matrix {{0},{0},{0},{1},{0},{1},{1},{1}};
eqlhsPd2 = map(QQ^0, QQ^4, 0);
eqrhsPd2 = map(QQ^0, QQ^1, 0);
Pd2 = polyhedronFromHData(ineqlhsPd2, ineqrhsPd2, eqlhsPd2, eqrhsPd2);
assert(volume P2 == 11/24)
assert(volume Pd2 == 11/24)
LE2 = reverse {1,13/4,97/24,9/4,11/24};
etest2 = vector apply(LE2, e->promote(e, QQ));
ep2 = vector apply(flatten entries (coefficients ehrhart P2)#1, e->lift(e, QQ));
epd2 = vector apply(flatten entries (coefficients ehrhart Pd2)#1, e->lift(e, QQ));
assert(ep2 == etest2)
assert(epd2 == etest2)

-- Test cayley_polytope/c1.poly
-- Checking ehrhart and volume
verticesP3 = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0},{0,0,0,0,0,0,0,0,1,1,1}};
raysP3 = map(QQ^5, QQ^0, 0);
linealityP3 = map(QQ^5, QQ^0, 0);
P3 = convexHull(verticesP3,raysP3,linealityP3);
ineqlhsPd3 = matrix {{0,0,-1,0,0},{-1,0,0,0,0},{0,-1,0,0,0},{1,1,-1,-1,0},{0,0,0,-1,0},{0,1,0,0,0},{1,0,0,0,0},{0,0,1,1,0}};
ineqrhsPd3 = matrix {{0},{0},{0},{1},{0},{1},{1},{1}};
eqlhsPd3 = matrix {{0,0,-1,-1,-1}};
eqrhsPd3 = matrix {{-1}};
Pd3 = polyhedronFromHData(ineqlhsPd3, ineqrhsPd3, eqlhsPd3, eqrhsPd3);
assert(volume P3 == 11/24)
assert(volume Pd3 == 11/24)
LE3 = reverse {1,13/4,97/24,9/4,11/24};
etest3 = vector apply(LE3, e->promote(e, QQ));
ep3 = vector apply(flatten entries (coefficients ehrhart P3)#1, e->lift(e, QQ));
epd3 = vector apply(flatten entries (coefficients ehrhart Pd3)#1, e->lift(e, QQ));
assert(ep3 == etest3)
assert(epd3 == etest3)

-- Test cayley_polytope/c4.poly
-- Checking ehrhart and volume
verticesP4 = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP4 = map(QQ^5, QQ^0, 0);
linealityP4 = map(QQ^5, QQ^0, 0);
P4 = convexHull(verticesP4,raysP4,linealityP4);
ineqlhsPd4 = matrix {{-1,0,-3,-2,-2},{0,-1,-4,-4,-4},{1,0,2,2,2},{0,1,3,4,4},{0,0,0,0,-1},{0,0,0,-1,0},{0,0,1,1,1}};
ineqrhsPd4 = matrix {{-3},{-4},{3},{4},{0},{0},{1}};
eqlhsPd4 = map(QQ^0, QQ^5, 0);
eqrhsPd4 = map(QQ^0, QQ^1, 0);
Pd4 = polyhedronFromHData(ineqlhsPd4, ineqrhsPd4, eqlhsPd4, eqrhsPd4);
assert(volume P4 == 1/60)
assert(volume Pd4 == 1/60)
LE4 = reverse {1,149/60,55/24,1,5/24,1/60};
etest4 = vector apply(LE4, e->promote(e, QQ));
ep4 = vector apply(flatten entries (coefficients ehrhart P4)#1, e->lift(e, QQ));
epd4 = vector apply(flatten entries (coefficients ehrhart Pd4)#1, e->lift(e, QQ));
assert(ep4 == etest4)
assert(epd4 == etest4)

-- Test cayley_polytope/2.poly
-- Checking ehrhart and volume
verticesP5 = matrix {{0,1,0},{0,0,1}};
raysP5 = map(QQ^2, QQ^0, 0);
linealityP5 = map(QQ^2, QQ^0, 0);
P5 = convexHull(verticesP5,raysP5,linealityP5);
ineqlhsPd5 = matrix {{1,1},{-1,0},{0,-1}};
ineqrhsPd5 = matrix {{1},{0},{0}};
eqlhsPd5 = map(QQ^0, QQ^2, 0);
eqrhsPd5 = map(QQ^0, QQ^1, 0);
Pd5 = polyhedronFromHData(ineqlhsPd5, ineqrhsPd5, eqlhsPd5, eqrhsPd5);
assert(volume P5 == 1/2)
assert(volume Pd5 == 1/2)
LE5 = reverse {1,3/2,1/2};
etest5 = vector apply(LE5, e->promote(e, QQ));
ep5 = vector apply(flatten entries (coefficients ehrhart P5)#1, e->lift(e, QQ));
epd5 = vector apply(flatten entries (coefficients ehrhart Pd5)#1, e->lift(e, QQ));
assert(ep5 == etest5)
assert(epd5 == etest5)

-- Test cayley_polytope/c3.poly
-- Checking ehrhart and volume
verticesP6 = matrix {{0,1,0,1,1,3},{0,0,1,1,0,4},{1,1,1,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}};
raysP6 = map(QQ^5, QQ^0, 0);
linealityP6 = map(QQ^5, QQ^0, 0);
P6 = convexHull(verticesP6,raysP6,linealityP6);
ineqlhsPd6 = matrix {{-1,0,-3,-2,0},{0,-1,-4,-4,0},{1,0,2,2,0},{0,1,3,4,0},{0,0,0,-1,0},{0,0,1,1,0}};
ineqrhsPd6 = matrix {{-3},{-4},{3},{4},{0},{1}};
eqlhsPd6 = matrix {{0,0,-1,-1,-1}};
eqrhsPd6 = matrix {{-1}};
Pd6 = polyhedronFromHData(ineqlhsPd6, ineqrhsPd6, eqlhsPd6, eqrhsPd6);
assert(volume P6 == 1/12)
assert(volume Pd6 == 1/12)
LE6 = reverse {1,7/3,23/12,2/3,1/12};
etest6 = vector apply(LE6, e->promote(e, QQ));
ep6 = vector apply(flatten entries (coefficients ehrhart P6)#1, e->lift(e, QQ));
epd6 = vector apply(flatten entries (coefficients ehrhart Pd6)#1, e->lift(e, QQ));
assert(ep6 == etest6)
assert(epd6 == etest6)

-- Test cayley_polytope/3.poly
-- Checking ehrhart and volume
verticesP7 = matrix {{1},{0}};
raysP7 = map(QQ^2, QQ^0, 0);
linealityP7 = map(QQ^2, QQ^0, 0);
P7 = convexHull(verticesP7,raysP7,linealityP7);
ineqlhsPd7 = matrix {{0,0}};
ineqrhsPd7 = matrix {{1}};
eqlhsPd7 = matrix {{-1,0},{0,-1}};
eqrhsPd7 = matrix {{-1},{0}};
Pd7 = polyhedronFromHData(ineqlhsPd7, ineqrhsPd7, eqlhsPd7, eqrhsPd7);
assert(volume P7 == 1)
assert(volume Pd7 == 1)
LE7 = reverse {1};
etest7 = vector apply(LE7, e->promote(e, QQ));
ep7 = vector apply(flatten entries (coefficients ehrhart P7)#1, e->lift(e, QQ));
epd7 = vector apply(flatten entries (coefficients ehrhart Pd7)#1, e->lift(e, QQ));
assert(ep7 == etest7)
assert(epd7 == etest7)

-- Test print_constraints/1.poly
-- Checking ehrhart and volume
verticesP8 = matrix {{1,1,1,0,0,0},{1,0,0,1,1,0},{0,1,0,1,0,1},{0,0,1,0,1,1}};
raysP8 = map(QQ^4, QQ^0, 0);
linealityP8 = map(QQ^4, QQ^0, 0);
P8 = convexHull(verticesP8,raysP8,linealityP8);
ineqlhsPd8 = matrix {{0,-1,0,0},{0,0,1,0},{-1,0,0,0},{-1,-1,-1,0},{0,1,0,0},{0,0,-1,0},{1,1,1,0},{1,0,0,0}};
ineqrhsPd8 = matrix {{0},{1},{0},{-1},{1},{0},{2},{1}};
eqlhsPd8 = matrix {{-1,-1,-1,-1}};
eqrhsPd8 = matrix {{-2}};
Pd8 = polyhedronFromHData(ineqlhsPd8, ineqrhsPd8, eqlhsPd8, eqrhsPd8);
assert(volume P8 == 2/3)
assert(volume Pd8 == 2/3)
LE8 = reverse {1,7/3,2,2/3};
etest8 = vector apply(LE8, e->promote(e, QQ));
ep8 = vector apply(flatten entries (coefficients ehrhart P8)#1, e->lift(e, QQ));
epd8 = vector apply(flatten entries (coefficients ehrhart Pd8)#1, e->lift(e, QQ));
assert(ep8 == etest8)
assert(epd8 == etest8)

-- Test delpezzo/1.poly
-- Checking ehrhart and volume
verticesP9 = matrix {{1,0,0,0,-1,0,0,0,1,-1},{0,1,0,0,0,-1,0,0,1,-1},{0,0,1,0,0,0,-1,0,1,-1},{0,0,0,1,0,0,0,-1,1,-1}};
raysP9 = map(QQ^4, QQ^0, 0);
linealityP9 = map(QQ^4, QQ^0, 0);
P9 = convexHull(verticesP9,raysP9,linealityP9);
ineqlhsPd9 = matrix {{1,1,-1,-1},{1,0,-1,-1},{1,-1,1,-1},{1,-1,0,-1},{1,-1,-1,1},{1,-1,-1,0},{-1,-1,1,1},{-1,-1,1,0},{0,-1,1,-1},{-1,0,1,-1},{-1,0,-1,1},{0,-1,-1,1},{-1,-1,0,1},{0,1,-1,-1},{-1,1,-1,0},{-1,1,-1,1},{-1,1,0,-1},{-1,1,1,-1},{-1,0,1,1},{-1,1,0,1},{-1,1,1,0},{0,1,1,-1},{0,1,-1,1},{0,-1,1,1},{1,-1,0,1},{1,-1,1,0},{1,0,-1,1},{1,1,-1,0},{1,0,1,-1},{1,1,0,-1}};
ineqrhsPd9 = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd9 = map(QQ^0, QQ^4, 0);
eqrhsPd9 = map(QQ^0, QQ^1, 0);
Pd9 = polyhedronFromHData(ineqlhsPd9, ineqrhsPd9, eqlhsPd9, eqrhsPd9);
assert(volume P9 == 5/4)
assert(volume Pd9 == 5/4)
LE9 = reverse {1,5/2,15/4,5/2,5/4};
etest9 = vector apply(LE9, e->promote(e, QQ));
ep9 = vector apply(flatten entries (coefficients ehrhart P9)#1, e->lift(e, QQ));
epd9 = vector apply(flatten entries (coefficients ehrhart Pd9)#1, e->lift(e, QQ));
assert(ep9 == etest9)
assert(epd9 == etest9)

///

TEST ///
-- Test delpezzo/2.poly
-- Checking ehrhart and volume
verticesP10 = matrix {{1,0,0,0,0,-1,0,0,0,0,1},{0,1,0,0,0,0,-1,0,0,0,1},{0,0,1,0,0,0,0,-1,0,0,1},{0,0,0,1,0,0,0,0,-1,0,1},{0,0,0,0,1,0,0,0,0,-1,1}};
raysP10 = map(QQ^5, QQ^0, 0);
linealityP10 = map(QQ^5, QQ^0, 0);
P10 = convexHull(verticesP10,raysP10,linealityP10);
ineqlhsPd10 = matrix {{1,1,-1,-1,-1},{1,-1,1,-1,-1},{1,-1,-1,1,-1},{1,-1,-1,-1,-1},{1,-1,-1,-1,1},{-1,-1,1,1,-1},{-1,-1,1,-1,-1},{-1,-1,1,-1,1},{-1,-1,-1,-1,-1},{-1,-1,-1,-1,1},{-1,-1,-1,1,1},{-1,-1,-1,1,-1},{-1,1,-1,-1,1},{-1,1,-1,-1,-1},{-1,1,-1,1,-1},{-1,1,1,-1,-1},{-1,1,1,1,-1},{-1,1,1,-1,1},{-1,1,-1,1,1},{-1,-1,1,1,1},{1,-1,-1,1,1},{1,-1,1,-1,1},{1,-1,1,1,-1},{1,1,-1,-1,1},{1,1,-1,1,-1},{1,1,1,-1,-1}};
ineqrhsPd10 = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd10 = map(QQ^0, QQ^5, 0);
eqrhsPd10 = map(QQ^0, QQ^1, 0);
Pd10 = polyhedronFromHData(ineqlhsPd10, ineqrhsPd10, eqlhsPd10, eqrhsPd10);
assert(volume P10 == 23/60)
assert(volume Pd10 == 23/60)
LE10 = reverse {1,187/60,85/24,3,23/24,23/60};
etest10 = vector apply(LE10, e->promote(e, QQ));
ep10 = vector apply(flatten entries (coefficients ehrhart P10)#1, e->lift(e, QQ));
epd10 = vector apply(flatten entries (coefficients ehrhart Pd10)#1, e->lift(e, QQ));
assert(ep10 == etest10)
assert(epd10 == etest10)

///
