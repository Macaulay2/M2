TEST ///
-- Test dim: 2, ambientDim: 2, vertices: 4, facets: 4
-- Checking representation vs dual representation
verticesP0 = matrix {{0,1,0,1},{0,0,1,1}};
raysP0 = map(QQ^2, QQ^0, 0);
linealityP0 = map(QQ^2, QQ^0, 0);
P0 = convexHull(verticesP0,raysP0,linealityP0);
ineqlhsPd0 = matrix {{0,-1},{-1,0},{0,1},{1,0}};
ineqrhsPd0 = matrix {{0},{0},{1},{1}};
eqlhsPd0 = map(QQ^0, QQ^2, 0);
eqrhsPd0 = map(QQ^0, QQ^1, 0);
Pd0 = polyhedronFromHData(ineqlhsPd0, ineqrhsPd0, eqlhsPd0, eqrhsPd0);
assert(Pd0 == P0)
assert(isEmpty Pd0 === isEmpty P0)
assert(isCompact Pd0 === isCompact P0)
assert(isLatticePolytope Pd0 === isLatticePolytope P0)
assert(isNormal Pd0 === isNormal P0)
assert(numColumns vertices Pd0 == numColumns vertices P0)
assert(numColumns rays Pd0 == numColumns rays P0)
assert(numColumns linealitySpace Pd0 == numColumns linealitySpace P0)
facetsP0 = facets P0;
facetsPd0 = facets Pd0;
assert(numRows (facetsPd0#0) == numRows (facetsP0#0))
assert(numRows (facetsPd0#1) == numRows (facetsP0#1))
hyperplanesP0 = hyperplanes P0;
hyperplanesPd0 = hyperplanes Pd0;
assert(numRows (hyperplanesPd0#0) == numRows (hyperplanesP0#0))
assert(numRows (hyperplanesPd0#1) == numRows (hyperplanesP0#1))
assert(contains(P0, Pd0))
assert(contains(Pd0, P0))

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 0
-- Checking representation vs dual representation
verticesP1 = matrix {{3},{4}};
raysP1 = map(QQ^2, QQ^0, 0);
linealityP1 = map(QQ^2, QQ^0, 0);
P1 = convexHull(verticesP1,raysP1,linealityP1);
ineqlhsPd1 = matrix {{0,0}};
ineqrhsPd1 = matrix {{1}};
eqlhsPd1 = matrix {{-1,0},{0,-1}};
eqrhsPd1 = matrix {{-3},{-4}};
Pd1 = polyhedronFromHData(ineqlhsPd1, ineqrhsPd1, eqlhsPd1, eqrhsPd1);
assert(Pd1 == P1)
assert(isEmpty Pd1 === isEmpty P1)
assert(isCompact Pd1 === isCompact P1)
assert(isLatticePolytope Pd1 === isLatticePolytope P1)
assert(isNormal Pd1 === isNormal P1)
assert(numColumns vertices Pd1 == numColumns vertices P1)
assert(numColumns rays Pd1 == numColumns rays P1)
assert(numColumns linealitySpace Pd1 == numColumns linealitySpace P1)
facetsP1 = facets P1;
facetsPd1 = facets Pd1;
assert(numRows (facetsPd1#0) == numRows (facetsP1#0))
assert(numRows (facetsPd1#1) == numRows (facetsP1#1))
hyperplanesP1 = hyperplanes P1;
hyperplanesPd1 = hyperplanes Pd1;
assert(numRows (hyperplanesPd1#0) == numRows (hyperplanesP1#0))
assert(numRows (hyperplanesPd1#1) == numRows (hyperplanesP1#1))
assert(contains(P1, Pd1))
assert(contains(Pd1, P1))

-- Test dim: 4, ambientDim: 4, vertices: 11, facets: 8
-- Checking representation vs dual representation
verticesP2 = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0}};
raysP2 = map(QQ^4, QQ^0, 0);
linealityP2 = map(QQ^4, QQ^0, 0);
P2 = convexHull(verticesP2,raysP2,linealityP2);
ineqlhsPd2 = matrix {{0,0,-1,0},{-1,0,0,0},{0,-1,0,0},{1,1,-1,-1},{0,0,0,-1},{0,1,0,0},{1,0,0,0},{0,0,1,1}};
ineqrhsPd2 = matrix {{0},{0},{0},{1},{0},{1},{1},{1}};
eqlhsPd2 = map(QQ^0, QQ^4, 0);
eqrhsPd2 = map(QQ^0, QQ^1, 0);
Pd2 = polyhedronFromHData(ineqlhsPd2, ineqrhsPd2, eqlhsPd2, eqrhsPd2);
assert(Pd2 == P2)
assert(isEmpty Pd2 === isEmpty P2)
assert(isCompact Pd2 === isCompact P2)
assert(isLatticePolytope Pd2 === isLatticePolytope P2)
assert(isNormal Pd2 === isNormal P2)
assert(numColumns vertices Pd2 == numColumns vertices P2)
assert(numColumns rays Pd2 == numColumns rays P2)
assert(numColumns linealitySpace Pd2 == numColumns linealitySpace P2)
facetsP2 = facets P2;
facetsPd2 = facets Pd2;
assert(numRows (facetsPd2#0) == numRows (facetsP2#0))
assert(numRows (facetsPd2#1) == numRows (facetsP2#1))
hyperplanesP2 = hyperplanes P2;
hyperplanesPd2 = hyperplanes Pd2;
assert(numRows (hyperplanesPd2#0) == numRows (hyperplanesP2#0))
assert(numRows (hyperplanesPd2#1) == numRows (hyperplanesP2#1))
assert(contains(P2, Pd2))
assert(contains(Pd2, P2))

-- Test dim: 4, ambientDim: 5, vertices: 11, facets: 8
-- Checking representation vs dual representation
verticesP3 = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0},{0,0,0,0,0,0,0,0,1,1,1}};
raysP3 = map(QQ^5, QQ^0, 0);
linealityP3 = map(QQ^5, QQ^0, 0);
P3 = convexHull(verticesP3,raysP3,linealityP3);
ineqlhsPd3 = matrix {{0,0,-1,0,0},{-1,0,0,0,0},{0,-1,0,0,0},{1,1,-1,-1,0},{0,0,0,-1,0},{0,1,0,0,0},{1,0,0,0,0},{0,0,1,1,0}};
ineqrhsPd3 = matrix {{0},{0},{0},{1},{0},{1},{1},{1}};
eqlhsPd3 = matrix {{0,0,-1,-1,-1}};
eqrhsPd3 = matrix {{-1}};
Pd3 = polyhedronFromHData(ineqlhsPd3, ineqrhsPd3, eqlhsPd3, eqrhsPd3);
assert(Pd3 == P3)
assert(isEmpty Pd3 === isEmpty P3)
assert(isCompact Pd3 === isCompact P3)
assert(isLatticePolytope Pd3 === isLatticePolytope P3)
assert(isNormal Pd3 === isNormal P3)
assert(numColumns vertices Pd3 == numColumns vertices P3)
assert(numColumns rays Pd3 == numColumns rays P3)
assert(numColumns linealitySpace Pd3 == numColumns linealitySpace P3)
facetsP3 = facets P3;
facetsPd3 = facets Pd3;
assert(numRows (facetsPd3#0) == numRows (facetsP3#0))
assert(numRows (facetsPd3#1) == numRows (facetsP3#1))
hyperplanesP3 = hyperplanes P3;
hyperplanesPd3 = hyperplanes Pd3;
assert(numRows (hyperplanesPd3#0) == numRows (hyperplanesP3#0))
assert(numRows (hyperplanesPd3#1) == numRows (hyperplanesP3#1))
assert(contains(P3, Pd3))
assert(contains(Pd3, P3))

-- Test dim: 5, ambientDim: 5, vertices: 7, facets: 7
-- Checking representation vs dual representation
verticesP4 = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP4 = map(QQ^5, QQ^0, 0);
linealityP4 = map(QQ^5, QQ^0, 0);
P4 = convexHull(verticesP4,raysP4,linealityP4);
ineqlhsPd4 = matrix {{-1,0,-3,-2,-2},{0,-1,-4,-4,-4},{1,0,2,2,2},{0,1,3,4,4},{0,0,0,0,-1},{0,0,0,-1,0},{0,0,1,1,1}};
ineqrhsPd4 = matrix {{-3},{-4},{3},{4},{0},{0},{1}};
eqlhsPd4 = map(QQ^0, QQ^5, 0);
eqrhsPd4 = map(QQ^0, QQ^1, 0);
Pd4 = polyhedronFromHData(ineqlhsPd4, ineqrhsPd4, eqlhsPd4, eqrhsPd4);
assert(Pd4 == P4)
assert(isEmpty Pd4 === isEmpty P4)
assert(isCompact Pd4 === isCompact P4)
assert(isLatticePolytope Pd4 === isLatticePolytope P4)
assert(isNormal Pd4 === isNormal P4)
assert(numColumns vertices Pd4 == numColumns vertices P4)
assert(numColumns rays Pd4 == numColumns rays P4)
assert(numColumns linealitySpace Pd4 == numColumns linealitySpace P4)
facetsP4 = facets P4;
facetsPd4 = facets Pd4;
assert(numRows (facetsPd4#0) == numRows (facetsP4#0))
assert(numRows (facetsPd4#1) == numRows (facetsP4#1))
hyperplanesP4 = hyperplanes P4;
hyperplanesPd4 = hyperplanes Pd4;
assert(numRows (hyperplanesPd4#0) == numRows (hyperplanesP4#0))
assert(numRows (hyperplanesPd4#1) == numRows (hyperplanesP4#1))
assert(contains(P4, Pd4))
assert(contains(Pd4, P4))

-- Test dim: 2, ambientDim: 2, vertices: 3, facets: 3
-- Checking representation vs dual representation
verticesP5 = matrix {{0,1,0},{0,0,1}};
raysP5 = map(QQ^2, QQ^0, 0);
linealityP5 = map(QQ^2, QQ^0, 0);
P5 = convexHull(verticesP5,raysP5,linealityP5);
ineqlhsPd5 = matrix {{1,1},{-1,0},{0,-1}};
ineqrhsPd5 = matrix {{1},{0},{0}};
eqlhsPd5 = map(QQ^0, QQ^2, 0);
eqrhsPd5 = map(QQ^0, QQ^1, 0);
Pd5 = polyhedronFromHData(ineqlhsPd5, ineqrhsPd5, eqlhsPd5, eqrhsPd5);
assert(Pd5 == P5)
assert(isEmpty Pd5 === isEmpty P5)
assert(isCompact Pd5 === isCompact P5)
assert(isLatticePolytope Pd5 === isLatticePolytope P5)
assert(isNormal Pd5 === isNormal P5)
assert(numColumns vertices Pd5 == numColumns vertices P5)
assert(numColumns rays Pd5 == numColumns rays P5)
assert(numColumns linealitySpace Pd5 == numColumns linealitySpace P5)
facetsP5 = facets P5;
facetsPd5 = facets Pd5;
assert(numRows (facetsPd5#0) == numRows (facetsP5#0))
assert(numRows (facetsPd5#1) == numRows (facetsP5#1))
hyperplanesP5 = hyperplanes P5;
hyperplanesPd5 = hyperplanes Pd5;
assert(numRows (hyperplanesPd5#0) == numRows (hyperplanesP5#0))
assert(numRows (hyperplanesPd5#1) == numRows (hyperplanesP5#1))
assert(contains(P5, Pd5))
assert(contains(Pd5, P5))

-- Test dim: 4, ambientDim: 5, vertices: 6, facets: 6
-- Checking representation vs dual representation
verticesP6 = matrix {{0,1,0,1,1,3},{0,0,1,1,0,4},{1,1,1,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}};
raysP6 = map(QQ^5, QQ^0, 0);
linealityP6 = map(QQ^5, QQ^0, 0);
P6 = convexHull(verticesP6,raysP6,linealityP6);
ineqlhsPd6 = matrix {{-1,0,-3,-2,0},{0,-1,-4,-4,0},{1,0,2,2,0},{0,1,3,4,0},{0,0,0,-1,0},{0,0,1,1,0}};
ineqrhsPd6 = matrix {{-3},{-4},{3},{4},{0},{1}};
eqlhsPd6 = matrix {{0,0,-1,-1,-1}};
eqrhsPd6 = matrix {{-1}};
Pd6 = polyhedronFromHData(ineqlhsPd6, ineqrhsPd6, eqlhsPd6, eqrhsPd6);
assert(Pd6 == P6)
assert(isEmpty Pd6 === isEmpty P6)
assert(isCompact Pd6 === isCompact P6)
assert(isLatticePolytope Pd6 === isLatticePolytope P6)
assert(isNormal Pd6 === isNormal P6)
assert(numColumns vertices Pd6 == numColumns vertices P6)
assert(numColumns rays Pd6 == numColumns rays P6)
assert(numColumns linealitySpace Pd6 == numColumns linealitySpace P6)
facetsP6 = facets P6;
facetsPd6 = facets Pd6;
assert(numRows (facetsPd6#0) == numRows (facetsP6#0))
assert(numRows (facetsPd6#1) == numRows (facetsP6#1))
hyperplanesP6 = hyperplanes P6;
hyperplanesPd6 = hyperplanes Pd6;
assert(numRows (hyperplanesPd6#0) == numRows (hyperplanesP6#0))
assert(numRows (hyperplanesPd6#1) == numRows (hyperplanesP6#1))
assert(contains(P6, Pd6))
assert(contains(Pd6, P6))

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 0
-- Checking representation vs dual representation
verticesP7 = matrix {{1},{0}};
raysP7 = map(QQ^2, QQ^0, 0);
linealityP7 = map(QQ^2, QQ^0, 0);
P7 = convexHull(verticesP7,raysP7,linealityP7);
ineqlhsPd7 = matrix {{0,0}};
ineqrhsPd7 = matrix {{1}};
eqlhsPd7 = matrix {{-1,0},{0,-1}};
eqrhsPd7 = matrix {{-1},{0}};
Pd7 = polyhedronFromHData(ineqlhsPd7, ineqrhsPd7, eqlhsPd7, eqrhsPd7);
assert(Pd7 == P7)
assert(isEmpty Pd7 === isEmpty P7)
assert(isCompact Pd7 === isCompact P7)
assert(isLatticePolytope Pd7 === isLatticePolytope P7)
assert(isNormal Pd7 === isNormal P7)
assert(numColumns vertices Pd7 == numColumns vertices P7)
assert(numColumns rays Pd7 == numColumns rays P7)
assert(numColumns linealitySpace Pd7 == numColumns linealitySpace P7)
facetsP7 = facets P7;
facetsPd7 = facets Pd7;
assert(numRows (facetsPd7#0) == numRows (facetsP7#0))
assert(numRows (facetsPd7#1) == numRows (facetsP7#1))
hyperplanesP7 = hyperplanes P7;
hyperplanesPd7 = hyperplanes Pd7;
assert(numRows (hyperplanesPd7#0) == numRows (hyperplanesP7#0))
assert(numRows (hyperplanesPd7#1) == numRows (hyperplanesP7#1))
assert(contains(P7, Pd7))
assert(contains(Pd7, P7))

-- Test dim: 3, ambientDim: 4, vertices: 6, facets: 8
-- Checking representation vs dual representation
verticesP8 = matrix {{1,1,1,0,0,0},{1,0,0,1,1,0},{0,1,0,1,0,1},{0,0,1,0,1,1}};
raysP8 = map(QQ^4, QQ^0, 0);
linealityP8 = map(QQ^4, QQ^0, 0);
P8 = convexHull(verticesP8,raysP8,linealityP8);
ineqlhsPd8 = matrix {{0,-1,0,0},{0,0,1,0},{-1,0,0,0},{-1,-1,-1,0},{0,1,0,0},{0,0,-1,0},{1,1,1,0},{1,0,0,0}};
ineqrhsPd8 = matrix {{0},{1},{0},{-1},{1},{0},{2},{1}};
eqlhsPd8 = matrix {{-1,-1,-1,-1}};
eqrhsPd8 = matrix {{-2}};
Pd8 = polyhedronFromHData(ineqlhsPd8, ineqrhsPd8, eqlhsPd8, eqrhsPd8);
assert(Pd8 == P8)
assert(isEmpty Pd8 === isEmpty P8)
assert(isCompact Pd8 === isCompact P8)
assert(isLatticePolytope Pd8 === isLatticePolytope P8)
assert(isNormal Pd8 === isNormal P8)
assert(numColumns vertices Pd8 == numColumns vertices P8)
assert(numColumns rays Pd8 == numColumns rays P8)
assert(numColumns linealitySpace Pd8 == numColumns linealitySpace P8)
facetsP8 = facets P8;
facetsPd8 = facets Pd8;
assert(numRows (facetsPd8#0) == numRows (facetsP8#0))
assert(numRows (facetsPd8#1) == numRows (facetsP8#1))
hyperplanesP8 = hyperplanes P8;
hyperplanesPd8 = hyperplanes Pd8;
assert(numRows (hyperplanesPd8#0) == numRows (hyperplanesP8#0))
assert(numRows (hyperplanesPd8#1) == numRows (hyperplanesP8#1))
assert(contains(P8, Pd8))
assert(contains(Pd8, P8))

-- Test dim: 4, ambientDim: 4, vertices: 10, facets: 30
-- Checking representation vs dual representation
verticesP9 = matrix {{1,0,0,0,-1,0,0,0,1,-1},{0,1,0,0,0,-1,0,0,1,-1},{0,0,1,0,0,0,-1,0,1,-1},{0,0,0,1,0,0,0,-1,1,-1}};
raysP9 = map(QQ^4, QQ^0, 0);
linealityP9 = map(QQ^4, QQ^0, 0);
P9 = convexHull(verticesP9,raysP9,linealityP9);
ineqlhsPd9 = matrix {{1,1,-1,-1},{1,0,-1,-1},{1,-1,1,-1},{1,-1,0,-1},{1,-1,-1,1},{1,-1,-1,0},{-1,-1,1,1},{-1,-1,1,0},{0,-1,1,-1},{-1,0,1,-1},{-1,0,-1,1},{0,-1,-1,1},{-1,-1,0,1},{0,1,-1,-1},{-1,1,-1,0},{-1,1,-1,1},{-1,1,0,-1},{-1,1,1,-1},{-1,0,1,1},{-1,1,0,1},{-1,1,1,0},{0,1,1,-1},{0,1,-1,1},{0,-1,1,1},{1,-1,0,1},{1,-1,1,0},{1,0,-1,1},{1,1,-1,0},{1,0,1,-1},{1,1,0,-1}};
ineqrhsPd9 = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd9 = map(QQ^0, QQ^4, 0);
eqrhsPd9 = map(QQ^0, QQ^1, 0);
Pd9 = polyhedronFromHData(ineqlhsPd9, ineqrhsPd9, eqlhsPd9, eqrhsPd9);
assert(Pd9 == P9)
assert(isEmpty Pd9 === isEmpty P9)
assert(isCompact Pd9 === isCompact P9)
assert(isLatticePolytope Pd9 === isLatticePolytope P9)
assert(isNormal Pd9 === isNormal P9)
assert(numColumns vertices Pd9 == numColumns vertices P9)
assert(numColumns rays Pd9 == numColumns rays P9)
assert(numColumns linealitySpace Pd9 == numColumns linealitySpace P9)
facetsP9 = facets P9;
facetsPd9 = facets Pd9;
assert(numRows (facetsPd9#0) == numRows (facetsP9#0))
assert(numRows (facetsPd9#1) == numRows (facetsP9#1))
hyperplanesP9 = hyperplanes P9;
hyperplanesPd9 = hyperplanes Pd9;
assert(numRows (hyperplanesPd9#0) == numRows (hyperplanesP9#0))
assert(numRows (hyperplanesPd9#1) == numRows (hyperplanesP9#1))
assert(contains(P9, Pd9))
assert(contains(Pd9, P9))

///

TEST ///
-- Test dim: 5, ambientDim: 5, vertices: 11, facets: 26
-- Checking representation vs dual representation
verticesP10 = matrix {{1,0,0,0,0,-1,0,0,0,0,1},{0,1,0,0,0,0,-1,0,0,0,1},{0,0,1,0,0,0,0,-1,0,0,1},{0,0,0,1,0,0,0,0,-1,0,1},{0,0,0,0,1,0,0,0,0,-1,1}};
raysP10 = map(QQ^5, QQ^0, 0);
linealityP10 = map(QQ^5, QQ^0, 0);
P10 = convexHull(verticesP10,raysP10,linealityP10);
ineqlhsPd10 = matrix {{1,1,-1,-1,-1},{1,-1,1,-1,-1},{1,-1,-1,1,-1},{1,-1,-1,-1,-1},{1,-1,-1,-1,1},{-1,-1,1,1,-1},{-1,-1,1,-1,-1},{-1,-1,1,-1,1},{-1,-1,-1,-1,-1},{-1,-1,-1,-1,1},{-1,-1,-1,1,1},{-1,-1,-1,1,-1},{-1,1,-1,-1,1},{-1,1,-1,-1,-1},{-1,1,-1,1,-1},{-1,1,1,-1,-1},{-1,1,1,1,-1},{-1,1,1,-1,1},{-1,1,-1,1,1},{-1,-1,1,1,1},{1,-1,-1,1,1},{1,-1,1,-1,1},{1,-1,1,1,-1},{1,1,-1,-1,1},{1,1,-1,1,-1},{1,1,1,-1,-1}};
ineqrhsPd10 = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd10 = map(QQ^0, QQ^5, 0);
eqrhsPd10 = map(QQ^0, QQ^1, 0);
Pd10 = polyhedronFromHData(ineqlhsPd10, ineqrhsPd10, eqlhsPd10, eqrhsPd10);
assert(Pd10 == P10)
assert(isEmpty Pd10 === isEmpty P10)
assert(isCompact Pd10 === isCompact P10)
assert(isLatticePolytope Pd10 === isLatticePolytope P10)
assert(isNormal Pd10 === isNormal P10)
assert(numColumns vertices Pd10 == numColumns vertices P10)
assert(numColumns rays Pd10 == numColumns rays P10)
assert(numColumns linealitySpace Pd10 == numColumns linealitySpace P10)
facetsP10 = facets P10;
facetsPd10 = facets Pd10;
assert(numRows (facetsPd10#0) == numRows (facetsP10#0))
assert(numRows (facetsPd10#1) == numRows (facetsP10#1))
hyperplanesP10 = hyperplanes P10;
hyperplanesPd10 = hyperplanes Pd10;
assert(numRows (hyperplanesPd10#0) == numRows (hyperplanesP10#0))
assert(numRows (hyperplanesPd10#1) == numRows (hyperplanesP10#1))
assert(contains(P10, Pd10))
assert(contains(Pd10, P10))

-- Test dim: 6, ambientDim: 6, vertices: 13, facets: 102
-- Checking representation vs dual representation
verticesP11 = matrix {{1,0,0,0,0,0,-1,0,0,0,0,0,1},{0,1,0,0,0,0,0,-1,0,0,0,0,1},{0,0,1,0,0,0,0,0,-1,0,0,0,1},{0,0,0,1,0,0,0,0,0,-1,0,0,1},{0,0,0,0,1,0,0,0,0,0,-1,0,1},{0,0,0,0,0,1,0,0,0,0,0,-1,1}};
raysP11 = map(QQ^6, QQ^0, 0);
linealityP11 = map(QQ^6, QQ^0, 0);
P11 = convexHull(verticesP11,raysP11,linealityP11);
ineqlhsPd11 = matrix {{1,1,1,-1,-1,-1},{1,1,-1,1,-1,-1},{1,1,-1,-1,1,-1},{1,1,-1,-1,-1,-1},{1,1,-1,-1,-1,1},{1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1},{1,-1,1,-1,-1,-1},{1,-1,1,-1,-1,1},{1,-1,-1,1,1,-1},{1,-1,-1,1,-1,-1},{1,-1,-1,1,-1,1},{1,-1,-1,-1,1,-1},{1,-1,-1,-1,1,1},{1,-1,-1,-1,-1,-1},{1,-1,-1,-1,-1,1},{-1,-1,1,1,1,-1},{-1,-1,1,1,-1,-1},{-1,-1,1,1,-1,1},{-1,-1,1,-1,1,-1},{-1,-1,1,-1,1,1},{-1,-1,1,-1,-1,-1},{-1,-1,1,-1,-1,1},{-1,-1,-1,-1,1,-1},{-1,-1,-1,-1,1,1},{-1,-1,-1,-1,-1,1},{-1,-1,-1,-1,-1,-1},{-1,-1,-1,1,-1,1},{-1,-1,-1,1,-1,-1},{-1,-1,-1,1,1,1},{-1,-1,-1,1,1,-1},{-1,1,-1,-1,-1,1},{-1,1,-1,-1,-1,-1},{-1,1,-1,-1,1,1},{-1,1,-1,-1,1,-1},{-1,1,-1,1,-1,1},{-1,1,-1,1,-1,-1},{-1,1,-1,1,1,-1},{-1,1,1,-1,-1,1},{-1,1,1,-1,-1,-1},{-1,1,1,-1,1,-1},{-1,1,1,1,-1,-1},{-1,-1,0,1,1,1},{-1,-1,1,0,1,1},{-1,-1,1,1,0,1},{-1,-1,1,1,1,0},{-1,0,1,1,1,-1},{-1,0,1,1,-1,1},{-1,0,1,-1,1,1},{-1,0,-1,1,1,1},{-1,1,-1,0,1,1},{-1,1,-1,1,0,1},{-1,1,-1,1,1,0},{-1,1,0,-1,1,1},{-1,1,1,-1,0,1},{-1,1,1,-1,1,0},{-1,1,0,1,-1,1},{-1,1,1,0,-1,1},{-1,1,1,1,-1,0},{-1,1,0,1,1,-1},{-1,1,1,0,1,-1},{-1,1,1,1,0,-1},{0,1,1,1,-1,-1},{0,1,1,-1,1,-1},{0,1,1,-1,-1,1},{0,1,-1,1,1,-1},{0,1,-1,1,-1,1},{0,1,-1,-1,1,1},{0,-1,1,1,1,-1},{0,-1,1,1,-1,1},{0,-1,1,-1,1,1},{0,-1,-1,1,1,1},{1,-1,-1,0,1,1},{1,-1,-1,1,0,1},{1,-1,-1,1,1,0},{1,-1,0,-1,1,1},{1,-1,1,-1,0,1},{1,-1,1,-1,1,0},{1,-1,0,1,-1,1},{1,-1,1,0,-1,1},{1,-1,1,1,-1,0},{1,-1,0,1,1,-1},{1,-1,1,0,1,-1},{1,-1,1,1,0,-1},{1,0,-1,-1,1,1},{1,1,-1,-1,0,1},{1,1,-1,-1,1,0},{1,0,-1,1,-1,1},{1,1,-1,0,-1,1},{1,1,-1,1,-1,0},{1,0,-1,1,1,-1},{1,1,-1,0,1,-1},{1,1,-1,1,0,-1},{1,0,1,-1,-1,1},{1,1,0,-1,-1,1},{1,1,1,-1,-1,0},{1,0,1,-1,1,-1},{1,1,0,-1,1,-1},{1,1,1,-1,0,-1},{1,0,1,1,-1,-1},{1,1,0,1,-1,-1},{1,1,1,0,-1,-1}};
ineqrhsPd11 = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd11 = map(QQ^0, QQ^6, 0);
eqrhsPd11 = map(QQ^0, QQ^1, 0);
Pd11 = polyhedronFromHData(ineqlhsPd11, ineqrhsPd11, eqlhsPd11, eqrhsPd11);
assert(Pd11 == P11)
assert(isEmpty Pd11 === isEmpty P11)
assert(isCompact Pd11 === isCompact P11)
assert(isLatticePolytope Pd11 === isLatticePolytope P11)
assert(isNormal Pd11 === isNormal P11)
assert(numColumns vertices Pd11 == numColumns vertices P11)
assert(numColumns rays Pd11 == numColumns rays P11)
assert(numColumns linealitySpace Pd11 == numColumns linealitySpace P11)
facetsP11 = facets P11;
facetsPd11 = facets Pd11;
assert(numRows (facetsPd11#0) == numRows (facetsP11#0))
assert(numRows (facetsPd11#1) == numRows (facetsP11#1))
hyperplanesP11 = hyperplanes P11;
hyperplanesPd11 = hyperplanes Pd11;
assert(numRows (hyperplanesPd11#0) == numRows (hyperplanesP11#0))
assert(numRows (hyperplanesPd11#1) == numRows (hyperplanesP11#1))
assert(contains(P11, Pd11))
assert(contains(Pd11, P11))

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
verticesP12 = matrix {{1,0,0,-1,0,0,1,-1},{0,1,0,0,-1,0,1,-1},{0,0,1,0,0,-1,1,-1}};
raysP12 = map(QQ^3, QQ^0, 0);
linealityP12 = map(QQ^3, QQ^0, 0);
P12 = convexHull(verticesP12,raysP12,linealityP12);
ineqlhsPd12 = matrix {{1,-1,-1},{-1,-1,1},{-1,1,-1},{-1,1,1},{1,-1,1},{1,1,-1}};
ineqrhsPd12 = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd12 = map(QQ^0, QQ^3, 0);
eqrhsPd12 = map(QQ^0, QQ^1, 0);
Pd12 = polyhedronFromHData(ineqlhsPd12, ineqrhsPd12, eqlhsPd12, eqrhsPd12);
assert(Pd12 == P12)
assert(isEmpty Pd12 === isEmpty P12)
assert(isCompact Pd12 === isCompact P12)
assert(isLatticePolytope Pd12 === isLatticePolytope P12)
assert(isNormal Pd12 === isNormal P12)
assert(numColumns vertices Pd12 == numColumns vertices P12)
assert(numColumns rays Pd12 == numColumns rays P12)
assert(numColumns linealitySpace Pd12 == numColumns linealitySpace P12)
facetsP12 = facets P12;
facetsPd12 = facets Pd12;
assert(numRows (facetsPd12#0) == numRows (facetsP12#0))
assert(numRows (facetsPd12#1) == numRows (facetsP12#1))
hyperplanesP12 = hyperplanes P12;
hyperplanesPd12 = hyperplanes Pd12;
assert(numRows (hyperplanesPd12#0) == numRows (hyperplanesP12#0))
assert(numRows (hyperplanesPd12#1) == numRows (hyperplanesP12#1))
assert(contains(P12, Pd12))
assert(contains(Pd12, P12))

-- Test dim: 6, ambientDim: 7, vertices: 19, facets: 11
-- Checking representation vs dual representation
verticesP13 = matrix {{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1},{0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1},{0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0},{0,1,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0},{1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1},{1,1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0},{1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0}};
raysP13 = map(QQ^7, QQ^0, 0);
linealityP13 = map(QQ^7, QQ^0, 0);
P13 = convexHull(verticesP13,raysP13,linealityP13);
ineqlhsPd13 = matrix {{0,0,0,0,0,1,0},{-1,0,0,0,0,0,0},{-1,-1,0,0,-1,-1,0},{0,-1,0,0,0,0,0},{0,0,0,0,1,0,0},{0,1,0,0,0,0,0},{0,0,0,0,-1,-1,0},{1,0,0,0,0,0,0},{0,0,-1,0,0,0,0},{0,0,0,-1,0,0,0},{1,1,1,1,1,1,0}};
ineqrhsPd13 = matrix {{1},{0},{-2},{0},{1},{1},{-1},{1},{0},{0},{3}};
eqlhsPd13 = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd13 = matrix {{-3}};
Pd13 = polyhedronFromHData(ineqlhsPd13, ineqrhsPd13, eqlhsPd13, eqrhsPd13);
assert(Pd13 == P13)
assert(isEmpty Pd13 === isEmpty P13)
assert(isCompact Pd13 === isCompact P13)
assert(isLatticePolytope Pd13 === isLatticePolytope P13)
assert(isNormal Pd13 === isNormal P13)
assert(numColumns vertices Pd13 == numColumns vertices P13)
assert(numColumns rays Pd13 == numColumns rays P13)
assert(numColumns linealitySpace Pd13 == numColumns linealitySpace P13)
facetsP13 = facets P13;
facetsPd13 = facets Pd13;
assert(numRows (facetsPd13#0) == numRows (facetsP13#0))
assert(numRows (facetsPd13#1) == numRows (facetsP13#1))
hyperplanesP13 = hyperplanes P13;
hyperplanesPd13 = hyperplanes Pd13;
assert(numRows (hyperplanesPd13#0) == numRows (hyperplanesP13#0))
assert(numRows (hyperplanesPd13#1) == numRows (hyperplanesP13#1))
assert(contains(P13, Pd13))
assert(contains(Pd13, P13))

-- Test dim: 6, ambientDim: 7, vertices: 35, facets: 14
-- Checking representation vs dual representation
verticesP14 = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0}};
raysP14 = map(QQ^7, QQ^0, 0);
linealityP14 = map(QQ^7, QQ^0, 0);
P14 = convexHull(verticesP14,raysP14,linealityP14);
ineqlhsPd14 = matrix {{0,0,-1,0,0,0,0},{0,0,0,1,0,0,0},{0,0,0,0,0,1,0},{0,-1,0,0,0,0,0},{-1,0,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,0},{0,0,0,0,1,0,0},{0,0,0,-1,0,0,0},{0,0,1,0,0,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,-1,0},{1,1,1,1,1,1,0},{0,1,0,0,0,0,0},{1,0,0,0,0,0,0}};
ineqrhsPd14 = matrix {{0},{1},{1},{0},{0},{-2},{1},{0},{1},{0},{0},{3},{1},{1}};
eqlhsPd14 = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd14 = matrix {{-3}};
Pd14 = polyhedronFromHData(ineqlhsPd14, ineqrhsPd14, eqlhsPd14, eqrhsPd14);
assert(Pd14 == P14)
assert(isEmpty Pd14 === isEmpty P14)
assert(isCompact Pd14 === isCompact P14)
assert(isLatticePolytope Pd14 === isLatticePolytope P14)
assert(isNormal Pd14 === isNormal P14)
assert(numColumns vertices Pd14 == numColumns vertices P14)
assert(numColumns rays Pd14 == numColumns rays P14)
assert(numColumns linealitySpace Pd14 == numColumns linealitySpace P14)
facetsP14 = facets P14;
facetsPd14 = facets Pd14;
assert(numRows (facetsPd14#0) == numRows (facetsP14#0))
assert(numRows (facetsPd14#1) == numRows (facetsP14#1))
hyperplanesP14 = hyperplanes P14;
hyperplanesPd14 = hyperplanes Pd14;
assert(numRows (hyperplanesPd14#0) == numRows (hyperplanesP14#0))
assert(numRows (hyperplanesPd14#1) == numRows (hyperplanesP14#1))
assert(contains(P14, Pd14))
assert(contains(Pd14, P14))

-- Test dim: 6, ambientDim: 7, vertices: 30, facets: 16
-- Checking representation vs dual representation
verticesP15 = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,0,1,0,1,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,0}};
raysP15 = map(QQ^7, QQ^0, 0);
linealityP15 = map(QQ^7, QQ^0, 0);
P15 = convexHull(verticesP15,raysP15,linealityP15);
ineqlhsPd15 = matrix {{0,0,0,0,0,1,0},{0,0,0,0,-1,0,0},{0,0,0,1,0,0,0},{-1,0,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,0},{0,0,1,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,-1,0},{0,1,0,0,0,0,0},{0,0,-1,-1,-1,-1,0},{1,0,0,0,0,0,0},{0,0,-1,0,0,0,0},{1,1,1,1,1,1,0},{0,0,0,-1,0,0,0},{1,1,1,1,0,0,0}};
ineqrhsPd15 = matrix {{1},{0},{1},{0},{-2},{1},{0},{1},{0},{1},{-1},{1},{0},{3},{0},{2}};
eqlhsPd15 = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd15 = matrix {{-3}};
Pd15 = polyhedronFromHData(ineqlhsPd15, ineqrhsPd15, eqlhsPd15, eqrhsPd15);
assert(Pd15 == P15)
assert(isEmpty Pd15 === isEmpty P15)
assert(isCompact Pd15 === isCompact P15)
assert(isLatticePolytope Pd15 === isLatticePolytope P15)
assert(isNormal Pd15 === isNormal P15)
assert(numColumns vertices Pd15 == numColumns vertices P15)
assert(numColumns rays Pd15 == numColumns rays P15)
assert(numColumns linealitySpace Pd15 == numColumns linealitySpace P15)
facetsP15 = facets P15;
facetsPd15 = facets Pd15;
assert(numRows (facetsPd15#0) == numRows (facetsP15#0))
assert(numRows (facetsPd15#1) == numRows (facetsP15#1))
hyperplanesP15 = hyperplanes P15;
hyperplanesPd15 = hyperplanes Pd15;
assert(numRows (hyperplanesPd15#0) == numRows (hyperplanesP15#0))
assert(numRows (hyperplanesPd15#1) == numRows (hyperplanesP15#1))
assert(contains(P15, Pd15))
assert(contains(Pd15, P15))

-- Test dim: 2, ambientDim: 2, vertices: 3, facets: 3
-- Checking representation vs dual representation
verticesP16 = matrix {{0,1,1/2},{0,0,11/2}};
raysP16 = map(QQ^2, QQ^0, 0);
linealityP16 = map(QQ^2, QQ^0, 0);
P16 = convexHull(verticesP16,raysP16,linealityP16);
ineqlhsPd16 = matrix {{11,1},{0,-1},{-11,1}};
ineqrhsPd16 = matrix {{11},{0},{0}};
eqlhsPd16 = map(QQ^0, QQ^2, 0);
eqrhsPd16 = map(QQ^0, QQ^1, 0);
Pd16 = polyhedronFromHData(ineqlhsPd16, ineqrhsPd16, eqlhsPd16, eqrhsPd16);
assert(Pd16 == P16)
assert(isEmpty Pd16 === isEmpty P16)
assert(isCompact Pd16 === isCompact P16)
assert(isLatticePolytope Pd16 === isLatticePolytope P16)
assert(isNormal Pd16 === isNormal P16)
assert(numColumns vertices Pd16 == numColumns vertices P16)
assert(numColumns rays Pd16 == numColumns rays P16)
assert(numColumns linealitySpace Pd16 == numColumns linealitySpace P16)
facetsP16 = facets P16;
facetsPd16 = facets Pd16;
assert(numRows (facetsPd16#0) == numRows (facetsP16#0))
assert(numRows (facetsPd16#1) == numRows (facetsP16#1))
hyperplanesP16 = hyperplanes P16;
hyperplanesPd16 = hyperplanes Pd16;
assert(numRows (hyperplanesPd16#0) == numRows (hyperplanesP16#0))
assert(numRows (hyperplanesPd16#1) == numRows (hyperplanesP16#1))
assert(contains(P16, Pd16))
assert(contains(Pd16, P16))

-- Test dim: 4, ambientDim: 4, vertices: 8, facets: 16
-- Checking representation vs dual representation
verticesP17 = matrix {{3/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2},{1/2,1/2,3/2,-1/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,1/2,3/2,-1/2,1/2,1/2},{1/2,1/2,1/2,1/2,1/2,1/2,3/2,-1/2}};
raysP17 = map(QQ^4, QQ^0, 0);
linealityP17 = map(QQ^4, QQ^0, 0);
P17 = convexHull(verticesP17,raysP17,linealityP17);
ineqlhsPd17 = matrix {{-1,1,1,-1},{-1,1,1,1},{-1,1,-1,-1},{-1,1,-1,1},{-1,-1,-1,-1},{-1,-1,-1,1},{-1,-1,1,1},{-1,-1,1,-1},{1,-1,-1,1},{1,-1,-1,-1},{1,-1,1,1},{1,-1,1,-1},{1,1,-1,1},{1,1,-1,-1},{1,1,1,1},{1,1,1,-1}};
ineqrhsPd17 = matrix {{1},{2},{0},{1},{-1},{0},{1},{0},{1},{0},{2},{1},{2},{1},{3},{2}};
eqlhsPd17 = map(QQ^0, QQ^4, 0);
eqrhsPd17 = map(QQ^0, QQ^1, 0);
Pd17 = polyhedronFromHData(ineqlhsPd17, ineqrhsPd17, eqlhsPd17, eqrhsPd17);
assert(Pd17 == P17)
assert(isEmpty Pd17 === isEmpty P17)
assert(isCompact Pd17 === isCompact P17)
assert(isLatticePolytope Pd17 === isLatticePolytope P17)
assert(isNormal Pd17 === isNormal P17)
assert(numColumns vertices Pd17 == numColumns vertices P17)
assert(numColumns rays Pd17 == numColumns rays P17)
assert(numColumns linealitySpace Pd17 == numColumns linealitySpace P17)
facetsP17 = facets P17;
facetsPd17 = facets Pd17;
assert(numRows (facetsPd17#0) == numRows (facetsP17#0))
assert(numRows (facetsPd17#1) == numRows (facetsP17#1))
hyperplanesP17 = hyperplanes P17;
hyperplanesPd17 = hyperplanes Pd17;
assert(numRows (hyperplanesPd17#0) == numRows (hyperplanesP17#0))
assert(numRows (hyperplanesPd17#1) == numRows (hyperplanesP17#1))
assert(contains(P17, Pd17))
assert(contains(Pd17, P17))

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
verticesP18 = matrix {{1,0,0,1,0,1,0,1},{1,1,0,0,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP18 = map(QQ^3, QQ^0, 0);
linealityP18 = map(QQ^3, QQ^0, 0);
P18 = convexHull(verticesP18,raysP18,linealityP18);
ineqlhsPd18 = matrix {{0,0,-1},{0,-1,0},{-1,0,0},{1,0,0},{0,1,0},{0,0,1}};
ineqrhsPd18 = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd18 = map(QQ^0, QQ^3, 0);
eqrhsPd18 = map(QQ^0, QQ^1, 0);
Pd18 = polyhedronFromHData(ineqlhsPd18, ineqrhsPd18, eqlhsPd18, eqrhsPd18);
assert(Pd18 == P18)
assert(isEmpty Pd18 === isEmpty P18)
assert(isCompact Pd18 === isCompact P18)
assert(isLatticePolytope Pd18 === isLatticePolytope P18)
assert(isNormal Pd18 === isNormal P18)
assert(numColumns vertices Pd18 == numColumns vertices P18)
assert(numColumns rays Pd18 == numColumns rays P18)
assert(numColumns linealitySpace Pd18 == numColumns linealitySpace P18)
facetsP18 = facets P18;
facetsPd18 = facets Pd18;
assert(numRows (facetsPd18#0) == numRows (facetsP18#0))
assert(numRows (facetsPd18#1) == numRows (facetsP18#1))
hyperplanesP18 = hyperplanes P18;
hyperplanesPd18 = hyperplanes Pd18;
assert(numRows (hyperplanesPd18#0) == numRows (hyperplanesP18#0))
assert(numRows (hyperplanesPd18#1) == numRows (hyperplanesP18#1))
assert(contains(P18, Pd18))
assert(contains(Pd18, P18))

-- Test dim: 2, ambientDim: 2, vertices: 4, facets: 4
-- Checking representation vs dual representation
verticesP19 = matrix {{1,0,0,1},{0,0,1,1}};
raysP19 = map(QQ^2, QQ^0, 0);
linealityP19 = map(QQ^2, QQ^0, 0);
P19 = convexHull(verticesP19,raysP19,linealityP19);
ineqlhsPd19 = matrix {{0,-1},{-1,0},{1,0},{0,1}};
ineqrhsPd19 = matrix {{0},{0},{1},{1}};
eqlhsPd19 = map(QQ^0, QQ^2, 0);
eqrhsPd19 = map(QQ^0, QQ^1, 0);
Pd19 = polyhedronFromHData(ineqlhsPd19, ineqrhsPd19, eqlhsPd19, eqrhsPd19);
assert(Pd19 == P19)
assert(isEmpty Pd19 === isEmpty P19)
assert(isCompact Pd19 === isCompact P19)
assert(isLatticePolytope Pd19 === isLatticePolytope P19)
assert(isNormal Pd19 === isNormal P19)
assert(numColumns vertices Pd19 == numColumns vertices P19)
assert(numColumns rays Pd19 == numColumns rays P19)
assert(numColumns linealitySpace Pd19 == numColumns linealitySpace P19)
facetsP19 = facets P19;
facetsPd19 = facets Pd19;
assert(numRows (facetsPd19#0) == numRows (facetsP19#0))
assert(numRows (facetsPd19#1) == numRows (facetsP19#1))
hyperplanesP19 = hyperplanes P19;
hyperplanesPd19 = hyperplanes Pd19;
assert(numRows (hyperplanesPd19#0) == numRows (hyperplanesP19#0))
assert(numRows (hyperplanesPd19#1) == numRows (hyperplanesP19#1))
assert(contains(P19, Pd19))
assert(contains(Pd19, P19))

///

TEST ///
-- Test dim: 2, ambientDim: 2, vertices: 3, facets: 3
-- Checking representation vs dual representation
verticesP20 = matrix {{0,1,1/2},{0,0,5}};
raysP20 = map(QQ^2, QQ^0, 0);
linealityP20 = map(QQ^2, QQ^0, 0);
P20 = convexHull(verticesP20,raysP20,linealityP20);
ineqlhsPd20 = matrix {{0,-1},{-10,1},{10,1}};
ineqrhsPd20 = matrix {{0},{0},{10}};
eqlhsPd20 = map(QQ^0, QQ^2, 0);
eqrhsPd20 = map(QQ^0, QQ^1, 0);
Pd20 = polyhedronFromHData(ineqlhsPd20, ineqrhsPd20, eqlhsPd20, eqrhsPd20);
assert(Pd20 == P20)
assert(isEmpty Pd20 === isEmpty P20)
assert(isCompact Pd20 === isCompact P20)
assert(isLatticePolytope Pd20 === isLatticePolytope P20)
assert(isNormal Pd20 === isNormal P20)
assert(numColumns vertices Pd20 == numColumns vertices P20)
assert(numColumns rays Pd20 == numColumns rays P20)
assert(numColumns linealitySpace Pd20 == numColumns linealitySpace P20)
facetsP20 = facets P20;
facetsPd20 = facets Pd20;
assert(numRows (facetsPd20#0) == numRows (facetsP20#0))
assert(numRows (facetsPd20#1) == numRows (facetsP20#1))
hyperplanesP20 = hyperplanes P20;
hyperplanesPd20 = hyperplanes Pd20;
assert(numRows (hyperplanesPd20#0) == numRows (hyperplanesP20#0))
assert(numRows (hyperplanesPd20#1) == numRows (hyperplanesP20#1))
assert(contains(P20, Pd20))
assert(contains(Pd20, P20))

-- Test dim: 4, ambientDim: 4, vertices: 24, facets: 32
-- Checking representation vs dual representation
verticesP21 = matrix {{1,2/3,2/3,1/3,1/2,1/2,2/3,2/3,1/3,1/3,1/3,2/3,1/3,2/3,1/2,1/3,1/3,0,2/3,1/3,2/3,1/2,1/2,1/2},{1/2,1/3,2/3,2/3,0,1/2,1/3,2/3,1/3,2/3,1/3,1/3,1/3,1/3,1/2,2/3,1/3,1/2,2/3,2/3,2/3,1,1/2,1/2},{1/2,2/3,2/3,1/3,1/2,0,1/3,1/3,1/3,1/3,1/3,1/3,2/3,2/3,1/2,2/3,2/3,1/2,1/3,2/3,2/3,1/2,1,1/2},{1/2,2/3,1/3,2/3,1/2,1/2,2/3,1/3,2/3,1/3,1/3,1/3,1/3,1/3,0,1/3,2/3,1/2,2/3,2/3,2/3,1/2,1/2,1}};
raysP21 = map(QQ^4, QQ^0, 0);
linealityP21 = map(QQ^4, QQ^0, 0);
P21 = convexHull(verticesP21,raysP21,linealityP21);
ineqlhsPd21 = matrix {{0,-1,-1,-1},{-1,0,-1,-1},{-1,-1,0,-1},{-1,-1,-1,0},{1,0,-1,-1},{1,-1,0,-1},{1,-1,-1,0},{0,1,-1,-1},{0,-1,1,-1},{0,-1,-1,1},{-1,1,0,-1},{-1,1,-1,0},{-1,0,1,-1},{-1,0,-1,1},{-1,-1,1,0},{-1,-1,0,1},{1,1,0,-1},{1,1,-1,0},{1,0,1,-1},{1,0,-1,1},{1,-1,1,0},{1,-1,0,1},{0,1,1,-1},{0,1,-1,1},{0,-1,1,1},{-1,1,1,0},{-1,1,0,1},{-1,0,1,1},{1,1,1,0},{1,1,0,1},{1,0,1,1},{0,1,1,1}};
ineqrhsPd21 = matrix {{-1},{-1},{-1},{-1},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{2},{2},{2},{2}};
eqlhsPd21 = map(QQ^0, QQ^4, 0);
eqrhsPd21 = map(QQ^0, QQ^1, 0);
Pd21 = polyhedronFromHData(ineqlhsPd21, ineqrhsPd21, eqlhsPd21, eqrhsPd21);
assert(Pd21 == P21)
assert(isEmpty Pd21 === isEmpty P21)
assert(isCompact Pd21 === isCompact P21)
assert(isLatticePolytope Pd21 === isLatticePolytope P21)
assert(isNormal Pd21 === isNormal P21)
assert(numColumns vertices Pd21 == numColumns vertices P21)
assert(numColumns rays Pd21 == numColumns rays P21)
assert(numColumns linealitySpace Pd21 == numColumns linealitySpace P21)
facetsP21 = facets P21;
facetsPd21 = facets Pd21;
assert(numRows (facetsPd21#0) == numRows (facetsP21#0))
assert(numRows (facetsPd21#1) == numRows (facetsP21#1))
hyperplanesP21 = hyperplanes P21;
hyperplanesPd21 = hyperplanes Pd21;
assert(numRows (hyperplanesPd21#0) == numRows (hyperplanesP21#0))
assert(numRows (hyperplanesPd21#1) == numRows (hyperplanesP21#1))
assert(contains(P21, Pd21))
assert(contains(Pd21, P21))

-- Test dim: 2, ambientDim: 2, vertices: 4, facets: 4
-- Checking representation vs dual representation
verticesP22 = matrix {{3/2,-1/2,1/2,1/2},{1/2,1/2,3/2,-1/2}};
raysP22 = map(QQ^2, QQ^0, 0);
linealityP22 = map(QQ^2, QQ^0, 0);
P22 = convexHull(verticesP22,raysP22,linealityP22);
ineqlhsPd22 = matrix {{-1,-1},{-1,1},{1,1},{1,-1}};
ineqrhsPd22 = matrix {{0},{1},{2},{1}};
eqlhsPd22 = map(QQ^0, QQ^2, 0);
eqrhsPd22 = map(QQ^0, QQ^1, 0);
Pd22 = polyhedronFromHData(ineqlhsPd22, ineqrhsPd22, eqlhsPd22, eqrhsPd22);
assert(Pd22 == P22)
assert(isEmpty Pd22 === isEmpty P22)
assert(isCompact Pd22 === isCompact P22)
assert(isLatticePolytope Pd22 === isLatticePolytope P22)
assert(isNormal Pd22 === isNormal P22)
assert(numColumns vertices Pd22 == numColumns vertices P22)
assert(numColumns rays Pd22 == numColumns rays P22)
assert(numColumns linealitySpace Pd22 == numColumns linealitySpace P22)
facetsP22 = facets P22;
facetsPd22 = facets Pd22;
assert(numRows (facetsPd22#0) == numRows (facetsP22#0))
assert(numRows (facetsPd22#1) == numRows (facetsP22#1))
hyperplanesP22 = hyperplanes P22;
hyperplanesPd22 = hyperplanes Pd22;
assert(numRows (hyperplanesPd22#0) == numRows (hyperplanesP22#0))
assert(numRows (hyperplanesPd22#1) == numRows (hyperplanesP22#1))
assert(contains(P22, Pd22))
assert(contains(Pd22, P22))

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
verticesP23 = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP23 = map(QQ^3, QQ^0, 0);
linealityP23 = map(QQ^3, QQ^0, 0);
P23 = convexHull(verticesP23,raysP23,linealityP23);
ineqlhsPd23 = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd23 = matrix {{0},{1},{0},{1},{0},{1}};
eqlhsPd23 = map(QQ^0, QQ^3, 0);
eqrhsPd23 = map(QQ^0, QQ^1, 0);
Pd23 = polyhedronFromHData(ineqlhsPd23, ineqrhsPd23, eqlhsPd23, eqrhsPd23);
assert(Pd23 == P23)
assert(isEmpty Pd23 === isEmpty P23)
assert(isCompact Pd23 === isCompact P23)
assert(isLatticePolytope Pd23 === isLatticePolytope P23)
assert(isNormal Pd23 === isNormal P23)
assert(numColumns vertices Pd23 == numColumns vertices P23)
assert(numColumns rays Pd23 == numColumns rays P23)
assert(numColumns linealitySpace Pd23 == numColumns linealitySpace P23)
facetsP23 = facets P23;
facetsPd23 = facets Pd23;
assert(numRows (facetsPd23#0) == numRows (facetsP23#0))
assert(numRows (facetsPd23#1) == numRows (facetsP23#1))
hyperplanesP23 = hyperplanes P23;
hyperplanesPd23 = hyperplanes Pd23;
assert(numRows (hyperplanesPd23#0) == numRows (hyperplanesP23#0))
assert(numRows (hyperplanesPd23#1) == numRows (hyperplanesP23#1))
assert(contains(P23, Pd23))
assert(contains(Pd23, P23))

-- Test dim: 4, ambientDim: 4, vertices: 16, facets: 8
-- Checking representation vs dual representation
verticesP24 = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP24 = map(QQ^4, QQ^0, 0);
linealityP24 = map(QQ^4, QQ^0, 0);
P24 = convexHull(verticesP24,raysP24,linealityP24);
ineqlhsPd24 = matrix {{-1,0,0,0},{1,0,0,0},{0,-1,0,0},{0,1,0,0},{0,0,-1,0},{0,0,1,0},{0,0,0,-1},{0,0,0,1}};
ineqrhsPd24 = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd24 = map(QQ^0, QQ^4, 0);
eqrhsPd24 = map(QQ^0, QQ^1, 0);
Pd24 = polyhedronFromHData(ineqlhsPd24, ineqrhsPd24, eqlhsPd24, eqrhsPd24);
assert(Pd24 == P24)
assert(isEmpty Pd24 === isEmpty P24)
assert(isCompact Pd24 === isCompact P24)
assert(isLatticePolytope Pd24 === isLatticePolytope P24)
assert(isNormal Pd24 === isNormal P24)
assert(numColumns vertices Pd24 == numColumns vertices P24)
assert(numColumns rays Pd24 == numColumns rays P24)
assert(numColumns linealitySpace Pd24 == numColumns linealitySpace P24)
facetsP24 = facets P24;
facetsPd24 = facets Pd24;
assert(numRows (facetsPd24#0) == numRows (facetsP24#0))
assert(numRows (facetsPd24#1) == numRows (facetsP24#1))
hyperplanesP24 = hyperplanes P24;
hyperplanesPd24 = hyperplanes Pd24;
assert(numRows (hyperplanesPd24#0) == numRows (hyperplanesP24#0))
assert(numRows (hyperplanesPd24#1) == numRows (hyperplanesP24#1))
assert(contains(P24, Pd24))
assert(contains(Pd24, P24))

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
verticesP25 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP25 = map(QQ^3, QQ^0, 0);
linealityP25 = map(QQ^3, QQ^0, 0);
P25 = convexHull(verticesP25,raysP25,linealityP25);
ineqlhsPd25 = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd25 = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd25 = map(QQ^0, QQ^3, 0);
eqrhsPd25 = map(QQ^0, QQ^1, 0);
Pd25 = polyhedronFromHData(ineqlhsPd25, ineqrhsPd25, eqlhsPd25, eqrhsPd25);
assert(Pd25 == P25)
assert(isEmpty Pd25 === isEmpty P25)
assert(isCompact Pd25 === isCompact P25)
assert(isLatticePolytope Pd25 === isLatticePolytope P25)
assert(isNormal Pd25 === isNormal P25)
assert(numColumns vertices Pd25 == numColumns vertices P25)
assert(numColumns rays Pd25 == numColumns rays P25)
assert(numColumns linealitySpace Pd25 == numColumns linealitySpace P25)
facetsP25 = facets P25;
facetsPd25 = facets Pd25;
assert(numRows (facetsPd25#0) == numRows (facetsP25#0))
assert(numRows (facetsPd25#1) == numRows (facetsP25#1))
hyperplanesP25 = hyperplanes P25;
hyperplanesPd25 = hyperplanes Pd25;
assert(numRows (hyperplanesPd25#0) == numRows (hyperplanesP25#0))
assert(numRows (hyperplanesPd25#1) == numRows (hyperplanesP25#1))
assert(contains(P25, Pd25))
assert(contains(Pd25, P25))

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 1
-- Checking representation vs dual representation
verticesP26 = matrix {{1},{-1}};
raysP26 = map(QQ^2, QQ^0, 0);
linealityP26 = map(QQ^2, QQ^0, 0);
P26 = convexHull(verticesP26,raysP26,linealityP26);
ineqlhsPd26 = matrix {{0,0}};
ineqrhsPd26 = matrix {{1}};
eqlhsPd26 = matrix {{-1,-1},{1/2,-1/2}};
eqrhsPd26 = matrix {{0},{1}};
Pd26 = polyhedronFromHData(ineqlhsPd26, ineqrhsPd26, eqlhsPd26, eqrhsPd26);
assert(Pd26 == P26)
assert(isEmpty Pd26 === isEmpty P26)
assert(isCompact Pd26 === isCompact P26)
assert(isLatticePolytope Pd26 === isLatticePolytope P26)
assert(isNormal Pd26 === isNormal P26)
assert(numColumns vertices Pd26 == numColumns vertices P26)
assert(numColumns rays Pd26 == numColumns rays P26)
assert(numColumns linealitySpace Pd26 == numColumns linealitySpace P26)
facetsP26 = facets P26;
facetsPd26 = facets Pd26;
assert(numRows (facetsPd26#0) == numRows (facetsP26#0))
assert(numRows (facetsPd26#1) == numRows (facetsP26#1))
hyperplanesP26 = hyperplanes P26;
hyperplanesPd26 = hyperplanes Pd26;
assert(numRows (hyperplanesPd26#0) == numRows (hyperplanesP26#0))
assert(numRows (hyperplanesPd26#1) == numRows (hyperplanesP26#1))
assert(contains(P26, Pd26))
assert(contains(Pd26, P26))

-- Test dim: 0, ambientDim: 0, vertices: 1, facets: 1
-- Checking representation vs dual representation
verticesP27 = map(QQ^0, QQ^1, 0);
raysP27 = map(QQ^0, QQ^0, 0);
linealityP27 = map(QQ^0, QQ^0, 0);
P27 = convexHull(verticesP27,raysP27,linealityP27);
ineqlhsPd27 = map(QQ^1, QQ^0, 0);
ineqrhsPd27 = matrix {{1}};
eqlhsPd27 = map(QQ^0, QQ^0, 0);
eqrhsPd27 = map(QQ^0, QQ^1, 0);
Pd27 = polyhedronFromHData(ineqlhsPd27, ineqrhsPd27, eqlhsPd27, eqrhsPd27);
assert(Pd27 == P27)
assert(isEmpty Pd27 === isEmpty P27)
assert(isCompact Pd27 === isCompact P27)
assert(isLatticePolytope Pd27 === isLatticePolytope P27)
assert(isNormal Pd27 === isNormal P27)
assert(numColumns vertices Pd27 == numColumns vertices P27)
assert(numColumns rays Pd27 == numColumns rays P27)
assert(numColumns linealitySpace Pd27 == numColumns linealitySpace P27)
facetsP27 = facets P27;
facetsPd27 = facets Pd27;
assert(numRows (facetsPd27#0) == numRows (facetsP27#0))
assert(numRows (facetsPd27#1) == numRows (facetsP27#1))
hyperplanesP27 = hyperplanes P27;
hyperplanesPd27 = hyperplanes Pd27;
assert(numRows (hyperplanesPd27#0) == numRows (hyperplanesP27#0))
assert(numRows (hyperplanesPd27#1) == numRows (hyperplanesP27#1))
assert(contains(P27, Pd27))
assert(contains(Pd27, P27))

-- Test dim: 1, ambientDim: 2, vertices: 1, facets: 1
-- Checking representation vs dual representation
verticesP28 = matrix {{1},{1}};
raysP28 = map(QQ^2, QQ^0, 0);
linealityP28 = matrix {{0},{1}};
P28 = convexHull(verticesP28,raysP28,linealityP28);
ineqlhsPd28 = matrix {{0,0}};
ineqrhsPd28 = matrix {{1}};
eqlhsPd28 = matrix {{-1,0}};
eqrhsPd28 = matrix {{-1}};
Pd28 = polyhedronFromHData(ineqlhsPd28, ineqrhsPd28, eqlhsPd28, eqrhsPd28);
assert(Pd28 == P28)
assert(isEmpty Pd28 === isEmpty P28)
assert(isCompact Pd28 === isCompact P28)
assert(isLatticePolytope Pd28 === isLatticePolytope P28)
assert(numColumns vertices Pd28 == numColumns vertices P28)
assert(numColumns rays Pd28 == numColumns rays P28)
assert(numColumns linealitySpace Pd28 == numColumns linealitySpace P28)
facetsP28 = facets P28;
facetsPd28 = facets Pd28;
assert(numRows (facetsPd28#0) == numRows (facetsP28#0))
assert(numRows (facetsPd28#1) == numRows (facetsP28#1))
hyperplanesP28 = hyperplanes P28;
hyperplanesPd28 = hyperplanes Pd28;
assert(numRows (hyperplanesPd28#0) == numRows (hyperplanesP28#0))
assert(numRows (hyperplanesPd28#1) == numRows (hyperplanesP28#1))
assert(contains(P28, Pd28))
assert(contains(Pd28, P28))

-- Test dim: 2, ambientDim: 2, vertices: 6, facets: 6
-- Checking representation vs dual representation
verticesP29 = matrix {{0,0,1,3/2,5/2,5/2},{0,1/2,5/2,5/2,0,1/2}};
raysP29 = map(QQ^2, QQ^0, 0);
linealityP29 = map(QQ^2, QQ^0, 0);
P29 = convexHull(verticesP29,raysP29,linealityP29);
ineqlhsPd29 = matrix {{0,-1},{-1,0},{-4,2},{0,1},{2,1},{1,0}};
ineqrhsPd29 = matrix {{0},{0},{1},{5/2},{11/2},{5/2}};
eqlhsPd29 = map(QQ^0, QQ^2, 0);
eqrhsPd29 = map(QQ^0, QQ^1, 0);
Pd29 = polyhedronFromHData(ineqlhsPd29, ineqrhsPd29, eqlhsPd29, eqrhsPd29);
assert(Pd29 == P29)
assert(isEmpty Pd29 === isEmpty P29)
assert(isCompact Pd29 === isCompact P29)
assert(isLatticePolytope Pd29 === isLatticePolytope P29)
assert(isNormal Pd29 === isNormal P29)
assert(numColumns vertices Pd29 == numColumns vertices P29)
assert(numColumns rays Pd29 == numColumns rays P29)
assert(numColumns linealitySpace Pd29 == numColumns linealitySpace P29)
facetsP29 = facets P29;
facetsPd29 = facets Pd29;
assert(numRows (facetsPd29#0) == numRows (facetsP29#0))
assert(numRows (facetsPd29#1) == numRows (facetsP29#1))
hyperplanesP29 = hyperplanes P29;
hyperplanesPd29 = hyperplanes Pd29;
assert(numRows (hyperplanesPd29#0) == numRows (hyperplanesP29#0))
assert(numRows (hyperplanesPd29#1) == numRows (hyperplanesP29#1))
assert(contains(P29, Pd29))
assert(contains(Pd29, P29))

///

TEST ///
-- Test dim: 2, ambientDim: 2, vertices: 5, facets: 5
-- Checking representation vs dual representation
verticesP30 = matrix {{5/3,-1/3,-1,1,1/3},{1/3,-5/3,1,-1,5/3}};
raysP30 = map(QQ^2, QQ^0, 0);
linealityP30 = map(QQ^2, QQ^0, 0);
P30 = convexHull(verticesP30,raysP30,linealityP30);
ineqlhsPd30 = matrix {{1,-2},{-4,-1},{-1,2},{1,1},{2,-1}};
ineqrhsPd30 = matrix {{3},{3},{3},{2},{3}};
eqlhsPd30 = map(QQ^0, QQ^2, 0);
eqrhsPd30 = map(QQ^0, QQ^1, 0);
Pd30 = polyhedronFromHData(ineqlhsPd30, ineqrhsPd30, eqlhsPd30, eqrhsPd30);
assert(Pd30 == P30)
assert(isEmpty Pd30 === isEmpty P30)
assert(isCompact Pd30 === isCompact P30)
assert(isLatticePolytope Pd30 === isLatticePolytope P30)
assert(isNormal Pd30 === isNormal P30)
assert(numColumns vertices Pd30 == numColumns vertices P30)
assert(numColumns rays Pd30 == numColumns rays P30)
assert(numColumns linealitySpace Pd30 == numColumns linealitySpace P30)
facetsP30 = facets P30;
facetsPd30 = facets Pd30;
assert(numRows (facetsPd30#0) == numRows (facetsP30#0))
assert(numRows (facetsPd30#1) == numRows (facetsP30#1))
hyperplanesP30 = hyperplanes P30;
hyperplanesPd30 = hyperplanes Pd30;
assert(numRows (hyperplanesPd30#0) == numRows (hyperplanesP30#0))
assert(numRows (hyperplanesPd30#1) == numRows (hyperplanesP30#1))
assert(contains(P30, Pd30))
assert(contains(Pd30, P30))

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 1
-- Checking representation vs dual representation
verticesP31 = matrix {{1},{1}};
raysP31 = map(QQ^2, QQ^0, 0);
linealityP31 = map(QQ^2, QQ^0, 0);
P31 = convexHull(verticesP31,raysP31,linealityP31);
ineqlhsPd31 = matrix {{0,0}};
ineqrhsPd31 = matrix {{1}};
eqlhsPd31 = matrix {{-1,0},{0,-1}};
eqrhsPd31 = matrix {{-1},{-1}};
Pd31 = polyhedronFromHData(ineqlhsPd31, ineqrhsPd31, eqlhsPd31, eqrhsPd31);
assert(Pd31 == P31)
assert(isEmpty Pd31 === isEmpty P31)
assert(isCompact Pd31 === isCompact P31)
assert(isLatticePolytope Pd31 === isLatticePolytope P31)
assert(isNormal Pd31 === isNormal P31)
assert(numColumns vertices Pd31 == numColumns vertices P31)
assert(numColumns rays Pd31 == numColumns rays P31)
assert(numColumns linealitySpace Pd31 == numColumns linealitySpace P31)
facetsP31 = facets P31;
facetsPd31 = facets Pd31;
assert(numRows (facetsPd31#0) == numRows (facetsP31#0))
assert(numRows (facetsPd31#1) == numRows (facetsP31#1))
hyperplanesP31 = hyperplanes P31;
hyperplanesPd31 = hyperplanes Pd31;
assert(numRows (hyperplanesPd31#0) == numRows (hyperplanesP31#0))
assert(numRows (hyperplanesPd31#1) == numRows (hyperplanesP31#1))
assert(contains(P31, Pd31))
assert(contains(Pd31, P31))

-- Test dim: 3, ambientDim: 3, vertices: 4, facets: 4
-- Checking representation vs dual representation
verticesP32 = matrix {{-1},{-1},{-1}};
raysP32 = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityP32 = map(QQ^3, QQ^0, 0);
P32 = convexHull(verticesP32,raysP32,linealityP32);
ineqlhsPd32 = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,0}};
ineqrhsPd32 = matrix {{1},{1},{1},{1}};
eqlhsPd32 = map(QQ^0, QQ^3, 0);
eqrhsPd32 = map(QQ^0, QQ^1, 0);
Pd32 = polyhedronFromHData(ineqlhsPd32, ineqrhsPd32, eqlhsPd32, eqrhsPd32);
assert(Pd32 == P32)
assert(isEmpty Pd32 === isEmpty P32)
assert(isCompact Pd32 === isCompact P32)
assert(isLatticePolytope Pd32 === isLatticePolytope P32)
assert(numColumns vertices Pd32 == numColumns vertices P32)
assert(numColumns rays Pd32 == numColumns rays P32)
assert(numColumns linealitySpace Pd32 == numColumns linealitySpace P32)
facetsP32 = facets P32;
facetsPd32 = facets Pd32;
assert(numRows (facetsPd32#0) == numRows (facetsP32#0))
assert(numRows (facetsPd32#1) == numRows (facetsP32#1))
hyperplanesP32 = hyperplanes P32;
hyperplanesPd32 = hyperplanes Pd32;
assert(numRows (hyperplanesPd32#0) == numRows (hyperplanesP32#0))
assert(numRows (hyperplanesPd32#1) == numRows (hyperplanesP32#1))
assert(contains(P32, Pd32))
assert(contains(Pd32, P32))

///
