-- Test dim: 2, ambientDim: 2, vertices: 4, facets: 4
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{0},{0},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 0
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{3},{4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,0},{0,-1}};
eqrhsPd = matrix {{-3},{-4}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 4, vertices: 11, facets: 8
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0},{-1,0,0,0},{0,-1,0,0},{1,1,-1,-1},{0,0,0,-1},{0,1,0,0},{1,0,0,0},{0,0,1,1}};
ineqrhsPd = matrix {{0},{0},{0},{1},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 5, vertices: 11, facets: 8
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0},{0,0,0,0,0,0,0,0,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0},{-1,0,0,0,0},{0,-1,0,0,0},{1,1,-1,-1,0},{0,0,0,-1,0},{0,1,0,0,0},{1,0,0,0,0},{0,0,1,1,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{0},{1},{1},{1}};
eqlhsPd = matrix {{0,0,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 5, ambientDim: 5, vertices: 7, facets: 7
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,-3,-2,-2},{0,-1,-4,-4,-4},{1,0,2,2,2},{0,1,3,4,4},{0,0,0,0,-1},{0,0,0,-1,0},{0,0,1,1,1}};
ineqrhsPd = matrix {{-3},{-4},{3},{4},{0},{0},{1}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 3, facets: 3
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{-1,0},{0,-1}};
ineqrhsPd = matrix {{1},{0},{0}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 5, vertices: 6, facets: 6
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0,1,1,3},{0,0,1,1,0,4},{1,1,1,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,-3,-2,0},{0,-1,-4,-4,0},{1,0,2,2,0},{0,1,3,4,0},{0,0,0,-1,0},{0,0,1,1,0}};
ineqrhsPd = matrix {{-3},{-4},{3},{4},{0},{1}};
eqlhsPd = matrix {{0,0,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 0
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1},{0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,0},{0,-1}};
eqrhsPd = matrix {{-1},{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 3, ambientDim: 4, vertices: 6, facets: 8
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,1,1,0,0,0},{1,0,0,1,1,0},{0,1,0,1,0,1},{0,0,1,0,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0,0},{0,0,1,0},{-1,0,0,0},{-1,-1,-1,0},{0,1,0,0},{0,0,-1,0},{1,1,1,0},{1,0,0,0}};
ineqrhsPd = matrix {{0},{1},{0},{-1},{1},{0},{2},{1}};
eqlhsPd = matrix {{-1,-1,-1,-1}};
eqrhsPd = matrix {{-2}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 4, vertices: 10, facets: 30
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,0,0,0,-1,0,0,0,1,-1},{0,1,0,0,0,-1,0,0,1,-1},{0,0,1,0,0,0,-1,0,1,-1},{0,0,0,1,0,0,0,-1,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,-1,-1},{1,0,-1,-1},{1,-1,1,-1},{1,-1,0,-1},{1,-1,-1,1},{1,-1,-1,0},{-1,-1,1,1},{-1,-1,1,0},{0,-1,1,-1},{-1,0,1,-1},{-1,0,-1,1},{0,-1,-1,1},{-1,-1,0,1},{0,1,-1,-1},{-1,1,-1,0},{-1,1,-1,1},{-1,1,0,-1},{-1,1,1,-1},{-1,0,1,1},{-1,1,0,1},{-1,1,1,0},{0,1,1,-1},{0,1,-1,1},{0,-1,1,1},{1,-1,0,1},{1,-1,1,0},{1,0,-1,1},{1,1,-1,0},{1,0,1,-1},{1,1,0,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 5, ambientDim: 5, vertices: 11, facets: 26
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,0,0,0,0,-1,0,0,0,0,1},{0,1,0,0,0,0,-1,0,0,0,1},{0,0,1,0,0,0,0,-1,0,0,1},{0,0,0,1,0,0,0,0,-1,0,1},{0,0,0,0,1,0,0,0,0,-1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,-1,-1,-1},{1,-1,1,-1,-1},{1,-1,-1,1,-1},{1,-1,-1,-1,-1},{1,-1,-1,-1,1},{-1,-1,1,1,-1},{-1,-1,1,-1,-1},{-1,-1,1,-1,1},{-1,-1,-1,-1,-1},{-1,-1,-1,-1,1},{-1,-1,-1,1,1},{-1,-1,-1,1,-1},{-1,1,-1,-1,1},{-1,1,-1,-1,-1},{-1,1,-1,1,-1},{-1,1,1,-1,-1},{-1,1,1,1,-1},{-1,1,1,-1,1},{-1,1,-1,1,1},{-1,-1,1,1,1},{1,-1,-1,1,1},{1,-1,1,-1,1},{1,-1,1,1,-1},{1,1,-1,-1,1},{1,1,-1,1,-1},{1,1,1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 6, ambientDim: 6, vertices: 13, facets: 102
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,0,0,0,0,0,-1,0,0,0,0,0,1},{0,1,0,0,0,0,0,-1,0,0,0,0,1},{0,0,1,0,0,0,0,0,-1,0,0,0,1},{0,0,0,1,0,0,0,0,0,-1,0,0,1},{0,0,0,0,1,0,0,0,0,0,-1,0,1},{0,0,0,0,0,1,0,0,0,0,0,-1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,-1,-1,-1},{1,1,-1,1,-1,-1},{1,1,-1,-1,1,-1},{1,1,-1,-1,-1,-1},{1,1,-1,-1,-1,1},{1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1},{1,-1,1,-1,-1,-1},{1,-1,1,-1,-1,1},{1,-1,-1,1,1,-1},{1,-1,-1,1,-1,-1},{1,-1,-1,1,-1,1},{1,-1,-1,-1,1,-1},{1,-1,-1,-1,1,1},{1,-1,-1,-1,-1,-1},{1,-1,-1,-1,-1,1},{-1,-1,1,1,1,-1},{-1,-1,1,1,-1,-1},{-1,-1,1,1,-1,1},{-1,-1,1,-1,1,-1},{-1,-1,1,-1,1,1},{-1,-1,1,-1,-1,-1},{-1,-1,1,-1,-1,1},{-1,-1,-1,-1,1,-1},{-1,-1,-1,-1,1,1},{-1,-1,-1,-1,-1,1},{-1,-1,-1,-1,-1,-1},{-1,-1,-1,1,-1,1},{-1,-1,-1,1,-1,-1},{-1,-1,-1,1,1,1},{-1,-1,-1,1,1,-1},{-1,1,-1,-1,-1,1},{-1,1,-1,-1,-1,-1},{-1,1,-1,-1,1,1},{-1,1,-1,-1,1,-1},{-1,1,-1,1,-1,1},{-1,1,-1,1,-1,-1},{-1,1,-1,1,1,-1},{-1,1,1,-1,-1,1},{-1,1,1,-1,-1,-1},{-1,1,1,-1,1,-1},{-1,1,1,1,-1,-1},{-1,-1,0,1,1,1},{-1,-1,1,0,1,1},{-1,-1,1,1,0,1},{-1,-1,1,1,1,0},{-1,0,1,1,1,-1},{-1,0,1,1,-1,1},{-1,0,1,-1,1,1},{-1,0,-1,1,1,1},{-1,1,-1,0,1,1},{-1,1,-1,1,0,1},{-1,1,-1,1,1,0},{-1,1,0,-1,1,1},{-1,1,1,-1,0,1},{-1,1,1,-1,1,0},{-1,1,0,1,-1,1},{-1,1,1,0,-1,1},{-1,1,1,1,-1,0},{-1,1,0,1,1,-1},{-1,1,1,0,1,-1},{-1,1,1,1,0,-1},{0,1,1,1,-1,-1},{0,1,1,-1,1,-1},{0,1,1,-1,-1,1},{0,1,-1,1,1,-1},{0,1,-1,1,-1,1},{0,1,-1,-1,1,1},{0,-1,1,1,1,-1},{0,-1,1,1,-1,1},{0,-1,1,-1,1,1},{0,-1,-1,1,1,1},{1,-1,-1,0,1,1},{1,-1,-1,1,0,1},{1,-1,-1,1,1,0},{1,-1,0,-1,1,1},{1,-1,1,-1,0,1},{1,-1,1,-1,1,0},{1,-1,0,1,-1,1},{1,-1,1,0,-1,1},{1,-1,1,1,-1,0},{1,-1,0,1,1,-1},{1,-1,1,0,1,-1},{1,-1,1,1,0,-1},{1,0,-1,-1,1,1},{1,1,-1,-1,0,1},{1,1,-1,-1,1,0},{1,0,-1,1,-1,1},{1,1,-1,0,-1,1},{1,1,-1,1,-1,0},{1,0,-1,1,1,-1},{1,1,-1,0,1,-1},{1,1,-1,1,0,-1},{1,0,1,-1,-1,1},{1,1,0,-1,-1,1},{1,1,1,-1,-1,0},{1,0,1,-1,1,-1},{1,1,0,-1,1,-1},{1,1,1,-1,0,-1},{1,0,1,1,-1,-1},{1,1,0,1,-1,-1},{1,1,1,0,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,0,0,-1,0,0,1,-1},{0,1,0,0,-1,0,1,-1},{0,0,1,0,0,-1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1,-1},{-1,-1,1},{-1,1,-1},{-1,1,1},{1,-1,1},{1,1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 6, ambientDim: 7, vertices: 19, facets: 11
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1},{0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1},{0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0},{0,1,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0},{1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1},{1,1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0},{1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,0,1,0},{-1,0,0,0,0,0,0},{-1,-1,0,0,-1,-1,0},{0,-1,0,0,0,0,0},{0,0,0,0,1,0,0},{0,1,0,0,0,0,0},{0,0,0,0,-1,-1,0},{1,0,0,0,0,0,0},{0,0,-1,0,0,0,0},{0,0,0,-1,0,0,0},{1,1,1,1,1,1,0}};
ineqrhsPd = matrix {{1},{0},{-2},{0},{1},{1},{-1},{1},{0},{0},{3}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 6, ambientDim: 7, vertices: 35, facets: 14
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0,0,0},{0,0,0,1,0,0,0},{0,0,0,0,0,1,0},{0,-1,0,0,0,0,0},{-1,0,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,0},{0,0,0,0,1,0,0},{0,0,0,-1,0,0,0},{0,0,1,0,0,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,-1,0},{1,1,1,1,1,1,0},{0,1,0,0,0,0,0},{1,0,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{1},{1},{0},{0},{-2},{1},{0},{1},{0},{0},{3},{1},{1}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 6, ambientDim: 7, vertices: 30, facets: 16
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,0,1,0,1,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,0,1,0},{0,0,0,0,-1,0,0},{0,0,0,1,0,0,0},{-1,0,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,0},{0,0,1,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,-1,0},{0,1,0,0,0,0,0},{0,0,-1,-1,-1,-1,0},{1,0,0,0,0,0,0},{0,0,-1,0,0,0,0},{1,1,1,1,1,1,0},{0,0,0,-1,0,0,0},{1,1,1,1,0,0,0}};
ineqrhsPd = matrix {{1},{0},{1},{0},{-2},{1},{0},{1},{0},{1},{-1},{1},{0},{3},{0},{2}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 3, facets: 3
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,1/2},{0,0,11/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{11,1},{0,-1},{-11,1}};
ineqrhsPd = matrix {{11},{0},{0}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 4, vertices: 8, facets: 16
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{3/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2},{1/2,1/2,3/2,-1/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,1/2,3/2,-1/2,1/2,1/2},{1/2,1/2,1/2,1/2,1/2,1/2,3/2,-1/2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,1,-1},{-1,1,1,1},{-1,1,-1,-1},{-1,1,-1,1},{-1,-1,-1,-1},{-1,-1,-1,1},{-1,-1,1,1},{-1,-1,1,-1},{1,-1,-1,1},{1,-1,-1,-1},{1,-1,1,1},{1,-1,1,-1},{1,1,-1,1},{1,1,-1,-1},{1,1,1,1},{1,1,1,-1}};
ineqrhsPd = matrix {{1},{2},{0},{1},{-1},{0},{1},{0},{1},{0},{2},{1},{2},{1},{3},{2}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,0,0,1,0,1,0,1},{1,1,0,0,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{0,-1,0},{-1,0,0},{1,0,0},{0,1,0},{0,0,1}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 4, facets: 4
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,0,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{1,0},{0,1}};
ineqrhsPd = matrix {{0},{0},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 3, facets: 3
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,1/2},{0,0,5}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-10,1},{10,1}};
ineqrhsPd = matrix {{0},{0},{10}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 4, vertices: 24, facets: 32
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1,2/3,2/3,1/3,1/2,1/2,2/3,2/3,1/3,1/3,1/3,2/3,1/3,2/3,1/2,1/3,1/3,0,2/3,1/3,2/3,1/2,1/2,1/2},{1/2,1/3,2/3,2/3,0,1/2,1/3,2/3,1/3,2/3,1/3,1/3,1/3,1/3,1/2,2/3,1/3,1/2,2/3,2/3,2/3,1,1/2,1/2},{1/2,2/3,2/3,1/3,1/2,0,1/3,1/3,1/3,1/3,1/3,1/3,2/3,2/3,1/2,2/3,2/3,1/2,1/3,2/3,2/3,1/2,1,1/2},{1/2,2/3,1/3,2/3,1/2,1/2,2/3,1/3,2/3,1/3,1/3,1/3,1/3,1/3,0,1/3,2/3,1/2,2/3,2/3,2/3,1/2,1/2,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,-1,-1},{-1,0,-1,-1},{-1,-1,0,-1},{-1,-1,-1,0},{1,0,-1,-1},{1,-1,0,-1},{1,-1,-1,0},{0,1,-1,-1},{0,-1,1,-1},{0,-1,-1,1},{-1,1,0,-1},{-1,1,-1,0},{-1,0,1,-1},{-1,0,-1,1},{-1,-1,1,0},{-1,-1,0,1},{1,1,0,-1},{1,1,-1,0},{1,0,1,-1},{1,0,-1,1},{1,-1,1,0},{1,-1,0,1},{0,1,1,-1},{0,1,-1,1},{0,-1,1,1},{-1,1,1,0},{-1,1,0,1},{-1,0,1,1},{1,1,1,0},{1,1,0,1},{1,0,1,1},{0,1,1,1}};
ineqrhsPd = matrix {{-1},{-1},{-1},{-1},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{2},{2},{2},{2}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 4, facets: 4
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{3/2,-1/2,1/2,1/2},{1/2,1/2,3/2,-1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1},{-1,1},{1,1},{1,-1}};
ineqrhsPd = matrix {{0},{1},{2},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd = matrix {{0},{1},{0},{1},{0},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 4, ambientDim: 4, vertices: 16, facets: 8
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{1,0,0,0},{0,-1,0,0},{0,1,0,0},{0,0,-1,0},{0,0,1,0},{0,0,0,-1},{0,0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 3, ambientDim: 3, vertices: 8, facets: 6
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 1
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1},{-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,-1},{1/2,-1/2}};
eqrhsPd = matrix {{0},{1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 0, ambientDim: 0, vertices: 1, facets: 1
-- Checking representation vs dual representation
TEST ///
verticesP = map(QQ^0, QQ^1, 0);
raysP = map(QQ^0, QQ^0, 0);
linealityP = map(QQ^0, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^1, QQ^0, 0);
ineqrhsPd = matrix {{1}};
eqlhsPd = map(QQ^0, QQ^0, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 1, ambientDim: 2, vertices: 1, facets: 1
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1},{1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = matrix {{0},{1}};
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,0}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 6, facets: 6
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{0,0,1,3/2,5/2,5/2},{0,1/2,5/2,5/2,0,1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{-4,2},{0,1},{2,1},{1,0}};
ineqrhsPd = matrix {{0},{0},{1},{5/2},{11/2},{5/2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 2, ambientDim: 2, vertices: 5, facets: 5
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{5/3,-1/3,-1,1,1/3},{1/3,-5/3,1,-1,5/3}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-2},{-4,-1},{-1,2},{1,1},{2,-1}};
ineqrhsPd = matrix {{3},{3},{3},{2},{3}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 0, ambientDim: 2, vertices: 1, facets: 1
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{1},{1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,0},{0,-1}};
eqrhsPd = matrix {{-1},{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(isNormal Pd === isNormal P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///

-- Test dim: 3, ambientDim: 3, vertices: 4, facets: 4
-- Checking representation vs dual representation
TEST ///
verticesP = matrix {{-1},{-1},{-1}};
raysP = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(Pd == P)
assert(isEmpty Pd === isEmpty P)
assert(isCompact Pd === isCompact P)
assert(isLatticePolytope Pd === isLatticePolytope P)
assert(numColumns vertices Pd == numColumns vertices P)
assert(numColumns rays Pd == numColumns rays P)
assert(numColumns linealitySpace Pd == numColumns linealitySpace P)
facetsP = facets P;
facetsPd = facets Pd;
assert(numRows (facetsPd#0) == numRows (facetsP#0))
assert(numRows (facetsPd#1) == numRows (facetsP#1))
hyperplanesP = hyperplanes P;
hyperplanesPd = hyperplanes Pd;
assert(numRows (hyperplanesPd#0) == numRows (hyperplanesP#0))
assert(numRows (hyperplanesPd#1) == numRows (hyperplanesP#1))
///


