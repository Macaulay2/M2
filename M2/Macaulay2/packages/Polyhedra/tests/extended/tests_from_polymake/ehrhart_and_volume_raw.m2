-- Test cayley_polytope/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,2,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/4.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/c2.poly
-- Checking ehrhart and volume
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
assert(volume P == 11/24)
assert(volume Pd == 11/24)
LE = reverse {1,13/4,97/24,9/4,11/24};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/c1.poly
-- Checking ehrhart and volume
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
assert(volume P == 11/24)
assert(volume Pd == 11/24)
LE = reverse {1,13/4,97/24,9/4,11/24};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/c4.poly
-- Checking ehrhart and volume
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
assert(volume P == 1/60)
assert(volume Pd == 1/60)
LE = reverse {1,149/60,55/24,1,5/24,1/60};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/2.poly
-- Checking ehrhart and volume
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
assert(volume P == 1/2)
assert(volume Pd == 1/2)
LE = reverse {1,3/2,1/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/c3.poly
-- Checking ehrhart and volume
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
assert(volume P == 1/12)
assert(volume Pd == 1/12)
LE = reverse {1,7/3,23/12,2/3,1/12};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_polytope/3.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test print_constraints/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,7/3,2,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test delpezzo/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 5/4)
assert(volume Pd == 5/4)
LE = reverse {1,5/2,15/4,5/2,5/4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test delpezzo/2.poly
-- Checking ehrhart and volume
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
assert(volume P == 23/60)
assert(volume Pd == 23/60)
LE = reverse {1,187/60,85/24,3,23/24,23/60};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test delpezzo/3.poly
-- Checking ehrhart and volume
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
assert(volume P == 17/120)
assert(volume Pd == 17/120)
LE = reverse {1,197/60,553/120,67/24,7/4,17/40,17/120};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test delpezzo/0.poly
-- Checking ehrhart and volume
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
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,3,3,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cell_from_subdivision/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 1/16)
assert(volume Pd == 1/16)
LE = reverse {1,227/60,6,245/48,39/16,49/80,1/16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cell_from_subdivision/1-in.poly
-- Checking ehrhart and volume
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
assert(volume P == 151/360)
assert(volume Pd == 151/360)
LE = reverse {1,259/60,3199/360,21/2,259/36,161/60,151/360};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cell_from_subdivision/2.poly
-- Checking ehrhart and volume
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
assert(volume P == 14/45)
assert(volume Pd == 14/45)
LE = reverse {1,25/6,1439/180,211/24,205/36,49/24,14/45};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test gc_closure/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test gc_closure/4.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,2,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test gc_closure/1-0.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test SCHLEGEL_VERTEX_COLORS/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_DUAL_FACE_LATTICE/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test compress_incidence/13.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test compress_incidence/11.poly
-- Checking ehrhart and volume
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
assert(volume P == 0)
assert(volume Pd == 0)
LE = reverse {1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test compress_incidence/9.poly
-- Checking ehrhart and volume
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test compress_incidence/5.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,15}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1},{-1}};
ineqrhsPd = matrix {{15},{0}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 15)
assert(volume Pd == 15)
LE = reverse {1,15};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test initial/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = map(QQ^1, QQ^0, 0);
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^0, QQ^1, 0);
ineqrhsPd = map(QQ^0, QQ^1, 0);
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test initial/8.poly
-- Checking ehrhart and volume
TEST ///
verticesP = map(QQ^2, QQ^0, 0);
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^0, QQ^2, 0);
ineqrhsPd = map(QQ^0, QQ^1, 0);
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test initial/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = map(QQ^1, QQ^0, 0);
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^0, QQ^1, 0);
ineqrhsPd = map(QQ^0, QQ^1, 0);
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lawrence/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,2,2,3,3,0,0,0,0,0,0},{2,3,1,3,1,2,0,0,0,0,0,0},{3,2,3,1,2,1,0,0,0,0,0,0},{1,0,0,0,0,0,1,0,0,0,0,0},{0,1,0,0,0,0,0,1,0,0,0,0},{0,0,1,0,0,0,0,0,1,0,0,0},{0,0,0,1,0,0,0,0,0,1,0,0},{0,0,0,0,1,0,0,0,0,0,1,0},{0,0,0,0,0,1,0,0,0,0,0,1}};
raysP = map(QQ^9, QQ^0, 0);
linealityP = map(QQ^9, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,1,1,1,1,1,0},{-5,1,7,-18,-12,-12,0,0,0},{1,-5,1,0,0,0,0,0,0},{-1,-7,5,0,0,-6,0,0,0},{-1,-1,2,-3,0,-3,0,0,0},{-7,-1,5,-6,0,0,0,0,0},{-5,1,1,0,0,0,0,0,0},{-1,-1,1,0,0,0,0,0,0},{-1,1,-1,0,0,0,0,0,0},{-7,5,-1,0,-6,0,0,0,0},{-2,1,1,-3,-3,0,0,0,0},{-5,7,1,-12,-18,0,-12,0,0},{-1,2,-1,0,-3,0,-3,0,0},{-1,5,-7,0,0,0,-6,0,0},{1,1,-5,0,0,0,0,0,0},{1,-1,-1,0,0,0,0,0,0},{5,-1,-7,6,6,6,6,6,0},{5,-7,-1,0,0,0,0,-6,0},{2,-1,-1,3,3,3,3,0,0},{1,1,-2,3,3,3,0,3,0},{7,1,-5,18,18,18,6,6,0},{1,-2,1,0,0,-3,0,-3,0},{7,-5,1,12,12,0,12,-6,0},{1,-5,7,-12,0,-18,0,-12,0},{-1,-1,5,-12,-6,-12,0,-6,0},{1,-1,1,0,2,-2,2,-2,0},{1,7,-5,12,0,12,-6,12,0},{-1,1,1,-4,-4,-2,-2,0,0},{-1,5,-1,0,-6,6,-6,6,0},{1,1,-1,4,2,4,0,2,0},{5,-1,-1,12,12,6,6,0,0},{0,0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,-1,0,0},{0,0,0,-1,0,0,0,0,0},{0,0,0,0,-1,0,0,0,0},{0,0,0,0,0,-1,0,0,0}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{6},{0},{3},{3},{18},{0},{12},{0},{0},{2},{12},{0},{6},{4},{12},{0},{0},{0},{0},{0}};
eqlhsPd = matrix {{0,0,0,-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/560)
assert(volume Pd == 3/560)
LE = reverse {1,1619/420,3569/840,61/40,103/240,11/20,19/60,59/840,3/560};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test zonotope/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1},{0,0,2,0,2,2,0,0,2,2,2,2,2,0,0,0},{0,4,4,0,0,4,4,0,0,4,0,4,0,4,0,4},{0,0,8,8,0,8,0,8,0,0,8,0,8,8,0,8}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{0},{8},{4},{2},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 64)
assert(volume Pd == 64)
LE = reverse {1,15,70,120,64};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test zonotope/8.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,4,5,-1,1,4,3,2,5,0,-5,0,-2,1,-3,4,-7,-3,-2,-4,5,-9,-4,2,5,-11,2,-1,-10,-12,2,-7,0,3,-2,-4,0,0,-9,-12,0,-12,-3,-6,2,-1,-7,-6,-7,-9,-4,-4,-1,-11,-9,-12,-9,-11,-3,-5,-8,-6,-7,-10,-5,-3,-8,-4,-7,-3,-6,-9},{-2,0,1,-4,-1,3,4,-3,-1,6,-3,-3,-3,1,-4,2,-3,7,-1,-4,2,-1,7,-2,0,0,5,7,-1,1,4,-1,-2,0,6,-1,0,4,6,4,2,2,-3,-4,4,6,6,7,1,5,6,1,3,3,-2,3,5,1,-2,6,4,-3,5,3,4,4,2,5,3,2,0,-1},{0,-1,0,0,-3,0,0,2,2,1,-1,4,-3,-5,2,-3,1,0,-5,2,3,-2,0,5,5,2,-3,-2,2,-1,4,-4,7,7,3,-6,9,6,0,0,8,2,6,4,-6,-5,-2,2,-6,2,-4,-8,-8,3,5,-3,-3,5,9,5,5,7,-5,-5,7,8,7,-7,-7,10,10,8}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{2,-1,-1},{5/4,-7/4,-1},{4/3,-2,-1},{1,-1,-1},{7/5,-1,-8/5},{7,10,1},{5/3,3,-1},{1,3,-3},{6/5,-9/5,-1},{1,-3/2,-3/2},{1,-3/2,-5/3},{1,4,1},{1,-9/2,3/2},{-1,9,-3},{0,1,0},{-1,6,-3/2},{-1,3/2,-6},{1,11/2,3/2},{-1,-13/4,-3/2},{-1,2,2},{1,-9,3},{-1,-3,3},{-1,9/2,-3/2},{-1,-3/2,-3/2},{-4/3,-1,-2},{-1,-4,-1},{-4/3,2,1},{-5/3,-3,1},{-1,3,-1},{-1,1,1},{-5/4,7/4,1},{-2,3,-1},{-2,1,1},{-1,0,0},{-6,-3,1},{-1,-1,0},{-9/2,-3,1},{-4,-11/2,-1},{-1,-1,-1},{-1,3/2,-1},{-7,-10,-1},{-7/5,1,8/5},{-1,-5/2,-1},{-6/5,9/5,1},{-1,3/2,3/2},{-1,3/2,-3/2},{-1,5,-1},{-1,3/2,5/3},{1,-2,-2},{0,-1,0},{-1,-11/2,-3/2},{1,-6,3/2},{1,-3/2,6},{1,13/4,3/2},{1,3/2,3/2},{1,-5,1},{1,-3,1},{1,5/2,1},{1,-3/2,3/2},{4/3,1,2},{9/2,3,-1},{1,-3/2,1},{2,-3,1},{1,1,1},{1,1,0},{4,11/2,1},{6,3,-1},{1,0,0}};
ineqrhsPd = matrix {{9},{6},{20/3},{5},{42/5},{61},{64/3},{32},{6},{7},{47/6},{25},{39/2},{70},{7},{46},{107/2},{71/2},{65/4},{27},{42},{36},{37},{29/2},{61/3},{18},{24},{26},{27},{17},{22},{36},{28},{12},{68},{11},{103/2},{87/2},{12},{39/2},{78},{122/5},{27/2},{109/5},{43/2},{22},{39},{68/3},{10},{4},{23},{24},{54},{22},{15},{19},{13},{16},{27/2},{18},{27},{10},{15},{10},{7},{34},{33},{5}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1466)
assert(volume Pd == 1466)
LE = reverse {1,11,76,1466};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test zonotope/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,1,-1,0,0},{1,-1,0,0,1,-1},{0,0,-1,1,-1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0},{-1,-1,0},{-1,0,0},{0,1,0},{1,1,0},{1,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = matrix {{-1,-1,-1}};
eqrhsPd = matrix {{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,3,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test zonotope/9.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2,0,2,0,2,0,2,0,2,0,2},{0,0,2,2,4,4,0,0,2,2,4,4},{0,0,2,2,0,0,-2,-2,-4,-4,-2,-2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1,-1},{0,-1,-1},{-1,0,0},{0,-1,0},{0,-1,1},{0,1,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{6},{2},{0},{0},{0},{4},{4},{2}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32)
assert(volume Pd == 32)
LE = reverse {1,8,28,32};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test zonotope/7.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,2,2,0,1,0},{-1,-1,0,1,1,0},{0,-1,-2,-1,-2,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0},{-1,-1,0},{-1,0,0},{0,1,0},{1,1,0},{1,0,0}};
ineqrhsPd = matrix {{1},{0},{0},{1},{2},{2}};
eqlhsPd = matrix {{-1,-1,-1}};
eqrhsPd = matrix {{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,3,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test representations/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test SIMPLICITY/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0},{1,0,0,0,0},{0,0,1,0,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{2,2,1},{-1,0,0},{0,-1,0}};
ineqrhsPd = matrix {{0},{2},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/3)
assert(volume Pd == 1/3)
LE = reverse {1,13/6,3/2,1/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test SIMPLICITY/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/6)
assert(volume Pd == 1/6)
LE = reverse {1,11/6,1,1/6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VERTEX_LABELS/2.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/birkhoff2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0},{0,1},{0,1},{1,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0,0},{-1,0,0,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{-1,-1,0,0},{-1/2,1/2,-1,0},{1/3,-1/3,-1/3,-1}};
eqrhsPd = matrix {{-1},{-1/2},{-2/3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/zsolve.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1},{-1,1,1,-1,1,-1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/hilb2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2,2,1,0},{0,0,2,2,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{0},{1},{0},{2},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 7/2)
assert(volume Pd == 7/2)
LE = reverse {1,7/2,7/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/hilb1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,2,1},{0,0,1,1,1,2},{0,0,0,3,3,3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-3,0,1},{0,0,-1},{0,0,1},{0,-3,1},{3/2,3/2,-1}};
ineqrhsPd = matrix {{0},{0},{3},{0},{3/2}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,5/2,2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/groeb2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,1,0,0},{0,1,0,0,1,0},{0,0,1,0,0,1},{0,1,1,0,0,0},{1,0,0,0,0,1},{0,0,0,1,1,0},{0,0,0,0,1,1},{0,0,1,1,0,0},{1,1,0,0,0,0}};
raysP = map(QQ^9, QQ^0, 0);
linealityP = map(QQ^9, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1,0,-1,-1,0,0,0,0},{-1,0,0,0,0,0,0,0,0},{0,0,0,0,-1,0,0,0,0},{1,0,0,1,0,0,0,0,0},{1,1,0,0,0,0,0,0,0},{0,0,0,1,1,0,0,0,0},{0,1,0,0,1,0,0,0,0},{0,-1,0,0,0,0,0,0,0},{0,0,0,-1,0,0,0,0,0}};
ineqrhsPd = matrix {{-1},{0},{0},{1},{1},{1},{1},{0},{0}};
eqlhsPd = matrix {{-1,-1,-1,0,0,0,0,0,0},{0,0,0,-1,-1,-1,0,0,0},{-2/3,1/3,1/3,-2/3,1/3,1/3,-1,0,0},{1/7,-4/7,3/7,1/7,-4/7,3/7,-2/7,-1,0},{1/5,1/5,-2/5,1/5,1/5,-2/5,-2/5,-2/5,-1}};
eqrhsPd = matrix {{-1},{-1},{-1/3},{-3/7},{-3/5}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/8)
assert(volume Pd == 1/8)
LE = reverse {1,9/4,15/8,3/4,1/8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/groeb-drl.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,0,-1,-1},{0,1,1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-2},{-1,1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5/2)
assert(volume Pd == 5/2)
LE = reverse {1,5/2,5/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/zsolve1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3},{2,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{2,1}};
ineqrhsPd = matrix {{0},{5}};
eqlhsPd = matrix {{-1,-1}};
eqrhsPd = matrix {{-2}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/groeb-dl.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,0,-1,-1},{0,1,1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-2},{-1,1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5/2)
assert(volume Pd == 5/2)
LE = reverse {1,5/2,5/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/groeb-l.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,0,-1},{0,1,1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,1},{1,-2},{1,0},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/groeb1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,4}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1},{-1}};
ineqrhsPd = matrix {{4},{-1}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 4ti2/sparse.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1},{-1,0,0,0,0},{0,-1,0,0,0},{0,0,-1,0,0},{0,0,0,-1,0},{0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/120)
assert(volume Pd == 1/120)
LE = reverse {1,137/60,15/8,17/24,1/8,1/120};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test POSITIVE/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{0},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,4,6,4,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test POSITIVE/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,1,-1},{-1,1,1,1},{-1,1,-1,-1},{-1,1,-1,1},{-1,-1,-1,-1},{-1,-1,-1,1},{-1,-1,1,1},{-1,-1,1,-1},{1,-1,-1,1},{1,-1,-1,-1},{1,-1,1,1},{1,-1,1,-1},{1,1,-1,1},{1,1,-1,-1},{1,1,1,1},{1,1,1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,8/3,10/3,4/3,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test split_compatibility_graph/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test split_compatibility_graph/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,2,1,0},{0,0,1,3,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,1},{-1,0},{0,-1},{1,-1},{2,1}};
ineqrhsPd = matrix {{1},{0},{0},{1},{5}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 7/2)
assert(volume Pd == 7/2)
LE = reverse {1,5/2,7/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test split_compatibility_graph/3.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_MIN_MAX_FACE/in1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0},{0,1,0},{0,0,1},{-1,0,0},{0,0,-1},{0,-1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_MIN_MAX_FACE/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test spherize/1-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,2,-2,2,-2,2,-2},{-2,-2,2,2,-2,-2,2,2},{-2,-2,-2,-2,2,2,2,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{2},{2},{2},{2},{2},{2}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 64)
assert(volume Pd == 64)
LE = reverse {1,12,48,64};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test free_sum_decomposition/2-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,1,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1,1},{-2,-1,1},{-2,-1,-1},{0,1,-1},{1,1,-1},{1,1,1},{1,-1,1},{1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,8/3,2,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test free_sum_decomposition/3-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,0,1,1,0,0,-4},{1,0,1,-2,1,1,-3,1},{0,0,-1,1,1,1,1,-3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1,-2},{1,0,0},{1,0,-1},{0,1,0},{0,0,1},{-1,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 25/3)
assert(volume Pd == 25/3)
LE = reverse {1,37/6,25/2,25/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test AFFINE_HULL/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/6)
assert(volume Pd == 1/6)
LE = reverse {1,11/6,1,1/6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_GRAPH/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test schlegel_params/1-in.poly
-- Checking ehrhart and volume
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
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test schlegel_params/2.poly
-- Checking ehrhart and volume
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
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test DUAL_BOUNDED_H_VECTOR/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,1,0},{0,0,1,0,0,1},{0,0,0,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{0,0,-1},{0,0,1},{0,-1,0},{1,1,0}};
ineqrhsPd = matrix {{0},{0},{1},{0},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/2)
assert(volume Pd == 1/2)
LE = reverse {1,5/2,2,1/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_VERTEX_COLORS/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_VERTEX_COLORS/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,8/3,2,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_bipyramid/simpyr2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3,0,0,1},{0,0,3,0,1},{0,0,0,3,-5}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-5,0,-1},{-1,0,0},{1,1,1},{5,5,-1},{0,-1,0},{0,-5,-1}};
ineqrhsPd = matrix {{0},{0},{3},{15},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 12)
assert(volume Pd == 12)
LE = reverse {1,3,9,12};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_bipyramid/cubepyr1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,1},{-1,-1,-1,-1,1,1,1,1,-1,1},{0,0,0,0,0,0,0,0,2,-2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{1,0,0,1},{0,-1,0,0},{0,1,0,1},{0,0,-1,0},{0,0,1,1},{-1,0,0,-1},{1,0,0,0},{0,-1,0,-1},{0,1,0,0},{0,0,-1,-1},{0,0,1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,14,16,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_bipyramid/simplex.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3,0},{0,0,3}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{-1,0},{0,-1}};
ineqrhsPd = matrix {{3},{0},{0}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 9/2)
assert(volume Pd == 9/2)
LE = reverse {1,9/2,9/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_bipyramid/cube.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_bipyramid/simpyr1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3,0,1,1},{0,0,3,1,1},{0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,-1},{-1,0,1},{1,1,1},{1,1,-1},{0,-1,1},{0,-1,-1}};
ineqrhsPd = matrix {{0},{0},{3},{3},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,7/2,9/2,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test vertex_barycenter/cr.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1},{-1,1,1,1},{1,-1,1,1},{-1,-1,1,1},{1,1,-1,1},{-1,1,-1,1},{1,-1,-1,1},{-1,-1,-1,1},{1,1,1,-1},{-1,1,1,-1},{1,-1,1,-1},{-1,-1,1,-1},{1,1,-1,-1},{-1,1,-1,-1},{1,-1,-1,-1},{-1,-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,8/3,10/3,4/3,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test TRIANGULATION/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test rand_aof/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,8/3,2,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_automorphisms/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1,1,1,1,-1,-1,-1,-1},{1,0,1,-1,0,1,1,-3,-1,1},{-1,-1,1,1,0,0,1,1,-1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1,-1},{1,0,0},{1,0,-1},{0,1,0},{0,0,-1},{0,0,1},{-1,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 23/3)
assert(volume Pd == 23/3)
LE = reverse {1,35/6,23/2,23/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_automorphisms/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,1,0,-1,-1},{1,1,0,-1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1},{0,-1},{-1,0},{-1,1},{0,1},{1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,3,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/3p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,8/3,2,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/4p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,-1,1,-1,1,-1,-1},{1,-1,1,1,-1,-1,1,-1},{1,-1,-1,-1,1,1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/2p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,0,0,1,1,0,1},{1,1,1,0,0,0,0,1,0,1},{0,0,1,0,1,1,0,0,0,0},{0,0,1,0,1,1,0,1,1,1},{0,1,1,1,0,1,0,0,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0},{-1,0,-1,0,-1},{-1,0,0,-1,0},{1,0,1,0,-1},{0,0,1,-1,0},{1,0,0,0,0},{0,0,0,0,1},{1,-1,0,1,0},{-1,1,0,0,0}};
ineqrhsPd = matrix {{0},{-1},{-1},{1},{0},{1},{1},{1},{0}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/15)
assert(volume Pd == 1/15)
LE = reverse {1,44/15,41/12,2,7/12,1/15};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/1p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,-1,0,0},{0,1,0,0,-1,0},{0,0,1,0,0,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,8/3,2,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,1,0,1,1,1,0,1,1},{1,0,0,0,1,1,0,0,1,1},{0,1,0,0,0,0,0,1,0,1},{1,1,0,1,0,1,0,1,0,1},{0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0},{-1,0,-1,0,-1},{-1,0,0,-1,0},{1,0,1,0,-1},{0,0,1,-1,0},{1,0,0,0,0},{0,0,0,0,1},{1,-1,0,1,0},{-1,1,0,0,0}};
ineqrhsPd = matrix {{0},{-1},{-1},{1},{0},{1},{1},{1},{0}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/15)
assert(volume Pd == 1/15)
LE = reverse {1,44/15,41/12,2,7/12,1/15};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/5.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VertexPerm/5p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test congruent_polytopes/24cell-non-regular.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,1,0,0,1,1,0,1,0,-1,-1,0,0,1,0,0,-1,0,-1,0,0,-1},{0,0,0,-1,0,0,0,-1,0,0,-1,0,0,-1,0,-1,0,-1,1,1,1,1,1,1},{-1,1,1,0,0,0,0,0,1,-1,1,1,0,0,-1,-1,-1,0,0,0,1,-1,0,0},{-1,1,-1,-1,-1,1,-1,0,0,0,0,0,1,0,0,0,1,1,1,-1,0,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0},{1,1,0,1},{1,0,0,1},{0,-1,0,0},{1,0,1,1},{-1,-1,-1,0},{-1,0,0,-1},{0,0,1,0},{0,0,0,1},{-1,-1,0,0},{-1,0,0,0},{-2,-1,-1,-1},{-1,0,-1,0},{1,1,1,1},{0,1,0,0},{-1,-1,0,-1},{-1,-1,-1,-1},{-1,0,-1,-1},{0,0,0,-1},{1,1,1,0},{1,0,1,0},{2,1,1,1},{1,1,0,0},{1,0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,8,8,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test congruent_polytopes/polar_24-cell.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,-1,-1,-1,-1,-1,-1},{-1,0,-1,-1,0,1,1,1,-1,0,0,1,0,0,1,0,-1,0,0,1,0,0,0,-1},{-1,-1,0,1,1,-1,0,1,0,0,0,0,1,-1,0,1,0,-1,0,0,1,0,-1,0},{0,-1,-1,0,-1,0,-1,0,0,-1,1,0,0,0,1,1,1,1,1,0,0,-1,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{-1/2,-1/2,-1/2,-1/2},{-1/2,-1/2,-1/2,1/2},{-1/2,-1/2,1/2,-1/2},{-1/2,-1/2,1/2,1/2},{-1/2,1/2,-1/2,-1/2},{-1/2,1/2,-1/2,1/2},{-1/2,1/2,1/2,-1/2},{-1/2,1/2,1/2,1/2},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1},{0,0,0,1},{0,0,1,0},{0,1,0,0},{1/2,-1/2,-1/2,-1/2},{1/2,-1/2,-1/2,1/2},{1/2,-1/2,1/2,-1/2},{1/2,-1/2,1/2,1/2},{1/2,1/2,-1/2,-1/2},{1/2,1/2,-1/2,1/2},{1/2,1/2,1/2,-1/2},{1/2,1/2,1/2,1/2},{1,0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,16/3,8,32/3,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1c.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/8.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0},{1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0},{1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1},{0,1,0,0,1,0,0,1,1,0,1,0,0,1,1,0,1,1,0,1,1,0,0,1,1,0,1,1,0,1,1,1,0,1,1},{0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1,1,0,1,1,0,1,0,1,0,1,1,0,1,1,1,0,1,1,1},{0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,1,0,1,1,1,0,0,1,0,1,1,0,1,1,1,0,1,1,1,1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0,0,0,0,0},{-1,0,0,0,0,0,0},{0,1,0,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,1,0,0,0,0},{0,0,-1,0,0,0,0},{0,0,0,1,0,0,0},{0,0,0,-1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,1,0},{0,0,0,0,0,-1,0},{0,0,0,0,0,0,1},{0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{1},{0},{1},{0},{1},{0},{1},{0},{1},{0},{1},{0}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-4}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 151/360)
assert(volume Pd == 151/360)
LE = reverse {1,259/60,3199/360,21/2,259/36,161/60,151/360};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1h.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/19.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,1,1,1},{1,1,1,0,0,0},{1,1,2,1,1,2},{1,2,1,1,2,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1},{1,-1,0,0},{0,0,-1,0},{-1,1,0,0},{0,0,1,1}};
ineqrhsPd = matrix {{-1},{1},{-1},{1},{3}};
eqlhsPd = matrix {{-1,-1,0,0}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/2)
assert(volume Pd == 1/2)
LE = reverse {1,5/2,2,1/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/3-cube+action.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1b.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1f.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/18.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,1,1,0,0,0,1,1},{0,0,1,0,1,0,1,1,0,0},{0,1,1,0,0,1,0,0,0,1},{1,1,0,0,0,0,0,1,1,0},{1,0,0,1,0,1,1,0,0,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0,0,0},{0,0,1,0,0},{-1,0,0,0,0},{-1,-1,-1,-1,0},{0,0,0,1,0},{0,1,0,0,0},{1,0,0,0,0},{0,0,-1,0,0},{0,0,0,-1,0},{1,1,1,1,0}};
ineqrhsPd = matrix {{0},{1},{0},{-1},{1},{1},{1},{0},{0},{2}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-2}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 11/24)
assert(volume Pd == 11/24)
LE = reverse {1,35/12,85/24,25/12,11/24};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1d.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/3a.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,-1,0},{1,0,0,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1},{-1,1},{1,1},{1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/14.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0},{0,-1,0,0,0},{0,1,0,0,0},{1,0,0,0,0}};
ineqrhsPd = matrix {{0},{0},{1},{1}};
eqlhsPd = matrix {{0,0,-1,0,0},{0,0,0,-1,0},{0,0,0,0,-1}};
eqrhsPd = matrix {{0},{0},{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,2,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1a.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1e.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/1g.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-211,21,20,-1176/5,-1,0},{1160/121,105/11,-1,1050/121,1055/121,0},{-1,-1281/5,-22,-21,-232,0},{1171/121,1176/121,1,1281/121,116/11,0},{1,-210,22,21,-1171/5,0},{-1276/5,-21,-20,-231,1,0}};
ineqrhsPd = matrix {{10},{1105/121},{-11},{1226/121},{11},{-10}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 777/40)
assert(volume Pd == 777/40)
LE = reverse {1,1599/20,15/8,-771/8,1/8,777/40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1},{0,1,0},{1,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,0},{-1,0,0},{0,-1,0}};
ineqrhsPd = matrix {{1},{0},{0}};
eqlhsPd = matrix {{-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/2)
assert(volume Pd == 1/2)
LE = reverse {1,3/2,1/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,0,1},{0,-1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1},{-1,1},{1,1},{1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/2a.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1},{0,1,0},{1,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,0},{-1,0,0},{0,-1,0}};
ineqrhsPd = matrix {{1},{0},{0}};
eqlhsPd = matrix {{-1,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/2)
assert(volume Pd == 1/2)
LE = reverse {1,3/2,1/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/12.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test action/7.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test edge_middle/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,-1,1,0,-1,1,0,-1,-1,1,1,0},{-1,0,0,1,-1,-1,-1,1,0,1,0,1},{-1,-1,-1,-1,0,0,1,0,1,0,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,1},{1,-1,1},{0,-1,0},{-1,1,-1},{-1,0,0},{-1,-1,-1},{-1,-1,1},{-1,1,1},{1,-1,-1},{0,0,-1},{1,1,-1},{1,1,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{1},{2},{1},{2},{1},{2},{2},{2},{2},{1},{2},{2},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 20/3)
assert(volume Pd == 20/3)
LE = reverse {1,10/3,8,20/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test edge_middle/1-in.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test centroid_volume/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0,0},{1,0,0,0,0,0},{0,-1,0,0,0,0},{0,1,0,0,0,0},{0,0,-1,0,0,0},{0,0,1,0,0,0},{0,0,0,-1,0,0},{0,0,0,1,0,0},{0,0,0,0,-1,0},{0,0,0,0,1,0},{0,0,0,0,0,-1},{0,0,0,0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 64)
assert(volume Pd == 64)
LE = reverse {1,12,60,160,240,192,64};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test centroid_volume/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,1,-1,0},{-1,-1,1,1,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{1,0},{0,-1},{-1,0},{-1,1}};
ineqrhsPd = matrix {{2},{1},{1},{1},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5)
assert(volume Pd == 5)
LE = reverse {1,4,5};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test centroid_volume/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},{1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,8/3,2,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test permutahedron/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4},{2,2,3,3,4,4,1,1,3,3,4,4,1,1,2,2,4,4,1,1,2,2,3,3},{3,4,2,4,2,3,3,4,1,4,1,3,2,4,1,4,1,2,2,3,1,3,1,2},{4,3,4,2,3,2,4,3,4,1,3,1,4,2,4,1,2,1,3,2,3,1,2,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0},{0,-1,-1,0},{-1,-1,-1,0},{-1,0,-1,0},{-1,0,0,0},{-1,-1,0,0},{0,1,1,0},{0,0,1,0},{0,1,0,0},{0,-1,0,0},{1,0,1,0},{1,1,1,0},{1,1,0,0},{1,0,0,0}};
ineqrhsPd = matrix {{-1},{-3},{-6},{-3},{-1},{-3},{7},{4},{4},{-1},{7},{9},{7},{4}};
eqlhsPd = matrix {{-1,-1,-1,-1}};
eqrhsPd = matrix {{-10}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,6,15,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test permutahedron/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,3,-3,3,-3,3,-3,3,-3,3,-3,3,-3,3,-3,3,-3},{2,2,-2,-2,2,2,-2,-2,3,3,-3,-3,3,3,-3,-3,1,1,-1,-1,1,1,-1,-1,3,3,-3,-3,3,3,-3,-3,1,1,-1,-1,1,1,-1,-1,2,2,-2,-2,2,2,-2,-2},{3,3,3,3,-3,-3,-3,-3,2,2,2,2,-2,-2,-2,-2,3,3,3,3,-3,-3,-3,-3,1,1,1,1,-1,-1,-1,-1,2,2,2,2,-2,-2,-2,-2,1,1,1,1,-1,-1,-1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,-1},{1,0,-1},{1,-1,-1},{0,0,-1},{0,1,-1},{0,-1,-1},{-1,1,-1},{-1,-1,-1},{-1,0,-1},{-1,0,0},{-1,-1,0},{-1,0,1},{-1,1,0},{-1,-1,1},{-1,1,1},{0,-1,1},{0,1,1},{0,-1,0},{0,0,1},{0,1,0},{1,-1,0},{1,-1,1},{1,0,1},{1,1,1},{1,1,0},{1,0,0}};
ineqrhsPd = matrix {{6},{5},{6},{3},{5},{5},{6},{6},{5},{3},{5},{5},{5},{6},{6},{5},{5},{3},{3},{3},{5},{6},{5},{6},{5},{3}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 172)
assert(volume Pd == 172)
LE = reverse {1,12,66,172};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test permutahedron/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5},{2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,1,1,1,1,1,1,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,1,1,1,1,1,1,2,2,2,2,2,2,4,4,4,4,4,4,5,5,5,5,5,5,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4},{3,3,4,4,5,5,2,2,4,4,5,5,2,2,3,3,5,5,2,2,3,3,4,4,3,3,4,4,5,5,1,1,4,4,5,5,1,1,3,3,5,5,1,1,3,3,4,4,2,2,4,4,5,5,1,1,4,4,5,5,1,1,2,2,5,5,1,1,2,2,4,4,2,2,3,3,5,5,1,1,3,3,5,5,1,1,2,2,5,5,1,1,2,2,3,3,2,2,3,3,4,4,1,1,3,3,4,4,1,1,2,2,4,4,1,1,2,2,3,3},{4,5,3,5,3,4,4,5,2,5,2,4,3,5,2,5,2,3,3,4,2,4,2,3,4,5,3,5,3,4,4,5,1,5,1,4,3,5,1,5,1,3,3,4,1,4,1,3,4,5,2,5,2,4,4,5,1,5,1,4,2,5,1,5,1,2,2,4,1,4,1,2,3,5,2,5,2,3,3,5,1,5,1,3,2,5,1,5,1,2,2,3,1,3,1,2,3,4,2,4,2,3,3,4,1,4,1,3,2,4,1,4,1,2,2,3,1,3,1,2},{5,4,5,3,4,3,5,4,5,2,4,2,5,3,5,2,3,2,4,3,4,2,3,2,5,4,5,3,4,3,5,4,5,1,4,1,5,3,5,1,3,1,4,3,4,1,3,1,5,4,5,2,4,2,5,4,5,1,4,1,5,2,5,1,2,1,4,2,4,1,2,1,5,3,5,2,3,2,5,3,5,1,3,1,5,2,5,1,2,1,3,2,3,1,2,1,4,3,4,2,3,2,4,3,4,1,3,1,4,2,4,1,2,1,3,2,3,1,2,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1,0},{0,0,-1,-1,0},{0,-1,-1,-1,0},{0,-1,0,-1,0},{-1,-1,-1,-1,0},{-1,0,-1,-1,0},{-1,-1,0,-1,0},{-1,0,0,-1,0},{-1,0,0,0,0},{-1,-1,0,0,0},{-1,0,-1,0,0},{0,1,1,1,0},{-1,-1,-1,0,0},{0,0,1,1,0},{0,1,0,1,0},{0,1,1,0,0},{0,0,0,1,0},{0,0,1,0,0},{0,1,0,0,0},{0,-1,0,0,0},{0,-1,-1,0,0},{1,0,1,1,0},{1,0,0,1,0},{1,0,1,0,0},{0,0,-1,0,0},{1,1,0,1,0},{1,1,1,1,0},{1,1,1,0,0},{1,1,0,0,0},{1,0,0,0,0}};
ineqrhsPd = matrix {{-1},{-3},{-6},{-3},{-10},{-6},{-6},{-3},{-1},{-3},{-3},{12},{-6},{9},{9},{9},{5},{5},{5},{-1},{-3},{12},{9},{9},{-1},{12},{14},{12},{9},{5}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-15}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 125)
assert(volume Pd == 125)
LE = reverse {1,10,45,110,125};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test SPECIAL_FACETS/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,1,-1},{0,1,-1,-1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{0,1},{-2,-1},{1,1},{1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5/2)
assert(volume Pd == 5/2)
LE = reverse {1,5/2,5/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test dual_linear_program/3-0.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test dual_linear_program/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = map(QQ^4, QQ^0, 0);
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^0, QQ^4, 0);
ineqrhsPd = map(QQ^0, QQ^1, 0);
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test CUBICAL/2.poly
-- Checking ehrhart and volume
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
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test minkowski_cone/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,1,-1,-2,-1,1},{0,1,1,0,-1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1},{-1,-1},{-1,1},{0,-1},{1,-1},{1,1}};
ineqrhsPd = matrix {{1},{2},{2},{1},{2},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 6)
assert(volume Pd == 6)
LE = reverse {1,4,6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test minkowski_cone/s2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,-2,-1},{0,0,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1},{0,1},{-1,-1}};
ineqrhsPd = matrix {{0},{0},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,2,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test minkowski_cone/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,1,-1,-2,-1,1},{0,1,1,0,-1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1},{-1,-1},{-1,1},{0,-1},{1,-1},{1,1}};
ineqrhsPd = matrix {{1},{2},{2},{1},{2},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 6)
assert(volume Pd == 6)
LE = reverse {1,4,6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test minkowski_cone/s1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,-1,-2},{0,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{0,-1},{-1,1}};
ineqrhsPd = matrix {{0},{0},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,2,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test SCHLEGEL_SOLID/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test TRIANGULATION_BOUNDARY/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{0,-1,0},{0,0,-1},{2,2,1}};
ineqrhsPd = matrix {{0},{0},{0},{2}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/3)
assert(volume Pd == 1/3)
LE = reverse {1,13/6,3/2,1/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test TRIANGULATION_BOUNDARY/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1},{-1,1,1,1},{1,-1,1,1},{-1,-1,1,1},{1,1,-1,1},{-1,1,-1,1},{1,-1,-1,1},{-1,-1,-1,1},{1,1,1,-1},{-1,1,1,-1},{1,-1,1,-1},{-1,-1,1,-1},{1,1,-1,-1},{-1,1,-1,-1},{1,-1,-1,-1},{-1,-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,8/3,10/3,4/3,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test 2-face-sizes-simple/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/9-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/2-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,1,0,0,1},{0,0,1,1,0,1},{1,0,0,0,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0},{1,-1,-1},{-1,1,1}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1},{0},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,7/3,2,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/9.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,-1},{0,1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1},{1,-1},{-1,1},{1,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/5-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/1-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/8-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1},{0,0},{0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0},{-1,0,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{0,-1,0},{0,0,-1}};
eqrhsPd = matrix {{0},{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/3-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/7-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1},{0,0},{0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0},{-1,0,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{0,-1,0},{0,0,-1}};
eqrhsPd = matrix {{0},{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test truncation/4-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{-1,0,0},{0,-1,0},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1,3,3,1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test FAR_HYPERPLANE/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/6)
assert(volume Pd == 1/6)
LE = reverse {1,11/6,1,1/6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test compose/in1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test graph_from_face_lattice/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test graph_from_face_lattice/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 16)
assert(volume Pd == 16)
LE = reverse {1,8,24,32,16};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test splits_in_subdivision/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{0,0,-1},{0,-1,0},{0,1,0},{1,0,0},{0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test FACE_SIMPLICITY/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0},{1,0,0,1,0},{0,0,1,1,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1,-1},{0,-1,0},{-1,0,0},{-1,2,1},{2,-1,1},{1,1,-1}};
ineqrhsPd = matrix {{-1},{0},{0},{2},{2},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,11/6,3/2,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test FACE_SIMPLICITY/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/6)
assert(volume Pd == 1/6)
LE = reverse {1,11/6,1,1/6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test SCHLEGEL_CONSTRUCTION/1.poly
-- Checking ehrhart and volume
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
assert(volume P == 8)
assert(volume Pd == 8)
LE = reverse {1,6,12,8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test product/2-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,2,2},{0,2,0,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{0},{0},{2},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test product/1-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1},{0,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{-1,0},{0,-1}};
ineqrhsPd = matrix {{1},{0},{0}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/2)
assert(volume Pd == 1/2)
LE = reverse {1,3/2,1/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test product/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,1,0,0,0,0},{0,0,2,2,0,0,2,2,0,0,2,2},{0,2,0,2,0,2,0,2,0,2,0,2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1},{-1,0,0,0},{0,0,-1,0},{0,0,0,1},{0,0,1,0},{0,-1,0,0},{1,1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{2},{2},{0},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,11/2,21/2,8,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test product/1-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,2,2},{0,2,0,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{0},{0},{2},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test product/2-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1},{0,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{-1,0}